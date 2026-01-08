/* Programa: ESFT009.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Faturamento
** Objetivo: Listar o relat¢rio de Notas Fiscais por Representante.
** Autor...: Sandro Wiest/Gilvando de Souza Araujo - Junho/95
** Obs.....: Especifico da CIA.RENASCENCA INDUSTRIAL
**
** Conversao para EMS 2.04:
**   Programa: ESFT009.P  =>  ESFT0005RP.P
**   Autor...: Gilvando Souza Araujo
**   Data....: 20/01/2004
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESFT0005RP 2.04.00.000}

DEFINE TEMP-TABLE tt-param  NO-UNDO
       FIELD destino           AS INTEGER
       FIELD arquivo           AS CHAR FORMAT "x(35)"
       FIELD usuario           AS CHAR FORMAT "x(12)"
       FIELD data-exec         AS DATE
       FIELD hora-exec         AS INTEGER
       FIELD classifica        AS INTEGER
       FIELD desc-classifica   AS CHAR FORMAT "x(40)"
       FIELD ini-cod-estabel   like nota-fiscal.cod-estabel 
       FIELD fin-cod-estabel   like nota-fiscal.cod-estabel 
       FIELD ini-dt-emissao    LIKE nota-fiscal.dt-emis-nota
       FIELD fin-dt-emissao    LIKE nota-fiscal.dt-emis-nota
       FIELD ini-cod-emitente  LIKE nota-fiscal.cod-emitente
       FIELD fin-cod-emitente  LIKE nota-fiscal.cod-emitente
       FIELD ini-cod-rep       LIKE nota-fiscal.cod-rep
       FIELD fin-cod-rep       LIKE nota-fiscal.cod-rep
       FIELD enviar-e-mail     AS LOG FORMAT "Sim/NÆo"
       FIELD subject-e-mail    AS CHAR FORMAT "x(40)"
       FIELD texto-e-mail      AS CHAR FORMAT "x(2000)"
       FIELD imp-oque          AS INT
       FIELD desc-imp-oque     AS CHAR FORMAT "x(40)"
       FIELD l-batch           AS LOG
       FIELD imp-param         AS LOG.

define temp-table tt-raw-digita
       field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

/* Altera parƒmetros, quando execu‡Æo for em batch */
IF tt-param.l-batch THEN DO:
   assign tt-param.usuario          = "super"
          tt-param.destino          = 2 /* Arquivo */
          tt-param.arquivo          = "ESFT0005.LST"
          tt-param.data-exec        = TODAY
          tt-param.hora-exec        = TIME
          tt-param.ini-cod-estabel  = "1"
          tt-param.fin-cod-estabel  = "999"
          tt-param.ini-cod-emitente = 1                 
          tt-param.fin-cod-emitente = 999999999
          tt-param.ini-cod-rep      = 2
          tt-param.fin-cod-rep      = 999999
          tt-param.enviar-e-mail    = YES
          tt-param.subject-e-mail   = "Pedidos Faturados - #PER-INI a #PER-FIN"
          tt-param.texto-e-mail     = "Segue anexo Rela‡Æo de Pedidos faturados no per¡odo de #PER-INI a #PER-FIN." +
                                      CHR(13) + CHR(13) + "Atenciosamente," + CHR(13) + CHR(13) +
                                      "TEAR TEXTIL INDUSTRIA E COMERCIO LTDA." + CHR(13) + 
                                      "Departamento Comercial."
          tt-param.imp-oque         = 2 
          tt-param.desc-imp-oque    = "Somente Representantes sem e-mail"
          tt-param.imp-param        = NO. 

   /* O programa esta agendado para rodar de Seg a Sex …s 06:00, selecionando
   ** o dia anterior quando for Ter a Dom e, Sex, Sab e Dom, quando for Seg */
   IF WEEKDAY(TODAY) = 2 THEN /* Seg */
      ASSIGN tt-param.ini-dt-emissao = TODAY - 3
             tt-param.fin-dt-emissao = TODAY - 1.
   ELSE                       /* Ter a Dom */
      ASSIGN tt-param.ini-dt-emissao = TODAY - 1
             tt-param.fin-dt-emissao = TODAY - 1.
END.

DEFINE TEMP-TABLE tt-rep-email
       FIELD cod-rep LIKE repres.cod-rep
       INDEX ch-rep cod-rep.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* include padrÆo para impressÆo de campos editores em relat¢rio  */
{include/tt-edit.i}

/* defini‡Æo de vari veis  */
def var h-acomp as handle no-undo.

def var de-quantidade      as dec format ">>>>,>>9.99" label "Quantidade".
def var de-quantidade-nota like de-quantidade.
def var de-quantidade-cli  like de-quantidade.
def var de-quantidade-rep  like de-quantidade.
def var de-valor           as dec format ">,>>>,>>9.99" label "Valor".
def var de-valor-nota      like de-valor.
def var de-valor-cli       like de-valor.
def var de-valor-rep       like de-valor.
DEF VAR c-aux-e-mail       AS CHAR.
DEF VAR i-e-mail-enviado   AS INT.
DEF VAR i-e-mail-nenviado  AS INT.
DEF VAR c-destinatar       AS CHAR.

form 
    "*------------ Parƒmetros/Sele‡Æo -------------*" SKIP
    tt-param.ini-cod-estabel  LABEL "Estabelec....."  SKIP
    "A"  AT 30
    tt-param.fin-cod-estabel  NO-LABELS SKIP
    tt-param.ini-dt-emissao   label "Data Emissao.."
    "A"  AT 30
    tt-param.fin-dt-emissao   NO-LABELS SKIP
    tt-param.ini-cod-emitente label "Cliente......."
    "A"  AT 30
    tt-param.fin-cod-emitente no-labels SKIP
    tt-param.ini-cod-rep      label "Representante."
    "A"  AT 30
    tt-param.fin-cod-rep      no-labels SKIP
    tt-param.enviar-e-mail    LABEL "Enviar e-mail."  SKIP
    tt-param.desc-imp-oque    LABEL "Imprimir......"  SKIP
    i-e-mail-enviado          LABEL "E-mail env...."  SKIP
    i-e-mail-nenviado         LABEL "E-mail n/env.."  SKIP
    with FRAME f-param side-labels no-box width 132 STREAM-IO.

FORM HEADER      
   /*          1         2         3         4         5         6         
      123456789.123456789.123456789.123456789.123456789.123456789.123456" */
     "Cliente Nome-Abrev   Pedido Ped Rep      Nota     Emissao    Item " AT  1
     "  Descricao                     Refer     Preco Unit Quantidade Un" AT 67
     "------- ------------ ------ ------------ -------- ---------- -----" at  1
     "- ----------------------------- ------- ------------ ---------- --" at 67
     with page-top no-box width 132 STREAM-IO frame f-cab-nota.

FORM
    nota-fiscal.cod-emitente   FORMAT "9999999"
    nota-fiscal.nome-ab-cli   
    it-nota-fisc.nr-pedcli     FORMAT "x(6)"
    ped-venda.nr-pedrep        FORMAT "x(12)"
    it-nota-fisc.nr-nota-fis   FORMAT "x(8)"
    it-nota-fisc.dt-emis-nota 
    it-nota-fisc.it-codigo     FORMAT "x(6)" 
    ITEM.desc-item             FORMAT "x(29)"
    it-nota-fisc.cod-refer     FORMAT "x(7)"
    it-nota-fisc.vl-preori     FORMAT ">,>>>,>>9.99"
    de-quantidade              FORMAT ">>>,>>9.99"
    it-nota-fisc.un[1]         
    with no-labels no-box 55 down width 132 STREAM-IO frame f-detalhe.

FORM
    fat-duplic.nr-fatura
    fat-duplic.parcela      FORMAT "99"
    fat-duplic.dt-venciment
    fat-duplic.vl-parcela   
    with no-box 55 down width 132 STREAM-IO frame f-det-fat.

form
    "Representante: " 
    repres.cod-rep " - " 
    repres.nome
    SKIP(1)
    with no-labels no-box no-attr-space width 132 STREAM-IO 1 down frame f-repres.
                
/* include padrÆo para output de relat¢rios */
{include/i-rpout.i}

/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */
{include/i-rpcab.i}

/* bloco principal do programa */
find first param-global no-lock no-error.
find first empresa
     where empresa.ep-codigo = param-global.empresa-prin no-lock no-error. 

assign c-empresa = (if avail empresa then empresa.razao-social else "").

{utp/ut-liter.i FATURAMENTO * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Relatorio_de_Notas_Fiscais_por_Representantes * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.
VIEW FRAME f-cab-nota.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

DEF STREAM email.
ASSIGN c-aux-e-mail = SESSION:TEMP-DIRECTORY + "aux-e-mail.txt".

for each nota-fiscal WHERE nota-fiscal.cod-estabel  >= tt-param.ini-cod-estabel
                       AND nota-fiscal.cod-estabel  <= tt-param.fin-cod-estabel
                       AND nota-fiscal.dt-emis-nota >= tt-param.ini-dt-emissao
                       and nota-fiscal.dt-emis-nota <= tt-param.fin-dt-emissao
                       AND nota-fiscal.cod-emitente >= tt-param.ini-cod-emitente
                       AND nota-fiscal.cod-emitente <= tt-param.fin-cod-emitente
                       AND nota-fiscal.cod-rep      >= tt-param.ini-cod-rep
                       AND nota-fiscal.cod-rep      <= tt-param.fin-cod-rep
                       AND nota-fiscal.emite-dup    =  YES
                       and nota-fiscal.dt-cancela   =  ? 
                     no-lock
    break by nota-fiscal.cod-rep
          by nota-fiscal.cod-emit
          by nota-fiscal.nr-nota-fis:

    run pi-acompanhar in h-acomp (input string(nota-fiscal.cod-rep) + " " + nota-fiscal.nr-nota-fis).

    IF FIRST-OF (nota-fiscal.cod-rep) then do:
       FIND repres where repres.cod-rep = nota-fiscal.cod-rep no-lock.

       IF  tt-param.imp-oque = 1 OR
          (tt-param.imp-oque = 2 AND repres.e-mail = "") THEN DO:
          DISPLAY repres.cod-rep 
                  repres.nome
                  with frame f-repres.
          DOWN with frame f-repres.
       END.

       IF tt-param.enviar-e-mail THEN DO:
          OUTPUT STREAM email TO value(c-aux-e-mail).
          PUT STREAM email
              c-empresa " - "
              " PEDIDOS FATURADOS - PERIODO: "
              tt-param.ini-dt-emissao 
              " a "
              tt-param.fin-dt-emissao
              " - EXTRATO GERADO EM: " TODAY " " STRING(TIME,"HH:MM")
              SKIP(1)
              "Representante: " 
              repres.cod-rep " - " 
              repres.nome
              SKIP(1)
              "Cliente Nome-Abrev   Pedido Ped Rep      Nota     Emissao    Item " AT  1
              "  Descricao                     Refer     Preco Unit Quantidade Un" AT 67
              "------- ------------ ------ ------------ -------- ---------- -----" at  1
              "- ----------------------------- ------- ------------ ---------- --" at 67
              SKIP.

          /* Atualiza temp-table com Representantes aos quais foram enviados e-mails */
          FIND tt-rep-email WHERE tt-rep-email.cod-rep = nota-fiscal.cod-rep NO-LOCK NO-ERROR.
          IF NOT AVAIL tt-rep-email THEN DO:
             CREATE tt-rep-email.
             ASSIGN tt-rep-email.cod-rep = nota-fiscal.cod-rep.
          END.
       END.
    END.

    find ped-venda where ped-venda.nr-pedcli  = nota-fiscal.nr-pedcli
                     and ped-venda.nome-abrev = nota-fiscal.nome-ab-cli
                   no-lock no-error.
    for each it-nota-fisc of nota-fiscal no-lock.
        assign de-quantidade = it-nota-fisc.qt-faturada[1].
        FIND ITEM WHERE ITEM.it-codigo = it-nota-fisc.it-codigo 
                  NO-LOCK NO-ERROR.

        IF  tt-param.imp-oque = 1 OR
           (tt-param.imp-oque = 2 AND repres.e-mail = "") THEN DO:
           DISPLAY nota-fiscal.cod-emitente 
                   nota-fiscal.nome-ab-cli  
                   it-nota-fisc.nr-pedcli
                   ped-venda.nr-pedrep      WHEN AVAIL ped-venda
                   it-nota-fisc.nr-nota-fis
                   it-nota-fisc.dt-emis-nota
                   it-nota-fisc.it-codigo
                   ITEM.desc-item
                   it-nota-fisc.cod-refer
                   it-nota-fisc.vl-preori
                   de-quantidade
                   it-nota-fisc.un[1]
                   with frame f-detalhe. 
           DOWN WITH FRAME f-detalhe.
        END.

        IF tt-param.enviar-e-mail THEN DO:
           PUT STREAM email
                      nota-fiscal.cod-emitente    FORMAT "9999999"        AT   1                   
                      nota-fiscal.nome-ab-cli                             AT   9                
                      it-nota-fisc.nr-pedcli      FORMAT "x(6)"           AT  22                
                      ped-venda.nr-pedrep         FORMAT "x(12)"          AT  29
                      it-nota-fisc.nr-nota-fis    FORMAT "x(8)"           AT  42                
                      it-nota-fisc.dt-emis-nota                           AT  51                
                      it-nota-fisc.it-codigo      FORMAT "x(6)"           AT  62                
                      ITEM.desc-item              FORMAT "x(29)"          AT  69                
                      it-nota-fisc.cod-refer      FORMAT "x(7)"           AT  99                
                      it-nota-fisc.vl-preori      FORMAT ">,>>>,>>9.99"   AT 107                
                      de-quantidade               FORMAT ">>>,>>9.99"     AT 120                
                      it-nota-fisc.un[1]                                  AT 131                
                      SKIP.
        END.

        assign de-quantidade-nota = de-quantidade-nota +
                                    de-quantidade
               de-quantidade-rep = de-quantidade-rep +
                                   de-quantidade
               de-quantidade-cli = de-quantidade-cli +
                                   de-quantidade.
    END.

    IF  tt-param.imp-oque = 1 OR
       (tt-param.imp-oque = 2 AND repres.e-mail = "") THEN
       DOWN(1) WITH FRAME f-detalhe.
    
    IF tt-param.enviar-e-mail THEN DO:
       PUT STREAM email
           SKIP(1)
           "Fatura  Pa Vencimento             Valor"
           SKIP
           "------- -- ---------- -----------------"
           SKIP.
    END.

    FOR EACH fat-duplic where fat-duplic.cod-estabel = nota-fiscal.cod-estabel
                          AND fat-duplic.serie       = nota-fiscal.serie
                          AND fat-duplic.nr-fatura   = nota-fiscal.nr-nota-fis
                        no-lock:

        IF  tt-param.imp-oque = 1 OR
           (tt-param.imp-oque = 2 AND repres.e-mail = "") THEN DO:
           DISPLAY fat-duplic.nr-fatura
                   fat-duplic.parcela
                   fat-duplic.dt-venciment
                   fat-duplic.vl-parcela
                   with frame f-det-fat.
           down with frame f-det-fat.
        END.

        assign de-valor-nota = de-valor-nota + fat-duplic.vl-parcela
               de-valor-rep  = de-valor-rep + fat-duplic.vl-parcela
               de-valor-cli  = de-valor-cli + fat-duplic.vl-parcela.

        IF tt-param.enviar-e-mail THEN DO:
           PUT STREAM email
                      fat-duplic.nr-fatura    FORMAT "x(7)" AT  1
                      fat-duplic.parcela      FORMAT "99"   AT  9
                      fat-duplic.dt-venciment               AT 12
                      fat-duplic.vl-parcela                 AT 23
                      SKIP.
        END.
    end.
    
    IF  tt-param.imp-oque = 1 OR
       (tt-param.imp-oque = 2 AND repres.e-mail = "") THEN DO:
       DISPLAY "Total Faturas" @ fat-duplic.nr-fatura
               de-valor-nota   @ fat-duplic.vl-parcela
               with frame f-det-fat.
       DOWN 2 with frame f-det-fat.
    END.

    IF tt-param.enviar-e-mail THEN
       PUT STREAM email            
                  "Total Faturas" AT  1 
                  de-valor-nota   AT 28
                  SKIP(1).

    assign de-quantidade-nota = 0
           de-valor-nota      = 0.

    if last-of(nota-fiscal.cod-emit) then do:
       find emitente where emitente.cod-emit = nota-fiscal.cod-emit
                           no-lock no-error.

       IF  tt-param.imp-oque = 1 OR
          (tt-param.imp-oque = 2 AND repres.e-mail = "") THEN DO:
          DISPLAY "Total"             @ nota-fiscal.cod-emitente
                  emitente.nome-abrev @ nota-fiscal.nome-ab-cli
                  de-valor-cli        @ it-nota-fisc.vl-preori
                  de-quantidade-cli   @ de-quantidade
                  WITH FRAME f-detalhe.
          DOWN 2 WITH FRAME f-detalhe.
       END.

       IF tt-param.enviar-e-mail THEN
          PUT STREAM email
              "Total"               AT   1     
              emitente.nome-abrev   AT   9
              de-valor-cli          AT 107
              de-quantidade-cli     AT 119
              SKIP(1).

       assign de-quantidade-cli = 0
              de-valor-cli      = 0.
    end.

    if last-of(nota-fiscal.cod-rep) then do:
       FIND repres WHERE repres.cod-rep = nota-fiscal.cod-rep
                   NO-LOCK NO-ERROR.

       IF  tt-param.imp-oque = 1 OR
          (tt-param.imp-oque = 2 AND repres.e-mail = "") THEN DO:
          DISPLAY "Total"           @ nota-fiscal.cod-emitente
                  repres.nome-abrev @ nota-fiscal.nome-ab-cli
                  "Periodo: " + string(tt-param.ini-dt-emissao) + " a " + 
                                STRING(tt-param.fin-dt-emissao)
                                    @ ITEM.desc-item
                  de-valor-rep      @ it-nota-fisc.vl-preori
                  de-quantidade-rep @ de-quantidade
                  with frame f-detalhe.
          down with frame f-detalhe.
       END.

       IF tt-param.enviar-e-mail THEN DO:
          IF repres.e-mail <> "" THEN DO:
             PUT STREAM email
                 "Total"             AT   1         
                 repres.nome-abrev   AT   9         
                 de-valor-rep        AT 107
                 de-quantidade-rep   AT 119.
             OUTPUT STREAM email CLOSE.
             
             ASSIGN tt-param.subject-e-mail = REPLACE(tt-param.subject-e-mail,"#PER-INI",STRING(tt-param.ini-dt-emissao))
                    tt-param.subject-e-mail = REPLACE(tt-param.subject-e-mail,"#PER-FIN",STRING(tt-param.fin-dt-emissao))
                    tt-param.texto-e-mail   = REPLACE(tt-param.texto-e-mail,"#PER-INI",STRING(tt-param.ini-dt-emissao))
                    tt-param.texto-e-mail   = REPLACE(tt-param.texto-e-mail,"#PER-FIN",STRING(tt-param.fin-dt-emissao))
                    c-destinatar = repres.e-mail + "," + "diley.lima@teartextil.com.br" + "," + 
                                   "teartextil@teartextil.com.br".
             RUN esapi/esapi002.p (INPUT "vendas@teartextil.com.br", /* e-mail remetente */
                                   INPUT c-destinatar, /* e-mail destinat rio */
                                   INPUT tt-param.subject-e-mail + " - " + repres.nome-abrev, /* Assunto */
                                   INPUT tt-param.texto-e-mail, /* Mensagem */
                                   INPUT c-aux-e-mail, /* Arquivo Anexo */
                                   INPUT YES). /* Mostra Erros */
             ASSIGN i-e-mail-enviado = i-e-mail-enviado + 1.
          END.
          ELSE DO:
             OUTPUT STREAM email CLOSE.
             ASSIGN i-e-mail-nenviado = i-e-mail-nenviado + 1.
          END.
          OS-DELETE VALUE(c-aux-e-mail). 
       END.

       assign de-quantidade-rep = 0
              de-valor-rep      = 0.

       IF  tt-param.imp-oque = 1 OR
          (tt-param.imp-oque = 2 AND repres.e-mail = "") THEN
          page.
    end.
end.

/* Envio de e-mail para Representantes que nÆo tiveram faturamento no per¡odo */
IF tt-param.enviar-e-mail THEN DO:
   FOR EACH repres WHERE repres.cod-rep >= tt-param.ini-cod-rep
                     AND repres.cod-rep <= tt-param.fin-cod-rep
                     AND repres.e-mail  <> ""
                   NO-LOCK:

       run pi-acompanhar in h-acomp (input string(repres.cod-rep)).
       
       FIND tt-rep-email WHERE tt-rep-email.cod-rep = repres.cod-rep NO-LOCK NO-ERROR.
       IF AVAIL tt-rep-email THEN NEXT. /* e-mail j  enviado */

       OUTPUT STREAM email TO value(c-aux-e-mail).
       PUT STREAM email
           c-empresa " - "
           " PEDIDOS FATURADOS - PERIODO: "
           tt-param.ini-dt-emissao 
           " a "
           tt-param.fin-dt-emissao
           " - EXTRATO GERADO EM: " TODAY " " STRING(TIME,"HH:MM")
           SKIP(1)
           "Representante: " 
           repres.cod-rep " - " 
           repres.nome
           SKIP(1)
           "Cliente Nome-Abrev   Pedido Ped Rep      Nota     Emissao    Item " AT  1
           "  Descricao                     Refer     Preco Unit Quantidade Un" AT 67
           "------- ------------ ------ ------------ -------- ---------- -----" at  1
           "- ----------------------------- ------- ------------ ---------- --" at 67
           SKIP(1)
           "*** NÆo houve faturamento para o per¡odo: " + STRING(tt-param.ini-dt-emissao) + " a " + 
                                                          STRING(tt-param.fin-dt-emissao) + " ***"
                                                          FORMAT "x(132)"
           SKIP.

       OUTPUT STREAM email CLOSE.
 
       ASSIGN tt-param.subject-e-mail = REPLACE(tt-param.subject-e-mail,"#PER-INI",STRING(tt-param.ini-dt-emissao))
              tt-param.subject-e-mail = REPLACE(tt-param.subject-e-mail,"#PER-FIN",STRING(tt-param.fin-dt-emissao))
              tt-param.texto-e-mail   = REPLACE(tt-param.texto-e-mail,"#PER-INI",STRING(tt-param.ini-dt-emissao))
              tt-param.texto-e-mail   = REPLACE(tt-param.texto-e-mail,"#PER-FIN",STRING(tt-param.fin-dt-emissao))
              c-destinatar = repres.e-mail + "," + "diley.lima@teartextil.com.br" + "," + 
                             "teartextil@teartextil.com.br".
       RUN esapi/esapi002.p (INPUT "vendas@teartextil.com.br", /* e-mail remetente */
                             INPUT c-destinatar, /* e-mail destinat rio */
                             INPUT tt-param.subject-e-mail + " - " + repres.nome-abrev, /* Assunto */
                             INPUT tt-param.texto-e-mail, /* Mensagem */
                             INPUT c-aux-e-mail, /* Arquivo Anexo */
                             INPUT YES). /* Mostra Erros */
                             
       ASSIGN i-e-mail-enviado = i-e-mail-enviado + 1.
       OS-DELETE VALUE(c-aux-e-mail). 
   END.
END.

IF tt-param.imp-param THEN DO:
   PAGE.
   display tt-param.ini-cod-estabel
           tt-param.fin-cod-estabel
           tt-param.ini-dt-emissao
           tt-param.fin-dt-emissao
           tt-param.ini-cod-emitente
           tt-param.fin-cod-emitente
           tt-param.ini-cod-rep
           tt-param.fin-cod-rep
           tt-param.enviar-e-mail
           tt-param.desc-imp-oque
           i-e-mail-enviado
           i-e-mail-nenviado
           with frame f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

