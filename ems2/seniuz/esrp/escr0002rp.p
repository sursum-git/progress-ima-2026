/* Programa: ESCR004.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Contas a Receber
** Objetivo: Listar Titulos a Receber por Representante
** Autor...: Alterado Por Fabio Coelho Lanza - OUTUBRO/98
** Obs.....: Especifico da CIA.RENASCENCA INDUSTRIAL
**
** Conversao para EMS 2.04:
**   Programa: ESCR003.P  =>  ESCR0002RP.P
**   Autor...: Gilvando Souza Araujo
**   Data....: 15/02/2004
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESCR0002RP 2.04.00.000}

DEFINE TEMP-TABLE tt-param NO-UNDO
       FIELD destino         AS INTEGER
       FIELD arquivo         AS CHAR FORMAT "x(35)"
       FIELD usuario         AS CHAR FORMAT "x(12)"
       FIELD data-exec       AS DATE
       FIELD hora-exec       AS INTEGER
       FIELD ep-codigo       LIKE titulo.ep-codigo
       FIELD cod-estabel-ini LIKE titulo.cod-estabel
       FIELD cod-estabel-fin LIKE titulo.cod-estabel
       FIELD cod-rep-ini     LIKE titulo.cod-rep
       FIELD cod-rep-fin     LIKE titulo.cod-rep
       FIELD cod-port-ini    LIKE titulo.cod-port 
       FIELD cod-port-fin    LIKE titulo.cod-port
       FIELD cod-esp-ini     LIKE titulo.cod-esp
       FIELD cod-esp-fin     LIKE titulo.cod-esp
       FIELD dt-vencto-ini   AS DATE FORMAT 99/99/9999
       FIELD dt-vencto-fin   AS DATE FORMAT 99/99/9999
       FIELD dt-corte        AS DATE FORMAT 99/99/9999
       FIELD enviar-e-mail   AS LOG FORMAT "Sim/NÆo"
       FIELD subject-e-mail  AS CHAR FORMAT "x(40)"
       FIELD texto-e-mail    AS CHAR FORMAT "x(2000)"
       FIELD l-batch         AS LOG
       FIELD impr-param      AS LOGICAL
       FIELD apenas-resumo   AS LOGICAL.

define temp-table tt-raw-digita
       field raw-digita as raw.

DEFINE TEMP-TABLE tt-resumo
       FIELD cod-rep    LIKE repres.cod-rep
       FIELD nome-rep   LIKE repres.nome
       FIELD tot-valor  AS DEC FORMAT ">>>,>>>,>>9.99"
       FIELD qtd-titulo AS INT
       INDEX ch-rep cod-rep.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

/* Altera parƒmetros, quando execu‡Æo for em batch */
IF tt-param.l-batch THEN DO:
   assign tt-param.usuario          = "super"
          tt-param.destino          = 2 /* Arquivo */
          tt-param.arquivo          = "ESCR0002.LST"
          tt-param.data-exec        = TODAY
          tt-param.hora-exec        = TIME
          tt-param.ep-codigo        = 1
          tt-param.cod-estabel-ini = "1"
          tt-param.cod-estabel-fin = "999"
          tt-param.cod-rep-ini      = 2
          tt-param.cod-rep-fin      = 99999
          tt-param.cod-port-ini     = 1
          tt-param.cod-port-fin     = 99999
          tt-param.cod-esp-ini      = "CQ"
          tt-param.cod-esp-fin      = "DP"
          tt-param.dt-vencto-ini    = 01/01/0001
          tt-param.dt-vencto-fin    = TODAY
          tt-param.dt-corte         = TODAY
          tt-param.enviar-e-mail    = YES
          tt-param.subject-e-mail   = "T¡tulos a Receber - #PER-INI a #PER-FIN"
          tt-param.texto-e-mail     = "Segue anexo Rela‡Æo de T¡tulos a Receber, vencimento de #PER-INI a #PER-FIN." +
                                      CHR(13) + CHR(13) + "Atenciosamente," + CHR(13) + CHR(13) +
                                      "TEAR TEXTIL INDUSTRIA E COMERCIO LTDA." + CHR(13) + 
                                      "Departamento Financeiro."
          tt-param.apenas-resumo    = NO
          tt-param.impr-param       = NO. 
END.

DEFINE TEMP-TABLE tt-rep-email
       FIELD cod-rep LIKE repres.cod-rep
       INDEX ch-rep cod-rep.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* defini‡Æo de vari veis  */
DEF VAR h-acomp AS HANDLE NO-UNDO.

DEF BUFFER b-emitente FOR emitente.

DEF VAR c-endereco        LIKE emitente.endereco.
DEF VAR c-bairro          LIKE emitente.bairro.
DEF VAR c-cidade          LIKE emitente.cidade.
DEF VAR c-cep             LIKE emitente.cep.
DEF VAR c-estado          LIKE emitente.estado.
DEF VAR i-atraso          AS INT.
DEF VAR l-prim-rep        AS LOG.
DEF VAR l-prim-cli        AS LOG.
DEF VAR de-saldo          AS DEC FORMAT ">,>>>,>>>,>>9.99".
DEF VAR de-tot-rep        AS DEC.
DEF VAR de-tot-cli        AS DEC.
DEF VAR de-total          AS DEC.
DEF VAR i-cont-tit-cli    AS INT.
DEF VAR i-cont-tit-rep    AS INT.
DEF VAR i-cont-tit-ger    AS INT.
DEF VAR c-aux-e-mail      AS CHAR.
DEF VAR i-e-mail-enviado  AS INT.
DEF VAR i-e-mail-nenviado AS INT.
DEF VAR c-destinatar      AS CHAR.

form
    "*------------ Parƒmetros/Sele‡Æo -------------*" SKIP
    tt-param.ep-codigo       LABEL "Empresa........" AT 1
    tt-param.cod-estabel-ini LABEL "Estabelecimento" AT 1
    "a"  AT 30
    tt-param.cod-estabel-fin NO-LABELS
    tt-param.cod-rep-ini     LABEL "Representante.." AT 1
    "a"  AT 30
    tt-param.cod-rep-fin     NO-LABELS
    tt-param.cod-port-ini    label "Portador......." AT 1
    "a"  AT 30
    tt-param.cod-port-fin    no-labels
    tt-param.cod-esp-ini     LABEL "Especie........" AT 1
    "a"  AT 30
    tt-param.cod-esp-fin     NO-LABELS
    tt-param.dt-vencto-ini   label "Vencimento....." AT 1
    "a"  AT 30
    tt-param.dt-vencto-fin   NO-LABELS
    tt-param.dt-corte        LABEL "Data Corte....." AT 1
    tt-param.enviar-e-mail   LABEL "Enviar e-mail.." AT 1
    i-e-mail-enviado         LABEL "E-mail env....." AT 1
    i-e-mail-nenviado        LABEL "E-mail n/env..." AT 1
    with no-box side-labels width 132 stream-io frame f-param.

form header
    "Esp Nr.Docto Pa  Port Nr.Titulo no Banco   Dt.Emissao Dt.Vencto."
    "DtPagto               Saldo     Atraso"
    skip
    "--- -------- -- ----- -------------------- ---------- ----------"
    "------- ------------------- ----------"
    with no-labels no-attr-space no-box page-top width 132 STREAM-IO 1 DOWN frame f-cab-dados.

form
    titulo.cod-esp      FORMAT "x(3)"
    titulo.nr-docto     FORMAT "x(8)"
    titulo.parcela      
    titulo.cod-port     
    titulo.titulo-banco 
    titulo.dt-emissao   
    titulo.dt-vencimen  
    titulo.dt-pg-prev   
    de-saldo            
    i-atraso            
    WITH NO-BOX NO-LABEL 55 DOWN WIDTH 132 STREAM-IO FRAME f-detalhe.

form
    tt-resumo.cod-rep    LABEL "Repres"
    tt-resumo.nome-rep   LABEL "RazÆo Social"
    tt-resumo.tot-valor  LABEL "Valor"
    tt-resumo.qtd-titulo LABEL "Qtd.T¡tulo"
    WITH NO-BOX NO-LABEL 55 DOWN WIDTH 132 STREAM-IO FRAME f-resumo.

form
    "Representante: " 
    repres.cod-rep " - " 
    repres.nome
    WITH NO-LABELS 2 DOWN NO-BOX NO-ATTR-SPACE WIDTH 132 STREAM-IO FRAME f-repres.

FORM
    "Cliente:"
    emitente.cod-emitente
    emitente.nome-emit
    c-endereco 
    c-bairro skip
    c-cidade 
    c-estado
    c-cep
    "Fone:"
    emitente.telefone[1]
    emitente.telefone[2]
    "Fax:"
    emitente.telefax
    "Lim.Credito: "
    emitente.lim-credito format ">>,>>>,>>9" skip(1)
    with no-labels no-box no-attr-space width 132 STREAM-IO frame f-cliente.

/* include padrÆo para output de relat¢rios */
{include/i-rpout.i}

/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */
{include/i-rpcab.i}

/* bloco principal do programa */
find first param-global no-lock no-error.
find first empresa
     where empresa.ep-codigo = param-global.empresa-prin no-lock no-error. 

assign c-empresa = (if avail empresa then empresa.razao-social else "").

{utp/ut-liter.i ESTOQUE * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i T¡tulos_a_Receber_por_Representante * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
IF tt-param.apenas-resumo = NO THEN
   VIEW FRAME f-cab-dados.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

DEF STREAM email.
ASSIGN c-aux-e-mail = SESSION:TEMP-DIRECTORY + "aux-e-mail.txt".

for each titulo where titulo.ep-codigo   =  tt-param.ep-codigo
                  and titulo.cod-est     >= tt-param.cod-estabel-ini
                  and titulo.cod-est     <= tt-param.cod-estabel-fin
                  and titulo.cod-esp     >= tt-param.cod-esp-ini
                  and titulo.cod-esp     <= tt-param.cod-esp-fin
                  and titulo.cod-rep     >= tt-param.cod-rep-ini
                  and titulo.cod-rep     <= tt-param.cod-rep-fin
                  and titulo.cod-port    >= tt-param.cod-port-ini
                  and titulo.cod-port    <= tt-param.cod-port-fin
                  and titulo.dt-vencimen >= tt-param.dt-vencto-ini
                  and titulo.dt-vencimen <= tt-param.dt-vencto-fin
                no-lock,
    each emitente where emitente.cod-emit = titulo.cod-emit
                  no-lock
                  break by titulo.cod-rep
                        by emitente.nome-emit
                        by titulo.dt-vencimen:


    run pi-acompanhar in h-acomp (input "Rep: " + string(titulo.cod-rep) + 
                                        " Cli: " + string(titulo.cod-emitente) +
                                        " Venc: " + STRING(titulo.dt-vencimen) +
                                        " Doc: " + titulo.nr-docto).
                          
    /* {esinc/escr999.i "tt-param.dt-corte" "de-saldo"} */
    
    assign de-saldo = titulo.vl-saldo.
    FOR EACH mov-tit OF titulo NO-LOCK:
        IF mov-tit.dt-trans <= tt-param.dt-corte THEN NEXT.
        
        if  mov-tit.transacao = 14 then
            assign de-saldo = de-saldo - mov-tit.vl-original.
        if  mov-tit.transacao = 2 then
            assign de-saldo = de-saldo + mov-tit.vl-baixa.
        if  mov-tit.transacao = 3 then
            assign de-saldo = de-saldo + mov-tit.vl-baixa.
        if  mov-tit.transacao = 13
        and mov-tit.lancamento = 1 then /* Deb */
            assign de-saldo = de-saldo + mov-tit.vl-baixa.
        if  mov-tit.transacao = 13
        and mov-tit.lancamento = 2 THEN /* Cred */
            assign de-saldo = de-saldo - mov-tit.vl-baixa.
    END.

    if first-of(titulo.cod-rep) then do:
       assign l-prim-rep = yes.
    end.
    if first-of(emitente.nome-emit) then do:
       assign l-prim-cli = yes.
    end.
    if de-saldo > 0 then do:
       if l-prim-rep = yes then do:
          find repres where repres.cod-rep = titulo.cod-rep
                      no-lock no-error.
          IF AVAIL repres THEN DO:
             IF tt-param.apenas-resumo = NO THEN DO:
                DISPLAY repres.cod-rep 
                        repres.nome
                        with frame f-repres.
                DOWN(2) with frame f-repres.
             END.
           
             IF tt-param.enviar-e-mail THEN DO:
                OUTPUT STREAM email TO value(c-aux-e-mail) CONVERT SOURCE "ibm850".
                PUT STREAM email
                    c-empresa " - "
                    " TITULOS A RECEBER - VENCIMENTO: "
                    tt-param.dt-vencto-ini
                    " a "
                    tt-param.dt-vencto-fin
                    " - EXTRATO GERADO EM: " STRING(TODAY,"99/99/9999") FORMAT "x(10)" " " STRING(TIME,"HH:MM")
                    SKIP(1)
                    "Representante: " 
                    repres.cod-rep " - " 
                    repres.nome
                    " - e-mail: " repres.e-mail
                    SKIP(1)
                    "Esp Nr.Docto Pa  Port Dt.Emissao Dt.Vencto." AT  1
                    "              Saldo     Atraso"              AT 45
                    "--- -------- -- ----- ---------- ----------" AT  1
                    "------------------- ----------"              AT 45
                    SKIP.

                /* Atualiza temp-table com Representantes aos quais foram enviados e-mails */
                FIND tt-rep-email WHERE tt-rep-email.cod-rep = titulo.cod-rep NO-LOCK NO-ERROR.
                IF NOT AVAIL tt-rep-email THEN DO:
                   CREATE tt-rep-email.
                   ASSIGN tt-rep-email.cod-rep = titulo.cod-rep.
                END.
             END.
          END.
          assign l-prim-rep = no.
       end.
       if l-prim-cli = yes then DO:
          assign c-endereco = emitente.endereco
                 l-prim-cli = no
                 c-bairro   = emitente.bairro
                 c-cidade   = emitente.cidade
                 c-estado   = emitente.estado
                 c-cep      = emitente.cep.
          if emitente.end-cobranca <> 0 and 
             emitente.end-cobranca <> emitente.cod-emit then do:
             find first b-emitente where b-emitente.cod-emit = emitente.end-cobranca
                                   no-lock no-error.
                                    
             assign c-endereco = b-emitente.endereco
                    c-bairro   = b-emitente.bairro
                    c-cidade   = b-emitente.cidade
                    c-estado   = b-emitente.estado
                    c-cep      = b-emitente.cep.
          end.
          ELSE DO:
             if emitente.endereco-cob <> "" then
                assign c-endereco = emitente.endereco-cob
                       c-bairro   = emitente.bairro-cob
                       c-cidade   = emitente.cidade-cob
                       c-estado   = emitente.estado-cob
                       c-cep      = emitente.cep-cob.
          END.

          IF tt-param.apenas-resumo = NO THEN DO:
             DISPLAY emitente.cod-emitente
                     emitente.nome-emit
                     c-endereco
                     c-bairro
                     c-cidade
                     c-estado
                     c-cep
                     emitente.telefone[1]
                     emitente.telefone[2]
                     emitente.telefax
                     emitente.lim-credito
                     WITH FRAME f-cliente.
             DOWN WITH FRAME f-cliente.
          END.
        
          IF tt-param.enviar-e-mail THEN DO:
             PUT STREAM email UNFORMAT
                 "Cliente: "
                 STRING(emitente.cod-emitente,"999999")
                 " " TRIM(emitente.nome-emit)
                 " " TRIM(c-endereco)
                 " " TRIM(c-bairro)
                 " " TRIM(c-cidade)
                 " " TRIM(c-estado)
                 SKIP(1).
          END.
       END.
       
       ASSIGN de-total   = de-total   + de-saldo
              de-tot-rep = de-tot-rep + de-saldo
              de-tot-cli = de-tot-cli + de-saldo
              i-atraso   = tt-param.dt-corte - titulo.dt-vencimen.
              
       IF tt-param.apenas-resumo = NO THEN DO:
          display titulo.cod-esp       
                  titulo.nr-docto      
                  titulo.parcela        
                  titulo.cod-port      
                  titulo.titulo-banco  
                  titulo.dt-emissao    
                  titulo.dt-vencimen   
                  titulo.dt-pg-prev    
                  i-atraso           when i-atraso > 0
                  de-saldo with frame f-detalhe.
          down with frame f-detalhe.
       END.

       IF tt-param.enviar-e-mail THEN DO:
          PUT STREAM email
              titulo.cod-esp               AT  1                     
              SUBSTR(titulo.nr-docto,1,8)  AT  5
              titulo.parcela               AT 14               
              titulo.cod-port              AT 17
              titulo.dt-emissao            AT 23
              titulo.dt-vencimen           AT 34
              de-saldo                     AT 48
              i-atraso                     AT 65
              SKIP.
       END.

       ASSIGN i-cont-tit-cli = i-cont-tit-cli + 1
              i-cont-tit-rep = i-cont-tit-rep + 1
              i-cont-tit-ger = i-cont-tit-ger + 1.
    end.
    
    if last-of(emitente.nome-emit) and 
       l-prim-cli = no then do:
       IF tt-param.apenas-resumo = NO THEN DO:
          display "Total Cli:" @ titulo.dt-vencimen
                   de-tot-cli  @ de-saldo
                  "Tit: " + TRIM(STRING(i-cont-tit-cli))
                               @ i-atraso
                   with frame f-detalhe.
          down(2) with frame f-detalhe.
       END.

       IF tt-param.enviar-e-mail THEN DO:
          PUT STREAM email
              "Total Cli:" AT  1
              de-tot-cli   AT 54
              "Tit: " + TRIM(STRING(i-cont-tit-cli)) AT 65
              SKIP(1).
       END.

       assign de-tot-cli     = 0
              i-cont-tit-cli = 0.
    end.

    if  last-of(titulo.cod-rep)
    and l-prim-rep = no then do:
        IF tt-param.apenas-resumo = NO THEN DO:
           display "Total Rep:" @ titulo.dt-vencimen
                   de-tot-rep  @ de-saldo
                   "Tit: " + TRIM(STRING(i-cont-tit-rep))
                               @ i-atraso
                   with frame f-detalhe.
           down with frame f-detalhe.
        END.
        
        IF tt-param.enviar-e-mail THEN DO:
           FIND repres WHERE repres.cod-rep = titulo.cod-rep NO-LOCK NO-ERROR.
           IF repres.e-mail <> "" THEN DO:
              PUT STREAM email
                  "Total Rep" AT  1         
                  de-tot-rep  AT 54
                  "Tit: " + TRIM(STRING(i-cont-tit-rep)) AT 65.
              OUTPUT STREAM email CLOSE.
     
              ASSIGN tt-param.subject-e-mail = REPLACE(tt-param.subject-e-mail,"#PER-INI",STRING(tt-param.dt-vencto-ini,"99/99/9999"))
                     tt-param.subject-e-mail = REPLACE(tt-param.subject-e-mail,"#PER-FIN",STRING(tt-param.dt-vencto-fin,"99/99/9999"))
                     tt-param.texto-e-mail   = REPLACE(tt-param.texto-e-mail,"#PER-INI",STRING(tt-param.dt-vencto-ini,"99/99/9999"))
                     tt-param.texto-e-mail   = REPLACE(tt-param.texto-e-mail,"#PER-FIN",STRING(tt-param.dt-vencto-fin,"99/99/9999"))
                     c-destinatar = repres.e-mail + "," + "wellington.vale@teartextil.com.br,teartextil@teartextil.com.br".
              RUN esapi/esapi002.p (INPUT "cobranca@teartextil.com.br", /* e-mail remetente */
                                    INPUT c-destinatar, /* e-mail destinat rio */
                                    INPUT tt-param.subject-e-mail, /* Assunto */
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

        CREATE tt-resumo.
        FIND repres WHERE repres.cod-rep = titulo.cod-rep NO-LOCK.
        ASSIGN tt-resumo.cod-rep    = repres.cod-rep
               tt-resumo.nome-rep   = repres.nome
               tt-resumo.tot-valor  = de-tot-rep
               tt-resumo.qtd-titulo = i-cont-tit-rep.
        
        assign de-tot-rep     = 0
               i-cont-tit-rep = 0.
        page.

    end.
end.
 
IF tt-param.apenas-resumo = NO THEN DO:
   display "Tot Geral" @ titulo.dt-vencimen
           de-total    @ de-saldo
           "Tit: " + TRIM(STRING(i-cont-tit-ger))
                       @ i-atraso
           with frame f-detalhe.
   down with frame f-detalhe.
END.

IF tt-param.apenas-resumo = NO THEN
   PAGE.
FOR EACH tt-resumo:
    ACCUMULATE tt-resumo.tot-valor(TOTAL).
    ACCUMULATE tt-resumo.qtd-titulo(TOTAL).
    DISPLAY tt-resumo.cod-rep
            tt-resumo.nome-rep
            tt-resumo.tot-valor
            tt-resumo.qtd-titulo
            WITH FRAME f-resumo.
    DOWN WITH FRAME f-resumo.
END.
DISPLAY "Total"                            @ tt-resumo.nome-rep 
        (ACCUM TOTAL tt-resumo.tot-valor)  @ tt-resumo.tot-valor
        (ACCUM TOTAL tt-resumo.qtd-titulo) @ tt-resumo.qtd-titulo
        WITH FRAME f-resumo.

IF tt-param.impr-param THEN DO:
   PAGE.
   display tt-param.ep-codigo      
           tt-param.cod-estabel-ini
           tt-param.cod-estabel-fin
           tt-param.cod-rep-ini
           tt-param.cod-rep-fin
           tt-param.cod-port-ini   
           tt-param.cod-port-fin   
           tt-param.cod-esp-ini    
           tt-param.cod-esp-fin    
           tt-param.dt-vencto-ini  
           tt-param.dt-vencto-fin  
           tt-param.dt-corte 
           tt-param.enviar-e-mail
           i-e-mail-enviado      
           i-e-mail-nenviado     
           with frame f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

