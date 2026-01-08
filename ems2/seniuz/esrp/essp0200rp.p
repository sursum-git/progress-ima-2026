/* Programa: ESSP0200.W
** Modulo..: Controle de Expedi‡Æo
** Objetivo: Gerar relat¢rio de faturamento de Cortes de Amostras e 
**           enviar e-mail para a Janete.
** Autor...: Gilvando de Souza Araujo - Fevereiro/2011
** Obs.....: Especifico da TEAR TEXTIL IND.COM.LTDA.
**
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESSP0200RP 2.04.00.000}

DEFINE TEMP-TABLE tt-param   NO-UNDO
       FIELD destino             AS INTEGER
       FIELD arquivo             AS CHAR FORMAT "x(35)"
       FIELD usuario             AS CHAR FORMAT "x(12)"
       FIELD data-exec           AS DATE
       FIELD hora-exec           AS INTEGER
       FIELD classifica          AS INTEGER
       FIELD desc-classifica     AS CHAR FORMAT "x(40)"
       FIELD cod-estabel-ini     LIKE nota-fiscal.cod-estabel
       FIELD cod-estabel-fin     LIKE nota-fiscal.cod-estabel
       FIELD serie-ini           LIKE nota-fiscal.serie
       FIELD serie-fin           LIKE nota-fiscal.serie
       FIELD dt-emis-nota-ini    LIKE nota-fiscal.dt-emis-nota 
       FIELD dt-emis-nota-fin    LIKE nota-fiscal.dt-emis-nota
       FIELD cod-rep-ini         LIKE nota-fiscal.cod-rep
       FIELD cod-rep-fin         LIKE nota-fiscal.cod-rep
       FIELD corte-faturado      AS INTEGER
       FIELD desc-corte-faturado AS CHAR FORMAT "x(5)"
       FIELD enviar-e-mail       AS LOG FORMAT "Sim/NÆo"
       FIELD subject-e-mail      AS CHAR FORMAT "x(40)"
       FIELD texto-e-mail        AS CHAR FORMAT "x(2000)"
       FIELD l-batch             AS LOG
       FIELD imp-param           AS LOG.

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
          tt-param.arquivo          = "ESSP0200.LST"
          tt-param.data-exec        = TODAY
          tt-param.hora-exec        = TIME
          tt-param.classifica       = 1
          tt-param.desc-classifica  = "Padrao"
          tt-param.cod-estabel-ini  = "1"
          tt-param.cod-estabel-fin  = "2"
          tt-param.dt-emis-nota-ini = TODAY - 7
          tt-param.dt-emis-nota-fin = TODAY
          tt-param.cod-rep-ini      = 0
          tt-param.cod-rep-fin      = 99999
          tt-param.enviar-e-mail    = YES
          tt-param.subject-e-mail   = "Faturamento de Cortes de Amostra - #PER-INI a #PER-FIN"
          tt-param.texto-e-mail     = "Segue anexo Relat¢rio de Faturamento de Cortes de Amostra, #PER-INI a #PER-FIN." + 
                                       CHR(13) + CHR(13) + "Atenciosamente," + CHR(13) + CHR(13) +
                                      "TEAR TEXTIL INDUSTRIA E COMERCIO LTDA." + CHR(13) + 
                                      "Setor de Inform tica."
          tt-param.imp-param        = NO.
END.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* include padrÆo para impressÆo de campos editores em relat¢rio  */
{include/tt-edit.i}

/* defini‡Æo de vari veis  */
DEF VAR h-acomp        AS HANDLE NO-UNDO.
DEF VAR c-aux-e-mail   AS CHAR.
DEF VAR c-destinatar   AS CHAR.
DEF VAR c-observ       AS CHAR FORMAT "x(2000)".

form 
    "*--------- Parƒmetros/Sele‡Æo ---------*" SKIP
    tt-param.cod-estabel-ini     LABEL  "Estabelec...."
    "A"  AT 27
    tt-param.cod-estabel-fin     NO-LABEL SKIP
    tt-param.serie-ini           LABEL  "S‚rie........"
    "A"  AT 27                
    tt-param.serie-fin           NO-LABELS SKIP
    tt-param.dt-emis-nota-ini    LABEL  "Data EmissÆo."
    "A"  AT 27                
    tt-param.dt-emis-nota-fin    NO-LABELS SKIP
    tt-param.cod-rep-ini         LABEL "Representante" 
    "A"  AT 27                                       
    tt-param.cod-rep-fin         NO-LABELS SKIP   
    tt-param.desc-corte-faturado LABEL "Corte Faturdo"
    WITH FRAME f-param SIDE-LABELS NO-BOX WIDTH 132 STREAM-IO.

FORM 
    nota-fiscal.no-ab-reppri    LABEL "Repres"          
    nota-fiscal.nome-ab-cli     LABEL "Cliente"         
    nota-fiscal.nr-nota-fis     LABEL "Nota"        FORMAT "x(7)"
    nota-fiscal.dt-emis-nota    LABEL "Dt-Emiss"       
    it-nota-fisc.nr-seq-fat     LABEL "Seq"         FORMAT ">>9"   
    it-nota-fisc.it-codigo      LABEL "Item"        FORMAT "x(6)"
    ITEM.desc-item              LABEL "Descri‡Æo"   FORMAT "x(20)"       
    it-nota-fisc.qt-faturada[1] LABEL "Quantid"     FORMAT ">>,>>9.99"
    it-nota-fisc.vl-preuni      LABEL "Pre‡o"       FORMAT ">,>>9.99"
    nota-fiscal.emite-dup       LABEL "Fat"     
    c-observ                    LABEL "Observa‡äes" FORMAT "x(31)"
    WITH NO-BOX NO-LABEL 55 DOWN WIDTH 132 STREAM-IO FRAME f-detalhe.

/* include padrÆo para output de relat¢rios */
{include/i-rpout.i}

/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */
{include/i-rpcab.i}

/* bloco principal do programa */
find first param-global no-lock no-error.
find first empresa
     where empresa.ep-codigo = param-global.empresa-prin no-lock no-error. 

assign c-empresa = (if avail empresa then empresa.razao-social else "").

{utp/ut-liter.i ESPECÖFICOS * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Faturamento_de_Cortes_de_Amostras * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

IF tt-param.enviar-e-mail THEN DO:
   DEF STREAM email.
   ASSIGN c-aux-e-mail = SESSION:TEMP-DIRECTORY + "aux-e-mail.txt".
   
   OUTPUT STREAM email TO value(c-aux-e-mail) CONVERT SOURCE "ibm850".
   PUT STREAM email
       c-empresa " - "
       " FATURAMENTO DE CORTES DE AMOSTRAS - PERIODO: " tt-param.dt-emis-nota-ini FORMAT "99/99/9999" " a "  
       tt-param.dt-emis-nota-fin FORMAT "99/99/9999"
       " - EMITIDO EM: " TODAY " " STRING(TIME,"HH:MM")
       SKIP(1)
       "Repres        Cliente      Nota    Dt-Emiss   Seq Item   Descri‡Æo           " AT  1
       "   Quantid    Pre‡o Fat Observa‡äes                    "                       AT 78
       "------------- ------------ ------- ---------- --- ------ --------------------" AT  1
       " --------- -------- --- -------------------------------"                       AT 78
       SKIP.
END.

FOR EACH nota-fiscal WHERE nota-fiscal.cod-estabel  >= tt-param.cod-estabel-ini
                       AND nota-fiscal.cod-estabel  <= tt-param.cod-estabel-fin
                       AND nota-fiscal.serie        >= tt-param.serie-ini
                       AND nota-fiscal.serie        <= tt-param.serie-fin
                       AND nota-fiscal.dt-emis-nota >= tt-param.dt-emis-nota-ini
                       AND nota-fiscal.dt-emis-nota <= tt-param.dt-emis-nota-fin
                       AND nota-fiscal.cod-rep      >= tt-param.cod-rep-ini
                       AND nota-fiscal.cod-rep      <= tt-param.cod-rep-fin
                       AND nota-fiscal.dt-cancela   =  ?
                       AND ((nota-fiscal.emite-dup = YES AND tt-param.corte-faturado = 1) OR
                            (nota-fiscal.emite-dup = NO  AND tt-param.corte-faturado = 2) OR
                                                            (tt-param.corte-faturado = 3))
                     NO-LOCK,
    EACH it-nota-fisc OF nota-fiscal NO-LOCK,
    FIRST ped-item-res WHERE ped-item-res.cod-estabel = nota-fiscal.cod-estabel
                         AND ped-item-res.serie       = nota-fiscal.serie
                         AND ped-item-res.nr-nota-fis = INT(nota-fiscal.nr-nota-fis)
                         AND ped-item-res.faturado    = YES 
                         AND ped-item-res.lote BEGINS "CA"
                       NO-LOCK
    BREAK BY nota-fiscal.no-ab-reppri
          BY nota-fiscal.nome-ab-cli
          BY it-nota-fisc.nr-nota-fis
          BY it-nota-fisc.nr-seq-fat:

    run pi-acompanhar in h-acomp (input "Repres/Nota: " + string(nota-fiscal.cod-rep) + " " + nota-fiscal.nr-nota-fis).
    
    ACCUMULATE it-nota-fisc.qt-faturada[1] (TOTAL).
    ACCUMULATE it-nota-fisc.qt-faturada[1] (TOTAL BY nota-fiscal.no-ab-reppri).
    ACCUMULATE it-nota-fisc.qt-faturada[1] (TOTAL BY it-nota-fisc.nr-seq-fat).

    FIND ITEM WHERE ITEM.it-codigo = it-nota-fisc.it-codigo NO-LOCK.
    FIND ped-venda WHERE ped-venda.nome-abrev = nota-fiscal.nome-ab-cli
                     AND ped-venda.nr-pedcli  = nota-fiscal.nr-pedcli
                   NO-LOCK.
    ASSIGN c-observ = replace(replace(ped-venda.observacoes,chr(13)," "),chr(10)," ").
    DISPLAY nota-fiscal.no-ab-reppri  WHEN FIRST-OF(nota-fiscal.no-ab-reppri)
            nota-fiscal.nome-ab-cli   WHEN FIRST-OF(nota-fiscal.nome-ab-cli)
            nota-fiscal.nr-nota-fis   WHEN FIRST-OF(it-nota-fisc.nr-nota-fis)
            nota-fiscal.dt-emis-nota  WHEN FIRST-OF(it-nota-fisc.nr-nota-fis)
            it-nota-fisc.nr-seq-fat 
            it-nota-fisc.it-codigo
            ITEM.desc-item
            it-nota-fisc.qt-faturada[1]
            it-nota-fisc.vl-preuni 
            nota-fiscal.emite-dup 
            c-observ
            WITH FRAME f-detalhe.
    
    DOWN WITH FRAME f-detalhe.
    
    IF tt-param.enviar-e-mail THEN DO:
       PUT STREAM email
                  nota-fiscal.no-ab-reppri    AT   1 
                  nota-fiscal.nome-ab-cli     AT  15 
                  nota-fiscal.nr-nota-fis     AT  28  FORMAT "x(7)"      
                  nota-fiscal.dt-emis-nota    AT  36
                  it-nota-fisc.nr-seq-fat     AT  47  FORMAT ">>9"       
                  it-nota-fisc.it-codigo      AT  51  FORMAT "x(6)"      
                  ITEM.desc-item              AT  58  FORMAT "x(20)"     
                  it-nota-fisc.qt-faturada[1] AT  79  FORMAT ">>,>>9.99" 
                  it-nota-fisc.vl-preuni      AT  89  FORMAT ">,>>9.99"  
                  nota-fiscal.emite-dup       AT  98  
                  c-observ                    AT 102  FORMAT "x(300)"
                  SKIP.
    END.
    
    IF LAST-OF(nota-fiscal.no-ab-reppri) THEN DO:
       DISPLAY "Total Repres." @ nota-fiscal.no-ab-reppri
               (ACCUM TOTAL BY nota-fiscal.no-ab-reppri it-nota-fisc.qt-faturada[1]) @ it-nota-fisc.qt-faturada[1]
               WITH FRAME f-detalhe.
       DOWN(2) WITH FRAME f-detalhe.

       IF tt-param.enviar-e-mail THEN DO:
          PUT STREAM email
                     "Total Repres." AT 28                                                     
                     (ACCUM TOTAL BY nota-fiscal.no-ab-reppri it-nota-fisc.qt-faturada[1]) AT 78 FORMAT ">>>,>>9.99"
                     SKIP(1).
       END.
    END.
END.

DISPLAY "Total GERAL"                             @ nota-fiscal.no-ab-reppri
        (ACCUM TOTAL it-nota-fisc.qt-faturada[1]) @ it-nota-fisc.qt-faturada[1]
        WITH FRAME f-detalhe.
DOWN WITH FRAME f-detalhe.

IF tt-param.enviar-e-mail THEN DO:
   PUT STREAM email
       "Total GERAL" AT 28         
       (ACCUM TOTAL it-nota-fisc.qt-faturada[1]) AT 78 FORMAT ">>>,>>9.99".

   OUTPUT STREAM email CLOSE.
   
   ASSIGN tt-param.subject-e-mail = REPLACE(tt-param.subject-e-mail,"#PER-INI",STRING(tt-param.dt-emis-nota-ini,"99/99/9999"))
          tt-param.subject-e-mail = REPLACE(tt-param.subject-e-mail,"#PER-FIN",STRING(tt-param.dt-emis-nota-fin,"99/99/9999"))
          tt-param.texto-e-mail   = REPLACE(tt-param.texto-e-mail,"#PER-INI",STRING(tt-param.dt-emis-nota-ini,"99/99/9999"))
          tt-param.texto-e-mail   = REPLACE(tt-param.texto-e-mail,"#PER-FIN",STRING(tt-param.dt-emis-nota-fin,"99/99/9999"))
          c-destinatar            = "janete.oliveira@teartextil.com.br,teartextil@teartextil.com.br".
   RUN esapi/esapi002.p (INPUT "teartextil@teartextil.com.br", /* e-mail remetente */
                         INPUT c-destinatar, /* e-mail destinat rio */
                         INPUT tt-param.subject-e-mail, /* Assunto */
                         INPUT tt-param.texto-e-mail, /* Mensagem */
                         INPUT c-aux-e-mail, /* Arquivo Anexo */
                         INPUT YES). /* Mostra Erros */
   OS-DELETE VALUE(c-aux-e-mail). 
END.

IF tt-param.imp-param THEN DO:
   PAGE.
   DISPLAY tt-param.cod-estabel-ini
           tt-param.cod-estabel-fin
           tt-param.serie-ini
           tt-param.serie-fin
           tt-param.dt-emis-nota-ini
           tt-param.dt-emis-nota-fin
           tt-param.cod-rep-ini
           tt-param.cod-rep-fin
           tt-param.desc-corte-faturado
           WITH FRAME f-param.
END. 

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
RUN pi-finalizar IN h-acomp.
RETURN "OK":U.


