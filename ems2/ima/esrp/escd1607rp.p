/******************************************************************************
** PROGRAMA: ESCD1607RP.P                                                    **
** DATA    : AGOSTO/2017                                                     **
** AUTOR   : Toninho                                                         **
** OBJETIVO: Exporta‡Æo para o Serasa Experian Positivo                      **
******************************************************************************/
/* Programa de controle de versao e seguran»a do Datasul EMS */

{include/i-prgvrs.i ESCD1607 2.04.00.001}

/* Definicao de variaveis globais comuns a todos os programas do EMS */
{utp/ut-glob.i}

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)"
    field modelo-rtf       as char format "x(35)"
    field l-habilitaRtf    as LOG.

/* Parametros de entrada logica obrigatoria */
DEF TEMP-TABLE tt-raw-digita
    FIELD raw-digita                   AS RAW.

DEF INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEF INPUT PARAMETER TABLE FOR tt-raw-digita.       

DEF BUFFER empresa FOR mgcad.empresa.

DEF VAR h-acomp     AS HANDLE NO-UNDO.
DEF VAR c-base      AS CHAR.

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

{include/i-rpvar.i}   

FIND empresa WHERE
     empresa.ep-codigo = '1' NO-LOCK NO-ERROR.
IF NOT AVAIL empresa THEN
   RETURN "ADM-ERROR":U.

RUN esapi/busca-base.p (OUTPUT c-base).
IF c-base <> 'base-bkp' THEN 
   RETURN "ADM-ERROR":U.

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
{utp/ut-liter.i Imprimindo *}
RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

FOR EACH ext-emitente WHERE
         ext-emitente.log-integr = NO SHARE-LOCK.

    FIND emitente WHERE
         emitente.cod-emit = ext-emitente.cod-emit NO-LOCK NO-ERROR.
    
    //IF emitente.identific = 2 THEN NEXT.

    RUN pi-acompanhar IN h-acomp (INPUT ext-emitente.cod-emitente).

    RUN cdp/cd1608.p (INPUT emitente.cod-emitente,
                      INPUT emitente.cod-emitente,
                      INPUT emitente.identific,
                      INPUT YES,
                      INPUT 1,
                      INPUT 0,
                      INPUT "c:\temp\escd1607.txt",
                      INPUT "Arquivo":U,
                      INPUT "").  
                          
    FIND FIRST clien_finan WHERE
               clien_finan.cdn_cliente = emitente.cod-emitente NO-LOCK NO-ERROR.
    IF AVAIL clien_finan THEN
       ASSIGN ext-emitente.log-integr = YES.

    FIND FIRST fornec_finan WHERE
               fornec_finan.cdn_fornec = emitente.cod-emitente NO-LOCK NO-ERROR.
    IF AVAIL fornec_finan THEN
       ASSIGN ext-emitente.log-integr = YES.
    
END.

/* echamento do output do relat¢rio  */
{include/i-rpclo.i}
RUN pi-finalizar IN h-acomp.
RETURN "OK":U.
