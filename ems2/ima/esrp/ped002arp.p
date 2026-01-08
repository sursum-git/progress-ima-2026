/* include de controle de versÆo */
{include/i-prgvrs.i XX9999RP 1.00.00.000}

DEF BUFFER empresa FOR mgadm.empresa.

/* Definicao de variaveis globais comuns a todos os programas do EMS */
{utp/ut-glob.i}

/* defini‡Æo das temp-tables para recebimento de parƒmetros */
DEFINE TEMP-TABLE tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)"
    field modelo-rtf       as char format "x(35)"
    field l-habilitaRtf    as LOG
    field i-cod-emitente-ini  like devol-cli.cod-emitente
    field i-cod-emitente-fim  like devol-cli.cod-emitente
    field c-nr-nota-fis-ini   like devol-cli.nr-nota-fis
    field c-nr-nota-fis-fim   like devol-cli.nr-nota-fis
    field da-dt-emis-nota-ini like nota-fiscal.dt-emis-nota
    field da-dt-emis-nota-fim like nota-fiscal.dt-emis-nota
    field da-dt-devol-ini     like devol-cli.dt-devol
    field da-dt-devol-fim     like devol-cli.dt-devol
    field i-cod-rep-ini       like nota-fiscal.cod-rep
    field i-cod-rep-fim       like nota-fiscal.cod-rep.

DEF TEMP-TABLE tt-raw-digita
   	FIELD raw-digita	AS RAW.

/* recebimento de parƒmetros */
DEF INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEF INPUT PARAMETER TABLE FOR tt-raw-digita.

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* defini‡Æo de vari veis  */
DEF VAR h-acomp AS HANDLE NO-UNDO.
DEF VAR de-tot-devol AS DEC.

/* defini‡Æo de frames do relat¢rio */
FORM
    devol-cli.it-codigo
    item.desc-item
    devol-cli.cod-refer
    devol-cli.qt-devolvida
    devol-cli.vl-devol
    WITH 55 DOWN WIDTH 132 STREAM-IO FRAME f-detalhe.

/* include padrÆo para output de relat¢rios */
{include/i-rpout.i &STREAM="stream str-rp"}

/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */
{include/i-rpcab.i &STREAM="str-rp"}

/* LOCALIZACAO DO NOME DA EMPRESA PARA COMPOR O CABECALHO PADRAO */
FIND empresa WHERE 
     empresa.ep-codigo = i-ep-codigo-usuario NO-LOCK NO-ERROR.
IF NOT AVAIL empresa THEN
   RETURN "ADM-ERROR":U.

/* VALORIZACAO DAS OUTRAS VARIAVEIS QUE COMPOEM O CABECALHO PADRAO */
ASSIGN c-programa     = "PED002ARP":U
       c-versao       = "2.04":U
       c-revisao      = ".00.001":U
       c-empresa      = empresa.razao-social
       c-sistema      = "EMS":U.

ASSIGN c-titulo-relat = "DEVOLU€ÇO DE CLIENTES":U. 

VIEW STREAM str-rp FRAME f-cabec.
VIEW STREAM str-rp FRAME f-rodape.

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
{utp/ut-liter.i Imprimindo *}
RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

for each devol-cli no-lock
         where devol-cli.cod-emitente >= i-cod-emitente-ini and 
               devol-cli.cod-emitente <= i-cod-emitente-fim and
               devol-cli.dt-devol >= da-dt-devol-ini and 
               devol-cli.dt-devol <= da-dt-devol-fim and
               devol-cli.nr-nota-fis >= c-nr-nota-fis-ini and 
               devol-cli.nr-nota-fis <= c-nr-nota-fis-fim,
    each nota-fiscal no-lock
         where nota-fiscal.cod-estabel = devol-cli.cod-estabel and
               nota-fiscal.serie = devol-cli.serie and
               nota-fiscal.nr-nota-fis = devol-cli.nr-nota-fis and
               nota-fiscal.cod-rep >= i-cod-rep-ini and 
               nota-fiscal.cod-rep <= i-cod-rep-fim and
               nota-fiscal.dt-emis-nota >= da-dt-emis-nota-ini and 
               nota-fiscal.dt-emis-nota <= da-dt-emis-nota-fim,
    each item no-lock
         where item.it-codigo = devol-cli.it-codigo
    break by devol-cli.cod-emitente
          by devol-cli.nr-nota-fis
          by devol-cli.it-codigo:

    run pi-acompanhar in h-acomp(input devol-cli.dt-devol).

    if  first-of(devol-cli.nr-nota-fis) THEN
        assign de-tot-devol = 0.

    /***  CàDIGO PARA SAÖDA EM 132 COLUNAS ***/

    if  first-of(devol-cli.cod-emitente) then do:
        display stream str-rp " " with stream-io no-box frame f-branco.
        display stream str-rp
            devol-cli.cod-emitente
            devol-cli.nome-ab-emi
            nota-fiscal.cd-vendedor
            nota-fiscal.no-ab-reppri
            with frame f-cliente.
    end.

    if  first-of(devol-cli.nr-nota-fis) then do:
        display stream str-rp " " with stream-io no-box frame f-branco.
        display stream str-rp 
             devol-cli.nr-nota-fis when first-of(devol-cli.nr-nota-fis)
             nota-fiscal.dt-emis-nota
             devol-cli.dt-devol
             devol-cli.nro-docto
             with stream-io frame f-notas.
        display stream str-rp " " with stream-io no-box frame f-branco.
    end.

    display stream str-rp 
        devol-cli.it-codigo
        item.desc-item
        devol-cli.cod-refer
        devol-cli.qt-devolvida
        devol-cli.vl-devol
        with frame f-detalhe.

    down stream str-rp with frame f-detalhe.

    assign de-tot-devol = de-tot-devol + devol-cli.vl-devol.

    if  last-of(devol-cli.nr-nota-fis) then do:
        display stream str-rp "-----------------" @  devol-cli.vl-devol
                with frame f-detalhe.
        down stream str-rp with frame f-detalhe.
        display stream str-rp 
                "Total.......:"  @ devol-cli.qt-devolvida
                de-tot-devol     @ devol-cli.vl-devol
                with frame f-detalhe.

        DOWN 1 stream str-rp with frame f-detalhe.
    END.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i &STREAM="stream str-rp"}
RUN pi-finalizar in h-acomp.
RETURN "OK":U.
