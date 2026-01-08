/* Programa: ESPD0020RP.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Pedidos
** Objetivo: Listar Pedidos de Venda por Situa‡Æo de Cr‚dito
** Autor...: Gilvando Souza Araujo - Maio/2005
** Obs.....: Especifico da TEAR TÒXTIL INDéSTRIA E COMRCIO LTDA.
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESPD0020RP 2.04.00.000}

define temp-table tt-param no-undo
       field destino          as integer
       field arquivo          as char format "x(35)"
       field usuario          as char format "x(12)"
       field data-exec        as date
       field hora-exec        as integer
       field classifica       as integer
       FIELD cod-estabel      LIKE ped-venda.cod-estabel
       FIELD dt-implant-ini   LIKE ped-venda.dt-implant
       FIELD dt-implant-fin   LIKE ped-venda.dt-implant
       FIELD impr-param       AS   LOGICAL.

define temp-table tt-raw-digita
       field raw-digita as raw.

DEFINE TEMP-TABLE tt-work
       FIELD cod-sit-aval  LIKE ped-venda.cod-sit-aval
       FIELD situacao      AS CHAR FORMAT "x(12)"
       FIELD tp-pedido     LIKE ped-venda.tp-pedido
       FIELD vl-tot-ped    LIKE ped-venda.vl-tot-ped
       FIELD cont-pedido   AS INTEGER FORMAT ">>>>9"
       INDEX ch-work cod-sit-aval tp-pedido.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* defini‡Æo de vari veis  */
def var h-acomp as handle no-undo.

DEF VAR c-situacao AS CHAR FORMAT "x(12)".

form
    "*--------- Parƒmetros/Sele‡Æo ----------*" SKIP
    tt-param.cod-estabel      LABEL "Estabelecimento." SKIP
    tt-param.dt-implant-ini   LABEL "Data Implanta‡Æo" 
    "a" AT 30
    tt-param.dt-implant-fin   NO-LABELS SKIP
    with no-box side-labels width 132 STREAM-IO frame f-param.

form
    ped-venda.cod-emitente   LABEL "Cliente"     FORMAT "9999999"
    ped-venda.nome-abrev     LABEL "Nome-Abrev"
    ped-venda.no-ab-reppri   LABEL "Repres"
    ped-venda.nr-pedcli      LABEL "Pedido"      FORMAT "x(6)"
    ped-venda.vl-tot-ped     LABEL "Vlr.Total"
    c-situacao               LABEL "Situa‡Æo"
    ped-venda.dt-implant     LABEL "Dt.Impl."    FORMAT "99/99/99"
    ped-venda.dt-apr-cred    LABEL "Dt.Aprov"    FORMAT "99/99/99"
    ped-venda.quem-aprovou   LABEL "Respons vel"
    ped-venda.desc-bloq-cr   LABEL "Motivo"      FORMAT "x(30)"
    WITH NO-LABEL NO-BOX 55 DOWN WIDTH 132 WITH STREAM-IO FRAME f-detalhe.

FORM
    tt-work.cod-sit-aval LABEL "Cod."
    tt-work.situacao     LABEL "Situa‡Æo"
    tt-work.tp-pedido    LABEL "TpPedido"
    tt-work.vl-tot-ped   LABEL "Vlr.Total"
    tt-work.cont-pedido  LABEL "Pedidos"
    WITH NO-LABEL NO-BOX 55 DOWN WIDTH 132 WITH STREAM-IO FRAME f-resumo.

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
{utp/ut-liter.i Pedidos_de_Vendas_por_Situa‡Æo_de_Cr‚dito * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

for each ped-venda where ped-venda.dt-implant >= tt-param.dt-implant-ini
                    and ped-venda.dt-implant  <= tt-param.dt-implant-fin
                   no-lock:       

    run pi-acompanhar in h-acomp (input "Pedido: " + ped-venda.nr-pedcli).
   
    {esinc/i-dsrb.i ped-venda.cod-sit-aval ped-venda.cod-sit-aval c-situacao}
   
    DISPLAY ped-venda.cod-emitente
            ped-venda.nome-abrev
            ped-venda.no-ab-reppri
            ped-venda.nr-pedcli
            ped-venda.vl-tot-ped
            c-situacao
            ped-venda.dt-implant
            ped-venda.dt-apr-cred
            ped-venda.desc-bloq-cr WHEN ped-venda.cod-sit-aval = 4
                @ ped-venda.desc-bloq-cr
            ped-venda.desc-forc-cr WHEN ped-venda.cod-sit-aval = 3
                @ ped-venda.desc-bloq-cr
            ped-venda.quem-aprovou
            WITH FRAME f-detalhe.
    DOWN 1 WITH FRAME f-detalhe.

    FIND tt-work WHERE tt-work.cod-sit-aval = ped-venda.cod-sit-aval
                   AND tt-work.tp-pedido    = ped-venda.tp-pedido
                 NO-LOCK NO-ERROR.
    IF NOT AVAIL tt-work THEN DO:
       CREATE tt-work.
       ASSIGN tt-work.cod-sit-aval = ped-venda.cod-sit-aval
              tt-work.situacao     = c-situacao
              tt-work.tp-pedido    = ped-venda.tp-pedido.
    END.
    ASSIGN tt-work.vl-tot-ped  = tt-work.vl-tot-ped + ped-venda.vl-tot-ped
           tt-work.cont-pedido = tt-work.cont-pedido + 1.  
END.
PAGE.
FOR EACH tt-work:
    ACCUMULATE tt-work.cont-pedido(TOTAL)
               tt-work.vl-tot-ped(TOTAL).
    DISPLAY tt-work.cod-sit-aval   
            tt-work.situacao             
            tt-work.tp-pedido      
            tt-work.cont-pedido    
            tt-work.vl-tot-ped     
            WITH FRAME f-resumo.
    DOWN WITH FRAME f-resumo.
END.
DISPLAY "Total"                         @ tt-work.situacao
        ACCUM TOTAL tt-work.cont-pedido @ tt-work.cont-pedido
        ACCUM TOTAL tt-work.vl-tot-ped  @ tt-work.vl-tot-ped
        WITH FRAME f-resumo.

IF tt-param.impr-param THEN DO:
   PAGE.
   display tt-param.cod-estabel
           tt-param.dt-implant-ini
           tt-param.dt-implant-fin
           with frame f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

