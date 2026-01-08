/* Programa: ESPD045.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Pedidos
** Objetivo: Gerar o relatorio de Volumes de Pedidos a Faturar
** Autor...: Gilvando de Souza Araujo - Novembro/1997
** Obs.....: Especifico da CIA.RENASCENCA INDUSTRIAL / SANTA ELISABETH
**
** Conversao para EMS 2.04:
**   Programa: ESPD045.P  =>  ESPD0013RP.P
**   Autor...: Prodb - Toninho
**   Data....: 08/01/2004
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESPD0013RP 2.04.00.000}

define temp-table tt-param no-undo
       field destino          as integer
       field arquivo          as char format "x(35)"
       field usuario          as char format "x(12)"
       field data-exec        as date
       field hora-exec        as integer
       FIELD c-sigla-emb-ini  LIKE ped-item-res.sigla-emb
       FIELD c-sigla-emb-fim  LIKE ped-item-res.sigla-emb
       FIELD i-volume-ini     LIKE ped-item-res.volume-ini
       FIELD i-volume-fim     LIKE ped-item-res.volume-fim
       FIELD impr-param       AS LOGICAL.

define temp-table tt-raw-digita
       field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* defini‡Æo de vari veis  */
def var h-acomp as handle no-undo.

form
    tt-param.c-sigla-emb-ini  column-label "Familia de" at 1
    "a"                                                 AT 26
    tt-param.c-sigla-emb-fim  NO-LABELS
    tt-param.i-volume-ini     column-label "Volume de"  at 2
    "a"                                                 
    tt-param.i-volume-fim  NO-LABELS
    with no-box side-labels width 132 STREAM-IO frame f-parlis.

form
    ped-item-res.sigla-emb   label "Emb"
    ped-item-res.volume-ini  label "Volume Inicial"
    ped-item-res.volume-fim  label "Volume Final"
    ped-item-res.nr-pedcli   label "Pedido"
    ped-item-res.nome-abrev  label "Cliente"
    with no-box 55 down width 132 STREAM-IO frame f-detalhe no-label.

/* include padrÆo para output de relat¢rios */
{include/i-rpout.i}

/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */
{include/i-rpcab.i}

/* bloco principal do programa */
find first param-global no-lock no-error.
find first empresa
     where empresa.ep-codigo = param-global.empresa-prin no-lock no-error. 

assign c-empresa = (if avail empresa then empresa.razao-social else "").

{utp/ut-liter.i PEDIDOS * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Pedidos_de_Vendas_a_Faturar_por_Volume * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

for each ped-item-res where 
         ped-item-res.faturado   = NO AND
         ped-item-res.sigla-emb  >= tt-param.c-sigla-emb-ini AND
         ped-item-res.sigla-emb  <= tt-param.c-sigla-emb-fim AND
         ped-item-res.volume-ini >= tt-param.i-volume-ini AND
         ped-item-res.volume-fim <= tt-param.i-volume-fim 
         NO-LOCK break by ped-item-res.sigla-emb
                       by ped-item-res.volume-ini
                       by ped-item-res.nr-pedcli:

    run pi-acompanhar in h-acomp (INPUT ped-item-res.sigla-emb + " " + ped-item-res.nr-pedcli).
                         
    if last-of(ped-item-res.nr-pedcli) then
       display ped-item-res.sigla-emb
               ped-item-res.volume-ini
               ped-item-res.volume-fim
               ped-item-res.nr-pedcli
               ped-item-res.nome-abrev
               with frame f-detalhe.
    down with frame f-detalhe.

    if last-of(ped-item-res.sigla-emb) then
       page.
end.

IF tt-param.impr-param THEN DO.
   PAGE.
   PUT "*****----- PAR¶METROS ------*****"
       SKIP(1).
    
   display tt-param.c-sigla-emb-ini
           tt-param.c-sigla-emb-fim
           tt-param.i-volume-ini   
           tt-param.i-volume-fim   
           with frame f-parlis.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

