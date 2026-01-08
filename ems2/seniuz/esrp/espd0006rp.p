/* Programa: ESPD0006RP.P
** Autor...: Gilvando Souza Araujo
** Data....: 26/01/2004
** Observ..: Chamado pelo programa ESPD0006.W
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESPD0006RP 2.04.00.000}

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
       FIELD ge-codigo-ini    LIKE ITEM.ge-codigo
       FIELD ge-codigo-fin    LIKE ITEM.ge-codigo
       FIELD impr-param       AS   LOGICAL.

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

def var c-descr-cond as char format "x(25)".
def var de-abe-vlr as dec format "->,>>>,>>9.99".
def var de-abe-qtd as dec format ">,>>>,>>9.9999".
def var i-abe-num  as int format ">,>>9".
def var de-atp-vlr as dec format "->,>>>,>>9.99".
def var de-atp-qtd as dec format ">,>>>,>>9.9999".
def var i-atp-num as int format ">,>>9".
def var de-att-vlr as dec format "->,>>>,>>9.99".
def var de-att-qtd as dec format ">,>>>,>>9.9999".
def var i-att-num as int format ">,>>9".
def var de-can-vlr as dec format "->,>>>,>>9.99".
def var de-can-qtd as dec format ">,>>>,>>9.9999".
def var i-can-num as int format ">,>>9".
def var de-out-vlr as dec format "->,>>>,>>9.99".
def var de-out-qtd as dec format ">,>>>,>>9.9999".
def var i-out-num as int format ">,>>9".

def var de-naval-vlr as dec format "->,>>>,>>9.99".
def var de-naval-qtd as dec format ">,>>>,>>9.9999".
def var i-naval-num as int format ">,>>9".
def var de-aprov-vlr as dec format "->,>>>,>>9.99".
def var de-aprov-qtd as dec format ">,>>>,>>9.9999".
def var i-aprov-num as int format ">,>>9".
def var de-naprv-vlr as dec format "->,>>>,>>9.99".
def var de-naprv-qtd as dec format ">,>>>,>>9.9999".
def var i-naprv-num as int format ">,>>9".
def var de-recus-vlr as dec format "->,>>>,>>9.99".
def var de-recus-qtd as dec format ">,>>>,>>9.9999".
def var i-recus-num as int format ">,>>9".
def var de-tot-vlr as dec format "->,>>>,>>9.99".
def var de-tot-qtd as dec format ">,>>>,>>9.9999".
def var i-tot-num as int format ">,>>9".
def var de-qtd as dec.
def var de-quant as dec.
def var de-valor as dec.
def var de-reserva as dec.
def var de-qtd-conv as dec.

form
    "*------------ Parƒmetros/Sele‡Æo -------------*" SKIP
    tt-param.cod-estabel      LABEL "Estabelecimento." SKIP
    tt-param.dt-implant-ini   LABEL "Data Implanta‡Æo" 
    "a" AT 30
    tt-param.dt-implant-fin   NO-LABELS SKIP
    tt-param.ge-codigo-ini    LABEL "Grupo de Estoque"
    "a" AT 30
    tt-param.ge-codigo-fin    NO-LABELS SKIP
    with no-box side-labels width 132 STREAM-IO frame f-param.

form
    c-descr-cond   at 02
    i-abe-num      at 27
    de-abe-vlr     at 33
    de-abe-qtd     at 49
    header
    "CONDICOES FATURAMENTO   PEDIDOS         VALOR       QUANTIDADE"
    with NO-LABELS row 6 55 DOWN NO-BOX STREAM-IO frame f-detalhe.

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
{utp/ut-liter.i Estat¡stica_de_Pedidos_de_Vendas * r}
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
   
   if ped-venda.cod-estabel <> tt-param.cod-estabel then next.
   assign de-quant = 0
          de-valor = 0.

   for each ped-item of ped-venda no-lock:
       
       if  ped-venda.cod-sit-ped <> 10
       and ped-item.cod-sit-item =  10 then next.
       
       FIND item WHERE item.it-codigo = ped-item.it-codigo NO-ERROR.
       IF item.ge-codigo < tt-param.ge-codigo-ini OR 
          item.ge-codigo > tt-param.ge-codigo-fin THEN NEXT.
       
       assign de-qtd = ped-item.qt-pedida.

       IF item.un <> "m" then do:
          find item-ext where 
               item-ext.it-codigo = item.it-codigo no-lock no-error.
          IF not avail item-ext then do:
             PUT "Nao ha fator de conversao para o Item: " item.it-codigo
                 skip.
             NEXT.
          END.
          ASSIGN de-qtd-conv = de-qtd * item-ext.fator-conv.
       END.
       ELSE
          ASSIGN de-qtd-conv = de-qtd.

       assign de-quant = de-quant + de-qtd-conv.
       assign de-valor = de-valor + (ped-item.vl-preori * de-qtd).
   end.

   if  de-quant = 0
   and de-valor = 0 then next.

   if ped-venda.cod-sit-ped = 1 then /* Aberto */
      assign de-abe-vlr = de-abe-vlr + de-valor
             i-abe-num  = i-abe-num  + 1
             de-abe-qtd = de-abe-qtd + de-quant.
   else
   if ped-venda.cod-sit-ped = 2 then /* Atend.Parcial */
      assign de-atp-vlr = de-atp-vlr + de-valor
             i-atp-num  = i-atp-num  + 1
             de-atp-qtd = de-atp-qtd + de-quant.
   else
   if ped-venda.cod-sit-ped = 3 then /* Atend.Total */
      assign de-att-vlr = de-att-vlr + de-valor
             i-att-num  = i-att-num  + 1
             de-att-qtd = de-att-qtd + de-quant.
   else
   if ped-venda.cod-sit-ped = 10 then /* Cancelado */
      assign de-can-vlr = de-can-vlr + de-valor
             i-can-num  = i-can-num  + 1
             de-can-qtd = de-can-qtd + de-quant.
   else
      assign de-out-vlr = de-out-vlr + de-valor
             i-out-num  = i-out-num  + 1
             de-out-qtd = de-out-qtd + de-quant.

   if ped-venda.cod-sit-aval = 1 then  /* Cred.Nao Avaliado */
      assign de-naval-vlr = de-naval-vlr + de-valor
             i-naval-num  = i-naval-num  + 1
             de-naval-qtd = de-naval-qtd + de-quant.
   else
   if ped-venda.cod-sit-aval = 2 then  /* Cred.Avaliado */
      assign de-aprov-vlr = de-aprov-vlr + de-valor
             i-aprov-num  = i-aprov-num  + 1
             de-aprov-qtd = de-aprov-qtd + de-quant.
   else
   if ped-venda.cod-sit-aval = 3 then  /* Cred.Forcado */
      assign de-naprv-vlr = de-naprv-vlr + de-valor
             i-naprv-num  = i-naprv-num  + 1
             de-naprv-qtd = de-naprv-qtd + de-quant.
   else
      assign de-recus-vlr = de-recus-vlr + de-valor
             i-recus-num  = i-recus-num  + 1
             de-recus-qtd = de-recus-qtd + de-quant.
end.
assign de-tot-vlr = de-abe-vlr + de-atp-vlr + de-att-vlr + de-out-vlr
       de-tot-qtd = de-abe-qtd + de-atp-qtd + de-att-qtd + de-out-qtd
       i-tot-num  = i-abe-num  + i-atp-num  + i-att-num  + i-out-num.

assign c-descr-cond = "Em aberto...........".
display c-descr-cond
        i-abe-num
        de-abe-vlr
        de-abe-qtd
        with frame f-detalhe.
down with frame f-detalhe.

assign c-descr-cond = "Atendido parcial....".
display c-descr-cond
        i-atp-num   @ i-abe-num
        de-atp-vlr  @ de-abe-vlr
        de-atp-qtd  @ de-abe-qtd
        with frame f-detalhe.
down with frame f-detalhe.

assign c-descr-cond = "Atendido total......".
display c-descr-cond
        i-att-num   @ i-abe-num
        de-att-vlr  @ de-abe-vlr
        de-att-qtd  @ de-abe-qtd
        with frame f-detalhe.
down with frame f-detalhe.

assign c-descr-cond = "Cancelado...........".
display c-descr-cond
        i-can-num   @ i-abe-num
        de-can-vlr  @ de-abe-vlr
        de-can-qtd  @ de-abe-qtd
        with frame f-detalhe.
down with frame f-detalhe.

assign c-descr-cond = "Outros..............".
display c-descr-cond
        i-out-num   @ i-abe-num
        de-out-vlr  @ de-abe-vlr
        de-out-qtd  @ de-abe-qtd
        with frame f-detalhe.
down with frame f-detalhe.

assign c-descr-cond = "Total...............".
display c-descr-cond
        i-tot-num   @ i-abe-num
        de-tot-vlr  @ de-abe-vlr
        de-tot-qtd  @ de-abe-qtd
        with frame f-detalhe.
down 2 with frame f-detalhe.

assign c-descr-cond = "Credito nao avaliado".
display c-descr-cond
        i-naval-num   @ i-abe-num
        de-naval-vlr  @ de-abe-vlr
        de-naval-qtd  @ de-abe-qtd
        with frame f-detalhe.
down with frame f-detalhe.

assign c-descr-cond = "Credito aprovado....".
display c-descr-cond
        i-aprov-num   @ i-abe-num
        de-aprov-vlr  @ de-abe-vlr
        de-aprov-qtd  @ de-abe-qtd
        with frame f-detalhe.
down with frame f-detalhe.

assign c-descr-cond = "Credito forcado.....".
display c-descr-cond
        i-naprv-num   @ i-abe-num
        de-naprv-vlr  @ de-abe-vlr
        de-naprv-qtd  @ de-abe-qtd
        with frame f-detalhe.
down with frame f-detalhe.

assign c-descr-cond = "Credito recusado....".
display c-descr-cond
        i-recus-num   @ i-abe-num
        de-recus-vlr  @ de-abe-vlr
        de-recus-qtd  @ de-abe-qtd
        with frame f-detalhe.
down with frame f-detalhe.

IF tt-param.impr-param THEN DO:
   PAGE.
   display tt-param.cod-estabel
           tt-param.dt-implant-ini
           tt-param.dt-implant-fin
           tt-param.ge-codigo-ini
           tt-param.ge-codigo-fin
           with frame f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

