/* Programa: ESPD0008RP.P
** Autor...: Gilvando Souza Araujo
** Data....: 26/01/2004
** Observ..: Chamado pelo programa ESPD0008.W
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESPD0008RP 2.04.00.000}

define temp-table tt-param no-undo
       field destino          as integer
       field arquivo          as char format "x(35)"
       field usuario          as char format "x(12)"
       field data-exec        as date
       field hora-exec        as integer
       field classifica       as integer
       FIELD cod-estabel      LIKE ped-venda.cod-estabel
       FIELD dt-entrega-ini   LIKE ped-venda.dt-entrega
       FIELD dt-entrega-fin   LIKE ped-venda.dt-entrega
       FIELD ge-codigo-ini    LIKE item.ge-codigo
       FIELD ge-codigo-fin    LIKE item.ge-codigo
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
def var de-naprov-vlr as dec format "->,>>>,>>9.99".
def var de-naprov-qtd as dec format ">>>>,>>9.9999".
def var de-naprov-res as dec format ">>>>,>>9.9999".
def var i-naprov-num as int format ">>>9".
def var de-avista-vlr as dec format "->,>>>,>>9.99".
def var de-avista-qtd as dec format ">>>>,>>9.9999".
def var de-avista-res as dec format ">>>>,>>9.9999".
def var i-avista-num as int format ">>>9".
def var de-export-vlr as dec format "->,>>>,>>9.99".
def var de-export-qtd as dec format ">>>>,>>9.9999".
def var de-export-res as dec format ">>>>,>>9.9999".
def var i-export-num as int format ">>>9".
def var de-nparc-vlr as dec format "->,>>>,>>9.99".
def var de-nparc-qtd as dec format ">>>>,>>9.9999".
def var de-nparc-res as dec format ">>>>,>>9.9999".
def var i-nparc-num as int format ">>>9".
def var de-parc-vlr as dec format "->,>>>,>>9.99".
def var de-parc-qtd as dec format ">>>>,>>9.9999".
def var de-parc-res as dec format ">>>>,>>9.9999".
def var i-parc-num as int format ">>>9".
def var de-vlr-tot as dec format "->,>>>,>>9.99".
def var de-qtd-tot as dec format ">>>>,>>9.9999".
def var de-res-tot as dec format ">>>>,>>9.9999".
def var i-num-tot as int format ">>>9".
def var l-completo as log.
def var de-totcmp-nap as dec.
def var de-totcmp-exp as dec.
def var de-totcmp-av  as dec.
def var de-totcmp-npa as dec.
def var de-totcmp-apa as dec.
def var de-percmp-nap as dec format ">>9.9".
def var de-percmp-exp as dec format ">>9.9".
def var de-percmp-av  as dec format ">>9.9".
def var de-percmp-npa as dec format ">>9.9".
def var de-percmp-apa as dec format ">>9.9".
def var de-qtd as dec.
def var de-quant as dec.
def var de-valor as dec.
def var de-reserva as dec.
def var de-qtd-conv as dec.

form
    "*------------ Parƒmetros/Sele‡Æo -------------*" SKIP
    tt-param.cod-estabel      LABEL "Estabelecimento." SKIP
    tt-param.dt-entrega-ini   LABEL "Data de Entrega." 
    "a" AT 30
    tt-param.dt-entrega-fin   NO-LABELS SKIP
    tt-param.ge-codigo-ini    LABEL "Grupo de Estoque"
    "a" AT 30
    tt-param.ge-codigo-fin    NO-LABELS SKIP
    with no-box side-labels width 132 STREAM-IO frame f-param.

form
    c-descr-cond   at 02
    i-naprov-num   at 27
    de-naprov-vlr  at 33
    de-naprov-qtd  at 49
    de-naprov-res  at 62
    de-percmp-nap  at 76
    header
    "CONDICOES FATURAMENTO   PEDIDOS         VALOR      QUANTIDADE      RESERVA %CMPL"
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
{utp/ut-liter.i Estat¡stica_de_Pedidos_de_Vendas_x_Atendimento * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

for each ped-venda where ped-venda.dt-entrega  >= tt-param.dt-entrega-ini
                     and ped-venda.dt-entrega  <= tt-param.dt-entrega-fin
                     and (ped-venda.cod-sit-ped < 3 or
                          ped-venda.cod-sit-ped = 5)
                   no-lock:

   run pi-acompanhar in h-acomp (input "Pedido: " + ped-venda.nr-pedcli).

   if ped-venda.cod-estabel <> tt-param.cod-estabel then next.
   assign de-quant = 0
          de-valor = 0.

   /* --- Verifica se pedido esta pronto --- */
   assign l-completo = yes.
   for each ped-item of ped-venda 
       where ped-item.cod-sit-item <> 3
         and ped-item.cod-sit-item <> 10
       no-lock:
       find ped-item-res 
          where ped-item-res.nome-abrev   = ped-item.nome-abrev
            and ped-item-res.nr-pedcli    = ped-item.nr-pedcli
            and ped-item-res.it-codigo    = ped-item.it-codigo
            AND ped-item-res.cod-refer    = ped-item.cod-refer 
            and ped-item-res.nr-sequencia = ped-item.nr-sequencia
          no-lock no-error.
       if not avail ped-item-res then
          assign l-completo = no.
   end.
   
   for each ped-item where ped-item.nome-abrev = ped-venda.nome-abrev
                       and ped-item.nr-pedcli  = ped-venda.nr-pedcli
                       and (ped-item.cod-sit-item < 3
                        or ped-item.cod-sit-item = 5)
                           no-lock:
       find item where item.it-codigo = ped-item.it-codigo no-error.
       if item.ge-codigo < tt-param.ge-codigo-ini
       OR item.ge-codigo > tt-param.ge-codigo-fin then next.
       assign de-qtd = ped-item.qt-pedida
                     - ped-item.qt-pendente
                     - ped-item.qt-atendida.

       if item.un <> "m" then do:
          find item-ext where 
               item-ext.it-codigo = item.it-codigo no-lock no-error.
          IF not avail item-ext then do:
             PUT "Nao ha fator de conversao para o Item: " item.it-codigo
                 skip.
             NEXT.
          END.
          ASSIGN de-qtd-conv = de-qtd * item-ext.fator-conv.
       end.
       else
          assign de-qtd-conv = de-qtd.

       assign de-quant = de-quant + de-qtd-conv.
       assign de-valor = de-valor + (ped-item.vl-preori * de-qtd).
   end.

   if  de-quant = 0
   and de-valor = 0 then next.

   assign de-reserva = 0.
   for each ped-item-res 
       where ped-item-res.nome-abrev = ped-venda.nome-abrev
         and ped-item-res.nr-pedcli  = ped-venda.nr-pedcli
       no-lock:
       if ped-item-res.faturado = no then
          assign de-reserva = de-reserva + ped-item-res.qt-pedida.
   end.
   
   if ped-venda.cod-sit-aval = 3 or  /* Credito Aprovado */
      ped-venda.cod-sit-aval = 2     /* Credito Avaliado */ 
   or (ped-venda.cod-cond-pag > 0 and ped-venda.cod-cond-pag < 4)
      then do:
      find emitente where emitente.cod-emitente = ped-venda.cod-emitente
           no-error.
      if emitente.natureza = 3 then do:
         assign de-export-vlr = de-export-vlr + de-valor
                i-export-num  = i-export-num  + 1
                de-export-qtd = de-export-qtd + de-quant
                de-export-res = de-export-res + de-reserva.
         if l-completo then
            assign de-totcmp-exp = de-totcmp-exp + de-reserva.
      end.
      else do:
         if  ped-venda.cod-cond-pag > 0
         and ped-venda.cod-cond-pag < 4 then do:
            assign de-avista-vlr = de-avista-vlr + de-valor
                   i-avista-num  = i-avista-num  + 1
                   de-avista-qtd = de-avista-qtd + de-quant
                   de-avista-res = de-avista-res + de-reserva.
            if l-completo then
               assign de-totcmp-av = de-totcmp-av + de-reserva.
         end.
         else do:
            if ped-venda.ind-fat-par then do:
               assign de-parc-vlr = de-parc-vlr + de-valor
                      i-parc-num  = i-parc-num  + 1
                      de-parc-qtd = de-parc-qtd + de-quant
                      de-parc-res = de-parc-res + de-reserva.
               if l-completo then
                  assign de-totcmp-apa = de-totcmp-apa + de-reserva.
            end.
            else do:
               assign de-nparc-vlr = de-nparc-vlr + de-valor
                      i-nparc-num  = i-nparc-num  + 1
                      de-nparc-qtd = de-nparc-qtd + de-quant
                      de-nparc-res = de-nparc-res + de-reserva.
               if l-completo then
                  assign de-totcmp-npa = de-totcmp-npa + de-reserva.
            end.
         end.
      end.
   end.
   else do:
      assign de-naprov-vlr = de-naprov-vlr + de-valor
             i-naprov-num  = i-naprov-num  + 1
             de-naprov-qtd = de-naprov-qtd + de-quant
             de-naprov-res = de-naprov-res + de-reserva.
      if l-completo then
         assign de-totcmp-nap = de-totcmp-nap + de-reserva.
   end.
end.
assign de-vlr-tot = de-naprov-vlr + de-avista-vlr + de-export-vlr
                 + de-nparc-vlr + de-parc-vlr
      de-qtd-tot = de-naprov-qtd + de-avista-qtd + de-export-qtd
                 + de-nparc-qtd + de-parc-qtd
      de-res-tot = de-naprov-res + de-avista-res + de-export-res
                 + de-nparc-res + de-parc-res
      i-num-tot  = i-naprov-num + i-avista-num + i-export-num
                 + i-nparc-num + i-parc-num.

hide message no-pause.

assign c-descr-cond = "Credito Nao Aprovado".
if de-naprov-res > 0 then
   assign de-percmp-nap = de-totcmp-nap / de-naprov-res * 100.
else
   assign de-percmp-nap = 0.
display c-descr-cond
        i-naprov-num
        de-naprov-vlr
        de-naprov-qtd
        de-naprov-res
        de-percmp-nap
        with frame f-detalhe.
down with frame f-detalhe.
assign c-descr-cond = "Exportacao .........".
if de-export-res > 0 then
   assign de-percmp-exp = de-totcmp-exp / de-export-res * 100.
else
   assign de-percmp-exp = 0.
display c-descr-cond
        i-export-num   @ i-naprov-num
        de-export-vlr  @ de-naprov-vlr
        de-export-qtd  @ de-naprov-qtd
        de-export-res  @ de-naprov-res
        de-percmp-exp  @ de-percmp-nap
        with frame f-detalhe.
down with frame f-detalhe.
assign c-descr-cond = "A Vista ............".
if de-avista-res > 0 then
   assign de-percmp-av = de-totcmp-av / de-avista-res * 100.
else
   assign de-percmp-nap = 0.
display c-descr-cond
        i-avista-num   @ i-naprov-num
        de-avista-vlr  @ de-naprov-vlr
        de-avista-qtd  @ de-naprov-qtd
        de-avista-res  @ de-naprov-res
        de-percmp-av   @ de-percmp-nap
        with frame f-detalhe.
down with frame f-detalhe.
assign c-descr-cond = "Nao Aceita Parcial .".
if de-nparc-res > 0 then
   assign de-percmp-npa = de-totcmp-npa / de-nparc-res * 100.
else
   assign de-percmp-npa = 0.
display c-descr-cond
        i-nparc-num   @ i-naprov-num
        de-nparc-vlr  @ de-naprov-vlr
        de-nparc-qtd  @ de-naprov-qtd
        de-nparc-res  @ de-naprov-res
        de-percmp-npa @ de-percmp-nap
        with frame f-detalhe.
down with frame f-detalhe.
assign c-descr-cond = "Aceita Parcial .....".
if de-parc-res > 0 then
   assign de-percmp-apa = de-totcmp-apa / de-parc-res * 100.
else
   assign de-percmp-apa = 0.
display c-descr-cond
        i-parc-num    @ i-naprov-num
        de-parc-vlr   @ de-naprov-vlr
        de-parc-qtd   @ de-naprov-qtd
        de-parc-res   @ de-naprov-res
        de-percmp-apa @ de-percmp-nap
        with frame f-detalhe.
down 2 with frame f-detalhe.
assign c-descr-cond = "Total ..............".
display c-descr-cond
        i-num-tot   @ i-naprov-num
        de-vlr-tot  @ de-naprov-vlr
        de-qtd-tot  @ de-naprov-qtd
        de-res-tot  @ de-naprov-res
        with frame f-detalhe.
down with frame f-detalhe.

IF tt-param.impr-param THEN DO:
   PAGE.
   display tt-param.cod-estabel
           tt-param.dt-entrega-ini
           tt-param.dt-entrega-fin
           tt-param.ge-codigo-ini
           tt-param.ge-codigo-fin
           with frame f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

