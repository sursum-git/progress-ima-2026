/* Programa: ESPD018.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Faturamento
** Objetivo: Gerar o relatorio de Faturamento por Representantes com Prazo M‚dio.
** Autor...: Gilvando de Souza Araujo - Junho/95
** Obs.....: Especifico da CIA.RENASCENCA INDUSTRIAL
**
** Conversao para EMS 2.04:
**   Programa: ESPD018.P  =>  ESFT0018RP.P
**   Autor...: Gilvando Souza Araujo
**   Data....: 22/01/2004
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESFT0018RP 2.04.00.000}

DEFINE temp-table tt-param  no-undo
       field destino           as integer
       field arquivo           as char format "x(35)"
       field usuario           as char format "x(12)"
       field data-exec         as date
       field hora-exec         as integer
       field classifica        as integer
       field desc-classifica   as char format "x(40)"
       FIELD ini-dt-entrega    LIKE ped-venda.dt-entrega
       FIELD fin-dt-entrega    LIKE ped-venda.dt-entrega
       FIELD ini-ge-codigo     LIKE ITEM.ge-codigo
       FIELD fin-ge-codigo     LIKE ITEM.ge-codigo
       FIELD ini-nome-repres   LIKE ped-venda.no-ab-reppri
       FIELD fin-nome-repres   LIKE ped-venda.no-ab-reppri
       FIELD imp-abe-tot       AS INT
       FIELD imp-param         AS LOG.

define temp-table tt-raw-digita
       field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

def TEMP-TABLE w-work
    field nom-abrv-rep like ped-venda.no-ab-reppri
    field it-codigo    like item.it-codigo
    field un           like item.un
    field quantidade   as dec
    field valor        as dec
    field prazo-total  as int
    field qtd-duplic   as int
    INDEX ch-work nom-abrv-rep
                  it-codigo.

def TEMP-TABLE tt-work  /* Itens sem fator de conversao */
    field it-codigo like item.it-codigo
    INDEX ch-work it-codigo.

create tt-param.
raw-transfer raw-param to tt-param.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* include padrÆo para impressÆo de campos editores em relat¢rio  */
{include/tt-edit.i}

/* defini‡Æo de vari veis  */
def var h-acomp as handle no-undo.

def var i-prazo-med-item as int format ">>9".
def var de-preco-med-item as dec format ">>>,>>9.99".
def var de-quantidade as dec.
def var de-quantidade-item as dec format ">>>,>>>,>>9.99".
def var de-valor as dec.
def var de-valor-item as dec format ">>>,>>>,>>9.99".
def var i-prazo-med-rep as int format ">>9".
def var de-preco-med-rep as dec format ">>>,>>9.99".
def var de-quantidade-rep as dec.
def var de-valor-rep as dec.
def var i-prazo-med-tot as int format ">>9".
def var de-preco-med-tot as dec format ">>>,>>9.99".
def var de-quantidade-tot as dec.
def var de-valor-tot as dec.
def var de-qt-conv as dec.
def var c-opcao as char.
def var i-cont as int.
def var i-qtd-duplic as int.
def var i-qtd-duplic-item as int.
def var i-prazo-total as int.
def var i-prazo-item as int.
def var i-qtd-duplic-rep as int.
def var i-prazo-rep as int.
def var i-qtd-duplic-tot as int.
def var i-prazo-tot as int.
def var l-prim-vez as log.
DEF VAR l-imp-abe-tot AS LOG FORMAT "Aberta/Total".
DEF VAR l-falta-fator    AS LOG.

form 
    "*------------ Parƒmetros/Sele‡Æo -------------*" SKIP
    tt-param.ini-dt-entrega   label "Data Entrega.."
    "A"  AT 30
    tt-param.fin-dt-entrega   NO-LABELS SKIP
    tt-param.ini-ge-codigo    label "Grupo Estoque."
    "A"  AT 30
    tt-param.fin-ge-codigo    no-labels SKIP
    tt-param.ini-nome-repres  label "Representante."
    "A"  AT 30
    tt-param.fin-nome-repres  NO-LABELS SKIP
    l-imp-abe-tot             LABEL "Quantidade...."
    skip(1)
    with FRAME f-param side-labels no-box width 132 STREAM-IO.

form
    w-work.nom-abrv-rep label "Representante"
    w-work.it-codigo    label "Item"
    item.desc-item      label "Descricao" FORMAT "x(36)"
    de-quantidade-item  label "Quantidade"
    w-work.un           label "Un"
    de-valor-item       label "Valor"
    i-prazo-med-item    label "PzMed"
    de-preco-med-item   label "Prec.Med"
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

{utp/ut-liter.i FATURAMENTO * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Faturamento_por_Representante_com_Prazo_M‚dio * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
VIEW FRAME f-cabecalho.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

for each w-work.
    delete w-work.
end.

ASSIGN l-imp-abe-tot = tt-param.imp-abe-tot = 1.

for each ped-venda where ped-venda.no-ab-reppri >= tt-param.ini-nome-repres
                     and ped-venda.no-ab-reppri <= tt-param.fin-nome-repres
                     and ped-venda.dt-entrega   >= tt-param.ini-dt-entrega
                     and ped-venda.dt-entrega   <= tt-param.fin-dt-entrega
                     and ped-venda.cod-sit-ped  <> 10
                     no-lock:

   run pi-acompanhar in h-acomp (input "Pedido: " + ped-venda.nr-pedcli).

   for each ped-item of ped-venda 
       where ped-item.cod-sit-item < 3 
          or ped-item.cod-sit-item = 5 
       no-lock: 
       find item where item.it-codigo = ped-item.it-codigo
                 no-error.
       IF  avail item 
       AND item.ge-codigo < tt-param.ini-ge-codigo
       OR  item.ge-codigo > tt-param.fin-ge-codigo then next.

       if tt-param.imp-abe-tot = 1 then do: /* Qtd.Aberta */
          assign de-quantidade = ped-item.qt-pedida -
                                 ped-item.qt-atendida +
                                 ped-item.qt-devolvida.
          assign de-valor      = de-quantidade * ped-item.vl-preori.
       end.
       else                                 /* Qtd.Total */
          assign de-quantidade = ped-item.qt-pedida
                 de-valor      = ped-item.qt-pedida
                               * ped-item.vl-preori.
       if  de-quantidade = 0
       and de-valor      = 0 then next.
       
       if ped-venda.cod-cond-pag <> 0 then do:
          find cond-pagto where cond-pagto.cod-cond-pag =
                                ped-venda.cod-cond-pag
                                no-lock no-error.
          if avail cond-pagto then do:
             assign i-qtd-duplic  = cond-pagto.num-parcelas
                    i-prazo-total = 0.
             do i-cont = 1 to 12:
                assign i-prazo-total = i-prazo-total
                                     + cond-pagto.prazos[i-cont].
             end.
          end.
       end.
       else do:
          find con-pges where con-pges.nr-pedido = ped-venda.nr-pedido
                              no-lock no-error.
          assign i-qtd-duplic  = 0
                 i-prazo-total = 0.
          if avail con-pges then
             do i-cont = 1 to 6:
                if con-pges.nr-dias-venc[i-cont] <> 0 then
                   assign i-qtd-duplic  = i-qtd-duplic + 1
                          i-prazo-total = i-prazo-total
                                        + con-pges.nr-dias-venc[i-cont].
             end.
       end.
       /*------ Conversao de M para Kg ------- */
       if item.un <> "m" then do:
          FIND item-ext WHERE ITEM-ext.it-codigo = ITEM.it-codigo
                        NO-LOCK NO-ERROR.
          if AVAIL item-ext then
             assign de-qt-conv = de-quantidade * 
                                 item-ext.fator-conv.
          else do:
             assign l-falta-fator = yes
                    de-qt-conv    = de-quantidade.
             find first tt-work where
                        tt-work.it-codigo = item.it-codigo
                        no-lock no-error.
             if not avail tt-work then do:
                create tt-work.
                assign tt-work.it-codigo = item.it-codigo.
             end.
          end.   
       end.

       find first w-work
            where w-work.nom-abrv-rep = ped-venda.no-ab-reppri
              and w-work.it-codigo    = item.it-codigo
              no-error.
       if not avail w-work then do:
          create w-work.
          assign w-work.nom-abrv-rep = ped-venda.no-ab-reppri
                 w-work.it-codigo    = item.it-codigo
                 w-work.un           = "M "
                 w-work.quantidade   = 0
                 w-work.valor        = 0
                 w-work.prazo-total  = 0
                 w-work.qtd-duplic   = 0.
       end.
       assign w-work.quantidade  = w-work.quantidade + de-qt-conv
              w-work.valor       = w-work.valor + de-valor
              w-work.prazo-total = w-work.prazo-total + i-prazo-total
              w-work.qtd-duplic  = w-work.qtd-duplic + i-qtd-duplic.

   end. /* ped-item */

end. /* ped-venda */

for each w-work break by w-work.nom-abrv-rep
                      by w-work.it-codigo.
   assign de-quantidade-item = w-work.quantidade
          de-valor-item      = w-work.valor
          i-prazo-item       = w-work.prazo-total
          i-qtd-duplic-item  = w-work.qtd-duplic
          de-quantidade-rep  = de-quantidade-rep + w-work.quantidade
          de-valor-rep       = de-valor-rep      + w-work.valor
          i-prazo-rep        = i-prazo-rep       + w-work.prazo-total
          i-qtd-duplic-rep   = i-qtd-duplic-rep  + w-work.qtd-duplic
          de-quantidade-tot  = de-quantidade-tot + w-work.quantidade
          de-valor-tot       = de-valor-tot      + w-work.valor
          i-prazo-tot        = i-prazo-tot       + w-work.prazo-total
          i-qtd-duplic-tot   = i-qtd-duplic-tot  + w-work.qtd-duplic.

   if i-qtd-duplic-item <> 0 then
      assign i-prazo-med-item = i-prazo-item / i-qtd-duplic-item.
   else
      assign i-prazo-med-item = 0.
   if de-quantidade-item <> 0 then
      assign de-preco-med-item = de-valor-item / de-quantidade-item.
   else
      assign de-preco-med-item = 0.

   find ITEM where item.it-codigo = w-work.it-codigo no-lock no-error.

   display w-work.nom-abrv-rep when first-of(w-work.nom-abrv-rep)
           w-work.it-codigo
           item.desc-item
           de-quantidade-item
           w-work.un
           de-valor-item
           i-prazo-med-item
           de-preco-med-item
           with frame f-detalhe.
   down with frame f-detalhe.

   if last-of(w-work.nom-abrv-rep) then do:
      if i-qtd-duplic-rep <> 0 then
         assign i-prazo-med-rep = i-prazo-rep / i-qtd-duplic-rep.
      else
         assign i-prazo-med-rep = 0.
      if de-valor-rep <> 0 then
         assign de-preco-med-rep = de-valor-rep / de-quantidade-rep.
      else
         assign de-preco-med-rep = 0.
      display "total"           @ w-work.nom-abrv-rep
              de-quantidade-rep @ de-quantidade-item
              "M "              @ w-work.un
              de-valor-rep      @ de-valor-item
              i-prazo-med-rep   @ i-prazo-med-item
              de-preco-med-rep  @ de-preco-med-item
              with frame f-detalhe.
      down with frame f-detalhe.
      put skip(1).
      assign de-quantidade-rep = 0
             de-valor-rep      = 0
             i-prazo-rep       = 0
             i-qtd-duplic-rep  = 0.
   end.
end.
if i-qtd-duplic-tot <> 0 then
  assign i-prazo-med-tot = i-prazo-tot / i-qtd-duplic-tot.
else
  assign i-prazo-med-tot = 0.
if de-valor-tot <> 0 then
  assign de-preco-med-tot = de-valor-tot / de-quantidade-tot.
else
  assign de-preco-med-tot = 0.

display "total geral"     @ w-work.nom-abrv-rep
        de-quantidade-tot @ de-quantidade-item
        "M "              @ w-work.un
        de-valor-tot      @ de-valor-item
        i-prazo-med-tot   @ i-prazo-med-item
        de-preco-med-tot  @ de-preco-med-item
        with frame f-detalhe.
down with frame f-detalhe.

assign de-quantidade-tot = 0
       de-valor-tot      = 0
       i-prazo-tot       = 0
       i-qtd-duplic-tot  = 0.

if l-falta-fator then do:
   page.
   put "Atencao ! - Ha itens sem fator de conversao:"
       skip(1).
   for each tt-work:
       FIND ITEM WHERE ITEM.it-codigo = tt-work.it-codigo
                 NO-LOCK NO-ERROR.
       IF AVAIL ITEM THEN
          PUT tt-work.it-codigo
              ITEM.desc-item
              SKIP.
       ELSE
          PUT tt-work.it-codigo
              SKIP.
   end.
   for each tt-work.
       delete tt-work.
   end.
end.  

IF tt-param.imp-param THEN DO:
   PAGE.
   display tt-param.ini-dt-entrega
           tt-param.fin-dt-entrega
           tt-param.ini-ge-codigo
           tt-param.fin-ge-codigo
           tt-param.ini-nome-repres
           tt-param.fin-nome-repres
           l-imp-abe-tot
           with frame f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

