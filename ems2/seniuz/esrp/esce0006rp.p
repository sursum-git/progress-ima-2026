/* Programa: ESCE010.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Controle de Estoque
** Objetivo: Listar o saldo atual de estoques por item de materiais,
**           separando itens normais, retalho, miscelanea perfeito/ld.
** Autor...: Gilvando de Souza Araujo - Agosto/95
** Obs.....: Especifico da CIA.RENASCENCA INDUSTRIAL.
**           Este programa leva em conta os codigos dos itens(posicao 7 a 11):
**           XXXXXX99000 Retalho
**           XXXXXX99500 Miscelanea Regular
**           XXXXXX99510 Miscelanea LD
**           Outros      Normal
**
** Conversao para EMS 2.04:
**   Autor: Prodb - Toninho
**   Data: 06/01/2004
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESCE0006RP 2.04.00.000}

define temp-table tt-param no-undo
       FIELD destino          as integer
       FIELD arquivo          as char format "x(35)"
       FIELD usuario          as char format "x(12)"
       FIELD data-exec        as date
       FIELD hora-exec        as integer
       FIELD c-item-ini       LIKE item.it-codigo
       FIELD c-item-fim       LIKE item.it-codigo
       FIELD l-imp-retalho    AS LOGICAL
       FIELD l-imp-miscreg    AS LOGICAL
       FIELD l-imp-miscld     AS LOGICAL
       FIELD l-imp-normal     AS LOGICAL
       FIELD l-lista-teccru   AS LOGICAL
       FIELD impr-param       AS LOGICAL.

define temp-table tt-raw-digita
       field raw-digita as raw.

def TEMP-TABLE tt-work  /* Itens sem fator de conversao */
    field it-codigo like item.it-codigo
    INDEX ch-work it-codigo.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* defini‡Æo de vari veis  */
def var h-acomp as handle no-undo.

def var l-acumula as log.
def var c-qualidade as char format "x(18)".
def var c-it-codigo like item.it-codigo.
def var c-descricao AS CHAR FORMAT "x(25)".
def var c-un as char format "x(2)" extent 4.
def var c-unidade as char format "x(2)".
def var c-tot-ger as char format "x(12)" init "Total Geral".
DEF VAR l-falta-fator AS LOG.

def var de-qtidade-atu like saldo-estoq.qtidade-atu.
def var de-qt-pedida   like ped-item-res.qt-pedida.
def var de-tot-qtd-ret as dec format "->>>,>>>,>>>,>>9.9999".
def var de-tot-qtd-mir as dec format "->>>,>>>,>>>,>>9.9999".
def var de-tot-qtd-mid as dec format "->>>,>>>,>>>,>>9.9999".
def var de-tot-qtd-nor as dec format "->>>,>>>,>>>,>>9.9999".
def var de-tot-qtd-ger as dec format "->>>,>>>,>>>,>>9.9999".
def var de-acm-qtd-ret as dec format "->>>,>>>,>>>,>>9.9999".
def var de-acm-qtd-mir as dec format "->>>,>>>,>>>,>>9.9999".
def var de-acm-qtd-mid as dec format "->>>,>>>,>>>,>>9.9999".
def var de-acm-qtd-nor as dec format "->>>,>>>,>>>,>>9.9999".
def var de-acm-qtd-ger as dec format "->>>,>>>,>>>,>>9.9999".
def var de-tot-res-ret as dec format ">>>,>>>,>>>,>>9.9999".
def var de-tot-res-mir as dec format ">>>,>>>,>>>,>>9.9999".
def var de-tot-res-mid as dec format ">>>,>>>,>>>,>>9.9999".
def var de-tot-res-nor as dec format ">>>,>>>,>>>,>>9.9999".
def var de-tot-res-ger as dec format ">>>,>>>,>>>,>>9.9999".
def var de-acm-res-ret as dec format ">>>,>>>,>>>,>>9.9999".
def var de-acm-res-mir as dec format ">>>,>>>,>>>,>>9.9999".
def var de-acm-res-mid as dec format ">>>,>>>,>>>,>>9.9999".
def var de-acm-res-nor as dec format ">>>,>>>,>>>,>>9.9999".
def var de-acm-res-ger as dec format ">>>,>>>,>>>,>>9.9999".
def var de-tot-sal-ret as dec format "->>>,>>>,>>>,>>9.9999".
def var de-tot-sal-mir as dec format "->>>,>>>,>>>,>>9.9999".
def var de-tot-sal-mid as dec format "->>>,>>>,>>>,>>9.9999".
def var de-tot-sal-nor as dec format "->>>,>>>,>>>,>>9.9999".
def var de-tot-sal-ger as dec format "->>>,>>>,>>>,>>9.9999".
def var de-acm-sal-ret as dec format "->>>,>>>,>>>,>>9.9999".
def var de-acm-sal-mir as dec format "->>>,>>>,>>>,>>9.9999".
def var de-acm-sal-mid as dec format "->>>,>>>,>>>,>>9.9999".
def var de-acm-sal-nor as dec format "->>>,>>>,>>>,>>9.9999".
def var de-acm-sal-ger as dec format "->>>,>>>,>>>,>>9.9999".

form
    tt-param.c-item-ini  column-label "item de"  at 1
    "a"
    tt-param.c-item-fim  no-labels skip(1)
    with no-box side-labels width 132 STREAM-IO frame f-parlis.

form
    c-it-codigo    label "Item"  FORMAT "x(6)"
    c-descricao    label "Descricao"
    c-unidade      label "Un"
    de-tot-qtd-ret label "Quant Atual"
    de-tot-res-ret label "Quant Reservada"
    de-tot-sal-ret label "Saldo Disponivel"
    c-qualidade    label "Qualidade"
    with no-box NO-LABEL 55 down width 132 STREAM-IO frame f-detalhe.

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
{utp/ut-liter.i Listagem_Estoque_Atual_Item/Qualidade * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).
        
for each item where
         item.it-codigo >= tt-param.c-item-ini AND
         item.it-codigo <= tt-param.c-item-fim NO-LOCK 
         BY item.it-codigo:
    
    run pi-acompanhar in h-acomp (input "Item: " + item.it-codigo).

    for each ref-item WHERE
             ref-item.it-codigo =  item.it-codigo NO-LOCK.

        if  tt-param.l-lista-teccru = NO AND
            substring(item.it-codigo,6,1) = "0" then next.

        assign l-acumula = yes.
        if substr(ref-item.cod-refer,3,5) = "99000" AND
           tt-param.l-imp-retalho = no then
           assign l-acumula = no.

        if substr(ref-item.cod-refer,3,5) = "99500" AND
           tt-param.l-imp-miscreg = no then
           assign l-acumula = no.

        if substr(ref-item.cod-refer,3,5) = "99510" AND
           tt-param.l-imp-miscld = no then
           assign l-acumula = no.

        if  tt-param.l-imp-normal = NO AND
            substr(ref-item.cod-refer,3,5) <> "99000" and
            substr(ref-item.cod-refer,3,5) <> "99500" and
            substr(ref-item.cod-refer,3,5) <> "99510" then
            assign l-acumula = no.

        if not l-acumula then next.

        if substr(ref-item.cod-refer,3,5) = "99000" then
           assign c-un[1] = item.un.
        if substr(ref-item.cod-refer,3,5) = "99500" then
           assign c-un[2] = item.un.
        if substr(ref-item.cod-refer,3,5) = "99510" then
           assign c-un[3] = item.un.

        if  substr(ref-item.cod-refer,3,5) <> "99000" and
            substr(ref-item.cod-refer,3,5) <> "99500" and
            substr(ref-item.cod-refer,3,5) <> "99510" then
            assign c-un[4] = item.un.

        for each saldo-estoq WHERE
                 saldo-estoq.it-codigo = item.it-codigo and
                 saldo-estoq.cod-refer = ref-item.cod-refer NO-LOCK.
            assign de-qtidade-atu = saldo-estoq.qtidade-atu.

            if substr(saldo-estoq.cod-refer,3,5) = "99000" then
               assign de-tot-qtd-ret = de-tot-qtd-ret + de-qtidade-atu.
            else
            if substr(saldo-estoq.cod-refer,3,5) = "99500" then
               assign de-tot-qtd-mir = de-tot-qtd-mir + de-qtidade-atu.
            else
            if substr(saldo-estoq.cod-refer,3,5) = "99510" then
               assign de-tot-qtd-mid = de-tot-qtd-mid + de-qtidade-atu.
            else
               assign de-tot-qtd-nor = de-tot-qtd-nor + de-qtidade-atu.
        
            /*------ Conversao de M para Kg ------- */
            if item.un <> "m" then do:
               FIND item-ext WHERE ITEM-ext.it-codigo = ITEM.it-codigo
                             NO-LOCK NO-ERROR.
               if AVAIL item-ext then
                  assign de-qtidade-atu = saldo-estoq.qtidade-atu * 
                                          item-ext.fator-conv.
               else do:
                  assign l-falta-fator = yes
                         de-qtidade-atu = saldo-estoq.qtidade-atu.
                  find first tt-work where
                             tt-work.it-codigo = item.it-codigo
                             no-lock no-error.
                  if not avail tt-work then do:
                     create tt-work.
                     assign tt-work.it-codigo = item.it-codigo.
                  end.
               end.   
            end.
            assign de-tot-qtd-ger = de-tot-qtd-ger + de-qtidade-atu
                   de-acm-qtd-ger = de-acm-qtd-ger + de-qtidade-atu.

            if substr(saldo-estoq.cod-refer,3,5) = "99000" then
               assign de-acm-qtd-ret = de-acm-qtd-ret + de-qtidade-atu.
            else
            if substr(saldo-estoq.cod-refer,3,5) = "99500" then
               assign de-acm-qtd-mir = de-acm-qtd-mir + de-qtidade-atu.
            else
            if substr(saldo-estoq.cod-refer,3,5) = "99510" then
               assign de-acm-qtd-mid = de-acm-qtd-mid + de-qtidade-atu.
            else
               assign de-acm-qtd-nor = de-acm-qtd-nor + de-qtidade-atu.
        end.

        for each ped-item-res where
                 ped-item-res.it-codigo = item.it-codigo no-lock.
            assign l-acumula = no.
            if ped-item-res.faturado = yes then do:
               find nota-fiscal where
                    nota-fiscal.nome-ab-cli = ped-item-res.nome-abrev AND
                    nota-fiscal.nr-pedcli   = ped-item-res.nr-pedcli
                    no-lock no-error.

               if avail nota-fiscal then
                  if  nota-fiscal.ind-sit-nota < 5
                  and nota-fiscal.dt-cancela   = ? then
                      assign l-acumula = yes.
            end.
            ELSE 
               assign l-acumula = yes.

            if l-acumula then do:
               assign de-qt-pedida = ped-item-res.qt-pedida.
               if substr(ped-item-res.cod-refer,3,5) = "99000" then
                  assign de-tot-res-ret = de-tot-res-ret + de-qt-pedida.
              else
              if substr(ped-item-res.cod-refer,3,5) = "99500" then
                 assign de-tot-res-mir = de-tot-res-mir + de-qt-pedida.
              else
              if substr(ped-item-res.cod-refer,3,5) = "99510" then
                 assign de-tot-res-mid = de-tot-res-mid + de-qt-pedida.
              ELSE 
                 assign de-tot-res-nor = de-tot-res-nor + de-qt-pedida.

              /*------ Conversao de M para Kg ------- */
              if item.un <> "m" then do:
                 FIND item-ext WHERE ITEM-ext.it-codigo = ITEM.it-codigo
                               NO-LOCK NO-ERROR.
                 if AVAIL item-ext then
                    assign de-qt-pedida = ped-item-res.qt-pedida * 
                                          item-ext.fator-conv.
                 else do:
                    assign l-falta-fator = yes
                           de-qt-pedida = ped-item-res.qt-pedida.
                    find first tt-work where
                               tt-work.it-codigo = item.it-codigo
                               no-lock no-error.
                    if not avail tt-work then do:
                       create tt-work.
                       assign tt-work.it-codigo = item.it-codigo.
                    end.
                 end.   
              end.

              assign de-tot-res-ger = de-tot-res-ger + de-qt-pedida
                     de-acm-res-ger = de-acm-res-ger + de-qt-pedida.
              if substr(ped-item-res.cod-refer,3,5) = "99000" then
                 assign de-acm-res-ret = de-acm-res-ret + de-qt-pedida.
              else
              if substr(ped-item-res.cod-refer,3,5) = "99500" then
                 assign de-acm-res-mir = de-acm-res-mir + de-qt-pedida.
              else
              if substr(ped-item-res.cod-refer,3,5) = "99510" then
                 assign de-acm-res-mid = de-acm-res-mid + de-qt-pedida.
              else
                 assign de-acm-res-nor = de-acm-res-nor + de-qt-pedida.
            end.
        end.
    end.
    assign de-tot-sal-ret = de-tot-qtd-ret - de-tot-res-ret
           de-tot-sal-mir = de-tot-qtd-mir - de-tot-res-mir
           de-tot-sal-mid = de-tot-qtd-mid - de-tot-res-mid
           de-tot-sal-nor = de-tot-qtd-nor - de-tot-res-nor
           de-tot-sal-ger = de-tot-qtd-ger - de-tot-res-ger.
    if de-tot-qtd-ger <> 0 then do:
       assign c-it-codigo = item.it-codigo
              c-descricao = item.descricao-1.
       if de-tot-qtd-ret <> 0 then do:
          assign c-qualidade = "Retalho"
                 c-unidade   = c-un[1].
          display c-it-codigo
                  c-descricao
                  c-unidade
                  de-tot-qtd-ret
                  de-tot-res-ret
                  de-tot-sal-ret
                  c-qualidade
                  with frame f-detalhe.
          down with frame f-detalhe.
          assign c-it-codigo = ""
                 c-descricao = "".
       end.
       if de-tot-qtd-mir <> 0 then do:
          assign c-qualidade = "Miscelanea Regular"
                 c-unidade   = c-un[2].
          display c-it-codigo
                  c-descricao
                  c-unidade
                  de-tot-qtd-mir @ de-tot-qtd-ret
                  de-tot-res-mir @ de-tot-res-ret
                  de-tot-sal-mir @ de-tot-sal-ret
                  c-qualidade
                  with frame f-detalhe.
          down with frame f-detalhe.
          assign c-it-codigo = ""
                 c-descricao = "".
       end.
       if de-tot-qtd-mid <> 0 then do:
          assign c-qualidade = "Miscelanea LD"
                 c-unidade   = c-un[3].
          display c-it-codigo
                  c-descricao
                  c-unidade
                  de-tot-qtd-mid @ de-tot-qtd-ret
                  de-tot-res-mid @ de-tot-res-ret
                  de-tot-sal-mid @ de-tot-sal-ret
                  c-qualidade
                  with frame f-detalhe.
          down with frame f-detalhe.
          assign c-it-codigo = ""
                 c-descricao = "".
       end.
       if de-tot-qtd-nor <> 0 then do:
          assign c-qualidade = "Normal"
                 c-unidade   = c-un[4].
          display c-it-codigo
                  c-descricao
                  c-unidade
                  de-tot-qtd-nor @ de-tot-qtd-ret
                  de-tot-res-nor @ de-tot-res-ret
                  de-tot-sal-nor @ de-tot-sal-ret
                  c-qualidade
                  with frame f-detalhe.
          down with frame f-detalhe.
       end.
       display "Total"        @ c-descricao
               "M "           @ c-unidade
               de-tot-qtd-ger @ de-tot-qtd-ret
               de-tot-res-ger @ de-tot-res-ret
               de-tot-sal-ger @ de-tot-sal-ret
               with frame f-detalhe.
       down with frame f-detalhe.
       put skip(1).
    end.
    assign de-tot-qtd-ret = 0         de-tot-qtd-mir = 0
           de-tot-qtd-mid = 0         de-tot-qtd-nor = 0
           de-tot-qtd-ger = 0         de-tot-res-ret = 0
           de-tot-res-mir = 0         de-tot-res-mid = 0
           de-tot-res-nor = 0         de-tot-res-ger = 0.
end.

assign de-acm-sal-ret = de-acm-qtd-ret - de-acm-res-ret
      de-acm-sal-mir = de-acm-qtd-mir - de-acm-res-mir
      de-acm-sal-mid = de-acm-qtd-mid - de-acm-res-mid
      de-acm-sal-nor = de-acm-qtd-nor - de-acm-res-nor
      de-acm-sal-ger = de-acm-qtd-ger - de-acm-res-ger.
if de-acm-sal-ger <> 0 then do:
  assign c-tot-ger   = "Total Geral".
  if de-acm-qtd-ret <> 0 then do:
     assign c-qualidade = "Retalho".
     display c-tot-ger      @ c-descricao
             "M "           @ c-unidade
             de-acm-qtd-ret @ de-tot-qtd-ret
             de-acm-res-ret @ de-tot-res-ret
             de-acm-sal-ret @ de-tot-sal-ret
             c-qualidade
             with frame f-detalhe.
     down with frame f-detalhe.
     assign c-tot-ger = "".
  end.
  if de-acm-qtd-mir <> 0 then do:
     assign c-qualidade = "Miscelanea Regular".
     display c-tot-ger      @ c-descricao
             "M "           @ c-unidade
             de-acm-qtd-mir @ de-tot-qtd-ret
             de-acm-res-mir @ de-tot-res-ret
             de-acm-sal-mir @ de-tot-sal-ret
             c-qualidade
             with frame f-detalhe.
     down with frame f-detalhe.
     assign c-tot-ger = "".
  end.
  if de-acm-qtd-mid <> 0 then do:
     assign c-qualidade = "Miscelanea LD".
     display c-tot-ger      @ c-descricao
             "M "           @ c-unidade
             de-acm-qtd-mid @ de-tot-qtd-ret
             de-acm-res-mid @ de-tot-res-ret
             de-acm-sal-mid @ de-tot-sal-ret
             c-qualidade
             with frame f-detalhe.
     down with frame f-detalhe.
     assign c-tot-ger = "".
  end.
  if de-acm-qtd-nor <> 0 then do:
     assign c-qualidade = "Normal".
     display c-tot-ger      @ c-descricao
             "M "           @ c-unidade
             de-acm-qtd-nor @ de-tot-qtd-ret
             de-acm-res-nor @ de-tot-res-ret
             de-acm-sal-nor @ de-tot-sal-ret
             c-qualidade
             with frame f-detalhe.
     down with frame f-detalhe.
  end.
  display "M "           @ c-unidade
          de-acm-qtd-ger @ de-tot-qtd-ret
          de-acm-res-ger @ de-tot-res-ret
          de-acm-sal-ger @ de-tot-sal-ret
          with frame f-detalhe.
  down with frame f-detalhe.
end.
assign de-acm-qtd-ret = 0          de-acm-qtd-mir = 0
       de-acm-qtd-mid = 0          de-acm-qtd-nor = 0
       de-acm-qtd-ger = 0          de-acm-res-ret = 0
       de-acm-res-mir = 0          de-acm-res-mid = 0
       de-acm-res-nor = 0          de-acm-res-ger = 0.

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

IF tt-param.impr-param THEN DO.
   PAGE.
   PUT "*****----- PAR¶METROS ------*****"
       SKIP(1).
    
   display tt-param.c-item-ini 
           tt-param.c-item-fim
           with frame f-parlis.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

