/* Programa: ESPD044
** Sistema.: Magnus da Datasul
** Modulo..: Pedidos
** Objetivo: Relatorio Resumo do Faturamento
** Autor...: Sandro Wiest/Gilvando Souza Araujo
** Data....: 30/07/95
** Obs.....: Programa especifico de SANTA ELISABETH/RENASCENCA
**
** Conversao para EMS 2.04:
**   Programa: ESPD044.P  =>  ESFT0029RP.P
**   Autor...: FµBIO COELHO LANZA
**   Data....: 30/01/2004
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESFT0029RP 2.04.00.000}

define temp-table tt-param  no-undo
       field destino           as integer
       field arquivo           as char format "x(35)"
       field usuario           as char format "x(12)"
       field data-exec         as date
       field hora-exec         as integer
       field classifica        as integer
       field desc-classifica   as char format "x(40)"
       FIELD cod-estabel       like nota-fiscal.cod-estabel 
       FIELD ini-dt-emissao    LIKE nota-fiscal.dt-emis-nota
       FIELD fin-dt-emissao    LIKE nota-fiscal.dt-emis-nota
       FIELD ini-dt-precoant   LIKE nota-fiscal.dt-emis-nota
       FIELD fin-dt-precoant   LIKE nota-fiscal.dt-emis-nota
       FIELD ini-it-codigo     LIKE item.it-codigo
       FIELD fin-it-codigo     LIKE item.it-codigo
       FIELD ini-ge-codigo     LIKE ITEM.ge-codigo
       FIELD fin-ge-codigo     LIKE ITEM.ge-codigo
       FIELD all-types         AS LOG FORMAT "Sim/NÆo"
       FIELD tp-pedido1        AS CHAR FORMAT "x(2)"
       FIELD tp-pedido2        AS CHAR FORMAT "x(2)"
       FIELD tp-pedido3        AS CHAR FORMAT "x(2)"
       FIELD tp-pedido4        AS CHAR FORMAT "x(2)"
       FIELD tp-pedido5        AS CHAR FORMAT "x(2)"
       FIELD tp-normal         AS LOG FORMAT "Sim/NÆo"
       FIELD qualidade         AS INTEGER
       FIELD desc-qualidade    AS CHAR FORMAT "x(10)"
       FIELD imp-param         AS LOG.

define temp-table tt-raw-digita
       field raw-digita as raw.

def TEMP-TABLE tt-work  /* Itens sem fator de conversao */
    field it-codigo like item.it-codigo
    INDEX ch-work it-codigo.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

def TEMP-TABLE w-work
    field it-codigo      like item.it-codigo
    field aliquota-icm   like it-nota-fisc.aliquota-icm
    field un             like item.un
    field quantidade     as dec format ">>>,>>>,>>9.99"
    field valor          as dec format ">>>,>>>,>>9.99"
    field quantidade-ant as dec
    field qt-soma        as dec
    field qt-ant-soma    as dec
    field prazo          as int
    field valor-ant      as dec
    field brasil         as log
    INDEX ch-work it-codigo
                  un
                  brasil.

def TEMP-TABLE w-work2
    field aliquota-icm   like it-nota-fisc.aliquota-icm
    field un             like item.un
    field quantidade     as dec
    field quantidade-ant as dec
    field valor-ant      as dec
    field brasil         as log
    INDEX ch-work2 un
                   brasil.

create tt-param.
raw-transfer raw-param to tt-param.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* include padrÆo para impressÆo de campos editores em relat¢rio  */
{include/tt-edit.i}

/* defini‡Æo de vari veis  */
def var h-acomp as handle no-undo.

def var de-valor-ant as dec.
def var de-valor-aux as dec.
def var de-quantidade-ant as dec.
def var de-tot-quantidade as dec.
def var de-tot-valor as dec.
def var de-tot-qtd-un as dec.
def var de-tot-qtd-ger as dec.
def var de-tot-valor-un as dec.
def var de-tot-valor-ger as dec.
def var de-tot-valor-ant as dec.
def var de-tot-quantidade-ant as dec.
def var l-brasil as log.
def var de-vl-total as dec.
def var de-quantidade as dec.
def var de-qt-conv as dec.
def var i-cont as int.
def var i-contador as int.
def var i-prazo as int format "->>9".
def var i-tot-prazo as int.
def var i-tot-prazo-un as int.
def var i-tot-prazo-ger as int.
def var de-perc-qtd as dec format "->>9.99".
def var de-preco-medio as dec format "->,>>9.99".
def var de-preco-medio-ant like de-preco-medio.
def var de-perc-medio  as dec format "->>9.99".
def var de-perc-valor  as dec format "->>9.99".
DEF VAR l-falta-fator    AS LOG.

form 
    "*------------ Parƒmetros/Sele‡Æo -------------*" SKIP
    tt-param.cod-estabel      LABEL "Estabelec....." SKIP
    tt-param.ini-dt-emissao   label "Data Emissao.."
    "A"  AT 34
    tt-param.fin-dt-emissao   NO-LABELS SKIP
    tt-param.ini-dt-precoant  label "Data Pre‡o Ant"
    "A"  AT 34
    tt-param.fin-dt-precoant  no-labels SKIP
    tt-param.ini-it-codigo    label "Item.........."
    "A"  AT 34
    tt-param.fin-it-codigo    no-labels SKIP
    tt-param.ini-ge-codigo    label "Grupo Estoque."
    "A"  AT 34
    tt-param.fin-ge-codigo    no-labels SKIP
    tt-param.all-types        LABEL "Todos Tipos..."
    tt-param.tp-pedido1       NO-LABELS AT 22
    tt-param.tp-pedido2       NO-LABELS AT 25
    tt-param.tp-pedido3       NO-LABELS AT 28
    tt-param.tp-pedido4       NO-LABELS AT 31
    tt-param.tp-pedido5       NO-LABELS AT 34 
    tt-param.tp-normal        LABEL "Tipo Normal..." AT 1
    tt-param.desc-qualidade   LABEL "Qualidade....." AT 1
    skip(1)
    with FRAME f-param side-labels no-box width 132 STREAM-IO.

form  header
    "Item                                                 Prazo" at  1
    "% Qtd        Preco Var %                  % Valor"          at 77
    "Produto   Descricao                                  Medio" at  1
    "Quantidade  S/Unid Un     Medio S/M ant    Valor Total S/Total  %ICMS" at 64
    "------------------------------------------------------------------" at  1
    "------------------------------------------------------------------" at 67
    with no-box page-top no-labels STREAM-IO frame f-cabecalho width 132 down.

form
    w-work.it-codigo FORMAT "x(9)" 
    item.desc-item   FORMAT "x(43)"  
    i-prazo                         
    w-work.quantidade               
    de-perc-qtd
    w-work.un
    de-preco-medio
    de-perc-medio
    w-work.valor
    de-perc-valor                  
    with no-box 55 down width 132 STREAM-IO frame f-detalhe NO-LABEL.

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
{utp/ut-liter.i Resumo_do_Faturamento_Por_Aliquota_de_ICMS * r}
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

for each w-work2.
    delete w-work2.
end.

for each item where
         item.it-codigo >= tt-param.ini-it-codigo and
         item.it-codigo <= tt-param.fin-it-codigo and
         item.ge-codigo >= tt-param.ini-ge-codigo and
         item.ge-codigo <= tt-param.fin-ge-codigo 
    NO-LOCK:

    run pi-acompanhar in h-acomp (input item.it-codigo).
   
    ASSIGN i-prazo = 0.
    for each it-nota-fisc
        where it-nota-fisc.cod-estabel  >= tt-param.cod-estabel
          and it-nota-fisc.it-codigo    =  item.it-codigo
          and it-nota-fisc.dt-emis-nota >= tt-param.ini-dt-emissao
          and it-nota-fisc.dt-emis-nota <= tt-param.fin-dt-emissao
          and it-nota-fisc.dt-cancela   =  ? NO-LOCK,
        FIRST nota-fiscal 
          where nota-fiscal.cod-estabel = it-nota-fisc.cod-estabel
            and nota-fiscal.nr-nota-fis = it-nota-fisc.nr-nota-fis
            and nota-fiscal.serie       = it-nota-fisc.serie
            AND nota-fiscal.emite-dup   = YES
          no-lock:
       
       FIND ped-venda 
          WHERE ped-venda.nome-abrev = nota-fiscal.nome-ab-cli
            AND ped-venda.nr-pedcli  = nota-fiscal.nr-pedcli
          NO-LOCK NO-ERROR.
       IF tt-param.all-types = NO THEN DO:
          IF ped-venda.tp-pedido <> tt-param.tp-pedido1 AND
             ped-venda.tp-pedido <> tt-param.tp-pedido2 AND
             ped-venda.tp-pedido <> tt-param.tp-pedido3 AND
             ped-venda.tp-pedido <> tt-param.tp-pedido4 AND
             ped-venda.tp-pedido <> tt-param.tp-pedido5 AND
             ped-venda.tp-pedido <> "" THEN NEXT.
          IF ped-venda.tp-pedido = "" AND tt-param.tp-normal = NO THEN NEXT.
       END.

       FIND ped-item WHERE ped-item.nome-abrev   = it-nota-fisc.nome-ab-cli
                       AND ped-item.nr-pedcli    = it-nota-fisc.nr-pedcli
                       AND ped-item.nr-sequencia = it-nota-fisc.nr-seq-ped
                     NO-LOCK NO-ERROR.
       IF AVAIL ped-item THEN DO:
          FIND ped-item-ext OF ped-item NO-LOCK NO-ERROR.
          IF AVAIL ped-item-ext THEN
             IF (SUBSTR(ped-item-ext.lote,2,1) = "p" AND tt-param.qualidade = 2) OR
                (SUBSTR(ped-item-ext.lote,2,1) = "d" AND tt-param.qualidade = 1) THEN NEXT.
       END.

       find natur-oper where natur-oper.nat-operacao =
          it-nota-fisc.nat-operacao no-lock no-error.
       if not avail natur-oper then
          next.
       if natur-oper.emite-duplic = no THEN next.
       find first emitente where
                  emitente.nome-abrev = it-nota-fisc.nome-ab-cli no-lock.

       ASSIGN l-brasil = IF emitente.pais = "brasil"
                         THEN yes
                         ELSE no.
       find first w-work
            where w-work.it-codigo    = item.it-codigo
              and w-work.aliquota-icm = it-nota-fisc.aliquota-icm
              and w-work.un           = item.un
              and w-work.brasil       = l-brasil no-error.
       if not avail w-work then do:
          create w-work.
          assign w-work.quantidade   = 0
                 w-work.aliquota-icm = it-nota-fisc.aliquota-icm
                 w-work.un           = item.un
                 w-work.valor        = 0
                 w-work.valor-ant    = 0
                 w-work.qt-soma      = 0
                 w-work.qt-ant-soma  = 0
                 w-work.it-codigo    = item.it-codigo
                 w-work.brasil       = l-brasil.
       end.
       if  it-nota-fisc.dt-emis-nota <= tt-param.ini-dt-precoant
       and it-nota-fisc.dt-emis-nota >= tt-param.fin-dt-precoant then do:
           assign w-work.valor-ant      = w-work.valor-ant + it-nota-fisc.vl-tot-item
                  w-work.quantidade-ant = w-work.quantidade-ant + 
                                          it-nota-fisc.qt-faturada[1].

           /*------ Conversao de M para Kg ------- */
           if it-nota-fisc.un-fatur[1] <> "m" then do:
              FIND item-ext WHERE ITEM-ext.it-codigo = it-nota-fisc.it-codigo
                            NO-LOCK NO-ERROR.
              if AVAIL item-ext then
                 assign de-qt-conv = it-nota-fisc.qt-faturada[1] * item-ext.fator-conv.
              else do:
                 assign l-falta-fator = yes
                        de-qt-conv    = it-nota-fisc.qt-faturada[1].
                 find first tt-work where
                            tt-work.it-codigo = it-nota-fisc.it-codigo
                            no-lock no-error.
                 if not avail tt-work then do:
                    create tt-work.
                    assign tt-work.it-codigo = it-nota-fisc.it-codigo.
                 end.
              end.   
           end.
           ELSE
              ASSIGN de-qt-conv = it-nota-fisc.qt-faturada[1].

           assign w-work.qt-ant-soma = w-work.qt-ant-soma + de-qt-conv.
       end.
       if it-nota-fisc.dt-emis-nota < tt-param.ini-dt-precoant then
          next.
       assign de-quantidade = it-nota-fisc.qt-faturada[1].
       assign i-prazo    = 0
              i-contador = 0.
       for each fat-duplic 
           where fat-duplic.cod-estab = it-nota-fisc.cod-estab
             and fat-duplic.serie     = it-nota-fisc.serie
             and fat-duplic.nr-fatura = it-nota-fisc.nr-nota-fis
           no-lock.
           assign i-contador = i-contador + 1.
       end.
       ASSIGN de-valor-aux = it-nota-fisc.vl-tot-item / i-contador.

       for each fat-duplic 
           where fat-duplic.cod-estab = it-nota-fisc.cod-estab
             and fat-duplic.serie     = it-nota-fisc.serie
             and fat-duplic.nr-fatura = it-nota-fisc.nr-nota-fis
           no-lock.
           assign i-prazo = i-prazo + ((fat-duplic.dt-vencimen -
                                        fat-duplic.dt-emissao) *
                                        de-valor-aux).
       end.
       assign w-work.prazo      = w-work.prazo + i-prazo
              w-work.quantidade = w-work.quantidade + de-quantidade
              w-work.valor      = w-work.valor + it-nota-fisc.vl-tot-item
              w-work.brasil     = (if  emitente.pais = "brasil" THEN yes
                                   else no).

       /*------ Conversao de M para Kg ------- */
       if it-nota-fisc.un-fatur[1] <> "m" then do:
          FIND item-ext WHERE ITEM-ext.it-codigo = it-nota-fisc.it-codigo
                        NO-LOCK NO-ERROR.
          if AVAIL item-ext then
             assign de-qt-conv = it-nota-fisc.qt-faturada[1] * 
                                 item-ext.fator-conv.
          else do:
             assign l-falta-fator = yes
                    de-qt-conv    = it-nota-fisc.qt-faturada[1].
             find first tt-work where
                        tt-work.it-codigo = it-nota-fisc.it-codigo
                        no-lock no-error.
             if not avail tt-work then do:
                create tt-work.
                assign tt-work.it-codigo = it-nota-fisc.it-codigo.
             end.
          end.   
       end.
       ELSE
          ASSIGN de-qt-conv = it-nota-fisc.qt-faturada[1].

       assign w-work.qt-soma = w-work.qt-soma + de-qt-conv.
    end.
end.

for each w-work.
    find first w-work2 where w-work2.aliquota-icm = w-work.aliquota-icm
                        AND w-work2.un           = w-work.un
                        and w-work2.brasil       = w-work.brasil
                            no-error.
    if not avail w-work2 then do:
      create w-work2.
      assign w-work2.aliquota-icm   = w-work.aliquota-icm
             w-work2.un             = w-work.un
             w-work2.quantidade     = 0
             w-work2.quantidade-ant = 0
             w-work2.valor-ant      = 0
             w-work2.brasil         = w-work.brasil.
    end.
    assign w-work2.quantidade     = w-work2.quantidade     + w-work.quantidade
           w-work2.quantidade-ant = w-work2.quantidade-ant + w-work.qt-ant-soma
           w-work2.valor-ant      = w-work2.valor-ant      + w-work.valor-ant
           w-work2.brasil         = w-work.brasil.
end.

put " " skip(1)
   "V e n d a s  M e r c a d o  I n t e r n o" at 45 skip(1).

{esinc/esft0029.i "yes"}

put " " skip(2)
    "V e n d a s  M e r c a d o  E x t e r n o" at 45 skip(1).

{esinc/esft0029.i "no"}

put " " skip(2).

assign de-preco-medio = de-tot-valor / de-tot-quantidade.
if de-preco-medio = ? then
   assign de-preco-medio = 0.
assign de-preco-medio-ant = de-tot-valor-ant / de-tot-quantidade-ant.
if de-preco-medio-ant = ? then
   assign de-preco-medio-ant = 0.
assign de-perc-medio = ((de-preco-medio / de-preco-medio-ant) - 1) * 100.
if de-perc-medio = ? then
   assign de-perc-medio = 0.
assign i-prazo = i-tot-prazo / de-tot-valor.
display "Total Geral"     @ item.desc-item
        i-prazo
        de-tot-quantidade @ w-work.quantidade
        de-preco-medio
        de-perc-medio
        de-tot-valor      @ w-work.valor
        with frame f-detalhe.
down with frame f-detalhe.

assign de-tot-quantidade     = 0
       i-tot-prazo           = 0
       de-tot-valor          = 0
       de-tot-valor-ant      = 0
       de-tot-quantidade-ant = 0.

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
   DISPLAY tt-param.cod-estabel
           tt-param.ini-dt-emissao
           tt-param.fin-dt-emissao
           tt-param.ini-dt-precoant
           tt-param.fin-dt-precoant
           tt-param.ini-it-codigo
           tt-param.fin-it-codigo
           tt-param.ini-ge-codigo
           tt-param.fin-ge-codigo
           tt-param.all-types
           tt-param.tp-pedido1
           tt-param.tp-pedido2
           tt-param.tp-pedido3
           tt-param.tp-pedido4
           tt-param.tp-pedido5
           tt-param.tp-normal
           tt-param.desc-qualidade
           with frame f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

