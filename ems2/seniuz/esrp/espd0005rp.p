/* Programa: ESPD007
** Sistema.: Magnus da Datasul
** Modulo..: Pedidos
** Objetivo: Relatorio de Pedidos em Carteira
** Autor...: Sandro Wiest/Gilvando Souza Araujo
** Data....: 30/07/95
** Obs.....: Programa especifico de SANTA ELISABETH/RENASCENCA
**
** Conversao para EMS 2.04:
**   Programa: ESPD007.P  =>  ESPD0005RP.P
**   Autor...: Gilvando Souza Araujo
**   Data....: 16/01/2004
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESPD0005RP 2.04.00.000}

DEFINE TEMP-TABLE tt-param  NO-UNDO
       FIELD destino           AS INTEGER
       FIELD arquivo           AS CHAR FORMAT "x(35)"
       FIELD usuario           AS CHAR FORMAT "x(12)"
       FIELD data-exec         AS DATE
       FIELD hora-exec         AS INTEGER
       FIELD classifica        AS INTEGER
       FIELD desc-classifica   AS CHAR FORMAT "x(40)"
       FIELD ini-dt-entrega    LIKE ped-venda.dt-entrega
       FIELD fin-dt-entrega    LIKE ped-venda.dt-entrega
       FIELD ini-dt-precoant   LIKE ped-venda.dt-entrega
       FIELD fin-dt-precoant   LIKE ped-venda.dt-entrega
       FIELD ini-it-codigo     LIKE ITEM.it-codigo
       FIELD fin-it-codigo     LIKE ITEM.it-codigo
       FIELD ini-ge-codigo     LIKE ITEM.ge-codigo
       FIELD fin-ge-codigo     LIKE ITEM.ge-codigo
       FIELD ini-nome-repres   LIKE ped-venda.no-ab-reppri
       FIELD fin-nome-repres   LIKE ped-venda.no-ab-reppri
       FIELD emite-dup         AS LOG FORMAT "Sim/NÆo"
       FIELD imp-param         AS LOG.

define temp-table tt-raw-digita
       field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

def TEMP-TABLE w-work
    field it-codigo      like item.it-codigo FORMAT "x(9)"  
    field un             like item.un
    field quantidade     as dec format ">>>,>>>,>>9.99"
    field valor          as dec format ">>>,>>>,>>9.99"
    field separado       as dec format ">>>,>>>,>>9.99"
    field quantidade-ant as dec
    field qt-soma        as dec
    field qt-ant-soma    as dec
    field qt-separ-conv  as dec
    field prazo          as int
    field valor-ant      as dec
    field brasil         as log
    FIELD indigo         AS LOG
    INDEX ch-work it-codigo
                  un
                  brasil.

def TEMP-TABLE w-work2
    field un             like item.un
    field quantidade     as dec
    field quantidade-ant as dec
    field valor-ant      as dec
    field brasil         as log
    INDEX ch-work2 un
                   brasil.

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

def var de-valor-ant as dec.
def var de-valor-aux as dec.
def var i-cont-prazo as int.
def var de-quantidade-ant as dec.
def var de-tot-quantidade as dec.
def var i-tot-prazo as int.
def var i-tot-prazo-ger as int.
def var i-tot-prazo-un as int.
def var de-tot-valor as dec.
def var de-tot-separado as dec.
def var de-tot-qtd-un as dec.
def var de-tot-separado-un as dec.
def var de-tot-separado-ger as dec.
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
def var i-prazo as int format "->>9".
def var de-perc-qtd as dec format "->>9.99".
def var de-preco-medio as dec format "->,>>9.99".
def var de-preco-medio-ant like de-preco-medio.
def var de-perc-medio  as dec format "->>9.99".
def var de-perc-valor  as dec format "->>9.99".
DEF VAR l-falta-fator  AS LOG.

form 
    "*------------ Parƒmetros/Sele‡Æo -------------*" SKIP
    tt-param.ini-dt-entrega   label "Data Entrega.."
    "A"  AT 34
    tt-param.fin-dt-entrega   NO-LABELS SKIP
    tt-param.ini-dt-precoant  label "Data Pre‡o Ant"
    "A"  AT 34
    tt-param.fin-dt-precoant  no-labels SKIP
    tt-param.ini-it-codigo    label "Item.........."
    "A"  AT 34
    tt-param.fin-it-codigo    no-labels SKIP
    tt-param.ini-ge-codigo    label "Grupo Estoque."
    "A"  AT 34
    tt-param.fin-ge-codigo    no-labels SKIP
    tt-param.ini-nome-repres  label "Representante."
    "A"  AT 34
    tt-param.fin-nome-repres  NO-LABELS SKIP
    tt-param.emite-dup        LABEL "Emite Duplic.."
    skip(1)
    with FRAME f-param side-labels no-box width 132 STREAM-IO.

form  header
    "Item                                    Prazo                % Qtd" at 1
    "         Preco Var %                  % Valor                     " at 67
    "Produto   Descricao                     Medio    Quantidade  S/Uni" at 1
    "d Un     Medio S/M ant    Valor Total S/Total  Qtd Reservada      " at 67
    "------------------------------------------------------------------" at 1
    "------------------------------------------------------------------" at 67
    with no-box page-top no-labels STREAM-IO frame f-cabecalho width 132 down.

form
    w-work.it-codigo
    item.desc-item   FORMAT "x(29)"
    i-prazo
    w-work.quantidade
    de-perc-qtd
    w-work.un
    de-preco-medio
    de-perc-medio
    w-work.valor
    de-perc-valor
    w-work.separado
    with no-box 55 down width 132 STREAM-IO NO-LABEL frame f-detalhe.

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
{utp/ut-liter.i Resumo_de_Pedidos_de_Vendas_em_Carteira * r}
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

FOR EACH ped-venda WHERE ped-venda.dt-entrega   >= tt-param.ini-dt-precoant
                     AND ped-venda.dt-entrega   <= tt-param.fin-dt-entrega
                     AND ped-venda.no-ab-reppri >= tt-param.ini-nome-repres
                     AND ped-venda.no-ab-reppri <= tt-param.fin-nome-repres
                   NO-LOCK,
    EACH ped-item OF ped-venda
         WHERE ped-item.cod-sit-item < 3 OR
               ped-item.cod-sit-item = 5
         NO-LOCK,
    FIRST natur-oper
         WHERE natur-oper.nat-operacao = ped-item.nat-operacao
           AND ((tt-param.emite-dup = YES AND natur-oper.emite-duplic = YES) OR
                (tt-param.emite-dup = NO))
         NO-LOCK,
    FIRST ITEM WHERE ITEM.it-codigo = ped-item.it-codigo
                AND ITEM.it-codigo >= tt-param.ini-it-codigo
                AND ITEM.it-codigo <= tt-param.fin-it-codigo
                AND ITEM.ge-codigo >= tt-param.ini-ge-codigo
                AND ITEM.ge-codigo <= tt-param.fin-ge-codigo
              NO-LOCK:
    
    assign i-prazo = 0.

    run pi-acompanhar in h-acomp (INPUT ped-venda.nr-pedcli + "-" + ped-venda.nome-abrev).

    find first emitente
         where emitente.cod-emit = ped-venda.cod-emit
         no-lock no-error.

    assign l-brasil = if emitente.pais = "brasil" then yes
                      else no.
    assign de-quantidade = ped-item.qt-pedida -
                           ped-item.qt-pendente -
                           ped-item.qt-atendida
                           + ped-item.qt-devolvida.
    if de-quantidade <= 0 then next.

    find first w-work
         where w-work.it-codigo = item.it-codigo
           and w-work.un        = item.un
           and w-work.brasil    = l-brasil
               no-error.
    if not avail w-work then do:
       FIND item-ext WHERE item-ext.it-codigo = item.it-codigo
            NO-LOCK NO-ERROR.
       create w-work.
       assign w-work.quantidade = 0
              w-work.un         = item.un
              w-work.valor      = 0
              w-work.separado   = 0
              w-work.valor-ant  = 0
              w-work.it-codigo  = ITEM.it-codigo
              w-work.brasil     = l-brasil
              w-work.indigo     = IF (AVAIL item-ext AND
                                     item-ext.indigo = YES) THEN YES
                                  ELSE NO.
    end.
    for each ped-item-res
        where ped-item-res.it-codigo    = item.it-codigo
          and ped-item-res.nr-sequencia = ped-item.nr-sequencia
          and ped-item-res.nr-pedcli    = ped-item.nr-pedcli
          and ped-item-res.nome-abrev   = ped-item.nome-abrev
          and ped-item-res.faturado     = no
              no-lock:
        if  ped-item.dt-entrega >= tt-param.ini-dt-entrega
        and ped-item.dt-entrega <= tt-param.fin-dt-entrega then do:
            assign w-work.separado = w-work.separado + ped-item-res.qt-pedida.

            /*------ Conversao de M para Kg ------- */
            if item.un <> "m" then do:
               FIND item-ext WHERE ITEM-ext.it-codigo = ITEM.it-codigo
                             NO-LOCK NO-ERROR.
               if AVAIL item-ext then
                  assign de-qt-conv = ped-item-res.qt-pedida * 
                                      item-ext.fator-conv.
               else do:
                  assign l-falta-fator = yes
                         de-qt-conv    = ped-item-res.qt-pedida.
                  find first tt-work where
                             tt-work.it-codigo = item.it-codigo
                             no-lock no-error.
                  if not avail tt-work then do:
                     create tt-work.
                     assign tt-work.it-codigo = item.it-codigo.
                  end.
               end.   
            end.
            ELSE
               ASSIGN de-qt-conv = ped-item-res.qt-pedida.

            assign w-work.qt-separ-conv = w-work.qt-separ-conv + de-qt-conv.
        END.
    end.
    if  ped-item.dt-entrega >= tt-param.ini-dt-precoant
    and ped-item.dt-entrega <= tt-param.fin-dt-precoant then do:
        assign w-work.valor-ant = w-work.valor-ant +
                                  (ped-item.vl-preori * de-quantidade)
               w-work.quantidade-ant = w-work.quantidade-ant + de-quantidade.

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
        ELSE
           ASSIGN de-qt-conv = de-quantidade.

        assign w-work.qt-ant-soma = w-work.qt-ant-soma + de-qt-conv.
        next.
    end.
    
    assign i-prazo      = 0
           i-cont-prazo = 0.
    if  ped-venda.cod-cond-pag <> 0 then do.
        find cond-pagto where cond-pagto.cod-cond-pag =
             ped-venda.cod-cond-pag
             no-lock.
        if avail cond-pagto then do.
           do i-cont = 1 to 12.
              if  cond-pagto.prazos[i-cont] <> ?
              and cond-pagto.prazos[i-cont] <> 0 then
                  assign i-cont-prazo = i-cont-prazo + 1.
           end.
        end.
    end.
    else do.
        find con-pges where con-pges.nr-pedido =
                            ped-venda.nr-pedido no-lock
                            no-error.
        if  avail con-pges then do.
            do  i-cont = 1 to 6.
                if  (con-pges.nr-dias-venc[i-cont] <> 0
                and con-pges.nr-dias-venc[i-cont] <> ?)
                or  con-pges.data-pagto[i-cont] <> ? then
                    assign i-cont-prazo = i-cont-prazo + 1.
            end.
        end.
    end.
    assign de-valor-aux = (ped-item.vl-preori *
                          de-quantidade) / i-cont-prazo.

    if  ped-venda.cod-cond-pag <> 0 then do.
        find cond-pagto where cond-pagto.cod-cond-pag =
             ped-venda.cod-cond-pag
             no-lock.
        if avail cond-pagto then do.
           do i-cont = 1 to i-cont-prazo.
              assign w-work.prazo = w-work.prazo +
                                 (cond-pagto.prazos[i-cont]
                                 * de-valor-aux).
           end.
        end.
    end.
    else do.
        find con-pges where con-pges.nr-pedido =
                            ped-venda.nr-pedido no-lock
                            no-error.
        if  avail con-pges then do.
            do  i-cont = 1 to i-cont-prazo.
                if  con-pges.nr-dias-venc[i-cont] <> 0
                and con-pges.nr-dias-venc[i-cont] <> ? then do.
                    assign w-work.prazo = w-work.prazo +
                              (con-pges.nr-dias-venc[i-cont]
                              * de-valor-aux).
                end.
                else do.
                    if  con-pges.data-pagto[i-cont] <> ? then
                        assign w-work.prazo = w-work.prazo +
                                   ((con-pges.data-pagto[i-cont]
                                - ped-venda.dt-emissao) *
                                de-valor-aux).
                end.
            end.
        end.
     end.

     assign w-work.quantidade = w-work.quantidade +
                                de-quantidade
            w-work.valor = w-work.valor + (ped-item.vl-preori
                           * de-quantidade)
            w-work.brasil = (if  emitente.pais = "brasil" then
                               yes
                             else no).

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
     ELSE
        ASSIGN de-qt-conv = de-quantidade.
     
     assign w-work.qt-soma = w-work.qt-soma + de-qt-conv.
end.


for each w-work.
    find first w-work2 where w-work2.un     = w-work.un
                         and w-work2.brasil = w-work.brasil 
                       no-error.
    if  not avail w-work2 then do.
        create w-work2.
        assign w-work2.un             = w-work.un
               w-work2.quantidade     = 0
               w-work2.quantidade-ant = 0
               w-work2.valor-ant      = 0
               w-work2.brasil         = w-work.brasil.
    end.
    assign w-work2.quantidade     = w-work2.quantidade +
                                    w-work.quantidade
           w-work2.quantidade-ant = w-work2.quantidade-ant +
                                    w-work.quantidade-ant
           w-work2.valor-ant      = w-work2.valor-ant +
                                    w-work.valor-ant
           w-work2.brasil         = w-work.brasil.
end.

put " " skip(1) 
    "V e n d a s  M e r c a d o  I n t e r n o" at 45 skip(1).

{esinc/espd0005.i "yes" "no"} 

put "" skip(2)
    "V e n d a s  M e r c a d o  E x t e r n o" at 45 skip(1).

{esinc/espd0005.i "no" "no"}

put " " skip(2).

assign de-preco-medio = de-tot-valor / de-tot-quantidade.
if  de-preco-medio = ? then
    assign de-preco-medio = 0.
assign de-preco-medio-ant = de-tot-valor-ant / de-tot-quantidade-ant.
if de-preco-medio-ant = ? then
   assign de-preco-medio-ant = 0.
assign de-perc-medio = ((de-preco-medio / de-preco-medio-ant) - 1) * 100.
if de-perc-medio = ? then
   assign de-perc-medio = 0.
assign i-prazo = i-tot-prazo / de-tot-valor.
if i-prazo = ? then
   assign i-prazo = 0.
display "Total Geral da Carteira" @ item.desc-item
        de-tot-quantidade @ w-work.quantidade
        i-prazo
        de-preco-medio
        de-perc-medio
        de-tot-valor      @ w-work.valor
        de-tot-separado   @ w-work.separado
        with frame f-detalhe.

down with frame f-detalhe.

put " " skip(2)
    "V e n d a s   d e   I n d i g o" at 50 skip(1).

{esinc/espd0005.i "no" "yes"}

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
           tt-param.ini-dt-precoant
           tt-param.fin-dt-precoant
           tt-param.ini-it-codigo
           tt-param.fin-it-codigo
           tt-param.ini-ge-codigo
           tt-param.fin-ge-codigo
           tt-param.ini-nome-repres
           tt-param.fin-nome-repres
           tt-param.emite-dup
           with frame f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

