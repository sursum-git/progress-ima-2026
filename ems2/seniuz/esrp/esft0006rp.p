/* Programa: ESFT002.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Faturamento
** Objetivo: Gerar o relatorio Distribuicao de Vendas.
** Autor...: Sandro Wiest/Gilvando de Souza Araujo - Junho/95
** Obs.....: Especifico da CIA.RENASCENCA INDUSTRIAL
**
** Conversao para EMS 2.04:
**   Programa: ESFT002.P  =>  ESFT0006RP.P
**   Autor...: Gilvando Souza Araujo
**   Data....: 20/01/2004
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESFT0006RP 2.04.00.000}

DEFINE temp-table tt-param  no-undo
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
       FIELD ini-ge-codigo     LIKE ITEM.ge-codigo
       FIELD fin-ge-codigo     LIKE ITEM.ge-codigo
       FIELD imp-param         AS LOG.

define temp-table tt-raw-digita
       field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

def TEMP-TABLE w-aux
    field ge-codigo like item.ge-codigo
    field cod-gr-cli like emitente.cod-gr-cli
    field estado like emitente.estado
    field quantidade as dec format ">>>,>>>,>>9.99" label "Quantidade"
    field quantm2    as dec format ">>>,>>>,>>9.99" label "Qtidade M2"
    field valor      as dec format ">>>,>>>,>>9.99" label "VL Total"
    INDEX ch-aux ge-codigo
                 cod-gr-cli.

def TEMP-TABLE w-aux2
    field ge-codigo like item.ge-codigo
    field quantidade as dec format ">>>,>>>,>>9.99" label "Quantidade"
    INDEX ch-aux2 ge-codigo.

def TEMP-TABLE w-aux3
    field ge-codigo like item.ge-codigo
    field cod-gr-cli like emitente.cod-gr-cli
    field quantidade as dec format ">>>,>>>,>>9.99" label "Quantidade"
    INDEX ch-aux3 ge-codigo
                  cod-gr-cli.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* include padrÆo para impressÆo de campos editores em relat¢rio  */
{include/tt-edit.i}

/* defini‡Æo de vari veis  */
def var h-acomp as handle no-undo.

def TEMP-TABLE tt-work  /* Itens sem fator de conversao */
    field it-codigo like item.it-codigo
    INDEX ch-work it-codigo.

def var de-quantidade      as dec   format ">>,>>>,>>9.99" label "Quantidade".
def var de-quantm2         as dec   format ">>,>>>,>>9.99" label "Qtidade M2".
def var de-valor           as dec   format ">>,>>>,>>9.99" label "VL Total".
def var de-perc-gru        as dec   format ">>>9.99"        label "Perc/Gr".
def var de-perc-cli        as dec   format ">>9.99"        label "Perc".
def var de-perc-tot        as dec   format ">>>>9.99"        label "Perc/Tot".

def var de-quantidade-cli  as dec   format ">>,>>>,>>9.99" label "Quantidade".
def var de-quantm2-cli     as dec   format ">>,>>>,>>9.99" label "Qtidade M2".
def var de-valor-cli       as dec   format ">>,>>>,>>9.99" label "VL Total".

def var de-quantidade-gru  as dec   format ">>,>>>,>>9.99" label "Quantidade".
def var de-quantm2-gru     as dec   format ">>,>>>,>>9.99" label "Qtidade M2".
def var de-valor-gru       as dec   format ">>,>>>,>>9.99" label "VL Total".

def var de-quantidade-ger  as dec   format ">>,>>>,>>9.99" label "Quantidade".
def var de-quantm2-ger     as dec   format ">>,>>>,>>9.99" label "Qtidade M2".
def var de-valor-ger       as dec   format ">>,>>>,>>9.99" label "VL Total".
DEF VAR l-falta-fator      AS LOG.

form 
    "*------------ Parƒmetros/Sele‡Æo -------------*" SKIP
    tt-param.cod-estabel      LABEL "Estabelec....." SKIP
    tt-param.ini-dt-emissao   label "Data Emissao.."
    "A"  AT 30
    tt-param.fin-dt-emissao   NO-LABELS SKIP
    tt-param.ini-ge-codigo    LABEL "Grupo Estoque."
    "A"  AT 30
    tt-param.fin-ge-codigo    NO-LABELS
    skip(1)
    with FRAME f-param side-labels no-box width 132 STREAM-IO.

form header
     "Grupo Descricao                   UF       Quantidade   "   AT  1
     "Qtidade M2       VL Total   Perc Perc/Gr Perc/Tot"          AT 59
     "----- --------------------------- -- ---------------- ----" AT  1
     "---------- -------------- ------ ------- --------"          AT 59
     with width 132 STREAM-IO no-labels no-box frame f-cabecalho page-top.

form
    w-aux.cod-gr-cli
    gr-cli.descricao
    w-aux.estado
    w-aux.quantidade
    w-aux.quantm2
    w-aux.valor
    de-perc-cli
    de-perc-gru
    de-perc-tot
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
{utp/ut-liter.i Distribui‡Æo_de_Vendas * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
VIEW FRAME f-cabecalho.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

for each w-aux.
    delete w-aux.
end.
for each w-aux2.
    delete w-aux2.
end.
for each w-aux3.
    delete w-aux3.
end.
for each nota-fiscal where nota-fiscal.cod-estabel =  tt-param.cod-estabel
                     and nota-fiscal.dt-emis-nota  >= tt-param.ini-dt-emissao
                     and nota-fiscal.dt-emis-nota  <= tt-param.fin-dt-emissao
                     and nota-fiscal.dt-cancela    =  ?
                     AND nota-fiscal.emite-dup     =  YES
                         no-lock:

    run pi-acompanhar in h-acomp (input nota-fiscal.nr-nota-fis).

    find first emitente where emitente.cod-emit = nota-fiscal.cod-emit
                        no-lock.
    for each it-nota-fisc of nota-fiscal
             where it-nota-fisc.ind-sit-nota <> 4
                   no-lock.
        find natur-oper where
             natur-oper.nat-operacao = it-nota-fisc.nat-operacao
                             no-lock no-error.
        if not avail natur-oper then
           next.
        if natur-oper.emite-duplic = no then
           next.
        find item where item.it-codigo = it-nota-fisc.it-codigo
             no-lock no-error.
        if not avail item then do:
           message it-nota-fisc.it-codigo.
           pause.
        end.
        if item.ge-codigo < tt-param.ini-ge-codigo
        or item.ge-codigo > tt-param.fin-ge-codigo then
           next.

        /*------ Conversao de M para Kg ------- */
        if it-nota-fisc.un-fatur[1] <> "m" then do:
           FIND item-ext WHERE ITEM-ext.it-codigo = it-nota-fisc.it-codigo
                         NO-LOCK NO-ERROR.
           if AVAIL item-ext then
              assign de-quantidade = it-nota-fisc.qt-faturada[1] * 
                                     item-ext.fator-conv.
           else do:
              assign l-falta-fator = yes
                     de-quantidade = it-nota-fisc.qt-faturada[1].
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
           ASSIGN de-quantidade = it-nota-fisc.qt-faturada[1].

        find first w-aux where w-aux.ge-codigo  = item.ge-codigo
                           and w-aux.cod-gr-cli = emitente.cod-gr-cli
                           and w-aux.estado     = emitente.estado
                           no-error.
        if  not avail w-aux then do:
            create w-aux.
            assign w-aux.ge-codigo  = item.ge-codigo
                   w-aux.cod-gr-cli = emitente.cod-gr-cli
                   w-aux.estado     = emitente.estado
                   w-aux.quantidade = 0.
        end.
        
        find item-ext
             where item-ext.it-codigo = item.it-codigo
                   no-lock no-error.
        if  avail item-ext then
            assign de-quantm2 = de-quantidade * item-ext.largura.
        else
            assign de-quantm2 = de-quantidade.
        assign w-aux.quantidade  = w-aux.quantidade + de-quantidade
               w-aux.quantm2     = w-aux.quantm2 + de-quantm2
               w-aux.valor       = w-aux.valor +
                                   it-nota-fisc.vl-tot-item
               de-quantidade-ger = de-quantidade-ger + de-quantidade
               de-quantm2-ger    = de-quantm2-ger + de-quantm2
               de-valor-ger      = de-valor-ger +
                                   it-nota-fisc.vl-tot-item.
        find first w-aux2 where w-aux2.ge-codigo = item.ge-codigo
             no-error.
        if not avail w-aux2 then do:
           create w-aux2.
           assign w-aux2.ge-codigo  = item.ge-codigo
                  w-aux2.quantidade = 0.
        end.
        assign w-aux2.quantidade = w-aux2.quantidade + de-quantidade.
        find first w-aux3 where w-aux3.ge-codigo = item.ge-codigo
                            and w-aux3.cod-gr-cli = emitente.cod-gr-cli
             no-error.
        if not avail w-aux3 then do:
           create w-aux3.
           assign w-aux3.ge-codigo  = item.ge-codigo
                  w-aux3.cod-gr-cli = emitente.cod-gr-cli
                  w-aux3.quantidade = 0.
        end.
        assign w-aux3.quantidade = w-aux3.quantidade + de-quantidade.
    end.
end.
for each w-aux break by w-aux.ge-codigo
                     by w-aux.cod-gr-cli
                     by w-aux.estado.
    if  first-of(w-aux.ge-codigo) then do.
        find grup-estoq where grup-estoq.ge-codigo = w-aux.ge-codigo
             no-lock.
        put "Grupo de Estoque: " w-aux.ge-codigo " - "
            grup-estoq.descricao skip(1).
        find first w-aux2 where w-aux2.ge-codigo = w-aux.ge-codigo
            no-error.
    end.
    if  first-of(w-aux.cod-gr-cli) then do.
        find gr-cli where gr-cli.cod-gr-cli = w-aux.cod-gr-cli no-lock.
        disp w-aux.cod-gr-cli
             gr-cli.descricao
             with frame f-detalhe.
        find first w-aux3 where w-aux3.ge-codigo = w-aux.ge-codigo
                            and w-aux3.cod-gr-cli = w-aux.cod-gr-cli.
    end.
    assign de-perc-gru = (w-aux.quantidade * 100) / w-aux2.quantidade
           de-perc-cli = (w-aux.quantidade * 100) / w-aux3.quantidade
           de-perc-tot = (w-aux.quantidade * 100) / de-quantidade-ger.
    if  w-aux2.quantidade = 0 then
        assign de-perc-gru = 100.
    if  w-aux3.quantidade = 0 then
        assign de-perc-cli = 100.
    if  de-quantidade-ger = 0 then
        assign de-perc-tot = 100.
    DISPLAY w-aux.estado
            w-aux.quantidade
            w-aux.quantm2
            w-aux.valor
            de-perc-gru
            de-perc-tot
            de-perc-cli
            with frame f-detalhe.
    down with frame f-detalhe.
    assign de-valor-cli   = de-valor-cli + w-aux.valor
           de-quantm2-cli = de-quantm2-cli + w-aux.quantm2
           de-valor-gru   = de-valor-gru + w-aux.valor
           de-quantm2-gru = de-quantm2-gru + w-aux.quantm2.
    if  last-of(w-aux.cod-gr-cli) then do.
        assign de-perc-gru = (w-aux3.quantidade * 100)
                             / w-aux2.quantidade
               de-perc-cli = 100
               de-perc-tot = (w-aux3.quantidade * 100)
                         / de-quantidade-ger.
        if  w-aux2.quantidade = 0 then
            assign de-perc-gru = 100.
        if  de-quantidade-ger = 0 then
            assign de-perc-tot = 100.
        DISPLAY "Total Grupo Cliente: " @ gr-cli.descricao
                w-aux3.quantidade       @ w-aux.quantidade
                de-valor-cli            @ w-aux.valor
                de-quantm2-cli          @ w-aux.quantm2
                de-perc-gru
                de-perc-tot
                de-perc-cli
                with frame f-detalhe.
        down with frame f-detalhe.
        assign de-valor-cli = 0
               de-quantm2-cli = 0.
        put skip(1).
    end.
    if  last-of(w-aux.ge-codigo) then do.
        assign de-perc-gru = 100
               de-perc-tot = (w-aux3.quantidade * 100)
                         / de-quantidade-ger.
        if  de-quantidade-ger = 0 then
            assign de-perc-tot = 100.
        DISPLAY "Total Grupo Estoque: " @ gr-cli.descricao
                w-aux2.quantidade       @ w-aux.quantidade
                de-valor-gru            @ w-aux.valor
                de-quantm2-gru          @ w-aux.quantm2
                de-perc-gru
                de-perc-tot
                with frame f-detalhe.
        down with frame f-detalhe.
        put skip(1).
        assign de-valor-gru   = 0
               de-quantm2-gru = 0.
    end.
end.
assign de-perc-tot = 100.
DISPLAY "Total Geral: "   @ gr-cli.descricao
        de-quantidade-ger @ w-aux.quantidade
        de-valor-ger      @ w-aux.valor
        de-quantm2-ger    @ w-aux.quantm2
        de-perc-tot
        with frame f-detalhe.
down with frame f-detalhe.
assign de-valor-ger   = 0
       de-quantm2-ger = 0.

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
   display tt-param.cod-estabel
           tt-param.ini-dt-emissao
           tt-param.fin-dt-emissao
           tt-param.ini-ge-codigo
           tt-param.fin-ge-codigo
           with frame f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

