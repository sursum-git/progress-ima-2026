/* Programa: ESCC0004RP.P
** Sistema.: EMS da DATASUL S/A.
** Modulo..: Compras
** Objetivo: Listar Curva ABC de Compras.
** Autor...: Gilvando Souza Araujo - Agosto/2006
** Obs.....: Especifico da TEAR TÒXTIL INDéSTRIA E COMRCIO LTDA.
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESCC0004RP 2.04.00.000}

define temp-table tt-param no-undo
       field destino         as integer
       field arquivo         as char format "x(35)"
       field usuario         as char format "x(12)"
       field data-exec       as date
       field hora-exec       as integer
       field classifica      as integer
       FIELD desc-classifica AS CHAR FORMAT "x(30)"
       FIELD ge-codigo-ini   LIKE item.ge-codigo         
       FIELD ge-codigo-fin   LIKE item.ge-codigo           
       FIELD it-codigo-ini   LIKE ordem-compra.it-codigo
       FIELD it-codigo-fin   LIKE ordem-compra.it-codigo
       FIELD dt-emissao-ini  LIKE ordem-compra.data-emissao 
       FIELD dt-emissao-fin  LIKE ordem-compra.data-emissao
       FIELD dt-entrega-ini  LIKE prazo-compra.data-entrega   
       FIELD dt-entrega-fin  LIKE prazo-compra.data-entrega   
       FIELD data-conv       AS INTEGER  
       FIELD desc-data-conv  AS CHAR FORMAT "x(10)"
       FIELD mo-codigo       LIKE moeda.mo-codigo     
       FIELD desc-moeda      LIKE moeda.descricao    
       FIELD nao-confirm     AS LOGICAL FORMAT "Sim/NÆo"     
       FIELD confirm         AS LOGICAL FORMAT "Sim/NÆo"     
       FIELD em-cotacao      AS LOGICAL FORMAT "Sim/NÆo"     
       FIELD terminada       AS LOGICAL FORMAT "Sim/NÆo"     
       FIELD eliminada       AS LOGICAL FORMAT "Sim/NÆo"
       FIELD cotada          AS LOGICAL FORMAT "Sim/NÆo"
       FIELD gerar-excel     AS LOG FORMAT "Sim/NÆo"
       FIELD arq-excel       AS CHAR FORMAT "x(45)"
       FIELD impr-param      AS LOGICAL.

define temp-table tt-raw-digita
       field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

DEF TEMP-TABLE tt-work
    FIELD it-codigo    LIKE ordem-compra.it-codigo
    FIELD cod-emitente LIKE ordem-compra.cod-emitente
    FIELD sc-codigo    LIKE requisitante.sc-codigo
    FIELD quantidade   AS DEC FORMAT ">>>,>>>,>>9.99"                     
    FIELD valor        AS DEC FORMAT ">>>,>>>,>>9.99"
    FIELD moeda        LIKE moeda.sigla 
    FIELD cons-item    AS DEC FORMAT ">>>,>>>,>>9.99"
    FIELD saldo-item   AS DEC FORMAT "->>>,>>>,>>9.99"
    INDEX ch-work it-codigo
                  cod-emitente
                  sc-codigo.

DEF TEMP-TABLE tt-work1
    FIELD it-codigo    LIKE ordem-compra.it-codigo
    FIELD quantidade   AS DEC FORMAT ">>>,>>>,>>9.99"                     
    FIELD valor        AS DEC FORMAT ">>>,>>>,>>9.99"
    FIELD moeda        LIKE moeda.sigla 
    FIELD cons-item    AS DEC FORMAT ">>>,>>>,>>9.99"
    FIELD saldo-item   AS DEC FORMAT "->>>,>>>,>>9.99"
    INDEX ch-work it-codigo.

DEF TEMP-TABLE tt-work2
    FIELD cod-emitente LIKE ordem-compra.cod-emitente
    FIELD quantidade   AS DEC FORMAT ">>>,>>>,>>9.99"                     
    FIELD valor        AS DEC FORMAT ">>>,>>>,>>9.99"
    FIELD moeda        LIKE moeda.sigla                                
    FIELD cons-item    AS DEC FORMAT ">>>,>>>,>>9.99"
    FIELD saldo-item   AS DEC FORMAT "->>>,>>>,>>9.99"
    INDEX ch-work cod-emitente.

DEF TEMP-TABLE tt-work3
    FIELD sc-codigo    LIKE requisitante.sc-codigo
    FIELD quantidade   AS DEC FORMAT ">>>,>>>,>>9.99"                     
    FIELD valor        AS DEC FORMAT ">>>,>>>,>>9.99"
    FIELD moeda        LIKE moeda.sigla                                
    FIELD cons-item    AS DEC FORMAT ">>>,>>>,>>9.99"
    FIELD saldo-item   AS DEC FORMAT "->>>,>>>,>>9.99"
    INDEX ch-work sc-codigo.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* defini‡Æo de vari veis  */
def var h-acomp as handle no-undo.

DEF VAR c-item AS CHAR FORMAT "x(132)".
DEF VAR de-preco-unit LIKE ordem-compra.preco-unit.
DEF VAR da-data-aux AS DATE.
DEF VAR de-cotacao-orig LIKE cotacao.cotacao[1].
DEF VAR de-cotacao-dest LIKE cotacao.cotacao[1].
DEF VAR c-sigla-dest LIKE moeda.sigla.
DEF VAR de-perc1 AS DEC FORMAT ">>9.99".
DEF VAR de-perc2 AS DEC FORMAT ">>9.99".
DEF VAR de-tot-vlr-ger AS DEC FORMAT ">>>,>>>,>>9.99".
DEF VAR de-tot-qtd-ger AS DEC FORMAT ">>>,>>>,>>9.99".
DEF VAR de-tot-con-ger AS DEC FORMAT ">>>,>>>,>>9.99".
DEF VAR de-tot-sld-ger AS DEC FORMAT "->>>,>>>,>>9.99".
DEF VAR de-cons-item AS DEC.
DEF VAR de-saldo-item AS DEC FORMAT "->>>,>>>,>>9.99".
DEF VAR i-sequencia AS INT.

DEF STREAM saida.

form
    "*-------------- Parƒmetros/Sele‡Æo ---------------*" SKIP
    tt-param.desc-classifica LABEL "Classifica‡Æo..." AT  1
    tt-param.ge-codigo-ini   LABEL "Grupo Estoque..." AT  1
    "a"                                               AT 35
    tt-param.ge-codigo-fin   NO-LABEL                      
    tt-param.it-codigo-ini   LABEL "Item............" AT  1
    "a"                                               AT 35
    tt-param.it-codigo-fin   NO-LABEL                
    tt-param.dt-emissao-ini  LABEL "Data EmissÆo...." AT  1
    "a"                                               AT 35
    tt-param.dt-emissao-fin  NO-LABELS
    tt-param.dt-entrega-ini  LABEL "Data Entrega...." AT  1 
    "a"                                               AT 35
    tt-param.dt-entrega-fin  NO-LABELS
    tt-param.desc-data-conv  LABEL "Data ConversÆo.." AT  1
    tt-param.mo-codigo       LABEL "Moeda..........." AT  1
    tt-param.desc-moeda      NO-LABELS                AT 21
    tt-param.nao-confirm     LABEL "NÆo Confirmada.." AT  1
    tt-param.confirm         LABEL "Confirmada......" AT  1
    tt-param.em-cotacao      LABEL "Em Cota‡Æo......" AT  1
    tt-param.terminada       LABEL "Terminada......." AT  1
    tt-param.eliminada       LABEL "Eliminada......." AT  1
    tt-param.cotada          LABEL "Cotada.........." AT  1
    tt-param.gerar-excel     LABEL "Gerar Excel....." AT  1
    tt-param.arq-excel       LABEL "Arquivo Excel..." AT  1
    with no-box side-labels width 132 STREAM-IO frame f-param.

FORM HEADER
    "Classifica‡Æo:" tt-param.desc-classifica
    " - Moeda:" tt-param.desc-moeda
    WITH NO-LABEL 2 DOWN NO-BOX NO-ATTR-SPACE WIDTH 132 STREAM-IO FRAME f-classifica.

FORM
    i-sequencia              LABEL "Seq." FORMAT ">>>9"
    tt-work1.it-codigo       LABEL "Item"
    ITEM.desc-item           LABEL "Descri‡Æo"
    tt-work1.valor           LABEL "Valor"
    de-perc1                 LABEL "% Vlr"
    tt-work1.quantidade      LABEL "Quantidade"
    ITEM.un                  LABEL "Un"
    de-perc2                 LABEL "% Qtd"
    WITH NO-LABEL NO-BOX 55 DOWN WIDTH 132 WITH STREAM-IO FRAME f-detalhe1.

FORM
    i-sequencia              LABEL "Seq." FORMAT ">>>9"
    tt-work1.it-codigo       LABEL "Item"
    ITEM.desc-item           LABEL "Descri‡Æo"
    tt-work1.quantidade      LABEL "Quantidade"
    ITEM.un                  LABEL "Un"
    de-perc2                 LABEL "% Qtd"
    tt-work1.valor           LABEL "Valor"
    de-perc1                 LABEL "% Vlr"
    WITH NO-LABEL NO-BOX 55 DOWN WIDTH 132 WITH STREAM-IO FRAME f-detalhe2.

FORM
    i-sequencia              LABEL "Seq." FORMAT ">>>9"
    tt-work2.cod-emitente    LABEL "Codigo"
    emitente.nome-emit       LABEL "Fornecedor"
    tt-work2.valor           LABEL "Valor"
    de-perc1                 LABEL "% Vlr" 
    tt-work2.quantidade      LABEL "Quantidade"
    de-perc2                 LABEL "% Qtd"      
    WITH NO-LABEL NO-BOX 55 DOWN WIDTH 132 WITH STREAM-IO FRAME f-detalhe3.

FORM
    i-sequencia              LABEL "Seq." FORMAT ">>>9"
    tt-work2.cod-emitente    LABEL "Codigo"
    emitente.nome-emit       LABEL "Fornecedor"
    tt-work2.quantidade      LABEL "Quantidade"
    de-perc2                 LABEL "% Qtd"      
    tt-work2.valor           LABEL "Valor"
    de-perc1                 LABEL "% Vlr" 
    WITH NO-LABEL NO-BOX 55 DOWN WIDTH 132 WITH STREAM-IO FRAME f-detalhe4.

FORM
    i-sequencia              LABEL "Seq." FORMAT ">>>9"
    tt-work3.sc-codigo       LABEL "Codigo"
    sub-conta.descricao      LABEL "Requisitante"
    tt-work3.valor           LABEL "Valor"
    de-perc1                 LABEL "% Vlr" 
    tt-work3.quantidade      LABEL "Quantidade"
    de-perc2                 LABEL "% Qtd"      
    WITH NO-LABEL NO-BOX 55 DOWN WIDTH 132 WITH STREAM-IO FRAME f-detalhe5.

FORM
    i-sequencia              LABEL "Seq." FORMAT ">>>9"
    tt-work3.sc-codigo       LABEL "Codigo"
    sub-conta.descricao      LABEL "Requiitante"
    tt-work3.quantidade      LABEL "Quantidade"
    de-perc2                 LABEL "% Qtd"      
    tt-work3.valor           LABEL "Valor"
    de-perc1                 LABEL "% Vlr" 
    WITH NO-LABEL NO-BOX 55 DOWN WIDTH 132 WITH STREAM-IO FRAME f-detalhe6.

/* include padrÆo para output de relat¢rios */
{include/i-rpout.i}

/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */
{include/i-rpcab.i}

/* bloco principal do programa */
find first param-global no-lock no-error.
find first empresa
     where empresa.ep-codigo = param-global.empresa-prin no-lock no-error. 

assign c-empresa = (if avail empresa then empresa.razao-social else "").

{utp/ut-liter.i COMPRAS * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Curva_ABC_de_Compras * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
VIEW FRAME f-classifica.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

IF tt-param.gerar-excel THEN DO:
   output STREAM saida to value(tt-param.arq-excel) CONVERT SOURCE "ibm850".
   PUT STREAM saida
              c-empresa
              " - " 
              c-titulo-relat
              " - PERIODO: " 
              tt-param.dt-emissao-ini
              " a "
              tt-param.dt-emissao-fin
              SKIP(1)
              "Classificacao: "
              tt-param.desc-classifica
              " - Moeda: "
              tt-param.desc-moeda
              SKIP(1).
   IF tt-param.classifica = 1 THEN
      PUT STREAM saida
                 "SEQ;"        
                 "ITEM;" 
                 "DESCRICAO;"     
                 "VALOR;"     
                 "% VLR;"           
                 "QUANTIDADE;"
                 "UN;"
                 "% QTD;".
   IF tt-param.classifica = 2 THEN
      PUT STREAM saida
                 "SEQ;"        
                 "ITEM;" 
                 "DESCRICAO;"     
                 "QUANTIDADE;" 
                 "UN;"
                 "% QTD;"
                 "VALOR;"     
                 "% VLR;".           
   IF tt-param.classifica = 3 THEN
      PUT STREAM saida
                 "SEQ;"        
                 "CODIGO;" 
                 "FORNECEDOR;"     
                 "VALOR;"     
                 "% VLR;"           
                 "QUANTIDADE;"
                 "% QTD;".
   IF tt-param.classifica = 4 THEN
      PUT STREAM saida
                 "SEQ;"        
                 "CODIGO;" 
                 "FORNECEDOR;"     
                 "QUANTIDADE;" 
                 "% QTD;"
                 "VALOR;"     
                 "% VLR;".           
   IF tt-param.classifica = 5 THEN
      PUT STREAM saida
                 "SEQ;"        
                 "CODIGO;" 
                 "REQUISITANTE;"     
                 "VALOR;"     
                 "% VLR;"           
                 "QUANTIDADE;"     
                 "% QTD;".
   IF tt-param.classifica = 6 THEN
      PUT STREAM saida
                 "SEQ;"        
                 "CODIGO;" 
                 "REQUISITANTE;"     
                 "QUANTIDADE;"     
                 "% QTD;"           
                 "VALOR;"     
                 "% VLR;".
   PUT STREAM saida
             "CONSUMO;"
             "SLD.ESTOQUE"
             SKIP.
END.

FOR EACH ordem-compra WHERE ordem-compra.it-codigo    >= tt-param.it-codigo-ini
                        AND ordem-compra.it-codigo    <= tt-param.it-codigo-fin
                        AND ordem-compra.data-emissao >= tt-param.dt-emissao-ini
                        AND ordem-compra.data-emissao <= tt-param.dt-emissao-fin
                      NO-LOCK,
    EACH ITEM WHERE item.it-codigo =  ordem-compra.it-codigo
                AND ITEM.ge-codigo >= tt-param.ge-codigo-ini
                AND ITEM.ge-codigo <= tt-param.ge-codigo-fin
              NO-LOCK,
    EACH requisitante WHERE requisitante.nome-abrev = ordem-compra.requisitante
                      NO-LOCK,
    EACH prazo-compra OF ordem-compra
                      WHERE prazo-compra.data-entrega >= tt-param.dt-entrega-ini
                        AND prazo-compra.data-entrega <= tt-param.dt-entrega-fin
                        AND ((prazo-compra.situacao = 1 AND tt-param.nao-confirm) OR
                             (prazo-compra.situacao = 2 AND tt-param.confirm) OR
                             (prazo-compra.situacao = 3 AND tt-param.cotada) OR
                             (prazo-compra.situacao = 4 AND tt-param.eliminada) OR
                             (prazo-compra.situacao = 5 AND tt-param.em-cotacao) OR
                             (prazo-compra.situacao = 6 AND tt-param.terminada))
                        NO-LOCK:
   
    RUN pi-acompanhar in h-acomp (input "Ordem: " + string(ordem-compra.numero-ordem)).
   
    IF ordem-compra.mo-codigo <> tt-param.mo-codigo THEN DO:
       IF tt-param.data-conv = 1 THEN
          ASSIGN da-data-aux = ordem-compra.data-emissao.
       ELSE
       IF tt-param.data-conv = 2 THEN
          ASSIGN da-data-aux = prazo-compra.data-entrega.
       ELSE
          ASSIGN da-data-aux = TODAY.
       
       IF ordem-compra.mo-codigo <> 0 THEN DO:
          FIND cotacao WHERE cotacao.mo-codigo   = ordem-compra.mo-codigo
                         AND cotacao.ano-periodo = STRING(YEAR(da-data-aux),"9999") +
                                                   STRING(MONTH(da-data-aux),"99")
                       NO-LOCK NO-ERROR.
          IF AVAIL cotacao THEN
             ASSIGN de-cotacao-orig = cotacao.cotacao[DAY(da-data-aux)].
          ELSE
             ASSIGN de-cotacao-orig = 0.
       END.
       IF tt-param.mo-codigo <> 0 THEN DO:
           FIND cotacao WHERE cotacao.mo-codigo   = tt-param.mo-codigo
                          AND cotacao.ano-periodo = STRING(YEAR(da-data-aux),"9999") +
                                                    STRING(MONTH(da-data-aux),"99")
                        NO-LOCK NO-ERROR.
           IF AVAIL cotacao THEN
              ASSIGN de-cotacao-dest = cotacao.cotacao[DAY(da-data-aux)].
           ELSE
              ASSIGN de-cotacao-dest = 0.
       END.
       IF ordem-compra.mo-codigo = 0 AND tt-param.mo-codigo <> 0 THEN DO:
          IF de-cotacao-dest <> 0 THEN
             ASSIGN de-preco-unit = ordem-compra.preco-unit / de-cotacao-dest.
          ELSE
             ASSIGN de-preco-unit = 0.
       END.
       IF ordem-compra.mo-codigo <> 0 AND tt-param.mo-codigo = 0 THEN
          ASSIGN de-preco-unit = ordem-compra.preco-unit * de-cotacao-orig.
       IF ordem-compra.mo-codigo <> 0 AND tt-param.mo-codigo <> 0 THEN DO:
          IF de-cotacao-orig <> 0 THEN
             ASSIGN de-preco-unit = ordem-compra.preco-unit / de-cotacao-orig * de-cotacao-dest.
          ELSE
             ASSIGN de-preco-unit = 0.
       END.
    END.
    ELSE
       ASSIGN de-preco-unit = ordem-compra.preco-unit.
    
    FIND moeda WHERE moeda.mo-codigo = tt-param.mo-codigo NO-LOCK NO-ERROR.
    IF AVAIL moeda THEN
       ASSIGN c-sigla-dest = moeda.sigla.
    ELSE
       ASSIGN c-sigla-dest = "".

    FIND FIRST tt-work WHERE tt-work.it-codigo    = ordem-compra.it-codigo
                         AND tt-work.cod-emitente = ordem-compra.cod-emitente
                         AND tt-work.sc-codigo    = requisitante.sc-codigo
                       NO-LOCK NO-ERROR.
    IF NOT AVAIL tt-work THEN DO:
       CREATE tt-work.
       ASSIGN tt-work.it-codigo    = ordem-compra.it-codigo
              tt-work.cod-emitente = ordem-compra.cod-emitente
              tt-work.sc-codigo    = requisitante.sc-codigo.   
    END.
    ASSIGN tt-work.quantidade = tt-work.quantidade + prazo-compra.quantidade
           tt-work.valor      = tt-work.valor      + (prazo-compra.quantidade * de-preco-unit).

    IF tt-param.gerar-excel THEN DO:
       /*--- Consumo do Item no per¡odo selecionado ---*/
       ASSIGN de-cons-item = 0.
       FOR EACH movto-estoq USE-INDEX item-data
          WHERE movto-estoq.it-codigo   =  ordem-compra.it-codigo
            AND movto-estoq.cod-estabel =  "2"
            AND movto-estoq.dt-trans    >= tt-param.dt-emissao-ini
            AND movto-estoq.dt-trans    <= tt-param.dt-emissao-fin
            AND movto-estoq.it-codigo   =  ordem-compra.it-codigo
            AND (movto-estoq.esp-docto  =  28 OR
                 movto-estoq.esp-docto  =  31)
           NO-LOCK:
           IF movto-estoq.tipo-trans = 1 THEN
              ASSIGN de-cons-item = de-cons-item - movto-estoq.quantidade.
           ELSE
              ASSIGN de-cons-item = de-cons-item + movto-estoq.quantidade.
       END.
       ASSIGN tt-work.cons-item = tt-work.cons-item + de-cons-item.
       
       /*--- Saldo do Item no final do per¡odo selecionado ---*/
       ASSIGN de-saldo-item = 0.
       FOR EACH saldo-estoq WHERE saldo-estoq.cod-estabel = "2"
                              AND saldo-estoq.it-codigo   = ordem-compra.it-codigo NO-LOCK:
           ASSIGN de-saldo-item = de-saldo-item + saldo-estoq.qtidade-atu.
       END.
       FOR EACH movto-estoq WHERE movto-estoq.it-codigo   = ordem-compra.it-codigo
                              AND movto-estoq.cod-estabel = "2"
                              AND movto-estoq.dt-trans    > tt-param.dt-emissao-fin
                            NO-LOCK:
           IF movto-estoq.tipo-trans = 1 THEN
              ASSIGN de-saldo-item = de-saldo-item - movto-estoq.quantidade.
           ELSE
              ASSIGN de-saldo-item = de-saldo-item + movto-estoq.quantidade.
       END.
       ASSIGN tt-work.saldo-item = tt-work.saldo-item + de-saldo-item.
    END.
END.

FOR EACH tt-work:
    FIND tt-work1 WHERE tt-work1.it-codigo = tt-work.it-codigo NO-LOCK NO-ERROR.
    IF NOT AVAIL tt-work1 THEN DO:
       CREATE tt-work1.
       ASSIGN tt-work1.it-codigo = tt-work.it-codigo.
    END.
    ASSIGN tt-work1.quantidade = tt-work1.quantidade + tt-work.quantidade
           tt-work1.valor      = tt-work1.valor      + tt-work.valor
           tt-work1.moeda      = tt-work.moeda
           tt-work1.cons-item  = tt-work1.cons-item  + tt-work.cons-item
           tt-work1.saldo-item = tt-work1.saldo-item + tt-work.saldo-item.
    
    FIND tt-work2 WHERE tt-work2.cod-emitente = tt-work.cod-emitente NO-LOCK NO-ERROR.
    IF NOT AVAIL tt-work2 THEN DO:
       CREATE tt-work2.
       ASSIGN tt-work2.cod-emitente = tt-work.cod-emitente.
    END.
    ASSIGN tt-work2.quantidade = tt-work2.quantidade + tt-work.quantidade
           tt-work2.valor      = tt-work2.valor      + tt-work.valor
           tt-work2.moeda      = tt-work.moeda
           tt-work2.cons-item  = tt-work2.cons-item  + tt-work.cons-item
           tt-work2.saldo-item = tt-work2.saldo-item + tt-work.saldo-item.
    
    FIND tt-work3 WHERE tt-work3.sc-codigo = tt-work.sc-codigo NO-LOCK NO-ERROR.
    IF NOT AVAIL tt-work3 THEN DO:
       CREATE tt-work3.
       ASSIGN tt-work3.sc-codigo = tt-work.sc-codigo.
    END.
    ASSIGN tt-work3.quantidade = tt-work3.quantidade + tt-work.quantidade
           tt-work3.valor      = tt-work3.valor      + tt-work.valor
           tt-work3.moeda      = tt-work.moeda
           tt-work3.cons-item  = tt-work3.cons-item  + tt-work.cons-item
           tt-work3.saldo-item = tt-work3.saldo-item + tt-work.saldo-item.

    ASSIGN de-tot-vlr-ger = de-tot-vlr-ger + tt-work.valor
           de-tot-qtd-ger = de-tot-qtd-ger + tt-work.quantidade
           de-tot-con-ger = de-tot-con-ger + tt-work.cons-item
           de-tot-sld-ger = de-tot-sld-ger + tt-work.saldo-item.
END.

IF tt-param.classifica = 1 THEN DO: /* Item/Valor */
   FOR EACH tt-work1 BY tt-work1.valor DESCENDING 
                     BY tt-work1.it-codigo:
       FIND ITEM WHERE ITEM.it-codigo = tt-work1.it-codigo NO-LOCK.
       ASSIGN de-perc1 = tt-work1.valor / de-tot-vlr-ger * 100
              de-perc2 = tt-work1.quantidade / de-tot-qtd-ger * 100.
       ASSIGN i-sequencia = i-sequencia + 1.
       DISPLAY i-sequencia
               tt-work1.it-codigo
               ITEM.desc-item
               tt-work1.valor
               de-perc1
               tt-work1.quantidade
               ITEM.un
               de-perc2
               WITH FRAME f-detalhe1.
       DOWN WITH FRAME f-detalhe1.

       IF tt-param.gerar-excel THEN DO:
          PUT STREAM saida 
                     i-sequencia ";"
                     tt-work1.it-codigo ";"
                     ITEM.desc-item ";"
                     tt-work1.valor ";"
                     de-perc1 ";"
                     tt-work1.quantidade ";"
                     ITEM.un ";"
                     de-perc2 ";"
                     tt-work1.cons-item ";"
                     tt-work1.saldo-item
                     SKIP.
       END.
   END.

   ASSIGN de-perc1 = de-tot-vlr-ger / de-tot-vlr-ger * 100
          de-perc2 = de-tot-qtd-ger / de-tot-qtd-ger * 100.
   DISPLAY "Total"        @ tt-work1.it-codigo
           de-tot-vlr-ger @ tt-work1.valor
           de-perc1
           de-tot-qtd-ger @ tt-work1.quantidade
           de-perc2
           WITH FRAME f-detalhe1.
   IF tt-param.gerar-excel THEN
      PUT STREAM saida
                 "Total;;;"
                 de-tot-vlr-ger ";"
                 de-perc1 ";"
                 de-tot-qtd-ger ";;"
                 de-perc2 ";"
                 de-tot-con-ger ";"
                 de-tot-sld-ger
                 SKIP.
END.

IF tt-param.classifica = 2 THEN DO: /* Item/Quantidade */
   FOR EACH tt-work1 BY tt-work1.quantidade DESCENDING 
                     BY tt-work1.it-codigo:
       FIND ITEM WHERE ITEM.it-codigo = tt-work1.it-codigo NO-LOCK.
       ASSIGN de-perc1 = tt-work1.valor / de-tot-vlr-ger * 100
              de-perc2 = tt-work1.quantidade / de-tot-qtd-ger * 100.
       ASSIGN i-sequencia = i-sequencia + 1.
       DISPLAY i-sequencia
               tt-work1.it-codigo
               ITEM.desc-item
               tt-work1.valor
               de-perc1
               tt-work1.quantidade
               ITEM.un
               de-perc2
               WITH FRAME f-detalhe2.
       DOWN WITH FRAME f-detalhe2.

       IF tt-param.gerar-excel THEN DO:
          PUT STREAM saida 
                     i-sequencia ";"
                     tt-work1.it-codigo ";"
                     ITEM.desc-item ";"
                     tt-work1.quantidade ";"
                     ITEM.un ";"
                     de-perc2 ";"
                     tt-work1.valor ";"
                     de-perc1 ";"
                     tt-work1.cons-item ";"
                     tt-work1.saldo-item
                     SKIP.
       END.
   END.

   ASSIGN de-perc1 = de-tot-vlr-ger / de-tot-vlr-ger * 100
          de-perc2 = de-tot-qtd-ger / de-tot-qtd-ger * 100.
   DISPLAY "Total"        @ tt-work1.it-codigo
           de-tot-vlr-ger @ tt-work1.valor
           de-perc1
           de-tot-qtd-ger @ tt-work1.quantidade
           de-perc2
           WITH FRAME f-detalhe2.
   IF tt-param.gerar-excel THEN
      PUT STREAM saida
                 "Total;;;"
                 de-tot-vlr-ger ";"
                 de-perc1 ";"
                 de-tot-qtd-ger ";;"
                 de-perc2 ";"
                 de-tot-con-ger ";"
                 de-tot-sld-ger
                 SKIP.
END.

IF tt-param.classifica = 3 THEN DO: /* Fornecedor/Valor */
   FOR EACH tt-work2 BY tt-work2.valor DESCENDING 
                     BY tt-work2.cod-emitente:
       FIND emitente WHERE emitente.cod-emitente = tt-work2.cod-emitente NO-LOCK.
       ASSIGN de-perc1 = tt-work2.valor / de-tot-vlr-ger * 100
              de-perc2 = tt-work2.quantidade / de-tot-qtd-ger * 100.
       ASSIGN i-sequencia = i-sequencia + 1.
       DISPLAY i-sequencia
               tt-work2.cod-emitente
               emitente.nome-emit
               tt-work2.valor
               de-perc1
               tt-work2.quantidade
               de-perc2
               WITH FRAME f-detalhe3.
       DOWN WITH FRAME f-detalhe3.

       IF tt-param.gerar-excel THEN DO:
          PUT STREAM saida 
                     i-sequencia ";"
                     tt-work2.cod-emitente ";"
                     emitente.nome-emit ";"
                     tt-work2.valor ";"
                     de-perc1 ";"
                     tt-work2.quantidade ";"
                     de-perc2 ";"
                     tt-work2.cons-item ";"
                     tt-work2.saldo-item
                     SKIP.
       END.
   END.

   ASSIGN de-perc1 = de-tot-vlr-ger / de-tot-vlr-ger * 100
          de-perc2 = de-tot-qtd-ger / de-tot-qtd-ger * 100.
   DISPLAY "Total"        @ tt-work2.cod-emitente
           de-tot-vlr-ger @ tt-work2.valor
           de-perc1
           de-tot-qtd-ger @ tt-work2.quantidade
           de-perc2
           WITH FRAME f-detalhe3.
   IF tt-param.gerar-excel THEN
      PUT STREAM saida
                 "Total;;;"
                 de-tot-vlr-ger ";"
                 de-perc1 ";"
                 de-tot-qtd-ger ";"
                 de-perc2 ";"
                 de-tot-con-ger ";"
                 de-tot-sld-ger
                 SKIP.
END.

IF tt-param.classifica = 4 THEN DO: /* Fornecedor/Quantidade */
   FOR EACH tt-work2 BY tt-work2.quantidade DESCENDING 
                     BY tt-work2.cod-emitente:
       FIND emitente WHERE emitente.cod-emitente = tt-work2.cod-emitente NO-LOCK.
       ASSIGN de-perc1 = tt-work2.valor / de-tot-vlr-ger * 100
              de-perc2 = tt-work2.quantidade / de-tot-qtd-ger * 100.
       ASSIGN i-sequencia = i-sequencia + 1.
       DISPLAY i-sequencia
               tt-work2.cod-emitente
               emitente.nome-emit
               tt-work2.valor
               de-perc1
               tt-work2.quantidade
               de-perc2
               WITH FRAME f-detalhe4.
       DOWN WITH FRAME f-detalhe4.

       IF tt-param.gerar-excel THEN DO:
          PUT STREAM saida 
                     i-sequencia ";"
                     tt-work2.cod-emitente ";"
                     emitente.nome-emit ";"
                     tt-work2.quantidade ";"
                     de-perc2 ";"
                     tt-work2.valor ";"
                     de-perc1 ";"
                     tt-work2.cons-item ";"
                     tt-work2.saldo-item
                     SKIP.
       END.
   END.

   ASSIGN de-perc1 = de-tot-vlr-ger / de-tot-vlr-ger * 100
          de-perc2 = de-tot-qtd-ger / de-tot-qtd-ger * 100.
   DISPLAY "Total"        @ tt-work2.cod-emitente
           de-tot-vlr-ger @ tt-work2.valor
           de-perc1
           de-tot-qtd-ger @ tt-work2.quantidade
           de-perc2
           WITH FRAME f-detalhe4.
   IF tt-param.gerar-excel THEN
      PUT STREAM saida
                 "Total;;;"
                 de-tot-qtd-ger ";"
                 de-perc2 ";"
                 de-tot-vlr-ger ";"
                 de-perc1 ";"
                 de-tot-con-ger ";"
                 de-tot-sld-ger
                 SKIP.
END.

IF tt-param.classifica = 5 THEN DO: /* Requisitante/Valor */
   FOR EACH tt-work3 BY tt-work3.valor DESCENDING 
                     BY tt-work3.sc-codigo:
       FIND sub-conta WHERE sub-conta.sc-codigo = tt-work3.sc-codigo NO-LOCK.
       ASSIGN de-perc1 = tt-work3.valor / de-tot-vlr-ger * 100
              de-perc2 = tt-work3.quantidade / de-tot-qtd-ger * 100.
       ASSIGN i-sequencia = i-sequencia + 1.
       DISPLAY i-sequencia
               tt-work3.sc-codigo   
               sub-conta.descricao
               tt-work3.valor
               de-perc1
               tt-work3.quantidade
               de-perc2
               WITH FRAME f-detalhe5.
       DOWN WITH FRAME f-detalhe5.

       IF tt-param.gerar-excel THEN DO:
          PUT STREAM saida 
                     i-sequencia ";"
                     tt-work3.sc-codigo ";"
                     sub-conta.descricao ";"
                     tt-work3.valor ";"
                     de-perc1 ";"
                     tt-work3.quantidade ";"
                     de-perc2 ";"
                     tt-work3.cons-item ";"
                     tt-work3.saldo-item
                     SKIP.
       END.
   END.

   ASSIGN de-perc1 = de-tot-vlr-ger / de-tot-vlr-ger * 100
          de-perc2 = de-tot-qtd-ger / de-tot-qtd-ger * 100.
   DISPLAY "Total"        @ tt-work3.sc-codigo   
           de-tot-vlr-ger @ tt-work3.valor
           de-perc1
           de-tot-qtd-ger @ tt-work3.quantidade
           de-perc2
           WITH FRAME f-detalhe5.
   IF tt-param.gerar-excel THEN
      PUT STREAM saida
                 "Total;;;"
                 de-tot-vlr-ger ";"
                 de-perc1 ";"
                 de-tot-qtd-ger ";"
                 de-perc2 ";"
                 de-tot-con-ger ";"
                 de-tot-sld-ger
                 SKIP.
END.

IF tt-param.classifica = 6 THEN DO: /* Requisitante/Quantidade */
   FOR EACH tt-work3 BY tt-work3.quantidade DESCENDING 
                     BY tt-work3.sc-codigo:
       FIND sub-conta WHERE sub-conta.sc-codigo = tt-work3.sc-codigo NO-LOCK.
       ASSIGN de-perc1 = tt-work3.valor / de-tot-vlr-ger * 100
              de-perc2 = tt-work3.quantidade / de-tot-qtd-ger * 100.
       ASSIGN i-sequencia = i-sequencia + 1.
       DISPLAY i-sequencia
               tt-work3.sc-codigo   
               sub-conta.descricao
               tt-work3.quantidade
               de-perc2
               tt-work3.valor
               de-perc1
               WITH FRAME f-detalhe6.
       DOWN WITH FRAME f-detalhe6.

       IF tt-param.gerar-excel THEN DO:
          PUT STREAM saida 
                     i-sequencia ";"
                     tt-work3.sc-codigo ";"
                     sub-conta.descricao ";"
                     tt-work3.quantidade ";"
                     de-perc2 ";"
                     tt-work3.valor ";"
                     de-perc1 ";"
                     tt-work3.cons-item ";"
                     tt-work3.saldo-item
                     SKIP.
       END.
   END.

   ASSIGN de-perc1 = de-tot-vlr-ger / de-tot-vlr-ger * 100
          de-perc2 = de-tot-qtd-ger / de-tot-qtd-ger * 100.
   DISPLAY "Total"        @ tt-work3.sc-codigo   
           de-tot-vlr-ger @ tt-work3.valor
           de-perc1
           de-tot-qtd-ger @ tt-work3.quantidade
           de-perc2
           WITH FRAME f-detalhe6.
   IF tt-param.gerar-excel THEN
      PUT STREAM saida
                 "Total;;;"
                 de-tot-qtd-ger ";"
                 de-perc2 ";"
                 de-tot-vlr-ger ";"
                 de-perc1 ";"
                 de-tot-con-ger ";"
                 de-tot-sld-ger
                 SKIP.
END.

IF tt-param.gerar-excel THEN
   OUTPUT STREAM saida CLOSE.

IF tt-param.impr-param THEN DO:
   PAGE.
   display tt-param.desc-classifica
           tt-param.ge-codigo-ini  
           tt-param.ge-codigo-fin  
           tt-param.it-codigo-ini  
           tt-param.it-codigo-fin  
           tt-param.dt-emissao-ini 
           tt-param.dt-emissao-fin 
           tt-param.dt-entrega-ini 
           tt-param.dt-entrega-fin 
           tt-param.desc-data-conv 
           tt-param.mo-codigo      
           tt-param.desc-moeda     
           tt-param.nao-confirm    
           tt-param.confirm        
           tt-param.em-cotacao     
           tt-param.terminada      
           tt-param.eliminada      
           tt-param.cotada 
           tt-param.gerar-excel 
           tt-param.arq-excel   
           with frame f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.
