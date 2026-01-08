/* Programa: ESCC0005RP.P
** Sistema.: EMS da DATASUL S/A.
** Modulo..: Compras
** Objetivo: Listar Relat¢rio Gerencial de Compras, Faturamento e Produá∆o.
** Autor...: Gilvando Souza Araujo - Agosto/2006
** Obs.....: Especifico da TEAR T“XTIL INDÈSTRIA E COMêRCIO LTDA.
*/

/* include de controle de vers∆o */
{include/i-prgvrs.i ESCC0005RP 2.04.00.000}

define temp-table tt-param no-undo
       field destino         as integer
       field arquivo         as char format "x(35)"
       field usuario         as char format "x(12)"
       field data-exec       as date
       field hora-exec       as integer
       field classifica      as integer
       FIELD desc-classifica AS CHAR FORMAT "x(45)"
       FIELD cod-estabel     LIKE saldo-estoq.cod-estabel
       FIELD periodo-ini     AS CHAR FORMAT "x(7)"
       FIELD periodo-fin     AS CHAR FORMAT "x(7)"
       FIELD esp-docto-prd1  LIKE movto-estoq.esp-docto
       FIELD esp-docto-prd2  LIKE movto-estoq.esp-docto
       FIELD esp-docto-prd3  LIKE movto-estoq.esp-docto
       FIELD c-per           AS CHAR FORMAT "x(8)" EXTENT 12
       FIELD ge-compra-ini   LIKE item.ge-codigo         
       FIELD ge-compra-fin   LIKE item.ge-codigo 
       FIELD it-compra-ini   LIKE ITEM.it-codigo
       FIELD it-compra-fin   LIKE ITEM.it-codigo
       FIELD ge-fatur-ini    LIKE item.ge-codigo         
       FIELD ge-fatur-fin    LIKE item.ge-codigo 
       FIELD it-fatur-ini    LIKE ITEM.it-codigo
       FIELD it-fatur-fin    LIKE ITEM.it-codigo
       FIELD serie-fatur     LIKE nota-fiscal.serie
       FIELD esp-docto-fat   LIKE nota-fiscal.esp-docto
       FIELD data-conv       AS INTEGER  
       FIELD desc-data-conv  AS CHAR FORMAT "x(10)"
       FIELD mo-codigo       LIKE moeda.mo-codigo     
       FIELD desc-moeda      LIKE moeda.descricao    
       FIELD nao-confirm     AS LOGICAL FORMAT "Sim/N∆o"     
       FIELD confirm         AS LOGICAL FORMAT "Sim/N∆o"     
       FIELD em-cotacao      AS LOGICAL FORMAT "Sim/N∆o"     
       FIELD terminada       AS LOGICAL FORMAT "Sim/N∆o"     
       FIELD eliminada       AS LOGICAL FORMAT "Sim/N∆o"
       FIELD cotada          AS LOGICAL FORMAT "Sim/N∆o"
       FIELD gerar-excel     AS LOG FORMAT "Sim/N∆o"
       FIELD arq-excel       AS CHAR FORMAT "x(45)"
       FIELD impr-param      AS LOGICAL.

define temp-table tt-raw-digita
       field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

DEF TEMP-TABLE tt-compra
    FIELD it-codigo    LIKE ordem-compra.it-codigo
    FIELD sc-codigo    LIKE requisitante.sc-codigo
    FIELD quantidade   AS DEC EXTENT 13 FORMAT ">>>,>>>,>>9.99"                     
    FIELD valor        AS DEC EXTENT 13 FORMAT ">>>,>>>,>>9.99"
    INDEX ch-compra it-codigo
                    sc-codigo.

DEF TEMP-TABLE tt-faturamento
    FIELD it-codigo    LIKE ITEM.it-codigo
    FIELD quantidade   AS DEC EXTENT 13 FORMAT ">>>,>>>,>>9.99"
    FIELD valor        AS DEC EXTENT 13 FORMAT ">>>,>>>,>>9.99"
    INDEX ch-faturamento it-codigo.

DEF TEMP-TABLE tt-producao
    FIELD it-codigo    LIKE ITEM.it-codigo
    FIELD quantidade   AS DEC EXTENT 13 FORMAT ">>>,>>>,>>9.99"
    INDEX ch-producao it-codigo.

/* include padr∆o para vari†veis de relat¢rio  */
{include/i-rpvar.i}

/* definiá∆o de vari†veis  */
def var h-acomp as handle no-undo.

DEF VAR de-preco-unit   AS DEC FORMAT ">>,>>>,>>9.99".
DEF VAR da-data-aux     AS DATE.
DEF VAR de-cotacao-orig LIKE cotacao.cotacao[1].
DEF VAR de-cotacao-dest LIKE cotacao.cotacao[1].
DEF VAR de-tot-vlr-aux  AS DEC FORMAT ">>,>>>,>>9.99".
DEF VAR de-qt-conv      AS DEC.
DEF VAR da-data-ini     LIKE prazo-compra.data-entrega.
DEF VAR da-data-fin     LIKE prazo-compra.data-entrega.
DEF VAR c-per-aux       AS CHAR FORMAT "x(108)".
DEF VAR i-cont          AS INT.
DEF VAR de-acm-qtd      AS INT EXTENT 13.
DEF VAR de-acm-vlr      AS INT EXTENT 13.
DEF VAR de-tot-qtd      AS INT EXTENT 13.
DEF VAR de-tot-vlr      AS INT EXTENT 13.
DEF VAR de-pr-medio     AS DEC FORMAT ">>>,>>9.99" EXTENT 13.
DEF VAR c-arq-excel     LIKE tt-param.arq-excel.

DEF STREAM saida.

form
    "*-------------- ParÉmetros/Seleá∆o ----------------*" SKIP
    tt-param.desc-classifica LABEL "Classificaá∆o..." AT  1
    tt-param.cod-estabel     LABEL "Estabelemcimento" AT  1
    tt-param.ge-compra-ini   LABEL "Gr Estoque Compr" AT  1
    "a"                                               AT 35
    tt-param.ge-compra-fin   NO-LABEL                      
    tt-param.it-compra-ini   LABEL "Item compra....." AT  1
    "a"                                               AT 35
    tt-param.it-compra-fin   NO-LABEL                
    tt-param.ge-fatur-ini    LABEL "Gr Estoque fatur" AT  1
    "a"                                               AT 35
    tt-param.ge-fatur-fin    NO-LABEL                      
    tt-param.it-fatur-ini    LABEL "Item faturamento" AT  1
    "a"                                               AT 35
    tt-param.it-fatur-fin    NO-LABEL 
    tt-param.serie           LABEL "SÇrie NF........" AT  1
    tt-param.esp-docto-fat   LABEL "EspÇcie NF......" AT  1
    tt-param.periodo-ini     LABEL "Per°odo........." FORMAT "XXXX/XX" AT 1
    "a"                                               AT 35
    tt-param.periodo-fin     NO-LABELS                FORMAT "XXXX/XX"
    tt-param.esp-docto-prd1  LABEL "EspÇcies reporte" AT  1
    tt-param.esp-docto-prd2  NO-LABELS                AT 21
    tt-param.esp-docto-prd3  NO-LABELS                AT 25
    tt-param.desc-data-conv  LABEL "Data conv.compra" AT  1
    tt-param.mo-codigo       LABEL "Moeda..........." AT  1
    tt-param.desc-moeda      NO-LABELS                AT 21
    tt-param.nao-confirm     LABEL "N∆o Confirmada.." AT  1
    tt-param.confirm         LABEL "Confirmada......" AT  1
    tt-param.em-cotacao      LABEL "Em Cotaá∆o......" AT  1
    tt-param.terminada       LABEL "Terminada......." AT  1
    tt-param.eliminada       LABEL "Eliminada......." AT  1
    tt-param.cotada          LABEL "Cotada.........." AT  1
    tt-param.gerar-excel     LABEL "Gerar Excel....." AT  1
    tt-param.arq-excel       LABEL "Arquivo Excel..." AT  1
    with no-box side-labels width 132 STREAM-IO frame f-param.

/* include padr∆o para output de relat¢rios */
{include/i-rpout.i}

/* include com a definiá∆o da frame de cabeáalho e rodapÇ */
{include/i-rpcab.i}

/* funá∆o localizadora de data no vetor acumulador */
FUNCTION f-acha-mes RETURNS INTEGER (INPUT da-data AS DATE).
   DEFINE VARIABLE c-data AS CHARACTER  FORMAT "x(8)".
   DEFINE VAR i-indice AS INTEGER.

   ASSIGN c-data = SUBSTR("JanFevMarAbrMaiJunJulAgoSetOutNovDez",MONTH(da-data) * 3 - 2, 3) +
                   "/" + STRING(YEAR(da-data),"9999").
   i-indice = LOOKUP(c-data,c-per-aux).
   RETURN i-indice.
END FUNCTION.

/* bloco principal do programa */
find first param-global no-lock no-error.
find first empresa
     where empresa.ep-codigo = param-global.empresa-prin no-lock no-error. 

assign c-empresa = (if avail empresa then empresa.razao-social else "").

{utp/ut-liter.i COMPRAS * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Gerencial_de_Compras,_Faturamento_e_Produá∆o * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Processando *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

ASSIGN da-data-ini = DATE(int(SUBSTR(tt-param.periodo-ini,5,2)),1,int(substr(tt-param.periodo-ini,1,4)))
       da-data-aux = DATE(int(SUBSTR(tt-param.periodo-fin,5,2)),28,int(substr(tt-param.periodo-fin,1,4))) + 4.
       da-data-fin = DATE(MONTH(da-data-aux),1,YEAR(da-data-aux)) - 1.
MESSAGE da-data-ini
        da-data-aux
        da-data-fin
        tt-param.periodo-ini
        tt-param.periodo-fin
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
DO i-cont = 1 TO 12:
   ASSIGN c-per-aux = c-per-aux + tt.param.c-per[i-cont] + ",".
END.

FOR EACH ordem-compra WHERE ordem-compra.it-codigo >= tt-param.it-compra-ini
                        AND ordem-compra.it-codigo <= tt-param.it-compra-fin
                      NO-LOCK,
    EACH ITEM WHERE item.it-codigo = ordem-compra.it-codigo
                AND ITEM.ge-codigo >= tt-param.ge-compra-ini
                AND ITEM.ge-codigo <= tt-param.ge-compra-fin
              NO-LOCK,
    EACH requisitante WHERE requisitante.nome-abrev = ordem-compra.requisitante
                      NO-LOCK,
    EACH prazo-compra OF ordem-compra
                      WHERE prazo-compra.data-entrega >= da-data-ini
                        AND prazo-compra.data-entrega <= da-data-fin
                        AND ((prazo-compra.situacao = 1 AND tt-param.nao-confirm) OR
                             (prazo-compra.situacao = 2 AND tt-param.confirm) OR
                             (prazo-compra.situacao = 3 AND tt-param.cotada) OR
                             (prazo-compra.situacao = 4 AND tt-param.eliminada) OR
                             (prazo-compra.situacao = 5 AND tt-param.em-cotacao) OR
                             (prazo-compra.situacao = 6 AND tt-param.terminada))
                        NO-LOCK:
       
    RUN pi-acompanhar in h-acomp (input "Compras - Ordem: " + string(ordem-compra.numero-ordem)).

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
    
    ASSIGN de-tot-vlr-aux = prazo-compra.quantidade * de-preco-unit.

    FIND FIRST tt-compra WHERE tt-compra.it-codigo = ordem-compra.it-codigo
                           AND tt-compra.sc-codigo = requisitante.sc-codigo
                         NO-LOCK NO-ERROR.
    IF NOT AVAIL tt-compra THEN DO:
       CREATE tt-compra.
       ASSIGN tt-compra.it-codigo = ordem-compra.it-codigo
              tt-compra.sc-codigo = requisitante.sc-codigo.
    END.
    ASSIGN tt-compra.quantidade[f-acha-mes(prazo-compra.data-entrega)] = 
           tt-compra.quantidade[f-acha-mes(prazo-compra.data-entrega)] + prazo-compra.quantidade
           tt-compra.valor[f-acha-mes(prazo-compra.data-entrega)] = 
           tt-compra.valor[f-acha-mes(prazo-compra.data-entrega)] + de-tot-vlr-aux
           tt-compra.quantidade[13] = tt-compra.quantidade[13] + prazo-compra.quantidade 
           tt-compra.valor[13] = tt-compra.valor[13] + de-tot-vlr-aux. 
END.

FOR EACH nota-fiscal
    WHERE nota-fiscal.cod-estabel  =  tt-param.cod-estabel
      AND nota-fiscal.serie        =  tt-param.serie
      AND nota-fiscal.dt-emis-nota >= da-data-ini
      AND nota-fiscal.dt-emis-nota <= da-data-fin
      AND nota-fiscal.dt-cancela   =  ? 
      AND nota-fiscal.emite-duplic =  YES
    NO-LOCK,

    EACH it-nota-fisc OF nota-fiscal NO-LOCK,

    EACH ITEM WHERE ITEM.it-codigo =  it-nota-fisc.it-codigo
                AND ITEM.ge-codigo >= tt-param.ge-fatur-ini
                AND ITEM.ge-codigo <= tt-param.ge-fatur-fin
                AND ITEM.it-codigo >= tt-param.it-fatur-ini
                AND ITEM.it-codigo <= tt-param.it-fatur-fin
              NO-LOCK:

    RUN pi-acompanhar in h-acomp (input "Faturamento - NF: " + nota-fiscal.nr-nota-fis).
    
    /*------ Conversao de Kg para M ------- */
    IF it-nota-fisc.un-fatur[1] <> "m" THEN DO:
       FIND item-ext WHERE ITEM-ext.it-codigo = it-nota-fisc.it-codigo
                     NO-LOCK NO-ERROR.
       IF AVAIL item-ext THEN
          assign de-qt-conv = it-nota-fisc.qt-faturada[1] * 
                              item-ext.fator-conv.
       ELSE DO:
          MESSAGE "Falta fator de convers∆o para o Item: " it-nota-fisc.it-codigo
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
          ASSIGN de-qt-conv = it-nota-fisc.qt-faturada[1].
       END.
    END.
    ELSE
       ASSIGN de-qt-conv = it-nota-fisc.qt-faturada[1].

    FIND FIRST tt-faturamento WHERE tt-faturamento.it-codigo = it-nota-fisc.it-codigo 
                              NO-LOCK NO-ERROR.
    IF NOT AVAIL tt-faturamento THEN DO:
       CREATE tt-faturamento.
       ASSIGN tt-faturamento.it-codigo = it-nota-fisc.it-codigo.
    END.

    ASSIGN tt-faturamento.quantidade[f-acha-mes(it-nota-fisc.dt-emis-nota)] = 
           tt-faturamento.quantidade[f-acha-mes(it-nota-fisc.dt-emis-nota)] + de-qt-conv
           tt-faturamento.valor[f-acha-mes(it-nota-fisc.dt-emis-nota)] = 
           tt-faturamento.valor[f-acha-mes(it-nota-fisc.dt-emis-nota)] + it-nota-fisc.vl-tot-item
           tt-faturamento.quantidade[13] = tt-faturamento.quantidade[13] + de-qt-conv
           tt-faturamento.valor[13] = tt-faturamento.valor[13] + it-nota-fisc.vl-tot-item.
END.

FOR EACH movto-estoq USE-INDEX item-data
    WHERE movto-estoq.it-codigo  >= tt-param.it-fatur-ini
      AND movto-estoq.it-codigo  <= tt-param.it-fatur-fin
      AND movto-estoq.cod-estabel = tt-param.cod-estabel
      AND movto-estoq.cod-depos   = "exp"
      AND movto-estoq.dt-trans   >= da-data-ini
      AND movto-estoq.dt-trans   <= da-data-fin
      AND (movto-estoq.esp-docto  = tt-param.esp-docto-prd1 OR
           movto-estoq.esp-docto  = tt-param.esp-docto-prd2 OR
           movto-estoq.esp-docto  = tt-param.esp-docto-prd3)
    NO-LOCK:

    RUN pi-acompanhar in h-acomp (input "Produá∆o - Item: " + movto-estoq.it-codigo).
    
    /*------ Conversao de Kg para M ------- */
    FIND ITEM WHERE ITEM.it-codigo = movto-estoq.it-codigo NO-LOCK.
    IF item.un <> "m" THEN DO:
       FIND item-ext WHERE ITEM-ext.it-codigo = item.it-codigo
                     NO-LOCK NO-ERROR.
       IF AVAIL item-ext THEN
          assign de-qt-conv = movto-estoq.quantidade * 
                              item-ext.fator-conv.
       ELSE DO:
          MESSAGE "Falta fator de convers∆o para o Item: " ITEM.it-codigo
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
          ASSIGN de-qt-conv = movto-estoq.quantidade.
       END.
    END.
    ELSE
       ASSIGN de-qt-conv = movto-estoq.quantidade.

    IF movto-estoq.tipo-trans = 2 THEN
       ASSIGN de-qt-conv = de-qt-conv * -1.

    FIND FIRST tt-producao WHERE tt-producao.it-codigo = movto-estoq.it-codigo 
                           NO-LOCK NO-ERROR.
    IF NOT AVAIL tt-producao THEN DO:
       CREATE tt-producao.
       ASSIGN tt-producao.it-codigo = movto-estoq.it-codigo.
    END.

    ASSIGN tt-producao.quantidade[f-acha-mes(movto-estoq.dt-trans)] = 
           tt-producao.quantidade[f-acha-mes(movto-estoq.dt-trans)] + de-qt-conv
           tt-producao.quantidade[13] = tt-producao.quantidade[13] + de-qt-conv.
END.

/* - Planilha1 - Compras nos £ltimos 12 meses - Por Setor - */
ASSIGN c-arq-excel = REPLACE(tt-param.arq-excel,"#","1").
OUTPUT STREAM saida TO VALUE(c-arq-excel) CONVERT SOURCE "ibm850".

PUT STREAM saida
           c-empresa
           " - " 
           c-titulo-relat
           " - PERIODO: " 
           da-data-ini
           " a "
           da-data-fin
           SKIP(1)
           "Compras nos ultimos 12 meses - por Setor - Em R$"
           SKIP(1)
           "SETOR;"
           tt-param.c-per[1] ";"
           tt-param.c-per[2] ";"
           tt-param.c-per[3] ";"
           tt-param.c-per[4] ";"
           tt-param.c-per[5] ";"
           tt-param.c-per[6] ";"
           tt-param.c-per[7] ";"
           tt-param.c-per[8] ";"
           tt-param.c-per[9] ";"
           tt-param.c-per[10] ";"
           tt-param.c-per[11] ";"
           tt-param.c-per[12] ";"
           "TOTAL"
           SKIP.

FOR EACH tt-compra BREAK BY tt-compra.sc-codigo:
    DO i-cont = 1 TO 13:
       ASSIGN de-acm-vlr[i-cont] = de-acm-vlr[i-cont] + tt-compra.valor[i-cont] 
              de-tot-vlr[i-cont] = de-tot-vlr[i-cont] + tt-compra.valor[i-cont].    
    END.
    IF LAST-OF(tt-compra.sc-codigo) THEN DO:
       FIND sub-conta WHERE sub-conta.sc-codigo = tt-compra.sc-codigo NO-LOCK.
       PUT STREAM saida
                  sub-conta.descricao ";"
                  de-acm-vlr[1] ";"
                  de-acm-vlr[2] ";"
                  de-acm-vlr[3] ";"
                  de-acm-vlr[4] ";"
                  de-acm-vlr[5] ";"
                  de-acm-vlr[6] ";"
                  de-acm-vlr[7] ";"
                  de-acm-vlr[8] ";"
                  de-acm-vlr[9] ";"
                  de-acm-vlr[10] ";"
                  de-acm-vlr[11] ";"
                  de-acm-vlr[12] ";"
                  de-acm-vlr[13]
                  SKIP.
       ASSIGN de-acm-vlr = 0.
    END.
END.

PUT STREAM saida
           SKIP
           "Total Geral;"
           de-tot-vlr[1] ";"
           de-tot-vlr[2] ";"
           de-tot-vlr[3] ";"
           de-tot-vlr[4] ";"
           de-tot-vlr[5] ";"
           de-tot-vlr[6] ";"
           de-tot-vlr[7] ";"
           de-tot-vlr[8] ";"
           de-tot-vlr[9] ";"
           de-tot-vlr[10] ";"
           de-tot-vlr[11] ";"
           de-tot-vlr[12] ";"
           de-tot-vlr[13]
           SKIP.
ASSIGN de-tot-vlr = 0.
OUTPUT STREAM saida CLOSE.

/* - Planilha2 - Compras nos £ltimos 12 meses - Por Setor/Produto - */
ASSIGN c-arq-excel = REPLACE(tt-param.arq-excel,"#","2").
OUTPUT STREAM saida TO VALUE(c-arq-excel) CONVERT SOURCE "ibm850".

PUT STREAM saida
           c-empresa
           " - " 
           c-titulo-relat
           " - PERIODO: " 
           da-data-ini
           " a "
           da-data-fin
           SKIP(1)
           "Compras nos ultimos 12 meses - por Setor x Produto"
           SKIP(1)
           ";;"
           tt-param.c-per[1] ";;"
           tt-param.c-per[2] ";;"
           tt-param.c-per[3] ";;"
           tt-param.c-per[4] ";;"
           tt-param.c-per[5] ";;"
           tt-param.c-per[6] ";;"
           tt-param.c-per[7] ";;"
           tt-param.c-per[8] ";;"
           tt-param.c-per[9] ";;"
           tt-param.c-per[10] ";;"
           tt-param.c-per[11] ";;"
           tt-param.c-per[12] ";;"
           "TOTAL"
           SKIP
           "PRODUTO;" 
           "UN;"
           "QUANT;"     
           "VALOR;"     
           "QUANT;"   
           "VALOR;"       
           "QUANT;"    
           "VALOR;"        
           "QUANT;"    
           "VALOR;"        
           "QUANT;"    
           "VALOR;"        
           "QUANT;"   
           "VALOR;"       
           "QUANT;"    
           "VALOR;"        
           "QUANT;"  
           "VALOR;"      
           "QUANT;"    
           "VALOR;"        
           "QUANT;"  
           "VALOR;"      
           "QUANT;"   
           "VALOR;"
           "QUANT;" 
           "VALOR;" 
           "QUANT;"    
           "VALOR"
           SKIP(1).

FOR EACH tt-compra,
    EACH ITEM WHERE ITEM.it-codigo = tt-compra.it-codigo NO-LOCK
    BREAK BY tt-compra.sc-codigo
          BY IF tt-param.classifica = 1 THEN item.desc-item
                                        ELSE ITEM.it-codigo:

    DO i-cont = 1 TO 13:
       ASSIGN de-acm-qtd[i-cont] = de-acm-qtd[i-cont] + tt-compra.quantidade[i-cont]
              de-acm-vlr[i-cont] = de-acm-vlr[i-cont] + tt-compra.valor[i-cont]
              de-tot-qtd[i-cont] = de-tot-qtd[i-cont] + tt-compra.quantidade[i-cont]
              de-tot-vlr[i-cont] = de-tot-vlr[i-cont] + tt-compra.valor[i-cont].    
    END.

    IF FIRST-OF(tt-compra.sc-codigo) THEN DO:
       FIND sub-conta WHERE sub-conta.sc-codigo = tt-compra.sc-codigo NO-LOCK.
       PUT STREAM saida 
                  "Setor: " sub-conta.descricao
                  SKIP(1).
    END.

    PUT STREAM saida
               item.desc-item FORMAT "x(30)" ";"
               ITEM.un ";"
               int(tt-compra.quantidade[1]) ";"
               int(tt-compra.valor[1]) ";"
               int(tt-compra.quantidade[2]) ";"
               int(tt-compra.valor[2]) ";"
               int(tt-compra.quantidade[3]) ";"
               int(tt-compra.valor[3]) ";"
               int(tt-compra.quantidade[4]) ";"
               int(tt-compra.valor[4]) ";"
               int(tt-compra.quantidade[5]) ";"
               int(tt-compra.valor[5]) ";"
               int(tt-compra.quantidade[6]) ";"
               int(tt-compra.valor[6]) ";"
               int(tt-compra.quantidade[7]) ";"
               int(tt-compra.valor[7]) ";"
               int(tt-compra.quantidade[8]) ";"
               int(tt-compra.valor[8]) ";"
               int(tt-compra.quantidade[9]) ";"
               int(tt-compra.valor[9]) ";"
               int(tt-compra.quantidade[10]) ";"
               int(tt-compra.valor[10]) ";"
               int(tt-compra.quantidade[11]) ";"
               int(tt-compra.valor[11]) ";"
               int(tt-compra.quantidade[12]) ";"
               int(tt-compra.valor[12]) ";"
               int(tt-compra.quantidade[13]) ";"
               int(tt-compra.valor[13])
               SKIP.

    IF LAST-OF(tt-compra.sc-codigo) THEN DO:
       FIND sub-conta WHERE sub-conta.sc-codigo = tt-compra.sc-codigo NO-LOCK.
       PUT STREAM saida
                  "Total Setor" ";;"
                  de-acm-qtd[1] ";"
                  de-acm-vlr[1] ";"
                  de-acm-qtd[2] ";"
                  de-acm-vlr[2] ";"
                  de-acm-qtd[3] ";"
                  de-acm-vlr[3] ";"
                  de-acm-qtd[4] ";"
                  de-acm-vlr[4] ";"
                  de-acm-qtd[5] ";"
                  de-acm-vlr[5] ";"
                  de-acm-qtd[6] ";"
                  de-acm-vlr[6] ";"
                  de-acm-qtd[7] ";"
                  de-acm-vlr[7] ";"
                  de-acm-qtd[8] ";"
                  de-acm-vlr[8] ";"
                  de-acm-qtd[9] ";"
                  de-acm-vlr[9] ";"
                  de-acm-qtd[10] ";"
                  de-acm-vlr[10] ";"
                  de-acm-qtd[11] ";"
                  de-acm-vlr[11] ";"
                  de-acm-qtd[12] ";"
                  de-acm-vlr[12] ";"
                  de-acm-qtd[13] ";" 
                  de-acm-vlr[13] 
                  SKIP(1).
       ASSIGN de-acm-qtd = 0
              de-acm-vlr = 0.
    END.
END.

PUT STREAM saida
           SKIP
           "Total Geral;;"
           de-tot-qtd[1] ";"
           de-tot-vlr[1] ";"
           de-tot-qtd[2] ";"
           de-tot-vlr[2] ";"
           de-tot-qtd[3] ";"
           de-tot-vlr[3] ";"
           de-tot-qtd[4] ";"
           de-tot-vlr[4] ";"
           de-tot-qtd[5] ";"
           de-tot-vlr[5] ";"
           de-tot-qtd[6] ";"
           de-tot-vlr[6] ";"
           de-tot-qtd[7] ";"
           de-tot-vlr[7] ";"
           de-tot-qtd[8] ";"
           de-tot-vlr[8] ";"
           de-tot-qtd[9] ";"
           de-tot-vlr[9] ";"
           de-tot-qtd[10] ";"
           de-tot-vlr[10] ";"
           de-tot-qtd[11] ";"
           de-tot-vlr[11] ";"
           de-tot-qtd[12] ";"
           de-tot-vlr[12] ";"
           de-tot-qtd[13] ";" 
           de-tot-vlr[13] 
           SKIP.
ASSIGN de-tot-qtd = 0
       de-tot-vlr = 0.
OUTPUT STREAM saida CLOSE.

/* - Planilha3 - Compras, Faturamento e Produá∆o nos £ltimos 12 meses - */
ASSIGN c-arq-excel = REPLACE(tt-param.arq-excel,"#","3").
OUTPUT STREAM saida TO VALUE(c-arq-excel) CONVERT SOURCE "ibm850".

PUT STREAM saida
           c-empresa
           " - " 
           c-titulo-relat
           " - PERIODO: " 
           da-data-ini
           " a "
           da-data-fin
           SKIP(1)
           "Compras, Faturamento e Produá∆o nos ultimos 12 meses"
           SKIP(1)
           ";"
           tt-param.c-per[1] ";"
           tt-param.c-per[2] ";"
           tt-param.c-per[3] ";"
           tt-param.c-per[4] ";"
           tt-param.c-per[5] ";"
           tt-param.c-per[6] ";"
           tt-param.c-per[7] ";"
           tt-param.c-per[8] ";"
           tt-param.c-per[9] ";"
           tt-param.c-per[10] ";"
           tt-param.c-per[11] ";"
           tt-param.c-per[12] ";"
           "TOTAL"
           SKIP.

ASSIGN de-tot-qtd = 0
       de-tot-vlr = 0.
FOR EACH tt-compra:
    DO i-cont = 1 TO 13:
       ASSIGN de-tot-qtd[i-cont] = de-tot-qtd[i-cont] + tt-compra.quantidade[i-cont] 
              de-tot-vlr[i-cont] = de-tot-vlr[i-cont] + tt-compra.valor[i-cont].     
    END.
END.

PUT STREAM saida
           "COMPRAS - Valor;"
           de-tot-vlr[1] ";"
           de-tot-vlr[2] ";"
           de-tot-vlr[3] ";"
           de-tot-vlr[4] ";"
           de-tot-vlr[5] ";"
           de-tot-vlr[6] ";"
           de-tot-vlr[7] ";"
           de-tot-vlr[8] ";"
           de-tot-vlr[9] ";"
           de-tot-vlr[10] ";"
           de-tot-vlr[11] ";"
           de-tot-vlr[12] ";"
           de-tot-vlr[13]
           SKIP(1).

ASSIGN de-tot-qtd = 0
       de-tot-vlr = 0.

FOR EACH tt-faturamento:
    DO i-cont = 1 TO 13:
       ASSIGN de-tot-qtd[i-cont] = de-tot-qtd[i-cont] + tt-faturamento.quantidade[i-cont] 
              de-tot-vlr[i-cont] = de-tot-vlr[i-cont] + tt-faturamento.valor[i-cont].     
    END.
END.

ASSIGN de-pr-medio = 0.
DO i-cont = 1 TO 13:
   IF de-tot-qtd[i-cont] <> 0 THEN
      ASSIGN de-pr-medio[i-cont] = de-tot-vlr[i-cont] / de-tot-qtd[i-cont].
END.

PUT STREAM saida
           "FATURAMENTO - Quantidade;"
           de-tot-qtd[1] ";"
           de-tot-qtd[2] ";"
           de-tot-qtd[3] ";"
           de-tot-qtd[4] ";"
           de-tot-qtd[5] ";"
           de-tot-qtd[6] ";"
           de-tot-qtd[7] ";"
           de-tot-qtd[8] ";"
           de-tot-qtd[9] ";"
           de-tot-qtd[10] ";"
           de-tot-qtd[11] ";"
           de-tot-qtd[12] ";"
           de-tot-qtd[13]
           SKIP
           "FATURAMENTO - Valor;"
           de-tot-vlr[1] ";"
           de-tot-vlr[2] ";"
           de-tot-vlr[3] ";"
           de-tot-vlr[4] ";"
           de-tot-vlr[5] ";"
           de-tot-vlr[6] ";"
           de-tot-vlr[7] ";"
           de-tot-vlr[8] ";"
           de-tot-vlr[9] ";"
           de-tot-vlr[10] ";"
           de-tot-vlr[11] ";"
           de-tot-vlr[12] ";"
           de-tot-vlr[13]
           SKIP
           "FATURAMENTO - Pr.Medio;"
           de-pr-medio[1] ";"
           de-pr-medio[2] ";"
           de-pr-medio[3] ";"
           de-pr-medio[4] ";"
           de-pr-medio[5] ";"
           de-pr-medio[6] ";"
           de-pr-medio[7] ";"
           de-pr-medio[8] ";"
           de-pr-medio[9] ";"
           de-pr-medio[10] ";"
           de-pr-medio[11] ";"
           de-pr-medio[12] ";"
           de-pr-medio[13]
           SKIP(1).

ASSIGN de-tot-qtd = 0.
FOR EACH tt-producao:
    DO i-cont = 1 TO 13:
       ASSIGN de-tot-qtd[i-cont] = de-tot-qtd[i-cont] + tt-producao.quantidade[i-cont].     
    END.
END.

PUT STREAM saida
           "PRODUCAO - Quantidade;"
           de-tot-qtd[1] ";"
           de-tot-qtd[2] ";"
           de-tot-qtd[3] ";"
           de-tot-qtd[4] ";"
           de-tot-qtd[5] ";"
           de-tot-qtd[6] ";"
           de-tot-qtd[7] ";"
           de-tot-qtd[8] ";"
           de-tot-qtd[9] ";"
           de-tot-qtd[10] ";"
           de-tot-qtd[11] ";"
           de-tot-qtd[12] ";"
           de-tot-qtd[13]
           SKIP(1).

OUTPUT STREAM saida CLOSE.

IF tt-param.impr-param THEN DO:
   PAGE.
   display tt-param.desc-classifica
           tt-param.cod-estabel
           tt-param.ge-compra-ini  
           tt-param.ge-compra-fin  
           tt-param.it-compra-ini  
           tt-param.it-compra-fin
           tt-param.ge-fatur-ini
           tt-param.ge-fatur-fin
           tt-param.it-fatur-ini
           tt-param.it-fatur-fin
           tt-param.serie
           tt-param.esp-docto-fat
           tt-param.periodo-ini 
           tt-param.periodo-fin
           tt-param.esp-docto-prd1
           tt-param.esp-docto-prd2
           tt-param.esp-docto-prd3
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
