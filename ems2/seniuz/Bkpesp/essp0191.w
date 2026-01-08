&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESSP0191 2.04.00.999}

{utp/utapi011.i} /* Geraá∆o de Graficos */

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
/* Global Variable Definitions ---                                       */

DEF NEW GLOBAL SHARED VAR gr-item AS ROWID NO-UNDO.

DEF TEMP-TABLE tt-digita
    FIELD opcao  AS CHAR
    FIELD campo AS CHAR
    FIELD valor AS CHAR.

/* Temp Table Definitions ---                                           */
DEF TEMP-TABLE tt-itens 
    FIELD it-codigo       LIKE movto-estoq.it-codigo
    FIELD cod-refer       LIKE movto-estoq.cod-refer
    FIELD nr-lote         LIKE ob-etiqueta.nr-lote
    FIELD desc-item       LIKE ITEM.desc-item
    FIELD un              LIKE ITEM.un
    FIELD estoque-ini     AS DEC 
    FIELD estoque-ent     AS DEC 
    FIELD estoque-div-ent AS DEC 
    FIELD estoque-div-sai AS DEC
    FIELD estoque-inv-sai AS DEC 
    FIELD estoque-inv-ent AS DEC
    FIELD estoque-dev     AS DEC 
    FIELD estoque-fat     AS DEC 
    FIELD estoque-final   AS DEC
    FIELD vl-movto        AS DEC
    INDEX indice1 IS PRIMARY it-codigo.

DEF TEMP-TABLE tt-nfe
    FIELD cod-estabel  LIKE movto-estoq.cod-estabel
    FIELD serie        LIKE movto-estoq.serie-docto
    FIELD nr-nota-fis  LIKE movto-estoq.nro-docto  
    FIELD it-codigo    LIKE movto-estoq.it-codigo  
    FIELD cod-rep      LIKE nota-fiscal.cod-rep.   
    


DEF TEMP-TABLE tt-nfiscal 
    FIELD cod-estabel  LIKE movto-estoq.cod-estabel
    FIELD serie        LIKE movto-estoq.serie-docto
    FIELD nr-nota-fis  LIKE movto-estoq.nro-docto
    FIELD it-codigo    LIKE movto-estoq.it-codigo
    FIELD cod-rep      LIKE nota-fiscal.cod-rep.

DEF TEMP-TABLE tt-work      NO-UNDO
    FIELD visualiza         AS LOG
    FIELD seq-item          AS INT
    FIELD seq-repres        AS INT
    FIELD seq-grupo         AS INT
    FIELD seq-cliente       AS INT
    FIELD seq-regiao        AS INT
    FIELD seq-uf            AS INT
    FIELD seq-nat-oper      AS INT
    FIELD seq-cond-pg       AS INT
    FIELD it-codigo         LIKE ped-item.it-codigo
    FIELD desc-item         LIKE ITEM.desc-item
    FIELD no-ab-reppri      LIKE ped-venda.no-ab-reppri
    FIELD cod-rep           LIKE repres.cod-rep
    FIELD matriz            LIKE emitente.nome-matriz
    FIELD nome-abrev        LIKE ped-venda.nome-abrev
    FIELD cod-emit          LIKE emitente.cod-emit
    FIELD regiao            AS CHAR FORMAT "x(20)"
    FIELD nat-operacao      LIKE natur-oper.nat-operacao
    FIELD aliq-icms         LIKE natur-oper.aliquota-icm
    FIELD vl-icms           LIKE it-nota-fisc.vl-icms-it
    FIELD cond-pagto        AS CHAR
    FIELD uf                AS CHAR
    FIELD lote              AS CHAR
    FIELD Und               AS CHAR
    FIELD qtd               AS DEC
    FIELD qtd-devol         AS DEC
    FIELD vlr               AS DEC
    FIELD vlr-devol         AS DEC
    FIELD vlr-custo         AS DEC
    FIELD preco-medio       AS DEC
    FIELD prazo-medio       AS DEC
    FIELD rentabilidade     AS DEC
    FIELD perc-sobr-total   AS DEC
    INDEX indice1 it-codigo    und lote 
    INDEX indice2 no-ab-reppri und lote 
    INDEX indice3 matriz       und lote 
    INDEX indice4 nome-abrev   und lote 
    INDEX indice5 regiao       und lote uf
    INDEX indice6 nat-operacao und lote 
    INDEX indice7 cond-pagto   und lote.

DEF TEMP-TABLE tt-devolucao NO-UNDO
    FIELD cod-estabel  LIKE movto-estoq.cod-estabel
    FIELD serie-docto  LIKE docum-est.serie-docto
    FIELD nro-docto    LIKE docum-est.nro-docto
    FIELD cod-emitente LIKE docum-est.cod-emitente
    FIELD nat-operacao LIKE docum-est.nat-operacao
    FIELD qtd-devol    AS DEC 
    FIELD vlr-devol    AS DEC.

DEF BUFFER b-tt-itens FOR tt-itens.                                                         

/* --- Local Variable Definitions --- */
DEF VAR h-acomp        AS HANDLE NO-UNDO.
DEF VAR h-query        AS HANDLE.
DEF VAR c-titulo       AS CHAR FORMAT "x(55)".
DEF VAR c-it-codigo    AS CHAR.
DEF VAR de-qtd-ini     AS DEC.
DEF VAR de-qtd-fin     AS DEC.
DEF VAR de-qtd-div-ent AS DEC.
DEF VAR de-qtd-div-sai AS DEC.
DEF VAR de-qtd-inv-ent AS DEC.
DEF VAR de-qtd-inv-sai AS DEC.
DEF VAR de-qtd-nfd     AS DEC.
DEF VAR de-qtd-nfe     AS DEC.
DEF VAR de-qtd-nfs     AS DEC.
DEF VAR c-empresa      AS CHAR.
DEF VAR i-cod-rep      AS INT.

/* Variaveis usadas na Rotina de Impress∆o */
DEFINE VAR i-saida      AS INT.
DEFINE VAR c-saida      AS CHAR.
DEFINE VAR i-num-copias AS INT.
DEFINE VAR i-lin        AS INT.
DEFINE VAR i-pag        AS INT.

/* Variaveis para o Excel */
DEF VAR chExcelApp  AS COM-HANDLE NO-UNDO.
DEF VAR chWorkbook  AS COM-HANDLE NO-UNDO.
DEF VAR chworksheet AS COM-HANDLE NO-UNDO.
DEF VAR c-lin       AS CHAR FORMAT "x(500)".
DEF VAR arq-saida   AS CHAR FORMAT "x(50)".

/* Variavies de ParÉmetros */
DEFINE VAR c-cod-estabel      LIKE movto-estoq.cod-estabel INIT "1".
DEFINE VAR da-dt-trans-ini    AS DATE.
DEFINE VAR da-dt-trans-fin    AS DATE.
DEFINE VAR c-it-codigo-ini    LIKE ob-etiqueta.it-codigo     INIT "".
DEFINE VAR c-it-codigo-fin    LIKE ob-etiqueta.it-codigo     INIT "ZZZZZZ".
DEF VAR c-cod-refer-ini    LIKE ped-item.cod-refer.
DEF VAR c-cod-refer-fin    LIKE ped-item.cod-refer         INIT "ZZZZZZZZZZ".
DEF VAR c-corte-comerc-ini LIKE ob-etiqueta.corte-comerc   INIT "A".
DEF VAR c-corte-comerc-fin LIKE ob-etiqueta.corte-comerc   INIT "Z".
DEF VAR c-cod-depos        LIKE saldo-estoq.cod-depos      INIT "EXP".
DEF VAR l-lote-todos       AS LOG INIT YES.
DEF VAR l-lote-pp          AS LOG INIT NO.
DEF VAR l-lote-pd          AS LOG INIT NO.
DEF VAR l-lote-rp          AS LOG INIT NO.
DEF VAR l-lote-rd          AS LOG INIT NO.
DEF VAR l-lote-sc          AS LOG INIT NO.
DEF VAR l-lote-ca          AS LOG INIT NO.
DEF VAR i-opc-acabado      AS INT INIT 3.
DEFINE VAR l-ok               AS LOG.


DEF VAR c-lotes            AS CHAR FORMAT "x(18)".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME br-itens

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-itens

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br-itens                                      */
&Scoped-define FIELDS-IN-QUERY-br-itens tt-itens.it-codigo tt-itens.cod-refer tt-itens.nr-lote tt-itens.un tt-itens.estoque-ini tt-itens.estoque-ent tt-itens.estoque-div-ent tt-itens.estoque-div-sai tt-itens.estoque-inv-ent tt-itens.estoque-inv-sai tt-itens.estoque-dev tt-itens.estoque-fat tt-itens.estoque-final   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-itens   
&Scoped-define SELF-NAME br-itens
&Scoped-define OPEN-QUERY-br-itens RUN pi-tot-itens. OPEN QUERY {&SELF-NAME} FOR EACH tt-itens WHERE NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-itens tt-itens
&Scoped-define FIRST-TABLE-IN-QUERY-br-itens tt-itens


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-itens RECT-6 RECT-7 rt-buttom bt-param ~
bt-vapara bt-excel bt-imprime bt-etiquetas bt-nfiscal bt-nfiscal-tot bt-nfe ~
bt-devolucao bt-Grafico-qtd fi-desc-item bt-exit bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS fi-desc-item fi-ini-m fi-ent-m ~
fi-div-ent-m fi-div-sai-m fi-inv-ent-m fi-inv-sai-m fi-dev-m fi-fat-m ~
fi-tot-m fi-ini-kg fi-ent-kg fi-div-ent-kg fi-div-sai-kg fi-inv-ent-kg ~
fi-inv-sai-kg fi-dev-kg fi-fat-kg fi-tot-kg fi-ini-und fi-ent-und ~
fi-div-ent-und fi-div-sai-und fi-inv-ent-und fi-inv-sai-und fi-dev-und ~
fi-fat-und fi-tot-und 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-4 bt-nfiscal bt-nfiscal-tot 
&Scoped-define List-6 bt-excel bt-nfiscal-tot bt-Grafico-qtd 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ajuda 
     IMAGE-UP FILE "image/im-hel.bmp":U
     LABEL "&Ajuda" 
     SIZE 4.86 BY 1.29 TOOLTIP "Help on Line"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-devolucao 
     IMAGE-UP FILE "image/im-desc.bmp":U
     LABEL "" 
     SIZE 5 BY 1.21 TOOLTIP "Detalhar Devoluá‰es"
     BGCOLOR 8 .

DEFINE BUTTON bt-etiquetas 
     IMAGE-UP FILE "image/im-local.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Detalha Item Selecionado"
     BGCOLOR 8 .

DEFINE BUTTON bt-excel 
     IMAGE-UP FILE "image/excel.bmp":U
     LABEL "Button 2" 
     SIZE 5 BY 1.25 TOOLTIP "Gerar Planilha da Posiá∆o de Estoque".

DEFINE BUTTON bt-exit AUTO-END-KEY 
     IMAGE-UP FILE "image/im-exi.bmp":U
     LABEL "" 
     SIZE 4.86 BY 1.29 TOOLTIP "Fechar Programa"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-Grafico-qtd 
     IMAGE-UP FILE "image/im-grf.bmp":U
     LABEL "Grafico" 
     SIZE 5 BY 1.21 TOOLTIP "Visualizar o Grafico das Metragens".

DEFINE BUTTON bt-imprime 
     IMAGE-UP FILE "image/im-printer.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Imprimir a Posiá∆o do Estoque".

DEFINE BUTTON bt-nfe 
     IMAGE-UP FILE "image/im-carg.bmp":U
     LABEL "" 
     SIZE 5 BY 1.21 TOOLTIP "Detalhar Notas Fiscais de Entrada"
     BGCOLOR 8 .

DEFINE BUTTON bt-nfiscal AUTO-GO 
     IMAGE-UP FILE "image/im-extra.bmp":U
     LABEL "" 
     SIZE 5 BY 1.21 TOOLTIP "Detalhar Faturamento do Item Posicionado"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-nfiscal-tot AUTO-GO 
     IMAGE-UP FILE "image/vendas-tot.bmp":U
     LABEL "" 
     SIZE 5 BY 1.21 TOOLTIP "Detalhar Faturamento de Todos os Itens"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-param AUTO-GO 
     IMAGE-UP FILE "image/im-fil.bmp":U
     LABEL "&Sair" 
     SIZE 5 BY 1.25 TOOLTIP "ParÉmetros"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-vapara AUTO-GO 
     IMAGE-UP FILE "image/im-vapra.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Posicionar no Item"
     BGCOLOR 8 FONT 10.

DEFINE VARIABLE fi-desc-item AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 65 BY .88
     BGCOLOR 15 FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE fi-dev-kg AS DECIMAL FORMAT "->>,>>>,>>9.99":R18 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-dev-m AS DECIMAL FORMAT "->>,>>>,>>9.99":R18 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-dev-und AS DECIMAL FORMAT "->>,>>>,>>9.99":R18 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-div-ent-kg AS DECIMAL FORMAT "->>,>>>,>>9.99":R18 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-div-ent-m AS DECIMAL FORMAT "->>,>>>,>>9.99":R18 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-div-ent-und AS DECIMAL FORMAT "->>,>>>,>>9.99":R18 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-div-sai-kg AS DECIMAL FORMAT "->>,>>>,>>9.99":R18 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-div-sai-m AS DECIMAL FORMAT "->>,>>>,>>9.99":R18 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-div-sai-und AS DECIMAL FORMAT "->>,>>>,>>9.99":R18 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-ent-kg AS DECIMAL FORMAT "->>,>>>,>>9.99":R18 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-ent-m AS DECIMAL FORMAT "->>,>>>,>>9.99":R18 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-ent-und AS DECIMAL FORMAT "->>,>>>,>>9.99":R18 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-fat-kg AS DECIMAL FORMAT "->>,>>>,>>9.99":R18 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-fat-m AS DECIMAL FORMAT "->>,>>>,>>9.99":R18 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-fat-und AS DECIMAL FORMAT "->>,>>>,>>9.99":R18 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-ini-kg AS DECIMAL FORMAT "->>,>>>,>>9.99":R20 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-ini-m AS DECIMAL FORMAT "->>,>>>,>>9.99":R20 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-ini-und AS DECIMAL FORMAT "->>,>>>,>>9.99":R20 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-inv-ent-kg AS DECIMAL FORMAT "->>,>>>,>>9.99":R18 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-inv-ent-m AS DECIMAL FORMAT "->>,>>>,>>9.99":R18 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-inv-ent-und AS DECIMAL FORMAT "->>,>>>,>>9.99":R18 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-inv-sai-kg AS DECIMAL FORMAT "->>,>>>,>>9.99":R18 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-inv-sai-m AS DECIMAL FORMAT "->>,>>>,>>9.99":R18 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-inv-sai-und AS DECIMAL FORMAT "->>,>>>,>>9.99":R18 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-tot-kg AS DECIMAL FORMAT "->>,>>>,>>9.99":R20 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88
     FGCOLOR 12 FONT 1 NO-UNDO.

DEFINE VARIABLE fi-tot-m AS DECIMAL FORMAT "->>,>>>,>>9.99":R20 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88
     FGCOLOR 12 FONT 1 NO-UNDO.

DEFINE VARIABLE fi-tot-und AS DECIMAL FORMAT "->>,>>>,>>9.99":R20 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88
     FGCOLOR 12 FONT 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 115 BY 4.13.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 115 BY 1.25.

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 6.72 BY 20
     BGCOLOR 8 FGCOLOR 0 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-itens FOR 
      tt-itens SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-itens
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-itens C-Win _FREEFORM
  QUERY br-itens NO-LOCK DISPLAY
      tt-itens.it-codigo       FORMAT "x(9)"           COLUMN-LABEL "Item"            WIDTH  8
      tt-itens.cod-refer       FORMAT "x(9)"           COLUMN-LABEL "Refer"           WIDTH  8
      tt-itens.nr-lote         FORMAT "x(4)"           COLUMN-LABEL "Lote"            WIDTH  3
      tt-itens.un              FORMAT "x(3)"           COLUMN-LABEL "Und"             WIDTH  3
      tt-itens.estoque-ini     FORMAT "->>,>>>,>>9.99" COLUMN-LABEL "Estoque Inicial" WIDTH 12 
      tt-itens.estoque-ent     FORMAT "->>,>>>,>>9.99" COLUMN-LABEL "Entradas"        WIDTH 10 
      tt-itens.estoque-div-ent FORMAT "->>,>>>,>>9.99" COLUMN-LABEL "Div Entrada"     WIDTH 10 
      tt-itens.estoque-div-sai FORMAT "->>,>>>,>>9.99" COLUMN-LABEL "Div Saida"       WIDTH 10 
      tt-itens.estoque-inv-ent FORMAT "->>,>>>,>>9.99" COLUMN-LABEL "Inv Entrada"     WIDTH 10 
      tt-itens.estoque-inv-sai FORMAT "->>,>>>,>>9.99" COLUMN-LABEL "Inv Saida"       WIDTH 10 
      tt-itens.estoque-dev     FORMAT "->>,>>>,>>9.99" COLUMN-LABEL "Devoluá∆o"       WIDTH 10 
      tt-itens.estoque-fat     FORMAT "->>,>>>,>>9.99" COLUMN-LABEL "Faturamento"     WIDTH 10 
      tt-itens.estoque-final   FORMAT "->>,>>>,>>9.99" COLUMN-LABEL "Estoque Final"   WIDTH 12.7
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 115 BY 13.96
         FONT 1
         TITLE "ITENS" ROW-HEIGHT-CHARS .5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     br-itens AT ROW 1.04 COL 1
     bt-param AT ROW 1.54 COL 117.72
     bt-vapara AT ROW 2.96 COL 117.72
     bt-excel AT ROW 4.33 COL 117.86
     bt-imprime AT ROW 5.71 COL 117.86
     bt-etiquetas AT ROW 7.08 COL 117.86
     bt-nfiscal AT ROW 8.46 COL 117.86
     bt-nfiscal-tot AT ROW 9.79 COL 117.86
     bt-nfe AT ROW 11.54 COL 117.86 WIDGET-ID 8
     bt-devolucao AT ROW 12.88 COL 117.86 WIDGET-ID 2
     bt-Grafico-qtd AT ROW 14.13 COL 117.86 WIDGET-ID 4
     fi-desc-item AT ROW 15.46 COL 36.72 COLON-ALIGNED NO-LABEL
     fi-ini-m AT ROW 17.88 COL 10 COLON-ALIGNED NO-LABEL
     fi-ent-m AT ROW 17.88 COL 23.43 COLON-ALIGNED NO-LABEL
     fi-div-ent-m AT ROW 17.88 COL 33.72 COLON-ALIGNED NO-LABEL
     fi-div-sai-m AT ROW 17.88 COL 44.14 COLON-ALIGNED NO-LABEL
     fi-inv-ent-m AT ROW 17.88 COL 54.72 COLON-ALIGNED NO-LABEL
     fi-inv-sai-m AT ROW 17.88 COL 65.29 COLON-ALIGNED NO-LABEL
     fi-dev-m AT ROW 17.88 COL 75.72 COLON-ALIGNED NO-LABEL
     fi-fat-m AT ROW 17.88 COL 86.29 COLON-ALIGNED NO-LABEL
     fi-tot-m AT ROW 17.88 COL 96.72 COLON-ALIGNED NO-LABEL
     bt-exit AT ROW 18.21 COL 117.72
     fi-ini-kg AT ROW 18.88 COL 10 COLON-ALIGNED NO-LABEL
     fi-ent-kg AT ROW 18.88 COL 23.43 COLON-ALIGNED NO-LABEL
     fi-div-ent-kg AT ROW 18.88 COL 33.72 COLON-ALIGNED NO-LABEL
     fi-div-sai-kg AT ROW 18.88 COL 44.14 COLON-ALIGNED NO-LABEL
     fi-inv-ent-kg AT ROW 18.88 COL 54.72 COLON-ALIGNED NO-LABEL
     fi-inv-sai-kg AT ROW 18.88 COL 65.29 COLON-ALIGNED NO-LABEL
     fi-dev-kg AT ROW 18.88 COL 75.72 COLON-ALIGNED NO-LABEL
     fi-fat-kg AT ROW 18.88 COL 86.29 COLON-ALIGNED NO-LABEL
     fi-tot-kg AT ROW 18.88 COL 96.72 COLON-ALIGNED NO-LABEL
     bt-ajuda AT ROW 19.54 COL 117.72
     fi-ini-und AT ROW 19.88 COL 10 COLON-ALIGNED NO-LABEL
     fi-ent-und AT ROW 19.88 COL 23.43 COLON-ALIGNED NO-LABEL
     fi-div-ent-und AT ROW 19.88 COL 33.72 COLON-ALIGNED NO-LABEL
     fi-div-sai-und AT ROW 19.88 COL 44.14 COLON-ALIGNED NO-LABEL
     fi-inv-ent-und AT ROW 19.88 COL 54.72 COLON-ALIGNED NO-LABEL
     fi-inv-sai-und AT ROW 19.88 COL 65.29 COLON-ALIGNED NO-LABEL
     fi-dev-und AT ROW 19.88 COL 75.72 COLON-ALIGNED NO-LABEL
     fi-fat-und AT ROW 19.88 COL 86.29 COLON-ALIGNED NO-LABEL
     fi-tot-und AT ROW 19.88 COL 96.72 COLON-ALIGNED NO-LABEL
     "UND:" VIEW-AS TEXT
          SIZE 5.14 BY .67 AT ROW 19.96 COL 6.43
          FGCOLOR 9 
     "ESTOQUE FINAL" VIEW-AS TEXT
          SIZE 12 BY .58 AT ROW 17.25 COL 99.29
          FGCOLOR 12 FONT 1
     "DESCRIÄ«O DO ITEM:" VIEW-AS TEXT
          SIZE 20.29 BY .79 AT ROW 15.54 COL 18
          FGCOLOR 9 
     "Faturamento" VIEW-AS TEXT
          SIZE 9 BY .58 AT ROW 17.25 COL 89.43
          FGCOLOR 9 FONT 1
     "ESTOQUE INICIAL" VIEW-AS TEXT
          SIZE 13 BY .58 AT ROW 17.25 COL 12.14
          FGCOLOR 9 FONT 1
     "Devoluá∆o" VIEW-AS TEXT
          SIZE 8 BY .58 AT ROW 17.25 COL 80
          FGCOLOR 9 FONT 1
     "Entradas" VIEW-AS TEXT
          SIZE 6.29 BY .58 AT ROW 17.25 COL 29
          FGCOLOR 9 FONT 1
     "Inv Saida" VIEW-AS TEXT
          SIZE 7 BY .58 AT ROW 17.25 COL 70.29
          FGCOLOR 9 FONT 1
     "KG:" VIEW-AS TEXT
          SIZE 3.72 BY .67 AT ROW 18.96 COL 7.86
          FGCOLOR 9 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.04
         SIZE 123 BY 20.33.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME DEFAULT-FRAME
     "M:" VIEW-AS TEXT
          SIZE 2.57 BY .67 AT ROW 17.96 COL 8.86
          FGCOLOR 9 
     "Div Entrada" VIEW-AS TEXT
          SIZE 8.57 BY .58 AT ROW 17.25 COL 37.43
          FGCOLOR 9 FONT 1
     "T O T A I S" VIEW-AS TEXT
          SIZE 10 BY .79 AT ROW 16.5 COL 1.86
          FGCOLOR 12 
     "Div Saida" VIEW-AS TEXT
          SIZE 7.14 BY .58 AT ROW 17.25 COL 48.86
          FGCOLOR 9 FONT 1
     "Inv Entrada" VIEW-AS TEXT
          SIZE 8 BY .58 AT ROW 17.25 COL 58.72
          FGCOLOR 9 FONT 1
     RECT-6 AT ROW 16.83 COL 1
     RECT-7 AT ROW 15.25 COL 1
     rt-buttom AT ROW 1.21 COL 116.72
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.04
         SIZE 123 BY 20.33.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Analise da Posiá∆o de Estoque"
         COLUMN             = 22.43
         ROW                = 7
         HEIGHT             = 20.25
         WIDTH              = 123
         MAX-HEIGHT         = 29.79
         MAX-WIDTH          = 182.86
         VIRTUAL-HEIGHT     = 29.79
         VIRTUAL-WIDTH      = 182.86
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB br-itens TEXT-3 DEFAULT-FRAME */
/* SETTINGS FOR BUTTON bt-excel IN FRAME DEFAULT-FRAME
   6                                                                    */
/* SETTINGS FOR BUTTON bt-Grafico-qtd IN FRAME DEFAULT-FRAME
   6                                                                    */
/* SETTINGS FOR BUTTON bt-nfiscal IN FRAME DEFAULT-FRAME
   4                                                                    */
/* SETTINGS FOR BUTTON bt-nfiscal-tot IN FRAME DEFAULT-FRAME
   4 6                                                                  */
/* SETTINGS FOR FILL-IN fi-dev-kg IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-dev-m IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-dev-und IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-div-ent-kg IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-div-ent-m IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-div-ent-und IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-div-sai-kg IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-div-sai-m IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-div-sai-und IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-ent-kg IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-ent-m IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-ent-und IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-fat-kg IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-fat-m IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-fat-und IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-ini-kg IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-ini-m IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-ini-und IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-inv-ent-kg IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-inv-ent-m IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-inv-ent-und IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-inv-sai-kg IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-inv-sai-m IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-inv-sai-und IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-kg IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-m IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-und IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-itens
/* Query rebuild information for BROWSE br-itens
     _START_FREEFORM
RUN pi-tot-itens.
OPEN QUERY {&SELF-NAME} FOR EACH tt-itens WHERE NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Query            is NOT OPENED
*/  /* BROWSE br-itens */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Analise da Posiá∆o de Estoque */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Analise da Posiá∆o de Estoque */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-itens
&Scoped-define SELF-NAME br-itens
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-itens C-Win
ON VALUE-CHANGED OF br-itens IN FRAME DEFAULT-FRAME /* ITENS */
DO:
  ASSIGN bt-nfiscal:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
  IF tt-itens.estoque-fat > 0 THEN
     ASSIGN bt-nfiscal:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

  ASSIGN fi-desc-item = tt-itens.desc-item.
  DISPLAY fi-desc-item
          WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda C-Win
ON CHOOSE OF bt-ajuda IN FRAME DEFAULT-FRAME /* Ajuda */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-devolucao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-devolucao C-Win
ON CHOOSE OF bt-devolucao IN FRAME DEFAULT-FRAME
DO:

  
  IF AVAIL tt-devolucao THEN
     /*RUN esp/espp001.w.*/
     RUN esppl/essp0191c.w (INPUT TABLE tt-devolucao,
                          INPUT string(da-dt-trans-ini),
                          INPUT string(da-dt-trans-fin)). 
                          
                    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-etiquetas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-etiquetas C-Win
ON CHOOSE OF bt-etiquetas IN FRAME DEFAULT-FRAME
DO:
    FIND item WHERE
         item.it-codigo = tt-itens.it-codigo NO-LOCK NO-ERROR.
    ASSIGN gr-item = ROWID(item).

    ASSIGN c-win:SENSITIVE = NO. 
  /*  RUN cpp\cp0107.p. */
    RUN pdp\pd1003.p.
    ASSIGN c-win:SENSITIVE = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-excel C-Win
ON CHOOSE OF bt-excel IN FRAME DEFAULT-FRAME /* Button 2 */
DO:
  RUN esdlg/d01essp0191.w (INPUT-OUTPUT arq-saida).
  IF arq-saida <> "" THEN DO:
     RUN pi-gera-excel.
     MESSAGE "O arquivo foi gerado em " TRIM(arq-saida) + "." SKIP 
             "Para acess†-lo,  abra-o atravÇs do Excel."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-exit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-exit C-Win
ON CHOOSE OF bt-exit IN FRAME DEFAULT-FRAME
DO:
   APPLY 'CLOSE':U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-Grafico-qtd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-Grafico-qtd C-Win
ON CHOOSE OF bt-Grafico-qtd IN FRAME DEFAULT-FRAME /* Grafico */
DO:
    IF fi-fat-m:INPUT-VALUE IN FRAME {&FRAME-NAME} > 0 THEN DO.
       RUN pi-grafico-qtd.
       FIND FIRST tt-itens NO-LOCK NO-ERROR.

    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-imprime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-imprime C-Win
ON CHOOSE OF bt-imprime IN FRAME DEFAULT-FRAME
DO:
  RUN pi-imprime.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-nfe
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-nfe C-Win
ON CHOOSE OF bt-nfe IN FRAME DEFAULT-FRAME
DO:
   
  IF AVAIL tt-nfe THEN
     RUN esppl/essp0191e.w (INPUT TABLE tt-nfe,
                          INPUT string(da-dt-trans-ini),
                          INPUT string(da-dt-trans-fin)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-nfiscal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-nfiscal C-Win
ON CHOOSE OF bt-nfiscal IN FRAME DEFAULT-FRAME
DO:
   IF AVAIL tt-itens THEN DO.
      IF AVAIL tt-nfiscal THEN DO.
      RUN esppl/essp0191b.w (INPUT TABLE tt-nfiscal,
                           INPUT tt-itens.it-codigo,
                           INPUT da-dt-trans-ini,
                           INPUT da-dt-trans-fin).
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-nfiscal-tot
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-nfiscal-tot C-Win
ON CHOOSE OF bt-nfiscal-tot IN FRAME DEFAULT-FRAME
DO:
  IF AVAIL tt-nfiscal THEN
     RUN esppl/essp0191b.w (INPUT TABLE tt-nfiscal,
                          INPUT "",
                          INPUT da-dt-trans-ini,
                          INPUT da-dt-trans-fin).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-param
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-param C-Win
ON CHOOSE OF bt-param IN FRAME DEFAULT-FRAME /* Sair */
DO:

   ASSIGN c-win:SENSITIVE = NO.
   RUN esp/essp0191a.w (INPUT-OUTPUT c-cod-estabel,   
                        INPUT-OUTPUT da-dt-trans-ini,
                        INPUT-OUTPUT da-dt-trans-fin,
                        INPUT-OUTPUT c-it-codigo-ini,   
                        INPUT-OUTPUT c-it-codigo-fin,
                        INPUT-OUTPUT c-cod-refer-ini,              
                        INPUT-OUTPUT c-cod-refer-fin, 
                        INPUT-OUTPUT c-corte-comerc-ini,           
                        INPUT-OUTPUT c-corte-comerc-fin, 
                        INPUT-OUTPUT c-cod-depos,                  
                        INPUT-OUTPUT l-lote-todos,                 
                        INPUT-OUTPUT l-lote-pp,                    
                        INPUT-OUTPUT l-lote-pd,                    
                        INPUT-OUTPUT l-lote-rp,                    
                        INPUT-OUTPUT l-lote-rd,
                        INPUT-OUTPUT l-lote-sc,
                        INPUT-OUTPUT l-lote-ca,
                        INPUT-OUTPUT i-opc-acabado,
                        INPUT-OUTPUT l-ok).
   IF l-ok THEN 
      RUN pi-popula-browse.
   ASSIGN c-win:SENSITIVE = YES.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-vapara
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-vapara C-Win
ON CHOOSE OF bt-vapara IN FRAME DEFAULT-FRAME
DO:
  
   RUN esdlg/d01essp0172.w (OUTPUT c-it-codigo).

   IF c-it-codigo <> ? THEN DO:
      FIND tt-itens WHERE
           tt-itens.it-codigo = c-it-codigo NO-LOCK NO-ERROR. 

      IF NOT AVAIL tt-itens THEN DO.
         MESSAGE "Item n∆o est† contido na seleá∆o!"
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
         RETURN NO-APPLY.
      END.
       /* br-itens:QUERY:REPOSITION-TO-ROWID(ROWID(tt-itens)) NO-ERROR. */
      h-query:REPOSITION-TO-ROWID(ROWID(tt-itens)) NO-ERROR.
      APPLY 'VALUE-CHANGED' TO br-itens.
   END.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Rotina para acionar a Tecla F5 em Qualquer lugar no programa
ON 'F5':U OF br-itens DO:
   {&OPEN-QUERY-br-itens}
   APPLY 'value-changed' TO br-itens.
END.

br-itens:NUM-LOCKED-COLUMNS = 3.
*/

ASSIGN h-query = br-itens:QUERY.

  
  FIND FIRST param-dis NO-LOCK NO-ERROR.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

   RUN enable_UI.

   ASSIGN da-dt-trans-ini = DATE("01" + STRING(MONTH(TODAY),"99") + STRING(YEAR(TODAY),"9999"))
          da-dt-trans-fin = TODAY.
   APPLY 'choose' TO bt-param.

   IF NOT THIS-PROCEDURE:PERSISTENT THEN
      WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY fi-desc-item fi-ini-m fi-ent-m fi-div-ent-m fi-div-sai-m fi-inv-ent-m 
          fi-inv-sai-m fi-dev-m fi-fat-m fi-tot-m fi-ini-kg fi-ent-kg 
          fi-div-ent-kg fi-div-sai-kg fi-inv-ent-kg fi-inv-sai-kg fi-dev-kg 
          fi-fat-kg fi-tot-kg fi-ini-und fi-ent-und fi-div-ent-und 
          fi-div-sai-und fi-inv-ent-und fi-inv-sai-und fi-dev-und fi-fat-und 
          fi-tot-und 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE br-itens RECT-6 RECT-7 rt-buttom bt-param bt-vapara bt-excel 
         bt-imprime bt-etiquetas bt-nfiscal bt-nfiscal-tot bt-nfe bt-devolucao 
         bt-Grafico-qtd fi-desc-item bt-exit bt-ajuda 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-abre-excel C-Win 
PROCEDURE pi-abre-excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 CREATE "Excel.Application" chExcelApp NO-ERROR.
 IF chExcelApp <> ? THEN /* Cria a Planilha */
    ASSIGN chExcelApp:VISIBLE     = TRUE  /* A Planilha Ficar† Visivel */
           chExcelApp:SheetsInNewWorkbook = 1 /* Nı PLANILHAS A SEREM CRIADAS */
           chWorkbook             = chExcelApp:Workbooks:ADD() /* Cria Planilha */
           chworksheet            = chExcelapp:sheets:ITEM(1).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-devolucao C-Win 
PROCEDURE pi-devolucao :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR c-natureza     AS CHAR.
DEF VAR c-regiao       AS CHAR.
DEF VAR i-ct           AS INTEGER.
DEF VAR c-cond-pagto   AS CHAR.
DEF VAR i-cod-vencto   AS INT.
DEF VAR i-prz          AS INT.

FOR EACH docum-est WHERE
         docum-est.cod-estabel = c-cod-estabel AND                 
         docum-est.dt-trans >= da-dt-trans-ini    AND                
         docum-est.dt-trans <= da-dt-trans-fin  NO-LOCK.

    FIND natur-oper WHERE
         natur-oper.nat-operacao = docum-est.nat-operacao NO-LOCK NO-ERROR.
    IF NOT AVAIL natur-oper THEN NEXT.
    IF natur-oper.tipo-compra <> 3 AND natur-oper.tipo-compra <> 1  THEN NEXT. /* Devoluá∆o de Cliente */
    IF natur-oper.tipo-compra = 1 AND docum-est.nat-operacao <>  "599FAB"  THEN NEXT.
    FIND emitente WHERE
         emitente.cod-emit = docum-est.cod-emitente NO-LOCK NO-ERROR.
    IF NOT AVAIL emitente THEN NEXT.
/*     IF emitente.nome-abrev < c-nome-abrev-ini OR          */
/*        emitente.nome-abrev > c-nome-abrev-fin  THEN NEXT. */
/*                                                           */
    /* 1=PF, 2=PJ, 3=ESTRANGEIRO */
/*     IF (c-tipo-mercado = "I" AND emitente.natureza  > 2) OR         */
/*        (c-tipo-mercado = "E" AND emitente.natureza <> 3) THEN NEXT. */

    /* Exclui Cliente Tear Textil */
    FIND estabelec WHERE
         estabelec.cgc = emitente.cgc NO-LOCK NO-ERROR.
    IF AVAIL estabel THEN NEXT.
    IF SUBSTR(emitente.cgc,1,8) = "03123987" THEN NEXT. /* Retirar Apos Testes */

    FIND repres WHERE
         repres.cod-rep = emitente.cod-rep NO-LOCK NO-ERROR.
    IF NOT AVAIL repres THEN NEXT.
/*     IF repres.nome-abrev < c-no-ab-reppri-ini OR         */
/*        repres.nome-abrev > c-no-ab-reppri-fin THEN NEXT. */
/*                                                          */
    ASSIGN c-regiao ="".
    IF emitente.pais = "BRASIL" THEN DO:
       FIND unid-feder WHERE
            unid-feder.pais = emitente.pais AND
            unid-feder.estado = emitente.estado NO-LOCK NO-ERROR.
       IF AVAIL unid-feder THEN
          ASSIGN c-regiao = unid-feder.char-2.
    END.
    ELSE
       ASSIGN c-regiao = "Exportaá∆o".

    FOR EACH item-doc-est OF docum-est NO-LOCK.

       RUN pi-acompanhar IN h-acomp (INPUT "Data: "    + STRING(docum-est.dt-trans) +
                                            " Nota Fiscal: " + item-doc-est.nro-docto).

/*        IF item-doc-est.it-codigo < c-it-codigo-ini OR         */
/*           item-doc-est.it-codigo > c-it-codigo-fin THEN NEXT. */
/*        RUN pi-ver-digita (INPUT "Item",                  */
/*                           INPUT item-doc-est.it-codigo). */
/*        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.          */

/*        IF item-doc-est.cod-refer < c-cod-refer-ini OR         */
/*           item-doc-est.cod-refer > c-cod-refer-fin THEN NEXT. */
/*        RUN pi-ver-digita (INPUT "Referància",            */
/*                           INPUT item-doc-est.cod-refer). */
/*        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.          */

/*        IF c-tipo-acabamento = "E" AND SUBSTR(item-doc-est.cod-refer,7,1)  = '0' THEN NEXT. */
/*        IF c-tipo-acabamento = "L" AND SUBSTR(item-doc-est.cod-refer,7,1) <> '0' THEN NEXT. */

/*       FIND item-ext WHERE
            item-ext.it-codigo = item-doc-est.it-codigo NO-LOCK NO-ERROR. */
/*        IF c-tipo-artigo <> 'A' THEN                                      */
/*           IF AVAIL item-ext AND                                          */
/*              (item-ext.indigo = YES AND c-tipo-artigo <> "I") OR         */
/*              (item-ext.indigo = NO  AND c-tipo-artigo <> "O") THEN NEXT. */

       /* Pegar o Lote do Item */
       FIND rat-lote WHERE
            rat-lote.serie-docto  = docum-est.serie-docto  AND
            rat-lote.nro-docto    = docum-est.nro-docto    AND 
            rat-lote.cod-emitente = docum-est.cod-emitente AND
            rat-lote.nat-operacao = docum-est.nat-operacao AND
            rat-lote.sequencia    = item-doc-est.sequencia NO-LOCK NO-ERROR.
       IF NOT AVAIL rat-lote THEN NEXT.

        FIND item WHERE                                                
             item.it-codigo = item-doc-est.it-codigo NO-LOCK NO-ERROR. 
        IF NOT AVAIL ITEM THEN NEXT.
        
/*        IF (ITEM.ge-codigo < i-ge-codigo-ini) OR                       */
/*           (ITEM.ge-codigo > i-ge-codigo-fin) THEN NEXT.               */

       ASSIGN i-ct         = 0
              c-cond-pagto = ""
              i-cod-vencto = 0
              c-natureza   = "Sem NF de Origem"
              i-prz        = 0.


       FIND nota-fiscal WHERE
            nota-fiscal.cod-estabel  = docum-est.cod-estabel   AND
            nota-fiscal.serie        = item-doc-est.serie-comp AND
            nota-fiscal.nr-nota-fis  = item-doc-est.nro-comp   NO-LOCK NO-ERROR.

       IF AVAIL nota-fiscal THEN DO:
          FIND ped-venda WHERE
               ped-venda.nr-pedcli = nota-fiscal.nr-pedcli NO-LOCK NO-ERROR.
          IF AVAIL ped-venda THEN DO:
             FIND ped-venda-ext WHERE 
                  ped-venda-ext.nr-pedido = ped-venda.nr-pedido NO-LOCK NO-ERROR.
             IF AVAIL ped-venda-ext AND 
                      LOOKUP(ped-venda-ext.tp-pedido,"AMOSTRA,AMOSTRA EXPORTAÄ«O") <> 0 THEN NEXT.
          END.
          ASSIGN c-natureza = nota-fiscal.nat-operacao.
          FOR EACH fat-duplic WHERE
                   fat-duplic.cod-estabel = nota-fiscal.cod-estabel AND
                   fat-duplic.serie       = nota-fiscal.serie       AND
                   fat-duplic.nr-fatura   = nota-fiscal.nr-nota-fis NO-LOCK.
                   ASSIGN i-prz        = i-prz + (fat-duplic.dt-vencimen - fat-duplic.dt-emissao)
                          i-ct         = i-ct + 1
                          i-cod-vencto = fat-duplic.cod-vencto.
          END.
          IF i-prz <> 0 OR i-cod-vencto = 2 OR i-cod-vencto = 9 THEN DO: 
             IF i-cod-vencto = 2 THEN
                ASSIGN c-cond-pagto = " A Vista".
             ELSE
             IF i-cod-vencto = 9 THEN
                ASSIGN c-cond-pagto = " Contra Apresentaá∆o".
             ELSE DO:
                i-prz = INT(i-prz / i-ct).
                IF i-prz <= 30 THEN
                   ASSIGN c-cond-pagto = " De 01 AtÇ 30 Dias".
                ELSE
                IF i-prz <= 60 THEN
                   ASSIGN c-cond-pagto = " De 31 AtÇ 60 Dias".
                ELSE
                IF i-prz <= 90 THEN
                   ASSIGN c-cond-pagto = " De 61 AtÇ 90 Dias".
                ELSE
                   ASSIGN c-cond-pagto = "ˇAcima de 90 Dias". /* 1¶ Pos Ç ALT 164 */
             END.
          END.
          ELSE DO:
             FIND cond-pagto WHERE
                  cond-pagto.cod-cond-pag = nota-fiscal.cod-cond-pag NO-LOCK NO-ERROR.
             IF AVAIL cond-pagto THEN DO:
                ASSIGN c-cond-pagto = cond-pagto.descricao.
                IF cond-pagto.log-2 THEN /* Fat Ç para VENDOR */
                   ASSIGN c-cond-pagto = "ˇVendor". /* 1¶ Pos Ç ALT 164 */
             END.
          END.
          IF c-cond-pagto = "" THEN
             ASSIGN c-cond-pagto = " Cupom Fiscal".
          
          IF c-regiao = "Exportaá∆o" THEN
             ASSIGN c-cond-pagto = "ˇExportaá∆o". /* 1¶ Pos Ç ALT 164 */

       END.
       IF c-cond-pagto = "" THEN 
          ASSIGN c-cond-pagto = "ˇSem NF de Origem". /* 1¶ Pos Ç ALT 164 */

       RUN pi-grava-devol (INPUT "1",
                           INPUT item-doc-est.it-codigo,
                           INPUT ITEM.un,
                           INPUT SUBSTR(rat-lote.lote,1,2),
                           INPUT item.desc-item,
                           INPUT item-doc-est.quantidade,
                           INPUT item-doc-est.preco-total[1],
                           INPUT docum-est.serie-docto,
                           INPUT docum-est.nro-docto,
                           INPUT docum-est.cod-emitente,
                           INPUT docum-est.nat-operacao).

       RUN pi-grava-devol (INPUT "2",
                           INPUT repres.nome-abrev,
                           INPUT ITEM.un,
                           INPUT SUBSTR(rat-lote.lote,1,2),
                           INPUT repres.cod-rep,
                           INPUT item-doc-est.quantidade,
                           INPUT item-doc-est.preco-total[1],
                           INPUT docum-est.serie-docto,
                           INPUT docum-est.nro-docto,
                           INPUT docum-est.cod-emitente,
                           INPUT docum-est.nat-operacao).

       RUN pi-grava-devol (INPUT "3",
                           INPUT emitente.nome-matriz,
                           INPUT ITEM.un,
                           INPUT SUBSTR(rat-lote.lote,1,2),
                           INPUT "",
                           INPUT item-doc-est.quantidade,   
                           INPUT item-doc-est.preco-total[1],
                           INPUT docum-est.serie-docto,
                           INPUT docum-est.nro-docto,
                           INPUT docum-est.cod-emitente,
                           INPUT docum-est.nat-operacao).

       RUN pi-grava-devol (INPUT "4",
                           INPUT emitente.nome-abrev,
                           INPUT ITEM.un,
                           INPUT SUBSTR(rat-lote.lote,1,2),
                           INPUT STRING(emitente.cod-emitente),
                           INPUT item-doc-est.quantidade,   
                           INPUT item-doc-est.preco-total[1],
                           INPUT docum-est.serie-docto,
                           INPUT docum-est.nro-docto,
                           INPUT docum-est.cod-emitente,
                           INPUT docum-est.nat-operacao).

       RUN pi-grava-devol (INPUT "5",
                           INPUT c-regiao,
                           INPUT ITEM.un,
                           INPUT SUBSTR(rat-lote.lote,1,2),
                           INPUT emitente.estado,
                           INPUT item-doc-est.quantidade,     
                           INPUT item-doc-est.preco-total[1],
                           INPUT docum-est.serie-docto,
                           INPUT docum-est.nro-docto,
                           INPUT docum-est.cod-emitente,
                           INPUT docum-est.nat-operacao).

       RUN pi-grava-devol (INPUT "6",
                           INPUT c-natureza,  /*natur-oper.nat-operacao,*/
                           INPUT ITEM.un,
                           INPUT SUBSTR(rat-lote.lote,1,2),
                           INPUT STRING(natur-oper.aliquota-icm),
                           INPUT item-doc-est.quantidade,    
                           INPUT item-doc-est.preco-total[1],
                           INPUT docum-est.serie-docto,
                           INPUT docum-est.nro-docto,
                           INPUT docum-est.cod-emitente,
                           INPUT docum-est.nat-operacao).

       RUN pi-grava-devol (INPUT "7",
                           INPUT c-cond-pagto,
                           INPUT ITEM.un,
                           INPUT SUBSTR(rat-lote.lote,1,2),
                           INPUT "",
                           INPUT item-doc-est.quantidade,   
                           INPUT item-doc-est.preco-total[1],
                           INPUT docum-est.serie-docto,
                           INPUT docum-est.nro-docto,
                           INPUT docum-est.cod-emitente,
                           INPUT docum-est.nat-operacao).


    END.
END.    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-excel-cabec C-Win 
PROCEDURE pi-excel-cabec :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

 DEFINE INPUT PARAMETER p-nr-plan AS INT.
 DEFINE INPUT PARAMETER p-nome AS CHAR.

 /* Nomear Aba da Planilha */
 chWorkSheet = chExcelapp:Sheets:ITEM(p-nr-plan).
 chWorkSheet:NAME = p-nome.
 chWorkSheet:TAB:ColorIndex = 19.

 /* Ativar a Planilha */
 chWorkSheet = chExcelapp:Sheets:ITEM(p-nr-plan).
 chWorkbook:Worksheets(p-nr-plan):activate.
 chExcelApp:ActiveWindow:Zoom = 100.

 /* Primeira Linha da Planilha */
 ASSIGN chworksheet:range("A1"):VALUE = c-titulo.

 /* Configura Alinhamento Horizontal do Titulo da Planilha */
    ChWorkSheet:range("A1:L1"):SELECT().
    ChWorksheet:range("A1:L1"):Merge.
    Chworksheet:Range("A1:L1"):HorizontalAlignment =  3.

 /* Colorir Titulo da Planilha */
 ASSIGN chWorkSheet:Range("A1:L1"):FONT:ColorIndex     = 18  /* Avermelhado */
        chWorkSheet:Range("A1:L1"):Interior:ColorIndex =  2. /* Branco */

 /* Configura a Linha do Titulo da Planilha */
 ASSIGN chWorkSheet:Rows("1:1"):RowHeight = 25
        chWorkSheet:Rows("2:2"):RowHeight =  4
        chWorkSheet:Rows("1:1"):FONT:SIZE = 16
        chWorkSheet:Rows("1:1"):FONT:bold = TRUE.

 /* Titulo das Colunas */
 ASSIGN chworksheet:range("A3"):VALUE = "ITEM"
        chworksheet:range("B3"):VALUE = "DESCRIÄ«O"    
        chworksheet:range("C3"):VALUE = "UND"  
        chworksheet:range("D3"):VALUE = "ESTOQUE INICIAL"
        chworksheet:range("E3"):VALUE = "ENTRADAS"     
        chworksheet:range("F3"):VALUE = "DIV ENTRADA" 
        chworksheet:range("G3"):VALUE = "DIV SAIDA"  
        chworksheet:range("H3"):VALUE = "INV ENTRADA"   
        chworksheet:range("I3"):VALUE = "INV SAIDA"
        chworksheet:range("J3"):VALUE = "DEVOLUÄ«O"  
        chworksheet:range("K3"):VALUE = "FATURAMENTO"   
        chworksheet:range("L3"):VALUE = "ESTOQUE FINAL".

  /* Tamanho das Colunas */
  ASSIGN chWorkSheet:Columns("A"):ColumnWidth =  6
         chWorkSheet:Columns("B"):ColumnWidth = 40
         chWorkSheet:Columns("C"):ColumnWidth =  3
         chWorkSheet:Columns("D"):ColumnWidth = 15
         chWorkSheet:Columns("E"):ColumnWidth = 13
         chWorkSheet:Columns("F"):ColumnWidth = 13
         chWorkSheet:Columns("G"):ColumnWidth = 13
         chWorkSheet:Columns("H"):ColumnWidth = 13
         chWorkSheet:Columns("I"):ColumnWidth = 13
         chWorkSheet:Columns("J"):ColumnWidth = 13
         chWorkSheet:Columns("K"):ColumnWidth = 13
         chWorkSheet:Columns("L"):ColumnWidth = 15.

  /* Configura as Colunas da Planilha */
  ASSIGN chworksheet:range("A:C"):NumberFormat        = "@".
  ASSIGN chworksheet:range("D:L"):NumberFormat        = "###.###.##0,00"
         Chworksheet:range("D:L"):HorizontalAlignment = 4. /* Alinhamento a Direita */

 /* Configura Cabeáalho das Colunas */
 chWorkSheet:Range("A3:L3"):SELECT().
 ASSIGN chExcelApp:SELECTION:FONT:NAME               = "Consolas"
        chExcelApp:SELECTION:FONT:SIZE               = 09
        chExcelApp:SELECTION:FONT:Bold               = TRUE 
        chExcelApp:SELECTION:Interior:ColorIndex     = 19
        chExcelApp:SELECTION:FONT:ColorIndex         = 11.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-gera-excel C-Win 
PROCEDURE pi-gera-excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN pi-abre-excel.
  IF chExcelApp = ? THEN DO:
     MESSAGE "O Aplicativo EXCEL n∆o foi encontrado. N∆o Ç possivel a execuá∆o do programa."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     ASSIGN arq-saida = "".
     RETURN.
  END.

  RUN pi-monta-planilha.

  /* Posiciona o Foco no Inicio da Planilha */
  chExcelApp:Range("A1"):SELECT.
  chExcelApp:Range("A:A"):EntireColumn:AutoFit.

  /* Posiciona na Planilha 1, Salva e Fecha */
  chWorkSheet = chExcelapp:Sheets:ITEM(1).
  chWorkbook:Worksheets(1):activate.

  /* Salva e Fecha Planilha */
  IF chExcelApp:Version < "12":U THEN DO.
     ASSIGN arq-saida = SUBSTR(arq-saida,1,LENGTH(arq-saida) - 3) + "xls".
     OS-DELETE VALUE(arq-saida).
     chWorkBook:SaveAs(arq-saida,39,,,,,,,TRUE). /* Salva em formato mais Antigo EXCEL97 */ 
  END.
  ELSE DO:
     ASSIGN arq-saida = SUBSTR(arq-saida,1,LENGTH(arq-saida) - 3) + "xlsx".
     OS-DELETE VALUE(arq-saida).
     chWorkBook:SaveAs(arq-saida,,,,,,,).  /* Salva na Vers∆o da planilha da ESTAÄ«O */
  END.

  chWorkBook:CLOSE().
  chExcelApp:QUIT().
  RELEASE OBJECT chExcelApp. 
  RELEASE OBJECT chworkBook.
  RELEASE OBJECT chworksheet.
    


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-grafico-qtd C-Win 
PROCEDURE pi-grafico-qtd :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

 DEF VAR i-point AS INT INITIAL 1.

 DEF VAR i-numsets AS INT INITIAL 0.
 DEF VAR c-titulo-grafico AS CHAR.

 ASSIGN c-titulo-grafico = "Metros Faturados".

 ASSIGN c-titulo-grafico = c-titulo-grafico + " Em ".


 EMPTY TEMP-TABLE tt-atributos.
 EMPTY TEMP-TABLE tt-sets.
 EMPTY TEMP-TABLE tt-points-2.
 EMPTY TEMP-TABLE tt-dados.
 EMPTY TEMP-TABLE tt-erros.

 /* Configuraá∆o Geral do Grafico */          
 /*                               */          
 CREATE tt-atributos.

    ASSIGN tt-atributos.cod-versao-integracao = 3
           tt-atributos.graphtype             = 4
           tt-atributos.graphtitle            = c-titulo-grafico + " " + 
                                                STRING(da-dt-trans-ini) + " † " + string(da-dt-trans-fin) + '.'
           tt-atributos.lefttitle             = 'Quantidade em METROS.'
           tt-atributos.lefttitlestyle        = 2
           tt-atributos.bottomtitle           = 'D I A S'
           tt-atributos.numgraph              = 1.



 /* Configuraá∆o das Variantes do Grafico (Linhas ou  Barras */
 /*                                                          */
 ASSIGN i-numsets = 1.



 CREATE tt-sets.
 ASSIGN tt-sets.NumSet   = i-numsets
        tt-sets.NumGraph = 1
        tt-sets.ColorSet = 2
        tt-sets.legendText = "Metros Faturados".

FOR EACH b-tt-itens NO-LOCK.

    IF b-tt-itens.estoque-fat <= 0.00 THEN NEXT.

 /* Valores do EIXO X (DIAS) */
 CREATE tt-points-2.

 ASSIGN tt-points-2.NumPoint  = i-point
        tt-points-2.NumGraph  = 1
        tt-points-2.labeltext = b-tt-itens.it-codigo.


 ASSIGN i-numsets = 1.

 /* Valores do EIXI Y (Metros Faturados) */


     CREATE tt-dados.
     ASSIGN tt-dados.NumPoint   = i-point
            tt-dados.NumSet     = i-numsets
            tt-dados.NumGraph   = 1
            tt-dados.graphdata  = b-tt-itens.estoque-fat.
    
    
     ASSIGN i-point = i-point + 1.

 END.
 DEF VAR h-utapi011 AS HANDLE NO-UNDO.


 RUN utp/utapi011.p PERSISTENT SET h-utapi011.


 RUN pi-execute IN h-utapi011 (INPUT  TABLE tt-atributos,
                               INPUT  TABLE tt-points-2,
                               INPUT  TABLE tt-sets,
                               INPUT  TABLE tt-dados,
                               INPUT  TABLE tt-ylabels,
                               OUTPUT TABLE tt-erros).

 IF RETURN-VALUE = "NOK" THEN DO: 
    FOR EACH tt-erros: 
        DISP cod-erro desc-erro FORMAT "x(100)" WITH 1 COL WIDTH 500. 
    END.
 END.                  

 DELETE PROCEDURE h-utapi011.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-grava-devol C-Win 
PROCEDURE pi-grava-devol :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER p-tipo         AS CHAR.
DEFINE INPUT PARAMETER p-codigo       AS CHAR.
DEFINE INPUT PARAMETER p-und          AS CHAR.
DEFINE INPUT PARAMETER p-lote         AS CHAR.
DEFINE INPUT PARAMETER p-desc         AS CHAR.
DEFINE INPUT PARAMETER p-qtd          AS DEC.
DEFINE INPUT PARAMETER p-vlr          AS DEC.
DEFINE INPUT PARAMETER p-serie-docto  LIKE docum-est.serie-docto.
DEFINE INPUT PARAMETER p-nro-docto    LIKE docum-est.nro-docto.
DEFINE INPUT PARAMETER p-cod-emitente LIKE docum-est.cod-emitente.
DEFINE INPUT PARAMETER p-nat-operacao LIKE docum-est.nat-operacao.


CASE p-tipo:
    WHEN "1" THEN
        FIND tt-work WHERE
             tt-work.it-codigo = p-codigo AND tt-work.und = p-und AND tt-work.lote = p-lote
             NO-ERROR.
    WHEN "2" THEN
        FIND tt-work WHERE
             tt-work.no-ab-reppri = p-codigo AND tt-work.und = p-und AND tt-work.lote = p-lote
              NO-ERROR.
    WHEN "3" THEN
        FIND tt-work WHERE
             tt-work.matriz = p-codigo AND tt-work.und = p-und AND tt-work.lote = p-lote
              NO-ERROR.
    WHEN "4" THEN
        FIND tt-work WHERE
             tt-work.nome-abrev = p-codigo AND tt-work.und = p-und AND tt-work.lote = p-lote
             NO-ERROR.
    WHEN "5" THEN
        FIND tt-work WHERE
             tt-work.regiao = p-codigo AND tt-work.und = p-und AND tt-work.lote = p-lote AND tt-work.uf = p-desc
             NO-ERROR.
    WHEN "6" THEN
        FIND tt-work WHERE
             tt-work.nat-operacao = p-codigo AND tt-work.und = p-und AND tt-work.lote = p-lote
             NO-ERROR.
    WHEN "7" THEN
        FIND tt-work WHERE
             tt-work.cond-pagto = p-codigo AND tt-work.und = p-und AND tt-work.lote = p-lote
             NO-ERROR.
END CASE.


IF NOT AVAIL tt-work THEN DO:
   CREATE tt-work.

   CASE p-tipo:
       WHEN "1" THEN DO:
           ASSIGN  tt-work.it-codigo = p-codigo
                   tt-work.desc-item = p-desc.
       END.
       WHEN "2" THEN                    
           ASSIGN  tt-work.no-ab-reppri = p-codigo
                   tt-work.cod-rep      = INT(p-desc).
       WHEN "3" THEN                         
           ASSIGN  tt-work.matriz = p-codigo.
       WHEN "4" THEN
           ASSIGN  tt-work.nome-abrev = p-codigo
                   tt-work.cod-emit   = INT(p-desc).
       WHEN "5" THEN 
           ASSIGN  tt-work.regiao = p-codigo
                   tt-work.uf     = p-desc.
       WHEN "6" THEN
           ASSIGN  tt-work.nat-operacao = p-codigo
                   tt-work.aliq-icms    = DEC(p-desc).
       WHEN "7" THEN
           ASSIGN  tt-work.cond-pagto = p-codigo.
   END CASE.
   ASSIGN tt-work.und       = p-und
          tt-work.lote      = p-lote.
END.

ASSIGN tt-work.qtd-devol = tt-work.qtd-devol + p-qtd
       tt-work.vlr-devol = tt-work.vlr-devol + p-vlr.

FIND tt-devolucao WHERE
     tt-devolucao.serie-docto = p-serie-docto   AND
     tt-devolucao.nro-docto   = p-nro-docto     AND
     tt-devolucao.cod-emitente = p-cod-emitente AND
     tt-devolucao.nat-operacao = p-nat-operacao NO-ERROR.
IF NOT AVAIL tt-devolucao THEN DO:
   CREATE tt-devolucao.
   ASSIGN tt-devolucao.serie-docto  = p-serie-docto
          tt-devolucao.nro-docto    = p-nro-docto
          tt-devolucao.cod-emitente = p-cod-emitente
          tt-devolucao.nat-operacao = p-nat-operacao
          tt-devolucao.cod-estabel  = c-cod-estabel.
END.
ASSIGN tt-devolucao.qtd-devol = tt-devolucao.qtd-devol + p-qtd
       tt-devolucao.vlr-devol = tt-devolucao.vlr-devol + p-vlr.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-grava-movto C-Win 
PROCEDURE pi-grava-movto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*
 DEFINE INPUT PARAMETER p-it-codigo AS CHAR.

 FOR EACH ob-sl-estoq-per WHERE
          ob-sl-estoq-per.periodo       = c-periodo          AND 
          ob-sl-estoq-per.it-codigo     = p-it-codigo        AND
          ob-sl-estoq-per.cod-refer    >= c-cod-refer-ini    AND
          ob-sl-estoq-per.cod-refer    <= c-cod-refer-fin    AND
          ob-sl-estoq-per.corte-comerc >= c-corte-comerc-ini AND
          ob-sl-estoq-per.corte-comerc <= c-corte-comerc-fin AND
          ob-sl-estoq-per.cod-qualid   >= c-cod-qualid-ini   AND
          ob-sl-estoq-per.cod-qualid   <= c-cod-qualid-fin   AND
          LOOKUP(ob-sl-estoq-per.nr-lote,c-lotes) <> 0  NO-LOCK.

     FIND tt-work WHERE
          tt-work.it-codigo = p-it-codigo               AND
          tt-work.cod-refer = ob-sl-estoq-per.cod-refer AND
          tt-work.nr-lote   = ob-sl-estoq-per.nr-lote   NO-ERROR.
     IF NOT AVAIL tt-work THEN DO:
        CREATE tt-work.
        ASSIGN tt-work.it-codigo = p-it-codigo
               tt-work.cod-refer = ob-sl-estoq-per.cod-refer
               tt-work.nr-lote   = ob-sl-estoq-per.nr-lote.
     END.
     ASSIGN tt-work.qtd-inicial   = tt-work.qtd-inicial   + ob-sl-estoq-per.qtd-inicial
            tt-work.qtd-entr-est  = tt-work.qtd-entr-est  + ob-sl-estoq-per.qtd-entr-est
            tt-work.qtd-transf    = tt-work.qtd-transf    + ob-sl-estoq-per.qtd-transf
            tt-work.qtd-devolvida = tt-work.qtd-devolvida + ob-sl-estoq-per.qtd-devolvida
            tt-work.qtd-faturada  = tt-work.qtd-faturada  + ob-sl-estoq-per.qtd-faturada
            tt-work.qtd-final     = tt-work.qtd-final     + ob-sl-estoq-per.qtd-final
            tt-work.qtd-real      = tt-work.qtd-real      + ob-sl-estoq-per.qtd-real.
 END.
*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-grava-nfe C-Win 
PROCEDURE pi-grava-nfe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*
DEFINE INPUT PARAMETER p-tipo         AS CHAR.
DEFINE INPUT PARAMETER p-codigo       AS CHAR.
DEFINE INPUT PARAMETER p-und          AS CHAR.
DEFINE INPUT PARAMETER p-lote         AS CHAR.
DEFINE INPUT PARAMETER p-desc         AS CHAR.
DEFINE INPUT PARAMETER p-qtd          AS DEC.
DEFINE INPUT PARAMETER p-vlr          AS DEC.
DEFINE INPUT PARAMETER p-serie-docto  LIKE docum-est.serie-docto.
DEFINE INPUT PARAMETER p-nro-docto    LIKE docum-est.nro-docto.
DEFINE INPUT PARAMETER p-cod-emitente LIKE docum-est.cod-emitente.
DEFINE INPUT PARAMETER p-nat-operacao LIKE docum-est.nat-operacao.

CASE p-tipo:
    WHEN "1" THEN
        FIND tt-work WHERE
             tt-work.it-codigo = p-codigo AND tt-work.und = p-und AND tt-work.lote = p-lote
             NO-ERROR.
    WHEN "2" THEN
        FIND tt-work WHERE
             tt-work.no-ab-reppri = p-codigo AND tt-work.und = p-und AND tt-work.lote = p-lote
              NO-ERROR.
    WHEN "3" THEN
        FIND tt-work WHERE
             tt-work.matriz = p-codigo AND tt-work.und = p-und AND tt-work.lote = p-lote
              NO-ERROR.
    WHEN "4" THEN
        FIND tt-work WHERE
             tt-work.nome-abrev = p-codigo AND tt-work.und = p-und AND tt-work.lote = p-lote
             NO-ERROR.
    WHEN "5" THEN
        FIND tt-work WHERE
             tt-work.regiao = p-codigo AND tt-work.und = p-und AND tt-work.lote = p-lote AND tt-work.uf = p-desc
             NO-ERROR.
    WHEN "6" THEN
        FIND tt-work WHERE
             tt-work.nat-operacao = p-codigo AND tt-work.und = p-und AND tt-work.lote = p-lote
             NO-ERROR.
    WHEN "7" THEN
        FIND tt-work WHERE
             tt-work.cond-pagto = p-codigo AND tt-work.und = p-und AND tt-work.lote = p-lote
             NO-ERROR.
END CASE.

IF NOT AVAIL tt-work THEN DO:
   CREATE tt-work.

   CASE p-tipo:
       WHEN "1" THEN DO:
           ASSIGN  tt-work.it-codigo = p-codigo
                   tt-work.desc-item = p-desc.
       END.
       WHEN "2" THEN                    
           ASSIGN  tt-work.no-ab-reppri = p-codigo
                   tt-work.cod-rep      = INT(p-desc).
       WHEN "3" THEN                         
           ASSIGN  tt-work.matriz = p-codigo.
       WHEN "4" THEN
           ASSIGN  tt-work.nome-abrev = p-codigo
                   tt-work.cod-emit   = INT(p-desc).
       WHEN "5" THEN 
           ASSIGN  tt-work.regiao = p-codigo
                   tt-work.uf     = p-desc.
       WHEN "6" THEN
           ASSIGN  tt-work.nat-operacao = p-codigo
                   tt-work.aliq-icms    = DEC(p-desc).
       WHEN "7" THEN
           ASSIGN  tt-work.cond-pagto = p-codigo.
   END CASE.
   ASSIGN tt-work.und       = p-und
          tt-work.lote      = p-lote.
END.

ASSIGN tt-work.qtd-devol = tt-work.qtd-devol + p-qtd
       tt-work.vlr-devol = tt-work.vlr-devol + p-vlr.

FIND tt-nfe WHERE
     tt-nfe.serie-docto = p-serie-docto   AND
     tt-nfe.nro-docto   = p-nro-docto     AND
     tt-nfe.cod-emitente = p-cod-emitente AND
     tt-nfe.nat-operacao = p-nat-operacao NO-ERROR.
IF NOT AVAIL tt-nfe THEN DO:
   CREATE tt-nfe.
   ASSIGN tt-nfe.serie-docto  = p-serie-docto
          tt-nfe.nro-docto    = p-nro-docto
          tt-nfe.cod-emitente = p-cod-emitente
          tt-nfe.nat-operacao = p-nat-operacao.
END.
ASSIGN tt-nfe.qtd-devol = tt-nfe.qtd-devol + p-qtd
       tt-nfe.vlr-devol = tt-nfe.vlr-devol + p-vlr.
*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imp-cabec C-Win 
PROCEDURE pi-imp-cabec :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    PUT c-empresa  FORMAT "X(40)"                 AT   1
        "DATA: "                                  AT  71
        STRING(TODAY,"99/99/9999") FORMAT "X(10)" AT  77
        "HORA: "                                  AT 104
        STRING(TIME,"hh:mm:ss")                   AT 110
        "PAG:"                                    AT 144
        i-pag FORMAT ">>>"                        AT 149
        SKIP(1).

    PUT c-titulo FORMAT "X(60)" AT 50 SKIP(1).

    PUT "ITEM   DESCRIÄ«O                                UND ESTOQUE INICIAL      ENTRADAS   DIV ENTRADA     DIV SAIDA   INV ENTRADA     INV SAIDA     DEVOLUÄ«O   FATURAMENTO ESTOQUE FINAL" AT 1.
    PUT "------ ---------------------------------------- --- --------------- ------------- ------------- ------------- ------------- ------------- ------------- ------------- -------------" AT 1.
    ASSIGN i-pag = i-pag + 1.                                                                              


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imprime C-Win 
PROCEDURE pi-imprime :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF VAR h-prog AS HANDLE NO-UNDO.
  DEF VAR i-ct   AS INT.

  RUN utp/ut-utils.p PERSISTENT SET h-prog.


  RUN esapi/saida-imp.p (OUTPUT i-saida,
                         OUTPUT c-saida,
                         OUTPUT i-num-copias,
                         OUTPUT l-ok).

  CASE i-saida:
      WHEN 1 THEN DO.
          OUTPUT TO VALUE(c-saida) CONVERT TARGET "ibm850" PAGED PAGE-SIZE 39.
          PUT CONTROL "~033&l1O~033(s16H". /* ORIENTAÄ«O PAISAGEM & COMPACTA */ 
         /*PUT CONTROL "~033E~033(s21H". */ /* ORIENTAÄ«O RETRATO & COMPACTA */
         /*PUT CONTROL "~033&l2S~033(s16H". /* DUPLEX BORDA CURTA */ */
      END.
      WHEN 2 THEN
          OUTPUT TO VALUE(c-saida) CONVERT SOURCE "ibm850".
      WHEN 3 THEN DO.
          ASSIGN c-saida = SESSION:TEMP-DIRECTORY + "essp0191-sint.tmp".
          OUTPUT TO VALUE(c-saida) CONVERT SOURCE "ibm850".
      END.
  END CASE.

  DO i-ct = 1 TO i-num-copias.
     ASSIGN i-pag      =  1
            i-lin      = 99.

     ASSIGN fi-ini-m       = 0 fi-ent-m      = 0 fi-div-ent-m   = 0 fi-div-sai-m   = 0 fi-inv-ent-m   = 0 fi-inv-sai-m   = 0
            fi-dev-m       = 0 fi-fat-m      = 0 fi-tot-m       = 0 fi-ini-kg      = 0 fi-ent-kg      = 0 fi-div-ent-kg  = 0
            fi-div-sai-kg  = 0 fi-inv-ent-kg = 0 fi-inv-sai-kg  = 0 fi-dev-kg      = 0 fi-fat-kg      = 0 fi-tot-kg      = 0
            fi-ini-und     = 0 fi-ent-und    = 0 fi-div-ent-und = 0 fi-div-sai-und = 0 fi-inv-ent-und = 0 fi-inv-sai-und = 0
            fi-dev-und     = 0 fi-fat-und    = 0 fi-tot-und     = 0.

     FOR EACH b-tt-itens NO-LOCK.
    
         IF i-lin > 39 THEN DO:
            RUN pi-imp-cabec.
            ASSIGN i-lin = 7.
         END.
    
         PUT b-tt-itens.it-codigo     FORMAT "x(6)"           AT   1
             b-tt-itens.desc-item     FORMAT "x(40)"          AT   8 
             b-tt-itens.un            FORMAT "x(3)"           AT  49.

         PUT b-tt-itens.estoque-ini     FORMAT "->,>>>,>>9.99"  AT  55
             b-tt-itens.estoque-ent     FORMAT "->,>>>,>>9.99"  AT  69
             b-tt-itens.estoque-div-ent FORMAT "->,>>>,>>9.99"  AT  83
             b-tt-itens.estoque-div-sai FORMAT "->,>>>,>>9.99"  AT  97
             b-tt-itens.estoque-inv-ent FORMAT "->,>>>,>>9.99"  AT 111
             b-tt-itens.estoque-inv-sai FORMAT "->,>>>,>>9.99"  AT 125
             b-tt-itens.estoque-dev     FORMAT "->,>>>,>>9.99"  AT 139
             b-tt-itens.estoque-fat     FORMAT "->,>>>,>>9.99"  AT 153
             b-tt-itens.estoque-final   FORMAT "->,>>>,>>9.99"  AT 167.

         ASSIGN i-lin = i-lin + 1.

         CASE b-tt-itens.un:
             WHEN 'M'   THEN
                 ASSIGN fi-ini-m     = fi-ini-m     + b-tt-itens.estoque-ini
                        fi-ent-m     = fi-ent-m     + b-tt-itens.estoque-ent
                        fi-div-ent-m = fi-div-ent-m + b-tt-itens.estoque-div-ent
                        fi-div-sai-m = fi-div-sai-m + b-tt-itens.estoque-div-sai
                        fi-inv-ent-m = fi-inv-ent-m + b-tt-itens.estoque-inv-ent 
                        fi-inv-sai-m = fi-inv-sai-m + b-tt-itens.estoque-inv-sai 
                        fi-dev-m     = fi-dev-m     + b-tt-itens.estoque-dev
                        fi-fat-m     = fi-fat-m     + b-tt-itens.estoque-fat
                        fi-tot-m     = fi-tot-m     + b-tt-itens.estoque-final.
             WHEN 'KG'  THEN
                 ASSIGN fi-ini-kg     = fi-ini-kg     + b-tt-itens.estoque-ini
                        fi-ent-kg     = fi-ent-kg     + b-tt-itens.estoque-ent
                        fi-div-ent-kg = fi-div-ent-kg + b-tt-itens.estoque-div-ent
                        fi-div-sai-kg = fi-div-sai-kg + b-tt-itens.estoque-div-sai
                        fi-inv-ent-kg = fi-inv-ent-kg + b-tt-itens.estoque-inv-ent 
                        fi-inv-sai-kg = fi-inv-sai-kg + b-tt-itens.estoque-inv-sai 
                        fi-dev-kg     = fi-dev-kg     + b-tt-itens.estoque-dev
                        fi-fat-kg     = fi-fat-kg     + b-tt-itens.estoque-fat
                        fi-tot-kg     = fi-tot-kg     + b-tt-itens.estoque-final.
             WHEN 'UN' THEN
                 ASSIGN fi-ini-und     = fi-ini-und     + b-tt-itens.estoque-ini
                        fi-ent-und     = fi-ent-und     + b-tt-itens.estoque-ent
                        fi-div-ent-und = fi-div-ent-und + b-tt-itens.estoque-div-ent
                        fi-div-sai-und = fi-div-sai-und + b-tt-itens.estoque-div-sai
                        fi-inv-ent-und = fi-inv-ent-und + b-tt-itens.estoque-inv-ent 
                        fi-inv-sai-und = fi-inv-sai-und + b-tt-itens.estoque-inv-sai 
                        fi-dev-und     = fi-dev-und     + b-tt-itens.estoque-dev
                        fi-fat-und     = fi-fat-und     + b-tt-itens.estoque-fat
                        fi-tot-und     = fi-tot-und     + b-tt-itens.estoque-final.
         END CASE.

     END.
     IF i-lin > 39 THEN DO:
        RUN pi-imp-cabec.
        ASSIGN i-lin = 7.
     END.
     PUT "--------------- ------------- ------------- ------------- ------------- ------------- ------------- ------------- -------------" AT 53.
     PUT "TOTAL GERAL: M" AT 36.
     PUT fi-ini-m     FORMAT "->,>>>,>>9.99"  AT  55
         fi-ent-m     FORMAT "->,>>>,>>9.99"  AT  69
         fi-div-ent-m FORMAT "->,>>>,>>9.99"  AT  83
         fi-div-sai-m FORMAT "->,>>>,>>9.99"  AT  97
         fi-inv-ent-m FORMAT "->,>>>,>>9.99"  AT 111
         fi-inv-sai-m FORMAT "->,>>>,>>9.99"  AT 125
         fi-dev-m     FORMAT "->,>>>,>>9.99"  AT 139
         fi-fat-m     FORMAT "->,>>>,>>9.99"  AT 153
         fi-tot-m     FORMAT "->,>>>,>>9.99"  AT 167.
     PUT "KG" AT 49.
     PUT fi-ini-kg     FORMAT "->,>>>,>>9.99"  AT  55
         fi-ent-kg     FORMAT "->,>>>,>>9.99"  AT  69
         fi-div-ent-kg FORMAT "->,>>>,>>9.99"  AT  83
         fi-div-sai-kg FORMAT "->,>>>,>>9.99"  AT  97
         fi-inv-ent-kg FORMAT "->,>>>,>>9.99"  AT 111
         fi-inv-sai-kg FORMAT "->,>>>,>>9.99"  AT 125
         fi-dev-kg     FORMAT "->,>>>,>>9.99"  AT 139
         fi-fat-kg     FORMAT "->,>>>,>>9.99"  AT 153
         fi-tot-kg     FORMAT "->,>>>,>>9.99"  AT 167.
     PUT "UND" AT 49.
     PUT fi-ini-und     FORMAT "->,>>>,>>9.99"  AT  55
         fi-ent-und     FORMAT "->,>>>,>>9.99"  AT  69
         fi-div-ent-und FORMAT "->,>>>,>>9.99"  AT  83
         fi-div-sai-und FORMAT "->,>>>,>>9.99"  AT  97
         fi-inv-ent-und FORMAT "->,>>>,>>9.99"  AT 111
         fi-inv-sai-und FORMAT "->,>>>,>>9.99"  AT 125
         fi-dev-und     FORMAT "->,>>>,>>9.99"  AT 139
         fi-fat-und     FORMAT "->,>>>,>>9.99"  AT 153
         fi-tot-und     FORMAT "->,>>>,>>9.99"  AT 167.

     IF i-saida = 1 THEN DO:
        PAGE.
        PUT "" AT 1.
     END.
  END.
  IF i-saida = 3 THEN DO.
     RUN EXECUTE IN h-prog(INPUT "notepad.exe",
                           INPUT c-saida).
     DELETE PROCEDURE h-prog.
  END.
  OUTPUT CLOSE.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-monta-planilha C-Win 
PROCEDURE pi-monta-planilha :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR c-lin AS CHAR.

  RUN pi-excel-cabec (INPUT 1,
                      INPUT "Estoque").
  ASSIGN i-Lin    = 4.

  ASSIGN fi-ini-m       = 0 fi-ent-m      = 0 fi-div-ent-m   = 0 fi-div-sai-m   = 0 fi-inv-ent-m   = 0 fi-inv-sai-m   = 0
         fi-dev-m       = 0 fi-fat-m      = 0 fi-tot-m       = 0 fi-ini-kg      = 0 fi-ent-kg      = 0 fi-div-ent-kg  = 0
         fi-div-sai-kg  = 0 fi-inv-ent-kg = 0 fi-inv-sai-kg  = 0 fi-dev-kg      = 0 fi-fat-kg      = 0 fi-tot-kg      = 0
         fi-ini-und     = 0 fi-ent-und    = 0 fi-div-ent-und = 0 fi-div-sai-und = 0 fi-inv-ent-und = 0 fi-inv-sai-und = 0
         fi-dev-und     = 0 fi-fat-und    = 0 fi-tot-und     = 0.

  FOR EACH b-tt-itens NO-LOCK.

      ASSIGN chworksheet:range("A" + STRING(i-lin)):VALUE = b-tt-itens.it-codigo
             chworksheet:range("B" + STRING(i-lin)):VALUE = b-tt-itens.desc-item
             chworksheet:range("C" + STRING(i-lin)):VALUE = b-tt-itens.un
             chworksheet:range("D" + STRING(i-lin)):VALUE = b-tt-itens.estoque-ini
             chworksheet:range("E" + STRING(i-lin)):VALUE = b-tt-itens.estoque-ent
             chworksheet:range("F" + STRING(i-lin)):VALUE = b-tt-itens.estoque-div-ent
             chworksheet:range("G" + STRING(i-lin)):VALUE = b-tt-itens.estoque-div-sai
             chworksheet:range("H" + STRING(i-lin)):VALUE = b-tt-itens.estoque-inv-ent
             chworksheet:range("I" + STRING(i-lin)):VALUE = b-tt-itens.estoque-inv-sai
             chworksheet:range("J" + STRING(i-lin)):VALUE = b-tt-itens.estoque-dev
             chworksheet:range("K" + STRING(i-lin)):VALUE = b-tt-itens.estoque-fat
             chworksheet:range("L" + STRING(i-lin)):VALUE = b-tt-itens.estoque-final.

      /*  Configura Tamanho da Fonte */
      ASSIGN c-lin = STRING(i-lin) + ":" + STRING(i-lin)
             chworksheet:Rows(c-lin):FONT:SIZE = 9.

      ASSIGN i-lin = i-lin + 1.

      CASE b-tt-itens.un:
          WHEN 'M'   THEN
              ASSIGN fi-ini-m     = fi-ini-m     + b-tt-itens.estoque-ini
                     fi-ent-m     = fi-ent-m     + b-tt-itens.estoque-ent
                     fi-div-ent-m = fi-div-ent-m + b-tt-itens.estoque-div-ent
                     fi-div-sai-m = fi-div-sai-m + b-tt-itens.estoque-div-sai
                     fi-inv-ent-m = fi-inv-ent-m + b-tt-itens.estoque-inv-ent 
                     fi-inv-sai-m = fi-inv-sai-m + b-tt-itens.estoque-inv-sai 
                     fi-dev-m     = fi-dev-m     + b-tt-itens.estoque-dev
                     fi-fat-m     = fi-fat-m     + b-tt-itens.estoque-fat
                     fi-tot-m     = fi-tot-m     + b-tt-itens.estoque-final.
          WHEN 'KG'  THEN
              ASSIGN fi-ini-kg     = fi-ini-kg     + b-tt-itens.estoque-ini
                     fi-ent-kg     = fi-ent-kg     + b-tt-itens.estoque-ent
                     fi-div-ent-kg = fi-div-ent-kg + b-tt-itens.estoque-div-ent
                     fi-div-sai-kg = fi-div-sai-kg + b-tt-itens.estoque-div-sai
                     fi-inv-ent-kg = fi-inv-ent-kg + b-tt-itens.estoque-inv-ent 
                     fi-inv-sai-kg = fi-inv-sai-kg + b-tt-itens.estoque-inv-sai 
                     fi-dev-kg     = fi-dev-kg     + b-tt-itens.estoque-dev
                     fi-fat-kg     = fi-fat-kg     + b-tt-itens.estoque-fat
                     fi-tot-kg     = fi-tot-kg     + b-tt-itens.estoque-final.
          WHEN 'UN' THEN
              ASSIGN fi-ini-und     = fi-ini-und     + b-tt-itens.estoque-ini
                     fi-ent-und     = fi-ent-und     + b-tt-itens.estoque-ent
                     fi-div-ent-und = fi-div-ent-und + b-tt-itens.estoque-div-ent
                     fi-div-sai-und = fi-div-sai-und + b-tt-itens.estoque-div-sai
                     fi-inv-ent-und = fi-inv-ent-und + b-tt-itens.estoque-inv-ent 
                     fi-inv-sai-und = fi-inv-sai-und + b-tt-itens.estoque-inv-sai 
                     fi-dev-und     = fi-dev-und     + b-tt-itens.estoque-dev
                     fi-fat-und     = fi-fat-und     + b-tt-itens.estoque-fat
                     fi-tot-und     = fi-tot-und     + b-tt-itens.estoque-final.
      END CASE.

  END.
  IF i-lin <> 4 THEN DO:
     ASSIGN chworksheet:range("B" + STRING(i-lin)):VALUE = "TOTAL GERAL"
            chworksheet:range("C" + STRING(i-lin)):VALUE = "  M".
     ASSIGN chworksheet:range("D" + STRING(i-lin)):VALUE = fi-ini-m     
            chworksheet:range("E" + STRING(i-lin)):VALUE = fi-ent-m     
            chworksheet:range("F" + STRING(i-lin)):VALUE = fi-div-ent-m 
            chworksheet:range("G" + STRING(i-lin)):VALUE = fi-div-sai-m 
            chworksheet:range("H" + STRING(i-lin)):VALUE = fi-inv-ent-m 
            chworksheet:range("I" + STRING(i-lin)):VALUE = fi-inv-sai-m 
            chworksheet:range("J" + STRING(i-lin)):VALUE = fi-dev-m     
            chworksheet:range("K" + STRING(i-lin)):VALUE = fi-fat-m     
            chworksheet:range("L" + STRING(i-lin)):VALUE = fi-tot-m.     
      /* Colorir a Linha / Negrito */
     ASSIGN chWorkSheet:Range("A" + STRING(i-lin) + ":L" + STRING(i-lin)):Interior:ColorIndex = 14
            chWorkSheet:Range("A" + STRING(i-lin) + ":L" + STRING(i-lin)):FONT:ColorIndex     =  2
            chWorkSheet:Range("A" + STRING(i-lin) + ":L" + STRING(i-lin)):FONT:Bold           = TRUE.

     ASSIGN i-lin = i-lin + 1.

     ASSIGN chworksheet:range("C" + STRING(i-lin)):VALUE = " KG".
     ASSIGN chworksheet:range("D" + STRING(i-lin)):VALUE = fi-ini-kg     
            chworksheet:range("E" + STRING(i-lin)):VALUE = fi-ent-kg    
            chworksheet:range("F" + STRING(i-lin)):VALUE = fi-div-ent-kg
            chworksheet:range("G" + STRING(i-lin)):VALUE = fi-div-sai-kg
            chworksheet:range("H" + STRING(i-lin)):VALUE = fi-inv-ent-kg
            chworksheet:range("I" + STRING(i-lin)):VALUE = fi-inv-sai-kg
            chworksheet:range("J" + STRING(i-lin)):VALUE = fi-dev-kg    
            chworksheet:range("K" + STRING(i-lin)):VALUE = fi-fat-kg    
            chworksheet:range("L" + STRING(i-lin)):VALUE = fi-tot-kg.     
      /* Colorir a Linha / Negrito */
     ASSIGN chWorkSheet:Range("A" + STRING(i-lin) + ":L" + STRING(i-lin)):Interior:ColorIndex = 14
            chWorkSheet:Range("A" + STRING(i-lin) + ":L" + STRING(i-lin)):FONT:ColorIndex     =  2
            chWorkSheet:Range("A" + STRING(i-lin) + ":L" + STRING(i-lin)):FONT:Bold           = TRUE.

     ASSIGN i-lin = i-lin + 1.

     ASSIGN chworksheet:range("C" + STRING(i-lin)):VALUE = "UND".
     ASSIGN chworksheet:range("D" + STRING(i-lin)):VALUE = fi-ini-und    
            chworksheet:range("E" + STRING(i-lin)):VALUE = fi-ent-und   
            chworksheet:range("F" + STRING(i-lin)):VALUE = fi-div-ent-und
            chworksheet:range("G" + STRING(i-lin)):VALUE = fi-div-sai-und
            chworksheet:range("H" + STRING(i-lin)):VALUE = fi-inv-ent-und
            chworksheet:range("I" + STRING(i-lin)):VALUE = fi-inv-sai-und
            chworksheet:range("J" + STRING(i-lin)):VALUE = fi-dev-und   
            chworksheet:range("K" + STRING(i-lin)):VALUE = fi-fat-und   
            chworksheet:range("L" + STRING(i-lin)):VALUE = fi-tot-und.    
      /* Colorir a Linha / Negrito */
     ASSIGN chWorkSheet:Range("A" + STRING(i-lin) + ":L" + STRING(i-lin)):Interior:ColorIndex = 14
            chWorkSheet:Range("A" + STRING(i-lin) + ":L" + STRING(i-lin)):FONT:ColorIndex     =  2
            chWorkSheet:Range("A" + STRING(i-lin) + ":L" + STRING(i-lin)):FONT:Bold           = TRUE.
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-nfe C-Win 
PROCEDURE pi-nfe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*
DEF VAR c-natureza     AS CHAR.
DEF VAR c-regiao       AS CHAR.
DEF VAR i-ct           AS INTEGER.
DEF VAR c-cond-pagto   AS CHAR.
DEF VAR i-cod-vencto   AS INT.
DEF VAR i-prz          AS INT.

FOR EACH docum-est WHERE
         docum-est.cod-estabel = c-cod-estabel AND                 
         docum-est.dt-trans >= da-dt-trans-ini    AND                
         docum-est.dt-trans <= da-dt-trans-fin  NO-LOCK.

    FIND natur-oper WHERE
         natur-oper.nat-operacao = docum-est.nat-operacao NO-LOCK NO-ERROR.
    IF NOT AVAIL natur-oper THEN NEXT.
    IF natur-oper.tipo-compra <> 1 THEN NEXT. /* nfe de Cliente */

    FIND emitente WHERE
         emitente.cod-emit = docum-est.cod-emitente NO-LOCK NO-ERROR.
    IF NOT AVAIL emitente THEN NEXT.
/*     IF emitente.nome-abrev < c-nome-abrev-ini OR          */
/*        emitente.nome-abrev > c-nome-abrev-fin  THEN NEXT. */
/*                                                           */
    /* 1=PF, 2=PJ, 3=ESTRANGEIRO */
/*     IF (c-tipo-mercado = "I" AND emitente.natureza  > 2) OR         */
/*        (c-tipo-mercado = "E" AND emitente.natureza <> 3) THEN NEXT. */

    /* Exclui Cliente Tear Textil */
    FIND estabelec WHERE
         estabelec.cgc = emitente.cgc NO-LOCK NO-ERROR.
    IF AVAIL estabel THEN NEXT.
    IF SUBSTR(emitente.cgc,1,8) = "03123987" THEN NEXT. /* Retirar Apos Testes */

    FIND repres WHERE
         repres.cod-rep = emitente.cod-rep NO-LOCK NO-ERROR.
    IF NOT AVAIL repres THEN NEXT.
/*     IF repres.nome-abrev < c-no-ab-reppri-ini OR         */
/*        repres.nome-abrev > c-no-ab-reppri-fin THEN NEXT. */
/*                                                          */
    ASSIGN c-regiao ="".
    IF emitente.pais = "BRASIL" THEN DO:
       FIND unid-feder WHERE
            unid-feder.pais = emitente.pais AND
            unid-feder.estado = emitente.estado NO-LOCK NO-ERROR.
       IF AVAIL unid-feder THEN
          ASSIGN c-regiao = unid-feder.char-2.
    END.
    ELSE
       ASSIGN c-regiao = "Exportaá∆o".

    FOR EACH item-doc-est OF docum-est NO-LOCK.

       RUN pi-acompanhar IN h-acomp (INPUT "Data: "    + STRING(docum-est.dt-trans) +
                                            " Nota Fiscal: " + item-doc-est.nro-docto).

/*        IF item-doc-est.it-codigo < c-it-codigo-ini OR         */
/*           item-doc-est.it-codigo > c-it-codigo-fin THEN NEXT. */
/*        RUN pi-ver-digita (INPUT "Item",                  */
/*                           INPUT item-doc-est.it-codigo). */
/*        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.          */

/*        IF item-doc-est.cod-refer < c-cod-refer-ini OR         */
/*           item-doc-est.cod-refer > c-cod-refer-fin THEN NEXT. */
/*        RUN pi-ver-digita (INPUT "Referància",            */
/*                           INPUT item-doc-est.cod-refer). */
/*        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.          */

/*        IF c-tipo-acabamento = "E" AND SUBSTR(item-doc-est.cod-refer,7,1)  = '0' THEN NEXT. */
/*        IF c-tipo-acabamento = "L" AND SUBSTR(item-doc-est.cod-refer,7,1) <> '0' THEN NEXT. */

       FIND item-ext WHERE
            item-ext.it-codigo = item-doc-est.it-codigo NO-LOCK NO-ERROR.
/*        IF c-tipo-artigo <> 'A' THEN                                      */
/*           IF AVAIL item-ext AND                                          */
/*              (item-ext.indigo = YES AND c-tipo-artigo <> "I") OR         */
/*              (item-ext.indigo = NO  AND c-tipo-artigo <> "O") THEN NEXT. */

       /* Pegar o Lote do Item */
       FIND rat-lote WHERE
            rat-lote.serie-docto  = docum-est.serie-docto  AND
            rat-lote.nro-docto    = docum-est.nro-docto    AND 
            rat-lote.cod-emitente = docum-est.cod-emitente AND
            rat-lote.nat-operacao = docum-est.nat-operacao AND
            rat-lote.sequencia    = item-doc-est.sequencia NO-LOCK NO-ERROR.
       IF NOT AVAIL rat-lote THEN NEXT.

/*        FIND item WHERE                                                */
/*             item.it-codigo = item-doc-est.it-codigo NO-LOCK NO-ERROR. */
/*        IF (ITEM.ge-codigo < i-ge-codigo-ini) OR                       */
/*           (ITEM.ge-codigo > i-ge-codigo-fin) THEN NEXT.               */

       ASSIGN i-ct         = 0
              c-cond-pagto = ""
              i-cod-vencto = 0
              c-natureza   = "Sem NF de Origem"
              i-prz        = 0.


       FIND nota-fiscal WHERE
            nota-fiscal.cod-estabel  = docum-est.cod-estabel   AND
            nota-fiscal.serie        = item-doc-est.serie-comp AND
            nota-fiscal.nr-nota-fis  = item-doc-est.nro-comp   NO-LOCK NO-ERROR.

       IF AVAIL nota-fiscal THEN DO:
          FIND ped-venda WHERE
               ped-venda.nr-pedcli = nota-fiscal.nr-pedcli NO-LOCK NO-ERROR.
          IF AVAIL ped-venda THEN DO:
             FIND ped-venda-ext WHERE 
                  ped-venda-ext.nr-pedido = ped-venda.nr-pedido NO-LOCK NO-ERROR.
             IF AVAIL ped-venda-ext AND 
                      LOOKUP(ped-venda-ext.tp-pedido,"AMOSTRA,AMOSTRA EXPORTAÄ«O") <> 0 THEN NEXT.
          END.
          ASSIGN c-natureza = nota-fiscal.nat-operacao.
          FOR EACH fat-duplic WHERE
                   fat-duplic.cod-estabel = nota-fiscal.cod-estabel AND
                   fat-duplic.serie       = nota-fiscal.serie       AND
                   fat-duplic.nr-fatura   = nota-fiscal.nr-nota-fis NO-LOCK.
                   ASSIGN i-prz        = i-prz + (fat-duplic.dt-vencimen - fat-duplic.dt-emissao)
                          i-ct         = i-ct + 1
                          i-cod-vencto = fat-duplic.cod-vencto.
          END.
          IF i-prz <> 0 OR i-cod-vencto = 2 OR i-cod-vencto = 9 THEN DO: 
             IF i-cod-vencto = 2 THEN
                ASSIGN c-cond-pagto = " A Vista".
             ELSE
             IF i-cod-vencto = 9 THEN
                ASSIGN c-cond-pagto = " Contra Apresentaá∆o".
             ELSE DO:
                i-prz = INT(i-prz / i-ct).
                IF i-prz <= 30 THEN
                   ASSIGN c-cond-pagto = " De 01 AtÇ 30 Dias".
                ELSE
                IF i-prz <= 60 THEN
                   ASSIGN c-cond-pagto = " De 31 AtÇ 60 Dias".
                ELSE
                IF i-prz <= 90 THEN
                   ASSIGN c-cond-pagto = " De 61 AtÇ 90 Dias".
                ELSE
                   ASSIGN c-cond-pagto = "ˇAcima de 90 Dias". /* 1¶ Pos Ç ALT 164 */
             END.
          END.
          ELSE DO:
             FIND cond-pagto WHERE
                  cond-pagto.cod-cond-pag = nota-fiscal.cod-cond-pag NO-LOCK NO-ERROR.
             IF AVAIL cond-pagto THEN DO:
                ASSIGN c-cond-pagto = cond-pagto.descricao.
                IF cond-pagto.log-2 THEN /* Fat Ç para VENDOR */
                   ASSIGN c-cond-pagto = "ˇVendor". /* 1¶ Pos Ç ALT 164 */
             END.
          END.
          IF c-cond-pagto = "" THEN
             ASSIGN c-cond-pagto = " Cupom Fiscal".
          
          IF c-regiao = "Exportaá∆o" THEN
             ASSIGN c-cond-pagto = "ˇExportaá∆o". /* 1¶ Pos Ç ALT 164 */

       END.
       IF c-cond-pagto = "" THEN 
          ASSIGN c-cond-pagto = "ˇSem NF de Origem". /* 1¶ Pos Ç ALT 164 */

       RUN pi-grava-nfe (INPUT "1",
                           INPUT item-doc-est.it-codigo,
                           INPUT ITEM.un,
                           INPUT SUBSTR(rat-lote.lote,1,2),
                           INPUT item.desc-item,
                           INPUT item-doc-est.quantidade,
                           INPUT item-doc-est.preco-total[1],
                           INPUT docum-est.serie-docto,
                           INPUT docum-est.nro-docto,
                           INPUT docum-est.cod-emitente,
                           INPUT docum-est.nat-operacao).

       RUN pi-grava-nfe (INPUT "2",
                           INPUT repres.nome-abrev,
                           INPUT ITEM.un,
                           INPUT SUBSTR(rat-lote.lote,1,2),
                           INPUT repres.cod-rep,
                           INPUT item-doc-est.quantidade,
                           INPUT item-doc-est.preco-total[1],
                           INPUT docum-est.serie-docto,
                           INPUT docum-est.nro-docto,
                           INPUT docum-est.cod-emitente,
                           INPUT docum-est.nat-operacao).

       RUN pi-grava-nfe (INPUT "3",
                           INPUT emitente.nome-matriz,
                           INPUT ITEM.un,
                           INPUT SUBSTR(rat-lote.lote,1,2),
                           INPUT "",
                           INPUT item-doc-est.quantidade,   
                           INPUT item-doc-est.preco-total[1],
                           INPUT docum-est.serie-docto,
                           INPUT docum-est.nro-docto,
                           INPUT docum-est.cod-emitente,
                           INPUT docum-est.nat-operacao).

       RUN pi-grava-nfe (INPUT "4",
                           INPUT emitente.nome-abrev,
                           INPUT ITEM.un,
                           INPUT SUBSTR(rat-lote.lote,1,2),
                           INPUT STRING(emitente.cod-emitente),
                           INPUT item-doc-est.quantidade,   
                           INPUT item-doc-est.preco-total[1],
                           INPUT docum-est.serie-docto,
                           INPUT docum-est.nro-docto,
                           INPUT docum-est.cod-emitente,
                           INPUT docum-est.nat-operacao).

       RUN pi-grava-nfe (INPUT "5",
                           INPUT c-regiao,
                           INPUT ITEM.un,
                           INPUT SUBSTR(rat-lote.lote,1,2),
                           INPUT emitente.estado,
                           INPUT item-doc-est.quantidade,     
                           INPUT item-doc-est.preco-total[1],
                           INPUT docum-est.serie-docto,
                           INPUT docum-est.nro-docto,
                           INPUT docum-est.cod-emitente,
                           INPUT docum-est.nat-operacao).

       RUN pi-grava-nfe (INPUT "6",
                           INPUT c-natureza,  /*natur-oper.nat-operacao,*/
                           INPUT ITEM.un,
                           INPUT SUBSTR(rat-lote.lote,1,2),
                           INPUT STRING(natur-oper.aliquota-icm),
                           INPUT item-doc-est.quantidade,    
                           INPUT item-doc-est.preco-total[1],
                           INPUT docum-est.serie-docto,
                           INPUT docum-est.nro-docto,
                           INPUT docum-est.cod-emitente,
                           INPUT docum-est.nat-operacao).

       RUN pi-grava-nfe (INPUT "7",
                           INPUT c-cond-pagto,
                           INPUT ITEM.un,
                           INPUT SUBSTR(rat-lote.lote,1,2),
                           INPUT "",
                           INPUT item-doc-est.quantidade,   
                           INPUT item-doc-est.preco-total[1],
                           INPUT docum-est.serie-docto,
                           INPUT docum-est.nro-docto,
                           INPUT docum-est.cod-emitente,
                           INPUT docum-est.nat-operacao).
    END.
END.    
*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-popula-browse C-Win 
PROCEDURE pi-popula-browse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
   {utp/ut-liter.i Selecionando_Itens_do_Estoque *}
   RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

   ASSIGN bt-devolucao:SENSITIVE IN FRAME {&FRAME-NAME}  = NO
          bt-nfe:SENSITIVE IN FRAME {&FRAME-NAME}  = NO
          bt-nfiscal-tot:SENSITIVE IN FRAME {&FRAME-NAME}  = NO.

   ASSIGN c-titulo = "ANALISE DO ESTOQUE NO PERIODO DE: " +                                                   
                     STRING(da-dt-trans-ini, "99/99/9999") + " A " + STRING(da-dt-trans-fin, "99/99/9999")
                     br-itens:TITLE IN FRAME {&FRAME-NAME} = c-titulo.


   FIND FIRST param-global NO-LOCK NO-ERROR.
   FIND FIRST empresa WHERE
              empresa.ep-codigo = param-global.empresa-prin NO-LOCK NO-ERROR. 
   ASSIGN c-empresa = (IF AVAIL empresa THEN empresa.razao-social ELSE "").

   EMPTY TEMP-TABLE tt-itens.
   EMPTY TEMP-TABLE tt-nfiscal.
   EMPTY TEMP-TABLE tt-work.
   EMPTY TEMP-TABLE tt-devolucao.
   EMPTY TEMP-TABLE tt-nfe.

   ASSIGN c-lotes = "".
    IF l-lote-todos = YES THEN
       ASSIGN c-lotes = "rp,rd,sc,ca,".
    ELSE DO:
       ASSIGN c-lotes = c-lotes + IF l-lote-pp = YES THEN "pp," ELSE ",".
       ASSIGN c-lotes = c-lotes + IF l-lote-pd = YES THEN "pd," ELSE ",".
       ASSIGN c-lotes = c-lotes + IF l-lote-rp = YES THEN "rp," ELSE ",".
       ASSIGN c-lotes = c-lotes + IF l-lote-rd = YES THEN "rd," ELSE ",".
       ASSIGN c-lotes = c-lotes + IF l-lote-sc = YES THEN "sc," ELSE ",".
       ASSIGN c-lotes = c-lotes + IF l-lote-ca = YES THEN "ca," ELSE ",".
    END.

   RUN pi-separa-itens. 

   RUN pi-devolucao.

/*    RUN pi-nfe. */

   IF AVAIL tt-devolucao THEN
      ASSIGN bt-devolucao:SENSITIVE IN FRAME {&FRAME-NAME}  = YES.

   IF AVAIL tt-nfe THEN
      ASSIGN bt-nfe:SENSITIVE IN FRAME {&FRAME-NAME}  = YES.

   IF AVAIL tt-nfiscal THEN
      ASSIGN bt-nfiscal-tot:SENSITIVE IN FRAME {&FRAME-NAME}  = YES.

   RUN pi-finalizar in h-acomp.

   {&OPEN-QUERY-br-itens}
/*
   APPLY 'value-changed' TO br-itens IN FRAME {&FRAME-NAME}.
   APPLY 'entry' TO br-itens IN FRAME {&FRAME-NAME}.
   */
   RETURN NO-APPLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-separa-itens C-Win 
PROCEDURE pi-separa-itens :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

 FOR EACH item WHERE
          item.cod-estabel  = c-cod-estabel   AND
          item.it-codigo   >= c-it-codigo-ini AND
          item.it-codigo   <= c-it-codigo-fin AND
          ITEM.cod-refer   >= c-cod-refer-ini AND
          item.cod-refer   <= c-cod-refer-fin NO-LOCK.

     IF item.ge-codigo <> 50 AND item.ge-codigo <> 60 THEN NEXT.

     ASSIGN de-qtd-ini = 0.
     FOR EACH saldo-estoq WHERE
              saldo-estoq.cod-estabel = item.cod-estabel AND
              saldo-estoq.it-codigo   = item.it-codigo NO-LOCK.

         ASSIGN de-qtd-ini = de-qtd-ini + saldo-estoq.qtidade-atu.
     END.
     
     ASSIGN de-qtd-fin = 0 de-qtd-div-ent = 0 de-qtd-div-sai = 0 de-qtd-inv-ent = 0 
            de-qtd-inv-sai = 0 de-qtd-nfd = 0 de-qtd-nfe = 0 de-qtd-nfs = 0.

     FOR EACH movto-estoq WHERE
              movto-estoq.cod-estabel  = item.cod-estabel AND 
              movto-estoq.it-codigo    = item.it-codigo   AND
              /* movto-estoq.cod-refer    = ITEM.cod-refer   AND */
              movto-estoq.dt-trans    >= da-dt-trans-ini        NO-LOCK.

         
         FIND docum-est WHERE
              docum-est.cod-estabel = movto-estoq.cod-estabel AND
              docum-est.serie-docto = movto-estoq.serie-docto AND
              docum-est.nro-docto = movto-estoq.nro-docto NO-LOCK NO-ERROR.

         IF AVAIL docum-est THEN DO.
            ASSIGN i-cod-rep = docum-est.cod-emitente.
         END.

         RUN pi-acompanhar IN h-acomp (INPUT "Item: " + item.it-codigo +
                                             "Data: " + STRING(movto-estoq.dt-trans, "99/99/9999")).

         IF movto-estoq.tipo-trans = 1 THEN DO: /* Movimento de ENTRADA */
            ASSIGN de-qtd-ini = de-qtd-ini - movto-estoq.quantidade.
            IF movto-estoq.dt-trans <= da-dt-trans-fin THEN DO:
               CASE movto-estoq.esp-docto:
                   WHEN  6 THEN /* DIV */
                       ASSIGN de-qtd-div-ent = de-qtd-div-ent + movto-estoq.quantidade.
                   WHEN 15 THEN /* INV */
                       ASSIGN de-qtd-inv-ent = de-qtd-inv-ent + movto-estoq.quantidade.
                   WHEN 20 THEN /* NFD */
                       ASSIGN de-qtd-nfd = de-qtd-nfd + movto-estoq.quantidade.
                   WHEN 21 THEN DO. /* NFE */
                       
                       ASSIGN de-qtd-nfe = de-qtd-nfe + movto-estoq.quantidade.
                       IF AVAIL docum-est THEN DO.
                          FIND tt-nfe WHERE
                               tt-nfe.cod-estabel  = docum-est.cod-estabel AND
                               tt-nfe.serie        = docum-est.serie-docto AND
                               tt-nfe.nr-nota-fis  = docum-est.nro-docto   AND
                               tt-nfe.it-codigo    = movto-estoq.it-codigo   NO-LOCK NO-ERROR.
                          IF NOT AVAIL tt-nfe THEN DO:

                             CREATE tt-nfe.
                             ASSIGN tt-nfe.cod-estabel  = docum-est.cod-estabel 
                                    tt-nfe.serie        = docum-est.serie-docto 
                                    tt-nfe.nr-nota-fis  = docum-est.nro-docto 
                                    tt-nfe.it-codigo    = movto-estoq.it-codigo.
                                    tt-nfe.cod-rep      = i-cod-rep.   
                         END.
                       END.
                       
                   END.
               END CASE.
            END.
         END.
         ELSE DO: /* Movimento de SAIDA */

         FIND nota-fiscal WHERE
              nota-fiscal.cod-estabel = movto-estoq.cod-estabel AND
              nota-fiscal.serie = movto-estoq.serie-docto AND
              nota-fiscal.nr-nota-fis = movto-estoq.nro-docto NO-LOCK NO-ERROR.

         IF AVAIL nota-fiscal THEN DO.
            ASSIGN i-cod-rep = nota-fiscal.cod-rep.
         END.

            ASSIGN de-qtd-ini = de-qtd-ini + movto-estoq.quantidade.
            IF movto-estoq.dt-trans <= da-dt-trans-fin THEN DO:
               CASE movto-estoq.esp-docto:
                   WHEN  6 THEN /* DIV */
                       ASSIGN de-qtd-div-sai = de-qtd-div-sai + movto-estoq.quantidade.
                   WHEN 15 THEN /* INV */
                       ASSIGN de-qtd-inv-sai = de-qtd-inv-sai + movto-estoq.quantidade.
                   WHEN 22 THEN  DO: /* NFS */
                       IF movto-estoq.nat-operacao = "599FAB" THEN
                          ASSIGN de-qtd-nfd = de-qtd-nfd + movto-estoq.quantidade.
                       ELSE
                          ASSIGN de-qtd-nfs = de-qtd-nfs + movto-estoq.quantidade.
                       FIND tt-nfiscal WHERE
                            tt-nfiscal.cod-estabel  = movto-estoq.cod-estabel AND
                            tt-nfiscal.serie        = movto-estoq.serie-docto AND
                            tt-nfiscal.nr-nota-fis  = movto-estoq.nro-docto   AND
                            tt-nfiscal.it-codigo    = movto-estoq.it-codigo   NO-LOCK NO-ERROR.
                       IF NOT AVAIL tt-nfiscal THEN DO:
                          CREATE tt-nfiscal.
                          ASSIGN tt-nfiscal.cod-estabel  = movto-estoq.cod-estabel 
                                 tt-nfiscal.serie        = movto-estoq.serie-docto 
                                 tt-nfiscal.nr-nota-fis  = movto-estoq.nro-docto   
                                 tt-nfiscal.it-codigo    = movto-estoq.it-codigo
                                 tt-nfiscal.cod-rep      = i-cod-rep.   
                       END.
                   END.
               END CASE.
            END.
         END.
     END. 
     IF de-qtd-ini + de-qtd-div-ent + de-qtd-div-sai + de-qtd-inv-ent + de-qtd-inv-sai + de-qtd-nfd + de-qtd-nfe + de-qtd-nfs = 0 THEN NEXT.


     CREATE tt-itens.
     ASSIGN tt-itens.it-codigo       = item.it-codigo
            tt-itens.desc-item       = item.desc-item
            tt-itens.cod-refer       = ITEM.cod-refer
            /* tt-itens.nr-lote         = ob-etiqueta.nr-lote */
            tt-itens.un              = item.un
            tt-itens.estoque-ini     = de-qtd-ini
            tt-itens.estoque-div-ent = de-qtd-div-ent
            tt-itens.estoque-div-sai = de-qtd-div-sai
            tt-itens.estoque-inv-ent = de-qtd-inv-ent
            tt-itens.estoque-inv-sai = de-qtd-inv-sai
            tt-itens.estoque-dev     = de-qtd-nfd
            tt-itens.estoque-ent     = de-qtd-nfe
            tt-itens.estoque-fat     = de-qtd-nfs
            tt-itens.estoque-final   = de-qtd-ini + de-qtd-div-ent - de-qtd-div-sai + de-qtd-inv-ent -
                                       de-qtd-inv-sai + de-qtd-nfd + de-qtd-nfe - de-qtd-nfs.

 END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-tot-itens C-Win 
PROCEDURE pi-tot-itens :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   ASSIGN fi-ini-m       = 0 fi-ent-m      = 0 fi-div-ent-m   = 0 fi-div-sai-m   = 0 fi-inv-ent-m   = 0 fi-inv-sai-m   = 0
          fi-dev-m       = 0 fi-fat-m      = 0 fi-tot-m       = 0 fi-ini-kg      = 0 fi-ent-kg      = 0 fi-div-ent-kg  = 0
          fi-div-sai-kg  = 0 fi-inv-ent-kg = 0 fi-inv-sai-kg  = 0 fi-dev-kg      = 0 fi-fat-kg      = 0 fi-tot-kg      = 0
          fi-ini-und     = 0 fi-ent-und    = 0 fi-div-ent-und = 0 fi-div-sai-und = 0 fi-inv-ent-und = 0 fi-inv-sai-und = 0
          fi-dev-und     = 0 fi-fat-und    = 0 fi-tot-und     = 0.
   FOR EACH tt-itens.
       CASE tt-itens.un:
           WHEN 'M'   THEN
               ASSIGN fi-ini-m     = fi-ini-m     + tt-itens.estoque-ini
                      fi-ent-m     = fi-ent-m     + tt-itens.estoque-ent
                      fi-div-ent-m = fi-div-ent-m + tt-itens.estoque-div-ent
                      fi-div-sai-m = fi-div-sai-m + tt-itens.estoque-div-sai
                      fi-inv-ent-m = fi-inv-ent-m + tt-itens.estoque-inv-ent 
                      fi-inv-sai-m = fi-inv-sai-m + tt-itens.estoque-inv-sai 
                      fi-dev-m     = fi-dev-m     + tt-itens.estoque-dev
                      fi-fat-m     = fi-fat-m     + tt-itens.estoque-fat
                      fi-tot-m     = fi-tot-m     + tt-itens.estoque-final.
           WHEN 'KG'  THEN
               ASSIGN fi-ini-kg     = fi-ini-kg     + tt-itens.estoque-ini
                      fi-ent-kg     = fi-ent-kg     + tt-itens.estoque-ent
                      fi-div-ent-kg = fi-div-ent-kg + tt-itens.estoque-div-ent
                      fi-div-sai-kg = fi-div-sai-kg + tt-itens.estoque-div-sai
                      fi-inv-ent-kg = fi-inv-ent-kg + tt-itens.estoque-inv-ent 
                      fi-inv-sai-kg = fi-inv-sai-kg + tt-itens.estoque-inv-sai 
                      fi-dev-kg     = fi-dev-kg     + tt-itens.estoque-dev
                      fi-fat-kg     = fi-fat-kg     + tt-itens.estoque-fat
                      fi-tot-kg     = fi-tot-kg     + tt-itens.estoque-final.
           WHEN 'UN' THEN
               ASSIGN fi-ini-und     = fi-ini-und     + tt-itens.estoque-ini
                      fi-ent-und     = fi-ent-und     + tt-itens.estoque-ent
                      fi-div-ent-und = fi-div-ent-und + tt-itens.estoque-div-ent
                      fi-div-sai-und = fi-div-sai-und + tt-itens.estoque-div-sai
                      fi-inv-ent-und = fi-inv-ent-und + tt-itens.estoque-inv-ent 
                      fi-inv-sai-und = fi-inv-sai-und + tt-itens.estoque-inv-sai 
                      fi-dev-und     = fi-dev-und     + tt-itens.estoque-dev
                      fi-fat-und     = fi-fat-und     + tt-itens.estoque-fat
                      fi-tot-und     = fi-tot-und     + tt-itens.estoque-final.
       END CASE.
   END.
   DISPLAY fi-ini-m   fi-ent-m   fi-div-ent-m   fi-div-sai-m   fi-inv-ent-m   fi-inv-sai-m   fi-dev-m   fi-fat-m   fi-tot-m   
           fi-ini-kg  fi-ent-kg  fi-div-ent-kg  fi-div-sai-kg  fi-inv-ent-kg  fi-inv-sai-kg  fi-dev-kg  fi-fat-kg  fi-tot-kg  
           fi-ini-und fi-ent-und fi-div-ent-und fi-div-sai-und fi-inv-ent-und fi-inv-sai-und fi-dev-und fi-fat-und fi-tot-und 
           WITH FRAME {&FRAME-NAME}.
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-ver-digita C-Win 
PROCEDURE pi-ver-digita :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER p-campo AS CHAR.
    DEF INPUT PARAMETER p-valor AS CHAR.

    IF CAN-FIND(FIRST tt-digita WHERE
                      tt-digita.opcao = 'D' AND
                      tt-digita.campo = p-campo) AND
       NOT CAN-FIND(FIRST tt-digita WHERE
                          tt-digita.opcao = 'D' AND
                          tt-digita.campo = p-campo AND
                          tt-digita.valor = p-valor) THEN RETURN 'ADM-ERROR'.
    ELSE
      IF CAN-FIND(FIRST tt-digita WHERE
                        tt-digita.opcao = 'E' AND
                        tt-digita.campo = p-campo AND
                        tt-digita.valor = p-valor) THEN RETURN 'ADM-ERROR'.
      ELSE
         RETURN 'OK'.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

