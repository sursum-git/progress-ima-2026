&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME w-relat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-relat 
/*:T*******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESCC0005 2.04.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/*:T Preprocessadores do Template de Relat¢rio                            */
/*:T Obs: Retirar o valor do preprocessador para as p ginas que nÆo existirem  */

&GLOBAL-DEFINE PGSEL f-pg-sel
&GLOBAL-DEFINE PGCLA f-pg-cla
&GLOBAL-DEFINE PGPAR f-pg-par
&GLOBAL-DEFINE PGDIG
&GLOBAL-DEFINE PGIMP f-pg-imp
  
/* Parameters Definitions ---                                           */

/* Temporary Table Definitions ---                                      */

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
       FIELD nao-confirm     AS LOGICAL FORMAT "Sim/NÆo"     
       FIELD confirm         AS LOGICAL FORMAT "Sim/NÆo"     
       FIELD em-cotacao      AS LOGICAL FORMAT "Sim/NÆo"     
       FIELD terminada       AS LOGICAL FORMAT "Sim/NÆo"     
       FIELD eliminada       AS LOGICAL FORMAT "Sim/NÆo"
       FIELD cotada          AS LOGICAL FORMAT "Sim/NÆo"
       FIELD gerar-excel     AS LOG FORMAT "Sim/NÆo"
       FIELD arq-excel       AS CHAR FORMAT "x(45)"
       FIELD impr-param      AS LOGICAL.

define temp-table tt-digita no-undo
       field ordem            as integer   format ">>>>9"
       field exemplo          as character format "x(30)"
       index id ordem.

define buffer b-tt-digita for tt-digita.

/* Transfer Definitions */

def var raw-param        as raw no-undo.

def temp-table tt-raw-digita
   field raw-digita      as raw.
                    
/* Local Variable Definitions ---                                       */

def var l-ok               as logical no-undo.
def var c-arq-digita       as char    no-undo.
def var c-terminal         as char    no-undo.


DEF VAR c-meses AS CHAR EXTENT 12 INIT ["Jan","Fev","Mar","Abr","Mai","Jun",
                                        "Jul","Ago","Set","Out","Nov","Dez"].

DEF VAR i-data     AS CHAR FORMAT "x(6)" EXTENT 12.
DEF VAR i-ano      AS INT.
DEF VAR i-mes      AS INT.
DEF VAR i-cont     AS INT.
DEF VAR i-ct       AS INT.
DEF VAR c-esp-fat  AS CHAR.
DEF VAR c-esp-prd  AS CHAR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-relat
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME f-pg-cla

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rs-classif 
&Scoped-Define DISPLAYED-OBJECTS rs-classif 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-relat AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE rs-classif AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Por Descri‡Æo dos Produtos", 1,
"Por C¢digo dos Produtos", 2
     SIZE 28 BY 2.5
     FONT 1 NO-UNDO.

DEFINE BUTTON bt-arquivo 
     IMAGE-UP FILE "image\im-sea":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE BUTTON bt-config-impr 
     IMAGE-UP FILE "image\im-cfprt":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE VARIABLE c-arquivo AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 40 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE text-destino AS CHARACTER FORMAT "X(256)":U INITIAL " Destino" 
      VIEW-AS TEXT 
     SIZE 8.57 BY .63 NO-UNDO.

DEFINE VARIABLE text-modo AS CHARACTER FORMAT "X(256)":U INITIAL "Execu‡Æo" 
      VIEW-AS TEXT 
     SIZE 10.86 BY .63 NO-UNDO.

DEFINE VARIABLE rs-destino AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Impressora", 1,
"Arquivo", 2,
"Terminal", 3
     SIZE 44 BY 1.08 NO-UNDO.

DEFINE VARIABLE rs-execucao AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "On-Line", 1,
"Batch", 2
     SIZE 27.72 BY .92 NO-UNDO.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 46.29 BY 2.92.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 46.29 BY 1.71.

DEFINE VARIABLE fi-arq-excel AS CHARACTER FORMAT "X(256)":U 
     LABEL "Arquivo" 
     VIEW-AS FILL-IN 
     SIZE 36 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-moeda AS CHARACTER FORMAT "x(12)" 
     VIEW-AS FILL-IN 
     SIZE 11.86 BY .79 NO-UNDO.

DEFINE VARIABLE fi-mo-codigo AS INTEGER FORMAT ">9" INITIAL 0 
     LABEL "Moeda":R8 
     VIEW-AS FILL-IN 
     SIZE 4 BY .79 TOOLTIP "C¢digo da moeda para conversÆo dos pre‡os." NO-UNDO.

DEFINE VARIABLE rs-data-conv AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Emissao", 1,
"Entrega", 2,
"Atual", 3
     SIZE 25 BY 1 TOOLTIP "Data a ser utilizada para a conversÆo dos pre‡os."
     FONT 1 NO-UNDO.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 70 BY 3.58.

DEFINE RECTANGLE RECT-27
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 70 BY 1.75.

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 70 BY 1.38.

DEFINE RECTANGLE RECT-29
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 70 BY 2.

DEFINE VARIABLE tg-confirm AS LOGICAL INITIAL yes 
     LABEL "Confirmada" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .83 NO-UNDO.

DEFINE VARIABLE tg-cotada AS LOGICAL INITIAL yes 
     LABEL "Cotada" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE tg-eliminada AS LOGICAL INITIAL no 
     LABEL "Eliminada" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .83 NO-UNDO.

DEFINE VARIABLE tg-em-cotacao AS LOGICAL INITIAL no 
     LABEL "Em Cota‡Æo" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .83 NO-UNDO.

DEFINE VARIABLE tg-gerar-excel AS LOGICAL INITIAL yes 
     LABEL "Gerar Planilha Excel" 
     VIEW-AS TOGGLE-BOX
     SIZE 18.14 BY .88 TOOLTIP "Imprimir Parƒmetros/Sele‡Æo ao final do relat¢rio."
     FONT 1 NO-UNDO.

DEFINE VARIABLE tg-nao-confirm AS LOGICAL INITIAL no 
     LABEL "NÆo Confirmada" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .83 NO-UNDO.

DEFINE VARIABLE tg-terminada AS LOGICAL INITIAL yes 
     LABEL "Terminada" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .83 NO-UNDO.

DEFINE VARIABLE to-impr-param AS LOGICAL INITIAL yes 
     LABEL "Imprimir Parƒmetros/Sele‡Æo" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .83 TOOLTIP "Imprimir Parƒmetros/Sele‡Æo ao final do relat¢rio."
     FONT 1 NO-UNDO.

DEFINE VARIABLE cb-esp-doc-fat AS CHARACTER FORMAT "X(5)":U INITIAL "NFS" 
     LABEL "Esp‚cie NF" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 8
     DROP-DOWN-LIST
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE cb-esp-doc-prd1 AS CHARACTER FORMAT "X(5)" INITIAL "1" 
     LABEL "Reporte":R21 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 7 BY 1 TOOLTIP "Esp‚cie do documento de reporte de produ‡Æo." NO-UNDO.

DEFINE VARIABLE cb-esp-doc-prd2 AS CHARACTER FORMAT "X(5)" INITIAL "1" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 7 BY 1 TOOLTIP "Esp‚cie do documento de reporte de produ‡Æo." NO-UNDO.

DEFINE VARIABLE cb-esp-doc-prd3 AS CHARACTER FORMAT "X(5)" INITIAL "1" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 7 BY 1 TOOLTIP "Esp‚cie do documento de reporte de produ‡Æo." NO-UNDO.

DEFINE VARIABLE fi-cod-estabel AS CHARACTER FORMAT "X(3)":U 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fin-ge-compra AS INTEGER FORMAT ">9" INITIAL 99 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 TOOLTIP "Grupo de estoque final" NO-UNDO.

DEFINE VARIABLE fi-fin-ge-fatur AS INTEGER FORMAT ">9" INITIAL 99 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 TOOLTIP "Grupo de estoque final" NO-UNDO.

DEFINE VARIABLE fi-fin-it-compra AS CHARACTER FORMAT "X(16)" INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 TOOLTIP "C¢digo do ¡tem final" NO-UNDO.

DEFINE VARIABLE fi-fin-it-fatur AS CHARACTER FORMAT "X(16)" INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 TOOLTIP "C¢digo do ¡tem final" NO-UNDO.

DEFINE VARIABLE fi-ini-ge-compra AS INTEGER FORMAT ">9" INITIAL 0 
     LABEL "Grupo Estoque":R16 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 TOOLTIP "Grupo de estoque inicial" NO-UNDO.

DEFINE VARIABLE fi-ini-ge-fatur AS INTEGER FORMAT ">9" INITIAL 0 
     LABEL "Grupo Estoque":R16 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 TOOLTIP "Grupo de estoque inicial" NO-UNDO.

DEFINE VARIABLE fi-ini-it-compra AS CHARACTER FORMAT "X(16)" 
     LABEL "Item":R5 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 TOOLTIP "C¢digo do ¡tem inicial" NO-UNDO.

DEFINE VARIABLE fi-ini-it-fatur AS CHARACTER FORMAT "X(16)" 
     LABEL "Item":R5 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 TOOLTIP "C¢digo do ¡tem inicial" NO-UNDO.

DEFINE VARIABLE fi-nome-estabel AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 55.2 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi-periodo-fin AS CHARACTER FORMAT "9999/99":U INITIAL "      /" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-periodo-ini AS CHARACTER FORMAT "9999/99":U INITIAL "      /" 
     LABEL "Periodo Inicial" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-serie-fatur AS CHARACTER FORMAT "x(5)" INITIAL "1" 
     LABEL "S‚rie":R7 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 TOOLTIP "S‚rie da NF do Faturamento." NO-UNDO.

DEFINE IMAGE IMAGE-29
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-30
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-31
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-32
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-67
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-68
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-69
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-70
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-71
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-72
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE RECTANGLE RECT-41
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 75 BY 2.54.

DEFINE RECTANGLE RECT-42
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 75 BY 2.58.

DEFINE RECTANGLE RECT-43
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 75 BY 3.71.

DEFINE BUTTON bt-ajuda 
     LABEL "Ajuda" 
     SIZE 10 BY 1
     FONT 1.

DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "Fechar" 
     SIZE 10 BY 1
     FONT 1.

DEFINE BUTTON bt-executar 
     LABEL "Executar" 
     SIZE 10 BY 1
     FONT 1.

DEFINE IMAGE im-pg-cla
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.21.

DEFINE IMAGE im-pg-imp
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.21.

DEFINE IMAGE im-pg-par
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.21.

DEFINE IMAGE im-pg-sel
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.21.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 79 BY 1.42
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 0  
     SIZE 78.72 BY .13
     BGCOLOR 7 .

DEFINE RECTANGLE rt-folder
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL 
     SIZE 79 BY 11.38
     FGCOLOR 0 .

DEFINE RECTANGLE rt-folder-left
     EDGE-PIXELS 0  
     SIZE .43 BY 11.21
     BGCOLOR 15 .

DEFINE RECTANGLE rt-folder-right
     EDGE-PIXELS 0  
     SIZE .43 BY 11.17
     BGCOLOR 7 .

DEFINE RECTANGLE rt-folder-top
     EDGE-PIXELS 0  
     SIZE 78.72 BY .13
     BGCOLOR 15 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-relat
     bt-executar AT ROW 14.54 COL 3 HELP
          "Dispara a execu‡Æo do relat¢rio"
     bt-cancelar AT ROW 14.54 COL 14 HELP
          "Fechar"
     bt-ajuda AT ROW 14.54 COL 70 HELP
          "Ajuda"
     rt-folder AT ROW 2.5 COL 2
     rt-folder-top AT ROW 2.54 COL 2.14
     rt-folder-left AT ROW 2.54 COL 2.14
     RECT-1 AT ROW 14.29 COL 2
     RECT-6 AT ROW 13.75 COL 2.14
     rt-folder-right AT ROW 2.67 COL 80.43
     im-pg-cla AT ROW 1.5 COL 17.72
     im-pg-imp AT ROW 1.5 COL 49.14
     im-pg-par AT ROW 1.5 COL 33.43
     im-pg-sel AT ROW 1.5 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81 BY 15
         DEFAULT-BUTTON bt-executar.

DEFINE FRAME f-pg-imp
     rs-destino AT ROW 2.38 COL 3.29 HELP
          "Destino de ImpressÆo do Relat¢rio" NO-LABEL
     bt-config-impr AT ROW 3.58 COL 43.29 HELP
          "Configura‡Æo da impressora"
     bt-arquivo AT ROW 3.58 COL 43.29 HELP
          "Escolha do nome do arquivo"
     c-arquivo AT ROW 3.63 COL 3.29 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL
     rs-execucao AT ROW 5.75 COL 3 HELP
          "Modo de Execu‡Æo" NO-LABEL
     text-destino AT ROW 1.63 COL 3.86 NO-LABEL
     text-modo AT ROW 5 COL 1.29 COLON-ALIGNED NO-LABEL
     RECT-7 AT ROW 1.92 COL 2.14
     RECT-9 AT ROW 5.29 COL 2.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 73.72 BY 10
         FONT 1.

DEFINE FRAME f-pg-par
     rs-data-conv AT ROW 1.92 COL 22 NO-LABEL
     fi-mo-codigo AT ROW 2 COL 55.29 COLON-ALIGNED HELP
          "F5 para zoom"
     fi-desc-moeda AT ROW 2 COL 59.43 COLON-ALIGNED HELP
          "Descri‡Æo da moeda" NO-LABEL
     tg-nao-confirm AT ROW 4.04 COL 6.57
     tg-terminada AT ROW 4.04 COL 27
     tg-confirm AT ROW 5.04 COL 6.57
     tg-eliminada AT ROW 5.04 COL 27
     tg-em-cotacao AT ROW 6.04 COL 6.57
     tg-cotada AT ROW 6.04 COL 27
     tg-gerar-excel AT ROW 7.96 COL 6.29
     fi-arq-excel AT ROW 7.96 COL 31.14 COLON-ALIGNED
     to-impr-param AT ROW 9.96 COL 26.29
     RECT-18 AT ROW 3.5 COL 4.57
     RECT-27 AT ROW 1.5 COL 4.57
     RECT-28 AT ROW 9.67 COL 4.57
     RECT-29 AT ROW 7.42 COL 4.57
     "Data para ConversÆo:" VIEW-AS TEXT
          SIZE 16 BY .75 AT ROW 2 COL 6
     "  Situa‡Æo das Ordens" VIEW-AS TEXT
          SIZE 16 BY .54 AT ROW 3.25 COL 8.29
     "Planilha para Excel" VIEW-AS TEXT
          SIZE 13.57 BY .54 AT ROW 7.13 COL 29.29
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 75 BY 10.46
         FONT 1.

DEFINE FRAME f-pg-cla
     rs-classif AT ROW 1.25 COL 2.72 HELP
          "Classifica‡Æo para emissÆo do relat¢rio" NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 76.86 BY 10.31.

DEFINE FRAME f-pg-sel
     fi-cod-estabel AT ROW 1.75 COL 13.29 COLON-ALIGNED
     fi-nome-estabel AT ROW 1.75 COL 18.57 COLON-ALIGNED NO-LABEL
     fi-periodo-ini AT ROW 2.75 COL 13.29 COLON-ALIGNED
     fi-periodo-fin AT ROW 2.75 COL 30.29 COLON-ALIGNED NO-LABEL
     cb-esp-doc-prd1 AT ROW 2.75 COL 51.72 COLON-ALIGNED HELP
          "Esp‚cie do Documento"
     cb-esp-doc-prd2 AT ROW 2.75 COL 59.43 COLON-ALIGNED HELP
          "Esp‚cie do Documento" NO-LABEL
     cb-esp-doc-prd3 AT ROW 2.75 COL 67.14 COLON-ALIGNED HELP
          "Esp‚cie do Documento" NO-LABEL
     fi-ini-ge-compra AT ROW 4.79 COL 16.86 COLON-ALIGNED HELP
          "Grupo de estoque a que pertence o item"
     fi-fin-ge-compra AT ROW 4.79 COL 44.29 COLON-ALIGNED HELP
          "Grupo de estoque a que pertence o item" NO-LABEL
     fi-ini-it-compra AT ROW 5.79 COL 16.86 COLON-ALIGNED
     fi-fin-it-compra AT ROW 5.79 COL 44.29 COLON-ALIGNED NO-LABEL
     fi-ini-ge-fatur AT ROW 7.88 COL 16.86 COLON-ALIGNED HELP
          "Grupo de estoque a que pertence o item"
     fi-fin-ge-fatur AT ROW 7.88 COL 44.29 COLON-ALIGNED HELP
          "Grupo de estoque a que pertence o item" NO-LABEL
     fi-ini-it-fatur AT ROW 8.88 COL 16.86 COLON-ALIGNED
     fi-fin-it-fatur AT ROW 8.88 COL 44.29 COLON-ALIGNED NO-LABEL
     fi-serie-fatur AT ROW 9.88 COL 16.86 COLON-ALIGNED HELP
          "S‚rie da nota fiscal"
     cb-esp-doc-fat AT ROW 9.88 COL 44.43 COLON-ALIGNED
     IMAGE-29 AT ROW 5.79 COL 33.14
     IMAGE-30 AT ROW 5.79 COL 43.14
     IMAGE-31 AT ROW 2.75 COL 24.43
     IMAGE-32 AT ROW 2.75 COL 29.14
     IMAGE-67 AT ROW 4.79 COL 33.14
     IMAGE-68 AT ROW 4.79 COL 43.14
     IMAGE-69 AT ROW 8.88 COL 33.14
     IMAGE-70 AT ROW 8.88 COL 43.14
     IMAGE-71 AT ROW 7.88 COL 33.14
     IMAGE-72 AT ROW 7.88 COL 43.14
     RECT-41 AT ROW 4.46 COL 2
     RECT-42 AT ROW 1.42 COL 2
     RECT-43 AT ROW 7.46 COL 2
     "COMPRAS" VIEW-AS TEXT
          SIZE 8 BY .54 AT ROW 4.17 COL 35
     "FATURAMENTO" VIEW-AS TEXT
          SIZE 12 BY .54 AT ROW 7.17 COL 33.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.85
         SIZE 76.86 BY 10.62
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: w-relat
   Allow: Basic,Browse,DB-Fields,Window,Query
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-relat ASSIGN
         HIDDEN             = YES
         TITLE              = "Gerencial de Compras, Faturamento e Produ‡Æo"
         HEIGHT             = 15
         WIDTH              = 81.14
         MAX-HEIGHT         = 28.75
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 28.75
         VIRTUAL-WIDTH      = 146.29
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         FONT               = 1
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-relat 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-relat.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-relat
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-pg-cla
                                                                        */
/* SETTINGS FOR FRAME f-pg-imp
                                                                        */
/* SETTINGS FOR FILL-IN text-destino IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       text-destino:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Destino".

/* SETTINGS FOR FILL-IN text-modo IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       text-modo:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Execu‡Æo".

/* SETTINGS FOR FRAME f-pg-par
                                                                        */
/* SETTINGS FOR FILL-IN fi-desc-moeda IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-gerar-excel IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME f-pg-sel
                                                                        */
/* SETTINGS FOR FILL-IN fi-nome-estabel IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-periodo-fin IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME f-relat
                                                                        */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-6 IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder-left IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder-right IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder-top IN FRAME f-relat
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-relat)
THEN w-relat:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-pg-imp
/* Query rebuild information for FRAME f-pg-imp
     _Query            is NOT OPENED
*/  /* FRAME f-pg-imp */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-pg-sel
/* Query rebuild information for FRAME f-pg-sel
     _Query            is NOT OPENED
*/  /* FRAME f-pg-sel */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-relat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON END-ERROR OF w-relat /* Gerencial de Compras, Faturamento e Produ‡Æo */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON WINDOW-CLOSE OF w-relat /* Gerencial de Compras, Faturamento e Produ‡Æo */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda w-relat
ON CHOOSE OF bt-ajuda IN FRAME f-relat /* Ajuda */
DO:
   {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-imp
&Scoped-define SELF-NAME bt-arquivo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-arquivo w-relat
ON CHOOSE OF bt-arquivo IN FRAME f-pg-imp
DO:
    {include/i-rparq.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME bt-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancelar w-relat
ON CHOOSE OF bt-cancelar IN FRAME f-relat /* Fechar */
DO:
   apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-imp
&Scoped-define SELF-NAME bt-config-impr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-config-impr w-relat
ON CHOOSE OF bt-config-impr IN FRAME f-pg-imp
DO:
   {include/i-rpimp.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME bt-executar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-executar w-relat
ON CHOOSE OF bt-executar IN FRAME f-relat /* Executar */
DO:
   do  on error undo, return no-apply:
       run pi-executar.
   end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME fi-arq-excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-arq-excel w-relat
ON LEAVE OF fi-arq-excel IN FRAME f-pg-par /* Arquivo */
DO:
  IF NOT fi-arq-excel:SCREEN-VALUE IN FRAME {&FRAME-NAME} MATCHES "*.csv*" THEN DO:
     MESSAGE "Nome do arquivo para Excel est  inv lido." SKIP
             "Deve ser do tipo Caminho/Arquivo.csv" VIEW-AS ALERT-BOX.
     ASSIGN fi-arq-excel:SCREEN-VALUE IN FRAME f-pg-par = SESSION:TEMP-DIRECTORY + 
                         "escc0005-#.csv".
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-sel
&Scoped-define SELF-NAME fi-cod-estabel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-estabel w-relat
ON ENTRY OF fi-cod-estabel IN FRAME f-pg-sel /* Estabelecimento */
DO:
  IF SELF:SCREEN-VALUE <> "" THEN DO.  
     FIND estabelec WHERE
          estabelec.cod-estabel = INPUT FRAME {&FRAME-NAME} fi-cod-estabel
          NO-LOCK NO-ERROR.
     IF AVAIL estabelec THEN
        ASSIGN fi-nome-estabel:SCREEN-VALUE IN FRAME {&FRAME-NAME} = estabelec.nome.
  END.
  ELSE ASSIGN fi-nome-estabel:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-estabel w-relat
ON LEAVE OF fi-cod-estabel IN FRAME f-pg-sel /* Estabelecimento */
DO:
  FIND estabelec WHERE
       estabelec.cod-estabel = INPUT FRAME {&FRAME-NAME} fi-cod-estabel NO-LOCK NO-ERROR.

  IF NOT AVAIL estabelec THEN DO.
     MESSAGE "Estabelecimento nÆo Cadastrado..." VIEW-AS ALERT-BOX.
     RETURN NO-APPLY.
  END.
  ASSIGN fi-nome-estabel:SCREEN-VALUE IN FRAME {&FRAME-NAME} = estabelec.nome.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-estabel w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-cod-estabel IN FRAME f-pg-sel /* Estabelecimento */
DO:
  {include/zoomvar.i &prog-zoom = adzoom/z01ad107.w
                     &campo     = fi-cod-estabel
                     &campozoom = cod-estabel
                     &FRAME     = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-fin-ge-compra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fin-ge-compra w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-fin-ge-compra IN FRAME f-pg-sel
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z01in142.w
                     &campo     = fi-fin-ge-compra
                     &campozoom = ge-codigo
                     &FRAME     = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-fin-ge-fatur
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fin-ge-fatur w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-fin-ge-fatur IN FRAME f-pg-sel
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z01in142.w
                     &campo     = fi-fin-ge-fatur
                     &campozoom = ge-codigo
                     &FRAME     = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-fin-it-compra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fin-it-compra w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-fin-it-compra IN FRAME f-pg-sel
DO:
   {include/zoomvar.i &prog-zoom = inzoom/z01in172.w
                    &campo       = fi-fin-it-compra
                    &campozoom   = it-codigo
                    &FRAME       = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-fin-it-fatur
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fin-it-fatur w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-fin-it-fatur IN FRAME f-pg-sel
DO:
   {include/zoomvar.i &prog-zoom = inzoom/z01in172.w
                    &campo       = fi-fin-it-fatur
                    &campozoom   = it-codigo
                    &FRAME       = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ini-ge-compra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-ge-compra w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ini-ge-compra IN FRAME f-pg-sel /* Grupo Estoque */
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z01in142.w
                     &campo     = fi-ini-ge-compra
                     &campozoom = ge-codigo
                     &FRAME     = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ini-ge-fatur
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-ge-fatur w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ini-ge-fatur IN FRAME f-pg-sel /* Grupo Estoque */
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z01in142.w
                     &campo     = fi-ini-ge-fatur
                     &campozoom = ge-codigo
                     &FRAME     = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ini-it-compra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-it-compra w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ini-it-compra IN FRAME f-pg-sel /* Item */
DO:
   {include/zoomvar.i &prog-zoom = inzoom/z01in172.w
                    &campo       = fi-ini-it-compra
                    &campozoom   = it-codigo
                    &FRAME       = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ini-it-fatur
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-it-fatur w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ini-it-fatur IN FRAME f-pg-sel /* Item */
DO:
   {include/zoomvar.i &prog-zoom = inzoom/z01in172.w
                    &campo       = fi-ini-it-fatur
                    &campozoom   = it-codigo
                    &FRAME       = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME fi-mo-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-mo-codigo w-relat
ON LEAVE OF fi-mo-codigo IN FRAME f-pg-par /* Moeda */
DO:
  FIND moeda WHERE moeda.mo-codigo = INT(INPUT FRAME f-pg-par fi-mo-codigo) NO-LOCK NO-ERROR.
  IF NOT AVAIL moeda THEN DO:
     MESSAGE "Moeda nÆo cadastrada." VIEW-AS ALERT-BOX.
     RETURN NO-APPLY.
  END.
  ELSE
     ASSIGN fi-desc-moeda:SCREEN-VALUE IN FRAME f-pg-par = moeda.descricao.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-sel
&Scoped-define SELF-NAME fi-periodo-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-periodo-ini w-relat
ON LEAVE OF fi-periodo-ini IN FRAME f-pg-sel /* Periodo Inicial */
DO:
  ASSIGN fi-periodo-fin:SCREEN-VALUE = IF SUBSTR(SELF:SCREEN-VALUE,6,2) = "01"
                                       THEN SUBSTR(SELF:SCREEN-VALUE,1,4) + "12" 
                                       ELSE STRING(INT(SUBSTR(SELF:SCREEN-VALUE,1,4)) + 1) +
                                            STRING(INT(SUBSTR(SELF:SCREEN-VALUE,6,2)) - 1,"99").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME im-pg-cla
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-cla w-relat
ON MOUSE-SELECT-CLICK OF im-pg-cla IN FRAME f-relat
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME im-pg-imp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-imp w-relat
ON MOUSE-SELECT-CLICK OF im-pg-imp IN FRAME f-relat
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME im-pg-par
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-par w-relat
ON MOUSE-SELECT-CLICK OF im-pg-par IN FRAME f-relat
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME im-pg-sel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-sel w-relat
ON MOUSE-SELECT-CLICK OF im-pg-sel IN FRAME f-relat
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-imp
&Scoped-define SELF-NAME rs-destino
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-destino w-relat
ON VALUE-CHANGED OF rs-destino IN FRAME f-pg-imp
DO:
do  with frame f-pg-imp:
    case self:screen-value:
        when "1" then do:
            assign c-arquivo:sensitive    = no
                   bt-arquivo:visible     = no
                   bt-config-impr:visible = yes.
        end.
        when "2" then do:
            assign c-arquivo:sensitive     = yes
                   bt-arquivo:visible      = yes
                   bt-config-impr:visible  = no.
        end.
        when "3" then do:
            assign c-arquivo:sensitive     = no
                   bt-arquivo:visible      = no
                   bt-config-impr:visible  = no.
        end.
    end case.
end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-execucao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-execucao w-relat
ON VALUE-CHANGED OF rs-execucao IN FRAME f-pg-imp
DO:
   {include/i-rprse.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME tg-gerar-excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-gerar-excel w-relat
ON VALUE-CHANGED OF tg-gerar-excel IN FRAME f-pg-par /* Gerar Planilha Excel */
DO:
  IF INPUT FRAME {&FRAME-NAME} tg-gerar-excel = NO THEN
     ASSIGN fi-arq-excel:SENSITIVE IN FRAME {&FRAME-NAME} = NO
            fi-arq-excel:SCREEN-VALUE IN FRAME f-pg-par = "".
  ELSE
     ASSIGN fi-arq-excel:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            fi-arq-excel:SCREEN-VALUE IN FRAME f-pg-par = SESSION:TEMP-DIRECTORY + 
                         "escc0005-#.csv".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-cla
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-relat 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{utp/ut9000.i "ESCC0005" "2.04.00.000"}

/*:T inicializa‡äes do template de relat¢rio */
{include/i-rpini.i}

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

   {include/i-rplbl.i}

   /* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO  ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    RUN enable_UI.
  
    ASSIGN fi-cod-estabel:SCREEN-VALUE IN FRAME f-pg-sel = "2"
           fi-periodo-ini:SCREEN-VALUE IN FRAME f-pg-sel = STRING(YEAR(TODAY) - 1) + 
                                                           STRING(MONTH(TODAY),"99").
           fi-periodo-fin:SCREEN-VALUE IN FRAME f-pg-sel = IF STRING(MONTH(TODAY)) = "01"
                                                              THEN STRING(YEAR(TODAY)) + "12" 
                                                              ELSE STRING(YEAR(TODAY) ) +  
                                                                   STRING(INT(MONTH(TODAY) - 1),"99").
    ASSIGN fi-ini-ge-compra:SCREEN-VALUE IN FRAME f-pg-sel = "1"
           fi-fin-ge-compra:SCREEN-VALUE IN FRAME f-pg-sel = "9"
           fi-ini-ge-fatur:SCREEN-VALUE IN FRAME f-pg-sel = "50"
           fi-fin-ge-fatur:SCREEN-VALUE IN FRAME f-pg-sel = "59"
           fi-ini-it-fatur:SCREEN-VALUE IN FRAME f-pg-sel = "5"
           fi-fin-it-fatur:SCREEN-VALUE IN FRAME f-pg-sel = "5ZZZZZZZZZZZZZZ". 
    
    {esinc/i-dsallrb.i nota-fiscal.esp-docto c-esp-fat}
    {esinc/i-dsallrb.i movto-estoq.esp-docto c-esp-prd}

    ASSIGN cb-esp-doc-fat:LIST-ITEMS IN FRAME f-pg-sel    = c-esp-fat
           fi-serie-fatur:SCREEN-VALUE IN FRAME f-pg-sel  = "1"
           cb-esp-doc-fat:SCREEN-VALUE IN FRAME f-pg-sel  = "NFS"
           cb-esp-doc-prd1:LIST-ITEMS IN FRAME f-pg-sel   = c-esp-prd
           cb-esp-doc-prd2:LIST-ITEMS IN FRAME f-pg-sel   = c-esp-prd
           cb-esp-doc-prd3:LIST-ITEMS IN FRAME f-pg-sel   = c-esp-prd
           cb-esp-doc-prd1:SCREEN-VALUE IN FRAME f-pg-sel = "ACA"    
           cb-esp-doc-prd2:SCREEN-VALUE IN FRAME f-pg-sel = "EAC"
           cb-esp-doc-prd3:SCREEN-VALUE IN FRAME f-pg-sel = "SOB"
           fi-arq-excel:SCREEN-VALUE IN FRAME f-pg-par    = SESSION:TEMP-DIRECTORY + 
                                                            "escc0005-#.csv".
    
    {include/i-rpmbl.i}
  
    fi-ini-ge-compra:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-fin-ge-compra:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-ini-it-compra:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-fin-it-compra:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-ini-ge-fatur:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-fin-ge-fatur:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-ini-it-fatur:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-fin-it-fatur:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    
    FIND moeda WHERE moeda.mo-codigo = INT(fi-mo-codigo:SCREEN-VALUE IN FRAME f-pg-par) 
               NO-LOCK NO-ERROR.
    IF AVAIL moeda THEN
       ASSIGN fi-desc-moeda:SCREEN-VALUE IN FRAME f-pg-par = moeda.descricao.


    IF  NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-relat  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-relat  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-relat  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-relat)
  THEN DELETE WIDGET w-relat.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-relat  _DEFAULT-ENABLE
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
  ENABLE bt-executar bt-cancelar bt-ajuda im-pg-cla im-pg-imp im-pg-par 
         im-pg-sel 
      WITH FRAME f-relat IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}
  DISPLAY fi-cod-estabel fi-nome-estabel fi-periodo-ini fi-periodo-fin 
          cb-esp-doc-prd1 cb-esp-doc-prd2 cb-esp-doc-prd3 fi-ini-ge-compra 
          fi-fin-ge-compra fi-ini-it-compra fi-fin-it-compra fi-ini-ge-fatur 
          fi-fin-ge-fatur fi-ini-it-fatur fi-fin-it-fatur fi-serie-fatur 
          cb-esp-doc-fat 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  ENABLE fi-cod-estabel fi-periodo-ini cb-esp-doc-prd1 cb-esp-doc-prd2 
         cb-esp-doc-prd3 fi-ini-ge-compra fi-fin-ge-compra fi-ini-it-compra 
         fi-fin-it-compra fi-ini-ge-fatur fi-fin-ge-fatur fi-ini-it-fatur 
         fi-fin-it-fatur fi-serie-fatur cb-esp-doc-fat IMAGE-29 IMAGE-30 
         IMAGE-31 IMAGE-32 IMAGE-67 IMAGE-68 IMAGE-69 IMAGE-70 IMAGE-71 
         IMAGE-72 RECT-41 RECT-42 RECT-43 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}
  DISPLAY rs-classif 
      WITH FRAME f-pg-cla IN WINDOW w-relat.
  ENABLE rs-classif 
      WITH FRAME f-pg-cla IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-cla}
  DISPLAY rs-destino c-arquivo rs-execucao 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  ENABLE rs-destino bt-config-impr bt-arquivo c-arquivo rs-execucao RECT-7 
         RECT-9 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}
  DISPLAY rs-data-conv fi-mo-codigo fi-desc-moeda tg-nao-confirm tg-terminada 
          tg-confirm tg-eliminada tg-em-cotacao tg-cotada tg-gerar-excel 
          fi-arq-excel to-impr-param 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  ENABLE rs-data-conv fi-mo-codigo tg-nao-confirm tg-terminada tg-confirm 
         tg-eliminada tg-em-cotacao tg-cotada fi-arq-excel to-impr-param 
         RECT-18 RECT-27 RECT-28 RECT-29 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-par}
  VIEW w-relat.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-relat 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
   RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-executar w-relat 
PROCEDURE pi-executar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define var r-tt-digita as rowid no-undo.

do on error undo, return error on stop  undo, return error:
    {include/i-rpexa.i}
    
    if input frame f-pg-imp rs-destino = 2 and
       input frame f-pg-imp rs-execucao = 1 then do:
        run utp/ut-vlarq.p (input input frame f-pg-imp c-arquivo).
        
        if return-value = "NOK":U then do:
            run utp/ut-msgs.p (input "show":U, input 73, input "").
            
            apply "MOUSE-SELECT-CLICK":U to im-pg-imp in frame f-relat.
            apply "ENTRY":U to c-arquivo in frame f-pg-imp.
            return error.
        end.
    end.
    
    /*:T Coloque aqui as valida‡äes das outras p ginas, lembrando que elas devem 
       apresentar uma mensagem de erro cadastrada, posicionar na p gina com 
       problemas e colocar o focus no campo com problemas */
    
    /*:T Aqui sÆo gravados os campos da temp-table que ser  passada como parƒmetro
       para o programa RP.P */
    
    create tt-param.
    assign tt-param.usuario         = c-seg-usuario
           tt-param.destino         = input frame f-pg-imp rs-destino
           tt-param.data-exec       = today
           tt-param.hora-exec       = TIME
           tt-param.classifica      = INPUT FRAME f-pg-cla rs-classif
           tt-param.desc-classifica = entry((tt-param.classifica - 1) * 2 + 1, 
                                      rs-classif:radio-buttons in frame f-pg-cla)
           tt-param.cod-estabel     = INPUT FRAME f-pg-sel fi-cod-estabel
           tt-param.periodo-ini     = INPUT FRAME f-pg-sel fi-periodo-ini     
           tt-param.periodo-fin     = INPUT FRAME f-pg-sel fi-periodo-fin
           tt-param.esp-docto-prd1  = LOOKUP(INPUT FRAME f-pg-sel cb-esp-doc-prd1,c-esp-prd)
           tt-param.esp-docto-prd2  = LOOKUP(INPUT FRAME f-pg-sel cb-esp-doc-prd2,c-esp-prd)
           tt-param.esp-docto-prd3  = LOOKUP(INPUT FRAME f-pg-sel cb-esp-doc-prd3,c-esp-prd)
           tt-param.ge-compra-ini   = input frame f-pg-sel fi-ini-ge-compra 
           tt-param.ge-compra-fin   = input frame f-pg-sel fi-fin-ge-compra 
           tt-param.it-compra-ini   = input frame f-pg-sel fi-ini-it-compra
           tt-param.it-compra-fin   = input frame f-pg-sel fi-fin-it-compra
           tt-param.ge-fatur-ini    = input frame f-pg-sel fi-ini-ge-fatur 
           tt-param.ge-fatur-fin    = input frame f-pg-sel fi-fin-ge-fatur 
           tt-param.it-fatur-ini    = input frame f-pg-sel fi-ini-it-fatur
           tt-param.it-fatur-fin    = input frame f-pg-sel fi-fin-it-fatur
           tt-param.serie-fatur     = INPUT FRAME f-pg-sel fi-serie-fatur
           tt-param.esp-docto-fat   = LOOKUP(INPUT FRAME f-pg-sel cb-esp-doc-fat,c-esp-fat)
           tt-param.data-conv       = INPUT FRAME f-pg-par rs-data-conv
           tt-param.desc-data-conv  = entry((tt-param.data-conv - 1) * 2 + 1, 
                                      rs-data-conv:radio-buttons in frame f-pg-par)
           tt-param.mo-codigo       = INPUT FRAME f-pg-par fi-mo-codigo
           tt-param.desc-moeda      = INPUT FRAME f-pg-par fi-desc-moeda
           tt-param.nao-confirm     = INPUT FRAME f-pg-par tg-nao-confirm    
           tt-param.confirm         = INPUT FRAME f-pg-par tg-confirm        
           tt-param.em-cotacao      = INPUT FRAME f-pg-par tg-em-cotacao     
           tt-param.terminada       = INPUT FRAME f-pg-par tg-terminada      
           tt-param.eliminada       = INPUT FRAME f-pg-par tg-eliminada      
           tt-param.cotada          = INPUT FRAME f-pg-par tg-cotada
           tt-param.gerar-excel     = INPUT FRAME f-pg-par tg-gerar-excel
           tt-param.arq-excel       = INPUT FRAME f-pg-par fi-arq-excel
           tt-param.impr-param      = INPUT FRAME f-pg-par to-impr-param.
    
    ASSIGN i-data[1] = tt-param.periodo-ini.
    DO i-cont = 2 TO 12:
       ASSIGN i-mes = INT(SUBSTR(i-data[i-cont - 1],5,2)) + 1
              i-ano = INT(SUBSTR(i-data[i-cont - 1],1,4)).
       IF i-mes > 12 THEN
          ASSIGN i-mes = 1
                 i-ano = i-ano + 1.
       ASSIGN i-data[i-cont] = STRING(i-ano,"9999") + STRING(i-mes,"99").
    END.

    DO i-cont = 1 TO EXTENT(i-data).
       ASSIGN tt-param.c-per[i-cont] = c-meses[INT(SUBSTR(i-data[i-cont],5,2))] + "/" + 
                                       SUBSTR(i-data[i-cont],1,4).
    END.

    if tt-param.destino = 1 
    then assign tt-param.arquivo = "".
    else if  tt-param.destino = 2 
         then assign tt-param.arquivo = input frame f-pg-imp c-arquivo.
         else assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp":U.
    
    /*:T Coloque aqui a l¢gica de grava‡Æo dos demais campos que devem ser passados
       como parƒmetros para o programa RP.P, atrav‚s da temp-table tt-param */
    
    /*:T Executar do programa RP.P que ir  criar o relat¢rio */
    {include/i-rpexb.i}
    
    SESSION:SET-WAIT-STATE("general":U).
    
    {include/i-rprun.i esrp/escc0005rp.p}
    
    {include/i-rpexc.i}
    
    SESSION:SET-WAIT-STATE("":U).
    
    {include/i-rptrm.i}
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-troca-pagina w-relat 
PROCEDURE pi-troca-pagina :
/*:T------------------------------------------------------------------------------
  Purpose: Gerencia a Troca de P gina (folder)   
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

{include/i-rptrp.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-relat  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this w-relat, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-relat 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
  
  run pi-trata-state (p-issuer-hdl, p-state).
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

