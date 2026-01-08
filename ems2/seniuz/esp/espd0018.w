&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          mgcad            PROGRESS
*/
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
{include/i-prgvrs.i ESPD0018 2.04.00.000}

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
&GLOBAL-DEFINE PGCLA 
&GLOBAL-DEFINE PGPAR f-pg-par
&GLOBAL-DEFINE PGDIG f-pg-dig
&GLOBAL-DEFINE PGIMP f-pg-imp
  
/* Parameters Definitions ---                                           */

/* Temporary Table Definitions ---                                      */

define temp-table tt-param no-undo
       field destino        as integer
       field arquivo        as char format "x(35)"
       field usuario        as char format "x(12)"
       field data-exec      as date
       field hora-exec      as integer
       field classifica     as integer
       FIELD grupo-ini      LIKE ITEM.ge-codigo
       FIELD grupo-fin      LIKE ITEM.ge-codigo
       FIELD item-ini       LIKE ITEM.it-codigo
       FIELD item-fin       LIKE ITEM.it-codigo
       FIELD refer-ini      LIKE ped-item.cod-refer
       FIELD refer-fin      LIKE ped-item.cod-refer
       FIELD desenho-ini    AS CHAR FORMAT "x(4)"
       FIELD desenho-fin    AS CHAR FORMAT "x(4)"
       FIELD cliente-ini    LIKE ped-venda.nome-abrev
       FIELD cliente-fin    LIKE ped-venda.nome-abrev
       FIELD repres-ini     LIKE ped-venda.no-ab-reppri 
       FIELD repres-fin     LIKE ped-venda.no-ab-reppri
       FIELD dt-entr-ini    LIKE ped-venda.dt-entrega   
       FIELD dt-entr-fin    LIKE ped-venda.dt-entrega   
       FIELD dt-impl-ini    LIKE ped-venda.dt-implant
       FIELD dt-impl-fin    LIKE ped-venda.dt-implant
       FIELD tipo-mercado   AS INTEGER
       FIELD desc-tipo-merc AS CHAR FORMAT "x(10)"
       FIELD cond-pagto     AS INTEGER
       FIELD desc-cond-pag  AS CHAR FORMAT "x(10)"
       FIELD all-types      AS LOGICAL FORMAT "Sim/NÆo"
       FIELD tp-pedido      AS CHAR FORMAT "x"
       FIELD tipo-rel       AS INTEGER
       FIELD desc-tipo-rel  AS CHAR FORMAT "x(10)"
       FIELD qualidade      AS INTEGER
       FIELD desc-qualidade AS CHAR FORMAT "x(10)"
       FIELD gerar-excel    AS LOG FORMAT "Sim/NÆo"
       FIELD arq-excel      AS CHAR FORMAT "x(45)"
       FIELD impr-param     AS LOGICAL.

define temp-table tt-digita no-undo
       field it-codigo LIKE ped-item.it-codigo
       index id it-codigo.

define buffer b-tt-digita for tt-digita.

/* Transfer Definitions */

def var raw-param        as raw no-undo.

def temp-table tt-raw-digita
   field raw-digita      as raw.
                    
/* Local Variable Definitions ---                                       */

def var l-ok               as logical no-undo.
def var c-arq-digita       as char    no-undo.
def var c-terminal         as char    no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-relat
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME f-pg-dig
&Scoped-define BROWSE-NAME br-digita

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-digita

/* Definitions for BROWSE br-digita                                     */
&Scoped-define FIELDS-IN-QUERY-br-digita tt-digita.it-codigo   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-digita tt-digita.it-codigo   
&Scoped-define ENABLED-TABLES-IN-QUERY-br-digita tt-digita
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br-digita tt-digita
&Scoped-define SELF-NAME br-digita
&Scoped-define QUERY-STRING-br-digita FOR EACH tt-digita
&Scoped-define OPEN-QUERY-br-digita OPEN QUERY br-digita FOR EACH tt-digita.
&Scoped-define TABLES-IN-QUERY-br-digita tt-digita
&Scoped-define FIRST-TABLE-IN-QUERY-br-digita tt-digita


/* Definitions for FRAME f-pg-dig                                       */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-pg-dig ~
    ~{&OPEN-QUERY-br-digita}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-digita bt-inserir bt-recuperar 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-relat AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-alterar 
     LABEL "Alterar" 
     SIZE 15 BY 1
     FONT 1.

DEFINE BUTTON bt-inserir 
     LABEL "Inserir" 
     SIZE 15 BY 1
     FONT 1.

DEFINE BUTTON bt-recuperar 
     LABEL "Recuperar" 
     SIZE 15 BY 1
     FONT 1.

DEFINE BUTTON bt-retirar 
     LABEL "Retirar" 
     SIZE 15 BY 1
     FONT 1.

DEFINE BUTTON bt-salvar 
     LABEL "Salvar" 
     SIZE 15 BY 1
     FONT 1.

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

DEFINE VARIABLE fi-tp-pedido AS CHARACTER FORMAT "x(2)" 
     VIEW-AS FILL-IN 
     SIZE 4.29 BY .79
     FONT 1 NO-UNDO.

DEFINE VARIABLE rs-cond-pagto AS INTEGER INITIAL 3 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "· Vista", 1,
"A Prazo", 2,
"Todos", 3
     SIZE 33 BY 1 TOOLTIP "Condi‡äes de pagamento." NO-UNDO.

DEFINE VARIABLE rs-qualidade AS INTEGER INITIAL 3 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Primeira", 1,
"Segunda", 2,
"Todas", 3
     SIZE 33 BY 1 TOOLTIP "Qualidade dos itens." NO-UNDO.

DEFINE VARIABLE rs-tipo-mercado AS INTEGER INITIAL 3 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Interno", 1,
"Externo", 2,
"Todos", 3
     SIZE 33 BY 1 TOOLTIP "Tipo de mercado." NO-UNDO.

DEFINE VARIABLE rs-tipo-rel AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Resumido", 1,
"Detalhado", 2
     SIZE 20.29 BY .88 TOOLTIP "Tipo do relat¢rio." NO-UNDO.

DEFINE RECTANGLE RECT-27
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 70 BY 1.25.

DEFINE RECTANGLE RECT-29
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 70 BY 1.58.

DEFINE RECTANGLE RECT-39
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 70 BY 5.46.

DEFINE VARIABLE tg-all-types AS LOGICAL INITIAL yes 
     LABEL "Todos os Tipos" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.43 BY .83 TOOLTIP "Todos os tipos de pedido, ou selecione um tipo." NO-UNDO.

DEFINE VARIABLE tg-gerar-excel AS LOGICAL INITIAL no 
     LABEL "Gerar Planilha Excel" 
     VIEW-AS TOGGLE-BOX
     SIZE 18.14 BY .88 TOOLTIP "Imprimir Parƒmetros/Sele‡Æo ao final do relat¢rio."
     FONT 1 NO-UNDO.

DEFINE VARIABLE to-impr-param AS LOGICAL INITIAL no 
     LABEL "Imprimir Parƒmetros/Sele‡Æo" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .83 TOOLTIP "Imprimir Parƒmetros/Sele‡Æo ao final do relat¢rio."
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-fin-cod-desenho AS CHARACTER FORMAT "x(4)" INITIAL "ZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 TOOLTIP "Desenho final" NO-UNDO.

DEFINE VARIABLE fi-fin-cod-refer AS CHARACTER FORMAT "x(8)" INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "Referˆncia final" NO-UNDO.

DEFINE VARIABLE fi-fin-dt-entrega AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fin-dt-implant AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fin-ge-codigo AS INTEGER FORMAT ">9":U INITIAL 99 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fin-it-codigo AS CHARACTER FORMAT "X(16)" INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fin-no-ab-reppri AS CHARACTER FORMAT "X(12)" INITIAL "ZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fin-nome-abrev AS CHARACTER FORMAT "X(12)" INITIAL "ZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-cod-desenho AS CHARACTER FORMAT "x(4)" 
     LABEL "Desenho":R12 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 TOOLTIP "Desenho inicial" NO-UNDO.

DEFINE VARIABLE fi-ini-cod-refer AS CHARACTER FORMAT "x(8)" 
     LABEL "Referˆncia":R12 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "Referˆncia inicial" NO-UNDO.

DEFINE VARIABLE fi-ini-dt-entrega AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Entrega" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-dt-implant AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Implanta‡Æo" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-ge-codigo AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Grupo de Estoque" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-it-codigo AS CHARACTER FORMAT "X(16)" 
     LABEL "Item":R8 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-no-ab-reppri AS CHARACTER FORMAT "X(12)":U 
     LABEL "Representante" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-nome-abrev AS CHARACTER FORMAT "X(12)" 
     LABEL "Cliente":R17 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-10
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-19
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-20
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-21
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-22
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-27
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-28
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-29
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-3
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-30
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-4
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-5
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-6
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-7
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-8
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-9
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

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

DEFINE IMAGE im-pg-dig
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

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-digita FOR 
      tt-digita SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-digita w-relat _FREEFORM
  QUERY br-digita DISPLAY
      tt-digita.it-codigo LABEL "ITEM"
ENABLE
tt-digita.it-codigo
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 76.57 BY 9
         BGCOLOR 15 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-relat
     bt-executar AT ROW 14.54 COL 3 HELP
          "Dispara a execu‡Æo do relat¢rio"
     bt-cancelar AT ROW 14.54 COL 14 HELP
          "Fechar"
     bt-ajuda AT ROW 14.54 COL 70 HELP
          "Ajuda"
     RECT-1 AT ROW 14.29 COL 2
     rt-folder-right AT ROW 2.67 COL 80.43
     rt-folder AT ROW 2.5 COL 2
     rt-folder-top AT ROW 2.54 COL 2.14
     rt-folder-left AT ROW 2.54 COL 2.14
     RECT-6 AT ROW 13.75 COL 2.14
     im-pg-dig AT ROW 1.5 COL 33.43
     im-pg-imp AT ROW 1.5 COL 49.14
     im-pg-par AT ROW 1.5 COL 17.72
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
         SIZE 73.72 BY 10.

DEFINE FRAME f-pg-dig
     br-digita AT ROW 1 COL 1
     bt-inserir AT ROW 10 COL 1
     bt-alterar AT ROW 10 COL 16
     bt-retirar AT ROW 10 COL 31
     bt-salvar AT ROW 10 COL 46
     bt-recuperar AT ROW 10 COL 61
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3.31
         SIZE 76.86 BY 10.15.

DEFINE FRAME f-pg-sel
     fi-ini-ge-codigo AT ROW 2 COL 19 COLON-ALIGNED
     fi-fin-ge-codigo AT ROW 2 COL 48 COLON-ALIGNED NO-LABEL
     fi-ini-it-codigo AT ROW 3 COL 19 COLON-ALIGNED
     fi-fin-it-codigo AT ROW 3 COL 48 COLON-ALIGNED NO-LABEL
     fi-ini-cod-refer AT ROW 4 COL 19 COLON-ALIGNED HELP
          "C¢digo de referˆncia do item"
     fi-fin-cod-refer AT ROW 4 COL 48 COLON-ALIGNED HELP
          "C¢digo de referˆncia do item" NO-LABEL
     fi-ini-cod-desenho AT ROW 5 COL 19 COLON-ALIGNED HELP
          "C¢digo de referˆncia do item"
     fi-fin-cod-desenho AT ROW 5 COL 48 COLON-ALIGNED HELP
          "C¢digo de referˆncia do item" NO-LABEL
     fi-ini-nome-abrev AT ROW 6 COL 19 COLON-ALIGNED HELP
          "Nome abreviado do cliente/fornecedor"
     fi-fin-nome-abrev AT ROW 6 COL 48 COLON-ALIGNED HELP
          "Nome abreviado do cliente/fornecedor" NO-LABEL
     fi-ini-no-ab-reppri AT ROW 7 COL 19 COLON-ALIGNED
     fi-fin-no-ab-reppri AT ROW 7 COL 48 COLON-ALIGNED HELP
          "Nome abreviado do representante principal" NO-LABEL
     fi-ini-dt-entrega AT ROW 8 COL 19 COLON-ALIGNED
     fi-fin-dt-entrega AT ROW 8 COL 48 COLON-ALIGNED NO-LABEL
     fi-ini-dt-implant AT ROW 9 COL 19 COLON-ALIGNED
     fi-fin-dt-implant AT ROW 9 COL 48 COLON-ALIGNED NO-LABEL
     IMAGE-10 AT ROW 2 COL 46.57
     IMAGE-19 AT ROW 4 COL 39
     IMAGE-20 AT ROW 4 COL 46.57
     IMAGE-21 AT ROW 5 COL 39
     IMAGE-22 AT ROW 5 COL 46.57
     IMAGE-27 AT ROW 6 COL 39
     IMAGE-28 AT ROW 6 COL 46.57
     IMAGE-29 AT ROW 7 COL 39
     IMAGE-3 AT ROW 9 COL 39
     IMAGE-30 AT ROW 7 COL 46.57
     IMAGE-4 AT ROW 9 COL 46.57
     IMAGE-5 AT ROW 8 COL 39
     IMAGE-6 AT ROW 8 COL 46.57
     IMAGE-7 AT ROW 3 COL 39
     IMAGE-8 AT ROW 3 COL 46.57
     IMAGE-9 AT ROW 2 COL 39
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.85
         SIZE 76.86 BY 10.62
         FONT 1.

DEFINE FRAME f-pg-par
     rs-tipo-mercado AT ROW 2.17 COL 27 NO-LABEL
     rs-cond-pagto AT ROW 3.13 COL 27 NO-LABEL
     tg-all-types AT ROW 4.25 COL 27.14
     fi-tp-pedido AT ROW 4.29 COL 54.29 COLON-ALIGNED HELP
          "Dispon¡vel para classifica‡Æo/indica‡Æo pr¢pria do usu rio" NO-LABEL
     rs-qualidade AT ROW 5.04 COL 26.86 NO-LABEL
     rs-tipo-rel AT ROW 6.04 COL 26.86 NO-LABEL
     to-impr-param AT ROW 7.88 COL 29.86
     tg-gerar-excel AT ROW 9.67 COL 7.86
     fi-arq-excel AT ROW 9.67 COL 32.72 COLON-ALIGNED
     RECT-27 AT ROW 7.67 COL 4.29
     RECT-29 AT ROW 9.33 COL 4.29
     RECT-39 AT ROW 1.79 COL 4.29
     "Tipos de Mercado:" VIEW-AS TEXT
          SIZE 13 BY .75 AT ROW 2.25 COL 11.57
     "Tipos de Pedidos:" VIEW-AS TEXT
          SIZE 12 BY .75 TOOLTIP "Tipos de pedidos" AT ROW 4.33 COL 12.29
     "Pedidos do Tipo:" VIEW-AS TEXT
          SIZE 12 BY .75 TOOLTIP "Tipo do pedido a ser considerado, ou marque Todos os tipos." AT ROW 4.29 COL 44.29
     " Condi‡äes de Pagamento:" VIEW-AS TEXT
          SIZE 18.57 BY .75 AT ROW 3.25 COL 6.14
     "Tipo do Relat¢rio:" VIEW-AS TEXT
          SIZE 12.29 BY .88 AT ROW 6 COL 12.29
     "Qualidade:" VIEW-AS TEXT
          SIZE 7.86 BY .75 AT ROW 5.17 COL 16.86
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 75 BY 10.42
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
         TITLE              = "Pedidos de Vendas com Pre‡os por Item"
         HEIGHT             = 15
         WIDTH              = 81.14
         MAX-HEIGHT         = 22.33
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 22.33
         VIRTUAL-WIDTH      = 114.29
         RESIZE             = yes
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
/* SETTINGS FOR FRAME f-pg-dig
                                                                        */
/* BROWSE-TAB br-digita 1 f-pg-dig */
/* SETTINGS FOR BUTTON bt-alterar IN FRAME f-pg-dig
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-retirar IN FRAME f-pg-dig
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-salvar IN FRAME f-pg-dig
   NO-ENABLE                                                            */
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
/* SETTINGS FOR FILL-IN fi-arq-excel IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tp-pedido IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME f-pg-sel
   Custom                                                               */
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

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-digita
/* Query rebuild information for BROWSE br-digita
     _START_FREEFORM
OPEN QUERY br-digita FOR EACH tt-digita.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-digita */
&ANALYZE-RESUME

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
ON END-ERROR OF w-relat /* Pedidos de Vendas com Pre‡os por Item */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON WINDOW-CLOSE OF w-relat /* Pedidos de Vendas com Pre‡os por Item */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-digita
&Scoped-define SELF-NAME br-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON DEL OF br-digita IN FRAME f-pg-dig
DO:
   apply 'choose':U to bt-retirar in frame f-pg-dig.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON END-ERROR OF br-digita IN FRAME f-pg-dig
ANYWHERE 
DO:
    if  br-digita:new-row in frame f-pg-dig then do:
        if  avail tt-digita then
            delete tt-digita.
        if  br-digita:delete-current-row() in frame f-pg-dig then. 
    end.                                                               
    else do:
        get current br-digita.
        display tt-digita.it-codigo
                with browse br-digita. 
    end.
    return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON ENTER OF br-digita IN FRAME f-pg-dig
ANYWHERE
DO:
  apply 'tab':U to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON INS OF br-digita IN FRAME f-pg-dig
DO:
   apply 'choose':U to bt-inserir in frame f-pg-dig.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON OFF-END OF br-digita IN FRAME f-pg-dig
DO:
   apply 'entry':U to bt-inserir in frame f-pg-dig.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON OFF-HOME OF br-digita IN FRAME f-pg-dig
DO:
  apply 'entry':U to bt-recuperar in frame f-pg-dig.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON ROW-ENTRY OF br-digita IN FRAME f-pg-dig
DO:
   /*:T trigger para inicializar campos da temp table de digita‡Æo */
   if  br-digita:new-row in frame f-pg-dig then do:
       assign tt-digita.it-codigo:SCREEN-VALUE IN BROWSE br-digita = "".
   end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON ROW-LEAVE OF br-digita IN FRAME f-pg-dig
DO:
    /*:T  aqui que a grava‡Æo da linha da temp-table ‚ efetivada.
       Por‚m as valida‡äes dos registros devem ser feitas na procedure pi-executar,
       no local indicado pelo coment rio */
    
    if br-digita:NEW-ROW in frame f-pg-dig then 
    do transaction on error undo, return no-apply:
        create tt-digita.
        assign input browse br-digita tt-digita.it-codigo.
    
        br-digita:CREATE-RESULT-LIST-ENTRY() in frame f-pg-dig.
    end.
    else do transaction on error undo, return no-apply:
        assign input browse br-digita tt-digita.it-codigo.
    end.
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


&Scoped-define FRAME-NAME f-pg-dig
&Scoped-define SELF-NAME bt-alterar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-alterar w-relat
ON CHOOSE OF bt-alterar IN FRAME f-pg-dig /* Alterar */
DO:
   apply 'entry':U to tt-digita.it-codigo in browse br-digita. 
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


&Scoped-define FRAME-NAME f-pg-dig
&Scoped-define SELF-NAME bt-inserir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-inserir w-relat
ON CHOOSE OF bt-inserir IN FRAME f-pg-dig /* Inserir */
DO:
    assign bt-alterar:SENSITIVE in frame f-pg-dig = yes
           bt-retirar:SENSITIVE in frame f-pg-dig = yes
           bt-salvar:SENSITIVE in frame f-pg-dig  = yes.
    
    if num-results("br-digita":U) > 0 then
        br-digita:INSERT-ROW("after":U) in frame f-pg-dig.
    else do transaction:
        create tt-digita.
        
        assign tt-digita.it-codigo = "".

        open query br-digita for each tt-digita.
        
        apply "entry":U to tt-digita.it-codigo in browse br-digita. 
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-recuperar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-recuperar w-relat
ON CHOOSE OF bt-recuperar IN FRAME f-pg-dig /* Recuperar */
DO:
    {include/i-rprcd.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-retirar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-retirar w-relat
ON CHOOSE OF bt-retirar IN FRAME f-pg-dig /* Retirar */
DO:
    if  br-digita:num-selected-rows > 0 then do on error undo, return no-apply:
        get current br-digita.
        delete tt-digita.
        if  br-digita:delete-current-row() in frame f-pg-dig then.
    end.
    
    if num-results("br-digita":U) = 0 then
        assign bt-alterar:SENSITIVE in frame f-pg-dig = no
               bt-retirar:SENSITIVE in frame f-pg-dig = no
               bt-salvar:SENSITIVE in frame f-pg-dig  = no.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-salvar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-salvar w-relat
ON CHOOSE OF bt-salvar IN FRAME f-pg-dig /* Salvar */
DO:
   {include/i-rpsvd.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME fi-arq-excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-arq-excel w-relat
ON LEAVE OF fi-arq-excel IN FRAME f-pg-par /* Arquivo */
DO:
  IF NOT fi-arq-excel:SCREEN-VALUE IN FRAME {&FRAME-NAME} MATCHES "*.xls*" THEN DO:
     MESSAGE "Nome do arquivo para Excel est  inv lido." SKIP
             "Deve ser do tipo Caminho/Arquivo.xls" VIEW-AS ALERT-BOX.
     ASSIGN fi-arq-excel:SCREEN-VALUE IN FRAME f-pg-par = SESSION:TEMP-DIRECTORY + 
                         "espd0018.xls".
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-sel
&Scoped-define SELF-NAME fi-fin-ge-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fin-ge-codigo w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-fin-ge-codigo IN FRAME f-pg-sel
DO:
    {include/zoomvar.i &prog-zoom = inzoom/z01in142.w
                       &campo     = fi-fin-ge-codigo
                       &campozoom = ge-codigo
                       &FRAME     = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-fin-it-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fin-it-codigo w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-fin-it-codigo IN FRAME f-pg-sel
DO:
    {include/zoomvar.i &prog-zoom = inzoom/z01in172.w
                     &campo       = fi-fin-it-codigo
                     &campozoom   = it-codigo
                     &FRAME       = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-fin-no-ab-reppri
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fin-no-ab-reppri w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-fin-no-ab-reppri IN FRAME f-pg-sel
DO:
   {include/zoomvar.i &prog-zoom = adzoom/z01ad229.w
                      &campo     = fi-fin-no-ab-reppri
                      &campozoom = nome-abrev
                      &FRAME     = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-fin-nome-abrev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fin-nome-abrev w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-fin-nome-abrev IN FRAME f-pg-sel
DO:
   {include/zoomvar.i &prog-zoom = adzoom/z02ad098.w
                      &campo     = fi-fin-nome-abrev
                      &campozoom = nome-abrev
                      &FRAME     = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ini-ge-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-ge-codigo w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ini-ge-codigo IN FRAME f-pg-sel /* Grupo de Estoque */
DO:
    {include/zoomvar.i &prog-zoom = inzoom/z01in142.w
                       &campo     = fi-ini-ge-codigo
                       &campozoom = ge-codigo
                       &FRAME     = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ini-it-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-it-codigo w-relat
ON LEAVE OF fi-ini-it-codigo IN FRAME f-pg-sel /* Item */
DO:
  ASSIGN fi-fin-it-codigo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 
                                    INPUT FRAME {&frame-name} fi-ini-it-codigo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-it-codigo w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ini-it-codigo IN FRAME f-pg-sel /* Item */
DO:
    {include/zoomvar.i &prog-zoom = inzoom/z01in172.w
                       &campo     = fi-ini-it-codigo
                       &campozoom = it-codigo
                       &FRAME     = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ini-no-ab-reppri
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-no-ab-reppri w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ini-no-ab-reppri IN FRAME f-pg-sel /* Representante */
DO:
   {include/zoomvar.i &prog-zoom = adzoom/z01ad229.w
                      &campo     = fi-ini-no-ab-reppri
                      &campozoom = nome-abrev
                      &FRAME     = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ini-nome-abrev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-nome-abrev w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ini-nome-abrev IN FRAME f-pg-sel /* Cliente */
DO:
   {include/zoomvar.i &prog-zoom = adzoom/z02ad098.w
                      &campo     = fi-ini-nome-abrev
                      &campozoom = nome-abrev
                      &FRAME     = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME im-pg-dig
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-dig w-relat
ON MOUSE-SELECT-CLICK OF im-pg-dig IN FRAME f-relat
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
&Scoped-define SELF-NAME tg-all-types
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-all-types w-relat
ON VALUE-CHANGED OF tg-all-types IN FRAME f-pg-par /* Todos os Tipos */
DO:
  IF INPUT FRAME {&FRAME-NAME} tg-all-types THEN 
     ASSIGN fi-tp-pedido:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
            fi-tp-pedido:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
  ELSE DO.
     ASSIGN fi-tp-pedido:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
     APPLY 'entry' TO fi-tp-pedido IN FRAME {&FRAME-NAME}.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
                         "espd0018.xls".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-dig
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-relat 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{utp/ut9000.i "ESPD0018" "2.04.00.000"}

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

    ASSIGN fi-ini-ge-codigo:SCREEN-VALUE IN FRAME f-pg-sel = "50"
           fi-fin-ge-codigo:SCREEN-VALUE IN FRAME f-pg-sel = "59"
           fi-ini-it-codigo:SCREEN-VALUE IN FRAME f-pg-sel = "5"
           fi-fin-it-codigo:SCREEN-VALUE IN FRAME f-pg-sel = "5ZZZZZZZZZZZZZZZ"
           fi-ini-dt-implant:SCREEN-VALUE IN FRAME f-pg-sel = STRING(TODAY - 90)
           fi-fin-dt-implant:SCREEN-VALUE IN FRAME f-pg-sel = STRING(TODAY).

    {include/i-rpmbl.i}

    fi-ini-ge-codigo:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-fin-ge-codigo:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-ini-it-codigo:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-fin-it-codigo:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-ini-nome-abrev:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-fin-nome-abrev:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-ini-no-ab-reppri:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-fin-no-ab-reppri:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.

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
  ENABLE bt-executar bt-cancelar bt-ajuda im-pg-dig im-pg-imp im-pg-par 
         im-pg-sel 
      WITH FRAME f-relat IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}
  DISPLAY fi-ini-ge-codigo fi-fin-ge-codigo fi-ini-it-codigo fi-fin-it-codigo 
          fi-ini-cod-refer fi-fin-cod-refer fi-ini-cod-desenho 
          fi-fin-cod-desenho fi-ini-nome-abrev fi-fin-nome-abrev 
          fi-ini-no-ab-reppri fi-fin-no-ab-reppri fi-ini-dt-entrega 
          fi-fin-dt-entrega fi-ini-dt-implant fi-fin-dt-implant 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  ENABLE fi-ini-ge-codigo fi-fin-ge-codigo fi-ini-it-codigo fi-fin-it-codigo 
         fi-ini-cod-refer fi-fin-cod-refer fi-ini-cod-desenho 
         fi-fin-cod-desenho fi-ini-nome-abrev fi-fin-nome-abrev 
         fi-ini-no-ab-reppri fi-fin-no-ab-reppri fi-ini-dt-entrega 
         fi-fin-dt-entrega fi-ini-dt-implant fi-fin-dt-implant IMAGE-10 
         IMAGE-19 IMAGE-20 IMAGE-21 IMAGE-22 IMAGE-27 IMAGE-28 IMAGE-29 IMAGE-3 
         IMAGE-30 IMAGE-4 IMAGE-5 IMAGE-6 IMAGE-7 IMAGE-8 IMAGE-9 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}
  DISPLAY rs-destino c-arquivo rs-execucao 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  ENABLE rs-destino bt-config-impr bt-arquivo c-arquivo rs-execucao RECT-7 
         RECT-9 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}
  DISPLAY rs-tipo-mercado rs-cond-pagto tg-all-types fi-tp-pedido rs-qualidade 
          rs-tipo-rel to-impr-param tg-gerar-excel fi-arq-excel 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  ENABLE rs-tipo-mercado rs-cond-pagto tg-all-types rs-qualidade rs-tipo-rel 
         to-impr-param tg-gerar-excel RECT-27 RECT-29 RECT-39 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-par}
  ENABLE br-digita bt-inserir bt-recuperar 
      WITH FRAME f-pg-dig IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-dig}
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
    
    /*:T Coloque aqui as valida‡äes da p gina de Digita‡Æo, lembrando que elas devem
       apresentar uma mensagem de erro cadastrada, posicionar nesta p gina e colocar
       o focus no campo com problemas */
    /*browse br-digita:SET-REPOSITIONED-ROW (browse br-digita:DOWN, "ALWAYS":U).*/
    
    for each tt-digita no-lock:
        assign r-tt-digita = rowid(tt-digita).
        
        /*:T Valida‡Æo de duplicidade de registro na temp-table tt-digita */
        find first b-tt-digita where b-tt-digita.it-codigo = tt-digita.it-codigo
                                 AND rowid(b-tt-digita) <> rowid(tt-digita) no-lock no-error.
        if avail b-tt-digita then do:
            apply "MOUSE-SELECT-CLICK":U to im-pg-dig in frame f-relat.
            reposition br-digita to rowid rowid(b-tt-digita).
            
            run utp/ut-msgs.p (input "show":U, input 108, input "").
            apply "ENTRY":U to tt-digita.it-codigo in browse br-digita.

            return error.
        end.
        
        /*:T Valida‡Æo de registro da temp-table tt-digita x sele‡Æo */
        IF tt-digita.it-codigo < INPUT FRAME f-pg-sel fi-ini-it-codigo OR
           tt-digita.it-codigo > INPUT FRAME f-pg-sel fi-fin-it-codigo THEN DO:
           
           MESSAGE "Item:" tt-digita.it-codigo " est  fora da faixa definida na Sele‡Æo."
                    VIEW-AS ALERT-BOX.
           
           apply "MOUSE-SELECT-CLICK":U to im-pg-dig in frame f-relat.
           reposition br-digita to rowid r-tt-digita.
           apply "ENTRY":U to tt-digita.it-codigo in browse br-digita.
           return NO-APPLY.
        end.
    
        /*:T Valida‡Æo de registro da temp-table tt-digita x item */
        find ITEM where ITEM.it-codigo = tt-digita.it-codigo
                  no-lock no-error.
        if NOT avail item then do:
            MESSAGE "Item:" tt-digita.it-codigo "- NÆo cadastrado."
                    VIEW-AS ALERT-BOX.
            apply "ENTRY":U to tt-digita.it-codigo in browse br-digita.
            return error.
        end.
    end.
    
    /*:T Coloque aqui as valida‡äes das outras p ginas, lembrando que elas devem 
       apresentar uma mensagem de erro cadastrada, posicionar na p gina com 
       problemas e colocar o focus no campo com problemas */
    
    
    
    /*:T Aqui sÆo gravados os campos da temp-table que ser  passada como parƒmetro
       para o programa RP.P */
    
    create tt-param.

    assign tt-param.usuario        = c-seg-usuario
           tt-param.destino        = input frame f-pg-imp rs-destino
           tt-param.data-exec      = today
           tt-param.hora-exec      = TIME
           tt-param.repres-ini     = input frame f-pg-sel fi-ini-no-ab-reppri
           tt-param.repres-fin     = input frame f-pg-sel fi-fin-no-ab-reppri
           tt-param.cliente-ini    = input frame f-pg-sel fi-ini-nome-abrev
           tt-param.cliente-fin    = input frame f-pg-sel fi-fin-nome-abrev
           tt-param.grupo-ini      = input frame f-pg-sel fi-ini-ge-codigo
           tt-param.grupo-fin      = input frame f-pg-sel fi-fin-ge-codigo
           tt-param.item-ini       = input frame f-pg-sel fi-ini-it-codigo 
           tt-param.item-fin       = input frame f-pg-sel fi-fin-it-codigo 
           tt-param.refer-ini      = input frame f-pg-sel fi-ini-cod-refer
           tt-param.refer-fin      = input frame f-pg-sel fi-fin-cod-refer
           tt-param.desenho-ini    = INPUT FRAME f-pg-sel fi-ini-cod-desenho
           tt-param.desenho-fin    = INPUT FRAME f-pg-sel fi-fin-cod-desenho
           tt-param.dt-entr-ini    = input frame f-pg-sel fi-ini-dt-entrega 
           tt-param.dt-entr-fin    = input frame f-pg-sel fi-fin-dt-entrega 
           tt-param.dt-impl-ini    = input frame f-pg-sel fi-ini-dt-implant 
           tt-param.dt-impl-fin    = input frame f-pg-sel fi-fin-dt-implant 
           tt-param.tipo-mercado   = INPUT FRAME f-pg-par rs-tipo-mercado
           tt-param.desc-tipo-merc = entry((tt-param.tipo-mercado - 1) * 2 + 1, 
                                     rs-tipo-mercado:radio-buttons in frame f-pg-par)
           tt-param.cond-pagto     = INPUT FRAME f-pg-par rs-cond-pagto
           tt-param.desc-cond-pag  = entry((tt-param.cond-pagto - 1) * 2 + 1, 
                                     rs-cond-pagto:radio-buttons in frame f-pg-par)
           tt-param.all-types      = INPUT FRAME f-pg-par tg-all-types
           tt-param.tp-pedido      = INPUT FRAME f-pg-par fi-tp-pedido
           tt-param.tipo-rel       = INPUT FRAME f-pg-par rs-tipo-rel
           tt-param.desc-tipo-rel  = entry((tt-param.tipo-rel - 1) * 2 + 1, 
                                     rs-tipo-rel:radio-buttons in frame f-pg-par)
           tt-param.qualidade      = INPUT FRAME f-pg-par rs-qualidade
           tt-param.desc-qualidade = entry((tt-param.qualidade - 1) * 2 + 1, 
                                     rs-qualidade:radio-buttons in frame f-pg-par)
           tt-param.gerar-excel    = INPUT FRAME f-pg-par tg-gerar-excel
           tt-param.arq-excel      = INPUT FRAME f-pg-par fi-arq-excel
           tt-param.impr-param     = INPUT FRAME f-pg-par to-impr-param.

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
    
    {include/i-rprun.i esrp/espd0018rp.p}
    
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

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-digita"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

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

