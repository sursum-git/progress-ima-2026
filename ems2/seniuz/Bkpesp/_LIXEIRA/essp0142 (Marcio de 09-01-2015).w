&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME w-relat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-relat 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESSP0142 2.04.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Preprocessadores do Template de Relat¢rio                            */
/* Obs: Retirar o valor do preprocessador para as p ginas que nÆo existirem  */

&GLOBAL-DEFINE PGSEL f-pg-sel
&GLOBAL-DEFINE PGCLA 
&GLOBAL-DEFINE PGPAR f-pg-par
&GLOBAL-DEFINE PGDIG 
&GLOBAL-DEFINE PGIMP f-pg-imp
  
/* Parameters Definitions ---                                           */

/* Temporary Table Definitions ---                                      */

DEFINE TEMP-TABLE tt-param   NO-UNDO
       FIELD destino            AS INTEGER
       FIELD arquivo            AS CHAR FORMAT "x(35)"
       FIELD usuario            AS CHAR FORMAT "x(12)"
       FIELD data-exec          AS DATE
       FIELD hora-exec          AS INTEGER
       FIELD classifica         AS INTEGER
       FIELD desc-classifica    AS CHAR FORMAT "x(40)"
       FIELD cod-estabel        LIKE saldo-estoq.cod-estabel
       FIELD it-codigo-ini      LIKE saldo-estoq.it-codigo
       FIELD it-codigo-fin      LIKE saldo-estoq.it-codigo
       FIELD cod-refer-ini      LIKE saldo-estoq.cod-refer 
       FIELD cod-refer-fin      LIKE saldo-estoq.cod-refer
       FIELD cod-depos-ini      LIKE saldo-estoq.cod-depos 
       FIELD cod-depos-fin      LIKE saldo-estoq.cod-depos
       FIELD lote-ini           AS CHAR FORMAT "x(2)"       
       FIELD lote-fin           AS CHAR FORMAT "x(2)" 
       FIELD corte-comerc-ini   LIKE corte-comerc.codigo 
       FIELD corte-comerc-fin   LIKE corte-comerc.codigo
       FIELD cod-obsoleto-ini   AS CHAR
       FIELD cod-obsoleto-fin   AS CHAR
       FIELD i-opc-acab         AS INT
       FIELD c-tipo-artigo      AS CHAR
       FIELD l-imp              AS LOG
       FIELD l-prod             as LOG
       FIELD l-est              AS LOG 
       FIELD l-res              AS LOG 
       FIELD l-fat              AS LOG       
       FIELD l-reproc           AS LOG
       FIELD l-corte            AS LOG
       FIELD l-bloq             AS LOG
       FIELD l-cons             AS LOG
       FIELD tipo-relatorio     AS INTEGER
       FIELD item-dif           AS LOG
       FIELD item-sem-est       AS LOG
       FIELD desc-tipo-relat    AS CHAR FORMAT "x(20)"
       FIELD imp-param          AS LOG.

define temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id ordem.

define buffer b-tt-digita for tt-digita.

/* Transfer Definitions */
DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.
def var raw-param        as raw no-undo.

def temp-table tt-raw-digita
   field raw-digita      as raw.
                    
/* Local Variable Definitions ---  */  
/*                                   
DEF INPUT PARAMETER p-it-codigo AS CHAR NO-UNDO.
DEF INPUT PARAMETER p-cod-refer AS CHAR NO-UNDO.
DEF INPUT PARAMETER p-lote      AS CHAR NO-UNDO.
*/

DEF {1} VAR c-it-codigo AS CHAR.
DEF {1} VAR c-cod-refer AS CHAR.
DEF {1} VAR c-lote      AS CHAR.
DEF {1} VAR l-imp       AS LOG.
DEF {1} VAR l-prod      AS LOG.
DEF {1} VAR l-est       AS LOG.
DEF {1} VAR l-res       AS LOG.
DEF {1} VAR l-fat       AS LOG.
DEF {1} VAR l-reproc    AS LOG.

def var l-ok               as logical no-undo.
def var c-arq-digita       as char    no-undo.
def var c-terminal         as char    no-undo.
DEF VAR da-aux             AS DATE.
DEF VAR c-cod-estabel      AS CHAR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-relat
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-pg-imp

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-7 RECT-9 rs-destino bt-arquivo ~
bt-config-impr c-arquivo rs-execucao 
&Scoped-Define DISPLAYED-OBJECTS rs-destino c-arquivo rs-execucao 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-relat AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
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

DEFINE VARIABLE rs-tipo-relatorio AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Sint‚tico", 1,
"Anal¡tico", 2
     SIZE 27 BY 1.17 TOOLTIP "Sele‡Æo do Tipo do Relatorio" NO-UNDO.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 66.86 BY 1.75.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 67 BY 3.54.

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 67 BY 3.

DEFINE VARIABLE tg-sem-est AS LOGICAL INITIAL no 
     LABEL "Somente Itens SEM Estoque e COM Etiquetas" 
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY .88 TOOLTIP "Imprimir Somente os Itens com Diferen‡a no Estoque" NO-UNDO.

DEFINE VARIABLE to-bloq AS LOGICAL INITIAL yes 
     LABEL "Bloqueado" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE to-cons AS LOGICAL INITIAL no 
     LABEL "Consumido" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE to-corte AS LOGICAL INITIAL no 
     LABEL "Consumido Corte" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .83 NO-UNDO.

DEFINE VARIABLE to-est1 AS LOGICAL INITIAL yes 
     LABEL "Em Estoque" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.14 BY .88 TOOLTIP "Etiqueta no Estoque" NO-UNDO.

DEFINE VARIABLE to-fat1 AS LOGICAL INITIAL no 
     LABEL "Faturada" 
     VIEW-AS TOGGLE-BOX
     SIZE 9.57 BY .88 TOOLTIP "Etiqueta Faturada" NO-UNDO.

DEFINE VARIABLE to-imp-param AS LOGICAL INITIAL no 
     LABEL "Imprimir Parƒmetros/Sele‡Æo" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .88 TOOLTIP "Imprimir Parƒmetros/Sele‡Æo ao final do relat¢rio."
     FONT 1 NO-UNDO.

DEFINE VARIABLE to-imp1 AS LOGICAL INITIAL no 
     LABEL "Impressa" 
     VIEW-AS TOGGLE-BOX
     SIZE 9.14 BY .88 TOOLTIP "Etiqueta Impressa" NO-UNDO.

DEFINE VARIABLE to-item-dif AS LOGICAL INITIAL no 
     LABEL "Somente Itens com diferen‡a estoque" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .88 TOOLTIP "Imprimir Somente os Itens com Diferen‡a no Estoque" NO-UNDO.

DEFINE VARIABLE to-prod1 AS LOGICAL INITIAL no 
     LABEL "Em Produ‡Æo" 
     VIEW-AS TOGGLE-BOX
     SIZE 12.14 BY .88 TOOLTIP "Etiqueta em Produ‡Æo" NO-UNDO.

DEFINE VARIABLE to-reproc1 AS LOGICAL INITIAL no 
     LABEL "Em Reprocesso" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.57 BY .88 TOOLTIP "Etiqueta em Reprocesso" NO-UNDO.

DEFINE VARIABLE to-res1 AS LOGICAL INITIAL yes 
     LABEL "Reservada" 
     VIEW-AS TOGGLE-BOX
     SIZE 10.57 BY .88 TOOLTIP "Etiqueta Reservada" NO-UNDO.

DEFINE VARIABLE fi-cod-depos-fin AS CHARACTER FORMAT "x(3)" INITIAL "ARM" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 TOOLTIP "Dep¢sito final." NO-UNDO.

DEFINE VARIABLE fi-cod-depos-ini AS CHARACTER FORMAT "x(3)" INITIAL "ARM" 
     LABEL "Dep¢sito":R10 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 TOOLTIP "Dep¢sito inicial." NO-UNDO.

DEFINE VARIABLE fi-cod-estabel AS CHARACTER FORMAT "x(3)" 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-obsoleto-fin AS CHARACTER FORMAT "x(1)" INITIAL "Z" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 TOOLTIP "C¢digo obsoleto final." NO-UNDO.

DEFINE VARIABLE fi-cod-obsoleto-ini AS CHARACTER FORMAT "x(1)" 
     LABEL "Cod.Obsoleto" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 TOOLTIP "C¢digo obsoleto inicial." NO-UNDO.

DEFINE VARIABLE fi-cod-refer-fin AS CHARACTER FORMAT "x(8)" INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 TOOLTIP "Codigo de Referˆncia Final" NO-UNDO.

DEFINE VARIABLE fi-cod-refer-ini AS CHARACTER FORMAT "x(8)" 
     LABEL "Referˆncia":R12 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 TOOLTIP "Codigo de Referˆncia Inicial" NO-UNDO.

DEFINE VARIABLE fi-corte-comerc-fin AS CHARACTER FORMAT "!" INITIAL "Z" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 TOOLTIP "Corte comercial final." NO-UNDO.

DEFINE VARIABLE fi-corte-comerc-ini AS CHARACTER FORMAT "!" 
     LABEL "Corte Comercial" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 TOOLTIP "Corte comercial inicial." NO-UNDO.

DEFINE VARIABLE fi-it-codigo-fin AS CHARACTER FORMAT "X(16)" INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 TOOLTIP "C¢digo do Item final." NO-UNDO.

DEFINE VARIABLE fi-it-codigo-ini AS CHARACTER FORMAT "x(16)" 
     LABEL "Item":R5 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 TOOLTIP "C¢digo do Item inicial." NO-UNDO.

DEFINE VARIABLE fi-lote-fin AS CHARACTER FORMAT "XX":U INITIAL "ZZ" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .79 TOOLTIP "Lote final." NO-UNDO.

DEFINE VARIABLE fi-lote-ini AS CHARACTER FORMAT "XX":U 
     LABEL "Lote" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .79 TOOLTIP "Lote inicial." NO-UNDO.

DEFINE VARIABLE fi-nome-estabel AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 36 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-28
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-29
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-30
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-31
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-32
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-33
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-88
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-89
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-90
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-91
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE VARIABLE rs-opc-acab AS INTEGER INITIAL 3 
     VIEW-AS RADIO-SET HORIZONTAL EXPAND 
     RADIO-BUTTONS 
          "Liso", 1,
"Estampado", 2,
"Ambos", 3
     SIZE 41.14 BY .79 TOOLTIP "Tipo de artigo." NO-UNDO.

DEFINE VARIABLE rs-opc-artigo AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL EXPAND 
     RADIO-BUTTONS 
          "Indigo", "I",
"Outros", "O",
"Ambos", "A"
     SIZE 41.14 BY .79 NO-UNDO.

DEFINE VARIABLE rs-tipo-artigo AS CHARACTER INITIAL "a" 
     VIEW-AS RADIO-SET HORIZONTAL EXPAND 
     RADIO-BUTTONS 
          "Indigo", "I",
"Outros", "O",
"Ambos", "A"
     SIZE 41.14 BY .79 NO-UNDO.

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

DEFINE FRAME f-pg-imp
     rs-destino AT ROW 2.38 COL 3.29 HELP
          "Destino de ImpressÆo do Relat¢rio" NO-LABEL
     bt-arquivo AT ROW 3.58 COL 43.29 HELP
          "Escolha do nome do arquivo"
     bt-config-impr AT ROW 3.58 COL 43.29 HELP
          "Configura‡Æo da impressora"
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

DEFINE FRAME f-relat
     bt-executar AT ROW 14.54 COL 3 HELP
          "Dispara a execu‡Æo do relat¢rio"
     bt-ajuda AT ROW 14.54 COL 70 HELP
          "Ajuda"
     bt-cancelar AT ROW 14.58 COL 14 HELP
          "Fechar"
     rt-folder-top AT ROW 2.54 COL 2.14
     RECT-6 AT ROW 13.75 COL 2.14
     rt-folder-right AT ROW 2.67 COL 80.43
     RECT-1 AT ROW 14.29 COL 2
     rt-folder-left AT ROW 2.54 COL 2.14
     rt-folder AT ROW 2.54 COL 2
     im-pg-imp AT ROW 1.5 COL 33.57
     im-pg-par AT ROW 1.5 COL 17.86
     im-pg-sel AT ROW 1.5 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81 BY 15
         DEFAULT-BUTTON bt-executar.

DEFINE FRAME f-pg-sel
     fi-nome-estabel AT ROW 1.54 COL 18 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     fi-cod-estabel AT ROW 1.54 COL 14.72 COLON-ALIGNED WIDGET-ID 2
     fi-it-codigo-ini AT ROW 2.54 COL 14.72 COLON-ALIGNED HELP
          "Codigo do Item Inicial"
     fi-it-codigo-fin AT ROW 2.54 COL 48.29 COLON-ALIGNED NO-LABEL
     fi-cod-refer-ini AT ROW 3.54 COL 14.72 COLON-ALIGNED HELP
          "Codigo de Referˆncia Inicial"
     fi-cod-refer-fin AT ROW 3.54 COL 48.29 COLON-ALIGNED HELP
          "Codigo de Referˆncia Final" NO-LABEL
     fi-lote-ini AT ROW 4.54 COL 14.72 COLON-ALIGNED HELP
          "N§ do Lote Inicial"
     fi-lote-fin AT ROW 4.54 COL 48.29 COLON-ALIGNED NO-LABEL
     fi-corte-comerc-ini AT ROW 5.54 COL 14.72 COLON-ALIGNED
     fi-corte-comerc-fin AT ROW 5.54 COL 48.29 COLON-ALIGNED HELP
          "Corte Comercial" NO-LABEL
     fi-cod-depos-ini AT ROW 6.54 COL 14.72 COLON-ALIGNED
     fi-cod-depos-fin AT ROW 6.54 COL 48.29 COLON-ALIGNED NO-LABEL
     fi-cod-obsoleto-ini AT ROW 7.54 COL 14.72 COLON-ALIGNED HELP
          "Codigo obsoleto"
     fi-cod-obsoleto-fin AT ROW 7.54 COL 48.29 COLON-ALIGNED HELP
          "Codigo obsoleto" NO-LABEL
     rs-opc-artigo AT ROW 8.83 COL 16.86 NO-LABEL WIDGET-ID 12
     rs-opc-acab AT ROW 9 COL 16.86 NO-LABEL WIDGET-ID 8
     rs-tipo-artigo AT ROW 10 COL 16.86 NO-LABEL WIDGET-ID 18
     "Acabamento:" VIEW-AS TEXT
          SIZE 9.43 BY .54 AT ROW 9.08 COL 7.57 WIDGET-ID 6
     "Acabamento:" VIEW-AS TEXT
          SIZE 9.57 BY 1 AT ROW 8.92 COL 7.14 WIDGET-ID 16
     "Tipo de Artigo:" VIEW-AS TEXT
          SIZE 10.57 BY 1 AT ROW 9.83 COL 6.43 WIDGET-ID 22
     IMAGE-1 AT ROW 2.54 COL 33.14
     IMAGE-2 AT ROW 2.54 COL 46.72
     IMAGE-28 AT ROW 3.54 COL 33.14
     IMAGE-29 AT ROW 3.54 COL 46.72
     IMAGE-30 AT ROW 4.54 COL 33.14
     IMAGE-31 AT ROW 4.54 COL 46.72
     IMAGE-32 AT ROW 5.54 COL 46.72
     IMAGE-33 AT ROW 5.54 COL 33.14
     IMAGE-88 AT ROW 6.54 COL 33.14
     IMAGE-89 AT ROW 6.54 COL 46.72
     IMAGE-90 AT ROW 7.54 COL 33.14
     IMAGE-91 AT ROW 7.54 COL 46.72
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 4 ROW 3.21
         SIZE 75.86 BY 10.25
         FONT 1.

DEFINE FRAME f-pg-par
     to-bloq AT ROW 3 COL 57 WIDGET-ID 4
     to-corte AT ROW 2 COL 57 WIDGET-ID 2
     to-item-dif AT ROW 8.33 COL 24
     to-imp-param AT ROW 9.92 COL 24
     to-imp1 AT ROW 1.96 COL 15.86
     to-res1 AT ROW 1.96 COL 36.43
     to-prod1 AT ROW 2.96 COL 15.86
     to-fat1 AT ROW 2.96 COL 36.43
     to-est1 AT ROW 3.96 COL 15.86
     to-reproc1 AT ROW 3.96 COL 36.43
     rs-tipo-relatorio AT ROW 5.83 COL 24 NO-LABEL
     to-cons AT ROW 4 COL 57 WIDGET-ID 6
     tg-sem-est AT ROW 9.13 COL 24 WIDGET-ID 8
     " Situa‡Æo das Etiquetas" VIEW-AS TEXT
          SIZE 17.72 BY .88 AT ROW 1 COL 10.29
          FONT 1
     " Tipo do Relat¢rio" VIEW-AS TEXT
          SIZE 13.14 BY .79 AT ROW 5.13 COL 10
     " Op‡Æo de ImpressÆo" VIEW-AS TEXT
          SIZE 15.86 BY .54 AT ROW 7.71 COL 10.14
     RECT-11 AT ROW 5.54 COL 6.14
     RECT-2 AT ROW 1.46 COL 6
     RECT-28 AT ROW 8 COL 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 77.14 BY 10.5
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
         TITLE              = "Estoque do EMS x Expedi‡Æo"
         HEIGHT             = 15
         WIDTH              = 81.14
         MAX-HEIGHT         = 28.63
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 28.63
         VIRTUAL-WIDTH      = 146.29
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
/* SETTINGS FOR FRAME f-pg-imp
   FRAME-NAME                                                           */
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
   Custom                                                               */
/* SETTINGS FOR FRAME f-pg-sel
   Custom                                                               */
/* SETTINGS FOR FILL-IN fi-nome-estabel IN FRAME f-pg-sel
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
ON END-ERROR OF w-relat /* Estoque do EMS x Expedi‡Æo */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON WINDOW-CLOSE OF w-relat /* Estoque do EMS x Expedi‡Æo */
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
   apply "close" to this-procedure.
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


&Scoped-define FRAME-NAME f-pg-sel
&Scoped-define SELF-NAME fi-cod-depos-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-depos-ini w-relat
ON LEAVE OF fi-cod-depos-ini IN FRAME f-pg-sel /* Dep¢sito */
DO:
   IF SELF:SCREEN-VALUE <> '' THEN
      ASSIGN fi-cod-depos-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-estabel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-estabel w-relat
ON LEAVE OF fi-cod-estabel IN FRAME f-pg-sel /* Estabelecimento */
DO:
    FIND estabelec WHERE
         estabelec.cod-estabel = INPUT FRAME {&FRAME-NAME} fi-cod-estabel 
         NO-LOCK NO-ERROR.
    IF NOT AVAIL estabelec THEN DO.
       MESSAGE 'Estabelecimento nÆo Cadastrado....'
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
       APPLY 'ENTRY' TO SELF.
       RETURN NO-APPLY.
    END.
    ASSIGN fi-nome-estabel:SCREEN-VALUE IN FRAME f-pg-sel = TRIM(estabelec.cidade) + " / " + TRIM(estabelec.bairro).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-obsoleto-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-obsoleto-ini w-relat
ON LEAVE OF fi-cod-obsoleto-ini IN FRAME f-pg-sel /* Cod.Obsoleto */
DO:
   IF SELF:SCREEN-VALUE <> '' THEN
      ASSIGN fi-cod-depos-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-refer-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-refer-ini w-relat
ON LEAVE OF fi-cod-refer-ini IN FRAME f-pg-sel /* Referˆncia */
DO:
   IF SELF:SCREEN-VALUE <> '' THEN
      ASSIGN fi-cod-refer-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-corte-comerc-fin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-corte-comerc-fin w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-corte-comerc-fin IN FRAME f-pg-sel
DO:
    {include/zoomvar.i &prog-zoom = eszoom/z01es065.w
                       &campo     = fi-corte-comerc-fin
                       &campozoom = codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-corte-comerc-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-corte-comerc-ini w-relat
ON LEAVE OF fi-corte-comerc-ini IN FRAME f-pg-sel /* Corte Comercial */
DO:
  IF SELF:SCREEN-VALUE <> '' THEN
     ASSIGN fi-corte-comerc-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-corte-comerc-ini w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-corte-comerc-ini IN FRAME f-pg-sel /* Corte Comercial */
DO:
    {include/zoomvar.i &prog-zoom = eszoom/z01es065.w
                       &campo     = fi-corte-comerc-ini
                       &campozoom = codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-it-codigo-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo-ini w-relat
ON LEAVE OF fi-it-codigo-ini IN FRAME f-pg-sel /* Item */
DO:
   IF SELF:SCREEN-VALUE <> '' THEN
      ASSIGN fi-it-codigo-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-lote-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-lote-ini w-relat
ON LEAVE OF fi-lote-ini IN FRAME f-pg-sel /* Lote */
DO:
   IF SELF:SCREEN-VALUE <> '' THEN
      ASSIGN fi-lote-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
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
&Scoped-define SELF-NAME rs-tipo-relatorio
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-tipo-relatorio w-relat
ON VALUE-CHANGED OF rs-tipo-relatorio IN FRAME f-pg-par
DO:
  IF rs-tipo-relatorio:SCREEN-VALUE = "2" THEN
     ASSIGN to-item-dif:SCREEN-VALUE = "no".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-sem-est
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-sem-est w-relat
ON VALUE-CHANGED OF tg-sem-est IN FRAME f-pg-par /* Somente Itens SEM Estoque e COM Etiquetas */
DO:
  IF to-item-dif:SCREEN-VALUE = "YES" THEN
     ASSIGN rs-tipo-relatorio:SCREEN-VALUE = "1".
  ELSE 
     ASSIGN rs-tipo-relatorio:SCREEN-VALUE = "2".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME to-item-dif
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL to-item-dif w-relat
ON VALUE-CHANGED OF to-item-dif IN FRAME f-pg-par /* Somente Itens com diferen‡a estoque */
DO:
  IF to-item-dif:SCREEN-VALUE = "YES" THEN
     ASSIGN rs-tipo-relatorio:SCREEN-VALUE = "1".
  ELSE 
     ASSIGN rs-tipo-relatorio:SCREEN-VALUE = "2".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-imp
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-relat 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{utp/ut9000.i "ESSP0142" "2.04.00.000"}

/* inicializa‡äes do template de relat¢rio */
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

    FIND FIRST para-ped NO-LOCK NO-ERROR.
    ASSIGN c-cod-estabel = para-ped.estab-padrao.

    ASSIGN fi-cod-estabel:SCREEN-VALUE IN FRAME f-pg-sel = c-cod-estabel.
    FIND estabelec WHERE
         estabelec.cod-estabel = c-cod-estabel 
         NO-LOCK NO-ERROR.
    IF AVAIL estabelec THEN
       ASSIGN fi-nome-estabel:SCREEN-VALUE IN FRAME f-pg-sel = TRIM(estabelec.cidade) + " / " + TRIM(estabelec.bairro).

    IF c-it-codigo <> "" THEN  DO: /* FOI RECEBIDO PARAMETROS */ 
       ASSIGN fi-it-codigo-ini:SCREEN-VALUE IN FRAME f-pg-sel = c-it-codigo
              fi-it-codigo-fin:SCREEN-VALUE IN FRAME f-pg-sel = c-it-codigo
              fi-cod-refer-ini:SCREEN-VALUE IN FRAME f-pg-sel = c-cod-refer
              fi-cod-refer-fin:SCREEN-VALUE IN FRAME f-pg-sel = c-cod-refer
              fi-lote-ini:SCREEN-VALUE IN FRAME f-pg-sel = c-lote     
              fi-lote-fin:SCREEN-VALUE IN FRAME f-pg-sel = c-lote
              to-imp1:SCREEN-VALUE IN FRAME f-pg-par = STRING(l-imp)
              to-prod1:SCREEN-VALUE IN FRAME f-pg-par = STRING(l-prod)
              to-est1:SCREEN-VALUE IN FRAME f-pg-par = STRING(l-est)
              to-res1:SCREEN-VALUE IN FRAME f-pg-par = STRING(l-res)      
              to-fat1:SCREEN-VALUE IN FRAME f-pg-par = STRING(l-fat)   
              to-reproc1:SCREEN-VALUE IN FRAME f-pg-par = STRING(l-reproc). 

       ASSIGN fi-it-codigo-ini:SENSITIVE IN FRAME f-pg-sel = NO
              fi-it-codigo-fin:SENSITIVE IN FRAME f-pg-sel = NO
              fi-cod-refer-ini:SENSITIVE IN FRAME f-pg-sel = NO
              fi-cod-refer-fin:SENSITIVE IN FRAME f-pg-sel = NO
              fi-lote-ini     :SENSITIVE IN FRAME f-pg-sel = NO
              fi-lote-fin     :SENSITIVE IN FRAME f-pg-sel = NO
              to-imp1:SENSITIVE IN FRAME f-pg-par = NO
              to-prod1:SENSITIVE IN FRAME f-pg-par = NO
              to-est1:SENSITIVE IN FRAME f-pg-par = NO
              to-res1:SENSITIVE IN FRAME f-pg-par = NO
              to-fat1:SENSITIVE IN FRAME f-pg-par = NO
              to-reproc1:SENSITIVE IN FRAME f-pg-par = NO
              to-imp-param:SENSITIVE IN FRAME f-pg-par = NO.
    END.

    {include/i-rpmbl.i}

    APPLY 'entry' TO fi-it-codigo-ini IN FRAME f-pg-sel.

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
  ENABLE im-pg-imp im-pg-par im-pg-sel bt-executar bt-ajuda bt-cancelar 
      WITH FRAME f-relat IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}
  DISPLAY rs-destino c-arquivo rs-execucao 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  ENABLE RECT-7 RECT-9 rs-destino bt-arquivo bt-config-impr c-arquivo 
         rs-execucao 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}
  DISPLAY to-bloq to-corte to-item-dif to-imp-param to-imp1 to-res1 to-prod1 
          to-fat1 to-est1 to-reproc1 rs-tipo-relatorio to-cons tg-sem-est 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  ENABLE to-bloq to-corte to-item-dif to-imp-param to-imp1 to-res1 to-prod1 
         to-fat1 to-est1 to-reproc1 rs-tipo-relatorio to-cons tg-sem-est 
         RECT-11 RECT-2 RECT-28 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-par}
  DISPLAY fi-nome-estabel fi-cod-estabel fi-it-codigo-ini fi-it-codigo-fin 
          fi-cod-refer-ini fi-cod-refer-fin fi-lote-ini fi-lote-fin 
          fi-corte-comerc-ini fi-corte-comerc-fin fi-cod-depos-ini 
          fi-cod-depos-fin fi-cod-obsoleto-ini fi-cod-obsoleto-fin rs-opc-artigo 
          rs-opc-acab rs-tipo-artigo 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  ENABLE fi-cod-estabel fi-it-codigo-ini fi-it-codigo-fin fi-cod-refer-ini 
         fi-cod-refer-fin fi-lote-ini fi-lote-fin fi-corte-comerc-ini 
         fi-corte-comerc-fin fi-cod-depos-ini fi-cod-depos-fin 
         fi-cod-obsoleto-ini fi-cod-obsoleto-fin rs-opc-artigo rs-opc-acab 
         rs-tipo-artigo IMAGE-1 IMAGE-2 IMAGE-28 IMAGE-29 IMAGE-30 IMAGE-31 
         IMAGE-32 IMAGE-33 IMAGE-88 IMAGE-89 IMAGE-90 IMAGE-91 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}
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
            run utp/ut-msgs.p (input "show", input 73, input "").
            
            apply "MOUSE-SELECT-CLICK":U to im-pg-imp in frame f-relat.
            apply "ENTRY":U to c-arquivo in frame f-pg-imp.
            return error.
        end.
    end.
    
    create tt-param.
    assign tt-param.usuario          = c-seg-usuario
           tt-param.destino          = input frame f-pg-imp rs-destino
           tt-param.data-exec        = today
           tt-param.hora-exec        = time
           tt-param.cod-estabel      = INPUT FRAME f-pg-sel fi-cod-estabel
           tt-param.it-codigo-ini    = INPUT FRAME f-pg-sel fi-it-codigo-ini                
           tt-param.it-codigo-fin    = INPUT FRAME f-pg-sel fi-it-codigo-fin             
           tt-param.cod-refer-ini    = INPUT FRAME f-pg-sel fi-cod-refer-ini                  
           tt-param.cod-refer-fin    = INPUT FRAME f-pg-sel fi-cod-refer-fin               
           tt-param.cod-depos-ini    = INPUT FRAME f-pg-sel fi-cod-depos-ini                  
           tt-param.cod-depos-fin    = INPUT FRAME f-pg-sel fi-cod-depos-fin               
           tt-param.lote-ini         = INPUT FRAME f-pg-sel fi-lote-ini                       
           tt-param.lote-fin         = INPUT FRAME f-pg-sel fi-lote-fin 
           tt-param.corte-comerc-ini = INPUT FRAME f-pg-sel fi-corte-comerc-ini                       
           tt-param.corte-comerc-fin = INPUT FRAME f-pg-sel fi-corte-comerc-fin
           tt-param.cod-obsoleto-ini = INPUT FRAME f-pg-sel fi-cod-obsoleto-ini
           tt-param.cod-obsoleto-fin = INPUT FRAME f-pg-sel fi-cod-obsoleto-fin
           tt-param.i-opc-acab       = INPUT FRAME f-pg-sel rs-opc-acab
           tt-param.c-tipo-artigo    = INPUT FRAME f-pg-sel rs-tipo-artigo
           tt-param.l-imp            = INPUT FRAME f-pg-par to-imp1                         
           tt-param.l-prod           = INPUT FRAME f-pg-par to-prod1                     
           tt-param.l-est            = INPUT FRAME f-pg-par to-est1                            
           tt-param.l-res            = INPUT FRAME f-pg-par to-res1                         
           tt-param.l-fat            = INPUT FRAME f-pg-par to-fat1                            
           tt-param.l-reproc         = INPUT FRAME f-pg-par to-reproc1
           tt-param.l-corte          = INPUT FRAME f-pg-par to-corte
           tt-param.l-bloq           = INPUT FRAME f-pg-par to-bloq
           tt-param.l-cons           = INPUT FRAME f-pg-par to-cons                            
           tt-param.tipo-relatorio   = INPUT FRAME f-pg-par rs-tipo-relatorio         
           tt-param.desc-tipo-relat  = entry((tt-param.tipo-relatorio - 1) * 2 + 1, 
                                              rs-tipo-relatorio:radio-buttons in frame f-pg-par)
           tt-param.item-dif         = INPUT FRAME f-pg-par to-item-dif
           tt-param.item-sem-est     = INPUT FRAME f-pg-par tg-sem-est
           tt-param.imp-param        = INPUT FRAME f-pg-par to-imp-param.                      
           
    if tt-param.destino = 1 THEN
       assign tt-param.arquivo = "".
    else 
    if tt-param.destino = 2 THEN
       assign tt-param.arquivo = input frame f-pg-imp c-arquivo.
    else 
       ASSIGN tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp".
    
    /* Executar do programa RP.P que ir  criar o relat¢rio */
    {include/i-rpexb.i}
    
    SESSION:SET-WAIT-STATE("general":U).
    
    {include/i-rprun.i esrp/essp0142rp.p}
    {include/i-rprun.i T:\especificos\base-tst\esrp\ob-etiqueta.p}
    
    {include/i-rpexc.i}
    
    SESSION:SET-WAIT-STATE("":U).
    
    {include/i-rptrm.i}
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-troca-pagina w-relat 
PROCEDURE pi-troca-pagina :
/*------------------------------------------------------------------------------
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

