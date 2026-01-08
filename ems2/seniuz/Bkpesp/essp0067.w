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
{include/i-prgvrs.i ESSP0067 2.04.00.000}

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
  
DEFINE TEMP-TABLE tt-param NO-UNDO
       FIELD destino             AS INTEGER
       FIELD arquivo             AS CHAR FORMAT "x(35)"
       FIELD usuario             AS CHAR FORMAT "x(12)"
       FIELD data-exec           AS DATE
       FIELD hora-exec           AS INTEGER
       FIELD classifica          AS INTEGER
       FIELD desc-classifica     AS CHAR FORMAT "x(40)"
       FIELD fi-ini-fm-codigo    LIKE teste-astm.fm-codigo
       FIELD fi-fin-fm-codigo    LIKE teste-astm.fm-codigo
       FIELD fi-ini-it-codigo    LIKE teste-astm.it-codigo
       FIELD fi-fin-it-codigo    LIKE teste-astm.it-codigo
       FIELD fi-ini-ano-mes      LIKE teste-astm.ano-mes    
       FIELD fi-fin-ano-mes      LIKE teste-astm.ano-mes    
       FIELD fi-ini-tipo-tear    LIKE teste-astm.tipo-tear
       FIELD fi-fin-tipo-tear    LIKE teste-astm.tipo-tear
       FIELD fi-ini-num-ob       LIKE teste-astm.num-ob
       FIELD fi-fin-num-ob       LIKE teste-astm.num-ob
       FIELD fi-ini-enc-tra      LIKE teste-astm.enc-tra     
       FIELD fi-fin-enc-tra      LIKE teste-astm.enc-tra
       FIELD fi-ini-enc-urd      LIKE teste-astm.enc-urd
       FIELD fi-fin-enc-urd      LIKE teste-astm.enc-urd
       FIELD fi-ini-dentra-ini   LIKE teste-astm.dentra-ini
       FIELD fi-fin-dentra-ini   LIKE teste-astm.dentra-ini
       FIELD fi-ini-dentra-fin   LIKE teste-astm.dentra-fin
       FIELD fi-fin-dentra-fin   LIKE teste-astm.dentra-fin
       FIELD tipo-acabamento     AS INTEGER
       FIELD desc-tipo-acab      AS CHAR FORMAT "x(15)"
       FIELD tipo-relatorio      AS INTEGER
       FIELD desc-tipo-relat     AS CHAR FORMAT "x(20)"
       FIELD imp-param           AS LOG.

/*
define temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id ordem.

define buffer b-tt-digita for tt-digita.
*/
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
DEFINE VARIABLE rs-classif AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Por Familia", 1,
"Por Ötem", 2,
"Por Per¡odo", 3
     SIZE 13.86 BY 3.21
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

DEFINE VARIABLE rs-tipo-acabamento AS INTEGER INITIAL 3 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Mercerizado    ", 1,
"NÆo Mercerizado", 2,
"Ambos          ", 3
     SIZE 22 BY 3
     FONT 1 NO-UNDO.

DEFINE VARIABLE rs-tipo-relatorio AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Sint‚tico", 1,
"Anal¡tico", 2
     SIZE 12 BY 3
     FONT 1 NO-UNDO.

DEFINE RECTANGLE RECT-27
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 20 BY 4.5.

DEFINE RECTANGLE RECT-34
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 61 BY 2.25.

DEFINE RECTANGLE RECT-38
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 29 BY 4.5.

DEFINE VARIABLE to-imp-param AS LOGICAL INITIAL no 
     LABEL "Imprimir Parƒmetros/Sele‡Æo" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .88 TOOLTIP "Imprimir Parƒmetros/Sele‡Æo ao final do relat¢rio."
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-fin-ano-mes AS CHARACTER FORMAT "9999/99" INITIAL "999912" 
     VIEW-AS FILL-IN 
     SIZE 7.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fin-dentra-fin AS DECIMAL FORMAT ">>9.9" INITIAL 999.9 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fin-dentra-ini AS DECIMAL FORMAT ">>9.9" INITIAL 999.9 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fin-enc-tra AS DECIMAL FORMAT "->9.99" INITIAL 99.99 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fin-enc-urd AS DECIMAL FORMAT "->9.99" INITIAL 99.99 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fin-fm-codigo AS CHARACTER FORMAT "x(6)" INITIAL "ZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 TOOLTIP "Fam¡lia final" NO-UNDO.

DEFINE VARIABLE fi-fin-it-codigo AS CHARACTER FORMAT "x(16)" INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 TOOLTIP "Item final." NO-UNDO.

DEFINE VARIABLE fi-fin-num-ob AS INTEGER FORMAT "999999" INITIAL 999999 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fin-tipo-tear AS CHARACTER FORMAT "!x(7)" INITIAL "ZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-ano-mes AS CHARACTER FORMAT "9999/99" INITIAL "000101" 
     LABEL "Periodo" 
     VIEW-AS FILL-IN 
     SIZE 7.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-dentra-fin AS DECIMAL FORMAT ">>9.9" INITIAL 0 
     LABEL "Densidade da Trama Final" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-dentra-ini AS DECIMAL FORMAT ">>9.9" INITIAL 0 
     LABEL "Densidade da Trama Inicial" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-enc-tra AS DECIMAL FORMAT "->9.99" INITIAL -9.99 
     LABEL "Encolhimento Trama" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-enc-urd AS DECIMAL FORMAT "->9.99" INITIAL -9.99 
     LABEL "Encolhimento Urdume" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-fm-codigo AS CHARACTER FORMAT "x(6)" 
     LABEL "Familia" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 TOOLTIP "Fam¡lia inicial" NO-UNDO.

DEFINE VARIABLE fi-ini-it-codigo AS CHARACTER FORMAT "x(16)" 
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 TOOLTIP "Item inicial." NO-UNDO.

DEFINE VARIABLE fi-ini-num-ob AS INTEGER FORMAT ">>>,>>9" INITIAL 0 
     LABEL "Numero da OB" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-tipo-tear AS CHARACTER FORMAT "!x(7)" 
     LABEL "Tipo Tear" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-27
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-28
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-31
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-32
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-33
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-34
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-35
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-36
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-37
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-38
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-40
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-41
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-42
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-43
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-44
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-45
     FILENAME "image\im-las":U
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
     rt-folder-left AT ROW 2.54 COL 2.14
     RECT-1 AT ROW 14.29 COL 2
     rt-folder-right AT ROW 2.67 COL 80.43
     RECT-6 AT ROW 13.75 COL 2.14
     rt-folder-top AT ROW 2.54 COL 2.14
     im-pg-cla AT ROW 1.5 COL 17.86
     im-pg-imp AT ROW 1.5 COL 49.29
     im-pg-par AT ROW 1.5 COL 33.57
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

DEFINE FRAME f-pg-par
     rs-tipo-relatorio AT ROW 2.54 COL 11.57 NO-LABEL
     rs-tipo-acabamento AT ROW 2.54 COL 45 NO-LABEL
     to-imp-param AT ROW 8.25 COL 26
     RECT-27 AT ROW 1.75 COL 7.86
     RECT-34 AT ROW 7.46 COL 8
     RECT-38 AT ROW 1.75 COL 40
     "Tipo do Relat¢rio" VIEW-AS TEXT
          SIZE 12.14 BY .79 AT ROW 1.42 COL 12
          FONT 1
     "Acabamento" VIEW-AS TEXT
          SIZE 9.14 BY .67 AT ROW 1.46 COL 49.72
          FONT 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 75 BY 10.

DEFINE FRAME f-pg-cla
     rs-classif AT ROW 1.25 COL 2.14 HELP
          "Classifica‡Æo para emissÆo do relat¢rio" NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 76.86 BY 10.31.

DEFINE FRAME f-pg-sel
     fi-ini-fm-codigo AT ROW 1.25 COL 19 COLON-ALIGNED HELP
          "Codigo da familia no Magnus"
     fi-fin-fm-codigo AT ROW 1.25 COL 52 COLON-ALIGNED HELP
          "Codigo da familia no Magnus" NO-LABEL
     fi-ini-it-codigo AT ROW 2.25 COL 19 COLON-ALIGNED
     fi-fin-it-codigo AT ROW 2.25 COL 52 COLON-ALIGNED NO-LABEL
     fi-ini-ano-mes AT ROW 3.25 COL 19 COLON-ALIGNED HELP
          "Periodo: AAAA/MM (AAAA = Ano, MM = Mes)"
     fi-fin-ano-mes AT ROW 3.25 COL 52 COLON-ALIGNED HELP
          "Periodo: AAAA/MM (AAAA = Ano, MM = Mes)" NO-LABEL
     fi-ini-tipo-tear AT ROW 4.25 COL 19 COLON-ALIGNED HELP
          "Tipo tear (Nissan, Tsudakoma, Sulzer,Toyota)."
     fi-fin-tipo-tear AT ROW 4.25 COL 52 COLON-ALIGNED HELP
          "Tipo tear (Nissan, Howa, Sulzer)." NO-LABEL
     fi-ini-num-ob AT ROW 5.25 COL 19 COLON-ALIGNED
     fi-fin-num-ob AT ROW 5.25 COL 52 COLON-ALIGNED NO-LABEL
     fi-ini-enc-tra AT ROW 6.25 COL 19 COLON-ALIGNED HELP
          "Encolhimento da Trama em percentual"
     fi-fin-enc-tra AT ROW 6.25 COL 52 COLON-ALIGNED HELP
          "Encolhimento da Trama em percentual" NO-LABEL
     fi-ini-enc-urd AT ROW 7.25 COL 19 COLON-ALIGNED HELP
          "Encolhimento do Urdume em percentual"
     fi-fin-enc-urd AT ROW 7.25 COL 52 COLON-ALIGNED HELP
          "Encolhimento do Urdume em percentual" NO-LABEL
     fi-ini-dentra-ini AT ROW 8.25 COL 19 COLON-ALIGNED HELP
          "Densidade da Trama inicial"
     fi-fin-dentra-ini AT ROW 8.25 COL 52 COLON-ALIGNED HELP
          "Densidade da Trama inicial" NO-LABEL
     fi-ini-dentra-fin AT ROW 9.25 COL 19 COLON-ALIGNED HELP
          "Densidade da Trama final"
     fi-fin-dentra-fin AT ROW 9.25 COL 52 COLON-ALIGNED HELP
          "Densidade da Trama final" NO-LABEL
     IMAGE-1 AT ROW 1.25 COL 36
     IMAGE-2 AT ROW 1.25 COL 50.86
     IMAGE-27 AT ROW 2.25 COL 36
     IMAGE-28 AT ROW 2.25 COL 50.86
     IMAGE-31 AT ROW 3.25 COL 36.14
     IMAGE-32 AT ROW 3.25 COL 50.86
     IMAGE-33 AT ROW 4.25 COL 36.14
     IMAGE-34 AT ROW 5.25 COL 36.14
     IMAGE-35 AT ROW 6.25 COL 36.14
     IMAGE-36 AT ROW 4.25 COL 50.86
     IMAGE-37 AT ROW 5.25 COL 50.86
     IMAGE-38 AT ROW 6.25 COL 50.86
     IMAGE-40 AT ROW 8.25 COL 36.14
     IMAGE-41 AT ROW 7.25 COL 36.14
     IMAGE-42 AT ROW 7.25 COL 50.86
     IMAGE-43 AT ROW 8.25 COL 50.86
     IMAGE-44 AT ROW 9.25 COL 36.14
     IMAGE-45 AT ROW 9.25 COL 50.86
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
         TITLE              = "Teste de ASTM"
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
/* SETTINGS FOR FRAME f-pg-sel
                                                                        */
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
ON END-ERROR OF w-relat /* Teste de ASTM */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON WINDOW-CLOSE OF w-relat /* Teste de ASTM */
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


&Scoped-define FRAME-NAME f-pg-sel
&Scoped-define SELF-NAME fi-fin-fm-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fin-fm-codigo w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-fin-fm-codigo IN FRAME f-pg-sel
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z01in122.w
                     &campo     = fi-fin-fm-codigo
                     &campozoom = fm-codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-fin-it-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fin-it-codigo w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-fin-it-codigo IN FRAME f-pg-sel
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z01in172.w
                     &campo     = fi-fin-it-codigo
                     &campozoom = it-codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ini-fm-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-fm-codigo w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ini-fm-codigo IN FRAME f-pg-sel /* Familia */
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z01in122.w
                     &campo     = fi-ini-fm-codigo
                     &campozoom = fm-codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ini-it-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-it-codigo w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ini-it-codigo IN FRAME f-pg-sel /* Item */
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z01in172.w
                     &campo     = fi-ini-it-codigo
                     &campozoom = it-codigo}
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


&Scoped-define FRAME-NAME f-pg-cla
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-relat 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{utp/ut9000.i "ESSP0067" "2.04.00.000"}

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
  
    ASSIGN fi-ini-fm-codigo:SCREEN-VALUE IN FRAME f-pg-sel = "" 
           fi-fin-fm-codigo:SCREEN-VALUE IN FRAME f-pg-sel = "ZZZZZZ"
           fi-ini-it-codigo:SCREEN-VALUE IN FRAME f-pg-sel = ""
           fi-fin-it-codigo:SCREEN-VALUE IN FRAME f-pg-sel = "ZZZZZZZZZZZZZZZZ"
           fi-ini-ano-mes:SCREEN-VALUE IN FRAME f-pg-sel   = "000101"
           fi-fin-ano-mes:SCREEN-VALUE IN FRAME f-pg-sel   = "999912".

    {include/i-rpmbl.i}

    fi-ini-fm-codigo:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME f-pg-sel.
    fi-fin-fm-codigo:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME f-pg-sel.
    fi-ini-it-codigo:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME f-pg-sel.
    fi-fin-it-codigo:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME f-pg-sel.
  
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
  DISPLAY fi-ini-fm-codigo fi-fin-fm-codigo fi-ini-it-codigo fi-fin-it-codigo 
          fi-ini-ano-mes fi-fin-ano-mes fi-ini-tipo-tear fi-fin-tipo-tear 
          fi-ini-num-ob fi-fin-num-ob fi-ini-enc-tra fi-fin-enc-tra 
          fi-ini-enc-urd fi-fin-enc-urd fi-ini-dentra-ini fi-fin-dentra-ini 
          fi-ini-dentra-fin fi-fin-dentra-fin 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  ENABLE fi-ini-fm-codigo fi-fin-fm-codigo fi-ini-it-codigo fi-fin-it-codigo 
         fi-ini-ano-mes fi-fin-ano-mes fi-ini-tipo-tear fi-fin-tipo-tear 
         fi-ini-num-ob fi-fin-num-ob fi-ini-enc-tra fi-fin-enc-tra 
         fi-ini-enc-urd fi-fin-enc-urd fi-ini-dentra-ini fi-fin-dentra-ini 
         fi-ini-dentra-fin fi-fin-dentra-fin IMAGE-1 IMAGE-2 IMAGE-27 IMAGE-28 
         IMAGE-31 IMAGE-32 IMAGE-33 IMAGE-34 IMAGE-35 IMAGE-36 IMAGE-37 
         IMAGE-38 IMAGE-40 IMAGE-41 IMAGE-42 IMAGE-43 IMAGE-44 IMAGE-45 
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
  DISPLAY rs-tipo-relatorio rs-tipo-acabamento to-imp-param 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  ENABLE rs-tipo-relatorio rs-tipo-acabamento to-imp-param RECT-27 RECT-34 
         RECT-38 
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
    
    /*:T Coloque aqui as valida‡äes da p gina de Digita‡Æo, lembrando que elas devem
       apresentar uma mensagem de erro cadastrada, posicionar nesta p gina e colocar
       o focus no campo com problemas */
    /*browse br-digita:SET-REPOSITIONED-ROW (browse br-digita:DOWN, "ALWAYS":U).*/
    
    /*:T Coloque aqui as valida‡äes das outras p ginas, lembrando que elas devem 
       apresentar uma mensagem de erro cadastrada, posicionar na p gina com 
       problemas e colocar o focus no campo com problemas */
    
    
    
    /*:T Aqui sÆo gravados os campos da temp-table que ser  passada como parƒmetro
       para o programa RP.P */
    
    create tt-param.
    assign tt-param.usuario         = c-seg-usuario
           tt-param.destino         = input frame f-pg-imp rs-destino
           tt-param.data-exec       = today
           tt-param.hora-exec       = time
           tt-param.classifica      = input frame f-pg-cla rs-classif
           tt-param.desc-classifica = entry((tt-param.classifica - 1) * 2 + 1, 
                                            rs-classif:radio-buttons in frame f-pg-cla)
           tt-param.fi-ini-fm-codigo    = INPUT FRAME f-pg-sel fi-ini-fm-codigo
           tt-param.fi-fin-fm-codigo    = INPUT FRAME f-pg-sel fi-fin-fm-codigo
           tt-param.fi-ini-it-codigo    = INPUT FRAME f-pg-sel fi-ini-it-codigo
           tt-param.fi-fin-it-codigo    = INPUT FRAME f-pg-sel fi-fin-it-codigo
           tt-param.fi-ini-ano-mes      = INPUT FRAME f-pg-sel fi-ini-ano-mes   
           tt-param.fi-fin-ano-mes      = INPUT FRAME f-pg-sel fi-fin-ano-mes   
           tt-param.fi-ini-tipo-tear    = INPUT FRAME f-pg-sel fi-ini-tipo-tear
           tt-param.fi-fin-tipo-tear    = INPUT FRAME f-pg-sel fi-fin-tipo-tear
           tt-param.fi-ini-num-ob       = INPUT FRAME f-pg-sel fi-ini-num-ob          
           tt-param.fi-fin-num-ob       = INPUT FRAME f-pg-sel fi-fin-num-ob     
           tt-param.fi-ini-enc-tra      = INPUT FRAME f-pg-sel fi-ini-enc-tra     
           tt-param.fi-fin-enc-tra      = INPUT FRAME f-pg-sel fi-fin-enc-tra    
           tt-param.fi-ini-enc-urd      = INPUT FRAME f-pg-sel fi-ini-enc-urd      
           tt-param.fi-fin-enc-urd      = INPUT FRAME f-pg-sel fi-fin-enc-urd      
           tt-param.fi-ini-dentra-ini   = INPUT FRAME f-pg-sel fi-ini-dentra-ini 
           tt-param.fi-fin-dentra-ini   = INPUT FRAME f-pg-sel fi-fin-dentra-ini  
           tt-param.fi-ini-dentra-fin   = INPUT FRAME f-pg-sel fi-ini-dentra-fin   
           tt-param.fi-fin-dentra-fin   = INPUT FRAME f-pg-sel fi-fin-dentra-fin 
           tt-param.tipo-acabamento     = INPUT FRAME f-pg-par rs-tipo-acabamento
           tt-param.desc-tipo-acab      = entry((tt-param.tipo-acabamento - 1) * 2 + 1, 
                                              rs-tipo-acabamento:radio-buttons in frame f-pg-par)
           tt-param.tipo-relatorio      = INPUT FRAME f-pg-par rs-tipo-relatorio         
           tt-param.desc-tipo-relat     = entry((tt-param.tipo-relatorio - 1) * 2 + 1, 
                                              rs-tipo-relatorio:radio-buttons in frame f-pg-par)
           tt-param.imp-param           = INPUT FRAME f-pg-par to-imp-param.
    
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
    
    {include/i-rprun.i esrp/essp0067rp.p}
    
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

