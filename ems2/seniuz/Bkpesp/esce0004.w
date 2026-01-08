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
{include/i-prgvrs.i ESCE0004 2.04.00.000}

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
&GLOBAL-DEFINE PGDIG 
&GLOBAL-DEFINE PGIMP f-pg-imp
  
/* Parameters Definitions ---                                           */

/* Temporary Table Definitions ---                                      */

define temp-table tt-param    no-undo
       field destino          as integer
       field arquivo          as char format "x(35)"
       field usuario          as char format "x(12)"
       field data-exec        as date
       field hora-exec        as integer
       FIELD cod-estabel      LIKE movto-estoq.cod-estabel
       FIELD dt-trans-ini     LIKE movto-estoq.dt-trans 
       FIELD dt-trans-fin     LIKE movto-estoq.dt-trans
       FIELD ct-codigo-ini    LIKE movto-estoq.ct-codigo
       FIELD ct-codigo-fin    LIKE movto-estoq.ct-codigo
       FIELD sc-codigo-ini    LIKE movto-estoq.sc-codigo
       FIELD sc-codigo-fin    LIKE movto-estoq.sc-codigo
       FIELD ct-excluir-1     LIKE movto-estoq.ct-codigo
       FIELD ct-excluir-2     LIKE movto-estoq.ct-codigo
       FIELD ct-excluir-3     LIKE movto-estoq.ct-codigo
       FIELD ct-excluir-4     LIKE movto-estoq.ct-codigo
       FIELD ct-excluir-5     LIKE movto-estoq.ct-codigo
       FIELD ct-excluir-6     LIKE movto-estoq.ct-codigo
       FIELD ct-excluir-7     LIKE movto-estoq.ct-codigo
       FIELD ct-excluir-8     LIKE movto-estoq.ct-codigo
       FIELD ct-excluir-9     LIKE movto-estoq.ct-codigo
       FIELD ct-excluir-10    LIKE movto-estoq.ct-codigo
       FIELD ct-excluir-11    LIKE movto-estoq.ct-codigo
       FIELD ct-excluir-12    LIKE movto-estoq.ct-codigo
       FIELD tipo-rel         AS INT
       FIELD desc-tipo-rel    AS CHAR FORMAT "x(10)"
       FIELD impr-param       AS LOGICAL.

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

DEF VAR dt-aux AS DATE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-relat
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME f-pg-imp

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rs-destino rs-tipo-rel bt-config-impr ~
bt-arquivo c-arquivo rs-execucao RECT-17 RECT-7 RECT-9 
&Scoped-Define DISPLAYED-OBJECTS rs-destino rs-tipo-rel c-arquivo ~
rs-execucao 

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

DEFINE VARIABLE rs-tipo-rel AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Resumido", 1,
"Detalhado", 2
     SIZE 12 BY 1.38 TOOLTIP "Tipo do relat¢rio" NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 77 BY 10.67.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 46.29 BY 2.92.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 46.29 BY 1.71.

DEFINE VARIABLE fi-ct-codigo-1 AS CHARACTER FORMAT "X(8)" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "Conta 1 a ser desconsiderada." NO-UNDO.

DEFINE VARIABLE fi-ct-codigo-10 AS CHARACTER FORMAT "X(8)" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "Conta 1 a ser desconsiderada." NO-UNDO.

DEFINE VARIABLE fi-ct-codigo-11 AS CHARACTER FORMAT "X(8)" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "Conta 1 a ser desconsiderada." NO-UNDO.

DEFINE VARIABLE fi-ct-codigo-12 AS CHARACTER FORMAT "X(8)" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "Conta 1 a ser desconsiderada." NO-UNDO.

DEFINE VARIABLE fi-ct-codigo-2 AS CHARACTER FORMAT "X(8)" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "Conta 1 a ser desconsiderada." NO-UNDO.

DEFINE VARIABLE fi-ct-codigo-3 AS CHARACTER FORMAT "X(8)" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "Conta 1 a ser desconsiderada." NO-UNDO.

DEFINE VARIABLE fi-ct-codigo-4 AS CHARACTER FORMAT "X(8)" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "Conta 1 a ser desconsiderada." NO-UNDO.

DEFINE VARIABLE fi-ct-codigo-5 AS CHARACTER FORMAT "X(8)" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "Conta 1 a ser desconsiderada." NO-UNDO.

DEFINE VARIABLE fi-ct-codigo-6 AS CHARACTER FORMAT "X(8)" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "Conta 1 a ser desconsiderada." NO-UNDO.

DEFINE VARIABLE fi-ct-codigo-7 AS CHARACTER FORMAT "X(8)" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "Conta 1 a ser desconsiderada." NO-UNDO.

DEFINE VARIABLE fi-ct-codigo-8 AS CHARACTER FORMAT "X(8)" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "Conta 1 a ser desconsiderada." NO-UNDO.

DEFINE VARIABLE fi-ct-codigo-9 AS CHARACTER FORMAT "X(8)" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "Conta 1 a ser desconsiderada." NO-UNDO.

DEFINE RECTANGLE RECT-15
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 77 BY 10.67.

DEFINE RECTANGLE RECT-34
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 71 BY 1.75.

DEFINE RECTANGLE RECT-39
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 71 BY 2.63.

DEFINE RECTANGLE RECT-40
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 71 BY 2.92.

DEFINE VARIABLE tg-impr-param AS LOGICAL INITIAL no 
     LABEL "Imprimir Parƒmetros/Sele‡Æo" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .88 TOOLTIP "Imprimir Parƒmetros/Sele‡Æo ao final do relat¢rio."
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-cod-estabel AS CHARACTER FORMAT "X(3)":U 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fin-ct-codigo AS CHARACTER FORMAT "x(8)" INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 TOOLTIP "Conta final" NO-UNDO.

DEFINE VARIABLE fi-fin-dt-trans AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "Data de transa‡Æo final" NO-UNDO.

DEFINE VARIABLE fi-fin-sc-codigo AS CHARACTER FORMAT "x(8)" INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 TOOLTIP "Sub-Conta final" NO-UNDO.

DEFINE VARIABLE fi-ini-ct-codigo AS CHARACTER FORMAT "x(8)" 
     LABEL "Conta":R7 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 TOOLTIP "Conta inicial" NO-UNDO.

DEFINE VARIABLE fi-ini-dt-trans AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 
     LABEL "Data Transa‡Æo":R17 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "Data de transa‡Æo inicial" NO-UNDO.

DEFINE VARIABLE fi-ini-sc-codigo AS CHARACTER FORMAT "x(8)" 
     LABEL "Sub-Conta":R11 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 TOOLTIP "Sub-Conta inicial" NO-UNDO.

DEFINE VARIABLE fi-nome-estabel AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 47.14 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-59
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-60
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-63
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-64
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE BUTTON bt-ajuda 
     LABEL "Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "Fechar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-executar 
     LABEL "Executar" 
     SIZE 10 BY 1.

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
     rt-folder-right AT ROW 2.67 COL 80.43
     rt-folder-left AT ROW 2.54 COL 2.14
     rt-folder AT ROW 2.5 COL 2
     rt-folder-top AT ROW 2.54 COL 2.14
     RECT-1 AT ROW 14.29 COL 2
     RECT-6 AT ROW 13.75 COL 2.14
     im-pg-imp AT ROW 1.5 COL 33.43
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
     rs-tipo-rel AT ROW 3.13 COL 52 NO-LABEL
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
     RECT-17 AT ROW 1.08 COL 1.29
     RECT-7 AT ROW 1.92 COL 2.14
     RECT-9 AT ROW 5.29 COL 2.14
     "Tipos de Relat¢rio" VIEW-AS TEXT
          SIZE 13.29 BY .75 AT ROW 2.33 COL 51.72
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2.43 ROW 2.83
         SIZE 77.72 BY 10.83.

DEFINE FRAME f-pg-par
     fi-ct-codigo-1 AT ROW 2.21 COL 7 COLON-ALIGNED HELP
          "Conta" NO-LABEL
     fi-ct-codigo-3 AT ROW 2.21 COL 17.14 COLON-ALIGNED HELP
          "Conta" NO-LABEL
     fi-ct-codigo-5 AT ROW 2.21 COL 27.29 COLON-ALIGNED HELP
          "Conta" NO-LABEL
     fi-ct-codigo-7 AT ROW 2.21 COL 37.43 COLON-ALIGNED HELP
          "Conta" NO-LABEL
     fi-ct-codigo-9 AT ROW 2.21 COL 47.57 COLON-ALIGNED HELP
          "Conta" NO-LABEL
     fi-ct-codigo-11 AT ROW 2.21 COL 57.72 COLON-ALIGNED HELP
          "Conta" NO-LABEL
     fi-ct-codigo-2 AT ROW 3.29 COL 7 COLON-ALIGNED HELP
          "Conta" NO-LABEL
     fi-ct-codigo-4 AT ROW 3.29 COL 17.14 COLON-ALIGNED HELP
          "Conta" NO-LABEL
     fi-ct-codigo-6 AT ROW 3.29 COL 27.29 COLON-ALIGNED HELP
          "Conta" NO-LABEL
     fi-ct-codigo-8 AT ROW 3.29 COL 37.43 COLON-ALIGNED HELP
          "Conta" NO-LABEL
     fi-ct-codigo-10 AT ROW 3.29 COL 47.57 COLON-ALIGNED HELP
          "Conta" NO-LABEL
     fi-ct-codigo-12 AT ROW 3.29 COL 57.72 COLON-ALIGNED HELP
          "Conta" NO-LABEL
     rs-tipo-rel AT ROW 6 COL 33.29 NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Resumido", 1,
"Detalhado", 2
          SIZE 12 BY 1.96 TOOLTIP "Tipo do relat¢rio"
     tg-impr-param AT ROW 9.63 COL 25.43
     RECT-15 AT ROW 1 COL 1
     RECT-34 AT ROW 9.21 COL 4
     RECT-39 AT ROW 5.58 COL 4
     RECT-40 AT ROW 1.75 COL 4
     "Tipos de Relat¢rio" VIEW-AS TEXT
          SIZE 13.29 BY .75 AT ROW 5.21 COL 32
     "Contas a Desconsiderar" VIEW-AS TEXT
          SIZE 17 BY .75 AT ROW 1.38 COL 31.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2.72 ROW 2.83
         SIZE 77.29 BY 10.79
         FONT 1.

DEFINE FRAME f-pg-sel
     fi-cod-estabel AT ROW 1.79 COL 18.43 COLON-ALIGNED
     fi-nome-estabel AT ROW 1.79 COL 23.57 COLON-ALIGNED NO-LABEL
     fi-ini-dt-trans AT ROW 2.79 COL 18.43 COLON-ALIGNED
     fi-fin-dt-trans AT ROW 2.75 COL 49.29 COLON-ALIGNED NO-LABEL
     fi-ini-ct-codigo AT ROW 3.79 COL 18.43 COLON-ALIGNED
     fi-fin-ct-codigo AT ROW 3.79 COL 49.29 COLON-ALIGNED NO-LABEL
     fi-ini-sc-codigo AT ROW 4.79 COL 18.43 COLON-ALIGNED HELP
          "Sub-Conta da Conta Cont bil"
     fi-fin-sc-codigo AT ROW 4.79 COL 49.29 COLON-ALIGNED HELP
          "Sub-Conta da Conta Cont bil" NO-LABEL
     IMAGE-1 AT ROW 3.79 COL 31.57
     IMAGE-2 AT ROW 3.79 COL 47.14
     IMAGE-59 AT ROW 4.79 COL 31.57
     IMAGE-60 AT ROW 4.79 COL 47.14
     IMAGE-63 AT ROW 2.79 COL 31.57
     IMAGE-64 AT ROW 2.79 COL 47.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2.72 ROW 2.83
         SIZE 77.43 BY 10.83
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
         TITLE              = "Movimento de Estoque por Centro de Custo"
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
ON END-ERROR OF w-relat /* Movimento de Estoque por Centro de Custo */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON WINDOW-CLOSE OF w-relat /* Movimento de Estoque por Centro de Custo */
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
  {include/zoomvar.i &prog-zoom=adzoom/z01ad107.w
                     &campo=fi-cod-estabel
                     &campozoom=cod-estabel
                     &FRAME=f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME fi-ct-codigo-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ct-codigo-1 w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ct-codigo-1 IN FRAME f-pg-par /* Conta */
DO:
  {include/zoomvar.i &prog-zoom = adzoom/z01ad047.w
                     &campo     = fi-ct-codigo-1
                     &campozoom = ct-codigo
                     &FRAME     = f-pg-par}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ct-codigo-10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ct-codigo-10 w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ct-codigo-10 IN FRAME f-pg-par
DO:
  {include/zoomvar.i &prog-zoom = adzoom/z01ad047.w
                     &campo     = fi-ct-codigo-10
                     &campozoom = ct-codigo
                     &FRAME     = f-pg-par}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ct-codigo-11
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ct-codigo-11 w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ct-codigo-11 IN FRAME f-pg-par
DO:
  {include/zoomvar.i &prog-zoom = adzoom/z01ad047.w
                     &campo     = fi-ct-codigo-11
                     &campozoom = ct-codigo
                     &FRAME     = f-pg-par}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ct-codigo-12
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ct-codigo-12 w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ct-codigo-12 IN FRAME f-pg-par
DO:
  {include/zoomvar.i &prog-zoom = adzoom/z01ad047.w
                     &campo     = fi-ct-codigo-12
                     &campozoom = ct-codigo
                     &FRAME     = f-pg-par}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ct-codigo-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ct-codigo-2 w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ct-codigo-2 IN FRAME f-pg-par
DO:
  {include/zoomvar.i &prog-zoom = adzoom/z01ad047.w
                     &campo     = fi-ct-codigo-2
                     &campozoom = ct-codigo
                     &FRAME     = f-pg-par}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ct-codigo-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ct-codigo-3 w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ct-codigo-3 IN FRAME f-pg-par
DO:
  {include/zoomvar.i &prog-zoom = adzoom/z01ad047.w
                     &campo     = fi-ct-codigo-3
                     &campozoom = ct-codigo
                     &FRAME     = f-pg-par}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ct-codigo-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ct-codigo-4 w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ct-codigo-4 IN FRAME f-pg-par
DO:
  {include/zoomvar.i &prog-zoom = adzoom/z01ad047.w
                     &campo     = fi-ct-codigo-4
                     &campozoom = ct-codigo
                     &FRAME     = f-pg-par}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ct-codigo-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ct-codigo-5 w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ct-codigo-5 IN FRAME f-pg-par
DO:
  {include/zoomvar.i &prog-zoom = adzoom/z01ad047.w
                     &campo     = fi-ct-codigo-5
                     &campozoom = ct-codigo
                     &FRAME     = f-pg-par}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ct-codigo-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ct-codigo-6 w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ct-codigo-6 IN FRAME f-pg-par
DO:
  {include/zoomvar.i &prog-zoom = adzoom/z01ad047.w
                     &campo     = fi-ct-codigo-6
                     &campozoom = ct-codigo
                     &FRAME     = f-pg-par}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ct-codigo-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ct-codigo-7 w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ct-codigo-7 IN FRAME f-pg-par
DO:
  {include/zoomvar.i &prog-zoom = adzoom/z01ad047.w
                     &campo     = fi-ct-codigo-7
                     &campozoom = ct-codigo
                     &FRAME     = f-pg-par}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ct-codigo-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ct-codigo-8 w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ct-codigo-8 IN FRAME f-pg-par
DO:
  {include/zoomvar.i &prog-zoom = adzoom/z01ad047.w
                     &campo     = fi-ct-codigo-8
                     &campozoom = ct-codigo
                     &FRAME     = f-pg-par}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ct-codigo-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ct-codigo-9 w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ct-codigo-9 IN FRAME f-pg-par
DO:
  {include/zoomvar.i &prog-zoom = adzoom/z01ad047.w
                     &campo     = fi-ct-codigo-9
                     &campozoom = ct-codigo
                     &FRAME     = f-pg-par}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-sel
&Scoped-define SELF-NAME fi-fin-ct-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fin-ct-codigo w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-fin-ct-codigo IN FRAME f-pg-sel
DO:
  {include/zoomvar.i &prog-zoom = adzoom/z01ad047.w
                     &campo     = fi-fin-ct-codigo
                     &campozoom = ct-codigo
                     &FRAME     = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-fin-sc-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fin-sc-codigo w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-fin-sc-codigo IN FRAME f-pg-sel
DO:
  {include/zoomvar.i &prog-zoom = adzoom/z01ad246.w
                     &campo     = fi-fin-sc-codigo
                     &campozoom = sc-codigo
                     &FRAME     = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ini-ct-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-ct-codigo w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ini-ct-codigo IN FRAME f-pg-sel /* Conta */
DO:
  {include/zoomvar.i &prog-zoom = adzoom/z01ad047.w
                     &campo     = fi-ini-ct-codigo
                     &campozoom = ct-codigo
                     &FRAME     = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ini-sc-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-sc-codigo w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ini-sc-codigo IN FRAME f-pg-sel /* Sub-Conta */
DO:
  {include/zoomvar.i &prog-zoom = adzoom/z01ad246.w
                     &campo     = fi-ini-sc-codigo
                     &campozoom = sc-codigo
                     &FRAME     = f-pg-sel}
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


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-relat 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{utp/ut9000.i "ESCE0004" "2.04.00.000"}

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
  
    ASSIGN dt-aux = date(month(today),1,year(today)) - 1.

    ASSIGN fi-cod-estabel:SCREEN-VALUE IN FRAME f-pg-sel  = "2"
           fi-ini-dt-trans:SCREEN-VALUE IN FRAME f-pg-sel = string(date(month(dt-aux),1,year(dt-aux)))
           fi-fin-dt-trans:SCREEN-VALUE IN FRAME f-pg-sel = string(dt-aux).
    
    {include/i-rpmbl.i}
        
    fi-cod-estabel:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-ini-ct-codigo:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-fin-ct-codigo:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-ini-sc-codigo:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-fin-sc-codigo:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-ct-codigo-1:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-par.
    fi-ct-codigo-2:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-par.
    fi-ct-codigo-3:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-par.
    fi-ct-codigo-4:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-par.
    fi-ct-codigo-5:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-par.
    fi-ct-codigo-6:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-par.
    fi-ct-codigo-7:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-par.
    fi-ct-codigo-8:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-par.
    fi-ct-codigo-9:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-par.
    fi-ct-codigo-10:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-par.
    fi-ct-codigo-11:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-par.
    fi-ct-codigo-12:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-par.

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
  ENABLE bt-executar bt-cancelar bt-ajuda im-pg-imp im-pg-par im-pg-sel 
      WITH FRAME f-relat IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}
  DISPLAY rs-destino rs-tipo-rel c-arquivo rs-execucao 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  ENABLE rs-destino rs-tipo-rel bt-config-impr bt-arquivo c-arquivo rs-execucao 
         RECT-17 RECT-7 RECT-9 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}
  DISPLAY fi-ct-codigo-1 fi-ct-codigo-3 fi-ct-codigo-5 fi-ct-codigo-7 
          fi-ct-codigo-9 fi-ct-codigo-11 fi-ct-codigo-2 fi-ct-codigo-4 
          fi-ct-codigo-6 fi-ct-codigo-8 fi-ct-codigo-10 fi-ct-codigo-12 
          rs-tipo-rel tg-impr-param 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  ENABLE fi-ct-codigo-1 fi-ct-codigo-3 fi-ct-codigo-5 fi-ct-codigo-7 
         fi-ct-codigo-9 fi-ct-codigo-11 fi-ct-codigo-2 fi-ct-codigo-4 
         fi-ct-codigo-6 fi-ct-codigo-8 fi-ct-codigo-10 fi-ct-codigo-12 
         rs-tipo-rel tg-impr-param RECT-15 RECT-34 RECT-39 RECT-40 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-par}
  DISPLAY fi-cod-estabel fi-nome-estabel fi-ini-dt-trans fi-fin-dt-trans 
          fi-ini-ct-codigo fi-fin-ct-codigo fi-ini-sc-codigo fi-fin-sc-codigo 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  ENABLE fi-cod-estabel fi-ini-dt-trans fi-fin-dt-trans fi-ini-ct-codigo 
         fi-fin-ct-codigo fi-ini-sc-codigo fi-fin-sc-codigo IMAGE-1 IMAGE-2 
         IMAGE-59 IMAGE-60 IMAGE-63 IMAGE-64 
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
            run utp/ut-msgs.p (input "show":U, input 73, input "").
            
            apply "MOUSE-SELECT-CLICK":U to im-pg-imp in frame f-relat.
            apply "ENTRY":U to c-arquivo in frame f-pg-imp.
            return error.
        end.
    end.
    
    create tt-param.
    assign tt-param.usuario   = c-seg-usuario
           tt-param.destino   = input frame f-pg-imp rs-destino
           tt-param.data-exec = today
           tt-param.hora-exec = time.
    
    if tt-param.destino = 1 
    then assign tt-param.arquivo = "".
    else if  tt-param.destino = 2 
         then assign tt-param.arquivo = input frame f-pg-imp c-arquivo.
         else assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp":U.
    
    ASSIGN tt-param.cod-estabel    = INPUT FRAME f-pg-sel fi-cod-estabel
           tt-param.dt-trans-ini   = INPUT FRAME f-pg-sel fi-ini-dt-trans
           tt-param.dt-trans-fin   = INPUT FRAME f-pg-sel fi-fin-dt-trans           
           tt-param.ct-codigo-ini  = INPUT FRAME f-pg-sel fi-ini-ct-codigo
           tt-param.ct-codigo-fin  = INPUT FRAME f-pg-sel fi-fin-ct-codigo
           tt-param.sc-codigo-ini  = INPUT FRAME f-pg-sel fi-ini-sc-codigo
           tt-param.sc-codigo-fin  = INPUT FRAME f-pg-sel fi-fin-sc-codigo
           tt-param.ct-excluir-1   = INPUT FRAME f-pg-par fi-ct-codigo-1
           tt-param.ct-excluir-2   = INPUT FRAME f-pg-par fi-ct-codigo-2
           tt-param.ct-excluir-3   = INPUT FRAME f-pg-par fi-ct-codigo-3
           tt-param.ct-excluir-4   = INPUT FRAME f-pg-par fi-ct-codigo-4
           tt-param.ct-excluir-5   = INPUT FRAME f-pg-par fi-ct-codigo-5
           tt-param.ct-excluir-6   = INPUT FRAME f-pg-par fi-ct-codigo-6
           tt-param.ct-excluir-7   = INPUT FRAME f-pg-par fi-ct-codigo-7
           tt-param.ct-excluir-8   = INPUT FRAME f-pg-par fi-ct-codigo-8
           tt-param.ct-excluir-9   = INPUT FRAME f-pg-par fi-ct-codigo-9
           tt-param.ct-excluir-10  = INPUT FRAME f-pg-par fi-ct-codigo-10
           tt-param.ct-excluir-11  = INPUT FRAME f-pg-par fi-ct-codigo-11
           tt-param.ct-excluir-12  = INPUT FRAME f-pg-par fi-ct-codigo-12
           tt-param.tipo-rel       = INPUT FRAME f-pg-par rs-tipo-rel                          
           tt-param.desc-tipo-rel  = entry((tt-param.tipo-rel - 1) * 2 + 1, 
                                         rs-tipo-rel:radio-buttons in frame f-pg-par)
           tt-param.impr-param       = INPUT FRAME f-pg-par tg-impr-param.
           
    /*:T Executar do programa RP.P que ir  criar o relat¢rio */
    {include/i-rpexb.i}
    
    SESSION:SET-WAIT-STATE("general":U).
    
    {include/i-rprun.i esrp/esce0004rp.p} 
    
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

