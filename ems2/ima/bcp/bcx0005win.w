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
{include/i-prgvrs.i bcx0005win 2.00.00.001}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Preprocessadores do Template de Relat¢rio                            */
/* Obs: Retirar o valor do preprocessador para as p†ginas que n∆o existirem  */

&GLOBAL-DEFINE PGSEL f-pg-sel
&GLOBAL-DEFINE PGCLA 
&GLOBAL-DEFINE PGPAR f-pg-par
&GLOBAL-DEFINE PGDIG 
&GLOBAL-DEFINE PGIMP f-pg-imp
  
/* Parameters Definitions ---                                           */

/* Temporary Table Definitions ---                                      */

define temp-table tt-param no-undo
    field destino           as integer
    field arquivo           as char format "x(35)"
    field usuario           as char format "x(12)"
    field data-exec         as date
    field hora-exec         as integer
    Field c-estab-ini       As Char
    Field c-estab-fim       As Char
    Field i-item-ini        As Char
    Field i-item-fim        As Char
    Field c-deposito-ini    As Char
    Field c-deposito-fim    As Char
    Field c-localizacao-ini As Char
    Field c-localizacao-fim As Char
    Field c-refer-ini       As Char
    Field c-refer-fim       As Char
    Field da-inventario-ini As Date
    Field da-inventario-fim As Date
    Field c-hora-inventario-ini As Char
    Field c-hora-inventario-fim As Char
    Field da-saldo-ini      As Date
    Field da-saldo-fim      As Date
    Field i-nr-contagem-ini As Int
    Field i-nr-contagem-fim As Int
    Field l-atual           As Log
    Field i-situacao        As Int.

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
&Scoped-define FRAME-NAME f-pg-imp

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rs-destino bt-arquivo bt-config-impr ~
c-arquivo rs-execucao RECT-7 RECT-9 
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

DEFINE VARIABLE text-modo AS CHARACTER FORMAT "X(256)":U INITIAL "Execuá∆o" 
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

DEFINE VARIABLE lb-atual AS CHARACTER FORMAT "X(256)":U 
     LABEL "Atualizaá∆o Invent†rio" 
      VIEW-AS TEXT 
     SIZE 2 BY .67 NO-UNDO.

DEFINE VARIABLE lb-situacao AS CHARACTER FORMAT "X(256)":U 
     LABEL "Situaá∆o Invent†rio" 
      VIEW-AS TEXT 
     SIZE 2 BY .67 NO-UNDO.

DEFINE VARIABLE rs-situacao AS INTEGER INITIAL 3 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Atualizado", 1,
"N∆o Atualizado", 2,
"Ambos", 3
     SIZE 54 BY 1.5 NO-UNDO.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 75 BY 2.25.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 75 BY 2.25.

DEFINE VARIABLE tg-atual AS LOGICAL INITIAL no 
     LABEL "Atualiza Movimento de Invent†rio?" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .83 NO-UNDO.

DEFINE VARIABLE c-cod-estabel-fim AS CHARACTER FORMAT "x(3)" INITIAL "ZZZ" 
     VIEW-AS FILL-IN 
     SIZE 13.86 BY .88 NO-UNDO.

DEFINE VARIABLE c-cod-estabel-ini AS CHARACTER FORMAT "x(3)" 
     LABEL "Estabelec" 
     VIEW-AS FILL-IN 
     SIZE 13.86 BY .88 NO-UNDO.

DEFINE VARIABLE c-cod-localizacao-fim AS CHARACTER FORMAT "X(10)":U INITIAL "ZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 13.86 BY .88 NO-UNDO.

DEFINE VARIABLE c-cod-localizacao-ini AS CHARACTER FORMAT "X(10)":U 
     LABEL "Localizaá∆o" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE c-cod-refer-fim AS CHARACTER FORMAT "X(10)":U INITIAL "ZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 13.86 BY .88 NO-UNDO.

DEFINE VARIABLE c-cod-refer-ini AS CHARACTER FORMAT "X(10)":U 
     LABEL "Referància" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE c-deposito-fim AS CHARACTER FORMAT "X(8)":U INITIAL "ZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 16.86 BY .88 NO-UNDO.

DEFINE VARIABLE c-deposito-ini AS CHARACTER FORMAT "X(8)":U 
     LABEL "Dep¢sito" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE c-hora-inventario-fim AS CHARACTER FORMAT "x(8)":U INITIAL "24:00:00" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE c-hora-inventario-ini AS CHARACTER FORMAT "x(8)":U INITIAL "00:00:00" 
     LABEL "Hr Movimento" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE c-it-codigo-fim AS CHARACTER FORMAT "X(16)" INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 21.86 BY .88 NO-UNDO.

DEFINE VARIABLE c-it-codigo-ini AS CHARACTER FORMAT "X(16)" 
     LABEL "Item":R5 
     VIEW-AS FILL-IN 
     SIZE 21.86 BY .88 NO-UNDO.

DEFINE VARIABLE da-inventario-fim AS DATE FORMAT "99/99/9999":U INITIAL 12/31/02 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE da-inventario-ini AS DATE FORMAT "99/99/9999":U 
     LABEL "Dt Movimento" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE da-saldo-fim AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE da-saldo-ini AS DATE FORMAT "99/99/9999":U 
     LABEL "Data de Corte" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE i-contagem-fim AS INTEGER FORMAT ">,>>>,>>9":U INITIAL 9 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE i-contagem-ini AS INTEGER FORMAT ">,>>>,>>9":U INITIAL 0 
     LABEL "Nr. Contagem" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-10
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-11
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-12
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-13
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-14
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-15
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-16
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-17
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-18
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-3
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-4
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-5
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-6
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-7
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-8
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-9
     FILENAME "image\im-fir":U
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
          "Dispara a execuá∆o do relat¢rio"
     bt-cancelar AT ROW 14.54 COL 14 HELP
          "Fechar"
     bt-ajuda AT ROW 14.54 COL 70 HELP
          "Ajuda"
     rt-folder AT ROW 2.5 COL 2
     rt-folder-top AT ROW 2.54 COL 2.14
     rt-folder-left AT ROW 2.54 COL 2.14
     rt-folder-right AT ROW 2.67 COL 80.43
     RECT-6 AT ROW 13.75 COL 2.14
     RECT-1 AT ROW 14.29 COL 2
     im-pg-imp AT ROW 1.5 COL 33.57
     im-pg-par AT ROW 1.5 COL 17.86
     im-pg-sel AT ROW 1.5 COL 2.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81 BY 15
         DEFAULT-BUTTON bt-executar.

DEFINE FRAME f-pg-imp
     rs-destino AT ROW 2.38 COL 3.29 HELP
          "Destino de Impress∆o do Relat¢rio" NO-LABEL
     bt-arquivo AT ROW 3.58 COL 43.29 HELP
          "Escolha do nome do arquivo"
     bt-config-impr AT ROW 3.58 COL 43.29 HELP
          "Configuraá∆o da impressora"
     c-arquivo AT ROW 3.63 COL 3.29 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL
     rs-execucao AT ROW 5.75 COL 3 HELP
          "Modo de Execuá∆o" NO-LABEL
     text-destino AT ROW 1.63 COL 3.86 NO-LABEL
     text-modo AT ROW 5 COL 1.29 COLON-ALIGNED NO-LABEL
     RECT-7 AT ROW 1.92 COL 2.14
     RECT-9 AT ROW 5.29 COL 2.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 73.72 BY 10.

DEFINE FRAME f-pg-par
     tg-atual AT ROW 2.5 COL 14
     rs-situacao AT ROW 5 COL 14 NO-LABEL
     lb-atual AT ROW 1.25 COL 24 COLON-ALIGNED
     lb-situacao AT ROW 4.25 COL 21 COLON-ALIGNED
     RECT-10 AT ROW 1.5 COL 2
     RECT-11 AT ROW 4.5 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 76.86 BY 10.31.

DEFINE FRAME f-pg-sel
     c-cod-estabel-ini AT ROW 2 COL 14.72 COLON-ALIGNED HELP
          "Estabelecimento padr∆o do item"
     c-cod-estabel-fim AT ROW 2 COL 48.14 COLON-ALIGNED HELP
          "Estabelecimento padr∆o do item" NO-LABEL
     c-it-codigo-ini AT ROW 3 COL 14.72 COLON-ALIGNED HELP
          "C¢digo do Item"
     c-it-codigo-fim AT ROW 3 COL 48.14 COLON-ALIGNED HELP
          "C¢digo do Item" NO-LABEL
     c-deposito-ini AT ROW 4 COL 14.72 COLON-ALIGNED
     c-deposito-fim AT ROW 4 COL 48.14 COLON-ALIGNED NO-LABEL
     c-cod-localizacao-ini AT ROW 5 COL 14.72 COLON-ALIGNED
     c-cod-localizacao-fim AT ROW 5 COL 48.14 COLON-ALIGNED NO-LABEL
     c-cod-refer-ini AT ROW 6.04 COL 14.72 COLON-ALIGNED
     c-cod-refer-fim AT ROW 6.04 COL 48.29 COLON-ALIGNED NO-LABEL
     da-inventario-ini AT ROW 7.04 COL 14.72 COLON-ALIGNED
     da-inventario-fim AT ROW 7.04 COL 48.14 COLON-ALIGNED NO-LABEL
     c-hora-inventario-ini AT ROW 8.04 COL 14.72 COLON-ALIGNED
     c-hora-inventario-fim AT ROW 8.04 COL 48.14 COLON-ALIGNED NO-LABEL
     da-saldo-ini AT ROW 9.04 COL 14.72 COLON-ALIGNED
     da-saldo-fim AT ROW 9.04 COL 48.14 COLON-ALIGNED NO-LABEL
     i-contagem-ini AT ROW 10.04 COL 14.72 COLON-ALIGNED
     i-contagem-fim AT ROW 10.04 COL 48.14 COLON-ALIGNED NO-LABEL
     IMAGE-1 AT ROW 2 COL 38.72
     IMAGE-10 AT ROW 8 COL 38.72
     IMAGE-11 AT ROW 7 COL 38.72
     IMAGE-12 AT ROW 6 COL 38.72
     IMAGE-13 AT ROW 9 COL 47.14
     IMAGE-14 AT ROW 8 COL 47.14
     IMAGE-15 AT ROW 7 COL 47.14
     IMAGE-16 AT ROW 6 COL 47.14
     IMAGE-17 AT ROW 10.04 COL 47.14
     IMAGE-18 AT ROW 10.04 COL 38.72
     IMAGE-2 AT ROW 2 COL 47.14
     IMAGE-3 AT ROW 5 COL 47.14
     IMAGE-4 AT ROW 4 COL 47.14
     IMAGE-5 AT ROW 3 COL 47.14
     IMAGE-6 AT ROW 5 COL 38.72
     IMAGE-7 AT ROW 4 COL 38.72
     IMAGE-8 AT ROW 3 COL 38.72
     IMAGE-9 AT ROW 9 COL 38.72
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2.86 ROW 2.71
         SIZE 77.57 BY 10.79.


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
         TITLE              = "Relat¢rio de Invent†rio"
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
                "Execuá∆o".

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
ON END-ERROR OF w-relat /* Relat¢rio de Invent†rio */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON WINDOW-CLOSE OF w-relat /* Relat¢rio de Invent†rio */
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
        Assign 
            Input Frame f-pg-sel c-cod-estabel-ini
            Input Frame f-pg-sel c-cod-estabel-fim
            Input Frame f-pg-sel c-it-codigo-ini
            Input Frame f-pg-sel c-it-codigo-fim
            Input Frame f-pg-sel c-deposito-ini
            Input Frame f-pg-sel c-deposito-fim
            Input Frame f-pg-sel c-cod-localizacao-ini
            Input Frame f-pg-sel c-cod-localizacao-fim
            Input Frame f-pg-sel da-inventario-ini
            Input Frame f-pg-sel da-inventario-fim
            Input Frame f-pg-sel c-hora-inventario-ini
            Input Frame f-pg-sel c-hora-inventario-fim
            Input Frame f-pg-sel da-saldo-ini
            Input Frame f-pg-sel da-saldo-fim
            Input Frame f-pg-sel i-contagem-ini
            Input Frame f-pg-sel i-contagem-fim
            
            Input Frame f-pg-par rs-situacao
            Input Frame f-pg-par tg-atual.
       run pi-executar.
   end.
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
&Scoped-define SELF-NAME tg-atual
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-atual w-relat
ON VALUE-CHANGED OF tg-atual IN FRAME f-pg-par /* Atualiza Movimento de Invent†rio? */
DO:
  
    IF tg-atual:CHECKED IN FRAME {&FRAME-NAME}
    THEN
        ASSIGN rs-situacao:SCREEN-VALUE IN FRAME f-pg-par = "2"
               rs-situacao:SENSITIVE IN FRAME f-pg-par    = NO.

    IF tg-atual:CHECKED IN FRAME {&FRAME-NAME} = NO
    THEN
        ASSIGN rs-situacao:SCREEN-VALUE IN FRAME f-pg-par = "3"
               rs-situacao:SENSITIVE IN FRAME f-pg-par    = YES.
    
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

{utp/ut9000.i "BCX0005WIN" "2.00.00.000"}

/* inicializaá‰es do template de relat¢rio */
{include/i-rpini.i}

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

{include/i-rplbl.i}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.


    Assign 
        da-saldo-fim = Today
        da-saldo-ini = Today
        da-inventario-ini = date('01/01/2002').

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO  ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    RUN enable_UI.
  
    {include/i-rpmbl.i}
  
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
  DISPLAY c-cod-estabel-ini c-cod-estabel-fim c-it-codigo-ini c-it-codigo-fim 
          c-deposito-ini c-deposito-fim c-cod-localizacao-ini 
          c-cod-localizacao-fim c-cod-refer-ini c-cod-refer-fim 
          da-inventario-ini da-inventario-fim c-hora-inventario-ini 
          c-hora-inventario-fim da-saldo-ini da-saldo-fim i-contagem-ini 
          i-contagem-fim 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  ENABLE c-cod-estabel-ini c-cod-estabel-fim c-it-codigo-ini c-it-codigo-fim 
         c-deposito-ini c-deposito-fim c-cod-localizacao-ini 
         c-cod-localizacao-fim c-cod-refer-ini c-cod-refer-fim 
         da-inventario-ini da-inventario-fim c-hora-inventario-ini 
         c-hora-inventario-fim da-saldo-ini da-saldo-fim i-contagem-ini 
         i-contagem-fim IMAGE-1 IMAGE-10 IMAGE-11 IMAGE-12 IMAGE-13 IMAGE-14 
         IMAGE-15 IMAGE-16 IMAGE-17 IMAGE-18 IMAGE-2 IMAGE-3 IMAGE-4 IMAGE-5 
         IMAGE-6 IMAGE-7 IMAGE-8 IMAGE-9 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}
  DISPLAY rs-destino c-arquivo rs-execucao 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  ENABLE rs-destino bt-arquivo bt-config-impr c-arquivo rs-execucao RECT-7 
         RECT-9 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}
  DISPLAY tg-atual rs-situacao lb-atual lb-situacao 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  ENABLE tg-atual rs-situacao lb-atual lb-situacao RECT-10 RECT-11 
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
            run utp/ut-msgs.p (input "show", input 73, input "").
            
            apply "MOUSE-SELECT-CLICK":U to im-pg-imp in frame f-relat.
            apply "ENTRY":U to c-arquivo in frame f-pg-imp.
            return error.
        end.
    end.
    
    create  tt-param.
    assign  tt-param.usuario        = c-seg-usuario
            tt-param.destino        = input frame f-pg-imp rs-destino
            tt-param.data-exec      = today
            tt-param.hora-exec      = time
            tt-param.c-estab-ini    = c-cod-estabel-ini
            tt-param.c-estab-fim    = c-cod-estabel-fim
            tt-param.i-item-ini     = c-it-codigo-ini
            tt-param.i-item-fim     = c-it-codigo-fim
            tt-param.c-deposito-ini = c-deposito-ini
            tt-param.c-deposito-fim = c-deposito-fim
            tt-param.c-localizacao-ini  = c-cod-localizacao-ini
            tt-param.c-localizacao-fim  = c-cod-localizacao-fim
            tt-param.c-refer-ini    = c-cod-refer-ini
            tt-param.c-refer-fim    = c-cod-refer-fim
            tt-param.da-inventario-ini  = da-inventario-ini
            tt-param.da-inventario-fim  = da-inventario-fim
            tt-param.c-hora-inventario-ini  = c-hora-inventario-ini
            tt-param.c-hora-inventario-fim  = c-hora-inventario-fim
            tt-param.da-saldo-ini       = da-saldo-ini
            tt-param.da-saldo-fim       = da-saldo-fim
            tt-param.i-nr-contagem-ini  = i-contagem-ini
            tt-param.i-nr-contagem-fim  = i-contagem-fim
            tt-param.l-atual            = tg-atual
            tt-param.i-situacao         = rs-situacao.
    
    if tt-param.destino = 1 
    then assign tt-param.arquivo = "".
    else if  tt-param.destino = 2 
         then assign tt-param.arquivo = input frame f-pg-imp c-arquivo.
         else assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp".
    
    /* Coloque aqui a l¢gica de gravaá∆o dos demais campos que devem ser passados
       como parÉmetros para o programa RP.P, atravÇs da temp-table tt-param */
    
    
    
    /* Executar do programa RP.P que ir† criar o relat¢rio */
    {include/i-rpexb.i}
    
    SESSION:SET-WAIT-STATE("general":U).
    
    {include/i-rprun.i bcp/bcx0005rp.p}
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
  Purpose: Gerencia a Troca de P†gina (folder)   
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

