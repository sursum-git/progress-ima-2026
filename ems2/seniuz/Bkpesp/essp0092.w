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
/*{include/i-prgvrs.i XX9999 9.99.99.999} */

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
&GLOBAL-DEFINE PGPAR 
&GLOBAL-DEFINE PGDIG f-pg-dig
&GLOBAL-DEFINE PGIMP f-pg-imp
  
/* Parameters Definitions ---                                           */

/* Temporary Table Definitions ---                                      */

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    FIELD it-codigo        AS CHAR FORMAT "x(16)"
    FIELD cod-refer-ini    AS CHAR FORMAT "x(8)"
    FIELD cod-refer-fim    AS CHAR FORMAT "x(8)"
    FIELD seq-ini          AS INT  FORMAT ">>9"
    FIELD seq-fim          AS INT  FORMAT ">>9"
    FIELD partida          AS INT  FORMAT ">>>>>9"
    FIELD nuance           AS CHAR FORMAT "x(1)"
    FIELD qualidade        AS CHAR FORMAT "x(2)"
    FIELD metro            AS DEC  FORMAT ">>,>>9.99"
    FIELD embalagem        AS CHAR FORMAT "x(2)".

define temp-table tt-digita no-undo
    FIELD it-codigo        AS CHAR FORMAT "x(16)"
    FIELD cod-refer        AS CHAR FORMAT "x(8)"
    FIELD seq-ini          AS INT  FORMAT ">>9" 
    FIELD seq-fim          AS INT  FORMAT ">>9" 
    FIELD partida          AS INT  FORMAT ">>>>>9"
    FIELD nuance           AS CHAR FORMAT "x(1)" 
    FIELD qualidade        AS CHAR FORMAT "x(2)"
    FIELD metro            AS DEC  FORMAT ">>,>>9.99"
    FIELD embalagem        AS CHAR FORMAT "x(2)"
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
&Scoped-define FIELDS-IN-QUERY-br-digita tt-digita.it-codigo tt-digita.cod-refer tt-digita.seq-ini tt-digita.seq-fim tt-digita.partida tt-digita.nuance tt-digita.qualidade tt-digita.metro tt-digita.embalagem   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-digita tt-digita.it-codigo ~
 ~
tt-digita.cod-refer   tt-digita.seq-ini ~
 tt-digita.seq-fim ~
 tt-digita.partida ~
 tt-digita.nuance ~
 tt-digita.qualidade ~
tt-digita.metro ~
  tt-digita.embalagem   
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


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-grava-macro w-relat 
FUNCTION fn-grava-macro RETURNS CHARACTER
  ( INPUT arq-image AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-relat AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-alterar 
     LABEL "Alterar" 
     SIZE 15 BY 1.

DEFINE BUTTON bt-inserir 
     LABEL "Inserir" 
     SIZE 15 BY 1.

DEFINE BUTTON bt-recuperar 
     LABEL "Recuperar" 
     SIZE 15 BY 1.

DEFINE BUTTON bt-retirar 
     LABEL "Retirar" 
     SIZE 15 BY 1.

DEFINE BUTTON bt-salvar 
     LABEL "Salvar" 
     SIZE 15 BY 1.

DEFINE BUTTON bt-arquivo 
     IMAGE-UP FILE "image\im-sea":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE BUTTON bt-config-impr 
     IMAGE-UP FILE "image\im-cfprt":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE VARIABLE cb-impressoras AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 40 BY 1 NO-UNDO.

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

DEFINE VARIABLE rs-destino-ok AS INTEGER INITIAL 1 
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

DEFINE BUTTON bt-logo 
     IMAGE-UP FILE "image/logo-etq.bmp":U NO-FOCUS
     LABEL "Button 1" 
     SIZE 16 BY 3.42.

DEFINE BUTTON bt-rlgp1 
     IMAGE-UP FILE "image/rlgp1.bmp":U
     LABEL "Button 5" 
     SIZE 26 BY 1.75.

DEFINE BUTTON bt-rlgp2 
     IMAGE-UP FILE "image/rlgp2.bmp":U
     LABEL "Button 4" 
     SIZE 26 BY 1.75.

DEFINE BUTTON bt-rlgp3 
     IMAGE-UP FILE "image/rlgp3.bmp":U
     LABEL "bt rlgp 3" 
     SIZE 26 BY 1.75.

DEFINE BUTTON bt-rlgp4 
     IMAGE-UP FILE "image/rlgp4.bmp":U
     LABEL "bt rlgp 3" 
     SIZE 26 BY 1.75.

DEFINE VARIABLE fi-desc-item AS CHARACTER FORMAT "X(36)":U 
     VIEW-AS FILL-IN 
     SIZE 38 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi-embalagem AS CHARACTER FORMAT "!!":U INITIAL "RL" 
     LABEL "Embalagem" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fim-cod-refer AS CHARACTER FORMAT "X(8)":U INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 9.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fim-seq AS INTEGER FORMAT "999":U INITIAL 1 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-cod-refer AS CHARACTER FORMAT "X(8)":U 
     LABEL "Referˆncia" 
     VIEW-AS FILL-IN 
     SIZE 9.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-seq AS INTEGER FORMAT "999":U INITIAL 1 
     LABEL "Sequˆncia" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-it-codigo AS CHARACTER FORMAT "X(16)":U 
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE fi-metro AS DECIMAL FORMAT ">>,>>9.99":U INITIAL 0 
     LABEL "Metros" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nuance AS CHARACTER FORMAT "X":U 
     LABEL "Nuance" 
     VIEW-AS FILL-IN 
     SIZE 2 BY .88 NO-UNDO.

DEFINE VARIABLE fi-partida AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Partida" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE fi-qualidade AS CHARACTER FORMAT "!!":U 
     LABEL "Qualidade" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-3
     FILENAME "image\im-fir":U
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

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 77 BY 4.25.

DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 77 BY 5.75.

DEFINE VARIABLE TOGGLE-1 AS LOGICAL INITIAL no 
     LABEL "Carregar Imagens a serem Impressas na Etiqueta" 
     VIEW-AS TOGGLE-BOX
     SIZE 37 BY .83
     FGCOLOR 0  NO-UNDO.

DEFINE BUTTON bt-ajuda 
     LABEL "Ajuda" 
     SIZE 12 BY 1.

DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "Fechar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-executar 
     LABEL "Executar" 
     SIZE 10 BY 1.

DEFINE IMAGE im-pg-dig
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.21.

DEFINE IMAGE im-pg-imp
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
      tt-digita.it-codigo  LABEL "Item"     
    tt-digita.cod-refer  LABEL "Referencia" 
    tt-digita.seq-ini    LABEL "Seq INI"         
    tt-digita.seq-fim    LABEL "Seq FIM"         
    tt-digita.partida    LABEL "Partida"         
    tt-digita.nuance     LABEL "Nuance"         
    tt-digita.qualidade  LABEL "Qualidade"         
    tt-digita.metro      LABEL "Metro"         
    tt-digita.embalagem  LABEL "Embalagem"         
ENABLE                    
    tt-digita.it-codigo           
    tt-digita.cod-refer
    tt-digita.seq-ini    
    tt-digita.seq-fim    
    tt-digita.partida    
    tt-digita.nuance     
    tt-digita.qualidade  
    tt-digita.metro      
    tt-digita.embalagem
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 76.57 BY 9
         BGCOLOR 15 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-relat
     bt-executar AT ROW 14.54 COL 3 HELP
          "Dispara a execu‡Æo do relat¢rio"
     bt-cancelar AT ROW 14.54 COL 13.86 HELP
          "Fechar"
     bt-ajuda AT ROW 14.54 COL 68 HELP
          "Ajuda"
     rt-folder-left AT ROW 2.54 COL 2.14
     RECT-1 AT ROW 14.29 COL 2
     RECT-6 AT ROW 13.75 COL 2.14
     rt-folder-top AT ROW 2.54 COL 2.14
     rt-folder AT ROW 2.5 COL 2
     rt-folder-right AT ROW 2.67 COL 80.43
     im-pg-dig AT ROW 1.5 COL 17.72
     im-pg-imp AT ROW 1.5 COL 33.43
     im-pg-sel AT ROW 1.5 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81 BY 15
         DEFAULT-BUTTON bt-executar.

DEFINE FRAME f-pg-imp
     rs-destino-ok AT ROW 2.33 COL 3.43 HELP
          "Destino de ImpressÆo do Relat¢rio" NO-LABEL
     rs-destino AT ROW 2.38 COL 3.29 HELP
          "Destino de ImpressÆo do Relat¢rio" NO-LABEL
     cb-impressoras AT ROW 3.58 COL 1.29 COLON-ALIGNED NO-LABEL
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
         SIZE 77.29 BY 10.67.

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
     bt-logo AT ROW 8 COL 3
     fi-it-codigo AT ROW 1.75 COL 19 COLON-ALIGNED
     fi-desc-item AT ROW 1.75 COL 35 COLON-ALIGNED NO-LABEL
     fi-ini-cod-refer AT ROW 2.75 COL 19 COLON-ALIGNED
     fi-fim-cod-refer AT ROW 2.75 COL 44.86 COLON-ALIGNED NO-LABEL
     fi-ini-seq AT ROW 3.75 COL 19 COLON-ALIGNED
     fi-fim-seq AT ROW 3.75 COL 44.86 COLON-ALIGNED NO-LABEL
     fi-partida AT ROW 4.75 COL 19 COLON-ALIGNED
     fi-metro AT ROW 4.75 COL 45 COLON-ALIGNED
     fi-embalagem AT ROW 5.75 COL 19 COLON-ALIGNED
     fi-qualidade AT ROW 5.75 COL 45 COLON-ALIGNED
     fi-nuance AT ROW 5.75 COL 71.57 COLON-ALIGNED
     TOGGLE-1 AT ROW 7.08 COL 2.72
     bt-rlgp3 AT ROW 7.92 COL 50
     bt-rlgp1 AT ROW 8 COL 21.43
     bt-rlgp2 AT ROW 9.71 COL 21.43
     bt-rlgp4 AT ROW 9.71 COL 50
     IMAGE-3 AT ROW 2.75 COL 37
     IMAGE-4 AT ROW 2.75 COL 43
     IMAGE-5 AT ROW 3.75 COL 37
     IMAGE-6 AT ROW 3.75 COL 43
     RECT-18 AT ROW 7.5 COL 1
     RECT-19 AT ROW 1.25 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.71
         SIZE 77.14 BY 10.96
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
         TITLE              = "ImpressÆo de Etiquetas C¢digo de Barras"
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
/* SETTINGS FOR COMBO-BOX cb-impressoras IN FRAME f-pg-imp
   NO-DISPLAY                                                           */
/* SETTINGS FOR RADIO-SET rs-destino IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       rs-destino:HIDDEN IN FRAME f-pg-imp           = TRUE.

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

/* SETTINGS FOR FRAME f-pg-sel
                                                                        */
/* SETTINGS FOR BUTTON bt-logo IN FRAME f-pg-sel
   NO-ENABLE                                                            */
ASSIGN 
       bt-logo:HIDDEN IN FRAME f-pg-sel           = TRUE.

/* SETTINGS FOR BUTTON bt-rlgp1 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
ASSIGN 
       bt-rlgp1:HIDDEN IN FRAME f-pg-sel           = TRUE.

/* SETTINGS FOR BUTTON bt-rlgp2 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
ASSIGN 
       bt-rlgp2:HIDDEN IN FRAME f-pg-sel           = TRUE.

/* SETTINGS FOR BUTTON bt-rlgp3 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
ASSIGN 
       bt-rlgp3:HIDDEN IN FRAME f-pg-sel           = TRUE.

/* SETTINGS FOR BUTTON bt-rlgp4 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
ASSIGN 
       bt-rlgp4:HIDDEN IN FRAME f-pg-sel           = TRUE.

/* SETTINGS FOR FILL-IN fi-desc-item IN FRAME f-pg-sel
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
ON END-ERROR OF w-relat /* ImpressÆo de Etiquetas C¢digo de Barras */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON WINDOW-CLOSE OF w-relat /* ImpressÆo de Etiquetas C¢digo de Barras */
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
        DISPLAY tt-digita.it-codigo     
                tt-digita.cod-refer
                tt-digita.seq-ini    
                tt-digita.seq-fim    
                tt-digita.partida    
                tt-digita.nuance     
                tt-digita.qualidade  
                tt-digita.metro      
                tt-digita.embalagem  
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
       assign tt-digita.seq-ini:screen-value in browse br-digita = "001"
              tt-digita.seq-fim:screen-value in browse br-digita = "001"
              tt-digita.partida:screen-value in browse br-digita = "0"
              tt-digita.nuance:screen-value in browse br-digita = ""
              tt-digita.metro:screen-value in browse br-digita = "0"
              tt-digita.embalagem:screen-value in browse br-digita = "RL".
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
        assign input browse br-digita tt-digita.it-codigo           
               input browse br-digita tt-digita.cod-refer
               input browse br-digita tt-digita.seq-ini
               input browse br-digita tt-digita.seq-fim    
               input browse br-digita tt-digita.partida    
               input browse br-digita tt-digita.nuance     
               input browse br-digita tt-digita.qualidade  
               input browse br-digita tt-digita.metro      
               input browse br-digita tt-digita.embalagem.  
    
        br-digita:CREATE-RESULT-LIST-ENTRY() in frame f-pg-dig.
    end.
    else do transaction on error undo, return no-apply:
        assign input browse br-digita tt-digita.it-codigo 
               input browse br-digita tt-digita.cod-refer
               input browse br-digita tt-digita.seq-ini
               input browse br-digita tt-digita.seq-fim    
               input browse br-digita tt-digita.partida    
               input browse br-digita tt-digita.nuance     
               input browse br-digita tt-digita.qualidade  
               input browse br-digita tt-digita.metro      
               input browse br-digita tt-digita.embalagem.  
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

        assign tt-digita.seq-ini = 001
               tt-digita.seq-fim = 001
               tt-digita.nuance = ""
               tt-digita.embalagem = "RL".
        
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


&Scoped-define FRAME-NAME f-pg-sel
&Scoped-define SELF-NAME fi-it-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo w-relat
ON LEAVE OF fi-it-codigo IN FRAME f-pg-sel /* Item */
DO:
  IF SELF:SCREEN-VALUE <> "" THEN DO.
     FIND mgind.item WHERE
          mgind.item.it-codigo = SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
     IF NOT AVAIL item THEN DO.
        MESSAGE "Item nÆo Cadastrado...." VIEW-AS ALERT-BOX.
        APPLY 'entry' TO SELF.
        RETURN NO-APPLY.
     END.
     ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = mgind.item.descricao-1 + mgind.item.descricao-2.
  END.
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


&Scoped-define SELF-NAME rs-destino-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-destino-ok w-relat
ON VALUE-CHANGED OF rs-destino-ok IN FRAME f-pg-imp
DO:
do  with frame f-pg-imp:
    case self:screen-value:
        when "1" then do:
            ASSIGN cb-impressoras:SCREEN-VALUE = SESSION:PRINTER-NAME.
            assign c-arquivo:sensitive    = no
                   c-arquivo:VISIBLE      = NO
                   bt-arquivo:visible     = no
                   bt-config-impr:VISIBLE = NO
                   cb-impressoras:VISIBLE = YES.
        end.
        when "2" then do:
            assign c-arquivo:sensitive     = yes
                   bt-arquivo:visible      = yes
                   bt-config-impr:VISIBLE  = NO.
        end.
        when "3" then do:
            assign c-arquivo:sensitive    = no
                   c-arquivo:VISIBLE      = NO
                   bt-arquivo:visible     = no
                   bt-config-impr:VISIBLE = NO
                   cb-impressoras:VISIBLE = NO.
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


&Scoped-define FRAME-NAME f-pg-sel
&Scoped-define SELF-NAME TOGGLE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOGGLE-1 w-relat
ON VALUE-CHANGED OF TOGGLE-1 IN FRAME f-pg-sel /* Carregar Imagens a serem Impressas na Etiqueta */
DO:
    SESSION:SET-WAIT-STATE("general":U).
    
    fn-grava-macro("image\logo-etq10.prn"). 

    ASSIGN bt-logo:HIDDEN IN FRAME f-pg-sel = NO
           bt-logo:SENSITIVE IN FRAME f-pg-sel = YES.

    fn-grava-macro("image\rlgp11.prn").
    fn-grava-macro("image\rlgp12.prn"). 
    ASSIGN bt-rlgp1:HIDDEN IN FRAME f-pg-sel = NO
           bt-rlgp1:SENSITIVE IN FRAME f-pg-sel = YES.

    fn-grava-macro("image\rlgp13.prn").
    fn-grava-macro("image\rlgp14.prn"). 
    ASSIGN bt-rlgp2:HIDDEN IN FRAME f-pg-sel = NO
           bt-rlgp2:SENSITIVE IN FRAME f-pg-sel = YES.

    fn-grava-macro("image\rlgp15.prn"). 
    fn-grava-macro("image\rlgp16.prn"). 
    fn-grava-macro("image\rlgp17.prn"). 
    ASSIGN bt-rlgp3:HIDDEN IN FRAME f-pg-sel = NO
           bt-rlgp3:SENSITIVE IN FRAME f-pg-sel = YES.

    fn-grava-macro("image\rlgp18.prn"). 
    fn-grava-macro("image\rlgp19.prn"). 
    fn-grava-macro("image\rlgp20.prn"). 
    ASSIGN bt-rlgp4:HIDDEN IN FRAME f-pg-sel = NO
           bt-rlgp4:SENSITIVE IN FRAME f-pg-sel = YES.

    ASSIGN SELF:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
    SESSION:SET-WAIT-STATE("":U).
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

{utp/ut9000.i "ESSP0092" "2.04.00.001"}

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
  
    {include/i-rpmbl.i}

    ASSIGN bt-logo:HIDDEN IN FRAME f-pg-sel = YES
           bt-rlgp1:HIDDEN IN FRAME f-pg-sel = YES
           bt-rlgp2:HIDDEN IN FRAME f-pg-sel = YES
           bt-rlgp3:HIDDEN IN FRAME f-pg-sel = YES
           bt-rlgp4:HIDDEN IN FRAME f-pg-sel = YES.

    rs-execucao:DISABLE(ENTRY(3,rs-execucao:RADIO-BUTTONS IN FRAME f-pg-imp)) IN FRAME f-pg-imp.
    rs-destino-ok:DISABLE(ENTRY(3,rs-destino-ok:RADIO-BUTTONS IN FRAME f-pg-imp)) IN FRAME f-pg-imp.
    rs-destino-ok:DISABLE(ENTRY(5,rs-destino-ok:RADIO-BUTTONS IN FRAME f-pg-imp)) IN FRAME f-pg-imp.
    
    ASSIGN cb-impressoras:LIST-ITEMS IN FRAME f-pg-imp = SESSION:PRINTER-NAME.  
    APPLY "value-changed" TO rs-destino-ok IN FRAME f-pg-imp. 

    APPLY "MOUSE-SELECT-CLICK":U to im-pg-sel in frame f-relat. 

    APPLY 'entry' TO fi-it-codigo IN FRAME f-pg-sel.
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
  ENABLE bt-executar bt-cancelar bt-ajuda im-pg-dig im-pg-imp im-pg-sel 
      WITH FRAME f-relat IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}
  DISPLAY fi-it-codigo fi-desc-item fi-ini-cod-refer fi-fim-cod-refer fi-ini-seq 
          fi-fim-seq fi-partida fi-metro fi-embalagem fi-qualidade fi-nuance 
          TOGGLE-1 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  ENABLE fi-it-codigo fi-ini-cod-refer fi-fim-cod-refer fi-ini-seq fi-fim-seq 
         fi-partida fi-metro fi-embalagem fi-qualidade fi-nuance TOGGLE-1 
         IMAGE-3 IMAGE-4 IMAGE-5 IMAGE-6 RECT-18 RECT-19 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}
  DISPLAY rs-destino-ok c-arquivo rs-execucao 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  ENABLE rs-destino-ok cb-impressoras bt-config-impr bt-arquivo c-arquivo 
         rs-execucao RECT-7 RECT-9 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}
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
           tt-param.hora-exec       = time.

    ASSIGN tt-param.it-codigo     = INPUT FRAME f-pg-sel fi-it-codigo
           tt-param.cod-refer-ini = INPUT FRAME f-pg-sel fi-ini-cod-refer 
           tt-param.cod-refer-fim = INPUT FRAME f-pg-sel fi-fim-cod-refer
           tt-param.seq-ini       = INPUT FRAME f-pg-sel fi-ini-seq
           tt-param.seq-fim       = INPUT FRAME f-pg-sel fi-fim-seq
           tt-param.partida       = INPUT FRAME f-pg-sel fi-partida
           tt-param.nuance        = INPUT FRAME f-pg-sel fi-nuance
           tt-param.qualidade     = CAPS(INPUT FRAME f-pg-sel fi-qualidade)
           tt-param.metro         = INPUT FRAME f-pg-sel fi-metro
           tt-param.embalagem     = CAPS(INPUT FRAME f-pg-sel fi-embalagem).
    
    if tt-param.destino = 1 
    then assign tt-param.arquivo = "".
    else if  tt-param.destino = 2 
         then assign tt-param.arquivo = input frame f-pg-imp c-arquivo.
         else assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp":U.
    
    /*:T Coloque aqui a l¢gica de grava‡Æo dos demais campos que devem ser passados
       como parƒmetros para o programa RP.P, atrav‚s da temp-table tt-param */
    
    
    
    /*:T Executar do programa RP.P que ir  criar o relat¢rio */

     SESSION:SET-WAIT-STATE("general":U).

     OUTPUT TO PRINTER.
         {include/i-rprun.i esrp/essp0092rp.p}
     OUTPUT CLOSE.

     SESSION:SET-WAIT-STATE("":U).
     APPLY "MOUSE-SELECT-CLICK":U to im-pg-sel in frame f-relat.

END.
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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-grava-macro w-relat 
FUNCTION fn-grava-macro RETURNS CHARACTER
  ( INPUT arq-image AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose: Gravar Imagens na Mem¢ria da Impressora 
    Notes: Esta funcao Grava as imagens em macros, lembrando que a defini‡Æo das
           macros estÆo inclusas nas imagens e foram feitas com um editor bin rio 
------------------------------------------------------------------------------*/
    DEF VAR c-comando AS CHAR.
    ASSIGN c-comando = "net use lpt2: " + IF SESSION:PRINTER-NAME BEGINS "\\" 
                                          THEN SESSION:PRINTER-NAME
                                          ELSE SESSION:PRINTER-PORT.
    OS-COMMAND SILENT VALUE(c-comando).  

    ASSIGN c-comando = "copy /Y /b " + arq-image + " lpt2". 
    OS-COMMAND SILENT VALUE(c-comando). 

    ASSIGN c-comando = "net use lpt2: /DELETE".
    OS-COMMAND SILENT VALUE(c-comando).  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

