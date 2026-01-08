&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF TEMP-TABLE tt-menus 
    FIELD i-seq            AS INT  FORMAT "999" 
    FIELD c-tipo           AS CHAR FORMAT "x"
    FIELD c-nom-men        AS CHAR FORMAT "x(20)"
    FIELD c-nom-rot        AS CHAR FORMAT "x(20)"
    FIELD c-nom-sub        AS CHAR FORMAT "x(20)"
    FIELD c-nom-prog       AS CHAR FORMAT "x(10)"
    FIELD c-cod-prog       AS CHAR FORMAT "x(10)"
    FIELD c-icone          AS CHAR FORMAT "x" 
    FIELD c-image          AS CHAR FORMAT "x(20)"
    FIELD c-tooltip        AS CHAR FORMAT "x(50)"
    INDEX seq i-seq.

DEF BUFFER b-rotinas   FOR tt-menus.
DEF BUFFER b-submenus  FOR tt-menus.
DEF BUFFER b-programas FOR tt-menus.
DEF BUFFER b-aux       FOR tt-menus.

DEF VAR r-rowid        AS ROWID.
DEF VAR i-nr-seq       AS INT.
DEF VAR l-new-record   AS LOG.
DEF VAR l-ok           AS LOG.
DEF VAR c-tipos        AS CHAR INIT "M,R,S,P".
DEF VAR c-linha        AS CHAR FORMAT "x(200)".
DEF VAR c-arquivo      AS CHAR FORMAT "x(30)".

DEFINE VARIABLE i-bt  AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-col AS DECIMAL     NO-UNDO.
DEFINE VARIABLE i-row AS DECIMAL     NO-UNDO.

DEFINE VARIABLE i-usr    AS INTEGER     NO-UNDO.
DEFINE VARIABLE pos-pont AS INTEGER     NO-UNDO.
DEFINE VARIABLE pos-barr AS INTEGER     NO-UNDO.

DEF VAR wh-bt AS HANDLE EXTENT 100.

DEFINE VARIABLE var-confirmar-salvar AS LOGICAL     NO-UNDO.
DEFINE VARIABLE var-alterado AS LOGICAL     NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME br-men

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-menus b-programas b-rotinas b-submenus

/* Definitions for BROWSE br-men                                        */
&Scoped-define FIELDS-IN-QUERY-br-men tt-menus.c-nom-men   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-men   
&Scoped-define SELF-NAME br-men
&Scoped-define QUERY-STRING-br-men FOR EACH tt-menus WHERE                                  tt-menus.c-tipo = "M" NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-men OPEN QUERY {&SELF-NAME} FOR EACH tt-menus WHERE                                  tt-menus.c-tipo = "M" NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-men tt-menus
&Scoped-define FIRST-TABLE-IN-QUERY-br-men tt-menus


/* Definitions for BROWSE br-prog                                       */
&Scoped-define FIELDS-IN-QUERY-br-prog b-programas.c-nom-prog   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-prog   
&Scoped-define SELF-NAME br-prog
&Scoped-define QUERY-STRING-br-prog FOR EACH b-programas WHERE                                  b-programas.c-nom-men = b-submenus.c-nom-men AND                                  b-programas.c-nom-rot = b-submenus.c-nom-rot AND                                  b-programas.c-nom-sub = b-submenus.c-nom-sub AND                                  b-programas.c-tipo = "P" NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-prog OPEN QUERY {&SELF-NAME} FOR EACH b-programas WHERE                                  b-programas.c-nom-men = b-submenus.c-nom-men AND                                  b-programas.c-nom-rot = b-submenus.c-nom-rot AND                                  b-programas.c-nom-sub = b-submenus.c-nom-sub AND                                  b-programas.c-tipo = "P" NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-prog b-programas
&Scoped-define FIRST-TABLE-IN-QUERY-br-prog b-programas


/* Definitions for BROWSE br-rot                                        */
&Scoped-define FIELDS-IN-QUERY-br-rot b-rotinas.c-nom-rot   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-rot   
&Scoped-define SELF-NAME br-rot
&Scoped-define QUERY-STRING-br-rot FOR EACH b-rotinas WHERE                                  b-rotinas.c-nom-men = tt-menus.c-nom-men AND                                  b-rotinas.c-tipo = "R" NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-rot OPEN QUERY {&SELF-NAME} FOR EACH b-rotinas WHERE                                  b-rotinas.c-nom-men = tt-menus.c-nom-men AND                                  b-rotinas.c-tipo = "R" NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-rot b-rotinas
&Scoped-define FIRST-TABLE-IN-QUERY-br-rot b-rotinas


/* Definitions for BROWSE br-sub                                        */
&Scoped-define FIELDS-IN-QUERY-br-sub b-submenus.c-nom-sub   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-sub   
&Scoped-define SELF-NAME br-sub
&Scoped-define QUERY-STRING-br-sub FOR EACH b-submenus WHERE                                  b-submenus.c-nom-men = b-rotinas.c-nom-men AND                                  b-submenus.c-nom-rot = b-rotinas.c-nom-rot AND                                  b-submenus.c-tipo = "S" NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-sub OPEN QUERY {&SELF-NAME} FOR EACH b-submenus WHERE                                  b-submenus.c-nom-men = b-rotinas.c-nom-men AND                                  b-submenus.c-nom-rot = b-rotinas.c-nom-rot AND                                  b-submenus.c-tipo = "S" NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-sub b-submenus
&Scoped-define FIRST-TABLE-IN-QUERY-br-sub b-submenus


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-br-men}~
    ~{&OPEN-QUERY-br-prog}~
    ~{&OPEN-QUERY-br-rot}~
    ~{&OPEN-QUERY-br-sub}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-rot br-men tg-salvar-como bt-load ~
bt-salvar fi-usuario bt-inc bt-mod bt-del bt-up bt-down bt-ajuda bt-ok ~
bt-usuario bt-atualiza-botao RECT-1 rt-buttom RECT-2 RECT-3 RECT-4 RECT-5 ~
RECT-6 br-sub RECT-7 br-prog RECT-8 
&Scoped-Define DISPLAYED-OBJECTS rs-estrutura tg-salvar-como fi-usuario ~
rs-tipo fi-desc fi-cod-prog tg-visual-bt fi-image fi-tooltip fi-salvar-como 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 rs-estrutura rs-tipo fi-desc fi-cod-prog bt-prog ~
bt-prog-2 tg-visual-bt fi-image bt-image fi-tooltip bt-can bt-conf RECT-8 
&Scoped-define List-2 tg-salvar-como bt-load bt-salvar fi-usuario bt-inc ~
bt-mod bt-del bt-up bt-down bt-ok fi-salvar-como bt-usuario bt-salvar-como 
&Scoped-define List-3 fi-cod-prog bt-prog bt-prog-2 tg-visual-bt bt-inc ~
bt-mod bt-del 
&Scoped-define List-4 fi-image bt-image fi-tooltip 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ajuda 
     IMAGE-UP FILE "image/im-hel.bmp":U
     LABEL "&Ajuda" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-atualiza-botao 
     IMAGE-UP FILE "image/im-chck1.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Atualizar".

DEFINE BUTTON bt-can 
     IMAGE-UP FILE "image/im-cancel.bmp":U
     LABEL "bt can" 
     SIZE 4 BY 1.13 TOOLTIP "Cancela Manutená‰es".

DEFINE BUTTON bt-conf 
     IMAGE-UP FILE "image\im-chck1.bmp":U
     LABEL "Button 8" 
     SIZE 4 BY 1.13 TOOLTIP "Confirma Manutená‰es".

DEFINE BUTTON bt-del 
     IMAGE-UP FILE "image/im-era.bmp":U
     LABEL "Button 2" 
     SIZE 4 BY 1.13 TOOLTIP "Elimina Menu Selecionado".

DEFINE BUTTON bt-down 
     IMAGE-UP FILE "image/im-dw.bmp":U
     LABEL "Button 4" 
     SIZE 4 BY 1.13 TOOLTIP "Mover para Baixo".

DEFINE BUTTON bt-image 
     IMAGE-UP FILE "image/im-open.bmp":U
     LABEL "Button 7" 
     SIZE 4 BY 1 TOOLTIP "Abrir imagem".

DEFINE BUTTON bt-inc 
     IMAGE-UP FILE "image/im-new.bmp":U
     LABEL "Button 1" 
     SIZE 4 BY 1.13 TOOLTIP "Inclui novo Menu".

DEFINE BUTTON bt-load 
     IMAGE-UP FILE "image/im-chck1.bmp":U
     LABEL "bt-load" 
     SIZE 4 BY 1 TOOLTIP "Atualizar".

DEFINE BUTTON bt-mod 
     IMAGE-UP FILE "image/im-mod.bmp":U
     LABEL "Button 9" 
     SIZE 4 BY 1.13 TOOLTIP "Modifica Menu Selecionado".

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "&OK" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-prog 
     IMAGE-UP FILE "image/fields-i4.bmp":U
     LABEL "Button 10" 
     SIZE 4 BY 1 TOOLTIP "Abrir programas cadastrados".

DEFINE BUTTON bt-prog-2 
     IMAGE-UP FILE "image/im-open.bmp":U
     LABEL "bt-prog 2" 
     SIZE 4 BY 1 TOOLTIP "Abrir outros arquivos".

DEFINE BUTTON bt-salvar 
     IMAGE-UP FILE "image/im-save.bmp":U
     LABEL "BT Salvar" 
     SIZE 4 BY 1 TOOLTIP "Salva".

DEFINE BUTTON bt-salvar-como 
     IMAGE-UP FILE "image/im-open.bmp":U
     LABEL "bt salvar como" 
     SIZE 4 BY 1 TOOLTIP "Buscar Usu†rio".

DEFINE BUTTON bt-up 
     IMAGE-UP FILE "image/im-up.bmp":U
     LABEL "Button 3" 
     SIZE 4 BY 1.13 TOOLTIP "Mover para Acima".

DEFINE BUTTON bt-usuario 
     IMAGE-UP FILE "image/im-open.bmp":U
     LABEL "bt usuario" 
     SIZE 4 BY 1 TOOLTIP "Buscar Usu†rio".

DEFINE VARIABLE fi-cod-prog AS CHARACTER FORMAT "X(256)":U 
     LABEL "C¢digo do Programa" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc AS CHARACTER FORMAT "X(40)":U 
     LABEL "Descricao do Menu" 
     VIEW-AS FILL-IN 
     SIZE 47 BY .88 NO-UNDO.

DEFINE VARIABLE fi-image AS CHARACTER FORMAT "X(256)":U 
     LABEL "R¢tulo/Imagem" 
     VIEW-AS FILL-IN 
     SIZE 38 BY .88 NO-UNDO.

DEFINE VARIABLE fi-salvar-como AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 TOOLTIP "Usu†rio destino" NO-UNDO.

DEFINE VARIABLE fi-tooltip AS CHARACTER FORMAT "X(50)":U 
     LABEL "Mensagem de Ajuda" 
     VIEW-AS FILL-IN 
     SIZE 31 BY .88 NO-UNDO.

DEFINE VARIABLE fi-usuario AS CHARACTER FORMAT "X(256)":U 
     LABEL "Usu†iro" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE rs-estrutura AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Programa", 1,
"Menu", 2,
"RÇgua", 3
     SIZE 45.29 BY .75 NO-UNDO.

DEFINE VARIABLE rs-tipo AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Menu", 1,
"Rotina", 2,
"SubMenu", 3,
"Procedimento", 4
     SIZE 45.14 BY .71 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 100 BY 10.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 100 BY 1.75.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 30.72 BY 1.75.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 19.57 BY 18.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 8  NO-FILL   
     SIZE 19.57 BY 1.75
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 47 BY 1.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 47 BY 1.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 61 BY 2.75.

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 120 BY 1.42
     BGCOLOR 7 .

DEFINE VARIABLE tg-salvar-como AS LOGICAL INITIAL no 
     LABEL "Salvar como:" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .88 NO-UNDO.

DEFINE VARIABLE tg-visual-bt AS LOGICAL INITIAL no 
     LABEL "Visualizar como Bot∆o" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .58 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-men FOR 
      tt-menus SCROLLING.

DEFINE QUERY br-prog FOR 
      b-programas SCROLLING.

DEFINE QUERY br-rot FOR 
      b-rotinas SCROLLING.

DEFINE QUERY br-sub FOR 
      b-submenus SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-men
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-men C-Win _FREEFORM
  QUERY br-men NO-LOCK DISPLAY
      tt-menus.c-nom-men COLUMN-LABEL "Descriá∆o"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 24 BY 9.5
         FONT 1
         TITLE "Menus" FIT-LAST-COLUMN.

DEFINE BROWSE br-prog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-prog C-Win _FREEFORM
  QUERY br-prog NO-LOCK DISPLAY
      b-programas.c-nom-prog COLUMN-LABEL "Descriá∆o"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 23 BY 9.5
         FONT 1
         TITLE "Procedimentos" FIT-LAST-COLUMN.

DEFINE BROWSE br-rot
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-rot C-Win _FREEFORM
  QUERY br-rot NO-LOCK DISPLAY
      b-rotinas.c-nom-rot COLUMN-LABEL "Descriá∆o"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 24 BY 9.5
         FONT 1
         TITLE "Rotinas" FIT-LAST-COLUMN.

DEFINE BROWSE br-sub
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-sub C-Win _FREEFORM
  QUERY br-sub NO-LOCK DISPLAY
      b-submenus.c-nom-sub COLUMN-LABEL "Descriá∆o"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 24 BY 9.5
         FONT 1
         TITLE "SubMenus" FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     br-rot AT ROW 3 COL 27.29
     br-men AT ROW 3 COL 2.29
     rs-estrutura AT ROW 13.08 COL 28 NO-LABEL WIDGET-ID 34
     tg-salvar-como AT ROW 1.5 COL 38.43 WIDGET-ID 22
     bt-load AT ROW 1.46 COL 27.29 WIDGET-ID 8
     bt-salvar AT ROW 1.46 COL 32.72 WIDGET-ID 14
     fi-usuario AT ROW 1.5 COL 6 COLON-ALIGNED WIDGET-ID 6
     rs-tipo AT ROW 14.17 COL 27.86 NO-LABEL
     fi-desc AT ROW 15.21 COL 25.43 COLON-ALIGNED
     fi-cod-prog AT ROW 16.21 COL 25.43 COLON-ALIGNED
     bt-prog AT ROW 16.17 COL 45.86
     bt-prog-2 AT ROW 16.17 COL 50 WIDGET-ID 2
     tg-visual-bt AT ROW 17.5 COL 20
     fi-image AT ROW 18.25 COL 33.43 COLON-ALIGNED
     bt-image AT ROW 18.17 COL 74.43
     fi-tooltip AT ROW 19.25 COL 33.43 COLON-ALIGNED
     bt-inc AT ROW 21.08 COL 33
     bt-mod AT ROW 21.08 COL 37.14
     bt-del AT ROW 21.08 COL 41.14
     bt-can AT ROW 21.08 COL 45.43
     bt-conf AT ROW 21.08 COL 49.57
     bt-up AT ROW 21.08 COL 57.14
     bt-down AT ROW 21.08 COL 61.43
     bt-ajuda AT ROW 21.21 COL 110
     bt-ok AT ROW 21.21 COL 2.29
     fi-salvar-como AT ROW 1.5 COL 48.14 COLON-ALIGNED NO-LABEL WIDGET-ID 18
     bt-usuario AT ROW 1.46 COL 23.29 WIDGET-ID 30
     bt-salvar-como AT ROW 1.46 COL 66.43 WIDGET-ID 32
     bt-atualiza-botao AT ROW 1.46 COL 115.72 WIDGET-ID 46
     br-sub AT ROW 3 COL 52.29
     br-prog AT ROW 3 COL 77.29
     "Administrar:" VIEW-AS TEXT
          SIZE 8.14 BY .75 AT ROW 14.13 COL 19.14
     "Menu" VIEW-AS TEXT
          SIZE 5 BY .75 AT ROW 1.58 COL 113 RIGHT-ALIGNED WIDGET-ID 26
          FONT 0
     "Tipo:" VIEW-AS TEXT
          SIZE 3.29 BY .54 AT ROW 13.21 COL 23.29 WIDGET-ID 38
     RECT-1 AT ROW 2.75 COL 1
     rt-buttom AT ROW 20.96 COL 1.29
     RECT-2 AT ROW 1 COL 1.29 WIDGET-ID 10
     RECT-3 AT ROW 1 COL 1.29 WIDGET-ID 12
     RECT-4 AT ROW 2.75 COL 101.72 WIDGET-ID 24
     RECT-5 AT ROW 1.08 COL 101.72 WIDGET-ID 28
     RECT-6 AT ROW 12.96 COL 27.43 WIDGET-ID 40
     RECT-7 AT ROW 14.04 COL 27.43 WIDGET-ID 42
     RECT-8 AT ROW 17.75 COL 19 WIDGET-ID 44
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 120.72 BY 21.5
         FONT 1.

DEFINE FRAME fr-botao
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         NO-LABELS NO-UNDERLINE THREE-D 
         AT COL 102.14 ROW 2.88
         SIZE 18 BY 17.63
         FONT 1 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Manutená∆o de Menus - szt002"
         HEIGHT             = 21.5
         WIDTH              = 120.72
         MAX-HEIGHT         = 31.25
         MAX-WIDTH          = 194.29
         VIRTUAL-HEIGHT     = 31.25
         VIRTUAL-WIDTH      = 194.29
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("adeicon/icfdcu.ico":U) THEN
    MESSAGE "Unable to load icon: adeicon/icfdcu.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME fr-botao:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB br-rot 1 DEFAULT-FRAME */
/* BROWSE-TAB br-men br-rot DEFAULT-FRAME */
/* BROWSE-TAB br-sub RECT-6 DEFAULT-FRAME */
/* BROWSE-TAB br-prog RECT-7 DEFAULT-FRAME */
/* SETTINGS FOR BUTTON bt-can IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR BUTTON bt-conf IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR BUTTON bt-del IN FRAME DEFAULT-FRAME
   2 3                                                                  */
/* SETTINGS FOR BUTTON bt-down IN FRAME DEFAULT-FRAME
   2                                                                    */
/* SETTINGS FOR BUTTON bt-image IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 4                                                        */
/* SETTINGS FOR BUTTON bt-inc IN FRAME DEFAULT-FRAME
   2 3                                                                  */
/* SETTINGS FOR BUTTON bt-load IN FRAME DEFAULT-FRAME
   2                                                                    */
/* SETTINGS FOR BUTTON bt-mod IN FRAME DEFAULT-FRAME
   2 3                                                                  */
/* SETTINGS FOR BUTTON bt-ok IN FRAME DEFAULT-FRAME
   2                                                                    */
/* SETTINGS FOR BUTTON bt-prog IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 3                                                        */
/* SETTINGS FOR BUTTON bt-prog-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 3                                                        */
/* SETTINGS FOR BUTTON bt-salvar IN FRAME DEFAULT-FRAME
   2                                                                    */
/* SETTINGS FOR BUTTON bt-salvar-como IN FRAME DEFAULT-FRAME
   NO-ENABLE 2                                                          */
/* SETTINGS FOR BUTTON bt-up IN FRAME DEFAULT-FRAME
   2                                                                    */
/* SETTINGS FOR BUTTON bt-usuario IN FRAME DEFAULT-FRAME
   2                                                                    */
/* SETTINGS FOR FILL-IN fi-cod-prog IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 3                                                        */
/* SETTINGS FOR FILL-IN fi-desc IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN fi-image IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 4                                                        */
/* SETTINGS FOR FILL-IN fi-salvar-como IN FRAME DEFAULT-FRAME
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi-tooltip IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 4                                                        */
/* SETTINGS FOR FILL-IN fi-usuario IN FRAME DEFAULT-FRAME
   2                                                                    */
/* SETTINGS FOR RECTANGLE RECT-8 IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR RADIO-SET rs-estrutura IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR RADIO-SET rs-tipo IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR TOGGLE-BOX tg-salvar-como IN FRAME DEFAULT-FRAME
   2                                                                    */
/* SETTINGS FOR TOGGLE-BOX tg-visual-bt IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 3                                                        */
/* SETTINGS FOR TEXT-LITERAL "Menu"
          SIZE 5 BY .75 AT ROW 1.58 COL 113 RIGHT-ALIGNED               */

/* SETTINGS FOR FRAME fr-botao
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-men
/* Query rebuild information for BROWSE br-men
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-menus WHERE
                                 tt-menus.c-tipo = "M" NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-men */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-prog
/* Query rebuild information for BROWSE br-prog
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH b-programas WHERE
                                 b-programas.c-nom-men = b-submenus.c-nom-men AND
                                 b-programas.c-nom-rot = b-submenus.c-nom-rot AND
                                 b-programas.c-nom-sub = b-submenus.c-nom-sub AND
                                 b-programas.c-tipo = "P" NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-prog */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-rot
/* Query rebuild information for BROWSE br-rot
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH b-rotinas WHERE
                                 b-rotinas.c-nom-men = tt-menus.c-nom-men AND
                                 b-rotinas.c-tipo = "R" NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-rot */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-sub
/* Query rebuild information for BROWSE br-sub
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH b-submenus WHERE
                                 b-submenus.c-nom-men = b-rotinas.c-nom-men AND
                                 b-submenus.c-nom-rot = b-rotinas.c-nom-rot AND
                                 b-submenus.c-tipo = "S" NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-sub */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fr-botao
/* Query rebuild information for FRAME fr-botao
     _Query            is NOT OPENED
*/  /* FRAME fr-botao */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Manutená∆o de Menus - szt002 */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Manutená∆o de Menus - szt002 */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-men
&Scoped-define SELF-NAME br-men
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-men C-Win
ON MOUSE-SELECT-CLICK OF br-men IN FRAME DEFAULT-FRAME /* Menus */
DO:
  ASSIGN rs-tipo:SCREEN-VALUE = '1'
         fi-desc:LABEL = "Descriá∆o do Menu".

  RUN pi-cor (INPUT 1).
  APPLY 'VALUE-CHANGED' TO SELF.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-men C-Win
ON VALUE-CHANGED OF br-men IN FRAME DEFAULT-FRAME /* Menus */
DO:
   RUN pi-limpa.
   {&OPEN-QUERY-br-rot}
   APPLY 'VALUE-CHANGED' TO br-rot.

   IF l-new-record = NO THEN DO.
      IF AVAIL tt-menus THEN DO.
         RUN pi-estrutura.
    
         IF rs-tipo:SCREEN-VALUE = '1' THEN
            ASSIGN fi-desc:SCREEN-VALUE = tt-menus.c-nom-men
                   fi-cod-prog:SCREEN-VALUE = tt-menus.c-cod-prog
                   tg-visual-bt:SCREEN-VALUE = IF tt-menus.c-icone = "S" THEN "YES" ELSE "NO"
                   fi-image:SCREEN-VALUE = tt-menus.c-image 
                   fi-tooltip:SCREEN-VALUE = tt-menus.c-tooltip.
    
         /*APPLY 'VALUE-CHANGED' TO rs-tipo.*/
      END.
      /*ASSIGN bt-inc:SENSITIVE = YES 
             bt-mod:SENSITIVE = AVAIL tt-menus AND tg-regua:SCREEN-VALUE = "NO"
             bt-del:SENSITIVE = AVAIL tt-menus.*/
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-prog
&Scoped-define SELF-NAME br-prog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-prog C-Win
ON MOUSE-SELECT-CLICK OF br-prog IN FRAME DEFAULT-FRAME /* Procedimentos */
DO:
  ASSIGN rs-tipo:SCREEN-VALUE = '4'
         fi-desc:LABEL = "Descriá∆o do Programa".

  RUN pi-cor (INPUT 4).

  APPLY 'VALUE-CHANGED' TO SELF.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-prog C-Win
ON VALUE-CHANGED OF br-prog IN FRAME DEFAULT-FRAME /* Procedimentos */
DO:
   RUN pi-limpa.

   IF l-new-record = NO THEN DO.
      IF AVAIL b-programas THEN DO.
         RUN pi-estrutura. 
                                                                                            
         IF rs-tipo:SCREEN-VALUE = '4' THEN
            ASSIGN fi-desc:SCREEN-VALUE = b-programas.c-nom-prog
                   fi-cod-prog:SCREEN-VALUE = b-programas.c-cod-prog
                   tg-visual-bt:SCREEN-VALUE = IF b-programas.c-icone = "S" THEN "YES" ELSE "NO"
                   fi-image:SCREEN-VALUE = b-programas.c-image 
                   fi-tooltip:SCREEN-VALUE = b-programas.c-tooltip.
    
         /*APPLY 'VALUE-CHANGED' TO rs-tipo.*/
      END.

     /* ASSIGN bt-inc:SENSITIVE = AVAIL b-submenus
             bt-mod:SENSITIVE = AVAIL b-submenus AND tg-regua:SCREEN-VALUE = "NO"
             bt-del:SENSITIVE = AVAIL b-submenus.*/
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-rot
&Scoped-define SELF-NAME br-rot
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-rot C-Win
ON MOUSE-SELECT-CLICK OF br-rot IN FRAME DEFAULT-FRAME /* Rotinas */
DO:
    ASSIGN rs-tipo:SCREEN-VALUE IN FRAME {&FRAME-NAME}= '2'
           fi-desc:LABEL = "Descriá∆o da Rotina".

    RUN pi-cor (INPUT 2).

    APPLY 'VALUE-CHANGED' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-rot C-Win
ON VALUE-CHANGED OF br-rot IN FRAME DEFAULT-FRAME /* Rotinas */
DO:
  RUN pi-limpa.

  {&OPEN-QUERY-br-sub}
  APPLY 'VALUE-CHANGED' TO br-sub.

  IF l-new-record = NO THEN DO.
     IF AVAIL b-rotinas THEN DO.
        RUN pi-estrutura.
        IF rs-tipo:SCREEN-VALUE = '2' THEN
           ASSIGN fi-desc:SCREEN-VALUE = b-rotinas.c-nom-rot
                  fi-cod-prog:SCREEN-VALUE = b-rotinas.c-cod-prog
                  tg-visual-bt:SCREEN-VALUE = IF b-rotinas.c-icone = "S" THEN "YES" ELSE "NO"
                  fi-image:SCREEN-VALUE = b-rotinas.c-image 
                  fi-tooltip:SCREEN-VALUE = b-rotinas.c-tooltip.
    
        /*APPLY 'VALUE-CHANGED' TO rs-tipo.*/
     END.
/*
     ASSIGN bt-inc:SENSITIVE = AVAIL tt-menus
            bt-mod:SENSITIVE = AVAIL tt-menus AND tg-regua:SCREEN-VALUE = "NO"
            bt-del:SENSITIVE = AVAIL tt-menus.*/
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-sub
&Scoped-define SELF-NAME br-sub
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-sub C-Win
ON MOUSE-SELECT-CLICK OF br-sub IN FRAME DEFAULT-FRAME /* SubMenus */
DO:
  ASSIGN rs-tipo:SCREEN-VALUE = '3' 
         fi-desc:LABEL = "Descriá∆o do SubMenu".

  RUN pi-cor (INPUT 3).

  APPLY 'VALUE-CHANGED' TO SELF.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-sub C-Win
ON VALUE-CHANGED OF br-sub IN FRAME DEFAULT-FRAME /* SubMenus */
DO:
  RUN pi-limpa.
  
  {&OPEN-QUERY-br-prog}
  APPLY 'VALUE-CHANGED' TO br-prog.

  IF l-new-record = NO THEN DO.
     IF AVAIL b-submenus THEN DO.
        RUN pi-estrutura.
        IF rs-tipo:SCREEN-VALUE = '3' THEN
           ASSIGN fi-desc:SCREEN-VALUE = b-submenus.c-nom-sub
                  fi-cod-prog:SCREEN-VALUE = b-submenus.c-cod-prog
                  tg-visual-bt:SCREEN-VALUE = IF b-submenus.c-icone = "S" THEN "YES" ELSE "NO"
                  fi-image:SCREEN-VALUE = b-submenus.c-image 
                  fi-tooltip:SCREEN-VALUE = b-submenus.c-tooltip.
    
        /*APPLY 'VALUE-CHANGED' TO rs-tipo.*/
     END.
     /*ASSIGN bt-inc:SENSITIVE = AVAIL b-rotinas
            bt-mod:SENSITIVE = AVAIL b-rotinas AND tg-regua:SCREEN-VALUE = "NO"
            bt-del:SENSITIVE = AVAIL b-rotinas.*/
  END.
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


&Scoped-define SELF-NAME bt-atualiza-botao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-atualiza-botao C-Win
ON CHOOSE OF bt-atualiza-botao IN FRAME DEFAULT-FRAME
DO:
  RUN pi-botao.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-can
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-can C-Win
ON CHOOSE OF bt-can IN FRAME DEFAULT-FRAME /* bt can */
DO:
  
   DISABLE {&list-1} WITH FRAME {&FRAME-NAME}.
   ENABLE {&list-2} WITH FRAME {&FRAME-NAME}.
   
   ASSIGN l-new-record = NO.

   APPLY "VALUE-CHANGED" TO rs-tipo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-conf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-conf C-Win
ON CHOOSE OF bt-conf IN FRAME DEFAULT-FRAME /* Button 8 */
DO:
   ASSIGN var-alterado = YES.
   IF l-new-record THEN DO.
      FIND LAST b-aux NO-ERROR.
      ASSIGN i-nr-seq = IF AVAIL b-aux
                        THEN b-aux.i-seq + 1
                        ELSE 1.

      CREATE b-aux.
      ASSIGN b-aux.i-seq  = i-nr-seq         
             b-aux.c-tipo = ENTRY(INT(rs-tipo:SCREEN-VALUE),c-tipos).
   END.
   ELSE DO.
      CASE rs-tipo:SCREEN-VALUE.
           WHEN "1" THEN DO.
              FIND b-aux WHERE 
                   ROWID(b-aux) = ROWID(tt-menus) NO-LOCK NO-ERROR.
              ASSIGN r-rowid = ROWID(tt-menus).
              FOR EACH tt-menus WHERE
                       tt-menus.c-nom-men = b-aux.c-nom-men.
                  ASSIGN tt-menus.c-nom-men = fi-desc:SCREEN-VALUE.
              END.
           END.
           WHEN "2" THEN DO.
               FIND b-aux WHERE 
                    ROWID(b-aux) = ROWID(b-rotinas) NO-LOCK NO-ERROR.
               ASSIGN r-rowid = ROWID(b-rotinas).
               FOR EACH b-rotinas WHERE
                        b-rotinas.c-nom-men = b-aux.c-nom-men AND
                        b-rotinas.c-nom-rot = b-aux.c-nom-rot.
                   ASSIGN b-rotinas.c-nom-rot = fi-desc:SCREEN-VALUE.
               END.
           END.
           WHEN "3" THEN DO.
               FIND b-aux WHERE 
                    ROWID(b-aux) = ROWID(b-submenus) NO-LOCK NO-ERROR.
               ASSIGN r-rowid = ROWID(b-submenus).
               FOR EACH b-submenus WHERE
                        b-submenus.c-nom-men = b-aux.c-nom-men AND
                        b-submenus.c-nom-rot = b-aux.c-nom-rot AND
                        b-submenus.c-nom-sub = b-aux.c-nom-sub.
                   ASSIGN b-submenus.c-nom-sub = fi-desc:SCREEN-VALUE.
               END.
           END.
           WHEN "4" THEN 
               ASSIGN r-rowid = ROWID(b-programas).
      END CASE.
      FIND b-aux WHERE 
           ROWID(b-aux) = r-rowid NO-LOCK NO-ERROR.
   END.

   ASSIGN b-aux.c-nom-men  = IF b-aux.c-tipo = "M" 
                             THEN fi-desc:SCREEN-VALUE
                             ELSE IF AVAIL tt-menus
                                  THEN tt-menus.c-nom-men
                                  ELSE ""
          b-aux.c-nom-rot  = IF b-aux.c-tipo = "R"      
                             THEN fi-desc:SCREEN-VALUE 
                             ELSE IF AVAIL b-rotinas 
                                  THEN b-rotinas.c-nom-rot
                                  ELSE ""
          b-aux.c-nom-sub  = IF b-aux.c-tipo = "S"     
                             THEN fi-desc:SCREEN-VALUE 
                             ELSE IF AVAIL b-submenus
                                  THEN b-submenus.c-nom-sub
                                  ELSE ""
          b-aux.c-nom-prog = IF b-aux.c-tipo = "P"     
                             THEN fi-desc:SCREEN-VALUE 
                             ELSE IF AVAIL b-programas
                                  THEN b-programas.c-nom-prog
                                  ELSE ""
          b-aux.c-cod-prog = fi-cod-prog:SCREEN-VALUE     
          b-aux.c-icone    = IF tg-visual-bt:INPUT-VALUE = "YES" 
                             THEN "S" ELSE ""
          b-aux.c-image    = fi-image:SCREEN-VALUE     
          b-aux.c-tooltip  = fi-tooltip:SCREEN-VALUE.      

   DISABLE {&list-1} WITH FRAME {&FRAME-NAME}.
   ENABLE {&list-2} WITH FRAME {&FRAME-NAME}.

   ASSIGN l-new-record = NO.

   CASE b-aux.c-tipo.
      WHEN 'M' THEN DO.
          {&OPEN-QUERY-br-men}
           APPLY 'MOUSE-SELECT-CLICK' TO br-men.
      END.
      WHEN 'R' THEN DO.
          {&OPEN-QUERY-br-rot}
          APPLY 'MOUSE-SELECT-CLICK' TO br-rot.
      END.
      WHEN 'S' THEN DO.
          {&OPEN-QUERY-br-sub}
          APPLY 'MOUSE-SELECT-CLICK' TO br-sub.
      END.
      WHEN 'P' THEN DO.
          {&OPEN-QUERY-br-prog}
          APPLY 'MOUSE-SELECT-CLICK' TO br-prog.
      END.
   END CASE.
   RUN pi-botao.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-del
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-del C-Win
ON CHOOSE OF bt-del IN FRAME DEFAULT-FRAME /* Button 2 */
DO:
   CASE rs-tipo:SCREEN-VALUE.
      WHEN '1' THEN DO.
          FOR EACH tt-menus WHERE
                   tt-menus.c-nom-men = fi-desc:SCREEN-VALUE.
              DELETE tt-menus.
          END.
          {&OPEN-QUERY-br-men}
           APPLY 'MOUSE-SELECT-CLICK' TO br-men.
      END.
      WHEN '2' THEN DO.
          FOR EACH b-rotinas WHERE
                   b-rotinas.c-nom-rot = fi-desc:SCREEN-VALUE.
              DELETE b-rotinas.
          END.
          {&OPEN-QUERY-br-rot}
          APPLY 'MOUSE-SELECT-CLICK' TO br-rot.
      END.
      WHEN '3' THEN DO.
          FOR EACH b-submenus WHERE
                   b-submenus.c-nom-sub = fi-desc:SCREEN-VALUE.
              DELETE b-submenus.
          END.
          {&OPEN-QUERY-br-sub}
          APPLY 'MOUSE-SELECT-CLICK' TO br-sub.
      END.
      WHEN '4' THEN DO.
          DELETE b-programas.
          {&OPEN-QUERY-br-prog}
          APPLY 'MOUSE-SELECT-CLICK' TO br-prog.
      END.
   END.
   RUN pi-botao.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-down
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-down C-Win
ON CHOOSE OF bt-down IN FRAME DEFAULT-FRAME /* Button 4 */
DO:
   RUN pi-move (INPUT "DOWN").
   /*RUN pi-botao.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-image
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-image C-Win
ON CHOOSE OF bt-image IN FRAME DEFAULT-FRAME /* Button 7 */
DO:
  SYSTEM-DIALOG GET-FILE fi-image
        TITLE      "Escolha o Programa"
        FILTERS    "BMP e PNG (*.bmp,*.png)"  "*.bmp, *.png",
                   "Windows Bitmap" "*.bmp",
                   "Portable Network Graphics (*.png)" "*.png",
                   "Todos (*.*)"   "*.*"
        MUST-EXIST
        USE-FILENAME
        UPDATE l-ok.
      
    IF l-ok = TRUE THEN
       ASSIGN fi-image:SCREEN-VALUE = fi-image.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-inc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-inc C-Win
ON CHOOSE OF bt-inc IN FRAME DEFAULT-FRAME /* Button 1 */
DO:
    /*ASSIGN rs-tipo:SENSITIVE = YES
           rs-tipo:SENSITIVE = YES
           fi-desc:SENSITIVE = YES
           tg-regua:SENSITIVE = YES
           l-new-record = YES.
   
    RUN pi-limpa.

    ENABLE {&list-2} WITH FRAME {&FRAME-NAME}.
    ENABLE {&list-4} WITH FRAME {&FRAME-NAME}.

    DISABLE {&list-3} WITH FRAME {&FRAME-NAME}.*/
    ENABLE {&list-1} WITH FRAME {&FRAME-NAME}.
    DISABLE {&list-2} WITH FRAME {&FRAME-NAME}.
    
    APPLY 'entry' TO fi-desc.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-load
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-load C-Win
ON CHOOSE OF bt-load IN FRAME DEFAULT-FRAME /* bt-load */
DO:
   IF var-alterado THEN
      RUN pi-save.

   RUN pi-load.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-mod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-mod C-Win
ON CHOOSE OF bt-mod IN FRAME DEFAULT-FRAME /* Button 9 */
DO:
    ENABLE {&list-1} WITH FRAME {&FRAME-NAME}.
    DISABLE {&list-2} WITH FRAME {&FRAME-NAME}.

    ASSIGN l-new-record = NO.

    DISABLE rs-tipo WITH FRAME {&FRAME-NAME}.
    DISABLE rs-estrutura WITH FRAME {&FRAME-NAME}.

    IF rs-estrutura:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "2" THEN
       DISABLE {&list-3} WITH FRAME {&FRAME-NAME}. /* Codigo do Programa e tg-visual-bt*/
    
    IF tg-visual-bt:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "no" THEN
       DISABLE {&list-4} WITH FRAME {&FRAME-NAME}. /* Dados do botao*/
    
    APPLY 'entry' TO fi-desc.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok C-Win
ON CHOOSE OF bt-ok IN FRAME DEFAULT-FRAME /* OK */
DO:
   IF var-alterado THEN 
      RUN pi-save.

   APPLY 'CLOSE' TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-prog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-prog C-Win
ON CHOOSE OF bt-prog IN FRAME DEFAULT-FRAME /* Button 10 */
DO:
  RUN seniuz\sztools\szt001z.w.
  ASSIGN fi-cod-prog:SCREEN-VALUE = RETURN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-prog-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-prog-2 C-Win
ON CHOOSE OF bt-prog-2 IN FRAME DEFAULT-FRAME /* bt-prog 2 */
DO:
  SYSTEM-DIALOG GET-FILE fi-cod-prog
        TITLE      "Escolha o Programa"
        FILTERS    "AppBuilder (*.w,*.p)"  "*.w, *.p",
                   "Todos (*.*)"   "*.*"
        MUST-EXIST
        USE-FILENAME
        UPDATE l-ok.
      
    IF l-ok = TRUE THEN
       ASSIGN fi-cod-prog:SCREEN-VALUE = fi-cod-prog.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-salvar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-salvar C-Win
ON CHOOSE OF bt-salvar IN FRAME DEFAULT-FRAME /* BT Salvar */
DO:
  RUN pi-save.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-salvar-como
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-salvar-como C-Win
ON CHOOSE OF bt-salvar-como IN FRAME DEFAULT-FRAME /* bt salvar como */
DO:
  SYSTEM-DIALOG GET-FILE fi-salvar-como
        TITLE      "Escolha o Usu†rio"
        FILTERS    "Arquivo de Configuraá∆o"  "*.ini"
        MUST-EXIST
        INITIAL-DIR "M:\ems206\especificos\Seniuz\SzTools\ini" 
        USE-FILENAME
        UPDATE l-ok.
      
    IF l-ok = TRUE THEN DO.
        DO i-usr = 1 TO LENGTH(fi-salvar-como).
           IF SUBSTRING(fi-salvar-como,i-usr,1) = "." THEN DO.
              ASSIGN pos-pont = i-usr.
           END.
           IF SUBSTRING(fi-salvar-como,i-usr,1) = "/" OR SUBSTRING(fi-salvar-como,i-usr,1) = "\" THEN
              ASSIGN pos-barr = i-usr.
        END.
        ASSIGN fi-salvar-como:SCREEN-VALUE IN FRAME {&FRAME-NAME} = SUBSTRING(fi-salvar-como,pos-barr + 1,pos-pont - pos-barr - 1).
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-up
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-up C-Win
ON CHOOSE OF bt-up IN FRAME DEFAULT-FRAME /* Button 3 */
DO:
   RUN pi-move (INPUT "UP").
  /* RUN pi-botao.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-usuario
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-usuario C-Win
ON CHOOSE OF bt-usuario IN FRAME DEFAULT-FRAME /* bt usuario */
DO:
   IF var-alterado THEN
      RUN pi-save.

   SYSTEM-DIALOG GET-FILE fi-usuario
        TITLE      "Escolha o Usu†rio"
        FILTERS    "Arquivo de Configuraá∆o"  "*.ini"
        MUST-EXIST
        INITIAL-DIR "M:\ems206\especificos\Seniuz\SzTools\ini" 
        USE-FILENAME
        UPDATE l-ok.
      
    IF l-ok = TRUE THEN DO.
        DO i-usr = 1 TO LENGTH(fi-usuario).
           IF SUBSTRING(fi-usuario,i-usr,1) = "." THEN DO.
              ASSIGN pos-pont = i-usr.
           END.
           IF SUBSTRING(fi-usuario,i-usr,1) = "/" OR SUBSTRING(fi-usuario,i-usr,1) = "\" THEN
              ASSIGN pos-barr = i-usr.
        END.
        ASSIGN fi-usuario:SCREEN-VALUE IN FRAME {&FRAME-NAME} = SUBSTRING(fi-usuario,pos-barr + 1,pos-pont - pos-barr - 1).
    END.

    RUN pi-load.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-prog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-prog C-Win
ON LEAVE OF fi-cod-prog IN FRAME DEFAULT-FRAME /* C¢digo do Programa */
DO:
  FIND FIRST prog_dtsul WHERE prog_dtsul.cod_prog_dtsul = fi-cod-prog:SCREEN-VALUE IN FRAME {&FRAME-NAME} NO-LOCK NO-ERROR.
    IF AVAIL prog_dtsul THEN
       ASSIGN fi-desc:SCREEN-VALUE IN FRAME {&FRAME-NAME} = prog_dtsul.des_prog_dtsul WHEN fi-desc:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
              fi-cod-prog:SCREEN-VALUE IN FRAME {&FRAME-NAME} = prog_dtsul.nom_prog_ext.
    /*ELSE     
       if  search (tt-menus.c-cod-prog) = ? and search(substring(tt-menus.c-cod-prog,1,r-index(tt-menus.c-cod-prog,".")) + "r") = ?
        then do:
            run utp/ut-msgs.p (input "show",
                               input 4,
                               input fi-cod-prog:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
            /*leave search_block.*/
        end.
        ELSE
           ASSIGN fi-cod-prog:SCREEN-VALUE IN FRAME {&FRAME-NAME} = tt-menus.c-cod-prog.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-estrutura
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-estrutura C-Win
ON VALUE-CHANGED OF rs-estrutura IN FRAME DEFAULT-FRAME
DO:
  IF SELF:INPUT-VALUE = 3 THEN DO.
      DISABLE {&list-4} WITH FRAME {&FRAME-NAME}.
      ASSIGN fi-desc:SENSITIVE = NO.
      ASSIGN fi-desc:SCREEN-VALUE = "Regua".
  END.
  IF SELF:INPUT-VALUE = 2 THEN DO.        
      DISABLE {&list-4} WITH FRAME {&FRAME-NAME}.
     
      ASSIGN fi-desc:SENSITIVE = YES.
      ASSIGN fi-desc:SCREEN-VALUE = "".
   END.
   IF SELF:INPUT-VALUE = 1 THEN DO.        
      ENABLE {&list-4} WITH FRAME {&FRAME-NAME}.
      ASSIGN fi-desc:SENSITIVE = YES.
      ASSIGN fi-desc:SCREEN-VALUE = "".
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-tipo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-tipo C-Win
ON VALUE-CHANGED OF rs-tipo IN FRAME DEFAULT-FRAME
DO:
    RUN pi-cor (INPUT SELF:INPUT-VALUE).
    
    IF NUM-RESULTS('br-men') = 0 AND SELF:SCREEN-VALUE > '1' THEN
       ASSIGN SELF:SCREEN-VALUE = '1'.
    ELSE 
        IF NUM-RESULTS('br-rot') = 0 AND SELF:SCREEN-VALUE > '2' THEN
           ASSIGN SELF:SCREEN-VALUE = '2'.
        ELSE 
           IF NUM-RESULTS('br-sub') = 0 AND SELF:SCREEN-VALUE > '3' THEN
              ASSIGN SELF:SCREEN-VALUE = '3'.
    
    CASE SELF:SCREEN-VALUE.
        WHEN '1' THEN DO.
            APPLY 'MOUSE-SELECT-CLICK' TO br-men.
            ASSIGN fi-cod-prog:SENSITIVE = bt-conf:SENSITIVE AND
                                           NOT CAN-FIND(FIRST b-rotinas WHERE
                                                              b-rotinas.c-nom-men = tt-menus.c-nom-men AND
                                                              b-rotinas.c-tipo = "R").
            IF fi-cod-prog:SENSITIVE OR l-new-record THEN
        END.
        WHEN '2' THEN DO.
            APPLY 'MOUSE-SELECT-CLICK' TO br-rot.
            ASSIGN fi-cod-prog:SENSITIVE = bt-conf:SENSITIVE AND
                                           NOT CAN-FIND(FIRST b-submenus WHERE
                                                              b-submenus.c-nom-men = b-rotinas.c-nom-men AND
                                                              b-submenus.c-nom-rot = b-rotinas.c-nom-rot AND
                                                              b-submenus.c-tipo = "S").
            IF fi-cod-prog:SENSITIVE OR l-new-record THEN
        END.
        WHEN '3' THEN DO.
            APPLY 'MOUSE-SELECT-CLICK' TO br-sub.
            ASSIGN fi-cod-prog:SENSITIVE = bt-conf:SENSITIVE AND 
                                           NOT CAN-FIND(FIRST b-programas WHERE
                                                              b-programas.c-nom-men = b-submenus.c-nom-men AND
                                                              b-programas.c-nom-rot = b-submenus.c-nom-rot AND
                                                              b-programas.c-nom-sub = b-submenus.c-nom-sub AND
                                                              b-programas.c-tipo = "P").
            IF fi-cod-prog:SENSITIVE OR l-new-record THEN
        END.
        WHEN '4' THEN DO.
            IF bt-conf:SENSITIVE OR l-new-record THEN

            APPLY 'MOUSE-SELECT-CLICK' TO br-prog.
        END.
    END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-salvar-como
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-salvar-como C-Win
ON VALUE-CHANGED OF tg-salvar-como IN FRAME DEFAULT-FRAME /* Salvar como: */
DO:
  fi-salvar-como:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
  IF INPUT FRAME {&frame-name} tg-salvar-como THEN DO.
     ENABLE fi-salvar-como WITH FRAME {&FRAME-NAME}.
     ENABLE bt-salvar-como WITH FRAME {&FRAME-NAME}.
  END.
  ELSE DO.
     DISABLE fi-salvar-como WITH FRAME {&FRAME-NAME}.
     DISABLE bt-salvar-como WITH FRAME {&FRAME-NAME}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-visual-bt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-visual-bt C-Win
ON VALUE-CHANGED OF tg-visual-bt IN FRAME DEFAULT-FRAME /* Visualizar como Bot∆o */
DO:
    ASSIGN fi-image:SENSITIVE = SELF:INPUT-VALUE
           bt-image:SENSITIVE = SELF:INPUT-VALUE
           fi-tooltip:SENSITIVE = SELF:INPUT-VALUE.
    ASSIGN fi-tooltip:SCREEN-VALUE IN FRAME {&FRAME-NAME} = fi-desc:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-men
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

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.

  APPLY 'VALUE-CHANGED' TO br-men IN FRAME {&FRAME-NAME}.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

ASSIGN fi-usuario:SCREEN-VALUE IN FRAME {&frame-name} = LC(TRIM(OS-GETENV("username"))).
      /* fi-salvar-como:SCREEN-VALUE IN FRAME {&frame-name} = LC(TRIM(OS-GETENV("username"))).*/

RUN pi-load.

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
  DISPLAY rs-estrutura tg-salvar-como fi-usuario rs-tipo fi-desc fi-cod-prog 
          tg-visual-bt fi-image fi-tooltip fi-salvar-como 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE br-rot br-men tg-salvar-como bt-load bt-salvar fi-usuario bt-inc 
         bt-mod bt-del bt-up bt-down bt-ajuda bt-ok bt-usuario 
         bt-atualiza-botao RECT-1 rt-buttom RECT-2 RECT-3 RECT-4 RECT-5 RECT-6 
         br-sub RECT-7 br-prog RECT-8 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW FRAME fr-botao IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-fr-botao}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-botao C-Win 
PROCEDURE pi-botao :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


  DO i-bt = 1 TO 33.
     IF VALID-HANDLE(wh-bt[i-bt]) THEN DO.
        DELETE WIDGET wh-bt[i-bt].
     END.
  END.
 
  ASSIGN i-bt = 1.
  FOR EACH tt-menus WHERE
           tt-menus.c-icone = 'S' NO-LOCK.

      IF i-bt = 33 THEN LEAVE.

      ASSIGN i-col = IF i-bt MODULO 3 = 0
                     THEN 14
                     ELSE ((i-bt MODULO 3) * 6) - 4 
             i-row = (TRUNC(i-bt / 3.1,0) + 1) * 1.4.

      CREATE BUTTON wh-bt[i-bt]
             ASSIGN FRAME = FRAME fr-botao:HANDLE
                    NO-FOCUS = YES
                    FLAT-BUTTON  = YES
                    WIDTH        = 4.86
                    HEIGHT       = 1.25
                    ROW          = i-row
                    COL          = i-col
                    VISIBLE      = YES
                    SENSITIVE    = YES
                    LABEL        = tt-menus.c-image
                    TOOLTIP      = tt-menus.c-tooltip
                    TRIGGERS:
                         ON "CHOOSE":U PERSISTENT RUN VALUE(tt-menus.c-cod-prog).
                    END TRIGGERS.
                    
    
      IF SEARCH(tt-menus.c-image) <> ? THEN
         wh-bt[i-bt]:LOAD-IMAGE-UP(tt-menus.c-image).

      ASSIGN i-bt = i-bt + 1.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cor C-Win 
PROCEDURE pi-cor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER p-browse AS INT.
    ASSIGN br-men:BGCOLOR IN FRAME {&FRAME-NAME} = ?
           br-rot:BGCOLOR IN FRAME {&FRAME-NAME} = ?
           br-sub:BGCOLOR IN FRAME {&FRAME-NAME} = ?
           br-prog:BGCOLOR IN FRAME {&FRAME-NAME} = ?.

    CASE p-browse.
        WHEN 1 THEN ASSIGN br-men:BGCOLOR = 8.
        WHEN 2 THEN ASSIGN br-rot:BGCOLOR = 8.
        WHEN 3 THEN ASSIGN br-sub:BGCOLOR = 8.
        WHEN 4 THEN ASSIGN br-prog:BGCOLOR = 8.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-display C-Win 
PROCEDURE pi-display :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DISABLE rs-estrutura WITH FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-estrutura C-Win 
PROCEDURE pi-estrutura :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF AVAIL tt-menus THEN DO.
   IF tt-menus.c-nom-men = 'Regua' THEN 
      ASSIGN rs-estrutura:SCREEN-VALUE IN FRAME {&FRAME-NAME}= "3".
   ELSE 
      IF tt-menus.c-cod-prog = '' THEN 
         ASSIGN rs-estrutura:SCREEN-VALUE IN FRAME {&FRAME-NAME}= "2". 
      ELSE
         ASSIGN rs-estrutura:SCREEN-VALUE IN FRAME {&FRAME-NAME}= "1". 
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-limpa C-Win 
PROCEDURE pi-limpa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN rs-tipo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "1"
           fi-desc:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
           fi-cod-prog:SCREEN-VALUE = ""
           tg-visual-bt:SCREEN-VALUE = "NO"
           fi-image:SCREEN-VALUE = ""
           fi-tooltip:SCREEN-VALUE = "".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-load C-Win 
PROCEDURE pi-load :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

ASSIGN c-arquivo = "m:\ems206\especificos\seniuz\sztools\ini\" + fi-usuario:SCREEN-VALUE IN FRAME {&frame-name} + ".ini".

empty temp-table tt-menus    .
empty temp-table b-rotinas   .
empty temp-table b-submenus  .
empty temp-table b-programas .
empty temp-table b-aux       .


IF SEARCH(c-arquivo) <> ? THEN DO.
   INPUT FROM VALUE(c-arquivo) NO-ECHO CONVERT SOURCE "iso8859-1".
   REPEAT WITH WIDTH 550.
      CREATE tt-menus.
      IMPORT DELIMITER "|" tt-menus.
   END.
   INPUT CLOSE.
END.
/*else do:
    output to value(c-arquivo).
    output close.
end.*/

{&OPEN-QUERY-br-men}
{&OPEN-QUERY-br-prog}
{&OPEN-QUERY-br-rot} 
{&OPEN-QUERY-br-sub}
RUN pi-botao.

fi-salvar-como:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-move C-Win 
PROCEDURE pi-move :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF INPUT PARAMETER p-direcao AS CHAR.

   CASE rs-tipo:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
      WHEN '1' THEN DO.
          FIND b-aux WHERE
               b-aux.i-seq = tt-menus.i-seq NO-ERROR.

          IF p-direcao = "UP" THEN
             FIND PREV b-aux WHERE
                       b-aux.c-tipo = "M" NO-ERROR.
          ELSE
             FIND NEXT b-aux WHERE
                       b-aux.c-tipo = "M" NO-ERROR.

          IF NOT AVAIL b-aux THEN RETURN.

          ASSIGN i-nr-seq = b-aux.i-seq.

          ASSIGN b-aux.i-seq = tt-menus.i-seq
                 tt-menus.i-seq = i-nr-seq.

          FIND b-aux WHERE
               b-aux.i-seq = tt-menus.i-seq NO-ERROR.

          {&OPEN-QUERY-br-men}

          REPOSITION br-men TO ROWID ROWID(b-aux).
      END.
      WHEN '2' THEN DO.
          FIND b-aux WHERE
               b-aux.i-seq = b-rotinas.i-seq NO-ERROR.

          IF p-direcao = "UP" THEN
             FIND PREV b-aux WHERE
                       b-aux.c-nom-men = b-rotinas.c-nom-men AND
                       b-aux.c-tipo = "R" NO-LOCK.
          ELSE
             FIND NEXT b-aux WHERE
                       b-aux.c-nom-men = b-rotinas.c-nom-men AND
                       b-aux.c-tipo = "R" NO-LOCK.

          IF NOT AVAIL b-aux THEN RETURN.

          ASSIGN i-nr-seq = b-aux.i-seq.

          ASSIGN b-aux.i-seq = b-rotinas.i-seq
                 b-rotinas.i-seq = i-nr-seq.

          FIND b-aux WHERE
               b-aux.i-seq = b-rotinas.i-seq NO-ERROR.

          {&OPEN-QUERY-br-rot}

          REPOSITION br-rot TO ROWID ROWID(b-aux).
      END.
      WHEN '3' THEN DO.
          FIND b-aux WHERE
               b-aux.i-seq = b-submenus.i-seq NO-ERROR.

          IF p-direcao = "UP" THEN
             FIND PREV b-aux WHERE
                       b-aux.c-nom-men = b-submenus.c-nom-men AND
                       b-aux.c-nom-rot = b-submenus.c-nom-rot AND
                       b-aux.c-tipo = "S" NO-LOCK.
          ELSE
             FIND NEXT b-aux WHERE
                       b-aux.c-nom-men = b-submenus.c-nom-men AND
                       b-aux.c-nom-rot = b-submenus.c-nom-rot AND
                       b-aux.c-tipo = "S" NO-LOCK.

          IF NOT AVAIL b-aux THEN RETURN.

          ASSIGN i-nr-seq = b-aux.i-seq.

          ASSIGN b-aux.i-seq = b-submenus.i-seq
                 b-submenus.i-seq = i-nr-seq.

          FIND b-aux WHERE
               b-aux.i-seq = b-submenus.i-seq NO-ERROR.

          {&OPEN-QUERY-br-sub}

          REPOSITION br-sub TO ROWID ROWID(b-aux).
      END.
      WHEN '4' THEN DO.
          FIND b-aux WHERE
               b-aux.i-seq = b-programas.i-seq NO-ERROR.

          IF p-direcao = "UP" THEN
             FIND PREV b-aux WHERE
                       b-aux.c-nom-men = b-programas.c-nom-men AND
                       b-aux.c-nom-rot = b-programas.c-nom-rot AND
                       b-aux.c-nom-sub = b-programas.c-nom-sub AND
                       b-aux.c-tipo = "P" NO-LOCK.
          ELSE
             FIND NEXT b-aux WHERE
                       b-aux.c-nom-men = b-programas.c-nom-men AND
                       b-aux.c-nom-rot = b-programas.c-nom-rot AND
                       b-aux.c-nom-sub = b-programas.c-nom-sub AND
                       b-aux.c-tipo = "P" NO-LOCK.

          IF NOT AVAIL b-aux THEN RETURN.

          ASSIGN i-nr-seq = b-aux.i-seq.

          ASSIGN b-aux.i-seq = b-programas.i-seq
                 b-programas.i-seq = i-nr-seq.

          FIND b-aux WHERE
               b-aux.i-seq = b-programas.i-seq NO-ERROR.

          {&OPEN-QUERY-br-prog}

          REPOSITION br-prog TO ROWID ROWID(b-aux).
      END.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-save C-Win 
PROCEDURE pi-save :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF INPUT FRAME {&frame-name} tg-salvar-como THEN DO.
     ASSIGN c-arquivo = "m:\ems206\especificos\seniuz\sztools\ini\" + fi-salvar-como:SCREEN-VALUE IN FRAME {&frame-name} + ".ini".
     MESSAGE "Deseja salvar em: " fi-salvar-como:SCREEN-VALUE IN FRAME {&frame-name}
             VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL 
             TITLE "Salvar Como" UPDATE var-confirmar-salvar.
  END.
  ELSE DO.
     ASSIGN c-arquivo = "m:\ems206\especificos\seniuz\sztools\ini\" + fi-usuario:SCREEN-VALUE IN FRAME {&frame-name} + ".ini".
     MESSAGE "Deseja salvar em: " fi-usuario:SCREEN-VALUE IN FRAME {&frame-name}
             VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL 
             TITLE "Salvar" UPDATE var-confirmar-salvar.
  END.
  IF var-confirmar-salvar THEN DO:
     OUTPUT TO VALUE(c-arquivo) CONVERT TARGET "iso8859-1".
        FOR EACH b-aux.
            PUT UNFORMATTED 
                b-aux.i-seq      FORMAT "999"   "|"
                b-aux.c-tipo                    "|"
                b-aux.c-nom-men                 "|"
                b-aux.c-nom-rot                 "|"
                b-aux.c-nom-sub                 "|"
                b-aux.c-nom-prog                "|"
                b-aux.c-cod-prog                "|"
                b-aux.c-icone                   "|"
                b-aux.c-image                   "|"
                b-aux.c-tooltip                 "|"
                SKIP.
        END.
      OUTPUT CLOSE.
      ASSIGN var-alterado = NO.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

