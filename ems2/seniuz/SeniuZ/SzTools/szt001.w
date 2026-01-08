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

ASSIGN c-arquivo = "m:\ems206\especificos\seniuz\sztools\ini\" + LC(TRIM(OS-GETENV("username"))) + ".ini".

IF SEARCH(c-arquivo) <> ? THEN DO.
   INPUT FROM VALUE(c-arquivo) NO-ECHO CONVERT SOURCE "iso8859-1".
   REPEAT WITH WIDTH 550.
      CREATE tt-menus.
      IMPORT DELIMITER "|" tt-menus.
   END.
   INPUT CLOSE.
END.
else do:
    output to value(c-arquivo).
    output close.
end.

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
&Scoped-Define ENABLED-OBJECTS br-rot br-men bt-up bt-down bt-ajuda bt-ok ~
RECT-1 br-sub rt-buttom br-prog 
&Scoped-Define DISPLAYED-OBJECTS rs-tipo fi-desc tg-regua fi-cod-prog ~
tg-visual-bt fi-image fi-tooltip 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 rs-tipo fi-desc tg-regua 
&Scoped-define List-2 bt-can bt-conf 
&Scoped-define List-3 bt-inc bt-mod bt-del 
&Scoped-define List-4 fi-cod-prog bt-prog bt-prog-2 tg-visual-bt 
&Scoped-define List-5 fi-image bt-image fi-tooltip 

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
     SIZE 4 BY 1.

DEFINE BUTTON bt-inc 
     IMAGE-UP FILE "image/im-add.bmp":U
     LABEL "Button 1" 
     SIZE 4 BY 1.13 TOOLTIP "Inclui novo Menu".

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

DEFINE BUTTON bt-up 
     IMAGE-UP FILE "image/im-up.bmp":U
     LABEL "Button 3" 
     SIZE 4 BY 1.13 TOOLTIP "Mover para Acima".

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

DEFINE VARIABLE fi-tooltip AS CHARACTER FORMAT "X(50)":U 
     LABEL "Mensagem de Ajuda" 
     VIEW-AS FILL-IN 
     SIZE 31 BY .88 NO-UNDO.

DEFINE VARIABLE rs-tipo AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Menu", 1,
"Rotina", 2,
"SubMenu", 3,
"Procedimento", 4
     SIZE 51.14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 100 BY 16.25.

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 100 BY 1.42
     BGCOLOR 7 .

DEFINE VARIABLE tg-regua AS LOGICAL INITIAL no 
     LABEL "RÇgua" 
     VIEW-AS TOGGLE-BOX
     SIZE 8.43 BY .83 NO-UNDO.

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
     br-rot AT ROW 1.25 COL 27
     br-men AT ROW 1.25 COL 2
     rs-tipo AT ROW 10.92 COL 26.86 NO-LABEL
     fi-desc AT ROW 12 COL 25 COLON-ALIGNED
     tg-regua AT ROW 12.21 COL 74.57
     fi-cod-prog AT ROW 13 COL 25 COLON-ALIGNED
     bt-prog AT ROW 12.96 COL 45.43
     bt-prog-2 AT ROW 12.96 COL 49.57 WIDGET-ID 2
     tg-visual-bt AT ROW 14.33 COL 27
     fi-image AT ROW 15 COL 25 COLON-ALIGNED
     bt-image AT ROW 15 COL 66
     fi-tooltip AT ROW 16 COL 25 COLON-ALIGNED
     bt-inc AT ROW 17.71 COL 37.86
     bt-mod AT ROW 17.71 COL 42
     bt-del AT ROW 17.71 COL 46
     bt-can AT ROW 17.71 COL 50.29
     bt-conf AT ROW 17.71 COL 54.43
     bt-up AT ROW 17.71 COL 62
     bt-down AT ROW 17.71 COL 66.29
     bt-ajuda AT ROW 17.71 COL 90.29
     bt-ok AT ROW 17.75 COL 2
     br-sub AT ROW 1.25 COL 52
     br-prog AT ROW 1.25 COL 77
     "Administrar:" VIEW-AS TEXT
          SIZE 8.14 BY .75 AT ROW 11 COL 18.72
     RECT-1 AT ROW 1 COL 1
     rt-buttom AT ROW 17.5 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 100.43 BY 18.13
         FONT 1.


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
         TITLE              = "Manutená∆o de Menus - szt001"
         HEIGHT             = 18.13
         WIDTH              = 100.43
         MAX-HEIGHT         = 31.25
         MAX-WIDTH          = 182.86
         VIRTUAL-HEIGHT     = 31.25
         VIRTUAL-WIDTH      = 182.86
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
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB br-rot 1 DEFAULT-FRAME */
/* BROWSE-TAB br-men br-rot DEFAULT-FRAME */
/* BROWSE-TAB br-sub RECT-1 DEFAULT-FRAME */
/* BROWSE-TAB br-prog rt-buttom DEFAULT-FRAME */
/* SETTINGS FOR BUTTON bt-can IN FRAME DEFAULT-FRAME
   NO-ENABLE 2                                                          */
/* SETTINGS FOR BUTTON bt-conf IN FRAME DEFAULT-FRAME
   NO-ENABLE 2                                                          */
/* SETTINGS FOR BUTTON bt-del IN FRAME DEFAULT-FRAME
   NO-ENABLE 3                                                          */
/* SETTINGS FOR BUTTON bt-image IN FRAME DEFAULT-FRAME
   NO-ENABLE 5                                                          */
/* SETTINGS FOR BUTTON bt-inc IN FRAME DEFAULT-FRAME
   NO-ENABLE 3                                                          */
/* SETTINGS FOR BUTTON bt-mod IN FRAME DEFAULT-FRAME
   NO-ENABLE 3                                                          */
/* SETTINGS FOR BUTTON bt-prog IN FRAME DEFAULT-FRAME
   NO-ENABLE 4                                                          */
/* SETTINGS FOR BUTTON bt-prog-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-cod-prog IN FRAME DEFAULT-FRAME
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-desc IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN fi-image IN FRAME DEFAULT-FRAME
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN fi-tooltip IN FRAME DEFAULT-FRAME
   NO-ENABLE 5                                                          */
/* SETTINGS FOR RADIO-SET rs-tipo IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR TOGGLE-BOX tg-regua IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR TOGGLE-BOX tg-visual-bt IN FRAME DEFAULT-FRAME
   NO-ENABLE 4                                                          */
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

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Manutená∆o de Menus - szt001 */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Manutená∆o de Menus - szt001 */
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
         ASSIGN tg-regua:SCREEN-VALUE = IF tt-menus.c-nom-men = 'Regua' THEN "YES" ELSE "NO".
    
         IF rs-tipo:SCREEN-VALUE = '1' THEN
            ASSIGN fi-desc:SCREEN-VALUE = tt-menus.c-nom-men
                   fi-cod-prog:SCREEN-VALUE = tt-menus.c-cod-prog
                   tg-visual-bt:SCREEN-VALUE = IF tt-menus.c-icone = "S" THEN "YES" ELSE "NO"
                   fi-image:SCREEN-VALUE = tt-menus.c-image 
                   fi-tooltip:SCREEN-VALUE = tt-menus.c-tooltip.
    
         APPLY 'VALUE-CHANGED' TO rs-tipo.
      END.
      ASSIGN bt-inc:SENSITIVE = YES 
             bt-mod:SENSITIVE = AVAIL tt-menus AND tg-regua:SCREEN-VALUE = "NO"
             bt-del:SENSITIVE = AVAIL tt-menus.
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
         ASSIGN tg-regua:SCREEN-VALUE = IF b-programas.c-nom-prog = 'Regua' THEN "YES" ELSE "NO".
         IF rs-tipo:SCREEN-VALUE = '4' THEN
            ASSIGN fi-desc:SCREEN-VALUE = b-programas.c-nom-prog
                   fi-cod-prog:SCREEN-VALUE = b-programas.c-cod-prog
                   tg-visual-bt:SCREEN-VALUE = IF b-programas.c-icone = "S" THEN "YES" ELSE "NO"
                   fi-image:SCREEN-VALUE = b-programas.c-image 
                   fi-tooltip:SCREEN-VALUE = b-programas.c-tooltip.
    
         APPLY 'VALUE-CHANGED' TO rs-tipo.
      END.

      ASSIGN bt-inc:SENSITIVE = AVAIL b-submenus
             bt-mod:SENSITIVE = AVAIL b-submenus AND tg-regua:SCREEN-VALUE = "NO"
             bt-del:SENSITIVE = AVAIL b-submenus.
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
        ASSIGN tg-regua:SCREEN-VALUE = IF b-rotinas.c-nom-rot = 'Regua' THEN "YES" ELSE "NO".
        IF rs-tipo:SCREEN-VALUE = '2' THEN
           ASSIGN fi-desc:SCREEN-VALUE = b-rotinas.c-nom-rot
                  fi-cod-prog:SCREEN-VALUE = b-rotinas.c-cod-prog
                  tg-visual-bt:SCREEN-VALUE = IF b-rotinas.c-icone = "S" THEN "YES" ELSE "NO"
                  fi-image:SCREEN-VALUE = b-rotinas.c-image 
                  fi-tooltip:SCREEN-VALUE = b-rotinas.c-tooltip.
    
        APPLY 'VALUE-CHANGED' TO rs-tipo.
     END.

     ASSIGN bt-inc:SENSITIVE = AVAIL tt-menus
            bt-mod:SENSITIVE = AVAIL tt-menus AND tg-regua:SCREEN-VALUE = "NO"
            bt-del:SENSITIVE = AVAIL tt-menus.
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
        ASSIGN tg-regua:SCREEN-VALUE = IF b-submenus.c-nom-sub = 'Regua' THEN "YES" ELSE "NO".
        IF rs-tipo:SCREEN-VALUE = '3' THEN
           ASSIGN fi-desc:SCREEN-VALUE = b-submenus.c-nom-sub
                  fi-cod-prog:SCREEN-VALUE = b-submenus.c-cod-prog
                  tg-visual-bt:SCREEN-VALUE = IF b-submenus.c-icone = "S" THEN "YES" ELSE "NO"
                  fi-image:SCREEN-VALUE = b-submenus.c-image 
                  fi-tooltip:SCREEN-VALUE = b-submenus.c-tooltip.
    
        APPLY 'VALUE-CHANGED' TO rs-tipo.
     END.
     ASSIGN bt-inc:SENSITIVE = AVAIL b-rotinas
            bt-mod:SENSITIVE = AVAIL b-rotinas AND tg-regua:SCREEN-VALUE = "NO"
            bt-del:SENSITIVE = AVAIL b-rotinas.
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


&Scoped-define SELF-NAME bt-can
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-can C-Win
ON CHOOSE OF bt-can IN FRAME DEFAULT-FRAME /* bt can */
DO:
   ASSIGN fi-cod-prog:SENSITIVE = NO
          bt-prog:SENSITIVE = NO.

   DISABLE {&list-1} WITH FRAME {&FRAME-NAME}.
   DISABLE {&list-2} WITH FRAME {&FRAME-NAME}.
   DISABLE {&list-4} WITH FRAME {&FRAME-NAME}.
   DISABLE {&list-5} WITH FRAME {&FRAME-NAME}.

   ENABLE {&list-3} WITH FRAME {&FRAME-NAME}.

   ASSIGN l-new-record = NO.

   APPLY "VALUE-CHANGED" TO rs-tipo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-conf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-conf C-Win
ON CHOOSE OF bt-conf IN FRAME DEFAULT-FRAME /* Button 8 */
DO:
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

   ASSIGN fi-cod-prog:SENSITIVE = NO
          bt-prog:SENSITIVE = NO.

   DISABLE {&list-1} WITH FRAME {&FRAME-NAME}.
   DISABLE {&list-2} WITH FRAME {&FRAME-NAME}.
   DISABLE {&list-4} WITH FRAME {&FRAME-NAME}.
   DISABLE {&list-5} WITH FRAME {&FRAME-NAME}.

   ENABLE {&list-3} WITH FRAME {&FRAME-NAME}.

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
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-down
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-down C-Win
ON CHOOSE OF bt-down IN FRAME DEFAULT-FRAME /* Button 4 */
DO:
   RUN pi-move (INPUT "DOWN").
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
    ASSIGN rs-tipo:SENSITIVE = YES
           fi-desc:SENSITIVE = YES
           tg-regua:SENSITIVE = YES
           l-new-record = YES.
   
    RUN pi-limpa.

    ENABLE {&list-2} WITH FRAME {&FRAME-NAME}.
    ENABLE {&list-4} WITH FRAME {&FRAME-NAME}.

    DISABLE {&list-3} WITH FRAME {&FRAME-NAME}.

    APPLY 'entry' TO fi-desc.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-mod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-mod C-Win
ON CHOOSE OF bt-mod IN FRAME DEFAULT-FRAME /* Button 9 */
DO:
    ENABLE {&list-2} WITH FRAME {&FRAME-NAME}.
    DISABLE {&list-3} WITH FRAME {&FRAME-NAME}.

    ASSIGN rs-tipo:SENSITIVE = YES
           fi-desc:SENSITIVE = YES
           l-new-record = NO.

    IF fi-cod-prog:SCREEN-VALUE <> '' THEN DO.
       ENABLE {&list-4} WITH FRAME {&FRAME-NAME}.
       ENABLE {&list-5} WITH FRAME {&FRAME-NAME}.
    END.

    APPLY 'entry' TO fi-desc.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok C-Win
ON CHOOSE OF bt-ok IN FRAME DEFAULT-FRAME /* OK */
DO:

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


&Scoped-define SELF-NAME bt-up
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-up C-Win
ON CHOOSE OF bt-up IN FRAME DEFAULT-FRAME /* Button 3 */
DO:
   RUN pi-move (INPUT "UP").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-prog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-prog C-Win
ON LEAVE OF fi-cod-prog IN FRAME DEFAULT-FRAME /* C¢digo do Programa */
DO:
  FIND FIRST prog_dtsul WHERE prog_dtsul.cod_prog_dtsul = fi-cod-prog:SCREEN-VALUE IN FRAME {&FRAME-NAME} NO-LOCK NO-ERROR.
    IF AVAIL prog_dtsul THEN
       ASSIGN fi-cod-prog:SCREEN-VALUE IN FRAME {&FRAME-NAME} = prog_dtsul.nom_prog_ext.
   /* ELSE     
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


&Scoped-define SELF-NAME rs-tipo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-tipo C-Win
ON VALUE-CHANGED OF rs-tipo IN FRAME DEFAULT-FRAME
DO:
    RUN pi-cor (INPUT SELF:INPUT-VALUE).

    DISABLE {&list-4} WITH FRAME {&FRAME-NAME}.

    CASE SELF:SCREEN-VALUE.
        WHEN '1' THEN DO.
            APPLY 'MOUSE-SELECT-CLICK' TO br-men.
            ASSIGN fi-cod-prog:SENSITIVE = bt-conf:SENSITIVE AND
                                           NOT CAN-FIND(FIRST b-rotinas WHERE
                                                              b-rotinas.c-nom-men = tt-menus.c-nom-men AND
                                                              b-rotinas.c-tipo = "R").
            IF fi-cod-prog:SENSITIVE OR l-new-record THEN
               ENABLE {&list-4} WITH FRAME {&FRAME-NAME}.
        END.
        WHEN '2' THEN DO.
            APPLY 'MOUSE-SELECT-CLICK' TO br-rot.
            ASSIGN fi-cod-prog:SENSITIVE = bt-conf:SENSITIVE AND
                                           NOT CAN-FIND(FIRST b-submenus WHERE
                                                              b-submenus.c-nom-men = b-rotinas.c-nom-men AND
                                                              b-submenus.c-nom-rot = b-rotinas.c-nom-rot AND
                                                              b-submenus.c-tipo = "S").
            IF fi-cod-prog:SENSITIVE OR l-new-record THEN
               ENABLE {&list-4} WITH FRAME {&FRAME-NAME}.
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
               ENABLE {&list-4} WITH FRAME {&FRAME-NAME}.
        END.
        WHEN '4' THEN DO.
            IF bt-conf:SENSITIVE OR l-new-record THEN
               ENABLE {&list-4} WITH FRAME {&FRAME-NAME}.

            APPLY 'MOUSE-SELECT-CLICK' TO br-prog.
        END.
    END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-regua
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-regua C-Win
ON VALUE-CHANGED OF tg-regua IN FRAME DEFAULT-FRAME /* RÇgua */
DO:
   IF SELF:INPUT-VALUE = YES THEN DO.
      DISABLE {&list-4} WITH FRAME {&FRAME-NAME}.
      ASSIGN fi-desc:SENSITIVE = NO.
      ASSIGN fi-desc:SCREEN-VALUE = "Regua".
   END.
   ELSE DO.
       ENABLE {&list-4} WITH FRAME {&FRAME-NAME}.
       ASSIGN fi-desc:SENSITIVE = YES.
       ASSIGN fi-desc:SCREEN-VALUE = "".
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
  DISPLAY rs-tipo fi-desc tg-regua fi-cod-prog tg-visual-bt fi-image fi-tooltip 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE br-rot br-men bt-up bt-down bt-ajuda bt-ok RECT-1 br-sub rt-buttom 
         br-prog 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-limpa C-Win 
PROCEDURE pi-limpa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN fi-desc:SCREEN-VALUE IN FRAME {&FRAME-NAME}      = ""
           fi-cod-prog:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = ""
           tg-visual-bt:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO"
           fi-image:SCREEN-VALUE IN FRAME {&FRAME-NAME}     = ""
           fi-tooltip:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = "".

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

