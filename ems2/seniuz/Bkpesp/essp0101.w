&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          espec            PROGRESS
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
DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

DEF NEW SHARED VAR i-rev AS INT.
DEF VAR h-query AS HANDLE.
DEF VAR v-row-table AS ROWID.
DEF VAR c-busca AS CHAR.
DEF VAR c-desc-item AS CHAR.
DEF VAR c-cod-estabel   AS CHAR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME br-ob

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ordem-benefic

/* Definitions for BROWSE br-ob                                         */
&Scoped-define FIELDS-IN-QUERY-br-ob ordem-benefic.nr-ob ~
ordem-benefic.nr-carro fn-desc-item(ordem-benefic.it-codigo) @ c-desc-item ~
ordem-benefic.cod-refer ordem-benefic.quantidade 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-ob 
&Scoped-define QUERY-STRING-br-ob FOR EACH ordem-benefic ~
      WHERE ordem-benefic.cod-estabel = c-cod-estabel AND ~
(espec.ordem-benefic.situacao = 1 OR  ~
 ordem-benefic.situacao = 3) NO-LOCK ~
    BY ordem-benefic.nr-ob ~
       BY ordem-benefic.nr-carro INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-ob OPEN QUERY br-ob FOR EACH ordem-benefic ~
      WHERE ordem-benefic.cod-estabel = c-cod-estabel AND ~
(espec.ordem-benefic.situacao = 1 OR  ~
 ordem-benefic.situacao = 3) NO-LOCK ~
    BY ordem-benefic.nr-ob ~
       BY ordem-benefic.nr-carro INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-ob ordem-benefic
&Scoped-define FIRST-TABLE-IN-QUERY-br-ob ordem-benefic


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-br-ob}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-buttom br-ob fi-observacao btn-revisao ~
bt-cancela bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS fi-observacao 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-desc-item C-Win 
FUNCTION fn-desc-item RETURNS CHARACTER
    ( p-item AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ajuda 
     LABEL "&Ajuda" 
     SIZE 14 BY 1.75
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-cancela AUTO-END-KEY 
     LABEL "&Sair" 
     SIZE 10 BY 1.75
     BGCOLOR 8 FONT 10.

DEFINE BUTTON btn-revisao AUTO-GO 
     LABEL "&Revisar" 
     SIZE 15 BY 1.75
     BGCOLOR 8 FONT 10.

DEFINE VARIABLE fi-observacao AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 118 BY 6
     FGCOLOR 12 FONT 10 NO-UNDO.

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 119.14 BY 2.25
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-ob FOR 
      ordem-benefic SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-ob
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-ob C-Win _STRUCTURED
  QUERY br-ob NO-LOCK DISPLAY
      ordem-benefic.nr-ob FORMAT ">>>,>>9":U WIDTH 14.43 COLUMN-FONT 10
      ordem-benefic.nr-carro FORMAT "9XX":U WIDTH 8.43 COLUMN-FONT 10
      fn-desc-item(ordem-benefic.it-codigo) @ c-desc-item COLUMN-LABEL "Descriá∆o do Item" FORMAT "x(50)":U
            WIDTH 55 COLUMN-FONT 11
      ordem-benefic.cod-refer FORMAT "X(10)":U WIDTH 12 COLUMN-FONT 11
      ordem-benefic.quantidade FORMAT "->>>,>>9.99":U WIDTH 15
            COLUMN-FONT 11
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 118 BY 14.5 ROW-HEIGHT-CHARS 1.17.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     br-ob AT ROW 1.25 COL 2
     fi-observacao AT ROW 16.08 COL 2 NO-LABEL
     btn-revisao AT ROW 22.63 COL 3
     bt-cancela AT ROW 22.63 COL 19.14
     bt-ajuda AT ROW 22.63 COL 106
     "    <F5>-Atualizar         <ENTER>-Revisar        <ESC>-Sair" VIEW-AS TEXT
          SIZE 75 BY 1.67 AT ROW 22.63 COL 30
          FONT 0
     rt-buttom AT ROW 22.33 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 120.14 BY 23.79
         DEFAULT-BUTTON btn-revisao.


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
         TITLE              = "Revis∆o de Acabados - ESSP0101"
         COLUMN             = 12.43
         ROW                = 6.04
         HEIGHT             = 23.79
         WIDTH              = 120.14
         MAX-HEIGHT         = 29.79
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 29.79
         VIRTUAL-WIDTH      = 146.29
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
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
/* BROWSE-TAB br-ob rt-buttom DEFAULT-FRAME */
ASSIGN 
       fi-observacao:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-ob
/* Query rebuild information for BROWSE br-ob
     _TblList          = "espec.ordem-benefic"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "espec.ordem-benefic.nr-ob|yes,espec.ordem-benefic.nr-carro|yes"
     _Where[1]         = "espec.ordem-benefic.cod-estabel = c-cod-estabel AND
(espec.ordem-benefic.situacao = 1 OR 
 espec.ordem-benefic.situacao = 3)"
     _FldNameList[1]   > espec.ordem-benefic.nr-ob
"ordem-benefic.nr-ob" ? ? "integer" ? ? 10 ? ? ? no ? no no "14.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > espec.ordem-benefic.nr-carro
"ordem-benefic.nr-carro" ? ? "character" ? ? 10 ? ? ? no ? no no "8.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"fn-desc-item(ordem-benefic.it-codigo) @ c-desc-item" "Descriá∆o do Item" "x(50)" ? ? ? 11 ? ? ? no ? no no "55" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > espec.ordem-benefic.cod-refer
"ordem-benefic.cod-refer" ? "X(10)" "character" ? ? 11 ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > espec.ordem-benefic.quantidade
"ordem-benefic.quantidade" ? ? "decimal" ? ? 11 ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE br-ob */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Revis∆o de Acabados - ESSP0101 */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Revis∆o de Acabados - ESSP0101 */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-ob
&Scoped-define SELF-NAME br-ob
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-ob C-Win
ON ANY-KEY OF br-ob IN FRAME DEFAULT-FRAME
DO:
  IF INDEX("1234567890",KEYFUNCTION(LASTKEY)) <> 0 THEN DO.
     ASSIGN c-busca = c-busca + KEYFUNCTION(LASTKEY).
    
     IF SUBSTR(STRING(ordem-benefic.nr-ob),1,LENGTH(c-busca)) <> c-busca THEN DO.
        FIND FIRST ordem-benefic WHERE
                   (ordem-benefic.situacao = 1 OR ordem-benefic.situacao = 3) AND
                   STRING(ordem-benefic.nr-ob) BEGINS c-busca NO-LOCK NO-ERROR.
        IF AVAIL ordem-benefic THEN DO.
           h-query:REPOSITION-TO-ROWID(ROWID(ordem-benefic)) NO-ERROR.   
           APPLY 'value-changed' TO SELF.
        END.
        ELSE DO.
           ASSIGN c-busca = "".
           FIND ordem-benefic WHERE 
                ROWID(ordem-benefic) = v-row-table NO-LOCK NO-ERROR.
        END.
     END.
     IF AVAIL ordem-benefic THEN DO.
        IF ordem-benefic.nr-ob = INT(c-busca) THEN
           ASSIGN c-busca = "".
     END.
  END.
  ELSE 
     ASSIGN c-busca = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-ob C-Win
ON MOUSE-SELECT-DBLCLICK OF br-ob IN FRAME DEFAULT-FRAME
DO:
  APPLY 'choose' TO btn-revisao.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-ob C-Win
ON RETURN OF br-ob IN FRAME DEFAULT-FRAME
DO:
  APPLY 'choose' TO btn-revisao.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-ob C-Win
ON VALUE-CHANGED OF br-ob IN FRAME DEFAULT-FRAME
DO:
  IF AVAIL ordem-benefic THEN
     ASSIGN fi-observacao:SCREEN-VALUE = ordem-benefic.observacao
            v-row-table = ROWID(ordem-benefic).
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


&Scoped-define SELF-NAME bt-cancela
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancela C-Win
ON CHOOSE OF bt-cancela IN FRAME DEFAULT-FRAME /* Sair */
DO:
   APPLY 'CLOSE':U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-revisao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-revisao C-Win
ON CHOOSE OF btn-revisao IN FRAME DEFAULT-FRAME /* Revisar */
DO:
  RUN esdlg/d01es047.w (INPUT ROWID(ordem-benefic)).

  {&OPEN-QUERY-br-ob}
  APPLY 'value-changed' TO br-ob.
  APPLY 'entry' TO br-ob.
  RETURN NO-APPLY.
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

ON 'F5':U OF br-ob DO:
   {&OPEN-QUERY-br-ob}
   APPLY 'value-changed' TO br-ob.
END.

ASSIGN h-query = br-ob:QUERY.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.

  RUN esp/essp0101a.p.
  IF RETURN-VALUE = '0' THEN DO.
     MESSAGE 'Revisadeira n∆o Informada....'
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     APPLY 'choose' TO bt-cancela.
  END.
  ASSIGN i-rev = INT(RETURN-VALUE).

  RUN esapi/busca-estab-usuar.p (INPUT c-seg-usuario,
                                 OUTPUT c-cod-estabel).

  IF c-cod-estabel = "" THEN 
     ASSIGN c-cod-estabel = '1'.

  {&OPEN-QUERY-br-ob}

  APPLY 'value-changed' TO br-ob.

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
  DISPLAY fi-observacao 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE rt-buttom br-ob fi-observacao btn-revisao bt-cancela bt-ajuda 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-desc-item C-Win 
FUNCTION fn-desc-item RETURNS CHARACTER
    ( p-item AS CHARACTER ) :

    FIND item-ext WHERE
         item-ext.it-codigo = p-item NO-LOCK NO-ERROR.

    RETURN TRIM(item-ext.descricao).
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

