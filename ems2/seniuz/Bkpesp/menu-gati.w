&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-1 RECT-1 BUTTON-10 BUTTON-2 BUTTON-4 ~
BUTTON-5 BUTTON-6 BUTTON-7 BUTTON-8 BUTTON-9 BUTTON-3 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-1  NO-FOCUS NO-CONVERT-3D-COLORS
     LABEL "Gera‡Æo Manual XML (GTNF003)" 
     SIZE 26.43 BY 1.13
     FONT 1.

DEFINE BUTTON BUTTON-10  NO-FOCUS
     LABEL "Vago" 
     SIZE 26.43 BY 1.13
     FONT 1.

DEFINE BUTTON BUTTON-2  NO-FOCUS
     LABEL "Man.Parƒmetros NF-e (GT902)" 
     SIZE 26.43 BY 1.13
     FONT 1.

DEFINE BUTTON BUTTON-3  NO-FOCUS
     LABEL "Man.Parƒmetros e-mail NF-e(GT901)" 
     SIZE 26.43 BY 1.13
     FONT 1.

DEFINE BUTTON BUTTON-4  NO-FOCUS
     LABEL "Inutiliza‡Æo NF-e (Prepar. GTNF013)" 
     SIZE 26.43 BY 1.13
     FONT 1.

DEFINE BUTTON BUTTON-5  NO-FOCUS
     LABEL "Inutiliza‡Æo NF-e (FT2201)" 
     SIZE 26.43 BY 1.13
     FONT 1.

DEFINE BUTTON BUTTON-6  NO-FOCUS
     LABEL "DANFE (FT0516AA)" 
     SIZE 26.43 BY 1.13
     FONT 1.

DEFINE BUTTON BUTTON-7  NO-FOCUS
     LABEL "Consulta Nota Fiscal (FT0904)" 
     SIZE 26.43 BY 1.13
     FONT 1.

DEFINE BUTTON BUTTON-8  NO-FOCUS
     LABEL "Vago" 
     SIZE 26.43 BY 1.13
     FONT 1.

DEFINE BUTTON BUTTON-9  NO-FOCUS
     LABEL "Vago" 
     SIZE 26.43 BY 1.13
     FONT 1.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 30 BY 16.25.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BUTTON-1 AT ROW 2.13 COL 3.86
     BUTTON-10 AT ROW 15.67 COL 3.86
     BUTTON-2 AT ROW 3.63 COL 3.86
     BUTTON-4 AT ROW 6.63 COL 3.86
     BUTTON-5 AT ROW 8.13 COL 3.86
     BUTTON-6 AT ROW 9.63 COL 3.86
     BUTTON-7 AT ROW 11.13 COL 3.86
     BUTTON-8 AT ROW 12.63 COL 3.86
     BUTTON-9 AT ROW 14.13 COL 3.86
     BUTTON-3 AT ROW 5.13 COL 3.86
     RECT-1 AT ROW 1.25 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 32.43 BY 16.88.


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
         TITLE              = "MENU GATI"
         HEIGHT             = 16.88
         WIDTH              = 32.43
         MAX-HEIGHT         = 30.04
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 30.04
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
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* MENU GATI */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* MENU GATI */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 C-Win
ON CHOOSE OF BUTTON-1 IN FRAME DEFAULT-FRAME /* Gera‡Æo Manual XML (GTNF003) */
DO:
  RUN gtp/gtnf003.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-10 C-Win
ON CHOOSE OF BUTTON-10 IN FRAME DEFAULT-FRAME /* Vago */
DO:
  /* RUN esp/essp0190.w.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 C-Win
ON CHOOSE OF BUTTON-2 IN FRAME DEFAULT-FRAME /* Man.Parƒmetros NF-e (GT902) */
DO:
   RUN gtp/gt902.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-3 C-Win
ON CHOOSE OF BUTTON-3 IN FRAME DEFAULT-FRAME /* Man.Parƒmetros e-mail NF-e(GT901) */
DO:
   RUN gtp/gt901.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-4 C-Win
ON CHOOSE OF BUTTON-4 IN FRAME DEFAULT-FRAME /* Inutiliza‡Æo NF-e (Prepar. GTNF013) */
DO:
   RUN gtp/gtnf013.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-5 C-Win
ON CHOOSE OF BUTTON-5 IN FRAME DEFAULT-FRAME /* Inutiliza‡Æo NF-e (FT2201) */
DO:
   RUN ftp/ft2201.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-6 C-Win
ON CHOOSE OF BUTTON-6 IN FRAME DEFAULT-FRAME /* DANFE (FT0516AA) */
DO:
   RUN ftp/ft0516aa.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-7 C-Win
ON CHOOSE OF BUTTON-7 IN FRAME DEFAULT-FRAME /* Consulta Nota Fiscal (FT0904) */
DO:
   RUN ftp/ft0904.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-8 C-Win
ON CHOOSE OF BUTTON-8 IN FRAME DEFAULT-FRAME /* Vago */
DO:
  /* RUN esp/essp0183.w.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-9 C-Win
ON CHOOSE OF BUTTON-9 IN FRAME DEFAULT-FRAME /* Vago */
DO:
  /* RUN esp/esft0001.w.*/
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

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
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
  ENABLE BUTTON-1 RECT-1 BUTTON-10 BUTTON-2 BUTTON-4 BUTTON-5 BUTTON-6 BUTTON-7 
         BUTTON-8 BUTTON-9 BUTTON-3 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

