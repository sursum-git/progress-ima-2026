&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
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
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

/* Variaveis para API do Windows */
DEF VAR sBuffer  AS CHAR FORMAT "x(256)" NO-UNDO.
DEF VAR lSize    AS INT INIT 255.

PROCEDURE GetUserNameA EXTERNAL "advapi32.dll":
    DEFINE OUTPUT       PARAMETER lpBuffer  AS CHARACTER.
    DEFINE INPUT-OUTPUT PARAMETER nSize AS LONG.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 rt-buttom fi-revisadeira bt-ok ~
bt-cancela bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS fi-revisador fi-revisadeira 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ajuda 
     LABEL "&Ajuda" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-cancela AUTO-END-KEY 
     LABEL "&Cancelar" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "&OK" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE fi-revisadeira AS INTEGER FORMAT ">9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1.5
     FONT 10 NO-UNDO.

DEFINE VARIABLE fi-revisador AS CHARACTER FORMAT "X(12)" 
     VIEW-AS FILL-IN 
     SIZE 36 BY 1.5
     FONT 10 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 62 BY 5.

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 62 BY 1.42
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     fi-revisador AT ROW 2 COL 25 COLON-ALIGNED NO-LABEL
     fi-revisadeira AT ROW 4 COL 25 COLON-ALIGNED NO-LABEL
     bt-ok AT ROW 6.75 COL 3
     bt-cancela AT ROW 6.75 COL 15
     bt-ajuda AT ROW 6.75 COL 52
     "Revisadeira:" VIEW-AS TEXT
          SIZE 24 BY 1.5 AT ROW 4 COL 3
          FONT 10
     "Revisador:" VIEW-AS TEXT
          SIZE 20 BY 1.5 AT ROW 2 COL 25 RIGHT-ALIGNED
          FONT 10
     RECT-6 AT ROW 1.25 COL 2
     rt-buttom AT ROW 6.5 COL 2
     SPACE(0.99) SKIP(0.24)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Seleciona Revisadeira".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fi-revisador IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR TEXT-LITERAL "Revisador:"
          SIZE 20 BY 1.5 AT ROW 2 COL 25 RIGHT-ALIGNED                  */

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Seleciona Revisadeira */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda Dialog-Frame
ON CHOOSE OF bt-ajuda IN FRAME Dialog-Frame /* Ajuda */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cancela
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancela Dialog-Frame
ON CHOOSE OF bt-cancela IN FRAME Dialog-Frame /* Cancelar */
DO:
   RETURN '0'.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok Dialog-Frame
ON CHOOSE OF bt-ok IN FRAME Dialog-Frame /* OK */
DO:
  IF fi-revisadeira:INPUT-VALUE < 1 OR
     fi-revisadeira:INPUT-VALUE > 11 THEN DO.
     MESSAGE 'Revisadeira n∆o Cadastrada....'
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     APPLY 'ENTRY' TO fi-revisadeira.
     RETURN NO-APPLY.
  END.

  ASSIGN c-seg-usuario = fi-revisador:SCREEN-VALUE.

  RETURN STRING(fi-revisadeira:INPUT-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-revisadeira
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-revisadeira Dialog-Frame
ON RETURN OF fi-revisadeira IN FRAME Dialog-Frame
DO:
   APPLY 'choose' TO bt-ok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.

  RUN GetUserNameA (OUTPUT sBuffer, INPUT-OUTPUT lSize).

  ASSIGN fi-revisador:SCREEN-VALUE  = TRIM(sBuffer).

  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  DISPLAY fi-revisador fi-revisadeira 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-6 rt-buttom fi-revisadeira bt-ok bt-cancela bt-ajuda 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

