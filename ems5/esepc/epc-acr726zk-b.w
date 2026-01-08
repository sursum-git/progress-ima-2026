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
/*          This .W file was created with the Progress AppBulder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE NEW GLOBAL SHARED VARIABLE da-dt-debito       AS DATE FORMAT "99/99/9999".
DEFINE NEW GLOBAL SHARED VARIABLE da-dt-credito      AS DATE FORMAT "99/99/9999".
DEFINE NEW GLOBAL SHARED VARIABLE i-diferenca       AS INTEGER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-4 Btn_OK 
&Scoped-Define DISPLAYED-OBJECTS dt-debito dt-credito txt-diferenca 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 10 BY 1
     BGCOLOR 8 FONT 1.

DEFINE VARIABLE dt-credito AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 12.29 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE dt-debito AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 12.29 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE txt-diferenca AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 25.29 BY .88
     FONT 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 44 BY 4.13.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 44 BY 1.46
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     dt-debito AT ROW 2.71 COL 13.72 COLON-ALIGNED NO-LABEL
     dt-credito AT ROW 2.71 COL 26.72 COLON-ALIGNED NO-LABEL
     txt-diferenca AT ROW 3.83 COL 13.72 COLON-ALIGNED NO-LABEL
     Btn_OK AT ROW 5.92 COL 2.86
     "Data M‚dia:" VIEW-AS TEXT
          SIZE 8.14 BY .88 AT ROW 2.71 COL 7.14
          FONT 1
     "Diferen‡a:" VIEW-AS TEXT
          SIZE 7 BY .88 AT ROW 3.83 COL 14.43 RIGHT-ALIGNED
          FONT 1
     "D‚bito" VIEW-AS TEXT
          SIZE 9 BY .63 AT ROW 1.96 COL 24 RIGHT-ALIGNED
          FONT 1
     "Cr‚dito" VIEW-AS TEXT
          SIZE 9 BY .63 AT ROW 1.96 COL 37 RIGHT-ALIGNED
          FONT 1
     RECT-1 AT ROW 1.38 COL 2
     RECT-4 AT ROW 5.71 COL 2
     SPACE(0.71) SKIP(0.20)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "C lculo da Data M‚dia - epc-cr726zk-b.w"
         DEFAULT-BUTTON Btn_OK.


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

/* SETTINGS FOR FILL-IN dt-credito IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN dt-debito IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN txt-diferenca IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR TEXT-LITERAL "D‚bito"
          SIZE 9 BY .63 AT ROW 1.96 COL 24 RIGHT-ALIGNED                */

/* SETTINGS FOR TEXT-LITERAL "Cr‚dito"
          SIZE 9 BY .63 AT ROW 1.96 COL 37 RIGHT-ALIGNED                */

/* SETTINGS FOR TEXT-LITERAL "Diferen‡a:"
          SIZE 7 BY .88 AT ROW 3.83 COL 14.43 RIGHT-ALIGNED             */

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON ENTRY OF FRAME Dialog-Frame /* C lculo da Data M‚dia - epc-cr726zk-b.w */
DO:
    ASSIGN dt-debito:SCREEN-VALUE      = STRING(da-dt-debito)
         dt-credito:SCREEN-VALUE     = STRING(da-dt-credito).
    IF i-diferenca = 0 THEN
        ASSIGN txt-diferenca:SCREEN-VALUE  = "NÆo h  diferen‡a"
               txt-diferenca:FGCOLOR = 0.
    ELSE DO:
        IF i-diferenca < 0 THEN DO:
            ASSIGN txt-diferenca:SCREEN-VALUE  = "Antecipado em " + STRING(i-diferenca * (-1))
                   txt-diferenca:FGCOLOR = 9.
            IF i-diferenca = -1 THEN
                ASSIGN txt-diferenca:SCREEN-VALUE = txt-diferenca:SCREEN-VALUE + " dia".
            ELSE
                ASSIGN txt-diferenca:SCREEN-VALUE = txt-diferenca:SCREEN-VALUE + " dias".
        END.
        ELSE DO:
            IF i-diferenca > 0 THEN
                ASSIGN txt-diferenca:SCREEN-VALUE  = STRING(i-diferenca)
                       txt-diferenca:FGCOLOR = 12.
            IF i-diferenca = 1 THEN
                ASSIGN txt-diferenca:SCREEN-VALUE = txt-diferenca:SCREEN-VALUE + " dia de atraso".
            ELSE
                ASSIGN txt-diferenca:SCREEN-VALUE = txt-diferenca:SCREEN-VALUE + " dias de atraso".
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* C lculo da Data M‚dia - epc-cr726zf-b.w */
DO:
  APPLY "END-ERROR":U TO SELF.
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
  DISPLAY dt-debito dt-credito txt-diferenca 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-1 RECT-4 Btn_OK 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

