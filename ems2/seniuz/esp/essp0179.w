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
&Scoped-Define ENABLED-OBJECTS RECT-31 fi-num-etiqueta bt-etiqueta ~
fi-num-teares bt-confirma bt-desfaz bt-sair 
&Scoped-Define DISPLAYED-OBJECTS fi-num-etiqueta fi-num-teares 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-confirma 
     LABEL "Confirmar" 
     SIZE 15 BY 1.13.

DEFINE BUTTON bt-desfaz 
     LABEL "Desfazer" 
     SIZE 15 BY 1.13.

DEFINE BUTTON bt-etiqueta 
     IMAGE-UP FILE "image/im-vapra.bmp":U
     LABEL "Button 5" 
     SIZE 4 BY .88.

DEFINE BUTTON bt-sair 
     LABEL "Sair" 
     SIZE 15 BY 1.13.

DEFINE VARIABLE fi-num-etiqueta AS INTEGER FORMAT ">>>,>>9":U INITIAL 0 
     LABEL "N£mero Etiqueta" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 TOOLTIP "N£mero da etiqueta gerada na montagem da peáa."
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-num-teares AS CHARACTER FORMAT "X(60)":U 
     LABEL "N£mero Teares" 
     VIEW-AS FILL-IN 
     SIZE 64 BY .88 TOOLTIP "N£meros dos teares que produziram a peáa, separados por v°rgula.".

DEFINE RECTANGLE RECT-31
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 79.29 BY 4.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fi-num-etiqueta AT ROW 1.46 COL 14 COLON-ALIGNED
     bt-etiqueta AT ROW 1.46 COL 24.43
     fi-num-teares AT ROW 2.46 COL 14 COLON-ALIGNED
     bt-confirma AT ROW 3.75 COL 6
     bt-desfaz AT ROW 3.75 COL 22.72
     bt-sair AT ROW 3.75 COL 46
     RECT-31 AT ROW 1.21 COL 1.43
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 79.86 BY 4.38
         FONT 1.


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
         TITLE              = "Inclus∆o de Etiquetas de Pano Cru"
         HEIGHT             = 4.38
         WIDTH              = 79.86
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 80
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
ON END-ERROR OF C-Win /* Inclus∆o de Etiquetas de Pano Cru */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Inclus∆o de Etiquetas de Pano Cru */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-confirma
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-confirma C-Win
ON CHOOSE OF bt-confirma IN FRAME DEFAULT-FRAME /* Confirmar */
DO:
  FIND etiq-pano-cru WHERE etiq-pano-cru.num-etiqueta = INPUT FRAME {&FRAME-NAME} fi-num-etiqueta
                  NO-LOCK NO-ERROR.
  IF AVAIL etiq-pano-cru THEN DO:
     MESSAGE "Etiqueta j† foi inclu°da. N«O PODE SER INCLU÷DA NOVAMENTE."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     APPLY 'entry' TO fi-num-etiqueta IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END.
  ELSE DO:
     IF INPUT FRAME {&FRAME-NAME} fi-num-teares = "" THEN DO:
        MESSAGE "Informe os n£meros dos Teares que produziram a peáa."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY 'entry' TO fi-num-teares IN FRAME {&FRAME-NAME}.
        RETURN NO-APPLY.
     END.
     ELSE DO:
        CREATE etiq-pano-cru.
        ASSIGN etiq-pano-cru.num-etiqueta = INPUT FRAME {&FRAME-NAME} fi-num-etiqueta
               etiq-pano-cru.num-teares   = CAPS(INPUT FRAME {&FRAME-NAME} fi-num-teares)
               etiq-pano-cru.data-trans   = TODAY.
        MESSAGE "Etiqueta inclu°da com sucesso!"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        ASSIGN fi-num-etiqueta:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
               fi-num-teares:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
        APPLY 'entry' TO fi-num-etiqueta IN FRAME {&FRAME-NAME}.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-desfaz
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-desfaz C-Win
ON CHOOSE OF bt-desfaz IN FRAME DEFAULT-FRAME /* Desfazer */
DO:
   ASSIGN fi-num-etiqueta:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
          fi-num-teares:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
   APPLY 'entry' TO fi-num-etiqueta IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-etiqueta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-etiqueta C-Win
ON CHOOSE OF bt-etiqueta IN FRAME DEFAULT-FRAME /* Button 5 */
DO:
  FIND etiq-pano-cru WHERE etiq-pano-cru.num-etiqueta = INPUT FRAME {&FRAME-NAME} fi-num-etiqueta
                  NO-LOCK NO-ERROR.
  IF AVAIL etiq-pano-cru THEN DO:
     MESSAGE "Etiqueta j† foi inclu°da. N«O PODE SER INCLU÷DA NOVAMENTE."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     APPLY 'entry' TO fi-num-etiqueta IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END.
  ELSE DO.
     APPLY 'entry' TO fi-num-teares IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-sair
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-sair C-Win
ON CHOOSE OF bt-sair IN FRAME DEFAULT-FRAME /* Sair */
DO:
  apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-num-etiqueta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-num-etiqueta C-Win
ON RETURN OF fi-num-etiqueta IN FRAME DEFAULT-FRAME /* N£mero Etiqueta */
DO:
   APPLY 'TAB' TO SELF.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-num-etiqueta C-Win
ON TAB OF fi-num-etiqueta IN FRAME DEFAULT-FRAME /* N£mero Etiqueta */
DO:
  APPLY 'choose' TO bt-etiqueta.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-num-teares
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-num-teares C-Win
ON LEAVE OF fi-num-teares IN FRAME DEFAULT-FRAME /* N£mero Teares */
DO:
  ASSIGN SELF:SCREEN-VALUE = UPPER(SELF:SCREEN-VALUE).
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
  DISPLAY fi-num-etiqueta fi-num-teares 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-31 fi-num-etiqueta bt-etiqueta fi-num-teares bt-confirma 
         bt-desfaz bt-sair 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

