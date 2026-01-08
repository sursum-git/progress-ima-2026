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
DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

/* Local Variable Definitions ---                                       */

DEF VAR c-form-epl  AS CHAR FORMAT "x(30)".
DEF VAR c-prog-epl  AS CHAR FORMAT "x(50)".
DEF VAR i-etq       AS INT.
DEF VAR i-tot-etq   AS INT.
DEF VAR c-seq       AS CHAR.
DEF VAR c-desc-item AS CHAR FORMAT "x(36)".
DEF VAR c-comando   AS CHAR.
DEF VAR c-impressora AS CHAR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi-rua fi-doca bt-imprime bt-sair RECT-1 
&Scoped-Define DISPLAYED-OBJECTS fi-rua fi-doca 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-imprime AUTO-GO 
     IMAGE-UP FILE "image/im-f-dv.bmp":U
     LABEL "Btn 2" 
     SIZE 6 BY 1.75 TOOLTIP "Imprime Etiqueta".

DEFINE BUTTON bt-sair AUTO-END-KEY 
     IMAGE-UP FILE "image/im-exi.bmp":U
     LABEL "Btn 2" 
     SIZE 6 BY 1.75 TOOLTIP "Sair".

DEFINE VARIABLE fi-doca AS INTEGER FORMAT "999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1.75
     FONT 20 NO-UNDO.

DEFINE VARIABLE fi-rua AS INTEGER FORMAT "999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1.75
     FONT 20 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 18 BY 3.75
     BGCOLOR 8 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fi-rua AT ROW 2 COL 27 COLON-ALIGNED NO-LABEL
     fi-doca AT ROW 4 COL 27 COLON-ALIGNED NO-LABEL
     bt-imprime AT ROW 3 COL 44.43
     bt-sair AT ROW 3 COL 51.86
     "Rua:" VIEW-AS TEXT
          SIZE 9 BY 1.5 AT ROW 2.17 COL 19
          FONT 20
     "Doca:" VIEW-AS TEXT
          SIZE 11 BY 1.5 AT ROW 4.17 COL 17
          FONT 20
     RECT-1 AT ROW 2 COL 42
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 6.08
         CANCEL-BUTTON bt-sair.


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
         TITLE              = "Emiss∆o de Etiqueta de Localizaá∆o"
         HEIGHT             = 6.08
         WIDTH              = 80
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
   FRAME-NAME Custom                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Emiss∆o de Etiqueta de Localizaá∆o */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Emiss∆o de Etiqueta de Localizaá∆o */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-imprime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-imprime C-Win
ON CHOOSE OF bt-imprime IN FRAME DEFAULT-FRAME /* Btn 2 */
DO:
    ASSIGN INPUT FRAME {&FRAME-NAME} fi-rua
           INPUT FRAME {&FRAME-NAME} fi-doca.

    IF fi-rua = 0 OR fi-doca = 0 THEN DO.
       MESSAGE 'Dados Incorretos, favor informar Rua e Doca...'
           VIEW-AS ALERT-BOX ERROR BUTTONS OK.
       APPLY 'entry' TO fi-rua.
       RETURN NO-APPLY.
    END.

    RUN pi-etiqueta. 

    ASSIGN fi-rua:SCREEN-VALUE = ''
           fi-doca:SCREEN-VALUE = ''.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-sair
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-sair C-Win
ON CHOOSE OF bt-sair IN FRAME DEFAULT-FRAME /* Btn 2 */
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-doca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-doca C-Win
ON RETURN OF fi-doca IN FRAME DEFAULT-FRAME
DO:
   APPLY 'CHOOSE' TO bt-imprime.
   APPLY 'entry' TO fi-rua.
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

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.

  FIND FIRST para-ped NO-LOCK NO-ERROR.

    ASSIGN c-prog-epl = SESSION:TEMP-DIRECTORY + "etq-loc.epl".
   IF para-ped.estab-padrao = '1' THEN
    ASSIGN c-form-epl = "M:\EMS206\especificos\etiqueta\form-loc-ima.epl".
   ELSE
    ASSIGN c-form-epl = "M:\EMS206\especificos\etiqueta\form-loc-med.epl".
  /*
  RUN esapi/busca-estab-usuar.p (INPUT c-seg-usuario,
                                 OUTPUT c-cod-estabel).
  IF c-cod-estabel = "" THEN DO:
     MESSAGE "Usu†rio: " + c-seg-usuario + " n∆o cadastrado no parÉmetro de permiss‰es."
         VIEW-AS ALERT-BOX WARNING BUTTONS OK.
     APPLY "CLOSE":U TO THIS-PROCEDURE.
     RETURN NO-APPLY.
  END.
  */

  APPLY 'ENTRY' TO fi-rua.
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
  DISPLAY fi-rua fi-doca 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE fi-rua fi-doca bt-imprime bt-sair RECT-1 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-etiqueta C-Win 
PROCEDURE pi-etiqueta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    OS-COPY VALUE(c-form-epl) VALUE(c-prog-epl).

    OUTPUT TO VALUE(c-prog-epl) APPEND.
       PUT UNFORMATTED 
           "A400,145,0,4,4,4,N," '"' STRING(fi-rua,"999") '"' SKIP
           "A400,255,0,4,4,4,N," '"' STRING(fi-doca,"999") '"' SKIP
           "B250,360,0,1,4,5,110,N," '"' STRING(fi-rua,"999") + STRING(fi-doca,"999") '"' SKIP.

         /*  "A400,160,0,4,4,4,N," '"' STRING(fi-rua,"999") '"' SKIP
           "A400,280,0,4,4,4,N," '"' STRING(fi-doca,"999") '"' SKIP
           "B250,360,0,1,4,5,110,N," '"' STRING(fi-rua,"999") + STRING(fi-doca,"999") '"' SKIP. */


       PUT UNFORMATTED
           "P1" SKIP.
    OUTPUT CLOSE.
        
    FIND imprsor_usuar WHERE 
         imprsor_usuar.nom_impressora = "rabbit" AND 
         imprsor_usuar.cod_usuario = c-seg-usuario NO-LOCK NO-ERROR.
    IF AVAIL imprsor_usuar THEN DO:
       ASSIGN c-impressora = imprsor_usuar.nom_disposit_so
              c-comando = "copy /Y /b " + c-prog-epl + " " + c-impressora. 
       
       OS-COMMAND SILENT VALUE(c-comando).
    END.
        
    /*
    ASSIGN c-comando = "copy /Y /b " + c-prog-epl + " lpt1". 
    OS-COMMAND SILENT VALUE(c-comando). 
    */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

