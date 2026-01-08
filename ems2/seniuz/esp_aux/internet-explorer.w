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

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi-uf BUTTON-1 
&Scoped-Define DISPLAYED-OBJECTS fi-uf 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-1 
     LABEL "Explorer" 
     SIZE 15 BY 1.13.

DEFINE VARIABLE fi-uf AS CHARACTER FORMAT "X(256)":U INITIAL "MG" 
     LABEL "Estado" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fi-uf AT ROW 2 COL 11 COLON-ALIGNED
     BUTTON-1 AT ROW 3.5 COL 13
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 32.57 BY 5.13.


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
         TITLE              = "<insert window title>"
         HEIGHT             = 5.13
         WIDTH              = 32.57
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
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 C-Win
ON CHOOSE OF BUTTON-1 IN FRAME DEFAULT-FRAME /* Explorer */
DO:
    RUN pi-sintegra (INPUT fi-uf:SCREEN-VALUE).
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
  DISPLAY fi-uf 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE fi-uf BUTTON-1 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-open-ie C-Win 
PROCEDURE pi-open-ie :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER p-site AS CHAR FORMAT "x(100)".

  DEF VAR c-arq-java AS CHAR.
  DEF VAR c-comando AS CHAR.                                

  ASSIGN c-arq-java = SESSION:TEMP-DIRECTORY + "abrir.js".

  OUTPUT TO VALUE(c-arq-java).
      PUT 'var oIE = new ActiveXObject("InternetExplorer.Application");' SKIP
          'oIE.Navigate2("' + p-site + '");' FORMAT "x(150)" SKIP     
          'oIE.Visible = true;' SKIP.
  OUTPUT CLOSE.

  ASSIGN c-comando = 'wscript.exe ' + c-arq-java.
  
  OS-COMMAND SILENT VALUE(c-comando).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-sintegra C-Win 
PROCEDURE pi-sintegra :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER p-estado AS CHAR.

      CASE p-estado.
          WHEN 'AC' THEN
              RUN pi-open-ie (INPUT "http://www.sefaz.ac.gov.br:8080/portalsefaz/servlet/hpfsinco").
          WHEN 'AL' THEN
              RUN pi-open-ie (INPUT "http://www.sefaz.al.gov.br/asp/sintegra/sintegra.asp?estado=AL").
          WHEN 'AP' THEN
              RUN pi-open-ie (INPUT "http://www.sintegra.ap.gov.br/").     
          WHEN 'AM' THEN
              RUN pi-open-ie (INPUT "http://www.sefaz.am.gov.br/sintegra/sintegra0.asp").     
          WHEN 'BA' THEN
              RUN pi-open-ie (INPUT "http://www.sefaz.ba.gov.br/Sintegra/sintegra.asp?estado=BA").     
          WHEN 'CE' THEN
              RUN pi-open-ie (INPUT "http://www.sefaz.ce.gov.br/Sintegra/Sintegra.Asp?estado=CE").
          WHEN 'DF' THEN
              RUN pi-open-ie (INPUT "http://www.fazenda.df.gov.br/area.cfm?id_area=110").
          WHEN 'ES' THEN
              RUN pi-open-ie (INPUT "http://www.sintegra.es.gov.br/").
          WHEN 'GO' THEN
              RUN pi-open-ie (INPUT "http://www.sefaz.go.gov.br/sintegra/sintegra.asp").     
          WHEN 'MA' THEN
              RUN pi-open-ie (INPUT "http://www.sefaz.ma.gov.br/sintegra/sintegra.asp").
          WHEN 'MT' THEN
              RUN pi-open-ie (INPUT "http://www.sefaz.mt.gov.br/sintegra").
          WHEN 'MS' THEN
              RUN pi-open-ie (INPUT "http://www.sintegra.ms.gov.br/sintegra.asp?estado=ms").
          WHEN 'MG' THEN
              RUN pi-open-ie (INPUT "http://www.sintegra.fazenda.mg.gov.br/").
          WHEN 'PA' THEN
              RUN pi-open-ie (INPUT "http://www.sefa.pa.gov.br/sintegra/consultapublica/index.cfm?fuseaction=inicio").     
          WHEN 'PB' THEN
              RUN pi-open-ie (INPUT "http://sintegra.receita.pb.gov.br/sintegra/sintegra.asp?estado=pb").     
          WHEN 'PR' THEN
              RUN pi-open-ie (INPUT "http://www.fazenda.pr.gov.br/sintegra/").     
          WHEN 'PE' THEN
              RUN pi-open-ie (INPUT "http://www.sintegra.sefaz.pe.gov.br").     
          WHEN 'PI' THEN
              RUN pi-open-ie (INPUT "http://web.sintegra.sefaz.pi.gov.br").     
          WHEN 'RJ' THEN
              RUN pi-open-ie (INPUT "http://p1-webapp.sef.rj.gov.br/app/cps/index.jsp").
          WHEN 'RN' THEN
              RUN pi-open-ie (INPUT "http://ww3.set.rn.gov.br/sintegra").     
          WHEN 'RS' THEN
              RUN pi-open-ie (INPUT "http://www.sefaz.rs.gov.br/SEF_root/inf/sintegra_entrada.asp").     
          WHEN 'RO' THEN
              RUN pi-open-ie (INPUT "http://www.sefin.ro.gov.br/sint_consul.asp").
          WHEN 'RR' THEN
              RUN pi-open-ie (INPUT "http://sintegra.sefaz.rr.gov.br/").     
          WHEN 'SC' THEN
              RUN pi-open-ie (INPUT "http://sistemas.sef.sc.gov.br/sintegra").     
          WHEN 'SP' THEN
              RUN pi-open-ie (INPUT "http://pfeserv1.fazenda.sp.gov.br/sintegrapfe/sintegra.html").
          WHEN 'SE' THEN
              RUN pi-open-ie (INPUT "http://www.sefaz.se.gov.br/sintegra").     
          WHEN 'TO' THEN
              RUN pi-open-ie (INPUT "http://sintegra.sefaz.to.gov.br").     
          OTHERWISE
              RUN pi-open-ie (INPUT "http://sintegra.gov.br").     
      END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

