&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME w-window
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-window 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i IMPD030X 1.00.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Temp-Table Definitions ---                                           */
DEF TEMP-TABLE tt-estabelec-orig LIKE estabelec.
DEFINE NEW GLOBAL SHARED TEMP-TABLE tt-estabelec-dest LIKE estabelec.


/* Global Variable Definitions ---                                       */
DEFINE NEW GLOBAL SHARED VAR h-win-sel-estab AS WIDGET-HANDLE NO-UNDO.

/* Local Variable Definitions ---                                       */
DEF VAR i-num-lin AS INT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE JanelaDetalhe
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-destino

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-estabelec-dest tt-estabelec-orig

/* Definitions for BROWSE br-destino                                    */
&Scoped-define FIELDS-IN-QUERY-br-destino tt-estabelec-dest.cod-estabel tt-estabelec-dest.ep-codigo tt-estabelec-dest.nome   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-destino   
&Scoped-define SELF-NAME br-destino
&Scoped-define QUERY-STRING-br-destino FOR EACH tt-estabelec-dest NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-destino OPEN QUERY {&SELF-NAME} FOR EACH tt-estabelec-dest NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-destino tt-estabelec-dest
&Scoped-define FIRST-TABLE-IN-QUERY-br-destino tt-estabelec-dest


/* Definitions for BROWSE br-origem                                     */
&Scoped-define FIELDS-IN-QUERY-br-origem tt-estabelec-orig.cod-estabel tt-estabelec-orig.ep-codigo tt-estabelec-orig.nome   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-origem   
&Scoped-define SELF-NAME br-origem
&Scoped-define QUERY-STRING-br-origem FOR EACH tt-estabelec-orig NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-origem OPEN QUERY {&SELF-NAME} FOR EACH tt-estabelec-orig NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-origem tt-estabelec-orig
&Scoped-define FIRST-TABLE-IN-QUERY-br-origem tt-estabelec-orig


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-br-destino}~
    ~{&OPEN-QUERY-br-origem}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS i-ep-codigo-ini i-ep-codigo-fin bt-confirma ~
br-origem bt-add bt-add-all bt-del bt-del-all br-destino bt-ok bt-cancelar ~
bt-ajuda IMAGE-1 IMAGE-2 RECT-1 rt-key-parent rt-source-browse ~
rt-source-browse-2 
&Scoped-Define DISPLAYED-OBJECTS i-ep-codigo-ini i-ep-codigo-fin 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-window AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-add 
     IMAGE-UP FILE "adeicon\next-au":U
     IMAGE-INSENSITIVE FILE "adeicon\next-ai":U
     LABEL "" 
     SIZE 7 BY 1.

DEFINE BUTTON bt-add-all 
     IMAGE-UP FILE "image/add-all.bmp":U
     IMAGE-INSENSITIVE FILE "adeicon\prev-ai":U
     LABEL "" 
     SIZE 7 BY 1.

DEFINE BUTTON bt-ajuda 
     LABEL "Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "Fechar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-confirma 
     IMAGE-UP FILE "image/im-chck1.bmp":U
     LABEL "Confirma Sele‡Æo Empresa" 
     SIZE 6 BY 1.

DEFINE BUTTON bt-del 
     IMAGE-UP FILE "adeicon\prev-au":U
     IMAGE-INSENSITIVE FILE "adeicon\prev-ai":U
     LABEL "" 
     SIZE 7 BY 1.

DEFINE BUTTON bt-del-all 
     IMAGE-UP FILE "image/del-all.bmp":U
     IMAGE-INSENSITIVE FILE "adeicon\prev-ai":U
     LABEL "" 
     SIZE 7 BY 1.

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "OK" 
     SIZE 10 BY 1.

DEFINE VARIABLE i-ep-codigo-fin AS CHARACTER FORMAT "X(256)" INITIAL "ZZZ" 
     VIEW-AS FILL-IN 
     SIZE 4.57 BY .88 NO-UNDO.

DEFINE VARIABLE i-ep-codigo-ini AS CHARACTER FORMAT "X(256)" 
     LABEL "Empresa":R9 
     VIEW-AS FILL-IN 
     SIZE 4.57 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 87.86 BY 1.38
     BGCOLOR 7 .

DEFINE RECTANGLE rt-key-parent
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 87.86 BY 1.67.

DEFINE RECTANGLE rt-source-browse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 38.57 BY 9.5.

DEFINE RECTANGLE rt-source-browse-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 38.57 BY 9.5.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-destino FOR 
      tt-estabelec-dest SCROLLING.

DEFINE QUERY br-origem FOR 
      tt-estabelec-orig SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-destino
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-destino w-window _FREEFORM
  QUERY br-destino NO-LOCK DISPLAY
      tt-estabelec-dest.cod-estabel COLUMN-LABEL "Estab" FORMAT "X(3)":U WIDTH 5.00
      tt-estabelec-dest.ep-codigo COLUMN-LABEL "Emp" WIDTH 3.00
      tt-estabelec-dest.nome FORMAT "X(40)":U WIDTH 27.86
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 37 BY 8.75 FIT-LAST-COLUMN.

DEFINE BROWSE br-origem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-origem w-window _FREEFORM
  QUERY br-origem NO-LOCK DISPLAY
      tt-estabelec-orig.cod-estabel COLUMN-LABEL "Estab" FORMAT "X(3)":U WIDTH 5.00
      tt-estabelec-orig.ep-codigo COLUMN-LABEL "Emp" WIDTH 3.00
      tt-estabelec-orig.nome FORMAT "X(40)":U WIDTH 27.86
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 37 BY 8.75 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     i-ep-codigo-ini AT ROW 1.83 COL 33.43 COLON-ALIGNED HELP
          "C¢digo da empresa"
     i-ep-codigo-fin AT ROW 1.83 COL 50 COLON-ALIGNED HELP
          "C¢digo da empresa" NO-LABEL
     bt-confirma AT ROW 1.79 COL 57.43
     br-origem AT ROW 4 COL 3
     bt-add AT ROW 5.38 COL 42.29
     bt-add-all AT ROW 7.04 COL 42.29
     bt-del AT ROW 8.71 COL 42.29
     bt-del-all AT ROW 10.38 COL 42.29
     br-destino AT ROW 4 COL 52.29
     bt-ok AT ROW 13.71 COL 3
     bt-cancelar AT ROW 13.71 COL 14
     bt-ajuda AT ROW 13.71 COL 78.43
     IMAGE-1 AT ROW 1.83 COL 40.43
     IMAGE-2 AT ROW 1.83 COL 49.14
     RECT-1 AT ROW 13.5 COL 2
     rt-key-parent AT ROW 1.42 COL 2.14
     rt-source-browse AT ROW 3.58 COL 2.14
     rt-source-browse-2 AT ROW 3.63 COL 51.29
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.86 BY 14.08.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: JanelaDetalhe
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-window ASSIGN
         HIDDEN             = YES
         TITLE              = "Seleciona Estabelecimentos An lise Cr‚dito"
         HEIGHT             = 14.08
         WIDTH              = 89.86
         MAX-HEIGHT         = 22
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 22
         VIRTUAL-WIDTH      = 114.29
         TOP-ONLY           = yes
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-window 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-window.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-window
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB br-origem bt-confirma F-Main */
/* BROWSE-TAB br-destino bt-del-all F-Main */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-window)
THEN w-window:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-destino
/* Query rebuild information for BROWSE br-destino
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-estabelec-dest NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-destino */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-origem
/* Query rebuild information for BROWSE br-origem
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-estabelec-orig NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-origem */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-window
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON END-ERROR OF w-window /* Seleciona Estabelecimentos An lise Cr‚dito */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON WINDOW-CLOSE OF w-window /* Seleciona Estabelecimentos An lise Cr‚dito */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-add w-window
ON CHOOSE OF bt-add IN FRAME F-Main
DO:
    REPEAT i-num-lin = 1 TO br-origem:NUM-SELECTED-ROWS:
        br-origem:FETCH-SELECTED-ROW(i-num-lin).
        RUN pi-transfere-orig-dest.
    END.
    {&OPEN-BROWSERS-IN-QUERY-F-Main}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-add-all
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-add-all w-window
ON CHOOSE OF bt-add-all IN FRAME F-Main
DO:
  FOR EACH tt-estabelec-orig:
      RUN pi-transfere-orig-dest.
  END.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda w-window
ON CHOOSE OF bt-ajuda IN FRAME F-Main /* Ajuda */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancelar w-window
ON CHOOSE OF bt-cancelar IN FRAME F-Main /* Fechar */
DO:
  apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-confirma
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-confirma w-window
ON CHOOSE OF bt-confirma IN FRAME F-Main /* Confirma Sele‡Æo Empresa */
DO:
  RUN pi-carrega-tt-origem.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-del
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-del w-window
ON CHOOSE OF bt-del IN FRAME F-Main
DO:
    REPEAT i-num-lin = 1 TO br-destino:NUM-SELECTED-ROWS:
        br-destino:FETCH-SELECTED-ROW(i-num-lin).
        RUN pi-transfere-dest-orig.
    END.
    {&OPEN-BROWSERS-IN-QUERY-F-Main}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-del-all
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-del-all w-window
ON CHOOSE OF bt-del-all IN FRAME F-Main
DO:
    FOR EACH tt-estabelec-dest:
        RUN pi-transfere-dest-orig.
    END.
    {&OPEN-BROWSERS-IN-QUERY-F-Main}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-window
ON CHOOSE OF bt-ok IN FRAME F-Main /* OK */
DO:
  apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-destino
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-window 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-window  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-window  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-window  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-window)
  THEN DELETE WIDGET w-window.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-window  _DEFAULT-ENABLE
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
  DISPLAY i-ep-codigo-ini i-ep-codigo-fin 
      WITH FRAME F-Main IN WINDOW w-window.
  ENABLE i-ep-codigo-ini i-ep-codigo-fin bt-confirma br-origem bt-add 
         bt-add-all bt-del bt-del-all br-destino bt-ok bt-cancelar bt-ajuda 
         IMAGE-1 IMAGE-2 RECT-1 rt-key-parent rt-source-browse 
         rt-source-browse-2 
      WITH FRAME F-Main IN WINDOW w-window.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW w-window.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-window 
PROCEDURE local-destroy :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .
  {include/i-logfin.i}

  /* Code placed here will execute AFTER standard behavior.    */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-window 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-window 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  {include/win-size.i}


  {utp/ut9000.i "IMPD030X" "1.00.00.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  APPLY "choose" TO bt-confirma.

  ASSIGN h-win-sel-estab = w-window:HANDLE.  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-carrega-tt-origem w-window 
PROCEDURE pi-carrega-tt-origem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH tt-estabelec-orig:
        DELETE tt-estabelec-orig.
    END.

    FOR EACH estabelec NO-LOCK WHERE
             estabelec.ep-codigo >= INPUT FRAME {&FRAME-NAME} i-ep-codigo-ini AND 
             estabelec.ep-codigo <= INPUT FRAME {&FRAME-NAME} i-ep-codigo-fin:
        CREATE tt-estabelec-orig.
        BUFFER-COPY estabelec TO tt-estabelec-orig.
    END.

    FOR EACH  tt-estabelec-orig,
        FIRST tt-estabelec-dest NO-LOCK WHERE
              tt-estabelec-dest.cod-estabel = tt-estabelec-orig.cod-estabel:
        DELETE tt-estabelec-orig.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-transfere-dest-orig w-window 
PROCEDURE pi-transfere-dest-orig :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    CREATE tt-estabelec-orig.
    BUFFER-COPY tt-estabelec-dest TO tt-estabelec-orig.
    DELETE tt-estabelec-dest.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-transfere-orig-dest w-window 
PROCEDURE pi-transfere-orig-dest :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    CREATE tt-estabelec-dest.
    BUFFER-COPY tt-estabelec-orig TO tt-estabelec-dest.
    DELETE tt-estabelec-orig.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-window  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-estabelec-orig"}
  {src/adm/template/snd-list.i "tt-estabelec-dest"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-window 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

