&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          espec            PROGRESS
*/
&Scoped-define WINDOW-NAME w-cadsim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-cadsim 
/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i XX9999 9.99.99.999}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
define variable wh-imprime as handle no-undo.



DEFINE TEMP-TABLE tt-movto-etq LIKE movto-etq.
DEF VAR c-usuario AS CHAR FORMAT "x(300)".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad
&Scoped-define BROWSE-NAME br-movto

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-movto-etq

/* Definitions for BROWSE br-movto                                      */
&Scoped-define FIELDS-IN-QUERY-br-movto tt-movto-etq.dt-trans tt-movto-etq.esp-docto fn-obs() @ c-usuario   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-movto   
&Scoped-define SELF-NAME br-movto
&Scoped-define QUERY-STRING-br-movto FOR EACH tt-movto-etq NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-movto OPEN QUERY {&SELF-NAME} FOR EACH tt-movto-etq NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-movto tt-movto-etq
&Scoped-define FIRST-TABLE-IN-QUERY-br-movto tt-movto-etq


/* Definitions for FRAME f-cad                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-cad ~
    ~{&OPEN-QUERY-br-movto}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi-num-etiqueta bt-confirma ed-log bt-sair ~
bt-ajuda rt-button br-movto RECT-64 
&Scoped-Define DISPLAYED-OBJECTS fi-num-etiqueta ed-log 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-obs w-cadsim 
FUNCTION fn-obs RETURNS CHARACTER
  ( )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-cadsim AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-bt-ajuda 
       MENU-ITEM mi-sobre       LABEL "Sobre..."      .


/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ajuda 
     LABEL "&Ajuda" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-confirma 
     IMAGE-UP FILE "image\im-sav":U
     LABEL "Button 1" 
     SIZE 4 BY 1.

DEFINE BUTTON bt-sair AUTO-END-KEY 
     LABEL "&Sair" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE ed-log AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 37 BY 8.04
     FGCOLOR 12 FONT 6 NO-UNDO.

DEFINE VARIABLE fi-num-etiqueta LIKE ob-etiqueta.num-etiqueta
     VIEW-AS FILL-IN 
     SIZE 11.43 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-64
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 77.57 BY 3.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 78 BY 1.38
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-movto FOR 
      tt-movto-etq SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-movto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-movto w-cadsim _FREEFORM
  QUERY br-movto NO-LOCK DISPLAY
      tt-movto-etq.dt-trans COLUMN-LABEL "Dt Trans" FORMAT "99/99/9999":U 
      tt-movto-etq.esp-docto FORMAT "X(3)":U
      fn-obs() @ c-usuario COLUMN-LABEL "Usuario" FORMAT "x(15)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 40.72 BY 8.04
         FONT 1 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     fi-num-etiqueta AT ROW 2.38 COL 16 COLON-ALIGNED HELP
          "" WIDGET-ID 4
     bt-confirma AT ROW 2.33 COL 30.29 WIDGET-ID 6
     ed-log AT ROW 4.46 COL 43 NO-LABEL WIDGET-ID 2
     bt-sair AT ROW 12.96 COL 3
     bt-ajuda AT ROW 12.92 COL 69
     br-movto AT ROW 4.46 COL 2.29 WIDGET-ID 200
     "Controle de Etiquetas" VIEW-AS TEXT
          SIZE 17 BY .67 AT ROW 1 COL 5 WIDGET-ID 12
     rt-button AT ROW 12.71 COL 2
     RECT-64 AT ROW 1.33 COL 2.43 WIDGET-ID 8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 13.17
         FONT 1 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-cadsim ASSIGN
         HIDDEN             = YES
         TITLE              = "Consulta Etiquetas Exclu¡das"
         HEIGHT             = 13.21
         WIDTH              = 80
         MAX-HEIGHT         = 22
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 22
         VIRTUAL-WIDTH      = 114.29
         RESIZE             = yes
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-cadsim 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
/*{include/w-incsim.i}*/
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-cadsim
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-cad
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB br-movto rt-button f-cad */
ASSIGN 
       bt-ajuda:POPUP-MENU IN FRAME f-cad       = MENU POPUP-MENU-bt-ajuda:HANDLE.

/* SETTINGS FOR FILL-IN fi-num-etiqueta IN FRAME f-cad
   LIKE = espec.ob-etiqueta.num-etiqueta EXP-SIZE                       */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-cadsim)
THEN w-cadsim:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-movto
/* Query rebuild information for BROWSE br-movto
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-movto-etq NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "espec.movto-etq.num-etiqueta = INTEGER(fi-num-etiqueta:SCREEN-VALUE IN FRAME {&FRAME-NAME}) AND
espec.movto-etq.cod-estabel = ""1"" "
     _Query            is OPENED
*/  /* BROWSE br-movto */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-cadsim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-cadsim w-cadsim
ON END-ERROR OF w-cadsim /* Consulta Etiquetas Exclu¡das */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-cadsim w-cadsim
ON WINDOW-CLOSE OF w-cadsim /* Consulta Etiquetas Exclu¡das */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-movto
&Scoped-define SELF-NAME br-movto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-movto w-cadsim
ON VALUE-CHANGED OF br-movto IN FRAME f-cad
DO:
  ASSIGN ed-log:READ-ONLY = YES.
  IF AVAIL movto-etq THEN
     ASSIGN ed-log:SCREEN-VALUE = movto-etq.char-1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda w-cadsim
ON CHOOSE OF bt-ajuda IN FRAME f-cad /* Ajuda */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-confirma
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-confirma w-cadsim
ON CHOOSE OF bt-confirma IN FRAME f-cad /* Button 1 */
DO:
  ASSIGN INPUT FRAME {&FRAME-NAME} fi-num-etiqueta.

  FIND FIRST movto-etq WHERE
             movto-etq.num-etiqueta = INTEGER(fi-num-etiqueta) 
             NO-LOCK NO-ERROR.
  IF AVAIL movto-etq THEN DO.
     RUN pi-processa.
  END.
  ELSE DO.
     MESSAGE "NÆo existe movimenta‡Æo da Etiqueta"
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     APPLY "entry" TO fi-num-etiqueta.

  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-sair
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-sair w-cadsim
ON CHOOSE OF bt-sair IN FRAME f-cad /* Sair */
DO:
  RUN notify ('cancel-record':U).
  APPLY "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sobre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sobre w-cadsim
ON CHOOSE OF MENU-ITEM mi-sobre /* Sobre... */
DO:
  {include/sobre.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-cadsim 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-cadsim  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-cadsim  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-cadsim  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-cadsim)
  THEN DELETE WIDGET w-cadsim.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-cadsim  _DEFAULT-ENABLE
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
  DISPLAY fi-num-etiqueta ed-log 
      WITH FRAME f-cad IN WINDOW w-cadsim.
  ENABLE fi-num-etiqueta bt-confirma ed-log bt-sair bt-ajuda rt-button br-movto 
         RECT-64 
      WITH FRAME f-cad IN WINDOW w-cadsim.
  {&OPEN-BROWSERS-IN-QUERY-f-cad}
  VIEW w-cadsim.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-cadsim 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-cadsim 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-cadsim 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  {include/win-size.i}

  {utp/ut9000.i "ESSP0148B" "2.04.00.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  RUN dispatch  IN this-procedure ('enable-fields':U).

  FIND FIRST para-ped NO-LOCK NO-ERROR.
  APPLY "ENTRY" TO fi-num-etiqueta.
  RETURN NO-APPLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-processa w-cadsim 
PROCEDURE pi-processa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH movto-etq WHERE
         movto-etq.cod-estabel = para-ped.estab-padrao AND
         movto-etq.num-etiqueta = INTEGER(fi-num-etiqueta) NO-LOCK.

    MESSAGE 'ok'
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    CREATE tt-movto-etq.
    ASSIGN tt-movto-etq.dt-trans = movto-etq.dt-trans
           tt-movto-etq.esp-docto = movto-etq.esp-docto.

END.

   {&OPEN-QUERY-br-movto}
   APPLY 'VALUE-CHANGED' TO br-movto IN FRAME {&FRAME-NAME}.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-cadsim  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-movto-etq"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-cadsim 
PROCEDURE state-changed :
/*:T -----------------------------------------------------------
  Purpose:     Manuseia trocas de estado dos SmartObjects
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.

  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-obs w-cadsim 
FUNCTION fn-obs RETURNS CHARACTER
  ( ) :

  /*RETURN TRIM(SUBSTR(movto-etq.char-1,1,20)).   */

  IF TRIM(ENTRY(2,SUBSTR(tt-movto-etq.char-1,1,30),":")) = "Hora" THEN
     RETURN "".
  ELSE
     RETURN TRIM(ENTRY(2,SUBSTR(tt-movto-etq.char-1,1,30),":")).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

