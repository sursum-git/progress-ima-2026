&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*:T*******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i D99XX999 9.99.99.999}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEF TEMP-TABLE tt-digita
    FIELD opcao AS CHAR
    FIELD campo AS CHAR
    FIELD valor AS CHAR.

DEF TEMP-TABLE tt-work LIKE tt-digita.

DEF INPUT-OUTPUT PARAMETER TABLE FOR tt-digita.
DEF INPUT PARAMETER p-opcao AS CHAR.
DEF INPUT PARAMETER p-campo AS CHAR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog
&Scoped-define BROWSE-NAME br-digita

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-work

/* Definitions for BROWSE br-digita                                     */
&Scoped-define FIELDS-IN-QUERY-br-digita tt-work.campo tt-work.valor   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-digita tt-work.valor   
&Scoped-define ENABLED-TABLES-IN-QUERY-br-digita tt-work
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br-digita tt-work
&Scoped-define SELF-NAME br-digita
&Scoped-define QUERY-STRING-br-digita FOR EACH tt-work WHERE                                  tt-work.opcao = p-opcao                                  NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-digita OPEN QUERY {&SELF-NAME} FOR EACH tt-work WHERE                                  tt-work.opcao = p-opcao                                  NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-digita tt-work
&Scoped-define FIRST-TABLE-IN-QUERY-br-digita tt-work


/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-D-Dialog ~
    ~{&OPEN-QUERY-br-digita}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 rt-buttom br-digita bt-add bt-del ~
bt-ok bt-cancela bt-ajuda 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-4 bt-add bt-del 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-add 
     IMAGE-UP FILE "image/im-add.bmp":U
     LABEL "Button 1" 
     SIZE 5 BY 4.

DEFINE BUTTON bt-ajuda 
     LABEL "&Ajuda" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-cancela AUTO-END-KEY 
     LABEL "&Cancelar" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-del 
     IMAGE-UP FILE "image/im-era.bmp":U
     LABEL "Button 2" 
     SIZE 5.29 BY 3.92.

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "&OK" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 7 BY 9.5
     BGCOLOR 8 .

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 68 BY 1.42
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-digita FOR 
      tt-work SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-digita D-Dialog _FREEFORM
  QUERY br-digita NO-LOCK DISPLAY
      tt-work.campo FORMAT "x(20)":U WIDTH 20  COLUMN-LABEL "Campo" 
      tt-work.valor FORMAT "x(20)":U     
ENABLE
    tt-work.valor
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 60 BY 9.5
         FONT 6 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     br-digita AT ROW 1.25 COL 2
     bt-add AT ROW 1.75 COL 64
     bt-del AT ROW 6.29 COL 64
     bt-ok AT ROW 11.25 COL 3
     bt-cancela AT ROW 11.25 COL 14
     bt-ajuda AT ROW 11.25 COL 59
     RECT-6 AT ROW 1.25 COL 63
     rt-buttom AT ROW 11 COL 2
     SPACE(0.85) SKIP(0.28)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Digita‡Æo de Valores - d01digita"
         DEFAULT-BUTTON bt-ok.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/d-dialog.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
   FRAME-NAME L-To-R                                                    */
/* BROWSE-TAB br-digita rt-buttom D-Dialog */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON bt-add IN FRAME D-Dialog
   4                                                                    */
/* SETTINGS FOR BUTTON bt-del IN FRAME D-Dialog
   4                                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-digita
/* Query rebuild information for BROWSE br-digita
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-work WHERE
                                 tt-work.opcao = p-opcao
                                 NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-digita */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Digita‡Æo de Valores - d01digita */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-digita
&Scoped-define SELF-NAME br-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita D-Dialog
ON END-ERROR OF br-digita IN FRAME D-Dialog
ANYWHERE 
DO:
  IF br-digita:NEW-ROW THEN do:
     IF AVAIL tt-work THEN
        DELETE tt-work.
     IF br-digita:DELETE-CURRENT-ROW() THEN. 
  END.                                                               
  ELSE DO:
      GET CURRENT br-digita.
      DISPLAY tt-work.campo
              tt-work.valor WITH BROWSE br-digita. 
  END.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita D-Dialog
ON RETURN OF br-digita IN FRAME D-Dialog
ANYWHERE
DO:
   APPLY 'TAB':U TO br-digita.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita D-Dialog
ON ROW-ENTRY OF br-digita IN FRAME D-Dialog
DO:
   /*:T trigger para inicializar campos da temp table de digita‡Æo */
   IF br-digita:NEW-ROW THEN DO:
      ASSIGN tt-work.campo:SCREEN-VALUE IN BROWSE br-digita = p-campo.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita D-Dialog
ON ROW-LEAVE OF br-digita IN FRAME D-Dialog
DO:
  IF br-digita:NEW-ROW THEN DO TRANSACTION ON ERROR UNDO, RETURN NO-APPLY:
     CREATE tt-work.
     ASSIGN tt-work.opcao = p-opcao
            INPUT BROWSE br-digita tt-work.campo
            INPUT BROWSE br-digita tt-work.valor.

      br-digita:CREATE-RESULT-LIST-ENTRY().
  END.
  ELSE DO TRANSACTION ON ERROR UNDO, RETURN NO-APPLY:
      ASSIGN tt-work.opcao = p-opcao
             INPUT BROWSE br-digita tt-work.campo
             INPUT BROWSE br-digita tt-work.valor.
  END.

  APPLY 'entry' TO bt-add.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-add D-Dialog
ON CHOOSE OF bt-add IN FRAME D-Dialog /* Button 1 */
DO:
  IF NUM-RESULTS("br-digita":U) > 0 THEN
     br-digita:INSERT-ROW("after":U).
  ELSE DO TRANSACTION:
      
     CREATE tt-work.
     ASSIGN tt-work.opcao = p-opcao
            tt-work.campo = p-campo.
     
     {&OPEN-QUERY-br-digita}

     APPLY "entry":U TO tt-work.valor IN BROWSE br-digita. 
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda D-Dialog
ON CHOOSE OF bt-ajuda IN FRAME D-Dialog /* Ajuda */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-del
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-del D-Dialog
ON CHOOSE OF bt-del IN FRAME D-Dialog /* Button 2 */
DO:
    FIND CURRENT tt-work NO-ERROR.
    DELETE tt-work.

    IF NOT CAN-FIND(FIRST tt-work) THEN
       ASSIGN bt-del:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

    {&OPEN-QUERY-br-digita}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok D-Dialog
ON CHOOSE OF bt-ok IN FRAME D-Dialog /* OK */
DO:
   IF CAN-FIND(FIRST tt-work WHERE
                     tt-work.valor = "") THEN DO.
      MESSAGE 'Existem parƒmetros sem valor definido...'
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
       APPLY 'ENTRY' TO br-digita.
       RETURN NO-APPLY.
   END.
   ELSE DO.
       EMPTY TEMP-TABLE tt-digita.
    
       FOR EACH tt-work.
           CREATE tt-digita.
           BUFFER-COPY tt-work TO tt-digita.
       END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */
ON 'leave':U OF tt-work.valor DO:
   IF tt-work.campo:SCREEN-VALUE IN BROWSE br-digita = 'Representante' THEN DO.
      FIND repres WHERE
           repres.cod-rep = INTEGER(SELF:INPUT-VALUE) NO-LOCK NO-ERROR.
      IF AVAIL repres THEN
         ASSIGN SELF:SCREEN-VALUE = repres.nome-abrev.
   END.
END.

{src/adm/template/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects D-Dialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available D-Dialog  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI D-Dialog  _DEFAULT-DISABLE
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
  HIDE FRAME D-Dialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI D-Dialog  _DEFAULT-ENABLE
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
  ENABLE RECT-6 rt-buttom br-digita bt-add bt-del bt-ok bt-cancela bt-ajuda 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy D-Dialog 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize D-Dialog 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  FOR EACH tt-digita.
      CREATE tt-work.
      BUFFER-COPY tt-digita TO tt-work.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF p-opcao = 'E' THEN DO.
     ASSIGN FRAME D-Dialog:TITLE = 'ExcessÆo de Valores da Sele‡Æo  -  d01-digita'.
     ASSIGN tt-work.valor:LABEL IN BROWSE br-digita = 'Exceto Valor'.
  END.
  ELSE
     ASSIGN tt-work.valor:LABEL IN BROWSE br-digita = 'Filtrar Valor'.

  APPLY 'entry' TO bt-add IN FRAME {&FRAME-NAME}.
  RETURN NO-APPLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records D-Dialog  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-work"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed D-Dialog 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
  
  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

