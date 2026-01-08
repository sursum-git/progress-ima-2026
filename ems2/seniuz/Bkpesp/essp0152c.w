&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          espec            PROGRESS
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

DEF NEW GLOBAL SHARED VAR c-cod-estabel AS CHAR.
DEF NEW GLOBAL SHARED VAR i-num-trf-ini LIKE ob-trf.num-trf.
DEF NEW GLOBAL SHARED VAR i-num-trf-fim LIKE ob-trf.num-trf.
DEF NEW GLOBAL SHARED VAR da-dt-solic-ini LIKE ob-trf.dt-solic.
DEF NEW GLOBAL SHARED VAR da-dt-solic-fim LIKE ob-trf.dt-solic.
DEF NEW GLOBAL SHARED VAR i-situacao LIKE ob-trf.situacao.

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES espec.ob-trf

/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define QUERY-STRING-D-Dialog FOR EACH espec.ob-trf SHARE-LOCK
&Scoped-define OPEN-QUERY-D-Dialog OPEN QUERY D-Dialog FOR EACH espec.ob-trf SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-D-Dialog espec.ob-trf
&Scoped-define FIRST-TABLE-IN-QUERY-D-Dialog espec.ob-trf


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS IMAGE-1 IMAGE-2 RECT-1 rt-buttom IMAGE-84 ~
IMAGE-85 fi-cod-estabel fi-num-trf-ini fi-num-trf-fim fi-dt-solic-ini ~
fi-dt-solic-fim rs-situacao bt-ok bt-cancela bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS fi-cod-estabel fi-nome-estabel ~
fi-num-trf-ini fi-num-trf-fim fi-dt-solic-ini fi-dt-solic-fim rs-situacao 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-5 fi-cod-estabel 

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

DEFINE VARIABLE fi-cod-estabel AS CHARACTER FORMAT "X(3)":U 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 4.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-solic-fim AS DATE FORMAT "99/99/9999" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88.

DEFINE VARIABLE fi-dt-solic-ini AS DATE FORMAT "99/99/9999" 
     LABEL "Data Transf." 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88.

DEFINE VARIABLE fi-nome-estabel AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 39 BY .88 NO-UNDO.

DEFINE VARIABLE fi-num-trf-fim AS INTEGER FORMAT ">>>>>>>>9" INITIAL 999999999 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88.

DEFINE VARIABLE fi-num-trf-ini AS INTEGER FORMAT ">>>>>>>9" INITIAL 0 
     LABEL "Nr Transforma‡Æo" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-84
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-85
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE VARIABLE rs-situacao AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Pendente", 1,
"Executada", 2
     SIZE 29 BY 1.04.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 68 BY 5.

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 68 BY 1.42
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY D-Dialog FOR 
      espec.ob-trf SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     fi-cod-estabel AT ROW 1.5 COL 13.86 COLON-ALIGNED WIDGET-ID 10
     fi-nome-estabel AT ROW 1.5 COL 18.86 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     fi-num-trf-ini AT ROW 2.5 COL 14 COLON-ALIGNED
     fi-num-trf-fim AT ROW 2.5 COL 33.14 COLON-ALIGNED NO-LABEL
     fi-dt-solic-ini AT ROW 3.5 COL 14 COLON-ALIGNED WIDGET-ID 4
     fi-dt-solic-fim AT ROW 3.5 COL 33.14 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     rs-situacao AT ROW 4.54 COL 15.86 NO-LABEL
     bt-ok AT ROW 6.46 COL 2.29
     bt-cancela AT ROW 6.46 COL 13
     bt-ajuda AT ROW 6.46 COL 58.29
     "Situa‡Æo:" VIEW-AS TEXT
          SIZE 7 BY .75 AT ROW 4.67 COL 8.86
     IMAGE-1 AT ROW 2.5 COL 26.86
     IMAGE-2 AT ROW 2.5 COL 31.57
     RECT-1 AT ROW 1 COL 1.29
     rt-buttom AT ROW 6.21 COL 1.29
     IMAGE-84 AT ROW 3.5 COL 26.86 WIDGET-ID 6
     IMAGE-85 AT ROW 3.5 COL 31.57 WIDGET-ID 8
     SPACE(34.99) SKIP(3.44)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 1
         TITLE "Parƒmetros das Transforma‡äes - ESSP0152C"
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
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fi-cod-estabel IN FRAME D-Dialog
   5                                                                    */
/* SETTINGS FOR FILL-IN fi-nome-estabel IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _TblList          = "espec.ob-trf"
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Parƒmetros das Transforma‡äes - ESSP0152C */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
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


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok D-Dialog
ON CHOOSE OF bt-ok IN FRAME D-Dialog /* OK */
DO:
   ASSIGN c-cod-estabel = INPUT FRAME {&FRAME-NAME} fi-cod-estabel
          i-num-trf-ini = INPUT FRAME {&FRAME-NAME} fi-num-trf-ini 
          i-num-trf-fim = INPUT FRAME {&FRAME-NAME} fi-num-trf-fim 
          da-dt-solic-ini = INPUT FRAME {&FRAME-NAME} fi-dt-solic-ini
          da-dt-solic-fim = INPUT FRAME {&FRAME-NAME} fi-dt-solic-fim 
          i-situacao = INPUT FRAME {&FRAME-NAME} rs-situacao.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-estabel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-estabel D-Dialog
ON ENTRY OF fi-cod-estabel IN FRAME D-Dialog /* Estabelecimento */
DO:
  FIND estabelec WHERE
       estabelec.cod-estabel = INPUT FRAME {&FRAME-NAME} fi-cod-estabel 
       NO-LOCK NO-ERROR.
  IF AVAIL estabelec THEN 
     ASSIGN fi-nome-estabel:SCREEN-VALUE = TRIM(estabelec.cidade) + " / " + TRIM(estabelec.bairro).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-estabel D-Dialog
ON LEAVE OF fi-cod-estabel IN FRAME D-Dialog /* Estabelecimento */
DO:
  FIND estabelec WHERE
       estabelec.cod-estabel = INPUT FRAME {&FRAME-NAME} fi-cod-estabel 
       NO-LOCK NO-ERROR.
  IF NOT AVAIL estabelec THEN DO.
     MESSAGE 'Estabelecimento nÆo Cadastrado....'
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
     APPLY 'ENTRY' TO SELF.
     RETURN NO-APPLY.
  END.
  ASSIGN fi-nome-estabel:SCREEN-VALUE = TRIM(estabelec.cidade) + " / " + TRIM(estabelec.bairro).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

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
  DISPLAY fi-cod-estabel fi-nome-estabel fi-num-trf-ini fi-num-trf-fim 
          fi-dt-solic-ini fi-dt-solic-fim rs-situacao 
      WITH FRAME D-Dialog.
  ENABLE IMAGE-1 IMAGE-2 RECT-1 rt-buttom IMAGE-84 IMAGE-85 fi-cod-estabel 
         fi-num-trf-ini fi-num-trf-fim fi-dt-solic-ini fi-dt-solic-fim 
         rs-situacao bt-ok bt-cancela bt-ajuda 
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

   ASSIGN fi-cod-estabel = c-cod-estabel
          fi-num-trf-ini = i-num-trf-ini 
          fi-num-trf-fim = i-num-trf-fim
          fi-dt-solic-ini = da-dt-solic-ini 
          fi-dt-solic-fim = da-dt-solic-fim
          rs-situacao = i-situacao.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

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
  {src/adm/template/snd-list.i "espec.ob-trf"}

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

