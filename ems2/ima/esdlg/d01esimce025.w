&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
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

DEFINE NEW GLOBAL SHARED VARIABLE var-glb-cod-estab-ini AS CHARACTER   NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE var-glb-cod-estab-fim AS CHARACTER   NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE var-glb-refer-ini     AS CHARACTER   NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE var-glb-refer-fim     AS CHARACTER   NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE var-glb-cod-depos-ini AS CHARACTER   NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE var-glb-cod-depos-fim AS CHARACTER   NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE var-glb-estab         AS INT         NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE var-glb-outlet        AS INT         NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE var-glb-ge-ini        AS INTEGER   NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE var-glb-ge-fim        AS INTEGER   NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE var-mostra-neg        AS LOG NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-buttom IMAGE-3 IMAGE-4 IMAGE-1 IMAGE-2 ~
RECT-23 IMAGE-106 IMAGE-107 IMAGE-110 IMAGE-111 RECT-61 rs-estab rs-outlet ~
fi-cod-estab-ini fi-cod-estab-fim fi-ge-ini fi-ge-fim fi-cod-depos-ini ~
fi-cod-depos-fim fi-refer-ini fi-refer-fim tg-mostra-neg bt-ok bt-cancela ~
bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS rs-estab rs-outlet fi-cod-estab-ini ~
fi-cod-estab-fim fi-ge-ini fi-ge-fim fi-cod-depos-ini fi-cod-depos-fim ~
fi-refer-ini fi-refer-fim tg-mostra-neg 

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

DEFINE VARIABLE fi-cod-depos-fim AS CHARACTER FORMAT "X(5)":U INITIAL "ZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 6.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-depos-ini AS CHARACTER FORMAT "X(5)":U 
     LABEL "Deposito" 
     VIEW-AS FILL-IN 
     SIZE 6.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-estab-fim AS CHARACTER FORMAT "X(5)":U INITIAL "ZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 6.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-estab-ini AS CHARACTER FORMAT "X(5)":U 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 6.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ge-fim AS INTEGER FORMAT ">>9":U INITIAL 999 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ge-ini AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Grupo Estoque" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-refer-fim AS CHARACTER FORMAT "X(5)":U INITIAL "ZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 6.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi-refer-ini AS CHARACTER FORMAT "X(5)":U 
     LABEL "Referencia" 
     VIEW-AS FILL-IN 
     SIZE 6.72 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-106
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-107
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-110
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-111
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-3
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-4
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE VARIABLE rs-estab AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL EXPAND 
     RADIO-BUTTONS 
          "Todas", 9,
"Ima", 1,
"Med", 5
     SIZE 42.57 BY .75 NO-UNDO.

DEFINE VARIABLE rs-outlet AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Outlet IMA", 1,
"Rubi", 4,
"Pronta Entrega", 2,
"Ambos", 3
     SIZE 52.43 BY .75 NO-UNDO.

DEFINE RECTANGLE RECT-23
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 68 BY 1.5.

DEFINE RECTANGLE RECT-61
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 68.29 BY 7.

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 68 BY 1.42
     BGCOLOR 7 .

DEFINE VARIABLE tg-mostra-neg AS LOGICAL INITIAL no 
     LABEL "Mostrar Saldo Negativo" 
     VIEW-AS TOGGLE-BOX
     SIZE 25.57 BY .83 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     rs-estab AT ROW 1.54 COL 16.43 NO-LABEL WIDGET-ID 66 NO-TAB-STOP 
     rs-outlet AT ROW 2.63 COL 16.57 NO-LABEL WIDGET-ID 18
     fi-cod-estab-ini AT ROW 4.04 COL 15 COLON-ALIGNED WIDGET-ID 86
     fi-cod-estab-fim AT ROW 4.04 COL 31.43 COLON-ALIGNED NO-LABEL WIDGET-ID 84
     fi-ge-ini AT ROW 5.04 COL 15 COLON-ALIGNED WIDGET-ID 78
     fi-ge-fim AT ROW 5.04 COL 31.43 COLON-ALIGNED NO-LABEL WIDGET-ID 76
     fi-cod-depos-ini AT ROW 6 COL 15.14 COLON-ALIGNED WIDGET-ID 50
     fi-cod-depos-fim AT ROW 6 COL 31.57 COLON-ALIGNED NO-LABEL WIDGET-ID 48
     fi-refer-ini AT ROW 7 COL 15.14 COLON-ALIGNED WIDGET-ID 54
     fi-refer-fim AT ROW 7 COL 31.57 COLON-ALIGNED NO-LABEL WIDGET-ID 52
     tg-mostra-neg AT ROW 9 COL 19 WIDGET-ID 74
     bt-ok AT ROW 10.5 COL 3
     bt-cancela AT ROW 10.5 COL 14
     bt-ajuda AT ROW 10.5 COL 59
     "Estab:" VIEW-AS TEXT
          SIZE 4.57 BY .54 AT ROW 1.58 COL 11.72 WIDGET-ID 70
     "Estoque:" VIEW-AS TEXT
          SIZE 6 BY .75 AT ROW 2.54 COL 10 WIDGET-ID 28
     rt-buttom AT ROW 10.25 COL 2
     IMAGE-3 AT ROW 5.96 COL 24.86 WIDGET-ID 60
     IMAGE-4 AT ROW 5.96 COL 30 WIDGET-ID 62
     IMAGE-1 AT ROW 6.96 COL 24.86 WIDGET-ID 56
     IMAGE-2 AT ROW 6.96 COL 30 WIDGET-ID 58
     RECT-23 AT ROW 8.5 COL 2 WIDGET-ID 72
     IMAGE-106 AT ROW 5 COL 24.72 WIDGET-ID 80
     IMAGE-107 AT ROW 5 COL 29.86 WIDGET-ID 82
     IMAGE-110 AT ROW 4 COL 24.72 WIDGET-ID 88
     IMAGE-111 AT ROW 4 COL 29.86 WIDGET-ID 90
     RECT-61 AT ROW 1.25 COL 2 WIDGET-ID 92
     SPACE(0.56) SKIP(3.53)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 1
         TITLE "<insert SmartDialog title>"
         DEFAULT-BUTTON bt-ok WIDGET-ID 100.


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

ASSIGN 
       IMAGE-1:HIDDEN IN FRAME D-Dialog           = TRUE.

ASSIGN 
       IMAGE-106:HIDDEN IN FRAME D-Dialog           = TRUE.

ASSIGN 
       IMAGE-107:HIDDEN IN FRAME D-Dialog           = TRUE.

ASSIGN 
       IMAGE-110:HIDDEN IN FRAME D-Dialog           = TRUE.

ASSIGN 
       IMAGE-111:HIDDEN IN FRAME D-Dialog           = TRUE.

ASSIGN 
       IMAGE-2:HIDDEN IN FRAME D-Dialog           = TRUE.

ASSIGN 
       IMAGE-3:HIDDEN IN FRAME D-Dialog           = TRUE.

ASSIGN 
       IMAGE-4:HIDDEN IN FRAME D-Dialog           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* <insert SmartDialog title> */
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
  ASSIGN var-glb-cod-estab-ini = fi-cod-estab-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME}
         var-glb-cod-estab-fim = fi-cod-estab-fim:SCREEN-VALUE IN FRAME {&FRAME-NAME}
         var-glb-cod-depos-ini = fi-cod-depos-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME}
         var-glb-cod-depos-fim = fi-cod-depos-fim:SCREEN-VALUE IN FRAME {&FRAME-NAME}
         var-glb-refer-ini     = fi-refer-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME}
         var-glb-refer-fim     = fi-refer-fim:SCREEN-VALUE IN FRAME {&FRAME-NAME}
         var-glb-ge-ini        = int(fi-ge-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME})
         var-glb-ge-fim        = int(fi-ge-fim:SCREEN-VALUE IN FRAME {&FRAME-NAME})
         var-glb-estab         = int(rs-estab:SCREEN-VALUE IN FRAME {&FRAME-NAME})
         var-glb-outlet        = int(rs-outlet:SCREEN-VALUE IN FRAME {&FRAME-NAME})
         var-mostra-neg        = INPUT FRAME {&FRAME-NAME} tg-mostra-neg.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-depos-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-depos-ini D-Dialog
ON LEAVE OF fi-cod-depos-ini IN FRAME D-Dialog /* Deposito */
DO:
   IF SELF:SCREEN-VALUE <> "" THEN
      ASSIGN fi-cod-depos-fim:SCREEN-VALUE IN FRAME {&FRAME-NAME} = SELF:SCREEN-VALUE.
   ELSE
      ASSIGN fi-cod-depos-fim:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "zzzzz".
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-estab-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-estab-ini D-Dialog
ON LEAVE OF fi-cod-estab-ini IN FRAME D-Dialog /* Estabelecimento */
DO:
   IF SELF:SCREEN-VALUE <> "" THEN
      ASSIGN fi-cod-estab-fim:SCREEN-VALUE IN FRAME {&FRAME-NAME} = SELF:SCREEN-VALUE.
   ELSE
      ASSIGN fi-cod-estab-fim:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "zzzzz".
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ge-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ge-ini D-Dialog
ON LEAVE OF fi-ge-ini IN FRAME D-Dialog /* Grupo Estoque */
DO:
   IF SELF:SCREEN-VALUE <> "" THEN
      ASSIGN fi-cod-depos-fim:SCREEN-VALUE IN FRAME {&FRAME-NAME} = SELF:SCREEN-VALUE.
   ELSE
      ASSIGN fi-cod-depos-fim:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "zzzzz".
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-refer-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-refer-ini D-Dialog
ON LEAVE OF fi-refer-ini IN FRAME D-Dialog /* Referencia */
DO:
   IF SELF:SCREEN-VALUE <> "" THEN
      ASSIGN fi-refer-fim:SCREEN-VALUE IN FRAME {&FRAME-NAME} = SELF:SCREEN-VALUE.
   ELSE
      ASSIGN fi-refer-fim:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "zzzzz".
   
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
  DISPLAY rs-estab rs-outlet fi-cod-estab-ini fi-cod-estab-fim fi-ge-ini 
          fi-ge-fim fi-cod-depos-ini fi-cod-depos-fim fi-refer-ini fi-refer-fim 
          tg-mostra-neg 
      WITH FRAME D-Dialog.
  ENABLE rt-buttom IMAGE-3 IMAGE-4 IMAGE-1 IMAGE-2 RECT-23 IMAGE-106 IMAGE-107 
         IMAGE-110 IMAGE-111 RECT-61 rs-estab rs-outlet fi-cod-estab-ini 
         fi-cod-estab-fim fi-ge-ini fi-ge-fim fi-cod-depos-ini fi-cod-depos-fim 
         fi-refer-ini fi-refer-fim tg-mostra-neg bt-ok bt-cancela bt-ajuda 
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

  {utp/ut9000.i "D99XX999" "9.99.99.999"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
 ASSIGN fi-cod-estab-ini:screen-value in frame {&frame-name} = var-glb-cod-estab-ini 
        fi-cod-estab-fim:screen-value in frame {&frame-name} = var-glb-cod-estab-fim 
        fi-ge-ini:screen-value in frame {&frame-name}        = STRING(var-glb-ge-ini)
        fi-ge-fim:screen-value in frame {&frame-name}        = STRING(var-glb-ge-fim)     
        fi-refer-ini:screen-value in frame {&frame-name}     = var-glb-refer-ini     
        fi-refer-fim:screen-value in frame {&frame-name}     = var-glb-refer-fim     
        fi-cod-depos-ini:screen-value in frame {&frame-name} = var-glb-cod-depos-ini 
        fi-cod-depos-fim:screen-value in frame {&frame-name} = var-glb-cod-depos-fim 
        rs-estab:screen-value in frame {&frame-name}         = STRING(var-glb-estab)
        rs-outlet:screen-value in frame {&frame-name}        = STRING(var-glb-outlet)
        tg-mostra-neg:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = STRING(var-mostra-neg).         
         
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

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartDialog, and there are no
     tables specified in any contained Browse, Query, or Frame. */

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

