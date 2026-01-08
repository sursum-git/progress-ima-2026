&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS F-Frame-Win 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrfrm.w - ADM SmartFrame Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
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
DEFINE VARIABLE c-cod-param     AS CHAR.
DEFINE VARIABLE c-container     AS CHAR.
DEFINE VARIABLE c-lst-grp-usuar AS CHAR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE CustomViewDigita
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-3 fi-usuario fi-senha fi-url ed-token ~
fi-natur-oper 
&Scoped-Define DISPLAYED-OBJECTS fi-usuario fi-senha fi-url ed-token ~
fi-natur-oper 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-param F-Frame-Win 
FUNCTION fn-param RETURNS CHARACTER ( p1 AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE ed-token AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 97 BY 2.75 NO-UNDO.

DEFINE VARIABLE fi-natur-oper AS CHARACTER FORMAT "X(256)":U 
     LABEL "Natureza de Sa¡da" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE fi-senha AS CHARACTER FORMAT "X(256)":U 
     LABEL "Senha" 
     VIEW-AS FILL-IN 
     SIZE 22 BY .88 NO-UNDO.

DEFINE VARIABLE fi-url AS CHARACTER FORMAT "X(256)":U 
     LABEL "URL" 
     VIEW-AS FILL-IN 
     SIZE 66 BY .88 NO-UNDO.

DEFINE VARIABLE fi-usuario AS CHARACTER FORMAT "X(256)":U 
     LABEL "Usu rio API" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 141.72 BY 23.17.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fi-usuario AT ROW 2.5 COL 26 COLON-ALIGNED WIDGET-ID 2
     fi-senha AT ROW 3.5 COL 26 COLON-ALIGNED WIDGET-ID 12 PASSWORD-FIELD 
     fi-url AT ROW 4.5 COL 26 COLON-ALIGNED WIDGET-ID 4
     ed-token AT ROW 5.5 COL 28 NO-LABEL WIDGET-ID 8
     fi-natur-oper AT ROW 11.5 COL 26 COLON-ALIGNED WIDGET-ID 48
     "Token:" VIEW-AS TEXT
          SIZE 5 BY .54 AT ROW 5.5 COL 22.43 WIDGET-ID 10
     RECT-3 AT ROW 1.08 COL 1.29
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 142.86 BY 23.54
         FONT 1 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: CustomViewDigita
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: Neither
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW F-Frame-Win ASSIGN
         HEIGHT             = 23.54
         WIDTH              = 142.86.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB F-Frame-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{utp/ut-glob.i}
{include/i_dbtype.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW F-Frame-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME F-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = ""
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME ed-token
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ed-token F-Frame-Win
ON LEAVE OF ed-token IN FRAME F-Main
DO:
  ASSIGN c-cod-param = "Token API".
  RUN pi-grava-param (INPUT c-cod-param,
                      INPUT SELF:SCREEN-VALUE).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-natur-oper
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-natur-oper F-Frame-Win
ON LEAVE OF fi-natur-oper IN FRAME F-Main /* Natureza de Sa¡da */
DO:
    ASSIGN c-cod-param = "NatOper Saida".
    RUN pi-grava-param (INPUT c-cod-param,
                        INPUT SELF:SCREEN-VALUE).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-senha
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-senha F-Frame-Win
ON LEAVE OF fi-senha IN FRAME F-Main /* Senha */
DO:
    ASSIGN c-cod-param = "Senha API".
    RUN pi-grava-param (INPUT c-cod-param,
                        INPUT SELF:SCREEN-VALUE).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-url
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-url F-Frame-Win
ON LEAVE OF fi-url IN FRAME F-Main /* URL */
DO:
    ASSIGN c-cod-param = "URL API".
    RUN pi-grava-param (INPUT c-cod-param,
                        INPUT SELF:SCREEN-VALUE).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-usuario
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-usuario F-Frame-Win
ON LEAVE OF fi-usuario IN FRAME F-Main /* Usu rio API */
DO:
    ASSIGN c-cod-param = "Usuario API".
    RUN pi-grava-param (INPUT c-cod-param,
                        INPUT SELF:SCREEN-VALUE).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK F-Frame-Win 


/* ***************************  Main Block  *************************** */
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN
   /* Now enable the interface  if in test mode - otherwise this happens when
      the object is explicitly initialized from its container. */
   RUN dispatch IN THIS-PROCEDURE ('initialize':U).

&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects F-Frame-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available F-Frame-Win  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI F-Frame-Win  _DEFAULT-DISABLE
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
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI F-Frame-Win  _DEFAULT-ENABLE
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
  DISPLAY fi-usuario fi-senha fi-url ed-token fi-natur-oper 
      WITH FRAME F-Main.
  ENABLE RECT-3 fi-usuario fi-senha fi-url ed-token fi-natur-oper 
      WITH FRAME F-Main.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-busca-param F-Frame-Win 
PROCEDURE pi-busca-param :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}.
    END.
    ASSIGN fi-usuario:SCREEN-VALUE = fn-param("Usuario API")
           fi-url:SCREEN-VALUE = fn-param("URL API")
           ed-token:SCREEN-VALUE = fn-param("Token API")
           fi-senha:SCREEN-VALUE = fn-param("Senha API")
           fi-natur-oper:SCREEN-VALUE = fn-param("NatOper Saida").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-entry F-Frame-Win 
PROCEDURE pi-entry :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}.
    END.

    RUN get-link-handle IN adm-broker-hdl (INPUT  THIS-PROCEDURE,
                                           INPUT  "CONTAINER",
                                           OUTPUT c-container).


    ASSIGN c-lst-grp-usuar = ''.
    FOR EACH grp_usuar NO-LOCK
          BY grp_usuar.cod_grp_usuar.
        ASSIGN c-lst-grp-usuar = IF c-lst-grp-usuar = '' 
                                 THEN grp_usuar.cod_grp_usuar
                                 ELSE c-lst-grp-usuar + ',' + grp_usuar.cod_grp_usuar.
    END.
    ASSIGN c-lst-grp-usuar = ',' + c-lst-grp-usuar.




    RUN pi-busca-param.

    /*
    RUN pi-habilita-folder IN WIDGET-HANDLE(c-container) (INPUT tg-tab-prod:INPUT-VALUE, INPUT 2).
    RUN pi-habilita-folder IN WIDGET-HANDLE(c-container) (INPUT tg-tab-cli:INPUT-VALUE, INPUT 3).
    RUN pi-habilita-folder IN WIDGET-HANDLE(c-container) (INPUT tg-tab-coml:INPUT-VALUE, INPUT 4).
    RUN pi-habilita-folder IN WIDGET-HANDLE(c-container) (INPUT tg-tab-pessoas:INPUT-VALUE, INPUT 5).
    RUN pi-habilita-folder IN WIDGET-HANDLE(c-container) (INPUT tg-tab-conc:INPUT-VALUE, INPUT 6).
    */
    APPLY 'ENTRY' TO fi-usuario IN FRAME {&FRAME-NAME}.
    RETURN NO-APPLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-grava-param F-Frame-Win 
PROCEDURE pi-grava-param :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER p-param AS CHAR.
    DEF INPUT PARAMETER p-valor AS CHAR.

    FIND lisa-param WHERE
         lisa-param.cod-param = p-param SHARE-LOCK NO-ERROR.
    IF NOT AVAIL lisa-param THEN DO.
       CREATE lisa-param.
       ASSIGN lisa-param.cod-param = p-param.
    END.
    ASSIGN lisa-param.val-param = p-valor.
           

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records F-Frame-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this CustomViewDigita, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed F-Frame-Win 
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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-param F-Frame-Win 
FUNCTION fn-param RETURNS CHARACTER ( p1 AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEF VAR c-retorno AS CHAR.

    FIND lisa-param WHERE
         lisa-param.cod-param = p1 NO-LOCK NO-ERROR.
    IF AVAIL lisa-param THEN
        ASSIGN c-retorno = lisa-param.val-param.
    
    RETURN c-retorno.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

