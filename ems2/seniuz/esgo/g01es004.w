&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          espec            PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME d-vapara
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS d-vapara 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i V99XX999 9.99.99.999}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
define output parameter     p-row-tabela    as rowid    no-undo.

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartVaPara
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME d-vapara

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS contr-sac.fm-codigo contr-sac.des-cor ~
contr-sac.data-reg contr-sac.cod-emitente 
&Scoped-define ENABLED-TABLES contr-sac
&Scoped-define FIRST-ENABLED-TABLE contr-sac
&Scoped-Define ENABLED-OBJECTS bt-ok bt-cancela bt-ajuda rt-button 
&Scoped-Define DISPLAYED-FIELDS contr-sac.fm-codigo contr-sac.des-cor ~
contr-sac.data-reg contr-sac.cod-emitente 
&Scoped-define DISPLAYED-TABLES contr-sac
&Scoped-define FIRST-DISPLAYED-TABLE contr-sac
&Scoped-Define DISPLAYED-OBJECTS fi-desc-familia fi-nome-emit 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-bt-ajuda 
       MENU-ITEM mi-sobre       LABEL "Sobre..."      .


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

DEFINE VARIABLE fi-desc-familia AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 38.43 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nome-emit AS CHARACTER FORMAT "X(40)" 
     VIEW-AS FILL-IN 
     SIZE 43 BY .88 NO-UNDO.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 68 BY 1.42
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME d-vapara
     contr-sac.fm-codigo AT ROW 1.17 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.14 BY .88
     fi-desc-familia AT ROW 1.17 COL 27.29 COLON-ALIGNED NO-LABEL
     contr-sac.des-cor AT ROW 2.17 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.14 BY .88
     contr-sac.data-reg AT ROW 3.17 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.57 BY .88
     contr-sac.cod-emitente AT ROW 4.17 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY .88
     fi-nome-emit AT ROW 4.17 COL 23.14 COLON-ALIGNED HELP
          "Nome Completo do Emitente" NO-LABEL
     bt-ok AT ROW 5.5 COL 2.14
     bt-cancela AT ROW 5.5 COL 13
     bt-ajuda AT ROW 5.5 COL 58
     rt-button AT ROW 5.25 COL 1
     SPACE(0.13) SKIP(0.11)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "V  Para Controle de SAC"
         DEFAULT-BUTTON bt-ok CANCEL-BUTTON bt-cancela.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartVaPara
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB d-vapara 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/d-vapara.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX d-vapara
   L-To-R                                                               */
ASSIGN 
       FRAME d-vapara:SCROLLABLE       = FALSE
       FRAME d-vapara:HIDDEN           = TRUE.

ASSIGN 
       bt-ajuda:POPUP-MENU IN FRAME d-vapara       = MENU POPUP-MENU-bt-ajuda:HANDLE.

/* SETTINGS FOR FILL-IN fi-desc-familia IN FRAME d-vapara
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nome-emit IN FRAME d-vapara
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX d-vapara
/* Query rebuild information for DIALOG-BOX d-vapara
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX d-vapara */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME d-vapara
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL d-vapara d-vapara
ON GO OF FRAME d-vapara /* V  Para Controle de SAC */
DO:
  find contr-sac no-lock 
      where contr-sac.fm-codigo    = input frame {&frame-name} contr-sac.fm-codigo
        AND contr-sac.des-cor      = INPUT FRAME {&FRAME-NAME} contr-sac.des-cor
        AND contr-sac.data-reg     = INPUT FRAME {&FRAME-NAME} contr-sac.data-reg
        AND contr-sac.cod-emitente = INPUT FRAME {&FRAME-NAME} contr-sac.cod-emitente
      no-error.
  if  not avail contr-sac then do:
      {utp/ut-table.i espec contr-sac 1}
      run utp/ut-msgs.p (input "show",
                         input 2,
                         input RETURN-VALUE).
      return no-apply.
  end.
  assign p-row-tabela = rowid(contr-sac).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL d-vapara d-vapara
ON WINDOW-CLOSE OF FRAME d-vapara /* V  Para Controle de SAC */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda d-vapara
ON CHOOSE OF bt-ajuda IN FRAME d-vapara /* Ajuda */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME contr-sac.cod-emitente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contr-sac.cod-emitente d-vapara
ON LEAVE OF contr-sac.cod-emitente IN FRAME d-vapara /* Cliente */
DO:
   FIND emitente WHERE emitente.cod-emitente = INPUT FRAME {&FRAME-NAME} contr-sac.cod-emitente
                 NO-LOCK NO-ERROR.
   IF AVAIL emitente THEN
      ASSIGN fi-nome-emit:SCREEN-VALUE IN FRAME {&FRAME-NAME} = emitente.nome-emit.
   ELSE DO:
      MESSAGE "Cliente inv lido." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME contr-sac.fm-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contr-sac.fm-codigo d-vapara
ON LEAVE OF contr-sac.fm-codigo IN FRAME d-vapara /* Familia */
DO:
  FIND familia WHERE familia.fm-codigo = INPUT FRAME {&FRAME-NAME} contr-sac.fm-codigo
               NO-LOCK NO-ERROR.
  IF AVAIL familia THEN
     ASSIGN fi-desc-familia:SCREEN-VALUE IN FRAME {&FRAME-NAME} = familia.descricao.
  ELSE DO:
     MESSAGE "Fam¡lia inv lida." VIEW-AS ALERT-BOX.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sobre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sobre d-vapara
ON CHOOSE OF MENU-ITEM mi-sobre /* Sobre... */
DO:
  {include/sobre.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK d-vapara 


/* ***************************  Main Block  *************************** */

assign p-row-tabela = ?.

{src/adm/template/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects d-vapara  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available d-vapara  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI d-vapara  _DEFAULT-DISABLE
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
  HIDE FRAME d-vapara.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI d-vapara  _DEFAULT-ENABLE
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
  DISPLAY fi-desc-familia fi-nome-emit 
      WITH FRAME d-vapara.
  IF AVAILABLE contr-sac THEN 
    DISPLAY contr-sac.fm-codigo contr-sac.des-cor contr-sac.data-reg 
          contr-sac.cod-emitente 
      WITH FRAME d-vapara.
  ENABLE contr-sac.fm-codigo contr-sac.des-cor contr-sac.data-reg 
         contr-sac.cod-emitente bt-ok bt-cancela bt-ajuda rt-button 
      WITH FRAME d-vapara.
  VIEW FRAME d-vapara.
  {&OPEN-BROWSERS-IN-QUERY-d-vapara}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy d-vapara 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize d-vapara 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  {utp/ut9000.i "G99XX999" "9.99.99.999"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records d-vapara  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartVaPara, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed d-vapara 
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

