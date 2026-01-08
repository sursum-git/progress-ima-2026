&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME d-vapara
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS d-vapara 
/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i G01DI154 2.04.00.000}

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
DEF NEW GLOBAL SHARED VAR v-nome-abrev LIKE ped-item.nome-abrev.
DEF NEW GLOBAL SHARED VAR v-nr-pedcli LIKE ped-item.nr-pedcli.
DEF NEW GLOBAL SHARED VAR v-nr-sequencia LIKE ped-item.nr-sequencia.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartVaPara
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME d-vapara

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi-cliente fi-pedido fi-sequencia bt-ok ~
bt-cancela bt-ajuda RECT-1 rt-button 
&Scoped-Define DISPLAYED-OBJECTS fi-cliente fi-pedido fi-sequencia 

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

DEFINE VARIABLE fi-cliente AS CHARACTER FORMAT "X(256)":U 
     LABEL "Cliente" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-pedido AS CHARACTER FORMAT "X(256)":U 
     LABEL "Pedido Cliente" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-sequencia AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Sequˆncia" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 57.29 BY 3.75.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 57.57 BY 1.42
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME d-vapara
     fi-cliente AT ROW 1.63 COL 19 COLON-ALIGNED
     fi-pedido AT ROW 2.63 COL 19 COLON-ALIGNED
     fi-sequencia AT ROW 3.63 COL 19 COLON-ALIGNED
     bt-ok AT ROW 5.38 COL 2.57
     bt-cancela AT ROW 5.38 COL 13.43
     bt-ajuda AT ROW 5.38 COL 47.86
     RECT-1 AT ROW 1.21 COL 1.72
     rt-button AT ROW 5.13 COL 1.43
     SPACE(0.85) SKIP(0.32)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 1
         TITLE "V  Para Itens Pedido"
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
ON GO OF FRAME d-vapara /* V  Para Itens Pedido */
DO:
  FIND ped-item NO-LOCK WHERE
       ped-item.nome-abrev = INPUT FRAME {&FRAME-NAME} fi-cliente AND
       ped-item.nr-pedcli  = INPUT FRAME {&FRAME-NAME} fi-pedido AND
       ped-item.nr-sequencia = INPUT FRAME {&FRAME-NAME} fi-sequencia
       NO-ERROR.
  IF NOT AVAIL ped-item THEN DO:
     {utp/ut-table.i mgmov ped-item 1}
     run utp/ut-msgs.p (INPUT "show":U,
                        INPUT 2,
                        INPUT RETURN-VALUE).
     return no-apply.
  END.
  ASSIGN p-row-tabela = ROWID(ped-item).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL d-vapara d-vapara
ON WINDOW-CLOSE OF FRAME d-vapara /* V  Para Itens Pedido */
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


&Scoped-define SELF-NAME fi-cliente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cliente d-vapara
ON LEAVE OF fi-cliente IN FRAME d-vapara /* Cliente */
DO:
  FIND emitente WHERE
       emitente.nome-abrev = SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
  IF NOT AVAIL emitente THEN
     FIND emitente WHERE
          emitente.cod-emitente = INT(SELF:SCREEN-VALUE) NO-LOCK NO-ERROR.
  IF AVAIL emitente THEN
     ASSIGN SELF:SCREEN-VALUE = emitente.nome-abrev.
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

ASSIGN p-row-tabela = ?.

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
  DISPLAY fi-cliente fi-pedido fi-sequencia 
      WITH FRAME d-vapara.
  ENABLE fi-cliente fi-pedido fi-sequencia bt-ok bt-cancela bt-ajuda RECT-1 
         rt-button 
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

  {utp/ut9000.i "G01DI154" "2.04.00.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  ASSIGN fi-cliente:SCREEN-VALUE IN FRAME {&FRAME-NAME} = v-nome-abrev
         fi-pedido:SCREEN-VALUE IN FRAME {&FRAME-NAME} = v-nr-pedcli
         fi-sequencia:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(v-nr-sequencia).

  APPLY 'entry' TO fi-cliente.

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

