&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*:T*******************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i D99XX999 9.99.99.999}

/* Chamada a include do gerenciador de licenáas. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */
/*                                                                                */
/* OBS: Para os smartobjects o parametro m¢dulo dever† ser MUT                    */

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i <programa> MUT}
&ENDIF

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEF INPUT PARAMETER p-nr-pedcli AS CHAR.
DEF INPUT PARAMETER p-acao AS INT.
DEF OUTPUT PARAMETER p-ok AS LOGICAL.

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE cErro AS CHARACTER   NO-UNDO.

DEFINE VARIABLE rowidNF AS ROWID       NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS rt-buttom RECT-99 edobs bt-ok bt-cancela ~
bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS fiPedido cbAcao edobs 

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

DEFINE VARIABLE cbAcao AS INTEGER FORMAT ">9":U INITIAL 1 
     LABEL "Aá∆o" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Reprovar Separaá∆o Pedido",1,
                     "Cancelar Pedido",2,
                     "Excluir Pedido",3,
                     "Troca NF Enviada",4,
                     "Canc.NF na LISA",5
     DROP-DOWN-LIST
     SIZE 34.57 BY 1 NO-UNDO.

DEFINE VARIABLE edobs AS CHARACTER 
     VIEW-AS EDITOR
     SIZE 40 BY 3.75 NO-UNDO.

DEFINE VARIABLE fiPedido AS CHARACTER FORMAT "X(20)":U 
     LABEL "Pedido" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1
     FONT 0 NO-UNDO.

DEFINE RECTANGLE RECT-99
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 61 BY 8.75.

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 61 BY 1.42
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     fiPedido AT ROW 2 COL 13 COLON-ALIGNED WIDGET-ID 2
     cbAcao AT ROW 3.38 COL 13 COLON-ALIGNED WIDGET-ID 10
     edobs AT ROW 5.5 COL 15 NO-LABEL WIDGET-ID 6
     bt-ok AT ROW 10.5 COL 3
     bt-cancela AT ROW 10.5 COL 14
     bt-ajuda AT ROW 10.5 COL 52
     "Observaá‰es Reprovaá∆o" VIEW-AS TEXT
          SIZE 21 BY .54 AT ROW 4.92 COL 15 WIDGET-ID 8
     rt-buttom AT ROW 10.25 COL 2
     RECT-99 AT ROW 1.25 COL 2 WIDGET-ID 12
     SPACE(0.56) SKIP(1.78)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 1
         TITLE "Reprovar Separaá∆o Pedido"
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

/* SETTINGS FOR COMBO-BOX cbAcao IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiPedido IN FRAME D-Dialog
   NO-ENABLE                                                            */
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
ON WINDOW-CLOSE OF FRAME D-Dialog /* Reprovar Separaá∆o Pedido */
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
   ASSIGN cErro = ''.
   IF NOT CAN-FIND(PED-VENDA WHERE ped-venda.nr-pedido = INT(fiPedido:SCREEN-VALUE)
                   AND ped-venda.cod-estabel = '505') THEN DO:
      MESSAGE "ê necess†rio Informar um Pedido Existente e que seja do estab. 505"
          VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
      RETURN NO-APPLY.
   END.
   ELSE DO:
      FIND ped-venda NO-LOCK
                  WHERE ped-venda.nr-pedido =INT(fiPedido:SCREEN-VALUE)
                  NO-ERROR.


      CASE cbAcao:SCREEN-VALUE :
          WHEN '1' THEN DO:
              IF edObs:SCREEN-VALUE = '' THEN DO:
                 MESSAGE 'ê obrigat¢rio informar o motivo da reprovaá∆o'
                     VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
                 RETURN NO-APPLY.
              END.
              RUN esapi/envia-reprovacao-sep-pedido-lisa.p(INPUT ROWID(ped-venda),
                                                           INPUT edObs:SCREEN-VALUE,
                                                           OUTPUT cErro).
          END.
          WHEN '2' THEN DO:
              RUN esapi/envia-cancelamento-pedido.p(INPUT ROWID(ped-venda),
                                                    OUTPUT cErro).
          END.
          WHEN '3'  THEN DO:
              RUN esapi/envia-exclusao-pedido.p(INPUT ROWID(ped-venda),
                                                OUTPUT cErro).
          END.
          WHEN '4' THEN DO:
              RUN esapi/getNFVendaPedido.p(ROWID(ped-venda),OUTPUT rowidNF,OUTPUT cErro).
              IF cErro = '' THEN DO:
                 RUN envia-troca-nfs-venda-lisa.p(rowidNF). 
              END.
              ELSE DO:
                 MESSAGE cErro
                     VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
                 RETURN NO-APPLY.
              END.

          END.
          WHEN '5' THEN DO:
             RUN esapi/envia-canc-pedido-lisa.p(INPUT ROWID(ped-venda), OUTPUT cErro).
          END.
      END CASE.   
   END.

   IF cErro = '' THEN DO:
      MESSAGE "Aá∆o processada com sucesso..."
          VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
      ASSIGN p-ok = YES.
   END.
   ELSE DO:
       MESSAGE cErro
           VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
       //RETURN NO-APPLY.
       ASSIGN p-ok = YES.
   END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbAcao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbAcao D-Dialog
ON VALUE-CHANGED OF cbAcao IN FRAME D-Dialog /* Aá∆o */
DO:
  ASSIGN EdObs:SENSITIVE  = SELF:SCREEN-VALUE = '1'.
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
  DISPLAY fiPedido cbAcao edobs 
      WITH FRAME D-Dialog.
  ENABLE rt-buttom RECT-99 edobs bt-ok bt-cancela bt-ajuda 
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

  ASSIGN fiPedido = p-nr-pedcli
         cbAcao = p-acao.

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

