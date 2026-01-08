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
{include/i-prgvrs.i ESPD4000B 2.04.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEF INPUT        PARAMETER p-row-ped-item AS ROWID.
DEF INPUT-OUTPUT PARAMETER p-nr-seq-div LIKE ped-item.nr-sequencia.
DEF OUTPUT       PARAMETER p-corte-div AS CHAR.
DEF OUTPUT       PARAMETER p-qtd-div AS DEC.
DEF OUTPUT       PARAMETER p-novo-corte AS CHAR.
DEF OUTPUT       PARAMETER l-ok AS LOG.

/* Buffer Definitions ---                                               */

/* Local Variable Definitions ---                                       */
DEF VAR h-menu      AS HANDLE.
DEF VAR h-menu-item AS HANDLE.

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
&Scoped-Define ENABLED-OBJECTS RECT-45 rt-buttom fi-qt-pedida-div ~
fi-corte-comerc-div bt-ok bt-cancela bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS fi-nr-seq fi-it-codigo fi-desc-item ~
fi-cod-refer fi-lote fi-qt-pedida fi-preco-un fi-corte-comerc fi-desc-corte ~
fi-nova-qtde fi-novo-corte fi-desc-novo-corte fi-nr-seq-div ~
fi-qt-pedida-div fi-corte-comerc-div fi-desc-corte-div 

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

DEFINE VARIABLE fi-cod-refer AS CHARACTER FORMAT "x(16)":U 
     LABEL "Referˆncia" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE fi-corte-comerc AS CHARACTER FORMAT "x":U 
     LABEL "Corte Comerc." 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-corte-comerc-div AS CHARACTER FORMAT "x":U 
     LABEL "Corte Comerc," 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 TOOLTIP "Clique com BotÆo Direito para Sele‡Æo de Cortes" NO-UNDO.

DEFINE VARIABLE fi-desc-corte AS CHARACTER FORMAT "X(13)":U 
     VIEW-AS FILL-IN 
     SIZE 41.43 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-corte-div AS CHARACTER FORMAT "X(20)":U 
     VIEW-AS FILL-IN 
     SIZE 50.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-item AS CHARACTER FORMAT "X(13)":U 
     VIEW-AS FILL-IN 
     SIZE 43.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-novo-corte AS CHARACTER FORMAT "X(13)":U 
     VIEW-AS FILL-IN 
     SIZE 41.43 BY .88 NO-UNDO.

DEFINE VARIABLE fi-it-codigo AS CHARACTER FORMAT "x(16)":U 
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE fi-lote AS CHARACTER FORMAT "x(2)":U 
     LABEL "Lote" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nova-qtde AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Nova Qtde" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88
     FGCOLOR 12 FONT 0 NO-UNDO.

DEFINE VARIABLE fi-novo-corte AS CHARACTER FORMAT "x":U 
     LABEL "Novo Corte" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nr-seq AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Sequencia" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nr-seq-div AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Sequencia" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE VARIABLE fi-preco-un AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Pre‡o" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-qt-pedida AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Qtde Pedida" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-qt-pedida-div AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Quantidade" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-45
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 68 BY 12.5.

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 68 BY 1.42
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     fi-nr-seq AT ROW 1.5 COL 12 COLON-ALIGNED
     fi-it-codigo AT ROW 2.5 COL 12 COLON-ALIGNED
     fi-desc-item AT ROW 2.5 COL 23.43 COLON-ALIGNED NO-LABEL
     fi-cod-refer AT ROW 3.5 COL 12 COLON-ALIGNED
     fi-lote AT ROW 3.5 COL 46 COLON-ALIGNED
     fi-qt-pedida AT ROW 4.5 COL 12 COLON-ALIGNED
     fi-preco-un AT ROW 4.5 COL 46 COLON-ALIGNED
     fi-corte-comerc AT ROW 5.5 COL 12 COLON-ALIGNED
     fi-desc-corte AT ROW 5.5 COL 16.43 COLON-ALIGNED NO-LABEL
     fi-nova-qtde AT ROW 7.25 COL 12 COLON-ALIGNED
     fi-novo-corte AT ROW 8.25 COL 12 COLON-ALIGNED
     fi-desc-novo-corte AT ROW 8.25 COL 16.43 COLON-ALIGNED NO-LABEL
     fi-nr-seq-div AT ROW 10.25 COL 12 COLON-ALIGNED
     fi-qt-pedida-div AT ROW 11.25 COL 12 COLON-ALIGNED
     fi-corte-comerc-div AT ROW 12.25 COL 12 COLON-ALIGNED
     fi-desc-corte-div AT ROW 12.25 COL 16.43 COLON-ALIGNED NO-LABEL
     bt-ok AT ROW 14.25 COL 3
     bt-cancela AT ROW 14.25 COL 14
     bt-ajuda AT ROW 14.25 COL 59
     "Dados Nova Sequencia" VIEW-AS TEXT
          SIZE 66 BY .79 AT ROW 9.25 COL 3
          BGCOLOR 9 FGCOLOR 15 FONT 6
     RECT-45 AT ROW 1.25 COL 2
     rt-buttom AT ROW 14 COL 2
     SPACE(0.85) SKIP(0.49)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 1
         TITLE "DivisÆo Itens"
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

/* SETTINGS FOR FILL-IN fi-cod-refer IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-corte-comerc IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-corte IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-corte-div IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-item IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-novo-corte IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-it-codigo IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-lote IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nova-qtde IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-novo-corte IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nr-seq IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nr-seq-div IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-preco-un IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qt-pedida IN FRAME D-Dialog
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
ON WINDOW-CLOSE OF FRAME D-Dialog /* DivisÆo Itens */
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


&Scoped-define SELF-NAME bt-cancela
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancela D-Dialog
ON CHOOSE OF bt-cancela IN FRAME D-Dialog /* Cancelar */
DO:
  ASSIGN l-ok = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok D-Dialog
ON CHOOSE OF bt-ok IN FRAME D-Dialog /* OK */
DO:
   IF INPUT FRAME {&FRAME-NAME} fi-qt-pedida-div = 0 THEN DO.
      MESSAGE 'Quantidade da Nova Sequˆncia deve ser Maior que Zero....'
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY 'entry' TO fi-qt-pedida-div.
      RETURN NO-APPLY.
   END.

   IF INPUT FRAME {&FRAME-NAME} fi-qt-pedida-div >= fi-nova-qtde THEN DO.
      MESSAGE 'Quantidade da Nova Sequˆncia deve ser Menor que Quantidade Pedida Original....'
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY 'entry' TO fi-qt-pedida-div.
      RETURN NO-APPLY.
   END.

   FIND corte-comerc WHERE
        corte-comerc.codigo = INPUT FRAME {&FRAME-NAME} fi-novo-corte
        NO-LOCK NO-ERROR.
   IF INPUT FRAME {&FRAME-NAME} fi-nova-qtde < corte-comerc.compr-min THEN DO.
      MESSAGE 'Corte Comercial Inv lido para Quantidade Origem...'
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      ASSIGN fi-novo-corte:SENSITIVE = YES.
      APPLY 'entry' TO fi-novo-corte.
      RETURN NO-APPLY.
   END.
   ASSIGN p-novo-corte = corte-comerc.codigo + " - " + corte-comerc.descricao.

   FIND corte-comerc WHERE
        corte-comerc.codigo = INPUT FRAME {&FRAME-NAME} fi-corte-comerc-div
        NO-LOCK NO-ERROR.
   IF NOT AVAIL corte-comerc THEN DO.
      MESSAGE 'Corte Comercial nÆo Cadastado...'
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY 'entry' TO fi-corte-comerc-div.
      RETURN NO-APPLY.
   END.
   ASSIGN p-corte-div = corte-comerc.codigo + " - " + corte-comerc.descricao.

   IF INPUT FRAME {&FRAME-NAME} fi-qt-pedida-div < corte-comerc.compr-min THEN DO.
      MESSAGE 'Corte Comercial Inv lido para Quantidade Informada...'
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY 'entry' TO fi-corte-comerc-div.
      RETURN NO-APPLY.
   END.

   ASSIGN p-nr-seq-div = INPUT FRAME {&FRAME-NAME} fi-nr-seq-div
          p-qtd-div = INPUT FRAME {&FRAME-NAME} fi-qt-pedida-div
          l-ok = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-corte-comerc-div
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-corte-comerc-div D-Dialog
ON ENTRY OF fi-corte-comerc-div IN FRAME D-Dialog /* Corte Comerc, */
DO:
   ASSIGN SELF:POPUP-MENU = h-menu.
   STATUS INPUT "Click BotÆo Direito do Mouse para Cortes".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-corte-comerc-div D-Dialog
ON LEAVE OF fi-corte-comerc-div IN FRAME D-Dialog /* Corte Comerc, */
DO:
  FIND corte-comerc WHERE
       corte-comerc.codigo = INPUT FRAME {&FRAME-NAME} fi-corte-comerc-div
       NO-LOCK NO-ERROR.
  IF NOT AVAIL corte-comerc THEN DO.
     MESSAGE 'Corte Comercial nÆo Cadastado...'
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     APPLY 'entry' TO SELF.
     RETURN NO-APPLY.
  END.
  ASSIGN fi-desc-corte-div:SCREEN-VALUE IN FRAME {&FRAME-NAME} = corte-comerc.descricao.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-novo-corte
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-novo-corte D-Dialog
ON LEAVE OF fi-novo-corte IN FRAME D-Dialog /* Novo Corte */
DO:
  FIND corte-comerc WHERE
       corte-comerc.codigo = INPUT FRAME {&FRAME-NAME} fi-novo-corte
       NO-LOCK NO-ERROR.
  IF NOT AVAIL corte-comerc THEN DO.
     MESSAGE 'Corte Comercial nÆo Cadastado...'
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     APPLY 'entry' TO SELF.
     RETURN NO-APPLY.
  END.
  ASSIGN fi-desc-novo-corte:SCREEN-VALUE IN FRAME {&FRAME-NAME} = corte-comerc.descricao.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-qt-pedida-div
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-qt-pedida-div D-Dialog
ON VALUE-CHANGED OF fi-qt-pedida-div IN FRAME D-Dialog /* Quantidade */
DO:
  ASSIGN fi-nova-qtde:SCREEN-VALUE = STRING(fi-qt-pedida:INPUT-VALUE - SELF:INPUT-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */
/* Cria um POPUP Menu com os Cortes Comerciais */
CREATE MENU h-menu
      ASSIGN POPUP-ONLY = TRUE.

FOR EACH corte-comerc.
    CREATE MENU-ITEM h-menu-item
           ASSIGN PARENT = h-menu
                   LABEL = corte-comerc.codigo + " - " + corte-comerc.descricao
          TRIGGERS:
             ON CHOOSE DO.
                ASSIGN fi-corte-comerc-div:SCREEN-VALUE IN FRAME {&FRAME-NAME} = TRIM(ENTRY(1,SELF:LABEL,"-"))
                       fi-desc-corte-div:SCREEN-VALUE IN FRAME {&FRAME-NAME} = TRIM(ENTRY(2,SELF:LABEL,"-")).
                APPLY 'entry' TO fi-corte-comerc-div IN FRAME {&FRAME-NAME}.
             END.
          END TRIGGERS.
END.

fi-corte-comerc-div:LOAD-MOUSE-POINTER("image/rbm.cur").

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
  DISPLAY fi-nr-seq fi-it-codigo fi-desc-item fi-cod-refer fi-lote fi-qt-pedida 
          fi-preco-un fi-corte-comerc fi-desc-corte fi-nova-qtde fi-novo-corte 
          fi-desc-novo-corte fi-nr-seq-div fi-qt-pedida-div fi-corte-comerc-div 
          fi-desc-corte-div 
      WITH FRAME D-Dialog.
  ENABLE RECT-45 rt-buttom fi-qt-pedida-div fi-corte-comerc-div bt-ok 
         bt-cancela bt-ajuda 
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

  {utp/ut9000.i "ESPD4000B" "2.04.00.000"}

  FIND ped-item WHERE
       ROWID(ped-item) = p-row-ped-item NO-LOCK NO-ERROR.
  IF AVAIL ped-item THEN DO.
     FIND ped-item-ext OF ped-item NO-LOCK NO-ERROR.
     FIND ITEM WHERE
          ITEM.it-codigo = ped-item.it-codigo NO-LOCK NO-ERROR.
     FIND corte-comerc WHERE
          corte-comerc.codigo = ped-item-ext.corte-comerc NO-LOCK NO-ERROR.

     ASSIGN fi-nr-seq = ped-item.nr-sequencia
            fi-nr-seq-div = p-nr-seq-div
            fi-it-codigo = ped-item.it-codigo
            fi-desc-item = item.desc-item
            fi-cod-refer = ped-item.cod-refer
            fi-qt-pedida = ped-item.qt-pedida
            fi-nova-qtde = ped-item.qt-pedida
            fi-preco-un = ped-item.vl-preuni
            fi-lote = ped-item-ext.lote
            fi-corte-comerc = ped-item-ext.corte-comerc
            fi-novo-corte = ped-item-ext.corte-comerc
            fi-desc-corte = corte-comerc.descricao
            fi-desc-novo-corte = corte-comerc.descricao
            fi-corte-comerc-div = ped-item-ext.corte-comerc
            fi-desc-corte-div = corte-comerc.descricao.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  APPLY 'entry' TO fi-qt-pedida-div.
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

