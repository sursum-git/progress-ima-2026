&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          ems206           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
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
&Scop adm-attribute-dlg support/viewerd.w

/* global variable definitions */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
def var v-row-parent as rowid no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES ped-item
&Scoped-define FIRST-EXTERNAL-TABLE ped-item


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR ped-item.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS ped-item.nome-abrev 
&Scoped-define ENABLED-TABLES ped-item
&Scoped-define FIRST-ENABLED-TABLE ped-item
&Scoped-Define ENABLED-OBJECTS RECT-2 rt-key rt-mold 
&Scoped-Define DISPLAYED-FIELDS ped-item.nome-abrev ped-item.nr-pedcli ~
ped-item.nr-sequencia ped-item.dt-entrega ped-item.it-codigo ~
ped-item.cod-sit-item 
&Scoped-define DISPLAYED-TABLES ped-item
&Scoped-define FIRST-DISPLAYED-TABLE ped-item
&Scoped-Define DISPLAYED-OBJECTS fi-nome-emit fi-desc-item cod-refer ~
fi-lote fi-compl-lote fi-acondicionamento fi-reservado fi-obs-it-cliente 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define List-4 fi-lote fi-acondicionamento fi-obs-it-cliente 
&Scoped-define List-5 cod-refer 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "",
     Keys-Supplied = ""':U).
/**************************
</EXECUTING-CODE> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE cod-refer LIKE ped-item.cod-refer
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE fi-acondicionamento AS CHARACTER FORMAT "x(10)" 
     LABEL "Acondicionamento" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-compl-lote AS CHARACTER FORMAT "X(7)":U 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-item AS CHARACTER FORMAT "x(60)" 
     VIEW-AS FILL-IN 
     SIZE 52.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-lote AS CHARACTER FORMAT "x(2)" 
     LABEL "Lote" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nome-emit AS CHARACTER FORMAT "X(40)" 
     VIEW-AS FILL-IN 
     SIZE 52.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-obs-it-cliente AS CHARACTER FORMAT "X(40)" 
     LABEL "Obs.Item Cliente" 
     VIEW-AS FILL-IN 
     SIZE 44 BY .88 TOOLTIP "Observaá∆o do item/referància para o Cliente." NO-UNDO.

DEFINE VARIABLE fi-reservado AS LOGICAL FORMAT "Sim/Nao" INITIAL NO 
     LABEL "Reservado" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 84 BY 1.75.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88 BY 7.25.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88 BY 3.54.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     ped-item.nome-abrev AT ROW 1.17 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16.14 BY .88
     fi-nome-emit AT ROW 1.17 COL 32.29 COLON-ALIGNED HELP
          "Nome Completo do Emitente" NO-LABEL
     ped-item.nr-pedcli AT ROW 2.17 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16.14 BY .88
     ped-item.nr-sequencia AT ROW 3.17 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY .88
     ped-item.dt-entrega AT ROW 3.17 COL 40 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
     ped-item.it-codigo AT ROW 4.17 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.14 BY .88
     fi-desc-item AT ROW 4.17 COL 33.29 COLON-ALIGNED NO-LABEL
     cod-refer AT ROW 5.17 COL 16 COLON-ALIGNED HELP
          ""
     ped-item.cod-sit-item AT ROW 6.79 COL 5.72 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Aberto", 1,
"Atendido Parcial", 2,
"Atendido Total", 3,
"Pendente", 4,
"Suspenso", 5,
"Cancelado", 6,
"Fatur Balc∆o", 7
          SIZE 79.43 BY .71
     fi-lote AT ROW 8.83 COL 16 COLON-ALIGNED
     fi-compl-lote AT ROW 8.83 COL 20 COLON-ALIGNED NO-LABEL
     fi-acondicionamento AT ROW 9.83 COL 16 COLON-ALIGNED HELP
          "Acondicionamento do item (Ex.: Rolo 100, Peca 30)"
     fi-reservado AT ROW 9.83 COL 40 COLON-ALIGNED HELP
          "Indica se o item ja foi reservado ou nao"
     fi-obs-it-cliente AT ROW 10.83 COL 16 COLON-ALIGNED HELP
          "Observaá∆o do item para o Cliente"
     "Situaá∆o do ÷tem de Pedido" VIEW-AS TEXT
          SIZE 19.29 BY .54 AT ROW 5.96 COL 34.72
     RECT-2 AT ROW 6.25 COL 3
     rt-key AT ROW 1 COL 1
     rt-mold AT ROW 8.46 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: movdis.ped-item
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 11.17
         WIDTH              = 88.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
{include/c-viewer.i}
{utp/ut-glob.i}
{include/i_dbtype.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME f-main:SCROLLABLE       = FALSE
       FRAME f-main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN cod-refer IN FRAME f-main
   NO-ENABLE 5 LIKE = ems206mov.ped-item. EXP-SIZE                      */
/* SETTINGS FOR RADIO-SET ped-item.cod-sit-item IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ped-item.dt-entrega IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-acondicionamento IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-compl-lote IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-item IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-lote IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-nome-emit IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-obs-it-cliente IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-reservado IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ped-item.it-codigo IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ped-item.nr-pedcli IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ped-item.nr-sequencia IN FRAME f-main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-main
/* Query rebuild information for FRAME f-main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME f-main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME fi-acondicionamento
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-acondicionamento V-table-Win
ON LEAVE OF fi-acondicionamento IN FRAME f-main /* Acondicionamento */
DO:
  IF (lookup(substr(INPUT FRAME {&FRAME-NAME} fi-acondicionamento,1,4),"Peca,Rolo") = 0) OR
     (substr(INPUT FRAME {&FRAME-NAME} fi-acondicionamento,5,1) <> " ") OR
     (INT(substr(INPUT FRAME {&FRAME-NAME} fi-acondicionamento,6,4)) = 0) THEN DO:
     MESSAGE "Acondicionamento deve ser, por exemplo: Rolo 100, Peca 30." VIEW-AS ALERT-BOX.
     APPLY 'entry' TO fi-acondicionamento IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-lote
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-lote V-table-Win
ON LEAVE OF fi-lote IN FRAME f-main /* Lote */
DO:
  IF lookup(INPUT FRAME {&FRAME-NAME} fi-lote,"PP,PD,RP,RD") = 0 THEN DO:
     MESSAGE "Lote deve ser PP,PD,RP ou RD." VIEW-AS ALERT-BOX.
     APPLY 'entry' TO fi-lote IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win  _ADM-ROW-AVAILABLE
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

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "ped-item"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "ped-item"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win  _DEFAULT-DISABLE
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
  HIDE FRAME f-main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
    {include/i-valid.i}
    
    /* Ponha na pi-validate todas as validaá‰es */
    /* N∆o gravar nada no registro antes do dispatch do assign-record e 
       nem na PI-validate. */
    
  /* Dispatch standard ADM method.                             */
  /* --- Comentado porque viewer s¢ tem campos fill-in habilitados ---
  * RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
  * if RETURN-VALUE = 'ADM-ERROR':U then 
  *    return 'ADM-ERROR':U. 
  -----------------------------------Gilvando Nov/2003---------------*/
  RUN pi-validate.
  if RETURN-VALUE = 'ADM-ERROR':U then 
     return 'ADM-ERROR':U.

  FIND ped-venda OF ped-item NO-LOCK NO-ERROR.                               /*  daf  */
  FIND ped-item-ext WHERE ped-item-ext.cod-estabel  = ped-venda.cod-estabel  /*  daf  */
                      AND ped-item-ext.nome-abrev   = ped-item.nome-abrev
                      AND ped-item-ext.nr-pedcli    = ped-item.nr-pedcli
                      AND ped-item-ext.nr-sequencia = ped-item.nr-sequencia
                      AND ped-item-ext.it-codigo    = ped-item.it-codigo
                      AND ped-item-ext.cod-refer    = ped-item.cod-refer
                      SHARE-LOCK NO-ERROR.
  IF NOT AVAIL ped-item-ext THEN DO:
     CREATE ped-item-ext.
     ASSIGN ped-item-ext.cod-estabel  = ped-venda.cod-estabel   /*  daf  */
            ped-item-ext.nome-abrev   = ped-item.nome-abrev  
            ped-item-ext.nr-pedcli    = ped-item.nr-pedcli   
            ped-item-ext.nr-sequencia = ped-item.nr-sequencia
            ped-item-ext.it-codigo    = ped-item.it-codigo   
            ped-item-ext.cod-refer    = ped-item.cod-refer.   
  END.
  ASSIGN ped-item-ext.lote = INPUT FRAME {&FRAME-NAME} fi-lote + ped-item.cod-refer
         ped-item-ext.acondicionamento = INPUT FRAME {&FRAME-NAME} fi-acondicionamento
         ped-item-ext.obs-it-cliente = INPUT FRAME {&FRAME-NAME} fi-obs-it-cliente.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-disable-fields V-table-Win 
PROCEDURE local-disable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    
    /* Code placed here will execute PRIOR to standard behavior. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'disable-fields':U ) .
    
    /* Code placed here will execute AFTER standard behavior.    */
    &if  defined(ADM-MODIFY-FIELDS) &then
    disable {&ADM-MODIFY-FIELDS} with frame {&frame-name}.
    &endif
    
    &if defined(list-4) &then
        DISABLE {&list-4} WITH FRAME {&FRAME-NAME}.
    &endif
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    
    /* Code placed here will execute PRIOR to standard behavior. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ).
    
    /* Code placed here will execute AFTER standard behavior.    */
    
    FIND emitente WHERE emitente.nome-abrev = ped-item.nome-abrev NO-LOCK NO-ERROR.
    IF AVAIL emitente THEN
       ASSIGN fi-nome-emit:SCREEN-VALUE IN FRAME {&FRAME-NAME} = emitente.nome-emit.
    
    FIND ITEM WHERE ITEM.it-codigo = ped-item.it-codigo NO-LOCK NO-ERROR.
    IF AVAIL ITEM THEN
       ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ITEM.desc-item.

    IF AVAIL ped-item then
       ASSIGN cod-refer:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ped-item.cod-refer.
    
    FIND ped-item-ext WHERE ped-item-ext.nome-abrev   = ped-item.nome-abrev
                        AND ped-item-ext.nr-pedcli    = ped-item.nr-pedcli
                        AND ped-item-ext.nr-sequencia = ped-item.nr-sequencia
                        AND ped-item-ext.it-codigo    = ped-item.it-codigo
                        AND ped-item-ext.cod-refer    = ped-item.cod-refer
                      NO-LOCK NO-ERROR.
    IF AVAIL ped-item-ext THEN
       ASSIGN fi-lote:SCREEN-VALUE IN FRAME {&frame-name} = ped-item-ext.lote
              fi-compl-lote:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ped-item-ext.cod-refer
              fi-acondicionamento:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ped-item-ext.acondicionamento
              fi-obs-it-cliente:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ped-item-ext.obs-it-cliente
              fi-reservado:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(ped-item-ext.reservado). 
    ELSE
       ASSIGN fi-lote:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
              fi-compl-lote:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
              fi-acondicionamento:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
              fi-obs-it-cliente:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
              fi-reservado:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Nao".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields V-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    
    /* Code placed here will execute PRIOR to standard behavior. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .
    
    /* Code placed here will execute AFTER standard behavior.    */
    &if  defined(ADM-MODIFY-FIELDS) &then
    if adm-new-record = yes then
        enable {&ADM-MODIFY-FIELDS} with frame {&frame-name}.
    &endif
    
    &if defined(list-4) &then
        ENABLE {&list-4} WITH FRAME {&FRAME-NAME}.
    &endif

    &if defined(list-5) &then
        DISABLE {&list-5} WITH FRAME {&FRAME-NAME}.
    &endif
    
    ASSIGN ped-item.nome-abrev:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-atualiza-parent V-table-Win 
PROCEDURE pi-atualiza-parent :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    define input parameter v-row-parent-externo as rowid no-undo.
    
    assign v-row-parent = v-row-parent-externo.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pi-validate V-table-Win 
PROCEDURE Pi-validate :
/*------------------------------------------------------------------------------
  Purpose:Validar a viewer     
  Parameters:  <none>
  Notes: N∆o fazer assign aqui. Nesta procedure
  devem ser colocadas apenas validaá‰es, pois neste ponto do programa o registro 
  ainda n∆o foi criado.       
------------------------------------------------------------------------------*/
    {include/i-vldfrm.i} /* Validaá∆o de dicion†rio */
    
/*/*    Segue um exemplo de validaá∆o de programa */
 *     find tabela where tabela.campo1 = c-variavel and
 *                       tabela.campo2 > i-variavel no-lock no-error.
 *     
 *     /* Este include deve ser colocado sempre antes do ut-msgs.p */
 *     {include/i-vldprg.i}
 *     run utp/ut-msgs.p (input "show":U, input 7, input return-value).
 *     return 'ADM-ERROR':U.*/

  IF lookup(INPUT FRAME {&FRAME-NAME} fi-lote,"PP,PD,RP,RD") = 0 THEN DO:
     MESSAGE "Lote deve ser PP,PD,RP ou RD." VIEW-AS ALERT-BOX.
     APPLY 'entry' TO fi-lote IN FRAME {&FRAME-NAME}.
     return 'ADM-ERROR':U.                                                                    
  END.

  IF (lookup(substr(INPUT FRAME {&FRAME-NAME} fi-acondicionamento,1,4),"Peca,Rolo") = 0) OR
     (substr(INPUT FRAME {&FRAME-NAME} fi-acondicionamento,5,1) <> " ") OR
     (INT(substr(INPUT FRAME {&FRAME-NAME} fi-acondicionamento,6,4)) = 0) THEN DO:
     MESSAGE "Acondicionamento deve ser, por exemplo: Rolo 100, Peca 30." VIEW-AS ALERT-BOX.
     APPLY 'entry' TO fi-acondicionamento IN FRAME {&FRAME-NAME}.
     return 'ADM-ERROR':U.                                                                    
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "ped-item"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed V-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  IF p-state = "update-begin" THEN DO:
     FIND ITEM OF ped-item NO-LOCK NO-ERROR.  
     IF ITEM.tipo-con-est <> 4 THEN DO:
       MESSAGE "Item n∆o Ç controlado por Referància, ent∆o n∆o requer Lote/Acondicionamento" 
               VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
     END.
  END.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/vstates.i}
  END CASE.

  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

