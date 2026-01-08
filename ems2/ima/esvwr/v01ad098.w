&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          ems2cad          PROGRESS
          ems2ima          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*:T *******************************************************************************
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

DEF VAR c-lst-finalidade AS CHAR.

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
&Scoped-define EXTERNAL-TABLES emitente
&Scoped-define FIRST-EXTERNAL-TABLE emitente


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR emitente.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS emitente.nome-emit 
&Scoped-define ENABLED-TABLES emitente
&Scoped-define FIRST-ENABLED-TABLE emitente
&Scoped-Define ENABLED-OBJECTS rt-key rt-mold RECT-5 
&Scoped-Define DISPLAYED-FIELDS emitente.cod-emitente emitente.nome-emit 
&Scoped-define DISPLAYED-TABLES emitente
&Scoped-define FIRST-DISPLAYED-TABLE emitente
&Scoped-Define DISPLAYED-OBJECTS fi-cod-ramo-ativ fi-desc-ramo ~
cb-finalidade tg-varejo tg-industria tg-atacado tg-servico 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-MODIFY-FIELDS fi-cod-ramo-ativ cb-finalidade tg-varejo ~
tg-industria tg-atacado tg-servico 

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
DEFINE VARIABLE cb-finalidade AS INTEGER FORMAT ">>>,>>9":U INITIAL 0 
     LABEL "Finalid. Venda" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1",0
     DROP-DOWN-LIST
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cod-ramo-ativ AS INTEGER FORMAT "999" INITIAL 0 
     LABEL "C¢digo Ramo" 
     VIEW-AS FILL-IN 
     SIZE 5.29 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-ramo AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40.14 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 47 BY 2.75.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.57 BY 1.5.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.57 BY 6.

DEFINE VARIABLE tg-atacado AS LOGICAL INITIAL no 
     LABEL "Atacado" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE tg-industria AS LOGICAL INITIAL no 
     LABEL "Industria" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE tg-servico AS LOGICAL INITIAL no 
     LABEL "Servi‡o" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE tg-varejo AS LOGICAL INITIAL no 
     LABEL "Varejo" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     emitente.cod-emitente AT ROW 1.25 COL 12.86 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 11.43 BY .88
     emitente.nome-emit AT ROW 1.25 COL 24.86 COLON-ALIGNED NO-LABEL WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 55 BY .88
     fi-cod-ramo-ativ AT ROW 3.25 COL 13 COLON-ALIGNED WIDGET-ID 16
     fi-desc-ramo AT ROW 3.25 COL 18.86 COLON-ALIGNED NO-LABEL WIDGET-ID 18
     cb-finalidade AT ROW 4.25 COL 13 COLON-ALIGNED WIDGET-ID 14
     tg-varejo AT ROW 6.25 COL 21 WIDGET-ID 6
     tg-industria AT ROW 6.25 COL 40 WIDGET-ID 10
     tg-atacado AT ROW 7 COL 21 WIDGET-ID 8
     tg-servico AT ROW 7 COL 40 WIDGET-ID 12
     " Atividade" VIEW-AS TEXT
          SIZE 8 BY .75 AT ROW 5.42 COL 15 WIDGET-ID 28
     rt-key AT ROW 1 COL 1
     rt-mold AT ROW 2.75 COL 1
     RECT-5 AT ROW 5.75 COL 14 WIDGET-ID 26
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 1 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ems2ima.emitente
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
         HEIGHT             = 7.92
         WIDTH              = 88.57.
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

/* SETTINGS FOR COMBO-BOX cb-finalidade IN FRAME f-main
   NO-ENABLE 3                                                          */
/* SETTINGS FOR FILL-IN emitente.cod-emitente IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-cod-ramo-ativ IN FRAME f-main
   NO-ENABLE 3                                                          */
/* SETTINGS FOR FILL-IN fi-desc-ramo IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-atacado IN FRAME f-main
   NO-ENABLE 3                                                          */
/* SETTINGS FOR TOGGLE-BOX tg-industria IN FRAME f-main
   NO-ENABLE 3                                                          */
/* SETTINGS FOR TOGGLE-BOX tg-servico IN FRAME f-main
   NO-ENABLE 3                                                          */
/* SETTINGS FOR TOGGLE-BOX tg-varejo IN FRAME f-main
   NO-ENABLE 3                                                          */
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

&Scoped-define SELF-NAME fi-cod-ramo-ativ
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-ramo-ativ V-table-Win
ON LEAVE OF fi-cod-ramo-ativ IN FRAME f-main /* C¢digo Ramo */
DO:
   FIND FIRST ramo-ativ WHERE 
              ramo-ativ.cod-ramo-ativ = INPUT FRAME {&FRAME-NAME} fi-cod-ramo-ativ NO-LOCK NO-ERROR.
   
   ASSIGN fi-desc-ramo:SCREEN-VALUE = "".
   IF AVAIL ramo-ativ THEN
      ASSIGN fi-desc-ramo:SCREEN-VALUE = ramo-ativ.descricao.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-ramo-ativ V-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-cod-ramo-ativ IN FRAME f-main /* C¢digo Ramo */
DO:
  {include/zoomvar.i &prog-zoom=eszoom/z01ra001.r
                     &campo     = fi-cod-ramo-ativ
                     &campozoom = cod-ramo-ativ}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-atacado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-atacado V-table-Win
ON VALUE-CHANGED OF tg-atacado IN FRAME f-main /* Atacado */
DO:
  RUN pi-trata-atividade.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-industria
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-industria V-table-Win
ON VALUE-CHANGED OF tg-industria IN FRAME f-main /* Industria */
DO:
  RUN pi-trata-atividade.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-servico
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-servico V-table-Win
ON VALUE-CHANGED OF tg-servico IN FRAME f-main /* Servi‡o */
DO:
  RUN pi-trata-atividade.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-varejo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-varejo V-table-Win
ON VALUE-CHANGED OF tg-varejo IN FRAME f-main /* Varejo */
DO:
   RUN pi-trata-atividade.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
  FOR EACH finalidades_venda NO-LOCK.
      ASSIGN c-lst-finalidade = IF c-lst-finalidade = ''
                                THEN finalidades.desc_finalidade_venda + ',' + STRING(finalidades_venda.cod_finalidade)
                                ELSE c-lst-finalidade + "," +  finalidades.desc_finalidade_venda + ',' + STRING(finalidades_venda.cod_finalidade).
  END.
  ASSIGN c-lst-finalidade = c-lst-finalidade + ",,0".
  ASSIGN cb-finalidade:LIST-ITEM-PAIRS = c-lst-finalidade.

  fi-cod-ramo-ativ:LOAD-MOUSE-POINTER("image/lupa.cur").

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
  {src/adm/template/row-list.i "emitente"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "emitente"}

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
    
    /*:T Ponha na pi-validate todas as valida‡äes */
    /*:T NÆo gravar nada no registro antes do dispatch do assign-record e 
       nem na PI-validate. */
    
    ASSIGN INPUT FRAME {&FRAME-NAME} fi-cod-ramo-ativ
           INPUT FRAME {&FRAME-NAME} tg-varejo
           INPUT FRAME {&FRAME-NAME} tg-atacado
           INPUT FRAME {&FRAME-NAME} tg-industria 
           INPUT FRAME {&FRAME-NAME} tg-servico
           INPUT FRAME {&FRAME-NAME} cb-finalidade.

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.
    
    /*:T Todos os assignïs nÆo feitos pelo assign-record devem ser feitos aqui */  
    /* Code placed here will execute AFTER standard behavior.    */
    FIND ext-emitente WHERE 
         ext-emitente.cod-emitente = emitente.cod-emitente SHARE-LOCK NO-ERROR.
       
    IF NOT AVAIL ext-emitente THEN DO:
       CREATE ext-emitente.
       ASSIGN ext-emitente.cod-emitente = emitente.cod-emitente.
    END.
    ASSIGN ext-emitente.cod-ramo-ativ = fi-cod-ramo-ativ
           ext-emitente.log_varejo = tg-varejo              
           ext-emitente.log_atacado = tg-atacado             
           ext-emitente.log_industria = tg-industria           
           ext-emitente.log_servico = tg-servico             
           ext-emitente.cod_finalidade_venda = INT(cb-finalidade).
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
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /* Code placed here will execute PRIOR to standard behavior. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .
    
    /* Code placed here will execute AFTER standard behavior.    */
    IF AVAIL emitente THEN DO.
       ASSIGN fi-cod-ramo-ativ:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ''
              fi-desc-ramo:SCREEN-VALUE = ""
              tg-varejo:SCREEN-VALUE     = 'NO'
              tg-atacado:SCREEN-VALUE    = 'NO'
              tg-industria:SCREEN-VALUE  = 'NO'
              tg-servico:SCREEN-VALUE    = 'NO'
              cb-finalidade:SCREEN-VALUE = ''.

       FIND ext-emitente WHERE
            ext-emitente.cod-emitente = emitente.cod-emitente NO-LOCK NO-ERROR.
       IF AVAIL ext-emitente THEN DO.
          ASSIGN fi-cod-ramo-ativ:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ext-emitente.cod-ramo-ativ)
                 tg-varejo:SCREEN-VALUE     = STRING(ext-emitente.log_varejo)           
                 tg-atacado:SCREEN-VALUE    = STRING(ext-emitente.log_atacado)          
                 tg-industria:SCREEN-VALUE  = STRING(ext-emitente.log_industria)        
                 tg-servico:SCREEN-VALUE    = STRING(ext-emitente.log_servico)          
                 cb-finalidade:SCREEN-VALUE = STRING(ext-emitente.cod_finalidade_venda).

          FIND FIRST ramo-ativ WHERE 
                     ramo-ativ.cod-ramo-ativ = ext-emitente.cod-ramo-ativ NO-LOCK NO-ERROR.

          IF AVAIL ramo-ativ THEN
             ASSIGN fi-desc-ramo:SCREEN-VALUE = ramo-ativ.descricao.  
       END.
    END.
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
    if adm-new-record = NO then
        enable {&ADM-MODIFY-FIELDS} with frame {&frame-name}.
    &endif
    ASSIGN emitente.nome-emit:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-trata-atividade V-table-Win 
PROCEDURE pi-trata-atividade :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN tg-atacado:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'
           tg-varejo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'
           tg-industria:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'
           tg-servico:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'.

    ASSIGN SELF:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'YES'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pi-validate V-table-Win 
PROCEDURE Pi-validate :
/*:T------------------------------------------------------------------------------
  Purpose:Validar a viewer     
  Parameters:  <none>
  Notes: NÆo fazer assign aqui. Nesta procedure
  devem ser colocadas apenas valida‡äes, pois neste ponto do programa o registro 
  ainda nÆo foi criado.       
------------------------------------------------------------------------------*/
    {include/i-vldfrm.i} /*:T Valida‡Æo de dicion rio */
    
/*:T    Segue um exemplo de valida‡Æo de programa */
/*       find tabela where tabela.campo1 = c-variavel and               */
/*                         tabela.campo2 > i-variavel no-lock no-error. */
      
      /*:T Este include deve ser colocado sempre antes do ut-msgs.p */
/*       {include/i-vldprg.i}                                             */
/*       run utp/ut-msgs.p (input "show":U, input 7, input return-value). */
/*       return 'ADM-ERROR':U.                                            */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-verifica V-table-Win 
PROCEDURE pi-verifica :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
    IF INPUT FRAME {&FRAME-NAME} fi-cod-ramo-ativ = 0 THEN DO.
       MESSAGE 'Favor Informar o Ramo de Atividade'
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
       RETURN 'ADM-ERROR'.
    END.

    IF INPUT FRAME {&FRAME-NAME} cb-finalidade = 0 THEN DO.
       MESSAGE 'Favor Informar a Finalidade de Venda...'
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
       RETURN 'ADM-ERROR'.
    END.

    IF (tg-varejo:SCREEN-VALUE  IN FRAME {&FRAME-NAME} = 'NO' AND
        tg-atacado:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO' AND 
        tg-servico:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO' AND
        tg-industria:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO') THEN DO.
       MESSAGE 'Favor Informar a Atividade...'
             VIEW-AS ALERT-BOX ERROR BUTTONS OK.
       RETURN 'ADM-ERROR'.
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
  {src/adm/template/snd-list.i "emitente"}

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

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/vstates.i}
  END CASE.
  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

