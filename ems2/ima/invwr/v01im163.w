&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          ems2med          PROGRESS
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
def var v-row-parent    as rowid    no-undo.

DEF NEW GLOBAL SHARED VAR gr-it-doc-fisico AS ROWID     NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-container      AS HANDLE    NO-UNDO.

DEF NEW GLOBAL SHARED TEMP-TABLE tt-etiqueta LIKE mgcld.bc-etiqueta 
    FIELD cod-emitente LIKE emitente.cod-emitente
    FIELD desc-item    LIKE item.desc-item
    field log-registro as logical
    field r-rowid      as rowid
    field qt-etiqueta  as integer format ">>>9" label "Qt Etiq"
    index codigo is primary nr-etiq.

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
&Scoped-define EXTERNAL-TABLES it-doc-fisico
&Scoped-define FIRST-EXTERNAL-TABLE it-doc-fisico


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR it-doc-fisico.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS it-doc-fisico.cod-refer ~
it-doc-fisico.quantidade 
&Scoped-define ENABLED-TABLES it-doc-fisico
&Scoped-define FIRST-ENABLED-TABLE it-doc-fisico
&Scoped-Define ENABLED-OBJECTS rt-key 
&Scoped-Define DISPLAYED-FIELDS it-doc-fisico.nro-docto ~
it-doc-fisico.cod-emitente it-doc-fisico.it-codigo it-doc-fisico.cod-refer ~
it-doc-fisico.sequencia it-doc-fisico.serie-docto it-doc-fisico.quantidade 
&Scoped-define DISPLAYED-TABLES it-doc-fisico
&Scoped-define FIRST-DISPLAYED-TABLE it-doc-fisico
&Scoped-Define DISPLAYED-OBJECTS c-nome-emit c-desc-item 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */

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
DEFINE VARIABLE c-desc-item AS CHARACTER FORMAT "X(36)":U 
     VIEW-AS FILL-IN 
     SIZE 37 BY .88 NO-UNDO.

DEFINE VARIABLE c-nome-emit AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 43 BY .88 NO-UNDO.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 90 BY 5.25.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     it-doc-fisico.nro-docto AT ROW 1.17 COL 18.43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.14 BY .88
     it-doc-fisico.cod-emitente AT ROW 2.17 COL 18.43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.43 BY .88
     c-nome-emit AT ROW 2.17 COL 30.43 COLON-ALIGNED NO-LABEL
     it-doc-fisico.it-codigo AT ROW 3.17 COL 18.43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.14 BY .88
     c-desc-item AT ROW 3.17 COL 36.43 COLON-ALIGNED NO-LABEL
     it-doc-fisico.cod-refer AT ROW 4.17 COL 18.43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.14 BY .88
     it-doc-fisico.sequencia AT ROW 4.17 COL 56.29 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.86 BY .88
     it-doc-fisico.serie-docto AT ROW 5.17 COL 18.43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.14 BY .88
     it-doc-fisico.quantidade AT ROW 5.17 COL 56.29 COLON-ALIGNED
          LABEL "Quantidade":R12
          VIEW-AS FILL-IN 
          SIZE 17.14 BY .88
     rt-key AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: movind.it-doc-fisico
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
         HEIGHT             = 5.33
         WIDTH              = 90.
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
       FRAME f-main:SCROLLABLE       = FALSE.

/* SETTINGS FOR FILL-IN c-desc-item IN FRAME f-main
   NO-ENABLE                                                            */
ASSIGN 
       c-desc-item:READ-ONLY IN FRAME f-main        = TRUE.

/* SETTINGS FOR FILL-IN c-nome-emit IN FRAME f-main
   NO-ENABLE                                                            */
ASSIGN 
       c-nome-emit:READ-ONLY IN FRAME f-main        = TRUE.

/* SETTINGS FOR FILL-IN it-doc-fisico.cod-emitente IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN it-doc-fisico.it-codigo IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN it-doc-fisico.nro-docto IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN it-doc-fisico.quantidade IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN it-doc-fisico.sequencia IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN it-doc-fisico.serie-docto IN FRAME f-main
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
  {src/adm/template/row-list.i "it-doc-fisico"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "it-doc-fisico"}

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
    
    /* Ponha na pi-validate todas as valida‡äes */
    /* NÆo gravar nada no registro antes do dispatch do assign-record e 
       nem na PI-validate. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    IF  RETURN-VALUE = 'ADM-ERROR':U THEN
        RETURN 'ADM-ERROR':U.
    
    /* Todos os assignïs nÆo feitos pelo assign-record devem ser feitos aqui */  
    /* Code placed here will execute AFTER standard behavior.    */

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
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
    
  ASSIGN gr-it-doc-fisico = ROWID(it-doc-fisico).
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  FOR EACH tt-etiqueta:
      DELETE tt-etiqueta.
  END.

  FOR EACH bc-etiqueta NO-LOCK WHERE bc-etiqueta.nr-nota-fis   = it-doc-fisico.nro-docto
                                 AND bc-etiqueta.serie         = it-doc-fisico.serie-docto
                                 AND bc-etiqueta.it-codigo     = it-doc-fisico.it-codigo
                                 AND bc-etiqueta.nr-romaneio   = it-doc-fisico.sequencia.
      CREATE tt-etiqueta.
      BUFFER-COPY bc-etiqueta TO tt-etiqueta.
      ASSIGN tt-etiqueta.log-registro     = NO
             tt-etiqueta.r-rowid          = ROWID(bc-etiqueta)
             tt-etiqueta.logico-livre-5   = NO
             tt-etiqueta.inteiro-livre-1  = 1.
  END.

  FIND emitente
      WHERE emitente.cod-emitente = it-doc-fisico.cod-emitente
      NO-LOCK NO-ERROR.
  ASSIGN c-nome-emit:SCREEN-VALUE IN FRAME {&FRAME-NAME} = IF AVAIL emitente THEN emitente.nome-abrev ELSE "":U.

  FIND ITEM
      WHERE ITEM.it-codigo = it-doc-fisico.it-codigo                                          
      NO-LOCK NO-ERROR.
  ASSIGN c-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = IF AVAIL ITEM THEN item.desc-item ELSE "":U.

  RUN pi-mostra-filhos IN h-container.

  /* Code placed here will execute AFTER standard behavior.    */

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
  Notes: NÆo fazer assign aqui. Nesta procedure
  devem ser colocadas apenas valida‡äes, pois neste ponto do programa o registro 
  ainda nÆo foi criado.       
------------------------------------------------------------------------------*/
    {include/i-vldfrm.i} /* Valida‡Æo de dicion rio */
    
/*/*    Segue um exemplo de valida‡Æo de programa */
 *     find tabela where tabela.campo1 = c-variavel and
 *                       tabela.campo2 > i-variavel no-lock no-error.
 *     
 *     /* Este include deve ser colocado sempre antes do ut-msgs.p */
 *     {include/i-vldprg.i}
 *     run utp/ut-msgs.p (input "show":U, input 7, input return-value).
 *     return 'ADM-ERROR':U.*/

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
  {src/adm/template/snd-list.i "it-doc-fisico"}

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

