&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          espec            PROGRESS
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

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME f-main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES teste-nip
&Scoped-define FIRST-EXTERNAL-TABLE teste-nip


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR teste-nip.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS teste-nip.pre-esqrd teste-nip.pre-centro ~
teste-nip.pre-direita teste-nip.res-esqrd teste-nip.res-centro ~
teste-nip.res-direita teste-nip.observ 
&Scoped-define ENABLED-TABLES teste-nip
&Scoped-define FIRST-ENABLED-TABLE teste-nip
&Scoped-Define ENABLED-OBJECTS rt-key rt-mold 
&Scoped-Define DISPLAYED-FIELDS teste-nip.cod-maq teste-nip.num-foulard ~
teste-nip.data-teste teste-nip.num-seq teste-nip.pre-esqrd ~
teste-nip.pre-centro teste-nip.pre-direita teste-nip.res-esqrd ~
teste-nip.res-centro teste-nip.res-direita teste-nip.observ 
&Scoped-define DISPLAYED-TABLES teste-nip
&Scoped-define FIRST-DISPLAYED-TABLE teste-nip
&Scoped-Define DISPLAYED-OBJECTS fi-desc-maq 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-CREATE-FIELDS teste-nip.cod-maq teste-nip.num-foulard ~
teste-nip.data-teste 
&Scoped-define ADM-ASSIGN-FIELDS teste-nip.num-seq 

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
DEFINE VARIABLE fi-desc-maq AS CHARACTER FORMAT "x(20)" 
     VIEW-AS FILL-IN 
     SIZE 34.43 BY .88 NO-UNDO.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 88 BY 4.25.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 88 BY 7.42.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     teste-nip.cod-maq AT ROW 1.17 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.14 BY .88
     fi-desc-maq AT ROW 1.17 COL 27.43 COLON-ALIGNED NO-LABEL
     teste-nip.num-foulard AT ROW 2.17 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7.14 BY .88
     teste-nip.data-teste AT ROW 3.17 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.57 BY .88
     teste-nip.num-seq AT ROW 4.17 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 2.29 BY .88
     teste-nip.pre-esqrd AT ROW 5.71 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5.72 BY .88
     teste-nip.pre-centro AT ROW 6.71 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5.72 BY .88
     teste-nip.pre-direita AT ROW 7.71 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5.72 BY .88
     teste-nip.res-esqrd AT ROW 8.71 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.86 BY .88
     teste-nip.res-centro AT ROW 9.71 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.86 BY .88
     teste-nip.res-direita AT ROW 10.71 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY .88
     teste-nip.observ AT ROW 11.71 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 31.14 BY .88
     rt-key AT ROW 1 COL 1
     rt-mold AT ROW 5.42 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: espec.teste-nip
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
         HEIGHT             = 11.83
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
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME f-main:SCROLLABLE       = FALSE
       FRAME f-main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN teste-nip.cod-maq IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN teste-nip.data-teste IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN fi-desc-maq IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN teste-nip.num-foulard IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN teste-nip.num-seq IN FRAME f-main
   NO-ENABLE 2                                                          */
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

&Scoped-define SELF-NAME teste-nip.cod-maq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL teste-nip.cod-maq V-table-Win
ON ENTRY OF teste-nip.cod-maq IN FRAME f-main /* Maquina */
DO:
   FIND maq-benef WHERE maq-benef.codigo = INPUT FRAME {&FRAME-NAME} teste-nip.cod-maq NO-LOCK NO-ERROR.
   IF AVAIL maq-benef THEN
      ASSIGN fi-desc-maq:SCREEN-VALUE IN FRAME {&FRAME-NAME} = maq-benef.descricao.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL teste-nip.cod-maq V-table-Win
ON LEAVE OF teste-nip.cod-maq IN FRAME f-main /* Maquina */
DO:
  FIND maq-benef WHERE maq-benef.codigo = INPUT FRAME {&FRAME-NAME} teste-nip.cod-maq NO-LOCK NO-ERROR.
  IF AVAIL maq-benef THEN
     ASSIGN fi-desc-maq:SCREEN-VALUE IN FRAME {&FRAME-NAME} = maq-benef.descricao.
  ELSE DO:
     MESSAGE "M quina inv lida." VIEW-AS ALERT-BOX.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL teste-nip.cod-maq V-table-Win
ON MOUSE-SELECT-DBLCLICK OF teste-nip.cod-maq IN FRAME f-main /* Maquina */
DO:
  {include/zoomvar.i &prog-zoom = eszoom/z01es018.w
                     &campo     = teste-nip.cod-maq
                     &campozoom = codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME teste-nip.data-teste
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL teste-nip.data-teste V-table-Win
ON LEAVE OF teste-nip.data-teste IN FRAME f-main /* Data Teste */
DO:
  FIND LAST teste-nip 
       WHERE teste-nip.cod-maq     = INPUT FRAME {&FRAME-NAME} teste-nip.cod-maq
         AND teste-nip.num-foulard = INPUT FRAME {&FRAME-NAME} teste-nip.num-foulard
         AND teste-nip.data-teste  = INPUT FRAME {&FRAME-NAME} teste-nip.data-teste
       NO-LOCK NO-ERROR.
  IF AVAIL teste-nip THEN DO:
     IF teste-nip.num-seq = 9 THEN DO:
        MESSAGE "Alcan‡ado o limite de Teste para M quina/Foulard/Data" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
     END.
     ASSIGN teste-nip.num-seq:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(teste-nip.num-seq + 1).
  END.
  ELSE
     ASSIGN teste-nip.num-seq:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "1".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF
           
  teste-nip.cod-maq:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.
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
  {src/adm/template/row-list.i "teste-nip"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "teste-nip"}

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
    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.
    
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
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ).
    
    /* Code placed here will execute AFTER standard behavior.    */
    FIND maq-benef WHERE maq-benef.codigo = teste-nip.cod-maq NO-LOCK NO-ERROR.
    IF AVAIL maq-benef THEN
       ASSIGN fi-desc-maq:SCREEN-VALUE IN FRAME {&FRAME-NAME} = maq-benef.descricao.

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
  {src/adm/template/snd-list.i "teste-nip"}

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

