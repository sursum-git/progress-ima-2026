&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW


/* Temp-Table and Buffer definitions                                    */
DEFINE NEW GLOBAL SHARED TEMP-TABLE tt-preco-item NO-UNDO LIKE preco-item.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i V04im026 9.99.99.999}

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
DEF BUFFER b-tt-preco-item FOR tt-preco-item.

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
&Scoped-define EXTERNAL-TABLES tt-preco-item
&Scoped-define FIRST-EXTERNAL-TABLE tt-preco-item


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR tt-preco-item.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS tt-preco-item.dec-1 tt-preco-item.preco-venda ~
tt-preco-item.preco-fob 
&Scoped-define ENABLED-TABLES tt-preco-item
&Scoped-define FIRST-ENABLED-TABLE tt-preco-item
&Scoped-Define ENABLED-OBJECTS rt-key rt-mold 
&Scoped-Define DISPLAYED-FIELDS tt-preco-item.nr-tabpre ~
tt-preco-item.it-codigo tt-preco-item.char-1 tt-preco-item.dec-1 ~
tt-preco-item.preco-venda tt-preco-item.preco-fob 
&Scoped-define DISPLAYED-TABLES tt-preco-item
&Scoped-define FIRST-DISPLAYED-TABLE tt-preco-item
&Scoped-Define DISPLAYED-OBJECTS c-tab 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-CREATE-FIELDS tt-preco-item.it-codigo ~
tt-preco-item.preco-fob 
&Scoped-define ADM-ASSIGN-FIELDS tt-preco-item.nr-tabpre ~
tt-preco-item.it-codigo 

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
DEFINE VARIABLE c-tab AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 43 BY .88 NO-UNDO.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 83 BY 2.75.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 83 BY 3.5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     tt-preco-item.nr-tabpre AT ROW 1.25 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.14 BY .88
     c-tab AT ROW 1.25 COL 26 COLON-ALIGNED NO-LABEL
     tt-preco-item.it-codigo AT ROW 2.5 COL 13 COLON-ALIGNED
          LABEL "Item":R5
          VIEW-AS FILL-IN 
          SIZE 17.14 BY .88
     tt-preco-item.char-1 AT ROW 2.5 COL 33 NO-LABEL FORMAT "x(45)"
          VIEW-AS FILL-IN 
          SIZE 45 BY .88
     tt-preco-item.dec-1 AT ROW 4.25 COL 13 COLON-ALIGNED
          LABEL "Qt M¡nima"
          VIEW-AS FILL-IN 
          SIZE 26.29 BY .88
     tt-preco-item.preco-venda AT ROW 5.25 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20.57 BY .88
     tt-preco-item.preco-fob AT ROW 6.25 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20.57 BY .88
     rt-key AT ROW 1 COL 1
     rt-mold AT ROW 4 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: Temp-Tables.tt-preco-item
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: tt-preco-item T "NEW GLOBAL SHARED" NO-UNDO mgdis preco-item
   END-TABLES.
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
         HEIGHT             = 6.58
         WIDTH              = 83.14.
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

/* SETTINGS FOR FILL-IN c-tab IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tt-preco-item.char-1 IN FRAME f-main
   NO-ENABLE ALIGN-L EXP-LABEL EXP-FORMAT                               */
/* SETTINGS FOR FILL-IN tt-preco-item.dec-1 IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN tt-preco-item.it-codigo IN FRAME f-main
   NO-ENABLE 1 2 EXP-LABEL                                              */
/* SETTINGS FOR FILL-IN tt-preco-item.nr-tabpre IN FRAME f-main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN tt-preco-item.preco-fob IN FRAME f-main
   1                                                                    */
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

&Scoped-define SELF-NAME tt-preco-item.it-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-preco-item.it-codigo V-table-Win
ON ENTRY OF tt-preco-item.it-codigo IN FRAME f-main /* Item */
DO:
  FIND tb-preco WHERE ROWID(tb-preco) = v-row-parent NO-LOCK NO-ERROR.  
    IF AVAIL tb-preco THEN DO:
       ASSIGN tt-preco-item.nr-tabpre:SCREEN-VALUE IN FRAME {&frame-name}  = tb-preco.nr-tabpre
              c-tab:SCREEN-VALUE IN FRAME {&frame-name}                    = tb-preco.descricao.
    END.
    DISABLE tt-preco-item.nr-tabpre WITH  FRAME {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-preco-item.it-codigo V-table-Win
ON f5 OF tt-preco-item.it-codigo IN FRAME f-main /* Item */
DO:
    {include/zoomvar.i &prog-zoom=inzoom/z01in172.w
                         &campo=tt-preco-item.it-codigo
                         &campozoom=it-codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-preco-item.it-codigo V-table-Win
ON LEAVE OF tt-preco-item.it-codigo IN FRAME f-main /* Item */
DO:
  FIND FIRST ITEM WHERE ITEM.it-codigo = INPUT tt-preco-item.it-codigo NO-LOCK no-error.
  IF AVAIL ITEM THEN
     ASSIGN tt-preco-item.char-1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-preco-item.it-codigo V-table-Win
ON MOUSE-SELECT-DBLCLICK OF tt-preco-item.it-codigo IN FRAME f-main /* Item */
DO:
    APPLY "f5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
if tt-preco-item.it-codigo:load-mouse-pointer("image\lupa.cur") then.

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
  {src/adm/template/row-list.i "tt-preco-item"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "tt-preco-item"}

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
     IF AVAIL tt-preco-item THEN DO:
        ASSIGN tt-preco-item.log-2 = YES.
      
        FIND FIRST ITEM WHERE ITEM.it-codigo =  tt-preco-item.it-codigo NO-LOCK no-error.
        IF AVAIL ITEM THEN
           ASSIGN tt-preco-item.char-1 = item.desc-item.

        FIND FIRST tb-preco WHERE tb-preco.nr-tabpre =  tt-preco-item.nr-tabpre NO-LOCK no-error.
        IF AVAIL tb-preco THEN
           ASSIGN tt-preco-item.situacao = tb-preco.situacao
                  tt-preco-item.dt-inival = tb-preco.dt-inival.

        FOR EACH ref-item WHERE 
                 ref-item.it-codigo = tt-preco-item.it-codigo NO-LOCK:
            FIND FIRST preco-item WHERE preco-item.nr-tabpre = tt-preco-item.nr-tabpre AND
                                        preco-item.it-codigo = tt-preco-item.it-codigo and
                                        preco-item.cod-refer = ref-item.cod-refer SHARE-LOCK NO-ERROR.
            IF AVAIL preco-item THEN
               ASSIGN preco-item.preco-venda = tt-preco-item.preco-venda
                      preco-item.preco-fob   = tt-preco-item.preco-fob
                      preco-item.dec-1       = tt-preco-item.dec-1
                      preco-item.preco-min-cif = tt-preco-item.preco-venda
                      preco-item.preco-min-fob = tt-preco-item.preco-fob.
            ELSE DO:
                CREATE preco-item.
                ASSIGN preco-item.nr-tabpre  = tt-preco-item.nr-tabpre 
                       preco-item.it-codigo  = tt-preco-item.it-codigo 
                       preco-item.situacao   = tt-preco-item.situacao
                       preco-item.dt-inival  = tt-preco-item.dt-inival 
                       preco-item.quant-min  = tt-preco-item.dec-1
                       preco-item.cod-refer  = ref-item.cod-refer
                       preco-item.preco-venda = tt-preco-item.preco-venda
                       preco-item.preco-fob   = tt-preco-item.preco-fob
                       preco-item.dec-1       = tt-preco-item.dec-1
                       preco-item.preco-min-cif = tt-preco-item.preco-venda
                       preco-item.preco-min-fob = tt-preco-item.preco-fob.
           
            END.
        END.
     END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
 
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  FIND tb-preco WHERE ROWID(tb-preco) = v-row-parent NO-LOCK NO-ERROR.  
  IF AVAIL tb-preco THEN DO:
      ASSIGN tt-preco-item.nr-tabpre = tb-preco.nr-tabpre. 
  END.

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

    IF INPUT tt-preco-item.it-codigo = " "  THEN DO:
       MESSAGE "Informe o C¢digo do then item" VIEW-AS ALERT-BOX.
       return 'ADM-ERROR':U.
    END.

    FIND FIRST ITEM WHERE ITEM.it-codigo = INPUT tt-preco-item.it-codigo NO-LOCK no-error.
    IF NOT AVAIL ITEM THEN DO:
       MESSAGE "Item nÆo cadastrado" VIEW-AS ALERT-BOX.
       return 'ADM-ERROR':U.
    END.

    FIND FIRST b-tt-preco-ITEM WHERE b-tt-preco-item.nr-tabpre = INPUT tt-preco-item.nr-tabpre AND 
                                     b-tt-preco-item.it-codigo = INPUT tt-preco-item.it-codigo NO-LOCK no-error.
    IF AVAIL b-tt-preco-item THEN DO:
       MESSAGE "Item j  cadastrado nesta tabela de pre‡o" VIEW-AS ALERT-BOX.
       return 'ADM-ERROR':U.
    END.
     
   

    
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
  {src/adm/template/snd-list.i "tt-preco-item"}

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

