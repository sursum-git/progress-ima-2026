&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          espec            PROGRESS
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
{include/i-prgvrs.i V01ES067 2.04.00.000}

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
&Scoped-define EXTERNAL-TABLES mp-comp-mistura
&Scoped-define FIRST-EXTERNAL-TABLE mp-comp-mistura


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR mp-comp-mistura.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS mp-comp-mistura.padrao ~
mp-comp-mistura.cd-tipo mp-comp-mistura.cd-coloracao ~
mp-comp-mistura.cd-compr mp-comp-mistura.fator mp-comp-mistura.nr-lote 
&Scoped-define ENABLED-TABLES mp-comp-mistura
&Scoped-define FIRST-ENABLED-TABLE mp-comp-mistura
&Scoped-Define ENABLED-OBJECTS rt-key rt-mold 
&Scoped-Define DISPLAYED-FIELDS mp-comp-mistura.nr-mistura ~
mp-comp-mistura.padrao mp-comp-mistura.cd-tipo mp-comp-mistura.cd-coloracao ~
mp-comp-mistura.cd-compr mp-comp-mistura.fator mp-comp-mistura.nr-lote 
&Scoped-define DISPLAYED-TABLES mp-comp-mistura
&Scoped-define FIRST-DISPLAYED-TABLE mp-comp-mistura
&Scoped-Define DISPLAYED-OBJECTS fi-dt-mistura fi-tp-mistura fi-tipo ~
fi-coloracao fi-comprimento 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-ASSIGN-FIELDS mp-comp-mistura.nr-mistura 

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
DEFINE VARIABLE fi-coloracao AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20.14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-comprimento AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20.14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-mistura AS DATE FORMAT "99/99/9999" 
     LABEL "Data Mistura" 
     VIEW-AS FILL-IN 
     SIZE 10.86 BY .88.

DEFINE VARIABLE fi-tipo AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20.14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-tp-mistura AS CHARACTER FORMAT "X(256)":U 
     LABEL "Tipos Mistura" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 80 BY 1.25.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 80 BY 7.75.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     mp-comp-mistura.nr-mistura AT ROW 1.17 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY .88
     fi-dt-mistura AT ROW 1.17 COL 36.14 COLON-ALIGNED
     fi-tp-mistura AT ROW 1.17 COL 62 COLON-ALIGNED
     mp-comp-mistura.padrao AT ROW 2.42 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 21.14 BY .88
     mp-comp-mistura.cd-tipo AT ROW 3.42 COL 13 COLON-ALIGNED
          LABEL "Tipo"
          VIEW-AS FILL-IN 
          SIZE 3.43 BY .88
     fi-tipo AT ROW 3.42 COL 16.43 COLON-ALIGNED NO-LABEL
     mp-comp-mistura.cd-coloracao AT ROW 4.42 COL 13 COLON-ALIGNED
          LABEL "Tonalidade"
          VIEW-AS FILL-IN 
          SIZE 3.43 BY .88
     fi-coloracao AT ROW 4.42 COL 16.43 COLON-ALIGNED NO-LABEL
     mp-comp-mistura.cd-compr AT ROW 5.42 COL 13 COLON-ALIGNED
          LABEL "Comprimento (mm)"
          VIEW-AS FILL-IN 
          SIZE 3.43 BY .88
     fi-comprimento AT ROW 5.42 COL 16.43 COLON-ALIGNED NO-LABEL
     mp-comp-mistura.fator AT ROW 6.42 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.57 BY .88
     mp-comp-mistura.nr-lote AT ROW 7.42 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16.14 BY .88
          FONT 1
     rt-key AT ROW 1 COL 1
     rt-mold AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: espec.mp-comp-mistura
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
         HEIGHT             = 7.79
         WIDTH              = 80.
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

/* SETTINGS FOR FILL-IN mp-comp-mistura.cd-coloracao IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN mp-comp-mistura.cd-compr IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN mp-comp-mistura.cd-tipo IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN fi-coloracao IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-comprimento IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-dt-mistura IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tipo IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tp-mistura IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN mp-comp-mistura.nr-mistura IN FRAME f-main
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

&Scoped-define SELF-NAME mp-comp-mistura.cd-coloracao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mp-comp-mistura.cd-coloracao V-table-Win
ON ENTRY OF mp-comp-mistura.cd-coloracao IN FRAME f-main /* Tonalidade */
DO:
    FIND mp-coloracao WHERE mp-coloracao.codigo = INPUT FRAME {&FRAME-NAME} mp-comp-mistura.cd-coloracao NO-LOCK NO-ERROR.
    IF AVAIL mp-coloracao THEN
       ASSIGN fi-coloracao:SCREEN-VALUE IN FRAME {&FRAME-NAME} = mp-coloracao.tonalidade.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mp-comp-mistura.cd-coloracao V-table-Win
ON LEAVE OF mp-comp-mistura.cd-coloracao IN FRAME f-main /* Tonalidade */
DO:
    FIND mp-coloracao WHERE mp-coloracao.codigo = INPUT FRAME {&FRAME-NAME} mp-comp-mistura.cd-coloracao NO-LOCK NO-ERROR.
    IF AVAIL mp-coloracao THEN
       ASSIGN fi-coloracao:SCREEN-VALUE IN FRAME {&FRAME-NAME} = mp-coloracao.tonalidade.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mp-comp-mistura.cd-coloracao V-table-Win
ON MOUSE-SELECT-DBLCLICK OF mp-comp-mistura.cd-coloracao IN FRAME f-main /* Tonalidade */
DO:
    {include/zoomvar.i &prog-zoom=especificos\eszoom\Z01ES054.w
                       &campo=mp-comp-mistura.cd-coloracao
                       &campozoom=codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mp-comp-mistura.cd-compr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mp-comp-mistura.cd-compr V-table-Win
ON ENTRY OF mp-comp-mistura.cd-compr IN FRAME f-main /* Comprimento (mm) */
DO:
    FIND mp-classificacao WHERE mp-classificacao.codigo = INPUT FRAME {&FRAME-NAME} mp-comp-mistura.cd-compr NO-LOCK NO-ERROR.
    IF AVAIL mp-classificacao THEN
       ASSIGN fi-comprimento:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(mp-classificacao.compr-min) + "  A  " + STRING(mp-classificacao.compr-max).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mp-comp-mistura.cd-compr V-table-Win
ON LEAVE OF mp-comp-mistura.cd-compr IN FRAME f-main /* Comprimento (mm) */
DO:
    FIND mp-classificacao WHERE mp-classificacao.codigo = INPUT FRAME {&FRAME-NAME} mp-comp-mistura.cd-compr NO-LOCK NO-ERROR.
    IF AVAIL mp-classificacao THEN
       ASSIGN fi-comprimento:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(mp-classificacao.compr-min) + "  A  " + STRING(mp-classificacao.compr-max).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mp-comp-mistura.cd-compr V-table-Win
ON MOUSE-SELECT-DBLCLICK OF mp-comp-mistura.cd-compr IN FRAME f-main /* Comprimento (mm) */
DO:
    {include/zoomvar.i &prog-zoom=especificos\eszoom\Z01ES056.w
                       &campo=mp-comp-mistura.cd-compr
                       &campozoom=codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mp-comp-mistura.cd-tipo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mp-comp-mistura.cd-tipo V-table-Win
ON ENTRY OF mp-comp-mistura.cd-tipo IN FRAME f-main /* Tipo */
DO:
   FIND mp-tipo WHERE mp-tipo.codigo = INPUT FRAME {&FRAME-NAME} mp-comp-mistura.cd-tipo NO-LOCK NO-ERROR.
   IF AVAIL mp-tipo THEN
      ASSIGN fi-tipo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = mp-tipo.tipo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mp-comp-mistura.cd-tipo V-table-Win
ON LEAVE OF mp-comp-mistura.cd-tipo IN FRAME f-main /* Tipo */
DO:
   FIND mp-tipo WHERE mp-tipo.codigo = INPUT FRAME {&FRAME-NAME} mp-comp-mistura.cd-tipo NO-LOCK NO-ERROR.
   IF AVAIL mp-tipo THEN
      ASSIGN fi-tipo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = mp-tipo.tipo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mp-comp-mistura.cd-tipo V-table-Win
ON MOUSE-SELECT-DBLCLICK OF mp-comp-mistura.cd-tipo IN FRAME f-main /* Tipo */
DO:
    {include/zoomvar.i &prog-zoom=especificos\eszoom\Z01ES055.w
                       &campo=mp-comp-mistura.cd-tipo
                       &campozoom=codigo}
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

  mp-comp-mistura.cd-tipo:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.
  mp-comp-mistura.cd-coloracao:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.
  mp-comp-mistura.cd-compr:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.

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
  {src/adm/template/row-list.i "mp-comp-mistura"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "mp-comp-mistura"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-apply-entry V-table-Win 
PROCEDURE local-apply-entry :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'apply-entry':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  FIND mp-mistura WHERE rowid(mp-mistura) = v-row-parent NO-LOCK NO-ERROR.
  IF AVAIL mp-mistura THEN
     ASSIGN mp-comp-mistura.nr-mistura:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(mp-mistura.nr-mistura)
            fi-dt-mistura:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(mp-mistura.dt-mistura)
            fi-tp-mistura:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(mp-mistura.tp-mistura).
            
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
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.
    
    /*:T Todos os assignïs nÆo feitos pelo assign-record devem ser feitos aqui */  
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

    FIND mp-coloracao WHERE mp-coloracao.codigo = INPUT FRAME {&FRAME-NAME} mp-comp-mistura.cd-coloracao NO-LOCK NO-ERROR.
    IF AVAIL mp-coloracao THEN
       ASSIGN fi-coloracao:SCREEN-VALUE IN FRAME {&FRAME-NAME} = mp-coloracao.tonalidade.

    FIND mp-classificacao WHERE mp-classificacao.codigo = INPUT FRAME {&FRAME-NAME} mp-comp-mistura.cd-compr NO-LOCK NO-ERROR.
    IF AVAIL mp-classificacao THEN
       ASSIGN fi-comprimento:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(mp-classificacao.compr-min) + "  A  " + STRING(mp-classificacao.compr-max).

    FIND mp-tipo WHERE mp-tipo.codigo = INPUT FRAME {&FRAME-NAME} mp-comp-mistura.cd-tipo NO-LOCK NO-ERROR.
    IF AVAIL mp-tipo THEN
       ASSIGN fi-tipo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = mp-tipo.tipo.

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
  {src/adm/template/snd-list.i "mp-comp-mistura"}

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

