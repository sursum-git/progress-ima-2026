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
DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
def var v-row-parent as rowid no-undo.
DEF VAR h-container AS HANDLE.
DEF VAR h-b01es020 AS HANDLE.

DEF VAR de-perc AS DEC FORMAT "->>9.99".

DEF VAR c-cod-estabel AS CHAR.

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
&Scoped-define EXTERNAL-TABLES mov-est-acbm
&Scoped-define FIRST-EXTERNAL-TABLE mov-est-acbm


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR mov-est-acbm.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS mov-est-acbm.cod-estabel ~
mov-est-acbm.cod-refer mov-est-acbm.tipo-tear mov-est-acbm.qtd-tot-perf 
&Scoped-define ENABLED-TABLES mov-est-acbm
&Scoped-define FIRST-ENABLED-TABLE mov-est-acbm
&Scoped-Define ENABLED-OBJECTS rt-key rt-mold 
&Scoped-Define DISPLAYED-FIELDS mov-est-acbm.cod-estabel ~
mov-est-acbm.data-mov mov-est-acbm.num-lote mov-est-acbm.it-codigo ~
mov-est-acbm.cod-refer mov-est-acbm.tipo-tear mov-est-acbm.qtd-tot-def ~
mov-est-acbm.qtd-tot-perf mov-est-acbm.qtd-tot-sob 
&Scoped-define DISPLAYED-TABLES mov-est-acbm
&Scoped-define FIRST-DISPLAYED-TABLE mov-est-acbm
&Scoped-Define DISPLAYED-OBJECTS fi-desc-item fi-tot-sanfor fi-cor1 ~
fi-perc-sobra fi-cor-2 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-CREATE-FIELDS mov-est-acbm.data-mov ~
mov-est-acbm.num-lote mov-est-acbm.it-codigo 
&Scoped-define ADM-ASSIGN-FIELDS mov-est-acbm.qtd-tot-def ~
mov-est-acbm.qtd-tot-sob 
&Scoped-define List-4 fi-tot-sanfor 

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
DEFINE VARIABLE fi-cor-2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 3 BY .5
     BGCOLOR 12  NO-UNDO.

DEFINE VARIABLE fi-cor1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 3 BY .5
     BGCOLOR 2  NO-UNDO.

DEFINE VARIABLE fi-desc-item AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 47 BY .88 NO-UNDO.

DEFINE VARIABLE fi-perc-sobra AS DECIMAL FORMAT ">>9.99 %":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88
     FONT 0 NO-UNDO.

DEFINE VARIABLE fi-tot-sanfor AS DECIMAL FORMAT ">>>,>>9.99":U INITIAL 0 
     LABEL "Sanorizado" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88 BY 3.25.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88 BY 3.5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     mov-est-acbm.cod-estabel AT ROW 1.17 COL 18 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 3.29 BY .88
     mov-est-acbm.data-mov AT ROW 1.17 COL 38.86 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.57 BY .88
     mov-est-acbm.num-lote AT ROW 1.25 COL 75.72 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.86 BY .88
     mov-est-acbm.it-codigo AT ROW 2.17 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.14 BY .88
     fi-desc-item AT ROW 2.17 COL 36 COLON-ALIGNED NO-LABEL
     mov-est-acbm.cod-refer AT ROW 3.17 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY .88
     mov-est-acbm.tipo-tear AT ROW 4.75 COL 18 COLON-ALIGNED
          VIEW-AS COMBO-BOX 
          LIST-ITEMS "Howa","Nissan","Sulzer","Picanol","Tsudakoma","Toyota" 
          DROP-DOWN-LIST
          SIZE 9.86 BY .88
     fi-tot-sanfor AT ROW 5.75 COL 18 COLON-ALIGNED
     mov-est-acbm.qtd-tot-def AT ROW 5.75 COL 49 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.57 BY .88
     fi-cor1 AT ROW 6.71 COL 72.43 COLON-ALIGNED NO-LABEL
     mov-est-acbm.qtd-tot-perf AT ROW 6.75 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.57 BY .88
     mov-est-acbm.qtd-tot-sob AT ROW 6.75 COL 49 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.57 BY .88
     fi-perc-sobra AT ROW 6.75 COL 62.43 COLON-ALIGNED NO-LABEL
     fi-cor-2 AT ROW 7.25 COL 72.43 COLON-ALIGNED NO-LABEL
     "N«O Aceita" VIEW-AS TEXT
          SIZE 10.14 BY .5 AT ROW 7.21 COL 78
     "Sobra Aceita" VIEW-AS TEXT
          SIZE 10.14 BY .71 AT ROW 6.54 COL 77.86
     rt-key AT ROW 1 COL 1
     rt-mold AT ROW 4.5 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: espec.mov-est-acbm
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
         HEIGHT             = 7.17
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

/* SETTINGS FOR FILL-IN mov-est-acbm.data-mov IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN fi-cor-2 IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-cor1 IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-item IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-perc-sobra IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-sanfor IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN mov-est-acbm.it-codigo IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN mov-est-acbm.num-lote IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN mov-est-acbm.qtd-tot-def IN FRAME f-main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN mov-est-acbm.qtd-tot-sob IN FRAME f-main
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

&Scoped-define SELF-NAME mov-est-acbm.cod-estabel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-est-acbm.cod-estabel V-table-Win
ON LEAVE OF mov-est-acbm.cod-estabel IN FRAME f-main /* Estabelecimento */
DO:
  FIND estabelec WHERE
       estabelec.cod-estabel = INPUT FRAME {&FRAME-NAME} mov-est-acbm.cod-estabel NO-LOCK NO-ERROR.

  IF NOT AVAIL estabelec THEN DO.
     MESSAGE "Estabelecimento n∆o Cadastrado..." VIEW-AS ALERT-BOX.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mov-est-acbm.cod-refer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-est-acbm.cod-refer V-table-Win
ON LEAVE OF mov-est-acbm.cod-refer IN FRAME f-main /* Referencia */
DO:
   FIND ref-item WHERE 
        ref-item.it-codigo = INPUT FRAME {&FRAME-NAME} mov-est-acbm.it-codigo AND 
        ref-item.cod-refer = INPUT FRAME {&FRAME-NAME} mov-est-acbm.cod-refer
        NO-LOCK NO-ERROR.

   IF NOT AVAIL ref-item THEN DO.
      MESSAGE "Referencia nao Relacionada ao Item..." VIEW-AS ALERT-BOX.
      APPLY 'entry' TO SELF.
      RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-est-acbm.cod-refer V-table-Win
ON MOUSE-SELECT-DBLCLICK OF mov-est-acbm.cod-refer IN FRAME f-main /* Referencia */
DO:
    {include/zoomvar.i &prog-zoom=inzoom/z01in375.w
                       &campo=mov-est-acbm.cod-refer
                       &campozoom=cod-refer
                       &parametros="run pi-seta-inicial in
                        wh-pesquisa(INPUT INPUT FRAME {&FRAME-NAME} mov-est-acbm.it-codigo)."}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mov-est-acbm.it-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-est-acbm.it-codigo V-table-Win
ON ENTRY OF mov-est-acbm.it-codigo IN FRAME f-main /* Item */
DO:
   IF SELF:SCREEN-VALUE <> '' THEN DO.
      FIND item WHERE
           item.it-codigo = INPUT FRAME {&FRAME-NAME} mov-est-acbm.it-codigo NO-LOCK NO-ERROR.

      IF AVAIL item THEN
         ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-est-acbm.it-codigo V-table-Win
ON LEAVE OF mov-est-acbm.it-codigo IN FRAME f-main /* Item */
DO:
 FIND item WHERE
      item.it-codigo = INPUT FRAME {&FRAME-NAME} mov-est-acbm.it-codigo NO-LOCK NO-ERROR.
 IF NOT AVAIL item THEN DO.
    MESSAGE "Item n∆o Cadastrado..." VIEW-AS ALERT-BOX.
    APPLY 'entry' TO SELF.
    RETURN NO-APPLY.
 END.
 ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
    
 FIND item-ext WHERE
      item-ext.it-codigo = item.it-codigo NO-LOCK NO-ERROR.

 IF AVAIL item-ext AND NOT item-ext.indigo THEN
    RUN pi-trata-folder IN h-container (INPUT "D", INPUT 2).
 ELSE      
    RUN pi-trata-folder IN h-container (INPUT "E", INPUT 2).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-est-acbm.it-codigo V-table-Win
ON MOUSE-SELECT-DBLCLICK OF mov-est-acbm.it-codigo IN FRAME f-main /* Item */
DO:
  {include/zoomvar.i &prog-zoom=inzoom/z01in172.w
                     &campo=mov-est-acbm.it-codigo
                     &campozoom=it-codigo}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mov-est-acbm.qtd-tot-perf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-est-acbm.qtd-tot-perf V-table-Win
ON LEAVE OF mov-est-acbm.qtd-tot-perf IN FRAME f-main /* Total Perfeito */
DO:
  IF INPUT FRAME {&FRAME-NAME} mov-est-acbm.qtd-tot-perf > INPUT FRAME {&FRAME-NAME} fi-tot-sanfor -
                                                           INPUT FRAME {&FRAME-NAME} mov-est-acbm.qtd-tot-def THEN DO.
     MESSAGE "Total Perfeito deve ser menor ou igual ao Sanforizado - Defeitos..." VIEW-AS ALERT-BOX.
     APPLY 'entry' TO SELF.
     RETURN NO-APPLY.
  END.

  IF INPUT FRAME {&FRAME-NAME} mov-est-acbm.qtd-tot-perf = INPUT FRAME {&FRAME-NAME} fi-tot-sanfor THEN DO.
     IF item-ext.indigo THEN
        RUN pi-select-page IN h-container (INPUT 2).
  END.
  ELSE DO.
     RUN pi-calc-defeito (INPUT "-", INPUT 0).

     RUN pi-habilita IN h-b01es020.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  RUN esapi/busca-estab-usuar.p (INPUT c-seg-usuario,
                                 OUTPUT c-cod-estabel).
  IF c-cod-estabel = '' OR c-cod-estabel = '0' THEN
     ASSIGN c-cod-estabel = '2'.

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  
  mov-est-acbm.it-codigo:LOAD-MOUSE-POINTER("image/lupa.cur").
  mov-est-acbm.cod-refer:LOAD-MOUSE-POINTER("image/lupa.cur").
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
  {src/adm/template/row-list.i "mov-est-acbm"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "mov-est-acbm"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record V-table-Win 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN mov-est-acbm.data-mov:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY)
         fi-tot-sanfor:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '0,00'.

  RUN pi-popula-browse IN h-b01es020 (INPUT ROWID(mov-est-acbm)).
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
    
    RUN pi-validate.
    if RETURN-VALUE = 'ADM-ERROR':U then 
       return 'ADM-ERROR':U.

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.
    
    /* Todos os assignÔs n∆o feitos pelo assign-record devem ser feitos aqui */  
    /* Code placed here will execute AFTER standard behavior.    */

    RUN pi-assign-record IN h-b01es020 (INPUT ROWID(mov-est-acbm)).

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
    
    &IF DEFINED(list-4) &THEN
        DISABLE {&list-4} with frame {&frame-name}.
    &ENDIF
    
    RUN pi-desabilita IN h-b01es020.
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
    
    IF AVAIL mov-est-acbm THEN DO.
       ASSIGN fi-tot-sanfor = mov-est-acbm.qtd-tot-perf + mov-est-acbm.qtd-tot-def + mov-est-acbm.qtd-tot-sob.  

       FIND item WHERE
            item.it-codigo = mov-est-acbm.it-codigo NO-LOCK NO-ERROR.

       IF AVAIL item THEN
          ASSIGN fi-desc-item = item.desc-item.

       FIND item-ext WHERE
            item-ext.it-codigo = item.it-codigo NO-LOCK NO-ERROR.

       IF AVAIL item-ext AND NOT item-ext.indigo THEN
          RUN pi-trata-folder IN h-container (INPUT "D", INPUT 2).
       ELSE      
          RUN pi-trata-folder IN h-container (INPUT "E", INPUT 2).

       IF mov-est-acbm.qtd-tot-perf = fi-tot-sanfor AND
          AVAIL item-ext AND item-ext.indigo THEN 
          RUN pi-select-page IN h-container (INPUT 2).
       ELSE
          RUN pi-select-page IN h-container (INPUT 1). 
    END.
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .
    
    /* Code placed here will execute AFTER standard behavior.    */

    RUN pi-calc-defeito (INPUT "-", INPUT 0).
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
    
    &IF DEFINED(list-4) &THEN
        enable {&list-4} with frame {&frame-name}.
    &ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-row-available V-table-Win 
PROCEDURE local-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-available':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  IF AVAIL mov-est-acbm THEN 
     RUN pi-popula-browse IN h-b01es020 (INPUT ROWID(mov-est-acbm)).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-calc-defeito V-table-Win 
PROCEDURE pi-calc-defeito :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER p-operacao AS CHAR.
    DEF INPUT PARAMETER p-valor AS DEC.

    CASE p-operacao:
        WHEN "-" THEN
           ASSIGN mov-est-acbm.qtd-tot-def:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(INPUT FRAME {&FRAME-NAME} mov-est-acbm.qtd-tot-def -
                                                                                        p-valor).
        WHEN "+" THEN
           ASSIGN mov-est-acbm.qtd-tot-def:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(INPUT FRAME {&FRAME-NAME} mov-est-acbm.qtd-tot-def +
                                                                                        p-valor).
    END CASE.

    ASSIGN mov-est-acbm.qtd-tot-sob:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(INPUT FRAME {&FRAME-NAME} fi-tot-sanfor -
                                                                                 INPUT FRAME {&FRAME-NAME} mov-est-acbm.qtd-tot-perf -
                                                                                 INPUT FRAME {&FRAME-NAME} mov-est-acbm.qtd-tot-def).

    ASSIGN de-perc = INPUT FRAME {&FRAME-NAME} mov-est-acbm.qtd-tot-sob / 
                     INPUT FRAME {&FRAME-NAME} fi-tot-sanfor * 100.

    ASSIGN fi-perc-sobra:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(de-perc).

    
    ASSIGN fi-perc-sobra:FGCOLOR = IF de-perc <= 5 THEN 2 ELSE 12.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-obtem-handle V-table-Win 
PROCEDURE pi-obtem-handle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER p1 AS HANDLE.
    DEF INPUT PARAMETER p2 AS HANDLE.
    ASSIGN h-container = p1
            h-b01es020 = p2.
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
    
    IF INPUT FRAME {&FRAME-NAME} fi-perc-sobra > 5 THEN DO.
       MESSAGE "Percentual n∆o Aceit†vel para Sobra" VIEW-AS ALERT-BOX.
       RETURN 'ADM-ERROR':U.
    END.
    IF INT(INPUT FRAME {&FRAME-NAME} mov-est-acbm.cod-estabel) = 0  THEN DO:
       MESSAGE "Estabelecimento n∆o informado." VIEW-AS ALERT-BOX.
       APPLY 'entry' TO mov-est-acbm.cod-estabel.
       RETURN NO-APPLY.
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
  {src/adm/template/snd-list.i "mov-est-acbm"}

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

