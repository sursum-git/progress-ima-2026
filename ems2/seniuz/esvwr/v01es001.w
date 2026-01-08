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
&Scoped-define EXTERNAL-TABLES amostra-fam
&Scoped-define FIRST-EXTERNAL-TABLE amostra-fam


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR amostra-fam.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS amostra-fam.tipo-tear amostra-fam.merceriz ~
amostra-fam.observ amostra-fam.larg-ini amostra-fam.larg-fin ~
amostra-fam.enc-urd amostra-fam.enc-tra amostra-fam.dentra-ini ~
amostra-fam.dentra-fin amostra-fam.num-ob 
&Scoped-define ENABLED-TABLES amostra-fam
&Scoped-define FIRST-ENABLED-TABLE amostra-fam
&Scoped-Define ENABLED-OBJECTS rt-key rt-mold 
&Scoped-Define DISPLAYED-FIELDS amostra-fam.fm-codigo amostra-fam.it-codigo ~
amostra-fam.ano-mes amostra-fam.num-amostra amostra-fam.tipo-tear ~
amostra-fam.merceriz amostra-fam.observ amostra-fam.larg-ini ~
amostra-fam.larg-fin amostra-fam.enc-urd amostra-fam.enc-tra ~
amostra-fam.dentra-ini amostra-fam.dentra-fin amostra-fam.num-ob 
&Scoped-define DISPLAYED-TABLES amostra-fam
&Scoped-define FIRST-DISPLAYED-TABLE amostra-fam
&Scoped-Define DISPLAYED-OBJECTS fi-desc-familia fi-desc-item 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-CREATE-FIELDS amostra-fam.fm-codigo ~
amostra-fam.it-codigo amostra-fam.ano-mes 
&Scoped-define ADM-ASSIGN-FIELDS amostra-fam.num-amostra 

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
DEFINE VARIABLE fi-desc-familia AS CHARACTER FORMAT "X(36)":U 
     VIEW-AS FILL-IN 
     SIZE 40.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-item AS CHARACTER FORMAT "X(36)":U 
     VIEW-AS FILL-IN 
     SIZE 50 BY .88 NO-UNDO.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 88 BY 4.25.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 88 BY 9.54.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     amostra-fam.fm-codigo AT ROW 1.17 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.14 BY .88
     fi-desc-familia AT ROW 1.17 COL 27.43 COLON-ALIGNED NO-LABEL
     amostra-fam.it-codigo AT ROW 2.17 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.57 BY .88
     fi-desc-item AT ROW 2.17 COL 29.86 COLON-ALIGNED NO-LABEL
     amostra-fam.ano-mes AT ROW 3.17 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.14 BY .88 TOOLTIP "Ano/màs do teste, no formato AAAA/MM."
     amostra-fam.num-amostra AT ROW 4.17 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 3.43 BY .88
     amostra-fam.tipo-tear AT ROW 5.71 COL 17 COLON-ALIGNED
          VIEW-AS COMBO-BOX 
          LIST-ITEMS "Howa","Nissan","Sulzer","Picanol","Tsudakoma","Toyota" 
          DROP-DOWN-LIST
          SIZE 9.72 BY 1
     amostra-fam.merceriz AT ROW 5.71 COL 33.14
          VIEW-AS TOGGLE-BOX
          SIZE 15.14 BY .88
     amostra-fam.observ AT ROW 6.71 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16.14 BY .88
     amostra-fam.larg-ini AT ROW 7.71 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.86 BY .88
     amostra-fam.larg-fin AT ROW 8.71 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.86 BY .88
     amostra-fam.enc-urd AT ROW 9.71 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.86 BY .88
     amostra-fam.enc-tra AT ROW 10.71 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.86 BY .88
     amostra-fam.dentra-ini AT ROW 11.71 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.72 BY .79
     amostra-fam.dentra-fin AT ROW 12.71 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.72 BY .79
     amostra-fam.num-ob AT ROW 13.71 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.86 BY .88
     rt-key AT ROW 1 COL 1
     rt-mold AT ROW 5.46 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: espec.amostra-fam
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
         HEIGHT             = 14.17
         WIDTH              = 88.14.
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

/* SETTINGS FOR FILL-IN amostra-fam.ano-mes IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN fi-desc-familia IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-item IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN amostra-fam.fm-codigo IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN amostra-fam.it-codigo IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN amostra-fam.num-amostra IN FRAME f-main
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

&Scoped-define SELF-NAME amostra-fam.ano-mes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL amostra-fam.ano-mes V-table-Win
ON LEAVE OF amostra-fam.ano-mes IN FRAME f-main /* Periodo */
DO:
  FIND LAST amostra-fam 
       WHERE amostra-fam.fm-codigo = INPUT FRAME {&FRAME-NAME} amostra-fam.fm-codigo
         AND amostra-fam.it-codigo = INPUT FRAME {&FRAME-NAME} amostra-fam.it-codigo
         AND amostra-fam.ano-mes   = INPUT FRAME {&FRAME-NAME} amostra-fam.ano-mes
       NO-LOCK NO-ERROR.
  IF AVAIL amostra-fam THEN DO:
     IF amostra-fam.num-amostra = 999 THEN DO:
        MESSAGE "Alcanáado o limite de 999 Testes para Familia/Item/Data" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
     END.
     ASSIGN amostra-fam.num-amostra:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(amostra-fam.num-amostra + 1).
  END.
  ELSE
     ASSIGN amostra-fam.num-amostra:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "001".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME amostra-fam.fm-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL amostra-fam.fm-codigo V-table-Win
ON ENTRY OF amostra-fam.fm-codigo IN FRAME f-main /* Familia */
DO:
  FIND familia WHERE familia.fm-codigo = INPUT FRAME {&FRAME-NAME} amostra-fam.fm-codigo 
               NO-LOCK NO-ERROR.
  IF AVAIL familia THEN
     ASSIGN fi-desc-familia:SCREEN-VALUE IN FRAME {&FRAME-NAME} = familia.descricao.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL amostra-fam.fm-codigo V-table-Win
ON LEAVE OF amostra-fam.fm-codigo IN FRAME f-main /* Familia */
DO:
  FIND familia WHERE familia.fm-codigo = INPUT FRAME {&FRAME-NAME} amostra-fam.fm-codigo 
               NO-LOCK NO-ERROR.
  IF AVAIL familia THEN
     ASSIGN fi-desc-familia:SCREEN-VALUE IN FRAME {&FRAME-NAME} = familia.descricao.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL amostra-fam.fm-codigo V-table-Win
ON MOUSE-SELECT-DBLCLICK OF amostra-fam.fm-codigo IN FRAME f-main /* Familia */
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z01in122.w
                     &campo     = amostra-fam.fm-codigo
                     &campozoom = fm-codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME amostra-fam.it-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL amostra-fam.it-codigo V-table-Win
ON ENTRY OF amostra-fam.it-codigo IN FRAME f-main /* Item */
DO:
  FIND item WHERE item.it-codigo = INPUT FRAME {&FRAME-NAME} amostra-fam.it-codigo 
               NO-LOCK NO-ERROR.
  IF AVAIL item THEN
     ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL amostra-fam.it-codigo V-table-Win
ON LEAVE OF amostra-fam.it-codigo IN FRAME f-main /* Item */
DO:
  FIND item WHERE item.it-codigo = INPUT FRAME {&FRAME-NAME} amostra-fam.it-codigo 
               NO-LOCK NO-ERROR.
  IF AVAIL item AND item.it-codigo <> "" THEN DO:
     ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
     FIND familia WHERE familia.fm-codigo = ITEM.fm-codigo NO-LOCK.
     ASSIGN amostra-fam.fm-codigo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = familia.fm-codigo
            fi-desc-familia:SCREEN-VALUE IN FRAME {&FRAME-NAME} = familia.descricao.
  END.
  ELSE DO:
     IF ITEM.it-codigo <> "" THEN DO:
        MESSAGE "÷tem inv†lido."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY 'entry' TO amostra-fam.it-codigo IN FRAME {&FRAME-NAME}.
        RETURN NO-APPLY.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL amostra-fam.it-codigo V-table-Win
ON MOUSE-SELECT-DBLCLICK OF amostra-fam.it-codigo IN FRAME f-main /* Item */
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z01in172.w
                     &campo     = amostra-fam.it-codigo
                     &campozoom = it-codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  amostra-fam.fm-codigo:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.
  amostra-fam.it-codigo:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.
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
  {src/adm/template/row-list.i "amostra-fam"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "amostra-fam"}

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
  ASSIGN amostra-fam.tipo-tear:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Howa".
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
    FIND familia WHERE familia.fm-codigo = amostra-fam.fm-codigo NO-LOCK NO-ERROR.
    IF AVAIL familia THEN
       ASSIGN fi-desc-familia:SCREEN-VALUE IN FRAME {&FRAME-NAME} = familia.descricao.

    FIND ITEM WHERE ITEM.it-codigo = amostra-fam.it-codigo NO-LOCK NO-ERROR.
    IF AVAIL familia THEN
       ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ITEM.desc-item.

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
  Notes: N∆o fazer assign aqui. Nesta procedure
  devem ser colocadas apenas validaá‰es, pois neste ponto do programa o registro 
  ainda n∆o foi criado.       
------------------------------------------------------------------------------*/
    
    {include/i-vldfrm.i} /* Validaá∆o de dicion†rio */

    IF NOT CAN-FIND(familia WHERE familia.fm-codigo = INPUT FRAME {&FRAME-NAME} amostra-fam.fm-codigo) OR
       INPUT FRAME {&FRAME-NAME} amostra-fam.fm-codigo = "" THEN DO:
       MESSAGE "Fam°lia inv†lida."
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
       APPLY 'entry' TO amostra-fam.fm-codigo IN FRAME {&FRAME-NAME}.
       RETURN 'ADM-ERROR':U.
    END.
    IF NOT CAN-FIND(ITEM WHERE item.it-codigo = INPUT FRAME {&FRAME-NAME} amostra-fam.it-codigo) OR
       INPUT FRAME {&FRAME-NAME} amostra-fam.it-codigo = "" THEN DO:
       MESSAGE "Item inv†lido."
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
       APPLY 'entry' TO amostra-fam.it-codigo IN FRAME {&FRAME-NAME}.
       RETURN 'ADM-ERROR':U.
    END.
    IF SUBSTR(INPUT FRAME {&FRAME-NAME} amostra-fam.ano-mes,1,4) < "1900" OR
       SUBSTR(INPUT FRAME {&FRAME-NAME} amostra-fam.ano-mes,5,2) < "01" OR 
       SUBSTR(INPUT FRAME {&FRAME-NAME} amostra-fam.ano-mes,5,2) > "12" THEN DO:
       MESSAGE "Per°odo inv†lido. Entre com o per°odo no formato AAAA/MM."
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
       APPLY 'entry' TO amostra-fam.ano-mes IN FRAME {&FRAME-NAME}.
       RETURN 'ADM-ERROR':U.
    END.
    IF LOOKUP(INPUT FRAME {&FRAME-NAME} amostra-fam.tipo-tear,"Nissan,Howa,Sulzer,Picanol,Tsudakoma,Toyota") = 0 THEN DO:
       MESSAGE "Tipo Tear inv†lido. Entre com Nissan, Howa, Sulzer, Picanol, Tsudakoma."
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
       APPLY 'entry' TO amostra-fam.tipo-tear IN FRAME {&FRAME-NAME}.
       RETURN 'ADM-ERROR':U.
    END.
    IF INT(INPUT FRAME {&FRAME-NAME} amostra-fam.larg-ini) <= 0 THEN DO:
       MESSAGE "Largura Inicial inv†lida. Entre com um valor Maior que Zero."
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
       APPLY 'entry' TO amostra-fam.larg-ini IN FRAME {&FRAME-NAME}.
       RETURN 'ADM-ERROR':U.
    END.
    IF INT(INPUT FRAME {&FRAME-NAME} amostra-fam.larg-fin) <= 0 THEN DO:
       MESSAGE "Largura Final inv†lida. Entre com um valor Maior que Zero."
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
       APPLY 'entry' TO amostra-fam.larg-fin IN FRAME {&FRAME-NAME}.
       RETURN 'ADM-ERROR':U.
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
  {src/adm/template/snd-list.i "amostra-fam"}

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

