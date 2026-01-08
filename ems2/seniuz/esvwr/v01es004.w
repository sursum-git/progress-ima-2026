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

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES contr-sac
&Scoped-define FIRST-EXTERNAL-TABLE contr-sac


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR contr-sac.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS contr-sac.it-codigo contr-sac.cod-rep 
&Scoped-define ENABLED-TABLES contr-sac
&Scoped-define FIRST-ENABLED-TABLE contr-sac
&Scoped-Define ENABLED-OBJECTS rt-key rt-mold 
&Scoped-Define DISPLAYED-FIELDS contr-sac.fm-codigo contr-sac.it-codigo ~
contr-sac.des-cor contr-sac.data-reg contr-sac.cod-emitente ~
contr-sac.cod-rep 
&Scoped-define DISPLAYED-TABLES contr-sac
&Scoped-define FIRST-DISPLAYED-TABLE contr-sac
&Scoped-Define DISPLAYED-OBJECTS fi-desc-familia fi-desc-item ~
fi-nom-emitente fi-nom-repres 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-CREATE-FIELDS contr-sac.fm-codigo contr-sac.des-cor ~
contr-sac.data-reg contr-sac.cod-emitente 

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
DEFINE VARIABLE fi-desc-familia AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 34.43 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-item AS CHARACTER FORMAT "X(36)":U 
     VIEW-AS FILL-IN 
     SIZE 50 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nom-emitente AS CHARACTER FORMAT "x(45)" 
     VIEW-AS FILL-IN 
     SIZE 54.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nom-repres AS CHARACTER FORMAT "x(40)" 
     VIEW-AS FILL-IN 
     SIZE 55.72 BY .88 NO-UNDO.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.57 BY 4.25.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.57 BY 1.25.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     contr-sac.fm-codigo AT ROW 1.17 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.14 BY .88
     fi-desc-familia AT ROW 1.17 COL 28.43 COLON-ALIGNED NO-LABEL
     contr-sac.it-codigo AT ROW 2.17 COL 16 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 12.57 BY .88
     fi-desc-item AT ROW 2.17 COL 28.86 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     contr-sac.des-cor AT ROW 3.17 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.14 BY .88
     contr-sac.data-reg AT ROW 3.17 COL 50.43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.57 BY .88
     contr-sac.cod-emitente AT ROW 4.17 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY .88
     fi-nom-emitente AT ROW 4.17 COL 24.29 COLON-ALIGNED NO-LABEL
     contr-sac.cod-rep AT ROW 5.54 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.86 BY .88
     fi-nom-repres AT ROW 5.54 COL 23.14 COLON-ALIGNED HELP
          "Nome do Representante" NO-LABEL
     rt-key AT ROW 1 COL 1
     rt-mold AT ROW 5.38 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: espec.contr-sac
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
         HEIGHT             = 5.67
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

/* SETTINGS FOR FILL-IN contr-sac.cod-emitente IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN contr-sac.data-reg IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN contr-sac.des-cor IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN fi-desc-familia IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-item IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nom-emitente IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nom-repres IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN contr-sac.fm-codigo IN FRAME f-main
   NO-ENABLE 1                                                          */
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

&Scoped-define SELF-NAME contr-sac.cod-emitente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contr-sac.cod-emitente V-table-Win
ON ENTRY OF contr-sac.cod-emitente IN FRAME f-main /* Cliente */
DO:
   FIND emitente WHERE emitente.cod-emitente = INPUT FRAME {&FRAME-NAME} contr-sac.cod-emitente
                 NO-LOCK NO-ERROR.
   IF AVAIL emitente THEN
      ASSIGN fi-nom-emitente:SCREEN-VALUE IN FRAME {&FRAME-NAME} = emitente.nome-emit.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contr-sac.cod-emitente V-table-Win
ON LEAVE OF contr-sac.cod-emitente IN FRAME f-main /* Cliente */
DO:
   FIND emitente WHERE emitente.cod-emitente = INPUT FRAME {&FRAME-NAME} contr-sac.cod-emitente
                 NO-LOCK NO-ERROR.
   IF AVAIL emitente THEN
      ASSIGN fi-nom-emitente:SCREEN-VALUE IN FRAME {&FRAME-NAME} = emitente.nome-emit
             contr-sac.cod-rep:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(emitente.cod-rep).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contr-sac.cod-emitente V-table-Win
ON MOUSE-SELECT-DBLCLICK OF contr-sac.cod-emitente IN FRAME f-main /* Cliente */
DO:
  {include/zoomvar.i &prog-zoom = adzoom/z02ad098.w
                     &campo     = contr-sac.cod-emitente
                     &campozoom = cod-emitente}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME contr-sac.cod-rep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contr-sac.cod-rep V-table-Win
ON ENTRY OF contr-sac.cod-rep IN FRAME f-main /* Representante */
DO:
   FIND repres WHERE repres.cod-rep = INPUT FRAME {&FRAME-NAME} contr-sac.cod-rep NO-LOCK NO-ERROR.
   IF AVAIL repres THEN
      ASSIGN fi-nom-repres:SCREEN-VALUE IN FRAME {&FRAME-NAME} = repres.nome.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contr-sac.cod-rep V-table-Win
ON LEAVE OF contr-sac.cod-rep IN FRAME f-main /* Representante */
DO:
   FIND repres WHERE repres.cod-rep = INPUT FRAME {&FRAME-NAME} contr-sac.cod-rep NO-LOCK NO-ERROR.
   IF AVAIL repres THEN
      ASSIGN fi-nom-repres:SCREEN-VALUE IN FRAME {&FRAME-NAME} = repres.nome.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contr-sac.cod-rep V-table-Win
ON MOUSE-SELECT-DBLCLICK OF contr-sac.cod-rep IN FRAME f-main /* Representante */
DO:
  {include/zoomvar.i &prog-zoom = adzoom/z01ad229.w
                     &campo     = contr-sac.cod-rep
                     &campozoom = cod-rep}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME contr-sac.fm-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contr-sac.fm-codigo V-table-Win
ON ENTRY OF contr-sac.fm-codigo IN FRAME f-main /* Familia */
DO:
  FIND familia WHERE familia.fm-codigo = INPUT FRAME {&frame-name} contr-sac.fm-codigo 
               NO-LOCK NO-ERROR.
  IF AVAIL familia THEN
     ASSIGN fi-desc-familia:SCREEN-VALUE IN FRAME {&FRAME-NAME} = familia.descricao.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contr-sac.fm-codigo V-table-Win
ON LEAVE OF contr-sac.fm-codigo IN FRAME f-main /* Familia */
DO:
  FIND familia WHERE familia.fm-codigo = INPUT FRAME {&FRAME-NAME} contr-sac.fm-codigo 
               NO-LOCK NO-ERROR.
  IF AVAIL familia THEN
     ASSIGN fi-desc-familia:SCREEN-VALUE IN FRAME {&FRAME-NAME} = familia.descricao.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contr-sac.fm-codigo V-table-Win
ON MOUSE-SELECT-DBLCLICK OF contr-sac.fm-codigo IN FRAME f-main /* Familia */
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z01in122.w
                     &campo     = contr-sac.fm-codigo
                     &campozoom = fm-codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME contr-sac.it-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contr-sac.it-codigo V-table-Win
ON LEAVE OF contr-sac.it-codigo IN FRAME f-main /* Item */
DO:
  FIND item WHERE item.it-codigo = INPUT FRAME {&FRAME-NAME} contr-sac.it-codigo 
               NO-LOCK NO-ERROR.
  IF AVAIL item AND item.it-codigo <> "" THEN DO:
     ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
     FIND familia WHERE familia.fm-codigo = ITEM.fm-codigo NO-LOCK.
     ASSIGN contr-sac.fm-codigo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = familia.fm-codigo
            fi-desc-familia:SCREEN-VALUE IN FRAME {&FRAME-NAME} = familia.descricao.
  END.
  ELSE DO:
     IF ITEM.it-codigo <> "" THEN DO:
        MESSAGE "÷tem inv†lido."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY 'entry' TO contr-sac.it-codigo IN FRAME {&FRAME-NAME}.
        RETURN NO-APPLY.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contr-sac.it-codigo V-table-Win
ON MOUSE-SELECT-DBLCLICK OF contr-sac.it-codigo IN FRAME f-main /* Item */
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z01in172.w
                     &campo     = contr-sac.it-codigo
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
  contr-sac.fm-codigo:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.
  contr-sac.it-codigo:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.
  contr-sac.cod-emitente:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.
  contr-sac.cod-rep:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.
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
  {src/adm/template/row-list.i "contr-sac"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "contr-sac"}

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
    FIND familia WHERE familia.fm-codigo = contr-sac.fm-codigo NO-LOCK NO-ERROR.
    IF AVAIL familia THEN
       ASSIGN fi-desc-familia:SCREEN-VALUE IN FRAME {&FRAME-NAME} = familia.descricao.


    FIND emitente WHERE emitente.cod-emitente = contr-sac.cod-emitente NO-LOCK NO-ERROR.
    IF AVAIL emitente THEN
       ASSIGN fi-nom-emitente:SCREEN-VALUE IN FRAME {&FRAME-NAME} = emitente.nome-emit.

    FIND repres WHERE repres.cod-rep = contr-sac.cod-rep NO-LOCK NO-ERROR.
    IF AVAIL repres THEN
       ASSIGN fi-nom-repres:SCREEN-VALUE IN FRAME {&FRAME-NAME} = repres.nome.

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
    
/*/*    Segue um exemplo de validaá∆o de programa */
 *     find tabela where tabela.campo1 = c-variavel and
 *                       tabela.campo2 > i-variavel no-lock no-error.
 *     
 *     /* Este include deve ser colocado sempre antes do ut-msgs.p */
 *     {include/i-vldprg.i}
 *     run utp/ut-msgs.p (input "show":U, input 7, input return-value).
 *     return 'ADM-ERROR':U.*/


IF NOT CAN-FIND(familia WHERE familia.fm-codigo = INPUT FRAME {&FRAME-NAME} contr-sac.fm-codigo) OR
   INPUT FRAME {&FRAME-NAME} contr-sac.fm-codigo = "" THEN DO:
   MESSAGE "Fam°lia inv†lida."
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
   APPLY 'entry' TO contr-sac.fm-codigo IN FRAME {&FRAME-NAME}.
   RETURN 'ADM-ERROR':U.
END.
IF NOT CAN-FIND(ITEM WHERE item.it-codigo = INPUT FRAME {&FRAME-NAME} contr-sac.it-codigo) OR
   INPUT FRAME {&FRAME-NAME} contr-sac.it-codigo = "" THEN DO:
   MESSAGE "Item inv†lido."
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
   APPLY 'entry' TO contr-sac.it-codigo IN FRAME {&FRAME-NAME}.
   IF adm-new-record AND INPUT FRAME {&FRAME-NAME} contr-sac.it-codigo = "" THEN
      RETURN 'ADM-ERROR':U.
END.
IF INPUT FRAME {&FRAME-NAME} contr-sac.des-cor <> "" THEN DO:
    FIND ITEM WHERE ITEM.it-codigo = INPUT FRAME {&FRAME-NAME} contr-sac.fm-codigo +
                                     INPUT FRAME {&FRAME-NAME} contr-sac.des-cor
              NO-LOCK NO-ERROR.
    IF NOT AVAIL ITEM THEN DO:
       MESSAGE "N∆o existe ÷tem com essa Fam°lia/Desenho/Cor." VIEW-AS ALERT-BOX.                
       APPLY "entry" TO contr-sac.des-cor.                                                            
       return 'ADM-ERROR':U.
    END.
END.

FIND emitente WHERE emitente.cod-emitente = INPUT FRAME {&FRAME-NAME} contr-sac.cod-emitente             
              NO-LOCK NO-ERROR.                                                                         
IF NOT AVAIL emitente THEN DO:                                                                          
   MESSAGE "Cliente inv†lido." VIEW-AS ALERT-BOX.                
   APPLY "entry" TO contr-sac.cod-emitente.                                                            
   return 'ADM-ERROR':U.                                                                               
END.                                                                                                   

IF INPUT FRAME {&frame-name} contr-sac.data-reg = ? or
   INPUT FRAME {&frame-name} contr-sac.data-reg > TODAY THEN DO:                                                                          
   MESSAGE "Data n∆o poder Nula ou Maior que a de Hoje." VIEW-AS ALERT-BOX.                
   APPLY "entry" TO contr-sac.data-reg.                                                            
   return 'ADM-ERROR':U.                                                                               
END.                                                                                                   

FIND repres WHERE repres.cod-rep = INPUT FRAME {&FRAME-NAME} contr-sac.cod-rep             
            NO-LOCK NO-ERROR.                                                                         
IF NOT AVAIL repres THEN DO:                                                                          
   MESSAGE "Representante inv†lido." VIEW-AS ALERT-BOX.                
   APPLY "entry" TO contr-sac.cod-rep.                                                            
   return 'ADM-ERROR':U.                                                                               
END.                                                                                                   

FIND contr-sac WHERE contr-sac.fm-codigo    = INPUT FRAME {&FRAME-NAME} contr-sac.fm-codigo        
                 AND contr-sac.des-cor      = INPUT FRAME {&FRAME-NAME} contr-sac.des-cor               
                 AND contr-sac.data-reg     = INPUT FRAME {&FRAME-NAME} contr-sac.data-reg              
                 AND contr-sac.cod-emitente = INPUT FRAME {&FRAME-NAME} contr-sac.cod-emitente          
               NO-LOCK NO-ERROR.                                                                        
IF AVAIL contr-sac THEN DO:                                                                           
   MESSAGE "J† existe Registro para essa Fam°lia/Data/Cliente." VIEW-AS ALERT-BOX.                      
   APPLY "entry" TO contr-sac.fm-codigo.                                                            
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
  {src/adm/template/snd-list.i "contr-sac"}

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

