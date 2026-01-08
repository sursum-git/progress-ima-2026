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
DEF VAR c-tipos        AS CHAR.
DEF VAR c-obsoleto     AS CHAR.
DEF VAR c-cod-obsoleto AS CHAR.
DEF VAR c-lista-aux    AS CHAR.
DEF VAR i-tipo-cliente AS INT.
DEF VAR i-ct           AS INT FORMAT "9".

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
&Scoped-define EXTERNAL-TABLES espec.param-pef
&Scoped-define FIRST-EXTERNAL-TABLE espec.param-pef


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR espec.param-pef.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS espec.param-pef.ate-500 ~
espec.param-pef.ate-1000 espec.param-pef.aci-1000 ~
espec.param-pef.lim-inferior espec.param-pef.lim-superior 
&Scoped-define ENABLED-TABLES espec.param-pef
&Scoped-define FIRST-ENABLED-TABLE espec.param-pef
&Scoped-Define ENABLED-OBJECTS rt-key rt-mold 
&Scoped-Define DISPLAYED-FIELDS espec.param-pef.cod-gr-cli ~
espec.param-pef.lote espec.param-pef.ate-500 espec.param-pef.ate-1000 ~
espec.param-pef.aci-1000 espec.param-pef.lim-inferior ~
espec.param-pef.lim-superior 
&Scoped-define DISPLAYED-TABLES espec.param-pef
&Scoped-define FIRST-DISPLAYED-TABLE espec.param-pef
&Scoped-Define DISPLAYED-OBJECTS fi-desc-grupo cb-tipo-cliente ~
cb-cod-obsoleto fi-seq[1] fi-seq[2] fi-seq[3] fi-seq[4] fi-seq[5] fi-seq[6] 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-CREATE-FIELDS espec.param-pef.cod-gr-cli cb-tipo-cliente ~
espec.param-pef.lote cb-cod-obsoleto 
&Scoped-define List-4 fi-seq[1] fi-seq[2] fi-seq[3] fi-seq[4] fi-seq[5] ~
fi-seq[6] 

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
DEFINE VARIABLE cb-cod-obsoleto AS CHARACTER FORMAT "X(256)":U 
     LABEL "C¢digo Obsoleto" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE cb-tipo-cliente AS CHARACTER FORMAT "X(256)":U 
     LABEL "Tipo do Cliente" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fi-desc-grupo AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 31 BY .88 NO-UNDO.

DEFINE VARIABLE fi-seq AS CHARACTER FORMAT "X(1)":U  EXTENT 6
     VIEW-AS FILL-IN 
     SIZE 2 BY .88 NO-UNDO.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.57 BY 4.25.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.57 BY 6.5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     espec.param-pef.cod-gr-cli AT ROW 1.17 COL 18 COLON-ALIGNED
          LABEL "Grupo do Cliente"
          VIEW-AS FILL-IN 
          SIZE 3.43 BY .88
     fi-desc-grupo AT ROW 1.17 COL 21.72 COLON-ALIGNED NO-LABEL
     cb-tipo-cliente AT ROW 2.17 COL 18 COLON-ALIGNED
     espec.param-pef.lote AT ROW 3.17 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5.14 BY .88
     cb-cod-obsoleto AT ROW 4.17 COL 18 COLON-ALIGNED
     fi-seq[1] AT ROW 5.75 COL 18 COLON-ALIGNED NO-LABEL
     fi-seq[2] AT ROW 5.75 COL 20.57 COLON-ALIGNED NO-LABEL
     fi-seq[3] AT ROW 5.75 COL 23 COLON-ALIGNED NO-LABEL
     fi-seq[4] AT ROW 5.75 COL 25.57 COLON-ALIGNED NO-LABEL
     fi-seq[5] AT ROW 5.75 COL 28 COLON-ALIGNED NO-LABEL
     fi-seq[6] AT ROW 5.75 COL 30.57 COLON-ALIGNED NO-LABEL
     espec.param-pef.ate-500 AT ROW 6.75 COL 18 COLON-ALIGNED
          LABEL "Nuance AtÇ 500 M"
          VIEW-AS FILL-IN 
          SIZE 4.57 BY .88
     espec.param-pef.ate-1000 AT ROW 7.75 COL 18 COLON-ALIGNED
          LABEL "Nuance AtÇ 1000 M"
          VIEW-AS FILL-IN 
          SIZE 4.57 BY .88
     espec.param-pef.aci-1000 AT ROW 8.75 COL 18 COLON-ALIGNED
          LABEL "Nuance Acima 1000 M"
          VIEW-AS FILL-IN 
          SIZE 4.57 BY .88
     espec.param-pef.lim-inferior AT ROW 9.75 COL 18 COLON-ALIGNED
          LABEL "Limite Inferior"
          VIEW-AS FILL-IN 
          SIZE 3.43 BY .88
     espec.param-pef.lim-superior AT ROW 10.75 COL 18 COLON-ALIGNED
          LABEL "Limite Superior"
          VIEW-AS FILL-IN 
          SIZE 3.43 BY .88
     "%" VIEW-AS TEXT
          SIZE 2 BY .54 AT ROW 9.96 COL 23.86
     "%" VIEW-AS TEXT
          SIZE 2 BY .54 AT ROW 10.96 COL 23.86
     "Sequància Qualidade:" VIEW-AS TEXT
          SIZE 15 BY .88 AT ROW 5.75 COL 4.72
     rt-key AT ROW 1 COL 1
     rt-mold AT ROW 5.25 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: espec.param-pef
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit Custom                            */
ASSIGN 
       FRAME f-main:SCROLLABLE       = FALSE
       FRAME f-main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN espec.param-pef.aci-1000 IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN espec.param-pef.ate-1000 IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN espec.param-pef.ate-500 IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR COMBO-BOX cb-cod-obsoleto IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR COMBO-BOX cb-tipo-cliente IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN espec.param-pef.cod-gr-cli IN FRAME f-main
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN fi-desc-grupo IN FRAME f-main
   NO-ENABLE DEF-LABEL                                                  */
/* SETTINGS FOR FILL-IN fi-seq[1] IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-seq[2] IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-seq[3] IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-seq[4] IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-seq[5] IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-seq[6] IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN espec.param-pef.lim-inferior IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN espec.param-pef.lim-superior IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN espec.param-pef.lote IN FRAME f-main
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

&Scoped-define SELF-NAME espec.param-pef.cod-gr-cli
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL espec.param-pef.cod-gr-cli V-table-Win
ON ENTRY OF espec.param-pef.cod-gr-cli IN FRAME f-main /* Grupo do Cliente */
DO:
  FIND gr-cli WHERE gr-cli.cod-gr-cli = INPUT FRAME {&FRAME-NAME} param-pef.cod-gr-cli NO-LOCK NO-ERROR.
  IF AVAIL gr-cli THEN
     ASSIGN fi-desc-grupo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = gr-cli.descricao.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL espec.param-pef.cod-gr-cli V-table-Win
ON LEAVE OF espec.param-pef.cod-gr-cli IN FRAME f-main /* Grupo do Cliente */
DO:
  FIND gr-cli WHERE gr-cli.cod-gr-cli = INPUT FRAME {&FRAME-NAME} param-pef.cod-gr-cli NO-LOCK NO-ERROR.
  IF AVAIL gr-cli THEN
     ASSIGN fi-desc-grupo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = gr-cli.descricao.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL espec.param-pef.cod-gr-cli V-table-Win
ON MOUSE-SELECT-DBLCLICK OF espec.param-pef.cod-gr-cli IN FRAME f-main /* Grupo do Cliente */
DO:
  {include/zoomvar.i &prog-zoom = adzoom/z01ad129.w
                     &campo     = param-pef.cod-gr-cli
                     &campozoom = cod-gr-cli}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-seq[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-seq[1] V-table-Win
ON LEAVE OF fi-seq[1] IN FRAME f-main
DO:
  ASSIGN SELF:SCREEN-VALUE = UPPER(SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-seq[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-seq[2] V-table-Win
ON LEAVE OF fi-seq[2] IN FRAME f-main
DO:
  ASSIGN SELF:SCREEN-VALUE = UPPER(SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-seq[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-seq[3] V-table-Win
ON LEAVE OF fi-seq[3] IN FRAME f-main
DO:
  ASSIGN SELF:SCREEN-VALUE = UPPER(SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-seq[4]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-seq[4] V-table-Win
ON LEAVE OF fi-seq[4] IN FRAME f-main
DO:
  ASSIGN SELF:SCREEN-VALUE = UPPER(SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-seq[5]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-seq[5] V-table-Win
ON LEAVE OF fi-seq[5] IN FRAME f-main
DO:
   ASSIGN SELF:SCREEN-VALUE = UPPER(SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-seq[6]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-seq[6] V-table-Win
ON LEAVE OF fi-seq[6] IN FRAME f-main
DO:
  ASSIGN SELF:SCREEN-VALUE = UPPER(SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME espec.param-pef.lote
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL espec.param-pef.lote V-table-Win
ON LEAVE OF espec.param-pef.lote IN FRAME f-main /* Lote */
DO:
  ASSIGN SELF:SCREEN-VALUE = UPPER(SELF:SCREEN-VALUE).
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
  param-pef.cod-gr-cli:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.

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
  {src/adm/template/row-list.i "espec.param-pef"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "espec.param-pef"}

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
  ASSIGN cb-tipo-cliente:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Normal"
         cb-cod-obsoleto:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Lancamento".

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
    
    /*:T Ponha na pi-validate todas as validaá‰es */
    /*:T N∆o gravar nada no registro antes do dispatch do assign-record e 
       nem na PI-validate. */
    
    RUN pi-validate.
    if RETURN-VALUE = 'ADM-ERROR':U then 
       RETURN 'ADM-ERROR':U.

    ASSIGN i-tipo-cliente = LOOKUP(INPUT FRAME {&FRAME-NAME} cb-tipo-cliente,c-tipos)
           c-cod-obsoleto = STRING((LOOKUP(INPUT FRAME {&FRAME-NAME} cb-cod-obsoleto,c-obsoleto) - 1),"9")
           c-lista-aux = "".
    DO i-ct = 1 TO EXTENT(fi-seq).
       IF c-lista-aux = "" THEN
          ASSIGN c-lista-aux = fi-seq[i-ct].
       ELSE DO.
          IF fi-seq[i-ct] <> ""  THEN
             ASSIGN c-lista-aux = c-lista-aux + "," + fi-seq[i-ct].
       END.
    END.

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    if RETURN-VALUE = 'ADM-ERROR':U then 
        RETURN 'ADM-ERROR':U.
    
    /*:T Todos os assignÔs n∆o feitos pelo assign-record devem ser feitos aqui */  
    /* Code placed here will execute AFTER standard behavior.    */

    ASSIGN param-pef.tipo-cliente = i-tipo-cliente
           param-pef.cod-obsoleto = c-cod-obsoleto
           param-pef.seq-qualid   = UPPER(c-lista-aux).


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
    DISABLE {&list-4} WITH FRAME {&FRAME-NAME}.
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
 IF AVAIL param-pef THEN DO:
    ASSIGN cb-tipo-cliente:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(param-pef.tipo-cliente,c-tipos)
           cb-cod-obsoleto:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(INT(param-pef.cod-obsoleto) + 1,c-obsoleto).
    FIND gr-cli WHERE gr-cli.cod-gr-cli = param-pef.cod-gr-cli NO-LOCK NO-ERROR.
    IF AVAIL gr-cli THEN
       ASSIGN fi-desc-grupo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = gr-cli.descricao.
    ASSIGN fi-seq = "".
    DO i-ct = 1 TO NUM-ENTRIES(param-pef.seq-qualid).
       IF ENTRY(i-ct,param-pef.seq-qualid) <> "" THEN
          ASSIGN fi-seq[i-ct] = ENTRY(i-ct,param-pef.seq-qualid).
    END.
    ASSIGN fi-seq[1]:SCREEN-VALUE = fi-seq[1]
           fi-seq[2]:SCREEN-VALUE = fi-seq[2]
           fi-seq[3]:SCREEN-VALUE = fi-seq[3]
           fi-seq[4]:SCREEN-VALUE = fi-seq[4]
           fi-seq[5]:SCREEN-VALUE = fi-seq[5]
           fi-seq[6]:SCREEN-VALUE = fi-seq[6].
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
    if adm-new-record = yes then
        enable {&ADM-MODIFY-FIELDS} with frame {&frame-name}.
    &endif
    ENABLE {&list-4} WITH FRAME {&FRAME-NAME}.
    APPLY 'entry' TO fi-seq.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize V-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  {esinc/i-dsallrb.i param-pef.tipo-cliente c-tipos}
  ASSIGN cb-tipo-cliente:LIST-ITEMS IN FRAME {&FRAME-NAME} = c-tipos
         cb-tipo-cliente:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Normal".

  {esinc/i-dsallrb.i param-pef.cod-obsoleto c-obsoleto}
  ASSIGN cb-cod-obsoleto:LIST-ITEMS IN FRAME {&FRAME-NAME} = c-obsoleto     
         cb-cod-obsoleto:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Lancamento".



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
  Notes: N∆o fazer assign aqui. Nesta procedure
  devem ser colocadas apenas validaá‰es, pois neste ponto do programa o registro 
  ainda n∆o foi criado.       
------------------------------------------------------------------------------*/
 {include/i-vldfrm.i} /*:T Validaá∆o de dicion†rio */
    
  ASSIGN INPUT FRAME {&FRAME-NAME} fi-seq[1]
         INPUT FRAME {&FRAME-NAME} fi-seq[2]
         INPUT FRAME {&FRAME-NAME} fi-seq[3]
         INPUT FRAME {&FRAME-NAME} fi-seq[4]
         INPUT FRAME {&FRAME-NAME} fi-seq[5]
         INPUT FRAME {&FRAME-NAME} fi-seq[6].

 FIND gr-cli WHERE gr-cli.cod-gr-cli = INPUT FRAME {&FRAME-NAME} param-pef.cod-gr-cli NO-LOCK NO-ERROR.
 IF NOT AVAIL gr-cli THEN DO:
    MESSAGE "O Grupo de Cliente n∆o foi encontrado ! ! "  VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    APPLY 'entry' TO param-pef.cod-gr-cli IN FRAME {&FRAME-NAME}.
    RETURN 'ADM-ERROR':U.                                                    
 END.

 IF LOOKUP(cb-tipo-cliente:SCREEN-VALUE , c-tipos)= 0 THEN DO. 
    MESSAGE "O Tipo do Cliente informado esta Incorreto ! ! !" VIEW-AS ALERT-BOX.
    APPLY 'entry' TO cb-tipo-cliente IN FRAME {&FRAME-NAME}.
    RETURN 'ADM-ERROR':U.
 END.

 IF param-pef.lote:SCREEN-VALUE <> "RP" AND param-pef.lote:SCREEN-VALUE <> "PP" AND 
    param-pef.lote:SCREEN-VALUE <> "RD" AND param-pef.lote:SCREEN-VALUE <> "PD" THEN DO:
    MESSAGE "Lotes permitidos 'RP' 'PP' 'RD' 'PD' ! ! !" VIEW-AS ALERT-BOX.
    APPLY 'entry' TO param-pef.lote IN FRAME {&FRAME-NAME}.
    RETURN 'ADM-ERROR':U.
 END.

 IF LOOKUP(cb-cod-obsoleto:SCREEN-VALUE, c-obsoleto)= 0 THEN DO. 
    MESSAGE "O Codigo Obsoleto informado esta Incorreto ! ! !" VIEW-AS ALERT-BOX.
    APPLY 'entry' TO cb-cod-obsoleto IN FRAME {&FRAME-NAME}.
    RETURN 'ADM-ERROR':U.
 END.

 IF fi-seq[1] + fi-seq[2] + fi-seq[3] + fi-seq[4] + fi-seq[5] + fi-seq[6] = ""  THEN DO:
    MESSAGE "Sequencia Qualidade n∆o foi informada  ! ! !" VIEW-AS ALERT-BOX.
    APPLY 'entry' TO fi-seq[1] IN FRAME {&FRAME-NAME}.
    RETURN 'ADM-ERROR':U.
 END.
 DO i-ct = 1 TO EXTENT(fi-seq).
    IF i-ct > 1 AND fi-seq[i-ct] <> "" THEN DO: 
       IF fi-seq[i-ct] <> "A" AND fi-seq[i-ct] <> "B" AND 
          fi-seq[i-ct] <> "C" AND fi-seq[i-ct] <> "D" AND fi-seq[i-ct] <> "R" THEN DO.
             MESSAGE "Sequencia Qualidade permitidas 'A'  'B'  'C'  'D'  'R' ! ! !" VIEW-AS ALERT-BOX.
             APPLY 'entry' TO fi-seq[1] IN FRAME {&FRAME-NAME}.
             RETURN 'ADM-ERROR':U.
       END.
    END.
 END.

 IF INPUT FRAME {&FRAME-NAME} param-pef.ate-500 = 0 THEN DO:
    MESSAGE "A Nuance atÇ 500 Metros  n∆o foi informada ! ! !" VIEW-AS ALERT-BOX. 
    APPLY 'entry' TO param-pef.ate-500 IN FRAME {&FRAME-NAME}.
    RETURN 'ADM-ERROR':U.
 END. 

 IF INPUT FRAME {&FRAME-NAME} param-pef.ate-1000 = 0 THEN DO:
    MESSAGE "A Nuance atÇ 1000 Metros  n∆o foi informada ! ! !" VIEW-AS ALERT-BOX. 
    APPLY 'entry' TO param-pef.ate-1000 IN FRAME {&FRAME-NAME}.
    RETURN 'ADM-ERROR':U.
 END. 

 IF INPUT FRAME {&FRAME-NAME} param-pef.aci-1000 = 0 THEN DO:
    MESSAGE "A Nuance Acima de 1000 Metros  n∆o foi informada ! ! !" VIEW-AS ALERT-BOX. 
    APPLY 'entry' TO param-pef.aci-1000 IN FRAME {&FRAME-NAME}.
    RETURN 'ADM-ERROR':U.
 END. 

 IF INPUT FRAME {&FRAME-NAME} param-pef.lim-inferior = 0 THEN DO:
    MESSAGE "O Limite Inferior n∆o foi informado ! ! !" VIEW-AS ALERT-BOX. 
    APPLY 'entry' TO param-pef.lim-inferior IN FRAME {&FRAME-NAME}.
    RETURN 'ADM-ERROR':U.
 END. 

 IF INPUT FRAME {&FRAME-NAME} param-pef.lim-superior = 0 THEN DO:
    MESSAGE "O Limite Superior n∆o foi informado ! ! !" VIEW-AS ALERT-BOX. 
    APPLY 'entry' TO param-pef.lim-superior IN FRAME {&FRAME-NAME}.
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
  {src/adm/template/snd-list.i "espec.param-pef"}

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

