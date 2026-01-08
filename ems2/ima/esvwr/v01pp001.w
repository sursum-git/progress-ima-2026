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
DEF VAR c-handle AS CHAR.

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
&Scoped-define EXTERNAL-TABLES pp-container
&Scoped-define FIRST-EXTERNAL-TABLE pp-container


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR pp-container.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS pp-container.dt-compra ~
pp-container.cod-emit-forn pp-container.comprador pp-container.ptax ~
pp-container.cod-depos 
&Scoped-define ENABLED-TABLES pp-container
&Scoped-define FIRST-ENABLED-TABLE pp-container
&Scoped-Define ENABLED-OBJECTS im-fecha rt-key rt-mold 
&Scoped-Define DISPLAYED-FIELDS pp-container.nr-container ~
pp-container.dt-compra pp-container.cod-estabel pp-container.cod-emit-forn ~
pp-container.comprador pp-container.num-pedido pp-container.dt-prev-chegada ~
pp-container.dt-recebimento pp-container.ptax pp-container.cod-depos 
&Scoped-define DISPLAYED-TABLES pp-container
&Scoped-define FIRST-DISPLAYED-TABLE pp-container
&Scoped-Define DISPLAYED-OBJECTS fi-nome-estab fi-nome-forn fi-perc-venda 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-CREATE-FIELDS pp-container.nr-container ~
pp-container.cod-estabel pp-container.dt-prev-chegada 
&Scoped-define ADM-ASSIGN-FIELDS pp-container.cod-estabel 

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
DEFINE BUTTON bt-perc-venda 
     IMAGE-UP FILE "image/repl-down.bmp":U
     LABEL "Button 3" 
     SIZE 4 BY 1.13 TOOLTIP "Replica % para Itens do Container".

DEFINE VARIABLE fi-nome-estab AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 47.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nome-forn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 62 BY .88 NO-UNDO.

DEFINE VARIABLE fi-perc-venda AS DECIMAL FORMAT ">>9.99":U INITIAL 0 
     LABEL "%Venda Geral" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE IMAGE im-fecha
     FILENAME "image/im-uchk1.bmp":U
     SIZE 8 BY 2.25 TOOLTIP "Indica se Container est† Aberto, Fechado ou Suspenso".

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 96 BY 2.5.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 96 BY 3.38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     pp-container.nr-container AT ROW 1.25 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.14 BY .88
     pp-container.dt-compra AT ROW 1.25 COL 62 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY .88
     pp-container.cod-estabel AT ROW 2.25 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5 BY .88
     fi-nome-estab AT ROW 2.25 COL 26.43 COLON-ALIGNED NO-LABEL
     pp-container.cod-emit-forn AT ROW 3.92 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.57 BY .88
     fi-nome-forn AT ROW 3.92 COL 31 COLON-ALIGNED NO-LABEL
     pp-container.comprador AT ROW 4.92 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.57 BY .88
     pp-container.num-pedido AT ROW 4.92 COL 42 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .88
     fi-perc-venda AT ROW 4.96 COL 81 COLON-ALIGNED WIDGET-ID 2
     bt-perc-venda AT ROW 4.83 COL 91.14 WIDGET-ID 4
     pp-container.dt-prev-chegada AT ROW 5.88 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.57 BY .88
     pp-container.dt-recebimento AT ROW 5.92 COL 42 COLON-ALIGNED
          LABEL "Recebimento"
          VIEW-AS FILL-IN 
          SIZE 11 BY .88
     pp-container.ptax AT ROW 6 COL 81 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 8.29 BY .88
     pp-container.cod-depos AT ROW 5.92 COL 62.86 COLON-ALIGNED WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 6 BY .88
     im-fecha AT ROW 1.25 COL 81.86
     rt-key AT ROW 1 COL 1
     rt-mold AT ROW 3.67 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: espec.pp-container
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
         HEIGHT             = 6.04
         WIDTH              = 96.57.
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

/* SETTINGS FOR BUTTON bt-perc-venda IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN pp-container.cod-estabel IN FRAME f-main
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN pp-container.dt-prev-chegada IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN pp-container.dt-recebimento IN FRAME f-main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN fi-nome-estab IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nome-forn IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-perc-venda IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN pp-container.nr-container IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN pp-container.num-pedido IN FRAME f-main
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

&Scoped-define SELF-NAME bt-perc-venda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-perc-venda V-table-Win
ON CHOOSE OF bt-perc-venda IN FRAME f-main /* Button 3 */
DO:
  ASSIGN INPUT FRAME {&FRAME-NAME} fi-perc-venda.

  IF VALID-HANDLE(WIDGET-HANDLE(c-handle)) THEN 
     RUN pi-replica-perc IN WIDGET-HANDLE(c-handle) (INPUT fi-perc-venda).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME pp-container.cod-depos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pp-container.cod-depos V-table-Win
ON MOUSE-SELECT-DBLCLICK OF pp-container.cod-depos IN FRAME f-main /* Dep¢sito */
DO:
  {include/zoomvar.i &prog-zoom=inzoom\z01in084.w
                     &campo=pp-container.cod-depos
                     &campozoom=cod-depos}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME pp-container.cod-emit-forn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pp-container.cod-emit-forn V-table-Win
ON ENTRY OF pp-container.cod-emit-forn IN FRAME f-main /* Fornecedor */
DO:
    IF  fi-nome-forn:SCREEN-VALUE IN FRAME {&FRAME-NAME} <> " " THEN DO:
        FIND emitente WHERE
                   emitente.identific <> 1 AND 
                   emitente.cod-emitente = INPUT FRAME {&FRAME-NAME} pp-container.cod-emit-forn NO-LOCK NO-ERROR.
        IF AVAIL emitente THEN
           ASSIGN fi-nome-forn:SCREEN-VALUE IN FRAME {&FRAME-NAME} = emitente.nome-emit.
        ELSE DO:
              ASSIGN fi-nome-forn:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Fornecedor n∆o Cadastrado".
              APPLY 'entry' TO pp-container.cod-emit-forn.
              RETURN NO-APPLY.
        END.
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pp-container.cod-emit-forn V-table-Win
ON LEAVE OF pp-container.cod-emit-forn IN FRAME f-main /* Fornecedor */
DO:
    FIND emitente WHERE
         emitente.identific <> 1 AND 
         emitente.cod-emitente = INPUT FRAME {&FRAME-NAME} pp-container.cod-emit-forn NO-LOCK NO-ERROR.
    IF AVAIL emitente THEN
       ASSIGN fi-nome-forn:SCREEN-VALUE IN FRAME {&FRAME-NAME} = emitente.nome-emit.
    ELSE DO:
       ASSIGN fi-nome-forn:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Fornecedor n∆o Cadastrado".
       APPLY 'entry' TO pp-container.cod-emit-forn.
       RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pp-container.cod-emit-forn V-table-Win
ON MOUSE-SELECT-DBLCLICK OF pp-container.cod-emit-forn IN FRAME f-main /* Fornecedor */
DO:
  {include/zoomvar.i &prog-zoom=adzoom\z01ad098.w
                     &campo=pp-container.cod-emit-forn
                     &campozoom=cod-emitente}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME pp-container.cod-estabel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pp-container.cod-estabel V-table-Win
ON ENTRY OF pp-container.cod-estabel IN FRAME f-main /* Estabelecimento */
DO:
    IF  fi-nome-estab:SCREEN-VALUE IN FRAME {&FRAME-NAME} <> " "THEN DO:
          FIND estabelec WHERE 
            estabelec.cod-estabel = pp-container.cod-estabel:SCREEN-VALUE IN FRAME {&FRAME-NAME} NO-LOCK NO-ERROR.
       IF AVAIL estabelec THEN
          ASSIGN fi-nome-estab:SCREEN-VALUE IN FRAME {&FRAME-NAME} = estabelec.nome.
       ELSE DO:
          ASSIGN fi-nome-estab:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Estabelecimento n∆o Cadastrado".
          APPLY 'entry' TO pp-container.cod-estabel.
          RETURN NO-APPLY.
       END.
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pp-container.cod-estabel V-table-Win
ON LEAVE OF pp-container.cod-estabel IN FRAME f-main /* Estabelecimento */
DO:
   FIND estabelec WHERE 
        estabelec.cod-estabel = pp-container.cod-estabel:SCREEN-VALUE IN FRAME {&FRAME-NAME} NO-LOCK NO-ERROR.
   IF AVAIL estabelec THEN
      ASSIGN fi-nome-estab:SCREEN-VALUE IN FRAME {&FRAME-NAME} = estabelec.nome.
   ELSE DO:
      ASSIGN fi-nome-estab:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Estabelecimento n∆o Cadastrado".
      APPLY 'entry' TO pp-container.cod-estabel.
      RETURN NO-APPLY.
   END.
   IF estabelec.estado = 'SC' THEN
      ASSIGN pp-container.cod-depos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'ITA'.
   ELSE
      ASSIGN pp-container.cod-depos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'ARM'.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pp-container.cod-estabel V-table-Win
ON MOUSE-SELECT-DBLCLICK OF pp-container.cod-estabel IN FRAME f-main /* Estabelecimento */
DO:
  {include/zoomvar.i &prog-zoom=adzoom\z01ad107.w
                     &campo=pp-container.cod-estabel
                     &campozoom=cod-estabel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME pp-container.comprador
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pp-container.comprador V-table-Win
ON ENTRY OF pp-container.comprador IN FRAME f-main /* Comprador */
DO:
  /*
  IF pp-container.comprador:SCREEN-VALUE IN FRAME {&FRAME-NAME} <> " " THEN DO:
      FIND comprador WHERE 
           comprador.cod-comprado = pp-container.comprador:SCREEN-VALUE IN FRAME {&FRAME-NAME} NO-LOCK NO-ERROR.
      IF AVAIL comprador THEN
          ASSIGN fi-nome-compr:SCREEN-VALUE IN FRAME {&FRAME-NAME} = comprador.nome.
      ELSE DO:
          ASSIGN fi-nome-compr:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Comprador N∆o Cadastrado".
          APPLY 'entry' TO pp-container.comprador.
          RETURN NO-APPLY.
      END.
   END.
   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pp-container.comprador V-table-Win
ON LEAVE OF pp-container.comprador IN FRAME f-main /* Comprador */
DO:
  /*
  FIND comprador WHERE 
       comprador.cod-comprado = pp-container.comprador:SCREEN-VALUE IN FRAME {&FRAME-NAME} NO-LOCK NO-ERROR.
  IF AVAIL comprador THEN
      ASSIGN fi-nome-compr:SCREEN-VALUE IN FRAME {&FRAME-NAME} = comprador.nome.
  ELSE DO:
      ASSIGN fi-nome-compr:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Comprador N∆o Cadastrado".
      APPLY 'entry' TO pp-container.comprador.
      RETURN NO-APPLY.
  END.
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pp-container.comprador V-table-Win
ON MOUSE-SELECT-DBLCLICK OF pp-container.comprador IN FRAME f-main /* Comprador */
DO:
  {include/zoomvar.i &prog-zoom=inzoom\z01in055.w
                     &campo=pp-container.comprador
                     &campozoom=cod-comprado}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pp-container.comprador V-table-Win
ON SELECTION OF pp-container.comprador IN FRAME f-main /* Comprador */
DO:
  MESSAGE "Selection"
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME pp-container.dt-prev-chegada
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pp-container.dt-prev-chegada V-table-Win
ON LEAVE OF pp-container.dt-prev-chegada IN FRAME f-main /* Data Prev Chegada */
DO:
   /*
   IF SELF:INPUT-VALUE < TODAY THEN DO.
      MESSAGE 'Data de Chegada n∆o pode ser menor que Hoje...'
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
   END.
   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME pp-container.nr-container
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pp-container.nr-container V-table-Win
ON LEAVE OF pp-container.nr-container IN FRAME f-main /* Container */
DO:
  IF pp-container.dt-compra:SCREEN-VALUE IN FRAME {&FRAME-NAME} = " " THEN
     ASSIGN pp-container.dt-compra:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY) .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
pp-container.cod-estabel:LOAD-MOUSE-POINTER("image/lupa.cur").
pp-container.cod-emit-forn:LOAD-MOUSE-POINTER("image/lupa.cur").
pp-container.comprador:LOAD-MOUSE-POINTER("image/lupa.cur").
pp-container.cod-depos:LOAD-MOUSE-POINTER("image/lupa.cur").

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

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
  {src/adm/template/row-list.i "pp-container"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "pp-container"}

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

  ASSIGN pp-container.dt-compra:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY,"99/99/9999").

  IF VALID-HANDLE(WIDGET-HANDLE(c-handle)) THEN 
     RUN pi-popula-browse IN WIDGET-HANDLE(c-handle) (INPUT ?).

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
    IF RETURN-VALUE = 'ADM-ERROR':U THEN 
        RETURN 'ADM-ERROR':U.
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    IF RETURN-VALUE = 'ADM-ERROR':U THEN 
        RETURN 'ADM-ERROR':U.
    
    /*:T Todos os assignÔs n∆o feitos pelo assign-record devem ser feitos aqui */  
    /* Code placed here will execute AFTER standard behavior.    */
    RUN pi-grava-itens IN WIDGET-HANDLE(c-handle) (INPUT ROWID(pp-container)).

    FIND emitente WHERE 
         emitente.cod-emitente = INT (pp-container.cod-emit-forn:SCREEN-VALUE IN FRAME {&FRAME-NAME}) 
         NO-LOCK NO-ERROR.
    IF AVAIL emitente THEN
       ASSIGN pp-container.nome-ab-forn = emitente.nome-abrev.

    IF adm-new-record OR pp-container.situacao = 2 THEN
       ASSIGN pp-container.situacao = 1.

    IF pp-container.dt-prev-chegada <> ? THEN DO.
       FOR EACH ped-venda-ext WHERE
                ped-venda-ext.nr-container = pp-container.nr-container NO-LOCK.
           FIND ped-venda WHERE
                ped-venda.nr-pedido = ped-venda-ext.nr-pedido SHARE-LOCK NO-ERROR.
           IF ped-venda.dt-entrega < pp-container.dt-prev-chegada THEN
              ASSIGN ped-venda.dt-entrega = pp-container.dt-prev-chegada.
       END.
    END.
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
    
    ASSIGN fi-perc-venda:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           bt-perc-venda:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
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
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .
    
    /* Code placed here will execute AFTER standard behavior.    */
    IF AVAIL pp-container THEN DO.
       IF VALID-HANDLE(WIDGET-HANDLE(c-handle)) AND NOT adm-new-record THEN 
          RUN pi-popula-browse IN WIDGET-HANDLE(c-handle) (INPUT ROWID(pp-container)).

       RUN pi-change-image.

       FIND estabelec WHERE 
            estabelec.cod-estabel = pp-container.cod-estabel:SCREEN-VALUE IN FRAME {&FRAME-NAME} NO-LOCK NO-ERROR.
       IF AVAIL estabelec THEN
          ASSIGN fi-nome-estab:SCREEN-VALUE IN FRAME {&FRAME-NAME} = estabelec.nome.
       ELSE
          ASSIGN fi-nome-estab:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
    
       FIND emitente WHERE
            emitente.identific <> 1 AND 
            emitente.cod-emitente = INPUT FRAME {&FRAME-NAME} pp-container.cod-emit-forn NO-LOCK NO-ERROR.
       IF AVAIL emitente THEN
          ASSIGN fi-nome-forn:SCREEN-VALUE IN FRAME {&FRAME-NAME} = emitente.nome-emit.
       ELSE DO:
          ASSIGN fi-nome-forn:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Fornecedor n∆o Cadastrado".
       END.
            
       /*
       FIND comprador WHERE 
            comprador.cod-comprado = pp-container.comprador:SCREEN-VALUE IN FRAME {&FRAME-NAME} NO-LOCK NO-ERROR.
       IF AVAIL comprador THEN
          ASSIGN fi-nome-compr:SCREEN-VALUE IN FRAME {&FRAME-NAME} = comprador.nome.
       ELSE 
          ASSIGN fi-nome-compr:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Comprador N∆o Cadastrado".
       */
       ASSIGN fi-perc-venda:SCREEN-VALUE = '0'.
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

    IF adm-new-record = NO THEN
       ASSIGN fi-perc-venda:SENSITIVE IN FRAME {&FRAME-NAME} = YES
              bt-perc-venda:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

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
  RUN get-link-handle IN adm-broker-hdl (INPUT THIS-PROCEDURE,
                                         INPUT "container",
                                         OUTPUT c-handle).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-busca-container V-table-Win 
PROCEDURE pi-busca-container :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER  p-container AS INT.
    ASSIGN p-container = INT(pp-container.nr-container:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
    RETURN STRING(ROWID(pp-container)).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-change-image V-table-Win 
PROCEDURE pi-change-image :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    CASE pp-container.situacao.
        WHEN 1 THEN im-fecha:LOAD-IMAGE("image/im-chk1.bmp") IN FRAME {&FRAME-NAME}.
        WHEN 2 THEN im-fecha:LOAD-IMAGE("image/im-uchk2.bmp") IN FRAME {&FRAME-NAME}.
        WHEN 3 THEN im-fecha:LOAD-IMAGE("image/im-uchk1.bmp") IN FRAME {&FRAME-NAME}.
    END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-fornec V-table-Win 
PROCEDURE pi-fornec :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER p-cod-fornec AS INTEGER NO-UNDO.


ASSIGN pp-container.cod-emit-forn:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(p-cod-fornec)
       pp-container.cod-estabel:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "1".

APPLY "entry" TO pp-container.cod-estabel.
APPLY "leave" TO pp-container.cod-estabel.
APPLY "entry" TO pp-container.cod-emit-forn.
APPLY "leave" TO pp-container.cod-emit-forn.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-validate V-table-Win 
PROCEDURE pi-validate :
/*:T------------------------------------------------------------------------------
  Purpose:Validar a viewer     
  Parameters:  <none>
  Notes: N∆o fazer assign aqui. Nesta procedure
  devem ser colocadas apenas validaá‰es, pois neste ponto do programa o registro 
  ainda n∆o foi criado.       
------------------------------------------------------------------------------*/
    {include/i-vldfrm.i} /*:T Validaá∆o de dicion†rio */
    
    IF INPUT FRAME {&FRAME-NAME} pp-container.nr-container = 0 THEN DO.
       MESSAGE 'N£mero do Container n∆o Informado...'
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
       APPLY 'entry' TO pp-container.nr-container.
       RETURN 'ADM-ERROR':U.                                            
    END.

    IF INPUT FRAME {&FRAME-NAME} pp-container.cod-estabel = '' THEN DO.
       MESSAGE 'Estabelecimento n∆o Informado...'
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
       APPLY 'entry' TO pp-container.cod-estabel.
       RETURN 'ADM-ERROR':U.                                            
    END.

    IF INPUT FRAME {&FRAME-NAME} pp-container.cod-depos <> '' THEN DO.
       FIND deposito WHERE
            deposito.cod-depos = INPUT FRAME {&FRAME-NAME} pp-container.cod-depos
            NO-LOCK NO-ERROR.
       IF NOT AVAIL deposito THEN DO.
          MESSAGE 'Dep¢sito n∆o Cadastrado...' SKIP
                  'Verifique programa CD0601'
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
          APPLY 'entry' TO pp-container.cod-depos.
          RETURN 'ADM-ERROR':U.                                            
       END.
    END.

    /*
    IF INPUT FRAME {&FRAME-NAME} pp-container.dt-prev-chegada < TODAY THEN DO.
      MESSAGE 'Data de Chegada n∆o pode ser menor que Hoje...'
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY 'entry' TO pp-container.dt-prev-chegada.
      RETURN 'ADM-ERROR':U.                                            
    END.
    */

    RUN pi-valida-itens IN WIDGET-HANDLE(c-handle).
    IF RETURN-VALUE = 'ADM-ERROR' THEN DO.
       MESSAGE 'N∆o foi Informado Itens para esse Container...' SKIP(1)
               'Confirma Inclus∆o ?'
           VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE l-confirma AS LOGICAL.

       IF NOT l-confirma THEN DO.
          APPLY 'entry' TO pp-container.nr-container.
          RETURN 'ADM-ERROR':U.  
       END.
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
  {src/adm/template/snd-list.i "pp-container"}

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

