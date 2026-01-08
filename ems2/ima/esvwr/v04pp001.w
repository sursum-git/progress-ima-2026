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
DEF NEW GLOBAL SHARED VAR h-espp005 AS HANDLE NO-UNDO.

/* Parameters Definitions ---                                           */
DEF BUFFER moeda FOR mgcad.moeda.

/* Local Variable Definitions ---                                       */
DEF VAR v-row-parent AS ROWID NO-UNDO.
DEF VAR c-handle AS CHAR.



/* Variaveis para Pedido de Venda ---                                   */
DEF TEMP-TABLE tt-ped-venda NO-UNDO LIKE ped-venda
     FIELD r-rowid AS ROWID.

DEF TEMP-TABLE tt-ped-item NO-UNDO LIKE ped-item 
    FIELD r-rowid AS ROWID.
DEF TEMP-TABLE wt-ped-item NO-UNDO LIKE ped-item 
    FIELD r-rowid AS ROWID.

DEF TEMP-TABLE tt-ped-repre NO-UNDO LIKE ped-repre
    FIELD r-rowid AS ROWID.
DEF TEMP-TABLE wt-ped-repre NO-UNDO LIKE ped-repre
    FIELD r-rowid AS ROWID.

DEF TEMP-TABLE tt-cond-ped NO-UNDO LIKE cond-ped
    FIELD r-rowid AS ROWID.
DEF TEMP-TABLE wt-cond-ped NO-UNDO LIKE cond-ped
    FIELD r-rowid AS ROWID.

/* Variaveis do Recebimento Fiscal ---                                  */
DEF TEMP-TABLE tt-docum-est NO-UNDO LIKE docum-est 
    FIELD r-rowid AS ROWID.

DEF TEMP-TABLE tt-item-doc-est NO-UNDO LIKE item-doc-est
    FIELD r-rowid AS ROWID.
DEF TEMP-TABLE wt-item-doc-est NO-UNDO LIKE item-doc-est
    FIELD r-rowid AS ROWID.

DEF TEMP-TABLE tt-rat-lote NO-UNDO LIKE rat-lote
    FIELD r-rowid AS ROWID.
DEF TEMP-TABLE wt-rat-lote NO-UNDO LIKE rat-lote
    FIELD r-rowid AS ROWID.

DEF TEMP-TABLE tt-dupli-apagar NO-UNDO LIKE dupli-apagar
    FIELD r-rowid AS ROWID.

DEF NEW GLOBAL SHARED VAR gr-docum-est AS ROWID NO-UNDO.
DEF VAR c-natur-oper  LIKE natur-oper.nat-operacao INIT '31201'.
DEF VAR i-qtd-parc    AS INT INIT 1.
DEF VAR i-dup         AS INT.
DEF VAR i-nr-seq      AS INT.
DEF VAR de-tot-peso   AS DEC FORMAT ">>>>,>>>,>>9.9999".
DEF VAR de-tot-valor  AS DEC.
DEF VAR de-tot-rateio AS DEC.
DEF VAR de-dif        AS DEC.
DEF VAR de-tot-qtde   AS DEC.
DEF VAR de-vl-apagar  AS DEC.
DEF VAR de-tot-despes AS DEC.
DEF VAR l-erro        AS LOG.
DEF VAR c-arq-pedidos AS CHAR.

DEF VAR i-ct-ped      AS INT.

DEF VAR c-arq-conv  as char no-undo.
DEF VAR l-ok as logical no-undo.

DEF BUFFER b-it-container FOR pp-it-container.

/* Temp-Table tt-erro Definitions ---*/
{method/dbotterr.i}

DEF VAR h-boin090      AS HANDLE.
DEF VAR h-boin092      AS HANDLE.
DEF VAR h-boin176      AS HANDLE.
DEF VAR h-boin367      AS HANDLE.
DEF VAR h-bodi018      AS HANDLE.
DEF VAR h-bodi154      AS HANDLE.
DEF VAR h-bodi157      AS HANDLE.
DEF VAR h-bodi159      AS HANDLE.
DEF VAR h-bodi159com   AS HANDLE.
DEF VAR h-bodi154sdf   AS HANDLE.

DEF VAR h-acomp AS HANDLE NO-UNDO.
RUN utp/ut-acomp.p PERSISTENT SET h-acomp.

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
pp-container.cod-estabel pp-container.cod-emit-forn ~
pp-container.dt-prev-chegada 
&Scoped-define ENABLED-TABLES pp-container
&Scoped-define FIRST-ENABLED-TABLE pp-container
&Scoped-Define ENABLED-OBJECTS im-fecha rt-key rt-mold 
&Scoped-Define DISPLAYED-FIELDS pp-container.nr-container ~
pp-container.dt-compra pp-container.cod-estabel pp-container.cod-emit-forn ~
pp-container.dt-prev-chegada 
&Scoped-define DISPLAYED-TABLES pp-container
&Scoped-define FIRST-DISPLAYED-TABLE pp-container
&Scoped-Define DISPLAYED-OBJECTS fi-nome-estab fi-nome-forn ~
fi-dt-recebimento 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-CREATE-FIELDS pp-container.nr-container 

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
DEFINE VARIABLE fi-dt-recebimento AS DATE FORMAT "99/99/9999" 
     LABEL "Data de Recebimento" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88.

DEFINE VARIABLE fi-nome-estab AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 47.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nome-forn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 43 BY .88 NO-UNDO.

DEFINE IMAGE im-fecha
     FILENAME "image/im-uchk1.bmp":U
     SIZE 8 BY 2.25 TOOLTIP "Indica se Container est† Aberto, Fechado ou Suspenso".

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88 BY 2.5.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88 BY 2.33.


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
     pp-container.cod-emit-forn AT ROW 3.79 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.57 BY .88
     fi-nome-forn AT ROW 3.79 COL 31 COLON-ALIGNED NO-LABEL
     pp-container.dt-prev-chegada AT ROW 4.79 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.57 BY .88
     fi-dt-recebimento AT ROW 4.79 COL 63 COLON-ALIGNED
     im-fecha AT ROW 1.25 COL 80
     rt-key AT ROW 1 COL 1
     rt-mold AT ROW 3.58 COL 1
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
         HEIGHT             = 5.04
         WIDTH              = 88.29.
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

/* SETTINGS FOR FILL-IN fi-dt-recebimento IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nome-estab IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nome-forn IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN pp-container.nr-container IN FRAME f-main
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


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

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

    ASSIGN pp-container.situacao = 1.
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
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .
    
    /* Code placed here will execute AFTER standard behavior.    */
    IF AVAIL pp-container THEN DO.
       IF VALID-HANDLE(WIDGET-HANDLE(c-handle)) THEN 
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
       ELSE
          ASSIGN fi-nome-forn:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Fornecedor n∆o Cadastrado".

       FIND docum-est WHERE
            docum-est.declaracao-import = STRING(pp-container.nr-container)
            NO-LOCK NO-ERROR.

       ASSIGN fi-dt-recebimento:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ''.
       IF AVAIL docum-est THEN
          ASSIGN fi-dt-recebimento:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(docum-est.dt-trans).
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

  ASSIGN c-arq-pedidos = SESSION:TEMP-DIRECTORY + "pedidos.txt".

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cria-pedvenda V-table-Win 
PROCEDURE pi-cria-pedvenda :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
    IF NOT VALID-HANDLE(h-bodi018) OR 
       h-bodi018:TYPE      <> "PROCEDURE":U OR
       h-bodi018:FILE-NAME <> "dibo/bodi018.p":U THEN
       RUN dibo/bodi018.p PERSISTENT SET h-bodi018.

    IF NOT VALID-HANDLE(h-bodi154) OR 
       h-bodi154:TYPE      <> "PROCEDURE":U OR
       h-bodi154:FILE-NAME <> "dibo/bodi154.p":U THEN
       RUN dibo/bodi154.p PERSISTENT SET h-bodi154.

    IF NOT VALID-HANDLE(h-bodi157) OR 
       h-bodi157:TYPE      <> "PROCEDURE":U OR
       h-bodi157:FILE-NAME <> "dibo/bodi157.p":U THEN
       RUN dibo/bodi157.p PERSISTENT SET h-bodi157.

    IF NOT VALID-HANDLE(h-bodi159) OR
       h-bodi159:TYPE      <> "PROCEDURE":U OR
       h-bodi159:FILE-NAME <> "dibo/bodi159.p":U THEN
       RUN dibo/bodi159.p PERSISTENT SET h-bodi159.

    IF NOT VALID-HANDLE(h-bodi159com) OR
       h-bodi159com:TYPE      <> "PROCEDURE":U OR
       h-bodi159com:FILE-NAME <> "dibo/bodi159com.p":U THEN
       RUN dibo/bodi159com.p PERSISTENT SET h-bodi159com.

    OUTPUT TO VALUE(c-arq-pedidos).
        {esinc/i-espp005.i}
    OUTPUT CLOSE.

    RUN utp/ut-utils.p PERSISTENT SET h-prog.
    RUN EXECUTE IN h-prog(INPUT "notepad.exe", 
                          INPUT c-arq-pedidos).
    DELETE PROCEDURE h-prog.


    IF VALID-HANDLE(h-bodi018) THEN
       DELETE PROCEDURE h-bodi018.

    IF VALID-HANDLE(h-bodi154) THEN
       DELETE PROCEDURE h-bodi154.

    IF VALID-HANDLE(h-bodi157) THEN
       DELETE PROCEDURE h-bodi157.

    IF VALID-HANDLE(h-bodi159) THEN
       DELETE PROCEDURE h-bodi159.

    IF VALID-HANDLE(h-bodi159com) THEN
       DELETE PROCEDURE h-bodi159com.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cria-recto V-table-Win 
PROCEDURE pi-cria-recto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF NOT VALID-HANDLE(h-boin090) OR
       h-boin090:TYPE      <> "PROCEDURE":U OR
       h-boin090:FILE-NAME <> "inbo/boin090.p":U THEN
       RUN inbo/boin090.p PERSISTENT SET h-boin090.
    
    IF NOT VALID-HANDLE(h-boin092) OR
       h-boin092:TYPE      <> "PROCEDURE":U OR
       h-boin092:FILE-NAME <> "inbo/boin092.p":U THEN
       RUN inbo/boin092.p PERSISTENT SET h-boin092.
    
    IF NOT VALID-HANDLE(h-boin367) OR
       h-boin367:TYPE      <> "PROCEDURE":U OR
       h-boin367:FILE-NAME <> "inbo/boin367.p":U THEN
       RUN inbo/boin367.p PERSISTENT SET h-boin367.

    IF NOT VALID-HANDLE(h-boin176) OR 
       h-boin176:TYPE      <> "PROCEDURE":U OR
       h-boin176:FILE-NAME <> "inbo/boin176.p":U THEN
       RUN inbo/boin176.p PERSISTENT SET h-boin176.

    EMPTY TEMP-TABLE tt-docum-est.
    EMPTY TEMP-TABLE tt-item-doc-est.
    EMPTY TEMP-TABLE tt-rat-lote.
    EMPTY TEMP-TABLE tt-dupli-apagar. 

    FIND estabelec WHERE
         estabelec.cod-estabel = pp-container.cod-estabel NO-LOCK NO-ERROR.

    FIND ser-estab WHERE
         ser-estab.serie = estabelec.serie AND
         ser-estab.cod-estabel = pp-container.cod-estabel NO-LOCK NO-ERROR.

    /* Cria Temp-Table para o Recebimento*/
    CREATE tt-docum-est.
    ASSIGN tt-docum-est.cod-emit = pp-container.cod-emit
           tt-docum-est.serie-docto = ser-estab.serie
           tt-docum-est.nro-docto = STRING(INT(ser-estab.nr-ult-fat) + 1,"9999999")
           tt-docum-est.nat-operacao = c-natur-oper
           tt-docum-est.cod-estabel = ser-estab.cod-estabel
           tt-docum-est.declaracao-import = STRING(pp-container.nr-container)
           tt-docum-est.valor-frete = pp-container.vlr-frete
           tt-docum-est.valor-outras = pp-container.vlr-despesas
           tt-docum-est.valor-seguro = pp-container.vlr-seguro
           tt-docum-est.dt-emissao = TODAY
           tt-docum-est.dt-trans = TODAY
           tt-docum-est.char-1 = tt-docum-est.nro-docto.
           /*tt-docum-est.char-1 = STRING(TODAY,"99/99/9999") + STRING(TIME,"HH:MM") + 'DIGITAÄ«O RµPIDA'.*/

    ASSIGN i-nr-seq = 0
           de-tot-despes = pp-container.vlr-frete + pp-container.vlr-seguro + pp-container.vlr-despesas.
    FOR EACH pp-it-container WHERE 
             pp-it-container.nr-container =  pp-container.nr-container AND
             pp-it-container.qt-recebida > 0 EXCLUSIVE-LOCK
             BREAK BY pp-it-container.it-codigo.

        IF FIRST-OF(pp-it-container.it-codigo) THEN DO.
           ASSIGN de-dif = 0
                  de-tot-qtde = 0
                  de-tot-rateio = 0
                  de-tot-peso = 0.
           FOR EACH b-it-container WHERE
                    b-it-container.nr-container = pp-it-container.nr-container AND
                    b-it-container.it-comprado = pp-it-container.it-comprado NO-LOCK.
               ASSIGN de-tot-qtde = de-tot-qtde + b-it-container.qt-recebida.
           END.
           FIND item WHERE
                item.it-codigo = pp-it-container.it-codigo NO-LOCK NO-ERROR.

           ASSIGN de-tot-valor = de-tot-valor + pp-it-container.preco-compra.
        END.
        ASSIGN i-nr-seq = i-nr-seq + 10.

        /* Cria os Itens do Recebimento */
        CREATE tt-item-doc-est.
        ASSIGN tt-item-doc-est.nro-docto      = tt-docum-est.nro-docto
               tt-item-doc-est.serie-docto    = tt-docum-est.serie-docto
               tt-item-doc-est.cod-emitente   = tt-docum-est.cod-emit
               tt-item-doc-est.nat-operacao   = tt-docum-est.nat-operacao
               tt-item-doc-est.sequencia      = i-nr-seq
               tt-item-doc-est.it-codigo      = pp-it-container.it-codigo
               tt-item-doc-est.cod-refer      = pp-it-container.cod-refer
               tt-item-doc-est.lote           = pp-it-container.cod-refer
               tt-item-doc-est.qt-do-forn     = pp-it-container.qt-recebida
               tt-item-doc-est.preco-total[1] = (pp-it-container.preco-compra / de-tot-qtde) * pp-it-container.qt-recebida
               tt-item-doc-est.preco-unit[1]  = tt-item-doc-est.preco-total[1] / pp-it-container.qt-recebida
               tt-item-doc-est.despesas       = de-tot-despes * pp-it-container.qt-recebida / de-tot-qtde
               tt-item-doc-est.pr-total-cmi   = pp-container.vlr-frete * pp-it-container.qt-recebida / de-tot-qtde
               /*tt-item-doc-est.peso-liquido   = (pp-it-container.qt-recebida * ITEM.peso-liquido)
               tt-item-doc-est.peso-bruto     = tt-item-doc-est.peso-liquido */
               tt-item-doc-est.cod-depos      = 'ARM'
               tt-item-doc-est.lote           = pp-it-container.cod-refer
               tt-item-doc-est.dt-vali-lote   = 12.31.9999
               tt-item-doc-est.narrativa      = pp-it-container.narrativa.
        
        ASSIGN de-tot-rateio = de-tot-rateio + tt-item-doc-est.preco-total[1]
               de-tot-peso = de-tot-peso + (pp-it-container.qt-recebida * ITEM.peso-liquido).

        CREATE tt-rat-lote.
        ASSIGN tt-rat-lote.nro-docto        = tt-docum-est.nro-docto
               tt-rat-lote.serie-docto      = tt-docum-est.serie-docto
               tt-rat-lote.cod-emitente     = tt-docum-est.cod-emit
               tt-rat-lote.nat-operacao     = tt-docum-est.nat-operacao
               tt-rat-lote.sequencia        = i-nr-seq
               tt-rat-lote.it-codigo        = pp-it-container.it-codigo
               tt-rat-lote.cod-refer        = pp-it-container.cod-refer
               tt-rat-lote.lote             = pp-it-container.cod-refer
               tt-rat-lote.cod-depos        = 'ARM'
               tt-rat-lote.dt-vali-lote     = 12.31.9999
               tt-rat-lote.quantidade       = pp-it-container.qt-recebida.

        IF LAST-OF(pp-it-container.it-codigo) THEN DO.
           ASSIGN de-dif = pp-it-container.preco-compra - de-tot-rateio.
        
           IF de-dif <> 0 THEN DO.
              IF de-dif >= 5 THEN DO.
                 MESSAGE "Diferenáa" de-dif " Superior a 5,00" SKIP
                         "Deseja Continuar ?"
                         VIEW-AS ALERT-BOX QUESTION  BUTTONS YES-NO
                         TITLE "" UPDATE l-continua AS LOGICAL.
                 IF NOT l-continua THEN
                    RETURN 'ADM-ERROR'.
              END.
              ASSIGN pp-it-container.preco-compra = pp-it-container.preco-compra + de-dif.
           END.
        END.
    END.

    ASSIGN tt-docum-est.tot-valor = de-tot-valor 
           /*
           tt-docum-est.tot-peso = de-tot-peso
           tt-docum-est.peso-bruto-tot = de-tot-peso
           SUBSTRING(tt-docum-est.char-1,157,17) = string(de-tot-peso,">>>>,>>>,>>9.9999") /*Peso Bruto*/
           tt-docum-est.peso-liquido-tot = de-tot-peso */
           tt-docum-est.valor-mercad = de-tot-valor
           tt-docum-est.base-ipi = de-tot-rateio
           tt-docum-est.base-icm = de-tot-valor + de-tot-despes
           tt-docum-est.despesa-nota = de-tot-despes.
    

    /* Cria Parcelas Ö Pagar */
    ASSIGN de-vl-apagar = tt-docum-est.tot-valor / i-qtd-parc.
    DO i-dup = 1 TO i-qtd-parc.
       CREATE tt-dupli-apagar.
       ASSIGN tt-dupli-apagar.serie-docto = tt-docum-est.serie-docto
              tt-dupli-apagar.nro-docto = tt-docum-est.nro-docto
              tt-dupli-apagar.cod-emitente = tt-docum-est.cod-emit
              tt-dupli-apagar.nat-operacao = tt-docum-est.nat-operacao
              tt-dupli-apagar.parcela = STRING(i-dup,"99")
              tt-dupli-apagar.cod-estabel = tt-docum-est.cod-estabel
              tt-dupli-apagar.cod-esp = 'DP'
              tt-dupli-apagar.nr-duplic = tt-docum-est.nro-docto
              tt-dupli-apagar.dt-emissao = TODAY
              tt-dupli-apagar.dt-trans = TODAY
              tt-dupli-apagar.dt-vencim = TODAY + 2
              tt-dupli-apagar.esp-movto = 1
              tt-dupli-apagar.tp-despesa = 5
              tt-dupli-apagar.ep-codigo = '1' 
              tt-dupli-apagar.vl-a-pagar = tt-docum-est.tot-valor. 
    END.

    /* Cria Recebimento Fiscal */
    RUN openQueryStatic IN h-boin090 (INPUT "Main":U).
    RUN emptyRowErrors IN h-boin090.
    RUN setRecord IN h-boin090 (INPUT TABLE tt-docum-est).
    RUN createRecord IN h-boin090.
    RUN getRowErrors IN h-boin090 (OUTPUT TABLE RowErrors).
    IF CAN-FIND(FIRST RowErrors 
               WHERE RowErrors.ErrorSubType = "ERROR":U  ) THEN DO:
       FOR EACH rowerrors WHERE
                RowErrors.ErrorSubType = "ERROR":U:
           MESSAGE "Erro ao Gerar o Recebimento: " tt-docum-est.nro-docto SKIP
                    rowerrors.errornumber
                    rowerrors.errordescription 
                   VIEW-AS ALERT-BOX.
       END.
       RETURN 'ADM-ERROR'.
    END.

    /* Cria Itens do Recebimento*/
    RUN openQueryStatic IN h-boin176 (INPUT "Main":U).
    RUN emptyRowErrors IN h-boin176.

    FOR EACH tt-item-doc-est:
        EMPTY TEMP-TABLE wt-item-doc-est.
        CREATE wt-item-doc-est.
        BUFFER-COPY tt-item-doc-est TO wt-item-doc-est.

        RUN setRecord IN h-boin176 (INPUT TABLE wt-item-doc-est).
        RUN createRecord IN h-boin176.
    END.
    RUN getRowErrors IN h-boin176 (OUTPUT TABLE RowErrors).
    IF CAN-FIND(FIRST RowErrors 
               WHERE RowErrors.ErrorSubType = "ERROR":U  ) THEN DO:
       FOR EACH rowerrors WHERE
                RowErrors.ErrorSubType = "ERROR":U:
           MESSAGE "Erro ao Gerar os Itens do Recebimento" SKIP
                    rowerrors.errordescription 
                   VIEW-AS ALERT-BOX.
       END.
       RETURN 'ADM-ERROR'.
    END.

    /* Cria Lotes dos Itens do Recebimento*/
    RUN openQueryStatic IN h-boin367 (INPUT "Main":U).
    RUN emptyRowErrors IN h-boin367.

    FOR EACH tt-rat-lote:
        EMPTY TEMP-TABLE wt-rat-lote.
        CREATE wt-rat-lote.
        BUFFER-COPY tt-rat-lote TO wt-rat-lote.

        RUN setRecord IN h-boin367 (INPUT TABLE wt-rat-lote).
        RUN createRecord IN h-boin367.
    END.
    RUN getRowErrors IN h-boin367 (OUTPUT TABLE RowErrors).
    IF CAN-FIND(FIRST RowErrors 
               WHERE RowErrors.ErrorSubType = "ERROR":U  ) THEN DO:
       FOR EACH rowerrors WHERE
                RowErrors.ErrorSubType = "ERROR":U:
           MESSAGE "Erro ao Gerar os Lotes dos Itens do Recebimento" SKIP
                    rowerrors.errordescription 
                   VIEW-AS ALERT-BOX.
       END.
       RETURN 'ADM-ERROR'.
    END.

    /* Cria Documentos Ö Pagar */
    RUN openQueryStatic IN h-boin092 (INPUT "Main":U).
    RUN emptyRowErrors IN h-boin092.
    RUN setRecord IN h-boin092 (INPUT TABLE tt-dupli-apagar).
    RUN createRecord IN h-boin092.
    RUN getRowErrors IN h-boin092 (OUTPUT TABLE RowErrors).
    IF CAN-FIND(FIRST RowErrors 
               WHERE RowErrors.ErrorSubType = "ERROR":U  ) THEN DO:
       FOR EACH rowerrors WHERE
                RowErrors.ErrorSubType = "ERROR":U:
           MESSAGE "Erro ao Gerar Duplicatas a Pagar do Recebimento" SKIP
                    rowerrors.errordescription 
                   VIEW-AS ALERT-BOX.
       END.
       RETURN 'ADM-ERROR'.
    END.

    FIND docum-est WHERE
         docum-est.serie-docto = tt-docum-est.serie AND
         docum-est.nro-docto = tt-docum-est.nro-docto AND
         docum-est.cod-emit = tt-docum-est.cod-emit AND
         docum-est.nat-operacao = tt-docum-est.nat-operacao
         NO-LOCK NO-ERROR.

    IF VALID-HANDLE(h-boin090) THEN
       DELETE PROCEDURE h-boin090.
    
    IF VALID-HANDLE(h-boin092) THEN
       DELETE PROCEDURE h-boin092.
    
    IF VALID-HANDLE(h-boin367) THEN
       DELETE PROCEDURE h-boin176.

    IF VALID-HANDLE(h-boin176) THEN
       DELETE PROCEDURE h-boin176.
              
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-fecha-container V-table-Win 
PROCEDURE pi-fecha-container :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER p-sit-receb AS LOG.
    DEF VAR l-erro AS LOG INIT NO.

    IF NOT CAN-FIND(FIRST pp-it-container OF pp-container)  THEN DO.
       MESSAGE 'N∆o Encontrado Itens para o Container...'
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
       ASSIGN l-erro = YES.
    END.

    IF p-sit-receb = NO THEN DO:
       FOR EACH pp-it-container OF pp-container NO-LOCK.
           FIND ITEM WHERE
                ITEM.it-codigo = pp-it-container.it-codigo NO-LOCK NO-ERROR.
           IF NOT AVAIL ITEM THEN DO.
              MESSAGE 'Item ' pp-it-container.it-codigo 'N∆o Cadastrado no Sistema...'
                  VIEW-AS ALERT-BOX ERROR BUTTONS OK.
              ASSIGN l-erro = YES.
           END.
  
           FIND referencia WHERE
                referencia.cod-refer = pp-it-container.cod-refer NO-LOCK NO-ERROR.
           IF NOT AVAIL referencia THEN DO.
              MESSAGE 'Referància ' pp-it-container.cod-refer 'N∆o Cadastrada no Sistema...'
                  VIEW-AS ALERT-BOX ERROR BUTTONS OK.
              ASSIGN l-erro = YES.
           END.
  
           FIND ref-item WHERE
                ref-item.cod-refer = pp-it-container.cod-refer AND
                ref-item.it-codigo = pp-it-container.it-codigo NO-LOCK NO-ERROR.
           IF NOT AVAIL ref-item THEN DO.
              MESSAGE "Referància " pp-it-container.cod-refer
                      "n∆o Vinculada ao Item " pp-it-container.it-codigo
                  VIEW-AS ALERT-BOX ERROR BUTTONS OK.
              ASSIGN l-erro = YES.
           END.
  
           IF pp-it-container.qt-recebida = 0 THEN DO.
              MESSAGE "Item:" pp-it-container.it-codigo 
                      "Referància:" pp-it-container.cod-refer SKIP
                      "Quantidade Recebida do Item n∆o foi Informada..."
                  VIEW-AS ALERT-BOX ERROR BUTTONS OK.
              ASSIGN l-erro = YES.
           END.
       END.

       IF l-erro THEN
          RETURN 'ADM-ERROR'. 
    END.

    FIND docum-est WHERE
         docum-est.declaracao-import = STRING(pp-container.nr-container)
         NO-LOCK NO-ERROR.
    
    IF NOT AVAIL docum-est THEN DO.
       RUN pi-cria-recto.
       IF RETURN-VALUE = 'ADM-ERROR' THEN
          RETURN 'ADM-ERROR'.
    END.

    IF AVAIL docum-est AND NOT docum-est.ce-atual THEN DO.
       ASSIGN gr-docum-est = ROWID(docum-est).

       RUN rep/re1001.w.

    END.

    FIND CURRENT pp-container NO-ERROR.
    ASSIGN pp-container.situacao = 3.

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
    
/*:T    Segue um exemplo de validaá∆o de programa */
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

