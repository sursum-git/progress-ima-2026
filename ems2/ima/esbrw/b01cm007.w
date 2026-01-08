&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          ems2cad          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i B99XX999 9.99.99.999}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

DEF BUFFER unid-feder FOR ems2cad.unid-feder.

/* ***************************  Definitions  ************************** */
&Scop adm-attribute-dlg support/browserd.w

/* Parameters Definitions ---                                           */
/* Local Variable Definitions ---                                       */

/*:T Variaveis usadas internamente pelo estilo, favor nao elimina-las     */

/*:T v ri veis de uso globla */
def new global shared var v-row-parent as rowid no-undo.

/*:T vari veis de uso local */
def var v-row-table  as rowid.

/*:T fim das variaveis utilizadas no estilo */

DEF BUFFER b-cm-ext-repres FOR cm-ext-repres.

DEF TEMP-TABLE tt-cm-unid-feder LIKE cm-unid-feder LABEL "tt-cm-unid-feder"
    FIELD no-estado LIKE ems2cad.unid-feder.no-estado.
    
DEF TEMP-TABLE tt-uf LIKE ems2cad.unid-feder LABEL "tt-uf".

DEF VAR i-ct AS INT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-table

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES repres
&Scoped-define FIRST-EXTERNAL-TABLE repres


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR repres.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-uf tt-cm-unid-feder

/* Definitions for BROWSE br-table                                      */
&Scoped-define FIELDS-IN-QUERY-br-table tt-uf.estado tt-uf.no-estado   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-table   
&Scoped-define SELF-NAME br-table
&Scoped-define QUERY-STRING-br-table FOR EACH tt-uf NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-table OPEN QUERY {&SELF-NAME} FOR EACH tt-uf NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-table tt-uf
&Scoped-define FIRST-TABLE-IN-QUERY-br-table tt-uf


/* Definitions for BROWSE br-uf                                         */
&Scoped-define FIELDS-IN-QUERY-br-uf tt-cm-unid-feder.uf tt-cm-unid-feder.no-estado tt-cm-unid-feder.perc-comis   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-uf tt-cm-unid-feder.perc-comis   
&Scoped-define ENABLED-TABLES-IN-QUERY-br-uf tt-cm-unid-feder
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br-uf tt-cm-unid-feder
&Scoped-define SELF-NAME br-uf
&Scoped-define QUERY-STRING-br-uf FOR EACH tt-cm-unid-feder OF repres NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-uf OPEN QUERY {&SELF-NAME} FOR EACH tt-cm-unid-feder OF repres NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-uf tt-cm-unid-feder
&Scoped-define FIRST-TABLE-IN-QUERY-br-uf tt-cm-unid-feder


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS repres.log-1 
&Scoped-define ENABLED-TABLES repres
&Scoped-define FIRST-ENABLED-TABLE repres
&Scoped-Define ENABLED-OBJECTS br-table br-uf 
&Scoped-Define DISPLAYED-FIELDS repres.log-1 
&Scoped-define DISPLAYED-TABLES repres
&Scoped-define FIRST-DISPLAYED-TABLE repres


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-5 bt-add bt-del 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" B-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<FOREIGN-KEYS>
cod-rep||y|espec.cm-exc-item.cod-rep
it-codigo||y|espec.cm-exc-item.it-codigo
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "cod-rep,it-codigo"':U).

/* Tell the ADM to use the OPEN-QUERY-CASES. */
&Scoped-define OPEN-QUERY-CASES RUN dispatch ('open-query-cases':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Advanced Query Optionsososos" B-table-Win _INLINE
/* Actions: ? adm/support/advqedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<SORTBY-OPTIONS>
</SORTBY-OPTIONS>
<SORTBY-RUN-CODE>
************************
* Set attributes related to SORTBY-OPTIONS */
RUN set-attribute-list (
    'SortBy-Options = ""':U).
/************************
</SORTBY-RUN-CODE>
<FILTER-ATTRIBUTES>
************************
* Initialize Filter Attributes */
RUN set-attribute-list IN THIS-PROCEDURE ('
  Filter-Value=':U).
/************************
</FILTER-ATTRIBUTES> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-add 
     IMAGE-UP FILE "adeicon\next-au":U
     IMAGE-INSENSITIVE FILE "adeicon\next-ai":U
     LABEL "" 
     SIZE 7 BY 1.

DEFINE BUTTON bt-del 
     IMAGE-UP FILE "adeicon\prev-au":U
     IMAGE-INSENSITIVE FILE "adeicon\prev-ai":U
     LABEL "" 
     SIZE 7 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-table FOR 
      tt-uf SCROLLING.

DEFINE QUERY br-uf FOR 
      tt-cm-unid-feder SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-table B-table-Win _FREEFORM
  QUERY br-table NO-LOCK DISPLAY
      tt-uf.estado     FORMAT "x(2)"   WIDTH 5    
      tt-uf.no-estado  FORMAT "x(30)"  WIDTH 30
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS MULTIPLE SIZE 40 BY 8.08
         FONT 1.

DEFINE BROWSE br-uf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-uf B-table-Win _FREEFORM
  QUERY br-uf NO-LOCK DISPLAY
      tt-cm-unid-feder.uf           FORMAT "x(2)"  WIDTH 5
      tt-cm-unid-feder.no-estado    FORMAT "x(30)" WIDTH 28
      tt-cm-unid-feder.perc-comis                  WIDTH 6
ENABLE
      tt-cm-unid-feder.perc-comis
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 45 BY 8.08
         FONT 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br-table AT ROW 1 COL 1
     repres.log-1 AT ROW 8.75 COL 39.57 COLON-ALIGNED NO-LABEL WIDGET-ID 38
          VIEW-AS FILL-IN 
          SIZE 1 BY .25
     bt-add AT ROW 4 COL 41.29 WIDGET-ID 34
     bt-del AT ROW 5.25 COL 41.29 WIDGET-ID 36
     br-uf AT ROW 1 COL 49 WIDGET-ID 200
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 FONT 1 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   External Tables: mgcom.repres
   Allow: Basic,Browse
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
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 8.21
         WIDTH              = 94.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{include/c-browse.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit Custom                            */
/* BROWSE-TAB br-table 1 F-Main */
/* BROWSE-TAB br-uf bt-del F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON bt-add IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR BUTTON bt-del IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-table
/* Query rebuild information for BROWSE br-table
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-uf NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is NOT OPENED
*/  /* BROWSE br-table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-uf
/* Query rebuild information for BROWSE br-uf
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-cm-unid-feder OF repres NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is NOT OPENED
*/  /* BROWSE br-uf */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br-table
&Scoped-define SELF-NAME br-table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table B-table-Win
ON VALUE-CHANGED OF br-table IN FRAME F-Main
DO:
  //ASSIGN bt-add:SENSITIVE = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-uf
&Scoped-define SELF-NAME br-uf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-uf B-table-Win
ON ROW-LEAVE OF br-uf IN FRAME F-Main
DO:
   ASSIGN INPUT BROWSE br-uf tt-cm-unid-feder.perc-comis.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-uf B-table-Win
ON VALUE-CHANGED OF br-uf IN FRAME F-Main
DO:
    /*
   ASSIGN fi-it-codigo = ""
          fi-perc-menor-tab = 0
          fi-perc-maior-tab = 0.

   DISABLE {&list-5} WITH FRAME {&FRAME-NAME}.

   IF NUM-RESULTS("br-table":U) > 0 THEN DO.
      ASSIGN fi-it-codigo = tt-cm-unid-feder.it-codigo
             fi-perc-menor-tab = tt-cm-unid-feder.perc-menor-tab
             fi-perc-maior-tab = tt-cm-unid-feder.perc-maior-tab
             ed-desc-item = tt-cm-unid-feder.desc-item.

      IF bt-inc:SENSITIVE THEN
         ENABLE {&list-5} WITH FRAME {&FRAME-NAME}.
   END.

   DISP fi-it-codigo 
        ed-desc-item
        fi-perc-menor-tab 
        fi-perc-maior-tab 
        WITH FRAME {&FRAME-NAME}.
        */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-add B-table-Win
ON CHOOSE OF bt-add IN FRAME F-Main
DO:
   DO i-ct = 1 TO br-table:NUM-SELECTED-ROWS.
      IF br-table:FETCH-SELECTED-ROW(i-ct) THEN DO.
         CREATE tt-cm-unid-feder.
         ASSIGN tt-cm-unid-feder.cod-rep = repres.cod-rep
                tt-cm-unid-feder.uf = tt-uf.estado
                tt-cm-unid-feder.no-estado = tt-uf.no-estado.

         DELETE tt-uf.
      END.
   END.
   {&OPEN-QUERY-br-table}
   {&OPEN-QUERY-br-uf}
   
   APPLY 'ENTRY' TO tt-cm-unid-feder.uf IN BROWSE br-uf.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-del
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-del B-table-Win
ON CHOOSE OF bt-del IN FRAME F-Main
DO:
   DO i-ct = 1 TO br-uf:NUM-SELECTED-ROWS.
      IF br-uf:FETCH-SELECTED-ROW(i-ct) THEN DO.
         CREATE tt-uf.
         ASSIGN tt-uf.estado = tt-cm-unid-feder.uf
                tt-uf.no-estado = tt-cm-unid-feder.no-estado.

         DELETE tt-cm-unid-feder.
      END.
   END.
   {&OPEN-QUERY-br-uf}
   {&OPEN-QUERY-br-table}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-table
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-open-query-cases B-table-Win  adm/support/_adm-opn.p
PROCEDURE adm-open-query-cases :
/*------------------------------------------------------------------------------
  Purpose:     Opens different cases of the query based on attributes
               such as the 'Key-Name', or 'SortBy-Case'
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* No Foreign keys are accepted by this SmartObject. */

  {&OPEN-QUERY-{&BROWSE-NAME}}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available B-table-Win  _ADM-ROW-AVAILABLE
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
  {src/adm/template/row-list.i "repres"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "repres"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI B-table-Win  _DEFAULT-DISABLE
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
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record B-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    
    FOR EACH cm-unid-feder OF repres EXCLUSIVE-LOCK.
        DELETE cm-unid-feder.
    END.

    FOR EACH tt-cm-unid-feder NO-LOCK.
        CREATE cm-unid-feder.
        BUFFER-COPY tt-cm-unid-feder TO cm-unid-feder.
    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-disable-fields B-table-Win 
PROCEDURE local-disable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */


  /* Code placed here will execute AFTER standard behavior.    */
  
  DISABLE {&list-5} WITH FRAME {&FRAME-NAME}.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields B-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  /* Code placed here will execute AFTER standard behavior.    */
  
  IF NUM-RESULTS("br-table":U) > 0 THEN 
     ENABLE {&list-5} WITH FRAME {&FRAME-NAME}.
     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-open-query-cases B-table-Win 
PROCEDURE local-open-query-cases :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
/* Code placed here will execute PRIOR to standard behavior. */

  EMPTY TEMP-TABLE tt-uf.
  EMPTY TEMP-TABLE tt-cm-unid-feder.

  FOR EACH unid-feder WHERE 
           unid-feder.pais = 'Brasil' NO-LOCK. 
           
      FIND cm-unid-feder WHERE
           cm-unid-feder.cod-rep = repres.cod-rep AND
           cm-unid-feder.uf = unid-feder.estado
           NO-LOCK NO-ERROR.
      IF AVAIL cm-unid-feder THEN NEXT.
      
      CREATE tt-uf.
      BUFFER-COPY unid-feder TO tt-uf.
  END.

  FOR EACH cm-unid-feder OF repres NO-LOCK.
      FIND unid-feder WHERE
           unid-feder.estado = cm-unid-feder.uf
           NO-LOCK NO-ERROR.
      IF NOT AVAIL unid-feder THEN NEXT.

      CREATE tt-cm-unid-feder.
      BUFFER-COPY cm-unid-feder TO tt-cm-unid-feder.

      ASSIGN tt-cm-unid-feder.no-estado = unid-feder.no-estado.
  END.
  
  ASSIGN repres.log-1:SENSITIVE IN FRAME {&FRAME-NAME} = NO
         repres.log-1:HIDDEN = YES.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query-cases':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {&OPEN-QUERY-br-table}
  {&OPEN-QUERY-br-uf}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-view B-table-Win 
PROCEDURE local-view :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'view':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  apply 'value-changed':U to {&browse-name} in frame {&frame-name}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-validate B-table-Win 
PROCEDURE pi-validate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*:T------------------------------------------------------------------------------
  Purpose:Validar a viewer     
  Parameters:  <none>
  Notes: NÆo fazer assign aqui. Nesta procedure
  devem ser colocadas apenas valida‡äes, pois neste ponto do programa o registro 
  ainda nÆo foi criado.       
------------------------------------------------------------------------------*/
/*    {include/i-vldfrm.i} /*:T Valida‡Æo de dicion rio */*/
    
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key B-table-Win  adm/support/_key-snd.p
PROCEDURE send-key :
/*------------------------------------------------------------------------------
  Purpose:     Sends a requested KEY value back to the calling
               SmartObject.
  Parameters:  <see adm/template/sndkytop.i>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/sndkytop.i}

  /* Return the key value associated with each key case.             */
  {src/adm/template/sndkycas.i "cod-rep" "cm-exc-item" "cod-rep"}
  {src/adm/template/sndkycas.i "it-codigo" "cm-exc-item" "it-codigo"}

  /* Close the CASE statement and end the procedure.                 */
  {src/adm/template/sndkyend.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records B-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "repres"}
  {src/adm/template/snd-list.i "tt-cm-unid-feder"}
  {src/adm/template/snd-list.i "tt-uf"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed B-table-Win 
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
      {src/adm/template/bstates.i}
  END CASE.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

