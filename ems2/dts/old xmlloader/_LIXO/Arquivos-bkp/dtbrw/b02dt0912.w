&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r11 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          xmlloader        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*:T *******************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i B02DT0912 2.06.00.002}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEF VAR cDescItemEMS AS CHAR NO-UNDO.
/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_table

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES dt-docum-est
&Scoped-define FIRST-EXTERNAL-TABLE dt-docum-est


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR dt-docum-est.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES dt-it-docum-est

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table dt-it-docum-est.sequencia dt-it-docum-est.it-codigo dt-it-docum-est.char-2 dt-it-docum-est.item-ems fnDescItem(dt-it-docum-est.item-ems) @ cDescItemEMS dt-it-docum-est.qt-do-forn dt-it-docum-est.un dt-it-docum-est.preco-total dt-it-docum-est.nat-operacao   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table   
&Scoped-define SELF-NAME br_table
&Scoped-define QUERY-STRING-br_table FOR EACH dt-it-docum-est                             WHERE dt-it-docum-est.nro-docto    = dt-docum-est.nro-docto                               AND dt-it-docum-est.serie-docto  = dt-docum-est.serie-docto                               AND dt-it-docum-est.cod-emitente = dt-docum-est.cod-emitente                               /*AND dt-it-docum-est.nat-operacao = dt-docum-est.nat-operacao */                               /* AND dt-it-docum-est.nome-arq     = dt-docum-est.nome-arq */       NO-LOCK BY dt-it-docum-est.sequenci      INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br_table OPEN QUERY {&SELF-NAME} FOR EACH dt-it-docum-est                             WHERE dt-it-docum-est.nro-docto    = dt-docum-est.nro-docto                               AND dt-it-docum-est.serie-docto  = dt-docum-est.serie-docto                               AND dt-it-docum-est.cod-emitente = dt-docum-est.cod-emitente                               /*AND dt-it-docum-est.nat-operacao = dt-docum-est.nat-operacao */                               /* AND dt-it-docum-est.nome-arq     = dt-docum-est.nome-arq */       NO-LOCK BY dt-it-docum-est.sequenci      INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br_table dt-it-docum-est
&Scoped-define FIRST-TABLE-IN-QUERY-br_table dt-it-docum-est


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br_table 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" B-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS
><EXECUTING-CODE>
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Advanced Query Options" B-table-Win _INLINE
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
</FILTER-ATTRIBUTES> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnDescItem B-table-Win 
FUNCTION fnDescItem RETURNS CHARACTER
  ( INPUT pItemEMS AS CHAR /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      dt-it-docum-est SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _FREEFORM
  QUERY br_table NO-LOCK DISPLAY
      dt-it-docum-est.sequencia     COLUMN-LABEL "Seq" WIDTH 3.50
      dt-it-docum-est.it-codigo     COLUMN-LABEL "Item Documento"
      dt-it-docum-est.char-2        COLUMN-LABEL "Descri‡Æo" FORMAT "x(60)":U WIDTH 30.00
      dt-it-docum-est.item-ems      COLUMN-LABEL "Item EMS" 
      fnDescItem(dt-it-docum-est.item-ems) @ cDescItemEMS COLUMN-LABEL "Descri‡Æo" FORMAT "x(60)":U WIDTH 30.00
      dt-it-docum-est.qt-do-forn    FORMAT ">>>>,>>>,>>9.9999" COLUMN-LABEL "Qtde"
      dt-it-docum-est.un            COLUMN-LABEL "Un"
      dt-it-docum-est.preco-total   COLUMN-LABEL "Vl Total"
      dt-it-docum-est.nat-operacao  COLUMN-LABEL "Nat Opera‡Æo"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 112 BY 7.25.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   External Tables: xmlloader.dt-docum-est
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
         HEIGHT             = 7.33
         WIDTH              = 112.29.
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB br_table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH dt-it-docum-est
                            WHERE dt-it-docum-est.nro-docto    = dt-docum-est.nro-docto
                              AND dt-it-docum-est.serie-docto  = dt-docum-est.serie-docto
                              AND dt-it-docum-est.cod-emitente = dt-docum-est.cod-emitente
                              /*AND dt-it-docum-est.nat-operacao = dt-docum-est.nat-operacao */
                              /* AND dt-it-docum-est.nome-arq     = dt-docum-est.nome-arq */

     NO-LOCK BY dt-it-docum-est.sequenci
     INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION KEY-PHRASE SORTBY-PHRASE"
     _JoinCode[1]      = "xmlloader.dt-it-docum-est.cod-estabel = xmlloader.dt-docum-est.cod-estabel
  AND xmlloader.dt-it-docum-est.cod-emitente = xmlloader.dt-docum-est.cod-emitente
  AND xmlloader.dt-it-docum-est.serie-docto = xmlloader.dt-docum-est.serie-docto
  AND xmlloader.dt-it-docum-est.nro-docto = xmlloader.dt-docum-est.nro-docto"
     _Where[1]         = "xmlloader.dt-it-docum-est.cod-estabel = xmlloader.dt-docum-est.cod-estabel
  AND xmlloader.dt-it-docum-est.cod-emitente = xmlloader.dt-docum-est.cod-emitente
  AND xmlloader.dt-it-docum-est.serie-docto = xmlloader.dt-docum-est.serie-docto
  AND xmlloader.dt-it-docum-est.nro-docto = xmlloader.dt-docum-est.nro-docto"
     _Query            is NOT OPENED
*/  /* BROWSE br_table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br_table
&Scoped-define SELF-NAME br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-DISPLAY OF br_table IN FRAME F-Main
DO:
    IF dt-it-docum-est.log-1   THEN
        ASSIGN dt-it-docum-est.sequencia    :bgcolor in browse br_table = 10 
               dt-it-docum-est.it-codigo    :bgcolor in browse br_table = 10 
               dt-it-docum-est.char-2       :bgcolor in browse br_table = 10 
               dt-it-docum-est.item-ems     :bgcolor in browse br_table = 10 
               cDescItemEMS                 :bgcolor in browse br_table = 10 
               dt-it-docum-est.qt-do-forn   :bgcolor in browse br_table = 10 
               dt-it-docum-est.un           :bgcolor in browse br_table = 10 
               dt-it-docum-est.preco-total  :bgcolor in browse br_table = 10
               dt-it-docum-est.nat-operacao :bgcolor in browse br_table = 10.
    ELSE 
        ASSIGN dt-it-docum-est.sequencia    :bgcolor in browse br_table = ? 
               dt-it-docum-est.it-codigo    :bgcolor in browse br_table = ? 
               dt-it-docum-est.char-2       :bgcolor in browse br_table = ? 
               dt-it-docum-est.item-ems     :bgcolor in browse br_table = ? 
               cDescItemEMS                 :bgcolor in browse br_table = ? 
               dt-it-docum-est.qt-do-forn   :bgcolor in browse br_table = ? 
               dt-it-docum-est.un           :bgcolor in browse br_table = ? 
               dt-it-docum-est.preco-total  :bgcolor in browse br_table = ?
               dt-it-docum-est.nat-operacao :bgcolor in browse br_table = ?.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-ENTRY OF br_table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-LEAVE OF br_table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON VALUE-CHANGED OF br_table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  {src/adm/template/row-list.i "dt-docum-est"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "dt-docum-est"}

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
  {src/adm/template/snd-list.i "dt-docum-est"}
  {src/adm/template/snd-list.i "dt-it-docum-est"}

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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnDescItem B-table-Win 
FUNCTION fnDescItem RETURNS CHARACTER
  ( INPUT pItemEMS AS CHAR /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE BUFFER bItemEMS FOR ITEM.

  FIND FIRST bItemEMS
      WHERE bItemEMS.it-codigo = pItemEMS NO-LOCK NO-ERROR.
  IF AVAIL bItemEMS THEN
      RETURN bItemEMS.desc-item.
  ELSE
    RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

