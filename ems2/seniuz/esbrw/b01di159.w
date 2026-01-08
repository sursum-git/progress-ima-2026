&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          mgcad            PROGRESS
          mgmov            PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i B01DI159 2.04.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
&Scop adm-attribute-dlg support/browserd.w

/* Parameters Definitions ---                                           */
 
/* Local Variable Definitions ---                                       */

/* Variaveis usadas internamente pelo estilo, favor nao elimina-las     */

/* v ri veis de uso globla */
def new global shared var v-row-parent as rowid no-undo.

DEF NEW GLOBAL SHARED VAR gr-ped-venda AS ROWID NO-UNDO.

DEFINE NEW GLOBAL SHARED VAR fi-ini-nr-pedcli  LIKE ped-item-ext.nr-pedcli.
DEFINE NEW GLOBAL SHARED VAR fi-fin-nr-pedcli  LIKE ped-item-ext.nr-pedcli.
DEFINE NEW GLOBAL SHARED VAR fi-ini-dt-entrega LIKE ped-item.dt-entrega.
DEFINE NEW GLOBAL SHARED VAR fi-fin-dt-entrega LIKE ped-item.dt-entrega.
DEFINE NEW GLOBAL SHARED VAR to-abe AS LOG.
DEFINE NEW GLOBAL SHARED VAR to-atp AS LOG.
DEFINE NEW GLOBAL SHARED VAR to-att AS LOG.
DEFINE NEW GLOBAL SHARED VAR to-pen AS LOG.
DEFINE NEW GLOBAL SHARED VAR to-sus AS LOG.
DEFINE NEW GLOBAL SHARED VAR to-can AS LOG.
DEFINE NEW GLOBAL SHARED VAR to-bal AS LOG.

/* vari veis de uso local */
def var v-row-table  as rowid.

DEF VAR c-situacao AS CHAR FORMAT "x(20)".
DEF VAR c-sit-nota AS CHAR FORMAT "x(20)".
DEF VAR c-ult-nota LIKE nota-fiscal.nr-nota-fis.
DEF VAR da-emi-nota LIKE nota-fiscal.dt-emis-nota.
DEF VAR da-sai-nota LIKE nota-fiscal.dt-saida.

/* fim das variaveis utilizadas no estilo */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-table

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES emitente
&Scoped-define FIRST-EXTERNAL-TABLE emitente


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR emitente.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ped-venda

/* Definitions for BROWSE br-table                                      */
&Scoped-define FIELDS-IN-QUERY-br-table ped-venda.nr-pedcli ~
ped-venda.nr-pedrep ped-venda.no-ab-reppri fn-situacao() @ c-situacao ~
ped-venda.dt-entrega fn-ult-nota() @ c-ult-nota fn-sit-nota() @ c-sit-nota ~
fn-dt-emi-nota() @ da-emi-nota fn-dt-sai-nota() @ da-sai-nota 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-table 
&Scoped-define QUERY-STRING-br-table FOR EACH ped-venda WHERE ped-venda.cod-emitente = emitente.cod-emitente ~
      AND ped-venda.nr-pedcli >= fi-ini-nr-pedcli ~
 AND ped-venda.nr-pedcli <= fi-fin-nr-pedcli ~
 and ped-venda.dt-entrega >= fi-ini-dt-entrega ~
 and ped-venda.dt-entrega <= fi-fin-dt-entrega ~
 AND (ped-venda.cod-sit-ped = 1 and to-abe = yes or ~
      ped-venda.cod-sit-ped = 2 and to-atp = yes or ~
      ped-venda.cod-sit-ped = 3 and to-att = yes or ~
      ped-venda.cod-sit-ped = 4 and to-pen = yes or ~
      ped-venda.cod-sit-ped = 5 and to-sus = yes or ~
      ped-venda.cod-sit-ped = 6 and to-can = yes or ~
      ped-venda.cod-sit-ped = 7 and to-bal = yes) ~
 NO-LOCK ~
    BY ped-venda.dt-entrega DESCENDING INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-table OPEN QUERY br-table FOR EACH ped-venda WHERE ped-venda.cod-emitente = emitente.cod-emitente ~
      AND ped-venda.nr-pedcli >= fi-ini-nr-pedcli ~
 AND ped-venda.nr-pedcli <= fi-fin-nr-pedcli ~
 and ped-venda.dt-entrega >= fi-ini-dt-entrega ~
 and ped-venda.dt-entrega <= fi-fin-dt-entrega ~
 AND (ped-venda.cod-sit-ped = 1 and to-abe = yes or ~
      ped-venda.cod-sit-ped = 2 and to-atp = yes or ~
      ped-venda.cod-sit-ped = 3 and to-att = yes or ~
      ped-venda.cod-sit-ped = 4 and to-pen = yes or ~
      ped-venda.cod-sit-ped = 5 and to-sus = yes or ~
      ped-venda.cod-sit-ped = 6 and to-can = yes or ~
      ped-venda.cod-sit-ped = 7 and to-bal = yes) ~
 NO-LOCK ~
    BY ped-venda.dt-entrega DESCENDING INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-table ped-venda
&Scoped-define FIRST-TABLE-IN-QUERY-br-table ped-venda


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-table bt-idet br-param 

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
nr-pedido||y|mgmov.ped-venda.nr-pedido
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "nr-pedido"':U).

/* Tell the ADM to use the OPEN-QUERY-CASES. */
&Scoped-define OPEN-QUERY-CASES RUN dispatch ('open-query-cases':U).
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
************************
* Initialize Filter Attributes */
RUN set-attribute-list IN THIS-PROCEDURE ('
  Filter-Value=':U).
/************************
</FILTER-ATTRIBUTES> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-dt-emi-nota B-table-Win 
FUNCTION fn-dt-emi-nota RETURNS DATE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-dt-sai-nota B-table-Win 
FUNCTION fn-dt-sai-nota RETURNS DATE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-sit-nota B-table-Win 
FUNCTION fn-sit-nota RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-situacao B-table-Win 
FUNCTION fn-situacao RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-ult-nota B-table-Win 
FUNCTION fn-ult-nota RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON br-param 
     IMAGE-UP FILE "image/im-fil.bmp":U
     LABEL "&Parƒmetros" 
     SIZE 4.86 BY 1 TOOLTIP "Parƒmetros".

DEFINE BUTTON bt-idet 
     LABEL "&Detalhar" 
     SIZE 10 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-table FOR 
      ped-venda SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-table B-table-Win _STRUCTURED
  QUERY br-table NO-LOCK DISPLAY
      ped-venda.nr-pedcli COLUMN-LABEL "Ped Cliente" FORMAT "x(12)":U
            WIDTH 8.43
      ped-venda.nr-pedrep COLUMN-LABEL "Ped Repres" FORMAT "x(12)":U
            WIDTH 8.43
      ped-venda.no-ab-reppri FORMAT "x(12)":U WIDTH 13.43
      fn-situacao() @ c-situacao COLUMN-LABEL "Situa‡Æo" WIDTH 10.43
      ped-venda.dt-entrega COLUMN-LABEL "Dt.Entrega" FORMAT "99/99/9999":U
      fn-ult-nota() @ c-ult-nota COLUMN-LABEL "éltima NF" WIDTH 6.5
      fn-sit-nota() @ c-sit-nota COLUMN-LABEL "Situa‡Æo NF" WIDTH 8
      fn-dt-emi-nota() @ da-emi-nota COLUMN-LABEL "Dt.EmissÆo"
      fn-dt-sai-nota() @ da-sai-nota COLUMN-LABEL "Dt.Sa¡da"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 89.57 BY 7
         FONT 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br-table AT ROW 1 COL 1
     bt-idet AT ROW 8.08 COL 1
     br-param AT ROW 8.08 COL 11.72
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   External Tables: mgcad.emitente
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
         HEIGHT             = 8.08
         WIDTH              = 89.86.
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
   NOT-VISIBLE Size-to-Fit L-To-R                                       */
/* BROWSE-TAB br-table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-table
/* Query rebuild information for BROWSE br-table
     _TblList          = "mgmov.ped-venda WHERE mgcad.emitente <external> ..."
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _TblOptList       = ", LAST OUTER"
     _OrdList          = "mgmov.ped-venda.dt-entrega|no"
     _JoinCode[1]      = "mgmov.ped-venda.cod-emitente = mgcad.emitente.cod-emitente"
     _Where[1]         = "mgmov.ped-venda.nr-pedcli >= fi-ini-nr-pedcli
 AND mgmov.ped-venda.nr-pedcli <= fi-fin-nr-pedcli
 and mgmov.ped-venda.dt-entrega >= fi-ini-dt-entrega
 and mgmov.ped-venda.dt-entrega <= fi-fin-dt-entrega
 AND (mgmov.ped-venda.cod-sit-ped = 1 and to-abe = yes or
      mgmov.ped-venda.cod-sit-ped = 2 and to-atp = yes or
      mgmov.ped-venda.cod-sit-ped = 3 and to-att = yes or
      mgmov.ped-venda.cod-sit-ped = 4 and to-pen = yes or
      mgmov.ped-venda.cod-sit-ped = 5 and to-sus = yes or
      mgmov.ped-venda.cod-sit-ped = 6 and to-can = yes or
      mgmov.ped-venda.cod-sit-ped = 7 and to-bal = yes)
"
     _FldNameList[1]   > mgmov.ped-venda.nr-pedcli
"mgmov.ped-venda.nr-pedcli" "Ped Cliente" ? "character" ? ? ? ? ? ? no ? no no "8.43" yes no no "U" "" ""
     _FldNameList[2]   > mgmov.ped-venda.nr-pedrep
"mgmov.ped-venda.nr-pedrep" "Ped Repres" ? "character" ? ? ? ? ? ? no ? no no "8.43" yes no no "U" "" ""
     _FldNameList[3]   > mgmov.ped-venda.no-ab-reppri
"mgmov.ped-venda.no-ab-reppri" ? ? "character" ? ? ? ? ? ? no ? no no "13.43" yes no no "U" "" ""
     _FldNameList[4]   > "_<CALC>"
"fn-situacao() @ c-situacao" "Situa‡Æo" ? ? ? ? ? ? ? ? no ? no no "10.43" yes no no "U" "" ""
     _FldNameList[5]   > mgmov.ped-venda.dt-entrega
"mgmov.ped-venda.dt-entrega" "Dt.Entrega" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > "_<CALC>"
"fn-ult-nota() @ c-ult-nota" "éltima NF" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[7]   > "_<CALC>"
"fn-sit-nota() @ c-sit-nota" "Situa‡Æo NF" ? ? ? ? ? ? ? ? no ? no no "8" yes no no "U" "" ""
     _FldNameList[8]   > "_<CALC>"
"fn-dt-emi-nota() @ da-emi-nota" "Dt.EmissÆo" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[9]   > "_<CALC>"
"fn-dt-sai-nota() @ da-sai-nota" "Dt.Sa¡da" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE br-table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME br-param
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-param B-table-Win
ON CHOOSE OF br-param IN FRAME F-Main /* Parƒmetros */
DO:
    RUN esdlg/d03di154.w.
    RUN adm-open-query-cases.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-table
&Scoped-define SELF-NAME br-table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table B-table-Win
ON MOUSE-SELECT-DBLCLICK OF br-table IN FRAME F-Main
DO:
    RUN New-State("DblClick, SELF":U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table B-table-Win
ON ROW-DISPLAY OF br-table IN FRAME F-Main
DO:
   /* Essa rotina foi transferida para Functions/fn-situacao
   {esinc/i-dsrb.i ped-venda.cod-sit-ped ped-venda.cod-sit-ped c-situacao} 
   ASSIGN c-situacao = string(ped-venda.cod-sit-ped,'99') + '-' + c-situacao.
   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table B-table-Win
ON ROW-ENTRY OF br-table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table B-table-Win
ON ROW-LEAVE OF br-table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table B-table-Win
ON VALUE-CHANGED OF br-table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
  /* run new-state('New-Line|':U + string(rowid({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}))). */
  ASSIGN gr-ped-venda = ROWID(ped-venda).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-idet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-idet B-table-Win
ON CHOOSE OF bt-idet IN FRAME F-Main /* Detalhar */
DO:
  apply "value-changed" to br-table.
  run pi-detalhe.
  RUN pdp/pd1001.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

 ASSIGN fi-ini-nr-pedcli  = ""
        fi-fin-nr-pedcli  = "ZZZZZZZZZZZZ"
        fi-ini-dt-entrega = 01/01/0001
        fi-fin-dt-entrega = 12/31/9999
        to-abe            = YES
        to-atp            = YES
        to-att            = YES
        to-pen            = NO
        to-sus            = YES
        to-can            = YES
        to-bal            = NO.

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
  DEF VAR Filter-Value AS CHAR NO-UNDO.

  /* Copy 'Filter-Attributes' into local variables. */
  RUN get-attribute ('Filter-Value':U).
  Filter-Value = RETURN-VALUE.

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
  {src/adm/template/row-list.i "emitente"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "emitente"}

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
  {src/adm/template/sndkycas.i "nr-pedido" "ped-venda" "nr-pedido"}

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
  {src/adm/template/snd-list.i "emitente"}
  {src/adm/template/snd-list.i "ped-venda"}

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
  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-dt-emi-nota B-table-Win 
FUNCTION fn-dt-emi-nota RETURNS DATE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
   FIND LAST nota-fiscal USE-INDEX ch-clinota
       WHERE nota-fiscal.nome-ab-cli = ped-venda.nome-abrev
         AND nota-fiscal.nr-pedcli   = ped-venda.nr-pedcli
       NO-LOCK NO-ERROR.
   
   ASSIGN da-emi-nota = ?.
   IF AVAIL nota-fiscal THEN
      ASSIGN da-emi-nota = nota-fiscal.dt-emis-nota.

   RETURN da-emi-nota.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-dt-sai-nota B-table-Win 
FUNCTION fn-dt-sai-nota RETURNS DATE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
   FIND LAST nota-fiscal USE-INDEX ch-clinota
       WHERE nota-fiscal.nome-ab-cli = ped-venda.nome-abrev
         AND nota-fiscal.nr-pedcli   = ped-venda.nr-pedcli
       NO-LOCK NO-ERROR.
   
   ASSIGN da-sai-nota = ?.
   IF AVAIL nota-fiscal THEN
      ASSIGN da-sai-nota = nota-fiscal.dt-saida.

   RETURN da-sai-nota.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-sit-nota B-table-Win 
FUNCTION fn-sit-nota RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
   FIND LAST nota-fiscal USE-INDEX ch-clinota
       WHERE nota-fiscal.nome-ab-cli = ped-venda.nome-abrev
         AND nota-fiscal.nr-pedcli   = ped-venda.nr-pedcli
       NO-LOCK NO-ERROR.
   
   ASSIGN c-sit-nota = "".
   IF AVAIL nota-fiscal THEN DO:
      {esinc/i-dsrb.i nota-fiscal.ind-sit-nota nota-fiscal.ind-sit-nota c-sit-nota}
      IF nota-fiscal.dt-cancela <> ? THEN
         ASSIGN c-sit-nota = "Canc. " + STRING(nota-fiscal.dt-cancela).
   END.

   RETURN c-sit-nota.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-situacao B-table-Win 
FUNCTION fn-situacao RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
   {esinc/i-dsrb.i ped-venda.cod-sit-ped ped-venda.cod-sit-ped c-situacao} 
   ASSIGN c-situacao = string(ped-venda.cod-sit-ped,'99') + '-' + c-situacao.

  RETURN c-situacao.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-ult-nota B-table-Win 
FUNCTION fn-ult-nota RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
   FIND LAST nota-fiscal USE-INDEX ch-clinota
       WHERE nota-fiscal.nome-ab-cli = ped-venda.nome-abrev
         AND nota-fiscal.nr-pedcli   = ped-venda.nr-pedcli
       NO-LOCK NO-ERROR.
   
   ASSIGN c-ult-nota = "".
   IF AVAIL nota-fiscal THEN
      ASSIGN c-ult-nota = nota-fiscal.nr-nota-fis.

   RETURN c-ult-nota.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

