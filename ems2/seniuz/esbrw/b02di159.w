&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          mgcad            PROGRESS
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
{include/i-prgvrs.i B02DI159 2.04.00.000}

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
DEF VAR de-qtd-total LIKE ped-item.qt-pedida.
DEF VAR de-qtd-aberta LIKE ped-item.qt-pedida.
DEF VAR c-nome-abrev  LIKE emitente.nome-abrev.

/* fim das variaveis utilizadas no estilo */
DEF TEMP-TABLE tt-ped-venda 
    FIELD nome-abrev  LIKE ped-venda.nome-abrev
    FIELD nr-pedcli   LIKE ped-venda.nr-pedcli
    FIELD nr-pedrep   LIKE ped-venda.nr-pedrep
    FIELD cod-sit-ped LIKE ped-venda.cod-sit-ped
    FIELD ind-sit     AS   INT
    FIELD situacao    AS   CHAR FORMAT "x(15)"
    FIELD dt-entrega  LIKE ped-venda.dt-entrega
    FIELD vl-tot-ped  LIKE ped-venda.vl-tot-ped
    FIELD vl-liq-abe  LIKE ped-venda.vl-liq-abe
    FIELD qt-pedida   LIKE ped-item.qt-pedida
    FIELD qt-aberta   LIKE ped-item.qt-pedida
    INDEX indice-1 ind-sit.

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
&Scoped-define INTERNAL-TABLES tt-ped-venda

/* Definitions for BROWSE br-table                                      */
&Scoped-define FIELDS-IN-QUERY-br-table tt-ped-venda.nr-pedcli tt-ped-venda.nr-pedrep tt-ped-venda.situacao tt-ped-venda.dt-entrega tt-ped-venda.vl-tot-ped tt-ped-venda.vl-liq-abe tt-ped-venda.qt-pedida tt-ped-venda.qt-aberta   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-table   
&Scoped-define SELF-NAME br-table
&Scoped-define QUERY-STRING-br-table FOR EACH tt-ped-venda WHERE                                  tt-ped-venda.nr-pedcli >= fi-ini-nr-pedcli AND                                  tt-ped-venda.nr-pedcli <= fi-fin-nr-pedcli AND                                  tt-ped-venda.dt-entrega >= fi-ini-dt-entrega AND                                  tt-ped-venda.dt-entrega <= fi-fin-dt-entrega AND                                  (tt-ped-venda.cod-sit-ped = 1 AND to-abe = YES OR                                   tt-ped-venda.cod-sit-ped = 2 AND to-atp = YES OR                                   tt-ped-venda.cod-sit-ped = 3 AND to-att = YES OR                                   tt-ped-venda.cod-sit-ped = 4 AND to-pen = YES OR                                   tt-ped-venda.cod-sit-ped = 5 AND to-sus = YES OR                                   tt-ped-venda.cod-sit-ped = 6 AND to-can = YES OR                                   tt-ped-venda.cod-sit-ped = 7 AND to-bal = YES)      NO-LOCK
&Scoped-define OPEN-QUERY-br-table OPEN QUERY {&SELF-NAME} FOR EACH tt-ped-venda WHERE                                  tt-ped-venda.nr-pedcli >= fi-ini-nr-pedcli AND                                  tt-ped-venda.nr-pedcli <= fi-fin-nr-pedcli AND                                  tt-ped-venda.dt-entrega >= fi-ini-dt-entrega AND                                  tt-ped-venda.dt-entrega <= fi-fin-dt-entrega AND                                  (tt-ped-venda.cod-sit-ped = 1 AND to-abe = YES OR                                   tt-ped-venda.cod-sit-ped = 2 AND to-atp = YES OR                                   tt-ped-venda.cod-sit-ped = 3 AND to-att = YES OR                                   tt-ped-venda.cod-sit-ped = 4 AND to-pen = YES OR                                   tt-ped-venda.cod-sit-ped = 5 AND to-sus = YES OR                                   tt-ped-venda.cod-sit-ped = 6 AND to-can = YES OR                                   tt-ped-venda.cod-sit-ped = 7 AND to-bal = YES)      NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-table tt-ped-venda
&Scoped-define FIRST-TABLE-IN-QUERY-br-table tt-ped-venda


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-table br-param bt-idet 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-qtd-aberta B-table-Win 
FUNCTION fn-qtd-aberta RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-qtd-total B-table-Win 
FUNCTION fn-qtd-total RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-situacao B-table-Win 
FUNCTION fn-situacao RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON br-param 
     IMAGE-UP FILE "image/im-fil.bmp":U
     LABEL "&Parƒmetros" 
     SIZE 4.86 BY 1 TOOLTIP "Parƒmetros"
     FONT 1.

DEFINE BUTTON bt-idet 
     LABEL "&Detalhar Pedido" 
     SIZE 12.14 BY 1
     FONT 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-table FOR 
      tt-ped-venda SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-table B-table-Win _FREEFORM
  QUERY br-table NO-LOCK DISPLAY
      tt-ped-venda.nr-pedcli COLUMN-LABEL "Ped Cliente" FORMAT "x(12)":U 
      tt-ped-venda.nr-pedrep COLUMN-LABEL "Ped Repres" FORMAT "x(12)":U  
      tt-ped-venda.situacao  COLUMN-LABEL "Situa‡Æo" FORMAT "x(17)":U
      tt-ped-venda.dt-entrega FORMAT "99/99/9999":U
      tt-ped-venda.vl-tot-ped FORMAT ">,>>>,>>>,>>9.99":U 
      tt-ped-venda.vl-liq-abe FORMAT ">,>>>,>>>,>>9.99":U 
      tt-ped-venda.qt-pedida COLUMN-LABEL "Qtd Pedida" 
      tt-ped-venda.qt-aberta COLUMN-LABEL "Qtd a Faturar"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 89.57 BY 7
         FONT 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br-table AT ROW 1 COL 1
     br-param AT ROW 8.08 COL 1.29
     bt-idet AT ROW 8.08 COL 6.86
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


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
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-ped-venda WHERE
                                 tt-ped-venda.nr-pedcli >= fi-ini-nr-pedcli AND
                                 tt-ped-venda.nr-pedcli <= fi-fin-nr-pedcli AND
                                 tt-ped-venda.dt-entrega >= fi-ini-dt-entrega AND
                                 tt-ped-venda.dt-entrega <= fi-fin-dt-entrega AND
                                 (tt-ped-venda.cod-sit-ped = 1 AND to-abe = YES OR
                                  tt-ped-venda.cod-sit-ped = 2 AND to-atp = YES OR
                                  tt-ped-venda.cod-sit-ped = 3 AND to-att = YES OR
                                  tt-ped-venda.cod-sit-ped = 4 AND to-pen = YES OR
                                  tt-ped-venda.cod-sit-ped = 5 AND to-sus = YES OR
                                  tt-ped-venda.cod-sit-ped = 6 AND to-can = YES OR
                                  tt-ped-venda.cod-sit-ped = 7 AND to-bal = YES)

    NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _JoinCode[1]      = "mgmov.ped-venda.cod-emitente = mgcad.emitente.cod-emitente"
     _Where[1]         = "mgmov.ped-venda.nr-pedcli >= fi-ini-nr-pedcli
 AND mgmov.ped-venda.nr-pedcli <= fi-fin-nr-pedcli
 AND mgmov.ped-venda.dt-entrega >= fi-ini-dt-entrega
 AND mgmov.ped-venda.dt-entrega <= fi-fin-dt-entrega
 AND (mgmov.ped-venda.cod-sit-ped = 1 and to-abe = yes or
      mgmov.ped-venda.cod-sit-ped = 2 and to-atp = yes or
      mgmov.ped-venda.cod-sit-ped = 3 and to-att = yes or
      mgmov.ped-venda.cod-sit-ped = 4 and to-pen = yes or
      mgmov.ped-venda.cod-sit-ped = 5 and to-sus = yes or
      mgmov.ped-venda.cod-sit-ped = 6 and to-can = yes or
      mgmov.ped-venda.cod-sit-ped = 7 and to-bal = yes)
"
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
  ASSIGN gr-ped-venda = ROWID(tt-ped-venda).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-idet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-idet B-table-Win
ON CHOOSE OF bt-idet IN FRAME F-Main /* Detalhar Pedido */
DO:
  apply "value-changed" to br-table.
  run pi-detalhe.
  FIND ped-venda WHERE
       ped-venda.nome-abrev = tt-ped-venda.nome-abrev AND
       ped-venda.nr-pedcli = tt-ped-venda.nr-pedcli NO-LOCK NO-ERROR.
  ASSIGN gr-ped-venda = ROWID(ped-venda). 
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
        to-sus            = NO
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-popula-browse B-table-Win 
PROCEDURE pi-popula-browse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER p-nome-abrev LIKE emitente.nome-abrev.
ASSIGN c-nome-abrev = p-nome-abrev.

EMPTY TEMP-TABLE tt-ped-venda.

SESSION:SET-WAIT-STATE("general":U).
FOR EACH ped-venda WHERE
         ped-venda.nome-abrev = c-nome-abrev NO-LOCK.

    CREATE tt-ped-venda.
    ASSIGN tt-ped-venda.nome-abrev = ped-venda.nome-abrev
           tt-ped-venda.nr-pedcli = ped-venda.nr-pedcli
           tt-ped-venda.nr-pedrep = ped-venda.nr-pedrep
           tt-ped-venda.cod-sit-ped = ped-venda.cod-sit-ped
           tt-ped-venda.situacao = fn-situacao()
           tt-ped-venda.dt-entrega = ped-venda.dt-entrega
           tt-ped-venda.vl-tot-ped = ped-venda.vl-tot-ped
           tt-ped-venda.vl-liq-abe = ped-venda.vl-liq-abe
           tt-ped-venda.qt-pedida = fn-qtd-total()
           tt-ped-venda.qt-aberta = fn-qtd-aberta().

    ASSIGN tt-ped-venda.ind-sit = IF ped-venda.cod-sit-ped = 1
                                  THEN 4
                                  ELSE IF ped-venda.cod-sit-ped = 2
                                       THEN 5
                                       ELSE IF ped-venda.cod-sit-ped = 3
                                            THEN 6
                                            ELSE IF ped-venda.cod-sit-ped = 6
                                                 THEN 1
                                                 ELSE IF ped-venda.cod-sit-ped = 5
                                                      THEN 2
                                                      ELSE IF ped-venda.cod-sit-ped = 4
                                                           THEN 3 ELSE 7.

END.
SESSION:SET-WAIT-STATE("":U).

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
  {src/adm/template/snd-list.i "tt-ped-venda"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-qtd-aberta B-table-Win 
FUNCTION fn-qtd-aberta RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  
  RETURN de-qtd-aberta.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-qtd-total B-table-Win 
FUNCTION fn-qtd-total RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  ASSIGN de-qtd-total  = 0
         de-qtd-aberta = 0.
  FOR EACH ped-item OF ped-venda NO-LOCK:
      ASSIGN de-qtd-total = de-qtd-total + ped-item.qt-pedida.
      IF ped-item.cod-sit-item <> 6 THEN
         ASSIGN de-qtd-aberta = de-qtd-aberta + (ped-item.qt-pedida - ped-item.qt-atendida).
  END.

  RETURN de-qtd-total.   /* Function return value. */

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

