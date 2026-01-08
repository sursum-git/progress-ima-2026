&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          ems2ima            PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS q-tables 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i Q01DI159 2.04.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
&Scop adm-attribute-dlg support/queryd.w

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE NEW GLOBAL SHARED VAR fi-ini-nome-abrev LIKE ped-item-ext.nome-abrev.
DEFINE NEW GLOBAL SHARED VAR fi-fin-nome-abrev LIKE ped-item-ext.nome-abrev.
DEFINE NEW GLOBAL SHARED VAR fi-ini-nr-pedcli  LIKE ped-item-ext.nr-pedcli.
DEFINE NEW GLOBAL SHARED VAR fi-fin-nr-pedcli  LIKE ped-item-ext.nr-pedcli.
DEFINE NEW GLOBAL SHARED VAR fi-ini-dt-entrega LIKE ped-item.dt-entrega.
DEFINE NEW GLOBAL SHARED VAR fi-fin-dt-entrega LIKE ped-item.dt-entrega.
DEFINE NEW GLOBAL SHARED VAR fi-ini-dt-implant LIKE ped-venda.dt-implant.
DEFINE NEW GLOBAL SHARED VAR fi-fin-dt-implant LIKE ped-venda.dt-implant.
DEFINE NEW GLOBAL SHARED VAR fi-ini-it-codigo  LIKE ped-item-ext.it-codigo.
DEFINE NEW GLOBAL SHARED VAR fi-fin-it-codigo  LIKE ped-item-ext.it-codigo.
DEFINE NEW GLOBAL SHARED VAR to-abe AS LOG.
DEFINE NEW GLOBAL SHARED VAR to-atp AS LOG.
DEFINE NEW GLOBAL SHARED VAR to-att AS LOG.
DEFINE NEW GLOBAL SHARED VAR to-pen AS LOG.
DEFINE NEW GLOBAL SHARED VAR to-sus AS LOG.
DEFINE NEW GLOBAL SHARED VAR to-can AS LOG.
DEFINE NEW GLOBAL SHARED VAR to-bal AS LOG.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartQuery
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,Navigation-Target

&Scoped-define QUERY-NAME Query-Main

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ped-venda

/* Definitions for QUERY Query-Main                                     */
&Scoped-define QUERY-STRING-Query-Main FOR EACH ped-venda ~
      WHERE ped-venda.nome-abrev >= fi-ini-nome-abrev ~
 AND ped-venda.nome-abrev <= fi-fin-nome-abrev ~
 AND ped-venda.nr-pedcli >= fi-ini-nr-pedcli ~
 AND ped-venda.nr-pedcli <= fi-fin-nr-pedcli ~
 and ped-venda.dt-entrega >= fi-ini-dt-entrega ~
 and ped-venda.dt-entrega <= fi-fin-dt-entrega ~
 AND (ems2ima.ped-venda.cod-sit-ped = 1 and to-abe = yes or ~
      ped-venda.cod-sit-ped = 2 and to-atp = yes or ~
      ped-venda.cod-sit-ped = 3 and to-att = yes or ~
      ped-venda.cod-sit-ped = 4 and to-pen = yes or ~
      ped-venda.cod-sit-ped = 5 and to-sus = yes or ~
      ped-venda.cod-sit-ped = 6 and to-can = yes or ~
      ped-venda.cod-sit-ped = 7 and to-bal = yes) NO-LOCK ~
    BY ped-venda.nome-abrev ~
       BY ped-venda.nr-pedcli INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH ped-venda ~
      WHERE ped-venda.nome-abrev >= fi-ini-nome-abrev ~
 AND ped-venda.nome-abrev <= fi-fin-nome-abrev ~
 AND ped-venda.nr-pedcli >= fi-ini-nr-pedcli ~
 AND ped-venda.nr-pedcli <= fi-fin-nr-pedcli ~
 and ped-venda.dt-entrega >= fi-ini-dt-entrega ~
 and ped-venda.dt-entrega <= fi-fin-dt-entrega ~
 AND (ems2ima.ped-venda.cod-sit-ped = 1 and to-abe = yes or ~
      ped-venda.cod-sit-ped = 2 and to-atp = yes or ~
      ped-venda.cod-sit-ped = 3 and to-att = yes or ~
      ped-venda.cod-sit-ped = 4 and to-pen = yes or ~
      ped-venda.cod-sit-ped = 5 and to-sus = yes or ~
      ped-venda.cod-sit-ped = 6 and to-can = yes or ~
      ped-venda.cod-sit-ped = 7 and to-bal = yes) NO-LOCK ~
    BY ped-venda.nome-abrev ~
       BY ped-venda.nr-pedcli INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Query-Main ped-venda
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main ped-venda


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" q-tables _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&QUERY-NAME
</KEY-OBJECT>
<FOREIGN-KEYS>
nr-pedido||y|movdis.ped-venda.nr-pedido
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Advanced Query Options" q-tables _INLINE
/* Actions: ? adm/support/advqedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&QUERY-NAME
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

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      ped-venda SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartQuery
   Allow: Basic,Query
   Frames: 1
   Add Fields to: NEITHER
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
  CREATE WINDOW q-tables ASSIGN
         HEIGHT             = 1.33
         WIDTH              = 22.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB q-tables 
/* ************************* Included-Libraries *********************** */

{utp/ut-glob.i}
{src/adm/method/query.i}
{include/c-query.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW q-tables
  VISIBLE,,RUN-PERSISTENT                                               */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK QUERY Query-Main
/* Query rebuild information for QUERY Query-Main
     _TblList          = "ems2ima.ped-venda"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "ems2ima.ped-venda.nome-abrev|yes,ems2ima.ped-venda.nr-pedcli|yes"
     _Where[1]         = "ems2ima.ped-venda.nome-abrev >= fi-ini-nome-abrev
 AND ems2ima.ped-venda.nome-abrev <= fi-fin-nome-abrev
 AND ems2ima.ped-venda.nr-pedcli >= fi-ini-nr-pedcli
 AND ems2ima.ped-venda.nr-pedcli <= fi-fin-nr-pedcli
 and ems2ima.ped-venda.dt-entrega >= fi-ini-dt-entrega
 and ems2ima.ped-venda.dt-entrega <= fi-fin-dt-entrega
 AND (ems2ima.ped-venda.cod-sit-ped = 1 and to-abe = yes or
      ems2ima.ped-venda.cod-sit-ped = 2 and to-atp = yes or
      ems2ima.ped-venda.cod-sit-ped = 3 and to-att = yes or
      ems2ima.ped-venda.cod-sit-ped = 4 and to-pen = yes or
      ems2ima.ped-venda.cod-sit-ped = 5 and to-sus = yes or
      ems2ima.ped-venda.cod-sit-ped = 6 and to-can = yes or
      ems2ima.ped-venda.cod-sit-ped = 7 and to-bal = yes)"
     _Design-Parent    is WINDOW q-tables @ ( 1.17 , 9.86 )
*/  /* QUERY Query-Main */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK q-tables 


/* ***************************  Main Block  *************************** */

 ASSIGN fi-ini-nome-abrev = ""
        fi-fin-nome-abrev = "ZZZZZZZZZZZZ"
        fi-ini-nr-pedcli  = ""
        fi-fin-nr-pedcli  = "ZZZZZZZZZZZZ"
        fi-ini-it-codigo  = ""
        fi-fin-it-codigo  = "ZZZZZZZZZZZZZZZZ"
        fi-ini-dt-entrega = 01/01/0001
        fi-fin-dt-entrega = 12/31/9999
        fi-ini-dt-implant = 01/01/0001
        fi-fin-dt-implant = 12/31/9999
        to-abe            = YES
        to-atp            = YES
        to-att            = NO
        to-pen            = NO
        to-sus            = NO
        to-can            = NO
        to-bal            = NO.

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).
  &ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-open-query-cases q-tables  adm/support/_adm-opn.p
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

  {&OPEN-QUERY-{&QUERY-NAME}}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available q-tables  _ADM-ROW-AVAILABLE
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

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI q-tables  _DEFAULT-DISABLE
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
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key q-tables  adm/support/_key-snd.p
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records q-tables  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "ped-venda"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed q-tables 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/qstates.i}
  END CASE.
  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

