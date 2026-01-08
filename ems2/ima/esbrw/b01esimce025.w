&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r11 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
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
{include/i-prgvrs.i B01esimce025 2.00.04.001}

DEF BUFFER ITEM FOR ITEM.

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

DEF TEMP-TABLE tt-saldo-estoq
    FIELD  cod-estabel   LIKE movadm.saldo-estoq.cod-estabel 
    FIELD  empresa       LIKE mgadm.empresa.nome
    FIELD  it-codigo     LIKE movadm.saldo-estoq.it-codigo
    field  cod-depos     like movadm.saldo-estoq.cod-depos
    field  cod-refer     like movadm.saldo-estoq.cod-refer 
    FIELD  qt-disponivel like movadm.saldo-estoq.qtidade-atu
    field  qt-aloc-ped   like movadm.saldo-estoq.qt-aloc-ped 
    field  qt-alocada    like movadm.saldo-estoq.qt-alocada 
    field  qtidade-atu   like movadm.saldo-estoq.qtidade-atu
    field  qt-aloc-prod  LIKE movadm.saldo-estoq.qt-aloc-prod
    FIELD  qt-aloc-pi    LIKE movadm.saldo-estoq.qtidade-atu.

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE NEW GLOBAL SHARED VARIABLE  base AS CHARACTER.

DEFINE NEW GLOBAL SHARED VARIABLE var-glb-cod-estab-ini AS CHARACTER   NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE var-glb-cod-estab-fim AS CHARACTER   NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE var-glb-refer-ini     AS CHARACTER   NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE var-glb-refer-fim     AS CHARACTER   NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE var-glb-cod-depos-ini AS CHARACTER   NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE var-glb-cod-depos-fim AS CHARACTER   NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE var-glb-estab         AS INT         NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE var-glb-frame         AS HANDLE      NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE var-glb-outlet        AS INT         NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE var-glb-ge-ini        AS INTEGER   NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE var-glb-ge-fim        AS INTEGER   NO-UNDO.

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
&Scoped-define EXTERNAL-TABLES item
&Scoped-define FIRST-EXTERNAL-TABLE item


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR item.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-saldo-estoq

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table tt-saldo-estoq.cod-estabel tt-saldo-estoq.empresa tt-saldo-estoq.cod-depos tt-saldo-estoq.cod-refer tt-saldo-estoq.qt-disponivel tt-saldo-estoq.qt-aloc-ped tt-saldo-estoq.qt-alocada tt-saldo-estoq.qtidade-atu   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table   
&Scoped-define SELF-NAME br_table
&Scoped-define QUERY-STRING-br_table FOR EACH tt-saldo-estoq  NO-LOCK BY tt-saldo-estoq.cod-refer                                                          BY tt-saldo-estoq.cod-estabel.      /*~{&SORTBY-PHRASE}.*/
&Scoped-define OPEN-QUERY-br_table OPEN QUERY {&SELF-NAME} FOR EACH tt-saldo-estoq  NO-LOCK BY tt-saldo-estoq.cod-refer                                                          BY tt-saldo-estoq.cod-estabel.      /*~{&SORTBY-PHRASE}.*/.
&Scoped-define TABLES-IN-QUERY-br_table tt-saldo-estoq
&Scoped-define FIRST-TABLE-IN-QUERY-br_table tt-saldo-estoq


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
it-codigo||y|mgadm.saldo-estoq.it-codigo
cod-depos||y|mgadm.saldo-estoq.cod-depos
cod-estabel||y|mgadm.saldo-estoq.cod-estabel
cod-refer||y|mgadm.saldo-estoq.cod-refer
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "it-codigo,cod-depos,cod-estabel,cod-refer"':U).

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
</FILTER-ATTRIBUTES> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-qt-disponivel B-table-Win 
FUNCTION fn-qt-disponivel RETURNS DECIMAL
  ( qt-atu AS DEC)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      tt-saldo-estoq SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _FREEFORM
  QUERY br_table NO-LOCK DISPLAY
      tt-saldo-estoq.cod-estabel  
      tt-saldo-estoq.empresa       FORMAT "x(12)" COLUMN-LABEL "Empresa" 
      tt-saldo-estoq.cod-depos    
      tt-saldo-estoq.cod-refer     FORMAT "x(6)"
      tt-saldo-estoq.qt-disponivel FORMAT "->>>,>>>,>>9.99" WIDTH 12 COLUMN-LABEL "Qt Disponivel" 
      tt-saldo-estoq.qt-aloc-ped   FORMAT "->>>,>>>,>>9.99" WIDTH 12
      tt-saldo-estoq.qt-alocada    FORMAT "->>>,>>>,>>9.99" WIDTH 12
      tt-saldo-estoq.qtidade-atu   FORMAT "->>>,>>>,>>9.99" WIDTH 12
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 87.86 BY 16 ROW-HEIGHT-CHARS .46.


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
   External Tables: mgadm.item
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
         HEIGHT             = 16
         WIDTH              = 88.57.
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
OPEN QUERY {&SELF-NAME} FOR EACH tt-saldo-estoq  NO-LOCK BY tt-saldo-estoq.cod-refer
                                                         BY tt-saldo-estoq.cod-estabel.

    /*~{&SORTBY-PHRASE}.*/
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
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
ON MOUSE-SELECT-DBLCLICK OF br_table IN FRAME F-Main
DO:
  IF tt-saldo-estoq.qt-aloc-ped = 0 THEN
     RETURN NO-APPLY.

  FIND FIRST repres WHERE
             SUBSTRING(repres.char-1,500,12) = c-seg-usuario NO-LOCK NO-ERROR.
  IF AVAIL repres THEN  DO.
     FIND cm-ext-repres WHERE
          cm-ext-repres.cod-rep = repres.cod-rep NO-ERROR.

     IF NOT AVAIL cm-ext-repres OR
        (AVAIL cm-ext-repres AND cm-ext-repres.classe > 2) THEN DO.

        MESSAGE 'Usu†rio sem Permiss∆o para detalhar Pedidos...'
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
     END.
  END.


  /*-------------- Conecta Bando IMA e Med ---------------*/
  RUN pi-connect.
  /*-------------- Popula tt-ima-ped-item ---------------*/

  RUN esrp/esimce025arp.p (INPUT tt-saldo-estoq.cod-estabel,
                           INPUT tt-saldo-estoq.it-codigo, 
                           INPUT tt-saldo-estoq.cod-refer, 
                           INPUT tt-saldo-estoq.cod-depos).

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
  {src/adm/template/row-list.i "item"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "item"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-open-query-cases B-table-Win 
PROCEDURE local-open-query-cases :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR i-estoq AS INT.

  EMPTY TEMP-TABLE tt-saldo-estoq.
  /* Code placed here will execute PRIOR to standard behavior. */
  
  
  /*-------------- Conecta Bando IMA e Med ---------------*/
  RUN pi-connect.
  /*-------------- Popula tt-saldo-estoq ---------------*/
  
  RUN esrp/esimce025rp.p (INPUT ITEM.it-codigo,
                          OUTPUT TABLE tt-saldo-estoq).

  IF CONNECTED('dbaux') THEN
     DISCONNECT dbaux.

  FOR EACH tt-saldo-estoq.
      IF tt-saldo-estoq.cod-estabel < var-glb-cod-estab-ini OR
         tt-saldo-estoq.cod-estabel > var-glb-cod-estab-fim OR
         tt-saldo-estoq.cod-refer   < var-glb-refer-ini OR
         tt-saldo-estoq.cod-refer   > var-glb-refer-fim THEN DO.
         DELETE tt-saldo-estoq.
         NEXT.
      END.

      IF var-glb-outlet <> 3 THEN DO.  // ambos
         ASSIGN i-estoq = 2.  // Pronta Entrega
         FIND liquida-ima WHERE 
              liquida-ima.cod-estabel = tt-saldo-estoq.cod-estabel AND
              liquida-ima.it-codigo = tt-saldo-estoq.it-codigo AND
              liquida-ima.cod-refer = tt-saldo-estoq.cod-refer
              NO-LOCK NO-ERROR.
         IF AVAIL liquida-ima THEN
            ASSIGN i-estoq = 1.  // OutLet
    
         IF var-glb-outlet <> i-estoq THEN
            DELETE tt-saldo-estoq.
      END.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE (INPUT 'open-query-cases':U) .

  MESSAGE 'Esse programa foi descontiuado. Favor utilizar a vers∆o Web atravÇs do portal de acesso'
      VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
  OS-COMMAND SILENT VALUE('start https://imaonline.imatextil.com.br/portal_acesso_2/app_ldap_Login/?url_apl_princ=cons_imce025').
  RETURN 'nok'.
  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-connect B-table-Win 
PROCEDURE pi-connect :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN esapi\connect-ima-med.p. 
  
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
  {src/adm/template/sndkycas.i "it-codigo" "saldo-estoq" "it-codigo"}
  {src/adm/template/sndkycas.i "cod-depos" "saldo-estoq" "cod-depos"}
  {src/adm/template/sndkycas.i "cod-estabel" "saldo-estoq" "cod-estabel"}
  {src/adm/template/sndkycas.i "cod-refer" "saldo-estoq" "cod-refer"}

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
  {src/adm/template/snd-list.i "item"}
  {src/adm/template/snd-list.i "tt-saldo-estoq"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-qt-disponivel B-table-Win 
FUNCTION fn-qt-disponivel RETURNS DECIMAL
  ( qt-atu AS DEC) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  

  RETURN 0.00.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

