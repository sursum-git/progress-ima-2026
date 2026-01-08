&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
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
{include/i-prgvrs.i B06AD098 2.04.00.000}

/* Create an unnamed pool to store all the widgets created by this procedure.
   This is a good default which assures that this procedure's triggers and 
   internal procedures will execute in this procedure's storage, and that 
   proper cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
&Scop adm-attribute-dlg support/browserd.w

/* Parameters Definitions ---                                           */
 
/* Local Variable Definitions ---                                       */

/*:T Variaveis usadas internamente pelo estilo, favor nao elimina-las     */

/*:T v ri veis de uso globla */
def new global shared var v-row-parent as rowid no-undo.

DEFINE NEW GLOBAL SHARED VAR fi-cod-rep          LIKE nota-fiscal.cod-rep.
DEFINE NEW GLOBAL SHARED VAR fi-ini-cod-estabel  LIKE nota-fiscal.cod-estabel.
DEFINE NEW GLOBAL SHARED VAR fi-fin-cod-estabel  LIKE nota-fiscal.cod-estabel.
DEFINE NEW GLOBAL SHARED VAR fi-ini-serie        LIKE nota-fiscal.serie.
DEFINE NEW GLOBAL SHARED VAR fi-fin-serie        LIKE nota-fiscal.serie.
DEFINE NEW GLOBAL SHARED VAR fi-ini-nr-nota-fis  LIKE nota-fiscal.nr-nota-fis.
DEFINE NEW GLOBAL SHARED VAR fi-fin-nr-nota-fis  LIKE nota-fiscal.nr-nota-fis.
DEFINE NEW GLOBAL SHARED VAR fi-ini-dt-emis-nota LIKE nota-fiscal.dt-emis-nota.
DEFINE NEW GLOBAL SHARED VAR fi-fin-dt-emis-nota LIKE nota-fiscal.dt-emis-nota.
DEFINE NEW GLOBAL SHARED VAR v-row-emitente AS ROWID NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR h-v01ad229 AS HANDLE.

/*:T vari veis de uso local */
def var v-row-table  as rowid.

DEF TEMP-TABLE tt-emitente
    FIELD LINE AS INT /*:T Este campo ‚ obrigat¢rio */
    FIELD cod-emitente LIKE emitente.cod-emitente
    FIELD nome-emit    LIKE emitente.nome-emit
    FIELD quantidade   AS DEC FORMAT ">>>,>>>,>>9.99"
    FIELD valor        AS DEC FORMAT ">>>,>>>,>>9.99"
    INDEX ch-emitente cod-emitente.

DEF VAR c-cod-lista-obj AS CHAR NO-UNDO.

DEF VAR de-quantidade    AS DEC FORMAT ">>>,>>>,>>9.99".
DEF VAR de-valor         AS DEC FORMAT ">>>,>>>,>>9.99".
DEF VAR de-tot-cli-quant AS DEC.
DEF VAR de-tot-cli-valor AS DEC.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE BrowseDigitacao
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-digita

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-emitente

/* Definitions for BROWSE br-digita                                     */
&Scoped-define FIELDS-IN-QUERY-br-digita tt-emitente.cod-emitente tt-emitente.nome-emit tt-emitente.quantidade tt-emitente.valor   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-digita   
&Scoped-define SELF-NAME br-digita
&Scoped-define QUERY-STRING-br-digita FOR EACH tt-emitente NO-LOCK
&Scoped-define OPEN-QUERY-br-digita OPEN QUERY {&self-name} FOR EACH tt-emitente NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-digita tt-emitente
&Scoped-define FIRST-TABLE-IN-QUERY-br-digita tt-emitente


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-digita bt-detalhar 
&Scoped-Define DISPLAYED-OBJECTS fi-tot-cli-quant fi-tot-cli-valor 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" B-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-name
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
&BROWSE-name
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
DEFINE BUTTON bt-detalhar 
     LABEL "&Detalhar" 
     SIZE 10 BY 1 TOOLTIP "Mostrar as Notas Fiscais do montate do Cliente."
     FONT 1.

DEFINE VARIABLE fi-tot-cli-quant AS DECIMAL FORMAT "ZZZ,ZZZ,ZZ9.99":U INITIAL 0 
      VIEW-AS TEXT 
     SIZE 14.57 BY .88
     FONT 2 NO-UNDO.

DEFINE VARIABLE fi-tot-cli-valor AS DECIMAL FORMAT "ZZZ,ZZZ,ZZ9.99":U INITIAL 0 
      VIEW-AS TEXT 
     SIZE 14.57 BY .88
     FONT 2 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-digita FOR 
      tt-emitente SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-digita B-table-Win _FREEFORM
  QUERY br-digita NO-LOCK DISPLAY
      tt-emitente.cod-emitente COLUMN-LABEL "Cliente"
      tt-emitente.nome-emit    COLUMN-LABEL "Nome"
      tt-emitente.quantidade   COLUMN-LABEL "Quantidade"
      tt-emitente.valor        COLUMN-LABEL "Valor"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 87.72 BY 10.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br-digita AT ROW 1 COL 1
     bt-detalhar AT ROW 11.08 COL 2.14
     fi-tot-cli-quant AT ROW 11.08 COL 65 RIGHT-ALIGNED NO-LABEL
     fi-tot-cli-valor AT ROW 11.08 COL 79.71 RIGHT-ALIGNED NO-LABEL
     "Totais:" VIEW-AS TEXT
          SIZE 5 BY .75 AT ROW 11.17 COL 46
          FONT 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: BrowseDigitacao
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: External-Tables
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 11.13
         WIDTH              = 88.14.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
/* BROWSE-TAB br-digita 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fi-tot-cli-quant IN FRAME F-Main
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN fi-tot-cli-valor IN FRAME F-Main
   NO-ENABLE ALIGN-R                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-digita
/* Query rebuild information for BROWSE br-digita
     _START_FREEFORM
OPEN QUERY {&self-name} FOR EACH tt-emitente NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* BROWSE br-digita */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br-digita
&Scoped-define SELF-NAME br-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita B-table-Win
ON ROW-ENTRY OF br-digita IN FRAME F-Main
DO:
    /* This code displays initial values for newly added or copied rows. */
    {src/adm/template/brsentry.i}
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita B-table-Win
ON ROW-LEAVE OF br-digita IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
       by pressing the Save button on an Update SmartPanel. */
    {src/adm/template/brsleave.i}

    run pi-row-leave.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita B-table-Win
ON VALUE-CHANGED OF br-digita IN FRAME F-Main
DO:
    /* This ADM trigger code must be preserved in order to notify other
       objects when the browser's current row changes. */
    {src/adm/template/brschnge.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-detalhar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-detalhar B-table-Win
ON CHOOSE OF bt-detalhar IN FRAME F-Main /* Detalhar */
DO:
  apply "value-changed":U to br-digita.

  FIND emitente WHERE
       emitente.cod-emitente = tt-emitente.cod-emitente NO-LOCK NO-ERROR.

  RUN esp/esft0036.w (INPUT ROWID(emitente)).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-popula-browse B-table-Win 
PROCEDURE pi-popula-browse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH tt-emitente.
      DELETE tt-emitente.
  END.
    
  ASSIGN de-tot-cli-quant = 0
         de-tot-cli-valor = 0.

  FIND repres WHERE
       repres.cod-rep = fi-cod-rep NO-LOCK NO-ERROR.

  FOR EACH nota-fiscal 
      WHERE nota-fiscal.no-ab-reppri  = repres.nome-abrev
        AND nota-fiscal.cod-estabel  >= fi-ini-cod-estabel
        AND nota-fiscal.cod-estabel  <= fi-fin-cod-estabel
        AND nota-fiscal.serie        >= fi-ini-serie
        AND nota-fiscal.serie        <= fi-fin-serie
        AND nota-fiscal.dt-emis-nota >= fi-ini-dt-emis-nota
        AND nota-fiscal.dt-emis-nota <= fi-fin-dt-emis-nota
        AND nota-fiscal.esp-docto     = 22 /* nfs */
        AND nota-fiscal.emite-dup     = YES
        AND nota-fiscal.dt-cancela    = ?
      NO-LOCK USE-INDEX ch-nome-repres:

      FIND tt-emitente WHERE tt-emitente.cod-emitente = nota-fiscal.cod-emitente
                       NO-LOCK NO-ERROR.

      IF NOT AVAIL tt-emitente THEN DO:
         FIND emitente WHERE emitente.cod-emitente = nota-fiscal.cod-emitente 
                       NO-LOCK NO-ERROR.
         CREATE tt-emitente.
         ASSIGN tt-emitente.cod-emitente = nota-fiscal.cod-emitente
                tt-emitente.nome-emit    = emitente.nome-emit.
      END.

      FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK:
          ASSIGN tt-emitente.quantidade = tt-emitente.quantidade + it-nota-fisc.qt-faturada[1]
                 tt-emitente.valor      = tt-emitente.valor + it-nota-fisc.vl-tot-item
                 de-tot-cli-quant       = de-tot-cli-quant + it-nota-fisc.qt-faturada[1]
                 de-tot-cli-valor       = de-tot-cli-valor + it-nota-fisc.vl-tot-item.
      END.
  END.

  ASSIGN fi-tot-cli-quant:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(de-tot-cli-quant)
         fi-tot-cli-valor:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(de-tot-cli-valor).
  
  {&OPEN-QUERY-br-digita}

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
  {src/adm/template/snd-list.i "tt-emitente"}

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

