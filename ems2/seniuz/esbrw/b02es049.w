&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          espec            PROGRESS
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
{include/i-prgvrs.i B02ES049 2.04.00.000}

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

/*:T Variaveis usadas internamente pelo estilo, favor nao elimina-las     */

/*:T v ri veis de uso globla */
def new global shared var v-row-parent as rowid no-undo.

/*:T vari veis de uso local */
def var v-row-table  as rowid.


/*:T fim das variaveis utilizadas no estilo */
DEF VAR c-situacao    AS CHAR FORMAT "x(12)".
DEFINE NEW GLOBAL SHARED VAR to-imp    AS LOG.
DEFINE NEW GLOBAL SHARED VAR to-prod   AS LOG.
DEFINE NEW GLOBAL SHARED VAR to-est    AS LOG.
DEFINE NEW GLOBAL SHARED VAR to-res    AS LOG.
DEFINE NEW GLOBAL SHARED VAR to-fat    AS LOG.
DEFINE NEW GLOBAL SHARED VAR to-reproc AS LOG.

DEF VAR c-item AS CHAR.
DEF VAR c-ref  AS CHAR.
DEF VAR c-lote AS CHAR.

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

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ob-etiqueta

/* Definitions for BROWSE br-table                                      */
&Scoped-define FIELDS-IN-QUERY-br-table ob-etiqueta.nr-ob ~
ob-etiqueta.num-etiqueta ob-etiqueta.nr-sequencia ob-etiqueta.nr-lote ~
ob-etiqueta.cod-refer ob-etiqueta.quantidade fn-situacao() @ c-situacao ~
ob-etiqueta.localizacao ob-etiqueta.it-codigo 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-table 
&Scoped-define QUERY-STRING-br-table FOR EACH ob-etiqueta WHERE TRUE /* Join to saldo-estoq incomplete */ ~
      AND   (espec.ob-etiqueta.situacao = 1 and to-imp = yes or ~
   ob-etiqueta.situacao = 2 and to-prod = yes or ~
   ob-etiqueta.situacao = 3 and to-est = yes or ~
   ob-etiqueta.situacao = 4 and to-res = yes or ~
   ob-etiqueta.situacao = 5 and to-fat = yes or ~
   ob-etiqueta.situacao = 6 and to-reproc = yes) ~
  AND ob-etiqueta.it-codigo = c-item ~
  AND ob-etiqueta.cod-refer = c-ref ~
  AND ob-etiqueta.nr-lote = substr(c-lote,1,2) ~
  ~
 NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-table OPEN QUERY br-table FOR EACH ob-etiqueta WHERE TRUE /* Join to saldo-estoq incomplete */ ~
      AND   (espec.ob-etiqueta.situacao = 1 and to-imp = yes or ~
   ob-etiqueta.situacao = 2 and to-prod = yes or ~
   ob-etiqueta.situacao = 3 and to-est = yes or ~
   ob-etiqueta.situacao = 4 and to-res = yes or ~
   ob-etiqueta.situacao = 5 and to-fat = yes or ~
   ob-etiqueta.situacao = 6 and to-reproc = yes) ~
  AND ob-etiqueta.it-codigo = c-item ~
  AND ob-etiqueta.cod-refer = c-ref ~
  AND ob-etiqueta.nr-lote = substr(c-lote,1,2) ~
  ~
 NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-table ob-etiqueta
&Scoped-define FIRST-TABLE-IN-QUERY-br-table ob-etiqueta


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-table bt-param 
&Scoped-Define DISPLAYED-OBJECTS fi-tot-qtd 

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
it-codigo||y|espec.ob-etiqueta.it-codigo
cod-refer||y|espec.ob-etiqueta.cod-refer
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "it-codigo,cod-refer"':U).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-situacao B-table-Win 
FUNCTION fn-situacao RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-param 
     IMAGE-UP FILE "image/im-param.bmp":U
     LABEL "&Parƒmetros" 
     SIZE 4.86 BY .88 TOOLTIP "Parƒmetros da Situa‡Æo das Etiquetas".

DEFINE VARIABLE fi-tot-qtd AS DECIMAL FORMAT "->,>>>,>>9.99":U INITIAL 0 
     LABEL "Total" 
     VIEW-AS FILL-IN 
     SIZE 8.57 BY .88
     FONT 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-table FOR 
      ob-etiqueta SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-table B-table-Win _STRUCTURED
  QUERY br-table NO-LOCK DISPLAY
      ob-etiqueta.nr-ob FORMAT ">>>,>>9":U
      ob-etiqueta.num-etiqueta FORMAT "999999999":U
      ob-etiqueta.nr-sequencia FORMAT ">>9":U
      ob-etiqueta.nr-lote FORMAT "X(8)":U WIDTH 3
      ob-etiqueta.cod-refer FORMAT "X(8)":U WIDTH 8
      ob-etiqueta.quantidade FORMAT ">>>,>>9.99":U
      fn-situacao() @ c-situacao COLUMN-LABEL "Situa‡Æo" WIDTH 9
      ob-etiqueta.localizacao FORMAT "XXX/XXX":U WIDTH 8
      ob-etiqueta.it-codigo FORMAT "X(16)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 86 BY 10.67
         FONT 1 ROW-HEIGHT-CHARS .58.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br-table AT ROW 1 COL 1
     fi-tot-qtd AT ROW 11.79 COL 33.57 COLON-ALIGNED
     bt-param AT ROW 11.88 COL 3.86
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
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
         HEIGHT             = 11.88
         WIDTH              = 86.14.
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

/* SETTINGS FOR FILL-IN fi-tot-qtd IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-table
/* Query rebuild information for BROWSE br-table
     _TblList          = "espec.ob-etiqueta WHERE mgmov.saldo-estoq <external> ... ... ..."
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "  (espec.ob-etiqueta.situacao = 1 and to-imp = yes or
   espec.ob-etiqueta.situacao = 2 and to-prod = yes or
   espec.ob-etiqueta.situacao = 3 and to-est = yes or
   espec.ob-etiqueta.situacao = 4 and to-res = yes or
   espec.ob-etiqueta.situacao = 5 and to-fat = yes or
   espec.ob-etiqueta.situacao = 6 and to-reproc = yes)
  AND espec.ob-etiqueta.it-codigo = c-item
  AND espec.ob-etiqueta.cod-refer = c-ref
  AND espec.ob-etiqueta.nr-lote = substr(c-lote,1,2)
 
"
     _FldNameList[1]   = espec.ob-etiqueta.nr-ob
     _FldNameList[2]   = espec.ob-etiqueta.num-etiqueta
     _FldNameList[3]   = espec.ob-etiqueta.nr-sequencia
     _FldNameList[4]   > espec.ob-etiqueta.nr-lote
"ob-etiqueta.nr-lote" ? ? "character" ? ? ? ? ? ? no ? no no "3" yes no no "U" "" ""
     _FldNameList[5]   > espec.ob-etiqueta.cod-refer
"ob-etiqueta.cod-refer" ? ? "character" ? ? ? ? ? ? no ? no no "8" yes no no "U" "" ""
     _FldNameList[6]   = espec.ob-etiqueta.quantidade
     _FldNameList[7]   > "_<CALC>"
"fn-situacao() @ c-situacao" "Situa‡Æo" ? ? ? ? ? ? ? ? no ? no no "9" yes no no "U" "" ""
     _FldNameList[8]   > espec.ob-etiqueta.localizacao
"ob-etiqueta.localizacao" ? "XXX/XXX" "character" ? ? ? ? ? ? no ? no no "8" yes no no "U" "" ""
     _FldNameList[9]   = espec.ob-etiqueta.it-codigo
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
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-param
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-param B-table-Win
ON CHOOSE OF bt-param IN FRAME F-Main /* Parƒmetros */
DO:
   RUN esdlg/d01es049.w.
   RUN local-open-query-cases.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
ASSIGN  to-imp              = NO
        to-prod             = NO
        to-est              = YES
        to-res              = YES
        to-fat              = NO
        to-reproc           = NO.

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

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query-cases':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  
  RUN pi-calcula.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-calcula B-table-Win 
PROCEDURE pi-calcula :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN fi-tot-qtd = 0.
    FOR EACH ob-etiqueta WHERE (ob-etiqueta.situacao = 1 AND to-imp    = YES OR 
                                ob-etiqueta.situacao = 2 AND to-prod   = YES OR  
                                ob-etiqueta.situacao = 3 AND to-est    = YES OR  
                                ob-etiqueta.situacao = 4 AND to-res    = YES OR  
                                ob-etiqueta.situacao = 5 AND to-fat    = YES OR  
                                ob-etiqueta.situacao = 6 AND to-reproc = YES) AND
                                ob-etiqueta.it-codigo = c-item                AND
                                ob-etiqueta.cod-refer = c-ref                 AND 
                                ob-etiqueta.nr-lote   = substr(c-lote,1,2)    NO-LOCK.
        ASSIGN fi-tot-qtd =  fi-tot-qtd + ob-etiqueta.quantidade.
    END.
    DISP fi-tot-qtd WITH FRAME {&FRAME-NAME}.
   {&OPEN-QUERY-{&BROWSE-NAME}}


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-limpa-browse B-table-Win 
PROCEDURE pi-limpa-browse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN c-item = ''
           c-ref  = ''
           c-lote = ''.

    RUN local-open-query-cases.

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
    DEF INPUT PARAMETER p-item LIKE ITEM.it-codigo.
    DEF INPUT PARAMETER p-ref  LIKE ref-item.cod-refer.
    DEF INPUT PARAMETER p-lote LIKE saldo-estoq.lote.

    ASSIGN c-item = p-item
           c-ref  = p-ref
           c-lote = p-lote.

    RUN local-open-query-cases.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-retorna-filtro B-table-Win 
PROCEDURE pi-retorna-filtro :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF OUTPUT PARAMETER p-imp    AS LOG.
DEF OUTPUT PARAMETER p-prod   AS LOG.
DEF OUTPUT PARAMETER p-est    AS LOG.
DEF OUTPUT PARAMETER p-res    AS LOG.
DEF OUTPUT PARAMETER p-fat    AS LOG.
DEF OUTPUT PARAMETER p-reproc AS LOG.

ASSIGN p-imp    = to-imp
       p-prod   = to-prod
       p-est    = to-est
       p-res    = to-res
       p-fat    = to-fat
       p-reproc = to-reproc.

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
  {src/adm/template/sndkycas.i "it-codigo" "ob-etiqueta" "it-codigo"}
  {src/adm/template/sndkycas.i "cod-refer" "ob-etiqueta" "cod-refer"}

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
  {src/adm/template/snd-list.i "ob-etiqueta"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-situacao B-table-Win 
FUNCTION fn-situacao RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

      {esinc/i-dsrb.i ob-etiqueta.situacao ob-etiqueta.situacao c-situacao} 

  RETURN c-situacao.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

