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
{include/i-prgvrs.i B05ES049 2.04.00.000}

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
define variable c-lista-valor as character init '':U no-undo.

DEF VAR c-cod-estabel AS CHAR.

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

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ob-etiqueta

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br-table                                      */
&Scoped-define FIELDS-IN-QUERY-br-table ob-etiqueta.cod-estabel ~
ob-etiqueta.num-etiqueta ob-etiqueta.localizacao ob-etiqueta.acondic ~
ob-etiqueta.it-codigo ob-etiqueta.cod-refer ob-etiqueta.quantidade 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-table 
&Scoped-define QUERY-STRING-br-table FOR EACH ob-etiqueta WHERE ~{&KEY-PHRASE} ~
      AND ob-etiqueta.situacao = rs-situacao and ~
espec.ob-etiqueta.num-etiqueta >= fi-ini-num-etiqueta ~
 AND ob-etiqueta.num-etiqueta <= fi-fin-num-etiqueta ~
 NO-LOCK ~
    BY ob-etiqueta.num-etiqueta
&Scoped-define OPEN-QUERY-br-table OPEN QUERY br-table FOR EACH ob-etiqueta WHERE ~{&KEY-PHRASE} ~
      AND ob-etiqueta.situacao = rs-situacao and ~
espec.ob-etiqueta.num-etiqueta >= fi-ini-num-etiqueta ~
 AND ob-etiqueta.num-etiqueta <= fi-fin-num-etiqueta ~
 NO-LOCK ~
    BY ob-etiqueta.num-etiqueta.
&Scoped-define TABLES-IN-QUERY-br-table ob-etiqueta
&Scoped-define FIRST-TABLE-IN-QUERY-br-table ob-etiqueta


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS IMAGE-1 IMAGE-2 rs-situacao ~
fi-ini-num-etiqueta bt-confirma fi-fin-num-etiqueta br-table 
&Scoped-Define DISPLAYED-OBJECTS rs-situacao fi-ini-num-etiqueta ~
fi-fin-num-etiqueta 

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
it-codigo|y|y|espec.ob-etiqueta.it-codigo
cod-refer||y|espec.ob-etiqueta.cod-refer
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "it-codigo",
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


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-confirma 
     IMAGE-UP FILE "image\im-sav":U
     LABEL "Button 1" 
     SIZE 5.14 BY 1.

DEFINE VARIABLE fi-fin-num-etiqueta AS INTEGER FORMAT "999999999" INITIAL 999999999 
     VIEW-AS FILL-IN 
     SIZE 10.14 BY .88 TOOLTIP "N£mero da Etiqueta Final"
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-ini-num-etiqueta AS INTEGER FORMAT "999999999" INITIAL 0 
     LABEL "Etiqueta" 
     VIEW-AS FILL-IN 
     SIZE 10.14 BY .88 TOOLTIP "N£mero da Etiqueta Inicial"
     FONT 1 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\ii-fir":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-2
     FILENAME "image\ii-las":U
     SIZE 2.86 BY 1.

DEFINE VARIABLE rs-situacao AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Impressa", 1,
"Em Produ‡Æo", 2,
"Estoque", 3,
"Reservada", 4,
"Faturada", 5,
"Consumido", 6
     SIZE 75 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-table FOR 
      ob-etiqueta SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-table B-table-Win _STRUCTURED
  QUERY br-table NO-LOCK DISPLAY
      ob-etiqueta.cod-estabel FORMAT "x(3)":U WIDTH 3
      ob-etiqueta.num-etiqueta FORMAT "999999999":U WIDTH 12
      ob-etiqueta.localizacao FORMAT "X(8)":U WIDTH 8
      ob-etiqueta.acondic FORMAT "X(15)":U
      ob-etiqueta.it-codigo FORMAT "X(16)":U
      ob-etiqueta.cod-refer FORMAT "X(8)":U
      ob-etiqueta.quantidade FORMAT ">>>,>>9.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 87 BY 11.75
         FONT 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     rs-situacao AT ROW 1.25 COL 12 NO-LABEL WIDGET-ID 8
     fi-ini-num-etiqueta AT ROW 2.25 COL 10 COLON-ALIGNED
     bt-confirma AT ROW 2.25 COL 82
     fi-fin-num-etiqueta AT ROW 2.29 COL 31 COLON-ALIGNED NO-LABEL
     br-table AT ROW 3.5 COL 1
     "Situa‡Æo:" VIEW-AS TEXT
          SIZE 6.72 BY .67 AT ROW 1.33 COL 4.86 WIDGET-ID 12
     IMAGE-1 AT ROW 2.33 COL 22.57
     IMAGE-2 AT ROW 2.29 COL 29.57
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
         HEIGHT             = 14.38
         WIDTH              = 87.29.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{include/c-brwzoo.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit L-To-R                            */
/* BROWSE-TAB br-table fi-fin-num-etiqueta F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-table
/* Query rebuild information for BROWSE br-table
     _TblList          = "espec.ob-etiqueta"
     _Options          = "NO-LOCK KEY-PHRASE"
     _OrdList          = "espec.ob-etiqueta.num-etiqueta|yes"
     _Where[1]         = "espec.ob-etiqueta.situacao = rs-situacao and
espec.ob-etiqueta.num-etiqueta >= fi-ini-num-etiqueta
 AND espec.ob-etiqueta.num-etiqueta <= fi-fin-num-etiqueta
"
     _FldNameList[1]   > espec.ob-etiqueta.cod-estabel
"ob-etiqueta.cod-estabel" ? ? "character" ? ? ? ? ? ? no ? no no "3" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > espec.ob-etiqueta.num-etiqueta
"ob-etiqueta.num-etiqueta" ? ? "integer" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > espec.ob-etiqueta.localizacao
"ob-etiqueta.localizacao" ? "X(8)" "character" ? ? ? ? ? ? no ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   = espec.ob-etiqueta.acondic
     _FldNameList[5]   = espec.ob-etiqueta.it-codigo
     _FldNameList[6]   = espec.ob-etiqueta.cod-refer
     _FldNameList[7]   = espec.ob-etiqueta.quantidade
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
    RUN New-State('DblClick':U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table B-table-Win
ON ROW-ENTRY OF br-table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}
  
  run new-state('New-Line|':U + string(rowid({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}))).
  run seta-valor.
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
  run new-state('New-Line|':U + string(rowid({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}))).
  run new-state('Value-Changed|':U + string(this-procedure)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-confirma
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-confirma B-table-Win
ON CHOOSE OF bt-confirma IN FRAME F-Main /* Button 1 */
DO:
  assign input frame {&frame-name} rs-situacao fi-ini-num-etiqueta fi-fin-num-etiqueta.
  RUN dispatch IN THIS-PROCEDURE ('open-query':U).
  apply 'value-changed':U to {&browse-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
FIND FIRST ped-venda NO-LOCK NO-ERROR.
ASSIGN c-cod-estabel = ped-venda.cod-estabel.
ASSIGN rs-situacao = 3.

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
  DEF VAR key-value AS CHAR NO-UNDO.

  DEF VAR Filter-Value AS CHAR NO-UNDO.

  /* Copy 'Filter-Attributes' into local variables. */
  RUN get-attribute ('Filter-Value':U).
  Filter-Value = RETURN-VALUE.
  /* Look up the current key-value. */
  RUN get-attribute ('Key-Value':U).
  key-value = RETURN-VALUE.

  /* Find the current record using the current Key-Name. */
  RUN get-attribute ('Key-Name':U).
  CASE RETURN-VALUE:
    WHEN 'it-codigo':U THEN DO:
       &Scope KEY-PHRASE ob-etiqueta.it-codigo eq key-value
       {&OPEN-QUERY-{&BROWSE-NAME}}
    END. /* it-codigo */
    OTHERWISE DO:
       &Scope KEY-PHRASE TRUE
       {&OPEN-QUERY-{&BROWSE-NAME}}
    END. /* OTHERWISE...*/
  END CASE.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-retorna-valor B-table-Win 
PROCEDURE pi-retorna-valor :
DEFINE INPUT PARAMETER P-CAMPO AS CHARACTER NO-UNDO.

    DEFINE VARIABLE P-VALOR AS CHAR INIT "" NO-UNDO.

    if  avail espec.ob-etiqueta then do:
        case p-campo:
            when "num-etiqueta" then
                assign p-valor = string(ob-etiqueta.num-etiqueta).
            when "localizacao" then
                assign p-valor = string(ob-etiqueta.localizacao).
            when "nuance" then
                assign p-valor = string(ob-etiqueta.nuance).
            when "acondic" then
                assign p-valor = string(ob-etiqueta.acondic).
            when "it-codigo" then
                assign p-valor = string(ob-etiqueta.it-codigo).
            when "cod-refer" then
                assign p-valor = string(ob-etiqueta.cod-refer).
            when "nr-ob" then
                assign p-valor = string(ob-etiqueta.nr-ob).
            when "nr-sequencia" then
                assign p-valor = string(ob-etiqueta.nr-sequencia).
            when "quantidade" then
                assign p-valor = string(ob-etiqueta.quantidade).
        end.
    end.
    return p-valor.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE seta-valor B-table-Win 
PROCEDURE seta-valor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "RetornaValorCampo" B-table-Win _INLINE
/* Actions: ? ? ? ? support/brwrtval.p */
/* Procedure desativada */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

