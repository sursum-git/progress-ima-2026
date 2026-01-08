&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          mgcad            PROGRESS
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
{include/i-prgvrs.i B99XX999 9.99.99.999}

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
&Scoped-define INTERNAL-TABLES ref-item ref-item-ext

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br-table                                      */
&Scoped-define FIELDS-IN-QUERY-br-table ref-item.it-codigo ~
ref-item.cod-refer ref-item-ext.qtd-prog ref-item-ext.qtd-proc ~
ref-item-ext.qtd-pron 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-table 
&Scoped-define QUERY-STRING-br-table FOR EACH ref-item WHERE ~{&KEY-PHRASE} ~
      AND ref-item.it-codigo >= fi-item-ini ~
 AND ref-item.it-codigo <= fi-item-fim ~
 AND ref-item.cod-refer >= fi-ref-ini ~
 AND ref-item.cod-refer <= fi-ref-fim NO-LOCK, ~
      EACH ref-item-ext WHERE ref-item-ext.it-codigo = ref-item.it-codigo ~
  AND ref-item-ext.cod-refer = ref-item.cod-refer NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-table OPEN QUERY br-table FOR EACH ref-item WHERE ~{&KEY-PHRASE} ~
      AND ref-item.it-codigo >= fi-item-ini ~
 AND ref-item.it-codigo <= fi-item-fim ~
 AND ref-item.cod-refer >= fi-ref-ini ~
 AND ref-item.cod-refer <= fi-ref-fim NO-LOCK, ~
      EACH ref-item-ext WHERE ref-item-ext.it-codigo = ref-item.it-codigo ~
  AND ref-item-ext.cod-refer = ref-item.cod-refer NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-table ref-item ref-item-ext
&Scoped-define FIRST-TABLE-IN-QUERY-br-table ref-item
&Scoped-define SECOND-TABLE-IN-QUERY-br-table ref-item-ext


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi-item-ini fi-item-fim fi-ref-ini ~
fi-ref-fim bt-confirma br-table IMAGE-1 IMAGE-2 IMAGE-3 IMAGE-4 
&Scoped-Define DISPLAYED-OBJECTS fi-item-ini fi-item-fim fi-ref-ini ~
fi-ref-fim 

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
it-codigo|y|y|mgcad.ref-item.it-codigo
cod-refer|y|y|mgcad.ref-item.cod-refer
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "it-codigo,cod-refer",
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

DEFINE VARIABLE fi-item-fim AS CHARACTER FORMAT "X(16)":U 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88 NO-UNDO.

DEFINE VARIABLE fi-item-ini AS CHARACTER FORMAT "X(16)":U 
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ref-fim AS CHARACTER FORMAT "X(10)":U INITIAL "ZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ref-ini AS CHARACTER FORMAT "X(256)":U 
     LABEL "Referˆncia" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\ii-fir":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-2
     FILENAME "image\ii-las":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-3
     FILENAME "image\ii-fir":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-4
     FILENAME "image\ii-las":U
     SIZE 2.86 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-table FOR 
      ref-item, 
      ref-item-ext SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-table B-table-Win _STRUCTURED
  QUERY br-table NO-LOCK DISPLAY
      ref-item.it-codigo FORMAT "x(16)":U WIDTH 12.43
      ref-item.cod-refer COLUMN-LABEL "Referencia" FORMAT "x(8)":U
            WIDTH 11.43
      ref-item-ext.qtd-prog FORMAT ">,>>>,>>9":U
      ref-item-ext.qtd-proc FORMAT ">,>>>,>>9":U
      ref-item-ext.qtd-pron FORMAT ">,>>>,>>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 86 BY 9.46
         FONT 1 ROW-HEIGHT-CHARS .67.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fi-item-ini AT ROW 1.13 COL 11.86 COLON-ALIGNED
     fi-item-fim AT ROW 1.13 COL 37.86 COLON-ALIGNED NO-LABEL
     fi-ref-ini AT ROW 2.08 COL 12 COLON-ALIGNED
     fi-ref-fim AT ROW 2.08 COL 38 COLON-ALIGNED NO-LABEL
     bt-confirma AT ROW 2.25 COL 82
     br-table AT ROW 3.46 COL 1
     IMAGE-1 AT ROW 1.13 COL 30.14
     IMAGE-2 AT ROW 1.13 COL 36.43
     IMAGE-3 AT ROW 2.08 COL 30.29
     IMAGE-4 AT ROW 2.08 COL 36.57
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
         HEIGHT             = 12.13
         WIDTH              = 87.
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
   NOT-VISIBLE Size-to-Fit L-To-R                                       */
/* BROWSE-TAB br-table bt-confirma F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-table
/* Query rebuild information for BROWSE br-table
     _TblList          = "mgcad.ref-item,espec.ref-item-ext WHERE mgcad.ref-item ..."
     _Options          = "NO-LOCK INDEXED-REPOSITION KEY-PHRASE SORTBY-PHRASE"
     _Where[1]         = "mgcad.ref-item.it-codigo >= fi-item-ini
 AND mgcad.ref-item.it-codigo <= fi-item-fim
 AND mgcad.ref-item.cod-refer >= fi-ref-ini
 AND mgcad.ref-item.cod-refer <= fi-ref-fim"
     _JoinCode[2]      = "espec.ref-item-ext.it-codigo = mgcad.ref-item.it-codigo
  AND espec.ref-item-ext.cod-refer = mgcad.ref-item.cod-refer"
     _FldNameList[1]   > mgcad.ref-item.it-codigo
"mgcad.ref-item.it-codigo" ? ? "character" ? ? ? ? ? ? no ? no no "12.43" yes no no "U" "" ""
     _FldNameList[2]   > mgcad.ref-item.cod-refer
"mgcad.ref-item.cod-refer" "Referencia" ? "character" ? ? ? ? ? ? no ? no no "11.43" yes no no "U" "" ""
     _FldNameList[3]   = espec.ref-item-ext.qtd-prog
     _FldNameList[4]   = espec.ref-item-ext.qtd-proc
     _FldNameList[5]   = espec.ref-item-ext.qtd-pron
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
  assign input frame {&frame-name} fi-item-ini fi-item-fim
         input frame {&frame-name} fi-ref-ini fi-ref-fim.
  RUN dispatch IN THIS-PROCEDURE ('open-query':U).
  apply 'value-changed':U to {&browse-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-item-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-item-ini B-table-Win
ON LEAVE OF fi-item-ini IN FRAME F-Main /* Item */
DO:
  ASSIGN fi-item-fim:SCREEN-VALUE IN FRAME {&FRAME-NAME} = SELF:SCREEN-VALUE.
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
       &Scope KEY-PHRASE ref-item.it-codigo eq key-value
       {&OPEN-QUERY-{&BROWSE-NAME}}
    END. /* it-codigo */
    WHEN 'cod-refer':U THEN DO:
       &Scope KEY-PHRASE ref-item.cod-refer eq key-value
       {&OPEN-QUERY-{&BROWSE-NAME}}
    END. /* cod-refer */
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

    if  avail mgcad.ref-item then do:
        case p-campo:
            when "it-codigo" then
                assign p-valor = string(ref-item.it-codigo).
            when "cod-refer" then
                assign p-valor = string(ref-item.cod-refer).
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
  {src/adm/template/sndkycas.i "it-codigo" "ref-item" "it-codigo"}
  {src/adm/template/sndkycas.i "cod-refer" "ref-item" "cod-refer"}

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
  {src/adm/template/snd-list.i "ref-item"}
  {src/adm/template/snd-list.i "ref-item-ext"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "RetornaValorCampo" B-table-Win _INLINE
/* Actions: ? ? ? ? support/brwrtval.p */
/* Procedure desativada */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

