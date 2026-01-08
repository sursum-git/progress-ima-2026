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
{include/i-prgvrs.i B06di154 2.06.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
DEFINE BUFFER empresa FOR mgcad.empresa.

/* Local Variable Definitions ---                                       */
DEF TEMP-TABLE tt-lisa-log-integr LIKE lisa-log-integr
    INDEX indice-1 data hora chave.

/* Parameters Definitions ---                                           */
DEF NEW GLOBAL SHARED VAR c-seg-usuario      AS CHAR NO-UNDO.

/* Local Variable Definitions ---                                       */
DEF VAR c-erro AS CHAR.
DEF VAR c-hora AS CHAR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-log

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-lisa-log-integr

/* Definitions for BROWSE br-log                                        */
&Scoped-define FIELDS-IN-QUERY-br-log tt-lisa-log-integr.cod-trans tt-lisa-log-integr.data STRING(tt-lisa-log-integr.hora,"HH:MM:SS") @ c-hora tt-lisa-log-integr.narrativa   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-log   
&Scoped-define SELF-NAME br-log
&Scoped-define QUERY-STRING-br-log FOR EACH tt-lisa-log-integr NO-LOCK
&Scoped-define OPEN-QUERY-br-log OPEN QUERY {&SELF-NAME} FOR EACH tt-lisa-log-integr NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-log tt-lisa-log-integr
&Scoped-define FIRST-TABLE-IN-QUERY-br-log tt-lisa-log-integr


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-br-log}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-log cb-cod-trans fi-data-ini tg-integrado ~
tg-erro fi-data-fin bt-sel fi-nr-pedcli-ini fi-nr-pedcli-fin RECT-2 RECT-4 ~
RECT-96 RECT-97 IMAGE-1 IMAGE-2 IMAGE-3 IMAGE-4 
&Scoped-Define DISPLAYED-OBJECTS cb-cod-trans fi-data-ini tg-integrado ~
tg-erro fi-data-fin fi-nr-pedcli-ini fi-nr-pedcli-fin 

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
nr-ord-produ||y|mgmov.ped-item.nr-ord-produ
nr-programa||y|mgmov.ped-item.nr-programa
it-codigo||y|mgmov.ped-item.it-codigo
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "nr-ord-produ,nr-programa,it-codigo"':U).

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


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-sel AUTO-GO 
     IMAGE-UP FILE "image\toolbar\im-zoo.bmp":U
     IMAGE-INSENSITIVE FILE "image\toolbar\ii-zoo.bmp":U
     LABEL "OK" 
     SIZE 6 BY 2.75 TOOLTIP "Processa Dados".

DEFINE VARIABLE cb-cod-trans AS CHARACTER FORMAT "X(256)":U INITIAL "Todas" 
     LABEL "Transa‡ao" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Todas","NotaRemessa","PackingList","NotaAvulsa","PackingAvulso","ISF","RetornoISF","RemessaNotaVenda","NotaRetorno","ConfEtiquetas" 
     DROP-DOWN-LIST
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE fi-data-fin AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-data-ini AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Per¡odo" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nr-pedcli-fin AS CHARACTER FORMAT "x(12)" INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 TOOLTIP "Pedido final." NO-UNDO.

DEFINE VARIABLE fi-nr-pedcli-ini AS CHARACTER FORMAT "x(12)" 
     LABEL "Pedido":R17 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 TOOLTIP "Pedido inicial." NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-3
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-4
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 66 BY 1.25
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 142 BY 22.75
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-96
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 140 BY 3.5.

DEFINE RECTANGLE RECT-97
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 28 BY 2.75.

DEFINE VARIABLE tg-erro AS LOGICAL INITIAL yes 
     LABEL "ERRO na Integra‡ao" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .67
     FGCOLOR 12  NO-UNDO.

DEFINE VARIABLE tg-integrado AS LOGICAL INITIAL yes 
     LABEL "Integrado com Sucesso" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .67
     FGCOLOR 2  NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-log FOR 
      tt-lisa-log-integr SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-log
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-log B-table-Win _FREEFORM
  QUERY br-log NO-LOCK DISPLAY
      tt-lisa-log-integr.cod-trans COLUMN-LABEL "Transa‡Æo" FORMAT "x(15)":U
      tt-lisa-log-integr.data FORMAT "99/99/9999":U
      STRING(tt-lisa-log-integr.hora,"HH:MM:SS") @ c-hora COLUMN-LABEL "Hora" FORMAT "x(8)":U
            WIDTH 6
      tt-lisa-log-integr.narrativa FORMAT "x(200)":U WIDTH 500
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 140 BY 18.5
         FONT 1
         TITLE "Hist¢rico de Integra‡äes" ROW-HEIGHT-CHARS .67.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br-log AT ROW 5 COL 2 WIDGET-ID 100
     cb-cod-trans AT ROW 1.5 COL 23 COLON-ALIGNED WIDGET-ID 470
     fi-data-ini AT ROW 2.5 COL 23 COLON-ALIGNED WIDGET-ID 476
     tg-integrado AT ROW 2.33 COL 88 WIDGET-ID 214
     tg-erro AT ROW 3.33 COL 88 WIDGET-ID 472
     fi-data-fin AT ROW 2.5 COL 43 COLON-ALIGNED NO-LABEL WIDGET-ID 482
     bt-sel AT ROW 1.75 COL 135 WIDGET-ID 96
     fi-nr-pedcli-ini AT ROW 3.5 COL 23 COLON-ALIGNED HELP
          "N£mero do pedido do cliente" WIDGET-ID 488
     fi-nr-pedcli-fin AT ROW 3.5 COL 43 COLON-ALIGNED NO-LABEL WIDGET-ID 486
     " Sele‡Æo" VIEW-AS TEXT
          SIZE 8 BY .54 AT ROW 1.08 COL 4.43 WIDGET-ID 444
     " Situa‡Æo" VIEW-AS TEXT
          SIZE 7 BY .75 AT ROW 1.46 COL 84.86 WIDGET-ID 114
     RECT-2 AT ROW 13 COL 2 WIDGET-ID 4
     RECT-4 AT ROW 1 COL 1 WIDGET-ID 296
     RECT-96 AT ROW 1.25 COL 2 WIDGET-ID 442
     RECT-97 AT ROW 1.75 COL 83 WIDGET-ID 466
     IMAGE-1 AT ROW 2.5 COL 37 WIDGET-ID 478
     IMAGE-2 AT ROW 2.5 COL 42 WIDGET-ID 480
     IMAGE-3 AT ROW 3.5 COL 37 WIDGET-ID 490
     IMAGE-4 AT ROW 3.5 COL 42 WIDGET-ID 484
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
         HEIGHT             = 22.96
         WIDTH              = 142.14.
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
/* BROWSE-TAB br-log 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-log
/* Query rebuild information for BROWSE br-log
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-lisa-log-integr NO-LOCK
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "espec.lisa-log-integr.data|yes,espec.lisa-log-integr.hora|yes,espec.lisa-log-integr.chave|yes"
     _Where[1]         = "(lisa-log-integr.cod-trans = cb-cod-trans OR cb-cod-trans = 'Todas' ) AND
lisa-log-integr.data >= fi-data-ini AND
lisa-log-integr.data <= fi-data-fin AND
lisa-log-integr.chave >= fi-nr-pedcli-ini AND
lisa-log-integr.chave <= fi-nr-pedcli-fin AND
LOOKUP(STRING(lisa-log-integr.log-erro),c-erro) > 0 
"
     _Query            is OPENED
*/  /* BROWSE br-log */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME bt-sel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-sel B-table-Win
ON CHOOSE OF bt-sel IN FRAME F-Main /* OK */
DO:
    
    ASSIGN INPUT FRAME {&FRAME-NAME} cb-cod-trans 
                                     fi-data-ini fi-data-fin 
                                     fi-nr-pedcli-ini fi-nr-pedcli-fin
                                     tg-integrado tg-erro.

    ASSIGN c-erro = ''.
    IF tg-integrado THEN
       ASSIGN c-erro = 'NO'.
    IF tg-erro THEN
       ASSIGN c-erro = IF c-erro = '' 
                      THEN 'YES' ELSE c-erro + ",YES".

    EMPTY TEMP-TABLE tt-lisa-log-integr.
    FOR EACH lisa-log-integr WHERE 
             (lisa-log-integr.cod-trans = cb-cod-trans OR cb-cod-trans = 'Todas' ) AND
             lisa-log-integr.data >= fi-data-ini AND
             lisa-log-integr.data <= fi-data-fin AND
             lisa-log-integr.chave >= fi-nr-pedcli-ini AND
             lisa-log-integr.chave <= fi-nr-pedcli-fin AND
             LOOKUP(STRING(lisa-log-integr.log-erro),c-erro) > 0 
             NO-LOCK.
        CREATE tt-lisa-log-integr.
        BUFFER-COPY lisa-log-integr TO tt-lisa-log-integr.
    END.

   {&OPEN-QUERY-br-log}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-cod-trans
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-cod-trans B-table-Win
ON VALUE-CHANGED OF cb-cod-trans IN FRAME F-Main /* Transa‡ao */
DO:
  APPLY 'CHOOSE' TO bt-sel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-nr-pedcli-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nr-pedcli-ini B-table-Win
ON LEAVE OF fi-nr-pedcli-ini IN FRAME F-Main /* Pedido */
DO:
  IF SELF:SCREEN-VALUE <> '' THEN
     ASSIGN fi-nr-pedcli-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-erro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-erro B-table-Win
ON VALUE-CHANGED OF tg-erro IN FRAME F-Main /* ERRO na Integra‡ao */
DO:
   APPLY 'CHOOSE' TO bt-sel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-integrado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-integrado B-table-Win
ON VALUE-CHANGED OF tg-integrado IN FRAME F-Main /* Integrado com Sucesso */
DO:
  APPLY 'CHOOSE' TO bt-sel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-log
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize B-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-processa B-table-Win 
PROCEDURE pi-processa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}.
    END.

    ASSIGN fi-data-ini:SCREEN-VALUE = STRING(TODAY)
           fi-data-fin:SCREEN-VALUE = STRING(TODAY).

    APPLY 'CHOOSE' TO bt-sel.
    
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
  {src/adm/template/sndkycas.i "nr-ord-produ" "ped-item" "nr-ord-produ"}
  {src/adm/template/sndkycas.i "nr-programa" "ped-item" "nr-programa"}
  {src/adm/template/sndkycas.i "it-codigo" "ped-item" "it-codigo"}

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
  {src/adm/template/snd-list.i "tt-lisa-log-integr"}

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

