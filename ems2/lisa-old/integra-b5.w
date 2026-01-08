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

/* Parameters Definitions ---                                           */
DEF NEW GLOBAL SHARED VAR c-tab-lisa AS CHAR NO-UNDO.

/* Local Variable Definitions ---                                       */
    
DEF TEMP-TABLE tt-cortes 
    FIELD num-rolo-imp      AS INT
    FIELD num-etiqueta      LIKE ob-etiqueta.num-etiqueta
    FIELD acao              AS CHAR
    FIELD nr-container      AS INT
    FIELD it-codigo         AS CHAR
    FIELD cod-refer         AS CHAR
    FIELD quantidade        LIKE ob-etiqueta.quantidade
    FIELD ind-situacao      AS   INTEGER
    FIELD visualiza         AS LOG INIT YES
    INDEX idx1 nr-container it-codigo cod-refer num-rolo-imp.

DEF BUFFER b-tt-cortes FOR tt-cortes.

/* Local Variable Definitions ---                                       */
DEF VAR i-cor AS INT.
DEF VAR c-tabela AS CHAR INIT "ISF".
DEF VAR c-status AS CHAR.

DEF VAR h-acomp     AS HANDLE  NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-cortes

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-cortes

/* Definitions for BROWSE br-cortes                                     */
&Scoped-define FIELDS-IN-QUERY-br-cortes tt-cortes.nr-container tt-cortes.it-codigo tt-cortes.cod-refer tt-cortes.num-rolo-imp tt-cortes.num-etiqueta tt-cortes.acao tt-cortes.quantidade   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-cortes   
&Scoped-define SELF-NAME br-cortes
&Scoped-define QUERY-STRING-br-cortes FOR EACH tt-cortes WHERE                                  tt-cortes.visualiza = YES NO-LOCK
&Scoped-define OPEN-QUERY-br-cortes OPEN QUERY {&SELF-NAME} FOR EACH tt-cortes WHERE                                  tt-cortes.visualiza = YES NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-cortes tt-cortes
&Scoped-define FIRST-TABLE-IN-QUERY-br-cortes tt-cortes


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-br-cortes}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi-nr-container-ini fi-nr-container-fin ~
fi-it-codigo-ini fi-it-codigo-fin fi-rolo-imp-ini fi-rolo-imp-fin ~
fi-dt-inicio tg-aguardando tg-integrado tg-erro bt-sel br-cortes bt-refresh ~
bt-integra FILL-IN-12 FILL-IN-8 FILL-IN-16 RECT-2 RECT-4 RECT-96 RECT-97 ~
IMAGE-108 IMAGE-109 IMAGE-3 IMAGE-4 IMAGE-110 IMAGE-111 
&Scoped-Define DISPLAYED-OBJECTS fi-nr-container-ini fi-nr-container-fin ~
fi-it-codigo-ini fi-it-codigo-fin fi-rolo-imp-ini fi-rolo-imp-fin ~
fi-dt-inicio tg-aguardando tg-integrado tg-erro FILL-IN-12 FILL-IN-8 ~
FILL-IN-16 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 bt-refresh bt-integra 

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

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-situacao B-table-Win 
FUNCTION fn-situacao RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-integra AUTO-GO 
     IMAGE-UP FILE "image/im-integra.jpg":U
     LABEL "" 
     SIZE 5 BY 1.42 TOOLTIP "Integra Documentos Selecionados"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-refresh AUTO-GO 
     IMAGE-UP FILE "image/im-autom.bmp":U
     LABEL "" 
     SIZE 5 BY 1.42 TOOLTIP "Atualiza Dados"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-sel AUTO-GO 
     IMAGE-UP FILE "image/im-cq.bmp":U
     LABEL "OK" 
     SIZE 9 BY 3.25 TOOLTIP "Processa Dados".

DEFINE VARIABLE fi-dt-inicio AS DATE FORMAT "99/99/9999":U INITIAL 01/01/23 
     LABEL "∑ partir de" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE fi-it-codigo-fin AS CHARACTER FORMAT "x(12)" INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 TOOLTIP "Item final" NO-UNDO.

DEFINE VARIABLE fi-it-codigo-ini AS CHARACTER FORMAT "x(12)" 
     LABEL "Item":R17 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 TOOLTIP "Item inicial" NO-UNDO.

DEFINE VARIABLE fi-nr-container-fin AS INTEGER FORMAT ">>>>>>9" INITIAL 999999 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nr-container-ini AS INTEGER FORMAT ">>>>>>9" INITIAL 0 
     LABEL "Container" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 NO-UNDO.

DEFINE VARIABLE fi-rolo-imp-fin AS INTEGER FORMAT ">,>>>,>>9" INITIAL 999999 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE fi-rolo-imp-ini AS INTEGER FORMAT ">,>>>,>>9" INITIAL 0 
     LABEL "Rolo Imp." 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE VARIABLE FILL-IN-12 AS CHARACTER FORMAT "X(256)":U INITIAL "Processar" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .79
     BGCOLOR 15 FGCOLOR 9 FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-16 AS CHARACTER FORMAT "X(256)":U INITIAL "Finalizado" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .79
     FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-8 AS CHARACTER FORMAT "X(256)":U INITIAL "Erro na Integraá∆o" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .79
     BGCOLOR 15 FGCOLOR 12 FONT 17 NO-UNDO.

DEFINE IMAGE IMAGE-108
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-109
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-110
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-111
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
     SIZE 142 BY 23
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-96
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 140 BY 4.

DEFINE RECTANGLE RECT-97
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 28 BY 3.25.

DEFINE VARIABLE tg-aguardando AS LOGICAL INITIAL yes 
     LABEL "Aguardando Integraá∆o" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .67
     FGCOLOR 12  NO-UNDO.

DEFINE VARIABLE tg-erro AS LOGICAL INITIAL yes 
     LABEL "Erro na Integraá∆o" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .67 NO-UNDO.

DEFINE VARIABLE tg-integrado AS LOGICAL INITIAL no 
     LABEL "Integrado" 
     VIEW-AS TOGGLE-BOX
     SIZE 10.86 BY .67 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-cortes FOR 
      tt-cortes SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-cortes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-cortes B-table-Win _FREEFORM
  QUERY br-cortes NO-LOCK DISPLAY
      tt-cortes.nr-container  COLUMN-LABEL "Container"          WIDTH 10
      tt-cortes.it-codigo     COLUMN-LABEL "Item"               WIDTH 10
      tt-cortes.cod-refer     COLUMN-LABEL "Ref" 
      tt-cortes.num-rolo-imp  COLUMN-LABEL "RoloImp"      WIDTH 10
      tt-cortes.num-etiqueta  COLUMN-LABEL "NumEtiqueta"        WIDTH 12
      tt-cortes.acao          COLUMN-LABEL "Ac∆o"               WIDTH 10
      tt-cortes.quantidade    COLUMN-LABEL "Quantidade"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 140 BY 16.5
         FONT 1
         TITLE "Cortes e Ajustes" ROW-HEIGHT-CHARS .67.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fi-nr-container-ini AT ROW 1.75 COL 20 COLON-ALIGNED HELP
          "Codigo do ÷tem no EMS" WIDGET-ID 474
     fi-nr-container-fin AT ROW 1.75 COL 41.14 COLON-ALIGNED HELP
          "Codigo do ÷tem no EMS" NO-LABEL WIDGET-ID 472
     fi-it-codigo-ini AT ROW 2.75 COL 20 COLON-ALIGNED HELP
          "N£mero do pedido do cliente" WIDGET-ID 480
     fi-it-codigo-fin AT ROW 2.75 COL 41.14 COLON-ALIGNED NO-LABEL WIDGET-ID 478
     fi-rolo-imp-ini AT ROW 3.75 COL 20 COLON-ALIGNED HELP
          "Codigo do ÷tem no EMS" WIDGET-ID 8
     fi-rolo-imp-fin AT ROW 3.75 COL 41.14 COLON-ALIGNED HELP
          "Codigo do ÷tem no EMS" NO-LABEL WIDGET-ID 6
     fi-dt-inicio AT ROW 2.75 COL 66 COLON-ALIGNED WIDGET-ID 492
     tg-aguardando AT ROW 2.54 COL 104 WIDGET-ID 174
     tg-integrado AT ROW 3.38 COL 104 WIDGET-ID 214
     tg-erro AT ROW 4.21 COL 104 WIDGET-ID 160
     bt-sel AT ROW 1.75 COL 132 WIDGET-ID 96
     br-cortes AT ROW 5.5 COL 2 WIDGET-ID 100
     bt-refresh AT ROW 22.25 COL 2 WIDGET-ID 420
     bt-integra AT ROW 22.25 COL 7 WIDGET-ID 468
     FILL-IN-12 AT ROW 22.25 COL 14 COLON-ALIGNED NO-LABEL WIDGET-ID 410
     FILL-IN-8 AT ROW 22.25 COL 44 COLON-ALIGNED NO-LABEL WIDGET-ID 404
     FILL-IN-16 AT ROW 22.25 COL 28 COLON-ALIGNED NO-LABEL WIDGET-ID 490
     " Seleá∆o" VIEW-AS TEXT
          SIZE 8 BY .54 AT ROW 1.08 COL 4.43 WIDGET-ID 444
     " Situaá∆o" VIEW-AS TEXT
          SIZE 7 BY .75 AT ROW 1.5 COL 99.86 WIDGET-ID 114
     RECT-2 AT ROW 13 COL 2 WIDGET-ID 4
     RECT-4 AT ROW 1 COL 1 WIDGET-ID 296
     RECT-96 AT ROW 1.25 COL 2 WIDGET-ID 442
     RECT-97 AT ROW 1.79 COL 98 WIDGET-ID 466
     IMAGE-108 AT ROW 3.75 COL 35.14 WIDGET-ID 22
     IMAGE-109 AT ROW 3.75 COL 39.14 WIDGET-ID 464
     IMAGE-3 AT ROW 2.75 COL 35.14 WIDGET-ID 482
     IMAGE-4 AT ROW 2.75 COL 39.14 WIDGET-ID 484
     IMAGE-110 AT ROW 1.75 COL 35.14 WIDGET-ID 34
     IMAGE-111 AT ROW 1.75 COL 39.14 WIDGET-ID 36
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
         HEIGHT             = 23.13
         WIDTH              = 142.72.
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
/* BROWSE-TAB br-cortes bt-sel F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON bt-integra IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON bt-refresh IN FRAME F-Main
   1                                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-cortes
/* Query rebuild information for BROWSE br-cortes
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-cortes WHERE
                                 tt-cortes.visualiza = YES NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-cortes */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br-cortes
&Scoped-define SELF-NAME br-cortes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-cortes B-table-Win
ON ROW-DISPLAY OF br-cortes IN FRAME F-Main /* Cortes e Ajustes */
DO:
    ASSIGN i-cor = ?.

    IF tt-cortes.ind-situacao = 1 THEN
        ASSIGN i-cor = 9.

   ASSIGN tt-cortes.num-rolo-imp:FGCOLOR IN BROWSE br-cortes = i-cor
          tt-cortes.num-etiqueta:FGCOLOR IN BROWSE br-cortes = i-cor
          tt-cortes.nr-container:FGCOLOR IN BROWSE br-cortes = i-cor 
          tt-cortes.it-codigo:FGCOLOR IN BROWSE br-cortes = i-cor    
          tt-cortes.cod-refer:FGCOLOR IN BROWSE br-cortes = i-cor    
          tt-cortes.acao:FGCOLOR IN BROWSE br-cortes = i-cor    
          tt-cortes.quantidade:FGCOLOR IN BROWSE br-cortes = i-cor.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-integra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-integra B-table-Win
ON CHOOSE OF bt-integra IN FRAME F-Main
DO:
    ASSIGN c-tab-lisa = 'ConfEtiquetas'.
    RUN lisa/rel-giv.w.
    APPLY 'CHOOSE' TO bt-refresh.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-refresh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-refresh B-table-Win
ON CHOOSE OF bt-refresh IN FRAME F-Main
DO:
   EMPTY TEMP-TABLE tt-cortes.
   RUN pi-processa.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-sel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-sel B-table-Win
ON CHOOSE OF bt-sel IN FRAME F-Main /* OK */
DO:
   ASSIGN INPUT FRAME {&FRAME-NAME} fi-nr-container-ini fi-nr-container-fin
                                    fi-it-codigo-ini fi-it-codigo-fin
                                    fi-rolo-imp-ini fi-rolo-imp-fin
                                    fi-dt-inicio
                                    tg-aguardando tg-integrado tg-erro.

   RUN pi-processa.

   FOR EACH tt-cortes NO-LOCK.
       ASSIGN tt-cortes.visualiza = fn-situacao().
   END.

   {&OPEN-QUERY-br-cortes}
   APPLY 'VALUE-CHANGED' TO br-cortes.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-it-codigo-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo-ini B-table-Win
ON LEAVE OF fi-it-codigo-ini IN FRAME F-Main /* Item */
DO:
  ASSIGN fi-it-codigo-fin:SCREEN-VALUE = 'ZZZZZZZZZZZ'.
  IF SELF:SCREEN-VALUE <> '' THEN
     ASSIGN fi-it-codigo-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-nr-container-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nr-container-ini B-table-Win
ON LEAVE OF fi-nr-container-ini IN FRAME F-Main /* Container */
DO:
  ASSIGN fi-nr-container-fin:SCREEN-VALUE = '9999999'.
  IF SELF:SCREEN-VALUE <> '0' THEN
     ASSIGN fi-nr-container-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-rolo-imp-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-rolo-imp-ini B-table-Win
ON LEAVE OF fi-rolo-imp-ini IN FRAME F-Main /* Rolo Imp. */
DO:
    ASSIGN fi-rolo-imp-fin:SCREEN-VALUE = '999999'.
    IF SELF:SCREEN-VALUE <> '0' THEN
       ASSIGN fi-rolo-imp-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-aguardando
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-aguardando B-table-Win
ON VALUE-CHANGED OF tg-aguardando IN FRAME F-Main /* Aguardando Integraá∆o */
DO:
   APPLY 'choose' TO bt-sel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-erro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-erro B-table-Win
ON VALUE-CHANGED OF tg-erro IN FRAME F-Main /* Erro na Integraá∆o */
DO:
  APPLY 'choose' TO bt-sel.
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
    DEF VAR i-situacao AS INT.
    DEF VAR c-it-codigo AS CHAR.

    RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
    RUN pi-inicializar IN h-acomp (INPUT "Processando..").

    ASSIGN i-situacao = 1.
    IF INPUT FRAME {&FRAME-NAME} tg-integrado THEN
       ASSIGN i-situacao = 2.

    EMPTY TEMP-TABLE tt-cortes.
    FOR EACH lisa-integra WHERE
             lisa-integra.cod-trans = "ConfEtiquetas" AND
             lisa-integra.ind-situacao <= i-situacao AND
             lisa-integra.val-livre-1 >= STRING(fi-rolo-imp-ini) AND
             lisa-integra.val-livre-1 <= STRING(fi-rolo-imp-fin) AND 
             lisa-integra.val-livre-2 >= fi-it-codigo-ini AND
             lisa-integra.val-livre-2 <= fi-it-codigo-fin AND
             entry(1,lisa-integra.val-livre-5,"|") >= STRING(fi-nr-container-ini) AND
             entry(1,lisa-integra.val-livre-5,"|") <= STRING(fi-nr-container-fin) AND
             lisa-integra.dt-trans    >= fi-dt-inicio
             NO-LOCK.
             
        RUN pi-acompanhar IN  h-acomp (INPUT "Data: " + STRING(lisa-integra.dt-trans,"99/99/9999") ).

        /*
        ASSIGN c-it-codigo = IF lisa-integra.val-livre-2 <> ''
                             THEN lisa-integra.val-livre-2 
                             ELSE SUBSTR(lisa-integra.val-livre-4,4).
        */
        ASSIGN c-it-codigo = lisa-integra.val-livre-2.

        FIND tt-cortes WHERE
             tt-cortes.nr-container = INT(lisa-integra.val-livre-5) AND
             tt-cortes.it-codigo    = c-it-codigo AND
             tt-cortes.cod-refer    = lisa-integra.val-livre-3 AND
             tt-cortes.num-rolo-imp = INTEGER(lisa-integra.val-livre-1) NO-ERROR.
             
        IF NOT AVAIL tt-cortes THEN DO.
           CREATE tt-cortes.
           ASSIGN tt-cortes.num-rolo-imp = INTEGER(lisa-integra.val-livre-1)
                  tt-cortes.acao         = lisa-integra.acao      
                  tt-cortes.nr-container = int(entry(1,lisa-integra.val-livre-5,"|"))
                  tt-cortes.it-codigo    = c-it-codigo
                  tt-cortes.cod-refer    = lisa-integra.val-livre-3
                  tt-cortes.quantidade   = DECIMAL(lisa-integra.conteudo)
                  tt-cortes.ind-situacao = lisa-integra.ind-situacao.
    
           IF lisa-integra.chave <> '0' THEN DO.
              FIND ob-etiqueta WHERE
                   ob-etiqueta.cod-estabel = '505' AND
                   ob-etiqueta.num-etiqueta = INTEGER(lisa-integra.chave)
                   SHARE-LOCK NO-ERROR.
              ASSIGN tt-cortes.num-etiqueta = ob-etiqueta.num-etiqueta.
           END.
        END.
    END.
    RUN pi-finalizar in h-acomp.

    APPLY 'CHOOSE' TO bt-sel IN FRAME {&FRAME-NAME}.

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
  {src/adm/template/snd-list.i "tt-cortes"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-situacao B-table-Win 
FUNCTION fn-situacao RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEF VAR l-mostra AS LOGICAL INITIAL NO.

    IF tg-aguardando = YES AND tt-cortes.ind-situacao = 1 THEN ASSIGN l-mostra = YES.
    IF tg-integrado  = YES AND tt-cortes.ind-situacao = 2 THEN ASSIGN l-mostra = YES.

    RETURN l-mostra.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

