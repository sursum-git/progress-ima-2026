&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          mgind            PROGRESS
          movind           PROGRESS
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
**
**  ACRA 006 - 25/01/2013 
**           - INCLUSÇO DA VARIµVEL GLOBAL p-estabelec-xmlloader PARA
**             PARA FILTRAR AS ORDENS DE COMPRA DO ESTABELECIMENTO
**           - INCLUSÇO DO CAMPO DESCRI€ÇO NO BROWSE
**           - INCLUSÇO DO BOTÇO DE NARRATIVA PARA EXIBIR A NARRATIVA DA 
**             ORDEM DE COMPRA.
**  ACRA 007 - 09/07/2013 
**           - CRIADA A VARIµVEL p-lista-oc-assoc.
**             ESTA VARIµVEL  PREENCHIDA COM TODAS AS ORDENS DE 
**             COMPRA Jµ ASSOCIADAS NO DOCUMENTO. NO ZOOM DE ORDENS DE COMPRA
**             Hµ UM FILTRO PARA QUE AS ORDENS CONSTANTES NESTA LISTA NÇO APARE€AM
**             NA PESQUISA.
*******************************************************************************/
{include/i-prgvrs.i B01DT0912Z 2.06.00.007}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
&Scop adm-attribute-dlg support/browserd.w

/* Parameters Definitions ---                                           */
DEFINE NEW GLOBAL SHARED VAR p-estabelec-xmlloader AS CHAR FORMAT "X(3)" NO-UNDO.
/* Local Variable Definitions ---                                       */
define variable c-lista-valor   as character init '':U no-undo.
DEFINE VARIABLE d-valor-unit    AS DECIMAL      NO-UNDO.
DEFINE VARIABLE l-preco-bruto   AS LOGICAL      NO-UNDO.
define variable l-mostra-todas  as logical      no-undo.
DEFINE NEW GLOBAL SHARED VARIABLE i-emitente           AS INT NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE p-lista-oc-assoc     AS CHAR NO-UNDO.

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
&Scoped-define INTERNAL-TABLES prazo-compra ordem-compra item

/* Definitions for BROWSE br-table                                      */
&Scoped-define FIELDS-IN-QUERY-br-table ordem-compra.nr-contrato ~
ordem-compra.num-pedido prazo-compra.numero-ordem prazo-compra.parcela ~
prazo-compra.quant-saldo ~
fnValorUnit(prazo-compra.numero-ordem) @ d-valor-unit ~
prazo-compra.it-codigo item.desc-item 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-table 
&Scoped-define QUERY-STRING-br-table FOR EACH prazo-compra ~
      WHERE prazo-compra.numero-ordem >= c-inicial and ~
prazo-compra.numero-ordem <= c-final and ~
prazo-compra.situacao  = 2 ~
 AND prazo-compra.it-codigo >= c-inicial-2 ~
 AND prazo-compra.it-codigo <= c-final-2 NO-LOCK, ~
      FIRST ordem-compra OF prazo-compra ~
      WHERE ordem-compra.cod-emitente = i-emitente ~
 AND ordem-compra.num-pedido >= c-inicial-3 ~
 AND ordem-compra.num-pedido <= c-final-3 ~
 AND ordem-compra.cod-estabel = p-estabelec-xmlloader ~
 AND if not(l-mostra-todas) then lookup(string(ordem-compra.numero-ordem), p-lista-oc-assoc, "|") <= 0 else true NO-LOCK, ~
      EACH item OF ordem-compra NO-LOCK ~
    BY prazo-compra.it-codigo
&Scoped-define OPEN-QUERY-br-table OPEN QUERY br-table FOR EACH prazo-compra ~
      WHERE prazo-compra.numero-ordem >= c-inicial and ~
prazo-compra.numero-ordem <= c-final and ~
prazo-compra.situacao  = 2 ~
 AND prazo-compra.it-codigo >= c-inicial-2 ~
 AND prazo-compra.it-codigo <= c-final-2 NO-LOCK, ~
      FIRST ordem-compra OF prazo-compra ~
      WHERE ordem-compra.cod-emitente = i-emitente ~
 AND ordem-compra.num-pedido >= c-inicial-3 ~
 AND ordem-compra.num-pedido <= c-final-3 ~
 AND ordem-compra.cod-estabel = p-estabelec-xmlloader ~
 AND if not(l-mostra-todas) then lookup(string(ordem-compra.numero-ordem), p-lista-oc-assoc, "|") <= 0 else true NO-LOCK, ~
      EACH item OF ordem-compra NO-LOCK ~
    BY prazo-compra.it-codigo.
&Scoped-define TABLES-IN-QUERY-br-table prazo-compra ordem-compra item
&Scoped-define FIRST-TABLE-IN-QUERY-br-table prazo-compra
&Scoped-define SECOND-TABLE-IN-QUERY-br-table ordem-compra
&Scoped-define THIRD-TABLE-IN-QUERY-br-table item


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-br-table}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rs-display bt-narrativa c-inicial-3 ~
c-inicial c-final c-inicial-2 c-final-2 bt-confirma c-final-3 IMAGE-1 ~
IMAGE-2 br-table IMAGE-3 IMAGE-4 IMAGE-19 IMAGE-20 RECT-5 
&Scoped-Define DISPLAYED-OBJECTS rs-display c-inicial-3 c-inicial c-final ~
c-inicial-2 c-final-2 c-final-3 

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
it-codigo||y|prazo-compra.it-codigo
numero-ordem||y|prazo-compra.numero-ordem
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "it-codigo,numero-ordem"':U).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnValorUnit B-table-Win 
FUNCTION fnValorUnit RETURNS DECIMAL
  ( pOrdem AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-confirma 
     IMAGE-UP FILE "image\im-sav":U
     LABEL "Button 1" 
     SIZE 5.14 BY 1.

DEFINE BUTTON bt-narrativa 
     LABEL "Narrativa" 
     SIZE 15 BY 1.

DEFINE VARIABLE c-final AS INTEGER FORMAT ">>>>>>>9" INITIAL 99999999 
     VIEW-AS FILL-IN 
     SIZE 10.14 BY .88 NO-UNDO.

DEFINE VARIABLE c-final-2 AS CHARACTER FORMAT "X(16)" INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88 NO-UNDO.

DEFINE VARIABLE c-final-3 AS INTEGER FORMAT ">>>>>,>>9" INITIAL 99999999 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE c-inicial AS INTEGER FORMAT ">>>>>>>9" INITIAL 0 
     LABEL "Ord. Compra" 
     VIEW-AS FILL-IN 
     SIZE 10.14 BY .88 NO-UNDO.

DEFINE VARIABLE c-inicial-2 AS CHARACTER FORMAT "X(16)" 
     LABEL "C¢d Item" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88 NO-UNDO.

DEFINE VARIABLE c-inicial-3 AS INTEGER FORMAT ">>>>>,>>9" INITIAL 0 
     LABEL "Pedido":R8 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\ii-fir":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-19
     FILENAME "image\ii-fir":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-2
     FILENAME "image\ii-las":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-20
     FILENAME "image\ii-las":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-3
     FILENAME "image\ii-fir":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-4
     FILENAME "image\ii-las":U
     SIZE 2.86 BY 1.

DEFINE VARIABLE rs-display AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Mostra Todas", 1,
"Mostra NÆo Utilizadas", 2
     SIZE 18 BY 1.75 NO-UNDO.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 20 BY 2.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-table FOR 
      prazo-compra, 
      ordem-compra, 
      item SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-table B-table-Win _STRUCTURED
  QUERY br-table NO-LOCK DISPLAY
      ordem-compra.nr-contrato COLUMN-LABEL "Nr Contrato"
      ordem-compra.num-pedido COLUMN-LABEL "Pedido"
      prazo-compra.numero-ordem COLUMN-LABEL "Ordem" WIDTH 12.43
      prazo-compra.parcela COLUMN-LABEL "Parc" WIDTH 4.43
      prazo-compra.quant-saldo COLUMN-LABEL "Qtde Saldo"
      fnValorUnit(prazo-compra.numero-ordem) @ d-valor-unit COLUMN-LABEL "Pre‡o Unit." FORMAT ">>>,>>9.99":U
      prazo-compra.it-codigo COLUMN-LABEL "Item" WIDTH 19.57
      item.desc-item FORMAT "x(60)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 90 BY 12
         FONT 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     rs-display AT ROW 1.38 COL 69 NO-LABEL WIDGET-ID 20
     bt-narrativa AT ROW 16.54 COL 76 WIDGET-ID 18
     c-inicial-3 AT ROW 1.29 COL 26.29 COLON-ALIGNED HELP
          "N£mero do Pedido" WIDGET-ID 10
     c-inicial AT ROW 2.29 COL 25 COLON-ALIGNED
     c-final AT ROW 2.29 COL 47.43 COLON-ALIGNED NO-LABEL
     c-inicial-2 AT ROW 3.29 COL 17.14 COLON-ALIGNED WIDGET-ID 4
     c-final-2 AT ROW 3.29 COL 47.43 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     bt-confirma AT ROW 3.29 COL 85.57
     c-final-3 AT ROW 1.25 COL 47.43 COLON-ALIGNED HELP
          "N£mero do Pedido" NO-LABEL WIDGET-ID 12
     br-table AT ROW 4.5 COL 1
     IMAGE-1 AT ROW 2.29 COL 36.86
     IMAGE-2 AT ROW 2.29 COL 46.43
     IMAGE-3 AT ROW 3.29 COL 37 WIDGET-ID 6
     IMAGE-4 AT ROW 3.29 COL 46.43 WIDGET-ID 8
     IMAGE-19 AT ROW 1.25 COL 36.86 WIDGET-ID 14
     IMAGE-20 AT ROW 1.25 COL 46.43 WIDGET-ID 16
     RECT-5 AT ROW 1.25 COL 68 WIDGET-ID 24
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 FONT 1 WIDGET-ID 100.


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
         HEIGHT             = 16.58
         WIDTH              = 90.86.
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit Custom                            */
/* BROWSE-TAB br-table IMAGE-2 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-table
/* Query rebuild information for BROWSE br-table
     _TblList          = "movind.prazo-compra,movind.ordem-compra OF movind.prazo-compra,mgind.item OF movind.ordem-compra"
     _Options          = "NO-LOCK"
     _TblOptList       = ", FIRST,"
     _OrdList          = "eai.prazo-compra.it-codigo|yes"
     _Where[1]         = "prazo-compra.numero-ordem >= c-inicial and
prazo-compra.numero-ordem <= c-final and
prazo-compra.situacao  = 2
 AND prazo-compra.it-codigo >= c-inicial-2
 AND prazo-compra.it-codigo <= c-final-2"
     _Where[2]         = "ordem-compra.cod-emitente = i-emitente
 AND ordem-compra.num-pedido >= c-inicial-3
 AND ordem-compra.num-pedido <= c-final-3
 AND ordem-compra.cod-estabel = p-estabelec-xmlloader
 AND if not(l-mostra-todas) then lookup(string(ordem-compra.numero-ordem), p-lista-oc-assoc, ""|"") <= 0 else true"
     _FldNameList[1]   > "_<CALC>"
"ordem-compra.nr-contrato" "Nr Contrato" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > "_<CALC>"
"ordem-compra.num-pedido" "Pedido" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"prazo-compra.numero-ordem" "Ordem" ? "integer" ? ? ? ? ? ? no ? no no "12.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"prazo-compra.parcela" "Parc" ? "integer" ? ? ? ? ? ? no ? no no "4.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"prazo-compra.quant-saldo" "Qtde Saldo" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"fnValorUnit(prazo-compra.numero-ordem) @ d-valor-unit" "Pre‡o Unit." ">>>,>>9.99" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"prazo-compra.it-codigo" "Item" ? "character" ? ? ? ? ? ? no ? no no "19.57" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   = mgind.item.desc-item
     _Query            is OPENED
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
  assign input frame {&frame-name} c-inicial c-final c-inicial-2 c-final-2 c-inicial-3 c-final-3.
  if input frame {&frame-name} rs-display = 1 then
      assign l-mostra-todas = yes.
  else
      assign l-mostra-todas = no.

  RUN dispatch IN THIS-PROCEDURE ('open-query':U).
  apply 'value-changed':U to {&browse-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-narrativa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-narrativa B-table-Win
ON CHOOSE OF bt-narrativa IN FRAME F-Main /* Narrativa */
DO:
  FIND CURRENT ordem-compra NO-LOCK NO-ERROR.
  IF AVAIL ordem-compra THEN
      RUN dtp/dts0912zdet.w(INPUT ordem-compra.narrativa).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

find first param-compra no-lock no-error.
if avail param-compra then
    assign l-preco-bruto = (param-compra.ipi-sobre-preco = 1).

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
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER P-CAMPO AS CHARACTER NO-UNDO.

    DEFINE VARIABLE P-VALOR AS CHAR INIT "" NO-UNDO.
    if  avail prazo-compra then do:
        case p-campo:
            when "num-pedido" THEN DO:
                FIND FIRST ordem-compra
                    WHERE ordem-compra.numero-ordem = prazo-compra.numero-ordem NO-LOCK NO-ERROR.
                IF AVAIL ordem-compra THEN DO:
                    assign p-valor = string(ordem-compra.num-pedido).                
                END.
                ELSE
                    ASSIGN p-valor = "0".
            END.
            when "numero-ordem" THEN DO:
                assign p-valor = string(prazo-compra.numero-ordem).                
            END.
            WHEN "it-codigo" THEN DO:
                ASSIGN p-valor = STRING(prazo-compra.it-codigo).                
            END.
            WHEN "cod-depos" THEN DO:
                FIND FIRST ordem-compra
                    WHERE ordem-compra.numero-ordem = prazo-compra.numero-ordem NO-LOCK NO-ERROR.
                IF AVAIL ordem-compra THEN DO:
                    ASSIGN p-valor = STRING(ordem-compra.dep-almoxar).
                END.
            END.
/*             when "num-pedido" THEN DO:                                                            */
/*                 FIND FIRST ordem-compra                                                           */
/*                     WHERE ordem-compra.numero-ordem = prazo-compra.numero-ordem NO-LOCK NO-ERROR. */
/*                 IF AVAIL ordem-compra THEN DO:                                                    */
/*                     assign p-valor = string(ordem-compra.num-pedido).                             */
/*             END.                                                                                  */
            WHEN "narrativa" THEN DO:
                ASSIGN p-valor = ordem-compra.narrativa.
            END.
            WHEN "quant-saldo" THEN DO:
                ASSIGN p-valor = string(prazo-compra.quant-saldo).
            END.
        END CASE.
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
  {src/adm/template/sndkycas.i "it-codigo" "prazo-compra" "it-codigo"}
  {src/adm/template/sndkycas.i "numero-ordem" "prazo-compra" "numero-ordem"}

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
  {src/adm/template/snd-list.i "prazo-compra"}
  {src/adm/template/snd-list.i "ordem-compra"}
  {src/adm/template/snd-list.i "item"}

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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnValorUnit B-table-Win 
FUNCTION fnValorUnit RETURNS DECIMAL
  ( pOrdem AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEFINE VARIABLE d-valor-ipi AS DECIMAL     NO-UNDO.
DEFINE VARIABLE de-val-orig AS DECIMAL     NO-UNDO.
DEFINE VARIABLE d-preco-unit AS DECIMAL     NO-UNDO.
if l-preco-bruto = no then do:
   assign d-valor-ipi = (ordem-compra.preco-unit
                      *  ordem-compra.aliquota-ipi)
                      / (100 + ordem-compra.aliquota-ipi).
end.
else do:
    if ordem-compra.perc-descto > 0 then
        assign de-val-orig = (ordem-compra.preco-unit * 100)
                           / (100 - ordem-compra.perc-descto).
    else
        assign de-val-orig = ordem-compra.preco-unit.

    assign d-valor-ipi = (de-val-orig
                       * ordem-compra.aliquota-ipi)
                       / (100 + ordem-compra.aliquota-ipi).
end.

assign d-preco-unit = ordem-compra.preco-unit - d-valor-ipi.

RETURN d-preco-unit.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

