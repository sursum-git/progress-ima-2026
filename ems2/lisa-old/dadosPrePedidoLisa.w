&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-livre 
/*:T *******************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i dadosprepedidolisa 9.99.99.999}

/* Chamada a include do gerenciador de licenáas. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i <programa> <m¢dulo>}
&ENDIF

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{esapi/analisarJsonObject2.i}
{lisa/extrairTtJsonPrePedido.i}

DEFINE INPUT PARAMETER TABLE FOR ttJson.


DEFINE VARIABLE cDescrSit AS CHARACTER   NO-UNDO.

DEFINE VARIABLE cItem AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cRef  AS CHARACTER   NO-UNDO.

DEF VAR i-num-etiqueta LIKE ob-etiqueta.num-etiqueta.
DEF VAR de-quantidade AS DECIMAL.
DEF VAR c-situacao AS CHAR FORMAT "x(20)".
DEF VAR c-status AS CHAR FORMAT "x(30)".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-livre
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad
&Scoped-define BROWSE-NAME brEtq

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttPedItemEtq ttPedItem ttPedItemFat

/* Definitions for BROWSE brEtq                                         */
&Scoped-define FIELDS-IN-QUERY-brEtq ttPedItemEtq.nrSeq ttPedItemEtq.rolo ttPedItemEtq.nrContainer ttPedItemEtq.quantidade ttPedItemEtq.data ttPedItemEtq.hora ttPedItemEtq.endereco ttPedItemEtq.idLisa fn-etq() @ i-num-etiqueta fn-qtde() @ de-quantidade fn-situacao() @ c-situacao fn-status() @ c-status   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brEtq   
&Scoped-define SELF-NAME brEtq
&Scoped-define QUERY-STRING-brEtq FOR EACH ttPedItemEtq                             WHERE ttPedItemEtq.itCodigo = cItem                             AND   ttPedItemEtq.codRefer = cRef
&Scoped-define OPEN-QUERY-brEtq OPEN QUERY {&SELF-NAME} FOR EACH ttPedItemEtq                             WHERE ttPedItemEtq.itCodigo = cItem                             AND   ttPedItemEtq.codRefer = cRef .
&Scoped-define TABLES-IN-QUERY-brEtq ttPedItemEtq
&Scoped-define FIRST-TABLE-IN-QUERY-brEtq ttPedItemEtq


/* Definitions for BROWSE brPedItem                                     */
&Scoped-define FIELDS-IN-QUERY-brPedItem ttPedItem.itCodigo ttPedItem.descricao ttPedItem.codRefer ttPedItem.qtSolicitada ttPedItem.qtSeparada ttPedItem.qtDiferenca   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brPedItem   
&Scoped-define SELF-NAME brPedItem
&Scoped-define QUERY-STRING-brPedItem FOR EACH ttPedItem      WHERE ttPEdItem.itCodigo <> ''
&Scoped-define OPEN-QUERY-brPedItem OPEN QUERY {&SELF-NAME} FOR EACH ttPedItem      WHERE ttPEdItem.itCodigo <> '' .
&Scoped-define TABLES-IN-QUERY-brPedItem ttPedItem
&Scoped-define FIRST-TABLE-IN-QUERY-brPedItem ttPedItem


/* Definitions for BROWSE brTerceiros                                   */
&Scoped-define FIELDS-IN-QUERY-brTerceiros ttPedItemFat.nfOrigem ttPedItemFat.serieNfOrigem ttPedItemFat.itemNfOrigem ttPedItemFat.nfRetorno ttPedItemFat.serieNfRetorno ttPedItemFat.qtFaturada ttPedItemFat.idNfOrigem   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brTerceiros   
&Scoped-define SELF-NAME brTerceiros
&Scoped-define QUERY-STRING-brTerceiros FOR EACH ttPedItemFat      WHERE ttPedItemFat.itCodigo = cItem      AND   ttPedItemFat.codRefer = cRef
&Scoped-define OPEN-QUERY-brTerceiros OPEN QUERY {&SELF-NAME} FOR EACH ttPedItemFat      WHERE ttPedItemFat.itCodigo = cItem      AND   ttPedItemFat.codRefer = cRef     .
&Scoped-define TABLES-IN-QUERY-brTerceiros ttPedItemFat
&Scoped-define FIRST-TABLE-IN-QUERY-brTerceiros ttPedItemFat


/* Definitions for FRAME f-cad                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-cad ~
    ~{&OPEN-QUERY-brEtq}~
    ~{&OPEN-QUERY-brPedItem}~
    ~{&OPEN-QUERY-brTerceiros}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-button RECT-1 RECT-2 RECT-3 RECT-5 ~
brPedItem brTerceiros brEtq 
&Scoped-Define DISPLAYED-OBJECTS fiPedido fiSituacaoPre tgSepEnviada edObs ~
fiPedidoLisa fiSituacaoPedido tgNFEnviada fiQtCaixa fiNF fiDtInclusao ~
fiHoraInclusao edObs-2 fiPesoBruto fiDtRomaneio fiPesoLiquido fiDtExpedido ~
fiHoraExpedicao 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-etq w-livre 
FUNCTION fn-etq RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-qtde w-livre 
FUNCTION fn-qtde RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-situacao w-livre 
FUNCTION fn-situacao RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-status w-livre 
FUNCTION fn-status RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-livre AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU mi-programa 
       MENU-ITEM mi-consultas   LABEL "Co&nsultas"     ACCELERATOR "CTRL-L"
       MENU-ITEM mi-imprimir    LABEL "&Relat¢rios"    ACCELERATOR "CTRL-P"
       RULE
       MENU-ITEM mi-sair        LABEL "&Sair"          ACCELERATOR "CTRL-X".

DEFINE SUB-MENU m_Ajuda 
       MENU-ITEM mi-conteudo    LABEL "&Conteudo"     
       MENU-ITEM mi-sobre       LABEL "&Sobre..."     .

DEFINE MENU m-livre MENUBAR
       SUB-MENU  mi-programa    LABEL "&Nome-do-Programa"
       SUB-MENU  m_Ajuda        LABEL "&Ajuda"        .


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_p-exihel AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE edObs AS CHARACTER 
     VIEW-AS EDITOR
     SIZE 43.43 BY 2 NO-UNDO.

DEFINE VARIABLE edObs-2 AS CHARACTER 
     VIEW-AS EDITOR
     SIZE 43.29 BY 2 NO-UNDO.

DEFINE VARIABLE fiDtExpedido AS DATE FORMAT "99/99/9999":U 
     LABEL "Dt.Hr.Expediá∆o" 
     VIEW-AS FILL-IN 
     SIZE 13.29 BY .79 NO-UNDO.

DEFINE VARIABLE fiDtInclusao AS DATE FORMAT "99/99/9999":U 
     LABEL "Dt.Hr.Inclus∆o" 
     VIEW-AS FILL-IN 
     SIZE 13.29 BY .79 NO-UNDO.

DEFINE VARIABLE fiDtRomaneio AS DATE FORMAT "99/99/9999":U 
     LABEL "Dt. Romaneio" 
     VIEW-AS FILL-IN 
     SIZE 13.29 BY .79 NO-UNDO.

DEFINE VARIABLE fiHoraExpedicao AS CHARACTER FORMAT "X(10)":U 
     VIEW-AS FILL-IN 
     SIZE 13.29 BY .79 NO-UNDO.

DEFINE VARIABLE fiHoraInclusao AS CHARACTER FORMAT "X(10)":U 
     VIEW-AS FILL-IN 
     SIZE 13.29 BY .79 NO-UNDO.

DEFINE VARIABLE fiNF AS CHARACTER FORMAT "X(256)":U 
     LABEL "NF Cliente" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .79
     FONT 0 NO-UNDO.

DEFINE VARIABLE fiPedido AS CHARACTER FORMAT "X(256)":U 
     LABEL "Pedido" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .79
     FONT 0 NO-UNDO.

DEFINE VARIABLE fiPedidoLisa AS CHARACTER FORMAT "X(256)":U 
     LABEL "Pedido LISA" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .79
     FONT 0 NO-UNDO.

DEFINE VARIABLE fiPesoBruto AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Peso Bruto" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .79
     FONT 0 NO-UNDO.

DEFINE VARIABLE fiPesoLiquido AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Peso Liquido" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .79
     FONT 0 NO-UNDO.

DEFINE VARIABLE fiQtCaixa AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Qt.Caixa" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .79
     FONT 0 NO-UNDO.

DEFINE VARIABLE fiSituacaoPedido AS CHARACTER FORMAT "X(256)":U 
     LABEL "Situaá∆o Pedido" 
     VIEW-AS FILL-IN 
     SIZE 22.57 BY .79
     FONT 0 NO-UNDO.

DEFINE VARIABLE fiSituacaoPre AS CHARACTER FORMAT "X(256)":U 
     LABEL "Situaá∆o PrÇ-Pedido" 
     VIEW-AS FILL-IN 
     SIZE 22.57 BY .79
     FONT 0 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 87 BY 2.5.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 40.57 BY 3.42.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46 BY 3.46.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 47.43 BY 6.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 135 BY 1.46
     BGCOLOR 7 .

DEFINE VARIABLE tgNFEnviada AS LOGICAL INITIAL no 
     LABEL "NF Enviada?" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.43 BY .83 NO-UNDO.

DEFINE VARIABLE tgSepEnviada AS LOGICAL INITIAL no 
     LABEL "Separaá∆o Enviada?" 
     VIEW-AS TOGGLE-BOX
     SIZE 17.29 BY .83 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brEtq FOR 
      ttPedItemEtq SCROLLING.

DEFINE QUERY brPedItem FOR 
      ttPedItem SCROLLING.

DEFINE QUERY brTerceiros FOR 
      ttPedItemFat SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brEtq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brEtq w-livre _FREEFORM
  QUERY brEtq DISPLAY
      ttPedItemEtq.nrSeq          
ttPedItemEtq.rolo           
ttPedItemEtq.nrContainer    
ttPedItemEtq.quantidade     
ttPedItemEtq.data           
ttPedItemEtq.hora           
ttPedItemEtq.endereco       
ttPedItemEtq.idLisa
fn-etq() @ i-num-etiqueta  COLUMN-LABEL "Etiqueta" 
fn-qtde() @ de-quantidade  COLUMN-LABEL "Qtde" 
fn-situacao() @ c-situacao COLUMN-LABEL "Situacao" 
fn-status() @ c-status     COLUMN-LABEL "Status"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 132 BY 4.75
         FONT 1
         TITLE "Etiquetas por Item/Referància".

DEFINE BROWSE brPedItem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brPedItem w-livre _FREEFORM
  QUERY brPedItem DISPLAY
      ttPedItem.itCodigo      COLUMN-LABEL "Produto"   FORMAT 'x(12)'
ttPedItem.descricao     COLUMN-LABEL "Descricao" FORMAT 'x(50)'
ttPedItem.codRefer      COLUMN-LABEL "Refer."    FORMAT 'x(4)'
ttPedItem.qtSolicitada  COLUMN-LABEL "Qt.Solic."  
ttPedItem.qtSeparada    COLUMN-LABEL "Qt.Coletada"  
ttPedItem.qtDiferenca   COLUMN-LABEL "Qt.Diferenáa"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 131.86 BY 7.5
         FONT 1
         TITLE "Itens do PrÇ-Pedido LISA" ROW-HEIGHT-CHARS .46.

DEFINE BROWSE brTerceiros
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brTerceiros w-livre _FREEFORM
  QUERY brTerceiros DISPLAY
      ttPedItemFat.nfOrigem       
ttPedItemFat.serieNfOrigem  
ttPedItemFat.itemNfOrigem   
ttPedItemFat.nfRetorno      
ttPedItemFat.serieNfRetorno 
ttPedItemFat.qtFaturada     
ttPedItemFat.idNfOrigem
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 132 BY 4.5
         FONT 1
         TITLE "Controle Retorno x Remessa" FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     fiPedido AT ROW 3.08 COL 10.72 COLON-ALIGNED WIDGET-ID 2
     fiSituacaoPre AT ROW 3.08 COL 41.43 COLON-ALIGNED WIDGET-ID 4
     tgSepEnviada AT ROW 3.17 COL 70.57 WIDGET-ID 28
     edObs AT ROW 3.58 COL 90.57 NO-LABEL WIDGET-ID 44
     fiPedidoLisa AT ROW 4 COL 10.72 COLON-ALIGNED WIDGET-ID 26
     fiSituacaoPedido AT ROW 4.04 COL 41.43 COLON-ALIGNED WIDGET-ID 24
     tgNFEnviada AT ROW 4.13 COL 70.72 WIDGET-ID 30
     fiQtCaixa AT ROW 5.63 COL 31.29 COLON-ALIGNED WIDGET-ID 50
     fiNF AT ROW 5.67 COL 10 COLON-ALIGNED WIDGET-ID 38
     fiDtInclusao AT ROW 5.79 COL 58.43 COLON-ALIGNED WIDGET-ID 8
     fiHoraInclusao AT ROW 5.79 COL 72.29 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     edObs-2 AT ROW 6.42 COL 90.72 NO-LABEL WIDGET-ID 52
     fiPesoBruto AT ROW 6.54 COL 10 COLON-ALIGNED WIDGET-ID 46
     fiDtRomaneio AT ROW 6.67 COL 58.72 COLON-ALIGNED WIDGET-ID 16
     fiPesoLiquido AT ROW 7.5 COL 10 COLON-ALIGNED WIDGET-ID 48
     fiDtExpedido AT ROW 7.58 COL 58.72 COLON-ALIGNED WIDGET-ID 18
     fiHoraExpedicao AT ROW 7.58 COL 72.57 COLON-ALIGNED NO-LABEL WIDGET-ID 20
     brPedItem AT ROW 8.96 COL 2.14 WIDGET-ID 200
     brTerceiros AT ROW 16.58 COL 2 WIDGET-ID 400
     brEtq AT ROW 21.25 COL 2 WIDGET-ID 300
     "Observaá∆o Separacao" VIEW-AS TEXT
          SIZE 16.86 BY .54 AT ROW 5.83 COL 90.86 WIDGET-ID 54
     "Observaá∆o Pedido" VIEW-AS TEXT
          SIZE 14.14 BY .54 AT ROW 2.92 COL 91.14 WIDGET-ID 42
     "Pedido" VIEW-AS TEXT
          SIZE 7 BY .54 AT ROW 2.5 COL 3 WIDGET-ID 22
     "Datas" VIEW-AS TEXT
          SIZE 5 BY .54 AT ROW 5.25 COL 49.14 WIDGET-ID 12
     rt-button AT ROW 1 COL 1
     RECT-1 AT ROW 2.75 COL 2 WIDGET-ID 6
     RECT-2 AT ROW 5.33 COL 48.43 WIDGET-ID 10
     RECT-3 AT ROW 5.29 COL 2 WIDGET-ID 34
     RECT-5 AT ROW 2.75 COL 89 WIDGET-ID 56
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         PAGE-TOP SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 135.43 BY 25.33
         FONT 1 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: w-livre
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-livre ASSIGN
         HIDDEN             = YES
         TITLE              = "Dados PrÇ-Pedido LISA"
         HEIGHT             = 25.33
         WIDTH              = 135.43
         MAX-HEIGHT         = 33
         MAX-WIDTH          = 228.57
         VIRTUAL-HEIGHT     = 33
         VIRTUAL-WIDTH      = 228.57
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU m-livre:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-livre 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-livre.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-livre
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-cad
   FRAME-NAME L-To-R                                                    */
/* BROWSE-TAB brPedItem fiHoraExpedicao f-cad */
/* BROWSE-TAB brTerceiros brPedItem f-cad */
/* BROWSE-TAB brEtq brTerceiros f-cad */
ASSIGN 
       brPedItem:NUM-LOCKED-COLUMNS IN FRAME f-cad     = 3.

/* SETTINGS FOR EDITOR edObs IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR edObs-2 IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiDtExpedido IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiDtInclusao IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiDtRomaneio IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiHoraExpedicao IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiHoraInclusao IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiNF IN FRAME f-cad
   NO-ENABLE                                                            */
ASSIGN 
       fiNF:READ-ONLY IN FRAME f-cad        = TRUE.

/* SETTINGS FOR FILL-IN fiPedido IN FRAME f-cad
   NO-ENABLE                                                            */
ASSIGN 
       fiPedido:READ-ONLY IN FRAME f-cad        = TRUE.

/* SETTINGS FOR FILL-IN fiPedidoLisa IN FRAME f-cad
   NO-ENABLE                                                            */
ASSIGN 
       fiPedidoLisa:READ-ONLY IN FRAME f-cad        = TRUE.

/* SETTINGS FOR FILL-IN fiPesoBruto IN FRAME f-cad
   NO-ENABLE                                                            */
ASSIGN 
       fiPesoBruto:READ-ONLY IN FRAME f-cad        = TRUE.

/* SETTINGS FOR FILL-IN fiPesoLiquido IN FRAME f-cad
   NO-ENABLE                                                            */
ASSIGN 
       fiPesoLiquido:READ-ONLY IN FRAME f-cad        = TRUE.

/* SETTINGS FOR FILL-IN fiQtCaixa IN FRAME f-cad
   NO-ENABLE                                                            */
ASSIGN 
       fiQtCaixa:READ-ONLY IN FRAME f-cad        = TRUE.

/* SETTINGS FOR FILL-IN fiSituacaoPedido IN FRAME f-cad
   NO-ENABLE                                                            */
ASSIGN 
       fiSituacaoPedido:READ-ONLY IN FRAME f-cad        = TRUE.

/* SETTINGS FOR FILL-IN fiSituacaoPre IN FRAME f-cad
   NO-ENABLE                                                            */
ASSIGN 
       fiSituacaoPre:READ-ONLY IN FRAME f-cad        = TRUE.

/* SETTINGS FOR TOGGLE-BOX tgNFEnviada IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tgSepEnviada IN FRAME f-cad
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brEtq
/* Query rebuild information for BROWSE brEtq
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttPedItemEtq
                            WHERE ttPedItemEtq.itCodigo = cItem
                            AND   ttPedItemEtq.codRefer = cRef .
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE brEtq */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brPedItem
/* Query rebuild information for BROWSE brPedItem
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttPedItem
     WHERE ttPEdItem.itCodigo <> '' .
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE brPedItem */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brTerceiros
/* Query rebuild information for BROWSE brTerceiros
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttPedItemFat
     WHERE ttPedItemFat.itCodigo = cItem
     AND   ttPedItemFat.codRefer = cRef
    .
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE brTerceiros */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON END-ERROR OF w-livre /* Dados PrÇ-Pedido LISA */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON WINDOW-CLOSE OF w-livre /* Dados PrÇ-Pedido LISA */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brPedItem
&Scoped-define SELF-NAME brPedItem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brPedItem w-livre
ON VALUE-CHANGED OF brPedItem IN FRAME f-cad /* Itens do PrÇ-Pedido LISA */
DO:
  ASSIGN cItem = ''
         cRef  = ''.
  IF AVAIL ttPedItem THEN DO:
     ASSIGN cItem = ttPedItem.itCodigo
            cRef  = ttPedItem.codRefer.
  END.
  {&OPEN-query-brTerceiros}
  {&OPEN-query-brEtq}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-consultas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-consultas w-livre
ON CHOOSE OF MENU-ITEM mi-consultas /* Consultas */
DO:
  RUN pi-consulta IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-conteudo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-conteudo w-livre
ON CHOOSE OF MENU-ITEM mi-conteudo /* Conteudo */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  RUN pi-ajuda IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-imprimir w-livre
ON CHOOSE OF MENU-ITEM mi-imprimir /* Relat¢rios */
DO:
  RUN pi-imprimir IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-programa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-programa w-livre
ON MENU-DROP OF MENU mi-programa /* Nome-do-Programa */
DO:
  run pi-disable-menu.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sair
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sair w-livre
ON CHOOSE OF MENU-ITEM mi-sair /* Sair */
DO:
  RUN pi-sair IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sobre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sobre w-livre
ON CHOOSE OF MENU-ITEM mi-sobre /* Sobre... */
DO:
  {include/sobre.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brEtq
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-livre 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-livre  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  CASE adm-current-page: 

    WHEN 0 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panel/p-exihel.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-exihel ).
       RUN set-position IN h_p-exihel ( 1.13 , 119.72 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             fiPedido:HANDLE IN FRAME f-cad , 'BEFORE':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-livre  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE atuCpsCab w-livre 
PROCEDURE atuCpsCab :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FIND FIRST ttPedido NO-ERROR.
IF AVAIL ttPedido THEN DO:
   ASSIGN fiPedido:SCREEN-VALUE IN FRAME {&FRAME-NAME}          = string(ttPedido.pedidoCliente)
          fiPedidoLisa:SCREEN-VALUE IN FRAME {&FRAME-NAME}      = STRING(ttPedido.pedidolisa)
          fiSituacaoPre:SCREEN-VALUE IN FRAME {&FRAME-NAME}     = ttPedido.descrSituacao
          fiSituacaoPedido:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = ttPedido.situacaoPed
          tgSepEnviada:SCREEN-VALUE IN FRAME {&FRAME-NAME}      = STRING(ttPedido.logEnviadoApi)
          tgNfEnviada:SCREEN-VALUE IN FRAME {&FRAME-NAME}       = STRING(ttPedido.logNfeEnviada)
          fiNF:SCREEN-VALUE IN FRAME {&FRAME-NAME}              = ttPedido.nfCliente
          fiQtCaixa:SCREEN-VALUE IN FRAME {&FRAME-NAME}         = string(ttPedido.qtCaixa)
          fiPesoBruto:SCREEN-VALUE IN FRAME {&FRAME-NAME}       = string(ttPedido.pesoBruto)
          fiPesoLiquido:SCREEN-VALUE IN FRAME {&FRAME-NAME}     = string(ttPedido.pesoLiquido)
          edObs-2:SCREEN-VALUE IN FRAME {&FRAME-NAME}           = ttPedido.obsSeparacao
          edObs:SCREEN-VALUE IN FRAME {&FRAME-NAME}             = ttPedido.obs
          fiDtInclusao:SCREEN-VALUE IN FRAME {&FRAME-NAME}      = string(ttPedido.dtInclusao)
          fiHoraInclusao:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = ttPedido.horaInclusao
          fiDtRomaneio:SCREEN-VALUE IN FRAME {&FRAME-NAME}      = string(ttPedido.dtRomaneio)
          fiDtExpedido:SCREEN-VALUE IN FRAME {&FRAME-NAME}      = STRING(ttPedido.dtExpedido)
          fiHoraExpedicao:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = ttPedido.horaExpedido
       .
  

END.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-livre  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
  THEN DELETE WIDGET w-livre.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-livre  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY fiPedido fiSituacaoPre tgSepEnviada edObs fiPedidoLisa 
          fiSituacaoPedido tgNFEnviada fiQtCaixa fiNF fiDtInclusao 
          fiHoraInclusao edObs-2 fiPesoBruto fiDtRomaneio fiPesoLiquido 
          fiDtExpedido fiHoraExpedicao 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE rt-button RECT-1 RECT-2 RECT-3 RECT-5 brPedItem brTerceiros brEtq 
      WITH FRAME f-cad IN WINDOW w-livre.
  {&OPEN-BROWSERS-IN-QUERY-f-cad}
  VIEW w-livre.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE extrairTtJson w-livre 
PROCEDURE extrairTtJson :
RUN lisa/extrairTtJsonPrePedido.p(INPUT TABLE ttJson, 
                             OUTPUT TABLE ttPedido,
                             OUTPUT TABLE ttPedItem,
                             OUTPUT TABLE ttPedItemFat,
                             OUTPUT TABLE ttPedItemEtq
                             ).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-livre 
PROCEDURE local-destroy :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .
  {include/i-logfin.i}

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-livre 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  
  RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-livre 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  run pi-before-initialize.

  {include/win-size.i}

  {utp/ut9000.i "dadosprepedidolisa" "2.06.00.001"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  
  run pi-after-initialize.
  RUN extrairttJSon.
  RUN atuCpsCab.
  {&open-query-brPedItem}
  APPLY 'value-changed' TO brPedItem .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-livre  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "ttPedItemFat"}
  {src/adm/template/snd-list.i "ttPedItem"}
  {src/adm/template/snd-list.i "ttPedItemEtq"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-livre 
PROCEDURE state-changed :
/*:T -----------------------------------------------------------
  Purpose:     Manuseia trocas de estado dos SmartObjects
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.

  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-etq w-livre 
FUNCTION fn-etq RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    FIND ob-etiqueta WHERE
         ob-etiqueta.cod-estabel = '505' AND
         ob-etiqueta.nr-container = INTEGER(ttPedItemEtq.nrContainer) AND
         ob-etiqueta.it-codigo = ttPedItemEtq.itCodigo AND
         ob-etiqueta.cod-refer = ttPedItemEtq.codRefer AND
         ob-etiqueta.num-rolo-imp = INTEGER(ttPedItemEtq.rolo) NO-LOCK NO-ERROR.

    IF AVAIL ob-etiqueta THEN 
       RETURN STRING(ob-etiqueta.num-etiqueta).   /* Function return */
    ELSE
       RETURN ''.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-qtde w-livre 
FUNCTION fn-qtde RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEF VAR de-qtde AS DEC.

    FIND ob-etiqueta WHERE
         ob-etiqueta.cod-estabel = '505' AND
         ob-etiqueta.nr-container = INTEGER(ttPedItemEtq.nrContainer) AND
         ob-etiqueta.it-codigo = ttPedItemEtq.itCodigo AND
         ob-etiqueta.cod-refer = ttPedItemEtq.codRefer AND
         ob-etiqueta.num-rolo-imp = INTEGER(ttPedItemEtq.rolo) NO-LOCK NO-ERROR.

    IF AVAIL ob-etiqueta THEN 
       ASSIGN de-qtde = ob-etiqueta.quantidade.

    RETURN de-qtde.   /* Function return */
    

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-situacao w-livre 
FUNCTION fn-situacao RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEF VAR c-sit AS CHAR.

    FIND ob-etiqueta WHERE
         ob-etiqueta.cod-estabel = '505' AND
         ob-etiqueta.nr-container = INTEGER(ttPedItemEtq.nrContainer) AND
         ob-etiqueta.it-codigo = ttPedItemEtq.itCodigo AND
         ob-etiqueta.cod-refer = ttPedItemEtq.codRefer AND
         ob-etiqueta.num-rolo-imp = INTEGER(ttPedItemEtq.rolo) NO-LOCK NO-ERROR.

    IF AVAIL ob-etiqueta THEN DO.
       IF ob-etiqueta.situacao = 3 THEN 
          ASSIGN c-sit = 'em Estoque'.
       ELSE IF ob-etiqueta.situacao = 4 THEN 
          ASSIGN c-sit = 'Reservada'.
       ELSE IF ob-etiqueta.situacao = 5 THEN 
          ASSIGN c-sit = 'Faturada'.
       ELSE
          ASSIGN c-sit = 'Indispon°vel'.
    END.

    RETURN c-sit.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-status w-livre 
FUNCTION fn-status RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    DEF VAR c-sit AS CHAR.

    FIND ob-etiqueta WHERE
         ob-etiqueta.cod-estabel = '505' AND
         ob-etiqueta.nr-container = INTEGER(ttPedItemEtq.nrContainer) AND
         ob-etiqueta.it-codigo = ttPedItemEtq.itCodigo AND
         ob-etiqueta.cod-refer = ttPedItemEtq.codRefer AND
         ob-etiqueta.num-rolo-imp = INTEGER(ttPedItemEtq.rolo) NO-LOCK NO-ERROR.

    IF AVAIL ob-etiqueta THEN DO.
       IF ob-etiqueta.situacao <> 3 THEN 
          ASSIGN c-sit = 'N«O ESTµ EM ESTOQUE'.
       ELSE DO.
          IF ob-etiqueta.quantidade <> ttPedItemEtq.quantidade THEN
             ASSIGN c-sit = 'QTDE DIFERENTE'.
          ELSE
             ASSIGN c-sit = 'OK'.
       END.
    END.
    ELSE
       ASSIGN c-sit = 'N«O ê DA IMA'.

    RETURN c-sit.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

