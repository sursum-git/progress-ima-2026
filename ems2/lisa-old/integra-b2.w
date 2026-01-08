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
DEF NEW GLOBAL SHARED VAR gr-cf-receita-padr AS ROWID NO-UNDO.
DEF NEW GLOBAL SHARED VAR c-seg-usuario      AS CHAR NO-UNDO.

/* Local Variable Definitions ---                                       */
    
DEF TEMP-TABLE tt-pedidos LIKE ped-venda
    FIELD dt-isf            AS DATE FORMAT "99/99/9999" 
    FIELD dt-ret-isf        AS DATE FORMAT "99/99/9999" 
    FIELD marca             AS LOGICAL
    FIELD acao              AS CHAR
    FIELD log-erro-integr   AS LOG
    FIELD nome-emit         LIKE emitente.nome-emit
    FIELD ind-situacao      AS   INTEGER
    FIELD erro-integra      AS LOG
    FIELD pre-pedido        AS CHAR
    FIELD nr-nota-fis       LIKE nota-fiscal.nr-nota-fis
    FIELD serie             LIKE nota-fiscal.serie
    FIELD visualiza         AS LOG INIT YES
    FIELD r-rowid           AS ROWID.

DEFINE TEMP-TABLE tt-itens NO-UNDO LIKE ped-item
    FIELD desc-item         AS CHAR FORMAT "x(50)"
    FIELD qt-separada       LIKE ped-item.qt-pedida
    FIELD acao              AS CHAR.

DEF BUFFER b-tt-pedidos FOR tt-pedidos.
DEF BUFFER b-lisa-integra FOR lisa-integra.

/* Local Variable Definitions ---                                       */
DEF NEW GLOBAL SHARED VAR c-tab-lisa AS CHAR NO-UNDO.

DEF VAR i-cor    AS INT.
DEF VAR c-status AS CHAR.
DEF VAR c-chave  AS CHAR.
DEF VAR l-ok     AS LOGICAL.

DEFINE VARIABLE cErros AS CHARACTER   NO-UNDO.

{esapi/analisarJsonObject2.i}

DEFINE VARIABLE h-acomp AS HANDLE      NO-UNDO.

DEFINE VARIABLE cArquivo AS CHARACTER   NO-UNDO.

DEFINE VARIABLE hBoLisaIntegra01 AS HANDLE      NO-UNDO.
DEFINE VARIABLE lAchouArq        AS LOGICAL     NO-UNDO.

DEFINE VARIABLE lErroRetIsf      AS LOGICAL     NO-UNDO.

DEFINE VARIABLE iCont AS INTEGER     NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-itens

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-itens tt-pedidos

/* Definitions for BROWSE br-itens                                      */
&Scoped-define FIELDS-IN-QUERY-br-itens tt-itens.it-codigo tt-itens.desc-item tt-itens.cod-refer tt-itens.qt-pedida tt-itens.qt-separada   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-itens   
&Scoped-define SELF-NAME br-itens
&Scoped-define OPEN-QUERY-br-itens RUN pi-totais. OPEN QUERY {&SELF-NAME} FOR EACH tt-itens NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-itens tt-itens
&Scoped-define FIRST-TABLE-IN-QUERY-br-itens tt-itens


/* Definitions for BROWSE br-pedidos                                    */
&Scoped-define FIELDS-IN-QUERY-br-pedidos tt-pedidos.nr-pedcli tt-pedidos.nome-abrev tt-pedidos.dt-implant tt-pedidos.dt-isf tt-pedidos.pre-pedido tt-pedidos.nr-nota-fis   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-pedidos   
&Scoped-define SELF-NAME br-pedidos
&Scoped-define OPEN-QUERY-br-pedidos RUN pi-tot-sel. OPEN QUERY {&SELF-NAME} FOR EACH tt-pedidos WHERE                                  tt-pedidos.visualiza = YES NO-LOCK                                  BY tt-pedidos.nr-nota-fis                                  BY tt-pedidos.acao BY tt-pedidos.nr-pedcli.
&Scoped-define TABLES-IN-QUERY-br-pedidos tt-pedidos
&Scoped-define FIRST-TABLE-IN-QUERY-br-pedidos tt-pedidos


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-br-itens}~
    ~{&OPEN-QUERY-br-pedidos}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fiArqSep btExecAcao btEnvLisa btPedLisa ~
fi-lbl-tot-qtde fi-cliente-ini fi-cliente-fin tg-enviar tg-finalizado ~
bt-sel br-pedidos bt-refresh bt-integra fi-nr-pedcli-ini fi-nr-pedcli-fin ~
FILL-IN-12 FILL-IN-10 FILL-IN-13 FILL-IN-8 FILL-IN-14 FILL-IN-15 FILL-IN-16 ~
bt-retorna bt-avanca tg-separar tg-reservar tg-faturar tg-aprovar ~
fi-lbl-qt-sel brReprocSepracao btEtqsSeparadas RECT-2 RECT-4 RECT-96 ~
IMAGE-108 IMAGE-109 RECT-97 IMAGE-3 IMAGE-4 RECT-101 br-itens 
&Scoped-Define DISPLAYED-OBJECTS fiArqSep fi-lbl-tot-qtde fi-tot-ped ~
fi-cliente-ini fi-cliente-fin tg-enviar tg-finalizado fi-nr-pedcli-ini ~
fi-nr-pedcli-fin FILL-IN-12 FILL-IN-10 FILL-IN-13 FILL-IN-8 FILL-IN-14 ~
FILL-IN-15 FILL-IN-16 tg-separar tg-reservar tg-faturar tg-aprovar ~
fi-lbl-qt-sel fi-qt-sel 

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
DEFINE BUTTON brReprocSepracao 
     IMAGE-UP FILE "image/im-cta.bmp":U
     LABEL "" 
     SIZE 5.72 BY 1.46 TOOLTIP "Reprocessar Separa‡Æo".

DEFINE BUTTON bt-avanca 
     IMAGE-UP FILE "image/toolbar/im-reini.bmp":U
     LABEL "ReEnviar" 
     SIZE 5 BY 1.42 TOOLTIP "Avan‡ar Proxima A‡Æo".

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

DEFINE BUTTON bt-retorna 
     IMAGE-UP FILE "image/toolbar/im-undo.bmp":U
     LABEL "ReEnviar" 
     SIZE 5 BY 1.42 TOOLTIP "Retornar A‡Æo Anterior".

DEFINE BUTTON bt-sel AUTO-GO 
     IMAGE-UP FILE "image/im-cq.bmp":U
     LABEL "OK" 
     SIZE 9 BY 2.75 TOOLTIP "Processa Dados".

DEFINE BUTTON btEnvLisa 
     LABEL "Etqs Enviadas LISA" 
     SIZE 15 BY 1.13.

DEFINE BUTTON btEtqsSeparadas 
     LABEL "Etqs. Separadas" 
     SIZE 15 BY 1.13.

DEFINE BUTTON btExecAcao 
     LABEL "Executar A‡Æo" 
     SIZE 15 BY 1.13.

DEFINE BUTTON btPedLisa 
     IMAGE-UP FILE "adeicon/prevw-u.bmp":U
     LABEL "Cons.Lisa" 
     SIZE 5.72 BY 1.46 TOOLTIP "Buscar Dados do Pedido na LISA".

DEFINE VARIABLE fi-cliente-fin AS CHARACTER FORMAT "x(16)" INITIAL "ZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 TOOLTIP "Item final." NO-UNDO.

DEFINE VARIABLE fi-cliente-ini AS CHARACTER FORMAT "x(16)" 
     LABEL "Cliente" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE fi-lbl-qt-sel AS CHARACTER FORMAT "X(256)":U INITIAL "Qt Sel" 
      VIEW-AS TEXT 
     SIZE 6 BY .67
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi-lbl-tot-qtde AS CHARACTER FORMAT "X(256)":U INITIAL "Qtde Total:" 
      VIEW-AS TEXT 
     SIZE 9.57 BY .67
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi-nr-pedcli-fin AS CHARACTER FORMAT "x(12)" INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 TOOLTIP "Pedido final." NO-UNDO.

DEFINE VARIABLE fi-nr-pedcli-ini AS CHARACTER FORMAT "x(12)" 
     LABEL "Pedido":R17 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 TOOLTIP "Pedido inicial." NO-UNDO.

DEFINE VARIABLE fi-qt-sel AS INTEGER FORMAT ">>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi-tot-ped AS DECIMAL FORMAT ">>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiArqSep AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 139 BY .79 NO-UNDO.

DEFINE VARIABLE FILL-IN-10 AS CHARACTER FORMAT "X(256)":U INITIAL "Em Separa‡Æo" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .79
     BGCOLOR 15 FGCOLOR 2 FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-12 AS CHARACTER FORMAT "X(256)":U INITIAL "Enviar ISF" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .79
     BGCOLOR 15 FGCOLOR 9 FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-13 AS CHARACTER FORMAT "X(256)":U INITIAL "Reservar" 
     VIEW-AS FILL-IN 
     SIZE 10.86 BY .71
     FGCOLOR 16 FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-14 AS CHARACTER FORMAT "X(256)":U INITIAL "Aprovar (Enviar NFS Venda)" 
     VIEW-AS FILL-IN 
     SIZE 25 BY .75
     BGCOLOR 15 FGCOLOR 4 FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-15 AS CHARACTER FORMAT "X(256)":U INITIAL "Faturar NFS Cliente" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .71
     FGCOLOR 13 FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-16 AS CHARACTER FORMAT "X(256)":U INITIAL "Finalizado" 
     VIEW-AS FILL-IN 
     SIZE 10.86 BY .79
     FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-8 AS CHARACTER FORMAT "X(256)":U INITIAL "Cancelar" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .79
     BGCOLOR 15 FGCOLOR 12 FONT 17 NO-UNDO.

DEFINE IMAGE IMAGE-108
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-109
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-3
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-4
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE RECTANGLE RECT-101
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 71 BY 3.5.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 66 BY 1.25
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 142 BY 24.25
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-96
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 140 BY 3.25.

DEFINE RECTANGLE RECT-97
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 57 BY 2.5.

DEFINE VARIABLE tg-aprovar AS LOGICAL INITIAL yes 
     LABEL "Enviar NFS" 
     VIEW-AS TOGGLE-BOX
     SIZE 12 BY .67
     FGCOLOR 12  NO-UNDO.

DEFINE VARIABLE tg-enviar AS LOGICAL INITIAL yes 
     LABEL "Enivar ISF" 
     VIEW-AS TOGGLE-BOX
     SIZE 12 BY .67
     FGCOLOR 12  NO-UNDO.

DEFINE VARIABLE tg-faturar AS LOGICAL INITIAL yes 
     LABEL "Faturar" 
     VIEW-AS TOGGLE-BOX
     SIZE 12 BY .67
     FGCOLOR 12  NO-UNDO.

DEFINE VARIABLE tg-finalizado AS LOGICAL INITIAL no 
     LABEL "Finalizado" 
     VIEW-AS TOGGLE-BOX
     SIZE 10.86 BY .67 NO-UNDO.

DEFINE VARIABLE tg-reservar AS LOGICAL INITIAL yes 
     LABEL "Reservar" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .67
     FGCOLOR 12  NO-UNDO.

DEFINE VARIABLE tg-separar AS LOGICAL INITIAL yes 
     LABEL "Em Separa‡Æo" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .67
     FGCOLOR 12  NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-itens FOR 
      tt-itens SCROLLING.

DEFINE QUERY br-pedidos FOR 
      tt-pedidos SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-itens
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-itens B-table-Win _FREEFORM
  QUERY br-itens NO-LOCK DISPLAY
      tt-itens.it-codigo       COLUMN-LABEL "Item"        WIDTH 8
      tt-itens.desc-item       COLUMN-LABEL "Descri‡ao"   WIDTH 34
      tt-itens.cod-refer       COLUMN-LABEL "Ref"         WIDTH 5
      tt-itens.qt-pedida       COLUMN-LABEL "QtPedida"    WIDTH 8
      tt-itens.qt-separada     COLUMN-LABEL "QtSeparada"  WIDTH 10
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 71 BY 13.5
         FONT 1
         TITLE "Itens do Pedido" ROW-HEIGHT-CHARS .67.

DEFINE BROWSE br-pedidos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-pedidos B-table-Win _FREEFORM
  QUERY br-pedidos NO-LOCK DISPLAY
      tt-pedidos.nr-pedcli        COLUMN-LABEL "Pedido"      WIDTH 10
      tt-pedidos.nome-abrev       COLUMN-LABEL "Cliente"     WIDTH 15
      tt-pedidos.dt-implant       COLUMN-LABEL "Data Pedido" WIDTH 10
      tt-pedidos.dt-isf           COLUMN-LABEL "Data ISF"    WIDTH 10
      tt-pedidos.pre-pedido       COLUMN-LABEL "PrePedido"   WIDTH 7
      tt-pedidos.nr-nota-fis      COLUMN-LABEL "Nota Venda"  WIDTH 9
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS MULTIPLE SIZE 68 BY 17.25
         FONT 1
         TITLE "Pedidos em Separa‡Æo" ROW-HEIGHT-CHARS .67.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fiArqSep AT ROW 24.21 COL 1 COLON-ALIGNED NO-LABEL WIDGET-ID 538
     btExecAcao AT ROW 22.38 COL 24.72 WIDGET-ID 536
     btEnvLisa AT ROW 18.5 COL 93 WIDGET-ID 530
     btPedLisa AT ROW 22.21 COL 41.14 WIDGET-ID 514
     fi-lbl-tot-qtde AT ROW 18.67 COL 69 COLON-ALIGNED NO-LABEL WIDGET-ID 502
     fi-tot-ped AT ROW 18.5 COL 79 COLON-ALIGNED NO-LABEL WIDGET-ID 498
     fi-cliente-ini AT ROW 3.25 COL 20 COLON-ALIGNED HELP
          "Codigo do Ötem no EMS" WIDGET-ID 8
     fi-cliente-fin AT ROW 3.25 COL 44 COLON-ALIGNED HELP
          "Codigo do Ötem no EMS" NO-LABEL WIDGET-ID 6
     tg-enviar AT ROW 2.5 COL 79 WIDGET-ID 174
     tg-finalizado AT ROW 3.25 COL 111 WIDGET-ID 214
     bt-sel AT ROW 1.5 COL 132 WIDGET-ID 96
     br-pedidos AT ROW 4.75 COL 2 WIDGET-ID 100
     bt-refresh AT ROW 22.21 COL 2 WIDGET-ID 420
     bt-integra AT ROW 22.21 COL 7 WIDGET-ID 468
     fi-nr-pedcli-ini AT ROW 2.25 COL 20 COLON-ALIGNED HELP
          "N£mero do pedido do cliente" WIDGET-ID 480
     fi-nr-pedcli-fin AT ROW 2.25 COL 44 COLON-ALIGNED NO-LABEL WIDGET-ID 478
     FILL-IN-12 AT ROW 21 COL 77 COLON-ALIGNED NO-LABEL WIDGET-ID 410
     FILL-IN-10 AT ROW 21 COL 88 COLON-ALIGNED NO-LABEL WIDGET-ID 408
     FILL-IN-13 AT ROW 21 COL 103 COLON-ALIGNED NO-LABEL WIDGET-ID 422
     FILL-IN-8 AT ROW 21.75 COL 114.86 COLON-ALIGNED NO-LABEL WIDGET-ID 404
     FILL-IN-14 AT ROW 21.83 COL 77 COLON-ALIGNED NO-LABEL WIDGET-ID 486
     FILL-IN-15 AT ROW 21 COL 114.86 COLON-ALIGNED NO-LABEL WIDGET-ID 488
     FILL-IN-16 AT ROW 21.75 COL 103 COLON-ALIGNED NO-LABEL WIDGET-ID 490
     bt-retorna AT ROW 22.21 COL 12 WIDGET-ID 496
     bt-avanca AT ROW 22.21 COL 17 WIDGET-ID 504
     tg-separar AT ROW 3.25 COL 79 WIDGET-ID 506
     tg-reservar AT ROW 2.5 COL 96 WIDGET-ID 508
     tg-faturar AT ROW 3.25 COL 96 WIDGET-ID 510
     tg-aprovar AT ROW 2.5 COL 111 WIDGET-ID 512
     fi-lbl-qt-sel AT ROW 22.08 COL 60.43 COLON-ALIGNED NO-LABEL WIDGET-ID 516
     fi-qt-sel AT ROW 22.75 COL 60.29 COLON-ALIGNED NO-LABEL WIDGET-ID 518
     brReprocSepracao AT ROW 22.21 COL 46.86 WIDGET-ID 520
     btEtqsSeparadas AT ROW 18.5 COL 108.14 WIDGET-ID 532
     br-itens AT ROW 4.75 COL 71 WIDGET-ID 200
     " Sele‡Æo" VIEW-AS TEXT
          SIZE 8 BY .54 AT ROW 1.08 COL 4.43 WIDGET-ID 444
     " Situa‡Æo" VIEW-AS TEXT
          SIZE 7 BY .75 AT ROW 1.46 COL 71.86 WIDGET-ID 114
     " Legenda" VIEW-AS TEXT
          SIZE 7 BY .54 AT ROW 19.79 COL 72 WIDGET-ID 528
     RECT-2 AT ROW 13 COL 2 WIDGET-ID 4
     RECT-4 AT ROW 1 COL 1 WIDGET-ID 296
     RECT-96 AT ROW 1.25 COL 2 WIDGET-ID 442
     IMAGE-108 AT ROW 3.25 COL 38 WIDGET-ID 22
     IMAGE-109 AT ROW 3.25 COL 42 WIDGET-ID 464
     RECT-97 AT ROW 1.75 COL 71 WIDGET-ID 466
     IMAGE-3 AT ROW 2.25 COL 38 WIDGET-ID 482
     IMAGE-4 AT ROW 2.25 COL 42 WIDGET-ID 484
     RECT-101 AT ROW 20 COL 71 WIDGET-ID 526
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
         HEIGHT             = 24.5
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
/* BROWSE-TAB br-pedidos bt-sel F-Main */
/* BROWSE-TAB br-itens RECT-101 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON bt-integra IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON bt-refresh IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN fi-qt-sel IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-ped IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fiArqSep:READ-ONLY IN FRAME F-Main        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-itens
/* Query rebuild information for BROWSE br-itens
     _START_FREEFORM
RUN pi-totais.
OPEN QUERY {&SELF-NAME} FOR EACH tt-itens NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-itens */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-pedidos
/* Query rebuild information for BROWSE br-pedidos
     _START_FREEFORM
RUN pi-tot-sel.
OPEN QUERY {&SELF-NAME} FOR EACH tt-pedidos WHERE
                                 tt-pedidos.visualiza = YES NO-LOCK
                                 BY tt-pedidos.nr-nota-fis
                                 BY tt-pedidos.acao BY tt-pedidos.nr-pedcli.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-pedidos */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br-itens
&Scoped-define SELF-NAME br-itens
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-itens B-table-Win
ON ROW-DISPLAY OF br-itens IN FRAME F-Main /* Itens do Pedido */
DO:

  ASSIGN i-cor = ?.

  CASE tt-pedidos.acao.
      WHEN 'ENVIAR'  THEN ASSIGN i-cor = 9.
      WHEN 'SEPARAR' THEN ASSIGN i-cor = 2.
      WHEN 'RESERVAR' THEN ASSIGN i-cor = 16.
      WHEN 'FATURAR' THEN ASSIGN i-cor = 13.
      WHEN 'APROVAR' THEN ASSIGN i-cor = 4.
  END CASE.

  ASSIGN tt-itens.it-codigo:FGCOLOR IN BROWSE br-itens = i-cor 
         tt-itens.desc-item:FGCOLOR IN BROWSE br-itens = i-cor 
         tt-itens.cod-refer:FGCOLOR IN BROWSE br-itens = i-cor 
         tt-itens.qt-pedida:FGCOLOR IN BROWSE br-itens = i-cor 
         tt-itens.qt-separada:FGCOLOR IN BROWSE br-itens = i-cor. 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-pedidos
&Scoped-define SELF-NAME br-pedidos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-pedidos B-table-Win
ON ROW-DISPLAY OF br-pedidos IN FRAME F-Main /* Pedidos em Separa‡Æo */
DO:
    ASSIGN i-cor = ?.

    CASE tt-pedidos.acao.
        WHEN 'ENVIAR'  OR WHEN 'ALTERAR' THEN ASSIGN i-cor = 9.
        WHEN 'SEPARAR' THEN ASSIGN i-cor = 2.
        WHEN 'RESERVAR' THEN ASSIGN i-cor = 16.
        WHEN 'FATURAR' THEN ASSIGN i-cor = 13.
        WHEN 'APROVAR' THEN ASSIGN i-cor = 4.
        WHEN 'CANCELAR' THEN ASSIGN i-cor = 12.
    END CASE.

    /*
    IF tt-pedidos.erro-integra THEN
       ASSIGN i-cor = 12. */

   ASSIGN tt-pedidos.nr-pedcli:FGCOLOR IN BROWSE br-pedidos = i-cor
          tt-pedidos.nome-abrev:FGCOLOR IN BROWSE br-pedidos = i-cor
          tt-pedidos.dt-implant:FGCOLOR IN BROWSE br-pedidos = i-cor 
          tt-pedidos.dt-isf:FGCOLOR IN BROWSE br-pedidos = i-cor
          tt-pedidos.nr-nota-fis:FGCOLOR IN BROWSE br-pedidos = i-cor  
          tt-pedidos.pre-pedido:FGCOLOR IN BROWSE br-pedidos = i-cor.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-pedidos B-table-Win
ON VALUE-CHANGED OF br-pedidos IN FRAME F-Main /* Pedidos em Separa‡Æo */
DO:
   DO WITH FRAME {&FRAME-NAME}.
   END.

   EMPTY TEMP-TABLE tt-itens.

   IF AVAIL tt-pedidos THEN DO.
      FOR EACH ped-item OF tt-pedidos WHERE 
               ped-item.cod-sit-item <= 3 NO-LOCK.
          CREATE tt-itens.
          BUFFER-COPY ped-item TO tt-itens.

          FIND item WHERE
               item.it-codigo = tt-itens.it-codigo NO-LOCK NO-ERROR.
          ASSIGN tt-itens.desc-item = item.desc-item.

          /*ASSIGN c-chave = ped-item.nr-pedcli + "|" + ped-item.it-codigo + "|" + ped-item.cod-refer.
          FOR EACH lisa-integra WHERE
                   lisa-integra.cod-trans = 'RetornoISF' AND 
                   lisa-integra.chave = c-chave NO-LOCK.
            
              FIND ob-etiqueta WHERE
                   ob-etiqueta.cod-estabel = tt-pedidos.cod-estabel AND
                   ob-etiqueta.num-etiqueta = INTEGER(lisa-integra.conteudo)
                   NO-LOCK NO-ERROR.
              IF AVAIL ob-etiqueta THEN
                 ASSIGN tt-itens.qt-separada = tt-itens.qt-separada + 
                                               ob-etiqueta.quantidade.
          END.*/
          RUN esapi/getQtEtqPedido.p(tt-Pedidos.nr-pedido,
                                     ped-item.it-codigo,
                                     ped-item.cod-refer,
                                     OUTPUT tt-itens.qt-separada).

         

      END.
   END.
   {&OPEN-QUERY-br-itens}
   RUN atuDescrSep.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME brReprocSepracao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brReprocSepracao B-table-Win
ON CHOOSE OF brReprocSepracao IN FRAME F-Main
DO:
  IF AVAIL tt-pedidos THEN DO:
    RUN utp/ut-acomp.p PERSIST SET h-acomp.
    RUN pi-inicializar IN h-acomp("Reprocessando Separa‡Æo").
    RUN pi-acompanhar  IN h-acomp("Pedido:" + tt-pedidos.nr-pedcli).
    
    RUN setPedido IN hBoLisaIntegra01(tt-pedidos.nr-pedcli).
    RUN processar IN hBoLisaIntegra01.
    
    RUN pi-finalizar IN h-acomp.
    RUN getAchouArq IN hBoLisaIntegra01(OUTPUT lAchouArq).
    
   
    IF NOT lAchouArq THEN
       MESSAGE 'NÆo foi encontrado arquivo de Separa‡Æo para o Pedido:' tt-pedidos.nr-pedcli
           VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    ELSE DO:
        RUN getErroRetIsf IN hBoLisaIntegra01(OUTPUT lErroRetIsf).
        IF lErroRetIsf THEN
           MESSAGE "Ocorreram erros durante o processamento do arquivo,verifique o log"
               VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        ELSE
          MESSAGE 'Arquivo de Separa‡Æo Processado com SUCESSO'
           VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.        
    END.                                                    
  END.                                                      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-avanca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-avanca B-table-Win
ON CHOOSE OF bt-avanca IN FRAME F-Main /* ReEnviar */
DO:
    IF tt-pedidos.acao = '' OR
       tt-pedidos.acao = 'CANCELAR' THEN 
       RETURN NO-APPLY.

    FOR EACH lisa-integra WHERE
             lisa-integra.cod-trans = 'ISF' AND
             lisa-integra.chave BEGINS STRING(tt-pedidos.nr-pedcli) SHARE-LOCK.

        ASSIGN lisa-integra.ind-situacao = 1.

        CASE tt-pedidos.acao.
            WHEN 'ENVIAR' OR WHEN 'ALTERAR' THEN ASSIGN lisa-integra.acao = 'SEPARAR'.
            WHEN 'SEPARAR'  THEN ASSIGN lisa-integra.acao = 'RESERVAR'.
            WHEN 'RESERVAR' THEN ASSIGN lisa-integra.acao = 'FATURAR'.
            WHEN 'FATURAR'  THEN ASSIGN lisa-integra.acao = 'APROVAR'.
            WHEN 'APROVAR'  THEN DO.
                 ASSIGN c-chave = tt-pedidos.cod-estabel + "|" +
                                  tt-pedidos.serie + "|" + 
                                  tt-pedidos.nr-nota-fis.

                 FOR EACH b-lisa-integra WHERE
                          b-lisa-integra.cod-trans = "RemessaNotaVenda" AND 
                          b-lisa-integra.chave = c-chave SHARE-LOCK.
                     ASSIGN b-lisa-integra.ind-situacao = 2. 
                 END.                 
                 ASSIGN lisa-integra.acao = ''.  // tem que cancelar a nota
            END.
        END CASE.

        ASSIGN tt-pedidos.acao = lisa-integra.acao.
        LEAVE.
    END.

    br-pedidos:REFRESH().
    APPLY 'VALUE-CHANGED' TO br-pedidos.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-integra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-integra B-table-Win
ON CHOOSE OF bt-integra IN FRAME F-Main
DO:
        
    CASE tt-pedidos.acao.
        WHEN 'ENVIAR' OR WHEN 'ALTERAR' THEN ASSIGN c-tab-lisa = 'ISF'.
        WHEN 'SEPARAR'  THEN.  // nÆo pode fazer nada
        WHEN 'RESERVAR' THEN ASSIGN c-tab-lisa = 'RetornoISF'.
        WHEN 'FATURAR'  THEN.  // utilizar essp0161
        WHEN 'APROVAR'  THEN ASSIGN c-tab-lisa = 'RemessaNotaVenda'.
    END CASE.

    RUN lisa/rel-giv.w.

    APPLY 'CHOOSE' TO bt-refresh.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-refresh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-refresh B-table-Win
ON CHOOSE OF bt-refresh IN FRAME F-Main
DO:
   RUN pi-processa.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-retorna
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-retorna B-table-Win
ON CHOOSE OF bt-retorna IN FRAME F-Main /* ReEnviar */
DO:
    IF tt-pedidos.acao = 'ENVIAR' OR 
       tt-pedidos.acao = 'ALTERAR' OR
       tt-pedidos.acao = 'CANCELAR' THEN
       RETURN NO-APPLY.


    FOR EACH lisa-integra WHERE
             lisa-integra.cod-trans = 'ISF' AND
             lisa-integra.chave BEGINS STRING(tt-pedidos.nr-pedcli) SHARE-LOCK.

        ASSIGN lisa-integra.ind-situacao = 1.

        CASE tt-pedidos.acao.
            WHEN 'ENVIAR' THEN.  // retornar para o essp0204
            WHEN 'SEPARAR' THEN DO.
                MESSAGE 'OK, Pedido de Venda RETORNADO para o ESSP0204' SKIP(1)
                        'Deseja EXCLUIR o Pre-Pedido ? ' SKIP
                        'Se sim, esse Pre-Pedido NÇO PODERµ SER RECURAPADO e,' 
                        'ser  Exclu¡do na LISA !!!' SKIP(1)
                        '<SIM - Solicita a LISA a EXCLUSÇO do PR-PEDIDO' SKIP
                        '<NAO - PR-PEDIDO ser  Mantido' SKIP(1)
                    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE l-limpa-pre-pedido AS LOGICAL.

                IF l-limpa-pre-pedido THEN DO.
                   FIND ped-venda WHERE
                        ped-venda.nr-pedcli = ENTRY(1,lisa-integra.chave,"|") AND
                        ped-venda.nome-abrev = ENTRY(2,lisa-integra.chave,"|") NO-LOCK NO-ERROR.

                   RUN lisa/acoesPedidoLISA.r (INPUT ped-venda.nr-pedcli,
                                               INPUT 2,  // Cancelar o pedido
                                               OUTPUT l-ok).

                   IF l-ok = NO THEN RETURN NO-APPLY.

                   ASSIGN lisa-integra.val-livre-1 = ''.

                   FIND ped-venda-ext WHERE
                        ped-venda-ext.cod-estabel = ped-venda.cod-estabel AND 
                        ped-venda-ext.nr-pedido = ped-venda.nr-pedido
                        SHARE-LOCK NO-ERROR.
                   IF AVAIL ped-venda-ext THEN
                      ASSIGN ped-venda-ext.dt-isf = ?
                             ped-venda-ext.nr-pedext = ''.
     
                   ASSIGN tt-pedidos.dt-isf = ?     
                          tt-pedidos.pre-pedido = ''.  

                   FIND CURRENT ped-venda-ext NO-LOCK NO-ERROR.

                   RUN esapi/cria-log-pedvenda.p (INPUT ped-venda.nr-pedcli,
                                                  INPUT ped-venda.nome-abrev,
                                                  INPUT "Eliminado Pr‚Pedido LISA (integra-b2.w)" ,
                                                  INPUT NO).
                END.

                ASSIGN lisa-integra.acao = 'ENVIAR'.  // manda alterar
            END.
            WHEN 'RESERVAR' THEN DO.
                MESSAGE 'Deseja Cancelar a Separa‡Æo do PR-PEDIDO na LISA ? ' SKIP(1)

                        '<SIM - Solicita a LISA o Cancelamento da Separa‡Æo' SKIP
                        '<NAO - Reabre Separa‡Æo' SKIP(1)

                    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE l-canc-separacao AS LOGICAL.

                ASSIGN l-ok = YES.
                IF l-canc-separacao THEN DO.
                   FIND ped-venda WHERE
                        ped-venda.nr-pedcli = ENTRY(1,lisa-integra.chave,"|") AND
                        ped-venda.nome-abrev = ENTRY(2,lisa-integra.chave,"|") NO-LOCK NO-ERROR.

                   RUN lisa/acoesPedidoLISA.r (INPUT ped-venda.nr-pedcli,
                                               INPUT 1,  // Solicitar Cancelamento
                                               OUTPUT l-ok).

                    IF l-ok = NO THEN RETURN NO-APPLY.

                    MESSAGE 'Separa‡Æo do PR-PEDIDO foi Cancelada na LISA'
                           VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.

                    FOR EACH b-lisa-integra WHERE
                             b-lisa-integra.cod-trans = 'RetornoISF' AND 
                             b-lisa-integra.chave BEGINS STRING(tt-pedidos.nr-pedcli) SHARE-LOCK.
                        DELETE b-lisa-integra.
                    END.                 
                END.
                ASSIGN lisa-integra.acao = 'SEPARAR'.  
            END.
            WHEN 'FATURAR' THEN DO.
                MESSAGE 'ATEN€ÇO !!!! ' SKIP
                        'As Reservas ser  Cancelada no ERP TOTVS...' SKIP
                        'Separa‡Æo da LISA ser  mantida...' SKIP(1)
                        'Confirma ? '
                    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE l-confirma AS LOGICAL.
                IF NOT l-confirma THEN RETURN NO-APPLY.

                // Cancela a Reserva
                RUN esapi/elimina-reserva-pedvenda.p (INPUT tt-pedidos.nr-pedcli).
                ASSIGN lisa-integra.acao = 'RESERVAR'. 
            END.
            WHEN 'APROVAR' THEN DO.
                // Verifica se a NOTA est  cancelada
                
                FIND LAST nota-fiscal WHERE
                          nota-fiscal.nome-ab-cli = tt-pedidos.nome-abrev AND
                          nota-fiscal.nr-pedcli = tt-pedidos.nr-pedcli AND
                          nota-fiscal.dt-cancela  = ? NO-LOCK NO-ERROR.

                IF AVAIL nota-fiscal THEN DO.
                   MESSAGE 'Favor Cancelar a Nota Fiscal'
                       VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
                   RETURN NO-APPLY.
                END.
                
                ASSIGN lisa-integra.acao = 'FATURAR'.  
            END.
            WHEN '' THEN DO.
                ASSIGN c-chave = tt-pedidos.cod-estabel + "|" +
                                 tt-pedidos.serie + "|" + 
                                 tt-pedidos.nr-nota-fis.
                                 
                FOR EACH b-lisa-integra WHERE
                         b-lisa-integra.cod-trans = "RemessaNotaVenda" AND 
                         b-lisa-integra.chave = c-chave SHARE-LOCK.
                    ASSIGN b-lisa-integra.ind-situacao = 1. 
                END.                 
                ASSIGN lisa-integra.acao = 'APROVAR'.  // tem que cancelar a nota
            END.
        END CASE.

        ASSIGN tt-pedidos.acao = lisa-integra.acao.
        
        LEAVE.
    END.
    RELEASE lisa-integra.


    br-pedidos:REFRESH().
    APPLY 'VALUE-CHANGED' TO br-pedidos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-sel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-sel B-table-Win
ON CHOOSE OF bt-sel IN FRAME F-Main /* OK */
DO:
   ASSIGN INPUT FRAME {&FRAME-NAME} fi-nr-pedcli-ini fi-nr-pedcli-fin
                                    fi-cliente-ini fi-cliente-fin
                                    tg-enviar tg-separar tg-reservar
                                    tg-faturar tg-aprovar tg-finalizado.

   RUN pi-processa.

   FOR EACH tt-pedidos NO-LOCK.
       ASSIGN tt-pedidos.visualiza = NO.
   END.

   RUN pi-open-query.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btEnvLisa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btEnvLisa B-table-Win
ON CHOOSE OF btEnvLisa IN FRAME F-Main /* Etqs Enviadas LISA */
DO:
   IF AVAIL tt-pedidos AND AVAIL tt-itens THEN DO:
      RUN lisa/espd701.w(tt-pedidos.nr-pedcli,tt-itens.it-codigo, tt-itens.cod-refer ).
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btEtqsSeparadas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btEtqsSeparadas B-table-Win
ON CHOOSE OF btEtqsSeparadas IN FRAME F-Main /* Etqs. Separadas */
DO:
   IF AVAIL tt-pedidos AND AVAIL tt-itens THEN DO:
      RUN lisa/espd700.w(tt-pedidos.nr-pedcli,tt-itens.it-codigo, tt-itens.cod-refer ).
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btExecAcao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btExecAcao B-table-Win
ON CHOOSE OF btExecAcao IN FRAME F-Main /* Executar A‡Æo */
DO:

 DO iCont = 1 TO br-pedidos:NUM-SELECTED-ROWS:
    IF AVAIL tt-pedidos THEN
  RUN lisa/aplicarAcaoCorrenteLisaIntegra.p(tt-pedidos.r-rowid, OUTPUT cErros).
  IF cErros <> '' THEN
     MESSAGE "pedido:" tt-pedidos.nr-pedcli SKIP
             cErros
         VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
 END.
 APPLY 'choose' TO bt-refresh.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btPedLisa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btPedLisa B-table-Win
ON CHOOSE OF btPedLisa IN FRAME F-Main /* Cons.Lisa */
DO:
  IF AVAIL tt-pedidos THEN DO:
    RUN utp/ut-acomp.p PERSIST SET h-acomp.
    RUN pi-inicializar IN h-acomp("BUSCANDO INFORMA€åES NA LISA").
    IF tt-Pedidos.pre-pedido = '' THEN DO:
       MESSAGE "Pedido de Venda sem Pr‚-Pedido"
           VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
       RETURN NO-APPLY.

    END.

    RUN pi-acompanhar  IN h-acomp("Pr‚-Pedido:" + tt-pedidos.pre-pedido).
    RUN lisa/consultarPedVenda.p(tt-pedidos.pre-pedido,
                        OUTPUT cErros,
                        OUTPUT TABLE ttJson
                        ).
     IF cErros <> '' THEN DO:
         MESSAGE cErros
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
         RUN pi-finalizar IN h-acomp.
     END.
        

     ELSE DO:
        /*ASSIGN cArquivo = SESSION:TEMP-DIRECTORY + "cons_pedido_lisa_" + STRING(TIME) + ".csv".
        OUTPUT TO value(cArquivo).
        FOR EACH  ttjson.
            EXPORT DELIMITER ";" ttjson.
        END.
        OUTPUT CLOSE.

        OS-COMMAND SILENT VALUE("start excel " + cArquivo).
        */
        RUN pi-finalizar IN h-acomp.
        RUN lisa/dadosPrePedidoLisa.w(INPUT TABLE ttJson).

     END.
        
     
     
  END.
  


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cliente-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cliente-ini B-table-Win
ON LEAVE OF fi-cliente-ini IN FRAME F-Main /* Cliente */
DO:
  ASSIGN fi-cliente-fin:SCREEN-VALUE = 'ZZZZZZZZZZZZZZZZ'.
  IF SELF:SCREEN-VALUE <> '' THEN
     ASSIGN fi-cliente-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-nr-pedcli-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nr-pedcli-ini B-table-Win
ON LEAVE OF fi-nr-pedcli-ini IN FRAME F-Main /* Pedido */
DO:
  ASSIGN fi-nr-pedcli-fin:SCREEN-VALUE = 'ZZZZZZZZZ'
         fi-nr-pedcli-fin:SENSITIVE = YES.
  IF SELF:SCREEN-VALUE <> '' THEN DO.
     ASSIGN fi-nr-pedcli-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
     ASSIGN fi-nr-pedcli-fin:SENSITIVE = NO.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-aprovar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-aprovar B-table-Win
ON VALUE-CHANGED OF tg-aprovar IN FRAME F-Main /* Enviar NFS */
DO:
   APPLY 'choose' TO bt-sel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-enviar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-enviar B-table-Win
ON VALUE-CHANGED OF tg-enviar IN FRAME F-Main /* Enivar ISF */
DO:
   APPLY 'choose' TO bt-sel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-faturar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-faturar B-table-Win
ON VALUE-CHANGED OF tg-faturar IN FRAME F-Main /* Faturar */
DO:
   APPLY 'choose' TO bt-sel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-finalizado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-finalizado B-table-Win
ON VALUE-CHANGED OF tg-finalizado IN FRAME F-Main /* Finalizado */
DO:
  APPLY 'choose' TO bt-sel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-reservar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-reservar B-table-Win
ON VALUE-CHANGED OF tg-reservar IN FRAME F-Main /* Reservar */
DO:
   APPLY 'choose' TO bt-sel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-separar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-separar B-table-Win
ON VALUE-CHANGED OF tg-separar IN FRAME F-Main /* Em Separa‡Æo */
DO:
   APPLY 'choose' TO bt-sel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-itens
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE atuDescrSep B-table-Win 
PROCEDURE atuDescrSep :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE cDescr AS CHARACTER   NO-UNDO.
DEFINE VARIABLE fiArqSeparacao AS CHAR.
IF AVAIL tt-pedidos THEN DO:

   RUN setPedido IN hBoLisaIntegra01(tt-pedidos.nr-pedcli).
   RUN getDescrArq IN hBoLisaIntegra01(OUTPUT cDescr).

   /*
   ASSIGN fiArqSeparacao:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Pedido:"  + tt-pedidos.nr-pedcli 
                                                            + " | " + cDescr .
   */

   ASSIGN fiArqSep:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Pedido:"  + tt-pedidos.nr-pedcli + " | " + cDescr .
   
   

END.

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
DEFINE VARIABLE lPermissao AS LOGICAL     NO-UNDO.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}.
  END.

  ASSIGN fi-lbl-tot-qtde:FONT = 6
         fi-tot-ped:FONT = 6
         fi-qt-sel:FONT = 6
         fi-lbl-qt-sel:FONT = 6.

  RUN esapi/verificUsuarioGrupo.p('lis',c-seg-usuario, OUTPUT lPermissao).

  FIND usuar_grp_usuar WHERE
       usuar_grp_usuar.cod_usuar = c-seg-usuario AND
       usuar_grp_usuar.cod_grp_usuar = 'SUP' NO-LOCK NO-ERROR.
  IF AVAIL usuar_grp_usuar THEN
     ASSIGN lPermissao = YES.


  ASSIGN bt-avanca:SENSITIVE IN FRAME {&FRAME-NAME} = lPermissao
         bt-retorna:SENSITIVE IN FRAME {&FRAME-NAME} = lPermissao .
  IF NOT VALID-HANDLE(hBolisaIntegra01) THEN
     RUN esbo/boLisaIntegra01.p PERSIST SET hBoLisaIntegra01.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-open-query B-table-Win 
PROCEDURE pi-open-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FOR EACH tt-pedidos WHERE
            tt-pedidos.nr-pedcli >= fi-nr-pedcli-ini AND
            tt-pedidos.nr-pedcli <= fi-nr-pedcli-fin NO-LOCK.

       ASSIGN tt-pedidos.visualiza = YES.

       ASSIGN tt-pedidos.visualiza = fn-situacao().
   END.

   {&OPEN-QUERY-br-pedidos}
   APPLY 'VALUE-CHANGED' TO br-pedidos IN FRAME {&FRAME-NAME}.

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

    RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
    RUN pi-inicializar IN h-acomp("Analise de JSON de Retorno").

    EMPTY TEMP-TABLE tt-pedidos.
    ASSIGN INPUT FRAME {&FRAME-NAME} fi-nr-pedcli-ini fi-nr-pedcli-fin
                                     fi-cliente-ini fi-cliente-fin
                                     tg-enviar tg-separar tg-reservar
                                     tg-faturar tg-aprovar tg-finalizado.

    ASSIGN i-situacao = 1.
    IF tg-finalizado THEN
       ASSIGN i-situacao = 2.

    FOR EACH lisa-integra WHERE
             lisa-integra.cod-trans = 'ISF' AND    // Instru‡ao de Separa‡ao
             (lisa-integra.chave BEGINS fi-nr-pedcli-ini OR fi-nr-pedcli-ini = '') AND 
             lisa-integra.ind-situacao <= i-situacao SHARE-LOCK USE-INDEX idx2.

        RUN pi-acompanhar IN h-acomp ("Pedido:" + ENTRY(1,lisa-integra.chave,"|")).

        FIND ped-venda WHERE 
             ped-venda.nr-pedido = INTEGER(ENTRY(1,lisa-integra.chave,"|") ) 
             USE-INDEX ch-pedseq NO-LOCK NO-ERROR.

        /*
        FIND ped-venda WHERE
             ped-venda.nr-pedcli = ENTRY(1,lisa-integra.chave,"|") AND
             ped-venda.nome-abrev = ENTRY(2,lisa-integra.chave,"|") NO-LOCK NO-ERROR.
        */

        IF NOT AVAIL ped-venda THEN DO.
           DELETE lisa-integra.
           NEXT.
        END.

        // Pedido foi Cancelado
        IF ped-venda.cod-sit-ped = 6 THEN DO.
           DELETE lisa-integra.
           NEXT.
        END.

        /*
        CREATE tt-pedidos.
        BUFFER-COPY ped-venda TO tt-pedidos.
        */
        
        CREATE tt-pedidos.
        ASSIGN tt-pedidos.r-rowid   = ROWID(lisa-integra)
               tt-pedidos.nr-pedido = INTEGER(ENTRY(1,lisa-integra.chave,"|"))
               tt-pedidos.nr-pedcli = ped-venda.nr-pedcli
               tt-pedidos.nome-abrev = ped-venda.nome-abrev.
               tt-pedidos.dt-implant = ped-venda.dt-implant.

        ASSIGN tt-pedidos.acao          = lisa-integra.acao.

        FIND ped-venda-ext WHERE
             ped-venda-ext.cod-estabel = ped-venda.cod-estabel AND 
             ped-venda-ext.nr-pedido = ped-venda.nr-pedido
             NO-LOCK NO-ERROR.
        IF AVAIL ped-venda-ext THEN DO.
           ASSIGN tt-pedidos.dt-isf = ped-venda-ext.dt-isf
                  tt-pedidos.pre-pedido = ped-venda-ext.nr-pedext.
        END.

        FIND LAST nota-fiscal WHERE
                  nota-fiscal.nome-ab-cli = ped-venda.nome-abrev AND
                  nota-fiscal.nr-pedcli = ped-venda.nr-pedcli AND
                  nota-fiscal.dt-cancela  = ? NO-LOCK NO-ERROR.
        IF AVAIL nota-fiscal THEN
           ASSIGN tt-pedidos.nr-nota-fis = nota-fiscal.nr-nota-fis
                  tt-pedidos.serie = nota-fiscal.serie
                  tt-pedidos.cod-estab = nota-fiscal.cod-estabel.
    END.
    FIND FIRST lisa-integra NO-LOCK NO-ERROR.

    RUN pi-finalizar IN h-acomp.

    RUN pi-open-query.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-tot-sel B-table-Win 
PROCEDURE pi-tot-sel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN fi-qt-sel = 0.
    FOR EACH tt-pedidos WHERE
             tt-pedidos.visualiza = YES NO-LOCK.
        ASSIGN fi-qt-sel = fi-qt-sel + 1.
    END.
    DISP fi-qt-sel WITH FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-totais B-table-Win 
PROCEDURE pi-totais :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN fi-tot-ped = 0.
    FOR EACH tt-itens NO-LOCK.
        ASSIGN fi-tot-ped = fi-tot-ped + tt-itens.qt-pedida.
    END.
    DISP fi-tot-ped WITH FRAME {&FRAME-NAME}.
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
  {src/adm/template/snd-list.i "tt-pedidos"}
  {src/adm/template/snd-list.i "tt-itens"}

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

    IF tg-enviar     = YES AND tt-pedidos.acao = 'ENVIAR' THEN ASSIGN l-mostra = YES.
    IF tg-separar    = YES AND tt-pedidos.acao = 'SEPARAR'  THEN ASSIGN l-mostra = YES.
    IF tg-reservar   = YES AND tt-pedidos.acao = 'RESERVAR'  THEN ASSIGN l-mostra = YES.
    IF tg-faturar    = YES AND tt-pedidos.acao = 'FATURAR'  THEN ASSIGN l-mostra = YES.
    IF tg-aprovar    = YES AND tt-pedidos.acao = 'APROVAR'  THEN ASSIGN l-mostra = YES.
    IF tg-finalizado = YES AND tt-pedidos.acao = ''  THEN ASSIGN l-mostra = YES.

    RETURN l-mostra.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

