&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          espec            PROGRESS
*/
&Scoped-define WINDOW-NAME w-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-digita 
/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i XX9999 9.99.99.999}
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: 
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

/* ***************************  Definitions  ************************** */
DEF NEW GLOBAL SHARED VAR gr-ped-venda  AS ROWID NO-UNDO.

/* Local Variable Definitions ---                                       */
DEF TEMP-TABLE tt_peds_web LIKE peds_web
    FIELD nome-rep      LIKE repres.nome-abrev
    FIELD nome-abrev    LIKE emitente.nome-abrev
    FIELD desc-situacao AS CHAR
    FIELD completo      AS LOGICAL.

DEF VAR c-situacao       AS CHAR FORMAT "x(20)".
DEF VAR c-sit-web        AS CHAR FORMAT "x(20)".
DEF VAR c-nome-abrev     AS CHAR FORMAT "x(15)".
DEF VAR c-desc-item      AS CHAR.
DEF VAR de-vl-preori     AS DECIMAL.
DEF VAR c-nome-reppri    LIKE repres.nome-abrev.
DEF VAR i-cor            AS INTEGER.
DEF VAR i-cod-reppri-ini LIKE repres.cod-rep.
DEF VAR i-cod-reppri-fin LIKE repres.cod-rep.
DEF VAR da-dt-trans-ini  AS DATETIME.
DEF VAR da-dt-trans-fin  AS DATETIME.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Digitacao
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-itens

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES itens_ped_web tt_peds_web

/* Definitions for BROWSE br-itens                                      */
&Scoped-define FIELDS-IN-QUERY-br-itens itens_ped_web.it_codigo fn-desc-item() @ c-desc-item itens_ped_web.cod_refer itens_ped_web.qt_pedida fn-preco-item() @ de-vl-preori   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-itens   
&Scoped-define SELF-NAME br-itens
&Scoped-define OPEN-QUERY-br-itens RUN pi-totais. OPEN QUERY {&SELF-NAME} FOR EACH itens_ped_web WHERE                                  itens_ped_web.ped_web_id = tt_peds_web.ped_web_id                                  NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-itens itens_ped_web
&Scoped-define FIRST-TABLE-IN-QUERY-br-itens itens_ped_web


/* Definitions for BROWSE br-pedidos-web                                */
&Scoped-define FIELDS-IN-QUERY-br-pedidos-web tt_peds_web.ped_web_id tt_peds_web.nome-rep tt_peds_web.dt_hr_registro tt_peds_web.cod_tipo_pedido tt_peds_web.nr_pedido_erp tt_peds_web.nome-abrev tt_peds_web.desc-situacao tt_peds_web.nr_container tt_peds_web.descr_rejeicao //tt-ped-venda.desc-cond-pag   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-pedidos-web   
&Scoped-define SELF-NAME br-pedidos-web
&Scoped-define QUERY-STRING-br-pedidos-web FOR EACH tt_peds_web WHERE                                  tt_peds_web.dt_hr_registro >= da-dt-trans-ini AND                                  tt_peds_web.dt_hr_registro <= da-dt-trans-fin AND                                  LOOKUP(STRING(tt_peds_web.ind_sit_ped_web), ~
      c-sit-web) > 0 AND                                  (tt_peds_web.completo = rs-sit-com OR rs-sit-com = ?)                                  NO-LOCK BY tt_peds_web.dt_hr_registro DESCENDING
&Scoped-define OPEN-QUERY-br-pedidos-web OPEN QUERY {&SELF-NAME} FOR EACH tt_peds_web WHERE                                  tt_peds_web.dt_hr_registro >= da-dt-trans-ini AND                                  tt_peds_web.dt_hr_registro <= da-dt-trans-fin AND                                  LOOKUP(STRING(tt_peds_web.ind_sit_ped_web), ~
      c-sit-web) > 0 AND                                  (tt_peds_web.completo = rs-sit-com OR rs-sit-com = ?)                                  NO-LOCK BY tt_peds_web.dt_hr_registro DESCENDING.
&Scoped-define TABLES-IN-QUERY-br-pedidos-web tt_peds_web
&Scoped-define FIRST-TABLE-IN-QUERY-br-pedidos-web tt_peds_web


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-br-itens}~
    ~{&OPEN-QUERY-br-pedidos-web}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 rt-buttom-2 RECT-9 RECT-74 IMAGE-88 ~
IMAGE-89 IMAGE-96 IMAGE-97 RECT-75 fi-dt-trans-ini fi-dt-trans-fin bt-sel ~
rs-sit-com tg-digita tg-integrado tg-efetivado tg-rejeitado cb-tp-pedido ~
tg-cancelado tg-vencido br-pedidos-web bt-modifica bt-log bt-status ~
FILL-IN-6 FILL-IN-8 FILL-IN-3 FILL-IN-10 FILL-IN-2 FILL-IN-9 FILL-IN-11 ~
FILL-IN-12 FILL-IN-15 FILL-IN-16 FILL-IN-13 FILL-IN-14 br-itens bt-ajuda ~
bt-ok 
&Scoped-Define DISPLAYED-OBJECTS fi-dt-trans-ini fi-dt-trans-fin rs-sit-com ~
tg-digita tg-integrado fi-no-ab-reppri-ini fi-no-ab-reppri-fin tg-efetivado ~
tg-rejeitado cb-tp-pedido tg-cancelado tg-vencido FILL-IN-6 FILL-IN-8 ~
FILL-IN-3 FILL-IN-10 FILL-IN-2 FILL-IN-9 FILL-IN-11 FILL-IN-12 FILL-IN-15 ~
FILL-IN-16 FILL-IN-13 FILL-IN-14 fi-vl-tot-ped 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 fi-dt-trans-ini fi-dt-trans-fin 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-desc-item w-digita 
FUNCTION fn-desc-item RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-preco-item w-digita 
FUNCTION fn-preco-item RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-digita AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ajuda 
     LABEL "Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-cancela AUTO-GO 
     IMAGE-UP FILE "image/im-cance.bmp":U
     LABEL "" 
     SIZE 5.86 BY 1.29 TOOLTIP "Cancelar Pedido"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-log AUTO-GO 
     IMAGE-UP FILE "image/im-log.bmp":U
     LABEL "" 
     SIZE 5.86 BY 1.29 TOOLTIP "Altera‡äes do Pedido"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-modifica 
     IMAGE-UP FILE "image/im-mod.bmp":U
     LABEL "&Sair" 
     SIZE 5.86 BY 1.29 TOOLTIP "Modificar Pedido"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "Sair" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-sel 
     IMAGE-UP FILE "image/im-cq.bmp":U
     LABEL "OK" 
     SIZE 6 BY 3.25 TOOLTIP "Processa Dados".

DEFINE BUTTON bt-status 
     IMAGE-UP FILE "image/imt-smfec.bmp":U
     IMAGE-INSENSITIVE FILE "image/imt-smfec.bmp":U
     LABEL "" 
     SIZE 5.86 BY 2.25 TOOLTIP "Status do Pedido / Confirma Pedido"
     BGCOLOR 8 FONT 10.

DEFINE VARIABLE cb-tp-pedido AS CHARACTER FORMAT "X(256)":U INITIAL "Todos" 
     LABEL "Tipo de Pedido" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "PE","PI","Todos" 
     DROP-DOWN-LIST
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE fi-dt-trans-fin AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-trans-ini AS DATE FORMAT "99/99/9999":U 
     LABEL "Periodo" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE fi-no-ab-reppri-fin AS CHARACTER FORMAT "x(16)" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 TOOLTIP "Representante final." NO-UNDO.

DEFINE VARIABLE fi-no-ab-reppri-ini AS CHARACTER FORMAT "x(16)" 
     LABEL "Representante" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 TOOLTIP "Representante inicial." NO-UNDO.

DEFINE VARIABLE fi-vl-tot-ped AS DECIMAL FORMAT ">>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15.43 BY .88
     FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-10 AS CHARACTER FORMAT "X(256)":U INITIAL "Efetivado" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .79
     BGCOLOR 15 FGCOLOR 2 FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-11 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 2 BY .79
     BGCOLOR 9  NO-UNDO.

DEFINE VARIABLE FILL-IN-12 AS CHARACTER FORMAT "X(256)":U INITIAL "Integrado" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .79
     BGCOLOR 15 FGCOLOR 9 FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-13 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 2 BY .79
     BGCOLOR 0  NO-UNDO.

DEFINE VARIABLE FILL-IN-14 AS CHARACTER FORMAT "X(256)":U INITIAL "Vencido" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .79
     BGCOLOR 15 FGCOLOR 0 FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-15 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 2 BY .79
     BGCOLOR 5  NO-UNDO.

DEFINE VARIABLE FILL-IN-16 AS CHARACTER FORMAT "X(256)":U INITIAL "Rejeitado" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .79
     BGCOLOR 15 FGCOLOR 5 FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 2 BY .79
     BGCOLOR 16 FGCOLOR 16  NO-UNDO.

DEFINE VARIABLE FILL-IN-3 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 2 BY .79
     BGCOLOR 2  NO-UNDO.

DEFINE VARIABLE FILL-IN-6 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 2 BY .79
     BGCOLOR 12 FGCOLOR 12  NO-UNDO.

DEFINE VARIABLE FILL-IN-8 AS CHARACTER FORMAT "X(256)":U INITIAL "em Digita‡Æo" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .79
     BGCOLOR 15 FGCOLOR 12 FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-9 AS CHARACTER FORMAT "X(256)":U INITIAL "Cancelado" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .79
     BGCOLOR 15 FGCOLOR 16 FONT 17 NO-UNDO.

DEFINE IMAGE IMAGE-88
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-89
     FILENAME "image\im-las":U
     SIZE 3 BY .79.

DEFINE IMAGE IMAGE-96
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-97
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE VARIABLE rs-sit-com AS LOGICAL 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Confirmado", yes,
"Pendente", no,
"Ambos", ?
     SIZE 11 BY 2.5 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 108.29 BY 1.38
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-74
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 29 BY 3.08.

DEFINE RECTANGLE RECT-75
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 13 BY 3.08.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 108.43 BY 3.79.

DEFINE RECTANGLE rt-buttom-2
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 8 BY 7.83
     BGCOLOR 8 FGCOLOR 0 .

DEFINE VARIABLE tg-cancelado AS LOGICAL INITIAL no 
     LABEL "Cancelado" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE tg-digita AS LOGICAL INITIAL no 
     LABEL "em Digita‡Æo" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83
     FGCOLOR 12  NO-UNDO.

DEFINE VARIABLE tg-efetivado AS LOGICAL INITIAL no 
     LABEL "Efetivado" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE tg-integrado AS LOGICAL INITIAL yes 
     LABEL "Integrado" 
     VIEW-AS TOGGLE-BOX
     SIZE 9.72 BY .83 NO-UNDO.

DEFINE VARIABLE tg-rejeitado AS LOGICAL INITIAL no 
     LABEL "Rejeitado" 
     VIEW-AS TOGGLE-BOX
     SIZE 9.72 BY .83 NO-UNDO.

DEFINE VARIABLE tg-vencido AS LOGICAL INITIAL no 
     LABEL "Vencido" 
     VIEW-AS TOGGLE-BOX
     SIZE 9.72 BY .83 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-itens FOR 
      itens_ped_web SCROLLING.

DEFINE QUERY br-pedidos-web FOR 
      tt_peds_web SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-itens
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-itens w-digita _FREEFORM
  QUERY br-itens NO-LOCK DISPLAY
      itens_ped_web.it_codigo         FORMAT "x(8)"             COLUMN-LABEL "Item" 
      fn-desc-item() @ c-desc-item    FORMAT "x(30)"            COLUMN-LABEL "Descri‡Æo"    WIDTH 36
      itens_ped_web.cod_refer         FORMAT "x(5)"             COLUMN-LABEL "Ref"
      itens_ped_web.qt_pedida         FORMAT ">>,>>9.99"        COLUMN-LABEL "Qtde" 
      fn-preco-item() @ de-vl-preori  FORMAT ">>,>>9.99"        COLUMN-LABEL "Pre‡o Un"     WIDTH 10
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 108 BY 8.25
         FONT 1
         TITLE "Itens do Pedido".

DEFINE BROWSE br-pedidos-web
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-pedidos-web w-digita _FREEFORM
  QUERY br-pedidos-web NO-LOCK DISPLAY
      tt_peds_web.ped_web_id                 FORMAT ">,>>>,>>9"     COLUMN-LABEL "Pedido WEB"    COLUMN-FONT 6
      tt_peds_web.nome-rep                   FORMAT "x(12)"         COLUMN-LABEL "Repres"        COLUMN-FONT 6  WIDTH 13
      tt_peds_web.dt_hr_registro                                                                 COLUMN-FONT 6  WIDTH 20
      tt_peds_web.cod_tipo_pedido            FORMAT "x(8)"          COLUMN-LABEL "Tp Ped"        COLUMN-FONT 6
      tt_peds_web.nr_pedido_erp              FORMAT ">,>>>,>>9"     COLUMN-LABEL "Pedido ERP"    COLUMN-FONT 6
      tt_peds_web.nome-abrev                 FORMAT "x(121)"        COLUMN-LABEL "Cliente"       COLUMN-FONT 6  WIDTH  13
      tt_peds_web.desc-situacao              FORMAT "x(15)"         COLUMN-LABEL "Situa‡Æo"      COLUMN-FONT 6  WIDTH  16
      tt_peds_web.nr_container
      tt_peds_web.descr_rejeicao             FORMAT "x(100)"


//tt-ped-venda.desc-cond-pag  WIDTH 60 COLUMN-LABEL "Cond Pagto"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 99 BY 7.83
         FONT 1
         TITLE "Pedidos do Portal".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fi-dt-trans-ini AT ROW 1.5 COL 15 COLON-ALIGNED WIDGET-ID 120
     fi-dt-trans-fin AT ROW 1.5 COL 39.57 COLON-ALIGNED NO-LABEL WIDGET-ID 126
     bt-sel AT ROW 1.5 COL 58 WIDGET-ID 96
     rs-sit-com AT ROW 2.13 COL 96.43 NO-LABEL WIDGET-ID 432
     tg-digita AT ROW 2.17 COL 67 WIDGET-ID 28
     tg-integrado AT ROW 2.17 COL 82.14 WIDGET-ID 36
     fi-no-ab-reppri-ini AT ROW 2.5 COL 15 COLON-ALIGNED HELP
          "Codigo do Ötem no EMS" WIDGET-ID 422
     fi-no-ab-reppri-fin AT ROW 2.5 COL 39.57 COLON-ALIGNED HELP
          "Codigo do Ötem no EMS" NO-LABEL WIDGET-ID 420
     tg-efetivado AT ROW 2.96 COL 67 WIDGET-ID 30
     tg-rejeitado AT ROW 2.96 COL 82 WIDGET-ID 34
     cb-tp-pedido AT ROW 3.5 COL 15 COLON-ALIGNED WIDGET-ID 68
     tg-cancelado AT ROW 3.75 COL 67 WIDGET-ID 32
     tg-vencido AT ROW 3.75 COL 82 WIDGET-ID 38
     br-pedidos-web AT ROW 5.25 COL 2 WIDGET-ID 200
     bt-modifica AT ROW 5.5 COL 103 WIDGET-ID 444
     bt-cancela AT ROW 6.88 COL 103 WIDGET-ID 446
     bt-log AT ROW 8.75 COL 103 WIDGET-ID 24
     bt-status AT ROW 10.5 COL 103 WIDGET-ID 20
     FILL-IN-6 AT ROW 13.25 COL 1.86 NO-LABEL WIDGET-ID 396
     FILL-IN-8 AT ROW 13.25 COL 2 COLON-ALIGNED NO-LABEL WIDGET-ID 404
     FILL-IN-3 AT ROW 13.25 COL 17 NO-LABEL WIDGET-ID 154
     FILL-IN-10 AT ROW 13.25 COL 17.14 COLON-ALIGNED NO-LABEL WIDGET-ID 408
     FILL-IN-2 AT ROW 13.25 COL 32 NO-LABEL WIDGET-ID 152
     FILL-IN-9 AT ROW 13.25 COL 32.14 COLON-ALIGNED NO-LABEL WIDGET-ID 406
     FILL-IN-11 AT ROW 13.25 COL 47.86 NO-LABEL WIDGET-ID 412
     FILL-IN-12 AT ROW 13.25 COL 48 COLON-ALIGNED NO-LABEL WIDGET-ID 410
     FILL-IN-15 AT ROW 13.25 COL 63 NO-LABEL WIDGET-ID 438
     FILL-IN-16 AT ROW 13.25 COL 63.14 COLON-ALIGNED NO-LABEL WIDGET-ID 440
     FILL-IN-13 AT ROW 13.25 COL 78.14 NO-LABEL WIDGET-ID 414
     FILL-IN-14 AT ROW 13.25 COL 78.29 COLON-ALIGNED NO-LABEL WIDGET-ID 416
     br-itens AT ROW 14.5 COL 2 WIDGET-ID 300
     bt-ajuda AT ROW 23.08 COL 99.14
     bt-ok AT ROW 23.13 COL 2.86
     fi-vl-tot-ped AT ROW 23.25 COL 54.57 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     " Situa‡Æo WEB" VIEW-AS TEXT
          SIZE 12.14 BY .5 AT ROW 1.5 COL 66.29 WIDGET-ID 114
     " Sele‡Æo" VIEW-AS TEXT
          SIZE 8 BY .54 AT ROW 1 COL 4 WIDGET-ID 220
     "Vlr Total Pedido WEB:" VIEW-AS TEXT
          SIZE 17.86 BY .54 AT ROW 23.38 COL 38 WIDGET-ID 442
          FONT 6
     "Comercial" VIEW-AS TEXT
          SIZE 7 BY .54 AT ROW 1.5 COL 96.14 WIDGET-ID 40
     RECT-1 AT ROW 22.92 COL 1.72
     rt-buttom-2 AT ROW 5.25 COL 101.86 WIDGET-ID 26
     RECT-9 AT ROW 1.21 COL 1.57 WIDGET-ID 418
     RECT-74 AT ROW 1.75 COL 65 WIDGET-ID 112
     IMAGE-88 AT ROW 1.54 COL 32.72 WIDGET-ID 122
     IMAGE-89 AT ROW 1.54 COL 37.86 WIDGET-ID 124
     IMAGE-96 AT ROW 2.5 COL 33 WIDGET-ID 424
     IMAGE-97 AT ROW 2.5 COL 37.86 WIDGET-ID 426
     RECT-75 AT ROW 1.75 COL 95.43 WIDGET-ID 436
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 109.29 BY 23.58
         FONT 1 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Digitacao
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
   Design Page: 1
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-digita ASSIGN
         HIDDEN             = YES
         TITLE              = "Consulta Pedidos WEB - ESSP0155D"
         HEIGHT             = 23.58
         WIDTH              = 109.29
         MAX-HEIGHT         = 23.58
         MAX-WIDTH          = 138.72
         VIRTUAL-HEIGHT     = 23.58
         VIRTUAL-WIDTH      = 138.72
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-digita 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-digit.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-digita
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
/* BROWSE-TAB br-pedidos-web tg-vencido F-Main */
/* BROWSE-TAB br-itens FILL-IN-14 F-Main */
/* SETTINGS FOR BUTTON bt-cancela IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-dt-trans-fin IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN fi-dt-trans-ini IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN fi-no-ab-reppri-fin IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-no-ab-reppri-ini IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-vl-tot-ped IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-11 IN FRAME F-Main
   ALIGN-L                                                              */
ASSIGN 
       FILL-IN-11:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-13 IN FRAME F-Main
   ALIGN-L                                                              */
ASSIGN 
       FILL-IN-13:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-15 IN FRAME F-Main
   ALIGN-L                                                              */
ASSIGN 
       FILL-IN-15:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-2 IN FRAME F-Main
   ALIGN-L                                                              */
ASSIGN 
       FILL-IN-2:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-3 IN FRAME F-Main
   ALIGN-L                                                              */
ASSIGN 
       FILL-IN-3:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-6 IN FRAME F-Main
   ALIGN-L                                                              */
ASSIGN 
       FILL-IN-6:READ-ONLY IN FRAME F-Main        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-digita)
THEN w-digita:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-itens
/* Query rebuild information for BROWSE br-itens
     _START_FREEFORM
RUN pi-totais.
OPEN QUERY {&SELF-NAME} FOR EACH itens_ped_web WHERE
                                 itens_ped_web.ped_web_id = tt_peds_web.ped_web_id
                                 NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "espec.itens_ped_web.ped_web_id = peds_web.ped_web_id"
     _Query            is OPENED
*/  /* BROWSE br-itens */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-pedidos-web
/* Query rebuild information for BROWSE br-pedidos-web
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt_peds_web WHERE
                                 tt_peds_web.dt_hr_registro >= da-dt-trans-ini AND
                                 tt_peds_web.dt_hr_registro <= da-dt-trans-fin AND
                                 LOOKUP(STRING(tt_peds_web.ind_sit_ped_web),c-sit-web) > 0 AND
                                 (tt_peds_web.completo = rs-sit-com OR rs-sit-com = ?)
                                 NO-LOCK BY tt_peds_web.dt_hr_registro DESCENDING.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "espec.peds_web.ind_sit_ped_web = rs-situacao OR rs-situacao = 7"
     _Query            is OPENED
*/  /* BROWSE br-pedidos-web */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-digita w-digita
ON END-ERROR OF w-digita /* Consulta Pedidos WEB - ESSP0155D */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-digita w-digita
ON WINDOW-CLOSE OF w-digita /* Consulta Pedidos WEB - ESSP0155D */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-pedidos-web
&Scoped-define SELF-NAME br-pedidos-web
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-pedidos-web w-digita
ON ROW-DISPLAY OF br-pedidos-web IN FRAME F-Main /* Pedidos do Portal */
DO:
  CASE tt_peds_web.ind_sit_ped_web.
      WHEN 1 THEN ASSIGN i-cor = 12. /* Vermelho */
      WHEN 2 THEN ASSIGN i-cor = 2.  /* Verde */
      WHEN 3 THEN ASSIGN i-cor = 16. /* Laranja */
      WHEN 4 THEN ASSIGN i-cor = 9.  /* Azul */ 
      WHEN 5 THEN ASSIGN i-cor = 5.  /* Vinho */ 
      WHEN 6 THEN ASSIGN i-cor = 0.  /* Preto */ 
  END CASE.

  ASSIGN tt_peds_web.ped_web_id:FGCOLOR IN BROWSE br-pedidos-web = i-cor
         tt_peds_web.nome-rep:FGCOLOR IN BROWSE br-pedidos-web = i-cor
         tt_peds_web.dt_hr_registro:FGCOLOR IN BROWSE br-pedidos-web = i-cor
         tt_peds_web.cod_tipo_pedido:FGCOLOR IN BROWSE br-pedidos-web = i-cor
         tt_peds_web.nr_pedido_erp:FGCOLOR IN BROWSE br-pedidos-web = i-cor
         tt_peds_web.nome-abrev:FGCOLOR IN BROWSE br-pedidos-web = i-cor
         tt_peds_web.desc-situacao:FGCOLOR IN BROWSE br-pedidos-web = i-cor
         tt_peds_web.nr_container:FGCOLOR IN BROWSE br-pedidos-web = i-cor.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-pedidos-web w-digita
ON VALUE-CHANGED OF br-pedidos-web IN FRAME F-Main /* Pedidos do Portal */
DO:
   ASSIGN bt-modifica:SENSITIVE = NO
          bt-log:SENSITIVE = NO
          bt-cancela:SENSITIVE = NO
          bt-status:SENSITIVE = NO.

   bt-status:LOAD-IMAGE-INSENSITIVE("image/imt-smate.bmp").
   IF AVAIL tt_peds_web THEN DO.
      IF tt_peds_web.ind_sit_ped_web = 5 THEN DO.
         FIND usuar_grp_usuar WHERE 
              usuar_grp_usuar.cod_usuar     = c-seg-usuario AND
              usuar_grp_usuar.cod_grp_usuar = "SUP" 
              NO-LOCK NO-ERROR.
          IF AVAIL usuar_grp_usuar THEN
             ASSIGN bt-cancela:SENSITIVE = YES.
      END.

      IF tt_peds_web.nr_pedido_erp <> 0 THEN DO.
         ASSIGN bt-log:SENSITIVE = YES
                bt-status:SENSITIVE = YES.

         IF NOT tt_peds_web.completo OR 
            rs-sit-com = NO THEN
            ASSIGN bt-modifica:SENSITIVE = YES.
         bt-status:LOAD-IMAGE-INSENSITIVE("image/imt-smfec.bmp").
         IF tt_peds_web.completo THEN DO.
            bt-status:LOAD-IMAGE-INSENSITIVE("image/imt-smabe.bmp").
            ASSIGN bt-status:SENSITIVE = NO.
         END.
      END.
   END.

   {&OPEN-QUERY-br-itens}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda w-digita
ON CHOOSE OF bt-ajuda IN FRAME F-Main /* Ajuda */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cancela
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancela w-digita
ON CHOOSE OF bt-cancela IN FRAME F-Main
DO:
  MESSAGE 'Confirma Cancelamento do Pedido ? '
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE l-conf AS LOGICAL.

  IF NOT l-conf THEN RETURN NO-APPLY.

  FIND peds_web WHERE
       peds_web.ped_web_id = tt_peds_web.ped_web_id SHARE-LOCK NO-ERROR.

  ASSIGN peds_web.ind_sit_ped_web = 3. // Cancelado

  RUN pi-popula-ped.

  {&OPEN-QUERY-br-pedidos-web}
  APPLY 'VALUE-CHANGED' TO br-pedidos-web IN FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-log
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-log w-digita
ON CHOOSE OF bt-log IN FRAME F-Main
DO:
   FIND ped-venda WHERE
        ped-venda.nr-pedido = tt_peds_web.nr_pedido_erp NO-LOCK NO-ERROR.
   ASSIGN gr-ped-venda = ROWID(ped-venda).

   
   RUN esp/essp0155b.p (INPUT ped-venda.nr-pedcli).
   

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-modifica
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-modifica w-digita
ON CHOOSE OF bt-modifica IN FRAME F-Main /* Sair */
DO:
   FIND ped-venda WHERE
        ped-venda.nr-pedido = tt_peds_web.nr_pedido_erp NO-LOCK NO-ERROR.
   ASSIGN gr-ped-venda = ROWID(ped-venda).

   ASSIGN w-digita:SENSITIVE = NO.
   RUN esp\espd4000.w (INPUT "Modificar").
   ASSIGN w-digita:SENSITIVE = YES.
   

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-digita
ON CHOOSE OF bt-ok IN FRAME F-Main /* Sair */
DO:
  apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-sel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-sel w-digita
ON CHOOSE OF bt-sel IN FRAME F-Main /* OK */
DO:
   ASSIGN INPUT FRAME {&FRAME-NAME} fi-dt-trans-ini
                                    fi-dt-trans-fin
                                    fi-no-ab-reppri-ini
                                    fi-no-ab-reppri-fin
                                    cb-tp-pedido
                                    tg-digita
                                    tg-efetivado
                                    tg-cancelado
                                    tg-integrado
                                    tg-vencido
                                    tg-rejeitado
                                    rs-sit-com.

   ASSIGN da-dt-trans-ini = DATETIME(fi-dt-trans-ini)
          da-dt-trans-fin = DATETIME(fi-dt-trans-fin + 1).

   SESSION:SET-WAIT-STATE("general":U).

   EMPTY TEMP-TABLE tt_peds_web.

   RUN pi-monta-sit.
   RUN pi-popula-ped.

   SESSION:SET-WAIT-STATE("":U).

   {&OPEN-QUERY-br-pedidos-web}
   APPLY 'VALUE-CHANGED' TO br-pedidos-web IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-status
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-status w-digita
ON CHOOSE OF bt-status IN FRAME F-Main
DO:
   SESSION:SET-WAIT-STATE("general":U).

   FIND ped-venda WHERE
        ped-venda.nr-pedido = tt_peds_web.nr_pedido_erp SHARE-LOCK NO-ERROR.

   ASSIGN ped-venda.completo = NO.
   IF ped-venda.tp-pedido = "PE" THEN 
      RUN esapi/completa-pedvenda.p (INPUT ped-venda.nr-pedcli).
   
   ASSIGN ped-venda.log-2 = YES.

   ASSIGN tt_peds_web.completo = IF ped-venda.tp-pedido = 'PE' 
                                 THEN ped-venda.completo ELSE ped-venda.log-2.

   SESSION:SET-WAIT-STATE("":U).

   {&OPEN-QUERY-br-pedidos-web}
   APPLY 'VALUE-CHANGED' TO br-pedidos-web IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-tp-pedido
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-tp-pedido w-digita
ON VALUE-CHANGED OF cb-tp-pedido IN FRAME F-Main /* Tipo de Pedido */
DO:
   APPLY 'CHOOSE' TO bt-sel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-no-ab-reppri-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-no-ab-reppri-ini w-digita
ON LEAVE OF fi-no-ab-reppri-ini IN FRAME F-Main /* Representante */
DO:
  IF SELF:SCREEN-VALUE <> '' THEN
     ASSIGN fi-no-ab-reppri-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-10 w-digita
ON ENTRY OF FILL-IN-10 IN FRAME F-Main
DO:
   APPLY 'ENTRY' TO bt-ok.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-11
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-11 w-digita
ON LEAVE OF FILL-IN-11 IN FRAME F-Main
DO:
    APPLY 'ENTRY' TO bt-ok.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-12
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-12 w-digita
ON ENTRY OF FILL-IN-12 IN FRAME F-Main
DO:
   APPLY 'ENTRY' TO bt-ok.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-13
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-13 w-digita
ON LEAVE OF FILL-IN-13 IN FRAME F-Main
DO:
    APPLY 'ENTRY' TO bt-ok.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-14
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-14 w-digita
ON ENTRY OF FILL-IN-14 IN FRAME F-Main
DO:
   APPLY 'ENTRY' TO bt-ok.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-15
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-15 w-digita
ON LEAVE OF FILL-IN-15 IN FRAME F-Main
DO:
    APPLY 'ENTRY' TO bt-ok.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-16
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-16 w-digita
ON ENTRY OF FILL-IN-16 IN FRAME F-Main
DO:
   APPLY 'ENTRY' TO bt-ok.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-2 w-digita
ON ENTRY OF FILL-IN-2 IN FRAME F-Main
DO:
    APPLY 'ENTRY' TO bt-ok.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-3 w-digita
ON LEAVE OF FILL-IN-3 IN FRAME F-Main
DO:
    APPLY 'ENTRY' TO bt-ok.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-6 w-digita
ON ENTRY OF FILL-IN-6 IN FRAME F-Main
DO:
    APPLY 'ENTRY' TO bt-ok.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-8 w-digita
ON ENTRY OF FILL-IN-8 IN FRAME F-Main
DO:
    APPLY 'ENTRY' TO bt-ok.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-9 w-digita
ON ENTRY OF FILL-IN-9 IN FRAME F-Main
DO:
    APPLY 'ENTRY' TO bt-ok.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-sit-com
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-sit-com w-digita
ON VALUE-CHANGED OF rs-sit-com IN FRAME F-Main
DO:
    ASSIGN INPUT FRAME {&FRAME-NAME} rs-sit-com.

    {&OPEN-QUERY-br-pedidos-web}
    APPLY 'VALUE-CHANGED' TO br-pedidos-web IN FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-cancelado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-cancelado w-digita
ON VALUE-CHANGED OF tg-cancelado IN FRAME F-Main /* Cancelado */
DO:
  RUN pi-monta-sit.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-digita w-digita
ON VALUE-CHANGED OF tg-digita IN FRAME F-Main /* em Digita‡Æo */
DO:
  RUN pi-monta-sit.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-efetivado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-efetivado w-digita
ON VALUE-CHANGED OF tg-efetivado IN FRAME F-Main /* Efetivado */
DO:
  RUN pi-monta-sit.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-integrado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-integrado w-digita
ON VALUE-CHANGED OF tg-integrado IN FRAME F-Main /* Integrado */
DO:
   RUN pi-monta-sit.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-rejeitado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-rejeitado w-digita
ON VALUE-CHANGED OF tg-rejeitado IN FRAME F-Main /* Rejeitado */
DO:
  RUN pi-monta-sit.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-vencido
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-vencido w-digita
ON VALUE-CHANGED OF tg-vencido IN FRAME F-Main /* Vencido */
DO:
  RUN pi-monta-sit.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-itens
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-digita 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-digita  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  /* Select a Startup page. */
  IF adm-current-page eq 0 
  THEN RUN select-page IN THIS-PROCEDURE ( 1 ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-digita  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-digita  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-digita)
  THEN DELETE WIDGET w-digita.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-digita  _DEFAULT-ENABLE
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
  DISPLAY fi-dt-trans-ini fi-dt-trans-fin rs-sit-com tg-digita tg-integrado 
          fi-no-ab-reppri-ini fi-no-ab-reppri-fin tg-efetivado tg-rejeitado 
          cb-tp-pedido tg-cancelado tg-vencido FILL-IN-6 FILL-IN-8 FILL-IN-3 
          FILL-IN-10 FILL-IN-2 FILL-IN-9 FILL-IN-11 FILL-IN-12 FILL-IN-15 
          FILL-IN-16 FILL-IN-13 FILL-IN-14 fi-vl-tot-ped 
      WITH FRAME F-Main IN WINDOW w-digita.
  ENABLE RECT-1 rt-buttom-2 RECT-9 RECT-74 IMAGE-88 IMAGE-89 IMAGE-96 IMAGE-97 
         RECT-75 fi-dt-trans-ini fi-dt-trans-fin bt-sel rs-sit-com tg-digita 
         tg-integrado tg-efetivado tg-rejeitado cb-tp-pedido tg-cancelado 
         tg-vencido br-pedidos-web bt-modifica bt-log bt-status FILL-IN-6 
         FILL-IN-8 FILL-IN-3 FILL-IN-10 FILL-IN-2 FILL-IN-9 FILL-IN-11 
         FILL-IN-12 FILL-IN-15 FILL-IN-16 FILL-IN-13 FILL-IN-14 br-itens 
         bt-ajuda bt-ok 
      WITH FRAME F-Main IN WINDOW w-digita.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW w-digita.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-digita 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-digita 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-digita 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

//  {utp/ut9000.i "XX9999" "9.99.99.999"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  FIND usuar_mestre WHERE
       usuar_mestre.cod_usuario = c-seg-usuario NO-LOCK NO-ERROR.

  ASSIGN i-cod-reppri-ini = 0
         i-cod-reppri-fin = 999999
         da-dt-trans-ini = DATETIME(TODAY - 30)
         da-dt-trans-fin = DATETIME(TODAY).

  ASSIGN fi-no-ab-reppri-ini:SENSITIVE IN FRAME {&FRAME-NAME} = YES
         fi-no-ab-reppri-fin:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

  ASSIGN fi-dt-trans-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY - 30)
         fi-dt-trans-fin:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY)
         fi-no-ab-reppri-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ''
         fi-no-ab-reppri-fin:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'ZZZZZZZZZZZZZZZZ'.

  FIND FIRST repres WHERE
             SUBSTRING(repres.char-1,500,12) = c-seg-usuario AND 
             repres.ind-sit = 1 NO-LOCK NO-ERROR.
    
  IF NOT AVAIL repres THEN
     FIND repres WHERE
          repres.nome = usuar_mestre.nom_usuar NO-LOCK NO-ERROR.
    
  IF AVAIL repres THEN  DO.
     FIND cm-ext-repres WHERE
          cm-ext-repres.cod-rep = repres.cod-rep NO-ERROR.
    
     IF NOT AVAIL cm-ext-repres OR
        (AVAIL cm-ext-repres AND cm-ext-repres.classe > 2) THEN DO.
        ASSIGN i-cod-reppri-ini = repres.cod-rep
               i-cod-reppri-fin = repres.cod-rep.

        ASSIGN fi-no-ab-reppri-ini:SENSITIVE = NO
               fi-no-ab-reppri-fin:SENSITIVE = NO.

        ASSIGN fi-no-ab-reppri-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = repres.nome-abrev
               fi-no-ab-reppri-fin:SCREEN-VALUE IN FRAME {&FRAME-NAME} = repres.nome-abrev.
     END.
  END.

  APPLY 'CHOOSE' TO bt-sel IN FRAME {&FRAME-NAME}.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-monta-sit w-digita 
PROCEDURE pi-monta-sit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ASSIGN c-sit-web = ''.
   IF INPUT FRAME {&FRAME-NAME} tg-digita     THEN ASSIGN c-sit-web = '1'.
   IF INPUT FRAME {&FRAME-NAME} tg-efetivado  THEN ASSIGN c-sit-web = c-sit-web + ',2'.
   IF INPUT FRAME {&FRAME-NAME} tg-cancelado  THEN ASSIGN c-sit-web = c-sit-web + ',3'.
   IF INPUT FRAME {&FRAME-NAME} tg-integrado  THEN ASSIGN c-sit-web = c-sit-web + ',4'.
   IF INPUT FRAME {&FRAME-NAME} tg-rejeitado  THEN ASSIGN c-sit-web = c-sit-web + ',5'.
   IF INPUT FRAME {&FRAME-NAME} tg-vencido    THEN ASSIGN c-sit-web = c-sit-web + ',6'.

   {&OPEN-QUERY-br-pedidos-web}
   APPLY 'VALUE-CHANGED' TO br-pedidos-web IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-popula-ped w-digita 
PROCEDURE pi-popula-ped :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE tt_peds_web.
    FOR EACH peds_web WHERE
             peds_web.repres_id >= i-cod-reppri-ini AND
             peds_web.repres_id <= i-cod-reppri-fin NO-LOCK.

        IF cb-tp-pedido <> 'Todos' AND
           peds_web.cod_tipo_pedido <> cb-tp-pedido THEN NEXT.

        CREATE tt_peds_web.
        BUFFER-COPY peds_web TO tt_peds_web
             ASSIGN tt_peds_web.cod_tipo_pedido =  STRING(peds_web.cod_tipo_pedido).

        IF tt_peds_web.log_novo_cliente THEN
           FIND emitente WHERE
                emitente.cgc = tt_peds_web.cnpj_novo_cliente NO-LOCK NO-ERROR.
        ELSE
           FIND emitente WHERE  
                emitente.cod-emit = tt_peds_web.cliente_id NO-LOCK NO-ERROR.

        IF AVAIL emitente THEN DO.
           ASSIGN c-nome-abrev = emitente.nome-abrev.

           IF emitente.cod-emit = 0 THEN DO.
              IF tt_peds_web.log_novo_cliente THEN
                 ASSIGN c-nome-abrev = 'CLIENTE NOVO'.
              ELSE
                 ASSIGN c-nome-abrev = 'NÇO INFORMADO'.
           END.
        END.
        ELSE IF tt_peds_web.log_novo_cliente THEN
           ASSIGN c-nome-abrev = 'CLIENTE NOVO'.
        ELSE
           ASSIGN c-nome-abrev = 'NÇO INFORMADO'.

        ASSIGN tt_peds_web.nome-abrev = c-nome-abrev.   


        CASE tt_peds_web.ind_sit_ped_web.
            WHEN 1 THEN ASSIGN c-situacao = 'Em Digita‡Æo'.
            WHEN 2 THEN ASSIGN c-situacao = 'Efetivado'.
            WHEN 3 THEN ASSIGN c-situacao = 'Cancelado'.
            WHEN 4 THEN ASSIGN c-situacao = 'Integrado'.
            WHEN 5 THEN ASSIGN c-situacao = 'Rejeitado'.
            WHEN 6 THEN ASSIGN c-situacao = 'Vencido'.
        END CASE.
        ASSIGN tt_peds_web.desc-situacao = c-situacao.  


        FIND repres WHERE 
             repres.cod-rep = tt_peds_web.repres_id NO-LOCK NO-ERROR.
        IF AVAIL repres THEN
           ASSIGN tt_peds_web.nome-rep = repres.nome-abrev.

        IF tt_peds_web.ind_sit_ped_web = 4 THEN DO.
           FIND ped-venda WHERE
                ped-venda.nr-pedido = peds_web.nr_pedido_erp NO-LOCK NO-ERROR.
           IF AVAIL ped-venda THEN
              ASSIGN tt_peds_web.completo = IF ped-venda.tp-pedido = 'PE' 
                                            THEN ped-venda.completo ELSE ped-venda.log-2.

           IF tt_peds_web.completo THEN
              ASSIGN tt_peds_web.desc-situacao = TRIM(tt_peds_web.desc-situacao) + "-CONF".  
           ELSE
              ASSIGN tt_peds_web.desc-situacao = TRIM(tt_peds_web.desc-situacao) + "-PEND".  
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-totais w-digita 
PROCEDURE pi-totais :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN fi-vl-tot-ped = 0.
    FOR EACH itens_ped_web WHERE
             itens_ped_web.ped_web_id = tt_peds_web.ped_web_id 
             NO-LOCK.
        ASSIGN fi-vl-tot-ped = fi-vl-tot-ped + (itens_ped_web.qt_pedida * fn-preco-item() ).
    END.
    DISP fi-vl-tot-ped
         WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-digita  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt_peds_web"}
  {src/adm/template/snd-list.i "itens_ped_web"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-digita 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-desc-item w-digita 
FUNCTION fn-desc-item RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
    ASSIGN c-desc-item = ''.
    FIND item WHERE
         item.it-codigo = itens_ped_web.it_codigo NO-LOCK NO-ERROR.
    IF AVAIL itens_ped_web THEN
       ASSIGN c-desc-item = item.desc-item.

    RETURN c-desc-item. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-preco-item w-digita 
FUNCTION fn-preco-item RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    ASSIGN de-vl-preori = 0.
    IF itens_ped_web.vl_informado <> 0 THEN
       ASSIGN de-vl-preori = itens_ped_web.vl_informado.
    ELSE IF itens_ped_web.vl_unit_tabela <> 0 THEN
       ASSIGN de-vl-preori = itens_ped_web.vl_unit_tabela.
    ELSE IF itens_ped_web.vl_unit_final <> 0 THEN
       ASSIGN de-vl-preori = itens_ped_web.vl_unit_final.

    RETURN de-vl-preori.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

