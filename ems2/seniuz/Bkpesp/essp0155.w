&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESSP0155 2.04.00.999}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
/* INICIO DA DEFINIÄ«O DAS TABELAS TEMPORARIAS; PARA A CHAMADA DO RELATORIO ESPD0002RP.P */

{esinc/espd0002.i}

DEF BUFFER moeda FOR ems2cad.moeda.
DEF BUFFER empresa FOR ems2cad.empresa.    

DEF TEMP-TABLE tt-raw-digita
    FIELD raw-digita      AS RAW.
/*
DEFINE TEMP-TABLE tt-digita NO-UNDO 
       FIELD nr-sequencia     AS   INT FORMAT '>>9'
       FIELD it-codigo        LIKE ped-item.it-codigo
       FIELD cod-refer        LIKE ped-item.cod-refer
       FIELD desc-item        AS   CHAR FORMAT "x(25)"
       FIELD qt-pedida        AS   DEC FORMAT ">>>,>>9.99" 
       FIELD qt-reserva       AS   DEC FORMAT ">>>,>>9.99" 
       FIELD sit-prog         AS   CHAR FORMAT "x(7)"
       INDEX seqped nr-sequencia.
*/
DEF TEMP-TABLE tt-digita
    FIELD opcao  AS CHAR
    FIELD campo AS CHAR
    FIELD valor AS CHAR.


/* FIM DAS DEFINIÄÂES DO RELATORIO ESPD0002RP.P */


/* Parameters Definitions ---                                           */

/* Global Variable Definitions ---                                       */
DEF NEW GLOBAL SHARED VAR gr-ped-venda  AS ROWID NO-UNDO.
DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

/* Temp Table Definitions ---                                           */
DEF TEMP-TABLE tt-ped-venda LIKE ped-venda
    FIELD qt-pedida   LIKE ped-item.qt-pedida
    FIELD qt-faturada LIKE ped-item.qt-pedida
    FIELD qt-cancelada LIKE ped-item.qt-pedida
    FIELD qt-reservada LIKE ped-item.qt-pedida
    FIELD qt-aberto    AS DEC FORMAT "->>>,>>9.99" 
    FIELD nr-embarque  LIKE pre-fatur.nr-embarque
    FIELD ped_web_id   LIKE ped-venda-ext.ped_web_id
    FIELD nr-container LIKE ped-venda-ext.nr-container
    FIELD situacao     AS CHAR FORMAT "x(4)"
    INDEX indice1 IS PRIMARY dt-implant nr-pedcli.

/* Local Variable Definitions ---                                       */
DEF VAR h-acomp           AS HANDLE NO-UNDO.

DEF VAR h-query           AS HANDLE.
DEF VAR v-row-table       AS ROWID.
DEF VAR c-busca           AS CHAR.
DEF VAR c-empresa         AS CHAR.

DEF VAR c-pedidos         AS CHAR.
DEF VAR c-desc-cond-pag   LIKE cond-pagto.descricao.
DEF VAR i-ct              AS INT.
DEF VAR de-qt-pedida      LIKE ped-item.qt-pedida.
DEF VAR de-qt-faturada    LIKE ped-item.qt-pedida.
DEF VAR de-qt-reservada   LIKE ped-item.qt-pedida.
DEF VAR de-qt-cancelada   LIKE ped-item.qt-pedida.
DEF VAR de-qt-aberto      LIKE ped-item.qt-pedida.
DEF VAR de-tot-desc       LIKE ped-item.val-desconto-total.

DEF VAR c-situacao        AS CHAR.
DEF VAR c-dia             AS CHAR.
DEF VAR da-dt-implant-ini AS DATE.
DEF VAR da-dt-implant-fin AS DATE.
DEF VAR da-dt-cancela-ini AS DATE.
DEF VAR da-dt-cancela-fin AS DATE.
DEF VAR c-lotes           AS CHAR FORMAT "x(18)".

DEF VAR i-lin            AS INT.
DEF VAR i-pag            AS INT.
DEF VAR de-tot-pedida    AS DEC.
DEF VAR de-tot-reservada AS DEC.
DEF VAR de-tot-saldo     AS DEC.
DEF VAR i-sit            AS INT.
DEF VAR arq-saida        AS CHAR FORMAT "x(54)".

DEF VAR c-motivo AS CHAR FORMAT "x(60)".
DEF VAR da-dt-trans AS DATE.
DEF VAR c-finalidade AS CHAR.
DEF VAR c-tipo-trans AS CHAR.


/* Variaveis para o Excel */
DEFINE VAR i-canal      AS INT.
DEFINE VAR sys          AS INT.
DEFINE VAR sis          AS INT.
DEFINE VAR c-lin        AS CHAR FORMAT "x(500)".
DEFINE VAR aux-command  AS CHAR FORMAT "x(100)".


/* Variaveis da Rotina de Impress∆o */
DEFINE VAR i-saida             AS INT.
DEFINE VAR c-saida             AS CHAR.
DEFINE VAR i-num-copias        AS INT.


/* Variavies de ParÉmetros */
DEFINE VAR c-cod-estabel       AS CHAR.
DEFINE VAR c-nr-pedcli-ini     LIKE ped-item-ext.nr-pedcli INIT "".
DEFINE VAR c-nr-pedcli-fin     LIKE ped-item-ext.nr-pedcli INIT "ZZZZZZZZ".  
DEFINE VAR c-it-codigo-ini     LIKE ped-item-ext.it-codigo INIT "".                 
DEFINE VAR c-it-codigo-fin     LIKE ped-item-ext.it-codigo INIT "ZZZZZZZZZZZZZZZZ".  
DEFINE VAR c-cod-refer-ini     LIKE ped-item-ext.cod-refer.
DEFINE VAR c-cod-refer-fin     LIKE ped-item-ext.cod-refer INIT "ZZZZZZZZZZ".   
DEFINE VAR c-nome-abrev-ini    LIKE ped-venda.nome-abrev. 
DEFINE VAR c-nome-abrev-fin    LIKE ped-venda.nome-abrev INIT "ZZZZZZZZZZZZ".
DEFINE VAR c-no-ab-reppri-ini  LIKE ped-venda.no-ab-reppri. 
DEFINE VAR c-no-ab-reppri-fin  LIKE ped-venda.no-ab-reppri INIT 'ZZZZZZZZZZZZ'.
DEFINE VAR c-uf-ini            LIKE ped-venda.estado. 
DEFINE VAR c-uf-fin            LIKE ped-venda.estado INIT 'ZZZ'.
DEFINE VAR c-nome-tra-ini      LIKE transporte.nome-abrev. 
DEFINE VAR c-nome-tra-fin      LIKE transporte.nome-abrev INIT "ZZZZZZZZZZZZ".
DEFINE VAR c-cidade-ini        LIKE ped-venda.cidade. 
DEFINE VAR c-cidade-fin        LIKE ped-venda.cidade INIT "ZZZZZZZZZZZZZZZZZZZZZZZZZZ".
DEFINE VAR i-nr-container-ini  LIKE pp-ped-venda.nr-container. 
DEFINE VAR i-nr-container-fin  LIKE pp-ped-venda.nr-container INIT "999999".
DEFINE VAR c-corte-comerc-ini  LIKE ob-etiqueta.corte-comerc INIT "A".   
DEFINE VAR c-corte-comerc-fin  LIKE ob-etiqueta.corte-comerc INIT "Z".   
DEFINE VAR c-cod-restr-ini     AS CHAR.
DEFINE VAR c-cod-restr-fin     AS CHAR INIT 'ZZZZZZZZZZ'.
DEFINE VAR c-tp-pedido         AS CHAR INIT "Todos,Todos".
DEFINE VAR l-corte             AS LOG INIT NO.
DEFINE VAR i-situacao          AS INT INIT 3.
DEFINE VAR i-preco             AS INT INIT 4.
DEFINE VAR i-aprovar           AS INT INIT 3.
DEFINE VAR i-espera            AS INT INIT 3.
DEFINE VAR c-cod-depos         AS CHAR INIT "ARM".
DEFINE VAR l-lote-todos        AS LOG INIT YES.
DEFINE VAR l-lote-pp           AS LOG INIT NO.
DEFINE VAR l-lote-pd           AS LOG INIT NO.
DEFINE VAR l-lote-rp           AS LOG INIT NO.
DEFINE VAR l-lote-rd           AS LOG INIT NO.
DEFINE VAR l-lote-sc           AS LOG INIT NO.
DEFINE VAR l-lote-ca           AS LOG INIT NO.
DEFINE VAR c-tp-mercado        AS CHAR INIT 'T'.
DEFINE VAR l-sit-todas         AS LOG INIT NO.
DEFINE VAR l-sit-abe           AS LOG INIT YES.
DEFINE VAR l-sit-atp           AS LOG INIT YES.
DEFINE VAR l-sit-att           AS LOG INIT NO.
DEFINE VAR l-sit-pen           AS LOG INIT YES.
DEFINE VAR l-sit-sus           AS LOG INIT NO.
DEFINE VAR l-sit-can           AS LOG INIT NO.
DEFINE VAR i-credito           AS INT INIT 3.
DEFINE VAR l-ok                AS LOG.

DEFINE VAR c-nr-pedcli         LIKE ped-venda.nr-pedcli.
/* DEFINIÄ«O DE VARIAVEIS PARA A CHAMADA DO RELATORIO ESPD0002RP.P */

/* Variaveis para a include i-rprun.i */
DEF VAR h-prog AS HANDLE.
DEFINE VAR raw-param   AS RAW NO-UNDO.
DEFINE VAR rs-execucao AS INTEGER INITIAL 1. 
DEFINE FRAME f-pg-imp rs-execucao AT ROW 5.75 COL 3 NO-LABEL.
DEFINE VAR c-programa-mg97 AS CHAR INIT "essp0155". 
DEFINE VAR c-versao-mg97 AS CHAR.

/* FIM DAS DEFINIÄÂES DE VARIAVEIS PARA CHAMADA DO RELATORIO ESPD0002RP.P */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME br-pedidos

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-ped-venda

/* Definitions for BROWSE br-pedidos                                    */
&Scoped-define FIELDS-IN-QUERY-br-pedidos tt-ped-venda.nr-pedcli tt-ped-venda.nome-abrev tt-ped-venda.nat-oper tt-ped-venda.nome-transp tt-ped-venda.dt-implant tt-ped-venda.situacao tt-ped-venda.qt-pedida tt-ped-venda.qt-faturada tt-ped-venda.qt-reservada tt-ped-venda.qt-cancelada tt-ped-venda.qt-aberto tt-ped-venda.vl-desconto tt-ped-venda.dt-cancela tt-ped-venda.dt-apr-cred tt-ped-venda.dt-entrega tt-ped-venda.nr-embarque tt-ped-venda.nr-container tt-ped-venda.nr-pedrep tt-ped-venda.ped_web_id   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-pedidos   
&Scoped-define SELF-NAME br-pedidos
&Scoped-define QUERY-STRING-br-pedidos FOR EACH tt-ped-venda NO-LOCK
&Scoped-define OPEN-QUERY-br-pedidos OPEN QUERY {&SELF-NAME} FOR EACH tt-ped-venda NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-pedidos tt-ped-venda
&Scoped-define FIRST-TABLE-IN-QUERY-br-pedidos tt-ped-venda


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-br-pedidos}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-4 rt-buttom br-pedidos bt-param ~
bt-inclui bt-modifica bt-cancela bt-suspende bt-status bt-vapara ~
bt-consulta bt-log bt-imprime bt-excel bt-imprime-txt bt-exit FILL-IN-1 ~
FILL-IN-2 FILL-IN-3 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-1 FILL-IN-2 fi-tot-pedida ~
fi-tot-fatur fi-tot-cancel fi-tot-aberto FILL-IN-3 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-6 bt-excel 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-cancela AUTO-GO 
     IMAGE-UP FILE "image/im-cance.bmp":U
     LABEL "" 
     SIZE 5.86 BY 1.29 TOOLTIP "Cancelar Pedido"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-consulta AUTO-GO 
     IMAGE-UP FILE "image/im-det.bmp":U
     LABEL "" 
     SIZE 5.86 BY 1.29 TOOLTIP "Detalhar Pedido"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-excel 
     IMAGE-UP FILE "image/excel.bmp":U
     LABEL "Button 2" 
     SIZE 5.86 BY 1.29.

DEFINE BUTTON bt-exit AUTO-END-KEY 
     IMAGE-UP FILE "image/im-exi.bmp":U
     LABEL "" 
     SIZE 5.86 BY 2.04
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-imprime 
     IMAGE-UP FILE "image/im-pri.bmp":U
     LABEL "" 
     SIZE 5.86 BY 1.29 TOOLTIP "Imprimir a Programaá∆o da Produá∆o".

DEFINE BUTTON bt-imprime-txt AUTO-GO 
     IMAGE-UP FILE "image/im-f-ibt.bmp":U
     LABEL "" 
     SIZE 6 BY 1.79 TOOLTIP "Pedidos WEB"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-inclui AUTO-GO 
     IMAGE-UP FILE "image/im-add.bmp":U
     LABEL "" 
     SIZE 5.86 BY 1.21 TOOLTIP "Incluir Pedido"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-log AUTO-GO 
     IMAGE-UP FILE "image/im-log.bmp":U
     LABEL "" 
     SIZE 5.86 BY 1.29 TOOLTIP "Alteraá‰es do Pedido"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-modifica AUTO-END-KEY 
     IMAGE-UP FILE "image/im-mod.bmp":U
     LABEL "&Sair" 
     SIZE 5.86 BY 1.29 TOOLTIP "Modificar Pedido"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-param AUTO-GO 
     IMAGE-UP FILE "image/im-fil.bmp":U
     LABEL "&Sair" 
     SIZE 5.86 BY 1.21 TOOLTIP "ParÉmetros"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-status AUTO-GO 
     IMAGE-UP FILE "image/imt-smfec.bmp":U
     IMAGE-INSENSITIVE FILE "image/imt-smfec.bmp":U
     LABEL "" 
     SIZE 5.86 BY 2.25 TOOLTIP "Status para Faturamento / Completa Pedido"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-suspende AUTO-GO 
     IMAGE-UP FILE "image/im-grava.bmp":U
     LABEL "" 
     SIZE 5.86 BY 1.29 TOOLTIP "Suspender/Reativar Pedido"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-vapara AUTO-GO 
     IMAGE-UP FILE "image/im-vapra.bmp":U
     LABEL "" 
     SIZE 5.86 BY 1.29 TOOLTIP "Posicionar no Pedido"
     BGCOLOR 8 FONT 10.

DEFINE VARIABLE fi-tot-aberto AS DECIMAL FORMAT "-ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.57 BY 1 NO-UNDO.

DEFINE VARIABLE fi-tot-cancel AS DECIMAL FORMAT "ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.29 BY 1 NO-UNDO.

DEFINE VARIABLE fi-tot-fatur AS DECIMAL FORMAT "-ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fi-tot-pedida AS DECIMAL FORMAT "-ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 2 BY .5
     BGCOLOR 12  NO-UNDO.

DEFINE VARIABLE FILL-IN-2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 2 BY .5
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE FILL-IN-3 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 2 BY .5
     BGCOLOR 2  NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 103 BY 2.25
     BGCOLOR 8 .

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 8 BY 20.25
     BGCOLOR 8 FGCOLOR 0 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-pedidos FOR 
      tt-ped-venda SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-pedidos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-pedidos C-Win _FREEFORM
  QUERY br-pedidos NO-LOCK DISPLAY
      tt-ped-venda.nr-pedcli                                FORMAT "x(12)":U        WIDTH 8    
      tt-ped-venda.nome-abrev                               FORMAT "x(12)":U        WIDTH 14
      tt-ped-venda.nat-oper     COLUMN-LABEL "NatOper"                      
      tt-ped-venda.nome-transp  COLUMN-LABEL "Transportadora" FORMAT "x(12)":U      WIDTH 14
      tt-ped-venda.dt-implant   COLUMN-LABEL "Implantaá∆o"                          
      tt-ped-venda.situacao     COLUMN-LABEL "Sit"          FORMAT "x(4)":U         WIDTH 5
      tt-ped-venda.qt-pedida    COLUMN-LABEL "Tot Pedido"                           WIDTH 12
      tt-ped-venda.qt-faturada  COLUMN-LABEL "Tot Fatur"                            WIDTH 12
      tt-ped-venda.qt-reservada COLUMN-LABEL "Tot Reservado"                        WIDTH 12
      tt-ped-venda.qt-cancelada COLUMN-LABEL "Tot Cancel"                           WIDTH 12
      tt-ped-venda.qt-aberto    COLUMN-LABEL "Tot Aberto"                           WIDTH 12
      tt-ped-venda.vl-desconto  COLUMN-LABEL "Vlr Desconto"  FORMAT ">>>,>>9.99"    WIDTH 12
      tt-ped-venda.dt-cancela   COLUMN-LABEL "DtCancela"
      tt-ped-venda.dt-apr-cred  COLUMN-LABEL "Aprovaá∆o"
      tt-ped-venda.dt-entrega                                FORMAT "99/99/9999":U
      tt-ped-venda.nr-embarque  COLUMN-LABEL "Embarque"
      tt-ped-venda.nr-container COLUMN-LABEL "Container"
      tt-ped-venda.nr-pedrep                                FORMAT "x(12)":U        WIDTH 10
      tt-ped-venda.ped_web_id   COLUMN-LABEL "Pedido WEB"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 103 BY 17.75
         FONT 6
         TITLE "Carteira de Pedidos Selecionada" ROW-HEIGHT-CHARS .67.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     br-pedidos AT ROW 1.25 COL 2
     bt-param AT ROW 1.54 COL 107.29
     bt-inclui AT ROW 3 COL 107.14
     bt-modifica AT ROW 4.21 COL 107.14
     bt-cancela AT ROW 5.54 COL 107.14
     bt-suspende AT ROW 6.88 COL 107.14
     bt-status AT ROW 8.21 COL 107.14
     bt-vapara AT ROW 10.5 COL 107.14
     bt-consulta AT ROW 11.79 COL 107.14
     bt-log AT ROW 13.13 COL 107.14
     bt-imprime AT ROW 14.46 COL 107.14
     bt-excel AT ROW 15.71 COL 107.14
     bt-imprime-txt AT ROW 17.25 COL 107
     bt-exit AT ROW 19.25 COL 107.14
     FILL-IN-1 AT ROW 19.42 COL 1 COLON-ALIGNED NO-LABEL
     FILL-IN-2 AT ROW 20.08 COL 2.86 NO-LABEL
     fi-tot-pedida AT ROW 20.17 COL 44 COLON-ALIGNED NO-LABEL
     fi-tot-fatur AT ROW 20.17 COL 58.57 COLON-ALIGNED NO-LABEL
     fi-tot-cancel AT ROW 20.17 COL 73.14 COLON-ALIGNED NO-LABEL
     fi-tot-aberto AT ROW 20.17 COL 87 COLON-ALIGNED NO-LABEL
     FILL-IN-3 AT ROW 20.75 COL 1 COLON-ALIGNED NO-LABEL
     "Tot Cancelado" VIEW-AS TEXT
          SIZE 12 BY .54 AT ROW 19.5 COL 76.29
          BGCOLOR 8 FONT 6
     "Reservado / Aceita Parcial" VIEW-AS TEXT
          SIZE 23.29 BY .67 AT ROW 20.67 COL 5.72
          BGCOLOR 8 FONT 6
     "Reserva Incompleta / N∆o Aceita Parcial" VIEW-AS TEXT
          SIZE 34.43 BY .67 AT ROW 20 COL 5.57
          BGCOLOR 8 FONT 6
     "Tot Faturado" VIEW-AS TEXT
          SIZE 10.86 BY .54 AT ROW 19.5 COL 63.72
          BGCOLOR 8 FONT 6
     "Tot Pedido" VIEW-AS TEXT
          SIZE 9 BY .54 AT ROW 19.5 COL 50.72
          BGCOLOR 8 FONT 6
     "Tot Aberto" VIEW-AS TEXT
          SIZE 9.43 BY .54 AT ROW 19.5 COL 93.86
          BGCOLOR 8 FONT 6
     "Restriá∆o de CrÇdito / Sem Reservas" VIEW-AS TEXT
          SIZE 32.43 BY .67 AT ROW 19.33 COL 5.57
          BGCOLOR 8 FONT 6
     RECT-4 AT ROW 19.25 COL 2
     rt-buttom AT ROW 1.25 COL 106
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113.72 BY 20.75.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Gerenciamento Pedido de Vendas -  ESSP0155"
         COLUMN             = 42.86
         ROW                = 5.83
         HEIGHT             = 20.75
         WIDTH              = 113.72
         MAX-HEIGHT         = 29.79
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 29.79
         VIRTUAL-WIDTH      = 146.29
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB br-pedidos rt-buttom DEFAULT-FRAME */
/* SETTINGS FOR BUTTON bt-excel IN FRAME DEFAULT-FRAME
   6                                                                    */
/* SETTINGS FOR FILL-IN fi-tot-aberto IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-cancel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-fatur IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-pedida IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-1:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-2 IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
ASSIGN 
       FILL-IN-2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FILL-IN-3:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-pedidos
/* Query rebuild information for BROWSE br-pedidos
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-ped-venda NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "mgmov.ped-venda.cod-sit-ped = 1"
     _Query            is OPENED
*/  /* BROWSE br-pedidos */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Gerenciamento Pedido de Vendas -  ESSP0155 */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Gerenciamento Pedido de Vendas -  ESSP0155 */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-pedidos
&Scoped-define SELF-NAME br-pedidos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-pedidos C-Win
ON ANY-KEY OF br-pedidos IN FRAME DEFAULT-FRAME /* Carteira de Pedidos Selecionada */
DO:
  IF INDEX("1234567890",KEYFUNCTION(LASTKEY)) <> 0 THEN DO.
     ASSIGN c-busca = c-busca + KEYFUNCTION(LASTKEY).
    
     IF SUBSTR(STRING(tt-ped-venda.nr-pedcli),1,LENGTH(c-busca)) <> c-busca THEN DO.
        FIND FIRST tt-ped-venda WHERE
                   (tt-ped-venda.cod-sit-ped = 1 OR tt-ped-venda.cod-sit-ped = 3) AND
                    tt-ped-venda.nr-pedcli BEGINS c-busca NO-LOCK NO-ERROR.
        IF AVAIL tt-ped-venda THEN DO.
           h-query:REPOSITION-TO-ROWID(ROWID(tt-ped-venda)) NO-ERROR.   
           APPLY 'value-changed' TO SELF.
        END.
        ELSE DO.
           ASSIGN c-busca = "".
           FIND tt-ped-venda WHERE 
                ROWID(tt-ped-venda) = v-row-table NO-LOCK NO-ERROR.
        END.
     END.
     IF AVAIL tt-ped-venda THEN DO.
        IF tt-ped-venda.nr-pedcli = c-busca THEN
           ASSIGN c-busca = "".
     END.
  END.
  ELSE 
     ASSIGN c-busca = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-pedidos C-Win
ON MOUSE-SELECT-DBLCLICK OF br-pedidos IN FRAME DEFAULT-FRAME /* Carteira de Pedidos Selecionada */
DO:
  APPLY 'choose' TO bt-consulta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-pedidos C-Win
ON RETURN OF br-pedidos IN FRAME DEFAULT-FRAME /* Carteira de Pedidos Selecionada */
DO:
  APPLY 'choose' TO bt-consulta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-pedidos C-Win
ON ROW-DISPLAY OF br-pedidos IN FRAME DEFAULT-FRAME /* Carteira de Pedidos Selecionada */
DO:
    IF tt-ped-venda.cod-sit-aval = 1 OR tt-ped-venda.cod-sit-aval > 3 THEN
       RUN pi-cor (INPUT 12).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-pedidos C-Win
ON VALUE-CHANGED OF br-pedidos IN FRAME DEFAULT-FRAME /* Carteira de Pedidos Selecionada */
DO:
   IF AVAIL tt-ped-venda THEN DO.
      FIND ped-venda-ext WHERE
           ped-venda-ext.cod-estabel = tt-ped-venda.cod-estabel AND  /* daf  */
           ped-venda-ext.nr-pedido = tt-ped-venda.nr-pedido NO-LOCK NO-ERROR.

      ASSIGN bt-modifica:SENSITIVE IN FRAME {&FRAME-NAME} = YES
             bt-suspende:SENSITIVE IN FRAME {&FRAME-NAME} = YES
             bt-cancela:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
    
      IF tt-ped-venda.cod-sit-ped = 3 OR 
         tt-ped-venda.cod-sit-ped = 6 THEN
         ASSIGN bt-modifica:SENSITIVE IN FRAME {&FRAME-NAME} = NO
                bt-suspende:SENSITIVE IN FRAME {&FRAME-NAME} = NO
                bt-cancela:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

      ASSIGN c-finalidade = "Suspens∆o".
      bt-suspende:LOAD-IMAGE("image/im-ngrava.bmp").
      IF tt-ped-venda.cod-sit-ped = 5 THEN DO.
         ASSIGN c-finalidade = 'Reativaá∆o'.
         bt-suspende:LOAD-IMAGE("image/im-grava.bmp").
      END.

      bt-status:LOAD-IMAGE-INSENSITIVE("image/imt-smfec.bmp").
      IF tt-ped-venda.qt-reservada > 0 AND 
         (tt-ped-venda.cod-sit-aval = 2 OR tt-ped-venda.cod-sit-aval = 3) THEN DO.
         IF tt-ped-venda.ind-fat-par THEN
            bt-status:LOAD-IMAGE-INSENSITIVE("image/imt-smabe.bmp").
         ELSE
            bt-status:LOAD-IMAGE-INSENSITIVE("image/imt-smate.bmp").
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cancela
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancela C-Win
ON CHOOSE OF bt-cancela IN FRAME DEFAULT-FRAME
DO:
   RUN pi-ver-permissao.
   IF NOT l-ok THEN DO.
      MESSAGE "Usu†rio: " + c-seg-usuario + ", n∆o tem permiss∆o para essa rotina."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
   END.

   FIND ped-venda WHERE
        ped-venda.nr-pedcli = tt-ped-venda.nr-pedcli AND
        ped-venda.nome-abrev = tt-ped-venda.nome-abrev 
        SHARE-LOCK NO-ERROR.

   FIND pre-fatur OF ped-venda NO-LOCK NO-ERROR.
   IF AVAIL pre-fatur THEN DO.
      MESSAGE "Pedido Embarcado no Embarque " pre-fatur.nr-embarque SKIP
              "Solicite ao Setor Respons†vel para Desembarc†-lo"
          VIEW-AS ALERT-BOX ERROR BUTTONS OK
          TITLE "Cancelamento n∆o Permitido".
      NEXT.
   END.

   FIND ped-venda-ext WHERE
        ped-venda-ext.cod-estabel = tt-ped-venda.cod-estabel AND  /* daf  */
        ped-venda-ext.nr-pedido = INT(tt-ped-venda.nr-pedcli)
        NO-LOCK NO-ERROR.
   IF AVAIL ped-venda-ext AND
      ped-venda-ext.l-etiqueta THEN DO.   /* Pedido em Separaá∆o */
      MESSAGE "Pedido est† em Processo de Separaá∆o," SKIP
              "Solicite ao Setor Respons†vel para Liber†-lo"
          VIEW-AS ALERT-BOX ERROR BUTTONS OK
          TITLE "Cancelamento n∆o Permitido".
      NEXT.
   END.

   FIND FIRST ped-item-res WHERE
              ped-item-res.nr-pedcli = tt-ped-venda.nr-pedcli AND
              ped-item-res.nome-abrev = tt-ped-venda.nome-abrev AND
              ped-item-res.faturado = NO
              NO-LOCK NO-ERROR.
   IF AVAIL ped-item-res THEN DO.
      MESSAGE "Existem Peáas Reservadas para esse Pedido," SKIP
              "Cancelamento s¢ ser† permitido se Cancelar TODAS as Reservas." SKIP(1)
              "Deseja Cancelar as Reservas ?"
              VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
              TITLE "" UPDATE l-opcao AS LOGICAL.
      IF l-opcao = NO THEN
         RETURN NO-APPLY.
   END.
  
   FIND ped-venda WHERE
        ped-venda.nr-pedcli = tt-ped-venda.nr-pedcli AND
        ped-venda.nome-abrev = tt-ped-venda.nome-abrev EXCLUSIVE-LOCK NO-ERROR.

   ASSIGN c-finalidade = 'Cancelamento'.
   RUN pdp/pd4000a.p (INPUT c-finalidade,
                      OUTPUT c-tipo-trans,
                      OUTPUT c-motivo,
                      OUTPUT da-dt-trans,
                      OUTPUT l-ok).
  
   IF l-ok THEN DO.

      SESSION:SET-WAIT-STATE("general":U).

      FIND motivo WHERE
           motivo.cod-motivo = INT(c-tipo-trans) AND 
           motivo.ind-tp-trans = 1 NO-LOCK NO-ERROR.
      ASSIGN c-motivo = motivo.descricao + " // " + c-motivo.

      IF c-motivo = "" THEN DO.
         MESSAGE "Favor Informat o Motivo do Cancelamento"
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
         RETURN NO-APPLY.
      END.

      IF LENGTH(c-motivo) < 10 THEN DO.
         MESSAGE "Descriá∆o do Motivo do Cancelamento Inv†lida"
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
         RETURN NO-APPLY.
      END.

      RUN esapi/cancela-pedvenda.p (INPUT tt-ped-venda.nr-pedcli,
                                    INPUT c-motivo).

      IF RETURN-VALUE = 'ADM-ERROR' THEN DO.
         SESSION:SET-WAIT-STATE("":U).
         RETURN 'ADM-ERROR'.
      END.
      ASSIGN ped-venda.cd-cancela = motivo.cod-motivo.

      RUN pi-cancela-reserva.
  
      FIND ped-venda WHERE
           ped-venda.nr-pedcli = tt-ped-venda.nr-pedcli AND
           ped-venda.nome-abrev = tt-ped-venda.nome-abrev NO-LOCK NO-ERROR.
      CASE ped-venda.cod-sit-ped.
           WHEN 1 THEN ASSIGN tt-ped-venda.situacao = 'ABE'.
           WHEN 2 THEN ASSIGN tt-ped-venda.situacao = 'ATP'.
           WHEN 3 THEN ASSIGN tt-ped-venda.situacao = 'ATT'.
           WHEN 4 THEN ASSIGN tt-ped-venda.situacao = 'PEN'.
           WHEN 5 THEN ASSIGN tt-ped-venda.situacao = 'SUS'.
           WHEN 6 THEN ASSIGN tt-ped-venda.situacao = 'CAN'.
      END CASE.
  
      RUN esapi/cria-log-pedvenda.p (INPUT ped-venda.nr-pedcli,
                                     INPUT ped-venda.nome-abrev,
                                     INPUT "Pedido Cancelado. " + c-motivo,
                                     INPUT YES).

      FOR EACH ped-item OF ped-venda WHERE 
               ped-item.cod-sit-item = 6 NO-LOCK.
          RUN esapi/cria-log-pedvenda.p (INPUT ped-item.nr-pedcli,
                                         INPUT ped-item.nome-abrev,
                                         INPUT TRIM(STRING(ped-item.nr-sequencia,">>>9")) + 
                                               " Item: " + TRIM(ped-item.it-codigo) + " Refer: " + TRIM(ped-item.cod-refer) + 
                                               " Qtde: " + TRIM(STRING(ped-item.qt-pedida,">>>,>>9.99")) + ": Cancelada",
                                         INPUT YES).
      END.
      SESSION:SET-WAIT-STATE("":U).




      {&OPEN-QUERY-br-pedidos}
      APPLY 'value-changed' TO br-pedidos.
      
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-consulta C-Win
ON CHOOSE OF bt-consulta IN FRAME DEFAULT-FRAME
DO:
   FIND ped-venda WHERE
        ped-venda.nr-pedcli = tt-ped-venda.nr-pedcli AND
        ped-venda.nome-abrev = tt-ped-venda.nome-abrev NO-LOCK NO-ERROR.
   ASSIGN gr-ped-venda = ROWID(ped-venda).

   ASSIGN c-win:SENSITIVE = NO.
   RUN esp\espd4000.w (INPUT "Consultar").
   ASSIGN c-win:SENSITIVE = YES.
   APPLY 'ENTRY' TO SELF.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-excel C-Win
ON CHOOSE OF bt-excel IN FRAME DEFAULT-FRAME /* Button 2 */
DO:
  
   RUN esdlg/d02essp0155.w (OUTPUT arq-saida,
                            INPUT da-dt-implant-ini,
                            INPUT da-dt-implant-fin).
   IF arq-saida <> "" THEN DO:
      SESSION:SET-WAIT-STATE("general":U).

      RUN pi-gera-excel (INPUT arq-saida). 

      SESSION:SET-WAIT-STATE("":U).

      MESSAGE "O arquivo foi gerado em " TRIM(arq-saida) + "." SKIP 
              "Para acess†-lo,  abra-o atravÇs do Excel."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
   END.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-exit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-exit C-Win
ON CHOOSE OF bt-exit IN FRAME DEFAULT-FRAME
DO:
   APPLY 'CLOSE':U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-imprime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-imprime C-Win
ON CHOOSE OF bt-imprime IN FRAME DEFAULT-FRAME
DO:
  RUN pi-imprime.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-imprime-txt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-imprime-txt C-Win
ON CHOOSE OF bt-imprime-txt IN FRAME DEFAULT-FRAME
DO:
    ASSIGN gr-ped-venda = ?.
    ASSIGN c-win:SENSITIVE = NO.
    RUN esp/essp0155d.w.
    ASSIGN c-win:SENSITIVE = YES.

    IF gr-ped-venda <> ? THEN DO.
       FIND ped-venda WHERE
            ROWID(ped-venda) = gr-ped-venda NO-LOCK NO-ERROR.
       IF AVAIL ped-venda THEN
          RUN pi-add-pedidos (INPUT ped-venda.nr-pedcli).

       FIND FIRST tt-ped-venda WHERE
                  tt-ped-venda.nr-pedcli = ENTRY(NUM-ENTRIES(c-pedidos),c-pedidos) NO-LOCK NO-ERROR.
       IF AVAIL tt-ped-venda THEN 
          h-query:REPOSITION-TO-ROWID(ROWID(tt-ped-venda)) NO-ERROR. 
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-inclui
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-inclui C-Win
ON CHOOSE OF bt-inclui IN FRAME DEFAULT-FRAME
DO:
   RUN pi-ver-permissao.
   IF NOT l-ok THEN
      MESSAGE "Usu†rio: " + c-seg-usuario + ", n∆o tem permiss∆o para essa rotina."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
   ELSE DO:
      ASSIGN c-win:SENSITIVE = NO.
      RUN esp\espd4000.w (INPUT "Incluir").
      ASSIGN c-win:SENSITIVE = YES.

      IF RETURN-VALUE <> '' THEN DO.
         ASSIGN c-pedidos = RETURN-VALUE.

         DO i-ct = 1 TO NUM-ENTRIES(c-pedidos).
            RUN pi-add-pedidos (INPUT ENTRY(i-ct,c-pedidos)).
         END.

         FIND FIRST tt-ped-venda WHERE
                    tt-ped-venda.nr-pedcli = ENTRY(NUM-ENTRIES(c-pedidos),c-pedidos) NO-LOCK NO-ERROR.
         IF AVAIL tt-ped-venda THEN 
            h-query:REPOSITION-TO-ROWID(ROWID(tt-ped-venda)) NO-ERROR. 
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-log
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-log C-Win
ON CHOOSE OF bt-log IN FRAME DEFAULT-FRAME
DO:
   RUN esp/essp0155b.p (INPUT tt-ped-venda.nr-pedcli).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-modifica
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-modifica C-Win
ON CHOOSE OF bt-modifica IN FRAME DEFAULT-FRAME /* Sair */
DO:
   RUN pi-ver-permissao.
   IF NOT l-ok THEN DO.
      MESSAGE "Usu†rio: " + c-seg-usuario + ", n∆o tem permiss∆o para essa rotina."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
   END.
   
   IF tt-ped-venda.situacao = 'CAN' THEN DO.
      MESSAGE 'Vocà acabaou de CANCELAR esse pedido, Alteraá∆o N«O PERMITIDA'
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      RETURN NO-APPLY.
   END.

   FIND ped-venda WHERE
        ped-venda.nr-pedcli = tt-ped-venda.nr-pedcli AND
        ped-venda.nome-abrev = tt-ped-venda.nome-abrev 
        SHARE-LOCK NO-ERROR.

  FIND ped-venda-ext WHERE
       ped-venda-ext.cod-estabel = ped-venda.cod-estabel AND  
       ped-venda-ext.nr-pedido = INT(ped-venda.nr-pedcli)
       NO-LOCK NO-ERROR.

   IF AVAIL ped-venda-ext AND
      ped-venda-ext.l-etiqueta THEN DO.   /* Pedido em Separaá∆o */
      MESSAGE "Os Itens do Pedido n∆o Poder∆o ser Alterados neste momento pois, " SKIP
              "este Pedido est† em Processo de Separaá∆o !!!" SKIP(1)
              "EVENTUAIS ALTERAÄÂES NOS ITENS SER«O DESCONSIDERADAS" SKIP
              "Deseja Continuar com a Alteraá∆o?"
              VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                    TITLE "" UPDATE l-sep AS LOGICAL.
      IF NOT l-sep THEN RETURN NO-APPLY.
   END.    

   ASSIGN i-sit = ped-venda.cod-sit-ped. /* Salva Situacao do Pedido */
   IF ped-venda.cod-sit-ped = 4 THEN
      ASSIGN ped-venda.cod-sit-ped = 1.

   ASSIGN gr-ped-venda = ROWID(ped-venda).

   ASSIGN c-win:SENSITIVE = NO.
   RUN esp\espd4000.w (INPUT "Modificar").
   ASSIGN c-win:SENSITIVE = YES.

   ASSIGN ped-venda.cod-sit-ped = i-sit.

   FIND CURRENT ped-venda NO-LOCK NO-ERROR.
   FIND FIRST ped-item OF ped-venda NO-LOCK NO-ERROR.

   APPLY 'ENTRY' TO SELF.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-param
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-param C-Win
ON CHOOSE OF bt-param IN FRAME DEFAULT-FRAME /* Sair */
DO:
   EMPTY TEMP-TABLE tt-digita.

   FIND FIRST ped-venda NO-LOCK NO-ERROR.
   ASSIGN c-cod-estabel = ped-venda.cod-estabel.
  
   ASSIGN c-win:SENSITIVE = NO.
   RUN esp/essp0155a.w (INPUT-OUTPUT c-cod-estabel,
                        INPUT-OUTPUT TABLE tt-digita,
                        INPUT-OUTPUT da-dt-implant-ini,   
                        INPUT-OUTPUT da-dt-implant-fin,   
                        INPUT-OUTPUT c-nr-pedcli-ini,   
                        INPUT-OUTPUT c-nr-pedcli-fin,   
                        INPUT-OUTPUT c-it-codigo-ini,   
                        INPUT-OUTPUT c-it-codigo-fin,   
                        INPUT-OUTPUT c-cod-refer-ini,   
                        INPUT-OUTPUT c-cod-refer-fin,   
                        INPUT-OUTPUT c-nome-abrev-ini,
                        INPUT-OUTPUT c-nome-abrev-fin,
                        INPUT-OUTPUT c-no-ab-reppri-ini,
                        INPUT-OUTPUT c-no-ab-reppri-fin,
                        INPUT-OUTPUT c-uf-ini,
                        INPUT-OUTPUT c-uf-fin,
                        INPUT-OUTPUT c-nome-tra-ini,
                        INPUT-OUTPUT c-nome-tra-fin,
                        INPUT-OUTPUT c-cidade-ini,
                        INPUT-OUTPUT c-cidade-fin,
                        INPUT-OUTPUT i-nr-container-ini,
                        INPUT-OUTPUT i-nr-container-fin,
                        INPUT-OUTPUT i-situacao,
                        INPUT-OUTPUT i-preco,
                        INPUT-OUTPUT i-aprovar,
                        INPUT-OUTPUT i-espera,
                        INPUT-OUTPUT c-tp-pedido,   
                        INPUT-OUTPUT l-corte,
                        INPUT-OUTPUT l-sit-todas,       
                        INPUT-OUTPUT l-sit-abe,         
                        INPUT-OUTPUT l-sit-atp,         
                        INPUT-OUTPUT l-sit-att,         
                        INPUT-OUTPUT l-sit-pen,         
                        INPUT-OUTPUT l-sit-sus,         
                        INPUT-OUTPUT l-sit-can, 
                        INPUT-OUTPUT i-credito,
                        INPUT-OUTPUT l-ok). 
   IF l-ok THEN                                     
      RUN pi-popula-browse.

    ASSIGN c-win:SENSITIVE = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-status
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-status C-Win
ON CHOOSE OF bt-status IN FRAME DEFAULT-FRAME
DO:
   SESSION:SET-WAIT-STATE("general":U).

   FIND ped-venda WHERE
        ped-venda.nr-pedcli = tt-ped-venda.nr-pedcli NO-ERROR.
   ASSIGN ped-venda.completo = NO.
   IF ped-venda.tp-pedido = "PE" THEN 
      RUN esapi/completa-pedvenda.p (INPUT tt-ped-venda.nr-pedcli).

   SESSION:SET-WAIT-STATE("":U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-suspende
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-suspende C-Win
ON CHOOSE OF bt-suspende IN FRAME DEFAULT-FRAME
DO:
   RUN pi-ver-permissao.
   IF NOT l-ok THEN
      MESSAGE "Usu†rio: " + c-seg-usuario + ", n∆o tem permiss∆o para essa rotina."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
   ELSE DO:
      RUN pdp/pd4000a.p (INPUT c-finalidade,
                         OUTPUT c-tipo-trans,
                         OUTPUT c-motivo,
                         OUTPUT da-dt-trans,
                         OUTPUT l-ok).
      
      IF l-ok THEN DO.
         IF tt-ped-venda.cod-sit-ped = 5 THEN 
            RUN esapi/reativa-pedvenda.p (INPUT tt-ped-venda.nr-pedcli,
                                          INPUT c-motivo).
         ELSE
            RUN esapi/suspende-pedvenda.p (INPUT tt-ped-venda.nr-pedcli,
                                           INPUT c-motivo).

        FIND ped-venda WHERE
             ped-venda.nr-pedcli = tt-ped-venda.nr-pedcli AND
             ped-venda.nome-abrev = tt-ped-venda.nome-abrev NO-LOCK NO-ERROR.
      
        CASE ped-venda.cod-sit-ped.
             WHEN 1 THEN ASSIGN tt-ped-venda.situacao = 'ABE'.
             WHEN 2 THEN ASSIGN tt-ped-venda.situacao = 'ATP'.
             WHEN 3 THEN ASSIGN tt-ped-venda.situacao = 'ATT'.
             WHEN 4 THEN ASSIGN tt-ped-venda.situacao = 'PEN'.
             WHEN 5 THEN ASSIGN tt-ped-venda.situacao = 'SUS'.
             WHEN 6 THEN ASSIGN tt-ped-venda.situacao = 'CAN'.
        END CASE.
        ASSIGN tt-ped-venda.cod-sit-ped = ped-venda.cod-sit-ped.

        {&OPEN-QUERY-br-pedidos}
        APPLY 'value-changed' TO br-pedidos.
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-vapara
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-vapara C-Win
ON CHOOSE OF bt-vapara IN FRAME DEFAULT-FRAME
DO:
   RUN esdlg/d01essp0155.w (OUTPUT c-nr-pedcli).

   IF c-nr-pedcli <> "" THEN DO:
      FIND FIRST tt-ped-venda WHERE
                 tt-ped-venda.nr-pedcli = c-nr-pedcli NO-LOCK NO-ERROR.

      IF NOT AVAIL tt-ped-venda THEN DO.
         MESSAGE "Pedido n∆o est† contido na seleá∆o!"
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
         RETURN NO-APPLY.
      END.

      h-query:REPOSITION-TO-ROWID(ROWID(tt-ped-venda)) NO-ERROR. 
      APPLY 'VALUE-CHANGED' TO br-pedidos.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

ON 'F5':U OF br-pedidos DO:
   {&OPEN-QUERY-br-pedidos}
   APPLY 'value-changed' TO br-pedidos.
END.

ASSIGN h-query = br-pedidos:QUERY.
br-pedidos:NUM-LOCKED-COLUMNS = 3.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

   RUN enable_UI.

   ASSIGN da-dt-implant-ini = 01.01.0001
          da-dt-implant-fin = 12.31.9999 
          l-sit-abe         = YES
          l-sit-atp         = YES
          l-lote-todos      = YES
          c-tp-pedido       = 'Todos,Todos'.

   APPLY 'choose' TO bt-param.

   IF NOT THIS-PROCEDURE:PERSISTENT THEN
      WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY FILL-IN-1 FILL-IN-2 fi-tot-pedida fi-tot-fatur fi-tot-cancel 
          fi-tot-aberto FILL-IN-3 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-4 rt-buttom br-pedidos bt-param bt-inclui bt-modifica bt-cancela 
         bt-suspende bt-status bt-vapara bt-consulta bt-log bt-imprime bt-excel 
         bt-imprime-txt bt-exit FILL-IN-1 FILL-IN-2 FILL-IN-3 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-abre-excel C-Win 
PROCEDURE pi-abre-excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   DEF INPUT PARAMETER p-arquivo AS CHAR.

   DEF VAR h-prog AS HANDLE NO-UNDO.
   RUN utp/ut-utils.p PERSISTENT SET h-prog.

   RUN EXECUTE IN h-prog(INPUT "EXCEL.EXE", INPUT p-arquivo).

   DELETE PROCEDURE h-prog.
   PAUSE 5 NO-MESSAGE.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-add-pedidos C-Win 
PROCEDURE pi-add-pedidos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER p-nr-pedido AS CHAR.

    FIND ped-venda WHERE
         ped-venda.nr-pedido = INTEGER(p-nr-pedido) NO-LOCK NO-ERROR.

    FOR EACH ped-item OF ped-venda NO-LOCK.
        FIND tt-ped-venda WHERE
             tt-ped-venda.nr-pedcli = ped-venda.nr-pedcli AND
             tt-ped-venda.nome-abrev = ped-venda.nome-abrev NO-ERROR.
    
        IF NOT AVAIL tt-ped-venda THEN DO:
           CREATE tt-ped-venda.
           BUFFER-COPY ped-venda TO tt-ped-venda.
    
           CASE ped-venda.cod-sit-ped.
                WHEN 1 THEN ASSIGN tt-ped-venda.situacao = 'ABE'.
                WHEN 2 THEN ASSIGN tt-ped-venda.situacao = 'ATP'.
                WHEN 3 THEN ASSIGN tt-ped-venda.situacao = 'ATT'.
                WHEN 4 THEN ASSIGN tt-ped-venda.situacao = 'PEN'.
                WHEN 5 THEN ASSIGN tt-ped-venda.situacao = 'SUS'.
                WHEN 6 THEN ASSIGN tt-ped-venda.situacao = 'CAN'.
           END CASE.
        END.
        ASSIGN tt-ped-venda.qt-pedida = tt-ped-venda.qt-pedida + ped-item.qt-pedida
               fi-tot-pedida = fi-tot-pedida + ped-item.qt-pedida.
    
        FOR EACH ped-item-res WHERE
                 ped-item-res.nome-abrev = tt-ped-venda.nome-abrev AND
                 ped-item-res.nr-pedcli  = tt-ped-venda.nr-pedcli  AND
                 ped-item-res.nr-sequencia = ped-item.nr-sequencia  AND
                 ped-item-res.faturado   = NO NO-LOCK.
            ASSIGN tt-ped-venda.qt-reservada = tt-ped-venda.qt-reservada + ped-item-res.qt-pedida.
        END.
    
        ASSIGN tt-ped-venda.qt-aberto = tt-ped-venda.qt-pedida - tt-ped-venda.qt-cancelada - 
                                        tt-ped-venda.qt-faturada.

        ASSIGN fi-tot-aberto = fi-tot-aberto + tt-ped-venda.qt-aberto.
    
        {&OPEN-QUERY-br-pedidos}
        APPLY 'value-changed' TO br-pedidos IN FRAME {&FRAME-NAME}.
    END.

    DISP fi-tot-pedida
         fi-tot-aberto
         WITH FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cancela-reserva C-Win 
PROCEDURE pi-cancela-reserva :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH ped-item-res WHERE
             ped-item-res.nr-pedcli = tt-ped-venda.nr-pedcli AND
             ped-item-res.nome-abrev = tt-ped-venda.nome-abrev AND
             ped-item-res.faturado = NO
             SHARE-LOCK.
        DELETE ped-item-res.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-carrega-dados C-Win 
PROCEDURE pi-carrega-dados :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 {include/i-rprun.i esrp/espd0002arp.p}    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cor C-Win 
PROCEDURE pi-cor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER p-cor AS INT.
    tt-ped-venda.nr-pedcli:FGCOLOR IN BROWSE {&browse-name} = p-cor.
    tt-ped-venda.nr-pedrep:FGCOLOR IN BROWSE {&browse-name} = p-cor.
    tt-ped-venda.nome-abrev:FGCOLOR IN BROWSE {&browse-name} = p-cor.
    tt-ped-venda.nome-transp:FGCOLOR IN BROWSE {&browse-name} = p-cor.    
    tt-ped-venda.nat-oper:FGCOLOR IN BROWSE {&browse-name} = p-cor.    
    tt-ped-venda.dt-implant:FGCOLOR IN BROWSE {&browse-name} = p-cor.
    tt-ped-venda.situacao:FGCOLOR IN BROWSE {&browse-name} = p-cor. 
    tt-ped-venda.qt-pedida:FGCOLOR IN BROWSE {&browse-name} = p-cor.
    tt-ped-venda.qt-faturada:FGCOLOR IN BROWSE {&browse-name} = p-cor.
    tt-ped-venda.qt-reservada:FGCOLOR IN BROWSE {&browse-name} = p-cor.
    tt-ped-venda.qt-cancelada:FGCOLOR IN BROWSE {&browse-name} = p-cor.
    tt-ped-venda.qt-aberto:FGCOLOR IN BROWSE {&browse-name} = p-cor.
    tt-ped-venda.vl-desconto:FGCOLOR IN BROWSE {&browse-name} = p-cor.
    tt-ped-venda.dt-cancela:FGCOLOR IN BROWSE {&browse-name} = p-cor.
    tt-ped-venda.dt-apr-cred:FGCOLOR IN BROWSE {&browse-name} = p-cor.
    tt-ped-venda.dt-entrega:FGCOLOR IN BROWSE {&browse-name} = p-cor.
    tt-ped-venda.nr-embarque:FGCOLOR IN BROWSE {&browse-name} = p-cor.
    tt-ped-venda.nr-container:FGCOLOR IN BROWSE {&browse-name} = p-cor. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cria-log C-Win 
PROCEDURE pi-cria-log :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER p-nr-pedcli LIKE ped-venda.nr-pedcli.
    DEF INPUT PARAMETER p-nome-abrev LIKE ped-venda.nome-abrev.
  /*  DEF INPUT PARAMETER p-nr-cod-estabel LIKE ped-venda.cod-estabel. */    /*  daf  */
    DEF INPUT PARAMETER p-ocorrencia AS CHAR.
    DEF INPUT PARAMETER p-envia-e-mail AS LOG.

    CREATE his-ped-venda-ext.
    ASSIGN /*  his-ped-venda-ext.cod-estabel = p-nr-cod-estabel  */      /*  daf  */
           his-ped-venda-ext.nr-pedcli  = p-nr-pedcli
           his-ped-venda-ext.nome-abrev = p-nome-abrev
           his-ped-venda-ext.dt-trans   = TODAY
           his-ped-venda-ext.hr-trans   = TIME
           his-ped-venda-ext.usuario    = c-seg-usuario
           his-ped-venda-ext.ocorrencia = p-ocorrencia.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-gera-excel C-Win 
PROCEDURE pi-gera-excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 
    DEF INPUT PARAMETER p-arq-saida AS CHAR FORMAT "x(54)".
    
    DEF VAR chExcelApp  AS COM-HANDLE NO-UNDO.
    DEF VAR chWorkbook  AS COM-HANDLE NO-UNDO.
    DEF var chWorksheet AS COM-HANDLE NO-UNDO.
    DEF VAR cFileName   AS CHAR.
    DEF VAR i-lin       AS INT.
    DEF VAR i-ct        AS INT.
   
    CREATE "Excel.Application" chExcelApp.
   
   /* Cria a Planilha */
    ASSIGN chExcelApp:VISIBLE     = FALSE  /* A Planilha N«O Ficar† Visivel */
           chWorkbook             = chExcelApp:Workbooks:ADD() /* Cria Planilha */
           chworksheet            = chExcelapp:sheets:ITEM(1).
    
   /* Incluir Uma Nova Planilha */
    ChWorksheet = chWorkbook:Worksheets(1).
    chWorkbook:Worksheets:add(,chWorksheet).
   
    /* Selecionar a Nova Planilha Criada */
    chWorkbook:Worksheets(1):Activate.
    ChWorksheet = chWorkbook:Worksheets(1). 
    
    /* Determina cabeáalho da planilha */
    ASSIGN c-Lin = c-empresa + "                    " + " PEDIDOS DO PERIODO: "  + STRING(da-dt-implant-ini, "99/99/9999") + " A " + STRING(da-dt-implant-fin, "99/99/9999"). 
    
    /* Incluir Dados no Cabeáalho da Planilha Selecionada*/
    ASSIGN chWorksheet:range("B1"):VALUE = c-Lin.
   
    /* Mesclando CÇlulas no Cabeáalho da Planilha Selecionada*/
    chWorksheet:range("A1:L1"):MergeCells = 1.
   
    /* Define Orientaá∆o da P†gina com Formato Paisagem */
    ASSIGN chWorkSheet:PageSetup:Orientation = 2. 
   
    /* Aplica 90% para ajuste na impressao da pagina */
    chWorkSheet:PageSetup:Zoom  = 65.
   
    /* Adiciona o cabeáalho em todas as paginas de Impressao */
    chWorkSheet:PageSetup:PrintTitleRows  = "$3:$3". 
   
    /* Altera formato da Celula */
    ASSIGN i-Lin = 1.
    ASSIGN chWorksheet:range("1:" + STRING(i-lin)):FONT:ColorIndex = 5
           chWorksheet:range("1:" + STRING(i-lin)):FONT:NAME = "Courrier New"
           chWorksheet:range("1:" + STRING(i-lin)):FONT:bold = TRUE
           chWorksheet:range("1:" + STRING(i-lin)):FONT:SIZE = 14.
   
     ASSIGN i-Lin = 3.
   
    chWorkSheet:range("A" + STRING(i-lin) + ":Z" + STRING(i-lin)):FONT:NAME = "Lucida Sans".
    chWorkSheet:range("A" + STRING(i-lin) + ":Z" + STRING(i-lin)):FONT:SIZE = 8.
    chWorkSheet:range("A" + STRING(i-lin) + ":Z" + STRING(i-lin)):FONT:bold = TRUE.
   
    ASSIGN chWorksheet:range("A" + STRING(i-lin)):VALUE = "EMISS«O"
           chWorksheet:range("B" + STRING(i-lin)):VALUE = "PEDIDO"
           chWorksheet:range("C" + STRING(i-lin)):VALUE = "REPRES."
           chWorksheet:range("D" + STRING(i-lin)):VALUE = "CLIENTE"
           chWorksheet:range("E" + STRING(i-lin)):VALUE = "COND. PAGTO"
           chWorksheet:range("F" + STRING(i-lin)):VALUE = "SIT"
           chWorksheet:range("G" + STRING(i-lin)):VALUE = "TOTAL PEDIDO"
           chWorksheet:range("H" + STRING(i-lin)):VALUE = "TOTAL RESERVADO"
           chWorksheet:range("I" + STRING(i-lin)):VALUE = "TOTAL ABERTO"
           chWorksheet:range("J" + STRING(i-lin)):VALUE = "VALOR DESCONTO".
           chWorksheet:range("K" + STRING(i-lin)):VALUE = "VALOR TOTAL PEDIDO".
           chWorksheet:range("L" + STRING(i-lin)):VALUE = "DT. APROV.".
           chWorksheet:range("M" + STRING(i-lin)):VALUE = "DT. IMPLANT".     
           chWorksheet:range("N" + STRING(i-lin)):VALUE = "DT. CANCEL".    
           chWorksheet:range("O" + STRING(i-lin)):VALUE = "USR CANCEL".    
           chWorksheet:range("P" + STRING(i-lin)):VALUE = "MOTIVO CANCEL".    
           chWorksheet:range("Q" + STRING(i-lin)):VALUE = "MOEDA".    
           chWorksheet:range("R" + STRING(i-lin)):VALUE = "CONTAINER".    
           chWorksheet:range("S" + STRING(i-lin)):VALUE = "OBSERVAÄÂES".

    ASSIGN i-Lin = 4.
    FOR EACH tt-ped-venda NO-LOCK
        BREAK BY tt-ped-venda.nr-pedcli:
   
        FIND moeda WHERE
             moeda.mo-codigo = tt-ped-venda.mo-codigo NO-LOCK NO-ERROR.

        FIND cond-pagto WHERE
             cond-pagto.cod-cond-pag = tt-ped-venda.cod-cond-pag NO-LOCK NO-ERROR.
        IF AVAIL cond-pagto THEN
           ASSIGN c-desc-cond-pag = cond-pagto.descricao.
        ELSE
           ASSIGN c-desc-cond-pag = 'ESPECIAL'.
        
        chWorksheet:range("A" + STRING(i-lin)):VALUE = tt-ped-venda.dt-emissao.
        chWorksheet:range("B" + STRING(i-lin)):VALUE = tt-ped-venda.nr-pedcli.
        chWorksheet:range("C" + STRING(i-lin)):VALUE = tt-ped-venda.no-ab-reppri.
        chWorksheet:range("D" + STRING(i-lin)):VALUE = tt-ped-venda.nome-abrev.
        chWorksheet:range("E" + STRING(i-lin)):VALUE = c-desc-cond-pag.
        chWorksheet:range("F" + STRING(i-lin)):VALUE = tt-ped-venda.situacao.
        chWorksheet:range("G" + STRING(i-lin)):VALUE = tt-ped-venda.qt-pedida.
        chWorksheet:range("H" + STRING(i-lin)):VALUE = tt-ped-venda.qt-reservada.
        chWorksheet:range("I" + STRING(i-lin)):VALUE = tt-ped-venda.qt-aberto.
        chWorksheet:range("J" + STRING(i-lin)):VALUE = tt-ped-venda.vl-desconto.
        chWorksheet:range("K" + STRING(i-lin)):VALUE = tt-ped-venda.vl-tot-ped.
        chWorksheet:range("L" + STRING(i-lin)):VALUE = tt-ped-venda.dt-apr-cred.
        chWorksheet:range("M" + STRING(i-lin)):VALUE = tt-ped-venda.dt-implant.   
        chWorksheet:range("N" + STRING(i-lin)):VALUE = tt-ped-venda.dt-cancel.   
        chWorksheet:range("O" + STRING(i-lin)):VALUE = tt-ped-venda.user-canc.
        chWorksheet:range("P" + STRING(i-lin)):VALUE = tt-ped-venda.desc-cancela.
        chWorksheet:range("Q" + STRING(i-lin)):VALUE = moeda.descricao.   
        chWorksheet:range("R" + STRING(i-lin)):VALUE = tt-ped-venda.nr-container.   
        chWorksheet:range("S" + STRING(i-lin)):VALUE = tt-ped-venda.observacoes.
   
        chWorkSheet:range("G" + STRING(i-lin) + ":I" + STRING(i-lin)):NumberFormat = "#.##0,00".
        chWorkSheet:range("J" + STRING(i-lin) + ":I" + STRING(i-lin)):NumberFormat = "#.##0,00".
        chWorkSheet:range("K" + STRING(i-lin) + ":I" + STRING(i-lin)):NumberFormat = "#.##0,00".
   
        ASSIGN i-Lin = i-lin + 1.
        
        ACCUMULATE tt-ped-venda.qt-pedida    (TOTAL).
        ACCUMULATE tt-ped-venda.qt-reservada (TOTAL). 
        ACCUMULATE tt-ped-venda.qt-aberto    (TOTAL).
        ACCUMULATE tt-ped-venda.vl-desconto  (TOTAL).
        ACCUMULATE tt-ped-venda.vl-tot-ped   (TOTAL).
      
    END.
   
    ASSIGN i-Lin = i-lin + 1.

    ASSIGN chWorksheet:range("E" + STRING(i-lin)):VALUE = "TOTAL"
           chWorkSheet:range("H" + STRING(i-lin) + ":K" + STRING(i-lin)):NumberFormat = "#.##0,00"
           chWorkSheet:range("F" + STRING(i-lin) + ":K" + STRING(i-lin)):FONT:ColorIndex     =  3
           chWorkSheet:range("A" + STRING(i-lin) + ":K" + STRING(i-lin)):FONT:Bold           = TRUE
           chWorkSheet:range("A" + STRING(i-lin) + ":K" + STRING(i-lin)):FONT:NAME = "Courrier New"
           chWorkSheet:range("A" + STRING(i-lin) + ":K" + STRING(i-lin)):FONT:SIZE = 12.

    ASSIGN chWorksheet:range("G" + STRING(i-lin)):VALUE = (ACCUM TOTAL tt-ped-venda.qt-pedida)
           chWorksheet:range("H" + STRING(i-lin)):VALUE = (ACCUM TOTAL tt-ped-venda.qt-reservada)  
           chWorksheet:range("I" + STRING(i-lin)):VALUE = (ACCUM TOTAL tt-ped-venda.qt-aberto)
           chWorksheet:range("J" + STRING(i-lin)):VALUE = (ACCUM TOTAL tt-ped-venda.vl-desconto)
           chWorksheet:range("K" + STRING(i-lin)):VALUE = (ACCUM TOTAL tt-ped-venda.vl-tot-ped).

    chWorkSheet:COLUMNS("A:S"):AutoFit.  /* Redimenciona automaticamente as colunas de A π L */
    chWorkSheet:Range("A4"):Activate().
    chWorkSheet:Range("A4"):SELECT().
    chWorkSheet:Application:ActiveWindow:FreezePanes = True.
    
    /* Determina o Nome do Arquivo */
    ASSIGN cFileName = TRIM(arq-saida) + ".".
    
    /* Salva e Fecha a Planilha */
    chExcelApp:DisplayAlerts = FALSE.
    chWorkBook:Save().
    chWorkBook:SaveAs(cFileName,,,,,,,).
    chWorkBook:CLOSE().
    chExcelApp:QUIT().  

    RELEASE OBJECT chworkBook. 
    RELEASE OBJECT chworksheet.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-grava-linha C-Win 
PROCEDURE pi-grava-linha :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imp-cabec C-Win 
PROCEDURE pi-imp-cabec :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    PUT c-empresa  FORMAT "X(40)"                 AT  1
        "DATA: "                                  AT 80
        STRING(TODAY,"99/99/9999") FORMAT "X(10)" AT 86
        "HORA: "                                  AT 105
        STRING(TIME,"hh:mm:ss")                   AT 111
        "PAG:"                                    AT 132
        i-pag FORMAT ">>>"                        AT 137
        SKIP(1).

    PUT "RELATORIO DE PEDIDOS NO PERIODO" AT 45.
    PUT da-dt-implant-ini FORMAT "99/99/9999" AT 77.
    PUT "A" AT 88.
    PUT da-dt-implant-fin FORMAT "99/99/9999" AT 90 SKIP(1).

    PUT "Pedido Ped. Repres. Cliente      Dt.Entrega Sit  Total Pedido  Tot.Faturado Tot.Reservado Tot.Cancelado  Total Aberto Dt.Implant  Aprovaá∆o" AT 1.
    PUT "------ ------------ ------------ ---------- --- ------------- ------------- ------------- ------------- ------------- ---------- ----------" AT 1.
    ASSIGN i-pag = i-pag + 1.                                                                              

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imprime C-Win 
PROCEDURE pi-imprime :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN esapi/saida-imp.p (OUTPUT i-saida,
                         OUTPUT c-saida,
                         OUTPUT i-num-copias,
                         OUTPUT l-ok).

  DO i-ct = 1 TO i-num-copias.
     RUN esapi/imp-ped-venda.p (INPUT INTEGER(tt-ped-venda.nr-pedcli),
                                INPUT i-saida).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imprime-old C-Win 
PROCEDURE pi-imprime-old :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /*
  DEF VAR h-prog AS HANDLE NO-UNDO.
  DEF VAR i-ct   AS INT.

  RUN utp/ut-utils.p PERSISTENT SET h-prog.


  RUN esapi/saida-imp.p (OUTPUT i-saida,
                         OUTPUT c-saida,
                         OUTPUT i-num-copias,
                         OUTPUT l-ok).

  CASE i-saida:
      WHEN 1 THEN DO.
          OUTPUT TO VALUE(c-saida) CONVERT TARGET "ibm850" PAGED PAGE-SIZE 62.
          PUT CONTROL "~033E~033(s19H".    
      END.
      WHEN 2 THEN
          OUTPUT TO VALUE(c-saida).
      WHEN 3 THEN DO.
          ASSIGN c-saida = SESSION:TEMP-DIRECTORY + "essp0183.tmp".
          OUTPUT TO VALUE(c-saida) CONVERT SOURCE "ibm850".
      END.
  END CASE.




  DO i-ct = 1 TO i-num-copias.
     ASSIGN i-lin = 99
            i-pag =  1.
     FOR EACH tt-ped-venda NO-LOCK.
    
         IF i-lin > 62 THEN DO:
            RUN pi-imp-cabec.
            ASSIGN i-lin = 7.
         END.
         PUT tt-ped-venda.nr-pedcli     FORMAT "x(6)"          AT   1
             tt-ped-venda.nr-pedrep     FORMAT "x(12)"         AT   8
             tt-ped-venda.nome-abrev    FORMAT "x(12)"         AT  21
             tt-ped-venda.dt-entrega    FORMAT "99/99/9999"    AT  34
             tt-ped-venda.situacao      FORMAT "x(3)"          AT  45
             tt-ped-venda.qt-pedida     FORMAT ">>,>>>,>>9.99" AT  49
             tt-ped-venda.qt-faturada   FORMAT ">>,>>>,>>9.99" AT  63
             tt-ped-venda.qt-reservada  FORMAT ">>,>>>,>>9.99" AT  77
             tt-ped-venda.qt-cancelada  FORMAT ">>,>>>,>>9.99" AT  91
             tt-ped-venda.qt-aberto     FORMAT ">>,>>>,>>9.99" AT 105
             tt-ped-venda.dt-implant    FORMAT "99/99/9999"    AT 119
             tt-ped-venda.dt-apr-cred   FORMAT "99/99/9999"    AT 130
             tt-ped-venda.observacoes   FORMAT "x(90)"         AT 141.
         ASSIGN i-lin            = i-lin + 1.
         ACCUMULATE tt-ped-venda.qt-pedida    (TOTAL).
         ACCUMULATE tt-ped-venda.qt-faturada  (TOTAL).
         ACCUMULATE tt-ped-venda.qt-reservada (TOTAL).
         ACCUMULATE tt-ped-venda.qt-cancelada (TOTAL).
         ACCUMULATE tt-ped-venda.qt-aberto    (TOTAL).
     END.
     PUT "------------- ------------- ------------- ------------- -------------" AT 49 SKIP.
     PUT "Total..............:" AT 28.
     PUT ACCUM TOTAL tt-ped-venda.qt-pedida    FORMAT ">>,>>>,>>9.99" AT  49.
     PUT ACCUM TOTAL tt-ped-venda.qt-faturada  FORMAT ">>,>>>,>>9.99" AT  63.
     PUT ACCUM TOTAL tt-ped-venda.qt-reservada FORMAT ">>,>>>,>>9.99" AT  77.
     PUT ACCUM TOTAL tt-ped-venda.qt-cancelada FORMAT ">>,>>>,>>9.99" AT  91.
     PUT ACCUM TOTAL tt-ped-venda.qt-aberto    FORMAT ">>,>>>,>>9.99" AT 105.

     IF i-saida = 1 THEN DO:
       PAGE.
       PUT "" AT 1.
     END.
  END.
  IF i-saida = 3 THEN DO.
     RUN EXECUTE IN h-prog(INPUT "notepad.exe",
                           INPUT c-saida).
     DELETE PROCEDURE h-prog.
  END.
  OUTPUT CLOSE.
  */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-monta-planilha C-Win 
PROCEDURE pi-monta-planilha :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    /* Cabeáalho  da Planilha */
/*  ASSIGN c-Lin = c-empresa + "                    " + " PEDIDOS DO PERIODO: "  + STRING(da-dt-entrega-ini, "99/99/9999") + " A " + STRING(da-dt-entrega-fin, "99/99/9999"). 
    DDE SEND i-canal SOURCE c-Lin ITEM "L1C1".
    DDE EXECUTE i-canal COMMAND '[select("L1C1:L1C12")]'.
    /* Fonte, Tamanho, Negrito, It†lico, Sublinhado, Atachado, Cor(0=Autom†tico, 1=Preto, 2=Branco 3=Vermelho...) */
    DDE EXECUTE sys COMMAND "[format.font(~"Verdana~",10,True,False,False,False,3)]".   
     
    ASSIGN c-Lin = c-empresa + "                    " + " PEDIDOS DO PERIODO: "  + STRING(da-dt-entrega-ini, "99/99/9999") + " A " + STRING(da-dt-entrega-fin, "99/99/9999"). */
    

    /* Cabeáalho dos Dados */
/*  DDE SEND i-canal SOURCE "PEDIDO"           ITEM "L3C1".
    DDE SEND i-canal SOURCE "REPRES."          ITEM "L3C2".
    DDE SEND i-canal SOURCE "CLIENTE"          ITEM "L3C3".
    DDE SEND i-canal SOURCE "DT.ENTREGA"       ITEM "L3C4".
    DDE SEND i-canal SOURCE "COND.PAGTO"       ITEM "L3C5".
    DDE SEND i-canal SOURCE "SIT"              ITEM "L3C6".
    DDE SEND i-canal SOURCE "TOTAL PEDIDO"     ITEM "L3C7".
    DDE SEND i-canal SOURCE "TOTAL FATURADO"   ITEM "L3C8".
    DDE SEND i-canal SOURCE "TOTAL RESERVADO"  ITEM "L3C9".
    DDE SEND i-canal SOURCE "TOTAL CANCELADO"  ITEM "L3C10".
    DDE SEND i-canal SOURCE "TOTAL ABERTO"     ITEM "L3C11".
    DDE SEND i-canal SOURCE "DT.IMPLANT"       ITEM "L3C12".
    DDE SEND i-canal SOURCE "DT.APROV"         ITEM "L3C13".

    /* Formataá∆o das Celulas do Cabeáalho de Dados */
    DDE EXECUTE i-canal COMMAND '[select("L3C1:L3C12")]'.
    /* Fonte, Tamanho, Negrito, It†lico, Sublinhado, Atachado, Cor(0=Autom†tico, 1=Preto, 2=Branco 3=Vermelho...) */
    DDE EXECUTE sys COMMAND "[format.font(~"Verdana~",8,True,False,False,False,5)]".
    
    DDE EXECUTE i-canal COMMAND "[select(~"C1~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(7.00)]".
    
    DDE EXECUTE i-canal COMMAND "[select(~"C2~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(12.00)]".
    
    DDE EXECUTE i-canal COMMAND "[select(~"C3~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(12.00)]".

    DDE EXECUTE i-canal COMMAND "[select(~"C4~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(11.00)]".

    DDE EXECUTE i-canal COMMAND "[select(~"C5~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(20.00)]".

    DDE EXECUTE i-canal COMMAND "[select(~"C6~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(4.00)]".

    DDE EXECUTE i-canal COMMAND "[select(~"C7~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(13.00)]". 
    DDE EXECUTE sys     COMMAND "[format.number(~"###.###.##0,00~")]".
    DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]".

    DDE EXECUTE i-canal COMMAND "[select(~"C8~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(13.00)]".
    DDE EXECUTE sys     COMMAND "[format.number(~"###.###.##0,00~")]".
    DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]".

    DDE EXECUTE i-canal COMMAND "[select(~"C9~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(13.00)]".
    DDE EXECUTE sys     COMMAND "[format.number(~"###.###.##0,00~")]".
    DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]".

    DDE EXECUTE i-canal COMMAND "[select(~"C10~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(13.00)]".
    DDE EXECUTE sys     COMMAND "[format.number(~"###.###.##0,00~")]".
    DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]".

    DDE EXECUTE i-canal COMMAND "[select(~"C11~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(13.00)]".
    DDE EXECUTE sys     COMMAND "[format.number(~"###.###.##0,00~")]".
    DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]".
    
    DDE EXECUTE i-canal COMMAND "[select(~"C12~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(11.00)]".

    DDE EXECUTE i-canal COMMAND "[select(~"C13~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(11.00)]".

    /* Montagem das Celulas de Dados */
    ASSIGN i-Lin = 4.
    FOR EACH tt-ped-venda NO-LOCK.

        FIND cond-pagto WHERE
             cond-pagto.cod-cond-pag = tt-ped-venda.cod-cond-pag NO-LOCK NO-ERROR.
        IF AVAIL cond-pagto THEN
           ASSIGN c-desc-cond-pag = cond-pagto.descricao.
        ELSE
           ASSIGN c-desc-cond-pag = 'ESPECIAL'.

        DDE SEND i-canal SOURCE STRING(tt-ped-venda.nr-pedcli)       ITEM "L" + TRIM(STRING(i-Lin)) + "C1". 
        DDE SEND i-canal SOURCE STRING(tt-ped-venda.no-ab-reppri)    ITEM "L" + TRIM(STRING(i-Lin)) + "C2".
        DDE SEND i-canal SOURCE STRING(tt-ped-venda.nome-abrev)      ITEM "L" + TRIM(STRING(i-Lin)) + "C3".
        DDE SEND i-canal SOURCE STRING(tt-ped-venda.dt-entrega)      ITEM "L" + TRIM(STRING(i-Lin)) + "C4".
        DDE SEND i-canal SOURCE STRING(c-desc-cond-pag)              ITEM "L" + TRIM(STRING(i-Lin)) + "C5".
        DDE SEND i-canal SOURCE STRING(tt-ped-venda.situacao)        ITEM "L" + TRIM(STRING(i-Lin)) + "C6".
        IF tt-ped-venda.qt-pedida <> 0 THEN
           DDE SEND i-canal SOURCE STRING(tt-ped-venda.qt-pedida)    ITEM "L" + TRIM(STRING(i-Lin)) + "C7".
        IF tt-ped-venda.qt-faturada <> 0  THEN
           DDE SEND i-canal SOURCE STRING(tt-ped-venda.qt-faturada)  ITEM "L" + TRIM(STRING(i-Lin)) + "C8".
        IF tt-ped-venda.qt-reservada <> 0 THEN
           DDE SEND i-canal SOURCE STRING(tt-ped-venda.qt-reservada) ITEM "L" + TRIM(STRING(i-Lin)) + "C9".
        IF tt-ped-venda.qt-cancelada <> 0 THEN
           DDE SEND i-canal SOURCE STRING(tt-ped-venda.qt-cancelada) ITEM "L" + TRIM(STRING(i-Lin)) + "C10".
        IF tt-ped-venda.qt-aberto <> 0 THEN
           DDE SEND i-canal SOURCE STRING(tt-ped-venda.qt-aberto)    ITEM "L" + TRIM(STRING(i-Lin)) + "C12". 
        IF tt-ped-venda.dt-implant <> ? THEN
           DDE SEND i-canal SOURCE STRING(tt-ped-venda.dt-implant)   ITEM "L" + TRIM(STRING(i-Lin)) + "C13".
        IF tt-ped-venda.dt-apr-cred <> ? THEN
           DDE SEND i-canal SOURCE STRING(tt-ped-venda.dt-apr-cred)  ITEM "L" + TRIM(STRING(i-Lin)) + "C14".

        ASSIGN aux-command = '[select("L' + TRIM(STRING(i-Lin)) + 'C1:L' + TRIM(STRING(i-Lin)) + 'C13")]'.
        DDE EXECUTE i-canal COMMAND aux-command.
        /* Fonte, Tamanho, Negrito, It†lico, Sublinhado, Atachado, Cor(0=Autom†tico, 1=Preto, 2=Branco 3=Vermelho...) */
        DDE EXECUTE sys COMMAND "[format.font(~"Verdana~",8,False,False,False,False,0)]".
        ASSIGN i-lin = i-lin + 1.
        ACCUMULATE tt-ped-venda.qt-pedida    (TOTAL).
        ACCUMULATE tt-ped-venda.qt-faturada  (TOTAL).
        ACCUMULATE tt-ped-venda.qt-reservada (TOTAL).
        ACCUMULATE tt-ped-venda.qt-cancelada (TOTAL).
        ACCUMULATE tt-ped-venda.qt-aberto    (TOTAL).
    END.
    DDE SEND i-canal SOURCE "T O T A L"                                      ITEM "L" + TRIM(STRING(i-Lin)) + "C5".
    DDE SEND i-canal SOURCE STRING((ACCUM TOTAL tt-ped-venda.qt-pedida))     ITEM "L" + TRIM(STRING(i-Lin)) + "C7".
    DDE SEND i-canal SOURCE STRING((ACCUM TOTAL tt-ped-venda.qt-faturada))   ITEM "L" + TRIM(STRING(i-Lin)) + "C8".
    DDE SEND i-canal SOURCE STRING((ACCUM TOTAL tt-ped-venda.qt-reservada))  ITEM "L" + TRIM(STRING(i-Lin)) + "C9".
    DDE SEND i-canal SOURCE STRING((ACCUM TOTAL tt-ped-venda.qt-cancelada))  ITEM "L" + TRIM(STRING(i-Lin)) + "C10".
    DDE SEND i-canal SOURCE STRING((ACCUM TOTAL tt-ped-venda.qt-aberto))     ITEM "L" + TRIM(STRING(i-Lin)) + "C11".
    ASSIGN aux-command = '[select("L' + TRIM(STRING(i-Lin)) + 'C1:L' + TRIM(STRING(i-Lin)) + 'C13")]'.
    DDE EXECUTE i-canal COMMAND aux-command.
    /* Fonte, Tamanho, Negrito, It†lico, Sublinhado, Atachado, Cor(0=Autom†tico, 1=Preto, 2=Branco 3=Vermelho...) */
    DDE EXECUTE sys COMMAND "[format.font(~"Verdana~",8,False,False,False,False,0)]".
    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-popula-browse C-Win 
PROCEDURE pi-popula-browse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
   {utp/ut-liter.i Selecionando_Pedidos *}
   RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

   EMPTY TEMP-TABLE tt-ped-venda.
   
   /* Busca Nome da Empresa */
   FIND FIRST param-global NO-LOCK NO-ERROR.
   FIND FIRST empresa WHERE
              empresa.ep-codigo = param-global.empresa-prin NO-LOCK NO-ERROR. 
   ASSIGN c-empresa = (IF AVAIL empresa THEN empresa.razao-social ELSE "").

   ASSIGN fi-tot-pedida = 0
          fi-tot-fatur  = 0
          fi-tot-aberto = 0
          fi-tot-cancel = 0
          c-situacao = ""
          c-lotes = "".

   IF l-lote-todos = YES THEN
      ASSIGN c-lotes = "pp,pd,rp,rd,sc,ca,".
   ELSE DO:
      ASSIGN c-lotes = c-lotes + IF l-lote-pp = YES THEN "pp," ELSE ",".
      ASSIGN c-lotes = c-lotes + IF l-lote-pd = YES THEN "pd," ELSE ",".
      ASSIGN c-lotes = c-lotes + IF l-lote-rp = YES THEN "rp," ELSE ",".
      ASSIGN c-lotes = c-lotes + IF l-lote-rd = YES THEN "rd," ELSE ",".
      ASSIGN c-lotes = c-lotes + IF l-lote-sc = YES THEN "sc," ELSE ",".
      ASSIGN c-lotes = c-lotes + IF l-lote-ca = YES THEN "ca," ELSE ",".
   END.

   IF l-sit-todas = YES THEN 
      ASSIGN c-situacao = '1,2,3,4,5,6,'.
   ELSE DO:
      IF l-sit-abe = YES THEN ASSIGN c-situacao = '1,'.
      IF l-sit-atp = YES THEN ASSIGN c-situacao = c-situacao + '2,'.
      IF l-sit-att = YES THEN ASSIGN c-situacao = c-situacao + '3,'.
      IF l-sit-pen = YES THEN ASSIGN c-situacao = c-situacao + '4,'.
      IF l-sit-sus = YES THEN ASSIGN c-situacao = c-situacao + '5,'.
      IF l-sit-can = YES THEN ASSIGN c-situacao = c-situacao + '6,'.
   END.
   OVERLAY(c-situacao,LENGTH(c-situacao),1) = ''.

   ASSIGN da-dt-cancela-ini = ?
          da-dt-cancela-fin = ?.
   IF l-sit-can THEN
      ASSIGN da-dt-cancela-ini = da-dt-implant-ini
             da-dt-cancela-fin = da-dt-implant-fin
             da-dt-implant-ini = 01.01.0001
             da-dt-implant-fin = 12.31.9999.

   /* Processa os Pedidos separado por Situaá∆o para gannhar performance */
   IF l-sit-abe OR l-sit-todas THEN RUN pi-separa-pedidos (INPUT 1).
   IF l-sit-atp OR l-sit-todas THEN RUN pi-separa-pedidos (INPUT 2).
   IF l-sit-att OR l-sit-todas THEN RUN pi-separa-pedidos (INPUT 3).
   IF l-sit-pen OR l-sit-todas THEN RUN pi-separa-pedidos (INPUT 4).
   IF l-sit-can                THEN RUN pi-separa-pedidos (INPUT 6).

   ASSIGN fi-tot-aberto = fi-tot-pedida - fi-tot-cancel - fi-tot-fatur.

   RUN pi-finalizar in h-acomp.

   DISP fi-tot-pedida
        fi-tot-fatur
        fi-tot-cancel
        fi-tot-aberto
        WITH FRAME {&FRAME-NAME}.

   {&OPEN-QUERY-br-pedidos}
   APPLY 'value-changed' TO br-pedidos.
   APPLY 'entry' TO br-pedidos.
   RETURN NO-APPLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-separa-itens C-Win 
PROCEDURE pi-separa-itens :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FOR EACH ped-item WHERE
            ped-item.it-codigo >= c-it-codigo-ini AND
            ped-item.it-codigo <= c-it-codigo-fin AND
            ped-item.cod-refer >= c-cod-refer-ini AND 
            ped-item.cod-refer <= c-cod-refer-fin NO-LOCK USE-INDEX ch-correcao,
      FIRST ped-item-ext OF ped-item WHERE
            LOOKUP(SUBSTR(ped-item-ext.lote,1,2),c-lotes) <> 0 NO-LOCK,
      FIRST ped-venda OF ped-item WHERE
            ped-venda.cod-estabel = c-cod-estabel AND
            LOOKUP(STRING(ped-venda.cod-sit-ped),TRIM(c-situacao)) > 0 AND
            ped-venda.nr-pedcli  >= c-nr-pedcli-ini AND
            ped-venda.nr-pedcli  <= c-nr-pedcli-fin AND 
            ped-venda.nome-abrev >= c-nome-abrev-ini AND
            ped-venda.nome-abrev <= c-nome-abrev-fin AND
            ped-venda.dt-implant >= da-dt-implant-ini AND
            ped-venda.dt-implant <= da-dt-implant-fin AND
            ped-venda.no-ab-reppri >= c-no-ab-reppri-ini AND
            ped-venda.no-ab-reppri <= c-no-ab-reppri-fin NO-LOCK.

       RUN pi-ver-digita (INPUT "Pedido_de_Venda",
                          INPUT ped-venda.nr-pedcli).
       IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

       RUN pi-ver-digita (INPUT "Item",
                          INPUT ped-item.it-codigo).
       IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

       RUN pi-ver-digita (INPUT "Referància",
                          INPUT ped-item.cod-refer).
       IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

       RUN pi-ver-digita (INPUT "Cliente",
                          INPUT ped-venda.nome-abrev).
       IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

       RUN pi-acompanhar IN h-acomp (INPUT "Pedido/Item: " + ped-item.nr-pedcli + " " + ped-item.it-codigo).

       IF i-credito = 1 AND
          (ped-venda.cod-sit-aval = 1 OR ped-venda.cod-sit-aval > 3) THEN NEXT.

       IF i-credito = 2 AND
          (ped-venda.cod-sit-aval >= 2 AND ped-venda.cod-sit-aval <= 3) THEN NEXT.

       IF ped-item-ext.corte-comerc < c-corte-comerc-ini OR  
          ped-item-ext.corte-comerc > c-corte-comerc-fin THEN NEXT.
       RUN pi-ver-digita (INPUT "Corte_Comercial",
                          INPUT ped-item-ext.corte-comerc).
       IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

       IF ENTRY(1,c-tp-pedido) <> 'Todos' THEN DO.
          FIND ped-venda-ext WHERE
               ped-venda-ext.cod-estabel = ped-venda.cod-estabel AND    /*   daf   */
               ped-venda-ext.nr-pedido = ped-venda.nr-pedido NO-LOCK NO-ERROR.
          IF NOT AVAIL ped-venda-ext OR
             ped-venda-ext.tp-pedido <> ENTRY(1,c-tp-pedido) THEN NEXT.
       END.

       IF ENTRY(2,c-tp-pedido) <> 'Todos' THEN 
          IF ped-venda.tp-pedido <> ENTRY(2,c-tp-pedido) THEN NEXT.

       FIND item WHERE
            item.it-codigo = ped-item.it-codigo NO-LOCK NO-ERROR.

       FIND item-ext WHERE
            item-ext.it-codigo = item.it-codigo NO-LOCK NO-ERROR.

       IF (c-tp-mercado = "0" AND item.codigo-orig <> 0) OR
          (c-tp-mercado = "1" AND item.codigo-orig <> 1) OR
          (c-tp-mercado = "2" AND item.codigo-orig <> 2) THEN NEXT.

       /*IF c-opc-artigo <> 'A' THEN 
          IF AVAIL item-ext AND
             (item-ext.indigo = YES AND c-opc-artigo <> "I") OR
             (item-ext.indigo = NO  AND c-opc-artigo <> "O") THEN NEXT.*/
       /*
       FIND ref-item-ext WHERE
            ref-item-ext.it-codigo = ped-item.it-codigo AND
            ref-item-ext.cod-refer = ped-item.cod-refer NO-LOCK NO-ERROR.
       IF AVAIL ref-item-ext THEN DO.
          IF ref-item-ext.cod-obsoleto < c-cod-obsoleto-ini OR
             ref-item-ext.cod-obsoleto > c-cod-obsoleto-fin THEN NEXT.

          RUN pi-ver-digita (INPUT "Codigo_Obsoleto",
                             INPUT ref-item-ext.cod-obsoleto).
          IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.
       END.
       
       IF i-tp-acab = 1 AND SUBSTR(ped-item.cod-refer,7,1) <> '0' THEN NEXT.
       IF i-tp-acab = 2 AND SUBSTR(ped-item.cod-refer,7,1) = '0' THEN NEXT.
       */

       FIND tt-ped-venda WHERE
            tt-ped-venda.nr-pedcli = ped-venda.nr-pedcli AND
            tt-ped-venda.nome-abrev = ped-venda.nome-abrev NO-ERROR.
       IF NOT AVAIL tt-ped-venda THEN DO:
          CREATE tt-ped-venda.
          BUFFER-COPY ped-venda TO tt-ped-venda.

          CASE ped-venda.cod-sit-ped.
               WHEN 1 THEN ASSIGN tt-ped-venda.situacao = 'ABE'.
               WHEN 2 THEN ASSIGN tt-ped-venda.situacao = 'ATP'.
               WHEN 3 THEN ASSIGN tt-ped-venda.situacao = 'ATT'.
               WHEN 4 THEN ASSIGN tt-ped-venda.situacao = 'PEN'.
               WHEN 5 THEN ASSIGN tt-ped-venda.situacao = 'SUS'.
               WHEN 6 THEN ASSIGN tt-ped-venda.situacao = 'CAN'.
          END CASE.
       END.
       ASSIGN tt-ped-venda.qt-pedida = tt-ped-venda.qt-pedida + ped-item.qt-pedida
              fi-tot-pedida = fi-tot-pedida + ped-item.qt-pedida.

       IF ped-item.cod-sit-item = 6 THEN
          ASSIGN tt-ped-venda.qt-cancelada = tt-ped-venda.qt-cancelada + ped-item.qt-pedida
                 fi-tot-cancel = fi-tot-cancel + ped-item.qt-pedida.

       FOR EACH ped-item-res OF ped-item WHERE
               /* ped-item-res.cod-estabel = ped-venda.cod-estabel AND */
                ped-item-res.faturado   = NO NO-LOCK.
           ASSIGN tt-ped-venda.qt-reservada = tt-ped-venda.qt-reservada + ped-item-res.qt-pedida.
       END.

       FOR EACH nota-fiscal WHERE
                nota-fiscal.cod-estabel = tt-ped-venda.cod-estabel AND
                nota-fiscal.nome-ab-cli = tt-ped-venda.nome-abrev AND
                nota-fiscal.nr-pedcli   = tt-ped-venda.nr-pedcli AND 
                nota-fiscal.dt-cancela  = ? NO-LOCK.
           FIND it-nota-fisc OF nota-fiscal WHERE
                it-nota-fisc.nr-seq-ped = ped-item.nr-sequencia NO-LOCK NO-ERROR.
           IF AVAIL it-nota-fisc THEN
              ASSIGN tt-ped-venda.qt-faturada = tt-ped-venda.qt-faturada + it-nota-fisc.qt-faturada[1]
                     fi-tot-fatur = fi-tot-fatur + it-nota-fisc.qt-faturada[1].
       END.
       ASSIGN tt-ped-venda.qt-aberto = tt-ped-venda.qt-pedida - tt-ped-venda.qt-cancelada - 
                                       tt-ped-venda.qt-faturada.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-separa-pedidos C-Win 
PROCEDURE pi-separa-pedidos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER p-sit-ped LIKE ped-venda.cod-sit-ped.

    FOR EACH ped-venda WHERE
             ped-venda.cod-sit-ped   = p-sit-ped AND
             ped-venda.cod-estabel   = c-cod-estabel AND
             ped-venda.nr-pedcli    >= c-nr-pedcli-ini AND
             ped-venda.nr-pedcli    <= c-nr-pedcli-fin AND 
             ped-venda.nome-abrev   >= c-nome-abrev-ini AND
             ped-venda.nome-abrev   <= c-nome-abrev-fin AND
             ped-venda.dt-implant   >= da-dt-implant-ini AND
             ped-venda.dt-implant   <= da-dt-implant-fin AND
             ped-venda.dt-cancela   >= da-dt-cancela-ini AND
             ped-venda.dt-cancela   <= da-dt-cancela-fin AND 
             ped-venda.no-ab-reppri >= c-no-ab-reppri-ini AND   
             ped-venda.no-ab-reppri <= c-no-ab-reppri-fin AND 
             ped-venda.estado       >= c-uf-ini AND
             ped-venda.estado       <= c-uf-fin AND 
             ped-venda.nome-transp  >= c-nome-tra-ini AND
             ped-venda.nome-transp  <= c-nome-tra-fin AND
             ped-venda.cidade       >= c-cidade-ini AND
             ped-venda.cidade       <= c-cidade-fin NO-LOCK
             USE-INDEX ch-tabfin,
        EACH ped-venda-ext WHERE
             ped-venda-ext.cod-estabel = ped-venda.cod-estabel AND    
             ped-venda-ext.nr-pedido = ped-venda.nr-pedido AND
             ped-venda-ext.nr-container >= i-nr-container-ini AND
             ped-venda-ext.nr-container <= i-nr-container-fin NO-LOCK.

        RUN pi-ver-digita (INPUT "Pedido_de_Venda",
                           INPUT ped-venda.nr-pedcli).
        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

        RUN pi-ver-digita (INPUT "Representante",
                           INPUT ped-venda.no-ab-reppri).
        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

        RUN pi-ver-digita (INPUT "Cliente",
                           INPUT ped-venda.nome-abrev).
        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

        RUN pi-ver-digita (INPUT "Container",
                           INPUT ped-venda-ext.nr-container).
        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

        RUN pi-acompanhar IN h-acomp (INPUT "Pedido: " + ped-venda.nr-pedcli).

        IF i-situacao = 1 AND ped-venda.completo = NO THEN NEXT.
        IF i-situacao = 2 AND ped-venda.completo = YES THEN NEXT.
        
        IF i-preco = 1 AND
           ped-venda.log-ped-bonif-pendente = YES THEN NEXT.

        IF i-preco = 2 AND ped-venda.cod-sit-preco <> 2 THEN NEXT.
        IF i-preco = 3 AND ped-venda.cod-sit-preco <> 3 THEN NEXT.
        
        IF i-credito = 1 AND
           (ped-venda.cod-sit-aval = 1 OR ped-venda.cod-sit-aval > 3) THEN NEXT.
        IF i-credito = 2 AND
           (ped-venda.cod-sit-aval >= 2 AND ped-venda.cod-sit-aval <= 3) THEN NEXT.

        IF ENTRY(1,c-tp-pedido) <> 'Todos' AND
           ped-venda-ext.tp-pedido <> ENTRY(1,c-tp-pedido) THEN NEXT.

        IF ENTRY(2,c-tp-pedido) <> 'Todos' AND
           ped-venda.tp-pedido <> ENTRY(2,c-tp-pedido) THEN NEXT.
        
        IF i-aprovar = 1 AND ped-venda-ext.l-nao-aprovar = YES THEN NEXT.
        IF i-aprovar = 2 AND ped-venda-ext.l-nao-aprovar = NO THEN NEXT.

        IF i-espera = 1 AND ped-venda-ext.l-em-espera = NO THEN NEXT.
        IF i-espera = 2 AND ped-venda-ext.l-em-espera = YES THEN NEXT.
        
        ASSIGN de-qt-pedida = 0         de-qt-faturada = 0
               de-qt-reservada = 0      de-qt-cancelada = 0      
               de-tot-desc = 0.

        FOR EACH ped-item OF ped-venda WHERE
                 ped-item.it-codigo >= c-it-codigo-ini AND
                 ped-item.it-codigo <= c-it-codigo-fin AND
                 ped-item.cod-refer >= c-cod-refer-ini AND 
                 ped-item.cod-refer <= c-cod-refer-fin NO-LOCK.

            FIND ped-item-ext WHERE
                 ped-item-ext.cod-estabel = ped-venda.cod-estabel AND
                 ped-item-ext.nome-abrev = ped-venda.nome-abrev AND
                 ped-item-ext.nr-pedcli = ped-venda.nr-pedcli AND 
                 ped-item-ext.nr-sequencia = ped-item.nr-sequencia 
                 NO-LOCK NO-ERROR. 

            IF NOT AVAIL ped-item-ext THEN NEXT.

            RUN pi-ver-digita (INPUT "Item",
                               INPUT ped-item.it-codigo).
            IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

            RUN pi-ver-digita (INPUT "Referància",
                               INPUT ped-item.cod-refer).
            IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

            IF l-corte AND ped-item-ext.retirar-corte = NO THEN NEXT.

            FIND item WHERE
                 item.it-codigo = ped-item.it-codigo NO-LOCK NO-ERROR.

            FIND item-ext WHERE
                 item-ext.it-codigo = item.it-codigo NO-LOCK NO-ERROR.

            IF (c-tp-mercado = "0" AND item.codigo-orig <> 0) OR
               (c-tp-mercado = "1" AND item.codigo-orig <> 1) OR
               (c-tp-mercado = "2" AND item.codigo-orig <> 2) THEN NEXT.

            IF ped-item.cod-sit-item = 6 THEN
               ASSIGN de-qt-cancelada = de-qt-cancelada + ped-item.qt-pedida
                      fi-tot-cancel = fi-tot-cancel + ped-item.qt-pedida.

            FOR EACH ped-item-res OF ped-item WHERE
                     ped-item-res.faturado = NO NO-LOCK.
                ASSIGN de-qt-reservada = de-qt-reservada + ped-item-res.qt-pedida.
            END.

            FOR EACH nota-fiscal WHERE
                     nota-fiscal.cod-estabel = ped-venda.cod-estabel AND
                     nota-fiscal.nome-ab-cli = ped-venda.nome-abrev AND
                     nota-fiscal.nr-pedcli   = ped-venda.nr-pedcli AND 
                     nota-fiscal.dt-cancela  = ? NO-LOCK.
                FIND it-nota-fisc OF nota-fiscal WHERE
                     it-nota-fisc.nr-seq-ped = ped-item.nr-sequencia NO-LOCK NO-ERROR.
                IF AVAIL it-nota-fisc THEN
                   ASSIGN de-qt-faturada = de-qt-faturada + it-nota-fisc.qt-faturada[1]
                          fi-tot-fatur = fi-tot-fatur + it-nota-fisc.qt-faturada[1].
            END.

            ASSIGN de-qt-pedida = de-qt-pedida + ped-item.qt-pedida.

            IF ped-item.cod-sit-item <> 6 THEN
               ASSIGN de-tot-desc = de-tot-desc + ped-item.val-desconto-total.
        END.

        IF de-qt-pedida = 0 AND 
           de-qt-cancelada = 0 AND 
           de-qt-reservada = 0 THEN NEXT.

        FIND tt-ped-venda WHERE
             tt-ped-venda.nr-pedcli = ped-venda.nr-pedcli AND
             tt-ped-venda.nome-abrev = ped-venda.nome-abrev NO-LOCK NO-ERROR.
        IF NOT AVAIL tt-ped-venda THEN DO:
           CREATE tt-ped-venda.
           BUFFER-COPY ped-venda TO tt-ped-venda.

           CASE ped-venda.cod-sit-ped.
                WHEN 1 THEN ASSIGN tt-ped-venda.situacao = 'ABE'.
                WHEN 2 THEN ASSIGN tt-ped-venda.situacao = 'ATP'.
                WHEN 3 THEN ASSIGN tt-ped-venda.situacao = 'ATT'.
                WHEN 4 THEN ASSIGN tt-ped-venda.situacao = 'PEN'.
                WHEN 5 THEN ASSIGN tt-ped-venda.situacao = 'SUS'.
                WHEN 6 THEN ASSIGN tt-ped-venda.situacao = 'CAN'.
           END CASE.
        END.
        ASSIGN tt-ped-venda.nr-container = ped-venda-ext.nr-container
               tt-ped-venda.ped_web_id = ped-venda-ext.ped_web_id
               tt-ped-venda.nat-oper = UPPER(tt-ped-venda.nat-oper)
               tt-ped-venda.qt-pedida = tt-ped-venda.qt-pedida + de-qt-pedida
               tt-ped-venda.qt-cancelada = tt-ped-venda.qt-cancelada + de-qt-cancelada
               tt-ped-venda.qt-faturada = tt-ped-venda.qt-faturada + de-qt-faturada
               tt-ped-venda.qt-reservada = tt-ped-venda.qt-reservada + de-qt-reservada
               tt-ped-venda.qt-aberto = tt-ped-venda.qt-pedida - tt-ped-venda.qt-cancelada - 
                                        tt-ped-venda.qt-faturada
               tt-ped-venda.vl-desconto = de-tot-desc.

        FIND pre-fatur OF ped-venda NO-LOCK NO-ERROR.
        IF AVAIL pre-fatur THEN
           ASSIGN tt-ped-venda.nr-embarque = pre-fatur.nr-embarque.

        ASSIGN fi-tot-pedida = fi-tot-pedida + de-qt-pedida.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-ver-digita C-Win 
PROCEDURE pi-ver-digita :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER p-campo AS CHAR.
    DEF INPUT PARAMETER p-valor AS CHAR.

    IF CAN-FIND(FIRST tt-digita WHERE
                      tt-digita.opcao = 'D' AND
                      tt-digita.campo = p-campo) AND
       NOT CAN-FIND(FIRST tt-digita WHERE
                          tt-digita.opcao = 'D' AND
                          tt-digita.campo = p-campo AND
                          tt-digita.valor = p-valor) THEN RETURN 'ADM-ERROR'.
    ELSE
      IF CAN-FIND(FIRST tt-digita WHERE
                        tt-digita.opcao = 'E' AND
                        tt-digita.campo = p-campo AND
                        tt-digita.valor = p-valor) THEN RETURN 'ADM-ERROR'.
      ELSE
         RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-ver-permissao C-Win 
PROCEDURE pi-ver-permissao :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR c-grupos AS CHAR FORMAT "x(20)".
    
    ASSIGN l-ok = NO.
    FIND FIRST espec.param-dis NO-LOCK NO-ERROR.

    CASE SELF:NAME.
        WHEN "bt-inclui" OR WHEN "bt-divide" THEN
           ASSIGN c-grupos = espec.param-dis.grp-inc-ped.
        WHEN "bt-modifica" OR WHEN "bt-divide" THEN
           ASSIGN c-grupos = espec.param-dis.grp-alt-ped.
        WHEN "bt-cancela" THEN
           ASSIGN c-grupos = espec.param-dis.grp-can-ped.
        WHEN "bt-suspende" THEN
           ASSIGN c-grupos = espec.param-dis.grp-sus-ped.
    END CASE.

    FOR EACH usuar_grp_usuar WHERE usuar_grp_usuar.cod_usuario = c-seg-usuario NO-LOCK:
        IF INDEX(c-grupos,usuar_grp_usuar.cod_grp_usuar) <> 0 THEN DO:
           ASSIGN l-ok = YES.
           LEAVE.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

