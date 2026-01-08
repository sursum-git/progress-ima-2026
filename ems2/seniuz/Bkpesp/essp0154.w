&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          ems2med          PROGRESS
*/
&Scoped-define WINDOW-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-livre 
/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i XX9999 9.99.99.999}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEF TEMP-TABLE tt-ped-venda NO-UNDO LIKE ped-venda 
    FIELD visualiza         AS   LOG INIT YES.

DEF TEMP-TABLE tt-itens-ped NO-UNDO LIKE ped-item 
    FIELD qt-reservada     LIKE ped-item.qt-pedida
    FIELD lote             LIKE ped-item-ext.lote
    FIELD corte-comerc     LIKE ped-item-ext.corte-comerc
    FIELD visualiza        AS   LOG INIT YES.

DEF TEMP-TABLE tt-etq-reservadas NO-UNDO LIKE ob-etiqueta
    FIELD nr-pedcli              LIKE ped-item.nr-pedcli
    FIELD nr-seq-ped-item        LIKE ped-item.nr-sequencia
    FIELD tp-acao                AS CHAR.

DEF TEMP-TABLE tt-etq-estoque NO-UNDO LIKE ob-etiqueta
    FIELD tipo-tear           AS CHAR
    INDEX indice1 localizacao ASCENDING num-etiqueta DESCENDING.

DEF TEMP-TABLE tt-ped-item NO-UNDO LIKE ped-item
     FIELD r-rowid AS ROWID.

DEF TEMP-TABLE tt-etq-lidas
    FIELD i-lin        AS INT
    FIELD num-etiqueta LIKE ob-etiqueta.num-etiqueta.

DEF BUFFER b-tt-itens-ped FOR tt-itens-ped.
DEF BUFFER b-ped-item FOR ped-item.
DEF BUFFER b-ped-venda FOR ped-venda.

DEF NEW GLOBAL SHARED VAR gr-ob-etiqueta AS ROWID NO-UNDO.

DEF VAR c-desc-dentro AS CHARACTER  NO-UNDO.
DEF VAR c-desc-reserva AS CHARACTER NO-UNDO.
DEF VAR de-tot-rom AS DECIMAL.
DEF VAR i-row AS INT.
DEF VAR h-acomp AS HANDLE.
DEF VAR c-dia AS CHAR.
DEF VAR da-dt-entrega AS DATE.
DEF VAR c-lotes AS CHAR.
DEF VAR h-query AS HANDLE.  
DEF VAR l-erro  AS LOG.
DEF VAR i-tot-etq AS INT.
DEF VAR c-nr-pedcli         LIKE ped-venda.nr-pedcli.
DEF VAR l-reservar AS LOG.
DEF VAR i-etq-fardo LIKE ob-etiqueta.num-etiqueta.
DEF VAR c-docas AS CHAR.
DEF VAR l-separado AS LOG.

/* Global Variable Definitions ---                                       */
DEF NEW GLOBAL SHARED VAR gr-ped-venda  AS ROWID NO-UNDO.
DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

/* Variavies de ParÉmetros */
DEFINE VAR c-cod-estabel       AS CHAR.
DEFINE VAR c-dt-limite         AS CHAR.
DEFINE VAR c-nr-pedcli-ini     LIKE ped-item-ext.nr-pedcli INIT "".
DEFINE VAR c-nr-pedcli-fin     LIKE ped-item-ext.nr-pedcli INIT "ZZZZZZZZ".  
DEFINE VAR c-nome-abrev-ini    LIKE ped-venda.nome-abrev. 
DEFINE VAR c-nome-abrev-fin    LIKE ped-venda.nome-abrev INIT "ZZZZZZZZZZZZ".
DEFINE VAR c-no-ab-reppri-ini  LIKE ped-venda.no-ab-reppri. 
DEFINE VAR c-no-ab-reppri-fin  LIKE ped-venda.no-ab-reppri INIT 'ZZZZZZZZZZZZ'.
DEFINE VAR c-it-codigo-ini     LIKE ped-item-ext.it-codigo INIT "".                 
DEFINE VAR c-it-codigo-fin     LIKE ped-item-ext.it-codigo INIT "ZZZZZZZZZZZZZZZ".  
DEFINE VAR c-cod-refer-ini     LIKE ped-item-ext.cod-refer.
DEFINE VAR c-cod-refer-fin     LIKE ped-item-ext.cod-refer INIT "ZZZZZZZZZZ".   
DEFINE VAR c-cod-obsoleto-ini  AS CHAR FORMAT "X" INIT '0'.
DEFINE VAR c-cod-obsoleto-fin  AS CHAR FORMAT "X" INIT 'Z'.
DEFINE VAR de-perc-min         AS DEC  INIT -10.
DEFINE VAR de-perc-max         AS DEC  INIT 50.
DEFINE VAR c-opc-artigo        AS CHAR INIT 'A'.
DEFINE VAR l-lote-todos        AS LOG INIT YES.
DEFINE VAR l-lote-pp           AS LOG INIT NO.
DEFINE VAR l-lote-pd           AS LOG INIT NO.
DEFINE VAR l-lote-rp           AS LOG INIT NO.
DEFINE VAR l-lote-rd           AS LOG INIT NO.
DEFINE VAR l-lote-sc           AS LOG INIT NO.
DEFINE VAR l-lote-ca           AS LOG INIT NO.
DEFINE VAR l-ok                AS LOG.

/* ***************************  Definitions  ************************** */
/* INICIO DA DEFINIÄ«O DAS TABELAS TEMPORARIAS; PARA A CHAMADA DO RELATORIO ESPD0002RP.P */
{esinc/espd0002.i}

DEF TEMP-TABLE tt-raw-digita
    FIELD raw-digita      AS RAW.

DEFINE TEMP-TABLE tt-digita NO-UNDO 
       FIELD nr-sequencia     AS   INT FORMAT '>>9'
       FIELD it-codigo        LIKE ped-item.it-codigo
       FIELD cod-refer        LIKE ped-item.cod-refer
       FIELD desc-item        AS   CHAR FORMAT "x(25)"
       FIELD qt-pedida        AS   DEC FORMAT ">>>,>>9.99" 
       FIELD qt-reserva       AS   DEC FORMAT ">>>,>>9.99" 
       FIELD sit-prog         AS   CHAR FORMAT "x(7)"
       INDEX seqped nr-sequencia.

DEFINE VAR raw-param   AS RAW NO-UNDO.
DEFINE VAR rs-execucao AS INTEGER INITIAL 1. 
DEFINE FRAME f-pg-imp rs-execucao AT ROW 5.75 COL 3 NO-LABEL.



/* FIM DAS DEFINIÄÂES DO RELATORIO ESPD0002RP.P */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-livre
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad
&Scoped-define BROWSE-NAME br-etq-estoque

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-etq-estoque tt-etq-reservadas ~
tt-itens-ped item tt-ped-venda

/* Definitions for BROWSE br-etq-estoque                                */
&Scoped-define FIELDS-IN-QUERY-br-etq-estoque tt-etq-estoque.localizacao tt-etq-estoque.num-etiqueta tt-etq-estoque.tipo-tear tt-etq-estoque.quantidade   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-etq-estoque   
&Scoped-define SELF-NAME br-etq-estoque
&Scoped-define OPEN-QUERY-br-etq-estoque RUN pi-soma-est. OPEN QUERY {&SELF-NAME} FOR EACH tt-etq-estoque NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-etq-estoque tt-etq-estoque
&Scoped-define FIRST-TABLE-IN-QUERY-br-etq-estoque tt-etq-estoque


/* Definitions for BROWSE br-etq-reservadas                             */
&Scoped-define FIELDS-IN-QUERY-br-etq-reservadas tt-etq-reservadas.num-etiqueta tt-etq-reservadas.quantidade   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-etq-reservadas   
&Scoped-define SELF-NAME br-etq-reservadas
&Scoped-define OPEN-QUERY-br-etq-reservadas RUN pi-soma-res. OPEN QUERY {&SELF-NAME} FOR EACH tt-etq-reservadas WHERE                                  tt-etq-reservadas.nr-seq-ped-item = tt-itens-ped.nr-sequencia AND                                  tt-etq-reservadas.tp-acao <> 'Del'                                  NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-etq-reservadas tt-etq-reservadas
&Scoped-define FIRST-TABLE-IN-QUERY-br-etq-reservadas tt-etq-reservadas


/* Definitions for BROWSE br-itens                                      */
&Scoped-define FIELDS-IN-QUERY-br-itens tt-itens-ped.nr-sequencia tt-itens-ped.it-codigo item.desc-item tt-itens-ped.cod-refer tt-itens-ped.lote tt-itens-ped.qt-pedida   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-itens   
&Scoped-define SELF-NAME br-itens
&Scoped-define QUERY-STRING-br-itens FOR EACH tt-itens-ped OF tt-ped-venda WHERE                                  tt-itens-ped.visualiza NO-LOCK, ~
                                  FIRST item WHERE                                  item.it-codigo = tt-itens-ped.it-codigo NO-LOCK
&Scoped-define OPEN-QUERY-br-itens OPEN QUERY {&SELF-NAME} FOR EACH tt-itens-ped OF tt-ped-venda WHERE                                  tt-itens-ped.visualiza NO-LOCK, ~
                                  FIRST item WHERE                                  item.it-codigo = tt-itens-ped.it-codigo NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-itens tt-itens-ped item
&Scoped-define FIRST-TABLE-IN-QUERY-br-itens tt-itens-ped
&Scoped-define SECOND-TABLE-IN-QUERY-br-itens item


/* Definitions for BROWSE br-pedidos                                    */
&Scoped-define FIELDS-IN-QUERY-br-pedidos tt-ped-venda.nome-abrev tt-ped-venda.nr-pedcli tt-ped-venda.dt-entrega   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-pedidos   
&Scoped-define SELF-NAME br-pedidos
&Scoped-define QUERY-STRING-br-pedidos FOR EACH tt-ped-venda WHERE                                  tt-ped-venda.visualiza = YES NO-LOCK                                  BY tt-ped-venda.dt-entrega
&Scoped-define OPEN-QUERY-br-pedidos OPEN QUERY {&SELF-NAME} FOR EACH tt-ped-venda WHERE                                  tt-ped-venda.visualiza = YES NO-LOCK                                  BY tt-ped-venda.dt-entrega.
&Scoped-define TABLES-IN-QUERY-br-pedidos tt-ped-venda
&Scoped-define FIRST-TABLE-IN-QUERY-br-pedidos tt-ped-venda


/* Definitions for FRAME f-cad                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-cad ~
    ~{&OPEN-QUERY-br-etq-estoque}~
    ~{&OPEN-QUERY-br-etq-reservadas}~
    ~{&OPEN-QUERY-br-itens}~
    ~{&OPEN-QUERY-br-pedidos}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-10 rt-button RECT-11 rs-reservas ~
bt-param br-pedidos br-itens ed-obs br-etq-estoque br-etq-reservadas ~
bt-leitor bt-coletor bt-add bt-del 
&Scoped-Define DISPLAYED-OBJECTS rs-reservas ed-obs fi-tot-estoque ~
fi-tot-reservado fi-qt-pedida 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-4 bt-fardo bt-log bt-vapara bt-modifica bt-consulta ~
bt-imprime bt-lib-fatur 

/* _UIB-PREPROCESSOR-BLOCK-END */
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
DEFINE BUTTON bt-add 
     IMAGE-UP FILE "adeicon\next-au":U
     IMAGE-INSENSITIVE FILE "adeicon\next-ai":U
     LABEL "" 
     SIZE 5 BY 1.17.

DEFINE BUTTON bt-coletor 
     IMAGE-UP FILE "image/im-calc3.bmp":U
     LABEL "" 
     SIZE 4.86 BY 1.17 TOOLTIP "Envia Etiquetas para Coletor".

DEFINE BUTTON bt-consulta AUTO-GO 
     IMAGE-UP FILE "image/im-det.bmp":U
     LABEL "" 
     SIZE 4.29 BY 1.25 TOOLTIP "Detalhar Pedido"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-del 
     IMAGE-UP FILE "adeicon\prev-au":U
     IMAGE-INSENSITIVE FILE "adeicon\prev-ai":U
     LABEL "" 
     SIZE 5 BY 1.17.

DEFINE BUTTON bt-det 
     IMAGE-UP FILE "image/im-det.bmp":U
     LABEL "" 
     SIZE 4.86 BY 1.17 TOOLTIP "Detalha Etiquetas Reservadas".

DEFINE BUTTON bt-fardo AUTO-GO 
     IMAGE-UP FILE "image/im-res-i.bmp":U NO-FOCUS
     LABEL "" 
     SIZE 4.29 BY 1.25 TOOLTIP "Alteraá‰es Fardos"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-imprime AUTO-GO 
     IMAGE-UP FILE "image/im-prigr.bmp":U
     LABEL "" 
     SIZE 4.29 BY 1.25 TOOLTIP "Imprimir Pedido"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-leitor 
     IMAGE-UP FILE "image/im-rmtct.gif":U
     LABEL "" 
     SIZE 4.86 BY 1.17 TOOLTIP "Separar Etiquetas pelo Leitor".

DEFINE BUTTON bt-lib-fatur 
     IMAGE-UP FILE "image/im-fin.bmp":U
     LABEL "Button 1" 
     SIZE 4.29 BY 1.25 TOOLTIP "Libera Pedido para Faturamento".

DEFINE BUTTON bt-log AUTO-GO 
     IMAGE-UP FILE "image/im-log.bmp":U NO-FOCUS
     LABEL "" 
     SIZE 4.29 BY 1.25 TOOLTIP "Log de Alteraá‰es do Pedido"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-modifica AUTO-GO 
     IMAGE-UP FILE "image/im-mod.bmp":U
     LABEL "" 
     SIZE 4.29 BY 1.25 TOOLTIP "Modificar Pedido"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-ok AUTO-GO 
     IMAGE-UP FILE "image/im-cq.bmp":U
     LABEL "OK" 
     SIZE 4 BY 1.21 TOOLTIP "Grava Reserva para o Item".

DEFINE BUTTON bt-param 
     IMAGE-UP FILE "image/im-fil.bmp":U
     LABEL "Button 3" 
     SIZE 4.29 BY 1.21 TOOLTIP "ParÉmetros".

DEFINE BUTTON bt-vapara AUTO-GO 
     IMAGE-UP FILE "image/im-vapra.bmp":U
     LABEL "" 
     SIZE 4.29 BY 1.25 TOOLTIP "Posicionar no Pedido"
     BGCOLOR 8 FONT 10.

DEFINE VARIABLE ed-obs AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 37 BY 6.5
     FGCOLOR 12  NO-UNDO.

DEFINE VARIABLE fi-qt-pedida AS DECIMAL FORMAT ">>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88
     FGCOLOR 12 FONT 6 NO-UNDO.

DEFINE VARIABLE fi-tot-estoque AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12.86 BY .79
     FGCOLOR 9 FONT 6 NO-UNDO.

DEFINE VARIABLE fi-tot-reservado AS DECIMAL FORMAT ">>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88
     FGCOLOR 2 FONT 6 NO-UNDO.

DEFINE VARIABLE rs-reservas AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Pedidos Reservados", 1,
"Pedidos Sem Reservas", 2,
"Todos os Pedidos", 3
     SIZE 70.29 BY 1.17
     FGCOLOR 9 FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 37 BY 1.63
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 68 BY 1.63
     BGCOLOR 8 .

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 107 BY 1.46
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-etq-estoque FOR 
      tt-etq-estoque SCROLLING.

DEFINE QUERY br-etq-reservadas FOR 
      tt-etq-reservadas SCROLLING.

DEFINE QUERY br-itens FOR 
      tt-itens-ped, 
      item SCROLLING.

DEFINE QUERY br-pedidos FOR 
      tt-ped-venda SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-etq-estoque
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-etq-estoque w-livre _FREEFORM
  QUERY br-etq-estoque NO-LOCK DISPLAY
      tt-etq-estoque.localizacao  FORMAT "999/999":U   COLUMN-LABEL "Localiz"  WIDTH 6.5
      tt-etq-estoque.num-etiqueta FORMAT "999999999":U COLUMN-LABEL "Etiqueta" WIDTH 10
      tt-etq-estoque.tipo-tear                         COLUMN-LABEL "Tecelagem"
      tt-etq-estoque.quantidade   FORMAT ">>9.99":U    COLUMN-LABEL "Qtde (m)"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 39 BY 6.5
         FONT 1
         TITLE "Peáas Dispon°veis".

DEFINE BROWSE br-etq-reservadas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-etq-reservadas w-livre _FREEFORM
  QUERY br-etq-reservadas NO-LOCK DISPLAY
      tt-etq-reservadas.num-etiqueta FORMAT "999999999":U COLUMN-LABEL "Etiqueta"
      tt-etq-reservadas.quantidade FORMAT ">>9.99":U COLUMN-LABEL "Qtde (m)" WIDTH 10
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 22 BY 6.5
         FONT 1
         TITLE "Peáas Reservadas".

DEFINE BROWSE br-itens
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-itens w-livre _FREEFORM
  QUERY br-itens NO-LOCK DISPLAY
      tt-itens-ped.nr-sequencia                WIDTH 3
      tt-itens-ped.it-codigo FORMAT "X(8)":U   WIDTH 8
      item.desc-item        FORMAT "x(30)"    WIDTH 30
      tt-itens-ped.cod-refer FORMAT "X(8)":U   WIDTH 8
      tt-itens-ped.lote      FORMAT "X(2)":U   WIDTH 3
      tt-itens-ped.qt-pedida                   WIDTH 9
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 68 BY 8.5
         FONT 1
         TITLE "Itens do Pedido".

DEFINE BROWSE br-pedidos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-pedidos w-livre _FREEFORM
  QUERY br-pedidos NO-LOCK DISPLAY
      tt-ped-venda.nome-abrev FORMAT "x(12)":U       WIDTH 13.5
      tt-ped-venda.nr-pedcli  FORMAT "x(12)":U       WIDTH 9
      tt-ped-venda.dt-entrega FORMAT "99/99/9999":U  WIDTH 10
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 37 BY 8.5
         FONT 6
         TITLE "Pedidos Dispon°veis".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     bt-fardo AT ROW 18.33 COL 29.29 WIDGET-ID 10
     bt-log AT ROW 18.33 COL 20.72
     bt-ok AT ROW 1.13 COL 82.29
     rs-reservas AT ROW 1.17 COL 1.72 NO-LABEL
     bt-param AT ROW 1.17 COL 77.57
     br-pedidos AT ROW 2.75 COL 2
     br-itens AT ROW 2.75 COL 40
     ed-obs AT ROW 11.5 COL 2 NO-LABEL WIDGET-ID 8
     br-etq-estoque AT ROW 11.5 COL 40
     br-etq-reservadas AT ROW 11.5 COL 86
     bt-det AT ROW 11.54 COL 80
     bt-leitor AT ROW 12.96 COL 80 WIDGET-ID 4
     bt-coletor AT ROW 14.13 COL 80 WIDGET-ID 6
     bt-add AT ROW 15.58 COL 80
     bt-del AT ROW 16.75 COL 80
     bt-vapara AT ROW 18.33 COL 2.72
     bt-modifica AT ROW 18.33 COL 7.14
     bt-consulta AT ROW 18.33 COL 11.57
     bt-imprime AT ROW 18.33 COL 16.14
     bt-lib-fatur AT ROW 18.33 COL 33.86 WIDGET-ID 12
     fi-tot-estoque AT ROW 18.5 COL 61.14 COLON-ALIGNED NO-LABEL
     fi-tot-reservado AT ROW 18.5 COL 87.14 COLON-ALIGNED NO-LABEL
     fi-qt-pedida AT ROW 18.5 COL 97 COLON-ALIGNED NO-LABEL
     "/" VIEW-AS TEXT
          SIZE 1 BY .54 AT ROW 18.67 COL 97.57
          BGCOLOR 8 FONT 10
     "Reservado:" VIEW-AS TEXT
          SIZE 8.29 BY .75 AT ROW 18.58 COL 80.57
          BGCOLOR 8 
     "Total Dispon°vel:" VIEW-AS TEXT
          SIZE 12 BY .75 AT ROW 18.54 COL 50.72
          BGCOLOR 8 
     RECT-10 AT ROW 18.13 COL 2
     rt-button AT ROW 1 COL 1
     RECT-11 AT ROW 18.13 COL 40 WIDGET-ID 14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 107.72 BY 18.92
         FONT 1.


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
         TITLE              = "Manutená∆o nas Reservas do Pedido"
         COLUMN             = 23.29
         ROW                = 7
         HEIGHT             = 18.92
         WIDTH              = 107.72
         MAX-HEIGHT         = 28.13
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 28.13
         VIRTUAL-WIDTH      = 146.29
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
/* BROWSE-TAB br-pedidos bt-param f-cad */
/* BROWSE-TAB br-itens br-pedidos f-cad */
/* BROWSE-TAB br-etq-estoque ed-obs f-cad */
/* BROWSE-TAB br-etq-reservadas br-etq-estoque f-cad */
/* SETTINGS FOR BUTTON bt-consulta IN FRAME f-cad
   NO-ENABLE 4                                                          */
/* SETTINGS FOR BUTTON bt-det IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-fardo IN FRAME f-cad
   NO-ENABLE 4                                                          */
/* SETTINGS FOR BUTTON bt-imprime IN FRAME f-cad
   NO-ENABLE 4                                                          */
/* SETTINGS FOR BUTTON bt-lib-fatur IN FRAME f-cad
   NO-ENABLE 4                                                          */
/* SETTINGS FOR BUTTON bt-log IN FRAME f-cad
   NO-ENABLE 4                                                          */
/* SETTINGS FOR BUTTON bt-modifica IN FRAME f-cad
   NO-ENABLE 4                                                          */
/* SETTINGS FOR BUTTON bt-ok IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-vapara IN FRAME f-cad
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-qt-pedida IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-estoque IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-reservado IN FRAME f-cad
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-etq-estoque
/* Query rebuild information for BROWSE br-etq-estoque
     _START_FREEFORM
RUN pi-soma-est.
OPEN QUERY {&SELF-NAME} FOR EACH tt-etq-estoque NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "espec.ob-etiqueta.situacao = 3 AND
espec.ob-etiqueta.it-codigo = tt-itens.it-codigo AND
espec.ob-etiqueta.cod-refer = tt-itens.cod-refer AND
espec.ob-etiqueta.nr-lote = tt-positivo.lote AND
espec.ob-etiqueta.corte-comerc = tt-positivo.corte-comerc"
     _Query            is OPENED
*/  /* BROWSE br-etq-estoque */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-etq-reservadas
/* Query rebuild information for BROWSE br-etq-reservadas
     _START_FREEFORM
RUN pi-soma-res.
OPEN QUERY {&SELF-NAME} FOR EACH tt-etq-reservadas WHERE
                                 tt-etq-reservadas.nr-seq-ped-item = tt-itens-ped.nr-sequencia AND
                                 tt-etq-reservadas.tp-acao <> 'Del'
                                 NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-etq-reservadas */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-itens
/* Query rebuild information for BROWSE br-itens
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-itens-ped OF tt-ped-venda WHERE
                                 tt-itens-ped.visualiza NO-LOCK,
                           FIRST item WHERE
                                 item.it-codigo = tt-itens-ped.it-codigo NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-itens */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-pedidos
/* Query rebuild information for BROWSE br-pedidos
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-ped-venda WHERE
                                 tt-ped-venda.visualiza = YES NO-LOCK
                                 BY tt-ped-venda.dt-entrega.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-pedidos */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON END-ERROR OF w-livre /* Manutená∆o nas Reservas do Pedido */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON WINDOW-CLOSE OF w-livre /* Manutená∆o nas Reservas do Pedido */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-etq-estoque
&Scoped-define SELF-NAME br-etq-estoque
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-etq-estoque w-livre
ON VALUE-CHANGED OF br-etq-estoque IN FRAME f-cad /* Peáas Dispon°veis */
DO:
   ASSIGN bt-add:SENSITIVE IN FRAME {&FRAME-NAME} = NO
          bt-del:SENSITIVE IN FRAME {&FRAME-NAME} = NO
          bt-det:SENSITIVE IN FRAME {&FRAME-NAME} = NO
          bt-leitor:SENSITIVE IN FRAME {&FRAME-NAME} = NO
          bt-coletor:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
   IF AVAIL tt-etq-estoque THEN DO.
      ASSIGN bt-add:SENSITIVE IN FRAME {&FRAME-NAME} = YES
             bt-leitor:SENSITIVE IN FRAME {&FRAME-NAME} = YES
             bt-coletor:SENSITIVE IN FRAME {&FRAME-NAME} = YES
             bt-det:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
   END.

   ASSIGN bt-del:SENSITIVE IN FRAME {&FRAME-NAME} = CAN-FIND(FIRST tt-etq-reservadas).

   FIND ob-etiqueta WHERE
        ob-etiqueta.cod-estabel = tt-etq-estoque.cod-estabel AND
        ob-etiqueta.num-etiqueta = tt-etq-estoque.num-etiqueta NO-LOCK NO-ERROR.
   ASSIGN gr-ob-etiqueta = ROWID(ob-etiqueta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-etq-reservadas
&Scoped-define SELF-NAME br-etq-reservadas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-etq-reservadas w-livre
ON ENTRY OF br-etq-reservadas IN FRAME f-cad /* Peáas Reservadas */
DO:
   APPLY 'value-changed' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-etq-reservadas w-livre
ON LEAVE OF br-etq-reservadas IN FRAME f-cad /* Peáas Reservadas */
DO:
   ASSIGN bt-det:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-etq-reservadas w-livre
ON MOUSE-SELECT-DBLCLICK OF br-etq-reservadas IN FRAME f-cad /* Peáas Reservadas */
DO:
   ASSIGN gr-ob-etiqueta = ROWID(tt-etq-reservadas).
   APPLY 'choose' TO bt-det.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-etq-reservadas w-livre
ON VALUE-CHANGED OF br-etq-reservadas IN FRAME f-cad /* Peáas Reservadas */
DO:
  ASSIGN bt-det:SENSITIVE IN FRAME {&FRAME-NAME} = NO
         bt-del:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
  IF AVAIL tt-etq-reservadas THEN DO.
     IF br-etq-reservadas:NUM-SELECTED-ROWS = 0 THEN
        br-etq-reservadas:SELECT-ROW(1).

     ASSIGN bt-det:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            bt-add:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            bt-del:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
  END.

  FIND ob-etiqueta WHERE
       ob-etiqueta.cod-estabel = tt-etq-reservadas.cod-estabel AND
       ob-etiqueta.num-etiqueta = tt-etq-reservadas.num-etiqueta NO-LOCK NO-ERROR.
  ASSIGN gr-ob-etiqueta = ROWID(ob-etiqueta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-itens
&Scoped-define SELF-NAME br-itens
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-itens w-livre
ON VALUE-CHANGED OF br-itens IN FRAME f-cad /* Itens do Pedido */
DO:
    EMPTY TEMP-TABLE tt-etq-estoque.

    FOR EACH ob-etiqueta WHERE
             ob-etiqueta.cod-estabel = tt-ped-venda.cod-estabel AND
             ob-etiqueta.situacao = 3 AND
             ob-etiqueta.it-codigo = tt-itens-ped.it-codigo AND
             ob-etiqueta.cod-refer = tt-itens-ped.cod-refer  NO-LOCK.

        IF ob-etiqueta.localizacao <> '' THEN DO.
           FIND ob-localiz WHERE
                ob-localiz.cod-localiz = ob-etiqueta.localizacao NO-LOCK NO-ERROR.
           IF AVAIL ob-localiz THEN DO.
              IF (ob-localiz.tipo = 2 OR /* Amostra */ 
                  ob-localiz.tipo = 4 OR /* Desenho Exclusivo */ 
                  ob-localiz.tipo = 7)   /* Bloqueado*/ 
                  THEN NEXT.
           END.
        END.

        FIND tt-etq-reservadas WHERE
             tt-etq-reservadas.num-etiqueta = ob-etiqueta.num-etiqueta NO-ERROR.
        IF AVAIL tt-etq-reservadas THEN NEXT.

        CREATE tt-etq-estoque.
        BUFFER-COPY ob-etiqueta TO tt-etq-estoque.

        FIND item-ext WHERE
             item-ext.it-codigo = ob-etiqueta.it-codigo NO-LOCK NO-ERROR.

    END.

    FOR EACH ped-item-rom WHERE
             ped-item-rom.nome-abrev = tt-itens-ped.nome-abrev AND
             ped-item-rom.nr-pedcli = tt-itens-ped.nr-pedcli AND
             ped-item-rom.nr-sequencia = tt-itens-ped.nr-sequencia
             NO-LOCK.

        FIND ob-etiqueta WHERE
             ob-etiqueta.cod-estabel = ped-item-rom.cod-estabel AND
             ob-etiqueta.num-etiqueta = ped-item-rom.num-etiqueta
             NO-LOCK NO-ERROR. 

        IF NOT AVAIL ob-etiqueta THEN DO.
           MESSAGE 'Erro CR÷TICO, a Etiqueta ' ped-item-rom.num-etiqueta SKIP
                   ' foi Romaneada e eliminada do Sistema..'
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
           NEXT.
        END.

        FIND tt-etq-reservadas WHERE
             tt-etq-reservadas.nr-pedcli = tt-itens-ped.nr-pedcli AND
             tt-etq-reservadas.nr-seq-ped-item = tt-itens-ped.nr-sequencia AND
             tt-etq-reservadas.num-etiqueta = ob-etiqueta.num-etiqueta
             NO-LOCK NO-ERROR.

        IF NOT AVAIL tt-etq-reservadas THEN DO.
           CREATE tt-etq-reservadas.
           BUFFER-COPY ob-etiqueta TO tt-etq-reservadas
                  ASSIGN tt-etq-reservadas.nr-pedcli = tt-itens-ped.nr-pedcli
                         tt-etq-reservadas.nr-seq-ped-item = tt-itens-ped.nr-sequencia.
        END.
    END.

    {&OPEN-QUERY-br-etq-estoque}
    {&OPEN-QUERY-br-etq-reservadas}

    ASSIGN bt-ok:SENSITIVE IN FRAME {&FRAME-NAME} = CAN-FIND(FIRST tt-etq-reservadas).

    IF AVAIL tt-itens-ped THEN DO.
       ASSIGN fi-qt-pedida = tt-itens-ped.qt-pedida
              fi-tot-reservado = tt-itens-ped.qt-reservada.
       DISP fi-qt-pedida 
            fi-tot-reservado 
            WITH FRAME {&FRAME-NAME}.
    END.

    APPLY 'value-changed' TO br-etq-estoque.
    APPLY 'value-changed' TO br-etq-reservadas.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-pedidos
&Scoped-define SELF-NAME br-pedidos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-pedidos w-livre
ON VALUE-CHANGED OF br-pedidos IN FRAME f-cad /* Pedidos Dispon°veis */
DO:
   EMPTY TEMP-TABLE tt-etq-reservadas.
   FOR EACH tt-itens-ped.
       ASSIGN tt-itens-ped.qt-reservada = 0.
   END.

   IF AVAIL tt-ped-venda THEN DO.
      ASSIGN c-nr-pedcli = tt-ped-venda.nr-pedcli.
      ENABLE {&list-4} WITH FRAME {&FRAME-NAME}.

      FIND ped-venda-ext WHERE
           ped-venda-ext.cod-estabel = tt-ped-venda.cod-estabel AND  /* daf */
           ped-venda-ext.nr-pedido = tt-ped-venda.nr-pedido 
           NO-LOCK NO-ERROR.
   END.

   IF AVAIL tt-ped-venda THEN
      ASSIGN ed-obs:SCREEN-VALUE = UPPER(tt-ped-venda.observ).

   ASSIGN bt-fardo:SENSITIVE = rs-reservas = 1
          bt-lib-fatur:SENSITIVE = rs-reservas = 1.

   {&OPEN-QUERY-br-itens}
   APPLY 'value-changed' TO br-itens.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-add w-livre
ON CHOOSE OF bt-add IN FRAME f-cad
DO:
   FIND ped-item WHERE
        ped-item.nome-abrev   = tt-itens-ped.nome-abrev   AND
        ped-item.nr-pedcli    = tt-itens-ped.nr-pedcli    AND
        ped-item.nr-sequencia = tt-itens-ped.nr-sequencia 
        NO-LOCK NO-ERROR.

   IF LOOKUP(STRING(ped-item.cod-sit-item),"1,2,5") = 0 THEN DO.
      MESSAGE 'A T E N Ä « O !!!!' SKIP(1)
              'Sequencia j† foi Faturada, Reserva n∆o poder† ser Eliminada...' SKIP
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
   END.

   DO i-row = 1 TO br-etq-estoque:NUM-SELECTED-ROWS:
      IF br-etq-estoque:FETCH-SELECTED-ROW(i-row) THEN DO.
         FIND tt-etq-reservadas WHERE
              tt-etq-reservadas.num-etiqueta = tt-etq-estoque.num-etiqueta NO-ERROR.
         IF NOT AVAIL tt-etq-reservadas THEN DO.
            CREATE tt-etq-reservadas.
            ASSIGN tt-etq-reservadas.cod-estabel = tt-etq-estoque.cod-estabel
                   tt-etq-reservadas.nr-pedcli = tt-itens-ped.nr-pedcli
                   tt-etq-reservadas.nr-seq-ped-item = tt-itens-ped.nr-sequencia
                   tt-etq-reservadas.num-etiqueta = tt-etq-estoque.num-etiqueta
                   tt-etq-reservadas.quantidade = tt-etq-estoque.quantidade.
         END.
         ASSIGN tt-etq-reservadas.tp-acao = 'Inc'.

         DELETE tt-etq-estoque.
      END.
      ASSIGN bt-ok:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
   END.

   {&OPEN-QUERY-br-etq-estoque}
   {&OPEN-QUERY-br-etq-reservadas}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-coletor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-coletor w-livre
ON CHOOSE OF bt-coletor IN FRAME f-cad
DO:
  MESSAGE "Deseja Receber Dados do Coletor?" 
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE l-receber AS LOG.


   IF l-receber THEN DO.
      INPUT FROM SEARCH("Coletor\Receber\sep.txt").
      REPEAT.


      END.
      INPUT CLOSE.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-consulta w-livre
ON CHOOSE OF bt-consulta IN FRAME f-cad
DO:
   FIND ped-venda WHERE
        ped-venda.nr-pedcli = tt-ped-venda.nr-pedcli AND
        ped-venda.nome-abrev = tt-ped-venda.nome-abrev NO-LOCK NO-ERROR.
   ASSIGN gr-ped-venda = ROWID(ped-venda).

   ASSIGN w-livre:SENSITIVE = NO.
   RUN esp\espd4000.w (INPUT "Consultar").
   ASSIGN w-livre:SENSITIVE = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-del
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-del w-livre
ON CHOOSE OF bt-del IN FRAME f-cad
DO:
   FIND ped-item WHERE
        ped-item.nome-abrev   = tt-itens-ped.nome-abrev   AND
        ped-item.nr-pedcli    = tt-itens-ped.nr-pedcli    AND
        ped-item.nr-sequencia = tt-itens-ped.nr-sequencia 
        NO-LOCK NO-ERROR.

   IF LOOKUP(STRING(ped-item.cod-sit-item),"1,2,5") = 0 THEN DO.
      MESSAGE 'A T E N Ä « O !!!!' SKIP(1)
              'Sequencia j† foi Faturada, Reserva n∆o poder† sofrer Alteraá‰es...' SKIP
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
   END.

   DO i-row = 1 TO br-etq-reservadas:NUM-SELECTED-ROWS.
      IF br-etq-reservadas:FETCH-SELECTED-ROW(i-row) THEN DO.
         ASSIGN tt-etq-reservadas.tp-acao = 'Del'.

         FIND ob-etiqueta WHERE
              ob-etiqueta.cod-estabel = tt-etq-reservadas.cod-estabel AND
              ob-etiqueta.num-etiqueta = tt-etq-reservadas.num-etiqueta
              NO-LOCK NO-ERROR.

         /*
         IF ob-etiqueta.localizacao = '' THEN NEXT.

         IF ob-etiqueta.nr-lote <> 'CA' AND 
            (ob-etiqueta.localizacao BEGINS '6' OR
             ob-etiqueta.localizacao BEGINS '7')
            THEN NEXT.
         */
         CREATE tt-etq-estoque.
         BUFFER-COPY ob-etiqueta TO tt-etq-estoque.

         FIND item-ext WHERE
              item-ext.it-codigo = ob-etiqueta.it-codigo NO-LOCK NO-ERROR.

      END.
   END.
   ASSIGN bt-ok:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

   {&OPEN-QUERY-br-etq-estoque}
   {&OPEN-QUERY-br-etq-reservadas}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-det
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-det w-livre
ON CHOOSE OF bt-det IN FRAME f-cad
DO:
   RUN esp/essp0146.p. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-fardo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-fardo w-livre
ON CHOOSE OF bt-fardo IN FRAME f-cad
DO:
   ASSIGN l-separado = YES.
   FOR EACH ped-item OF tt-ped-venda WHERE 
            ped-item.cod-sit-item = 1 NO-LOCK.
       FIND ped-item-res OF ped-item NO-LOCK NO-ERROR.

       IF (NOT AVAIL ped-item-res) OR
          (AVAIL ped-item-res AND ped-item-res.qt-pedida <> ped-item.qt-pedida) THEN
          ASSIGN l-separado = NO.
   END.

   
   IF l-separado = NO THEN DO.
      MESSAGE 'Separaá∆o INCOMPLETA, fardos N«O poder∆o ser Embalados...'
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
   END.
   

   /*
   Retirado para Resolver o Problema de liberar o Pedido sem informar os Fardos
   
   FIND ped-venda-ext WHERE
        ped-venda-ext.cod-estabel = tt-ped-venda.cod-estabel AND
        ped-venda-ext.nr-pedido = tt-ped-venda.nr-pedido NO-LOCK NO-ERROR.

   IF ped-venda-ext.qt-fardos = 0 THEN DO.
      FIND CURRENT ped-venda-ext SHARE-LOCK NO-ERROR.
      ASSIGN i-tot-etq = 0.
      FOR EACH ped-item-rom WHERE
               ped-item-rom.nome-abrev = tt-ped-venda.nome-abrev AND
               ped-item-rom.nr-pedcli = tt-ped-venda.nr-pedcli 
               NO-LOCK.
          ASSIGN i-tot-etq = i-tot-etq + 1.
      END.
      ASSIGN ped-venda-ext.qt-fardos = i-tot-etq.
      FIND CURRENT ped-venda-ext NO-LOCK NO-ERROR.
   END.
   */

   RUN esp/essp0154d.p (INPUT tt-ped-venda.nr-pedcli).

   IF ped-venda-ext.qt-fardos <> 0 THEN
      APPLY 'CHOOSE' TO bt-lib-fatur.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-imprime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-imprime w-livre
ON CHOOSE OF bt-imprime IN FRAME f-cad
DO:
  CREATE tt-param.
  ASSIGN tt-param.usuario          = c-seg-usuario
         tt-param.data-exec        = TODAY
         tt-param.hora-exec        = TIME
         tt-param.destino          = 3
         tt-param.classifica       = 1
         tt-param.arquivo          = SESSION:TEMP-DIRECTORY + c-programa-mg97 + ".tmp"
         tt-param.estabel-ini      = tt-ped-venda.cod-estabel
         tt-param.estabel-fin      = tt-ped-venda.cod-estabel
         tt-param.pedido-ini       = INT(tt-ped-venda.nr-pedcli)
         tt-param.pedido-fin       = INT(tt-ped-venda.nr-pedcli)
         tt-param.dt-emissao-ini   = 01.01.0001     
         tt-param.dt-emissao-fin   = 12.31.9999
         tt-param.dt-entrega-ini   = 01.01.0001      
         tt-param.dt-entrega-fin   = 12.31.9999
         tt-param.cod-emit-ini     = 0
         tt-param.cod-emit-fin     = 999999
         tt-param.no-ab-reppri-ini = ''   
         tt-param.no-ab-reppri-fin = 'ZZZZZZZZZZZZZZ'   
         tt-param.nome-transp-ini  = ''                  
         tt-param.nome-transp-fin  = 'ZZZZZZZZZZZZZZ'    
         tt-param.corte-comerc-ini = ''
         tt-param.corte-comerc-fin = 'ZZZ'
         tt-param.so-indigo        = NO    
         tt-param.exc-indigo       = NO
         tt-param.it-codigo        = ''
         tt-param.sit-total        = NO
         tt-param.sit-aberto       = YES   
         tt-param.sit-parcial      = YES    
         tt-param.sit-pendentes    = YES    
         tt-param.sit-suspensos    = YES    
         tt-param.sit-cancelados   = NO
         tt-param.cond-credito     = "T"   
         tt-param.cond-pagto       = "T"    
         tt-param.mercado          = "A"    
         tt-param.tp-pedido        = ""    
         tt-param.aceita-parc      = "T"    
         tt-param.qtd-minima       = 0    
         tt-param.perc-minres      = 0    
         tt-param.min-it-ares      = 0    
         tt-param.max-it-ares      = 9999    
         tt-param.it-reservados    = YES.

  SESSION:SET-WAIT-STATE("general":U).

  RUN pi-carrega-dados.
  {include/i-rprun.i esrp/espd0002rp.p}


  IF tt-param.destino = 3 THEN DO.
     RUN utp/ut-utils.p PERSISTENT SET h-prog.
     RUN EXECUTE IN h-prog(INPUT "notepad.exe", 
                           INPUT tt-param.arquivo).
     DELETE PROCEDURE h-prog.
  END.

  SESSION:SET-WAIT-STATE("":U).



END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-leitor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-leitor w-livre
ON CHOOSE OF bt-leitor IN FRAME f-cad
DO:
   RUN esp/essp0154c.w (OUTPUT TABLE tt-etq-lidas).

   FIND ped-item WHERE
        ped-item.nome-abrev   = tt-itens-ped.nome-abrev   AND
        ped-item.nr-pedcli    = tt-itens-ped.nr-pedcli    AND
        ped-item.nr-sequencia = tt-itens-ped.nr-sequencia 
        NO-LOCK NO-ERROR.

   IF LOOKUP(STRING(ped-item.cod-sit-item),"1,2,5") = 0 THEN DO.
      MESSAGE 'A T E N Ä « O !!!!' SKIP(1)
              'Sequencia j† foi Faturada, Reserva n∆o poder† ser Eliminada...' SKIP
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
   END.

   FOR EACH tt-etq-lidas.
       FIND ob-etiqueta WHERE 
            ob-etiqueta.cod-estabel = c-cod-estabel AND
            ob-etiqueta.num-etiqueta = tt-etq-lidas.num-etiqueta
            NO-LOCK NO-ERROR.

       FIND b-tt-itens-ped WHERE
            b-tt-itens-ped.it-codigo = ob-etiqueta.it-codigo AND
            b-tt-itens-ped.cod-refer = ob-etiqueta.cod-refer
            NO-LOCK NO-ERROR.
       IF NOT AVAIL b-tt-itens-ped THEN DO.
          MESSAGE 'Etiqueta ' ob-etiqueta.num-etiqueta SKIP
                  'Existem Itens/Ref Repetidos...'
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          NEXT.
       END.

       IF AVAIL ob-etiqueta THEN DO.
          FIND tt-etq-reservadas WHERE
               tt-etq-reservadas.nr-pedcli = b-tt-itens-ped.nr-pedcli AND
               tt-etq-reservadas.nr-seq-ped-item = b-tt-itens-ped.nr-sequencia AND
               tt-etq-reservadas.num-etiqueta = ob-etiqueta.num-etiqueta NO-ERROR.
          IF NOT AVAIL tt-etq-reservadas THEN DO.
             CREATE tt-etq-reservadas.
             ASSIGN tt-etq-reservadas.cod-estabel = tt-etq-estoque.cod-estabel
                    tt-etq-reservadas.nr-pedcli = b-tt-itens-ped.nr-pedcli
                    tt-etq-reservadas.nr-seq-ped-item = b-tt-itens-ped.nr-sequencia
                    tt-etq-reservadas.num-etiqueta = ob-etiqueta.num-etiqueta
                    tt-etq-reservadas.quantidade = ob-etiqueta.quantidade.
          END.
          ASSIGN tt-etq-reservadas.tp-acao = 'Inc'.

          ASSIGN bt-ok:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
       END.
   END.
   {&OPEN-QUERY-br-etq-estoque}
   {&OPEN-QUERY-br-etq-reservadas}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-lib-fatur
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-lib-fatur w-livre
ON CHOOSE OF bt-lib-fatur IN FRAME f-cad /* Button 1 */
DO:
    ASSIGN l-separado = YES.
    FOR EACH ped-item OF tt-ped-venda WHERE 
             ped-item.cod-sit-item = 1 NO-LOCK.
        FIND ped-item-res OF ped-item NO-LOCK NO-ERROR.

        IF (NOT AVAIL ped-item-res) OR
           (AVAIL ped-item-res AND ped-item-res.qt-pedida <> ped-item.qt-pedida) THEN
           ASSIGN l-separado = NO.
    END.

    IF l-separado = NO THEN DO.
       MESSAGE 'Separaá∆o INCOMPLETA, Pedido n∆o poder† ser Liberado para Faturamento...'
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
       RETURN NO-APPLY.
    END.

    FIND ped-venda-ext WHERE
         ped-venda-ext.cod-estabel = tt-ped-venda.cod-estabel AND
         ped-venda-ext.nr-pedido = tt-ped-venda.nr-pedido NO-LOCK NO-ERROR.

    IF ped-venda-ext.qt-fardos = 0 THEN DO.
       FIND CURRENT ped-venda-ext SHARE-LOCK NO-ERROR.
       ASSIGN i-tot-etq = 0.
       FOR EACH ped-item-rom WHERE
                ped-item-rom.nome-abrev = tt-ped-venda.nome-abrev AND
                ped-item-rom.nr-pedcli = tt-ped-venda.nr-pedcli 
                NO-LOCK.
           ASSIGN i-tot-etq = i-tot-etq + 1.
       END.
       ASSIGN ped-venda-ext.qt-fardos = i-tot-etq.
       FIND CURRENT ped-venda-ext NO-LOCK NO-ERROR.
    END.

    MESSAGE 'Pedido Liberado para Fauramento'
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-log
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-log w-livre
ON CHOOSE OF bt-log IN FRAME f-cad
DO:
   RUN esp/essp0155b.p (INPUT tt-ped-venda.nr-pedcli).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-modifica
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-modifica w-livre
ON CHOOSE OF bt-modifica IN FRAME f-cad
DO:
   FIND ped-venda WHERE
        ped-venda.nr-pedcli = tt-ped-venda.nr-pedcli AND
        ped-venda.nome-abrev = tt-ped-venda.nome-abrev NO-LOCK NO-ERROR.
   ASSIGN gr-ped-venda = ROWID(ped-venda).

   ASSIGN w-livre:SENSITIVE = NO.
   RUN esp\espd4000.w (INPUT "Modificar").
   ASSIGN w-livre:SENSITIVE = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-livre
ON CHOOSE OF bt-ok IN FRAME f-cad /* OK */
DO:
    IF SESSION:SET-WAIT-STATE("general":U) THEN.

    /*
    {utp/ut-liter.i Calculando_Carteira *}
    RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

    RUN pi-acompanhar IN h-acomp (INPUT "Pedido: " + ped-venda.nr-pedcli).
    RUN pi-finalizar in h-acomp.
    */

    FOR EACH tt-itens-ped NO-LOCK BY tt-itens-ped.nr-sequencia.
        IF tt-itens-ped.qt-reservada > 0 AND
           (tt-itens-ped.qt-reservada < tt-itens-ped.qt-pedida - (tt-itens-ped.qt-pedida * (ABS(de-perc-min) / 100)) OR
            tt-itens-ped.qt-reservada > tt-itens-ped.qt-pedida + (tt-itens-ped.qt-pedida * (ABS(de-perc-max) / 100)) ) THEN DO.

           MESSAGE "Seq: " tt-itens-ped.nr-sequencia " Item: " tt-itens-ped.it-codig  " Ref: " tt-itens-ped.cod-refer SKIP 
                   "Quantidade Reservada est† for dos Par∆metros Permitidos..." SKIP(1)
                   "Quantidade Pedida: " tt-itens-ped.qt-pedida SKIP
                   "Quantidade Minima: " tt-itens-ped.qt-pedida - (tt-itens-ped.qt-pedida * (ABS(de-perc-min) / 100)) SKIP
                   "Quantidade Maxima: " tt-itens-ped.qt-pedida + (tt-itens-ped.qt-pedida * (ABS(de-perc-max) / 100)) SKIP(1)
                   "Quantidade Reservada: " tt-itens-ped.qt-reservada 
                   VIEW-AS ALERT-BOX ERROR BUTTONS OK.
           NEXT.
        END.

        FIND FIRST tt-etq-reservadas WHERE
                   tt-etq-reservadas.nr-pedcli = tt-itens-ped.nr-pedcli AND
                   tt-etq-reservadas.nr-seq-ped-item = tt-itens-ped.nr-sequencia AND
                   tt-etq-reservadas.tp-acao <> '' NO-LOCK NO-ERROR.

        IF NOT AVAIL tt-etq-reservadas THEN NEXT.

        FIND ped-item WHERE
             ped-item.nr-pedcli = tt-itens-ped.nr-pedcli AND
             ped-item.nome-abrev = tt-itens-ped.nome-abrev AND
             ped-item.nr-sequencia = tt-itens-ped.nr-sequencia
             SHARE-LOCK NO-ERROR.

        IF ped-item.cod-sit-item > 2 THEN DO.
           MESSAGE 'Sequencia' ped-item.nr-sequencia ' n∆o est† mais disponivel para ser Reservada...' SKIP
                   ped-item.cod-sit-item
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
           NEXT.
        END.

        IF ped-item.qt-pedida <> tt-itens-ped.qt-pedida THEN DO.
           MESSAGE 'Sequencia' ped-item.nr-sequencia ' sofreu alteraá‰es e n∆o poder† para ser Reservada...' 
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
           NEXT.
        END.

        ASSIGN l-erro = NO.
        FOR EACH tt-etq-reservadas WHERE
                 tt-etq-reservadas.nr-pedcli = tt-itens-ped.nr-pedcli AND
                 tt-etq-reservadas.nr-seq-ped-item = tt-itens-ped.nr-sequencia AND
                 tt-etq-reservadas.tp-acao = 'Inc' NO-LOCK.

            FIND ob-etiqueta WHERE
                 ob-etiqueta.cod-estabel = tt-ped-venda.cod-estabel AND
                 ob-etiqueta.num-etiqueta = tt-etq-reservadas.num-etiqueta
                 NO-LOCK NO-ERROR.
    
            FIND ped-item-rom WHERE
                 ped-item-rom.cod-estabel = ob-etiqueta.cod-estabel AND
                 ped-item-rom.num-etiqueta = ob-etiqueta.num-etiqueta 
                 NO-LOCK NO-ERROR.
            IF AVAIL ped-item-rom THEN DO.
                MESSAGE 'Etiqueta ' ob-etiqueta.num-etiqueta 'J† est† Reservada para outro Pedido...'
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
               ASSIGN l-erro = YES.
            END.
    
            IF ob-etiqueta.situacao <> 3 THEN DO.
               MESSAGE 'Etiqueta ' ob-etiqueta.num-etiqueta ' n∆o est† mais disponivel para ser Reservada...' 
                   VIEW-AS ALERT-BOX INFO BUTTONS OK.
               ASSIGN l-erro = YES.
            END.
        END.
        IF l-erro THEN NEXT.

        DO TRANSACTION.
           FOR EACH tt-etq-reservadas WHERE
                    tt-etq-reservadas.nr-pedcli = tt-itens-ped.nr-pedcli AND
                    tt-etq-reservadas.nr-seq-ped-item = tt-itens-ped.nr-sequencia NO-LOCK.

               CASE tt-etq-reservadas.tp-acao.
                    WHEN 'Inc' THEN DO.
                        FIND ob-etiqueta WHERE
                             ob-etiqueta.cod-estabel = tt-ped-venda.cod-estabel AND
                             ob-etiqueta.num-etiqueta = tt-etq-reservadas.num-etiqueta
                             SHARE-LOCK NO-ERROR.
                        ASSIGN ob-etiqueta.situacao = 4
                               ob-etiqueta.ob-origem = "".
                        FIND CURRENT ob-etiqueta NO-LOCK NO-ERROR.
        
                        FIND ped-item-res WHERE
                             ped-item-res.cod-estabel  = tt-ped-venda.cod-estabel  AND 
                             ped-item-res.nome-abrev   = tt-ped-venda.nome-abrev   AND
                             ped-item-res.nr-pedcli    = tt-ped-venda.nr-pedcli    AND
                             ped-item-res.nr-sequencia = tt-itens-ped.nr-sequencia
                             SHARE-LOCK NO-ERROR.    
        
                        IF NOT AVAIL ped-item-res THEN DO.
                           CREATE ped-item-res.
                           ASSIGN ped-item-res.cod-estabel  = tt-ped-venda.cod-estabel
                                  ped-item-res.nome-abrev   = tt-ped-venda.nome-abrev
                                  ped-item-res.nr-pedcli    = tt-ped-venda.nr-pedcli
                                  ped-item-res.nr-sequencia = tt-itens-ped.nr-sequencia
                                  ped-item-res.it-codigo    = tt-itens-ped.it-codigo
                                  ped-item-res.cod-refer    = tt-itens-ped.cod-refer
                                  ped-item-res.nome-transp  = tt-ped-venda.nome-transp
                                  ped-item-res.sigla-emb    = tt-itens-ped.lote
                                  ped-item-res.desc-dentro  = c-desc-dentro
                                  ped-item-res.dt-trans     = TODAY
                                  ped-item-res.hr-trans     = STRING(TIME,"HH:MM:SS") + "Prog: ESSP0154  Usuario:" + c-seg-usuario
                                  ped-item-res.lote         = ob-etiqueta.nr-lote + ob-etiqueta.cod-refer.
                        END.
                        ASSIGN ped-item-res.qt-pedida = ped-item-res.qt-pedida + ob-etiqueta.quantidade. 

                        FIND CURRENT ped-item-res NO-LOCK NO-ERROR.

                        CREATE ped-item-rom.
                        ASSIGN ped-item-rom.cod-estabel = tt-ped-venda.cod-estabel
                               ped-item-rom.nome-abrev = ped-item-res.nome-abrev
                               ped-item-rom.nr-pedcli = ped-item-res.nr-pedcli
                               ped-item-rom.nr-sequencia = ped-item-res.nr-sequencia
                               ped-item-rom.num-etiqueta = tt-etq-reservadas.num-etiqueta
                               ped-item-rom.nr-ob = tt-etq-reservadas.nr-ob
                               ped-item-rom.nr-seq-etq = tt-etq-reservadas.nr-sequencia
                               ped-item-rom.quantidade = ob-etiqueta.quantidade.

                        /*
                        FIND ped-item-ext WHERE
                             ped-item-ext.cod-estabel = tt-ped-venda.cod-estabel AND
                             ped-item-ext.nr-pedcli = ped-item-rom.nr-pedcli AND
                             ped-item-ext.nr-sequencia = ped-item-rom.nr-sequencia
                             SHARE-LOCK NO-ERROR.

                        ASSIGN ped-item-ext.lote = ob-etiqueta.nr-lote + ob-etiqueta.cod-refer.

                        FIND CURRENT ped-item-ext NO-LOCK NO-ERROR.
                        */
                    END.
                    WHEN 'Del' THEN DO.
                        FIND ob-etiqueta WHERE
                             ob-etiqueta.cod-estabel = tt-ped-venda.cod-estabel AND
                             ob-etiqueta.num-etiqueta = tt-etq-reservadas.num-etiqueta
                             SHARE-LOCK NO-ERROR.
                        ASSIGN ob-etiqueta.situacao = 3
                               ob-etiqueta.ob-origem = "".
                        FIND CURRENT ob-etiqueta NO-LOCK NO-ERROR.

                        FIND ped-item-res WHERE
                             ped-item-res.cod-estabel  = tt-ped-venda.cod-estabel AND
                             ped-item-res.nome-abrev   = tt-ped-venda.nome-abrev   AND
                             ped-item-res.nr-pedcli    = tt-ped-venda.nr-pedcli    AND
                             ped-item-res.nr-sequencia = tt-itens-ped.nr-sequencia 
                             SHARE-LOCK NO-ERROR.    
    
                        IF AVAIL ped-item-res THEN DO.
                           ASSIGN ped-item-res.qt-pedida = ped-item-res.qt-pedida - ob-etiqueta.quantidade. 
    
                           IF ped-item-res.qt-pedida <= 0 THEN
                              DELETE ped-item-res.
                        END.

                        FIND ped-item-rom WHERE
                             ped-item-rom.cod-estabel = tt-ped-venda.cod-estabel AND
                             ped-item-rom.num-etiqueta = ob-etiqueta.num-etiqueta NO-ERROR.
        
                        IF AVAIL ped-item-rom THEN
                           DELETE ped-item-rom.

                        FIND CURRENT ped-item-rom NO-LOCK NO-ERROR.
                    END.
               END CASE.
           END.
    
           ASSIGN c-desc-reserva = "Seq. " + TRIM(STRING(tt-itens-ped.nr-sequencia,">>>9")).

           FIND ped-item-res WHERE
                ped-item-res.cod-estabel  = tt-ped-venda.cod-estabel  AND
                ped-item-res.nome-abrev   = tt-ped-venda.nome-abrev   AND
                ped-item-res.nr-pedcli    = tt-ped-venda.nr-pedcli    AND
                ped-item-res.nr-sequencia = tt-itens-ped.nr-sequencia 
                SHARE-LOCK NO-ERROR.    

           IF AVAIL ped-item-res AND
              tt-itens-ped.qt-pedida <> ped-item-res.qt-pedida AND
              ped-item-res.qt-pedida > 0 THEN DO.
        
              ASSIGN de-tot-rom = 0.
              FOR EACH ped-item-rom WHERE
                       ped-item-rom.cod-estabel = tt-ped-venda.cod-estabel AND
                       ped-item-rom.nome-abrev = ped-item-res.nome-abrev AND
                       ped-item-rom.nr-pedcli = ped-item-res.nr-pedcli AND
                       ped-item-rom.nr-sequencia = ped-item-res.nr-sequencia
                       NO-LOCK.

                  ASSIGN de-tot-rom = de-tot-rom + ped-item-rom.quantidade.
              END.
              IF de-tot-rom <> ped-item-res.qt-pedida THEN 
                 ASSIGN ped-item-res.qt-pedida = de-tot-rom.

              FIND CURRENT ped-item-res NO-LOCK NO-ERROR.

              EMPTY TEMP-TABLE tt-ped-item.
              CREATE tt-ped-item.
              BUFFER-COPY tt-itens-ped TO tt-ped-item
                          ASSIGN tt-ped-item.qt-pedida = ped-item-res.qt-pedida.
               
              RUN esapi/altera-peditem-v2.p (INPUT TABLE tt-ped-item).
              IF RETURN-VALUE = 'NOK' THEN
                 UNDO, NEXT. 
    
              ASSIGN c-desc-reserva = c-desc-reserva + " RESERVADA e Ajustada a Quantidade" + 
                              "   De: " + TRIM(STRING(tt-itens-ped.qt-pedida,">>>,>>9.99")) +                   
                              " Para: " + TRIM(STRING(ped-item-res.qt-pedida,">>>,>>9.99")).

              ASSIGN tt-itens-ped.qt-pedida = ped-item-res.qt-pedida.
           END.
           ELSE
              ASSIGN c-desc-reserva = c-desc-reserva + " Reserva ELIMINADA".

           RUN esapi/cria-log-pedvenda.p (INPUT tt-itens-ped.nr-pedcli,
                                          INPUT tt-itens-ped.nome-abrev,
                                          INPUT c-desc-reserva,
                                          INPUT YES).
        END.
    END.
    RUN esapi/completa-pedvenda.p (INPUT tt-ped-venda.nr-pedcli).
    
    IF SESSION:SET-WAIT-STATE("":U) THEN.

    ASSIGN l-separado = YES.
    FOR EACH ped-item OF tt-ped-venda NO-LOCK.
        FIND ped-item-res OF ped-item NO-LOCK NO-ERROR.

        IF (NOT AVAIL ped-item-res) OR
           (AVAIL ped-item-res AND ped-item-res.qt-pedida <> ped-item.qt-pedida) THEN
           ASSIGN l-separado = NO.
    END.
    
    IF l-separado THEN DO.
       RUN esapi/cria-log-pedvenda.p (INPUT tt-ped-venda.nr-pedcli,
                                      INPUT tt-ped-venda.nome-abrev,
                                      INPUT "SEPARAÄ«O Completa, disponivel para Faturamento", 
                                      INPUT YES).
       ASSIGN bt-fardo:SENSITIVE = YES.
    END.
    APPLY 'VALUE-CHANGED' TO br-pedidos.
    ASSIGN bt-ok:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-param
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-param w-livre
ON CHOOSE OF bt-param IN FRAME f-cad /* Button 3 */
DO:
  IF c-cod-estabel = '' THEN DO.
     MESSAGE 'Usuario ' c-seg-usuario ' n∆o relacionado Ö um Estabelecimento....'
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
     RETURN NO-APPLY.
  END.

  ASSIGN w-livre:SENSITIVE = NO.

  RUN esp/essp0154a.w (INPUT-OUTPUT c-cod-estabel,
                       INPUT-OUTPUT c-dt-limite,   
                       INPUT-OUTPUT c-nr-pedcli-ini,
                       INPUT-OUTPUT c-nr-pedcli-fin,
                       INPUT-OUTPUT c-nome-abrev-ini,
                       INPUT-OUTPUT c-nome-abrev-fin,
                       INPUT-OUTPUT c-no-ab-reppri-ini,
                       INPUT-OUTPUT c-no-ab-reppri-fin,
                       INPUT-OUTPUT c-it-codigo-ini,
                       INPUT-OUTPUT c-it-codigo-fin,
                       INPUT-OUTPUT c-cod-refer-ini,
                       INPUT-OUTPUT c-cod-refer-fin,
                       INPUT-OUTPUT c-cod-obsoleto-ini,
                       INPUT-OUTPUT c-cod-obsoleto-fin,
                       INPUT-OUTPUT de-perc-min,
                       INPUT-OUTPUT de-perc-max,
                       INPUT-OUTPUT c-opc-artigo,
                       INPUT-OUTPUT l-lote-todos,
                       INPUT-OUTPUT l-lote-pp,
                       INPUT-OUTPUT l-lote-pd,
                       INPUT-OUTPUT l-lote-rp,
                       INPUT-OUTPUT l-lote-rd,
                       INPUT-OUTPUT l-lote-sc,
                       INPUT-OUTPUT l-lote-ca,
                       OUTPUT l-ok). 
  
  IF l-ok THEN DO.
     RUN pi-processa.

     IF CAN-FIND(FIRST tt-ped-venda) THEN
        ASSIGN bt-vapara:SENSITIVE = YES
               bt-modifica:SENSITIVE = YES
               bt-consulta:SENSITIVE = YES
               bt-imprime:SENSITIVE = YES
               bt-log:SENSITIVE = YES.
  END.
  ASSIGN w-livre:SENSITIVE = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-vapara
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-vapara w-livre
ON CHOOSE OF bt-vapara IN FRAME f-cad
DO:
  RUN esp/essp0154b.w (OUTPUT c-nr-pedcli).

  IF c-nr-pedcli <> "" THEN DO:
     FIND FIRST tt-ped-venda WHERE
                tt-ped-venda.nr-pedcli = c-nr-pedcli NO-LOCK NO-ERROR.
      IF AVAIL tt-ped-venda THEN DO.
         h-query:REPOSITION-TO-ROWID(ROWID(tt-ped-venda)) NO-ERROR. 
         APPLY 'VALUE-CHANGED' TO br-pedidos IN FRAME {&FRAME-NAME}.
      END.
      ELSE
         MESSAGE "Pedido n∆o est† contido na seleá∆o!"
                 VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.
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


&Scoped-define SELF-NAME rs-reservas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-reservas w-livre
ON VALUE-CHANGED OF rs-reservas IN FRAME f-cad
DO:
   ASSIGN INPUT FRAME {&FRAME-NAME} rs-reservas.

   CASE rs-reservas.
       WHEN 1 THEN DO.
           FOR EACH tt-ped-venda. 
               FOR EACH tt-itens-ped OF tt-ped-venda.
                   ASSIGN tt-itens-ped.visualiza = NO.
                   IF CAN-FIND(FIRST ped-item-rom WHERE
                                     ped-item-rom.nome-abrev = tt-itens-ped.nome-abrev AND
                                     ped-item-rom.nr-pedcli = tt-itens-ped.nr-pedcli AND
                                     ped-item-rom.nr-sequencia = tt-itens-ped.nr-sequencia NO-LOCK) THEN
                       ASSIGN tt-itens-ped.visualiza = YES.
               END.
               FIND FIRST tt-itens-ped OF tt-ped-venda WHERE 
                          tt-itens-ped.visualiza = YES NO-ERROR.
               ASSIGN tt-ped-venda.visualiza = AVAIL tt-itens-ped.
           END.
       END.
       WHEN 2 THEN DO.
           FOR EACH tt-ped-venda. 
               FOR EACH tt-itens-ped OF tt-ped-venda.
                   ASSIGN tt-itens-ped.visualiza = YES.
                   IF CAN-FIND(FIRST ped-item-rom WHERE
                                     ped-item-rom.nome-abrev = tt-itens-ped.nome-abrev AND
                                     ped-item-rom.nr-pedcli = tt-itens-ped.nr-pedcli AND
                                     ped-item-rom.nr-sequencia = tt-itens-ped.nr-sequencia NO-LOCK) THEN
                      ASSIGN tt-itens-ped.visualiza = NO.
               END.
               FIND FIRST tt-itens-ped OF tt-ped-venda WHERE 
                          tt-itens-ped.visualiza = YES NO-ERROR.
               ASSIGN tt-ped-venda.visualiza = AVAIL tt-itens-ped.
           END.
       END.
       WHEN 3 THEN DO.
           FOR EACH tt-ped-venda. 
               ASSIGN tt-ped-venda.visualiza = YES.
               FOR EACH tt-itens-ped OF tt-ped-venda.
                   ASSIGN tt-itens-ped.visualiza = YES.
               END.
           END.
       END.
   END CASE.



   {&OPEN-QUERY-br-pedidos}

   IF c-nr-pedcli <> "" THEN DO:
      FIND tt-ped-venda WHERE
           tt-ped-venda.nr-pedcli = c-nr-pedcli NO-LOCK NO-ERROR.
      IF AVAIL tt-ped-venda AND
         tt-ped-venda.visualiza = YES THEN
         h-query:REPOSITION-TO-ROWID(ROWID(tt-ped-venda)) NO-ERROR. 
   END.

   APPLY 'VALUE-CHANGED' TO br-pedidos IN FRAME {&FRAME-NAME}.
   APPLY 'entry' TO br-pedidos IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-etq-estoque
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-livre 


/* ***************************  Main Block  *************************** */
ASSIGN h-query = br-pedidos:QUERY.
ASSIGN ed-obs:READ-ONLY = YES.

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
       RUN set-position IN h_p-exihel ( 1.17 , 91.29 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             bt-param:HANDLE IN FRAME f-cad , 'AFTER':U ).
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
  DISPLAY rs-reservas ed-obs fi-tot-estoque fi-tot-reservado fi-qt-pedida 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE RECT-10 rt-button RECT-11 rs-reservas bt-param br-pedidos br-itens 
         ed-obs br-etq-estoque br-etq-reservadas bt-leitor bt-coletor bt-add 
         bt-del 
      WITH FRAME f-cad IN WINDOW w-livre.
  {&OPEN-BROWSERS-IN-QUERY-f-cad}
  VIEW w-livre.
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

  {utp/ut9000.i "ESSP0154" "2.04.00.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  run pi-after-initialize.

  FIND FIRST ped-venda NO-LOCK NO-ERROR.
  ASSIGN c-cod-estabel = ped-venda.cod-estabel.

  /*
  FIND FIRST para-ped NO-LOCK NO-ERROR.
  ASSIGN c-cod-estabel = para-ped.estab-padrao.
  */

  ASSIGN c-dt-limite = STRING(MONTH(TODAY),'99') + STRING(YEAR(TODAY),'9999')
         l-lote-todos = YES.

  APPLY 'choose' TO bt-param.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-carrega-dados w-livre 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-processa w-livre 
PROCEDURE pi-processa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN utp/ut-acomp.p PERSISTENT SET h-acomp.

    FOR EACH tt-ped-venda.
        DELETE tt-ped-venda.
    END.

    FOR EACH tt-itens-ped.
        DELETE tt-itens-ped.
    END.

    RUN esapi/ret-udm.p (INPUT c-dt-limite, OUTPUT c-dia).
    ASSIGN da-dt-entrega = DATE(c-dia + SUBSTR(c-dt-limite,1,2) + SUBSTR(c-dt-limite,3,4)).

    ASSIGN c-lotes = ""
           c-nr-pedcli = "".

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

    {utp/ut-liter.i Calculando_Carteira *}
    RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

    RUN pi-separa-pedidos.
    /*
    IF c-nr-pedcli-ini <> '' OR 
       c-nome-abrev-ini <> '' OR 
       c-no-ab-reppri-ini <> '' THEN
       RUN pi-separa-pedidos.
    ELSE
       RUN pi-separa-itens.
    */
    RUN pi-finalizar in h-acomp.

    APPLY 'value-changed' TO rs-reservas IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-separa-itens w-livre 
PROCEDURE pi-separa-itens :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FOR EACH ped-item WHERE
            LOOKUP(STRING(ped-item.cod-sit-item),"1,2,5") > 0 AND
            ped-item.it-codigo >= c-it-codigo-ini AND
            ped-item.it-codigo <= c-it-codigo-fin AND
            ped-item.cod-refer >= c-cod-refer-ini AND 
            ped-item.cod-refer <= c-cod-refer-fin NO-LOCK,
      FIRST ped-venda OF ped-item WHERE
            LOOKUP(STRING(ped-venda.cod-sit-ped),"1,2,5") > 0 AND
            ped-venda.cod-estabel = c-cod-estabel   AND
            ped-venda.nr-pedcli  >= c-nr-pedcli-ini AND
            ped-venda.nr-pedcli  <= c-nr-pedcli-fin AND 
            ped-venda.nome-abrev >= c-nome-abrev-ini AND
            ped-venda.nome-abrev <= c-nome-abrev-fin AND
            LOOKUP(STRING(ped-venda.cod-sit-ped),"1,2,5") > 0 AND
            ped-venda.dt-entrega <= da-dt-entrega AND
            ped-venda.no-ab-reppri >= c-no-ab-reppri-ini AND
            ped-venda.no-ab-reppri <= c-no-ab-reppri-fin NO-LOCK,
      FIRST ped-item-ext WHERE
            ped-item-ext.cod-estabel = ped-venda.cod-estabel AND
            ped-item-ext.nr-pedcli = ped-item.nr-pedcli AND 
            ped-item-ext.nome-abrev = ped-item.nome-abrev AND
            ped-item-ext.nr-sequencia = ped-item.nr-sequencia NO-LOCK.
                        
       FIND cond-pagto OF ped-venda NO-LOCK NO-ERROR.
       IF AVAIL cond-pagto THEN 
          IF (cond-pagto.cod-vencto < 2 OR cond-pagto.cod-vencto > 3) AND  
             (ped-venda.cod-sit-aval = 1 OR ped-venda.cod-sit-aval > 3) THEN NEXT.

       FIND item-ext WHERE
            item-ext.it-codigo = ped-item.it-codigo NO-LOCK NO-ERROR.
       /*
       FIND ref-item-ext WHERE
            ref-item-ext.it-codigo = ped-item.it-codigo AND
            ref-item-ext.cod-refer = ped-item.cod-refer NO-LOCK NO-ERROR.
       IF AVAIL ref-item-ext AND 
          (ref-item-ext.cod-obsoleto < c-cod-obsoleto-ini OR
           ref-item-ext.cod-obsoleto > c-cod-obsoleto-fin) THEN NEXT.
       */
       RUN pi-acompanhar IN h-acomp (INPUT "Pedido/Item: " + ped-item.nr-pedcli + " " + ped-item.it-codigo).

       FIND tt-ped-venda WHERE
            tt-ped-venda.cod-estabel = ped-venda.cod-estabel AND   /*  daf  */
            tt-ped-venda.nr-pedcli = ped-venda.nr-pedcli AND
            tt-ped-venda.nome-abrev = ped-venda.nome-abrev NO-ERROR.
       IF NOT AVAIL tt-ped-venda THEN DO:
          CREATE tt-ped-venda.
          BUFFER-COPY ped-venda TO tt-ped-venda.
       END.

       FIND tt-itens-ped WHERE
            tt-itens-ped.nr-pedcli = ped-item.nr-pedcli AND
            tt-itens-ped.nome-abrev = ped-item.nome-abrev AND 
            tt-itens-ped.nr-sequencia = ped-item.nr-sequencia 
            NO-ERROR.
       IF NOT AVAIL tt-itens-ped THEN DO:
          CREATE tt-itens-ped.
          BUFFER-COPY ped-item TO tt-itens-ped
                      ASSIGN tt-itens-ped.lote = SUBSTR(ped-item-ext.lote,1,2)
                             tt-itens-ped.corte-comerc = ped-item-ext.corte-comerc.
       END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-separa-pedidos w-livre 
PROCEDURE pi-separa-pedidos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FIND ped-item NO-LOCK NO-ERROR. 

    FOR EACH ped-venda WHERE
             ped-venda.cod-sit-ped = 1 AND
             ped-venda.nr-pedcli >= c-nr-pedcli-ini AND
             ped-venda.nr-pedcli <= c-nr-pedcli-fin NO-LOCK,
        EACH ped-item OF ped-venda WHERE
             ped-item.cod-sit-item = 1 AND
             ped-item.it-codigo >= c-it-codigo-ini AND
             ped-item.it-codigo <= c-it-codigo-fin AND
             ped-item.cod-refer >= c-cod-refer-ini AND 
             ped-item.cod-refer <= c-cod-refer-fin NO-LOCK,
        FIRST ped-item-ext WHERE
              ped-item-ext.cod-estabel = ped-venda.cod-estabel AND
              ped-item-ext.nr-pedcli = ped-item.nr-pedcli AND 
              ped-item-ext.nome-abrev = ped-item.nome-abrev AND
              ped-item-ext.nr-sequencia = ped-item.nr-sequencia
              NO-LOCK.


        IF (ped-venda.cod-sit-aval <> 2 AND          /* Somente pedidos aprovados */
            ped-venda.cod-sit-aval <> 3) THEN NEXT.

        IF NOT ped-venda.completo THEN NEXT.  /* Somente Pedidos Completos */
        
        IF ped-venda.cod-sit-preco <> 2 THEN NEXT.  /* Preco n∆o Aprovado */
        IF ped-venda.cod-sit-com <> 2 THEN NEXT.    /* Frete n∆o Aprovado */
         
        IF ped-venda.mo-codigo <> 0 THEN NEXT.

        FIND ped-venda-ext WHERE
             ped-venda-ext.cod-estabel = ped-venda.cod-estabel AND  /*  daf   */
             ped-venda-ext.nr-pedido = INT(ped-venda.nr-pedcli) 
             NO-LOCK NO-ERROR.
        IF NOT AVAIL ped-venda-ext THEN NEXT.

        IF NOT ped-venda-ext.l-etiqueta THEN NEXT.   /* Somento Pedidos Impressos no ESSP0204 */
        IF ped-venda-ext.l-nao-aprovar THEN NEXT.

        IF ped-venda.cod-estabel <> c-cod-estabel THEN NEXT.

        IF ped-venda.nome-abrev < c-nome-abrev-ini OR
           ped-venda.nome-abrev > c-nome-abrev-fin THEN NEXT.

        IF ped-venda.dt-entrega > da-dt-entrega THEN NEXT.

        IF ped-venda.no-ab-reppri < c-no-ab-reppri-ini OR
           ped-venda.no-ab-reppri > c-no-ab-reppri-fin THEN NEXT.

        /*
        FIND cond-pagto OF ped-venda NO-LOCK NO-ERROR.
        IF AVAIL cond-pagto THEN 
           IF (cond-pagto.cod-vencto < 2 OR cond-pagto.cod-vencto > 3) AND  
              (ped-venda.cod-sit-aval = 1 OR ped-venda.cod-sit-aval > 3) THEN NEXT.
        */      
        
        RUN pi-acompanhar IN h-acomp (INPUT "Pedido: " + ped-venda.nr-pedcli).

        FIND tt-ped-venda WHERE
             tt-ped-venda.nr-pedcli = ped-venda.nr-pedcli AND
             tt-ped-venda.nome-abrev = ped-venda.nome-abrev NO-ERROR.
        IF NOT AVAIL tt-ped-venda THEN DO:
           CREATE tt-ped-venda.
           BUFFER-COPY ped-venda TO tt-ped-venda.
        END.
                 
        FIND tt-itens-ped WHERE
             tt-itens-ped.nr-pedcli = ped-item.nr-pedcli AND
             tt-itens-ped.nome-abrev = ped-item.nome-abrev AND 
             tt-itens-ped.nr-sequencia = ped-item.nr-sequencia 
             NO-ERROR.
        IF NOT AVAIL tt-itens-ped THEN DO:
           CREATE tt-itens-ped.
           BUFFER-COPY ped-item TO tt-itens-ped 
                       ASSIGN tt-itens-ped.lote = SUBSTR(ped-item-ext.lote,1,2)
                              tt-itens-ped.corte-comerc = ped-item-ext.corte-comerc.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-soma-est w-livre 
PROCEDURE pi-soma-est :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN fi-tot-estoque = 0.

    ASSIGN bt-add:SENSITIVE IN FRAME {&FRAME-NAME} = YES
           bt-leitor:SENSITIVE IN FRAME {&FRAME-NAME} = YES
           bt-coletor:SENSITIVE IN FRAME {&FRAME-NAME} = YES
           bt-det:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

    FOR EACH tt-etq-estoque NO-LOCK.
        ASSIGN fi-tot-estoque =  fi-tot-estoque + tt-etq-estoque.quantidade.
    END.
    DISP fi-tot-estoque WITH FRAME {&FRAME-NAME}.

    IF fi-tot-estoque = 0 THEN
       ASSIGN bt-add:SENSITIVE IN FRAME {&FRAME-NAME} = NO
              bt-det:SENSITIVE IN FRAME {&FRAME-NAME} = NO
              bt-leitor:SENSITIVE IN FRAME {&FRAME-NAME} = NO
              bt-coletor:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-soma-res w-livre 
PROCEDURE pi-soma-res :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN fi-tot-reservado = 0.

    ASSIGN bt-del:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

    FOR EACH tt-etq-reservadas WHERE
             tt-etq-reservadas.nr-pedcli = tt-itens-ped.nr-pedcli AND
             tt-etq-reservadas.nr-seq-ped-item = tt-itens-ped.nr-sequencia AND
             tt-etq-reservadas.tp-acao <> 'Del' NO-LOCK.
        ASSIGN fi-tot-reservado =  fi-tot-reservado + tt-etq-reservadas.quantidade.
    END.
    DISP fi-tot-reservado WITH FRAME {&FRAME-NAME}.

    IF fi-tot-reservado = 0 THEN
       ASSIGN bt-del:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

    IF AVAIL tt-itens-ped THEN
       ASSIGN tt-itens-ped.qt-reservada = fi-tot-reservado.
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
  {src/adm/template/snd-list.i "tt-ped-venda"}
  {src/adm/template/snd-list.i "tt-itens-ped"}
  {src/adm/template/snd-list.i "item"}
  {src/adm/template/snd-list.i "tt-etq-reservadas"}
  {src/adm/template/snd-list.i "tt-etq-estoque"}

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

