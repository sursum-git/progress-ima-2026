&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME w-relat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-relat 
/*:T*******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESPD0002 2.04.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/*:T Preprocessadores do Template de Relat¢rio                            */
/*:T Obs: Retirar o valor do preprocessador para as p ginas que nÆo existirem  */

&GLOBAL-DEFINE PGSEL f-pg-sel
&GLOBAL-DEFINE PGCLA f-pg-cla
&GLOBAL-DEFINE PGPAR f-pg-par
&GLOBAL-DEFINE PGDIG f-pg-dig
&GLOBAL-DEFINE PGIMP f-pg-imp
  
/* Parameters Definitions ---                                           */

/* Temporary Table Definitions ---                                      */
{esinc/espd0002.i}

DEFINE TEMP-TABLE tt-digita NO-UNDO 
       FIELD nr-sequencia     AS   INT FORMAT '>>9'
       FIELD it-codigo        LIKE ped-item.it-codigo
       FIELD cod-refer        LIKE ped-item.cod-refer
       FIELD desc-item        AS   CHAR FORMAT "x(25)"
       FIELD qt-pedida        AS   DEC FORMAT ">>>,>>9.99" 
       FIELD qt-reserva       AS   DEC FORMAT ">>>,>>9.99" 
       FIELD sit-prog         AS   CHAR FORMAT "x(7)"
       INDEX seqped nr-sequencia.

DEFINE BUFFER b-tt-digita FOR tt-digita.

/* Transfer Definitions */

def var raw-param        as raw no-undo.

def temp-table tt-raw-digita
   field raw-digita      as raw.
                    
/* Local Variable Definitions ---                                       */

def var l-ok               as logical no-undo.
def var c-arq-digita       as char    no-undo.
def var c-terminal         as char    no-undo.

/* Temp Table dos Pedidos para ImpressÆo */
define NEW GLOBAL SHARED temp-table tt-pedidos no-undo
       field nr-pedcli       like ped-venda.nr-pedcli
       field qt-aberto       LIKE ped-item.qt-atendida
       field perc-pronto     AS   DEC FORMAT ">>9.99"
       field qt-reserva      LIKE ped-item.qt-atendida
       FIELD it-ares         AS   INT
       FIELD atendido        AS   CHAR FORMAT "x(30)"
       FIELD marca           AS   LOG INIT YES
       index ch-pedido nr-pedcli.

DEF VAR v-row-tt-pedidos AS ROWID.
DEF VAR c-desc-rb  AS CHAR.
DEF VAR c-sit-prog AS CHAR.
DEF VAR l-continua AS LOGICAL INIT YES.
DEF VAR i-cont     AS INT.

DEF VAR c-arq-bat AS CHAR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-relat
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-pg-cla
&Scoped-define BROWSE-NAME br-digita

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-digita

/* Definitions for BROWSE br-digita                                     */
&Scoped-define FIELDS-IN-QUERY-br-digita tt-digita.nr-sequencia tt-digita.it-codigo tt-digita.cod-refer tt-digita.desc-item tt-digita.qt-pedida tt-digita.qt-reserva tt-digita.sit-prog   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-digita   
&Scoped-define SELF-NAME br-digita
&Scoped-define QUERY-STRING-br-digita FOR EACH tt-digita
&Scoped-define OPEN-QUERY-br-digita OPEN QUERY br-digita FOR EACH tt-digita.
&Scoped-define TABLES-IN-QUERY-br-digita tt-digita
&Scoped-define FIRST-TABLE-IN-QUERY-br-digita tt-digita


/* Definitions for FRAME f-pg-dig                                       */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-pg-dig ~
    ~{&OPEN-QUERY-br-digita}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-10 RECT-12 rs-classif 
&Scoped-Define DISPLAYED-OBJECTS rs-classif 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 btn_anterior btn_primeiro 
&Scoped-define List-2 btn_proximo btn_ultimo 
&Scoped-define List-3 btn_marca 
&Scoped-define List-4 fi-it-codigo[1] fi-it-codigo[2] fi-it-codigo[3] ~
fi-it-codigo[4] fi-it-codigo[5] fi-it-codigo[6] fi-it-codigo[7] ~
fi-it-codigo[8] fi-it-codigo[9] fi-it-codigo[10] 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-relat AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE rs-classif AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Sequencia/Item/Referˆncia  (Todos os Itens do Pedido)", 1,
"Volume/Item/Referˆncia       (Somente Itens Reservados)", 2
     SIZE 44 BY 2.5 NO-UNDO.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 57 BY 5.

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 76 BY 10.5.

DEFINE BUTTON btn-marca 
     IMAGE-UP FILE "image/im-chck1.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Btn 1" 
     SIZE 3.29 BY 1 TOOLTIP "Identifica se Pedido ser  Impresso / Marca e Demarca TODOS os Pedidos".

DEFINE BUTTON btn_anterior 
     IMAGE-UP FILE "image/im-pre.bmp":U NO-FOCUS
     LABEL "Button 3" 
     SIZE 4 BY 1 TOOLTIP "Pedido Anterior".

DEFINE BUTTON btn_carrega 
     IMAGE-UP FILE "image/im-plin.bmp":U NO-FOCUS
     LABEL "Button 1" 
     SIZE 4 BY 1 TOOLTIP "Carrega Pedidos para ImpressÆo".

DEFINE BUTTON btn_marca 
     IMAGE-UP FILE "image/im-aloc1.bmp":U NO-FOCUS
     LABEL "Button 6" 
     SIZE 4 BY 1 TOOLTIP "Marca/Desmarca Pedido para ImpressÆo".

DEFINE BUTTON btn_primeiro 
     IMAGE-UP FILE "image/im-fir.bmp":U NO-FOCUS
     LABEL "Button 2" 
     SIZE 4 BY 1 TOOLTIP "Primeiro Pedido".

DEFINE BUTTON btn_proximo 
     IMAGE-UP FILE "image/im-nex.bmp":U NO-FOCUS
     LABEL "Button 4" 
     SIZE 4 BY 1 TOOLTIP "Pr¢ximo Pedido".

DEFINE BUTTON btn_ultimo 
     IMAGE-UP FILE "image/im-las.bmp":U NO-FOCUS
     LABEL "Button 5" 
     SIZE 4 BY 1 TOOLTIP "éltimo Pedido".

DEFINE BUTTON btn_vapra 
     IMAGE-UP FILE "image/im-vapra.bmp":U
     LABEL "Btn 1" 
     SIZE 3.57 BY .88.

DEFINE VARIABLE ed-observ AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 33.86 BY 2.75
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE fi-desc-bloq-cr AS CHARACTER 
     VIEW-AS EDITOR
     SIZE 20.86 BY .88
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE fi-atendido AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 30.57 BY .88
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE fi-cod-emit AS INTEGER FORMAT ">>>>>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 7.29 BY .88
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE fi-cod-priori AS INTEGER FORMAT "99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 3.43 BY .88
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE fi-desc-cond-pagto AS CHARACTER FORMAT "x(20)" 
     VIEW-AS FILL-IN 
     SIZE 21 BY .88
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE fi-dt-emissao AS DATE FORMAT "99/99/9999" 
     VIEW-AS FILL-IN 
     SIZE 10.72 BY .88
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE fi-dt-entrega AS DATE FORMAT "99/99/9999" 
     VIEW-AS FILL-IN 
     SIZE 10.57 BY .88
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE fi-it-ares AS INTEGER FORMAT ">>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE fi-no-ab-repres AS CHARACTER FORMAT "x(12)" 
     VIEW-AS FILL-IN 
     SIZE 24.57 BY .88
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE fi-no-ab-transp AS CHARACTER FORMAT "X(12)" 
     VIEW-AS FILL-IN 
     SIZE 16.14 BY .88
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE fi-nome-abrev AS CHARACTER FORMAT "X(12)" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE fi-nr-pedcli AS CHARACTER FORMAT "x(12)" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE fi-nr-pedrep AS CHARACTER FORMAT "x(12)" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE fi-perc-pronto AS DECIMAL FORMAT ">>9.99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6.57 BY .88
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE fi-qt-aberto AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE fi-qt-reserva AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE fi-sit-aval AS CHARACTER FORMAT "x(12)" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE fi-situacao AS CHARACTER FORMAT "x(12)" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE fi-tp-pedido-dig AS CHARACTER FORMAT "x(2)" 
     VIEW-AS FILL-IN 
     SIZE 3.29 BY .88
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE RECTANGLE RECT-15
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 76 BY 10.67.

DEFINE VARIABLE tg-ind-fat-par AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 1.86 BY .54
     BGCOLOR 15  NO-UNDO.

DEFINE BUTTON bt-arquivo 
     IMAGE-UP FILE "image\im-sea":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE BUTTON bt-config-impr 
     IMAGE-UP FILE "image\im-cfprt":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE VARIABLE c-arquivo AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 40 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE text-destino AS CHARACTER FORMAT "X(256)":U INITIAL " Destino" 
      VIEW-AS TEXT 
     SIZE 8.57 BY .63 NO-UNDO.

DEFINE VARIABLE text-modo AS CHARACTER FORMAT "X(256)":U INITIAL "Execu‡Æo" 
      VIEW-AS TEXT 
     SIZE 10.86 BY .63 NO-UNDO.

DEFINE VARIABLE rs-destino AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Impressora", 1,
"Arquivo", 2,
"Terminal", 3
     SIZE 44 BY 1.08 NO-UNDO.

DEFINE VARIABLE rs-execucao AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "On-Line", 1,
"Batch", 2
     SIZE 27.72 BY .92 NO-UNDO.

DEFINE RECTANGLE RECT-16
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 76 BY 10.5.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 2.92.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 1.71.

DEFINE VARIABLE cb-parcial AS CHARACTER FORMAT "X(256)":U INITIAL "Todos" 
     LABEL "Entrega Parcial" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Aceita","NÆo Aceita","Todos" 
     DROP-DOWN-LIST
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE fi-max-it-ares AS INTEGER FORMAT ">>>9" INITIAL 9999 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE VARIABLE fi-min-it-ares AS INTEGER FORMAT ">>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE VARIABLE fi-perc-min AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "% M¡nimo Reserva" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-qtd-minima AS DECIMAL FORMAT ">>>,>>9.99":U INITIAL 0 
     LABEL "Quantidade M¡nima" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE fi-tp-pedido AS CHARACTER FORMAT "x(2)" 
     VIEW-AS FILL-IN 
     SIZE 4.29 BY .79
     FONT 1 NO-UNDO.

DEFINE IMAGE IMAGE-11
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-12
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE VARIABLE rs-cond-credito AS INTEGER INITIAL 3 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Aprovado", 1,
"NÆo Aprovado", 2,
"Todos", 3
     SIZE 19.86 BY 2.38 NO-UNDO.

DEFINE VARIABLE rs-cond-pagto AS INTEGER INITIAL 3 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "· Vista", 1,
"· Prazo", 2,
"Todos", 3
     SIZE 20.86 BY 2.5 NO-UNDO.

DEFINE VARIABLE rs-mercado AS INTEGER INITIAL 3 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Mercado Interno", 1,
"Mercado Externo", 2,
"Ambos", 3
     SIZE 19.29 BY 2.5 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 76 BY 10.75.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 23 BY 4.75.

DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 25.72 BY 3.25.

DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 24.72 BY 3.17.

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 50.72 BY 4.92.

DEFINE RECTANGLE RECT-22
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 23 BY 2.08.

DEFINE RECTANGLE RECT-23
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 50.72 BY 1.5.

DEFINE RECTANGLE RECT-25
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 23 BY 2.75.

DEFINE VARIABLE sl-perc-min AS INTEGER INITIAL 0 
     VIEW-AS SLIDER MIN-VALUE 0 MAX-VALUE 100 HORIZONTAL 
     TIC-MARKS TOP FREQUENCY 10
     SIZE 49 BY 1.42 NO-UNDO.

DEFINE VARIABLE tg-abertos AS LOGICAL INITIAL yes 
     LABEL "Abertos" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .58 NO-UNDO.

DEFINE VARIABLE tg-at-parcial AS LOGICAL INITIAL yes 
     LABEL "Atendidos Parcial" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .58 NO-UNDO.

DEFINE VARIABLE tg-at-total AS LOGICAL INITIAL no 
     LABEL "Atendidos Total" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .58 NO-UNDO.

DEFINE VARIABLE tg-cancelados AS LOGICAL INITIAL no 
     LABEL "Cancelados" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .58 NO-UNDO.

DEFINE VARIABLE tg-pendentes AS LOGICAL INITIAL yes 
     LABEL "Pendentes" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .58 NO-UNDO.

DEFINE VARIABLE tg-reservados AS LOGICAL INITIAL yes 
     LABEL "Itens Reservados" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .83 NO-UNDO.

DEFINE VARIABLE tg-suspensos AS LOGICAL INITIAL yes 
     LABEL "Suspensos" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .63 NO-UNDO.

DEFINE VARIABLE tg-tp-pedido AS LOGICAL INITIAL yes 
     LABEL "Todos os Tipos de Pedido" 
     VIEW-AS TOGGLE-BOX
     SIZE 20.86 BY .83 NO-UNDO.

DEFINE VARIABLE fi-desc-item AS CHARACTER FORMAT "X(256)":U 
     LABEL "Descr" 
     VIEW-AS FILL-IN 
     SIZE 43 BY .75 NO-UNDO.

DEFINE VARIABLE fi-fin-cod-emit AS INTEGER FORMAT ">>>>>>>>9" INITIAL 999999999 
     VIEW-AS FILL-IN 
     SIZE 10.14 BY .75 TOOLTIP "Cliente final" NO-UNDO.

DEFINE VARIABLE fi-fin-cod-estabel AS CHARACTER FORMAT "x(3)" INITIAL "999" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .75 TOOLTIP "C¢digo do estabelecimento final." NO-UNDO.

DEFINE VARIABLE fi-fin-corte-comerc AS CHARACTER FORMAT "!" INITIAL "Z" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .75 TOOLTIP "Corte comercial final." NO-UNDO.

DEFINE VARIABLE fi-fin-dt-emissao AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 10 BY .75 NO-UNDO.

DEFINE VARIABLE fi-fin-dt-entrega AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 10 BY .75 NO-UNDO.

DEFINE VARIABLE fi-fin-grupo-rep AS CHARACTER FORMAT "9" INITIAL "3" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .75 TOOLTIP "Grupo de representante final." NO-UNDO.

DEFINE VARIABLE fi-fin-no-ab-reppri AS CHARACTER FORMAT "X(12)" INITIAL "ZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .75 TOOLTIP "Representante final" NO-UNDO.

DEFINE VARIABLE fi-fin-nome-transp AS CHARACTER FORMAT "x(12)" INITIAL "ZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .75 TOOLTIP "Transportador final" NO-UNDO.

DEFINE VARIABLE fi-fin-pedido AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 999999999 
     VIEW-AS FILL-IN 
     SIZE 10 BY .75 NO-UNDO.

DEFINE VARIABLE fi-ini-cod-emit AS INTEGER FORMAT ">>>>>>>>9" INITIAL 0 
     LABEL "Cliente":R8 
     VIEW-AS FILL-IN 
     SIZE 10 BY .75 TOOLTIP "Cliente inicial" NO-UNDO.

DEFINE VARIABLE fi-ini-cod-estabel AS CHARACTER FORMAT "x(3)" INITIAL "1" 
     LABEL "Estabelecimento":R18 
     VIEW-AS FILL-IN 
     SIZE 5 BY .75 TOOLTIP "C¢digo do estabelecimento inicial." NO-UNDO.

DEFINE VARIABLE fi-ini-corte-comerc AS CHARACTER FORMAT "!" 
     LABEL "Corte Comercial" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .75 TOOLTIP "Corte comercial inicial." NO-UNDO.

DEFINE VARIABLE fi-ini-dt-emissao AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Emissao" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .75 NO-UNDO.

DEFINE VARIABLE fi-ini-dt-entrega AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Entrega" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .75 NO-UNDO.

DEFINE VARIABLE fi-ini-grupo-rep AS CHARACTER FORMAT "9" INITIAL "0" 
     LABEL "Grupo Rep" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .75 TOOLTIP "Grupo de representante inicial." NO-UNDO.

DEFINE VARIABLE fi-ini-no-ab-reppri AS CHARACTER FORMAT "X(12)" 
     LABEL "Representante":R28 
     VIEW-AS FILL-IN 
     SIZE 13 BY .75 TOOLTIP "Representante inicial" NO-UNDO.

DEFINE VARIABLE fi-ini-nome-transp AS CHARACTER FORMAT "x(12)" 
     LABEL "Transportador":R16 
     VIEW-AS FILL-IN 
     SIZE 14 BY .75 TOOLTIP "Transportador inicial" NO-UNDO.

DEFINE VARIABLE fi-ini-pedido AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     LABEL "Pedido":R11 
     VIEW-AS FILL-IN 
     SIZE 10 BY .75 NO-UNDO.

DEFINE VARIABLE fi-it-codigo AS CHARACTER FORMAT "x(8)"  EXTENT 10
     VIEW-AS FILL-IN 
     SIZE 8 BY .75 NO-UNDO.

DEFINE VARIABLE fi-nr-coletor AS INTEGER FORMAT "9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 3.86 BY 1.71
     FONT 10 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .75.

DEFINE IMAGE IMAGE-10
     FILENAME "image\im-las":U
     SIZE 3 BY .75.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .75.

DEFINE IMAGE IMAGE-3
     FILENAME "image\im-fir":U
     SIZE 3 BY .75.

DEFINE IMAGE IMAGE-31
     FILENAME "image\im-las":U
     SIZE 3 BY .75.

DEFINE IMAGE IMAGE-32
     FILENAME "image\im-fir":U
     SIZE 3 BY .75.

DEFINE IMAGE IMAGE-37
     FILENAME "image\im-fir":U
     SIZE 3 BY .75.

DEFINE IMAGE IMAGE-38
     FILENAME "image\im-las":U
     SIZE 3 BY .75.

DEFINE IMAGE IMAGE-39
     FILENAME "image\im-fir":U
     SIZE 3 BY .75.

DEFINE IMAGE IMAGE-4
     FILENAME "image\im-las":U
     SIZE 3 BY .75.

DEFINE IMAGE IMAGE-40
     FILENAME "image\im-las":U
     SIZE 3 BY .75.

DEFINE IMAGE IMAGE-41
     FILENAME "image\im-fir":U
     SIZE 3 BY .75.

DEFINE IMAGE IMAGE-42
     FILENAME "image\im-las":U
     SIZE 3 BY .75.

DEFINE IMAGE IMAGE-5
     FILENAME "image\im-fir":U
     SIZE 3 BY .75.

DEFINE IMAGE IMAGE-6
     FILENAME "image\im-las":U
     SIZE 3 BY .75.

DEFINE IMAGE IMAGE-7
     FILENAME "image\im-fir":U
     SIZE 3 BY .75.

DEFINE IMAGE IMAGE-8
     FILENAME "image\im-las":U
     SIZE 3 BY .75.

DEFINE IMAGE IMAGE-9
     FILENAME "image\im-fir":U
     SIZE 3 BY .75.

DEFINE RECTANGLE RECT-14
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 66 BY 3.08.

DEFINE RECTANGLE RECT-24
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 76 BY 10.63.

DEFINE RECTANGLE RECT-26
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 7 BY 3.13.

DEFINE VARIABLE tg-exc-indigo AS LOGICAL INITIAL no 
     LABEL "Exceto Öndigo" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.43 BY .83 TOOLTIP "Listar apenas Ötens que forem Öndigo?" NO-UNDO.

DEFINE VARIABLE tg-it-codigo AS LOGICAL INITIAL yes 
     LABEL "Todos os Itens" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .83 NO-UNDO.

DEFINE VARIABLE tg-so-indigo AS LOGICAL INITIAL no 
     LABEL "Somente Öndigo" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.43 BY .83 TOOLTIP "Listar apenas Ötens que forem Öndigo?" NO-UNDO.

DEFINE BUTTON bt-ajuda 
     LABEL "Ajuda" 
     SIZE 10 BY 1
     FONT 1.

DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "Fechar" 
     SIZE 10 BY 1
     FONT 1.

DEFINE BUTTON bt-carrega 
     LABEL "Carregar" 
     SIZE 10 BY 1 TOOLTIP "Carrega Pedidos para Marca‡Æo"
     FONT 1.

DEFINE BUTTON bt-coletor 
     IMAGE-UP FILE "image/emstec.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Exportar Dados para Coletor" 
     SIZE 9 BY 1.83 TOOLTIP "Exportar Dados para Coletor".

DEFINE BUTTON bt-executar 
     LABEL "Imprimir" 
     SIZE 10 BY 1
     FONT 1.

DEFINE IMAGE im-pg-cla
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.21.

DEFINE IMAGE im-pg-dig
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.21.

DEFINE IMAGE im-pg-imp
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.21.

DEFINE IMAGE im-pg-par
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.21.

DEFINE IMAGE im-pg-sel
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.21.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 69 BY 1.71
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 0    
     SIZE 78.72 BY .13
     BGCOLOR 7 .

DEFINE RECTANGLE rt-folder
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 79 BY 11.38
     FGCOLOR 0 .

DEFINE RECTANGLE rt-folder-left
     EDGE-PIXELS 0    
     SIZE .43 BY 11.21
     BGCOLOR 15 .

DEFINE RECTANGLE rt-folder-right
     EDGE-PIXELS 0    
     SIZE .43 BY 11.17
     BGCOLOR 7 .

DEFINE RECTANGLE rt-folder-top
     EDGE-PIXELS 0    
     SIZE 78.72 BY .13
     BGCOLOR 15 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-digita FOR 
      tt-digita SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-digita w-relat _FREEFORM
  QUERY br-digita DISPLAY
      tt-digita.nr-sequencia       COLUMN-LABEL "Seq"        
tt-digita.it-codigo          COLUMN-LABEL "Item" 
tt-digita.cod-refer          COLUMN-LABEL "Ref" 
tt-digita.desc-item          COLUMN-LABEL "Descri‡Æo"
tt-digita.qt-pedida          COLUMN-LABEL "Qt.Pedida"
tt-digita.qt-reserva         COLUMN-LABEL "Qt.Reserva"
tt-digita.sit-prog           COLUMN-LABEL "PrPcPt"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 65 BY 3
         BGCOLOR 15 FONT 1 ROW-HEIGHT-CHARS .5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-pg-cla
     rs-classif AT ROW 4.71 COL 17 HELP
          "Classifica‡Æo para emissÆo do relat¢rio" NO-LABEL
     "  Ordenar Relat¢rio por" VIEW-AS TEXT
          SIZE 17 BY .75 AT ROW 3.21 COL 12.57
     RECT-10 AT ROW 3.5 COL 10
     RECT-12 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.79
         SIZE 76.86 BY 10.5
         FONT 1.

DEFINE FRAME f-relat
     bt-coletor AT ROW 14 COL 72 HELP
          "Exportar Dados para Coletor"
     bt-executar AT ROW 14.42 COL 3 HELP
          "Dispara a execu‡Æo do relat¢rio"
     bt-carrega AT ROW 14.42 COL 13.43
     bt-cancelar AT ROW 14.42 COL 23.86 HELP
          "Fechar"
     bt-ajuda AT ROW 14.42 COL 59.57 HELP
          "Ajuda"
     rt-folder-left AT ROW 2.54 COL 2.14
     rt-folder AT ROW 2.5 COL 2
     rt-folder-top AT ROW 2.54 COL 2.14
     RECT-1 AT ROW 14.04 COL 2
     rt-folder-right AT ROW 2.67 COL 80.43
     RECT-6 AT ROW 13.75 COL 2.14
     im-pg-cla AT ROW 1.5 COL 17.72
     im-pg-dig AT ROW 1.5 COL 49.14
     im-pg-imp AT ROW 1.5 COL 64.86
     im-pg-par AT ROW 1.5 COL 33.43
     im-pg-sel AT ROW 1.5 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80.14 BY 15
         DEFAULT-BUTTON bt-executar.

DEFINE FRAME f-pg-imp
     rs-destino AT ROW 2.38 COL 3.29 HELP
          "Destino de ImpressÆo do Relat¢rio" NO-LABEL
     bt-arquivo AT ROW 3.58 COL 43.29 HELP
          "Escolha do nome do arquivo"
     bt-config-impr AT ROW 3.58 COL 43.29 HELP
          "Configura‡Æo da impressora"
     c-arquivo AT ROW 3.63 COL 3.29 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL
     rs-execucao AT ROW 5.75 COL 3 HELP
          "Modo de Execu‡Æo" NO-LABEL
     text-destino AT ROW 1.63 COL 3.86 NO-LABEL
     text-modo AT ROW 5 COL 1.29 COLON-ALIGNED NO-LABEL
     RECT-16 AT ROW 1 COL 1
     RECT-7 AT ROW 1.92 COL 2.14
     RECT-9 AT ROW 5.29 COL 2.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.75
         SIZE 76.86 BY 10.75.

DEFINE FRAME f-pg-dig
     btn_carrega AT ROW 10.5 COL 68
     btn-marca AT ROW 1.25 COL 1.72
     btn_marca AT ROW 10.5 COL 72
     btn_anterior AT ROW 9.5 COL 68
     btn_primeiro AT ROW 8.5 COL 68
     btn_proximo AT ROW 9.5 COL 72
     btn_ultimo AT ROW 8.5 COL 72
     fi-nr-pedcli AT ROW 1.5 COL 9 COLON-ALIGNED HELP
          "N£mero do pedido do cliente" NO-LABEL
     btn_vapra AT ROW 1.5 COL 22.29
     fi-tp-pedido-dig AT ROW 1.5 COL 37.86 COLON-ALIGNED HELP
          "Dispon¡vel para classifica‡Æo/indica‡Æo pr¢pria do usu rio" NO-LABEL
     fi-cod-emit AT ROW 1.5 COL 49.43 COLON-ALIGNED HELP
          "C¢digo do cliente" NO-LABEL
     fi-nome-abrev AT ROW 1.5 COL 57 COLON-ALIGNED HELP
          "Nome abreviado do cliente/fornecedor" NO-LABEL
     fi-desc-cond-pagto AT ROW 2.5 COL 9 COLON-ALIGNED HELP
          "Informe a descri‡Æo da condicao de pagamento" NO-LABEL
     fi-no-ab-repres AT ROW 2.5 COL 49.43 COLON-ALIGNED HELP
          "Nome abreviado do representante" NO-LABEL
     tg-ind-fat-par AT ROW 2.67 COL 33.86 NO-TAB-STOP 
     fi-nr-pedrep AT ROW 3.5 COL 9 COLON-ALIGNED HELP
          "N£mero do pedido do representante" NO-LABEL
     fi-situacao AT ROW 3.5 COL 28.57 COLON-ALIGNED HELP
          "N£mero do pedido do representante" NO-LABEL
     fi-perc-pronto AT ROW 3.5 COL 49.43 COLON-ALIGNED HELP
          "Nome abreviado do transportador" NO-LABEL
     fi-dt-emissao AT ROW 3.5 COL 63.29 COLON-ALIGNED HELP
          "Data em que o pedido foi emitido pelo representante/cliente" NO-LABEL
     fi-sit-aval AT ROW 4.5 COL 9 COLON-ALIGNED HELP
          "N£mero do pedido do representante" NO-LABEL
     fi-desc-bloq-cr AT ROW 4.5 COL 21 NO-LABEL NO-TAB-STOP 
     fi-cod-priori AT ROW 4.5 COL 49.57 COLON-ALIGNED HELP
          "C¢digo de Prioridade para fornecimento do pedido" NO-LABEL
     fi-dt-entrega AT ROW 4.5 COL 63.43 COLON-ALIGNED HELP
          "Data em que o pedido foi emitido pelo representante/cliente" NO-LABEL
     fi-it-ares AT ROW 5.5 COL 9 COLON-ALIGNED HELP
          "Nome abreviado do transportador" NO-LABEL
     fi-no-ab-transp AT ROW 5.5 COL 23.43 COLON-ALIGNED HELP
          "Nome abreviado do transportador" NO-LABEL
     ed-observ AT ROW 5.63 COL 42.14 NO-LABEL NO-TAB-STOP 
     fi-qt-reserva AT ROW 6.5 COL 9 COLON-ALIGNED HELP
          "Nome abreviado do transportador" NO-LABEL
     fi-qt-aberto AT ROW 6.5 COL 28.57 COLON-ALIGNED HELP
          "Nome abreviado do transportador" NO-LABEL
     fi-atendido AT ROW 7.5 COL 9 COLON-ALIGNED HELP
          "N£mero do pedido do representante" NO-LABEL
     br-digita AT ROW 8.5 COL 2
     "Itens a Res:" VIEW-AS TEXT
          SIZE 8.57 BY .75 AT ROW 5.5 COL 2.43
          FONT 1
     "Cr‚dito:" VIEW-AS TEXT
          SIZE 5 BY .75 AT ROW 4.5 COL 5.43
          FONT 1
     "Situa‡Æo:" VIEW-AS TEXT
          SIZE 6.57 BY .75 AT ROW 3.54 COL 23.43
          FONT 1
     "Pronto:" VIEW-AS TEXT
          SIZE 5 BY .75 AT ROW 7.5 COL 5.86
          FONT 1
     "Ped Repres:" VIEW-AS TEXT
          SIZE 8.57 BY .75 AT ROW 3.5 COL 2
          FONT 1
     "Repres:" VIEW-AS TEXT
          SIZE 5.43 BY .75 AT ROW 2.5 COL 45.57
          FONT 1
     "Cond Pagto:" VIEW-AS TEXT
          SIZE 9 BY .75 AT ROW 2.5 COL 2
          FONT 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.67
         SIZE 77 BY 10.96
         FONT 1.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME f-pg-dig
     "Cliente:" VIEW-AS TEXT
          SIZE 5 BY .75 AT ROW 1.54 COL 46
          FONT 1
     "Tipo Ped:" VIEW-AS TEXT
          SIZE 7 BY .75 AT ROW 1.54 COL 32.43
          FONT 1
     "Pedido:" VIEW-AS TEXT
          SIZE 5 BY .75 AT ROW 1.54 COL 5.43
          FONT 1
     "Fat Parcial" VIEW-AS TEXT
          SIZE 8 BY .75 AT ROW 2.54 COL 36.29
          FONT 1
     "% Pronto:" VIEW-AS TEXT
          SIZE 7 BY .75 AT ROW 3.5 COL 44.43
          FONT 1
     "Qtd Reserva:" VIEW-AS TEXT
          SIZE 9 BY .75 AT ROW 6.5 COL 1.72
          FONT 1
     "Qtd Aberto:" VIEW-AS TEXT
          SIZE 7.57 BY .75 AT ROW 6.5 COL 22.57
          FONT 1
     "Prioridade:" VIEW-AS TEXT
          SIZE 7.57 BY .75 AT ROW 4.5 COL 44
          FONT 1
     "Transp:" VIEW-AS TEXT
          SIZE 5.57 BY .75 AT ROW 5.5 COL 19.72
          FONT 1
     "Entrega:" VIEW-AS TEXT
          SIZE 6 BY .75 AT ROW 4.5 COL 59.14
          FONT 1
     "EmissÆo:" VIEW-AS TEXT
          SIZE 6 BY .75 AT ROW 3.5 COL 59
          FONT 1
     RECT-15 AT ROW 1.08 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.67
         SIZE 77 BY 10.96
         FONT 1.

DEFINE FRAME f-pg-par
     tg-at-total AT ROW 2.08 COL 3.86
     rs-cond-pagto AT ROW 2.25 COL 54 NO-LABEL
     rs-cond-credito AT ROW 2.38 COL 27.57 NO-LABEL
     tg-abertos AT ROW 2.83 COL 3.86
     tg-at-parcial AT ROW 3.58 COL 3.86
     tg-pendentes AT ROW 4.33 COL 3.86
     tg-suspensos AT ROW 5.08 COL 3.86
     cb-parcial AT ROW 5.5 COL 38.57 COLON-ALIGNED
     tg-reservados AT ROW 5.5 COL 55
     tg-cancelados AT ROW 5.83 COL 3.86
     fi-qtd-minima AT ROW 6.5 COL 38.57 COLON-ALIGNED
     fi-perc-min AT ROW 6.5 COL 66.72 COLON-ALIGNED
     rs-mercado AT ROW 6.75 COL 3.72 NO-LABEL
     sl-perc-min AT ROW 8.5 COL 25.86 NO-LABEL
     tg-tp-pedido AT ROW 9.75 COL 3.86
     fi-min-it-ares AT ROW 10.5 COL 48 COLON-ALIGNED NO-LABEL
     fi-max-it-ares AT ROW 10.5 COL 65.57 COLON-ALIGNED NO-LABEL
     fi-tp-pedido AT ROW 10.67 COL 16.72 COLON-ALIGNED HELP
          "Dispon¡vel para classifica‡Æo/indica‡Æo pr¢pria do usu rio" NO-LABEL
     "%" VIEW-AS TEXT
          SIZE 1.14 BY 1 AT ROW 6.5 COL 74
     " Condi‡Æo de Cr‚dito" VIEW-AS TEXT
          SIZE 16 BY .75 AT ROW 1.38 COL 26
     "  Situa‡Æo dos Pedidos" VIEW-AS TEXT
          SIZE 17 BY .75 AT ROW 1.33 COL 3
     "Pedidos com N Itens a Reservar:" VIEW-AS TEXT
          SIZE 23 BY .75 AT ROW 10.54 COL 26.86
     "   0       10      20       30      40       50      60       70      80       90    100 %" VIEW-AS TEXT
          SIZE 49 BY .75 AT ROW 7.92 COL 26
     "Pedidos do Tipo:" VIEW-AS TEXT
          SIZE 12 BY .75 AT ROW 10.67 COL 6.86
     " Condi‡Æo de Pagamento" VIEW-AS TEXT
          SIZE 18.57 BY .75 AT ROW 1.42 COL 52.57
     IMAGE-11 AT ROW 10.5 COL 57
     IMAGE-12 AT ROW 10.5 COL 63
     RECT-17 AT ROW 1.17 COL 1
     RECT-18 AT ROW 1.75 COL 2
     RECT-19 AT ROW 1.75 COL 25.29
     RECT-20 AT ROW 1.79 COL 51.29
     RECT-21 AT ROW 5.08 COL 25.29
     RECT-22 AT ROW 9.5 COL 2
     RECT-23 AT ROW 10.08 COL 25.29
     RECT-25 AT ROW 6.63 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.71
         SIZE 77.14 BY 10.92
         FONT 1.

DEFINE FRAME f-pg-sel
     fi-ini-cod-estabel AT ROW 1.17 COL 19 COLON-ALIGNED HELP
          "C¢digo do estabelecimento" WIDGET-ID 2
     fi-fin-cod-estabel AT ROW 1.17 COL 44 COLON-ALIGNED HELP
          "C¢digo do estabelecimento" NO-LABEL WIDGET-ID 4
     fi-ini-pedido AT ROW 1.96 COL 19 COLON-ALIGNED HELP
          "N£mero do pedido"
     fi-fin-pedido AT ROW 1.96 COL 44 COLON-ALIGNED HELP
          "N£mero do pedido" NO-LABEL
     fi-ini-dt-emissao AT ROW 2.75 COL 19 COLON-ALIGNED
     fi-fin-dt-emissao AT ROW 2.75 COL 44 COLON-ALIGNED NO-LABEL
     fi-ini-dt-entrega AT ROW 3.54 COL 19 COLON-ALIGNED
     fi-fin-dt-entrega AT ROW 3.54 COL 44 COLON-ALIGNED NO-LABEL
     fi-ini-cod-emit AT ROW 4.33 COL 19 COLON-ALIGNED
     fi-fin-cod-emit AT ROW 4.33 COL 44 COLON-ALIGNED NO-LABEL
     fi-ini-no-ab-reppri AT ROW 5.13 COL 19 COLON-ALIGNED HELP
          "Nome abreviado do representante principal"
     fi-fin-no-ab-reppri AT ROW 5.13 COL 44 COLON-ALIGNED HELP
          "Nome abreviado do representante principal" NO-LABEL
     fi-ini-nome-transp AT ROW 5.92 COL 19 COLON-ALIGNED HELP
          "Nome do transportador"
     fi-fin-nome-transp AT ROW 5.92 COL 44 COLON-ALIGNED HELP
          "Nome do transportador" NO-LABEL
     fi-ini-corte-comerc AT ROW 6.71 COL 19 COLON-ALIGNED
     fi-fin-corte-comerc AT ROW 6.71 COL 44 COLON-ALIGNED NO-LABEL
     fi-ini-grupo-rep AT ROW 7.5 COL 19 COLON-ALIGNED WIDGET-ID 12
     fi-fin-grupo-rep AT ROW 7.5 COL 44 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     tg-it-codigo AT ROW 8.58 COL 3.43
     fi-it-codigo[1] AT ROW 8.58 COL 22 COLON-ALIGNED HELP
          "C¢digo da Fam¡lia de Material" NO-LABEL
     fi-it-codigo[2] AT ROW 8.58 COL 30.57 COLON-ALIGNED HELP
          "C¢digo da Fam¡lia de Material" NO-LABEL
     fi-it-codigo[3] AT ROW 8.58 COL 39.14 COLON-ALIGNED HELP
          "C¢digo da Fam¡lia de Material" NO-LABEL
     fi-it-codigo[4] AT ROW 8.58 COL 47.72 COLON-ALIGNED HELP
          "C¢digo da Fam¡lia de Material" NO-LABEL
     fi-it-codigo[5] AT ROW 8.58 COL 56.29 COLON-ALIGNED HELP
          "C¢digo da Fam¡lia de Material" NO-LABEL
     fi-nr-coletor AT ROW 9.04 COL 68.57 COLON-ALIGNED NO-LABEL
     tg-so-indigo AT ROW 9.46 COL 3.43
     fi-it-codigo[6] AT ROW 9.5 COL 22 COLON-ALIGNED HELP
          "C¢digo da Fam¡lia de Material" NO-LABEL
     fi-it-codigo[7] AT ROW 9.5 COL 30.57 COLON-ALIGNED HELP
          "C¢digo da Fam¡lia de Material" NO-LABEL
     fi-it-codigo[8] AT ROW 9.5 COL 39.14 COLON-ALIGNED HELP
          "C¢digo da Fam¡lia de Material" NO-LABEL
     fi-it-codigo[9] AT ROW 9.5 COL 47.72 COLON-ALIGNED HELP
          "C¢digo da Fam¡lia de Material" NO-LABEL
     fi-it-codigo[10] AT ROW 9.5 COL 56.29 COLON-ALIGNED HELP
          "C¢digo da Fam¡lia de Material" NO-LABEL
     tg-exc-indigo AT ROW 10.29 COL 3.43
     fi-desc-item AT ROW 10.38 COL 22 COLON-ALIGNED
     "Outra sele‡Æo" VIEW-AS TEXT
          SIZE 10 BY .63 AT ROW 8 COL 2.86
     " Coletor" VIEW-AS TEXT
          SIZE 5.57 BY .88 AT ROW 7.88 COL 69.43
     "Itens:" VIEW-AS TEXT
          SIZE 4.43 BY .75 AT ROW 8.42 COL 19.57
     IMAGE-1 AT ROW 1.17 COL 35.29
     IMAGE-10 AT ROW 5.13 COL 42.57
     IMAGE-2 AT ROW 1.17 COL 42.43
     IMAGE-3 AT ROW 2.75 COL 35.29
     IMAGE-31 AT ROW 5.92 COL 42.57
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.85
         SIZE 76.86 BY 10.62
         FONT 1.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME f-pg-sel
     IMAGE-32 AT ROW 5.92 COL 35.29
     IMAGE-37 AT ROW 6.71 COL 35.29
     IMAGE-38 AT ROW 6.71 COL 42.57
     IMAGE-4 AT ROW 2.71 COL 42.57
     IMAGE-5 AT ROW 3.54 COL 35.29
     IMAGE-6 AT ROW 3.54 COL 42.57
     IMAGE-7 AT ROW 4.33 COL 35.29
     IMAGE-8 AT ROW 4.33 COL 42.57
     IMAGE-9 AT ROW 5.13 COL 35.29
     RECT-14 AT ROW 8.38 COL 2
     RECT-24 AT ROW 1 COL 1
     RECT-26 AT ROW 8.29 COL 69
     IMAGE-39 AT ROW 1.96 COL 35.29 WIDGET-ID 6
     IMAGE-40 AT ROW 1.96 COL 42.43 WIDGET-ID 8
     IMAGE-41 AT ROW 7.5 COL 35.29 WIDGET-ID 14
     IMAGE-42 AT ROW 7.5 COL 42.57 WIDGET-ID 16
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.85
         SIZE 76.86 BY 10.62
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: w-relat
   Allow: Basic,Browse,DB-Fields,Window,Query
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-relat ASSIGN
         HIDDEN             = YES
         TITLE              = "Relat¢rio de Pr‚-Nota"
         HEIGHT             = 15.08
         WIDTH              = 80.72
         MAX-HEIGHT         = 22.33
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 22.33
         VIRTUAL-WIDTH      = 114.29
         RESIZE             = yes
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-relat 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-relat.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-relat
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-pg-cla
   FRAME-NAME                                                           */
/* SETTINGS FOR FRAME f-pg-dig
                                                                        */
/* BROWSE-TAB br-digita fi-atendido f-pg-dig */
/* SETTINGS FOR BUTTON btn_anterior IN FRAME f-pg-dig
   NO-ENABLE 1                                                          */
/* SETTINGS FOR BUTTON btn_marca IN FRAME f-pg-dig
   NO-ENABLE 3                                                          */
/* SETTINGS FOR BUTTON btn_primeiro IN FRAME f-pg-dig
   NO-ENABLE 1                                                          */
/* SETTINGS FOR BUTTON btn_proximo IN FRAME f-pg-dig
   NO-ENABLE 2                                                          */
/* SETTINGS FOR BUTTON btn_ultimo IN FRAME f-pg-dig
   NO-ENABLE 2                                                          */
ASSIGN 
       ed-observ:READ-ONLY IN FRAME f-pg-dig        = TRUE.

/* SETTINGS FOR FILL-IN fi-atendido IN FRAME f-pg-dig
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-cod-emit IN FRAME f-pg-dig
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-cod-priori IN FRAME f-pg-dig
   NO-ENABLE                                                            */
ASSIGN 
       fi-desc-bloq-cr:READ-ONLY IN FRAME f-pg-dig        = TRUE.

/* SETTINGS FOR FILL-IN fi-desc-cond-pagto IN FRAME f-pg-dig
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-dt-emissao IN FRAME f-pg-dig
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-dt-entrega IN FRAME f-pg-dig
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-it-ares IN FRAME f-pg-dig
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-no-ab-repres IN FRAME f-pg-dig
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-no-ab-transp IN FRAME f-pg-dig
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nome-abrev IN FRAME f-pg-dig
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nr-pedrep IN FRAME f-pg-dig
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-perc-pronto IN FRAME f-pg-dig
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qt-aberto IN FRAME f-pg-dig
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qt-reserva IN FRAME f-pg-dig
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-sit-aval IN FRAME f-pg-dig
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-situacao IN FRAME f-pg-dig
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tp-pedido-dig IN FRAME f-pg-dig
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-ind-fat-par IN FRAME f-pg-dig
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME f-pg-imp
                                                                        */
/* SETTINGS FOR FILL-IN text-destino IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       text-destino:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Destino".

/* SETTINGS FOR FILL-IN text-modo IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       text-modo:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Execu‡Æo".

/* SETTINGS FOR FRAME f-pg-par
                                                                        */
/* SETTINGS FOR FILL-IN fi-tp-pedido IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR SLIDER sl-perc-min IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME f-pg-sel
                                                                        */
/* SETTINGS FOR FILL-IN fi-desc-item IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-it-codigo[10] IN FRAME f-pg-sel
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-it-codigo[1] IN FRAME f-pg-sel
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-it-codigo[2] IN FRAME f-pg-sel
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-it-codigo[3] IN FRAME f-pg-sel
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-it-codigo[4] IN FRAME f-pg-sel
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-it-codigo[5] IN FRAME f-pg-sel
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-it-codigo[6] IN FRAME f-pg-sel
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-it-codigo[7] IN FRAME f-pg-sel
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-it-codigo[8] IN FRAME f-pg-sel
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-it-codigo[9] IN FRAME f-pg-sel
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FRAME f-relat
                                                                        */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-6 IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder-left IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder-right IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder-top IN FRAME f-relat
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-relat)
THEN w-relat:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-digita
/* Query rebuild information for BROWSE br-digita
     _START_FREEFORM
OPEN QUERY br-digita FOR EACH tt-digita.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-digita */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-pg-imp
/* Query rebuild information for FRAME f-pg-imp
     _Query            is NOT OPENED
*/  /* FRAME f-pg-imp */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-pg-sel
/* Query rebuild information for FRAME f-pg-sel
     _Query            is NOT OPENED
*/  /* FRAME f-pg-sel */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-relat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON END-ERROR OF w-relat /* Relat¢rio de Pr‚-Nota */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON WINDOW-CLOSE OF w-relat /* Relat¢rio de Pr‚-Nota */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda w-relat
ON CHOOSE OF bt-ajuda IN FRAME f-relat /* Ajuda */
DO:
   {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-imp
&Scoped-define SELF-NAME bt-arquivo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-arquivo w-relat
ON CHOOSE OF bt-arquivo IN FRAME f-pg-imp
DO:
    {include/i-rparq.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME bt-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancelar w-relat
ON CHOOSE OF bt-cancelar IN FRAME f-relat /* Fechar */
DO:
   apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-carrega
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-carrega w-relat
ON CHOOSE OF bt-carrega IN FRAME f-relat /* Carregar */
DO:
    APPLY 'choose' TO btn_carrega IN FRAME f-pg-dig.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-coletor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-coletor w-relat
ON CHOOSE OF bt-coletor IN FRAME f-relat /* Exportar Dados para Coletor */
DO:
  IF INPUT FRAME f-pg-sel fi-nr-coletor = 0 THEN DO.
     MESSAGE "N£mero do Coletor deve ser Informado..." VIEW-AS ALERT-BOX.
     APPLY "MOUSE-SELECT-CLICK":U TO im-pg-sel IN FRAME f-relat.
     APPLY 'entry' TO fi-nr-coletor IN FRAME f-pg-sel.
     RETURN NO-APPLY.
  END.

  APPLY "MOUSE-SELECT-CLICK":U TO im-pg-imp IN FRAME f-relat.
  ASSIGN rs-destino:SCREEN-VALUE IN FRAME f-pg-imp = '2'.
  APPLY "MOUSE-SELECT-CLICK":U TO rs-destino IN FRAME f-pg-imp.

  PAUSE 1 NO-MESSAGE.
  APPLY 'choose' TO bt-executar. 

  IF SEARCH("C:\IMPROTEC\P220\P220.EXE") <> ? THEN DO.
     IF SEARCH(tt-param.arquivo) <> ? THEN DO.
        ASSIGN c-arq-bat = SESSION:TEMP-DIRECTORY + "p220.bat".
        OUTPUT TO value(c-arq-bat).
           PUT "c:" SKIP
               "cd " SESSION:TEMP-DIRECTORY SKIP
               "C:\IMPROTEC\P220\P220.EXE E 2 1 "
               tt-param.arquivo SKIP.
        OUTPUT CLOSE.

        IF SEARCH(c-arq-bat) <> ? THEN DO.
           OS-COMMAND SILENT VALUE(c-arq-bat).
           OS-DELETE SILENT VALUE(c-arq-bat).
        END.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-imp
&Scoped-define SELF-NAME bt-config-impr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-config-impr w-relat
ON CHOOSE OF bt-config-impr IN FRAME f-pg-imp
DO:
   {include/i-rpimp.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME bt-executar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-executar w-relat
ON CHOOSE OF bt-executar IN FRAME f-relat /* Imprimir */
DO:
   do  on error undo, return no-apply:
       run pi-executar.
   end.
   ASSIGN fi-nr-coletor:SCREEN-VALUE IN FRAME f-pg-sel = '0'.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-executar w-relat
ON MOUSE-SELECT-CLICK OF bt-executar IN FRAME f-relat /* Imprimir */
DO:
  IF rs-destino:SCREEN-VALUE IN FRAME f-pg-imp = '2' THEN DO.
     ASSIGN rs-destino:SCREEN-VALUE IN FRAME f-pg-imp = '3'.
     APPLY "MOUSE-SELECT-CLICK":U TO rs-destino IN FRAME f-pg-imp.
     PAUSE 1 NO-MESSAGE.
  END.
  APPLY 'choose' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-dig
&Scoped-define SELF-NAME btn-marca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-marca w-relat
ON CHOOSE OF btn-marca IN FRAME f-pg-dig /* Btn 1 */
DO:
  ASSIGN v-row-tt-pedidos = ROWID(tt-pedidos).

  IF tt-pedidos.marca THEN DO.
     MESSAGE "Confirma Desmarcar TODOS os Pedidos ?" 
              VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
              TITLE "Demarca Todos os Pedidos" UPDATE l-desmarca AS LOGICAL.

     IF NOT l-desmarca THEN RETURN NO-APPLY. 
     FOR EACH tt-pedidos.
         ASSIGN tt-pedidos.marca = NO.
     END.
  END.
  ELSE DO.
      MESSAGE "Confirma Marcar TODOS os Pedidos ?" 
               VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
               TITLE "Marca Todos os Pedidos" UPDATE l-marca AS LOGICAL.

      IF NOT l-marca THEN RETURN NO-APPLY. 
      FOR EACH tt-pedidos.
          ASSIGN tt-pedidos.marca = YES.
      END.
  END.

  FIND tt-pedidos WHERE
       ROWID(tt-pedidos) = v-row-tt-pedidos NO-ERROR.

  RUN pi-display-fields.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_anterior
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_anterior w-relat
ON CHOOSE OF btn_anterior IN FRAME f-pg-dig /* Button 3 */
DO:
   FIND PREV tt-pedidos NO-ERROR.
   RUN pi-display-fields.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_carrega
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_carrega w-relat
ON CHOOSE OF btn_carrega IN FRAME f-pg-dig /* Button 1 */
DO:
  MESSAGE "Confirma Carga dos Pedidos ?" 
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
          TITLE "Carrega Pedidos para ImpressÆo" UPDATE l-escolha AS LOGICAL.

  IF NOT l-escolha THEN RETURN NO-APPLY.

  ASSIGN btn-marca:VISIBLE IN FRAME f-pg-dig = NO.

  RUN pi-carrega-dados.

  FIND FIRST tt-pedidos NO-ERROR.
  IF NOT AVAIL tt-pedidos THEN DO.
     MESSAGE "NÆo foram Encontrados Pedidos com esta Sele‡Æo/Parƒmetros" view-as alert-box.
     RETURN NO-APPLY.
  END.

  APPLY "mouse-select-click" TO im-pg-dig IN FRAME f-relat.
  RUN pi-display-fields.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_marca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_marca w-relat
ON CHOOSE OF btn_marca IN FRAME f-pg-dig /* Button 6 */
DO:
  IF NOT tt-pedidos.marca THEN
     ASSIGN tt-pedidos.marca = YES.
  ELSE
     ASSIGN tt-pedidos.marca = NO.

  RUN pi-display-fields.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_primeiro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_primeiro w-relat
ON CHOOSE OF btn_primeiro IN FRAME f-pg-dig /* Button 2 */
DO:
  FIND FIRST tt-pedidos NO-ERROR.
  RUN pi-display-fields.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_proximo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_proximo w-relat
ON CHOOSE OF btn_proximo IN FRAME f-pg-dig /* Button 4 */
DO:
  FIND NEXT tt-pedidos NO-ERROR.
  RUN pi-display-fields.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_ultimo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_ultimo w-relat
ON CHOOSE OF btn_ultimo IN FRAME f-pg-dig /* Button 5 */
DO:
  FIND LAST tt-pedidos NO-ERROR.
  RUN pi-display-fields.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_vapra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_vapra w-relat
ON CHOOSE OF btn_vapra IN FRAME f-pg-dig /* Btn 1 */
DO:
    FIND FIRST tt-pedidos WHERE
               tt-pedidos.nr-pedcli = INPUT FRAME f-pg-dig fi-nr-pedcli NO-LOCK NO-ERROR.

    IF NOT AVAIL tt-pedidos THEN DO.
       MESSAGE "Pedido nÆo Carregado..." VIEW-AS ALERT-BOX.
       APPLY 'entry' TO fi-nr-pedcli IN FRAME f-pg-dig.
       RETURN NO-APPLY.
    END.
    RUN pi-display-fields.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-sel
&Scoped-define SELF-NAME fi-fin-cod-emit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fin-cod-emit w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-fin-cod-emit IN FRAME f-pg-sel
DO:
   {include/zoomvar.i &prog-zoom = adzoom/z02ad098.w
                      &campo     = fi-fin-cod-emit
                      &campozoom = cod-emitente
                      &FRAME     = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-fin-corte-comerc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fin-corte-comerc w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-fin-corte-comerc IN FRAME f-pg-sel
DO:
  {include/zoomvar.i &prog-zoom = eszoom/z01es065.w
                     &campo     = fi-fin-corte-comerc
                     &campozoom = codigo
                     &FRAME     = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-fin-grupo-rep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fin-grupo-rep w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-fin-grupo-rep IN FRAME f-pg-sel
DO:
  {include/zoomvar.i &prog-zoom = eszoom/z01es065.w
                     &campo     = fi-fin-corte-comerc
                     &campozoom = codigo
                     &FRAME     = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-fin-no-ab-reppri
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fin-no-ab-reppri w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-fin-no-ab-reppri IN FRAME f-pg-sel
DO:
   {include/zoomvar.i &prog-zoom = adzoom/z01ad229.w
                      &campo     = fi-fin-no-ab-reppri
                      &campozoom = nome-abrev
                      &FRAME     = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-fin-nome-transp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fin-nome-transp w-relat
ON ENTRY OF fi-fin-nome-transp IN FRAME f-pg-sel
DO:
  MESSAGE "Transportador"
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fin-nome-transp w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-fin-nome-transp IN FRAME f-pg-sel
DO:
  {include/zoomvar.i &prog-zoom = adzoom/z01ad268.w
                     &campo     = fi-fin-nome-transp
                     &campozoom = nome-abrev
                     &FRAME     = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ini-cod-emit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-cod-emit w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ini-cod-emit IN FRAME f-pg-sel /* Cliente */
DO:
   {include/zoomvar.i &prog-zoom = adzoom/z02ad098.w
                      &campo     = fi-ini-cod-emit
                      &campozoom = cod-emitente
                      &FRAME     = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ini-corte-comerc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-corte-comerc w-relat
ON LEAVE OF fi-ini-corte-comerc IN FRAME f-pg-sel /* Corte Comercial */
DO:
   IF INPUT FRAME {&FRAME-NAME} fi-ini-corte-comerc <> "" THEN
      ASSIGN fi-fin-corte-comerc:SCREEN-VALUE IN FRAME {&FRAME-NAME} = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-corte-comerc w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ini-corte-comerc IN FRAME f-pg-sel /* Corte Comercial */
DO:
  {include/zoomvar.i &prog-zoom = eszoom/z01es065.w
                     &campo     = fi-ini-corte-comerc
                     &campozoom = codigo
                     &FRAME     = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ini-grupo-rep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-grupo-rep w-relat
ON LEAVE OF fi-ini-grupo-rep IN FRAME f-pg-sel /* Grupo Rep */
DO:
   IF INPUT FRAME {&FRAME-NAME} fi-ini-corte-comerc <> "" THEN
      ASSIGN fi-fin-corte-comerc:SCREEN-VALUE IN FRAME {&FRAME-NAME} = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-grupo-rep w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ini-grupo-rep IN FRAME f-pg-sel /* Grupo Rep */
DO:
  {include/zoomvar.i &prog-zoom = eszoom/z01es065.w
                     &campo     = fi-ini-corte-comerc
                     &campozoom = codigo
                     &FRAME     = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ini-no-ab-reppri
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-no-ab-reppri w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ini-no-ab-reppri IN FRAME f-pg-sel /* Representante */
DO:
   {include/zoomvar.i &prog-zoom = adzoom/z01ad229.w
                      &campo     = fi-ini-no-ab-reppri
                      &campozoom = nome-abrev
                      &FRAME     = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ini-nome-transp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-nome-transp w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ini-nome-transp IN FRAME f-pg-sel /* Transportador */
DO:
  {include/zoomvar.i &prog-zoom = adzoom/z01ad268.w
                     &campo     = fi-ini-nome-transp
                     &campozoom = nome-abrev
                     &FRAME     = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ini-pedido
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-pedido w-relat
ON LEAVE OF fi-ini-pedido IN FRAME f-pg-sel /* Pedido */
DO:
  ASSIGN fi-fin-pedido:SCREEN-VALUE IN FRAME {&FRAME-NAME} = SELF:SCREEN-VALUE
         fi-nr-coletor:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '0'.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-it-codigo[10]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo[10] w-relat
ON ENTRY OF fi-it-codigo[10] IN FRAME f-pg-sel
DO:
  IF SELF:SCREEN-VALUE <> "" THEN DO.
     FIND item WHERE
          item.it-codigo = INPUT FRAME {&FRAME-NAME} fi-it-codigo[10]
          NO-LOCK NO-ERROR.
     IF AVAIL item THEN
        ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo[10] w-relat
ON F5 OF fi-it-codigo[10] IN FRAME f-pg-sel
DO:
  APPLY 'mouse-select-dblclick' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo[10] w-relat
ON LEAVE OF fi-it-codigo[10] IN FRAME f-pg-sel
DO:
  FIND item WHERE
       item.it-codigo = INPUT FRAME {&FRAME-NAME} fi-it-codigo[10] NO-LOCK NO-ERROR.
  IF NOT AVAIL item THEN DO.
     MESSAGE "Item NÆo Cadastrado..." VIEW-AS ALERT-BOX.
     APPLY 'entry' TO SELF.
     RETURN NO-APPLY.
  END.
  ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo[10] w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-it-codigo[10] IN FRAME f-pg-sel
DO:
  {include/zoomvar.i &prog-zoom=inzoom/z01in172.w
                     &campo=fi-it-codigo[10]
                     &campozoom=it-codigo
                     &FRAME=f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo[10] w-relat
ON VALUE-CHANGED OF fi-it-codigo[10] IN FRAME f-pg-sel
DO:
  IF SELF:SCREEN-VALUE <> "" THEN DO.
     FIND item WHERE
          item.it-codigo = INPUT FRAME {&FRAME-NAME} fi-it-codigo[10] NO-LOCK NO-ERROR.
     IF AVAIL item THEN 
        ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
     ELSE
        ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-it-codigo[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo[1] w-relat
ON ENTRY OF fi-it-codigo[1] IN FRAME f-pg-sel
DO:
   IF SELF:SCREEN-VALUE <> "" THEN DO.
      FIND item WHERE
           item.it-codigo = INPUT FRAME {&FRAME-NAME} fi-it-codigo[1]
           NO-LOCK NO-ERROR.
      IF AVAIL item THEN
         ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
   END.
   ELSE ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo[1] w-relat
ON F5 OF fi-it-codigo[1] IN FRAME f-pg-sel
DO:
  APPLY 'mouse-select-dblclick' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo[1] w-relat
ON LEAVE OF fi-it-codigo[1] IN FRAME f-pg-sel
DO:
  FIND item WHERE
       item.it-codigo = INPUT FRAME {&FRAME-NAME} fi-it-codigo[1] NO-LOCK NO-ERROR.
  IF NOT AVAIL item THEN DO.
     MESSAGE "Item NÆo Cadastrado..." VIEW-AS ALERT-BOX.
     APPLY 'entry' TO SELF.
     RETURN NO-APPLY.
  END.
  ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo[1] w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-it-codigo[1] IN FRAME f-pg-sel
DO:
  {include/zoomvar.i &prog-zoom=inzoom/z01in172.w
                     &campo=fi-it-codigo[1]
                     &campozoom=it-codigo
                     &FRAME=f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo[1] w-relat
ON VALUE-CHANGED OF fi-it-codigo[1] IN FRAME f-pg-sel
DO:
  IF SELF:SCREEN-VALUE <> "" THEN DO.
     FIND item WHERE
          item.it-codigo = INPUT FRAME {&FRAME-NAME} fi-it-codigo[1] NO-LOCK NO-ERROR.
     IF AVAIL item THEN 
        ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
     ELSE
        ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-it-codigo[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo[2] w-relat
ON ENTRY OF fi-it-codigo[2] IN FRAME f-pg-sel
DO:
  IF SELF:SCREEN-VALUE <> "" THEN DO.
     FIND item WHERE
          item.it-codigo = INPUT FRAME {&FRAME-NAME} fi-it-codigo[2]
          NO-LOCK NO-ERROR.
     IF AVAIL item THEN
        ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
  END.
  ELSE ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo[2] w-relat
ON F5 OF fi-it-codigo[2] IN FRAME f-pg-sel
DO:
  APPLY 'mouse-select-dblclick' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo[2] w-relat
ON LEAVE OF fi-it-codigo[2] IN FRAME f-pg-sel
DO:
  FIND item WHERE
       item.it-codigo = INPUT FRAME {&FRAME-NAME} fi-it-codigo[2] NO-LOCK NO-ERROR.
  IF NOT AVAIL item THEN DO.
     MESSAGE "Item NÆo Cadastrado..." VIEW-AS ALERT-BOX.
     APPLY 'entry' TO SELF.
     RETURN NO-APPLY.
  END.
  ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo[2] w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-it-codigo[2] IN FRAME f-pg-sel
DO:
  {include/zoomvar.i &prog-zoom=inzoom/z01in172.w
                     &campo=fi-it-codigo[2]
                     &campozoom=it-codigo
                     &FRAME=f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo[2] w-relat
ON VALUE-CHANGED OF fi-it-codigo[2] IN FRAME f-pg-sel
DO:
  IF SELF:SCREEN-VALUE <> "" THEN DO.
     FIND item WHERE
          item.it-codigo = INPUT FRAME {&FRAME-NAME} fi-it-codigo[2] NO-LOCK NO-ERROR.
     IF AVAIL item THEN 
        ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
     ELSE
        ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-it-codigo[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo[3] w-relat
ON ENTRY OF fi-it-codigo[3] IN FRAME f-pg-sel
DO:
  IF SELF:SCREEN-VALUE <> "" THEN DO.
     FIND item WHERE
          item.it-codigo = INPUT FRAME {&FRAME-NAME} fi-it-codigo[3]
          NO-LOCK NO-ERROR.
     IF AVAIL item THEN
        ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
  END.
  ELSE ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo[3] w-relat
ON F5 OF fi-it-codigo[3] IN FRAME f-pg-sel
DO:
  APPLY 'mouse-select-dblclick' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo[3] w-relat
ON LEAVE OF fi-it-codigo[3] IN FRAME f-pg-sel
DO:
  FIND item WHERE
       item.it-codigo = INPUT FRAME {&FRAME-NAME} fi-it-codigo[3] NO-LOCK NO-ERROR.
  IF NOT AVAIL item THEN DO.
     MESSAGE "Fam¡lia NÆo Cadastrada..." VIEW-AS ALERT-BOX.
     APPLY 'entry' TO SELF.
     RETURN NO-APPLY.
  END.
  ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo[3] w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-it-codigo[3] IN FRAME f-pg-sel
DO:
  {include/zoomvar.i &prog-zoom=inzoom/z01in172.w
                     &campo=fi-it-codigo[3]
                     &campozoom=it-codigo
                     &FRAME=f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo[3] w-relat
ON VALUE-CHANGED OF fi-it-codigo[3] IN FRAME f-pg-sel
DO:
    IF SELF:SCREEN-VALUE <> "" THEN DO.
       FIND item WHERE
            item.it-codigo = INPUT FRAME {&FRAME-NAME} fi-it-codigo[3] NO-LOCK NO-ERROR.
       IF AVAIL item THEN 
          ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
       ELSE
          ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-it-codigo[4]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo[4] w-relat
ON ENTRY OF fi-it-codigo[4] IN FRAME f-pg-sel
DO:
  IF SELF:SCREEN-VALUE <> "" THEN DO.
     FIND item WHERE
          item.it-codigo = INPUT FRAME {&FRAME-NAME} fi-it-codigo[4]
          NO-LOCK NO-ERROR.
     IF AVAIL item THEN
        ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
  END.
  ELSE ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo[4] w-relat
ON F5 OF fi-it-codigo[4] IN FRAME f-pg-sel
DO:
  APPLY 'mouse-select-dblclick' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo[4] w-relat
ON LEAVE OF fi-it-codigo[4] IN FRAME f-pg-sel
DO:
  FIND item WHERE
       item.it-codigo = INPUT FRAME {&FRAME-NAME} fi-it-codigo[4] NO-LOCK NO-ERROR.
  IF NOT AVAIL item THEN DO.
     MESSAGE "Fam¡lia NÆo Cadastrada..." VIEW-AS ALERT-BOX.
     APPLY 'entry' TO SELF.
     RETURN NO-APPLY.
  END.
  ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo[4] w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-it-codigo[4] IN FRAME f-pg-sel
DO:
  {include/zoomvar.i &prog-zoom=inzoom/z01in172.w
                     &campo=fi-it-codigo[4]
                     &campozoom=it-codigo
                     &FRAME=f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo[4] w-relat
ON VALUE-CHANGED OF fi-it-codigo[4] IN FRAME f-pg-sel
DO:
  IF SELF:SCREEN-VALUE <> "" THEN DO.
     FIND item WHERE
          item.it-codigo = INPUT FRAME {&FRAME-NAME} fi-it-codigo[4] NO-LOCK NO-ERROR.
     IF AVAIL item THEN 
        ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
     ELSE
        ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-it-codigo[5]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo[5] w-relat
ON ENTRY OF fi-it-codigo[5] IN FRAME f-pg-sel
DO:
  IF SELF:SCREEN-VALUE <> "" THEN DO.
     FIND item WHERE
          item.it-codigo = INPUT FRAME {&FRAME-NAME} fi-it-codigo[5]
          NO-LOCK NO-ERROR.
     IF AVAIL item THEN
        ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
  END.
  ELSE ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo[5] w-relat
ON F5 OF fi-it-codigo[5] IN FRAME f-pg-sel
DO:
  APPLY 'mouse-select-dblclick' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo[5] w-relat
ON LEAVE OF fi-it-codigo[5] IN FRAME f-pg-sel
DO:
  FIND item WHERE
       item.it-codigo = INPUT FRAME {&FRAME-NAME} fi-it-codigo[5] NO-LOCK NO-ERROR.
  IF NOT AVAIL item THEN DO.
     MESSAGE "Fam¡lia NÆo Cadastrada..." VIEW-AS ALERT-BOX.
     APPLY 'entry' TO SELF.
     RETURN NO-APPLY.
  END.
  ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo[5] w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-it-codigo[5] IN FRAME f-pg-sel
DO:
  {include/zoomvar.i &prog-zoom=inzoom/z01in172.w
                     &campo=fi-it-codigo[5]
                     &campozoom=it-codigo
                     &FRAME=f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo[5] w-relat
ON VALUE-CHANGED OF fi-it-codigo[5] IN FRAME f-pg-sel
DO:
  IF SELF:SCREEN-VALUE <> "" THEN DO.
     FIND item WHERE
          item.it-codigo = INPUT FRAME {&FRAME-NAME} fi-it-codigo[5] NO-LOCK NO-ERROR.
     IF AVAIL item THEN 
        ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
     ELSE
        ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-it-codigo[6]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo[6] w-relat
ON ENTRY OF fi-it-codigo[6] IN FRAME f-pg-sel
DO:
  IF SELF:SCREEN-VALUE <> "" THEN DO.
     FIND item WHERE
          item.it-codigo = INPUT FRAME {&FRAME-NAME} fi-it-codigo[6]
          NO-LOCK NO-ERROR.
     IF AVAIL item THEN
        ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
  END.
  ELSE ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo[6] w-relat
ON F5 OF fi-it-codigo[6] IN FRAME f-pg-sel
DO:
  APPLY 'mouse-select-dblclick' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo[6] w-relat
ON LEAVE OF fi-it-codigo[6] IN FRAME f-pg-sel
DO:
  FIND item WHERE
       item.it-codigo = INPUT FRAME {&FRAME-NAME} fi-it-codigo[6] NO-LOCK NO-ERROR.
  IF NOT AVAIL item THEN DO.
     MESSAGE "Fam¡lia NÆo Cadastrada..." VIEW-AS ALERT-BOX.
     APPLY 'entry' TO SELF.
     RETURN NO-APPLY.
  END.
  ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo[6] w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-it-codigo[6] IN FRAME f-pg-sel
DO:
  {include/zoomvar.i &prog-zoom=inzoom/z01in172.w
                     &campo=fi-it-codigo[6]
                     &campozoom=it-codigo
                     &FRAME=f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo[6] w-relat
ON VALUE-CHANGED OF fi-it-codigo[6] IN FRAME f-pg-sel
DO:
  IF SELF:SCREEN-VALUE <> "" THEN DO.
     FIND item WHERE
          item.it-codigo = INPUT FRAME {&FRAME-NAME} fi-it-codigo[6] NO-LOCK NO-ERROR.
     IF AVAIL item THEN 
        ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
     ELSE
        ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-it-codigo[7]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo[7] w-relat
ON ENTRY OF fi-it-codigo[7] IN FRAME f-pg-sel
DO:
  IF SELF:SCREEN-VALUE <> "" THEN DO.
     FIND item WHERE
          item.it-codigo = INPUT FRAME {&FRAME-NAME} fi-it-codigo[7]
          NO-LOCK NO-ERROR.
     IF AVAIL item THEN
        ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
  END.
  ELSE ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo[7] w-relat
ON F5 OF fi-it-codigo[7] IN FRAME f-pg-sel
DO:
  APPLY 'mouse-select-dblclick' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo[7] w-relat
ON LEAVE OF fi-it-codigo[7] IN FRAME f-pg-sel
DO:
  FIND item WHERE
       item.it-codigo = INPUT FRAME {&FRAME-NAME} fi-it-codigo[7] NO-LOCK NO-ERROR.
  IF NOT AVAIL item THEN DO.
     MESSAGE "Fam¡lia NÆo Cadastrada..." VIEW-AS ALERT-BOX.
     APPLY 'entry' TO SELF.
     RETURN NO-APPLY.
  END.
  ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo[7] w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-it-codigo[7] IN FRAME f-pg-sel
DO:
  {include/zoomvar.i &prog-zoom=inzoom/z01in172.w
                     &campo=fi-it-codigo[7]
                     &campozoom=it-codigo
                     &FRAME=f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo[7] w-relat
ON VALUE-CHANGED OF fi-it-codigo[7] IN FRAME f-pg-sel
DO:
  IF SELF:SCREEN-VALUE <> "" THEN DO.
     FIND item WHERE
          item.it-codigo = INPUT FRAME {&FRAME-NAME} fi-it-codigo[7] NO-LOCK NO-ERROR.
     IF AVAIL item THEN 
        ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
     ELSE
        ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-it-codigo[8]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo[8] w-relat
ON ENTRY OF fi-it-codigo[8] IN FRAME f-pg-sel
DO:
  IF SELF:SCREEN-VALUE <> "" THEN DO.
     FIND item WHERE
          item.it-codigo = INPUT FRAME {&FRAME-NAME} fi-it-codigo[8]
          NO-LOCK NO-ERROR.
     IF AVAIL item THEN
        ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
  END.
  ELSE ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo[8] w-relat
ON F5 OF fi-it-codigo[8] IN FRAME f-pg-sel
DO:
  APPLY 'mouse-select-dblclick' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo[8] w-relat
ON LEAVE OF fi-it-codigo[8] IN FRAME f-pg-sel
DO:
  FIND item WHERE
       item.it-codigo = INPUT FRAME {&FRAME-NAME} fi-it-codigo[8] NO-LOCK NO-ERROR.
  IF NOT AVAIL item THEN DO.
     MESSAGE "Fam¡lia NÆo Cadastrada..." VIEW-AS ALERT-BOX.
     APPLY 'entry' TO SELF.
     RETURN NO-APPLY.
  END.
  ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo[8] w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-it-codigo[8] IN FRAME f-pg-sel
DO:
  {include/zoomvar.i &prog-zoom=inzoom/z01in172.w
                     &campo=fi-it-codigo[8]
                     &campozoom=it-codigo
                     &FRAME=f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo[8] w-relat
ON VALUE-CHANGED OF fi-it-codigo[8] IN FRAME f-pg-sel
DO:
  IF SELF:SCREEN-VALUE <> "" THEN DO.
     FIND item WHERE
          item.it-codigo = INPUT FRAME {&FRAME-NAME} fi-it-codigo[8] NO-LOCK NO-ERROR.
     IF AVAIL item THEN 
        ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
     ELSE
        ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-it-codigo[9]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo[9] w-relat
ON ENTRY OF fi-it-codigo[9] IN FRAME f-pg-sel
DO:
  IF SELF:SCREEN-VALUE <> "" THEN DO.
     FIND item WHERE
          item.it-codigo = INPUT FRAME {&FRAME-NAME} fi-it-codigo[9]
          NO-LOCK NO-ERROR.
     IF AVAIL item THEN
        ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
  END.
  ELSE ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo[9] w-relat
ON F5 OF fi-it-codigo[9] IN FRAME f-pg-sel
DO:
  APPLY 'mouse-select-dblclick' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo[9] w-relat
ON LEAVE OF fi-it-codigo[9] IN FRAME f-pg-sel
DO:
  FIND item WHERE
       item.it-codigo = INPUT FRAME {&FRAME-NAME} fi-it-codigo[9] NO-LOCK NO-ERROR.
  IF NOT AVAIL item THEN DO.
     MESSAGE "Fam¡lia NÆo Cadastrada..." VIEW-AS ALERT-BOX.
     APPLY 'entry' TO SELF.
     RETURN NO-APPLY.
  END.
  ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo[9] w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-it-codigo[9] IN FRAME f-pg-sel
DO:
  {include/zoomvar.i &prog-zoom=inzoom/z01in172.w
                     &campo=fi-it-codigo[9]
                     &campozoom=it-codigo
                     &FRAME=f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo[9] w-relat
ON VALUE-CHANGED OF fi-it-codigo[9] IN FRAME f-pg-sel
DO:
  IF SELF:SCREEN-VALUE <> "" THEN DO.
     FIND item WHERE
          item.it-codigo = INPUT FRAME {&FRAME-NAME} fi-it-codigo[9] NO-LOCK NO-ERROR.
     IF AVAIL item THEN 
        ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
     ELSE
        ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME fi-perc-min
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-perc-min w-relat
ON LEAVE OF fi-perc-min IN FRAME f-pg-par /* % M¡nimo Reserva */
DO:
  IF INPUT FRAME {&FRAME-NAME} fi-perc-min > 100 THEN DO.
     MESSAGE "Percentual Inv lido, superior a 100%" VIEW-AS ALERT-BOX.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-perc-min w-relat
ON VALUE-CHANGED OF fi-perc-min IN FRAME f-pg-par /* % M¡nimo Reserva */
DO:
  ASSIGN sl-perc-min:SCREEN-VALUE IN FRAME {&FRAME-NAME} = INPUT FRAME {&frame-name} fi-perc-min.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME im-pg-cla
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-cla w-relat
ON MOUSE-SELECT-CLICK OF im-pg-cla IN FRAME f-relat
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME im-pg-dig
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-dig w-relat
ON MOUSE-SELECT-CLICK OF im-pg-dig IN FRAME f-relat
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME im-pg-imp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-imp w-relat
ON MOUSE-SELECT-CLICK OF im-pg-imp IN FRAME f-relat
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME im-pg-par
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-par w-relat
ON MOUSE-SELECT-CLICK OF im-pg-par IN FRAME f-relat
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME im-pg-sel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-sel w-relat
ON MOUSE-SELECT-CLICK OF im-pg-sel IN FRAME f-relat
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-imp
&Scoped-define SELF-NAME rs-destino
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-destino w-relat
ON MOUSE-SELECT-CLICK OF rs-destino IN FRAME f-pg-imp
DO:
  APPLY 'value-changed' TO SELF.
  IF SELF:SCREEN-VALUE = '2' THEN DO.
     ASSIGN c-arquivo:SCREEN-VALUE = "N:\COLETOR\ENVIAR\PEDIDO" + 
                                     INPUT FRAME f-pg-sel fi-nr-coletor + ".TXT".

     ASSIGN c-arquivo:SENSITIVE     = NO
            bt-arquivo:SENSITIVE    = NO
            bt-config-impr:VISIBLE  = NO.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-destino w-relat
ON VALUE-CHANGED OF rs-destino IN FRAME f-pg-imp
DO:
do  with frame f-pg-imp:
    case self:screen-value:
        when "1" then do:
            assign c-arquivo:sensitive    = no
                   bt-arquivo:visible     = no
                   bt-config-impr:visible = yes.
        end.
        when "2" then do:
            assign c-arquivo:sensitive     = yes
                   bt-arquivo:visible      = yes
                   bt-config-impr:visible  = no.
        end.
        when "3" then do:
            assign c-arquivo:sensitive     = no
                   bt-arquivo:visible      = no
                   bt-config-impr:visible  = no.
        end.
    end case.
end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-execucao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-execucao w-relat
ON VALUE-CHANGED OF rs-execucao IN FRAME f-pg-imp
DO:
   {include/i-rprse.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME sl-perc-min
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl-perc-min w-relat
ON VALUE-CHANGED OF sl-perc-min IN FRAME f-pg-par
DO:
  ASSIGN fi-perc-min:SCREEN-VALUE IN FRAME {&FRAME-NAME} = sl-perc-min:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-sel
&Scoped-define SELF-NAME tg-exc-indigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-exc-indigo w-relat
ON VALUE-CHANGED OF tg-exc-indigo IN FRAME f-pg-sel /* Exceto Öndigo */
DO:
  IF INPUT FRAME {&FRAME-NAME} tg-exc-indigo THEN 
     ASSIGN tg-so-indigo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "no". 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-it-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-it-codigo w-relat
ON VALUE-CHANGED OF tg-it-codigo IN FRAME f-pg-sel /* Todos os Itens */
DO:
  IF INPUT FRAME {&FRAME-NAME} tg-it-codigo THEN DO:
     ASSIGN fi-it-codigo[1]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
            fi-it-codigo[2]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
            fi-it-codigo[3]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
            fi-it-codigo[4]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
            fi-it-codigo[5]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
            fi-it-codigo[6]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
            fi-it-codigo[7]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
            fi-it-codigo[8]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
            fi-it-codigo[9]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
            fi-it-codigo[10]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
            fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".

     DISABLE {&list-4} WITH FRAME {&FRAME-NAME}.
  END.
  ELSE DO.
     ENABLE {&list-4} WITH FRAME {&FRAME-NAME}.

     APPLY 'entry' TO fi-it-codigo[1] IN FRAME {&FRAME-NAME}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME tg-reservados
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-reservados w-relat
ON VALUE-CHANGED OF tg-reservados IN FRAME f-pg-par /* Itens Reservados */
DO:
   ASSIGN fi-perc-min:SENSITIVE IN FRAME f-pg-par = INPUT FRAME {&FRAME-NAME} tg-reservados.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-sel
&Scoped-define SELF-NAME tg-so-indigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-so-indigo w-relat
ON VALUE-CHANGED OF tg-so-indigo IN FRAME f-pg-sel /* Somente Öndigo */
DO:
  IF INPUT FRAME {&FRAME-NAME} tg-so-indigo THEN 
       ASSIGN tg-exc-indigo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "no". 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME tg-tp-pedido
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-tp-pedido w-relat
ON VALUE-CHANGED OF tg-tp-pedido IN FRAME f-pg-par /* Todos os Tipos de Pedido */
DO:
  IF INPUT FRAME {&FRAME-NAME} tg-tp-pedido THEN 
     ASSIGN fi-tp-pedido:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
            fi-tp-pedido:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
  ELSE DO.
     ASSIGN fi-tp-pedido:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
     APPLY 'entry' TO fi-tp-pedido IN FRAME {&FRAME-NAME}.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-cla
&Scoped-define BROWSE-NAME br-digita
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-relat 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{utp/ut9000.i "ESPD0002" "2.04.00.000"}

/*:T inicializa‡äes do template de relat¢rio */
{include/i-rpini.i}

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

{include/i-rplbl.i}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO  ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    RUN enable_UI.
  
    {include/i-rpmbl.i}
    {esinc/i-chlbl.i "im-pg-dig" "Marca‡Æo"}
    
    FOR EACH tt-pedidos.
        DELETE tt-pedidos.
    END.
        
    ASSIGN fi-fin-dt-emissao:SCREEN-VALUE IN FRAME f-pg-sel = STRING(TODAY).
    ASSIGN btn-marca:VISIBLE IN FRAME f-pg-dig = NO.
    
    fi-ini-cod-emit:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-fin-cod-emit:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-ini-no-ab-reppri:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-fin-no-ab-reppri:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-ini-nome-transp:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-fin-nome-transp:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-ini-corte-comerc:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-fin-corte-comerc:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-it-codigo[1]:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-it-codigo[2]:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-it-codigo[3]:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-it-codigo[4]:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-it-codigo[5]:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-it-codigo[6]:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-it-codigo[7]:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-it-codigo[8]:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-it-codigo[9]:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-it-codigo[10]:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.

    IF  NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-relat  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-relat  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-relat  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-relat)
  THEN DELETE WIDGET w-relat.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-relat  _DEFAULT-ENABLE
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
  ENABLE bt-coletor im-pg-cla im-pg-dig im-pg-imp im-pg-par im-pg-sel 
         bt-executar bt-carrega bt-cancelar bt-ajuda 
      WITH FRAME f-relat IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}
  DISPLAY fi-nr-pedcli fi-tp-pedido-dig fi-cod-emit fi-nome-abrev 
          fi-desc-cond-pagto fi-no-ab-repres tg-ind-fat-par fi-nr-pedrep 
          fi-situacao fi-perc-pronto fi-dt-emissao fi-sit-aval fi-desc-bloq-cr 
          fi-cod-priori fi-dt-entrega fi-it-ares fi-no-ab-transp ed-observ 
          fi-qt-reserva fi-qt-aberto fi-atendido 
      WITH FRAME f-pg-dig IN WINDOW w-relat.
  ENABLE btn_carrega btn-marca RECT-15 fi-nr-pedcli btn_vapra fi-desc-bloq-cr 
         ed-observ br-digita 
      WITH FRAME f-pg-dig IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-dig}
  DISPLAY tg-at-total rs-cond-pagto rs-cond-credito tg-abertos tg-at-parcial 
          tg-pendentes tg-suspensos cb-parcial tg-reservados tg-cancelados 
          fi-qtd-minima fi-perc-min rs-mercado sl-perc-min tg-tp-pedido 
          fi-min-it-ares fi-max-it-ares fi-tp-pedido 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  ENABLE IMAGE-11 IMAGE-12 RECT-17 RECT-18 RECT-19 RECT-20 RECT-21 RECT-22 
         RECT-23 RECT-25 tg-at-total rs-cond-pagto rs-cond-credito tg-abertos 
         tg-at-parcial tg-pendentes tg-suspensos cb-parcial tg-reservados 
         tg-cancelados fi-qtd-minima fi-perc-min rs-mercado tg-tp-pedido 
         fi-min-it-ares fi-max-it-ares 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-par}
  DISPLAY rs-destino c-arquivo rs-execucao 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  ENABLE RECT-16 RECT-7 RECT-9 rs-destino bt-arquivo bt-config-impr c-arquivo 
         rs-execucao 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}
  DISPLAY rs-classif 
      WITH FRAME f-pg-cla IN WINDOW w-relat.
  ENABLE RECT-10 RECT-12 rs-classif 
      WITH FRAME f-pg-cla IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-cla}
  DISPLAY fi-ini-cod-estabel fi-fin-cod-estabel fi-ini-pedido fi-fin-pedido 
          fi-ini-dt-emissao fi-fin-dt-emissao fi-ini-dt-entrega 
          fi-fin-dt-entrega fi-ini-cod-emit fi-fin-cod-emit fi-ini-no-ab-reppri 
          fi-fin-no-ab-reppri fi-ini-nome-transp fi-fin-nome-transp 
          fi-ini-corte-comerc fi-fin-corte-comerc fi-ini-grupo-rep 
          fi-fin-grupo-rep tg-it-codigo fi-it-codigo[1] fi-it-codigo[2] 
          fi-it-codigo[3] fi-it-codigo[4] fi-it-codigo[5] fi-nr-coletor 
          tg-so-indigo fi-it-codigo[6] fi-it-codigo[7] fi-it-codigo[8] 
          fi-it-codigo[9] fi-it-codigo[10] tg-exc-indigo fi-desc-item 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  ENABLE IMAGE-1 IMAGE-10 IMAGE-2 IMAGE-3 IMAGE-31 IMAGE-32 IMAGE-37 IMAGE-38 
         IMAGE-4 IMAGE-5 IMAGE-6 IMAGE-7 IMAGE-8 IMAGE-9 RECT-14 RECT-24 
         RECT-26 IMAGE-39 IMAGE-40 IMAGE-41 IMAGE-42 fi-ini-cod-estabel 
         fi-fin-cod-estabel fi-ini-pedido fi-fin-pedido fi-ini-dt-emissao 
         fi-fin-dt-emissao fi-ini-dt-entrega fi-fin-dt-entrega fi-ini-cod-emit 
         fi-fin-cod-emit fi-ini-no-ab-reppri fi-fin-no-ab-reppri 
         fi-ini-nome-transp fi-fin-nome-transp fi-ini-corte-comerc 
         fi-fin-corte-comerc fi-ini-grupo-rep fi-fin-grupo-rep tg-it-codigo 
         fi-nr-coletor tg-so-indigo tg-exc-indigo 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}
  VIEW w-relat.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-relat 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-carrega-dados w-relat 
PROCEDURE pi-carrega-dados :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FIND FIRST tt-param NO-ERROR.
    IF NOT AVAIL tt-param THEN
       CREATE tt-param.

    ASSIGN tt-param.usuario          = c-seg-usuario
           tt-param.destino          = input frame f-pg-imp rs-destino
           tt-param.data-exec        = today
           tt-param.hora-exec        = time
           tt-param.classifica       = input frame f-pg-cla rs-classif
           tt-param.estabel-ini      = INPUT FRAME f-pg-sel fi-ini-cod-estabel
           tt-param.estabel-fin      = INPUT FRAME f-pg-sel fi-fin-cod-estabel
           tt-param.pedido-ini       = input frame f-pg-sel fi-ini-pedido
           tt-param.pedido-fin       = input frame f-pg-sel fi-fin-pedido      
           tt-param.dt-emissao-ini   = input frame f-pg-sel fi-ini-dt-emissao  
           tt-param.dt-emissao-fin   = input frame f-pg-sel fi-fin-dt-emissao
           tt-param.dt-entrega-ini   = input frame f-pg-sel fi-ini-dt-entrega
           tt-param.dt-entrega-fin   = input frame f-pg-sel fi-fin-dt-entrega  
           tt-param.cod-emit-ini     = input frame f-pg-sel fi-ini-cod-emit    
           tt-param.cod-emit-fin     = input frame f-pg-sel fi-fin-cod-emit    
           tt-param.no-ab-reppri-ini = input frame f-pg-sel fi-ini-no-ab-reppri
           tt-param.no-ab-reppri-fin = input frame f-pg-sel fi-fin-no-ab-reppri
           tt-param.so-indigo        = INPUT FRAME f-pg-sel tg-so-indigo
           tt-param.exc-indigo       = INPUT FRAME f-pg-sel tg-exc-indigo
           tt-param.nome-transp-ini  = INPUT FRAME f-pg-sel fi-ini-nome-transp
           tt-param.nome-transp-fin  = INPUT FRAME f-pg-sel fi-fin-nome-transp
           tt-param.corte-comerc-ini = INPUT FRAME f-pg-sel fi-ini-corte-comerc
           tt-param.corte-comerc-fin = INPUT FRAME f-pg-sel fi-fin-corte-comerc
           tt-param.grupo-rep-ini    = INPUT FRAME f-pg-sel fi-ini-grupo-rep
           tt-param.grupo-rep-fin    = INPUT FRAME f-pg-sel fi-fin-grupo-rep
           tt-param.it-codigo[1]     = input frame f-pg-sel fi-it-codigo[1]
           tt-param.it-codigo[2]     = input frame f-pg-sel fi-it-codigo[2]
           tt-param.it-codigo[3]     = input frame f-pg-sel fi-it-codigo[3]
           tt-param.it-codigo[4]     = input frame f-pg-sel fi-it-codigo[4]
           tt-param.it-codigo[5]     = input frame f-pg-sel fi-it-codigo[5]
           tt-param.it-codigo[6]     = input frame f-pg-sel fi-it-codigo[6]
           tt-param.it-codigo[7]     = input frame f-pg-sel fi-it-codigo[7]
           tt-param.it-codigo[8]     = input frame f-pg-sel fi-it-codigo[8]
           tt-param.it-codigo[9]     = input frame f-pg-sel fi-it-codigo[9]
           tt-param.it-codigo[10]    = input frame f-pg-sel fi-it-codigo[10]
           tt-param.sit-total        = INPUT FRAME f-pg-par tg-at-total
           tt-param.sit-aberto       = INPUT FRAME f-pg-par tg-abertos
           tt-param.sit-parcial      = INPUT FRAME f-pg-par tg-at-parcial
           tt-param.sit-pendentes    = INPUT FRAME f-pg-par tg-pendentes
           tt-param.sit-suspensos    = INPUT FRAME f-pg-par tg-suspensos
           tt-param.sit-cancelados   = INPUT FRAME f-pg-par tg-cancelados
           tt-param.cond-credito     = IF INPUT FRAME f-pg-par rs-cond-credito = 1
                                       THEN "A"
                                       ELSE IF INPUT FRAME f-pg-par rs-cond-credito = 2
                                            THEN "N" ELSE "T"
           tt-param.cond-pagto       = IF INPUT FRAME f-pg-par rs-cond-pagto = 1
                                       THEN "V"
                                       ELSE IF INPUT FRAME f-pg-par rs-cond-pagto = 2
                                            THEN "P" ELSE "T"
           tt-param.mercado          = IF INPUT FRAME f-pg-par rs-mercado = 1
                                       THEN "I"
                                       ELSE IF INPUT FRAME f-pg-par rs-mercado = 2
                                            THEN "E" ELSE "A"
           tt-param.tp-pedido        = INPUT FRAME f-pg-par fi-tp-pedido
           tt-param.aceita-parc      = SUBSTR(INPUT FRAME f-pg-par cb-parcial,1,1)
           tt-param.qtd-minima       = INPUT FRAME f-pg-par fi-qtd-minima
           tt-param.perc-minres      = INPUT FRAME f-pg-par fi-perc-min
           tt-param.min-it-ares      = INPUT FRAME f-pg-par fi-min-it-ares
           tt-param.max-it-ares      = INPUT FRAME f-pg-par fi-max-it-ares
           tt-param.it-reservados    = INPUT FRAME f-pg-par tg-reservados.

    SESSION:SET-WAIT-STATE("general":U).
    {include/i-rprun.i esrp/espd0002arp.p}
    SESSION:SET-WAIT-STATE("":U).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-display-fields w-relat 
PROCEDURE pi-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   CLEAR FRAME f-pg-dig ALL.
   ASSIGN btn-marca:VISIBLE IN FRAME f-pg-dig = NO.

   IF AVAIL tt-pedidos THEN DO:
       ASSIGN btn-marca:VISIBLE IN FRAME f-pg-dig = YES.

       IF tt-pedidos.marca = YES THEN
          btn-marca:LOAD-IMAGE("image/im-chck1.bmp") IN FRAME f-pg-dig.
       ELSE
          btn-marca:LOAD-IMAGE("image/im-can.bmp") IN FRAME f-pg-dig.
    
       FIND ped-venda WHERE
            ped-venda.nr-pedcli = tt-pedidos.nr-pedcli NO-LOCK NO-ERROR.
       
       FIND emitente WHERE
            emitente.cod-emitente = ped-venda.cod-emitente NO-LOCK NO-ERROR.
    
       FIND cond-pagto WHERE 
            cond-pagto.cod-cond-pag = ped-venda.cod-cond-pag NO-LOCK NO-ERROR.
    
       FIND FIRST repres WHERE 
                  repres.nome-abrev = ped-venda.no-ab-reppri NO-LOCK NO-ERROR.
    
       FIND transporte WHERE 
            transporte.nome-abrev = ped-venda.nome-trans NO-LOCK NO-ERROR.

       ASSIGN fi-nr-pedcli:SCREEN-VALUE IN FRAME f-pg-dig = tt-pedidos.nr-pedcli
              fi-tp-pedido-dig:SCREEN-VALUE IN FRAME f-pg-dig = ped-venda.tp-pedido 
              fi-cod-emit:SCREEN-VALUE IN FRAME f-pg-dig = STRING(ped-venda.cod-emitente)
              fi-nome-abrev:SCREEN-VALUE IN FRAME f-pg-dig = emitente.nome-abrev
              fi-desc-cond-pagto:SCREEN-VALUE IN FRAME f-pg-dig = IF AVAIL cond-pagto
                                                                  THEN STRING(cond-pagto.cod-cond-pag,"999") + "-" +
                                                                       cond-pagto.descricao
                                                                  ELSE "E S P E C I A L"
              fi-no-ab-repres:SCREEN-VALUE IN FRAME f-pg-dig = repres.nome-abrev
              fi-nr-pedrep:SCREEN-VALUE IN FRAME f-pg-dig = ped-venda.nr-pedrep
              tg-ind-fat-par:SCREEN-VALUE IN FRAME f-pg-dig = STRING(ped-venda.ind-fat-par).
              
       {esinc/i-dsrb.i ped-venda.cod-sit-ped ped-venda.cod-sit-ped c-desc-rb}.
       ASSIGN fi-situacao:SCREEN-VALUE IN FRAME f-pg-dig = c-desc-rb.

       {esinc/i-dsrb.i ped-venda.cod-sit-aval ped-venda.cod-sit-aval c-desc-rb}.
       ASSIGN fi-sit-aval:SCREEN-VALUE IN FRAME f-pg-dig = c-desc-rb.

       ASSIGN fi-dt-emissao:SCREEN-VALUE IN FRAME f-pg-dig = STRING(ped-venda.dt-emissao,"99/99/9999")
              fi-dt-entrega:SCREEN-VALUE IN FRAME f-pg-dig = STRING(ped-venda.dt-entrega,"99/99/9999")
              fi-cod-priori:SCREEN-VALUE IN FRAME f-pg-dig = STRING(ped-venda.cod-priori)
              fi-no-ab-transp:SCREEN-VALUE IN FRAME f-pg-dig = transporte.nome-abrev WHEN AVAIL transporte
              fi-qt-aberto:SCREEN-VALUE IN FRAME f-pg-dig = STRING(tt-pedidos.qt-aberto,">>>,>>>,>>9.99")
              fi-perc-pronto:SCREEN-VALUE IN FRAME f-pg-dig = STRING(tt-pedidos.perc-pronto,">>9.99")  
              fi-qt-reserva:SCREEN-VALUE IN FRAME f-pg-dig = STRING(tt-pedidos.qt-reserva,">>>,>>>,>>9.99")   
              fi-it-ares:SCREEN-VALUE IN FRAME f-pg-dig = STRING(tt-pedidos.it-ares,">>>9")
              fi-atendido:SCREEN-VALUE IN FRAME f-pg-dig = tt-pedidos.atendido
              fi-desc-bloq-cr:SCREEN-VALUE IN FRAME f-pg-dig = IF ped-venda.cod-sit-aval = 4 
                                                               THEN ped-venda.desc-bloq-cr
                                                               ELSE ""
              ed-observ:SCREEN-VALUE IN FRAME f-pg-dig = ped-venda.observacoes.

       FOR EACH tt-digita.
           DELETE tt-digita.
       END.
                                                                                         
       FOR EACH ped-item OF ped-venda NO-LOCK.
           FIND ITEM WHERE
                ITEM.it-codigo = ped-item.it-codigo NO-LOCK NO-ERROR.
    
           FIND ped-item-res WHERE
                ped-item-res.nome-abrev   = ped-item.nome-abrev AND
                ped-item-res.nr-pedcli    = ped-item.nr-pedcli  AND
                ped-item-res.it-codigo    = ped-item.it-codigo  AND
                ped-item-res.cod-refer    = ped-item.cod-refer  AND
                ped-item-res.nr-sequencia = ped-item.nr-sequencia
               NO-LOCK NO-ERROR.

           /*-- Verifica Corte Comercial --*/
           FIND ped-item-ext OF ped-item NO-LOCK NO-ERROR.
           IF AVAIL ped-item-ext THEN DO.
              IF ped-item-ext.corte-comerc < tt-param.corte-comerc-ini OR
                 ped-item-ext.corte-comerc > tt-param.corte-comerc-fin THEN
                 NEXT.
           END.
    
           FIND ITEM WHERE ITEM.it-codigo = ped-item.it-codigo NO-LOCK NO-ERROR.
           IF AVAIL ITEM THEN DO:
              FIND item-ext WHERE item-ext.it-codigo = ITEM.it-codigo 
                               NO-LOCK NO-ERROR.
              IF AVAIL item-ext THEN
                 IF (item-ext.indigo = NO  AND tt-param.so-indigo = YES) OR
                    (item-ext.indigo = YES AND tt-param.exc-indigo = yes) THEN
                    NEXT.
           END.

           ASSIGN l-continua = YES.
           DO i-cont = 1 TO EXTENT(tt-param.it-codigo).
              IF tt-param.it-codigo[i-cont] <> "" THEN DO:
                 IF ITEM.it-codigo = tt-param.it-codigo[i-cont] THEN DO.
                    ASSIGN l-continua = YES.
                    LEAVE.
                 END.
                 ASSIGN l-continua = NO.
              END.
           END.
           IF NOT l-continua THEN NEXT.

           if  ped-item.cod-sit-item = 3 
           and tt-param.sit-total = no then next.

           if  tt-param.sit-aberto = no
           and ped-item.cod-sit-item = 1 then next.

           if  tt-param.sit-parcial = no
           and ped-item.cod-sit-item = 2  then next.

           if  tt-param.sit-pendentes = no
           and ped-item.cod-sit-item = 4 then next.

           if  tt-param.sit-suspensos = no
           and ped-item.cod-sit-item = 5 then next.

           if  tt-param.sit-cancelados = no
           and ped-item.cod-sit-item = 6 then next.

           /* Verifica Programacao */
           find ref-item-ext where 
                ref-item-ext.it-codigo = item.it-codigo AND
                ref-item-ext.cod-refer = ped-item.cod-refer no-lock no-error.

           if not avail ref-item-ext then
              assign c-sit-prog = "N N N".
           else do:
              if ref-item-ext.qtd-prog <> 0 then
                 assign c-sit-prog = "S ".
              else
                 assign c-sit-prog = "N ".
              if ref-item-ext.qtd-proc <> 0 then
                 assign c-sit-prog = c-sit-prog + "S ".
              else
                 assign c-sit-prog = c-sit-prog + "N ".
              if ref-item-ext.qtd-pron <> 0 then
                 assign c-sit-prog = c-sit-prog + "S".
              else
                 assign c-sit-prog = c-sit-prog + "N".
           end.

           CREATE tt-digita.
           ASSIGN tt-digita.nr-sequencia = ped-item.nr-sequencia
                  tt-digita.it-codigo    = ped-item.it-codigo
                  tt-digita.cod-refer    = ped-item.cod-refer
                  tt-digita.desc-item    = ITEM.desc-item
                  tt-digita.qt-pedida    = ped-item.qt-pedida
                  tt-digita.qt-reserva   = ped-item-res.qt-pedida WHEN AVAIL ped-item-res
                  tt-digita.sit-prog     = c-sit-prog.
       END.
   END.
   {&OPEN-QUERY-br-digita}
   RUN pi-habilita-botoes.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-executar w-relat 
PROCEDURE pi-executar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define var r-tt-digita as rowid no-undo.

do on error undo, return error on stop  undo, return error:
    {include/i-rpexa.i}
    
    FIND FIRST tt-pedidos WHERE
               tt-pedidos.marca = YES NO-LOCK NO-ERROR.

    IF NOT AVAIL tt-pedidos THEN 
       RUN pi-carrega-dados.

    FIND FIRST tt-pedidos NO-ERROR.
    IF NOT AVAIL tt-pedidos THEN 
       RETURN ERROR.

    if input frame f-pg-imp rs-destino = 2 and
       input frame f-pg-imp rs-execucao = 1 then do:
       run utp/ut-vlarq.p (input input frame f-pg-imp c-arquivo).
        
       if return-value = "NOK":U then do:
          run utp/ut-msgs.p (input "show":U, input 73, input "").
            
          apply "MOUSE-SELECT-CLICK":U to im-pg-imp in frame f-relat.
          apply "ENTRY":U to c-arquivo in frame f-pg-imp.
          return error.
       end.
    end.
    
    IF INPUT FRAME f-pg-imp rs-destino = 2 AND
       INPUT FRAME f-pg-sel fi-nr-coletor = 0 THEN DO.
       MESSAGE "N£mero do Coletor deve ser Informado..." VIEW-AS ALERT-BOX.
       APPLY "MOUSE-SELECT-CLICK":U TO im-pg-sel IN FRAME f-relat.
       APPLY 'entry' TO fi-nr-coletor IN FRAME f-pg-sel.
       RETURN NO-APPLY.
    END.

    /*:T Aqui sÆo gravados os campos da temp-table que ser  passada como parƒmetro
       para o programa RP.P */
    
    FIND FIRST tt-param NO-ERROR.

    ASSIGN tt-param.usuario   = c-seg-usuario
           tt-param.destino   = INPUT FRAME f-pg-imp rs-destino
           tt-param.data-exec = TODAY 
           tt-param.hora-exec = TIME.

    if tt-param.destino = 1 
    then assign tt-param.arquivo = "".
    else if  tt-param.destino = 2 
         then assign tt-param.arquivo = input frame f-pg-imp c-arquivo.
         else assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp":U.
    
    /*:T Coloque aqui a l¢gica de grava‡Æo dos demais campos que devem ser passados
       como parƒmetros para o programa RP.P, atrav‚s da temp-table tt-param */
    ASSIGN tt-param.nr-coletor = INPUT FRAME f-pg-sel fi-nr-coletor.


    IF tt-pedidos.atendido = "Sep. Avulsa" THEN
       ASSIGN tt-param.arquivo = "N:\Coletor\Enviar\AVULSA.TXT".
    
    /*:T Executar do programa RP.P que ir  criar o relat¢rio */
    {include/i-rpexb.i}
    
    SESSION:SET-WAIT-STATE("general":U).
    
    {include/i-rprun.i esrp/espd0002rp.p}

    FOR EACH tt-pedidos.
        DELETE tt-pedidos.
    END.
    FOR EACH tt-digita.
        DELETE tt-digita.
    END.
    RUN pi-display-fields.

    {include/i-rpexc.i}
    
    SESSION:SET-WAIT-STATE("":U).
    
    {include/i-rptrm.i}
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-habilita-botoes w-relat 
PROCEDURE pi-habilita-botoes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DISABLE {&list-1} {&list-2} {&list-3} WITH FRAME f-pg-dig.
    
    IF CAN-FIND(FIRST tt-pedidos WHERE
                      tt-pedidos.nr-pedcli > INPUT FRAME f-pg-dig fi-nr-pedcli) THEN
       ENABLE {&list-2} {&list-3} WITH FRAME f-pg-dig.

    IF CAN-FIND(FIRST tt-pedidos WHERE
                      tt-pedidos.nr-pedcli < INPUT FRAME f-pg-dig fi-nr-pedcli) THEN
       ENABLE {&list-1} {&list-3} WITH FRAME f-pg-dig.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-troca-pagina w-relat 
PROCEDURE pi-troca-pagina :
/*:T------------------------------------------------------------------------------
  Purpose: Gerencia a Troca de P gina (folder)   
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

{include/i-rptrp.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-relat  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-digita"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-relat 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
  
  run pi-trata-state (p-issuer-hdl, p-state).
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

