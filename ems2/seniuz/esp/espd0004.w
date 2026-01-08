&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
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
{include/i-prgvrs.i ESPD0004 2.04.00.000}

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
&GLOBAL-DEFINE PGDIG 
&GLOBAL-DEFINE PGIMP f-pg-imp
  
/* Parameters Definitions ---                                           */

/* Temporary Table Definitions ---                                      */

define temp-table tt-param no-undo
       field destino            as integer
       field arquivo            as char format "x(35)"
       field usuario            as char format "x(12)"
       field data-exec          as date
       field hora-exec          as integer
       FIELD c-pedido-ini       like ped-item.nr-pedcli
       FIELD c-pedido-fim       like ped-item.nr-pedcli
       FIELD c-item-ini         LIKE ped-item.it-codigo
       FIELD c-item-fim         LIKE ped-item.it-codigo
       FIELD c-ref-ini          LIKE ped-item.cod-refer
       FIELD c-ref-fim          LIKE ped-item.cod-refer
       FIELD da-entr-ini        like ped-item.dt-entrega  
       FIELD da-entr-fim        like ped-item.dt-entrega  
       FIELD da-impl-ini        like ped-venda.dt-implant 
       FIELD da-impl-fim        like ped-venda.dt-implant 
       FIELD c-acond-ini        like ped-item.cod-refer
       FIELD c-acond-fim        like ped-item.cod-refer
       FIELD corte-comerc-ini   LIKE ped-item-ext.corte-comerc
       FIELD corte-comerc-fim   LIKE ped-item-ext.corte-comerc
       FIELD l-apaga-todas      AS LOG
       FIELD c-cod-refer1       LIKE ped-item.cod-refer
       FIELD c-cod-refer2       LIKE ped-item.cod-refer
       FIELD c-cod-refer3       LIKE ped-item.cod-refer
       FIELD c-cod-refer4       LIKE ped-item.cod-refer
       FIELD c-cod-refer5       LIKE ped-item.cod-refer
       FIELD c-cod-refer6       LIKE ped-item.cod-refer
       FIELD c-cod-refer7       LIKE ped-item.cod-refer
       FIELD c-cod-refer8       LIKE ped-item.cod-refer
       FIELD c-cod-refer9       LIKE ped-item.cod-refer
       FIELD c-cod-refer10      LIKE ped-item.cod-refer
       FIELD l-sit-aberto       as log  format "Sim/NÆo"
       FIELD l-sit-parcial      as log  format "Sim/NÆo"
       FIELD l-sit-pendentes    as log  format "Sim/NÆo"
       FIELD l-sit-suspensos    as log  format "Sim/NÆo"
       FIELD l-sit-cancelados   as log  format "Sim/NÆo"
       FIELD l-sit-so-aprovados as log  format "Sim/NÆo"
       FIELD l-res-nao          as log  format "Sim/NÆo"
       FIELD l-res-sim          as log  format "Sim/NÆo"
       FIELD l-res-parc         as log  format "Sim/NÆo"
       FIELD l-res-aceita       as log  format "Sim/NÆo"
       FIELD l-queb-desenho     AS LOG  FORMAT "Sim/NÆo"
       FIELD c-saldo-estoq      AS CHAR FORMAT "x"
       FIELD c-classificacao    AS CHAR FORMAT "x(20)"
       FIELD impr-param         AS LOGICAL
       FIELD nr-coletor         AS INT.

define temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id ordem.

define buffer b-tt-digita for tt-digita.

/* Transfer Definitions */

def var raw-param        as raw no-undo.

def temp-table tt-raw-digita
   field raw-digita      as raw.
                    
/* Local Variable Definitions ---                                       */

def var l-ok               as logical no-undo.
def var c-arq-digita       as char    no-undo.
def var c-terminal         as char    no-undo.

DEF VAR c-arq-bat AS CHAR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-relat
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME f-pg-cla

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rs-classif RECT-10 RECT-28 
&Scoped-Define DISPLAYED-OBJECTS rs-classif 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 fi-cod-refer1 fi-cod-refer2 fi-cod-refer3 ~
fi-cod-refer4 fi-cod-refer5 fi-cod-refer7 fi-cod-refer6 fi-cod-refer8 ~
fi-cod-refer9 fi-cod-refer10 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-relat AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE rs-classif AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Numero do Pedido", 1,
"Transportador", 2
     SIZE 27.86 BY 2.71 NO-UNDO.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 36 BY 5.

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 76.86 BY 10.63.

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

DEFINE RECTANGLE RECT-31
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 76.86 BY 10.63.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 46.29 BY 2.92.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 46.29 BY 1.71.

DEFINE VARIABLE rs-saldo-estoq AS INTEGER INITIAL 3 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Positivo", 1,
"Negativo", 2,
"Ambos", 3
     SIZE 12 BY 2.5 NO-UNDO.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 23 BY 5.17.

DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 41.86 BY 1.71.

DEFINE RECTANGLE RECT-27
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 42 BY 3.08.

DEFINE RECTANGLE RECT-29
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 42 BY 3.

DEFINE RECTANGLE RECT-32
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 23 BY 3.08.

DEFINE RECTANGLE RECT-33
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 76.86 BY 10.63.

DEFINE VARIABLE tg-abertos AS LOGICAL INITIAL yes 
     LABEL "Abertos" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .58 NO-UNDO.

DEFINE VARIABLE tg-at-parcial AS LOGICAL INITIAL yes 
     LABEL "Atendidos Parcial" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .58 NO-UNDO.

DEFINE VARIABLE tg-cancelados AS LOGICAL INITIAL no 
     LABEL "Cancelados" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .58 NO-UNDO.

DEFINE VARIABLE tg-impr-param AS LOGICAL INITIAL no 
     LABEL "Imprime Parƒmetros" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .83 NO-UNDO.

DEFINE VARIABLE tg-pendentes AS LOGICAL INITIAL yes 
     LABEL "Pendentes" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .58 NO-UNDO.

DEFINE VARIABLE tg-res-aceita AS LOGICAL INITIAL yes 
     LABEL "Aceita Parcial" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.14 BY .83 NO-UNDO.

DEFINE VARIABLE tg-res-nao AS LOGICAL INITIAL yes 
     LABEL "Nao Reservados" 
     VIEW-AS TOGGLE-BOX
     SIZE 16.86 BY .83 NO-UNDO.

DEFINE VARIABLE tg-res-parc AS LOGICAL INITIAL yes 
     LABEL "Reservados Parcial" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .83 NO-UNDO.

DEFINE VARIABLE tg-res-sim AS LOGICAL INITIAL no 
     LABEL "Reservados Total" 
     VIEW-AS TOGGLE-BOX
     SIZE 15.86 BY .83 NO-UNDO.

DEFINE VARIABLE tg-salta AS LOGICAL INITIAL yes 
     LABEL "Salta P gina por Desenho / Cor" 
     VIEW-AS TOGGLE-BOX
     SIZE 32.43 BY .83 NO-UNDO.

DEFINE VARIABLE tg-sit-so-aprovados AS LOGICAL INITIAL no 
     LABEL "Credito Aprovado" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .83 NO-UNDO.

DEFINE VARIABLE tg-suspensos AS LOGICAL INITIAL yes 
     LABEL "Suspensos" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .58 NO-UNDO.

DEFINE VARIABLE fi-cod-refer1 AS CHARACTER FORMAT "x(8)" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-refer10 AS CHARACTER FORMAT "x(8)" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-refer2 AS CHARACTER FORMAT "x(8)" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-refer3 AS CHARACTER FORMAT "x(8)" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-refer4 AS CHARACTER FORMAT "x(8)" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-refer5 AS CHARACTER FORMAT "x(8)" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-refer6 AS CHARACTER FORMAT "x(8)" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-refer7 AS CHARACTER FORMAT "x(8)" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-refer8 AS CHARACTER FORMAT "x(8)" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-refer9 AS CHARACTER FORMAT "x(8)" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fim-acond AS CHARACTER FORMAT "X(8)":U INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fim-cod-refer AS CHARACTER FORMAT "x(8)" INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fim-corte-comerc AS CHARACTER FORMAT "!" INITIAL "Z" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 TOOLTIP "Corte comercial final." NO-UNDO.

DEFINE VARIABLE fi-fim-dt-entrega AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fim-dt-implant AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fim-it-codigo AS CHARACTER FORMAT "x(16)" INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 17.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fim-nr-pedcli AS CHARACTER FORMAT "x(12)" INITIAL "ZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-acond AS CHARACTER FORMAT "X(8)":U 
     LABEL "Acondicionamento" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-cod-refer AS CHARACTER FORMAT "X(8)":U 
     LABEL "Referencia" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-corte-comerc AS CHARACTER FORMAT "!" 
     LABEL "Corte Comercial" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 TOOLTIP "Corte comercial inicial." NO-UNDO.

DEFINE VARIABLE fi-ini-dt-entrega AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Entrega" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-dt-implant AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Implata‡Æo" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-it-codigo AS CHARACTER FORMAT "x(16)" 
     LABEL "Item":R5 
     VIEW-AS FILL-IN 
     SIZE 17.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-nr-pedcli AS CHARACTER FORMAT "x(12)" 
     LABEL "Pedido Cliente":R17 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nr-coletor AS INTEGER FORMAT "9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 3.86 BY 1.71
     FONT 10 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-3
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-31
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-32
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-33
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-34
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-35
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-36
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-37
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-38
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-4
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-5
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-6
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 76.86 BY 10.63.

DEFINE RECTANGLE RECT-26
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 7 BY 2.58.

DEFINE RECTANGLE RECT-34
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 59 BY 2.58.

DEFINE VARIABLE tg-apaga-todas AS LOGICAL INITIAL yes 
     LABEL "Apaga" 
     VIEW-AS TOGGLE-BOX
     SIZE 8 BY .83 NO-UNDO.

DEFINE BUTTON bt-ajuda 
     LABEL "Ajuda" 
     SIZE 10 BY 1
     FONT 1.

DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "Fechar" 
     SIZE 10 BY 1
     FONT 1.

DEFINE BUTTON bt-coletor 
     IMAGE-UP FILE "image/emstec.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Exportar Dados para Coletor" 
     SIZE 9 BY 1.83 TOOLTIP "Exportar Dados para Coletor".

DEFINE BUTTON bt-executar 
     LABEL "Executar" 
     SIZE 10 BY 1
     FONT 1.

DEFINE IMAGE im-pg-cla
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
     SIZE 69 BY 1.75
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


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-pg-cla
     rs-classif AT ROW 4.21 COL 22.29 HELP
          "Classifica‡Æo para emissÆo do relat¢rio" NO-LABEL
     RECT-10 AT ROW 3.08 COL 19.29
     RECT-28 AT ROW 1 COL 1
     "  Ordenar Relat¢rio Pelo" VIEW-AS TEXT
          SIZE 17 BY .75 AT ROW 2.75 COL 20.57
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.83
         SIZE 76.86 BY 10.88
         FONT 1.

DEFINE FRAME f-pg-par
     tg-abertos AT ROW 2.54 COL 10.29
     tg-sit-so-aprovados AT ROW 2.58 COL 35.72
     tg-at-parcial AT ROW 3.46 COL 10.29
     tg-pendentes AT ROW 4.29 COL 10.29
     tg-res-nao AT ROW 4.5 COL 35.72
     tg-suspensos AT ROW 5.21 COL 10.29
     tg-res-sim AT ROW 5.29 COL 35.72
     tg-cancelados AT ROW 6.17 COL 10.29
     tg-res-parc AT ROW 6.17 COL 35.72
     tg-res-aceita AT ROW 6.17 COL 58.14
     rs-saldo-estoq AT ROW 8.17 COL 10.29 NO-LABEL
     tg-salta AT ROW 8.63 COL 35.72
     tg-impr-param AT ROW 9.67 COL 35.72
     RECT-18 AT ROW 2.04 COL 6
     RECT-20 AT ROW 2.04 COL 31.14
     RECT-27 AT ROW 7.79 COL 31
     RECT-29 AT ROW 4.21 COL 31.14
     RECT-32 AT ROW 7.79 COL 6
     RECT-33 AT ROW 1 COL 1
     " Outros" VIEW-AS TEXT
          SIZE 5.43 BY .75 AT ROW 7.46 COL 34
     " Condi‡Æo de Cr‚dito" VIEW-AS TEXT
          SIZE 16 BY .75 AT ROW 1.71 COL 33.72
     "  Situa‡Æo dos Pedidos" VIEW-AS TEXT
          SIZE 17 BY .75 AT ROW 1.71 COL 7.29
     " Resevas" VIEW-AS TEXT
          SIZE 7 BY .75 AT ROW 3.83 COL 33.86
     " Itens com Saldo Estoque" VIEW-AS TEXT
          SIZE 18 BY .75 AT ROW 7.42 COL 7
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.83
         SIZE 76.86 BY 10.83
         FONT 1.

DEFINE FRAME f-relat
     bt-coletor AT ROW 14 COL 72 HELP
          "Exportar Dados para Coletor"
     bt-ajuda AT ROW 14.42 COL 60 HELP
          "Ajuda"
     bt-executar AT ROW 14.46 COL 3 HELP
          "Dispara a execu‡Æo do relat¢rio"
     bt-cancelar AT ROW 14.46 COL 14 HELP
          "Fechar"
     rt-folder-top AT ROW 2.54 COL 2.14
     RECT-6 AT ROW 13.75 COL 2.14
     rt-folder-right AT ROW 2.67 COL 80.43
     rt-folder-left AT ROW 2.54 COL 2.14
     RECT-1 AT ROW 14.04 COL 2
     rt-folder AT ROW 2.5 COL 2
     im-pg-cla AT ROW 1.5 COL 17.72
     im-pg-imp AT ROW 1.5 COL 49.14
     im-pg-par AT ROW 1.5 COL 33.43
     im-pg-sel AT ROW 1.5 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81 BY 15
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
     RECT-31 AT ROW 1 COL 1
     RECT-7 AT ROW 1.92 COL 2.14
     RECT-9 AT ROW 5.29 COL 2.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.83
         SIZE 77.29 BY 10.83.

DEFINE FRAME f-pg-sel
     fi-ini-nr-pedcli AT ROW 1.46 COL 19 COLON-ALIGNED HELP
          "N£mero do pedido do cliente"
     fi-fim-nr-pedcli AT ROW 1.46 COL 48.43 COLON-ALIGNED HELP
          "N£mero do pedido do cliente" NO-LABEL
     fi-ini-it-codigo AT ROW 2.46 COL 19 COLON-ALIGNED HELP
          "C¢digo do Item"
     fi-fim-it-codigo AT ROW 2.46 COL 48.43 COLON-ALIGNED HELP
          "C¢digo do Item" NO-LABEL
     fi-ini-cod-refer AT ROW 3.42 COL 19 COLON-ALIGNED
     fi-fim-cod-refer AT ROW 3.46 COL 48.43 COLON-ALIGNED HELP
          "C¢digo da Referencia" NO-LABEL
     fi-ini-dt-entrega AT ROW 4.42 COL 19 COLON-ALIGNED
     fi-fim-dt-entrega AT ROW 4.42 COL 48.57 COLON-ALIGNED NO-LABEL
     fi-ini-dt-implant AT ROW 5.42 COL 19 COLON-ALIGNED
     fi-fim-dt-implant AT ROW 5.42 COL 48.57 COLON-ALIGNED NO-LABEL
     fi-ini-acond AT ROW 6.42 COL 19 COLON-ALIGNED
     fi-fim-acond AT ROW 6.42 COL 48.57 COLON-ALIGNED NO-LABEL
     fi-ini-corte-comerc AT ROW 7.42 COL 19 COLON-ALIGNED
     fi-fim-corte-comerc AT ROW 7.42 COL 48.57 COLON-ALIGNED NO-LABEL
     tg-apaga-todas AT ROW 8.58 COL 2.29
     fi-cod-refer1 AT ROW 9 COL 10.57 COLON-ALIGNED HELP
          "C¢digo de referˆncia do item" NO-LABEL
     fi-cod-refer2 AT ROW 9 COL 21.86 COLON-ALIGNED HELP
          "C¢digo de referˆncia do item" NO-LABEL
     fi-cod-refer3 AT ROW 9 COL 33.14 COLON-ALIGNED HELP
          "C¢digo de referˆncia do item" NO-LABEL
     fi-cod-refer4 AT ROW 9 COL 44.43 COLON-ALIGNED HELP
          "C¢digo de referˆncia do item" NO-LABEL
     fi-cod-refer5 AT ROW 9 COL 55.72 COLON-ALIGNED HELP
          "C¢digo de referˆncia do item" NO-LABEL
     fi-nr-coletor AT ROW 9.08 COL 69.29 COLON-ALIGNED NO-LABEL
     fi-cod-refer7 AT ROW 9.96 COL 21.86 COLON-ALIGNED HELP
          "C¢digo de referˆncia do item" NO-LABEL
     fi-cod-refer6 AT ROW 10 COL 10.57 COLON-ALIGNED HELP
          "C¢digo de referˆncia do item" NO-LABEL
     fi-cod-refer8 AT ROW 10 COL 33.14 COLON-ALIGNED HELP
          "C¢digo de referˆncia do item" NO-LABEL
     fi-cod-refer9 AT ROW 10 COL 44.43 COLON-ALIGNED HELP
          "C¢digo de referˆncia do item" NO-LABEL
     fi-cod-refer10 AT ROW 10 COL 55.72 COLON-ALIGNED HELP
          "C¢digo de referˆncia do item" NO-LABEL
     IMAGE-1 AT ROW 2.42 COL 39.43
     IMAGE-2 AT ROW 2.42 COL 47.14
     IMAGE-3 AT ROW 5.42 COL 39.57
     IMAGE-31 AT ROW 6.42 COL 39.57
     IMAGE-32 AT ROW 6.42 COL 47.14
     IMAGE-33 AT ROW 1.42 COL 39.43
     IMAGE-34 AT ROW 1.42 COL 47.14
     IMAGE-35 AT ROW 3.42 COL 39.43
     IMAGE-36 AT ROW 3.42 COL 47.14
     IMAGE-37 AT ROW 7.42 COL 39.57
     IMAGE-38 AT ROW 7.42 COL 47.14
     IMAGE-4 AT ROW 5.42 COL 47.14
     IMAGE-5 AT ROW 4.42 COL 39.57
     IMAGE-6 AT ROW 4.42 COL 47.14
     RECT-19 AT ROW 1 COL 1
     RECT-26 AT ROW 8.67 COL 70
     RECT-34 AT ROW 8.67 COL 10.57
     " Coletor" VIEW-AS TEXT
          SIZE 5.57 BY .88 AT ROW 8.17 COL 70.57
     " Referˆncias" VIEW-AS TEXT
          SIZE 9 BY .54 AT ROW 8.38 COL 12.57
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
         TITLE              = "Pedidos de Vendas p/Separa‡Æo de Mercadorias"
         HEIGHT             = 15
         WIDTH              = 81.14
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
                                                                        */
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
/* SETTINGS FOR FRAME f-pg-sel
                                                                        */
/* SETTINGS FOR FILL-IN fi-cod-refer1 IN FRAME f-pg-sel
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN fi-cod-refer10 IN FRAME f-pg-sel
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN fi-cod-refer2 IN FRAME f-pg-sel
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN fi-cod-refer3 IN FRAME f-pg-sel
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN fi-cod-refer4 IN FRAME f-pg-sel
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN fi-cod-refer5 IN FRAME f-pg-sel
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN fi-cod-refer6 IN FRAME f-pg-sel
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN fi-cod-refer7 IN FRAME f-pg-sel
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN fi-cod-refer8 IN FRAME f-pg-sel
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN fi-cod-refer9 IN FRAME f-pg-sel
   NO-ENABLE 1                                                          */
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
ON END-ERROR OF w-relat /* Pedidos de Vendas p/Separa‡Æo de Mercadorias */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON WINDOW-CLOSE OF w-relat /* Pedidos de Vendas p/Separa‡Æo de Mercadorias */
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

  ASSIGN tg-impr-param:SCREEN-VALUE IN FRAME f-pg-par = "NO"
         tg-salta:SCREEN-VALUE IN FRAME f-pg-par = "NO".

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
ON CHOOSE OF bt-executar IN FRAME f-relat /* Executar */
DO:
   do on error undo, return no-apply:
      run pi-executar.
   end.
   ASSIGN fi-nr-coletor:SCREEN-VALUE IN FRAME f-pg-sel = '0'.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-executar w-relat
ON MOUSE-SELECT-CLICK OF bt-executar IN FRAME f-relat /* Executar */
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


&Scoped-define FRAME-NAME f-pg-sel
&Scoped-define SELF-NAME fi-fim-cod-refer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fim-cod-refer w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-fim-cod-refer IN FRAME f-pg-sel
DO:
    {include/zoomvar.i &prog-zoom=inzoom/z01in172.w
                   &campo=fi-fim-it-codigo
                   &campozoom=it-codigo
                   &FRAME=f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-fim-it-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fim-it-codigo w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-fim-it-codigo IN FRAME f-pg-sel
DO:
    {include/zoomvar.i &prog-zoom=inzoom/z01in172.w
                   &campo=fi-fim-it-codigo
                   &campozoom=it-codigo
                   &FRAME=f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-fim-nr-pedcli
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fim-nr-pedcli w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-fim-nr-pedcli IN FRAME f-pg-sel
DO:
  {include/zoomvar.i &prog-zoom=dizoom/z01di159.w
                     &campo=fi-fim-nr-pedcli
                     &campozoom=nr-pedcli
                     &FRAME=f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ini-cod-refer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-cod-refer w-relat
ON LEAVE OF fi-ini-cod-refer IN FRAME f-pg-sel /* Referencia */
DO:
   IF SELF:SCREEN-VALUE <> "" THEN
      ASSIGN fi-fim-cod-refer:SCREEN-VALUE IN FRAME f-pg-sel = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ini-it-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-it-codigo w-relat
ON LEAVE OF fi-ini-it-codigo IN FRAME f-pg-sel /* Item */
DO:
   IF SELF:SCREEN-VALUE <> "" THEN
      ASSIGN fi-fim-it-codigo:SCREEN-VALUE IN FRAME f-pg-sel = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-it-codigo w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ini-it-codigo IN FRAME f-pg-sel /* Item */
DO:
    {include/zoomvar.i &prog-zoom=inzoom/z01in172.w
                     &campo=fi-ini-it-codigo
                     &campozoom=it-codigo
                     &FRAME=f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ini-nr-pedcli
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-nr-pedcli w-relat
ON LEAVE OF fi-ini-nr-pedcli IN FRAME f-pg-sel /* Pedido Cliente */
DO:
  ASSIGN fi-nr-coletor:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '0'.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-nr-pedcli w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ini-nr-pedcli IN FRAME f-pg-sel /* Pedido Cliente */
DO:
  {include/zoomvar.i &prog-zoom=dizoom/z01di159.w
                     &campo=fi-ini-nr-pedcli
                     &campozoom=nr-pedcli
                     &FRAME=f-pg-sel}
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
      ASSIGN c-arquivo:SCREEN-VALUE = "N:\COLETOR\ENVIAR\ITEM" + 
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


&Scoped-define FRAME-NAME f-pg-sel
&Scoped-define SELF-NAME tg-apaga-todas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-apaga-todas w-relat
ON VALUE-CHANGED OF tg-apaga-todas IN FRAME f-pg-sel /* Apaga */
DO:
  IF INPUT FRAME {&FRAME-NAME} tg-apaga-todas THEN DO:
     ASSIGN fi-cod-refer1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
            fi-cod-refer2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
            fi-cod-refer3:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
            fi-cod-refer4:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
            fi-cod-refer5:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
            fi-cod-refer6:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
            fi-cod-refer7:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
            fi-cod-refer8:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
            fi-cod-refer9:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
            fi-cod-refer10:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".

     DISABLE {&list-1} WITH FRAME {&FRAME-NAME}.
  END.
  ELSE DO:
     ASSIGN fi-ini-cod-refer:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
            fi-fim-cod-refer:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "ZZZZZZZZ".
     ENABLE {&list-1} WITH FRAME {&FRAME-NAME}.
     APPLY 'entry' TO fi-cod-refer1 IN FRAME {&FRAME-NAME}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME tg-res-parc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-res-parc w-relat
ON VALUE-CHANGED OF tg-res-parc IN FRAME f-pg-par /* Reservados Parcial */
DO:
  IF INPUT FRAME {&FRAME-NAME} tg-res-parc = YES THEN
     ASSIGN tg-res-aceita:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'YES'
            tg-res-aceita:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
  ELSE
     ASSIGN tg-res-aceita:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'
            tg-res-aceita:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-cla
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-relat 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{utp/ut9000.i "ESPD0004" "2.04.00.000"}

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
    fi-ini-nr-pedcli:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-fim-nr-pedcli:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-ini-it-codigo:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-fim-it-codigo:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.

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
  ENABLE bt-coletor bt-ajuda bt-executar bt-cancelar im-pg-cla im-pg-imp 
         im-pg-par im-pg-sel 
      WITH FRAME f-relat IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}
  DISPLAY rs-classif 
      WITH FRAME f-pg-cla IN WINDOW w-relat.
  ENABLE rs-classif RECT-10 RECT-28 
      WITH FRAME f-pg-cla IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-cla}
  DISPLAY rs-destino c-arquivo rs-execucao 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  ENABLE rs-destino bt-arquivo bt-config-impr c-arquivo rs-execucao RECT-31 
         RECT-7 RECT-9 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}
  DISPLAY tg-abertos tg-sit-so-aprovados tg-at-parcial tg-pendentes tg-res-nao 
          tg-suspensos tg-res-sim tg-cancelados tg-res-parc tg-res-aceita 
          rs-saldo-estoq tg-salta tg-impr-param 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  ENABLE tg-abertos tg-sit-so-aprovados tg-at-parcial tg-pendentes tg-res-nao 
         tg-suspensos tg-res-sim tg-cancelados tg-res-parc tg-res-aceita 
         rs-saldo-estoq tg-salta tg-impr-param RECT-18 RECT-20 RECT-27 RECT-29 
         RECT-32 RECT-33 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-par}
  DISPLAY fi-ini-nr-pedcli fi-fim-nr-pedcli fi-ini-it-codigo fi-fim-it-codigo 
          fi-ini-cod-refer fi-fim-cod-refer fi-ini-dt-entrega fi-fim-dt-entrega 
          fi-ini-dt-implant fi-fim-dt-implant fi-ini-acond fi-fim-acond 
          fi-ini-corte-comerc fi-fim-corte-comerc tg-apaga-todas fi-cod-refer1 
          fi-cod-refer2 fi-cod-refer3 fi-cod-refer4 fi-cod-refer5 fi-nr-coletor 
          fi-cod-refer7 fi-cod-refer6 fi-cod-refer8 fi-cod-refer9 fi-cod-refer10 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  ENABLE fi-ini-nr-pedcli fi-fim-nr-pedcli fi-ini-it-codigo fi-fim-it-codigo 
         fi-ini-cod-refer fi-fim-cod-refer fi-ini-dt-entrega fi-fim-dt-entrega 
         fi-ini-dt-implant fi-fim-dt-implant fi-ini-acond fi-fim-acond 
         fi-ini-corte-comerc fi-fim-corte-comerc tg-apaga-todas fi-nr-coletor 
         IMAGE-1 IMAGE-2 IMAGE-3 IMAGE-31 IMAGE-32 IMAGE-33 IMAGE-34 IMAGE-35 
         IMAGE-36 IMAGE-37 IMAGE-38 IMAGE-4 IMAGE-5 IMAGE-6 RECT-19 RECT-26 
         RECT-34 
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
    
    /*:T Coloque aqui as valida‡äes da p gina de Digita‡Æo, lembrando que elas devem
       apresentar uma mensagem de erro cadastrada, posicionar nesta p gina e colocar
       o focus no campo com problemas */
    /*browse br-digita:SET-REPOSITIONED-ROW (browse br-digita:DOWN, "ALWAYS":U).*/
    
    
    /*:T Coloque aqui as valida‡äes das outras p ginas, lembrando que elas devem 
       apresentar uma mensagem de erro cadastrada, posicionar na p gina com 
       problemas e colocar o focus no campo com problemas */
    
    
    
    /*:T Aqui sÆo gravados os campos da temp-table que ser  passada como parƒmetro
       para o programa RP.P */
    
    create tt-param.
    assign tt-param.usuario         = c-seg-usuario
           tt-param.destino         = input frame f-pg-imp rs-destino
           tt-param.data-exec       = today
           tt-param.hora-exec       = time.
    
    if tt-param.destino = 1 
    then assign tt-param.arquivo = "".
    else if  tt-param.destino = 2 
         then assign tt-param.arquivo = input frame f-pg-imp c-arquivo.
         else assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp":U.
    
    /*:T Coloque aqui a l¢gica de grava‡Æo dos demais campos que devem ser passados
       como parƒmetros para o programa RP.P, atrav‚s da temp-table tt-param */
    
     ASSIGN tt-param.c-pedido-ini       = INPUT FRAME f-pg-sel fi-ini-nr-pedcli
            tt-param.c-pedido-fim       = INPUT FRAME f-pg-sel fi-fim-nr-pedcli
            tt-param.c-item-ini         = INPUT FRAME f-pg-sel fi-ini-it-codigo
            tt-param.c-item-fim         = INPUT FRAME f-pg-sel fi-fim-it-codigo
            tt-param.c-ref-ini          = INPUT FRAME f-pg-sel fi-ini-cod-refer
            tt-param.c-ref-fim          = INPUT FRAME f-pg-sel fi-fim-cod-refer
            tt-param.da-entr-ini        = INPUT FRAME f-pg-sel fi-ini-dt-entrega
            tt-param.da-entr-fim        = INPUT FRAME f-pg-sel fi-fim-dt-entrega
            tt-param.da-impl-ini        = INPUT FRAME f-pg-sel fi-ini-dt-implant
            tt-param.da-impl-fim        = INPUT FRAME f-pg-sel fi-fim-dt-implant
            tt-param.c-acond-ini        = INPUT FRAME f-pg-sel fi-ini-acond
            tt-param.c-acond-fim        = INPUT FRAME f-pg-sel fi-fim-acond
            tt-param.corte-comerc-ini   = INPUT FRAME f-pg-sel fi-ini-corte-comerc
            tt-param.corte-comerc-fim   = INPUT FRAME f-pg-sel fi-fim-corte-comerc
            tt-param.l-apaga-todas      = INPUT FRAME f-pg-sel tg-apaga-todas
            tt-param.c-cod-refer1       = INPUT FRAME f-pg-sel fi-cod-refer1 
            tt-param.c-cod-refer2       = INPUT FRAME f-pg-sel fi-cod-refer2 
            tt-param.c-cod-refer3       = INPUT FRAME f-pg-sel fi-cod-refer3 
            tt-param.c-cod-refer4       = INPUT FRAME f-pg-sel fi-cod-refer4 
            tt-param.c-cod-refer5       = INPUT FRAME f-pg-sel fi-cod-refer5 
            tt-param.c-cod-refer6       = INPUT FRAME f-pg-sel fi-cod-refer6 
            tt-param.c-cod-refer7       = INPUT FRAME f-pg-sel fi-cod-refer7 
            tt-param.c-cod-refer8       = INPUT FRAME f-pg-sel fi-cod-refer8 
            tt-param.c-cod-refer9       = INPUT FRAME f-pg-sel fi-cod-refer9 
            tt-param.c-cod-refer10      = INPUT FRAME f-pg-sel fi-cod-refer10
            tt-param.c-ref-ini          = INPUT FRAME f-pg-sel fi-ini-cod-refer
            tt-param.l-sit-aberto       = INPUT FRAME f-pg-par tg-abertos
            tt-param.l-sit-parcial      = INPUT FRAME f-pg-par tg-at-parcial
            tt-param.l-sit-pendentes    = INPUT FRAME f-pg-par tg-pendentes
            tt-param.l-sit-suspensos    = INPUT FRAME f-pg-par tg-suspensos
            tt-param.l-sit-cancelados   = INPUT FRAME f-pg-par tg-cancelados
            tt-param.l-sit-so-aprovados = INPUT FRAME f-pg-par tg-sit-so-aprovados
            tt-param.l-res-nao          = INPUT FRAME f-pg-par tg-res-nao
            tt-param.l-res-sim          = INPUT FRAME f-pg-par tg-res-sim
            tt-param.l-res-parc         = INPUT FRAME f-pg-par tg-res-parc
            tt-param.l-res-aceita       = INPUT FRAME f-pg-par tg-res-aceita
            tt-param.l-queb-desenho     = INPUT FRAME f-pg-par tg-salta
            tt-param.c-saldo-estoq      = IF INPUT FRAME f-pg-par rs-saldo-estoq = 1
                                          THEN "P"
                                          ELSE IF INPUT FRAME f-pg-par rs-saldo-estoq = 2
                                               THEN "N" 
                                               ELSE "A"
            tt-param.c-classificacao    = entry((INPUT FRAME f-pg-cla rs-classif - 1) * 2 + 1, 
                                          rs-classif:radio-buttons in frame f-pg-cla)
            tt-param.impr-param         = INPUT FRAME f-pg-par tg-impr-param
            tt-param.nr-coletor         = INPUT FRAME f-pg-sel fi-nr-coletor.


    /*:T Executar do programa RP.P que ir  criar o relat¢rio */
    {include/i-rpexb.i}
    
    SESSION:SET-WAIT-STATE("general":U).
    
    {include/i-rprun.i esrp/espd0004rp.p}
    
    {include/i-rpexc.i}
    
    SESSION:SET-WAIT-STATE("":U).
    
    {include/i-rptrm.i}
end.
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

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this w-relat, and there are no
     tables specified in any contained Browse, Query, or Frame. */

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

