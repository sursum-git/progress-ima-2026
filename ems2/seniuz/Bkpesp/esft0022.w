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
{include/i-prgvrs.i ESFT0022 2.04.00.000}

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
&GLOBAL-DEFINE PGCLA 
&GLOBAL-DEFINE PGPAR f-pg-par
&GLOBAL-DEFINE PGDIG 
&GLOBAL-DEFINE PGIMP f-pg-imp
  
/* Parameters Definitions ---                                           */

/* Temporary Table Definitions ---                                      */

define temp-table tt-param no-undo
       field destino          as integer
       field arquivo          as char format "x(35)"
       field usuario          as char format "x(12)"
       field data-exec        as date
       field hora-exec        as integer
       field classifica       as integer
       FIELD c-estab-ini      like nota-fiscal.cod-estabel              
       FIELD c-estab-fim      like nota-fiscal.cod-estabel   
       FIELD c-repres-ini     LIKE nota-fiscal.no-ab-reppri 
       FIELD c-repres-fim     LIKE nota-fiscal.no-ab-reppri
       FIELD c-cliente-ini    LIKE nota-fiscal.nome-ab-cli
       FIELD c-cliente-fim    LIKE nota-fiscal.nome-ab-cli
       FIELD i-grupo-ini      LIKE ITEM.ge-codigo
       FIELD i-grupo-fim      LIKE ITEM.ge-codigo
       FIELD c-item-ini       LIKE ITEM.it-codigo
       FIELD c-item-fim       LIKE ITEM.it-codigo
       FIELD c-ref-ini        LIKE it-nota-fisc.cod-refer
       FIELD c-ref-fim        LIKE it-nota-fisc.cod-refer
       FIELD c-espdoc-ini     like nota-fiscal.esp-docto
       FIELD c-espdoc-fim     like nota-fiscal.esp-docto
       FIELD da-emis-ini      LIKE nota-fiscal.dt-emis-nota
       FIELD da-emis-fim      LIKE nota-fiscal.dt-emis-nota
       FIELD i-condpag-ini    like nota-fiscal.cod-cond-pag
       FIELD i-condpag-fim    like nota-fiscal.cod-cond-pag
       FIELD c-natoper-ini    like nota-fiscal.nat-operacao
       FIELD c-natoper-fim    like nota-fiscal.nat-operacao 
       FIELD c-emit-dup       AS   CHAR FORMAT "x" 
       FIELD c-nota-can       AS   CHAR format "x" 
       FIELD c-tipo-merc      AS   CHAR FORMAT "x"
       FIELD qualidade        AS INTEGER
       FIELD desc-qualidade   AS CHAR FORMAT "x(10)"
       FIELD l-tipo-rel       AS   LOG  FORMAT "Detalhado/Resumido"
       FIELD c-tp-pedido      AS   CHAR FORMAT "x"
       FIELD c-tp-pagto       AS   CHAR
       FIELD l-artigo         AS   LOG  format "Codigo/Descricao"
       FIELD l-pedido         AS   LOG  format "Nosso/Representante"
       FIELD l-nf-dev-ent     AS   LOG
       FIELD impr-param       AS   LOGICAL.

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

DEF VAR i-ct               AS INT.
DEF VAR c-especies         AS CHAR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-relat
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME f-pg-imp

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rs-destino bt-config-impr bt-arquivo ~
c-arquivo rs-execucao RECT-16 RECT-7 RECT-9 
&Scoped-Define DISPLAYED-OBJECTS rs-destino c-arquivo rs-execucao 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-relat AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
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

DEFINE VARIABLE cb-mercado AS CHARACTER FORMAT "X(256)":U INITIAL "Todos" 
     LABEL "Mercado" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Interno","Externo","Todos" 
     DROP-DOWN-LIST
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE cb-tp-pagto AS CHARACTER FORMAT "X(256)":U INITIAL "Todos" 
     LABEL "Tipo de Pagamento" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Normal","Vendor","Todos" 
     DROP-DOWN-LIST
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE fi-tp-pedido AS CHARACTER FORMAT "x(2)" 
     VIEW-AS FILL-IN 
     SIZE 4.29 BY .79
     FONT 1 NO-UNDO.

DEFINE VARIABLE rs-artigo AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "C¢digo", yes,
"Descricao", no
     SIZE 26.57 BY .75 NO-UNDO.

DEFINE VARIABLE rs-emit-dup AS INTEGER INITIAL 3 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Sim", 1,
"NÆo", 2,
"Todos", 3
     SIZE 31.57 BY 1 NO-UNDO.

DEFINE VARIABLE rs-nota-can AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Sim", 1,
"NÆo", 2,
"Todas", 3
     SIZE 28 BY 1 NO-UNDO.

DEFINE VARIABLE rs-pedido AS LOGICAL INITIAL yes 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Nosso Pedido", yes,
"Pedido do Representate", no
     SIZE 40.14 BY .75 NO-UNDO.

DEFINE VARIABLE rs-qualidade AS INTEGER INITIAL 3 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Primeira", 1,
"Segunda", 2,
"Todas", 3
     SIZE 10 BY 2.5 TOOLTIP "Qualidade dos itens." NO-UNDO.

DEFINE VARIABLE rs-tp-relat AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Detalhado", yes,
"Resumido", no
     SIZE 23.72 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 76 BY 10.58.

DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 35 BY 1.71.

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 69 BY 3.75.

DEFINE RECTANGLE RECT-22
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 23 BY 3.5.

DEFINE RECTANGLE RECT-23
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 33 BY 1.71.

DEFINE RECTANGLE RECT-32
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 45 BY 3.5.

DEFINE VARIABLE tg-impr-param AS LOGICAL INITIAL no 
     LABEL "Imprime Parƒmetros" 
     VIEW-AS TOGGLE-BOX
     SIZE 17.29 BY .75 NO-UNDO.

DEFINE VARIABLE tg-nf-dev-ent AS LOGICAL INITIAL no 
     LABEL "NF Devolu‡Æo/Entrada" 
     VIEW-AS TOGGLE-BOX
     SIZE 19.14 BY .75 NO-UNDO.

DEFINE VARIABLE tg-tp-pedido AS LOGICAL INITIAL yes 
     LABEL "Todos os Tipos de Pedido" 
     VIEW-AS TOGGLE-BOX
     SIZE 20.86 BY .83 NO-UNDO.

DEFINE VARIABLE cb-fim-esp-docto AS CHARACTER FORMAT "X(5)":U 
     VIEW-AS COMBO-BOX INNER-LINES 8
     DROP-DOWN-LIST
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE cb-ini-esp-docto AS CHARACTER FORMAT "X(5)":U 
     LABEL "Esp‚cie" 
     VIEW-AS COMBO-BOX INNER-LINES 8
     DROP-DOWN-LIST
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE fi-fim-cod-estabel AS CHARACTER FORMAT "X(3)":U INITIAL "ZZZ" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fim-cod-refer AS CHARACTER FORMAT "x(8)" INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 TOOLTIP "Referˆncia final" NO-UNDO.

DEFINE VARIABLE fi-fim-cond-pagto AS INTEGER FORMAT ">>9":U INITIAL 999 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fim-dt-emissao AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fim-ge-codigo AS INTEGER FORMAT ">9":U INITIAL 99 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fim-it-codigo AS CHARACTER FORMAT "X(16)" INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fim-natur-oper AS CHARACTER FORMAT "X(6)":U INITIAL "ZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fim-no-ab-reppri AS CHARACTER FORMAT "X(12)" INITIAL "ZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fim-nome-abrev AS CHARACTER FORMAT "X(12)" INITIAL "ZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-cod-estabel AS CHARACTER FORMAT "X(3)":U 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-cod-refer AS CHARACTER FORMAT "x(8)" 
     LABEL "Referˆncia":R12 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 TOOLTIP "Referˆncia inicial" NO-UNDO.

DEFINE VARIABLE fi-ini-cond-pagto AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Condi‡Æo de Pagamento" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-dt-emissao AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "EmissÆo" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-ge-codigo AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Grupo de Estoque" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-it-codigo AS CHARACTER FORMAT "X(16)" 
     LABEL "Item":R8 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-natur-oper AS CHARACTER FORMAT "x(06)" 
     LABEL "Natureza opera‡Æo":R21 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-no-ab-reppri AS CHARACTER FORMAT "X(12)":U 
     LABEL "Representante" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-nome-abrev AS CHARACTER FORMAT "X(12)" 
     LABEL "Cliente":R17 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-10
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-19
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-20
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-21
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-22
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-23
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-24
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-25
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-26
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-29
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-3
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-30
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-31
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-32
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

DEFINE IMAGE IMAGE-7
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-8
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-9
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE RECTANGLE RECT-24
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 77 BY 10.83.

DEFINE BUTTON bt-ajuda 
     LABEL "Ajuda" 
     SIZE 10 BY 1
     FONT 1.

DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "Fechar" 
     SIZE 10 BY 1
     FONT 1.

DEFINE BUTTON bt-executar 
     LABEL "Executar" 
     SIZE 10 BY 1
     FONT 1.

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
     SIZE 79 BY 1.42
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

DEFINE FRAME f-pg-imp
     rs-destino AT ROW 2.38 COL 3.29 HELP
          "Destino de ImpressÆo do Relat¢rio" NO-LABEL
     bt-config-impr AT ROW 3.58 COL 43.29 HELP
          "Configura‡Æo da impressora"
     bt-arquivo AT ROW 3.58 COL 43.29 HELP
          "Escolha do nome do arquivo"
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

DEFINE FRAME f-relat
     bt-executar AT ROW 14.54 COL 3 HELP
          "Dispara a execu‡Æo do relat¢rio"
     bt-cancelar AT ROW 14.54 COL 14 HELP
          "Fechar"
     bt-ajuda AT ROW 14.54 COL 70 HELP
          "Ajuda"
     rt-folder-left AT ROW 2.54 COL 2.14
     RECT-1 AT ROW 14.29 COL 2
     RECT-6 AT ROW 13.75 COL 2.14
     rt-folder-right AT ROW 2.67 COL 80.43
     rt-folder-top AT ROW 2.54 COL 2.14
     rt-folder AT ROW 2.5 COL 2
     im-pg-imp AT ROW 1.5 COL 33.43
     im-pg-par AT ROW 1.5 COL 17.72
     im-pg-sel AT ROW 1.5 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80.14 BY 15
         DEFAULT-BUTTON bt-executar.

DEFINE FRAME f-pg-sel
     fi-ini-cod-estabel AT ROW 1.5 COL 19 COLON-ALIGNED
     fi-fim-cod-estabel AT ROW 1.5 COL 48 COLON-ALIGNED NO-LABEL
     fi-ini-no-ab-reppri AT ROW 2.5 COL 19 COLON-ALIGNED
     fi-fim-no-ab-reppri AT ROW 2.5 COL 48 COLON-ALIGNED HELP
          "Nome abreviado do representante principal" NO-LABEL
     fi-ini-nome-abrev AT ROW 3.5 COL 19 COLON-ALIGNED HELP
          "Nome abreviado do cliente/fornecedor"
     fi-fim-nome-abrev AT ROW 3.5 COL 48 COLON-ALIGNED HELP
          "Nome abreviado do cliente/fornecedor" NO-LABEL
     fi-ini-ge-codigo AT ROW 4.5 COL 19 COLON-ALIGNED
     fi-fim-ge-codigo AT ROW 4.5 COL 48 COLON-ALIGNED NO-LABEL
     fi-ini-it-codigo AT ROW 5.5 COL 19 COLON-ALIGNED
     fi-fim-it-codigo AT ROW 5.5 COL 48 COLON-ALIGNED NO-LABEL
     fi-ini-cod-refer AT ROW 6.5 COL 19 COLON-ALIGNED HELP
          "C¢digo de referˆncia do item"
     fi-fim-cod-refer AT ROW 6.5 COL 48 COLON-ALIGNED HELP
          "C¢digo de referˆncia do item" NO-LABEL
     cb-ini-esp-docto AT ROW 7.5 COL 19 COLON-ALIGNED
     cb-fim-esp-docto AT ROW 7.5 COL 48 COLON-ALIGNED NO-LABEL
     fi-ini-dt-emissao AT ROW 8.5 COL 19 COLON-ALIGNED
     fi-fim-dt-emissao AT ROW 8.5 COL 48 COLON-ALIGNED NO-LABEL
     fi-ini-cond-pagto AT ROW 9.5 COL 19 COLON-ALIGNED
     fi-fim-cond-pagto AT ROW 9.5 COL 48 COLON-ALIGNED NO-LABEL
     fi-ini-natur-oper AT ROW 10.5 COL 19 COLON-ALIGNED HELP
          "Natureza de opera‡Æo"
     fi-fim-natur-oper AT ROW 10.5 COL 48 COLON-ALIGNED NO-LABEL
     IMAGE-10 AT ROW 2.5 COL 46.57
     IMAGE-19 AT ROW 4.5 COL 39
     IMAGE-20 AT ROW 4.5 COL 46.57
     IMAGE-21 AT ROW 5.5 COL 39
     IMAGE-22 AT ROW 5.5 COL 46.57
     IMAGE-23 AT ROW 9.5 COL 39
     IMAGE-24 AT ROW 9.5 COL 46.57
     IMAGE-25 AT ROW 10.5 COL 39
     IMAGE-26 AT ROW 10.5 COL 46.57
     IMAGE-29 AT ROW 1.5 COL 38.86
     IMAGE-3 AT ROW 8.5 COL 39
     IMAGE-30 AT ROW 1.5 COL 46.57
     IMAGE-31 AT ROW 6.5 COL 39
     IMAGE-32 AT ROW 6.5 COL 46.57
     IMAGE-4 AT ROW 8.5 COL 46.57
     IMAGE-5 AT ROW 7.5 COL 39
     IMAGE-6 AT ROW 7.5 COL 46.57
     IMAGE-7 AT ROW 3.5 COL 39
     IMAGE-8 AT ROW 3.5 COL 46.57
     IMAGE-9 AT ROW 2.5 COL 39
     RECT-24 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.83
         SIZE 77.43 BY 10.83
         FONT 1.

DEFINE FRAME f-pg-par
     rs-emit-dup AT ROW 2.25 COL 6.43 NO-LABEL
     rs-nota-can AT ROW 2.29 COL 43.57 NO-LABEL
     cb-mercado AT ROW 4 COL 20 COLON-ALIGNED
     rs-qualidade AT ROW 4 COL 62 NO-LABEL
     cb-tp-pagto AT ROW 5 COL 20 COLON-ALIGNED
     rs-pedido AT ROW 6.33 COL 21.72 NO-LABEL
     tg-tp-pedido AT ROW 8 COL 4.86
     rs-tp-relat AT ROW 8.04 COL 42.29 NO-LABEL
     rs-artigo AT ROW 9.17 COL 42.29 NO-LABEL
     fi-tp-pedido AT ROW 9.54 COL 16.72 COLON-ALIGNED HELP
          "Dispon¡vel para classifica‡Æo/indica‡Æo pr¢pria do usu rio" NO-LABEL
     tg-nf-dev-ent AT ROW 10.25 COL 30.86
     tg-impr-param AT ROW 10.25 COL 54
     RECT-17 AT ROW 1.25 COL 1
     RECT-20 AT ROW 1.79 COL 4
     RECT-21 AT ROW 3.75 COL 4
     RECT-22 AT ROW 7.71 COL 4
     RECT-23 AT ROW 1.83 COL 39.86
     RECT-32 AT ROW 7.71 COL 28
     " Emite Duplicata" VIEW-AS TEXT
          SIZE 12.29 BY .75 AT ROW 1.5 COL 5
     "Tipo de Relat¢rio:" VIEW-AS TEXT
          SIZE 13 BY .88 AT ROW 8 COL 29.29
     "Pedidos do Tipo:" VIEW-AS TEXT
          SIZE 12 BY .75 AT ROW 9.54 COL 6.86
     " Notas Canceladas" VIEW-AS TEXT
          SIZE 14 BY .75 AT ROW 1.54 COL 42.14
     "Imprimir N£mero do:" VIEW-AS TEXT
          SIZE 13.57 BY .75 AT ROW 6.25 COL 8.14
     "Imprimir Artigo:" VIEW-AS TEXT
          SIZE 10 BY 1 AT ROW 9 COL 31.86
     "Qualidade:" VIEW-AS TEXT
          SIZE 7.86 BY .75 AT ROW 4.04 COL 54
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.71
         SIZE 77.14 BY 10.92
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
         TITLE              = "Notas Fiscais Emitidas por Cliente"
         COLUMN             = 21.43
         ROW                = 8.42
         HEIGHT             = 15.04
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
/* SETTINGS FOR RADIO-SET rs-artigo IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME f-pg-sel
   Custom                                                               */
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
ON END-ERROR OF w-relat /* Notas Fiscais Emitidas por Cliente */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON WINDOW-CLOSE OF w-relat /* Notas Fiscais Emitidas por Cliente */
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
   do  on error undo, return no-apply:
       IF INPUT FRAME f-pg-par rs-qualidade < 3 AND 
          INPUT FRAME f-pg-par tg-nf-dev-ent = NO THEN 
          run pi-executar.
       ELSE
       IF INPUT FRAME f-pg-par rs-qualidade = 3 AND 
          INPUT FRAME f-pg-par tg-nf-dev-ent = NO THEN
          RUN pi-executar1.
       ELSE
          RUN pi-executar2.
   end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-sel
&Scoped-define SELF-NAME fi-fim-cod-estabel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fim-cod-estabel w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-fim-cod-estabel IN FRAME f-pg-sel
DO:
  {include/zoomvar.i &prog-zoom=adzoom/z01ad107.w
                     &campo=fi-fim-cod-estabel
                     &campozoom=cod-estabel
                     &FRAME=f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-fim-cond-pagto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fim-cond-pagto w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-fim-cond-pagto IN FRAME f-pg-sel
DO:
  {include/zoomvar.i &prog-zoom=adzoom/z01ad039.w
                     &campo=fi-fim-cond-pagto
                     &campozoom=cod-cond-pag
                     &FRAME=f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-fim-ge-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fim-ge-codigo w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-fim-ge-codigo IN FRAME f-pg-sel
DO:
    {include/zoomvar.i &prog-zoom=inzoom/z01in142.w
                       &campo=fi-fim-ge-codigo
                       &campozoom=ge-codigo
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


&Scoped-define SELF-NAME fi-fim-natur-oper
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fim-natur-oper w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-fim-natur-oper IN FRAME f-pg-sel
DO:
  {include/zoomvar.i &prog-zoom=inzoom/z01in245.w
                     &campo=fi-fim-natur-oper
                     &campozoom=nat-operacao
                     &FRAME=f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-fim-no-ab-reppri
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fim-no-ab-reppri w-relat
ON LEAVE OF fi-fim-no-ab-reppri IN FRAME f-pg-sel
DO:
  FIND repres WHERE
       repres.nome-abrev = SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
  IF NOT AVAIL repres THEN
     FIND repres WHERE
          repres.cod-rep = INT(SELF:SCREEN-VALUE) NO-LOCK NO-ERROR.
  IF AVAIL repres THEN
     ASSIGN SELF:SCREEN-VALUE = repres.nome-abrev.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fim-no-ab-reppri w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-fim-no-ab-reppri IN FRAME f-pg-sel
DO:
    {include/zoomvar.i &prog-zoom=adzoom/z01ad229.w
                       &campo=fi-fim-no-ab-reppri
                       &campozoom=nome-abrev
                       &FRAME=f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-fim-nome-abrev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fim-nome-abrev w-relat
ON LEAVE OF fi-fim-nome-abrev IN FRAME f-pg-sel
DO:
  FIND emitente WHERE
       emitente.nome-abrev = SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
  IF NOT AVAIL emitente THEN
     FIND emitente WHERE
          emitente.cod-emitente = INT(SELF:SCREEN-VALUE) NO-LOCK NO-ERROR.
  IF AVAIL emitente THEN
     ASSIGN SELF:SCREEN-VALUE = emitente.nome-abrev.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fim-nome-abrev w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-fim-nome-abrev IN FRAME f-pg-sel
DO:
   {include/zoomvar.i &prog-zoom = adzoom/z02ad098.w
                      &campo     = fi-fim-nome-abrev
                      &campozoom = nome-abrev
                      &FRAME     = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ini-cod-estabel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-cod-estabel w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ini-cod-estabel IN FRAME f-pg-sel /* Estabelecimento */
DO:
  {include/zoomvar.i &prog-zoom=adzoom/z01ad107.w
                     &campo=fi-ini-cod-estabel
                     &campozoom=cod-estabel
                     &FRAME=f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ini-cond-pagto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-cond-pagto w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ini-cond-pagto IN FRAME f-pg-sel /* Condi‡Æo de Pagamento */
DO:
    {include/zoomvar.i &prog-zoom=adzoom/z01ad039.w
                       &campo=fi-ini-cond-pagto
                       &campozoom=cod-cond-pag
                       &FRAME=f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ini-ge-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-ge-codigo w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ini-ge-codigo IN FRAME f-pg-sel /* Grupo de Estoque */
DO:
    {include/zoomvar.i &prog-zoom=inzoom/z01in142.w
                       &campo=fi-ini-ge-codigo
                       &campozoom=ge-codigo
                       &FRAME=f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ini-it-codigo
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


&Scoped-define SELF-NAME fi-ini-natur-oper
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-natur-oper w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ini-natur-oper IN FRAME f-pg-sel /* Natureza opera‡Æo */
DO:
  {include/zoomvar.i &prog-zoom=inzoom/z01in245.w
                     &campo=fi-ini-natur-oper
                     &campozoom=nat-operacao
                     &FRAME=f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ini-no-ab-reppri
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-no-ab-reppri w-relat
ON LEAVE OF fi-ini-no-ab-reppri IN FRAME f-pg-sel /* Representante */
DO:
  FIND repres WHERE
       repres.nome-abrev = SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
  IF NOT AVAIL repres THEN
     FIND repres WHERE
          repres.cod-rep = INT(SELF:SCREEN-VALUE) NO-LOCK NO-ERROR.
  IF AVAIL repres THEN
     ASSIGN SELF:SCREEN-VALUE = repres.nome-abrev.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-no-ab-reppri w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ini-no-ab-reppri IN FRAME f-pg-sel /* Representante */
DO:
    {include/zoomvar.i &prog-zoom=adzoom/z01ad229.w
                       &campo=fi-ini-no-ab-reppri
                       &campozoom=nome-abrev
                       &FRAME=f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ini-nome-abrev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-nome-abrev w-relat
ON LEAVE OF fi-ini-nome-abrev IN FRAME f-pg-sel /* Cliente */
DO:
  FIND emitente WHERE
       emitente.nome-abrev = SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
  IF NOT AVAIL emitente THEN
     FIND emitente WHERE
          emitente.cod-emitente = INT(SELF:SCREEN-VALUE) NO-LOCK NO-ERROR.
  IF AVAIL emitente THEN
     ASSIGN SELF:SCREEN-VALUE = emitente.nome-abrev.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-nome-abrev w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ini-nome-abrev IN FRAME f-pg-sel /* Cliente */
DO:
   {include/zoomvar.i &prog-zoom = adzoom/z02ad098.w
                      &campo     = fi-ini-nome-abrev
                      &campozoom = nome-abrev
                      &FRAME     = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
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
&Scoped-define SELF-NAME rs-tp-relat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-tp-relat w-relat
ON VALUE-CHANGED OF rs-tp-relat IN FRAME f-pg-par
DO:
  IF INPUT FRAME {&frame-name} rs-tp-relat = YES THEN
     ASSIGN rs-artigo:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
  ELSE
     ASSIGN rs-artigo:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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


&Scoped-define FRAME-NAME f-pg-imp
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-relat 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{utp/ut9000.i "ESFT0022" "2.04.00.000"}

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

    {esinc/i-dsallrb.i nota-fiscal.esp-docto c-especies}
    ASSIGN fi-ini-cod-estabel:SCREEN-VALUE IN FRAME f-pg-sel = "2"
           fi-fim-cod-estabel:SCREEN-VALUE IN FRAME f-pg-sel = "2"
           fi-ini-ge-codigo:SCREEN-VALUE IN FRAME f-pg-sel = "50"
           fi-fim-ge-codigo:SCREEN-VALUE IN FRAME f-pg-sel = "59"
           fi-ini-it-codigo:SCREEN-VALUE IN FRAME f-pg-sel = "5"
           fi-fim-it-codigo:SCREEN-VALUE IN FRAME f-pg-sel = "5ZZZZZZZZZZZZZZZ"
           cb-ini-esp-docto:LIST-ITEMS IN FRAME f-pg-sel = c-especies
           cb-fim-esp-docto:LIST-ITEMS IN FRAME f-pg-sel = c-especies
           cb-ini-esp-docto:SCREEN-VALUE IN FRAME f-pg-sel = "NFS"
           cb-fim-esp-docto:SCREEN-VALUE IN FRAME f-pg-sel = "NFS".

    fi-ini-cod-estabel:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-fim-cod-estabel:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-ini-no-ab-reppri:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-fim-no-ab-reppri:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-ini-nome-abrev:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-fim-nome-abrev:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-ini-ge-codigo:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-fim-ge-codigo:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-ini-it-codigo:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-fim-it-codigo:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-ini-cond-pagto:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-fim-cond-pagto:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-ini-natur-oper:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-fim-natur-oper:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
        
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
  ENABLE bt-executar bt-cancelar bt-ajuda im-pg-imp im-pg-par im-pg-sel 
      WITH FRAME f-relat IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}
  DISPLAY rs-emit-dup rs-nota-can cb-mercado rs-qualidade cb-tp-pagto rs-pedido 
          tg-tp-pedido rs-tp-relat rs-artigo fi-tp-pedido tg-nf-dev-ent 
          tg-impr-param 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  ENABLE rs-emit-dup rs-nota-can cb-mercado rs-qualidade cb-tp-pagto rs-pedido 
         tg-tp-pedido rs-tp-relat tg-nf-dev-ent tg-impr-param RECT-17 RECT-20 
         RECT-21 RECT-22 RECT-23 RECT-32 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-par}
  DISPLAY rs-destino c-arquivo rs-execucao 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  ENABLE rs-destino bt-config-impr bt-arquivo c-arquivo rs-execucao RECT-16 
         RECT-7 RECT-9 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}
  DISPLAY fi-ini-cod-estabel fi-fim-cod-estabel fi-ini-no-ab-reppri 
          fi-fim-no-ab-reppri fi-ini-nome-abrev fi-fim-nome-abrev 
          fi-ini-ge-codigo fi-fim-ge-codigo fi-ini-it-codigo fi-fim-it-codigo 
          fi-ini-cod-refer fi-fim-cod-refer cb-ini-esp-docto cb-fim-esp-docto 
          fi-ini-dt-emissao fi-fim-dt-emissao fi-ini-cond-pagto 
          fi-fim-cond-pagto fi-ini-natur-oper fi-fim-natur-oper 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  ENABLE fi-ini-cod-estabel fi-fim-cod-estabel fi-ini-no-ab-reppri 
         fi-fim-no-ab-reppri fi-ini-nome-abrev fi-fim-nome-abrev 
         fi-ini-ge-codigo fi-fim-ge-codigo fi-ini-it-codigo fi-fim-it-codigo 
         fi-ini-cod-refer fi-fim-cod-refer cb-ini-esp-docto cb-fim-esp-docto 
         fi-ini-dt-emissao fi-fim-dt-emissao fi-ini-cond-pagto 
         fi-fim-cond-pagto fi-ini-natur-oper fi-fim-natur-oper IMAGE-10 
         IMAGE-19 IMAGE-20 IMAGE-21 IMAGE-22 IMAGE-23 IMAGE-24 IMAGE-25 
         IMAGE-26 IMAGE-29 IMAGE-3 IMAGE-30 IMAGE-31 IMAGE-32 IMAGE-4 IMAGE-5 
         IMAGE-6 IMAGE-7 IMAGE-8 IMAGE-9 RECT-24 
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

     ASSIGN tt-param.c-estab-ini    = input frame f-pg-sel fi-ini-cod-estabel
            tt-param.c-estab-fim    = input frame f-pg-sel fi-fim-cod-estabel
            tt-param.c-repres-ini   = input frame f-pg-sel fi-ini-no-ab-reppri
            tt-param.c-repres-fim   = input frame f-pg-sel fi-fim-no-ab-reppri
            tt-param.c-cliente-ini  = input frame f-pg-sel fi-ini-nome-abrev
            tt-param.c-cliente-fim  = input frame f-pg-sel fi-fim-nome-abrev
            tt-param.i-grupo-ini    = input frame f-pg-sel fi-ini-ge-codigo
            tt-param.i-grupo-fim    = input frame f-pg-sel fi-fim-ge-codigo
            tt-param.c-item-ini     = input frame f-pg-sel fi-ini-it-codigo 
            tt-param.c-item-fim     = input frame f-pg-sel fi-fim-it-codigo
            tt-param.c-ref-ini      = INPUT FRAME f-pg-sel fi-ini-cod-refer
            tt-param.c-ref-fim      = INPUT FRAME f-pg-sel fi-fim-cod-refer
            tt-param.c-espdoc-ini   = LOOKUP(input frame f-pg-sel cb-ini-esp-docto,c-especies)
            tt-param.c-espdoc-fim   = LOOKUP(input frame f-pg-sel cb-fim-esp-docto,c-especies)
            tt-param.da-emis-ini    = input frame f-pg-sel fi-ini-dt-emissao 
            tt-param.da-emis-fim    = input frame f-pg-sel fi-fim-dt-emissao 
            tt-param.i-condpag-ini  = input frame f-pg-sel fi-ini-cond-pagto
            tt-param.i-condpag-fim  = input frame f-pg-sel fi-fim-cond-pagto
            tt-param.c-natoper-ini  = input frame f-pg-sel fi-ini-natur-oper
            tt-param.c-natoper-fim  = input frame f-pg-sel fi-fim-natur-oper
            tt-param.c-emit-dup     = IF INPUT FRAME f-pg-par rs-emit-dup = 1
                                      THEN "S" 
                                      ELSE IF INPUT FRAME f-pg-par rs-emit-dup = 2
                                           THEN "N" 
                                           ELSE "T"
            tt-param.c-nota-can     = IF INPUT FRAME f-pg-par rs-nota-can = 1
                                      THEN "S" 
                                      ELSE IF INPUT FRAME f-pg-par rs-nota-can = 2
                                           THEN "N" 
                                           ELSE "T" 
            tt-param.c-tipo-merc    = SUBSTR(cb-mercado,1,1)
            tt-param.qualidade      = INPUT FRAME f-pg-par rs-qualidade
            tt-param.desc-qualidade = entry((tt-param.qualidade - 1) * 2 + 1, 
                                             rs-qualidade:radio-buttons in frame f-pg-par)
            tt-param.l-tipo-rel     = INPUT FRAME f-pg-par rs-tp-relat
            tt-param.l-artigo       = INPUT FRAME f-pg-par rs-artigo
            tt-param.l-pedido       = INPUT FRAME f-pg-par rs-pedido 
            tt-param.c-tp-pedido    = INPUT FRAME f-pg-par fi-tp-pedido
            tt-param.c-tp-pagto     = INPUT FRAME f-pg-par cb-tp-pagto
            tt-param.l-nf-dev-ent   = INPUT FRAME f-pg-par tg-nf-dev-ent
            tt-param.impr-param     = INPUT FRAME f-pg-par tg-impr-param.

    /*:T Executar do programa RP.P que ir  criar o relat¢rio */
    {include/i-rpexb.i}
    
    SESSION:SET-WAIT-STATE("general":U).
    
    {include/i-rprun.i esrp/esft0022rp.p} 

    {include/i-rpexc.i}
    
    SESSION:SET-WAIT-STATE("":U).
    
    {include/i-rptrm.i}
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-executar1 w-relat 
PROCEDURE pi-executar1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
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

     ASSIGN tt-param.c-estab-ini    = input frame f-pg-sel fi-ini-cod-estabel
            tt-param.c-estab-fim    = input frame f-pg-sel fi-fim-cod-estabel
            tt-param.c-repres-ini   = input frame f-pg-sel fi-ini-no-ab-reppri
            tt-param.c-repres-fim   = input frame f-pg-sel fi-fim-no-ab-reppri
            tt-param.c-cliente-ini  = input frame f-pg-sel fi-ini-nome-abrev
            tt-param.c-cliente-fim  = input frame f-pg-sel fi-fim-nome-abrev
            tt-param.i-grupo-ini    = input frame f-pg-sel fi-ini-ge-codigo
            tt-param.i-grupo-fim    = input frame f-pg-sel fi-fim-ge-codigo
            tt-param.c-item-ini     = input frame f-pg-sel fi-ini-it-codigo 
            tt-param.c-item-fim     = input frame f-pg-sel fi-fim-it-codigo
            tt-param.c-ref-ini      = INPUT FRAME f-pg-sel fi-ini-cod-refer
            tt-param.c-ref-fim      = INPUT FRAME f-pg-sel fi-fim-cod-refer
            tt-param.c-espdoc-ini   = LOOKUP(input frame f-pg-sel cb-ini-esp-docto,c-especies)
            tt-param.c-espdoc-fim   = LOOKUP(input frame f-pg-sel cb-fim-esp-docto,c-especies)
            tt-param.da-emis-ini    = input frame f-pg-sel fi-ini-dt-emissao 
            tt-param.da-emis-fim    = input frame f-pg-sel fi-fim-dt-emissao 
            tt-param.i-condpag-ini  = input frame f-pg-sel fi-ini-cond-pagto
            tt-param.i-condpag-fim  = input frame f-pg-sel fi-fim-cond-pagto
            tt-param.c-natoper-ini  = input frame f-pg-sel fi-ini-natur-oper
            tt-param.c-natoper-fim  = input frame f-pg-sel fi-fim-natur-oper
            tt-param.c-emit-dup     = IF INPUT FRAME f-pg-par rs-emit-dup = 1
                                      THEN "S" 
                                      ELSE IF INPUT FRAME f-pg-par rs-emit-dup = 2
                                           THEN "N" 
                                           ELSE "T"
            tt-param.c-nota-can     = IF INPUT FRAME f-pg-par rs-nota-can = 1
                                      THEN "S" 
                                      ELSE IF INPUT FRAME f-pg-par rs-nota-can = 2
                                           THEN "N" 
                                           ELSE "T" 
            tt-param.c-tipo-merc    = SUBSTR(cb-mercado,1,1)
            tt-param.qualidade      = INPUT FRAME f-pg-par rs-qualidade
            tt-param.desc-qualidade = entry((tt-param.qualidade - 1) * 2 + 1, 
                                             rs-qualidade:radio-buttons in frame f-pg-par)
            tt-param.l-tipo-rel     = INPUT FRAME f-pg-par rs-tp-relat
            tt-param.l-artigo       = INPUT FRAME f-pg-par rs-artigo
            tt-param.l-pedido       = INPUT FRAME f-pg-par rs-pedido 
            tt-param.c-tp-pedido    = INPUT FRAME f-pg-par fi-tp-pedido
            tt-param.c-tp-pagto     = INPUT FRAME f-pg-par cb-tp-pagto
            tt-param.l-nf-dev-ent   = INPUT FRAME f-pg-par tg-nf-dev-ent
            tt-param.impr-param     = INPUT FRAME f-pg-par tg-impr-param.

    /*:T Executar do programa RP.P que ir  criar o relat¢rio */
    {include/i-rpexb.i}
    
    SESSION:SET-WAIT-STATE("general":U).
    
    {include/i-rprun.i esrp/esft0022rpa.p} 

    {include/i-rpexc.i}
    
    SESSION:SET-WAIT-STATE("":U).
    
    {include/i-rptrm.i}
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-executar2 w-relat 
PROCEDURE pi-executar2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
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

     ASSIGN tt-param.c-estab-ini    = input frame f-pg-sel fi-ini-cod-estabel
            tt-param.c-estab-fim    = input frame f-pg-sel fi-fim-cod-estabel
            tt-param.c-repres-ini   = input frame f-pg-sel fi-ini-no-ab-reppri
            tt-param.c-repres-fim   = input frame f-pg-sel fi-fim-no-ab-reppri
            tt-param.c-cliente-ini  = input frame f-pg-sel fi-ini-nome-abrev
            tt-param.c-cliente-fim  = input frame f-pg-sel fi-fim-nome-abrev
            tt-param.i-grupo-ini    = input frame f-pg-sel fi-ini-ge-codigo
            tt-param.i-grupo-fim    = input frame f-pg-sel fi-fim-ge-codigo
            tt-param.c-item-ini     = input frame f-pg-sel fi-ini-it-codigo 
            tt-param.c-item-fim     = input frame f-pg-sel fi-fim-it-codigo
            tt-param.c-ref-ini      = INPUT FRAME f-pg-sel fi-ini-cod-refer
            tt-param.c-ref-fim      = INPUT FRAME f-pg-sel fi-fim-cod-refer
            tt-param.c-espdoc-ini   = LOOKUP(input frame f-pg-sel cb-ini-esp-docto,c-especies)
            tt-param.c-espdoc-fim   = LOOKUP(input frame f-pg-sel cb-fim-esp-docto,c-especies)
            tt-param.da-emis-ini    = input frame f-pg-sel fi-ini-dt-emissao 
            tt-param.da-emis-fim    = input frame f-pg-sel fi-fim-dt-emissao 
            tt-param.i-condpag-ini  = input frame f-pg-sel fi-ini-cond-pagto
            tt-param.i-condpag-fim  = input frame f-pg-sel fi-fim-cond-pagto
            tt-param.c-natoper-ini  = input frame f-pg-sel fi-ini-natur-oper
            tt-param.c-natoper-fim  = input frame f-pg-sel fi-fim-natur-oper
            tt-param.c-emit-dup     = IF INPUT FRAME f-pg-par rs-emit-dup = 1
                                      THEN "S" 
                                      ELSE IF INPUT FRAME f-pg-par rs-emit-dup = 2
                                           THEN "N" 
                                           ELSE "T"
            tt-param.c-nota-can     = IF INPUT FRAME f-pg-par rs-nota-can = 1
                                      THEN "S" 
                                      ELSE IF INPUT FRAME f-pg-par rs-nota-can = 2
                                           THEN "N" 
                                           ELSE "T" 
            tt-param.c-tipo-merc    = SUBSTR(cb-mercado,1,1)
            tt-param.qualidade      = INPUT FRAME f-pg-par rs-qualidade
            tt-param.desc-qualidade = entry((tt-param.qualidade - 1) * 2 + 1, 
                                             rs-qualidade:radio-buttons in frame f-pg-par)
            tt-param.l-tipo-rel     = INPUT FRAME f-pg-par rs-tp-relat
            tt-param.l-artigo       = INPUT FRAME f-pg-par rs-artigo
            tt-param.l-pedido       = INPUT FRAME f-pg-par rs-pedido 
            tt-param.c-tp-pedido    = INPUT FRAME f-pg-par fi-tp-pedido
            tt-param.c-tp-pagto     = INPUT FRAME f-pg-par cb-tp-pagto
            tt-param.l-nf-dev-ent   = INPUT FRAME f-pg-par tg-nf-dev-ent
            tt-param.impr-param     = INPUT FRAME f-pg-par tg-impr-param.

    /*:T Executar do programa RP.P que ir  criar o relat¢rio */
    {include/i-rpexb.i}
    
    SESSION:SET-WAIT-STATE("general":U).
    
    {include/i-rprun.i esrp/esft0022rpb.p} 

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

