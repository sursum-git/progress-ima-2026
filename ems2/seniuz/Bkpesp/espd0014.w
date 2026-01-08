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
{include/i-prgvrs.i ESPD0014 2.04.00.000}

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
       FIELD c-repres-ini     LIKE ped-venda.no-ab-reppri 
       FIELD c-repres-fim     LIKE ped-venda.no-ab-reppri
       FIELD c-cliente-ini    LIKE ped-venda.nome-abrev
       FIELD c-cliente-fim    LIKE ped-venda.nome-abrev
       FIELD i-grupo-ini      LIKE ITEM.ge-codigo
       FIELD i-grupo-fim      LIKE ITEM.ge-codigo
       FIELD c-item-ini       LIKE ITEM.it-codigo
       FIELD c-item-fim       LIKE ITEM.it-codigo
       FIELD c-ref-ini        LIKE ref-item.cod-refer
       FIELD c-ref-fim        LIKE ref-item.cod-refer
       FIELD da-entr-ini      LIKE ped-venda.dt-entrega   
       FIELD da-entr-fim      LIKE ped-venda.dt-entrega   
       FIELD da-impl-ini      LIKE ped-venda.dt-implant
       FIELD da-impl-fim      LIKE ped-venda.dt-implant   
       FIELD da-can-ini       LIKE ped-venda.dt-cancela
       FIELD da-can-fim       LIKE ped-venda.dt-cancela
       FIELD de-percom-ini    LIKE ped-repre.perc-comis
       FIELD de-percom-fim    LIKE ped-repre.perc-comis
       FIELD c-cond-pag       AS   CHAR FORMAT "x"
       FIELD c-saldo          AS   CHAR FORMAT "x"
       FIELD c-tipo-rel       AS   CHAR FORMAT "x"
       FIELD c-tipo-merc      AS   CHAR FORMAT "x"
       FIELD c-tipo-artigo    AS   CHAR FORMAT "x"
       FIELD c-tp-pedido      AS   CHAR FORMAT "x"
       FIELD l-ped-abe        AS   LOG  FORMAT "Sim/NÆo"     
       FIELD l-ped-atp        AS   LOG  FORMAT "Sim/NÆo"     
       FIELD l-ped-att        AS   LOG  FORMAT "Sim/NÆo"     
       FIELD l-ped-pen        AS   LOG  FORMAT "Sim/NÆo"     
       FIELD l-ped-sus        AS   LOG  FORMAT "Sim/NÆo"     
       FIELD l-ped-can        AS   LOG  FORMAT "Sim/NÆo"     
       FIELD l-ped-out        AS   LOG  FORMAT "Sim/NÆo"     
       FIELD l-crd-nava       AS   LOG  FORMAT "Sim/NÆo"     
       FIELD l-crd-aval       AS   LOG  FORMAT "Sim/NÆo"     
       FIELD l-crd-aprv       AS   LOG  FORMAT "Sim/NÆo"     
       FIELD l-crd-repr       AS   LOG  FORMAT "Sim/NÆo"     
       FIELD l-pula-pag       AS   LOG  FORMAT "Sim/NÆo"
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-relat
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME f-pg-imp

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rs-destino bt-arquivo bt-config-impr ~
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

DEFINE VARIABLE cb-artigo AS CHARACTER FORMAT "X(256)":U INITIAL "Todos" 
     LABEL "Artigo" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Indigo","Outros","Todos" 
     DROP-DOWN-LIST
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE cb-mercado AS CHARACTER FORMAT "X(256)":U INITIAL "Todos" 
     LABEL "Mercado" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Interno","Externo","Todos" 
     DROP-DOWN-LIST
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE fi-tp-pedido AS CHARACTER FORMAT "x(2)" 
     VIEW-AS FILL-IN 
     SIZE 4.29 BY .79
     FONT 1 NO-UNDO.

DEFINE VARIABLE rs-cond-pagto AS INTEGER INITIAL 3 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "· Vista", 1,
"· Prazo", 2,
"Todos", 3
     SIZE 18.29 BY 3.38 NO-UNDO.

DEFINE VARIABLE rs-qtd-ped AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Aberta", 1,
"Total", 2
     SIZE 17.86 BY .88 NO-UNDO.

DEFINE VARIABLE rs-tp-relat AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Detalhado", 1,
"Resumido", 2,
"Compacto", 3
     SIZE 32.86 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 76 BY 10.58.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 23 BY 7.13.

DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 25.72 BY 4.08.

DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 24.43 BY 4.04.

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 50.57 BY 2.92.

DEFINE RECTANGLE RECT-22
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 23 BY 2.5.

DEFINE RECTANGLE RECT-32
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 50.57 BY 2.5.

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

DEFINE VARIABLE tg-crd-aprv AS LOGICAL INITIAL yes 
     LABEL "Aprovado" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE tg-crd-aval AS LOGICAL INITIAL yes 
     LABEL "Avaliado" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE tg-crd-nava AS LOGICAL INITIAL yes 
     LABEL "NÆo Avaliado" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .83 NO-UNDO.

DEFINE VARIABLE tg-crd-repr AS LOGICAL INITIAL yes 
     LABEL "NÆo Aprovado" 
     VIEW-AS TOGGLE-BOX
     SIZE 19.14 BY .83 NO-UNDO.

DEFINE VARIABLE tg-impr-param AS LOGICAL INITIAL no 
     LABEL "Imprime Parƒmetros" 
     VIEW-AS TOGGLE-BOX
     SIZE 17.29 BY .75 NO-UNDO.

DEFINE VARIABLE tg-outros AS LOGICAL INITIAL no 
     LABEL "Outros" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE tg-pendentes AS LOGICAL INITIAL yes 
     LABEL "Pendentes" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .58 NO-UNDO.

DEFINE VARIABLE tg-pula-pag AS LOGICAL INITIAL no 
     LABEL "Salta P gina por Representante" 
     VIEW-AS TOGGLE-BOX
     SIZE 25.14 BY .83 NO-UNDO.

DEFINE VARIABLE tg-suspensos AS LOGICAL INITIAL yes 
     LABEL "Suspensos" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .58 NO-UNDO.

DEFINE VARIABLE tg-tp-pedido AS LOGICAL INITIAL yes 
     LABEL "Todos os Tipos de Pedido" 
     VIEW-AS TOGGLE-BOX
     SIZE 20.86 BY .83 NO-UNDO.

DEFINE VARIABLE fi-fim-cod-refer AS CHARACTER FORMAT "x(8)" INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "Referˆncia final" NO-UNDO.

DEFINE VARIABLE fi-fim-dt-cancel AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fim-dt-entrega AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fim-dt-implant AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fim-ge-codigo AS INTEGER FORMAT ">9":U INITIAL 99 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fim-it-codigo AS CHARACTER FORMAT "X(16)" INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fim-no-ab-reppri AS CHARACTER FORMAT "X(12)" INITIAL "ZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fim-nome-abrev AS CHARACTER FORMAT "X(12)" INITIAL "ZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fim-perc-comis AS DECIMAL FORMAT ">>9.99" INITIAL 99.99 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-cod-refer AS CHARACTER FORMAT "x(8)" 
     LABEL "Referˆncia":R12 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "Referˆncia inicial" NO-UNDO.

DEFINE VARIABLE fi-ini-dt-cancel AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Cancelamento" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-dt-entrega AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Entrega" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-dt-implant AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Implata‡Æo" 
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

DEFINE VARIABLE fi-ini-no-ab-reppri AS CHARACTER FORMAT "X(12)":U 
     LABEL "Representante" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-nome-abrev AS CHARACTER FORMAT "X(12)" 
     LABEL "Cliente":R17 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-perc-comis AS DECIMAL FORMAT ">>9.99" INITIAL 0 
     LABEL "% ComissÆo":R8 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-10
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-15
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-16
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

DEFINE IMAGE IMAGE-3
     FILENAME "image\im-fir":U
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
     SIZE 10 BY 1.

DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "Fechar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-executar 
     LABEL "Executar" 
     SIZE 10 BY 1.

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

DEFINE FRAME f-relat
     bt-executar AT ROW 14.54 COL 3 HELP
          "Dispara a execu‡Æo do relat¢rio"
     bt-cancelar AT ROW 14.54 COL 14 HELP
          "Fechar"
     bt-ajuda AT ROW 14.54 COL 70 HELP
          "Ajuda"
     RECT-6 AT ROW 13.75 COL 2.14
     rt-folder-right AT ROW 2.67 COL 80.43
     rt-folder-top AT ROW 2.54 COL 2.14
     rt-folder AT ROW 2.5 COL 2
     rt-folder-left AT ROW 2.54 COL 2.14
     RECT-1 AT ROW 14.29 COL 2
     im-pg-imp AT ROW 1.5 COL 33.43
     im-pg-par AT ROW 1.5 COL 17.72
     im-pg-sel AT ROW 1.5 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80.14 BY 15
         DEFAULT-BUTTON bt-executar.

DEFINE FRAME f-pg-sel
     fi-ini-no-ab-reppri AT ROW 2 COL 19 COLON-ALIGNED
     fi-fim-no-ab-reppri AT ROW 2 COL 48 COLON-ALIGNED HELP
          "Nome abreviado do representante principal" NO-LABEL
     fi-ini-nome-abrev AT ROW 3 COL 19 COLON-ALIGNED HELP
          "Nome abreviado do cliente/fornecedor"
     fi-fim-nome-abrev AT ROW 3 COL 48 COLON-ALIGNED HELP
          "Nome abreviado do cliente/fornecedor" NO-LABEL
     fi-ini-ge-codigo AT ROW 4 COL 19 COLON-ALIGNED
     fi-fim-ge-codigo AT ROW 4 COL 48 COLON-ALIGNED NO-LABEL
     fi-ini-it-codigo AT ROW 4.92 COL 19 COLON-ALIGNED
     fi-fim-it-codigo AT ROW 5 COL 48 COLON-ALIGNED NO-LABEL
     fi-ini-cod-refer AT ROW 6 COL 19 COLON-ALIGNED
     fi-fim-cod-refer AT ROW 6 COL 48 COLON-ALIGNED NO-LABEL
     fi-ini-dt-entrega AT ROW 7 COL 19 COLON-ALIGNED
     fi-fim-dt-entrega AT ROW 7 COL 48 COLON-ALIGNED NO-LABEL
     fi-ini-dt-implant AT ROW 8 COL 19 COLON-ALIGNED
     fi-fim-dt-implant AT ROW 8 COL 48 COLON-ALIGNED NO-LABEL
     fi-ini-dt-cancel AT ROW 9 COL 19 COLON-ALIGNED
     fi-fim-dt-cancel AT ROW 9 COL 48 COLON-ALIGNED NO-LABEL
     fi-ini-perc-comis AT ROW 9.96 COL 19 COLON-ALIGNED
     fi-fim-perc-comis AT ROW 9.96 COL 48 COLON-ALIGNED NO-LABEL
     IMAGE-10 AT ROW 2 COL 46.57
     IMAGE-15 AT ROW 6 COL 39
     IMAGE-16 AT ROW 6 COL 46.57
     IMAGE-19 AT ROW 4 COL 39
     IMAGE-20 AT ROW 4 COL 46.57
     IMAGE-21 AT ROW 5 COL 39
     IMAGE-22 AT ROW 5 COL 46.57
     IMAGE-23 AT ROW 9 COL 39
     IMAGE-24 AT ROW 9 COL 46.57
     IMAGE-25 AT ROW 10 COL 39
     IMAGE-26 AT ROW 10 COL 46.57
     IMAGE-3 AT ROW 8 COL 39
     IMAGE-4 AT ROW 8 COL 46.57
     IMAGE-5 AT ROW 7 COL 39
     IMAGE-6 AT ROW 7 COL 46.57
     IMAGE-7 AT ROW 3 COL 39
     IMAGE-8 AT ROW 3 COL 46.57
     IMAGE-9 AT ROW 2 COL 39
     RECT-24 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.83
         SIZE 77.43 BY 10.83
         FONT 1.

DEFINE FRAME f-pg-par
     tg-crd-nava AT ROW 2.21 COL 28.14
     tg-abertos AT ROW 2.25 COL 3.86
     rs-cond-pagto AT ROW 2.25 COL 54 NO-LABEL
     tg-crd-aval AT ROW 3.04 COL 28.14
     tg-at-parcial AT ROW 3.17 COL 3.86
     tg-crd-aprv AT ROW 3.92 COL 28.14
     tg-at-total AT ROW 4.13 COL 3.86
     tg-crd-repr AT ROW 4.79 COL 28.14
     tg-pendentes AT ROW 5.08 COL 3.86
     tg-suspensos AT ROW 6.04 COL 3.86
     cb-mercado AT ROW 6.38 COL 33 COLON-ALIGNED
     cb-artigo AT ROW 6.38 COL 55.57 COLON-ALIGNED
     tg-cancelados AT ROW 7 COL 3.86
     rs-qtd-ped AT ROW 7.63 COL 50.57 NO-LABEL
     tg-outros AT ROW 7.83 COL 3.86
     tg-tp-pedido AT ROW 9.21 COL 3.86
     rs-tp-relat AT ROW 9.21 COL 41.29 NO-LABEL
     tg-pula-pag AT ROW 10.38 COL 28.29
     fi-tp-pedido AT ROW 10.42 COL 16.72 COLON-ALIGNED HELP
          "Dispon¡vel para classifica‡Æo/indica‡Æo pr¢pria do usu rio" NO-LABEL
     tg-impr-param AT ROW 10.5 COL 57
     RECT-17 AT ROW 1.17 COL 1
     RECT-18 AT ROW 1.75 COL 2
     RECT-19 AT ROW 1.75 COL 25.43
     RECT-20 AT ROW 1.79 COL 51.57
     RECT-21 AT ROW 5.96 COL 25.43
     RECT-22 AT ROW 9 COL 2
     RECT-32 AT ROW 9 COL 25.43
     "  Situa‡Æo dos Pedidos" VIEW-AS TEXT
          SIZE 17 BY .75 AT ROW 1.42 COL 3
     " Condi‡Æo de Cr‚dito" VIEW-AS TEXT
          SIZE 16 BY .75 AT ROW 1.46 COL 26
     "Listar Pedidos pela Quantidade:" VIEW-AS TEXT
          SIZE 22 BY .88 AT ROW 7.54 COL 28.14
     " Condi‡Æo de Pagamento" VIEW-AS TEXT
          SIZE 18.57 BY .75 AT ROW 1.5 COL 52.57
     "Tiipo de Relat¢rio:" VIEW-AS TEXT
          SIZE 13 BY .88 AT ROW 9.17 COL 28
     "Pedidos do Tipo:" VIEW-AS TEXT
          SIZE 12 BY .75 AT ROW 10.38 COL 6.86
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
         TITLE              = "Pedidos de Venda por Repres/Cliente/Reserva"
         COLUMN             = 26
         ROW                = 8.96
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
/* SETTINGS FOR FRAME f-pg-sel
                                                                        */
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
ON END-ERROR OF w-relat /* Pedidos de Venda por Repres/Cliente/Reserva */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON WINDOW-CLOSE OF w-relat /* Pedidos de Venda por Repres/Cliente/Reserva */
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
       run pi-executar.
   end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-sel
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


&Scoped-define SELF-NAME fi-fim-no-ab-reppri
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
ON MOUSE-SELECT-DBLCLICK OF fi-fim-nome-abrev IN FRAME f-pg-sel
DO:
   {include/zoomvar.i &prog-zoom = adzoom/z02ad098.w
                      &campo     = fi-fim-nome-abrev
                      &campozoom = nome-abrev
                      &FRAME     = f-pg-sel}
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


&Scoped-define SELF-NAME fi-ini-no-ab-reppri
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

{utp/ut9000.i "ESPD0014" "2.04.00.000"} 

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
    fi-ini-no-ab-reppri:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-fim-no-ab-reppri:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-ini-nome-abrev:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-fim-nome-abrev:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-ini-ge-codigo:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-fim-ge-codigo:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
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
  ENABLE bt-executar bt-cancelar bt-ajuda im-pg-imp im-pg-par im-pg-sel 
      WITH FRAME f-relat IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}
  DISPLAY tg-crd-nava tg-abertos rs-cond-pagto tg-crd-aval tg-at-parcial 
          tg-crd-aprv tg-at-total tg-crd-repr tg-pendentes tg-suspensos 
          cb-mercado cb-artigo tg-cancelados rs-qtd-ped tg-outros tg-tp-pedido 
          rs-tp-relat tg-pula-pag fi-tp-pedido tg-impr-param 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  ENABLE tg-crd-nava tg-abertos rs-cond-pagto tg-crd-aval tg-at-parcial 
         tg-crd-aprv tg-at-total tg-crd-repr tg-pendentes tg-suspensos 
         cb-mercado cb-artigo tg-cancelados rs-qtd-ped tg-outros tg-tp-pedido 
         rs-tp-relat tg-pula-pag tg-impr-param RECT-17 RECT-18 RECT-19 RECT-20 
         RECT-21 RECT-22 RECT-32 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-par}
  DISPLAY rs-destino c-arquivo rs-execucao 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  ENABLE rs-destino bt-arquivo bt-config-impr c-arquivo rs-execucao RECT-16 
         RECT-7 RECT-9 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}
  DISPLAY fi-ini-no-ab-reppri fi-fim-no-ab-reppri fi-ini-nome-abrev 
          fi-fim-nome-abrev fi-ini-ge-codigo fi-fim-ge-codigo fi-ini-it-codigo 
          fi-fim-it-codigo fi-ini-cod-refer fi-fim-cod-refer fi-ini-dt-entrega 
          fi-fim-dt-entrega fi-ini-dt-implant fi-fim-dt-implant fi-ini-dt-cancel 
          fi-fim-dt-cancel fi-ini-perc-comis fi-fim-perc-comis 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  ENABLE fi-ini-no-ab-reppri fi-fim-no-ab-reppri fi-ini-nome-abrev 
         fi-fim-nome-abrev fi-ini-ge-codigo fi-fim-ge-codigo fi-ini-it-codigo 
         fi-fim-it-codigo fi-ini-cod-refer fi-fim-cod-refer fi-ini-dt-entrega 
         fi-fim-dt-entrega fi-ini-dt-implant fi-fim-dt-implant fi-ini-dt-cancel 
         fi-fim-dt-cancel fi-ini-perc-comis fi-fim-perc-comis IMAGE-10 IMAGE-15 
         IMAGE-16 IMAGE-19 IMAGE-20 IMAGE-21 IMAGE-22 IMAGE-23 IMAGE-24 
         IMAGE-25 IMAGE-26 IMAGE-3 IMAGE-4 IMAGE-5 IMAGE-6 IMAGE-7 IMAGE-8 
         IMAGE-9 RECT-24 
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

     ASSIGN tt-param.c-repres-ini  = input frame f-pg-sel fi-ini-no-ab-reppri
            tt-param.c-repres-fim  = input frame f-pg-sel fi-fim-no-ab-reppri
            tt-param.c-cliente-ini = input frame f-pg-sel fi-ini-nome-abrev
            tt-param.c-cliente-fim = input frame f-pg-sel fi-fim-nome-abrev
            tt-param.i-grupo-ini   = input frame f-pg-sel fi-ini-ge-codigo
            tt-param.i-grupo-fim   = input frame f-pg-sel fi-fim-ge-codigo
            tt-param.c-item-ini    = input frame f-pg-sel fi-ini-it-codigo 
            tt-param.c-item-fim    = input frame f-pg-sel fi-fim-it-codigo 
            tt-param.c-ref-ini     = input frame f-pg-sel fi-ini-cod-refer
            tt-param.c-ref-fim     = input frame f-pg-sel fi-fim-cod-refer
            tt-param.da-entr-ini   = input frame f-pg-sel fi-ini-dt-entrega 
            tt-param.da-entr-fim   = input frame f-pg-sel fi-fim-dt-entrega 
            tt-param.da-impl-ini   = input frame f-pg-sel fi-ini-dt-implant 
            tt-param.da-impl-fim   = input frame f-pg-sel fi-fim-dt-implant 
            tt-param.da-can-ini    = input frame f-pg-sel fi-ini-dt-cancel
            tt-param.da-can-fim    = input frame f-pg-sel fi-fim-dt-cancel
            tt-param.de-percom-ini = input frame f-pg-sel fi-ini-perc-comis
            tt-param.de-percom-fim = input frame f-pg-sel fi-fim-perc-comis
            tt-param.c-cond-pag    = IF INPUT FRAME f-pg-par rs-cond-pagto = 1
                                     THEN "V"
                                     ELSE IF INPUT FRAME f-pg-par rs-cond-pagto = 2
                                          THEN "P" ELSE "T"
            tt-param.c-saldo       = IF INPUT FRAME f-pg-par rs-qtd-ped = 1
                                     THEN "A"
                                     ELSE "T" 
            tt-param.c-tipo-rel    = IF INPUT FRAME f-pg-par rs-tp-relat = 1
                                     THEN "D"
                                     ELSE IF INPUT FRAME f-pg-par rs-tp-relat = 2
                                          THEN "R" ELSE "C"
            tt-param.c-tipo-merc   = SUBSTR(INPUT FRAME f-pg-par cb-mercado,1,1) 
            tt-param.c-tipo-artigo = SUBSTR(INPUT FRAME f-pg-par cb-artigo,1,1) 
            tt-param.c-tp-pedido   = INPUT FRAME f-pg-par fi-tp-pedido
            tt-param.l-ped-abe     = INPUT FRAME f-pg-par tg-abertos
            tt-param.l-ped-atp     = INPUT FRAME f-pg-par tg-at-parcial
            tt-param.l-ped-att     = INPUT FRAME f-pg-par tg-at-total
            tt-param.l-ped-pen     = INPUT FRAME f-pg-par tg-pendentes
            tt-param.l-ped-sus     = INPUT FRAME f-pg-par tg-suspensos
            tt-param.l-ped-can     = INPUT FRAME f-pg-par tg-cancelados
            tt-param.l-ped-out     = INPUT FRAME f-pg-par tg-outros
            tt-param.l-crd-nava    = INPUT FRAME f-pg-par tg-crd-nava
            tt-param.l-crd-aval    = INPUT FRAME f-pg-par tg-crd-aval
            tt-param.l-crd-aprv    = INPUT FRAME f-pg-par tg-crd-aprv
            tt-param.l-crd-repr    = INPUT FRAME f-pg-par tg-crd-repr
            tt-param.l-pula-pag    = INPUT FRAME f-pg-par tg-pula-pag
            tt-param.impr-param    = INPUT FRAME f-pg-par tg-impr-param.

    /*:T Executar do programa RP.P que ir  criar o relat¢rio */
    {include/i-rpexb.i}
    
    SESSION:SET-WAIT-STATE("general":U).
    
    {include/i-rprun.i esrp/espd0014rp.p} 
    
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

