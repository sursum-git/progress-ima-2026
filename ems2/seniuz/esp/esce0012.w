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
{include/i-prgvrs.i ESCE0012 2.04.00.000}   
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
       FIELD c-est-ini        LIKE estabelec.cod-estabel
       FIELD c-est-fim        LIKE estabelec.cod-estabel
       FIELD c-item-ini       LIKE ITEM.it-codigo
       FIELD c-item-fim       LIKE ITEM.it-codigo
       FIELD c-ref-ini        LIKE ref-item.cod-refer
       FIELD c-ref-fim        LIKE ref-item.cod-refer
       FIELD desenho-ini      AS CHAR FORMAT "x(4)"
       FIELD desenho-fin      AS CHAR FORMAT "x(4)"
       FIELD variante-ini     AS CHAR FORMAT "x(1)"
       FIELD variante-fin     AS CHAR FORMAT "x(1)"
       FIELD c-cod-depos      LIKE deposito.cod-depos
       FIELD sit-lancamen     AS LOGICAL FORMAT "Sim/Nao"
       FIELD sit-foraprod     AS LOGICAL FORMAT "Sim/Nao"
       FIELD sit-emprod       AS LOGICAL FORMAT "Sim/Nao"
       FIELD sit-retalho      AS LOGICAL FORMAT "Sim/Nao"
       FIELD sit-exclusiv     AS LOGICAL FORMAT "Sim/Nao"
       FIELD sit-exportacao   AS LOGICAL FORMAT "Sim/Nao"
       FIELD l-teccru         AS LOGICAL
       FIELD l-sldneg         AS LOGICAL    
       FIELD l-sldzer         AS LOGICAL
       FIELD l-sldpos         AS LOGICAL
       FIELD c-opc-acond      AS CHAR
       FIELD c-opc-qualid     AS CHAR
       FIELD gerar-resumo     AS LOGICAL FORMAT "Sim/NÆo"
       FIELD c-opc-acab       AS CHAR
       FIELD c-opc-artigo     AS CHAR
       FIELD c-estoq-bloq     AS CHAR
       FIELD c-opc-rel        AS CHAR
       FIELD detalhado        AS LOGICAL FORMAT "Sim/NÆo"
       FIELD resumido         AS LOGICAL FORMAT "Sim/NÆo"
       FIELD de-sld-min       AS DEC FORMAT "->>,>>>,>>9.99" 
       FIELD l-pula-pag       AS LOGICAL
       FIELD gerar-excel      AS LOGICAL FORMAT "Sim/NÆo"
       FIELD arq-excel        AS CHAR FORMAT "x(45)"
       FIELD gerar-etiqueta   AS LOGICAL FORMAT "Sim/NÆo"
       FIELD arq-etiqueta     AS CHAR FORMAT "x(45)"
       FIELD impr-param       AS LOGICAL.

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
c-arquivo rs-execucao RECT-17 RECT-7 RECT-9 
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

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 77 BY 10.67.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 46.29 BY 2.92.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 46.29 BY 1.71.

DEFINE VARIABLE fi-arq-etiqueta AS CHARACTER FORMAT "X(256)":U 
     LABEL "Arquivo" 
     VIEW-AS FILL-IN 
     SIZE 36 BY .88 NO-UNDO.

DEFINE VARIABLE fi-arq-excel AS CHARACTER FORMAT "X(256)":U 
     LABEL "Arquivo" 
     VIEW-AS FILL-IN 
     SIZE 36 BY .88 NO-UNDO.

DEFINE VARIABLE fi-sld-min AS DECIMAL FORMAT "->>,>>>,>>9.99":U INITIAL 0 
     LABEL "Saldo Venda Minimo" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .88 TOOLTIP "Valor limite do SALDO VENDA para impressao" NO-UNDO.

DEFINE VARIABLE rs-estoq-bloq AS CHARACTER INITIAL "L" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Bloqueado", "B",
"Liberado", "L",
"Ambos", "A"
     SIZE 34.86 BY .75 TOOLTIP "Incluir estoque bloqueado: Inclusive, Exclusive ou Ambos." NO-UNDO.

DEFINE VARIABLE rs-opc-acab AS INTEGER INITIAL 3 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Liso", 1,
"Estampado", 2,
"Ambos", 3
     SIZE 13.72 BY 2.63 NO-UNDO.

DEFINE VARIABLE rs-opc-acond AS INTEGER INITIAL 3 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Rolo", 1,
"Pe‡a", 2,
"Ambos", 3
     SIZE 10 BY 2.63 NO-UNDO.

DEFINE VARIABLE rs-opc-artigo AS CHARACTER INITIAL "A" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Indigo", "I",
"Outros", "O",
"Ambos", "A"
     SIZE 34.43 BY 1 NO-UNDO.

DEFINE VARIABLE rs-opc-rel AS INTEGER INITIAL 3 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Dispon¡vel", 1,
"Venda", 2,
"Ambos", 3
     SIZE 13.72 BY 2.63 NO-UNDO.

DEFINE RECTANGLE RECT-15
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 77 BY 10.67.

DEFINE RECTANGLE RECT-26
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 16 BY 3.

DEFINE RECTANGLE RECT-27
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 71 BY 4.46.

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 16.86 BY 3.

DEFINE RECTANGLE RECT-29
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 17.57 BY 3.

DEFINE RECTANGLE RECT-30
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 52.29 BY 1.96.

DEFINE RECTANGLE RECT-32
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 18 BY 5.13.

DEFINE VARIABLE tg-acabamento AS LOGICAL INITIAL no 
     LABEL "Acabamento" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE tg-detalhado AS LOGICAL INITIAL yes 
     LABEL "Detalhado" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE tg-gerar-etiqueta AS LOGICAL INITIAL no 
     LABEL "Gerar Dados Etiqueta" 
     VIEW-AS TOGGLE-BOX
     SIZE 17.14 BY .83 TOOLTIP "Imprimir Parƒmetros/Sele‡Æo ao final do relat¢rio."
     FONT 1 NO-UNDO.

DEFINE VARIABLE tg-gerar-excel AS LOGICAL INITIAL no 
     LABEL "Gerar Planilha Excel" 
     VIEW-AS TOGGLE-BOX
     SIZE 18.14 BY .83 TOOLTIP "Imprimir Parƒmetros/Sele‡Æo ao final do relat¢rio."
     FONT 1 NO-UNDO.

DEFINE VARIABLE tg-impr-param AS LOGICAL INITIAL no 
     LABEL "Imprime Parƒmetros" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .83 NO-UNDO.

DEFINE VARIABLE tg-pula-pag AS LOGICAL INITIAL no 
     LABEL "Salta P gina por Item" 
     VIEW-AS TOGGLE-BOX
     SIZE 19.14 BY .83 NO-UNDO.

DEFINE VARIABLE tg-resumido AS LOGICAL INITIAL no 
     LABEL "Resumido" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE tg-sldneg AS LOGICAL INITIAL no 
     LABEL "Negativo" 
     VIEW-AS TOGGLE-BOX
     SIZE 10.29 BY .83 NO-UNDO.

DEFINE VARIABLE tg-sldpos AS LOGICAL INITIAL yes 
     LABEL "Positivo" 
     VIEW-AS TOGGLE-BOX
     SIZE 9.29 BY .83 NO-UNDO.

DEFINE VARIABLE tg-sldzer AS LOGICAL INITIAL no 
     LABEL "Zero" 
     VIEW-AS TOGGLE-BOX
     SIZE 7.29 BY .83 NO-UNDO.

DEFINE VARIABLE tg-teccru AS LOGICAL INITIAL no 
     LABEL "Imprimir Itens de Tecido CRU" 
     VIEW-AS TOGGLE-BOX
     SIZE 23.86 BY .83 NO-UNDO.

DEFINE VARIABLE fi-cod-depos AS CHARACTER FORMAT "x(3)" INITIAL "EXP" 
     LABEL "Dep¢sito":R10 
     VIEW-AS FILL-IN 
     SIZE 4.57 BY .88 TOOLTIP "C¢digo do deposito de estoque" NO-UNDO.

DEFINE VARIABLE fi-desc-deposito AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desenho-fin AS CHARACTER FORMAT "X(4)":U INITIAL "9999" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 TOOLTIP "Desenho final" NO-UNDO.

DEFINE VARIABLE fi-desenho-ini AS CHARACTER FORMAT "X(4)":U INITIAL "0000" 
     LABEL "Desenho/Var" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 TOOLTIP "Desenho inicial" NO-UNDO.

DEFINE VARIABLE fi-fim-cod-estabel AS CHARACTER FORMAT "X(3)":U INITIAL "2" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fim-cod-refer AS CHARACTER FORMAT "X(8)":U INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fim-it-codigo AS CHARACTER FORMAT "x(16)" INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 17.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-cod-estabel AS CHARACTER FORMAT "X(3)":U INITIAL "2" 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-cod-refer AS CHARACTER FORMAT "X(8)":U 
     LABEL "Referˆncia" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-it-codigo AS CHARACTER FORMAT "x(16)" 
     LABEL "Item":R5 
     VIEW-AS FILL-IN 
     SIZE 17.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi-variante-fin AS CHARACTER FORMAT "X(1)":U INITIAL "9" 
     VIEW-AS FILL-IN 
     SIZE 2 BY .88 TOOLTIP "Variante final" NO-UNDO.

DEFINE VARIABLE fi-variante-ini AS CHARACTER FORMAT "X(1)":U INITIAL "0" 
     VIEW-AS FILL-IN 
     SIZE 2 BY .88 TOOLTIP "Variante inicial" NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-10
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
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

DEFINE VARIABLE rs-opc-qualid AS INTEGER INITIAL 3 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Perfeito", 1,
"Defeituoso", 2,
"Ambos", 3
     SIZE 16 BY 3 NO-UNDO.

DEFINE RECTANGLE RECT-16
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 77 BY 10.75.

DEFINE RECTANGLE RECT-25
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 35.43 BY 4.83.

DEFINE RECTANGLE RECT-31
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 31 BY 4.83.

DEFINE VARIABLE tg-sit-emprod AS LOGICAL INITIAL yes 
     LABEL "Em produ‡Æo (2)" 
     VIEW-AS TOGGLE-BOX
     SIZE 15.57 BY .83 NO-UNDO.

DEFINE VARIABLE tg-sit-exclusiv AS LOGICAL INITIAL yes 
     LABEL "Exclusividade (4)" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.57 BY .83 NO-UNDO.

DEFINE VARIABLE tg-sit-exportacao AS LOGICAL INITIAL yes 
     LABEL "Exporta‡Æo (5)" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.43 BY .83 NO-UNDO.

DEFINE VARIABLE tg-sit-foraprod AS LOGICAL INITIAL yes 
     LABEL "Fora de produ‡Æo (1)" 
     VIEW-AS TOGGLE-BOX
     SIZE 17.57 BY .83 NO-UNDO.

DEFINE VARIABLE tg-sit-normal AS LOGICAL INITIAL yes 
     LABEL "Lan‡amento (0)" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.57 BY .83 NO-UNDO.

DEFINE VARIABLE tg-sit-retalho AS LOGICAL INITIAL yes 
     LABEL "Retalho (3)" 
     VIEW-AS TOGGLE-BOX
     SIZE 10.57 BY .83 NO-UNDO.

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
     RECT-17 AT ROW 1.08 COL 1.29
     RECT-7 AT ROW 1.92 COL 2.14
     RECT-9 AT ROW 5.29 COL 2.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2.43 ROW 2.83
         SIZE 77.72 BY 10.83.

DEFINE FRAME f-relat
     bt-executar AT ROW 14.54 COL 3 HELP
          "Dispara a execu‡Æo do relat¢rio"
     bt-cancelar AT ROW 14.54 COL 14 HELP
          "Fechar"
     bt-ajuda AT ROW 14.54 COL 70 HELP
          "Ajuda"
     rt-folder-top AT ROW 2.54 COL 2.14
     rt-folder-left AT ROW 2.54 COL 2.14
     rt-folder-right AT ROW 2.67 COL 80.43
     RECT-6 AT ROW 13.75 COL 2.14
     RECT-1 AT ROW 14.29 COL 2
     rt-folder AT ROW 2.5 COL 2
     im-pg-imp AT ROW 1.5 COL 33.43
     im-pg-par AT ROW 1.5 COL 17.72
     im-pg-sel AT ROW 1.5 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81 BY 15
         DEFAULT-BUTTON bt-executar.

DEFINE FRAME f-pg-par
     tg-acabamento AT ROW 1.25 COL 61
     rs-opc-rel AT ROW 1.79 COL 40.57 NO-LABEL
     rs-opc-acond AT ROW 1.83 COL 24.43 NO-LABEL
     rs-opc-acab AT ROW 1.88 COL 58.86 NO-LABEL
     tg-sldneg AT ROW 1.92 COL 6.29
     tg-sldzer AT ROW 2.75 COL 6.29
     tg-sldpos AT ROW 3.58 COL 6.29
     tg-detalhado AT ROW 4.88 COL 59
     rs-estoq-bloq AT ROW 4.96 COL 20.14 NO-LABEL
     rs-opc-artigo AT ROW 5.63 COL 20 NO-LABEL
     tg-resumido AT ROW 5.67 COL 59
     tg-teccru AT ROW 7.29 COL 7.86
     fi-sld-min AT ROW 7.29 COL 58.43 COLON-ALIGNED
     tg-pula-pag AT ROW 8.25 COL 7.86
     tg-impr-param AT ROW 8.25 COL 47.43
     tg-gerar-excel AT ROW 9.21 COL 7.86
     fi-arq-excel AT ROW 9.21 COL 32.72 COLON-ALIGNED
     tg-gerar-etiqueta AT ROW 10.17 COL 7.86
     fi-arq-etiqueta AT ROW 10.17 COL 32.72 COLON-ALIGNED
     RECT-15 AT ROW 1 COL 1
     RECT-26 AT ROW 1.58 COL 4.14
     RECT-27 AT ROW 7.04 COL 4
     RECT-28 AT ROW 1.58 COL 21
     RECT-29 AT ROW 1.58 COL 38.72
     RECT-30 AT ROW 4.75 COL 4
     RECT-32 AT ROW 1.58 COL 57
     " Itens com Saldo" VIEW-AS TEXT
          SIZE 12.86 BY .71 AT ROW 1.25 COL 5.86
     "Estoque para Venda:" VIEW-AS TEXT
          SIZE 15 BY .54 AT ROW 4.96 COL 5.43
     " Outros" VIEW-AS TEXT
          SIZE 5.43 BY .75 AT ROW 6.67 COL 6.14
     " Acondicionamento" VIEW-AS TEXT
          SIZE 14 BY .75 AT ROW 1.17 COL 22.43
     " Saldo a Imprimir" VIEW-AS TEXT
          SIZE 12 BY .75 AT ROW 1.17 COL 40.57
     "Tipo de Artigo:" VIEW-AS TEXT
          SIZE 10.57 BY 1 AT ROW 5.58 COL 9.72
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2.72 ROW 2.83
         SIZE 77.29 BY 10.79
         FONT 1.

DEFINE FRAME f-pg-sel
     fi-ini-cod-estabel AT ROW 1.38 COL 18.43 COLON-ALIGNED
     fi-fim-cod-estabel AT ROW 1.38 COL 49 COLON-ALIGNED NO-LABEL
     fi-ini-it-codigo AT ROW 2.38 COL 18.43 COLON-ALIGNED HELP
          "C¢digo do Item"
     fi-fim-it-codigo AT ROW 2.38 COL 49 COLON-ALIGNED HELP
          "C¢digo do Item" NO-LABEL
     fi-ini-cod-refer AT ROW 3.38 COL 18.43 COLON-ALIGNED
     fi-fim-cod-refer AT ROW 3.38 COL 49 COLON-ALIGNED NO-LABEL
     fi-desenho-ini AT ROW 4.38 COL 18.43 COLON-ALIGNED
     fi-variante-ini AT ROW 4.38 COL 23.72 COLON-ALIGNED NO-LABEL
     fi-desenho-fin AT ROW 4.38 COL 49 COLON-ALIGNED NO-LABEL
     fi-variante-fin AT ROW 4.38 COL 54.29 COLON-ALIGNED NO-LABEL
     fi-cod-depos AT ROW 5.38 COL 18.43 COLON-ALIGNED
     fi-desc-deposito AT ROW 5.38 COL 23.14 COLON-ALIGNED NO-LABEL
     tg-sit-normal AT ROW 7.08 COL 6
     tg-sit-foraprod AT ROW 8.08 COL 6
     tg-sit-emprod AT ROW 9.08 COL 6
     tg-sit-retalho AT ROW 7.08 COL 24.57
     tg-sit-exclusiv AT ROW 8.08 COL 24.57
     rs-opc-qualid AT ROW 7.58 COL 49 NO-LABEL
     tg-sit-exportacao AT ROW 9.08 COL 24.57
     IMAGE-1 AT ROW 3.38 COL 39.57
     IMAGE-10 AT ROW 4.38 COL 47.14
     IMAGE-2 AT ROW 3.38 COL 47.14
     IMAGE-5 AT ROW 1.38 COL 39.57
     IMAGE-6 AT ROW 1.38 COL 47.14
     IMAGE-7 AT ROW 2.38 COL 39.57
     IMAGE-8 AT ROW 2.38 COL 47.14
     IMAGE-9 AT ROW 4.38 COL 39.57
     RECT-16 AT ROW 1 COL 1
     RECT-25 AT ROW 6.63 COL 4.57
     RECT-31 AT ROW 6.63 COL 43.29
     " Itens com Situa‡Æo" VIEW-AS TEXT
          SIZE 14.57 BY .67 AT ROW 6.29 COL 7.57
     "  Qualidade" VIEW-AS TEXT
          SIZE 9 BY .79 AT ROW 6.29 COL 44
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2.72 ROW 2.79
         SIZE 77.43 BY 10.88
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
         TITLE              = "Estoque Dispon¡vel por Item"
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
/* SETTINGS FOR FILL-IN fi-arq-etiqueta IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-arq-excel IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET rs-opc-acab IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-detalhado IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-resumido IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME f-pg-sel
   Custom                                                               */
/* SETTINGS FOR FILL-IN fi-desc-deposito IN FRAME f-pg-sel
   NO-ENABLE                                                            */
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
ON END-ERROR OF w-relat /* Estoque Dispon¡vel por Item */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON WINDOW-CLOSE OF w-relat /* Estoque Dispon¡vel por Item */
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


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME fi-arq-etiqueta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-arq-etiqueta w-relat
ON LEAVE OF fi-arq-etiqueta IN FRAME f-pg-par /* Arquivo */
DO:
  IF NOT fi-arq-excel:SCREEN-VALUE IN FRAME {&FRAME-NAME} MATCHES "*.csv*" THEN DO:
     MESSAGE "Nome do arquivo para Excel est  inv lido." SKIP
             "Deve ser do tipo CAMINHO/ARQUIVO.CSV" VIEW-AS ALERT-BOX.
     ASSIGN fi-arq-excel:SCREEN-VALUE IN FRAME f-pg-par = SESSION:TEMP-DIRECTORY + "esce0012.csv".
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-arq-excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-arq-excel w-relat
ON LEAVE OF fi-arq-excel IN FRAME f-pg-par /* Arquivo */
DO:
  IF NOT fi-arq-excel:SCREEN-VALUE IN FRAME {&FRAME-NAME} MATCHES "*.csv*" THEN DO:
     MESSAGE "Nome do arquivo para Excel est  inv lido." SKIP
             "Deve ser do tipo CAMINHO/ARQUIVO.CSV" VIEW-AS ALERT-BOX.
     ASSIGN fi-arq-excel:SCREEN-VALUE IN FRAME f-pg-par = SESSION:TEMP-DIRECTORY + "esce0012.csv".
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-sel
&Scoped-define SELF-NAME fi-cod-depos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-depos w-relat
ON ENTRY OF fi-cod-depos IN FRAME f-pg-sel /* Dep¢sito */
DO:
  FIND deposito WHERE deposito.cod-depos = INPUT FRAME {&FRAME-NAME} fi-cod-depos NO-LOCK NO-ERROR.
  IF AVAIL deposito THEN
     ASSIGN fi-desc-deposito:SCREEN-VALUE IN FRAME {&FRAME-NAME} = deposito.nome.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-depos w-relat
ON LEAVE OF fi-cod-depos IN FRAME f-pg-sel /* Dep¢sito */
DO:
  FIND deposito WHERE deposito.cod-depos = INPUT FRAME {&FRAME-NAME} fi-cod-depos NO-LOCK NO-ERROR.
  IF AVAIL deposito THEN
     ASSIGN fi-desc-deposito:SCREEN-VALUE IN FRAME {&FRAME-NAME} = deposito.nome.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-depos w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-cod-depos IN FRAME f-pg-sel /* Dep¢sito */
DO:
    {include/zoomvar.i &prog-zoom=inzoom/z01in084.w
                   &campo=fi-cod-depos
                   &campozoom=cod-depos
                   &FRAME=f-pg-sel}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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


&Scoped-define SELF-NAME fi-ini-it-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-it-codigo w-relat
ON LEAVE OF fi-ini-it-codigo IN FRAME f-pg-sel /* Item */
DO:
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
&Scoped-define SELF-NAME tg-acabamento
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-acabamento w-relat
ON VALUE-CHANGED OF tg-acabamento IN FRAME f-pg-par /* Acabamento */
DO:
   IF INPUT FRAME {&FRAME-NAME} tg-acabamento = NO THEN
      ASSIGN rs-opc-acab:SCREEN-VALUE IN FRAME f-pg-par = "3"
             rs-opc-acab:SENSITIVE IN FRAME {&FRAME-NAME} = NO
             tg-detalhado:SCREEN-VALUE IN FRAME f-pg-par = "YES"
             tg-detalhado:SENSITIVE IN FRAME {&FRAME-NAME} = NO
             tg-resumido:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
   ELSE
      ASSIGN rs-opc-acab:SENSITIVE IN FRAME {&FRAME-NAME} = YES
             rs-opc-acab:SCREEN-VALUE IN FRAME f-pg-par = "3"
             tg-detalhado:SCREEN-VALUE IN FRAME f-pg-par = "NO" 
             tg-detalhado:SENSITIVE IN FRAME {&FRAME-NAME} = YES 
             tg-resumido:SCREEN-VALUE IN FRAME f-pg-par = "NO" 
             tg-resumido:SENSITIVE IN FRAME {&FRAME-NAME} = YES. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-detalhado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-detalhado w-relat
ON VALUE-CHANGED OF tg-detalhado IN FRAME f-pg-par /* Detalhado */
DO:
   IF INPUT FRAME {&FRAME-NAME} tg-detalhado = YES THEN
      ASSIGN tg-resumido:SCREEN-VALUE IN FRAME f-pg-par = "NO".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-gerar-etiqueta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-gerar-etiqueta w-relat
ON VALUE-CHANGED OF tg-gerar-etiqueta IN FRAME f-pg-par /* Gerar Dados Etiqueta */
DO:
  IF INPUT FRAME {&FRAME-NAME} tg-gerar-etiqueta = NO THEN
     ASSIGN fi-arq-etiqueta:SENSITIVE IN FRAME {&FRAME-NAME} = NO
            fi-arq-etiqueta:SCREEN-VALUE IN FRAME f-pg-par = "".
  ELSE
     ASSIGN fi-arq-etiqueta:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            fi-arq-etiqueta:SCREEN-VALUE IN FRAME f-pg-par = SESSION:TEMP-DIRECTORY + "esce0012e.csv". 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-gerar-excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-gerar-excel w-relat
ON VALUE-CHANGED OF tg-gerar-excel IN FRAME f-pg-par /* Gerar Planilha Excel */
DO:
  IF INPUT FRAME {&FRAME-NAME} tg-gerar-excel = NO THEN
     ASSIGN fi-arq-excel:SENSITIVE IN FRAME {&FRAME-NAME} = NO
            fi-arq-excel:SCREEN-VALUE IN FRAME f-pg-par = "".
  ELSE
     ASSIGN fi-arq-excel:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            fi-arq-excel:SCREEN-VALUE IN FRAME f-pg-par = SESSION:TEMP-DIRECTORY + "esce0012.csv". 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-resumido
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-resumido w-relat
ON VALUE-CHANGED OF tg-resumido IN FRAME f-pg-par /* Resumido */
DO:
   IF INPUT FRAME {&FRAME-NAME} tg-resumido = YES THEN
      ASSIGN tg-detalhado:SCREEN-VALUE IN FRAME f-pg-par = "NO".
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

{utp/ut9000.i "ESCE0012" "2.00.04.000"} 

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

    fi-ini-cod-estabel:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-fim-cod-estabel:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-ini-it-codigo:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-fim-it-codigo:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-cod-depos:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    
    ASSIGN fi-desc-deposito:SCREEN-VALUE IN FRAME f-pg-sel = "EXPEDI€ÇO".

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
  DISPLAY fi-ini-cod-estabel fi-fim-cod-estabel fi-ini-it-codigo 
          fi-fim-it-codigo fi-ini-cod-refer fi-fim-cod-refer fi-desenho-ini 
          fi-variante-ini fi-desenho-fin fi-variante-fin fi-cod-depos 
          fi-desc-deposito tg-sit-normal tg-sit-foraprod tg-sit-emprod 
          tg-sit-retalho tg-sit-exclusiv rs-opc-qualid tg-sit-exportacao 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  ENABLE fi-ini-cod-estabel fi-fim-cod-estabel fi-ini-it-codigo 
         fi-fim-it-codigo fi-ini-cod-refer fi-fim-cod-refer fi-desenho-ini 
         fi-variante-ini fi-desenho-fin fi-variante-fin fi-cod-depos 
         tg-sit-normal tg-sit-foraprod tg-sit-emprod tg-sit-retalho 
         tg-sit-exclusiv rs-opc-qualid tg-sit-exportacao IMAGE-1 IMAGE-10 
         IMAGE-2 IMAGE-5 IMAGE-6 IMAGE-7 IMAGE-8 IMAGE-9 RECT-16 RECT-25 
         RECT-31 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}
  DISPLAY rs-destino c-arquivo rs-execucao 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  ENABLE rs-destino bt-arquivo bt-config-impr c-arquivo rs-execucao RECT-17 
         RECT-7 RECT-9 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}
  DISPLAY tg-acabamento rs-opc-rel rs-opc-acond rs-opc-acab tg-sldneg tg-sldzer 
          tg-sldpos tg-detalhado rs-estoq-bloq rs-opc-artigo tg-resumido 
          tg-teccru fi-sld-min tg-pula-pag tg-impr-param tg-gerar-excel 
          fi-arq-excel tg-gerar-etiqueta fi-arq-etiqueta 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  ENABLE tg-acabamento rs-opc-rel rs-opc-acond tg-sldneg tg-sldzer tg-sldpos 
         rs-estoq-bloq rs-opc-artigo tg-teccru fi-sld-min tg-pula-pag 
         tg-impr-param tg-gerar-excel tg-gerar-etiqueta RECT-15 RECT-26 RECT-27 
         RECT-28 RECT-29 RECT-30 RECT-32 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-par}
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
    
    create tt-param.
    assign tt-param.usuario   = c-seg-usuario
           tt-param.destino   = input frame f-pg-imp rs-destino
           tt-param.data-exec = today
           tt-param.hora-exec = time.
    
    if tt-param.destino = 1 
    then assign tt-param.arquivo = "".
    else if  tt-param.destino = 2 
         then assign tt-param.arquivo = input frame f-pg-imp c-arquivo.
         else assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp":U.
    
    /*:T Coloque aqui a l¢gica de grava‡Æo dos demais campos que devem ser passados
       como parƒmetros para o programa RP.P, atrav‚s da temp-table tt-param */

    ASSIGN tt-param.c-est-ini      = INPUT FRAME f-pg-sel fi-ini-cod-estabel
           tt-param.c-est-fim      = INPUT FRAME f-pg-sel fi-fim-cod-estabel
           tt-param.c-item-ini     = INPUT FRAME f-pg-sel fi-ini-it-codigo
           tt-param.c-item-fim     = INPUT FRAME f-pg-sel fi-fim-it-codigo
           tt-param.c-ref-ini      = INPUT FRAME f-pg-sel fi-ini-cod-refer
           tt-param.c-ref-fim      = INPUT FRAME f-pg-sel fi-fim-cod-refer
           tt-param.desenho-ini    = INPUT FRAME f-pg-sel fi-desenho-ini
           tt-param.desenho-fin    = INPUT FRAME f-pg-sel fi-desenho-fin
           tt-param.variante-ini   = INPUT FRAME f-pg-sel fi-variante-ini
           tt-param.variante-fin   = INPUT FRAME f-pg-sel fi-variante-fin
           tt-param.c-cod-depos    = INPUT FRAME f-pg-sel fi-cod-depos
           tt-param.sit-lancamen   = INPUT FRAME f-pg-sel tg-sit-normal
           tt-param.sit-foraprod   = INPUT FRAME f-pg-sel tg-sit-foraprod
           tt-param.sit-emprod     = INPUT FRAME f-pg-sel tg-sit-emprod  
           tt-param.sit-retalho    = INPUT FRAME f-pg-sel tg-sit-retalho 
           tt-param.sit-exclusiv   = INPUT FRAME f-pg-sel tg-sit-exclusiv
           tt-param.sit-exportacao = INPUT FRAME f-pg-sel tg-sit-exportacao
           tt-param.c-opc-qualid   = IF INPUT FRAME f-pg-sel rs-opc-qualid = 1
                                     THEN "P"
                                     ELSE IF INPUT FRAME f-pg-sel rs-opc-qualid = 2
                                          THEN "D"
                                          ELSE "A"
           tt-param.l-sldneg       = INPUT FRAME f-pg-par tg-sldneg    
           tt-param.l-sldzer       = INPUT FRAME f-pg-par tg-sldzer    
           tt-param.l-sldpos       = INPUT FRAME f-pg-par tg-sldpos    
           tt-param.c-opc-acond    = IF INPUT FRAME f-pg-par rs-opc-acond = 1
                                     THEN "R"
                                     ELSE IF INPUT FRAME f-pg-par rs-opc-acond = 2
                                          THEN "P"
                                          ELSE "A"
           tt-param.c-opc-artigo   = INPUT FRAME f-pg-par rs-opc-artigo
           tt-param.c-estoq-bloq   = INPUT FRAME f-pg-par rs-estoq-bloq
           tt-param.c-opc-rel      = IF INPUT FRAME f-pg-par rs-opc-rel = 1
                                     THEN "D"
                                     ELSE IF INPUT FRAME f-pg-par rs-opc-rel = 2
                                          THEN "V"
                                          ELSE "A"
           tt-param.detalhado      = INPUT FRAME f-pg-par tg-detalhado
           tt-param.resumido       = INPUT FRAME f-pg-par tg-resumido
           tt-param.gerar-resumo   = INPUT FRAME f-pg-par tg-acabamento
           tt-param.c-opc-acab     = IF INPUT FRAME f-pg-par rs-opc-acab = 1
                                     THEN "L"
                                     ELSE IF INPUT FRAME f-pg-par rs-opc-acab = 2
                                          THEN "E"
                                          ELSE "A"
                                  
           tt-param.l-teccru        = INPUT FRAME f-pg-par tg-teccru
           tt-param.de-sld-min      = INPUT FRAME f-pg-par fi-sld-min
           tt-param.l-pula-pag      = INPUT FRAME f-pg-par tg-pula-pag
           tt-param.gerar-excel     = INPUT FRAME f-pg-par tg-gerar-excel
           tt-param.arq-excel       = INPUT FRAME f-pg-par fi-arq-excel
           tt-param.gerar-etiqueta  = INPUT FRAME f-pg-par tg-gerar-etiqueta
           tt-param.arq-etiqueta    = INPUT FRAME f-pg-par fi-arq-etiqueta
           tt-param.impr-param      = INPUT FRAME f-pg-par tg-impr-param.

    /*:T Executar do programa RP.P que ir  criar o relat¢rio */
    {include/i-rpexb.i}
    
    SESSION:SET-WAIT-STATE("general":U).
    
    {include/i-rprun.i esrp/esce0012rp.p} 
    
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

