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
{include/i-prgvrs.i ESCE0003 2.04.00.000}

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

DEFINE TEMP-TABLE tt-param    NO-UNDO
       FIELD destino          AS INTEGER
       FIELD arquivo          AS CHAR FORMAT "x(35)"
       FIELD usuario          AS CHAR FORMAT "x(12)"
       FIELD data-exec        AS DATE
       FIELD hora-exec        AS INTEGER
       field classifica       AS INTEGER
       FIELD desc-classifica  AS CHAR FORMAT "x(45)"
       FIELD cod-estabel      LIKE movto-estoq.cod-estabel
       FIELD ge-codigo-ini    LIKE ITEM.ge-codigo
       FIELD ge-codigo-fin    LIKE ITEM.ge-codigo
       FIELD it-codigo-ini    LIKE ITEM.it-codigo
       FIELD it-codigo-fin    LIKE ITEM.it-codigo
       FIELD cod-refer-ini    LIKE movto-estoq.cod-refer
       FIELD cod-refer-fin    LIKE movto-estoq.cod-refer
       FIELD dt-trans-ini     LIKE movto-estoq.dt-trans 
       FIELD dt-trans-fin     LIKE movto-estoq.dt-trans
       FIELD serie-docto-ini  LIKE movto-estoq.serie-docto
       FIELD serie-docto-fin  LIKE movto-estoq.serie-docto
       FIELD all-depos        AS LOG FORMAT "Sim/NÆo"
       FIELD cod-depos1       LIKE deposito.cod-depos
       FIELD cod-depos2       LIKE deposito.cod-depos
       FIELD cod-depos3       LIKE deposito.cod-depos
       FIELD cod-depos4       LIKE deposito.cod-depos
       FIELD cod-depos5       LIKE deposito.cod-depos
       FIELD cod-depos6       LIKE deposito.cod-depos
       FIELD cod-depos7       LIKE deposito.cod-depos
       FIELD cod-depos8       LIKE deposito.cod-depos
       FIELD cod-depos9       LIKE deposito.cod-depos
       FIELD cod-depos10      LIKE deposito.cod-depos
       FIELD esp-docto1       LIKE movto-estoq.esp-docto
       FIELD esp-docto2       LIKE movto-estoq.esp-docto
       FIELD esp-docto3       LIKE movto-estoq.esp-docto
       FIELD esp-docto4       LIKE movto-estoq.esp-docto
       FIELD esp-docto5       LIKE movto-estoq.esp-docto
       FIELD esp-docto6       LIKE movto-estoq.esp-docto
       FIELD esp-docto7       LIKE movto-estoq.esp-docto
       FIELD esp-docto8       LIKE movto-estoq.esp-docto
       FIELD esp-docto9       LIKE movto-estoq.esp-docto
       FIELD esp-docto10      LIKE movto-estoq.esp-docto
       FIELD gerar-excel      AS LOG FORMAT "Sim/NÆo"
       FIELD arq-excel        AS CHAR FORMAT "x(45)"
       FIELD tipo-rel         AS INT
       FIELD desc-tipo-rel    AS CHAR FORMAT "x(10)"
       FIELD tipo-valor       AS INT
       FIELD desc-tipo-valor  AS CHAR FORMAT "x(10)"
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

DEF VAR dt-aux     AS DATE.
DEF VAR i-ct       AS INT.
DEF VAR c-especies AS CHAR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-relat
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-pg-cla

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rs-classif 
&Scoped-Define DISPLAYED-OBJECTS rs-classif 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-4 fi-cod-depos1 fi-cod-depos2 fi-cod-depos3 ~
fi-cod-depos4 fi-cod-depos5 fi-cod-depos6 fi-cod-depos7 fi-cod-depos8 ~
fi-cod-depos9 fi-cod-depos10 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-relat AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE rs-classif AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Por C¢digo do Ötem", 1,
"Por Descri‡Æo do Ötem", 2
     SIZE 19.29 BY 2.75
     FONT 1 NO-UNDO.

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

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 2.92.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 1.71.

DEFINE VARIABLE cb-esp-docto1 AS CHARACTER FORMAT "X(5)":U 
     VIEW-AS COMBO-BOX SORT INNER-LINES 8
     DROP-DOWN-LIST
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE cb-esp-docto10 AS CHARACTER FORMAT "X(5)":U 
     VIEW-AS COMBO-BOX SORT INNER-LINES 8
     DROP-DOWN-LIST
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE cb-esp-docto2 AS CHARACTER FORMAT "X(5)":U 
     VIEW-AS COMBO-BOX SORT INNER-LINES 8
     DROP-DOWN-LIST
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE cb-esp-docto3 AS CHARACTER FORMAT "X(5)":U 
     VIEW-AS COMBO-BOX SORT INNER-LINES 8
     DROP-DOWN-LIST
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE cb-esp-docto4 AS CHARACTER FORMAT "X(5)":U 
     VIEW-AS COMBO-BOX SORT INNER-LINES 8
     DROP-DOWN-LIST
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE cb-esp-docto5 AS CHARACTER FORMAT "X(5)":U 
     VIEW-AS COMBO-BOX SORT INNER-LINES 8
     DROP-DOWN-LIST
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE cb-esp-docto6 AS CHARACTER FORMAT "X(5)":U 
     VIEW-AS COMBO-BOX SORT INNER-LINES 8
     DROP-DOWN-LIST
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE cb-esp-docto7 AS CHARACTER FORMAT "X(5)":U 
     VIEW-AS COMBO-BOX SORT INNER-LINES 8
     DROP-DOWN-LIST
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE cb-esp-docto8 AS CHARACTER FORMAT "X(5)":U 
     VIEW-AS COMBO-BOX SORT INNER-LINES 8
     DROP-DOWN-LIST
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE cb-esp-docto9 AS CHARACTER FORMAT "X(5)":U 
     VIEW-AS COMBO-BOX SORT INNER-LINES 8
     DROP-DOWN-LIST
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fi-arq-excel AS CHARACTER FORMAT "X(256)":U 
     LABEL "Arquivo" 
     VIEW-AS FILL-IN 
     SIZE 36 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-depos1 AS CHARACTER FORMAT "x(3)" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 TOOLTIP "1§ Dep¢sito" NO-UNDO.

DEFINE VARIABLE fi-cod-depos10 AS CHARACTER FORMAT "x(3)" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 TOOLTIP "2§ Dep¢sito" NO-UNDO.

DEFINE VARIABLE fi-cod-depos2 AS CHARACTER FORMAT "x(3)" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 TOOLTIP "2§ Dep¢sito" NO-UNDO.

DEFINE VARIABLE fi-cod-depos3 AS CHARACTER FORMAT "x(3)" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 TOOLTIP "2§ Dep¢sito" NO-UNDO.

DEFINE VARIABLE fi-cod-depos4 AS CHARACTER FORMAT "x(3)" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 TOOLTIP "2§ Dep¢sito" NO-UNDO.

DEFINE VARIABLE fi-cod-depos5 AS CHARACTER FORMAT "x(3)" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 TOOLTIP "2§ Dep¢sito" NO-UNDO.

DEFINE VARIABLE fi-cod-depos6 AS CHARACTER FORMAT "x(3)" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 TOOLTIP "2§ Dep¢sito" NO-UNDO.

DEFINE VARIABLE fi-cod-depos7 AS CHARACTER FORMAT "x(3)" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 TOOLTIP "2§ Dep¢sito" NO-UNDO.

DEFINE VARIABLE fi-cod-depos8 AS CHARACTER FORMAT "x(3)" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 TOOLTIP "2§ Dep¢sito" NO-UNDO.

DEFINE VARIABLE fi-cod-depos9 AS CHARACTER FORMAT "x(3)" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 TOOLTIP "2§ Dep¢sito" NO-UNDO.

DEFINE VARIABLE fi-desc-depos AS CHARACTER FORMAT "X(256)":U 
     LABEL "Descri‡Æo" 
     VIEW-AS FILL-IN 
     SIZE 46.43 BY .88 NO-UNDO.

DEFINE VARIABLE rs-tipo-rel AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Resumido", 1,
"Detalhado", 2
     SIZE 12 BY 2.04 TOOLTIP "Tipo do relat¢rio" NO-UNDO.

DEFINE VARIABLE rs-tipo-valor AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Entrada", 1,
"M‚dio", 2
     SIZE 19 BY .75 TOOLTIP "Tipo de Valor: Entrada ou M‚dio" NO-UNDO.

DEFINE RECTANGLE RECT-14
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 73.57 BY 3.

DEFINE RECTANGLE RECT-29
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 73.57 BY 1.42.

DEFINE RECTANGLE RECT-34
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 36 BY 1.29.

DEFINE RECTANGLE RECT-39
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 36.57 BY 2.79.

DEFINE RECTANGLE RECT-40
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 73.57 BY 1.63.

DEFINE RECTANGLE RECT-41
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 36 BY 1.29.

DEFINE VARIABLE tg-all-depos AS LOGICAL INITIAL yes 
     LABEL "Todos os Dep¢sitos" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .83 NO-UNDO.

DEFINE VARIABLE tg-gerar-excel AS LOGICAL INITIAL no 
     LABEL "Gerar Planilha Excel" 
     VIEW-AS TOGGLE-BOX
     SIZE 18.14 BY .88 TOOLTIP "Imprimir Parƒmetros/Sele‡Æo ao final do relat¢rio."
     FONT 1 NO-UNDO.

DEFINE VARIABLE tg-impr-param AS LOGICAL INITIAL no 
     LABEL "Imprimir Parƒmetros/Sele‡Æo" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .88 TOOLTIP "Imprimir Parƒmetros/Sele‡Æo ao final do relat¢rio."
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-cod-estabel AS CHARACTER FORMAT "X(3)":U 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fin-cod-refer AS CHARACTER FORMAT "x(8)" INITIAL "ZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 TOOLTIP "Referˆncia final" NO-UNDO.

DEFINE VARIABLE fi-fin-dt-trans AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "Data de transa‡Æo final" NO-UNDO.

DEFINE VARIABLE fi-fin-ge-codigo AS INTEGER FORMAT ">9" INITIAL 99 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 TOOLTIP "Grupo de estoque final" NO-UNDO.

DEFINE VARIABLE fi-fin-it-codigo AS CHARACTER FORMAT "x(16)" INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fin-serie-docto AS CHARACTER FORMAT "x(5)" INITIAL "ZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 TOOLTIP "S‚rie final" NO-UNDO.

DEFINE VARIABLE fi-ini-cod-refer AS CHARACTER FORMAT "x(8)" 
     LABEL "Referˆncia":R12 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 TOOLTIP "Referˆncia inicial" NO-UNDO.

DEFINE VARIABLE fi-ini-dt-trans AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 
     LABEL "Data Transa‡Æo":R17 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "Data de transa‡Æo inicial" NO-UNDO.

DEFINE VARIABLE fi-ini-ge-codigo AS INTEGER FORMAT ">9" INITIAL 0 
     LABEL "Grupo Estoque":R16 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 TOOLTIP "Grupo de estoque inicial" NO-UNDO.

DEFINE VARIABLE fi-ini-it-codigo AS CHARACTER FORMAT "x(16)" 
     LABEL "Item":R9 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-serie-docto AS CHARACTER FORMAT "x(5)" 
     LABEL "S‚rie Documento":R18 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 TOOLTIP "S‚rie inicial" NO-UNDO.

DEFINE VARIABLE fi-nome-estabel AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 47.14 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE IMAGE IMAGE-63
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-64
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-65
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-66
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-67
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-68
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-69
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-70
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-71
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-72
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

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

DEFINE FRAME f-relat
     bt-executar AT ROW 14.54 COL 3 HELP
          "Dispara a execu‡Æo do relat¢rio"
     bt-cancelar AT ROW 14.54 COL 14 HELP
          "Fechar"
     bt-ajuda AT ROW 14.54 COL 70 HELP
          "Ajuda"
     rt-folder-left AT ROW 2.54 COL 2.14
     RECT-1 AT ROW 14.29 COL 2
     rt-folder-right AT ROW 2.67 COL 80.43
     rt-folder AT ROW 2.5 COL 2
     rt-folder-top AT ROW 2.54 COL 2.14
     RECT-6 AT ROW 13.75 COL 2.14
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
     RECT-7 AT ROW 1.92 COL 2.14
     RECT-9 AT ROW 5.29 COL 2.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 73.72 BY 10
         FONT 1.

DEFINE FRAME f-pg-cla
     rs-classif AT ROW 1.25 COL 2.72 HELP
          "Classifica‡Æo para emissÆo do relat¢rio" NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 76.86 BY 10.31.

DEFINE FRAME f-pg-sel
     fi-cod-estabel AT ROW 1.79 COL 18 COLON-ALIGNED
     fi-nome-estabel AT ROW 1.79 COL 23.14 COLON-ALIGNED NO-LABEL
     fi-ini-ge-codigo AT ROW 2.79 COL 18 COLON-ALIGNED HELP
          "Grupo de estoque a que pertence o item"
     fi-fin-ge-codigo AT ROW 2.79 COL 43.72 COLON-ALIGNED HELP
          "Grupo de estoque a que pertence o item" NO-LABEL
     fi-ini-it-codigo AT ROW 3.79 COL 18 COLON-ALIGNED
     fi-fin-it-codigo AT ROW 3.79 COL 43.72 COLON-ALIGNED HELP
          "C¢digo do item" NO-LABEL
     fi-ini-cod-refer AT ROW 4.79 COL 18 COLON-ALIGNED
     fi-fin-cod-refer AT ROW 4.79 COL 43.72 COLON-ALIGNED NO-LABEL
     fi-ini-dt-trans AT ROW 5.79 COL 18 COLON-ALIGNED
     fi-fin-dt-trans AT ROW 5.79 COL 43.72 COLON-ALIGNED NO-LABEL
     fi-ini-serie-docto AT ROW 6.79 COL 18 COLON-ALIGNED
     fi-fin-serie-docto AT ROW 6.79 COL 43.72 COLON-ALIGNED NO-LABEL
     IMAGE-63 AT ROW 4.79 COL 37.57
     IMAGE-64 AT ROW 4.79 COL 42.43
     IMAGE-65 AT ROW 3.79 COL 37.57
     IMAGE-66 AT ROW 3.79 COL 42.43
     IMAGE-67 AT ROW 2.79 COL 37.57
     IMAGE-68 AT ROW 2.79 COL 42.43
     IMAGE-69 AT ROW 5.79 COL 37.57
     IMAGE-70 AT ROW 5.79 COL 42.43
     IMAGE-71 AT ROW 6.79 COL 37.57
     IMAGE-72 AT ROW 6.79 COL 42.43
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.85
         SIZE 76.86 BY 10.62
         FONT 1.

DEFINE FRAME f-pg-par
     tg-all-depos AT ROW 1.58 COL 19
     fi-cod-depos1 AT ROW 2.38 COL 17 COLON-ALIGNED NO-LABEL
     fi-cod-depos2 AT ROW 2.38 COL 21.72 COLON-ALIGNED NO-LABEL
     fi-cod-depos3 AT ROW 2.38 COL 26.43 COLON-ALIGNED NO-LABEL
     fi-cod-depos4 AT ROW 2.38 COL 31.14 COLON-ALIGNED NO-LABEL
     fi-cod-depos5 AT ROW 2.38 COL 35.86 COLON-ALIGNED NO-LABEL
     fi-cod-depos6 AT ROW 2.38 COL 40.57 COLON-ALIGNED NO-LABEL
     fi-cod-depos7 AT ROW 2.38 COL 45.29 COLON-ALIGNED NO-LABEL
     fi-cod-depos8 AT ROW 2.38 COL 50 COLON-ALIGNED NO-LABEL
     fi-cod-depos9 AT ROW 2.38 COL 54.72 COLON-ALIGNED NO-LABEL
     fi-cod-depos10 AT ROW 2.38 COL 59.29 COLON-ALIGNED NO-LABEL
     fi-desc-depos AT ROW 3.38 COL 17 COLON-ALIGNED
     cb-esp-docto1 AT ROW 5.17 COL 1.86 COLON-ALIGNED NO-LABEL
     cb-esp-docto2 AT ROW 5.17 COL 9 COLON-ALIGNED NO-LABEL
     cb-esp-docto3 AT ROW 5.17 COL 16.14 COLON-ALIGNED NO-LABEL
     cb-esp-docto4 AT ROW 5.17 COL 23.29 COLON-ALIGNED NO-LABEL
     cb-esp-docto5 AT ROW 5.17 COL 30.43 COLON-ALIGNED NO-LABEL
     cb-esp-docto6 AT ROW 5.17 COL 37.57 COLON-ALIGNED NO-LABEL
     cb-esp-docto7 AT ROW 5.17 COL 44.72 COLON-ALIGNED NO-LABEL
     cb-esp-docto8 AT ROW 5.17 COL 51.86 COLON-ALIGNED NO-LABEL
     cb-esp-docto9 AT ROW 5.17 COL 59 COLON-ALIGNED NO-LABEL
     cb-esp-docto10 AT ROW 5.17 COL 66.14 COLON-ALIGNED NO-LABEL
     tg-gerar-excel AT ROW 7.13 COL 6.14 WIDGET-ID 8
     fi-arq-excel AT ROW 7.13 COL 31 COLON-ALIGNED WIDGET-ID 2
     rs-tipo-valor AT ROW 8.88 COL 55.43 NO-LABEL
     rs-tipo-rel AT ROW 9 COL 14.57 NO-LABEL
     tg-impr-param AT ROW 10.25 COL 46.72
     "Planilha para Excel" VIEW-AS TEXT
          SIZE 13.57 BY .54 AT ROW 6.54 COL 29.14 WIDGET-ID 6
     "Dep¢sitos" VIEW-AS TEXT
          SIZE 7.57 BY .63 AT ROW 1.17 COL 9.43
     "Esp‚cies de Documentos" VIEW-AS TEXT
          SIZE 18 BY .54 AT ROW 4.54 COL 29.14
     "Tipos de Relat¢rio" VIEW-AS TEXT
          SIZE 13.29 BY .75 AT ROW 8.25 COL 13.43
     "Tipo de Valor:" VIEW-AS TEXT
          SIZE 10 BY .75 AT ROW 8.88 COL 43.72
     RECT-14 AT ROW 1.5 COL 2.43
     RECT-34 AT ROW 10.04 COL 40
     RECT-39 AT ROW 8.54 COL 2.43
     RECT-40 AT ROW 4.83 COL 2.43
     RECT-41 AT ROW 8.58 COL 40
     RECT-29 AT ROW 6.83 COL 2.43 WIDGET-ID 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 75 BY 10.46
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
         TITLE              = "Movimento de Estoque por Dep¢sito/Esp‚cie"
         HEIGHT             = 15
         WIDTH              = 81.14
         MAX-HEIGHT         = 28.75
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 28.75
         VIRTUAL-WIDTH      = 146.29
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         FONT               = 1
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
/* SETTINGS FOR FILL-IN fi-arq-excel IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-cod-depos1 IN FRAME f-pg-par
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-cod-depos10 IN FRAME f-pg-par
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-cod-depos2 IN FRAME f-pg-par
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-cod-depos3 IN FRAME f-pg-par
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-cod-depos4 IN FRAME f-pg-par
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-cod-depos5 IN FRAME f-pg-par
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-cod-depos6 IN FRAME f-pg-par
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-cod-depos7 IN FRAME f-pg-par
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-cod-depos8 IN FRAME f-pg-par
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-cod-depos9 IN FRAME f-pg-par
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-desc-depos IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME f-pg-sel
                                                                        */
/* SETTINGS FOR FILL-IN fi-nome-estabel IN FRAME f-pg-sel
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
ON END-ERROR OF w-relat /* Movimento de Estoque por Dep¢sito/Esp‚cie */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON WINDOW-CLOSE OF w-relat /* Movimento de Estoque por Dep¢sito/Esp‚cie */
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
&Scoped-define SELF-NAME fi-arq-excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-arq-excel w-relat
ON LEAVE OF fi-arq-excel IN FRAME f-pg-par /* Arquivo */
DO:
  IF NOT fi-arq-excel:SCREEN-VALUE IN FRAME {&FRAME-NAME} MATCHES "*.csv*" THEN DO:
     MESSAGE "Nome do arquivo para Excel est  inv lido." SKIP
             "Deve ser do tipo Caminho/Arquivo.csv" VIEW-AS ALERT-BOX.
     ASSIGN fi-arq-excel:SCREEN-VALUE IN FRAME f-pg-par = SESSION:TEMP-DIRECTORY + 
                         "esce0001.csv".
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-depos1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-depos1 w-relat
ON ENTRY OF fi-cod-depos1 IN FRAME f-pg-par
DO:
   IF SELF:SCREEN-VALUE <> "" THEN DO.
      FIND deposito WHERE
           deposito.cod-depos = INPUT FRAME {&FRAME-NAME} fi-cod-depos1
           NO-LOCK NO-ERROR.
      IF AVAIL deposito THEN
         ASSIGN fi-desc-depos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = deposito.nome.
   END.
   ELSE ASSIGN fi-desc-depos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-depos1 w-relat
ON F5 OF fi-cod-depos1 IN FRAME f-pg-par
DO:
  APPLY 'mouse-select-dblclick' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-depos1 w-relat
ON LEAVE OF fi-cod-depos1 IN FRAME f-pg-par
DO:
  IF SELF:SCREEN-VALUE <> "" THEN DO.
     FIND deposito WHERE
          deposito.cod-depos = INPUT FRAME {&FRAME-NAME} fi-cod-depos1 NO-LOCK NO-ERROR.
     IF NOT AVAIL deposito THEN DO.
        MESSAGE "Dep¢sito NÆo Cadastrado..." VIEW-AS ALERT-BOX.
        APPLY 'entry' TO SELF.
        RETURN NO-APPLY.
     END.
     ASSIGN fi-desc-depos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = deposito.nome.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-depos1 w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-cod-depos1 IN FRAME f-pg-par
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z01in084.w
                     &campo     = fi-cod-depos1
                     &campozoom = cod-depos
                     &FRAME     = f-pg-par}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-depos1 w-relat
ON VALUE-CHANGED OF fi-cod-depos1 IN FRAME f-pg-par
DO:
  IF SELF:SCREEN-VALUE <> "" THEN DO.
     FIND deposito WHERE
          deposito.cod-depos = INPUT FRAME {&FRAME-NAME} fi-cod-depos1 NO-LOCK NO-ERROR.
     IF AVAIL deposito THEN 
        ASSIGN fi-desc-depos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = deposito.nome.
     ELSE
        ASSIGN fi-desc-depos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-depos10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-depos10 w-relat
ON ENTRY OF fi-cod-depos10 IN FRAME f-pg-par
DO:
   IF SELF:SCREEN-VALUE <> "" THEN DO.
      FIND deposito WHERE
           deposito.cod-depos = INPUT FRAME {&FRAME-NAME} fi-cod-depos10
           NO-LOCK NO-ERROR.
      IF AVAIL deposito THEN
         ASSIGN fi-desc-depos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = deposito.nome.
   END.
   ELSE ASSIGN fi-desc-depos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-depos10 w-relat
ON F5 OF fi-cod-depos10 IN FRAME f-pg-par
DO:
  APPLY 'mouse-select-dblclick' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-depos10 w-relat
ON LEAVE OF fi-cod-depos10 IN FRAME f-pg-par
DO:
  IF SELF:SCREEN-VALUE <> "" THEN DO.
     FIND deposito WHERE
          deposito.cod-depos = INPUT FRAME {&FRAME-NAME} fi-cod-depos10 NO-LOCK NO-ERROR.
     IF NOT AVAIL deposito THEN DO.
        MESSAGE "Dep¢sito NÆo Cadastrado..." VIEW-AS ALERT-BOX.
        APPLY 'entry' TO SELF.
        RETURN NO-APPLY.
     END.
     ASSIGN fi-desc-depos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = deposito.nome.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-depos10 w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-cod-depos10 IN FRAME f-pg-par
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z01in084.w
                     &campo     = fi-cod-depos10
                     &campozoom = cod-depos
                     &FRAME     = f-pg-par}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-depos10 w-relat
ON VALUE-CHANGED OF fi-cod-depos10 IN FRAME f-pg-par
DO:
  IF SELF:SCREEN-VALUE <> "" THEN DO.
     FIND deposito WHERE
          deposito.cod-depos = INPUT FRAME {&FRAME-NAME} fi-cod-depos10 NO-LOCK NO-ERROR.
     IF AVAIL deposito THEN 
        ASSIGN fi-desc-depos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = deposito.nome.
     ELSE
        ASSIGN fi-desc-depos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-depos2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-depos2 w-relat
ON ENTRY OF fi-cod-depos2 IN FRAME f-pg-par
DO:
   IF SELF:SCREEN-VALUE <> "" THEN DO.
      FIND deposito WHERE
           deposito.cod-depos = INPUT FRAME {&FRAME-NAME} fi-cod-depos2
           NO-LOCK NO-ERROR.
      IF AVAIL deposito THEN
         ASSIGN fi-desc-depos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = deposito.nome.
   END.
   ELSE ASSIGN fi-desc-depos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "". 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-depos2 w-relat
ON F5 OF fi-cod-depos2 IN FRAME f-pg-par
DO:
  APPLY 'mouse-select-dblclick' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-depos2 w-relat
ON LEAVE OF fi-cod-depos2 IN FRAME f-pg-par
DO:
  IF SELF:SCREEN-VALUE <> "" THEN DO.
     FIND deposito WHERE
          deposito.cod-depos = INPUT FRAME {&FRAME-NAME} fi-cod-depos2 NO-LOCK NO-ERROR.
     IF NOT AVAIL deposito THEN DO.
        MESSAGE "Dep¢sito NÆo Cadastrado..." VIEW-AS ALERT-BOX.
        APPLY 'entry' TO SELF.
        RETURN NO-APPLY.
     END.
     ASSIGN fi-desc-depos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = deposito.nome.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-depos2 w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-cod-depos2 IN FRAME f-pg-par
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z01in084.w
                     &campo     = fi-cod-depos2
                     &campozoom = cod-depos
                     &FRAME     = f-pg-par}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-depos2 w-relat
ON VALUE-CHANGED OF fi-cod-depos2 IN FRAME f-pg-par
DO:
  IF SELF:SCREEN-VALUE <> "" THEN DO.
     FIND deposito WHERE
          deposito.cod-depos = INPUT FRAME {&FRAME-NAME} fi-cod-depos2 NO-LOCK NO-ERROR.
     IF AVAIL deposito THEN 
        ASSIGN fi-desc-depos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = deposito.nome.
     ELSE
        ASSIGN fi-desc-depos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-depos3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-depos3 w-relat
ON ENTRY OF fi-cod-depos3 IN FRAME f-pg-par
DO:
   IF SELF:SCREEN-VALUE <> "" THEN DO.
      FIND deposito WHERE
           deposito.cod-depos = INPUT FRAME {&FRAME-NAME} fi-cod-depos3
           NO-LOCK NO-ERROR.
      IF AVAIL deposito THEN
         ASSIGN fi-desc-depos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = deposito.nome.
   END.
   ELSE ASSIGN fi-desc-depos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-depos3 w-relat
ON F5 OF fi-cod-depos3 IN FRAME f-pg-par
DO:
  APPLY 'mouse-select-dblclick' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-depos3 w-relat
ON LEAVE OF fi-cod-depos3 IN FRAME f-pg-par
DO:
  IF SELF:SCREEN-VALUE <> "" THEN DO.
     FIND deposito WHERE
          deposito.cod-depos = INPUT FRAME {&FRAME-NAME} fi-cod-depos3 NO-LOCK NO-ERROR.
     IF NOT AVAIL deposito THEN DO.
        MESSAGE "Dep¢sito NÆo Cadastrado..." VIEW-AS ALERT-BOX.
        APPLY 'entry' TO SELF.
        RETURN NO-APPLY.
     END.
     ASSIGN fi-desc-depos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = deposito.nome.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-depos3 w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-cod-depos3 IN FRAME f-pg-par
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z01in084.w
                     &campo     = fi-cod-depos3
                     &campozoom = cod-depos
                     &FRAME     = f-pg-par}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-depos3 w-relat
ON VALUE-CHANGED OF fi-cod-depos3 IN FRAME f-pg-par
DO:
  IF SELF:SCREEN-VALUE <> "" THEN DO.
     FIND deposito WHERE
          deposito.cod-depos = INPUT FRAME {&FRAME-NAME} fi-cod-depos3 NO-LOCK NO-ERROR.
     IF AVAIL deposito THEN 
        ASSIGN fi-desc-depos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = deposito.nome.
     ELSE
        ASSIGN fi-desc-depos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-depos4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-depos4 w-relat
ON ENTRY OF fi-cod-depos4 IN FRAME f-pg-par
DO:
   IF SELF:SCREEN-VALUE <> "" THEN DO.
      FIND deposito WHERE
           deposito.cod-depos = INPUT FRAME {&FRAME-NAME} fi-cod-depos4
           NO-LOCK NO-ERROR.
      IF AVAIL deposito THEN
         ASSIGN fi-desc-depos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = deposito.nome.
   END.
   ELSE ASSIGN fi-desc-depos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-depos4 w-relat
ON F5 OF fi-cod-depos4 IN FRAME f-pg-par
DO:
  APPLY 'mouse-select-dblclick' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-depos4 w-relat
ON LEAVE OF fi-cod-depos4 IN FRAME f-pg-par
DO:
  IF SELF:SCREEN-VALUE <> "" THEN DO.
     FIND deposito WHERE
          deposito.cod-depos = INPUT FRAME {&FRAME-NAME} fi-cod-depos4 NO-LOCK NO-ERROR.
     IF NOT AVAIL deposito THEN DO.
        MESSAGE "Dep¢sito NÆo Cadastrado..." VIEW-AS ALERT-BOX.
        APPLY 'entry' TO SELF.
        RETURN NO-APPLY.
     END.
     ASSIGN fi-desc-depos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = deposito.nome.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-depos4 w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-cod-depos4 IN FRAME f-pg-par
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z01in084.w
                     &campo     = fi-cod-depos4
                     &campozoom = cod-depos
                     &FRAME     = f-pg-par}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-depos4 w-relat
ON VALUE-CHANGED OF fi-cod-depos4 IN FRAME f-pg-par
DO:
  IF SELF:SCREEN-VALUE <> "" THEN DO.
     FIND deposito WHERE
          deposito.cod-depos = INPUT FRAME {&FRAME-NAME} fi-cod-depos4 NO-LOCK NO-ERROR.
     IF AVAIL deposito THEN 
        ASSIGN fi-desc-depos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = deposito.nome.
     ELSE
        ASSIGN fi-desc-depos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-depos5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-depos5 w-relat
ON ENTRY OF fi-cod-depos5 IN FRAME f-pg-par
DO:
   IF SELF:SCREEN-VALUE <> "" THEN DO.
      FIND deposito WHERE
           deposito.cod-depos = INPUT FRAME {&FRAME-NAME} fi-cod-depos5
           NO-LOCK NO-ERROR.
      IF AVAIL deposito THEN
         ASSIGN fi-desc-depos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = deposito.nome.
   END.
   ELSE ASSIGN fi-desc-depos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-depos5 w-relat
ON F5 OF fi-cod-depos5 IN FRAME f-pg-par
DO:
  APPLY 'mouse-select-dblclick' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-depos5 w-relat
ON LEAVE OF fi-cod-depos5 IN FRAME f-pg-par
DO:
  IF SELF:SCREEN-VALUE <> "" THEN DO.
     FIND deposito WHERE
          deposito.cod-depos = INPUT FRAME {&FRAME-NAME} fi-cod-depos5 NO-LOCK NO-ERROR.
     IF NOT AVAIL deposito THEN DO.
        MESSAGE "Dep¢sito NÆo Cadastrado..." VIEW-AS ALERT-BOX.
        APPLY 'entry' TO SELF.
        RETURN NO-APPLY.
     END.
     ASSIGN fi-desc-depos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = deposito.nome.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-depos5 w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-cod-depos5 IN FRAME f-pg-par
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z01in084.w
                     &campo     = fi-cod-depos5
                     &campozoom = cod-depos
                     &FRAME     = f-pg-par}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-depos5 w-relat
ON VALUE-CHANGED OF fi-cod-depos5 IN FRAME f-pg-par
DO:
  IF SELF:SCREEN-VALUE <> "" THEN DO.
     FIND deposito WHERE
          deposito.cod-depos = INPUT FRAME {&FRAME-NAME} fi-cod-depos5 NO-LOCK NO-ERROR.
     IF AVAIL deposito THEN 
        ASSIGN fi-desc-depos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = deposito.nome.
     ELSE
        ASSIGN fi-desc-depos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-depos6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-depos6 w-relat
ON ENTRY OF fi-cod-depos6 IN FRAME f-pg-par
DO:
   IF SELF:SCREEN-VALUE <> "" THEN DO.
      FIND deposito WHERE
           deposito.cod-depos = INPUT FRAME {&FRAME-NAME} fi-cod-depos6
           NO-LOCK NO-ERROR.
      IF AVAIL deposito THEN
         ASSIGN fi-desc-depos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = deposito.nome.
   END.
   ELSE ASSIGN fi-desc-depos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-depos6 w-relat
ON F5 OF fi-cod-depos6 IN FRAME f-pg-par
DO:
  APPLY 'mouse-select-dblclick' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-depos6 w-relat
ON LEAVE OF fi-cod-depos6 IN FRAME f-pg-par
DO:
  IF SELF:SCREEN-VALUE <> "" THEN DO.
     FIND deposito WHERE
          deposito.cod-depos = INPUT FRAME {&FRAME-NAME} fi-cod-depos6 NO-LOCK NO-ERROR.
     IF NOT AVAIL deposito THEN DO.
        MESSAGE "Dep¢sito NÆo Cadastrado..." VIEW-AS ALERT-BOX.
        APPLY 'entry' TO SELF.
        RETURN NO-APPLY.
     END.
     ASSIGN fi-desc-depos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = deposito.nome.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-depos6 w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-cod-depos6 IN FRAME f-pg-par
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z01in084.w
                     &campo     = fi-cod-depos6
                     &campozoom = cod-depos
                     &FRAME     = f-pg-par}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-depos6 w-relat
ON VALUE-CHANGED OF fi-cod-depos6 IN FRAME f-pg-par
DO:
  IF SELF:SCREEN-VALUE <> "" THEN DO.
     FIND deposito WHERE
          deposito.cod-depos = INPUT FRAME {&FRAME-NAME} fi-cod-depos6 NO-LOCK NO-ERROR.
     IF AVAIL deposito THEN 
        ASSIGN fi-desc-depos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = deposito.nome.
     ELSE
        ASSIGN fi-desc-depos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-depos7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-depos7 w-relat
ON ENTRY OF fi-cod-depos7 IN FRAME f-pg-par
DO:
   IF SELF:SCREEN-VALUE <> "" THEN DO.
      FIND deposito WHERE
           deposito.cod-depos = INPUT FRAME {&FRAME-NAME} fi-cod-depos7
           NO-LOCK NO-ERROR.
      IF AVAIL deposito THEN
         ASSIGN fi-desc-depos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = deposito.nome.
   END.
   ELSE ASSIGN fi-desc-depos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-depos7 w-relat
ON F5 OF fi-cod-depos7 IN FRAME f-pg-par
DO:
  APPLY 'mouse-select-dblclick' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-depos7 w-relat
ON LEAVE OF fi-cod-depos7 IN FRAME f-pg-par
DO:
  IF SELF:SCREEN-VALUE <> "" THEN DO.
     FIND deposito WHERE
          deposito.cod-depos = INPUT FRAME {&FRAME-NAME} fi-cod-depos7 NO-LOCK NO-ERROR.
     IF NOT AVAIL deposito THEN DO.
        MESSAGE "Dep¢sito NÆo Cadastrado..." VIEW-AS ALERT-BOX.
        APPLY 'entry' TO SELF.
        RETURN NO-APPLY.
     END.
     ASSIGN fi-desc-depos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = deposito.nome.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-depos7 w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-cod-depos7 IN FRAME f-pg-par
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z01in084.w
                     &campo     = fi-cod-depos7
                     &campozoom = cod-depos
                     &FRAME     = f-pg-par}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-depos7 w-relat
ON VALUE-CHANGED OF fi-cod-depos7 IN FRAME f-pg-par
DO:
  IF SELF:SCREEN-VALUE <> "" THEN DO.
     FIND deposito WHERE
          deposito.cod-depos = INPUT FRAME {&FRAME-NAME} fi-cod-depos7 NO-LOCK NO-ERROR.
     IF AVAIL deposito THEN 
        ASSIGN fi-desc-depos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = deposito.nome.
     ELSE
        ASSIGN fi-desc-depos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-depos8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-depos8 w-relat
ON ENTRY OF fi-cod-depos8 IN FRAME f-pg-par
DO:
   IF SELF:SCREEN-VALUE <> "" THEN DO.
      FIND deposito WHERE
           deposito.cod-depos = INPUT FRAME {&FRAME-NAME} fi-cod-depos8
           NO-LOCK NO-ERROR.
      IF AVAIL deposito THEN
         ASSIGN fi-desc-depos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = deposito.nome.
   END.
   ELSE ASSIGN fi-desc-depos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-depos8 w-relat
ON F5 OF fi-cod-depos8 IN FRAME f-pg-par
DO:
  APPLY 'mouse-select-dblclick' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-depos8 w-relat
ON LEAVE OF fi-cod-depos8 IN FRAME f-pg-par
DO:
  IF SELF:SCREEN-VALUE <> "" THEN DO.
     FIND deposito WHERE
          deposito.cod-depos = INPUT FRAME {&FRAME-NAME} fi-cod-depos8 NO-LOCK NO-ERROR.
     IF NOT AVAIL deposito THEN DO.
        MESSAGE "Dep¢sito NÆo Cadastrado..." VIEW-AS ALERT-BOX.
        APPLY 'entry' TO SELF.
        RETURN NO-APPLY.
     END.
     ASSIGN fi-desc-depos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = deposito.nome.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-depos8 w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-cod-depos8 IN FRAME f-pg-par
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z01in084.w
                     &campo     = fi-cod-depos8
                     &campozoom = cod-depos
                     &FRAME     = f-pg-par}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-depos8 w-relat
ON VALUE-CHANGED OF fi-cod-depos8 IN FRAME f-pg-par
DO:
  IF SELF:SCREEN-VALUE <> "" THEN DO.
     FIND deposito WHERE
          deposito.cod-depos = INPUT FRAME {&FRAME-NAME} fi-cod-depos8 NO-LOCK NO-ERROR.
     IF AVAIL deposito THEN 
        ASSIGN fi-desc-depos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = deposito.nome.
     ELSE
        ASSIGN fi-desc-depos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-depos9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-depos9 w-relat
ON ENTRY OF fi-cod-depos9 IN FRAME f-pg-par
DO:
   IF SELF:SCREEN-VALUE <> "" THEN DO.
      FIND deposito WHERE
           deposito.cod-depos = INPUT FRAME {&FRAME-NAME} fi-cod-depos9
           NO-LOCK NO-ERROR.
      IF AVAIL deposito THEN
         ASSIGN fi-desc-depos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = deposito.nome.
   END.
   ELSE ASSIGN fi-desc-depos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-depos9 w-relat
ON F5 OF fi-cod-depos9 IN FRAME f-pg-par
DO:
  APPLY 'mouse-select-dblclick' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-depos9 w-relat
ON LEAVE OF fi-cod-depos9 IN FRAME f-pg-par
DO:
  IF SELF:SCREEN-VALUE <> "" THEN DO.
     FIND deposito WHERE
          deposito.cod-depos = INPUT FRAME {&FRAME-NAME} fi-cod-depos9 NO-LOCK NO-ERROR.
     IF NOT AVAIL deposito THEN DO.
        MESSAGE "Dep¢sito NÆo Cadastrado..." VIEW-AS ALERT-BOX.
        APPLY 'entry' TO SELF.
        RETURN NO-APPLY.
     END.
     ASSIGN fi-desc-depos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = deposito.nome.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-depos9 w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-cod-depos9 IN FRAME f-pg-par
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z01in084.w
                     &campo     = fi-cod-depos9
                     &campozoom = cod-depos
                     &FRAME     = f-pg-par}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-depos9 w-relat
ON VALUE-CHANGED OF fi-cod-depos9 IN FRAME f-pg-par
DO:
  IF SELF:SCREEN-VALUE <> "" THEN DO.
     FIND deposito WHERE
          deposito.cod-depos = INPUT FRAME {&FRAME-NAME} fi-cod-depos9 NO-LOCK NO-ERROR.
     IF AVAIL deposito THEN 
        ASSIGN fi-desc-depos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = deposito.nome.
     ELSE
        ASSIGN fi-desc-depos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-sel
&Scoped-define SELF-NAME fi-cod-estabel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-estabel w-relat
ON ENTRY OF fi-cod-estabel IN FRAME f-pg-sel /* Estabelecimento */
DO:
  IF SELF:SCREEN-VALUE <> "" THEN DO.
     FIND estabelec WHERE
          estabelec.cod-estabel = INPUT FRAME {&FRAME-NAME} fi-cod-estabel
          NO-LOCK NO-ERROR.
     IF AVAIL estabelec THEN
        ASSIGN fi-nome-estabel:SCREEN-VALUE IN FRAME {&FRAME-NAME} = estabelec.nome.
  END.
  ELSE ASSIGN fi-nome-estabel:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-estabel w-relat
ON LEAVE OF fi-cod-estabel IN FRAME f-pg-sel /* Estabelecimento */
DO:
  FIND estabelec WHERE
       estabelec.cod-estabel = INPUT FRAME {&FRAME-NAME} fi-cod-estabel NO-LOCK NO-ERROR.

  IF NOT AVAIL estabelec THEN DO.
     MESSAGE "Estabelecimento nÆo Cadastrado..." VIEW-AS ALERT-BOX.
     RETURN NO-APPLY.
  END.
  ASSIGN fi-nome-estabel:SCREEN-VALUE IN FRAME {&FRAME-NAME} = estabelec.nome.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-estabel w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-cod-estabel IN FRAME f-pg-sel /* Estabelecimento */
DO:
  {include/zoomvar.i &prog-zoom=adzoom/z01ad107.w
                     &campo=fi-cod-estabel
                     &campozoom=cod-estabel
                     &FRAME=f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-fin-ge-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fin-ge-codigo w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-fin-ge-codigo IN FRAME f-pg-sel
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z01in142.w
                     &campo     = fi-fin-ge-codigo
                     &campozoom = ge-codigo
                     &FRAME     = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-fin-it-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fin-it-codigo w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-fin-it-codigo IN FRAME f-pg-sel
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z01in172.w
                     &campo     = fi-fin-it-codigo
                     &campozoom = it-codigo
                     &FRAME     = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ini-ge-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-ge-codigo w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ini-ge-codigo IN FRAME f-pg-sel /* Grupo Estoque */
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z01in142.w
                     &campo     = fi-ini-ge-codigo
                     &campozoom = ge-codigo
                     &FRAME     = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ini-it-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-it-codigo w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ini-it-codigo IN FRAME f-pg-sel /* Item */
DO:
    {include/zoomvar.i &prog-zoom = inzoom/z01in172.w
                     &campo       = fi-ini-it-codigo
                     &campozoom   = it-codigo
                     &FRAME       = f-pg-sel}
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
&Scoped-define SELF-NAME tg-all-depos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-all-depos w-relat
ON VALUE-CHANGED OF tg-all-depos IN FRAME f-pg-par /* Todos os Dep¢sitos */
DO:
  IF INPUT FRAME {&FRAME-NAME} tg-all-depos THEN DO:
     ASSIGN fi-cod-depos1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
            fi-cod-depos2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
            fi-cod-depos3:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
            fi-cod-depos4:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
            fi-cod-depos5:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
            fi-cod-depos6:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
            fi-cod-depos7:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
            fi-cod-depos8:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
            fi-cod-depos9:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
            fi-cod-depos10:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
            fi-desc-depos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
     DISABLE {&list-4} WITH FRAME {&FRAME-NAME}.
  END.
  ELSE DO:
     ENABLE {&list-4} WITH FRAME {&FRAME-NAME}.
     APPLY 'entry' TO fi-cod-depos1 IN FRAME {&FRAME-NAME}.
  END.
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
            fi-arq-excel:SCREEN-VALUE IN FRAME f-pg-par = SESSION:TEMP-DIRECTORY + 
                         "esce0003.csv".
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

{utp/ut9000.i "ESCE0003" "2.04.00.000"}

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
  
    ASSIGN dt-aux = date(month(today),1,year(today)) - 1.

    ASSIGN fi-cod-estabel:SCREEN-VALUE IN FRAME f-pg-sel  = "2"
           fi-ini-dt-trans:SCREEN-VALUE IN FRAME f-pg-sel = string(date(month(dt-aux),1,year(dt-aux)))
           fi-fin-dt-trans:SCREEN-VALUE IN FRAME f-pg-sel = string(dt-aux).
    
    {include/i-rpmbl.i}
        
    fi-cod-estabel:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-ini-ge-codigo:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-fin-ge-codigo:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-ini-it-codigo:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-fin-it-codigo:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-cod-depos1:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-par.
    fi-cod-depos2:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-par.
    fi-cod-depos3:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-par.
    fi-cod-depos4:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-par.
    fi-cod-depos5:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-par.
    fi-cod-depos6:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-par.
    fi-cod-depos7:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-par.
    fi-cod-depos8:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-par.
    fi-cod-depos9:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-par.
    fi-cod-depos10:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-par.

    {esinc/i-dsallrb.i movto-estoq.esp-docto c-especies}
    ASSIGN cb-esp-docto1:LIST-ITEMS IN FRAME f-pg-par = c-especies
           cb-esp-docto2:LIST-ITEMS IN FRAME f-pg-par = c-especies
           cb-esp-docto3:LIST-ITEMS IN FRAME f-pg-par = c-especies 
           cb-esp-docto4:LIST-ITEMS IN FRAME f-pg-par = c-especies 
           cb-esp-docto5:LIST-ITEMS IN FRAME f-pg-par = c-especies
           cb-esp-docto6:LIST-ITEMS IN FRAME f-pg-par = c-especies
           cb-esp-docto7:LIST-ITEMS IN FRAME f-pg-par = c-especies
           cb-esp-docto8:LIST-ITEMS IN FRAME f-pg-par = c-especies
           cb-esp-docto9:LIST-ITEMS IN FRAME f-pg-par = c-especies
           cb-esp-docto10:LIST-ITEMS IN FRAME f-pg-par = c-especies
           cb-esp-docto1:SCREEN-VALUE IN FRAME f-pg-par = "ACA".

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
  ENABLE im-pg-cla im-pg-imp im-pg-par im-pg-sel bt-executar bt-cancelar 
         bt-ajuda 
      WITH FRAME f-relat IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}
  DISPLAY fi-cod-estabel fi-nome-estabel fi-ini-ge-codigo fi-fin-ge-codigo 
          fi-ini-it-codigo fi-fin-it-codigo fi-ini-cod-refer fi-fin-cod-refer 
          fi-ini-dt-trans fi-fin-dt-trans fi-ini-serie-docto fi-fin-serie-docto 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  ENABLE IMAGE-63 IMAGE-64 IMAGE-65 IMAGE-66 IMAGE-67 IMAGE-68 IMAGE-69 
         IMAGE-70 IMAGE-71 IMAGE-72 fi-cod-estabel fi-ini-ge-codigo 
         fi-fin-ge-codigo fi-ini-it-codigo fi-fin-it-codigo fi-ini-cod-refer 
         fi-fin-cod-refer fi-ini-dt-trans fi-fin-dt-trans fi-ini-serie-docto 
         fi-fin-serie-docto 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}
  DISPLAY rs-classif 
      WITH FRAME f-pg-cla IN WINDOW w-relat.
  ENABLE rs-classif 
      WITH FRAME f-pg-cla IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-cla}
  DISPLAY rs-destino c-arquivo rs-execucao 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  ENABLE RECT-7 RECT-9 rs-destino bt-config-impr bt-arquivo c-arquivo 
         rs-execucao 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}
  DISPLAY tg-all-depos fi-cod-depos1 fi-cod-depos2 fi-cod-depos3 fi-cod-depos4 
          fi-cod-depos5 fi-cod-depos6 fi-cod-depos7 fi-cod-depos8 fi-cod-depos9 
          fi-cod-depos10 fi-desc-depos cb-esp-docto1 cb-esp-docto2 cb-esp-docto3 
          cb-esp-docto4 cb-esp-docto5 cb-esp-docto6 cb-esp-docto7 cb-esp-docto8 
          cb-esp-docto9 cb-esp-docto10 tg-gerar-excel fi-arq-excel rs-tipo-valor 
          rs-tipo-rel tg-impr-param 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  ENABLE RECT-14 RECT-34 RECT-39 RECT-40 RECT-41 RECT-29 tg-all-depos 
         cb-esp-docto1 cb-esp-docto2 cb-esp-docto3 cb-esp-docto4 cb-esp-docto5 
         cb-esp-docto6 cb-esp-docto7 cb-esp-docto8 cb-esp-docto9 cb-esp-docto10 
         tg-gerar-excel rs-tipo-valor rs-tipo-rel tg-impr-param 
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
    assign tt-param.usuario         = c-seg-usuario
           tt-param.destino         = input frame f-pg-imp rs-destino
           tt-param.data-exec       = today
           tt-param.hora-exec       = time
           tt-param.classifica      = INPUT FRAME f-pg-cla rs-classif
           tt-param.desc-classifica = entry((tt-param.classifica - 1) * 2 + 1, 
                                      rs-classif:radio-buttons in frame f-pg-cla).
    
    if tt-param.destino = 1 
    then assign tt-param.arquivo = "".
    else if  tt-param.destino = 2 
         then assign tt-param.arquivo = input frame f-pg-imp c-arquivo.
         else assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp":U.
    
    ASSIGN tt-param.cod-estabel     = INPUT FRAME f-pg-sel fi-cod-estabel
           tt-param.ge-codigo-ini   = INPUT FRAME f-pg-sel fi-ini-ge-codigo
           tt-param.ge-codigo-fin   = INPUT FRAME f-pg-sel fi-fin-ge-codigo
           tt-param.it-codigo-ini   = INPUT FRAME f-pg-sel fi-ini-it-codigo
           tt-param.it-codigo-fin   = INPUT FRAME f-pg-sel fi-fin-it-codigo
           tt-param.cod-refer-ini   = INPUT FRAME f-pg-sel fi-ini-cod-refer
           tt-param.cod-refer-fin   = INPUT FRAME f-pg-sel fi-fin-cod-refer
           tt-param.dt-trans-ini    = INPUT FRAME f-pg-sel fi-ini-dt-trans
           tt-param.dt-trans-fin    = INPUT FRAME f-pg-sel fi-fin-dt-trans
           tt-param.serie-docto-ini = INPUT FRAME f-pg-sel fi-ini-serie-docto
           tt-param.serie-docto-fin = INPUT FRAME f-pg-sel fi-fin-serie-docto 
           tt-param.all-depos       = INPUT FRAME f-pg-par tg-all-depos
           tt-param.cod-depos1      = INPUT FRAME f-pg-par fi-cod-depos1
           tt-param.cod-depos2      = INPUT FRAME f-pg-par fi-cod-depos2
           tt-param.cod-depos3      = INPUT FRAME f-pg-par fi-cod-depos3
           tt-param.cod-depos4      = INPUT FRAME f-pg-par fi-cod-depos4
           tt-param.cod-depos5      = INPUT FRAME f-pg-par fi-cod-depos5
           tt-param.cod-depos6      = INPUT FRAME f-pg-par fi-cod-depos6
           tt-param.cod-depos7      = INPUT FRAME f-pg-par fi-cod-depos7
           tt-param.cod-depos8      = INPUT FRAME f-pg-par fi-cod-depos8
           tt-param.cod-depos9      = INPUT FRAME f-pg-par fi-cod-depos9 
           tt-param.cod-depos10     = INPUT FRAME f-pg-par fi-cod-depos10
           tt-param.esp-docto1      = LOOKUP(INPUT FRAME f-pg-par cb-esp-docto1,c-especies)
           tt-param.esp-docto2      = LOOKUP(INPUT FRAME f-pg-par cb-esp-docto2,c-especies)
           tt-param.esp-docto3      = LOOKUP(INPUT FRAME f-pg-par cb-esp-docto3,c-especies)
           tt-param.esp-docto4      = LOOKUP(INPUT FRAME f-pg-par cb-esp-docto4,c-especies)
           tt-param.esp-docto5      = LOOKUP(INPUT FRAME f-pg-par cb-esp-docto5,c-especies)
           tt-param.esp-docto6      = LOOKUP(INPUT FRAME f-pg-par cb-esp-docto6,c-especies)
           tt-param.esp-docto7      = LOOKUP(INPUT FRAME f-pg-par cb-esp-docto7,c-especies)
           tt-param.esp-docto8      = LOOKUP(INPUT FRAME f-pg-par cb-esp-docto8,c-especies)
           tt-param.esp-docto9      = LOOKUP(INPUT FRAME f-pg-par cb-esp-docto9,c-especies)
           tt-param.esp-docto10     = LOOKUP(INPUT FRAME f-pg-par cb-esp-docto10,c-especies)
           tt-param.gerar-excel     = INPUT FRAME f-pg-par tg-gerar-excel
           tt-param.arq-excel       = INPUT FRAME f-pg-par fi-arq-excel
           tt-param.tipo-rel        = INPUT FRAME f-pg-par rs-tipo-rel                          
           tt-param.desc-tipo-rel   = entry((tt-param.tipo-rel - 1) * 2 + 1, 
                                             rs-tipo-rel:radio-buttons in frame f-pg-par)
           tt-param.tipo-valor      = INPUT FRAME f-pg-par rs-tipo-valor
           tt-param.desc-tipo-valor = entry((tt-param.tipo-valor - 1) * 2 + 1, 
                                             rs-tipo-valor:radio-buttons in frame f-pg-par)
           tt-param.impr-param      = INPUT FRAME f-pg-par tg-impr-param.
           
    /*:T Executar do programa RP.P que ir  criar o relat¢rio */
    {include/i-rpexb.i}
    
    SESSION:SET-WAIT-STATE("general":U).
    
    {include/i-rprun.i esrp/esce0003rp.p} 
    
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

