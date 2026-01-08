&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME w-relat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-relat 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESSP0084 2.04.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Preprocessadores do Template de Relat¢rio                            */
/* Obs: Retirar o valor do preprocessador para as p ginas que nÆo existirem  */

&GLOBAL-DEFINE PGSEL f-pg-sel
&GLOBAL-DEFINE PGCLA 
&GLOBAL-DEFINE PGPAR f-pg-par
&GLOBAL-DEFINE PGDIG 
&GLOBAL-DEFINE PGIMP f-pg-imp
  
/* Parameters Definitions ---                                           */

/* Temporary Table Definitions ---                                      */

define temp-table tt-param  no-undo
       field destino         as integer
       field arquivo         as char format "x(35)"
       field usuario         as char format "x(12)"
       field data-exec       as date
       field hora-exec       as integer
       field classifica      as integer
       field desc-classifica as char format "x(40)"
       FIELD ini-num-os      LIKE mov-man.num-os
       FIELD fin-num-os      LIKE mov-man.num-os
       FIELD ini-cod-setor   LIKE mov-man.cod-setor
       FIELD fin-cod-setor   LIKE mov-man.cod-setor
       FIELD ini-data-abe    LIKE mov-man.data-abe
       FIELD fin-data-abe    LIKE mov-man.data-abe
       FIELD ini-cod-maq     LIKE mov-man.cod-maq
       FIELD fin-cod-maq     LIKE mov-man.cod-maq
       FIELD ini-func-abe    LIKE mov-man.func-abe
       FIELD fin-func-abe    LIKE mov-man.func-abe
       FIELD ini-sist-const  LIKE mov-man.sist-const
       FIELD fin-sist-const  LIKE mov-man.sist-const
       FIELD ini-cod-area    LIKE mov-man.cod-area
       FIELD fin-cod-area    LIKE mov-man.cod-area
       FIELD ini-ssist-exec  LIKE mov-man.ssist-exec[1]
       FIELD fin-ssist-exec  LIKE mov-man.ssist-exec[1]
       FIELD ini-func-exec   LIKE mov-man.func-exec[1]
       FIELD fin-func-exec   LIKE mov-man.func-exec[1]
       FIELD masc-ssist      LIKE mov-man.ssist-exec[1] 
       FIELD tp-man1         AS CHAR FORMAT "x(3)"
       FIELD tp-man2         AS CHAR FORMAT "x(3)"
       FIELD tp-man3         AS CHAR FORMAT "x(3)"
       FIELD tp-man4         AS CHAR FORMAT "x(3)"
       FIELD tp-man5         AS CHAR FORMAT "x(3)"
       FIELD tp-man6         AS CHAR FORMAT "x(3)"
       FIELD all-types       AS LOG FORMAT "Sim/NÆo"
       FIELD situacao        AS INT
       FIELD imp-obs         AS LOG FORMAT "Sim/NÆo"
       FIELD desc-situacao   AS CHAR FORMAT "x(10)"
       field imp-param       as log.

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
c-arquivo rs-execucao RECT-7 RECT-9 
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

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 46.29 BY 2.92.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 46.29 BY 1.71.

DEFINE VARIABLE fi-masc-ssist AS CHARACTER FORMAT "X(5)":U INITIAL "*****" 
     LABEL "M scara para subsistemas" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .79 TOOLTIP "M scara para sele‡Æo de subsistemas, posi‡Æo com * sem restri‡Æo" NO-UNDO.

DEFINE VARIABLE fi-tp-man1 AS CHARACTER FORMAT "!!!":U 
     LABEL "Tipos de manuten‡Æo" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 TOOLTIP "Primeiro tipo de manuten‡Æo" NO-UNDO.

DEFINE VARIABLE fi-tp-man2 AS CHARACTER FORMAT "!!!":U 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 TOOLTIP "Segundo tipo de manuten‡Æo" NO-UNDO.

DEFINE VARIABLE fi-tp-man3 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 TOOLTIP "Terceiro tipo de manuten‡Æo" NO-UNDO.

DEFINE VARIABLE fi-tp-man4 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 TOOLTIP "Quarto tipo de manuten‡Æo" NO-UNDO.

DEFINE VARIABLE fi-tp-man5 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 TOOLTIP "Quinto tipo de manuten‡Æo" NO-UNDO.

DEFINE VARIABLE fi-tp-man6 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 TOOLTIP "Sexto tipo de manuten‡Æo" NO-UNDO.

DEFINE VARIABLE rs-situacao AS INTEGER INITIAL 3 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Visada", 1,
"NÆo Visada", 2,
"Ambas", 3
     SIZE 31 BY 1.5 TOOLTIP "Tipos de OS: Visada, NÆo visada ou Ambas" NO-UNDO.

DEFINE RECTANGLE RECT-34
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 71 BY 1.75.

DEFINE RECTANGLE RECT-35
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 71 BY 1.75.

DEFINE RECTANGLE RECT-36
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 71 BY 1.75.

DEFINE RECTANGLE RECT-37
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 71 BY 1.75.

DEFINE RECTANGLE RECT-38
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 71 BY 1.75.

DEFINE VARIABLE tg-all-types AS LOGICAL INITIAL yes 
     LABEL "Todos os Tipos" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.57 BY .88 TOOLTIP "Todos os tipos de manuten‡Æo" NO-UNDO.

DEFINE VARIABLE tg-imp-obs AS LOGICAL INITIAL no 
     LABEL "Imprimir Observa‡äes" 
     VIEW-AS TOGGLE-BOX
     SIZE 17.57 BY .88 TOOLTIP "Imprimir observa‡äes"
     FONT 1 NO-UNDO.

DEFINE VARIABLE tg-imp-param AS LOGICAL INITIAL no 
     LABEL "Imprimir Parƒmetros/Sele‡Æo" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .88 TOOLTIP "Imprimir Parƒmetros/Sele‡Æo ao final do relat¢rio."
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-fin-cod-area AS CHARACTER FORMAT "x(3)" INITIAL "ZZZ" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 TOOLTIP "µrea final" NO-UNDO.

DEFINE VARIABLE fi-fin-cod-maq AS CHARACTER FORMAT "x(6)" INITIAL "ZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 TOOLTIP "M quina final" NO-UNDO.

DEFINE VARIABLE fi-fin-cod-setor AS CHARACTER FORMAT "x(3)" INITIAL "ZZZ" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 TOOLTIP "C¢digo do setor final" NO-UNDO.

DEFINE VARIABLE fi-fin-data-abe AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "Data abertura final" NO-UNDO.

DEFINE VARIABLE fi-fin-func-abe AS INTEGER FORMAT "999999" INITIAL 999999 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 TOOLTIP "Funcion rio final" NO-UNDO.

DEFINE VARIABLE fi-fin-func-exec AS INTEGER FORMAT "999999":U INITIAL 999999 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 TOOLTIP "Funcion rio interventor final" NO-UNDO.

DEFINE VARIABLE fi-fin-num-os AS CHARACTER FORMAT "!!99999" INITIAL "ZZ99999" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 TOOLTIP "N£mero OS final" NO-UNDO.

DEFINE VARIABLE fi-fin-sist-const AS CHARACTER FORMAT "x(1)" INITIAL "Z" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 TOOLTIP "Sistema final" NO-UNDO.

DEFINE VARIABLE fi-fin-ssist-exec AS CHARACTER FORMAT "!!999" INITIAL "ZZ999" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 TOOLTIP "Sub-sistema final" NO-UNDO.

DEFINE VARIABLE fi-ini-cod-area AS CHARACTER FORMAT "x(3)" 
     LABEL "Area" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 TOOLTIP "µrea inicial" NO-UNDO.

DEFINE VARIABLE fi-ini-cod-maq AS CHARACTER FORMAT "x(6)" 
     LABEL "M quina" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 TOOLTIP "M quina inicial" NO-UNDO.

DEFINE VARIABLE fi-ini-cod-setor AS CHARACTER FORMAT "x(3)" 
     LABEL "Setor" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 TOOLTIP "C¢digo do setor inicial" NO-UNDO.

DEFINE VARIABLE fi-ini-data-abe AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 
     LABEL "Data abertura" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "Data abertura inicial" NO-UNDO.

DEFINE VARIABLE fi-ini-func-abe AS INTEGER FORMAT "999999" INITIAL 0 
     LABEL "Funcion rio Abertura" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 TOOLTIP "Funcion rio inicial" NO-UNDO.

DEFINE VARIABLE fi-ini-func-exec AS INTEGER FORMAT "999999" INITIAL 0 
     LABEL "Funcion rio Interventor" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 TOOLTIP "Funcion rio interventor inicial" NO-UNDO.

DEFINE VARIABLE fi-ini-num-os AS CHARACTER FORMAT "!!99999" INITIAL "AA00000" 
     LABEL "Numero OS" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 TOOLTIP "N£mero OS inicial" NO-UNDO.

DEFINE VARIABLE fi-ini-sist-const AS CHARACTER FORMAT "x(1)" 
     LABEL "Sistema constatado" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 TOOLTIP "Sistema inicial" NO-UNDO.

DEFINE VARIABLE fi-ini-ssist-exec AS CHARACTER FORMAT "!!999" INITIAL "AA000" 
     LABEL "Subsistema" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 TOOLTIP "Sub-sistema inicial" NO-UNDO.

DEFINE IMAGE IMAGE-29
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

DEFINE IMAGE IMAGE-39
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-40
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-41
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-42
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-43
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-44
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-53
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-54
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

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
     RECT-7 AT ROW 1.92 COL 2.14
     RECT-9 AT ROW 5.29 COL 2.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 73.72 BY 10.

DEFINE FRAME f-relat
     bt-executar AT ROW 14.54 COL 3 HELP
          "Dispara a execu‡Æo do relat¢rio"
     bt-ajuda AT ROW 14.54 COL 70 HELP
          "Ajuda"
     bt-cancelar AT ROW 14.58 COL 14 HELP
          "Fechar"
     rt-folder-top AT ROW 2.54 COL 2.14
     rt-folder-right AT ROW 2.67 COL 80.43
     RECT-1 AT ROW 14.29 COL 2
     rt-folder AT ROW 2.54 COL 2
     rt-folder-left AT ROW 2.54 COL 2.14
     RECT-6 AT ROW 13.75 COL 2.14
     im-pg-imp AT ROW 1.5 COL 33.57
     im-pg-par AT ROW 1.5 COL 17.86
     im-pg-sel AT ROW 1.5 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81 BY 15
         DEFAULT-BUTTON bt-executar.

DEFINE FRAME f-pg-par
     fi-masc-ssist AT ROW 1.92 COL 36.72 COLON-ALIGNED
     fi-tp-man1 AT ROW 3.88 COL 20 COLON-ALIGNED
     fi-tp-man2 AT ROW 3.88 COL 26 COLON-ALIGNED NO-LABEL
     fi-tp-man3 AT ROW 3.88 COL 32 COLON-ALIGNED NO-LABEL
     fi-tp-man4 AT ROW 3.88 COL 38 COLON-ALIGNED NO-LABEL
     fi-tp-man5 AT ROW 3.88 COL 44 COLON-ALIGNED NO-LABEL
     fi-tp-man6 AT ROW 3.88 COL 50 COLON-ALIGNED NO-LABEL
     rs-situacao AT ROW 5.5 COL 29.57 NO-LABEL
     tg-all-types AT ROW 3.88 COL 58.43
     tg-imp-param AT ROW 9.79 COL 25.43
     tg-imp-obs AT ROW 7.79 COL 28.14
     RECT-34 AT ROW 9.38 COL 4
     RECT-35 AT ROW 3.42 COL 4
     RECT-36 AT ROW 5.38 COL 4
     RECT-37 AT ROW 1.5 COL 4
     RECT-38 AT ROW 7.38 COL 4
     "Tipos de OS:" VIEW-AS TEXT
          SIZE 9.86 BY .54 AT ROW 5.96 COL 19.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 77.14 BY 10.5
         FONT 1.

DEFINE FRAME f-pg-sel
     fi-ini-num-os AT ROW 1.25 COL 19 COLON-ALIGNED HELP
          "Numero da Ordem de Servico (2 Letras e 5 numeros)"
     fi-fin-num-os AT ROW 1.25 COL 52 COLON-ALIGNED HELP
          "Numero da Ordem de Servico (2 Letras e 5 numeros)" NO-LABEL
     fi-ini-cod-setor AT ROW 2.25 COL 19 COLON-ALIGNED HELP
          "Codido do setor que gerou a OS"
     fi-fin-cod-setor AT ROW 2.25 COL 52 COLON-ALIGNED HELP
          "Codido do setor que gerou a OS" NO-LABEL
     fi-ini-data-abe AT ROW 3.25 COL 19 COLON-ALIGNED HELP
          "Data da abertura da OS"
     fi-fin-data-abe AT ROW 3.25 COL 52 COLON-ALIGNED HELP
          "Data da abertura da OS" NO-LABEL
     fi-ini-cod-maq AT ROW 4.25 COL 19 COLON-ALIGNED HELP
          "Codigo da maquina"
     fi-fin-cod-maq AT ROW 4.25 COL 52 COLON-ALIGNED HELP
          "Codigo da maquina" NO-LABEL
     fi-ini-func-abe AT ROW 5.25 COL 19 COLON-ALIGNED HELP
          "Registro do funcionario que gerou a OS"
     fi-fin-func-abe AT ROW 5.25 COL 52 COLON-ALIGNED HELP
          "Registro do funcionario que gerou a OS" NO-LABEL
     fi-ini-sist-const AT ROW 6.25 COL 19 COLON-ALIGNED HELP
          "Sistema constatado (1=Mecanico 2=Eletrico 3=Hidraulico)"
     fi-fin-sist-const AT ROW 6.25 COL 52 COLON-ALIGNED HELP
          "Sistema constatado (1=Mecanico 2=Eletrico 3=Hidraulico)" NO-LABEL
     fi-ini-cod-area AT ROW 7.25 COL 19 COLON-ALIGNED HELP
          "Codigo da area de manutencao"
     fi-fin-cod-area AT ROW 7.25 COL 52 COLON-ALIGNED HELP
          "Codigo da area de manutencao" NO-LABEL
     fi-ini-ssist-exec AT ROW 8.25 COL 19 COLON-ALIGNED
     fi-fin-ssist-exec AT ROW 8.25 COL 52 COLON-ALIGNED NO-LABEL
     fi-ini-func-exec AT ROW 9.25 COL 19 COLON-ALIGNED
     fi-fin-func-exec AT ROW 9.25 COL 52 COLON-ALIGNED NO-LABEL
     IMAGE-29 AT ROW 2.25 COL 31.57
     IMAGE-30 AT ROW 2.25 COL 50.86
     IMAGE-31 AT ROW 1.25 COL 31.57
     IMAGE-32 AT ROW 1.25 COL 50.86
     IMAGE-33 AT ROW 3.25 COL 31.57
     IMAGE-34 AT ROW 3.25 COL 50.86
     IMAGE-35 AT ROW 4.25 COL 31.57
     IMAGE-36 AT ROW 4.25 COL 50.86
     IMAGE-37 AT ROW 5.25 COL 31.57
     IMAGE-38 AT ROW 5.25 COL 50.86
     IMAGE-39 AT ROW 6.25 COL 31.57
     IMAGE-40 AT ROW 6.25 COL 50.86
     IMAGE-41 AT ROW 8.25 COL 31.57
     IMAGE-42 AT ROW 8.25 COL 50.86
     IMAGE-43 AT ROW 9.25 COL 31.57
     IMAGE-44 AT ROW 9.25 COL 50.86
     IMAGE-53 AT ROW 7.25 COL 31.57
     IMAGE-54 AT ROW 7.25 COL 50.86
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.57 ROW 3.21
         SIZE 75.86 BY 10.25
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
         TITLE              = "Incidˆncia de Manuten‡Æo por Sistema"
         HEIGHT             = 15
         WIDTH              = 81.14
         MAX-HEIGHT         = 28.63
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 28.63
         VIRTUAL-WIDTH      = 146.29
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
   Custom                                                               */
/* SETTINGS FOR FILL-IN fi-tp-man1 IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tp-man2 IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tp-man3 IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tp-man4 IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tp-man5 IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tp-man6 IN FRAME f-pg-par
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
ON END-ERROR OF w-relat /* Incidˆncia de Manuten‡Æo por Sistema */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON WINDOW-CLOSE OF w-relat /* Incidˆncia de Manuten‡Æo por Sistema */
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
   apply "close" to this-procedure.
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
&Scoped-define SELF-NAME fi-fin-cod-area
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fin-cod-area w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-fin-cod-area IN FRAME f-pg-sel
DO:
  {include/zoomvar.i &prog-zoom = eszoom/z01es002.w
                     &campo     = fi-fin-cod-area
                     &campozoom = codigo
                     &FRAME     = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-fin-cod-maq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fin-cod-maq w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-fin-cod-maq IN FRAME f-pg-sel
DO:
    {include/zoomvar.i &prog-zoom = eszoom/z01es019.w
                     &campo       = fi-fin-cod-maq
                     &campozoom   = codigo
                     &FRAME       = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-fin-cod-setor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fin-cod-setor w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-fin-cod-setor IN FRAME f-pg-sel
DO:
    {include/zoomvar.i &prog-zoom = eszoom/z01es026.w
                     &campo       = fi-fin-cod-setor
                     &campozoom   = codigo
                     &FRAME       = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-fin-ssist-exec
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fin-ssist-exec w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-fin-ssist-exec IN FRAME f-pg-sel
DO:
  {include/zoomvar.i &prog-zoom = eszoom/z01es027.w
                     &campo     = fi-fin-ssist-exec
                     &campozoom = codigo
                     &FRAME     = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ini-cod-area
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-cod-area w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ini-cod-area IN FRAME f-pg-sel /* Area */
DO:
  {include/zoomvar.i &prog-zoom = eszoom/z01es002.w
                     &campo     = fi-ini-cod-area
                     &campozoom = codigo
                     &FRAME     = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ini-cod-maq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-cod-maq w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ini-cod-maq IN FRAME f-pg-sel /* M quina */
DO:
    {include/zoomvar.i &prog-zoom = eszoom/z01es019.w
                     &campo       = fi-ini-cod-maq
                     &campozoom   = codigo
                     &FRAME       = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ini-cod-setor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-cod-setor w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ini-cod-setor IN FRAME f-pg-sel /* Setor */
DO:
    {include/zoomvar.i &prog-zoom = eszoom/z01es026.w
                     &campo       = fi-ini-cod-setor
                     &campozoom   = codigo
                     &FRAME       = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ini-ssist-exec
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-ssist-exec w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ini-ssist-exec IN FRAME f-pg-sel /* Subsistema */
DO:
  {include/zoomvar.i &prog-zoom = eszoom/z01es027.w
                     &campo     = fi-ini-ssist-exec
                     &campozoom = codigo
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
&Scoped-define SELF-NAME tg-all-types
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-all-types w-relat
ON VALUE-CHANGED OF tg-all-types IN FRAME f-pg-par /* Todos os Tipos */
DO:
  IF INPUT FRAME {&FRAME-NAME} tg-all-types THEN 
     ASSIGN fi-tp-man1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
            fi-tp-man2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" 
            fi-tp-man3:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" 
            fi-tp-man4:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" 
            fi-tp-man5:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" 
            fi-tp-man6:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" 
            fi-tp-man1:SENSITIVE IN FRAME {&FRAME-NAME} = NO
            fi-tp-man2:SENSITIVE IN FRAME {&FRAME-NAME} = NO
            fi-tp-man3:SENSITIVE IN FRAME {&FRAME-NAME} = NO
            fi-tp-man4:SENSITIVE IN FRAME {&FRAME-NAME} = NO
            fi-tp-man5:SENSITIVE IN FRAME {&FRAME-NAME} = NO
            fi-tp-man6:SENSITIVE IN FRAME {&FRAME-NAME} = NO.    
  ELSE DO.
     ASSIGN fi-tp-man1:SENSITIVE IN FRAME {&FRAME-NAME} = yes  
            fi-tp-man2:SENSITIVE IN FRAME {&FRAME-NAME} = yes  
            fi-tp-man3:SENSITIVE IN FRAME {&FRAME-NAME} = yes  
            fi-tp-man4:SENSITIVE IN FRAME {&FRAME-NAME} = yes  
            fi-tp-man5:SENSITIVE IN FRAME {&FRAME-NAME} = yes  
            fi-tp-man6:SENSITIVE IN FRAME {&FRAME-NAME} = yes. 
     APPLY 'entry' TO fi-tp-man1 IN FRAME {&FRAME-NAME}.
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

{utp/ut9000.i "ESSP0084" "2.04.00.000"}

/* inicializa‡äes do template de relat¢rio */
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

    fi-ini-cod-setor:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-fin-cod-setor:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-ini-cod-maq:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-fin-cod-maq:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-ini-cod-area:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-fin-cod-area:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-ini-ssist-exec:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-fin-ssist-exec:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    
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
  ENABLE bt-executar bt-ajuda bt-cancelar im-pg-imp im-pg-par im-pg-sel 
      WITH FRAME f-relat IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}
  DISPLAY rs-destino c-arquivo rs-execucao 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  ENABLE rs-destino bt-arquivo bt-config-impr c-arquivo rs-execucao RECT-7 
         RECT-9 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}
  DISPLAY fi-masc-ssist fi-tp-man1 fi-tp-man2 fi-tp-man3 fi-tp-man4 fi-tp-man5 
          fi-tp-man6 rs-situacao tg-all-types tg-imp-param tg-imp-obs 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  ENABLE fi-masc-ssist rs-situacao tg-all-types tg-imp-param tg-imp-obs RECT-34 
         RECT-35 RECT-36 RECT-37 RECT-38 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-par}
  DISPLAY fi-ini-num-os fi-fin-num-os fi-ini-cod-setor fi-fin-cod-setor 
          fi-ini-data-abe fi-fin-data-abe fi-ini-cod-maq fi-fin-cod-maq 
          fi-ini-func-abe fi-fin-func-abe fi-ini-sist-const fi-fin-sist-const 
          fi-ini-cod-area fi-fin-cod-area fi-ini-ssist-exec fi-fin-ssist-exec 
          fi-ini-func-exec fi-fin-func-exec 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  ENABLE fi-ini-num-os fi-fin-num-os fi-ini-cod-setor fi-fin-cod-setor 
         fi-ini-data-abe fi-fin-data-abe fi-ini-cod-maq fi-fin-cod-maq 
         fi-ini-func-abe fi-fin-func-abe fi-ini-sist-const fi-fin-sist-const 
         fi-ini-cod-area fi-fin-cod-area fi-ini-ssist-exec fi-fin-ssist-exec 
         fi-ini-func-exec fi-fin-func-exec IMAGE-29 IMAGE-30 IMAGE-31 IMAGE-32 
         IMAGE-33 IMAGE-34 IMAGE-35 IMAGE-36 IMAGE-37 IMAGE-38 IMAGE-39 
         IMAGE-40 IMAGE-41 IMAGE-42 IMAGE-43 IMAGE-44 IMAGE-53 IMAGE-54 
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
            run utp/ut-msgs.p (input "show", input 73, input "").
            
            apply "MOUSE-SELECT-CLICK":U to im-pg-imp in frame f-relat.
            apply "ENTRY":U to c-arquivo in frame f-pg-imp.
            return error.
        end.
    end.
    
    create tt-param.
    assign tt-param.usuario          = c-seg-usuario
           tt-param.destino          = input frame f-pg-imp rs-destino
           tt-param.data-exec        = today
           tt-param.hora-exec        = time
           tt-param.ini-num-os       = INPUT FRAME f-pg-sel fi-ini-num-os
           tt-param.fin-num-os       = INPUT FRAME f-pg-sel fi-fin-num-os
           tt-param.ini-cod-setor    = INPUT FRAME f-pg-sel fi-ini-cod-setor
           tt-param.fin-cod-setor    = INPUT FRAME f-pg-sel fi-fin-cod-setor
           tt-param.ini-data-abe     = INPUT FRAME f-pg-sel fi-ini-data-abe
           tt-param.fin-data-abe     = INPUT FRAME f-pg-sel fi-fin-data-abe
           tt-param.ini-cod-maq      = INPUT FRAME f-pg-sel fi-ini-cod-maq
           tt-param.fin-cod-maq      = INPUT FRAME f-pg-sel fi-fin-cod-maq
           tt-param.ini-func-abe     = INPUT FRAME f-pg-sel fi-ini-func-abe
           tt-param.fin-func-abe     = INPUT FRAME f-pg-sel fi-fin-func-abe   
           tt-param.ini-sist-const   = INPUT FRAME f-pg-sel fi-ini-sist-const   
           tt-param.fin-sist-const   = INPUT FRAME f-pg-sel fi-fin-sist-const
           tt-param.ini-cod-area     = INPUT FRAME f-pg-sel fi-ini-cod-area   
           tt-param.fin-cod-area     = INPUT FRAME f-pg-sel fi-fin-cod-area
           tt-param.ini-ssist-exec   = INPUT FRAME f-pg-sel fi-ini-ssist-exec   
           tt-param.fin-ssist-exec   = INPUT FRAME f-pg-sel fi-fin-ssist-exec   
           tt-param.ini-func-exec    = INPUT FRAME f-pg-sel fi-ini-func-exec   
           tt-param.fin-func-exec    = INPUT FRAME f-pg-sel fi-fin-func-exec   
           tt-param.masc-ssist       = INPUT FRAME f-pg-par fi-masc-ssist   
           tt-param.tp-man1          = INPUT FRAME f-pg-par fi-tp-man1   
           tt-param.tp-man2          = INPUT FRAME f-pg-par fi-tp-man2   
           tt-param.tp-man3          = INPUT FRAME f-pg-par fi-tp-man3   
           tt-param.tp-man4          = INPUT FRAME f-pg-par fi-tp-man4   
           tt-param.tp-man5          = INPUT FRAME f-pg-par fi-tp-man5   
           tt-param.tp-man6          = INPUT FRAME f-pg-par fi-tp-man6  
           tt-param.all-types        = INPUT FRAME f-pg-par tg-all-types
           tt-param.situacao         = INPUT FRAME f-pg-par rs-situacao 
           tt-param.imp-obs          = INPUT FRAME f-pg-par tg-imp-obs
           tt-param.desc-situacao    = entry((tt-param.situacao - 1) * 2 + 1, 
                                              rs-situacao:radio-buttons in frame f-pg-par)   
           tt-param.imp-param        = INPUT FRAME f-pg-par tg-imp-param.
           
    if tt-param.destino = 1 THEN
       assign tt-param.arquivo = "".
    else 
    if tt-param.destino = 2 THEN
       assign tt-param.arquivo = input frame f-pg-imp c-arquivo.
    else 
       ASSIGN tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp".
    
    /* Executar do programa RP.P que ir  criar o relat¢rio */
    {include/i-rpexb.i}
    
    SESSION:SET-WAIT-STATE("general":U).
    
    {include/i-rprun.i esrp/essp0084rp.p}
    
    {include/i-rpexc.i}
    
    SESSION:SET-WAIT-STATE("":U).
    
    {include/i-rptrm.i}
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-troca-pagina w-relat 
PROCEDURE pi-troca-pagina :
/*------------------------------------------------------------------------------
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

