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
{include/i-prgvrs.i ESCB0001 2.04.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/*:T Preprocessadores do Template de Relat¢rio                            */
/*:T Obs: Retirar o valor do preprocessador para as p†ginas que n∆o existirem  */

&GLOBAL-DEFINE PGSEL f-pg-sel
&GLOBAL-DEFINE PGCLA 
&GLOBAL-DEFINE PGPAR f-pg-par
&GLOBAL-DEFINE PGDIG 
&GLOBAL-DEFINE PGIMP f-pg-imp
  
/* Parameters Definitions ---                                           */

/* Temporary Table Definitions ---                                      */

define temp-table tt-param         no-undo
       field destino               as integer
       field arquivo               as char format "x(35)"
       field usuario               as char format "x(12)"
       field data-exec             as date
       field hora-exec             as integer
       FIELD ep-codigo             LIKE cheque.ep-codigo-mvto
       FIELD cod-estabel-ini       LIKE cheque.cod-estabel-mvto
       FIELD cod-estabel-fin       LIKE cheque.cod-estabel-mvto
       FIELD cod-banco-ini         LIKE cheque.cod-banco 
       FIELD cod-banco-fin         LIKE cheque.cod-banco
       FIELD dt-vencimento-ini     LIKE cheque.dt-vencimento
       FIELD dt-vencimento-fin     LIKE cheque.dt-vencimento
       FIELD dt-deposito-ini       LIKE cheque.dt-deposito
       FIELD dt-deposito-fin       LIKE cheque.dt-deposito
       FIELD dt-emissao-ini        LIKE cheque.dt-emissao
       FIELD dt-emissao-fin        LIKE cheque.dt-emissao
       FIELD tp-codigo-ini         LIKE cheque.tp-codigo
       FIELD tp-codigo-fin         LIKE cheque.tp-codigo
       FIELD cod-emitente-ini      LIKE cheque.cod-emitente
       FIELD cod-emitente-fin      LIKE cheque.cod-emitente
       FIELD nom-emit-cheque-ini   LIKE cheque.nom-emit-cheque
       FIELD nom-emit-cheque-fin   LIKE cheque.nom-emit-cheque
       FIELD tipo-cheque           AS INT
       FIELD desc-tipo-cheque      AS CHAR FORMAT "x(17)"
       FIELD sit-chq-pend          AS LOGICAL FORMAT "Sim/N∆o"
       FIELD sit-chq-depos         AS LOGICAL FORMAT "Sim/N∆o"
       FIELD sit-chq-caucao        AS LOGICAL FORMAT "Sim/N∆o"
       FIELD sit-chq-subst         AS LOGICAL FORMAT "Sim/N∆o"
       FIELD sit-chq-devolv        AS LOGICAL FORMAT "Sim/N∆o"
       FIELD sit-chq-canc          AS LOGICAL FORMAT "Sim/N∆o"
       FIELD sit-chq-desc          AS LOGICAL FORMAT "Sim/N∆o" 
       FIELD sit-chq-comp          AS LOGICAL FORMAT "Sim/N∆o" 
       FIELD sit-chq-pgemp         AS LOGICAL FORMAT "Sim/N∆o" 
       FIELD tipo-rel              AS INT
       FIELD desc-tipo-rel         AS CHAR FORMAT "x(10)"
       FIELD impr-param            AS LOGICAL.

define temp-table tt-digita no-undo
       field ordem            as integer   format ">>>>9"
       field exemplo          as character format "x(30)"
       index id ordem.

define buffer b-tt-digita for tt-digita.

/* Transfer Definitions */

def var raw-param        as raw no-undo.

def temp-table tt-raw-digita
   field raw-digita      as raw.
                    
/* Local Variable Definitions --- */

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
&Scoped-Define ENABLED-OBJECTS rs-destino bt-config-impr bt-arquivo ~
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

DEFINE VARIABLE text-modo AS CHARACTER FORMAT "X(256)":U INITIAL "Execuá∆o" 
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

DEFINE VARIABLE rs-tipo-cheque AS INTEGER INITIAL 3 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Pr¢prio", 1,
"Terceiros", 2,
"Ambos", 3
     SIZE 29 BY 1 TOOLTIP "Tipo do relat¢rio" NO-UNDO.

DEFINE VARIABLE rs-tipo-rel AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Detalhado", 1,
"Totais por Dia", 2,
"Resumido", 3
     SIZE 38 BY 1 TOOLTIP "Tipo do relat¢rio" NO-UNDO.

DEFINE RECTANGLE RECT-35
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 71 BY 3.75.

DEFINE RECTANGLE RECT-36
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 71 BY 1.75.

DEFINE RECTANGLE RECT-37
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 71 BY 1.75.

DEFINE VARIABLE tg-impr-param AS LOGICAL INITIAL no 
     LABEL "Imprimir ParÉmetros/Seleá∆o" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .88 TOOLTIP "Imprimir ParÉmetros/Seleá∆o ao final do relat¢rio."
     FONT 1 NO-UNDO.

DEFINE VARIABLE tg-sit-chq-canc AS LOGICAL INITIAL no 
     LABEL "Cancelado" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE tg-sit-chq-caucao AS LOGICAL INITIAL no 
     LABEL "Cauá∆o" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE tg-sit-chq-comp AS LOGICAL INITIAL no 
     LABEL "Compensado" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE tg-sit-chq-depos AS LOGICAL INITIAL yes 
     LABEL "Depositado" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE tg-sit-chq-desc AS LOGICAL INITIAL no 
     LABEL "Descontado" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE tg-sit-chq-devolv AS LOGICAL INITIAL no 
     LABEL "Devolvido" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE tg-sit-chq-pend AS LOGICAL INITIAL yes 
     LABEL "Pendente" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE tg-sit-chq-pgemp AS LOGICAL INITIAL no 
     LABEL "Pagto EmprÇstimos" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .83 NO-UNDO.

DEFINE VARIABLE tg-sit-chq-subst AS LOGICAL INITIAL no 
     LABEL "Substitu°do" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE fi-ep-codigo AS INTEGER FORMAT ">>9" INITIAL 0 
     LABEL "Empresa":R9 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 TOOLTIP "C¢digo da empresa" NO-UNDO.

DEFINE VARIABLE fi-fin-cod-banco AS INTEGER FORMAT ">>9" INITIAL 999 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 TOOLTIP "Banco final" NO-UNDO.

DEFINE VARIABLE fi-fin-cod-emitente AS INTEGER FORMAT ">>>>>>>>9" INITIAL 999999999 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 TOOLTIP "C¢digo do emitente final." NO-UNDO.

DEFINE VARIABLE fi-fin-cod-estabel AS CHARACTER FORMAT "x(3)" INITIAL "ZZZ" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 TOOLTIP "Estabelecimento final" NO-UNDO.

DEFINE VARIABLE fi-fin-dt-deposito AS DATE FORMAT "99/99/9999" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "Data de dep¢sito final" NO-UNDO.

DEFINE VARIABLE fi-fin-dt-emissao AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "Data de emiss∆o final" NO-UNDO.

DEFINE VARIABLE fi-fin-dt-vencimento AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "Data de prevista de deposito final" NO-UNDO.

DEFINE VARIABLE fi-fin-nom-emit-cheque AS CHARACTER FORMAT "x(40)" INITIAL "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 24 BY .88 TOOLTIP "Nome do emitente do cheque final" NO-UNDO.

DEFINE VARIABLE fi-fin-tp-codigo AS INTEGER FORMAT ">>9" INITIAL 999 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 TOOLTIP "Tipo de Receita/Despesa final" NO-UNDO.

DEFINE VARIABLE fi-ini-cod-banco AS INTEGER FORMAT ">>9" INITIAL 0 
     LABEL "Banco":R7 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 TOOLTIP "Banco inicial" NO-UNDO.

DEFINE VARIABLE fi-ini-cod-emitente AS INTEGER FORMAT ">>>>>>>>9" INITIAL 0 
     LABEL "Emitente":R10 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 TOOLTIP "C¢digo do emitente inicial." NO-UNDO.

DEFINE VARIABLE fi-ini-cod-estabel AS CHARACTER FORMAT "X(3)":U 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-dt-deposito AS DATE FORMAT "99/99/9999" 
     LABEL "Data Dep¢sito":R16 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "Data de dep¢sito inicial" NO-UNDO.

DEFINE VARIABLE fi-ini-dt-emissao AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 
     LABEL "Data Emiss∆o":R15 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "Data de emiss∆o inicial" NO-UNDO.

DEFINE VARIABLE fi-ini-dt-vencimento AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 
     LABEL "Dt Prevista Dep¢sito":R24 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "Data de prevista de deposito inicial" NO-UNDO.

DEFINE VARIABLE fi-ini-nom-emit-cheque AS CHARACTER FORMAT "x(40)" 
     LABEL "Emitente Cheque":R18 
     VIEW-AS FILL-IN 
     SIZE 24 BY .88 TOOLTIP "Nome do emitente do cheque inicial" NO-UNDO.

DEFINE VARIABLE fi-ini-tp-codigo AS INTEGER FORMAT ">>9" INITIAL 0 
     LABEL "Tp Recta/Desp":R20 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 TOOLTIP "Tipo de Receita/Despesa inicial" NO-UNDO.

DEFINE VARIABLE fi-razao-social AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 47.14 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-57
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-58
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-59
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-60
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-61
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-62
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

DEFINE IMAGE IMAGE-73
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-74
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
          "Dispara a execuá∆o do relat¢rio"
     bt-cancelar AT ROW 14.54 COL 14 HELP
          "Fechar"
     bt-ajuda AT ROW 14.54 COL 70 HELP
          "Ajuda"
     rt-folder AT ROW 2.5 COL 2
     rt-folder-left AT ROW 2.54 COL 2.14
     rt-folder-right AT ROW 2.67 COL 80.43
     rt-folder-top AT ROW 2.54 COL 2.14
     RECT-1 AT ROW 14.29 COL 2
     RECT-6 AT ROW 13.75 COL 2.14
     im-pg-imp AT ROW 1.5 COL 33.43
     im-pg-par AT ROW 1.5 COL 17.72
     im-pg-sel AT ROW 1.5 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81 BY 15
         DEFAULT-BUTTON bt-executar.

DEFINE FRAME f-pg-imp
     rs-destino AT ROW 2.38 COL 3.29 HELP
          "Destino de Impress∆o do Relat¢rio" NO-LABEL
     bt-config-impr AT ROW 3.58 COL 43.29 HELP
          "Configuraá∆o da impressora"
     bt-arquivo AT ROW 3.58 COL 43.29 HELP
          "Escolha do nome do arquivo"
     c-arquivo AT ROW 3.63 COL 3.29 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL
     rs-execucao AT ROW 5.75 COL 3 HELP
          "Modo de Execuá∆o" NO-LABEL
     text-destino AT ROW 1.63 COL 3.86 NO-LABEL
     text-modo AT ROW 5 COL 1.29 COLON-ALIGNED NO-LABEL
     RECT-17 AT ROW 1.08 COL 1.29
     RECT-7 AT ROW 1.92 COL 2.14
     RECT-9 AT ROW 5.29 COL 2.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2.43 ROW 2.83
         SIZE 77.72 BY 10.83
         FONT 1.

DEFINE FRAME f-pg-par
     rs-tipo-cheque AT ROW 2.33 COL 10.29 NO-LABEL
     tg-sit-chq-pend AT ROW 5 COL 8.43
     tg-sit-chq-subst AT ROW 5 COL 31.57
     tg-sit-chq-desc AT ROW 5 COL 55
     tg-sit-chq-depos AT ROW 6 COL 8.43
     tg-sit-chq-devolv AT ROW 6 COL 31.57
     tg-sit-chq-comp AT ROW 6 COL 55
     tg-sit-chq-caucao AT ROW 7 COL 8.43
     tg-sit-chq-canc AT ROW 7 COL 31.57
     tg-sit-chq-pgemp AT ROW 7 COL 55
     rs-tipo-rel AT ROW 9.5 COL 5 NO-LABEL
     tg-impr-param AT ROW 9.5 COL 47.14
     RECT-35 AT ROW 4.5 COL 4
     RECT-36 AT ROW 1.96 COL 4
     RECT-37 AT ROW 9.13 COL 4
     "Tipos de Cheques" VIEW-AS TEXT
          SIZE 13 BY .54 AT ROW 1.71 COL 17.72
     "Situaá‰es dos Cheques" VIEW-AS TEXT
          SIZE 16.57 BY .54 AT ROW 4.21 COL 29.43
     "Tipo do Relat¢rio" VIEW-AS TEXT
          SIZE 13 BY .54 AT ROW 8.88 COL 17.72
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2.72 ROW 2.83
         SIZE 77.29 BY 10.79
         FONT 1.

DEFINE FRAME f-pg-sel
     fi-ep-codigo AT ROW 1.83 COL 15.43 COLON-ALIGNED HELP
          "C¢digo da empresa"
     fi-razao-social AT ROW 1.83 COL 19.57 COLON-ALIGNED NO-LABEL
     fi-ini-cod-estabel AT ROW 2.83 COL 15.43 COLON-ALIGNED
     fi-fin-cod-estabel AT ROW 2.83 COL 49.29 COLON-ALIGNED NO-LABEL
     fi-ini-cod-banco AT ROW 3.83 COL 15.43 COLON-ALIGNED
     fi-fin-cod-banco AT ROW 3.83 COL 49.29 COLON-ALIGNED NO-LABEL
     fi-ini-dt-vencimento AT ROW 4.83 COL 15.43 COLON-ALIGNED HELP
          "Data prevista para o dep¢sito"
     fi-fin-dt-vencimento AT ROW 4.83 COL 49.29 COLON-ALIGNED HELP
          "Data prevista para o dep¢sito" NO-LABEL
     fi-ini-dt-deposito AT ROW 5.83 COL 15.43 COLON-ALIGNED HELP
          "Data efetiva do dep¢sito do cheque"
     fi-fin-dt-deposito AT ROW 5.83 COL 49.29 COLON-ALIGNED HELP
          "Data efetiva do dep¢sito do cheque" NO-LABEL
     fi-ini-dt-emissao AT ROW 6.83 COL 15.43 COLON-ALIGNED HELP
          "Data Emiss∆o"
     fi-fin-dt-emissao AT ROW 6.83 COL 49.29 COLON-ALIGNED HELP
          "Data Emiss∆o" NO-LABEL
     fi-ini-tp-codigo AT ROW 7.83 COL 15.43 COLON-ALIGNED
     fi-fin-tp-codigo AT ROW 7.83 COL 49.29 COLON-ALIGNED NO-LABEL
     fi-ini-cod-emitente AT ROW 8.75 COL 15.43 COLON-ALIGNED HELP
          "C¢digo do emitente"
     fi-fin-cod-emitente AT ROW 8.83 COL 49.29 COLON-ALIGNED HELP
          "C¢digo do emitente" NO-LABEL
     fi-ini-nom-emit-cheque AT ROW 9.83 COL 15.43 COLON-ALIGNED
     fi-fin-nom-emit-cheque AT ROW 9.83 COL 49.29 COLON-ALIGNED NO-LABEL
     IMAGE-1 AT ROW 3.83 COL 41.72
     IMAGE-2 AT ROW 3.83 COL 47.57
     IMAGE-57 AT ROW 8.83 COL 41.72
     IMAGE-58 AT ROW 8.83 COL 47.57
     IMAGE-59 AT ROW 4.83 COL 41.72
     IMAGE-60 AT ROW 4.83 COL 47.57
     IMAGE-61 AT ROW 5.83 COL 41.72
     IMAGE-62 AT ROW 5.83 COL 47.57
     IMAGE-67 AT ROW 2.83 COL 41.72
     IMAGE-68 AT ROW 2.83 COL 47.57
     IMAGE-69 AT ROW 6.83 COL 41.72
     IMAGE-70 AT ROW 6.83 COL 47.57
     IMAGE-71 AT ROW 7.83 COL 41.72
     IMAGE-72 AT ROW 7.83 COL 47.57
     IMAGE-73 AT ROW 9.83 COL 41.72
     IMAGE-74 AT ROW 9.83 COL 47.57
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2.57 ROW 2.83
         SIZE 77.43 BY 10.83
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
         TITLE              = "Cheques por Tipo de Receita"
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
                "Execuá∆o".

/* SETTINGS FOR FRAME f-pg-par
                                                                        */
/* SETTINGS FOR FRAME f-pg-sel
                                                                        */
/* SETTINGS FOR FILL-IN fi-razao-social IN FRAME f-pg-sel
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
ON END-ERROR OF w-relat /* Cheques por Tipo de Receita */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON WINDOW-CLOSE OF w-relat /* Cheques por Tipo de Receita */
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
&Scoped-define SELF-NAME fi-ep-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ep-codigo w-relat
ON ENTRY OF fi-ep-codigo IN FRAME f-pg-sel /* Empresa */
DO:
  IF SELF:SCREEN-VALUE <> "" THEN DO.
     FIND empresa WHERE
          empresa.ep-codigo = INPUT FRAME {&FRAME-NAME} fi-ep-codigo
          NO-LOCK NO-ERROR.
     IF AVAIL empresa THEN
        ASSIGN fi-razao-social:SCREEN-VALUE IN FRAME {&FRAME-NAME} = empresa.razao-social.
  END.
  ELSE ASSIGN fi-razao-social:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ep-codigo w-relat
ON LEAVE OF fi-ep-codigo IN FRAME f-pg-sel /* Empresa */
DO:
  FIND empresa WHERE
       empresa.ep-codigo = INPUT FRAME {&FRAME-NAME} fi-ep-codigo NO-LOCK NO-ERROR.

  IF NOT AVAIL empresa THEN DO.
     MESSAGE "Empresa n∆o Cadastrada." VIEW-AS ALERT-BOX.
     RETURN NO-APPLY.
  END.
  ASSIGN fi-razao-social:SCREEN-VALUE IN FRAME {&FRAME-NAME} = empresa.razao-social.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ep-codigo w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ep-codigo IN FRAME f-pg-sel /* Empresa */
DO:
  {include/zoomvar.i &prog-zoom = unzoom/z01un004.w
                     &campo     = fi-ep-codigo
                     &campozoom = ep-codigo
                     &FRAME     = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-fin-nom-emit-cheque
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fin-nom-emit-cheque w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-fin-nom-emit-cheque IN FRAME f-pg-sel
DO:
  {include/zoomvar.i &prog-zoom = adzoom/z01ad107.w
                     &campo     = fi-fin-cod-estabel
                     &campozoom = cod-estabel
                     &FRAME     = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ini-cod-estabel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-cod-estabel w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ini-cod-estabel IN FRAME f-pg-sel /* Estabelecimento */
DO:
  {include/zoomvar.i &prog-zoom = adzoom/z01ad107.w
                     &campo     = fi-ini-cod-estabel
                     &campozoom = cod-estabel
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


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-relat 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{utp/ut9000.i "ESCB0001" "2.04.00.000"}

/*:T inicializaá‰es do template de relat¢rio */
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
  
    ASSIGN fi-ep-codigo:SCREEN-VALUE IN FRAME f-pg-sel = "1" 
           fi-ini-cod-estabel:SCREEN-VALUE IN FRAME f-pg-sel = "1"
           fi-fin-cod-estabel:SCREEN-VALUE IN FRAME f-pg-sel = "999"
           fi-ini-cod-banco:SCREEN-VALUE IN FRAME f-pg-sel = "1"
           fi-fin-cod-banco:SCREEN-VALUE IN FRAME f-pg-sel = "999".
    
    {include/i-rpmbl.i}
        
    fi-ep-codigo:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-ini-cod-estabel:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-fin-cod-estabel:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-ini-cod-banco:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-fin-cod-banco:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.

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
  DISPLAY rs-destino c-arquivo rs-execucao 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  ENABLE rs-destino bt-config-impr bt-arquivo c-arquivo rs-execucao RECT-17 
         RECT-7 RECT-9 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}
  DISPLAY fi-ep-codigo fi-razao-social fi-ini-cod-estabel fi-fin-cod-estabel 
          fi-ini-cod-banco fi-fin-cod-banco fi-ini-dt-vencimento 
          fi-fin-dt-vencimento fi-ini-dt-deposito fi-fin-dt-deposito 
          fi-ini-dt-emissao fi-fin-dt-emissao fi-ini-tp-codigo fi-fin-tp-codigo 
          fi-ini-cod-emitente fi-fin-cod-emitente fi-ini-nom-emit-cheque 
          fi-fin-nom-emit-cheque 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  ENABLE fi-ep-codigo fi-ini-cod-estabel fi-fin-cod-estabel fi-ini-cod-banco 
         fi-fin-cod-banco fi-ini-dt-vencimento fi-fin-dt-vencimento 
         fi-ini-dt-deposito fi-fin-dt-deposito fi-ini-dt-emissao 
         fi-fin-dt-emissao fi-ini-tp-codigo fi-fin-tp-codigo 
         fi-ini-cod-emitente fi-fin-cod-emitente fi-ini-nom-emit-cheque 
         fi-fin-nom-emit-cheque IMAGE-1 IMAGE-2 IMAGE-57 IMAGE-58 IMAGE-59 
         IMAGE-60 IMAGE-61 IMAGE-62 IMAGE-67 IMAGE-68 IMAGE-69 IMAGE-70 
         IMAGE-71 IMAGE-72 IMAGE-73 IMAGE-74 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}
  DISPLAY rs-tipo-cheque tg-sit-chq-pend tg-sit-chq-subst tg-sit-chq-desc 
          tg-sit-chq-depos tg-sit-chq-devolv tg-sit-chq-comp tg-sit-chq-caucao 
          tg-sit-chq-canc tg-sit-chq-pgemp rs-tipo-rel tg-impr-param 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  ENABLE rs-tipo-cheque tg-sit-chq-pend tg-sit-chq-subst tg-sit-chq-desc 
         tg-sit-chq-depos tg-sit-chq-devolv tg-sit-chq-comp tg-sit-chq-caucao 
         tg-sit-chq-canc tg-sit-chq-pgemp rs-tipo-rel tg-impr-param RECT-35 
         RECT-36 RECT-37 
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
    
    ASSIGN tt-param.ep-codigo            = INPUT FRAME f-pg-sel fi-ep-codigo
           tt-param.cod-estabel-ini      = INPUT FRAME f-pg-sel fi-ini-cod-estabel
           tt-param.cod-estabel-fin      = INPUT FRAME f-pg-sel fi-fin-cod-estabel
           tt-param.cod-banco-ini        = INPUT FRAME f-pg-sel fi-ini-cod-banco
           tt-param.cod-banco-fin        = INPUT FRAME f-pg-sel fi-fin-cod-banco           
           tt-param.dt-vencimento-ini    = INPUT FRAME f-pg-sel fi-ini-dt-vencimento
           tt-param.dt-vencimento-fin    = INPUT FRAME f-pg-sel fi-fin-dt-vencimento
           tt-param.dt-deposito-ini      = INPUT FRAME f-pg-sel fi-ini-dt-deposito
           tt-param.dt-deposito-fin      = INPUT FRAME f-pg-sel fi-fin-dt-deposito
           tt-param.dt-emissao-ini       = INPUT FRAME f-pg-sel fi-ini-dt-emissao
           tt-param.dt-emissao-fin       = INPUT FRAME f-pg-sel fi-fin-dt-emissao
           tt-param.tp-codigo-ini        = INPUT FRAME f-pg-sel fi-ini-tp-codigo
           tt-param.tp-codigo-fin        = INPUT FRAME f-pg-sel fi-fin-tp-codigo
           tt-param.cod-emitente-ini     = INPUT FRAME f-pg-sel fi-ini-cod-emitente
           tt-param.cod-emitente-fin     = INPUT FRAME f-pg-sel fi-fin-cod-emitente
           tt-param.nom-emit-cheque-ini  = INPUT FRAME f-pg-sel fi-ini-nom-emit-cheque
           tt-param.nom-emit-cheque-fin  = INPUT FRAME f-pg-sel fi-fin-nom-emit-cheque
           tt-param.tipo-cheque          = INPUT FRAME f-pg-par rs-tipo-cheque                          
           tt-param.desc-tipo-cheque     = entry((tt-param.tipo-cheque - 1) * 2 + 1, 
                                              rs-tipo-cheque:radio-buttons in frame f-pg-par)
           tt-param.sit-chq-pend         = INPUT FRAME f-pg-par tg-sit-chq-pend  
           tt-param.sit-chq-depos        = INPUT FRAME f-pg-par tg-sit-chq-depos 
           tt-param.sit-chq-caucao       = INPUT FRAME f-pg-par tg-sit-chq-caucao
           tt-param.sit-chq-subst        = INPUT FRAME f-pg-par tg-sit-chq-subst 
           tt-param.sit-chq-devolv       = INPUT FRAME f-pg-par tg-sit-chq-devolv
           tt-param.sit-chq-canc         = INPUT FRAME f-pg-par tg-sit-chq-canc  
           tt-param.sit-chq-desc         = INPUT FRAME f-pg-par tg-sit-chq-desc  
           tt-param.sit-chq-comp         = INPUT FRAME f-pg-par tg-sit-chq-comp  
           tt-param.sit-chq-pgemp        = INPUT FRAME f-pg-par tg-sit-chq-pgemp 
           tt-param.tipo-rel             = INPUT FRAME f-pg-par rs-tipo-rel                          
           tt-param.desc-tipo-rel        = entry((tt-param.tipo-rel - 1) * 2 + 1, 
                                              rs-tipo-rel:radio-buttons in frame f-pg-par)
           tt-param.impr-param           = INPUT FRAME f-pg-par tg-impr-param.
           
    /*:T Executar do programa RP.P que ir† criar o relat¢rio */
    {include/i-rpexb.i}
    
    SESSION:SET-WAIT-STATE("general":U).
    
    {include/i-rprun.i esrp/escb0001rp.p} 
    
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
  Purpose: Gerencia a Troca de P†gina (folder)   
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

