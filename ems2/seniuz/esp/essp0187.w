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
{include/i-prgvrs.i XX9999 9.99.99.999}

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
&GLOBAL-DEFINE PGPAR 
&GLOBAL-DEFINE PGDIG f-pg-dig
&GLOBAL-DEFINE PGIMP f-pg-imp

&GLOBAL-DEFINE RTF   YES
  
/* Parameters Definitions ---                                           */

/* Temporary Table Definitions ---                                      */

DEFINE TEMP-TABLE tt-param NO-UNDO
    FIELD destino          AS INTEGER
    FIELD arquivo          AS CHAR FORMAT "x(35)"
    FIELD usuario          AS CHAR FORMAT "x(12)"
    FIELD data-exec        AS DATE
    FIELD hora-exec        AS INTEGER
    FIELD classifica       AS INTEGER
    FIELD desc-classifica  AS CHAR FORMAT "x(40)"
    FIELD modelo-rtf       AS CHAR FORMAT "x(35)"
    FIELD cod-estabel      AS CHAR
    FIELD l-habilitaRtf    AS LOG
    FIELD l-retorna-sem    AS LOG
    FIELD l-espelho        AS LOG
    FIELD nome-abrev       LIKE emitente.nome-abrev
    FIELD l-etiquetas      AS LOG
    FIELD it-codigo        AS CHAR
    FIELD cod-refer        AS CHAR
    FIELD lote             AS CHAR
    FIELD localiz          AS CHAR
    FIELD l-retorna-com    AS LOG
    FIELD l-altera-qtd     AS LOG
    FIELD l-consumo        AS LOG
    FIELD i-acerto         AS INT
    FIELD justificativa    AS CHAR FORMAT "x(100)".

define temp-table tt-digita no-undo
    FIELD c-localiz        AS CHAR
    FIELD num-etiqueta     LIKE ob-etiqueta.num-etiqueta
    FIELD quantidade       LIKE ob-etiqueta.quantidade.

define buffer b-tt-digita for tt-digita.

/* Transfer Definitions */

def var raw-param        as raw no-undo.

def temp-table tt-raw-digita
   field raw-digita      as raw.
                    
/* Local Variable Definitions ---                                       */

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.
def var l-ok               as logical no-undo.
def var c-arq-digita       as char    no-undo.
def var c-terminal         as char    no-undo.
def var c-rtf              as char    no-undo.
def var c-modelo-default   as char    no-undo.
DEF VAR c-cod-estabel      AS CHAR.

DEF VAR l-error            AS LOG.
DEF VAR de-qtd-atu         AS DEC.

DEF VAR c-situacao AS CHAR.
DEF VAR c-localizacao AS CHAR.
DEF VAR i-sit-ini AS INT.
DEF VAR i-sit-fin AS INT.
DEF VAR c-loc-ini LIKE ob-etiqueta.localizacao.
DEF VAR c-loc-fin LIKE ob-etiqueta.localizacao.

/*15/02/2005 - tech1007 - Variavel definida para tratar se o programa est† rodando no WebEnabler*/
DEFINE SHARED VARIABLE hWenController AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-relat
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-pg-dig
&Scoped-define BROWSE-NAME br-digita

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-digita

/* Definitions for BROWSE br-digita                                     */
&Scoped-define FIELDS-IN-QUERY-br-digita tt-digita.num-etiqueta fn-localiz() @ c-localizacao fn-qtd-etiqueta() @ de-qtd-atu tt-digita.quantidade fn-situacao() @ c-situacao   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-digita tt-digita.num-etiqueta tt-digita.quantidade   
&Scoped-define ENABLED-TABLES-IN-QUERY-br-digita tt-digita
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br-digita tt-digita
&Scoped-define SELF-NAME br-digita
&Scoped-define QUERY-STRING-br-digita FOR EACH tt-digita BY tt-digita.num-etiqueta
&Scoped-define OPEN-QUERY-br-digita OPEN QUERY br-digita FOR EACH tt-digita BY tt-digita.num-etiqueta.
&Scoped-define TABLES-IN-QUERY-br-digita tt-digita
&Scoped-define FIRST-TABLE-IN-QUERY-br-digita tt-digita


/* Definitions for FRAME f-pg-dig                                       */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-pg-dig ~
    ~{&OPEN-QUERY-br-digita}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-digita bt-inserir bt-recuperar 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-localiz w-relat 
FUNCTION fn-localiz RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-qtd-etiqueta w-relat 
FUNCTION fn-qtd-etiqueta RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-situacao w-relat 
FUNCTION fn-situacao RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-relat AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-alterar 
     LABEL "Alterar" 
     SIZE 15 BY 1.

DEFINE BUTTON bt-inserir 
     LABEL "Inserir" 
     SIZE 15 BY 1.

DEFINE BUTTON bt-recuperar 
     LABEL "Recuperar" 
     SIZE 15 BY 1.

DEFINE BUTTON bt-retirar 
     LABEL "Retirar" 
     SIZE 15 BY 1.

DEFINE BUTTON bt-salvar 
     LABEL "Salvar" 
     SIZE 15 BY 1.

DEFINE BUTTON bt-arquivo 
     IMAGE-UP FILE "image\im-sea":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE BUTTON bt-config-impr 
     IMAGE-UP FILE "image\im-cfprt":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE BUTTON bt-modelo-rtf 
     IMAGE-UP FILE "image\im-sea":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE VARIABLE c-arquivo AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 40 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE c-modelo-rtf AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 40 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE text-destino AS CHARACTER FORMAT "X(256)":U INITIAL " Destino" 
      VIEW-AS TEXT 
     SIZE 8.57 BY .63 NO-UNDO.

DEFINE VARIABLE text-modelo-rtf AS CHARACTER FORMAT "X(256)":U INITIAL "Modelo:" 
      VIEW-AS TEXT 
     SIZE 10.86 BY .63 NO-UNDO.

DEFINE VARIABLE text-modo AS CHARACTER FORMAT "X(256)":U INITIAL "Execuá∆o" 
      VIEW-AS TEXT 
     SIZE 10.86 BY .63 NO-UNDO.

DEFINE VARIABLE text-rtf AS CHARACTER FORMAT "X(256)":U INITIAL "Rich Text Format(RTF)" 
      VIEW-AS TEXT 
     SIZE 20.86 BY .63 NO-UNDO.

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
     SIZE 46.29 BY 2.79.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 1.71.

DEFINE RECTANGLE rect-rtf
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 3.54.

DEFINE VARIABLE l-habilitaRtf AS LOGICAL INITIAL no 
     LABEL "RTF" 
     VIEW-AS TOGGLE-BOX
     SIZE 44 BY 1.08 NO-UNDO.

DEFINE BUTTON bt-act 
     IMAGE-UP FILE "image/im-chck1.bmp":U
     LABEL "Button 1" 
     SIZE 3.29 BY 1.

DEFINE VARIABLE cb-acerto AS INTEGER FORMAT "9":U INITIAL 2 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Retorna Etiq. para Estoque",1,
                     "Retirar Etiquetas do Estoque",2
     DROP-DOWN-LIST
     SIZE 23.29 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cod-estabel AS CHARACTER FORMAT "x(3)" 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-refer AS CHARACTER FORMAT "X(256)":U 
     LABEL "Nova Referància" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE fi-doca AS CHARACTER FORMAT "X(256)":U 
     LABEL "Nova Doca" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 NO-UNDO.

DEFINE VARIABLE fi-emitente AS CHARACTER FORMAT "X(15)":U 
     LABEL "Cliente" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-it-codigo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Novo Item" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE fi-it-codigo-act AS CHARACTER FORMAT "X(256)":U 
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 12.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-justificativa AS CHARACTER FORMAT "X(256)":U 
     LABEL "Justificativa" 
     VIEW-AS FILL-IN 
     SIZE 53 BY .88 NO-UNDO.

DEFINE VARIABLE fi-lote AS CHARACTER FORMAT "X(2)":U 
     LABEL "Novo Lote" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nome-emit AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 24.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nome-estabel AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 29.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi-refer-act-fin AS CHARACTER FORMAT "X(256)":U INITIAL "999" 
     VIEW-AS FILL-IN 
     SIZE 4.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-refer-act-ini AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ref." 
     VIEW-AS FILL-IN 
     SIZE 4.86 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 2.29 BY .88.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 74 BY 4.88.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46 BY 3.42.

DEFINE RECTANGLE RECT-55
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 27 BY 5.

DEFINE VARIABLE tg-acertos AS LOGICAL INITIAL no 
     LABEL "Acertos SEM Movto Estoque" 
     VIEW-AS TOGGLE-BOX
     SIZE 22.86 BY .63 NO-UNDO.

DEFINE VARIABLE tg-altera-qtd AS LOGICAL INITIAL no 
     LABEL "Alterar Quantidade da Etiqueta" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY .83 NO-UNDO.

DEFINE VARIABLE tg-consumo AS LOGICAL INITIAL no 
     LABEL "Retirar Etiquetas do Estoque (Consumo)" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .83 NO-UNDO.

DEFINE VARIABLE tg-devolucao AS LOGICAL INITIAL no 
     LABEL "Devoluá‰es" 
     VIEW-AS TOGGLE-BOX
     SIZE 12 BY .58 NO-UNDO.

DEFINE VARIABLE tg-espelho-nf AS LOGICAL INITIAL no 
     LABEL "Gera Espelho para Nota Fiscal" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .83 NO-UNDO.

DEFINE VARIABLE tg-etiquetas AS LOGICAL INITIAL no 
     LABEL "Altera Caracter°stica das Etiquetas" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY .83 NO-UNDO.

DEFINE VARIABLE tg-retorna-com AS LOGICAL INITIAL no 
     LABEL "Retorna Etiquetas para Estoque (COM Movimentos)" 
     VIEW-AS TOGGLE-BOX
     SIZE 37.86 BY .83 TOOLTIP "Altera a Situaá∆o e CRIA Movimento de Estoque" NO-UNDO.

DEFINE VARIABLE tg-retorna-sem AS LOGICAL INITIAL no 
     LABEL "Retorna Etiquetas para Estoque (SEM Movimentos)" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .83 TOOLTIP "Altera apenas a Situaá∆o, N«O Cria Movimento de Estoque" NO-UNDO.

DEFINE VARIABLE tg-sem-loc AS LOGICAL INITIAL yes 
     LABEL "Etiquetas sem Localizaá∆o" 
     VIEW-AS TOGGLE-BOX
     SIZE 23.43 BY .83 NO-UNDO.

DEFINE BUTTON bt-ajuda 
     LABEL "Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "Fechar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-executar 
     LABEL "Executar" 
     SIZE 10 BY 1.

DEFINE IMAGE im-pg-dig
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.21.

DEFINE IMAGE im-pg-imp
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

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-digita FOR 
      tt-digita SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-digita w-relat _FREEFORM
  QUERY br-digita DISPLAY
      tt-digita.num-etiqueta
      fn-localiz() @ c-localizacao         COLUMN-LABEL "Localizaá∆o" 
      fn-qtd-etiqueta() @ de-qtd-atu       COLUMN-LABEL "Qtde ATUAL" 
      tt-digita.quantidade                 COLUMN-LABEL "NOVA Qtde"
      fn-situacao() @ c-situacao           COLUMN-LABEL "Situaá∆o"  FORMAT "x(15)" 
ENABLE
tt-digita.num-etiqueta
tt-digita.quantidade
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 76.57 BY 9
         BGCOLOR 15 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-relat
     bt-executar AT ROW 14.54 COL 3 HELP
          "Dispara a execuá∆o do relat¢rio"
     bt-cancelar AT ROW 14.54 COL 14 HELP
          "Fechar"
     bt-ajuda AT ROW 14.54 COL 70 HELP
          "Ajuda"
     rt-folder-right AT ROW 2.67 COL 80.43
     rt-folder AT ROW 2.5 COL 2
     rt-folder-left AT ROW 2.54 COL 2.14
     RECT-1 AT ROW 14.29 COL 2
     RECT-6 AT ROW 13.75 COL 2.14
     rt-folder-top AT ROW 2.54 COL 2.14
     im-pg-dig AT ROW 1.5 COL 17.72
     im-pg-imp AT ROW 1.5 COL 33.43
     im-pg-sel AT ROW 1.5 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81 BY 15
         DEFAULT-BUTTON bt-executar.

DEFINE FRAME f-pg-imp
     rs-destino AT ROW 1.63 COL 3.29 HELP
          "Destino de Impress∆o do Relat¢rio" NO-LABEL
     bt-config-impr AT ROW 2.71 COL 43.29 HELP
          "Configuraá∆o da impressora"
     bt-arquivo AT ROW 2.71 COL 43.29 HELP
          "Escolha do nome do arquivo"
     c-arquivo AT ROW 2.75 COL 3.29 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL
     l-habilitaRtf AT ROW 4.83 COL 3.29
     c-modelo-rtf AT ROW 6.63 COL 3 HELP
          "Nome do arquivo de modelo do relat¢rio" NO-LABEL
     bt-modelo-rtf AT ROW 6.63 COL 43 HELP
          "Escolha do nome do arquivo"
     rs-execucao AT ROW 8.88 COL 2.86 HELP
          "Modo de Execuá∆o" NO-LABEL
     text-destino AT ROW 1.04 COL 3.86 NO-LABEL
     text-rtf AT ROW 4.17 COL 1.14 COLON-ALIGNED NO-LABEL
     text-modelo-rtf AT ROW 5.96 COL 1.14 COLON-ALIGNED NO-LABEL
     text-modo AT ROW 8.13 COL 1.14 COLON-ALIGNED NO-LABEL
     RECT-7 AT ROW 1.33 COL 2.14
     RECT-9 AT ROW 8.33 COL 2
     rect-rtf AT ROW 4.46 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 73.72 BY 10.5.

DEFINE FRAME f-pg-dig
     br-digita AT ROW 1 COL 1
     bt-inserir AT ROW 10 COL 1
     bt-alterar AT ROW 10 COL 16
     bt-retirar AT ROW 10 COL 31
     bt-salvar AT ROW 10 COL 46
     bt-recuperar AT ROW 10 COL 61
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3.13
         SIZE 76.86 BY 10.33.

DEFINE FRAME f-pg-sel
     fi-cod-estabel AT ROW 1.21 COL 13 COLON-ALIGNED WIDGET-ID 2
     fi-nome-estabel AT ROW 1.21 COL 17.29 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     tg-devolucao AT ROW 2.5 COL 5.29
     fi-emitente AT ROW 3.13 COL 7.86 COLON-ALIGNED
     fi-nome-emit AT ROW 3.17 COL 20.29 COLON-ALIGNED NO-LABEL WIDGET-ID 20
     tg-espelho-nf AT ROW 4.04 COL 9.86
     tg-retorna-sem AT ROW 4.88 COL 9.86 HELP
          "Altera apenas a Situaá∆o, N«O Cria Movimento de Estoque"
     tg-acertos AT ROW 1 COL 51.14 WIDGET-ID 10
     cb-acerto AT ROW 1.96 COL 50.57 COLON-ALIGNED NO-LABEL WIDGET-ID 28
     tg-sem-loc AT ROW 3.08 COL 52.57 WIDGET-ID 30
     fi-it-codigo-act AT ROW 4.17 COL 54.29 COLON-ALIGNED WIDGET-ID 24
     fi-refer-act-ini AT ROW 5.17 COL 54.29 COLON-ALIGNED WIDGET-ID 22
     fi-refer-act-fin AT ROW 5.17 COL 65.57 COLON-ALIGNED NO-LABEL WIDGET-ID 32
     bt-act AT ROW 5.13 COL 73 WIDGET-ID 26
     tg-etiquetas AT ROW 6.33 COL 5.43
     fi-it-codigo AT ROW 7.25 COL 17 COLON-ALIGNED
     fi-cod-refer AT ROW 8.25 COL 17 COLON-ALIGNED
     fi-lote AT ROW 9.25 COL 17 COLON-ALIGNED WIDGET-ID 6
     fi-doca AT ROW 9.25 COL 35 COLON-ALIGNED
     tg-retorna-com AT ROW 6.83 COL 37 HELP
          "Altera a Situaá∆o e CRIA Movimento de Estoque"
     tg-altera-qtd AT ROW 7.63 COL 37
     tg-consumo AT ROW 8.38 COL 37
     fi-justificativa AT ROW 10.5 COL 17 COLON-ALIGNED
     RECT-10 AT ROW 6.75 COL 3
     RECT-11 AT ROW 2.83 COL 3
     RECT-55 AT ROW 1.25 COL 50 WIDGET-ID 8
     IMAGE-1 AT ROW 5.21 COL 61.86 WIDGET-ID 34
     IMAGE-2 AT ROW 5.21 COL 64.57 WIDGET-ID 36
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.83
         SIZE 76.86 BY 10.71
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: w-relat
   Allow: Basic,Browse,DB-Fields,Window,Query
   Add Fields to: Neither
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-relat ASSIGN
         HIDDEN             = YES
         TITLE              = "Altera Caracter°stica das Etiquetas - ESSP0187"
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
/* SETTINGS FOR FRAME f-pg-dig
   FRAME-NAME                                                           */
/* BROWSE-TAB br-digita 1 f-pg-dig */
/* SETTINGS FOR BUTTON bt-alterar IN FRAME f-pg-dig
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-retirar IN FRAME f-pg-dig
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-salvar IN FRAME f-pg-dig
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME f-pg-imp
                                                                        */
/* SETTINGS FOR EDITOR c-modelo-rtf IN FRAME f-pg-imp
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN text-destino IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       text-destino:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Destino".

ASSIGN 
       text-modelo-rtf:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Modelo:".

/* SETTINGS FOR FILL-IN text-modo IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       text-modo:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Execuá∆o".

ASSIGN 
       text-rtf:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Rich Text Format(RTF)".

/* SETTINGS FOR FRAME f-pg-sel
   Custom                                                               */
/* SETTINGS FOR BUTTON bt-act IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX cb-acerto IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-cod-refer IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-doca IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-emitente IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-it-codigo IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-it-codigo-act IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-justificativa IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-lote IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nome-emit IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nome-estabel IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-refer-act-fin IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-refer-act-ini IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-altera-qtd IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-consumo IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-espelho-nf IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-retorna-com IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-retorna-sem IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-sem-loc IN FRAME f-pg-sel
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

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-digita
/* Query rebuild information for BROWSE br-digita
     _START_FREEFORM
OPEN QUERY br-digita FOR EACH tt-digita BY tt-digita.num-etiqueta.
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
ON END-ERROR OF w-relat /* Altera Caracter°stica das Etiquetas - ESSP0187 */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON WINDOW-CLOSE OF w-relat /* Altera Caracter°stica das Etiquetas - ESSP0187 */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-digita
&Scoped-define SELF-NAME br-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON DEL OF br-digita IN FRAME f-pg-dig
DO:
   apply 'choose':U to bt-retirar in frame f-pg-dig.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON END-ERROR OF br-digita IN FRAME f-pg-dig
ANYWHERE 
DO:
    if  br-digita:new-row in frame f-pg-dig then do:
        if  avail tt-digita then
            delete tt-digita.
        if  br-digita:delete-current-row() in frame f-pg-dig then. 
    end.                                                               
    else do:
        get current br-digita.
        DISPLAY tt-digita.num-etiqueta with browse br-digita. 
    end.
    return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON ENTER OF br-digita IN FRAME f-pg-dig
ANYWHERE
DO:
  apply 'tab':U to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON INS OF br-digita IN FRAME f-pg-dig
DO:
   apply 'choose':U to bt-inserir in frame f-pg-dig.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON OFF-END OF br-digita IN FRAME f-pg-dig
DO:
   apply 'entry':U to bt-inserir in frame f-pg-dig.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON OFF-HOME OF br-digita IN FRAME f-pg-dig
DO:
  apply 'entry':U to bt-recuperar in frame f-pg-dig.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON ROW-LEAVE OF br-digita IN FRAME f-pg-dig
DO:
    /*:T ê aqui que a gravaá∆o da linha da temp-table Ç efetivada.
       PorÇm as validaá‰es dos registros devem ser feitas na procedure pi-executar,
       no local indicado pelo coment†rio */
    
    if br-digita:NEW-ROW in frame f-pg-dig then 
    do transaction on error undo, return no-apply:
        create tt-digita.
        assign input browse br-digita tt-digita.num-etiqueta.
    
        br-digita:CREATE-RESULT-LIST-ENTRY() in frame f-pg-dig.
    end.
    else do transaction on error undo, return no-apply:
        assign input browse br-digita tt-digita.num-etiqueta.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-sel
&Scoped-define SELF-NAME bt-act
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-act w-relat
ON CHOOSE OF bt-act IN FRAME f-pg-sel /* Button 1 */
DO:
   SESSION:SET-WAIT-STATE("general":U).
    

   ASSIGN i-sit-ini = 1
          i-sit-fin = 9
          c-loc-ini = ""
          c-loc-fin = "ZZZZZZZ".

   IF tg-sem-loc:SCREEN-VALUE = 'YES' THEN
      ASSIGN i-sit-ini = 3
             i-sit-fin = 3
             c-loc-ini = ""
             c-loc-fin = "ZZZZZZ".

   EMPTY TEMP-TABLE tt-digita.
   FOR EACH ob-etiqueta WHERE
            ob-etiqueta.cod-estabel  = fi-cod-estabel:SCREEN-VALUE AND
            ob-etiqueta.situacao    >= i-sit-ini AND
            ob-etiqueta.situacao    <= i-sit-fin AND
            ob-etiqueta.it-codigo    = fi-it-codigo-act:SCREEN-VALUE AND
            ob-etiqueta.cod-refer   >= fi-refer-act-ini:SCREEN-VALUE AND
            ob-etiqueta.cod-refer   <= fi-refer-act-fin:SCREEN-VALUE AND
            ob-etiqueta.localizacao >= c-loc-ini AND
            ob-etiqueta.localizacao <= c-loc-fin 
            NO-LOCK.

       CREATE tt-digita.
       ASSIGN tt-digita.num-etiqueta = ob-etiqueta.num-etiqueta
              tt-digita.c-localiz = ""
              tt-digita.quantidade = ob-etiqueta.quantidade.
   END.

   {&OPEN-QUERY-br-digita}

   SESSION:SET-WAIT-STATE("":U).

   APPLY 'MOUSE-SELECT-CLICK' TO im-pg-dig IN FRAME f-relat.

   ASSIGN bt-alterar:SENSITIVE IN FRAME f-pg-dig = YES
          bt-retirar:SENSITIVE IN FRAME f-pg-dig = YES
          bt-salvar:SENSITIVE IN  FRAME f-pg-dig  = YES.

   APPLY "ENTRY":U TO bt-inserir IN FRAME f-pg-dig.
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


&Scoped-define FRAME-NAME f-pg-dig
&Scoped-define SELF-NAME bt-alterar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-alterar w-relat
ON CHOOSE OF bt-alterar IN FRAME f-pg-dig /* Alterar */
DO:
    ASSIGN tt-digita.quantidade:READ-ONLY IN BROWSE br-digita = YES.
    IF INPUT FRAME f-pg-sel tg-altera-qtd = YES THEN
       ASSIGN tt-digita.quantidade:READ-ONLY IN BROWSE br-digita = NO.
    
   apply 'entry':U to tt-digita.num-etiqueta in browse br-digita. 
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


&Scoped-define FRAME-NAME f-pg-dig
&Scoped-define SELF-NAME bt-inserir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-inserir w-relat
ON CHOOSE OF bt-inserir IN FRAME f-pg-dig /* Inserir */
DO:
    assign bt-alterar:SENSITIVE in frame f-pg-dig = yes
           bt-retirar:SENSITIVE in frame f-pg-dig = yes
           bt-salvar:SENSITIVE in frame f-pg-dig  = yes.
    
    
    ASSIGN tt-digita.quantidade:READ-ONLY IN BROWSE br-digita = YES.
    IF INPUT FRAME f-pg-sel tg-altera-qtd = YES THEN
       ASSIGN tt-digita.quantidade:READ-ONLY IN BROWSE br-digita = NO.

    if num-results("br-digita":U) > 0 THEN DO.
        br-digita:REFRESH().
        br-digita:INSERT-ROW("after":U) in frame f-pg-dig.
    END.
    else do transaction:
        create tt-digita.
        open query br-digita for each tt-digita.
        apply "entry":U to tt-digita.num-etiqueta in browse br-digita. 
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-imp
&Scoped-define SELF-NAME bt-modelo-rtf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-modelo-rtf w-relat
ON CHOOSE OF bt-modelo-rtf IN FRAME f-pg-imp
DO:
    def var c-arq-conv  as char no-undo.
    def var l-ok as logical no-undo.

    assign c-modelo-rtf = replace(input frame {&frame-name} c-modelo-rtf, "/", "\").
    SYSTEM-DIALOG GET-FILE c-arq-conv
       FILTERS "*.rtf" "*.rtf",
               "*.*" "*.*"
       DEFAULT-EXTENSION "rtf"
       INITIAL-DIR "modelos" 
       MUST-EXIST
       USE-FILENAME
       UPDATE l-ok.
    if  l-ok = yes then
        assign c-modelo-rtf:screen-value in frame {&frame-name}  = replace(c-arq-conv, "\", "/"). 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-dig
&Scoped-define SELF-NAME bt-recuperar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-recuperar w-relat
ON CHOOSE OF bt-recuperar IN FRAME f-pg-dig /* Recuperar */
DO:
    {include/i-rprcd.i}

    FOR EACH tt-digita.
        IF LENGTH(tt-digita.c-localiz) <> 9 THEN
           ASSIGN tt-digita.num-etiqueta = INT(SUBSTR(tt-digita.c-localiz,7,9)).
        ELSE
           ASSIGN tt-digita.num-etiqueta = INT(tt-digita.c-localiz).
    END.
    {&OPEN-QUERY-br-digita}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-retirar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-retirar w-relat
ON CHOOSE OF bt-retirar IN FRAME f-pg-dig /* Retirar */
DO:
    if  br-digita:num-selected-rows > 0 then do on error undo, return no-apply:
        get current br-digita.
        delete tt-digita.
        if  br-digita:delete-current-row() in frame f-pg-dig then.
    end.
    
    if num-results("br-digita":U) = 0 then
        assign bt-alterar:SENSITIVE in frame f-pg-dig = no
               bt-retirar:SENSITIVE in frame f-pg-dig = no
               bt-salvar:SENSITIVE in frame f-pg-dig  = no.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-salvar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-salvar w-relat
ON CHOOSE OF bt-salvar IN FRAME f-pg-dig /* Salvar */
DO:
   FOR EACH tt-digita.
       ASSIGN tt-digita.c-localiz = string(tt-digita.num-etiqueta,"999999999").
   END.
   {include/i-rpsvd.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-sel
&Scoped-define SELF-NAME cb-acerto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-acerto w-relat
ON VALUE-CHANGED OF cb-acerto IN FRAME f-pg-sel
DO:
   ASSIGN tg-sem-loc:SCREEN-VALUE = 'no'.
   IF SELF:INPUT-VALUE = 2 THEN
      ASSIGN tg-sem-loc:SCREEN-VALUE = 'yes'.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-estabel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-estabel w-relat
ON LEAVE OF fi-cod-estabel IN FRAME f-pg-sel /* Estabelecimento */
DO:

  IF SELF:SCREEN-VALUE <> '' THEN DO.
      FIND estabelec WHERE
           estabelec.cod-estabel = INPUT FRAME {&FRAME-NAME} fi-cod-estabel 
           NO-LOCK NO-ERROR.
      IF NOT AVAIL estabelec THEN DO.
         MESSAGE 'Estabelecimento n∆o Cadastrado....'
              VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY 'ENTRY' TO SELF.
         RETURN NO-APPLY.
      END.
      ASSIGN fi-nome-estabel:SCREEN-VALUE IN FRAME f-pg-sel = TRIM(estabelec.cidade) + " / " + TRIM(estabelec.bairro).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-estabel w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-cod-estabel IN FRAME f-pg-sel /* Estabelecimento */
DO:
    {include/zoomvar.i &prog-zoom=adzoom\z01ad107.w
                       &campo=fi-cod-estabel
                       &campozoom=cod-estabel}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-emitente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-emitente w-relat
ON LEAVE OF fi-emitente IN FRAME f-pg-sel /* Cliente */
DO:
   IF SELF:SCREEN-VALUE <> "" THEN DO.
      FIND emitente WHERE
           emitente.nome-abrev = SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
      IF NOT AVAIL emitente THEN
         FIND emitente WHERE
              emitente.cod-emit = INT(SELF:SCREEN-VALUE) NO-LOCK NO-ERROR.
    
      IF NOT AVAIL emitente THEN DO.
         MESSAGE 'Cliente n∆o Cadastrado...'
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
         APPLY 'ENTRY' TO SELF.
         RETURN NO-APPLY.
      END.
      ASSIGN SELF:SCREEN-VALUE IN FRAME {&FRAME-NAME} = emitente.nome-abrev
             fi-nome-emit:SCREEN-VALUE IN FRAME {&FRAME-NAME} = emitente.nome-emit.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-emitente w-relat
ON TAB OF fi-emitente IN FRAME f-pg-sel /* Cliente */
DO:
   APPLY 'LEAVE' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-refer-act-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-refer-act-ini w-relat
ON LEAVE OF fi-refer-act-ini IN FRAME f-pg-sel /* Ref. */
DO:
   IF SELF:SCREEN-VALUE <> '' THEN
      ASSIGN fi-refer-act-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
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


&Scoped-define SELF-NAME im-pg-sel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-sel w-relat
ON MOUSE-SELECT-CLICK OF im-pg-sel IN FRAME f-relat
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-imp
&Scoped-define SELF-NAME l-habilitaRtf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL l-habilitaRtf w-relat
ON VALUE-CHANGED OF l-habilitaRtf IN FRAME f-pg-imp /* RTF */
DO:
    &IF "{&RTF}":U = "YES":U &THEN
    RUN pi-habilitaRtf.
    &endif
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-destino
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-destino w-relat
ON VALUE-CHANGED OF rs-destino IN FRAME f-pg-imp
DO:
/*Alterado 15/02/2005 - tech1007 - Evento alterado para correto funcionamento dos novos widgets
  utilizados para a funcionalidade de RTF*/
do  with frame f-pg-imp:
    case self:screen-value:
        when "1" then do:
            assign c-arquivo:sensitive    = no
                   bt-arquivo:visible     = no
                   bt-config-impr:visible = YES
                   /*Alterado 17/02/2005 - tech1007 - Realizado teste de preprocessador para
                     verificar se o RTF est† ativo*/
                   &IF "{&RTF}":U = "YES":U &THEN
                   l-habilitaRtf:sensitive  = NO
                   l-habilitaRtf:SCREEN-VALUE IN FRAME f-pg-imp = "No"
                   l-habilitaRtf = NO
                   &endif
                   /*Fim alteracao 17/02/2005*/
                   .
        end.
        when "2" then do:
            assign c-arquivo:sensitive     = yes
                   bt-arquivo:visible      = yes
                   bt-config-impr:visible  = NO
                   /*Alterado 17/02/2005 - tech1007 - Realizado teste de preprocessador para
                     verificar se o RTF est† ativo*/
                   &IF "{&RTF}":U = "YES":U &THEN
                   l-habilitaRtf:sensitive  = YES
                   &endif
                   /*Fim alteracao 17/02/2005*/
                   .
        end.
        when "3" then do:
            assign c-arquivo:sensitive     = no
                   bt-arquivo:visible      = no
                   bt-config-impr:visible  = no
                   /*Alterado 17/02/2005 - tech1007 - Realizado teste de preprocessador para
                     verificar se o RTF est† ativo*/
                   &IF "{&RTF}":U = "YES":U &THEN
                   l-habilitaRtf:sensitive  = YES
                   &endif
                   /*Fim alteracao 17/02/2005*/
                   .
            /*Alterado 15/02/2005 - tech1007 - Teste para funcionar corretamente no WebEnabler*/
            &IF "{&RTF}":U = "YES":U &THEN
            IF VALID-HANDLE(hWenController) THEN DO:
                ASSIGN l-habilitaRtf:sensitive  = NO
                       l-habilitaRtf:SCREEN-VALUE IN FRAME f-pg-imp = "No"
                       l-habilitaRtf = NO.
            END.
            &endif
            /*Fim alteracao 15/02/2005*/
        end.
    end case.
end.
&IF "{&RTF}":U = "YES":U &THEN
RUN pi-habilitaRtf.
&endif
/*Fim alteracao 15/02/2005*/
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
&Scoped-define SELF-NAME tg-acertos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-acertos w-relat
ON VALUE-CHANGED OF tg-acertos IN FRAME f-pg-sel /* Acertos SEM Movto Estoque */
DO:
  IF SELF:INPUT-VALUE = 'YES' THEN 
     ASSIGN cb-acerto:SCREEN-VALUE IN FRAME f-pg-sel = '2'
            tg-sem-loc:SCREEN-VALUE IN FRAME f-pg-sel = 'yes'.

  ASSIGN cb-acerto:SENSITIVE IN FRAME f-pg-sel = SELF:INPUT-VALUE
         tg-sem-loc:SENSITIVE IN FRAME f-pg-sel = SELF:INPUT-VALUE
         fi-it-codigo-act:SENSITIVE IN FRAME f-pg-sel = SELF:INPUT-VALUE
         fi-refer-act-ini:SENSITIVE IN FRAME f-pg-sel = SELF:INPUT-VALUE
         fi-refer-act-fin:SENSITIVE IN FRAME f-pg-sel = SELF:INPUT-VALUE
         bt-act:SENSITIVE IN FRAME f-pg-sel = SELF:INPUT-VALUE.

  APPLY 'entry' TO fi-it-codigo-act.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-devolucao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-devolucao w-relat
ON VALUE-CHANGED OF tg-devolucao IN FRAME f-pg-sel /* Devoluá‰es */
DO:
   IF SELF:INPUT-VALUE = 'YES' THEN DO.
      ASSIGN tg-etiquetas:SCREEN-VALUE IN FRAME f-pg-sel = 'NO'
             fi-it-codigo:SCREEN-VALUE IN FRAME f-pg-sel = ""
             fi-cod-refer:SCREEN-VALUE IN FRAME f-pg-sel = ""
             fi-doca:SCREEN-VALUE IN FRAME f-pg-sel = ""
             tg-retorna-com:SCREEN-VALUE IN FRAME f-pg-sel = 'NO'
             tg-altera-qtd:SCREEN-VALUE IN FRAME f-pg-sel = 'NO'
             tg-consumo:SCREEN-VALUE IN FRAME f-pg-sel = 'NO'
             fi-justificativa:SCREEN-VALUE IN FRAME f-pg-sel = "".

      APPLY 'VALUE-CHANGED' TO tg-etiquetas.
   END.
   ASSIGN fi-emitente:SENSITIVE IN FRAME f-pg-sel = SELF:INPUT-VALUE
          tg-espelho-nf:SENSITIVE IN FRAME f-pg-sel = SELF:INPUT-VALUE
          tg-retorna-sem:SENSITIVE IN FRAME f-pg-sel = SELF:INPUT-VALUE.

   IF fi-emitente:SENSITIVE THEN 
      APPLY 'entry' TO fi-emitente.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-etiquetas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-etiquetas w-relat
ON VALUE-CHANGED OF tg-etiquetas IN FRAME f-pg-sel /* Altera Caracter°stica das Etiquetas */
DO:
   IF SELF:INPUT-VALUE = 'YES' THEN DO.
      ASSIGN tg-devolucao:SCREEN-VALUE IN FRAME f-pg-sel = 'NO'
             tg-espelho-nf:SCREEN-VALUE IN FRAME f-pg-sel = 'NO'
             tg-retorna-sem:SCREEN-VALUE IN FRAME f-pg-sel = 'NO'
             fi-emitente:SCREEN-VALUE IN FRAME f-pg-sel = ""
             fi-nome-emit:SCREEN-VALUE IN FRAME f-pg-sel = "".
      APPLY 'VALUE-CHANGED' TO tg-devolucao.
   END.

   ASSIGN fi-it-codigo:SENSITIVE IN FRAME f-pg-sel = SELF:INPUT-VALUE
          fi-cod-refer:SENSITIVE IN FRAME f-pg-sel = SELF:INPUT-VALUE
          fi-lote:SENSITIVE IN FRAME f-pg-sel = SELF:INPUT-VALUE
          fi-doca:SENSITIVE IN FRAME f-pg-sel = SELF:INPUT-VALUE
          tg-retorna-com:SENSITIVE IN FRAME f-pg-sel = SELF:INPUT-VALUE
          tg-altera-qtd:SENSITIVE IN FRAME f-pg-sel = SELF:INPUT-VALUE
          tg-consumo:SENSITIVE IN FRAME f-pg-sel = SELF:INPUT-VALUE
          fi-justificativa:SENSITIVE IN FRAME f-pg-sel = SELF:INPUT-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-dig
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-relat 


/* ***************************  Main Block  *************************** */
fi-cod-estabel:LOAD-MOUSE-POINTER("image/lupa.cur")    IN FRAME f-pg-sel.

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{utp/ut9000.i "ESSP0187" "2.04.00.000"}

/*:T inicializaá‰es do template de relat¢rio */
{include/i-rpini.i}

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

{include/i-rplbl.i}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

ON 'leave':U OF tt-digita.num-etiqueta IN BROWSE br-digita DO:
   FIND ob-etiqueta WHERE
        ob-etiqueta.cod-estabel = c-cod-estabel AND
        ob-etiqueta.num-etiqueta = INPUT BROWSE br-digita tt-digita.num-etiqueta
        NO-LOCK NO-ERROR.
   IF NOT AVAIL ob-etiqueta THEN DO.
      MESSAGE 'Etiqueta n∆o Cadastrada...'
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY 'ENTRY' TO SELF.
      RETURN NO-APPLY.
   END.

   DISP ob-etiqueta.quantidade @ de-qtd-atu
        WITH BROWSE br-digita.

END.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO  ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    RUN enable_UI.

    FIND FIRST ped-venda NO-LOCK NO-ERROR.
    ASSIGN c-cod-estabel = ped-venda.cod-estabel.

    FIND estabelec WHERE
         estabelec.cod-estabel = c-cod-estabel 
         NO-LOCK NO-ERROR.
    IF AVAIL estabelec THEN
       ASSIGN fi-cod-estabel:SCREEN-VALUE IN FRAME f-pg-sel = c-cod-estabel
              fi-nome-estabel:SCREEN-VALUE IN FRAME f-pg-sel = TRIM(estabelec.cidade) + " / " + TRIM(estabelec.bairro).


    {include/i-rpmbl.i}
  
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
  ENABLE im-pg-dig im-pg-imp im-pg-sel bt-executar bt-cancelar bt-ajuda 
      WITH FRAME f-relat IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}
  DISPLAY fi-cod-estabel fi-nome-estabel tg-devolucao fi-emitente fi-nome-emit 
          tg-espelho-nf tg-retorna-sem tg-acertos cb-acerto tg-sem-loc 
          fi-it-codigo-act fi-refer-act-ini fi-refer-act-fin tg-etiquetas 
          fi-it-codigo fi-cod-refer fi-lote fi-doca tg-retorna-com tg-altera-qtd 
          tg-consumo fi-justificativa 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  ENABLE fi-cod-estabel tg-devolucao tg-acertos tg-etiquetas RECT-10 RECT-11 
         RECT-55 IMAGE-1 IMAGE-2 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}
  DISPLAY rs-destino c-arquivo l-habilitaRtf c-modelo-rtf rs-execucao text-rtf 
          text-modelo-rtf 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  ENABLE RECT-7 RECT-9 rect-rtf rs-destino bt-config-impr bt-arquivo c-arquivo 
         l-habilitaRtf bt-modelo-rtf rs-execucao text-rtf text-modelo-rtf 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}
  ENABLE br-digita bt-inserir bt-recuperar 
      WITH FRAME f-pg-dig IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-dig}
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
    /*14/02/2005 - tech1007 - Alterada condicao para n∆o considerar mai o RTF como destino*/
    if input frame f-pg-imp rs-destino = 2 and
       input frame f-pg-imp rs-execucao = 1 then do:
       RUN utp/ut-vlarq.p (input input frame f-pg-imp c-arquivo).
        
       IF return-value = "NOK":U then do:
          run utp/ut-msgs.p (input "show":U, input 73, input "").
            
          apply "MOUSE-SELECT-CLICK":U to im-pg-imp in frame f-relat.
          apply "ENTRY":U to c-arquivo in frame f-pg-imp.
          return error.
       end.
    end.

    /*14/02/2005 - tech1007 - Teste efetuado para nao permitir modelo em branco*/
    &IF "{&RTF}":U = "YES":U &THEN
    IF ( INPUT FRAME f-pg-imp c-modelo-rtf = "" AND
         INPUT FRAME f-pg-imp l-habilitaRtf = "Yes" ) OR
       ( SEARCH(INPUT FRAME f-pg-imp c-modelo-rtf) = ? AND
         input frame f-pg-imp rs-execucao = 1 AND
         INPUT FRAME f-pg-imp l-habilitaRtf = "Yes" )
         THEN DO:
        run utp/ut-msgs.p (input "show":U, input 73, input "").        
        apply "MOUSE-SELECT-CLICK":U to im-pg-imp in frame f-relat.
        return error.
    END.
    &endif
    /*Fim teste Modelo*/
    
    /*:T Coloque aqui as validaá‰es das outras p†ginas, lembrando que elas devem 
       apresentar uma mensagem de erro cadastrada, posicionar na p†gina com 
       problemas e colocar o focus no campo com problemas */
    
    IF NOT INPUT FRAME f-pg-sel tg-devolucao AND
       NOT INPUT FRAME f-pg-sel tg-etiquetas AND
       NOT INPUT FRAME f-pg-sel tg-acertos THEN DO.
       MESSAGE 'Tipo de Processamento n∆o Selecionado...'
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
       APPLY "MOUSE-SELECT-CLICK":U TO im-pg-sel IN FRAME f-relat.
       APPLY "ENTRY":U TO tg-espelho-nf IN FRAME f-pg-sel.
       RETURN ERROR.
    END.

    IF INPUT FRAME f-pg-sel tg-devolucao AND
       INPUT FRAME f-pg-sel fi-emitente = "" THEN DO.
       MESSAGE 'Favor Informar o Cliente.... '
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
       APPLY "MOUSE-SELECT-CLICK":U TO im-pg-sel IN FRAME f-relat.
       APPLY "ENTRY":U TO fi-emitente IN FRAME f-pg-sel.
       RETURN ERROR.
    END.

    IF INPUT FRAME f-pg-sel tg-etiquetas AND
       LENGTH(INPUT FRAME f-pg-sel fi-justificativa) < 10 THEN DO.
       MESSAGE 'Favor Informar um Texto de no m°nimo 10 Caracteres' SKIP
               'Justificando a Alteraá∆o...' 
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
       APPLY "MOUSE-SELECT-CLICK":U TO im-pg-sel IN FRAME f-relat.
       APPLY "ENTRY":U TO fi-justificativa IN FRAME f-pg-sel.
       RETURN ERROR.
    END.

    IF INPUT FRAME f-pg-sel tg-devolucao AND
       INPUT FRAME f-pg-sel tg-espelho-nf = NO AND
       INPUT FRAME f-pg-sel tg-retorna-sem = NO THEN DO.
       MESSAGE 'Favor selecionar algo para Processar...'
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

       APPLY "MOUSE-SELECT-CLICK":U TO im-pg-sel IN FRAME f-relat.
       APPLY "ENTRY":U TO tg-devolucao IN FRAME f-pg-sel.
       RETURN ERROR.
    END.

    IF INPUT FRAME f-pg-sel tg-etiquetas AND
       INPUT FRAME f-pg-sel fi-it-codigo = "" AND
       INPUT FRAME f-pg-sel fi-cod-refer = "" AND
       INPUT FRAME f-pg-sel fi-lote = "" AND
       INPUT FRAME f-pg-sel fi-doca = "" AND
       INPUT FRAME f-pg-sel tg-retorna-com = NO AND
       INPUT FRAME f-pg-sel tg-altera-qtd = NO AND
       INPUT FRAME f-pg-sel tg-consumo = NO THEN DO.
       MESSAGE 'Favor selecionar algo para Alterar.....'
               VIEW-AS ALERT-BOX INFO BUTTONS OK.

       APPLY "MOUSE-SELECT-CLICK":U TO im-pg-sel IN FRAME f-relat.
       APPLY "ENTRY":U TO tg-etiquetas IN FRAME f-pg-sel.
       RETURN ERROR.
    END.
    
    /*:T Coloque aqui as validaá‰es da p†gina de Digitaá∆o, lembrando que elas devem
       apresentar uma mensagem de erro cadastrada, posicionar nesta p†gina e colocar
       o focus no campo com problemas */
    /*browse br-digita:SET-REPOSITIONED-ROW (browse br-digita:DOWN, "ALWAYS":U).*/
    
    ASSIGN l-error = NO.

    IF NOT CAN-FIND(FIRST tt-digita) THEN DO.
        MESSAGE 'N∆o Encontrado Etiquetas para Processar...'
                 VIEW-AS ALERT-BOX INFO BUTTONS OK.
       ASSIGN l-error = YES.
    END.

    FOR EACH tt-digita NO-LOCK:
        ASSIGN r-tt-digita = rowid(tt-digita).
        
        /*:T Validaá∆o de duplicidade de registro na temp-table tt-digita */
        FIND FIRST b-tt-digita where 
                   b-tt-digita.num-etiqueta = tt-digita.num-etiqueta and 
                   rowid(b-tt-digita) <> rowid(tt-digita) no-lock no-error.
        IF avail b-tt-digita then do:
            apply "MOUSE-SELECT-CLICK":U to im-pg-dig in frame f-relat.
            reposition br-digita to rowid rowid(b-tt-digita).
            
            run utp/ut-msgs.p (input "show":U, input 108, input "").
            apply "ENTRY":U to tt-digita.num-etiqueta in browse br-digita.
            
            return error.
        END.
        
        /*:T As demais validaá‰es devem ser feitas aqui */
        IF tt-digita.num-etiqueta <= 0 then do:
            assign browse br-digita:CURRENT-COLUMN = tt-digita.num-etiqueta:HANDLE in browse br-digita.
            
            apply "MOUSE-SELECT-CLICK":U to im-pg-dig in frame f-relat.
            reposition br-digita to rowid r-tt-digita.
            
            run utp/ut-msgs.p (input "show":U, input 99999, input "").
            apply "ENTRY":U to tt-digita.num-etiqueta in browse br-digita.
            
            return error.
        END.

        FIND ob-etiqueta WHERE
             ob-etiqueta.cod-estabel = INPUT FRAME f-pg-sel fi-cod-estabel AND
             ob-etiqueta.num-etiqueta = tt-digita.num-etiqueta 
             NO-LOCK NO-ERROR.
        IF NOT AVAIL ob-etiqueta THEN DO.
           MESSAGE 'Etiqueta ' tt-digita.num-etiqueta ' n∆o Cadastrada...'
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
           ASSIGN l-error = YES.
        END.

        IF INPUT FRAME f-pg-sel fi-it-codigo <> "" OR
           INPUT FRAME f-pg-sel fi-cod-refer <> "" OR 
           INPUT FRAME f-pg-sel fi-lote <> "" THEN DO.
           IF ob-etiqueta.situacao <> 3 THEN DO.
              MESSAGE 'Situacao da Etiqueta ' tt-digita.num-etiqueta ' n∆o permite alteraá∆o do Item/Referencia'
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
              ASSIGN l-error = YES.
           END.
        END.

        IF INPUT FRAME f-pg-sel tg-retorna-com THEN DO.
           IF ob-etiqueta.situacao <= 4 THEN DO.
              MESSAGE 'Situacao da Etiqueta ' tt-digita.num-etiqueta ' n∆o permite retorno para Estoque'
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
              ASSIGN l-error = YES.
           END.
        END.

        IF INPUT FRAME f-pg-sel tg-altera-qtd THEN DO.
           IF ob-etiqueta.situacao <> 3 THEN DO.
              MESSAGE 'Etiqueta ' tt-digita.num-etiqueta ' n∆o est† em Estoque,' SKIP
                      'Imposs°vel Alterar sua Quantidade ...'
                      VIEW-AS ALERT-BOX INFO BUTTONS OK.
              ASSIGN l-error = YES.
           END.

           IF tt-digita.quantidade = 0 THEN DO.
              MESSAGE 'Etiqueta ' tt-digita.num-etiqueta SKIP
                      'Para Zerar Utilize opá∆o Retirar Etiquetas do Estque ...'
                      VIEW-AS ALERT-BOX INFO BUTTONS OK.
              ASSIGN l-error = YES.
           END.

           IF tt-digita.quantidade = ob-etiqueta.quantidade THEN DO.
              MESSAGE 'Etiqueta ' tt-digita.num-etiqueta SKIP
                      'Quantidade Informada Igual Ö Quantidade da Etiqueta...'
                      VIEW-AS ALERT-BOX INFO BUTTONS OK.
              ASSIGN l-error = YES.
           END.
        END.

        IF INPUT FRAME f-pg-sel tg-consumo THEN DO.
           IF ob-etiqueta.situacao <> 3 THEN DO.
              MESSAGE 'Etiqueta ' tt-digita.num-etiqueta ' n∆o est† em Estoque,' SKIP
                      'Imposs°vel Retir†-la para Consumo...'
                      VIEW-AS ALERT-BOX INFO BUTTONS OK.
              ASSIGN l-error = YES.
           END.
        END.

        IF INPUT FRAME f-pg-sel tg-espelho-nf THEN DO.
           IF ob-etiqueta.situacao <> 5 THEN DO.
              MESSAGE 'Etiqueta ' tt-digita.num-etiqueta ' n∆o est† Faturada...'
                   VIEW-AS ALERT-BOX INFO BUTTONS OK.
              ASSIGN l-error = YES.
           END.

           FIND ped-item-rom WHERE
                ped-item-rom.cod-estabel = ob-etiqueta.cod-estabel AND
                ped-item-rom.num-etiqueta = ob-etiqueta.num-etiqueta NO-LOCK NO-ERROR.
           IF NOT AVAIL ped-item-rom THEN DO.
              MESSAGE 'Etiqueta ' tt-digita.num-etiqueta ' n∆o encontrada em Romaneio...'
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
              ASSIGN l-error = YES.
           END.
           IF ped-item-rom.nome-abrev <> INPUT FRAME f-pg-sel fi-emitente THEN DO.
              MESSAGE 'Etiqueta ' tt-digita.num-etiqueta ' foi faturada para o Cliente ' ped-item-rom.nome-abrev 
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
              ASSIGN l-error = YES.
           END.
        END.
    END.

    IF l-error THEN DO.
       APPLY "MOUSE-SELECT-CLICK":U TO im-pg-dig IN FRAME f-relat.
       APPLY "ENTRY":U TO tt-digita.num-etiqueta IN BROWSE br-digita.
       RETURN ERROR.
    END.

    IF INPUT FRAME f-pg-sel tg-acertos THEN DO.
       MESSAGE 'Deseja Realmente Retirar Etiquetas do Estoque ?'
           VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE l-ok AS LOGICAL.
       IF NOT l-ok THEN DO.
          APPLY "MOUSE-SELECT-CLICK":U TO im-pg-sel IN FRAME f-relat.
          APPLY "ENTRY":U TO fi-it-codigo-act IN FRAME f-pg-sel.
          RETURN ERROR.
       END.
    END.

    /*:T Aqui s∆o gravados os campos da temp-table que ser† passada como parÉmetro
       para o programa RP.P */
    
    CREATE tt-param.
    ASSIGN tt-param.usuario         = c-seg-usuario
           tt-param.destino         = input frame f-pg-imp rs-destino
           tt-param.data-exec       = today
           tt-param.hora-exec       = time
           &IF "{&RTF}":U = "YES":U &THEN
           tt-param.modelo-rtf      = INPUT FRAME f-pg-imp c-modelo-rtf
           /*Alterado 14/02/2005 - tech1007 - Armazena a informaá∆o se o RTF est† habilitado ou n∆o*/
           tt-param.l-habilitaRtf     = INPUT FRAME f-pg-imp l-habilitaRtf
           /*Fim alteracao 14/02/2005*/ 
           &endif
           .
    
    /*Alterado 14/02/2005 - tech1007 - Alterado o teste para verificar se a opá∆o de RTF est† selecionada*/
    if tt-param.destino = 1 
    then assign tt-param.arquivo = "".
    else if  tt-param.destino = 2
         then assign tt-param.arquivo = input frame f-pg-imp c-arquivo.
         else assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp":U.
    /*Fim alteracao 14/02/2005*/

    /*:T Coloque aqui a/l¢gica de gravaá∆o dos demais campos que devem ser passados
       como parÉmetros para o programa RP.P, atravÇs da temp-table tt-param */
    
    ASSIGN tt-param.cod-estabel = INPUT FRAME f-pg-sel fi-cod-estabel
           tt-param.l-retorna-sem = INPUT FRAME f-pg-sel tg-retorna-sem
           tt-param.l-espelho = INPUT FRAME f-pg-sel tg-espelho-nf
           tt-param.nome-abrev = INPUT FRAME f-pg-sel fi-emitente
           tt-param.l-etiquetas = INPUT FRAME f-pg-sel tg-etiquetas
           tt-param.it-codigo = INPUT FRAME f-pg-sel fi-it-codigo
           tt-param.cod-refer = INPUT FRAME f-pg-sel fi-cod-refer       
           tt-param.lote = INPUT FRAME f-pg-sel fi-lote       
           tt-param.localiz = INPUT FRAME f-pg-sel fi-doca
           tt-param.l-retorna-com = INPUT FRAME f-pg-sel tg-retorna-com
           tt-param.l-altera-qtd = INPUT FRAME f-pg-sel tg-altera-qtd
           tt-param.l-consumo = INPUT FRAME f-pg-sel tg-consumo
           tt-param.i-acerto = IF INPUT FRAME f-pg-sel tg-acertos = YES
                               THEN INPUT FRAME f-pg-sel cb-acerto ELSE ?
           tt-param.justificativa = INPUT FRAME f-pg-sel fi-justificativa.
    
    /*:T Executar do programa RP.P que ir† criar o relat¢rio */
    {include/i-rpexb.i}
    
    SESSION:SET-WAIT-STATE("general":U).
    
    {include/i-rprun.i esrp/essp0187rp.p}

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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-localiz w-relat 
FUNCTION fn-localiz RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND ob-etiqueta WHERE
       ob-etiqueta.cod-estabel = fi-cod-estabel:SCREEN-VALUE IN FRAME f-pg-sel AND 
       ob-etiqueta.num-etiqueta = tt-digita.num-etiqueta
       NO-LOCK NO-ERROR.
  IF AVAIL ob-etiqueta THEN
     RETURN ob-etiqueta.localizacao.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-qtd-etiqueta w-relat 
FUNCTION fn-qtd-etiqueta RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND ob-etiqueta WHERE
       ob-etiqueta.cod-estabel = fi-cod-estabel:SCREEN-VALUE IN FRAME f-pg-sel AND 
       ob-etiqueta.num-etiqueta = tt-digita.num-etiqueta
       NO-LOCK NO-ERROR.
  IF AVAIL ob-etiqueta THEN
     RETURN ob-etiqueta.quantidade.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-situacao w-relat 
FUNCTION fn-situacao RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  FIND ob-etiqueta WHERE
       ob-etiqueta.cod-estabel = fi-cod-estabel:SCREEN-VALUE IN FRAME f-pg-sel AND 
       ob-etiqueta.num-etiqueta = tt-digita.num-etiqueta
       NO-LOCK NO-ERROR.
  IF AVAIL ob-etiqueta THEN DO.
     {esinc/i-dsrb.i ob-etiqueta.situacao ob-etiqueta.situacao c-situacao}
     RETURN c-situacao.   /* Function return value. */
  END.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

