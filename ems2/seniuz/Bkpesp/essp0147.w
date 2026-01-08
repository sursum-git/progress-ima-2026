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
{include/i-prgvrs.i ESSP0147 2.04.00.000}

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

DEFINE TEMP-TABLE tt-param   NO-UNDO
       FIELD destino            AS INTEGER
       FIELD arquivo            AS CHAR FORMAT "x(35)"
       FIELD usuario            AS CHAR FORMAT "x(12)"
       FIELD data-exec          AS DATE
       FIELD hora-exec          AS INTEGER
       FIELD classifica         AS INTEGER
       FIELD desc-classifica    AS CHAR FORMAT "x(40)"
       FIELD cod-estabel-ini    LIKE ob-etiqueta.cod-estabel
       FIELD cod-estabel-fin    LIKE ob-etiqueta.cod-estabel
       FIELD localizacao-ini    LIKE ob-etiqueta.localizacao
       FIELD localizacao-fin    LIKE ob-etiqueta.localizacao
       FIELD dt-emissao-ini     LIKE ob-etiqueta.dt-emissao 
       FIELD dt-emissao-fin     LIKE ob-etiqueta.dt-emissao 
       FIELD nr-lote-ini        LIKE ob-etiqueta.nr-lote     
       FIELD nr-lote-fin        LIKE ob-etiqueta.nr-lote 
       FIELD fi-ini-it-codigo   like item.it-codigo
       FIELD fi-fin-it-codigo   like item.it-codigo
       FIELD fi-ini-cod-refer   like ref-item.cod-refer
       FIELD fi-fin-cod-refer   like ref-item.cod-refer
       FIELD un-ini             LIKE ob-etiqueta.un
       FIELD un-fin             LIKE ob-etiqueta.un
       FIELD qtd-maxima         LIKE ob-etiqueta.quantidade
       FIELD fi-desenho         AS CHAR FORMAT "x(4)"
       FIELD l-inc-exc          AS LOG FORMAT "Inclusive/Exclusive"
       FIELD i-situacao         AS INT
       FIELD opc-artigo         AS CHAR FORMAT "x"
       FIELD enviar-e-mail      AS LOG FORMAT "Sim/NÆo"
       FIELD subject-e-mail     AS CHAR FORMAT "x(40)"
       FIELD texto-e-mail       AS CHAR FORMAT "x(2000)"
       FIELD l-batch            AS LOG
       FIELD imp-param          AS LOG
       FIELD l-imp-layout       AS LOG
       FIELD l-resumo           AS LOG
       FIELD l-excel            AS LOG.

define temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id ordem.

define buffer b-tt-digita for tt-digita.

/* Transfer Definitions */

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

def var raw-param        as raw no-undo.

def temp-table tt-raw-digita
   field raw-digita      as raw.
                    
DEF {1} VAR c-local AS CHAR.

def var l-ok               as logical no-undo.
def var c-arq-digita       as char    no-undo.
def var c-terminal         as char    no-undo.
DEF VAR da-aux             AS DATE.
DEF VAR c-cod-estabel      AS CHAR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-relat
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-pg-imp

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-7 RECT-9 rs-destino bt-config-impr ~
bt-arquivo c-arquivo rs-execucao 
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

DEFINE VARIABLE fi-texto-e-mail AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 58 BY 4 NO-UNDO.

DEFINE VARIABLE fi-assunto-e-mail AS CHARACTER FORMAT "X(256)":U 
     LABEL "Assunto" 
     VIEW-AS FILL-IN 
     SIZE 47 BY .88 NO-UNDO.

DEFINE VARIABLE fi-qtd-maxima AS DECIMAL FORMAT ">>>,>>9.99":U INITIAL 999999.99 
     LABEL "Qtde M xima" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE rs-situacao AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Estoque", 1,
"Reservada", 2,
"Ambas", 3
     SIZE 11.57 BY 2.5 NO-UNDO.

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 71 BY 6.5.

DEFINE RECTANGLE RECT-43
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 71 BY 3.25.

DEFINE VARIABLE to-enviar-e-mail AS LOGICAL INITIAL no 
     LABEL "Envia e-mail" 
     VIEW-AS TOGGLE-BOX
     SIZE 12 BY .67 NO-UNDO.

DEFINE VARIABLE to-imp-layout AS LOGICAL INITIAL no 
     LABEL "Imprimir Layout Localiza‡Æo" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .88 TOOLTIP "Imprimir Layout Localiza‡Æo ao final do relat¢rio."
     FONT 1 NO-UNDO.

DEFINE VARIABLE to-imp-param AS LOGICAL INITIAL no 
     LABEL "Imprimir Parƒmetros/Sele‡Æo" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .88 TOOLTIP "Imprimir Parƒmetros/Sele‡Æo ao final do relat¢rio."
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-cod-estabel-ini AS CHARACTER FORMAT "x(3)" 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 TOOLTIP "C¢digo do estabelecimento inicial." NO-UNDO.

DEFINE VARIABLE fi-dt-emissao-fin AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 
     LABEL "Data de Corte" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fin-cod-refer AS CHARACTER FORMAT "x(8)" INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "Referˆncia final" NO-UNDO.

DEFINE VARIABLE fi-fin-it-codigo AS CHARACTER FORMAT "x(16)" INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 TOOLTIP "Item final" NO-UNDO.

DEFINE VARIABLE fi-ini-cod-refer AS CHARACTER FORMAT "x(8)" 
     LABEL "Referˆncia" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "Referˆncia inicial" NO-UNDO.

DEFINE VARIABLE fi-ini-it-codigo AS CHARACTER FORMAT "x(16)" 
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 TOOLTIP "Item inicial" NO-UNDO.

DEFINE VARIABLE fi-localizacao-fin AS CHARACTER FORMAT "XXX/XXX" INITIAL "ZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 TOOLTIP "Codigo da Doca Final" NO-UNDO.

DEFINE VARIABLE fi-localizacao-ini AS CHARACTER FORMAT "XXX/XXX" 
     LABEL "Doca" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 TOOLTIP "Codigo da Doca Inicial" NO-UNDO.

DEFINE VARIABLE fi-nome-estab AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 42.43 BY .88 NO-UNDO.

DEFINE VARIABLE fi-un-fin AS CHARACTER FORMAT "XX" INITIAL "ZZ" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-un-ini AS CHARACTER FORMAT "XX" 
     LABEL "Un" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-34
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-35
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-36
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-37
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-40
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-41
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-5
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-7
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE VARIABLE tg-excel AS LOGICAL INITIAL no 
     LABEL "Gera Dados em Excel" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .83 NO-UNDO.

DEFINE VARIABLE tg-resumo AS LOGICAL INITIAL no 
     LABEL "Resumido (Idade do Estoque)" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .83 NO-UNDO.

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
     RECT-1 AT ROW 14.29 COL 2
     rt-folder-right AT ROW 2.67 COL 80.43
     RECT-6 AT ROW 13.75 COL 2.14
     rt-folder AT ROW 2.54 COL 2
     rt-folder-left AT ROW 2.54 COL 2.14
     rt-folder-top AT ROW 2.54 COL 2.14
     im-pg-imp AT ROW 1.5 COL 33.57
     im-pg-par AT ROW 1.5 COL 17.86
     im-pg-sel AT ROW 1.5 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81 BY 15
         DEFAULT-BUTTON bt-executar.

DEFINE FRAME f-pg-par
     fi-qtd-maxima AT ROW 1.25 COL 49 COLON-ALIGNED WIDGET-ID 16
     rs-situacao AT ROW 1.42 COL 13.14 NO-LABEL WIDGET-ID 10
     to-imp-param AT ROW 2.25 COL 51
     to-enviar-e-mail AT ROW 5.17 COL 13.14 WIDGET-ID 8
     fi-assunto-e-mail AT ROW 6.04 COL 11 COLON-ALIGNED WIDGET-ID 2
     fi-texto-e-mail AT ROW 7.21 COL 13 NO-LABEL WIDGET-ID 4
     to-imp-layout AT ROW 3 COL 51 WIDGET-ID 18
     "Situa‡Æo:" VIEW-AS TEXT
          SIZE 7 BY .54 AT ROW 1.5 COL 5.86 WIDGET-ID 14
     RECT-43 AT ROW 1.17 COL 4
     RECT-13 AT ROW 5 COL 3.86 WIDGET-ID 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 77.14 BY 10.5
         FONT 1.

DEFINE FRAME f-pg-sel
     fi-cod-estabel-ini AT ROW 1.75 COL 18 COLON-ALIGNED WIDGET-ID 2
     fi-nome-estab AT ROW 1.75 COL 22.57 COLON-ALIGNED NO-LABEL WIDGET-ID 22
     fi-localizacao-ini AT ROW 2.75 COL 18 COLON-ALIGNED
     fi-localizacao-fin AT ROW 2.75 COL 48.86 COLON-ALIGNED NO-LABEL
     fi-ini-it-codigo AT ROW 3.75 COL 18 COLON-ALIGNED
     fi-fin-it-codigo AT ROW 3.75 COL 48.86 COLON-ALIGNED NO-LABEL
     fi-ini-cod-refer AT ROW 4.75 COL 18 COLON-ALIGNED
     fi-fin-cod-refer AT ROW 4.75 COL 48.86 COLON-ALIGNED NO-LABEL
     fi-un-ini AT ROW 5.75 COL 18 COLON-ALIGNED WIDGET-ID 14
     fi-un-fin AT ROW 5.75 COL 48.86 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     tg-resumo AT ROW 7.25 COL 20 WIDGET-ID 24
     fi-dt-emissao-fin AT ROW 8.08 COL 30.29 COLON-ALIGNED
     tg-excel AT ROW 9.5 COL 20 WIDGET-ID 20
     IMAGE-34 AT ROW 3.75 COL 36.57
     IMAGE-35 AT ROW 4.75 COL 36.57
     IMAGE-36 AT ROW 4.75 COL 47.29
     IMAGE-37 AT ROW 3.75 COL 47.29
     IMAGE-5 AT ROW 2.75 COL 36.57
     IMAGE-7 AT ROW 2.75 COL 47.29
     IMAGE-40 AT ROW 5.75 COL 36.57 WIDGET-ID 16
     IMAGE-41 AT ROW 5.75 COL 47.29 WIDGET-ID 18
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 4 ROW 3
         SIZE 75.86 BY 10.46
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
         TITLE              = "Estoque da Expedi‡Æo por Docas"
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
   FRAME-NAME                                                           */
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
/* SETTINGS FOR FILL-IN fi-assunto-e-mail IN FRAME f-pg-par
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR EDITOR fi-texto-e-mail IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME f-pg-sel
                                                                        */
/* SETTINGS FOR FILL-IN fi-dt-emissao-fin IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nome-estab IN FRAME f-pg-sel
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
ON END-ERROR OF w-relat /* Estoque da Expedi‡Æo por Docas */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON WINDOW-CLOSE OF w-relat /* Estoque da Expedi‡Æo por Docas */
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
&Scoped-define SELF-NAME fi-ini-cod-refer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-cod-refer w-relat
ON LEAVE OF fi-ini-cod-refer IN FRAME f-pg-sel /* Referˆncia */
DO:
   ASSIGN fi-fin-cod-refer:SCREEN-VALUE = 'ZZZZZZZZ'.
   IF SELF:SCREEN-VALUE <> '' THEN
      ASSIGN fi-fin-cod-refer:SCREEN-VALUE = SELF:SCREEN-VALUE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ini-it-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-it-codigo w-relat
ON LEAVE OF fi-ini-it-codigo IN FRAME f-pg-sel /* Item */
DO:
   ASSIGN fi-fin-it-codigo:SCREEN-VALUE = 'ZZZZZZZZZZZZZZZZ'.
   IF SELF:SCREEN-VALUE <> '' THEN
      ASSIGN fi-fin-it-codigo:SCREEN-VALUE = SELF:SCREEN-VALUE.
      
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


&Scoped-define FRAME-NAME f-pg-sel
&Scoped-define SELF-NAME tg-resumo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-resumo w-relat
ON VALUE-CHANGED OF tg-resumo IN FRAME f-pg-sel /* Resumido (Idade do Estoque) */
DO:
   ASSIGN fi-dt-emissao-fin:SENSITIVE = NO.
   ASSIGN tg-excel:SENSITIVE = YES.
   ASSIGN tg-excel:SCREEN-VALUE = 'NO'.
   IF SELF:SCREEN-VALUE = 'YES' THEN DO.
      ASSIGN fi-dt-emissao-fin:SENSITIVE = YES.
      APPLY 'ENTRY' TO fi-dt-emissao-fin.
      ASSIGN tg-excel:SCREEN-VALUE = 'YES'.
      ASSIGN tg-excel:SENSITIVE = NO.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME to-enviar-e-mail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL to-enviar-e-mail w-relat
ON VALUE-CHANGED OF to-enviar-e-mail IN FRAME f-pg-par /* Envia e-mail */
DO:
  IF INPUT FRAME {&frame-name} to-enviar-e-mail = YES THEN DO:
     ASSIGN fi-assunto-e-mail:SENSITIVE IN FRAME f-pg-par = YES 
            fi-texto-e-mail:SENSITIVE IN FRAME f-pg-par = YES.
             
     ASSIGN fi-assunto-e-mail:SCREEN-VALUE IN FRAME f-pg-par = "Estoque da Expedi‡Æo sem Docas - #PER-INI a #PER-FIN".

     ASSIGN fi-texto-e-mail:SCREEN-VALUE IN FRAME f-pg-par = 
            "Segue anexo Rela‡Æo de estoque da expedi‡Æo sem docas, #PER-INI a #PER-FIN." +
            CHR(13) + CHR(13) + "Atenciosamente," + CHR(13) + CHR(13) +
            "TEAR TEXTIL INDUSTRIA E COMERCIO LTDA." + CHR(13) + 
            "Setor de PCP de Produ‡Æo.". 
  END.
  ELSE DO.
     ASSIGN fi-assunto-e-mail:SENSITIVE IN FRAME f-pg-par = NO 
            fi-texto-e-mail:SENSITIVE IN FRAME f-pg-par = NO.

     ASSIGN fi-assunto-e-mail:SCREEN-VALUE IN FRAME f-pg-par = ""
            fi-texto-e-mail:SCREEN-VALUE IN FRAME f-pg-par = "".
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

{utp/ut9000.i "ESSP0147" "2.04.00.000"}

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

    FIND FIRST ped-venda NO-LOCK NO-ERROR.
    FIND estabelec WHERE 
         estabelec.cod-estab = ped-venda.cod-estab NO-LOCK NO-ERROR.
    ASSIGN fi-cod-estabel-ini:SCREEN-VALUE IN FRAME f-pg-sel = estabelec.cod-estabel.
    IF AVAIL estabelec THEN
       ASSIGN fi-nome-estab:SCREEN-VALUE IN FRAME f-pg-sel = estabelec.nome.

    ASSIGN fi-dt-emissao-fin:SCREEN-VALUE IN FRAME f-pg-sel = STRING(TODAY,"99/99/9999").

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
  ENABLE im-pg-imp im-pg-par im-pg-sel bt-executar bt-ajuda bt-cancelar 
      WITH FRAME f-relat IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}
  DISPLAY rs-destino c-arquivo rs-execucao 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  ENABLE RECT-7 RECT-9 rs-destino bt-config-impr bt-arquivo c-arquivo 
         rs-execucao 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}
  DISPLAY fi-qtd-maxima rs-situacao to-imp-param to-enviar-e-mail 
          fi-texto-e-mail to-imp-layout 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  ENABLE fi-qtd-maxima rs-situacao to-imp-param to-enviar-e-mail to-imp-layout 
         RECT-43 RECT-13 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-par}
  DISPLAY fi-cod-estabel-ini fi-nome-estab fi-localizacao-ini fi-localizacao-fin 
          fi-ini-it-codigo fi-fin-it-codigo fi-ini-cod-refer fi-fin-cod-refer 
          fi-un-ini fi-un-fin tg-resumo fi-dt-emissao-fin tg-excel 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  ENABLE IMAGE-34 IMAGE-35 IMAGE-36 IMAGE-37 IMAGE-5 IMAGE-7 IMAGE-40 IMAGE-41 
         fi-cod-estabel-ini fi-localizacao-ini fi-localizacao-fin 
         fi-ini-it-codigo fi-fin-it-codigo fi-ini-cod-refer fi-fin-cod-refer 
         fi-un-ini fi-un-fin tg-resumo tg-excel 
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
           tt-param.cod-estabel-ini  = INPUT FRAME f-pg-sel fi-cod-estabel-ini
           tt-param.localizacao-ini  = INPUT FRAME f-pg-sel fi-localizacao-ini                
           tt-param.localizacao-fin  = INPUT FRAME f-pg-sel fi-localizacao-fin             
           tt-param.dt-emissao-ini   = 01.01.0001
           tt-param.dt-emissao-fin   = INPUT FRAME f-pg-sel fi-dt-emissao-fin              
           tt-param.nr-lote-ini      = ""
           tt-param.nr-lote-fin      = "ZZZZZZZZZ"
           tt-param.fi-ini-it-codigo = INPUT FRAME f-pg-sel fi-ini-it-codigo
           tt-param.fi-fin-it-codigo = INPUT FRAME f-pg-sel fi-fin-it-codigo
           tt-param.fi-ini-cod-refer = INPUT FRAME f-pg-sel fi-ini-cod-refer
           tt-param.fi-fin-cod-refer = INPUT FRAME f-pg-sel fi-fin-cod-refer
           tt-param.un-ini           = INPUT FRAME f-pg-sel fi-un-ini
           tt-param.un-fin           = INPUT FRAME f-pg-sel fi-un-fin
           tt-param.qtd-maxima       = INPUT FRAME f-pg-par fi-qtd-maxima
           tt-param.i-situacao       = INPUT FRAME f-pg-par rs-situacao
           tt-param.enviar-e-mail    = INPUT FRAME f-pg-par to-enviar-e-mail
           tt-param.subject-e-mail   = INPUT FRAME f-pg-par fi-assunto-e-mail
           tt-param.texto-e-mail     = INPUT FRAME f-pg-par fi-texto-e-mail
           tt-param.l-batch          = IF INPUT FRAME f-pg-imp rs-execucao = 1 THEN NO ELSE YES
           tt-param.imp-param        = INPUT FRAME f-pg-par to-imp-param
           tt-param.l-imp-layout     = INPUT FRAME f-pg-par to-imp-layout
           tt-param.l-resumo         = INPUT FRAME f-pg-sel tg-resumo
           tt-param.l-excel          = INPUT FRAME f-pg-sel tg-excel.
           
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
    
    {include/i-rprun.i esrp/essp0147rp.p}
    
    {include/i-rpexc.i}
    
    SESSION:SET-WAIT-STATE("":U).
    
    IF tt-param.dt-emissao-fin = TODAY THEN DO.
        {include/i-rptrm.i}
    END.
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

