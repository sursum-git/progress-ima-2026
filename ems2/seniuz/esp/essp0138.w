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
{include/i-prgvrs.i ESSP0138 2.04.00.000}

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

DEFINE TEMP-TABLE tt-param NO-UNDO
       FIELD destino            AS INTEGER
       FIELD arquivo            AS CHAR FORMAT "x(35)"
       FIELD usuario            AS CHAR FORMAT "x(12)"
       FIELD data-exec          AS DATE
       FIELD hora-exec          AS INTEGER
       FIELD classifica         AS INTEGER
       FIELD desc-classifica    AS CHAR FORMAT "x(40)"
       FIELD cod-estabel        LIKE nota-fiscal.cod-estabel
       FIELD serie              LIKE nota-fiscal.serie
       FIELD nr-pedcli-ini      LIKE ped-venda.nr-pedcli
       FIELD nr-pedcli-fin      LIKE ped-venda.nr-pedcli
       FIELD nr-nota-fis-ini    LIKE nota-fiscal.nr-nota-fis
       FIELD nr-nota-fis-fin    LIKE nota-fiscal.nr-nota-fis
       FIELD transp-ini         LIKE nota-fiscal.nome-transp
       FIELD transp-fin         LIKE nota-fiscal.nome-transp
       FIELD estado-ini         LIKE emitente.estado
       FIELD estado-fin         LIKE emitente.estado
       FIELD nome-ab-cli-ini    LIKE nota-fiscal.nome-ab-cli
       FIELD nome-ab-cli-fin    LIKE nota-fiscal.nome-ab-cli
       FIELD no-ab-reppri-ini   LIKE nota-fiscal.no-ab-reppri
       FIELD no-ab-reppri-fin   LIKE nota-fiscal.no-ab-reppri
       FIELD corte-com-ini      LIKE corte-comerc.codigo
       FIELD corte-com-fin      LIKE corte-comerc.codigo
       FIELD tipo-rel           AS INT
       FIELD peso-max-carga     AS INTEGER
       FIELD enviar-e-mail      AS LOG FORMAT "Sim/NÆo"
       FIELD e-mail-remet       AS CHAR FORMAT "x(45)"
       FIELD subject-e-mail     AS CHAR FORMAT "x(40)"
       FIELD texto-e-mail       AS CHAR FORMAT "x(2000)"
       FIELD impr-param         AS LOG.

define temp-table tt-digita no-undo
       field nr-nota-fis LIKE nota-fiscal.nr-nota-fis
       FIELD nr-pedcli   LIKE ped-venda.nr-pedcli
       index id-nota nr-nota-fis
       INDEX id-ped  nr-pedcli.

define buffer b-tt-digita for tt-digita.

/* Transfer Definitions */
DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.
def var raw-param        as raw no-undo.

def temp-table tt-raw-digita
   field raw-digita      as raw.
                    
/* Local Variable Definitions ---                                       */

def var l-ok               as logical no-undo.
def var c-arq-digita       as char    no-undo.
def var c-terminal         as char    no-undo.
DEF VAR c-cod-estabel      AS CHAR.

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
&Scoped-define FIELDS-IN-QUERY-br-digita tt-digita.nr-nota-fis tt-digita.nr-pedcli   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-digita tt-digita.nr-nota-fis tt-digita.nr-pedcli   
&Scoped-define ENABLED-TABLES-IN-QUERY-br-digita tt-digita
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br-digita tt-digita
&Scoped-define SELF-NAME br-digita
&Scoped-define QUERY-STRING-br-digita FOR EACH tt-digita
&Scoped-define OPEN-QUERY-br-digita OPEN QUERY br-digita FOR EACH tt-digita.
&Scoped-define TABLES-IN-QUERY-br-digita tt-digita
&Scoped-define FIRST-TABLE-IN-QUERY-br-digita tt-digita


/* Definitions for FRAME f-pg-dig                                       */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-pg-dig ~
    ~{&OPEN-QUERY-br-digita}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-10 rs-classif 
&Scoped-Define DISPLAYED-OBJECTS rs-classif 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 fi-serie fi-ini-nr-nota-fis fi-fin-nr-nota-fis ~
fi-ini-transp fi-fin-transp fi-ini-estado fi-fin-estado fi-ini-nome-ab-cli ~
fi-fin-nome-ab-cli fi-ini-no-ab-reppri fi-fin-no-ab-reppri fi-ini-corte-com ~
fi-fin-corte-com 
&Scoped-define List-2 fi-nr-pedcli-ini fi-nr-pedcli-fin 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-relat AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE rs-classif AS INTEGER INITIAL 6 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Por Transportador/Localiza‡Æo", 1,
"Por Transportador/Produto", 2,
"Por Transportador/Nota Fiscal ou Pedido", 3,
"Por Localiza‡Æo", 4,
"Por Produto", 5,
"Por Nota Fiscal ou Pedido", 6
     SIZE 31.43 BY 7.33 TOOLTIP "Parƒmetro para ordena‡Æo dos dados do relat¢rio." NO-UNDO.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 38 BY 8.04.

DEFINE BUTTON bt-alterar 
     LABEL "Alterar" 
     SIZE 15 BY 1
     FONT 1.

DEFINE BUTTON bt-inserir 
     LABEL "Inserir" 
     SIZE 15 BY 1
     FONT 1.

DEFINE BUTTON bt-recuperar 
     LABEL "Recuperar" 
     SIZE 15 BY 1
     FONT 1.

DEFINE BUTTON bt-retirar 
     LABEL "Retirar" 
     SIZE 15 BY 1
     FONT 1.

DEFINE BUTTON bt-salvar 
     LABEL "Salvar" 
     SIZE 15 BY 1
     FONT 1.

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

DEFINE VARIABLE fi-e-mail-remet AS CHARACTER FORMAT "X(45)":U 
     LABEL "E-mail remetente" 
     VIEW-AS FILL-IN 
     SIZE 40 BY .79 TOOLTIP "Endere‡o de e-mail do remetente." NO-UNDO.

DEFINE VARIABLE fi-peso-max-carga AS INTEGER FORMAT ">>>,>>9":U INITIAL 0 
     LABEL "Peso M ximo(Kg)" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 TOOLTIP "Peso m ximo (Kg) da carga para carregamento." NO-UNDO.

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 71 BY 5.92.

DEFINE RECTANGLE RECT-34
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 71 BY 1.75.

DEFINE VARIABLE tg-impr-param AS LOGICAL INITIAL no 
     LABEL "Imprimir Parƒmetros/Sele‡Æo" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .88 TOOLTIP "Imprimir Parƒmetros/Sele‡Æo ao final do relat¢rio."
     FONT 1 NO-UNDO.

DEFINE VARIABLE to-enviar-e-mail AS LOGICAL INITIAL no 
     LABEL "Envia e-mail" 
     VIEW-AS TOGGLE-BOX
     SIZE 12.72 BY 1.08 NO-UNDO.

DEFINE VARIABLE fi-cod-estabel AS CHARACTER FORMAT "X(3)":U 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fin-corte-com AS CHARACTER FORMAT "!" INITIAL "Z" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 TOOLTIP "C¢digo do corte comercial final." NO-UNDO.

DEFINE VARIABLE fi-fin-estado AS CHARACTER FORMAT "X(3)":U INITIAL "ZZZ" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 TOOLTIP "UF do estado do cliente final." NO-UNDO.

DEFINE VARIABLE fi-fin-no-ab-reppri AS CHARACTER FORMAT "X(12)" INITIAL "ZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 TOOLTIP "Nome abreviado do representante final." NO-UNDO.

DEFINE VARIABLE fi-fin-nome-ab-cli AS CHARACTER FORMAT "X(12)" INITIAL "ZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 TOOLTIP "Nome abreviado do cliente final" NO-UNDO.

DEFINE VARIABLE fi-fin-nr-nota-fis AS CHARACTER FORMAT "x(16)" INITIAL "9999999999999999" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 TOOLTIP "N£mero da £ltima nota fiscal." NO-UNDO.

DEFINE VARIABLE fi-fin-transp AS CHARACTER FORMAT "X(15)" INITIAL "ZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88 TOOLTIP "Nome abreviado do transportador final." NO-UNDO.

DEFINE VARIABLE fi-ini-corte-com AS CHARACTER FORMAT "!" 
     LABEL "Corte Comercial" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 TOOLTIP "C¢digo do corte comercial inicial." NO-UNDO.

DEFINE VARIABLE fi-ini-estado AS CHARACTER FORMAT "X(3)":U 
     LABEL "Estado" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 TOOLTIP "UF do estado do cliente inicial." NO-UNDO.

DEFINE VARIABLE fi-ini-no-ab-reppri AS CHARACTER FORMAT "X(12)":U 
     LABEL "Representante" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 TOOLTIP "Nome abreviado do representante inicial." NO-UNDO.

DEFINE VARIABLE fi-ini-nome-ab-cli AS CHARACTER FORMAT "X(12)" 
     LABEL "Cliente":R17 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 TOOLTIP "Nome abreviado do cliente inicial." NO-UNDO.

DEFINE VARIABLE fi-ini-nr-nota-fis AS CHARACTER FORMAT "x(16)" 
     LABEL "Nr Nota Fiscal":R17 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 TOOLTIP "N£mero da primeira nota fiscal." NO-UNDO.

DEFINE VARIABLE fi-ini-transp AS CHARACTER FORMAT "X(15)":U 
     LABEL "Transportador" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88 TOOLTIP "Nome abreviado do transportador inicial." NO-UNDO.

DEFINE VARIABLE fi-nome-estabel AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 43.72 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi-nr-pedcli-fin AS CHARACTER FORMAT "x(12)" INITIAL "ZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nr-pedcli-ini AS CHARACTER FORMAT "x(12)" 
     LABEL "Pedido Cliente":R17 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE fi-serie AS CHARACTER FORMAT "x(5)" INITIAL "1" 
     LABEL "S‚rie":R7 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 TOOLTIP "S‚rie da Nota Fiscal." NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-13
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-14
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
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

DEFINE IMAGE IMAGE-4
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-7
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE VARIABLE rs-tp-relat AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Nota Fiscal", 1,
"Pedido", 2
     SIZE 29 BY .75 NO-UNDO.

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
      tt-digita.nr-nota-fis COLUMN-LABEL "Nro da Nota Fiscal"
      tt-digita.nr-pedcli   COLUMN-LABEL "Nro do Pedido"
ENABLE
tt-digita.nr-nota-fis
tt-digita.nr-pedcli
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 76.57 BY 9
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

DEFINE FRAME f-pg-dig
     br-digita AT ROW 1 COL 1
     bt-inserir AT ROW 10 COL 1
     bt-alterar AT ROW 10 COL 16
     bt-retirar AT ROW 10 COL 31
     bt-salvar AT ROW 10 COL 46
     bt-recuperar AT ROW 10 COL 61
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3.31
         SIZE 76.86 BY 10.15.

DEFINE FRAME f-pg-cla
     rs-classif AT ROW 1.67 COL 5.57 HELP
          "Classifica‡Æo para emissÆo do relat¢rio" NO-LABEL
     RECT-10 AT ROW 1.46 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 76.86 BY 10.31
         FONT 1.

DEFINE FRAME f-relat
     bt-executar AT ROW 14.54 COL 3 HELP
          "Dispara a execu‡Æo do relat¢rio"
     bt-cancelar AT ROW 14.54 COL 14 HELP
          "Fechar"
     bt-ajuda AT ROW 14.54 COL 70 HELP
          "Ajuda"
     rt-folder-left AT ROW 2.54 COL 2.14
     RECT-6 AT ROW 13.75 COL 2.14
     RECT-1 AT ROW 14.29 COL 2
     rt-folder-right AT ROW 2.67 COL 80.43
     rt-folder AT ROW 2.5 COL 2
     rt-folder-top AT ROW 2.54 COL 2.14
     im-pg-cla AT ROW 1.5 COL 17.72
     im-pg-dig AT ROW 1.5 COL 49
     im-pg-imp AT ROW 1.5 COL 64.86
     im-pg-par AT ROW 1.5 COL 33.43
     im-pg-sel AT ROW 1.5 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81 BY 15
         DEFAULT-BUTTON bt-executar.

DEFINE FRAME f-pg-par
     fi-peso-max-carga AT ROW 1.88 COL 21.72 COLON-ALIGNED
     tg-impr-param AT ROW 1.92 COL 41.57
     to-enviar-e-mail AT ROW 3.67 COL 3.86
     fi-e-mail-remet AT ROW 3.67 COL 30 COLON-ALIGNED
     fi-assunto-e-mail AT ROW 5.5 COL 12.29 COLON-ALIGNED
     fi-texto-e-mail AT ROW 6.5 COL 13 NO-LABEL
     " Dados da Mensagem" VIEW-AS TEXT
          SIZE 16 BY .75 AT ROW 4.71 COL 6
     RECT-13 AT ROW 5 COL 3.86
     RECT-34 AT ROW 1.42 COL 3.86
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 75 BY 10
         FONT 1.

DEFINE FRAME f-pg-sel
     rs-tp-relat AT ROW 1.42 COL 21 NO-LABEL
     fi-cod-estabel AT ROW 2.29 COL 16 COLON-ALIGNED
     fi-nome-estabel AT ROW 2.29 COL 21.29 COLON-ALIGNED NO-LABEL
     fi-serie AT ROW 3.29 COL 16 COLON-ALIGNED HELP
          "S‚rie da nota fiscal"
     fi-nr-pedcli-ini AT ROW 4.29 COL 16 COLON-ALIGNED HELP
          "N£mero do pedido do cliente" WIDGET-ID 8
     fi-nr-pedcli-fin AT ROW 4.29 COL 48.14 COLON-ALIGNED HELP
          "N£mero do pedido do cliente" NO-LABEL WIDGET-ID 10
     fi-ini-nr-nota-fis AT ROW 5.29 COL 16 COLON-ALIGNED HELP
          "N£mero da nota fiscal"
     fi-fin-nr-nota-fis AT ROW 5.29 COL 48.14 COLON-ALIGNED HELP
          "N£mero da nota fiscal" NO-LABEL
     fi-ini-transp AT ROW 6.29 COL 16 COLON-ALIGNED
     fi-fin-transp AT ROW 6.29 COL 48.43 COLON-ALIGNED NO-LABEL
     fi-ini-estado AT ROW 7.29 COL 16 COLON-ALIGNED
     fi-fin-estado AT ROW 7.29 COL 48.57 COLON-ALIGNED NO-LABEL
     fi-ini-nome-ab-cli AT ROW 8.29 COL 16 COLON-ALIGNED HELP
          "Nome abreviado do cliente/fornecedor"
     fi-fin-nome-ab-cli AT ROW 8.29 COL 48.57 COLON-ALIGNED HELP
          "Nome abreviado do cliente/fornecedor" NO-LABEL
     fi-ini-no-ab-reppri AT ROW 9.29 COL 16 COLON-ALIGNED
     fi-fin-no-ab-reppri AT ROW 9.29 COL 48.57 COLON-ALIGNED HELP
          "Nome abreviado do representante principal" NO-LABEL
     fi-ini-corte-com AT ROW 10.29 COL 16 COLON-ALIGNED
     fi-fin-corte-com AT ROW 10.29 COL 48.57 COLON-ALIGNED NO-LABEL
     IMAGE-1 AT ROW 6.29 COL 36.43
     IMAGE-13 AT ROW 7.29 COL 36.43
     IMAGE-14 AT ROW 7.29 COL 47
     IMAGE-2 AT ROW 6.29 COL 47
     IMAGE-33 AT ROW 8.29 COL 36.43
     IMAGE-34 AT ROW 8.29 COL 47
     IMAGE-35 AT ROW 9.29 COL 36.43
     IMAGE-36 AT ROW 9.29 COL 47
     IMAGE-37 AT ROW 10.29 COL 36.43
     IMAGE-38 AT ROW 10.29 COL 47
     IMAGE-4 AT ROW 5.29 COL 36.43
     IMAGE-7 AT ROW 5.29 COL 47
     IMAGE-39 AT ROW 4.29 COL 36.43 WIDGET-ID 12
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
         TITLE              = "Mapa de Separa‡Æo/Carregamento"
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
   FRAME-NAME                                                           */
/* SETTINGS FOR FRAME f-pg-dig
                                                                        */
/* BROWSE-TAB br-digita 1 f-pg-dig */
/* SETTINGS FOR BUTTON bt-alterar IN FRAME f-pg-dig
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-retirar IN FRAME f-pg-dig
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-salvar IN FRAME f-pg-dig
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
/* SETTINGS FOR FILL-IN fi-assunto-e-mail IN FRAME f-pg-par
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FILL-IN fi-e-mail-remet IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR fi-texto-e-mail IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME f-pg-sel
                                                                        */
/* SETTINGS FOR FILL-IN fi-fin-corte-com IN FRAME f-pg-sel
   1                                                                    */
/* SETTINGS FOR FILL-IN fi-fin-estado IN FRAME f-pg-sel
   1                                                                    */
/* SETTINGS FOR FILL-IN fi-fin-no-ab-reppri IN FRAME f-pg-sel
   1                                                                    */
/* SETTINGS FOR FILL-IN fi-fin-nome-ab-cli IN FRAME f-pg-sel
   1                                                                    */
/* SETTINGS FOR FILL-IN fi-fin-nr-nota-fis IN FRAME f-pg-sel
   1                                                                    */
/* SETTINGS FOR FILL-IN fi-fin-transp IN FRAME f-pg-sel
   1                                                                    */
/* SETTINGS FOR FILL-IN fi-ini-corte-com IN FRAME f-pg-sel
   1                                                                    */
/* SETTINGS FOR FILL-IN fi-ini-estado IN FRAME f-pg-sel
   1                                                                    */
/* SETTINGS FOR FILL-IN fi-ini-no-ab-reppri IN FRAME f-pg-sel
   1                                                                    */
/* SETTINGS FOR FILL-IN fi-ini-nome-ab-cli IN FRAME f-pg-sel
   1                                                                    */
/* SETTINGS FOR FILL-IN fi-ini-nr-nota-fis IN FRAME f-pg-sel
   1                                                                    */
/* SETTINGS FOR FILL-IN fi-ini-transp IN FRAME f-pg-sel
   1                                                                    */
/* SETTINGS FOR FILL-IN fi-nome-estabel IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nr-pedcli-fin IN FRAME f-pg-sel
   2                                                                    */
/* SETTINGS FOR FILL-IN fi-nr-pedcli-ini IN FRAME f-pg-sel
   2                                                                    */
/* SETTINGS FOR FILL-IN fi-serie IN FRAME f-pg-sel
   1                                                                    */
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
ON END-ERROR OF w-relat /* Mapa de Separa‡Æo/Carregamento */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON WINDOW-CLOSE OF w-relat /* Mapa de Separa‡Æo/Carregamento */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-digita
&Scoped-define FRAME-NAME f-pg-dig
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
        if input frame f-pg-sel rs-tp-relat  = 1 THEN
           display tt-digita.nr-nota-fis
                   with browse br-digita. 
        else
           display tt-digita.nr-pedcli
                   with browse br-digita. 
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
ON ROW-ENTRY OF br-digita IN FRAME f-pg-dig
DO:
   /*:T trigger para inicializar campos da temp table de digita‡Æo */
   /*
   if  br-digita:new-row in frame f-pg-dig then do:
       assign tt-digita.exemplo:screen-value in browse br-digita = string(today, "99/99/9999").
   end.
   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON ROW-LEAVE OF br-digita IN FRAME f-pg-dig
DO:
    /*:T  aqui que a grava‡Æo da linha da temp-table ‚ efetivada.
       Por‚m as valida‡äes dos registros devem ser feitas na procedure pi-executar,
       no local indicado pelo coment rio */
    
    if br-digita:NEW-ROW in frame f-pg-dig then 
    do transaction on error undo, return no-apply:
        create tt-digita.
        if input frame f-pg-sel rs-tp-relat  = 1 THEN
           assign input browse br-digita tt-digita.nr-nota-fis.
        else
           assign input browse br-digita tt-digita.nr-pedcli.
        ASSIGN input browse br-digita tt-digita.nr-pedcli.
        br-digita:CREATE-RESULT-LIST-ENTRY() in frame f-pg-dig.
    end.
    else do transaction on error undo, return no-apply:
        if input frame f-pg-sel rs-tp-relat  = 1 THEN
           assign input browse br-digita tt-digita.nr-nota-fis.
        else 
           assign input browse br-digita tt-digita.nr-pedcli.
    end.
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
   apply 'entry':U to tt-digita.nr-nota-fis in browse br-digita. 
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
    
    if num-results("br-digita":U) > 0 then
       br-digita:INSERT-ROW("after":U) in frame f-pg-dig.
    else do transaction:
       create tt-digita.
        
       open query br-digita for each tt-digita.

       if input frame f-pg-sel rs-tp-relat  = 1 then
          apply "entry":U to tt-digita.nr-nota-fis in browse br-digita. 
       else 
          apply "entry":U to tt-digita.nr-pedcli in browse br-digita. 

    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-recuperar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-recuperar w-relat
ON CHOOSE OF bt-recuperar IN FRAME f-pg-dig /* Recuperar */
DO:
    {include/i-rprcd.i}
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
   {include/i-rpsvd.i}
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
  {include/zoomvar.i &prog-zoom = adzoom/z01ad107.w
                     &campo     = fi-cod-estabel
                     &campozoom = cod-estabel
                     &FRAME     = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME fi-e-mail-remet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-e-mail-remet w-relat
ON LEAVE OF fi-e-mail-remet IN FRAME f-pg-par /* E-mail remetente */
DO:
   IF NOT(INPUT FRAME {&FRAME-NAME} fi-e-mail-remet MATCHES "*@*") THEN DO:
      MESSAGE "A informa‡Æo nÆo parece ser um endere‡o de e-mail v lido."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY 'entry' TO fi-e-mail-remet IN FRAME f-pg-par.
      RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-sel
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


&Scoped-define SELF-NAME fi-fin-nome-ab-cli
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fin-nome-ab-cli w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-fin-nome-ab-cli IN FRAME f-pg-sel
DO:
   {include/zoomvar.i &prog-zoom = adzoom/z02ad098.w
                      &campo     = fi-fin-nome-ab-cli
                      &campozoom = nome-abrev
                      &FRAME     = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-fin-transp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fin-transp w-relat
ON LEFT-MOUSE-DBLCLICK OF fi-fin-transp IN FRAME f-pg-sel
DO:
    {include/zoomvar.i &prog-zoom = adzoom/z01ad268.w
                     &campo       = fi-fin-transp
                     &campozoom   = nome-abrev
                     &FRAME       = f-pg-sel}
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


&Scoped-define SELF-NAME fi-ini-nome-ab-cli
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-nome-ab-cli w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ini-nome-ab-cli IN FRAME f-pg-sel /* Cliente */
DO:
   {include/zoomvar.i &prog-zoom = adzoom/z02ad098.w
                      &campo     = fi-ini-nome-ab-cli
                      &campozoom = nome-abrev
                      &FRAME     = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ini-transp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-transp w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ini-transp IN FRAME f-pg-sel /* Transportador */
DO:
    {include/zoomvar.i &prog-zoom=adzoom/z01ad268.w
                     &campo=fi-ini-transp
                     &campozoom=nome-abrev
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


&Scoped-define SELF-NAME im-pg-dig
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-dig w-relat
ON MOUSE-SELECT-CLICK OF im-pg-dig IN FRAME f-relat
DO:
  run pi-troca-pagina.
  tt-digita.nr-nota-fis:VISIBLE IN BROWSE br-digita  = NO.  
  tt-digita.nr-pedcli:VISIBLE IN BROWSE br-digita    = NO.  
  IF INPUT FRAME f-pg-sel rs-tp-relat  = 1 THEN
     tt-digita.nr-nota-fis:VISIBLE IN BROWSE br-digita  = YES.  
  ELSE
     tt-digita.nr-pedcli:VISIBLE IN BROWSE br-digita    = YES.  


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
  RUN pi-troca-pagina.
  /* ASSIGN rs-tp-relat:SCREEN-VALUE IN FRAME f-pg-sel = '1'. */
  ENABLE {&list-1} WITH FRAME f-pg-sel.
  DISABLE {&list-2} WITH FRAME f-pg-sel.
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
&Scoped-define SELF-NAME rs-tp-relat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-tp-relat w-relat
ON ENTRY OF rs-tp-relat IN FRAME f-pg-sel
DO:
    /*
  ASSIGN rs-tp-relat:SCREEN-VALUE = '1'.
  ENABLE {&list-1} WITH FRAME f-pg-sel.
  DISABLE {&list-2} WITH FRAME f-pg-sel.
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-tp-relat w-relat
ON VALUE-CHANGED OF rs-tp-relat IN FRAME f-pg-sel
DO:
   FOR EACH tt-digita.
       DELETE tt-digita.
   END.
   {&OPEN-QUERY-br-digita}

   IF SELF:INPUT-VALUE = 2 THEN DO.
      ENABLE {&list-2} WITH FRAME f-pg-sel.
      DISABLE {&list-1} WITH FRAME f-pg-sel.

      to-enviar-e-mail:SENSITIVE IN FRAME f-pg-par = NO.
      
      ASSIGN rs-classif:SCREEN-VALUE IN FRAME f-pg-cla = '6'.
      /*
      rs-classif:DISABLE(ENTRY( 1,(rs-classif:RADIO-BUTTONS IN FRAME f-pg-cla))).
      rs-classif:DISABLE(ENTRY( 3,(rs-classif:RADIO-BUTTONS IN FRAME f-pg-cla))).
      rs-classif:DISABLE(ENTRY( 5,(rs-classif:RADIO-BUTTONS IN FRAME f-pg-cla))).
      rs-classif:DISABLE(ENTRY( 9,(rs-classif:RADIO-BUTTONS IN FRAME f-pg-cla))).
      rs-classif:DISABLE(ENTRY(11,(rs-classif:RADIO-BUTTONS IN FRAME f-pg-cla))).
      */
      APPLY 'entry' TO fi-nr-pedcli-ini.
      RETURN NO-APPLY.

      /* APPLY "MOUSE-SELECT-CLICK":U TO im-pg-dig IN FRAME f-relat. */
   END.
   ELSE DO.
      ENABLE {&list-1} WITH FRAME f-pg-sel.
      DISABLE {&list-2} WITH FRAME f-pg-sel.
      
      to-enviar-e-mail:SENSITIVE IN FRAME f-pg-par = YES.

      ASSIGN rs-classif:SCREEN-VALUE IN FRAME f-pg-cla = '6'.
      /*
      rs-classif:ENABLE(ENTRY( 1,(rs-classif:RADIO-BUTTONS IN FRAME f-pg-cla))).
      rs-classif:ENABLE(ENTRY( 3,(rs-classif:RADIO-BUTTONS IN FRAME f-pg-cla))).
      rs-classif:ENABLE(ENTRY( 5,(rs-classif:RADIO-BUTTONS IN FRAME f-pg-cla))).
      rs-classif:ENABLE(ENTRY( 9,(rs-classif:RADIO-BUTTONS IN FRAME f-pg-cla))).
      rs-classif:ENABLE(ENTRY(11,(rs-classif:RADIO-BUTTONS IN FRAME f-pg-cla))).
      */
      APPLY 'entry' TO fi-cod-estabel.
      RETURN NO-APPLY.
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
            fi-texto-e-mail:SENSITIVE IN FRAME f-pg-par = YES
            fi-e-mail-remet:SENSITIVE IN FRAME f-pg-par = YES.
             
     ASSIGN fi-e-mail-remet:SCREEN-VALUE IN FRAME f-pg-par = "expedicao@teartextil.com.br"
            fi-assunto-e-mail:SCREEN-VALUE IN FRAME f-pg-par = "Solicita‡Æo de Coleta".

     ASSIGN fi-texto-e-mail:SCREEN-VALUE IN FRAME f-pg-par = 
            "Solicitamos proceder coleta de mercadorias para transporte, conforme anexo:" +
            CHR(13) + CHR(13) + "Atenciosamente," + CHR(13) + CHR(13) +
            "TEAR TEXTIL INDUSTRIA E COMERCIO LTDA." + CHR(13) + 
            "Av.General David Sarnoff, 5005" + CHR(13) + 
            "Fone: (31) 2191-4208" + CHR(13) +
            "Setor de Expedi‡Æo.". 
  END.
  ELSE DO.
     ASSIGN fi-assunto-e-mail:SENSITIVE IN FRAME f-pg-par = NO 
            fi-texto-e-mail:SENSITIVE IN FRAME f-pg-par = NO
            fi-e-mail-remet:SENSITIVE IN FRAME f-pg-par = NO.

     ASSIGN fi-e-mail-remet:SCREEN-VALUE IN FRAME f-pg-par = ""
            fi-assunto-e-mail:SCREEN-VALUE IN FRAME f-pg-par = ""
            fi-texto-e-mail:SCREEN-VALUE IN FRAME f-pg-par = "".
  END.
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

{utp/ut9000.i "ESSP0138" "2.04.00.000"}

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

    RUN esapi/busca-estab-usuar.p (INPUT c-seg-usuario,
                                   OUTPUT c-cod-estabel).
    IF c-cod-estabel = '' OR c-cod-estabel = '0' THEN 
       ASSIGN c-cod-estabel = '1'.
    ASSIGN fi-cod-estabel:SCREEN-VALUE IN FRAME f-pg-sel = c-cod-estabel.
    FIND estabelec WHERE
         estabelec.cod-estabel = c-cod-estabel 
         NO-LOCK NO-ERROR.
    IF AVAIL estabelec THEN
       ASSIGN fi-cod-estabel:SCREEN-VALUE IN FRAME f-pg-sel  = c-cod-estabel
              fi-nome-estabel:SCREEN-VALUE IN FRAME f-pg-sel = TRIM(estabelec.cidade) + " / " + TRIM(estabelec.bairro).

    FIND FIRST para-fat NO-LOCK NO-ERROR.

    ASSIGN fi-serie:SCREEN-VALUE IN FRAME f-pg-sel          = para-fat.serie-pad
           fi-peso-max-carga:SCREEN-VALUE IN FRAME f-pg-par = "12000".

    FIND FIRST nota-fiscal WHERE
               nota-fiscal.cod-estabel = fi-cod-estabel:SCREEN-VALUE IN FRAME f-pg-sel AND
               nota-fiscal.serie       = para-fat.serie-pad
               NO-LOCK NO-ERROR.
    IF AVAIL nota-fiscal THEN
       ASSIGN fi-ini-nr-nota-fis:SCREEN-VALUE IN FRAME f-pg-sel = nota-fiscal.nr-nota-fis.
    ELSE
       ASSIGN fi-ini-nr-nota-fis:SCREEN-VALUE IN FRAME f-pg-sel = "".

    FIND LAST nota-fiscal WHERE
              nota-fiscal.cod-estabel = fi-cod-estabel:SCREEN-VALUE IN FRAME f-pg-sel AND
              nota-fiscal.serie       = para-fat.serie-pad
              NO-LOCK NO-ERROR.
    IF AVAIL nota-fiscal THEN
       ASSIGN fi-fin-nr-nota-fis:SCREEN-VALUE IN FRAME f-pg-sel = nota-fiscal.nr-nota-fis.
    ELSE
       ASSIGN fi-fin-nr-nota-fis:SCREEN-VALUE IN FRAME f-pg-sel = "".

    FIND estabelec WHERE
         estabelec.cod-estabel = fi-cod-estabel:SCREEN-VALUE IN FRAME f-pg-sel NO-LOCK NO-ERROR.
    
    IF AVAIL estabelec THEN
       ASSIGN fi-nome-estabel:SCREEN-VALUE IN FRAME f-pg-sel = estabelec.nome.

    {include/i-rpmbl.i}

    fi-cod-estabel:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-ini-transp:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-fin-transp:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-ini-nome-ab-cli:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-fin-nome-ab-cli:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-ini-no-ab-reppri:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-fin-no-ab-reppri:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-ini-corte-com:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-fin-corte-com:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.

    IF NOT THIS-PROCEDURE:PERSISTENT THEN
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
  ENABLE im-pg-cla im-pg-dig im-pg-imp im-pg-par im-pg-sel bt-executar 
         bt-cancelar bt-ajuda 
      WITH FRAME f-relat IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}
  DISPLAY rs-tp-relat fi-cod-estabel fi-nome-estabel fi-serie fi-nr-pedcli-ini 
          fi-nr-pedcli-fin fi-ini-nr-nota-fis fi-fin-nr-nota-fis fi-ini-transp 
          fi-fin-transp fi-ini-estado fi-fin-estado fi-ini-nome-ab-cli 
          fi-fin-nome-ab-cli fi-ini-no-ab-reppri fi-fin-no-ab-reppri 
          fi-ini-corte-com fi-fin-corte-com 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  ENABLE IMAGE-1 IMAGE-13 IMAGE-14 IMAGE-2 IMAGE-33 IMAGE-34 IMAGE-35 IMAGE-36 
         IMAGE-37 IMAGE-38 IMAGE-4 IMAGE-7 IMAGE-39 rs-tp-relat fi-cod-estabel 
         fi-serie fi-nr-pedcli-ini fi-nr-pedcli-fin fi-ini-nr-nota-fis 
         fi-fin-nr-nota-fis fi-ini-transp fi-fin-transp fi-ini-estado 
         fi-fin-estado fi-ini-nome-ab-cli fi-fin-nome-ab-cli 
         fi-ini-no-ab-reppri fi-fin-no-ab-reppri fi-ini-corte-com 
         fi-fin-corte-com 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}
  DISPLAY rs-classif 
      WITH FRAME f-pg-cla IN WINDOW w-relat.
  ENABLE RECT-10 rs-classif 
      WITH FRAME f-pg-cla IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-cla}
  DISPLAY rs-destino c-arquivo rs-execucao 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  ENABLE RECT-7 RECT-9 rs-destino bt-arquivo bt-config-impr c-arquivo 
         rs-execucao 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}
  DISPLAY fi-peso-max-carga tg-impr-param to-enviar-e-mail fi-e-mail-remet 
          fi-texto-e-mail 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  ENABLE RECT-13 RECT-34 fi-peso-max-carga tg-impr-param to-enviar-e-mail 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-par}
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
    
    for each tt-digita no-lock:
        assign r-tt-digita = rowid(tt-digita).
        
        /*:T Valida‡Æo de duplicidade de registro na temp-table tt-digita */
        if input frame f-pg-sel rs-tp-relat  = 1 then do: /* NOTAS FISCAIS */
           find first b-tt-digita where
                      b-tt-digita.nr-nota-fis  = tt-digita.nr-nota-fis and 
                      rowid(b-tt-digita)      <> rowid(tt-digita) no-lock no-error.
           if avail b-tt-digita then do:
               apply "MOUSE-SELECT-CLICK":U to im-pg-dig in frame f-relat.
               reposition br-digita to rowid rowid(b-tt-digita).
            
               run utp/ut-msgs.p (input "show":U, input 108, input "").
               apply "ENTRY":U to tt-digita.nr-nota-fis in browse br-digita.
            
               return error.
           end.

           /*:T Valida‡Æo de registro da temp-table tt-digita x nota-fiscal */
           find nota-fiscal where
                nota-fiscal.cod-estabel = INPUT FRAME f-pg-sel fi-cod-estabel and 
                nota-fiscal.serie       = INPUT FRAME f-pg-sel fi-serie       and 
                nota-fiscal.nr-nota-fis = tt-digita.nr-nota-fis no-lock no-error.
           if not avail nota-fiscal then do:
               MESSAGE "Nota Fiscal - Estab:" INPUT FRAME f-pg-sel fi-cod-estabel
                                     "S‚rie:" INPUT FRAME f-pg-sel fi-serie
                                     "N£mero:" tt-digita.nr-nota-fis
                       "- NÆo cadastrada."
                       VIEW-AS ALERT-BOX.
               apply "ENTRY":U to tt-digita.nr-nota-fis in browse br-digita.
               return error.
           end.
        end.
        else do:
           find first b-tt-digita where
                      b-tt-digita.nr-nota-fis  = tt-digita.nr-pedcli and 
                      rowid(b-tt-digita)      <> rowid(tt-digita) no-lock no-error.
           if avail b-tt-digita then do:
               apply "MOUSE-SELECT-CLICK":U to im-pg-dig in frame f-relat.
               reposition br-digita to rowid rowid(b-tt-digita).

               run utp/ut-msgs.p (input "show":U, input 108, input "").
               apply "ENTRY":U to tt-digita.nr-pedcli in browse br-digita.

               return error.
           end.

           /*:T Valida‡Æo de registro da temp-table tt-digita x nota-fiscal */
           find ped-venda where
               /* ped-venda.cod-estabel = INPUT FRAME f-pg-sel fi-cod-estabel and  */
                ped-venda.nr-pedcli   = tt-digita.nr-pedcli no-lock no-error.
           if not avail ped-venda then do:
               MESSAGE "Pedido Venda - Estab:" INPUT FRAME f-pg-sel fi-cod-estabel
                                     "N£mero:" tt-digita.nr-pedcli
                       "- NÆo cadastrado."
                       VIEW-AS ALERT-BOX.
               apply "ENTRY":U to tt-digita.nr-pedcli in browse br-digita.
               return error.
           end.
        end.
    end.
    /*:T Coloque aqui as valida‡äes das outras p ginas, lembrando que elas devem 
       apresentar uma mensagem de erro cadastrada, posicionar na p gina com 
       problemas e colocar o focus no campo com problemas */
    
    /*:T Aqui sÆo gravados os campos da temp-table que ser  passada como parƒmetro
       para o programa RP.P */
    
    create tt-param.
    assign tt-param.usuario          = c-seg-usuario
           tt-param.destino          = input frame f-pg-imp rs-destino
           tt-param.data-exec        = today
           tt-param.hora-exec        = time
           tt-param.classifica       = input frame f-pg-cla rs-classif
           tt-param.desc-classifica  = entry((tt-param.classifica - 1) * 2 + 1, 
                                              rs-classif:radio-buttons in frame f-pg-cla)
           tt-param.cod-estabel      = INPUT FRAME f-pg-sel fi-cod-estabel
           tt-param.serie            = INPUT FRAME f-pg-sel fi-serie
           tt-param.nr-pedcli-ini    = INPUT FRAME f-pg-sel fi-nr-pedcli-ini
           tt-param.nr-pedcli-fin    = INPUT FRAME f-pg-sel fi-nr-pedcli-fin
           tt-param.nr-nota-fis-ini  = INPUT FRAME f-pg-sel fi-ini-nr-nota-fis
           tt-param.nr-nota-fis-fin  = INPUT FRAME f-pg-sel fi-fin-nr-nota-fis
           tt-param.transp-ini       = INPUT FRAME f-pg-sel fi-ini-transp
           tt-param.transp-fin       = INPUT FRAME f-pg-sel fi-fin-transp
           tt-param.estado-ini       = INPUT FRAME f-pg-sel fi-ini-estado
           tt-param.estado-fin       = INPUT FRAME f-pg-sel fi-fin-estado
           tt-param.nome-ab-cli-ini  = INPUT FRAME f-pg-sel fi-ini-nome-ab-cli
           tt-param.nome-ab-cli-fin  = INPUT FRAME f-pg-sel fi-fin-nome-ab-cli
           tt-param.no-ab-reppri-ini = INPUT FRAME f-pg-sel fi-ini-no-ab-reppri
           tt-param.no-ab-reppri-fin = INPUT FRAME f-pg-sel fi-fin-no-ab-reppri
           tt-param.corte-com-ini    = INPUT FRAME f-pg-sel fi-ini-corte-com
           tt-param.corte-com-fin    = INPUT FRAME f-pg-sel fi-fin-corte-com
           tt-param.tipo-rel         = INPUT FRAME f-pg-sel rs-tp-relat
           tt-param.peso-max-carga   = INPUT FRAME f-pg-par fi-peso-max-carga
           tt-param.enviar-e-mail    = INPUT FRAME f-pg-par to-enviar-e-mail
           tt-param.e-mail-remet     = INPUT FRAME f-pg-par fi-e-mail-remet
           tt-param.subject-e-mail   = INPUT FRAME f-pg-par fi-assunto-e-mail
           tt-param.texto-e-mail     = INPUT FRAME f-pg-par fi-texto-e-mail
           tt-param.impr-param       = INPUT FRAME f-pg-par tg-impr-param.
    
    if tt-param.destino = 1 
    then assign tt-param.arquivo = "".
    else if  tt-param.destino = 2 
         then assign tt-param.arquivo = input frame f-pg-imp c-arquivo.
         else assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp":U.
    
    /*:T Coloque aqui a l¢gica de grava‡Æo dos demais campos que devem ser passados
       como parƒmetros para o programa RP.P, atrav‚s da temp-table tt-param */
    
    
    
    /*:T Executar do programa RP.P que ir  criar o relat¢rio */
    {include/i-rpexb.i}
    
    SESSION:SET-WAIT-STATE("general":U).
    
    {include/i-rprun.i esrp/essp0138rp.p}

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

