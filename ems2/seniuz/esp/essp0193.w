&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME w-cadsim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-cadsim 
/*:T *******************************************************************************
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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
define variable wh-imprime as handle no-undo.


def var wh-pesquisa as widget-handle.
def new global shared var l-implanta as logical init no.
def new global shared var wh-composi    as widget-handle no-undo.
def new global shared var wh-window  as handle no-undo.
def new global shared var adm-broker-hdl as handle no-undo.


/* Local Variable Definitions ---                                       */
DEF TEMP-TABLE tt-clientes LIKE emitente
    FIELD repres           LIKE repres.nome-abrev
    FIELD tot-pend         AS DEC FORMAT ">>,>>>,>>9.99" 
    FIELD tot-aceitos      AS DEC FORMAT ">>,>>>,>>9.99" 
    FIELD tot-aprovados    AS DEC FORMAT ">>,>>>,>>9.99" 
    FIELD tit-vencidos     AS DEC FORMAT ">>,>>>,>>9.99" 
    FIELD tit-a-vencer     AS DEC FORMAT ">>,>>>,>>9.99"
    FIELD cheques-compsar  AS DEC FORMAT ">>,>>>,>>9.99"
    FIELD cheques-dev      AS DEC FORMAT ">>,>>>,>>9.99"
    FIELD saldo            AS DEC FORMAT "->>,>>>,>>9.99"
    FIELD tot-ped-ccred    AS DEC FORMAT ">>,>>>,>>9.99"
    FIELD tot-ped-scred    AS DEC FORMAT ">>,>>>,>>9.99"
    FIELD tot-ped-reav     AS DEC FORMAT ">>,>>>,>>9.99"
    FIELD restricao        AS LOG
    FIELD motivo           AS CHAR FORMAT "x(320)"
    FIELD desc-bloq-cr     LIKE ped-venda.desc-bloq-cr
    FIELD ordem            AS INT
    INDEX indice1 ordem repres nome-abrev.

DEF TEMP-TABLE tt-ped-venda LIKE ped-venda
    FIELD visualiza         AS LOG
    FIELD tipo-pedido       LIKE ped-venda-ext.tp-pedido
    FIELD vl-aberto         LIKE ped-venda.vl-tot-ped
    FIELD vl-reservado      LIKE ped-venda.vl-tot-ped
    FIELD usuario           AS CHAR
    FIELD marca             AS CHAR
    INDEX indice1 IS PRIMARY dt-entrega.

DEF TEMP-TABLE tt-titulos LIKE titulo
    FIELD visualiza AS LOG.

DEF TEMP-TABLE tt-cheques LIKE cheque
    FIELD visualiza AS LOG.

DEF BUFFER b-tt-ped-venda FOR tt-ped-venda.
DEF BUFFER b-tt-clientes  FOR tt-clientes.

DEF NEW GLOBAL SHARED VAR gr-ob-etiqueta AS ROWID NO-UNDO.
DEF VAR rw-cliente            AS ROWID.
DEF VAR h-acomp               AS HANDLE.
DEF VAR h-query               AS HANDLE. 
DEF VAR c-dia                 AS CHAR.
DEF VAR da-dt-entrega-fin     AS DATE.
DEF VAR c-lotes               AS CHAR.
DEF VAR c-sit-ped             AS CHAR FORMAT "x(20)".
DEF VAR c-sit-cli             AS CHAR FORMAT "x(20)".
DEF VAR c-sit-aval            AS CHAR.
DEF VAR c-modalidade          AS CHAR FORMAT "x(20)".
DEF VAR c-historico           AS CHAR.
DEF VAR de-tot-ped            AS DEC.
DEF VAR de-tot-res            AS DEC.
DEF VAR de-tot-aceitos        AS DEC.
DEF VAR de-tot-aprovados      AS DEC.
DEF VAR de-tot-pend           AS DEC.
DEF VAR de-tot-aprov          AS DEC.
DEF VAR de-tit-vencidos       AS DEC.
DEF VAR de-tit-a-vencer       AS DEC.
DEF VAR de-cheques-dev        AS DEC.
DEF VAR de-cheques-compsar    AS DEC.
DEF VAR c-nr-pedcli           LIKE ped-venda.nr-pedcli.
DEF VAR c-nome-abrev          LIKE emitente.nome-abrev.
DEF VAR c-texto-log           AS   CHAR FORMAT "x(100)".
DEF VAR i-cor-bg              AS   INT INIT 15.
DEF VAR i-cor-fg              AS   INT INIT 12.

DEF VAR i-lin       AS INT INITIAL 99.
DEF VAR i-pag       AS INT INITIAL  1.
DEF VAR c-mensagem  AS CHAR.
DEF VAR c-arq-email AS CHAR FORMAT "x(45)".

DEF VAR i-sit-ped-ini   LIKE ped-venda.cod-sit-ped.
DEF VAR i-sit-ped-fin   LIKE ped-venda.cod-sit-ped.

/* Variaveis da Rotina de Impress∆o */
DEFINE VAR i-saida      AS INT.
DEFINE VAR c-saida      AS CHAR.
DEFINE VAR i-num-copias AS INT.

/* Aprovaá∆o/Reprovaá∆o de CrÇdito */
DEF VAR c-quem-aprovou LIKE ped-venda.quem-aprovou. 
DEF VAR da-data-aprov  LIKE ped-venda.dt-aprov.   
DEF VAR c-motivo       LIKE ped-venda.motivo.       
DEF VAR c-motivo-no    LIKE ped-venda.motivo.    
DEF VAR i-tp-aprov     AS   INT.

/* Global Variable Definitions ---                                       */
DEFINE NEW GLOBAL SHARED VAR gr-ped-venda  AS ROWID   NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR gr-emitente   AS ROWID   NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR    NO-UNDO.

/* ***************************  Definitions  ************************** */
/* INICIO DA DEFINIÄ«O DAS TABELAS TEMPORARIAS; PARA A CHAMADA DO RELATORIO ESPD0002RP.P */

DEFINE TEMP-TABLE tt-param NO-UNDO
       FIELD destino          AS INTEGER
       FIELD arquivo          AS CHAR FORMAT "x(35)"
       FIELD usuario          AS CHAR FORMAT "x(12)"
       FIELD data-exec        AS DATE
       FIELD hora-exec        AS INTEGER
       FIELD classifica       AS INTEGER
       FIELD pedido-ini       LIKE ped-venda.nr-pedido 
       FIELD pedido-fin       LIKE ped-venda.nr-pedido
       FIELD dt-emissao-ini   LIKE ped-venda.dt-emissao
       FIELD dt-emissao-fin   LIKE ped-venda.dt-emissao
       FIELD dt-entrega-ini   LIKE ped-venda.dt-entrega   
       FIELD dt-entrega-fin   LIKE ped-venda.dt-entrega   
       FIELD cod-emit-ini     LIKE ped-venda.cod-emitente 
       FIELD cod-emit-fin     LIKE ped-venda.cod-emitente 
       FIELD no-ab-reppri-ini LIKE ped-venda.no-ab-reppri 
       FIELD no-ab-reppri-fin LIKE ped-venda.no-ab-reppri
       FIELD nome-transp-ini  LIKE ped-venda.nome-transp
       FIELD nome-transp-fin  LIKE ped-venda.nome-transp
       FIELD corte-comerc-ini LIKE ped-item-ext.corte-comerc
       FIELD corte-comerc-fin LIKE ped-item-ext.corte-comerc
       FIELD so-indigo        AS   LOG
       FIELD exc-indigo       AS   LOG
       FIELD it-codigo        LIKE item.it-codigo EXTENT 10
       FIELD sit-total        AS   LOG
       FIELD sit-aberto       AS   LOG 
       FIELD sit-parcial      AS   LOG
       FIELD sit-pendentes    AS   LOG 
       FIELD sit-suspensos    AS   LOG 
       FIELD sit-cancelados   AS   LOG
       FIELD cond-credito     AS   CHAR FORMAT "x"
       FIELD cond-pagto       AS   CHAR FORMAT "x"
       FIELD mercado          AS   CHAR FORMAT "x"
       FIELD tp-pedido        LIKE ped-venda.tp-pedido
       FIELD aceita-parc      AS   CHAR FORMAT "x"
       FIELD qtd-minima       AS   DEC FORMAT ">>>,>>>,>>9.99" 
       FIELD perc-minres      AS   DEC FORMAT ">>9.99"
       FIELD min-it-ares      AS   INT FORMAT ">>>9" 
       FIELD max-it-ares      AS   INT FORMAT ">>>9"
       FIELD it-reservados    AS   LOG
       FIELD nr-coletor       AS   INT.

DEF TEMP-TABLE tt-raw-digita
    FIELD raw-digita      AS RAW.

DEF TEMP-TABLE tt-digita
    FIELD opcao  AS CHAR
    FIELD campo AS CHAR
    FIELD valor AS CHAR.

DEFINE VAR raw-param   AS RAW NO-UNDO.
DEFINE VAR rs-execucao AS INTEGER INITIAL 1. 
DEFINE FRAME f-pg-imp rs-execucao AT ROW 5.75 COL 3 NO-LABEL.
/* FIM DAS DEFINIÄÂES DO RELATORIO ESPD0002RP.P */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad
&Scoped-define BROWSE-NAME br-clientes

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-clientes

/* Definitions for BROWSE br-clientes                                   */
&Scoped-define FIELDS-IN-QUERY-br-clientes tt-clientes.nome-abrev tt-clientes.lim-cred tt-clientes.tot-pend tt-clientes.tot-aceitos tt-clientes.tot-aprovados tt-clientes.tit-vencidos tt-clientes.tit-a-vencer tt-clientes.cheques-compsar tt-clientes.cheques-dev tt-clientes.saldo tt-clientes.portador fn-modalidade() @ c-modalidade fn-sit-cli() @ c-sit-cli   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-clientes   
&Scoped-define SELF-NAME br-clientes
&Scoped-define QUERY-STRING-br-clientes FOR EACH tt-clientes NO-LOCK                                  INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-clientes OPEN QUERY {&SELF-NAME} FOR EACH tt-clientes NO-LOCK                                  INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-clientes tt-clientes
&Scoped-define FIRST-TABLE-IN-QUERY-br-clientes tt-clientes


/* Definitions for FRAME f-cad                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-cad ~
    ~{&OPEN-QUERY-br-clientes}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-1 RECT-54 BUTTON-2 RECT-55 rt-button ~
bt-anl-credito fi-nome-abrev-ini fi-nome-abrev-fin bt-dig-cli bt-ex-cli ~
br-clientes bt-vapara bt-equifax bt-cons-cli bt-historico bt-cons-global ~
bt-sintegra bt-receita-federal ed-msg-credito bt-serasa bt-ok bt-ajuda ~
fi-cgc 
&Scoped-Define DISPLAYED-OBJECTS fi-nome-abrev-ini fi-nome-abrev-fin ~
ed-msg-credito fi-cgc 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-4 bt-equifax bt-serasa 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-modalidade w-cadsim 
FUNCTION fn-modalidade RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-sit-cli w-cadsim 
FUNCTION fn-sit-cli RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-cadsim AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-bt-ajuda 
       MENU-ITEM mi-sobre       LABEL "Sobre..."      .


/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ajuda 
     IMAGE-UP FILE "image/im-hel.bmp":U
     LABEL "&Ajuda" 
     SIZE 10.57 BY 1.21
     BGCOLOR 8 .

DEFINE BUTTON bt-anl-credito 
     IMAGE-UP FILE "image/im-aval.bmp":U
     LABEL "Button 3" 
     SIZE 10.57 BY 1.21 TOOLTIP "Processa Analise de CrÇdito".

DEFINE BUTTON bt-cons-cli AUTO-GO 
     IMAGE-UP FILE "image/im-det.bmp":U
     LABEL "" 
     SIZE 10.57 BY 1.38 TOOLTIP "Detalhar Cliente"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-cons-global AUTO-GO 
     IMAGE-UP FILE "image/im-pcust.bmp":U
     LABEL "" 
     SIZE 10.57 BY 1.38 TOOLTIP "An†lise Global de CrÇdito"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-dig-cli 
     IMAGE-UP FILE "image/im-ran.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-ran.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Digita Cliente".

DEFINE BUTTON bt-equifax AUTO-GO 
     IMAGE-UP FILE "image/img-equifax.bmp":U NO-FOCUS
     LABEL "" 
     SIZE 10.57 BY 1.38 TOOLTIP "Consulta EQUIFAX"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-ex-cli 
     IMAGE-UP FILE "image/imt-retira.bmp":U
     IMAGE-INSENSITIVE FILE "image/iit-retira.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Exceto Cliente".

DEFINE BUTTON bt-historico AUTO-GO 
     IMAGE-UP FILE "image/im-hist.bmp":U
     LABEL "" 
     SIZE 10.57 BY 1.38 TOOLTIP "Hist¢rico do Cliente"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-ok AUTO-GO 
     IMAGE-UP FILE "image/im-exi.bmp":U
     LABEL "&OK" 
     SIZE 10.57 BY 1.21
     BGCOLOR 8 .

DEFINE BUTTON bt-receita-federal AUTO-GO 
     IMAGE-UP FILE "image/img-receita-federal.bmp":U
     LABEL "" 
     SIZE 10.57 BY 1.38 TOOLTIP "Copia CNPJ para Receita Federal"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-serasa AUTO-GO 
     IMAGE-UP FILE "image/img-serasa.bmp":U
     LABEL "" 
     SIZE 10.57 BY 1.38 TOOLTIP "Consulta SERASA"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-sintegra AUTO-GO 
     IMAGE-UP FILE "image/img-sintegra.bmp":U
     LABEL "" 
     SIZE 10.57 BY 1.38 TOOLTIP "Copia CNPJ para Sintegra"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-vapara AUTO-GO 
     IMAGE-UP FILE "image/im-vapra.bmp":U
     LABEL "" 
     SIZE 10.57 BY 1.38 TOOLTIP "Posicionar no Cliente"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "image/im-fir.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 1" 
     SIZE 3.57 BY 1.

DEFINE BUTTON BUTTON-2 
     IMAGE-UP FILE "image/im-las.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 2" 
     SIZE 3.72 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE ed-msg-credito AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 94 BY 3
     FGCOLOR 12 FONT 11 NO-UNDO.

DEFINE VARIABLE fi-cgc AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 2 BY .67
     BGCOLOR 7 FGCOLOR 7  NO-UNDO.

DEFINE VARIABLE fi-nome-abrev-fin AS CHARACTER FORMAT "x(12)" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 TOOLTIP "Cliente final." NO-UNDO.

DEFINE VARIABLE fi-nome-abrev-ini AS CHARACTER FORMAT "x(12)" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 TOOLTIP "Cliente inicial." NO-UNDO.

DEFINE RECTANGLE RECT-54
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 12.43 BY 14.25
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-55
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 107.43 BY 1.5
     BGCOLOR 8 .

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 107.43 BY 1.63
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-clientes FOR 
      tt-clientes SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-clientes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-clientes w-cadsim _FREEFORM
  QUERY br-clientes NO-LOCK DISPLAY
      tt-clientes.nome-abrev          COLUMN-LABEL "Cliente"        WIDTH 15 COLUMN-FONT 6
      tt-clientes.lim-cred            COLUMN-LABEL "Lim.Cred"       WIDTH 9  
      tt-clientes.tot-pend            COLUMN-LABEL "Pendentes"      WIDTH 12
      tt-clientes.tot-aceitos         COLUMN-LABEL "Reavaliar"      WIDTH 12
      tt-clientes.tot-aprovados       COLUMN-LABEL "Aprovados"      WIDTH 12
      tt-clientes.tit-vencidos        COLUMN-LABEL "Tit.Vencidos"   WIDTH 12
      tt-clientes.tit-a-vencer        COLUMN-LABEL "Tit.a Vencer"   WIDTH 12
      tt-clientes.cheques-compsar     COLUMN-LABEL "Ch.Compensar"   WIDTH 12
      tt-clientes.cheques-dev         COLUMN-LABEL "Ch.Devolvidos"  WIDTH 12
      tt-clientes.saldo               COLUMN-LABEL "Saldo"          WIDTH 9
      tt-clientes.portador            COLUMN-LABEL "Port"           WIDTH 5
      fn-modalidade() @ c-modalidade  COLUMN-LABEL "Modalidade"     WIDTH 15
      fn-sit-cli() @ c-sit-cli        COLUMN-LABEL "Sit Cliente"    WIDTH 15
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 94 BY 11
         FONT 1
         TITLE "Clientes".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     BUTTON-1 AT ROW 1.29 COL 38.43 NO-TAB-STOP 
     BUTTON-2 AT ROW 1.29 COL 44.29 NO-TAB-STOP 
     bt-anl-credito AT ROW 1.25 COL 98
     fi-nome-abrev-ini AT ROW 1.33 COL 21.14 COLON-ALIGNED HELP
          "Nome abreviado do cliente" NO-LABEL
     fi-nome-abrev-fin AT ROW 1.33 COL 46.43 COLON-ALIGNED HELP
          "Nome abreviado do cliente" NO-LABEL
     bt-dig-cli AT ROW 1.33 COL 65
     bt-ex-cli AT ROW 1.33 COL 69.29
     br-clientes AT ROW 2.75 COL 2
     bt-vapara AT ROW 3.04 COL 98
     bt-equifax AT ROW 12.46 COL 98
     bt-cons-cli AT ROW 4.5 COL 98
     bt-historico AT ROW 5.96 COL 98
     bt-cons-global AT ROW 8.46 COL 98
     bt-sintegra AT ROW 11 COL 98
     bt-receita-federal AT ROW 13.92 COL 98
     ed-msg-credito AT ROW 14 COL 2 NO-LABEL
     bt-serasa AT ROW 15.38 COL 98
     bt-ok AT ROW 17.46 COL 3.14
     bt-ajuda AT ROW 17.46 COL 97.86
     fi-cgc AT ROW 18 COL 64 COLON-ALIGNED NO-LABEL
     "Cliente:" VIEW-AS TEXT
          SIZE 6.43 BY .54 AT ROW 1.5 COL 16.29
          BGCOLOR 8 FONT 6
     RECT-54 AT ROW 2.75 COL 97
     RECT-55 AT ROW 1.08 COL 2
     rt-button AT ROW 17.25 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 109.57 BY 18.04
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-cadsim ASSIGN
         HIDDEN             = YES
         TITLE              = "Consulta CrÇdito de Clientes - ESSP0193"
         HEIGHT             = 18.04
         WIDTH              = 109.57
         MAX-HEIGHT         = 22
         MAX-WIDTH          = 123.14
         VIRTUAL-HEIGHT     = 22
         VIRTUAL-WIDTH      = 123.14
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-cadsim 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-incsim.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-cadsim
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-cad
   FRAME-NAME L-To-R                                                    */
/* BROWSE-TAB br-clientes bt-ex-cli f-cad */
ASSIGN 
       bt-ajuda:POPUP-MENU IN FRAME f-cad       = MENU POPUP-MENU-bt-ajuda:HANDLE.

/* SETTINGS FOR BUTTON bt-equifax IN FRAME f-cad
   4                                                                    */
/* SETTINGS FOR BUTTON bt-serasa IN FRAME f-cad
   4                                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-cadsim)
THEN w-cadsim:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-clientes
/* Query rebuild information for BROWSE br-clientes
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-clientes NO-LOCK
                                 INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-clientes */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-cadsim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-cadsim w-cadsim
ON END-ERROR OF w-cadsim /* Consulta CrÇdito de Clientes - ESSP0193 */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-cadsim w-cadsim
ON WINDOW-CLOSE OF w-cadsim /* Consulta CrÇdito de Clientes - ESSP0193 */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-clientes
&Scoped-define SELF-NAME br-clientes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-clientes w-cadsim
ON ROW-DISPLAY OF br-clientes IN FRAME f-cad /* Clientes */
DO:
   IF tt-clientes.ordem = 999 THEN
      ASSIGN tt-clientes.nome-abrev:FGCOLOR IN BROWSE br-clientes = 15
             tt-clientes.lim-cred:FGCOLOR IN BROWSE br-clientes = 15
             tt-clientes.tot-pend:FONT IN BROWSE br-clientes = 6
             tt-clientes.tot-pend:BGCOLOR IN BROWSE br-clientes = i-cor-bg
             tt-clientes.tot-pend:FGCOLOR IN BROWSE br-clientes = i-cor-fg
             tt-clientes.tot-aceitos:FONT IN BROWSE br-clientes = 6
             tt-clientes.tot-aceitos:BGCOLOR IN BROWSE br-clientes = i-cor-bg
             tt-clientes.tot-aceitos:FGCOLOR IN BROWSE br-clientes = i-cor-fg
             tt-clientes.tot-aprovados:FONT IN BROWSE br-clientes = 6
             tt-clientes.tot-aprovados:BGCOLOR IN BROWSE br-clientes = i-cor-bg
             tt-clientes.tot-aprovados:FGCOLOR IN BROWSE br-clientes = i-cor-fg
             tt-clientes.tit-vencidos:FONT IN BROWSE br-clientes = 6
             tt-clientes.tit-vencidos:BGCOLOR IN BROWSE br-clientes = i-cor-bg   
             tt-clientes.tit-vencidos:FGCOLOR IN BROWSE br-clientes = i-cor-fg   
             tt-clientes.tit-a-vencer:FONT IN BROWSE br-clientes = 6
             tt-clientes.tit-a-vencer:BGCOLOR IN BROWSE br-clientes = i-cor-bg
             tt-clientes.tit-a-vencer:FGCOLOR IN BROWSE br-clientes = i-cor-fg   
             tt-clientes.cheques-compsar:FONT IN BROWSE br-clientes = 6
             tt-clientes.cheques-compsar:BGCOLOR IN BROWSE br-clientes = i-cor-bg
             tt-clientes.cheques-compsar:FGCOLOR IN BROWSE br-clientes = i-cor-fg
             tt-clientes.cheques-dev:FONT IN BROWSE br-clientes = 6
             tt-clientes.cheques-dev:BGCOLOR IN BROWSE br-clientes = i-cor-bg
             tt-clientes.cheques-dev:FGCOLOR IN BROWSE br-clientes = i-cor-fg
             tt-clientes.saldo:FGCOLOR IN BROWSE br-clientes = 15 
             tt-clientes.portador:FGCOLOR IN BROWSE br-clientes = 15
             c-modalidade:FGCOLOR IN BROWSE br-clientes = 15
             c-sit-cli:FGCOLOR IN BROWSE br-clientes = 15.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-clientes w-cadsim
ON VALUE-CHANGED OF br-clientes IN FRAME f-cad /* Clientes */
DO:
   IF AVAIL tt-clientes THEN DO.
      IF tt-clientes.ordem = 999 THEN 
         APPLY 'CURSOR-UP' TO SELF.

      FIND emitente WHERE
           emitente.nome-abrev = tt-clientes.nome-abrev NO-LOCK NO-ERROR.
      IF AVAIL emitente THEN
         ASSIGN gr-emitente = ROWID(emitente)
                rw-cliente = ROWID(tt-clientes).

      ASSIGN ed-msg-credito:SCREEN-VALUE = tt-clientes.motivo.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda w-cadsim
ON CHOOSE OF bt-ajuda IN FRAME f-cad /* Ajuda */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-anl-credito
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-anl-credito w-cadsim
ON CHOOSE OF bt-anl-credito IN FRAME f-cad /* Button 3 */
DO:
   ASSIGN INPUT FRAME {&FRAME-NAME} fi-nome-abrev-ini
          INPUT FRAME {&FRAME-NAME} fi-nome-abrev-fin.

   RUN pi-processa.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cons-cli
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cons-cli w-cadsim
ON CHOOSE OF bt-cons-cli IN FRAME f-cad
DO:
   RUN pdp/pd0902.p.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cons-global
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cons-global w-cadsim
ON CHOOSE OF bt-cons-global IN FRAME f-cad
DO:
   /*
   RUN esp/essp0180b.p (INPUT TABLE tt-clientes,
                        INPUT TABLE tt-titulos,
                        INPUT TABLE tt-cheques).
   */                      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-dig-cli
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-dig-cli w-cadsim
ON CHOOSE OF bt-dig-cli IN FRAME f-cad
DO:
    RUN esdlg/d01-digita.w (INPUT-OUTPUT TABLE tt-digita, 
                            INPUT "D",
                            INPUT "Cliente").

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-equifax
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-equifax w-cadsim
ON CHOOSE OF bt-equifax IN FRAME f-cad
DO:
    IF AVAIL emitente THEN DO.
       ASSIGN fi-cgc:SCREEN-VALUE = emitente.cgc.
       fi-cgc:SET-SELECTION(1,50). 
       fi-cgc:EDIT-COPY(). 

       w-cadsim:SENSITIVE = NO.

       RUN pi-open-ie (INPUT "https://consulta.equifax.com.br/Default.asp").

       ASSIGN c-historico = "EQUIFAX EMPRESARIAL" + CHR(13) +
                            "=====================" + CHR(13) + CHR(13) +
                            "FUNDAÄ«O:" + CHR(13) + CHR(13) + 
                            "S‡CIO:" + CHR(13) + CHR(13) +
                            "  CPF:" + CHR(13) + CHR(13) +
                            "  PERCENTUAL:" + CHR(13) + CHR(13) +
                            "CONSULTAS" + CHR(13) +
                            "==========" + CHR(13) + CHR(13) +
                            "  MAIOR FATURA:" + CHR(13) + CHR(13) +
                            "  MAIOR ACUMULO:" + CHR(13) + CHR(13) +
                            "PAGAMENTOS" + CHR(13) +
                            "============" + CHR(13) + CHR(13) +
                            "  DATA:" + CHR(13) + CHR(13) +
                            "  PONTUALIDADE:" + CHR(13) + CHR(13) +
                            "OBSERVAÄÂES:".

       RUN esp/essp0180e.p (INPUT emitente.cod-emit, INPUT c-historico).
       w-cadsim:SENSITIVE = YES.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ex-cli
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ex-cli w-cadsim
ON CHOOSE OF bt-ex-cli IN FRAME f-cad
DO:
    RUN esdlg/d01-digita.w (INPUT-OUTPUT TABLE tt-digita, 
                              INPUT "E",
                              INPUT "Cliente").

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-historico
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-historico w-cadsim
ON CHOOSE OF bt-historico IN FRAME f-cad
DO:
   RUN crp/cr0706.p.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-cadsim
ON CHOOSE OF bt-ok IN FRAME f-cad /* OK */
DO:
  RUN notify ('update-record':U).
  if return-value <> "adm-error":U then
     apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-receita-federal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-receita-federal w-cadsim
ON CHOOSE OF bt-receita-federal IN FRAME f-cad
DO:
    IF AVAIL emitente THEN DO.
       ASSIGN fi-cgc:SCREEN-VALUE = emitente.cgc.
       fi-cgc:SET-SELECTION(1,50). 
       fi-cgc:EDIT-COPY(). 

       RUN pi-open-ie (INPUT "http://www.receita.fazenda.gov.br/PessoaJuridica/CNPJ/cnpjreva/Cnpjreva_Solicitacao.asp").

       w-cadsim:SENSITIVE = NO.
       ASSIGN c-historico = "COMPOSIÄ«O EMPRESARIAL" + CHR(13) + 
                            "=========================" + CHR(13) + CHR(13) +
                            "FUNDAÄ«O:" + CHR(13) + CHR(13) + 
                            "DATA:" + CHR(13) + CHR(13) +
                            "CAPITAL SOCIAL:" + CHR(13) + CHR(13) +
                            "S‡CIO:" + CHR(13) + CHR(13) +
                            "  CPF:" + CHR(13) + CHR(13) +
                            "FONTES COMERCIAIS:" + CHR(13) +
                            "INFORMAÄÂES COMERCIAIS:" + CHR(13) +
                            "CLIENTE DESDE:" + CHR(13) + CHR(13) +
                            "ULTIMA FATURA:" + CHR(13) + CHR(13) +
                            "MAIOR FATURA:" + CHR(13) + CHR(13) +
                            "MAIOR ACUMULO:" + CHR(13) + CHR(13) +
                            "DEBITO ∑ VENCER:" + CHR(13) + CHR(13) +
                            "DEBITO VENCIDO:" + CHR(13) + CHR(13) +
                            "LIMITE DE CRêDITO:" + CHR(13) + CHR(13) +
                            "PAGAMENTOS:" + CHR(13) + CHR(13) +
                            "DUPLICATAS:" + CHR(13) + CHR(13) +
                            "CHEQUES:" + CHR(13) + CHR(13) +
                            "OBSERVAÄÂES:".

       RUN esp/essp0180e.p (INPUT emitente.cod-emit, INPUT c-historico).
       w-cadsim:SENSITIVE = YES.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-serasa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-serasa w-cadsim
ON CHOOSE OF bt-serasa IN FRAME f-cad
DO:
   IF AVAIL emitente THEN DO.
      ASSIGN fi-cgc:SCREEN-VALUE = emitente.cgc.
      fi-cgc:SET-SELECTION(1,50). 
      fi-cgc:EDIT-COPY(). 

      RUN pi-open-ie (INPUT "http://www.serasa.com.br/").

      w-cadsim:SENSITIVE = NO.
      ASSIGN c-historico = "SERASA RELATO:" + CHR(13) + CHR(13) +
                           "CREDIT RISKSCORING:" + CHR(13) + CHR(13) + 
                           "PROBABILIDADE INADIMPL“NCIA:" + CHR(13) + CHR(13) +
                           "FUNDAÄ«O:" + CHR(13) + CHR(13) + 
                           "CAPITAL SOCIAL:" + CHR(13) + CHR(13) +
                           "S‡CIO:" + CHR(13) + CHR(13) +
                           "  NADA CONSTA:" + CHR(13) + CHR(13) +
                           "  CPF:" + CHR(13) + CHR(13) +
                           "  PERCENTUAL:" + CHR(13) + CHR(13) +
                           "  CAPITAL:" + CHR(13) + CHR(13) +
                           "CONSULTAS" + CHR(13) +
                           "==========" + CHR(13) + CHR(13) +
                           "  ULTIMA COMPRA:" + CHR(13) + CHR(13) +
                           "  MAIOR FATURA:" + CHR(13) + CHR(13) +
                           "  MAIOR ACUMULO:" + CHR(13) + CHR(13) +
                           "  COMPROMISSOS NO MERCADO:" + CHR(13) + CHR(13) +
                           "PAGAMENTOS" + CHR(13) +
                           "============" + CHR(13) + CHR(13) +
                           "  DATA:" + CHR(13) + CHR(13) +
                           "  PONTUALIDADE:" + CHR(13) + CHR(13) +
                           "OBSERVAÄÂES:".

      RUN esp/essp0180e.p (INPUT emitente.cod-emit, INPUT c-historico).
      w-cadsim:SENSITIVE = YES.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-sintegra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-sintegra w-cadsim
ON CHOOSE OF bt-sintegra IN FRAME f-cad
DO:
   IF AVAIL emitente THEN DO.
      ASSIGN fi-cgc:SCREEN-VALUE = emitente.cgc.
      fi-cgc:SET-SELECTION(1,50). 
      fi-cgc:EDIT-COPY(). 

      RUN pi-sintegra (INPUT emitente.estado).

      w-cadsim:SENSITIVE = NO.
      ASSIGN c-historico = "SINTEGRA:".
      RUN esp/essp0180e.p (INPUT emitente.cod-emit, INPUT c-historico).
      w-cadsim:SENSITIVE = YES.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-vapara
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-vapara w-cadsim
ON CHOOSE OF bt-vapara IN FRAME f-cad
DO:
   RUN esdlg/d01essp0180.w (OUTPUT c-nome-abrev).

   IF c-nome-abrev <> "" THEN DO:
      FIND FIRST tt-clientes WHERE
                 SUBSTR(tt-clientes.nome-abrev,1,LENGTH(c-nome-abrev)) = c-nome-abrev 
                 NO-LOCK NO-ERROR. 

      IF NOT AVAIL tt-clientes THEN DO.
         FIND tt-clientes WHERE
              tt-clientes.cod-emitente = INTEGER(c-nome-abrev) NO-LOCK NO-ERROR.

         IF NOT AVAIL tt-clientes THEN DO:
            MESSAGE "Cliente n∆o est† contido na seleá∆o!"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN NO-APPLY.
         END.
      END.

      h-query:REPOSITION-TO-ROWID(ROWID(tt-clientes)) NO-ERROR. 
      APPLY 'VALUE-CHANGED' TO br-clientes.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-nome-abrev-fin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nome-abrev-fin w-cadsim
ON LEAVE OF fi-nome-abrev-fin IN FRAME f-cad
DO:
   IF SELF:SCREEN-VALUE <> '' THEN DO.
      FIND emitente USE-INDEX nome WHERE 
           emitente.nome-abrev = INPUT FRAME {&FRAME-NAME} fi-nome-abrev-fin NO-LOCK NO-ERROR.

      IF NOT AVAIL emitente THEN
         FIND emitente WHERE 
              STRING(emitente.cod-emit) = INPUT FRAME {&FRAME-NAME} fi-nome-abrev-fin 
              USE-INDEX codigo NO-LOCK NO-ERROR.

      IF AVAIL emitente THEN
         ASSIGN fi-nome-abrev-fin:SCREEN-VALUE IN FRAME {&FRAME-NAME} = emitente.nome-abrev.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nome-abrev-fin w-cadsim
ON MOUSE-SELECT-DBLCLICK OF fi-nome-abrev-fin IN FRAME f-cad
DO:
    {include/zoomvar.i &prog-zoom = adzoom/z02ad098.w
                       &campo     = fi-nome-abrev-fin
                       &campozoom = nome-abrev}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-nome-abrev-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nome-abrev-ini w-cadsim
ON LEAVE OF fi-nome-abrev-ini IN FRAME f-cad
DO:
   IF SELF:SCREEN-VALUE <> '' THEN DO.
      FIND emitente WHERE 
           emitente.nome-abrev = INPUT FRAME {&FRAME-NAME} fi-nome-abrev-ini
           USE-INDEX nome NO-LOCK NO-ERROR.

      IF NOT AVAIL emitente THEN
         FIND emitente WHERE 
              STRING(emitente.cod-emit) = INPUT FRAME {&FRAME-NAME} fi-nome-abrev-ini 
              USE-INDEX codigo NO-LOCK NO-ERROR.

      IF AVAIL emitente THEN
         ASSIGN fi-nome-abrev-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = emitente.nome-abrev.

      ASSIGN fi-nome-abrev-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nome-abrev-ini w-cadsim
ON MOUSE-SELECT-DBLCLICK OF fi-nome-abrev-ini IN FRAME f-cad
DO:
    {include/zoomvar.i &prog-zoom = adzoom/z02ad098.w
                       &campo     = fi-nome-abrev-ini
                       &campozoom = nome-abrev}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sobre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sobre w-cadsim
ON CHOOSE OF MENU-ITEM mi-sobre /* Sobre... */
DO:
  {include/sobre.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-cadsim 


/* ***************************  Main Block  *************************** */
fi-nome-abrev-ini:LOAD-MOUSE-POINTER("image/lupa.cur")   IN FRAME {&FRAME-NAME}.
fi-nome-abrev-fin:LOAD-MOUSE-POINTER("image/lupa.cur")   IN FRAME {&FRAME-NAME}.

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-cadsim  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-cadsim  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-cadsim  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-cadsim)
  THEN DELETE WIDGET w-cadsim.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-cadsim  _DEFAULT-ENABLE
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
  DISPLAY fi-nome-abrev-ini fi-nome-abrev-fin ed-msg-credito fi-cgc 
      WITH FRAME f-cad IN WINDOW w-cadsim.
  ENABLE BUTTON-1 RECT-54 BUTTON-2 RECT-55 rt-button bt-anl-credito 
         fi-nome-abrev-ini fi-nome-abrev-fin bt-dig-cli bt-ex-cli br-clientes 
         bt-vapara bt-equifax bt-cons-cli bt-historico bt-cons-global 
         bt-sintegra bt-receita-federal ed-msg-credito bt-serasa bt-ok bt-ajuda 
         fi-cgc 
      WITH FRAME f-cad IN WINDOW w-cadsim.
  {&OPEN-BROWSERS-IN-QUERY-f-cad}
  VIEW w-cadsim.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-cadsim 
PROCEDURE local-destroy :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .
  {include/i-logfin.i}

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-cadsim 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-cadsim 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  {include/win-size.i}

  {utp/ut9000.i "ESSP0193" "2.04.00.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  RUN dispatch  IN this-procedure ('enable-fields':U).

  {include/i-inifld.i}

   APPLY 'entry' TO fi-nome-abrev-ini.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cheques w-cadsim 
PROCEDURE pi-cheques :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {utp/ut-liter.i Selecionando_Cheques *}
    RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

    FOR EACH tt-clientes EXCLUSIVE-LOCK.
        FOR EACH titulo WHERE 
                 titulo.cod-emit = tt-clientes.cod-emit AND
                 titulo.cod-esp = 'CQ' AND
                 titulo.vl-saldo <> 0 NO-LOCK USE-INDEX emitente.

            RUN pi-acompanhar IN h-acomp (INPUT "Cliente: " + titulo.nome-abrev).

            IF titulo.dt-vencimen < TODAY THEN 
               ASSIGN tt-clientes.cheques-dev = tt-clientes.cheques-dev + titulo.vl-saldo.
        END.
    END.

    FOR EACH cheque WHERE 
             cheque.devolvido = NO AND 
             cheque.situacao-cheque = 1 NO-LOCK.    /* Sit 1 = Pendente */

        FIND tt-clientes WHERE
             tt-clientes.cod-emit = cheque.cod-emit NO-ERROR.
        IF NOT AVAIL tt-clientes THEN NEXT.

        RUN pi-acompanhar IN h-acomp (INPUT "Cliente: " + STRING(cheque.cod-emit)).

        IF cheque.origem-cheque <> 6 /*CR*/ THEN NEXT. 

        ASSIGN tt-clientes.cheques-compsar = tt-clientes.cheques-compsar + cheque.vl-cheque.

        CREATE tt-cheques.
        BUFFER-COPY cheque TO tt-cheques.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-clientes w-cadsim 
PROCEDURE pi-clientes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {utp/ut-liter.i Selecionando_Clientes *}
    RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

    FOR EACH emitente WHERE
             emitente.nome-abrev   >= fi-nome-abrev-ini   AND
             emitente.nome-abrev   <= fi-nome-abrev-fin   NO-LOCK.

        IF emitente.ind-cre-cli = 2 THEN NEXT.  /* CrÇdito Autom†tico */

        /*
        RUN pi-ver-digita (INPUT "Pedido_de_Venda",                                                             
                           INPUT emitente.nr-pedcli).                                                          
        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.                                                                
                                                                                                                 
        RUN pi-ver-digita (INPUT "Cliente",
                           INPUT emitente.nome-abrev).
        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.
        */

        RUN pi-acompanhar IN h-acomp (INPUT "Cliente: " + emitente.nome-abrev ).
    
        FIND tt-clientes WHERE
             tt-clientes.nome-abrev = emitente.nome-abrev NO-ERROR.

        IF NOT AVAIL tt-clientes THEN DO:
           CREATE tt-clientes.
           BUFFER-COPY emitente TO tt-clientes.
        END.
        ASSIGN tt-clientes.tot-pend = tt-clientes.tot-pend + de-tot-pend
               tt-clientes.tot-aceitos = tt-clientes.tot-aceitos + de-tot-aceitos
               tt-clientes.tot-aprovados = tt-clientes.tot-aprovados + de-tot-aprov.
        /*
        ASSIGN tt-clientes.desc-bloq-cr = IF tt-clientes.desc-bloq-cr = "" AND emitente.desc-bloq-cr <> ""
                                          THEN emitente.desc-bloq-cr
                                          ELSE IF emitente.desc-bloq-cr <> "" 
                                               THEN tt-clientes.desc-bloq-cr + " // " + emitente.desc-bloq-cr
                                               ELSE tt-clientes.desc-bloq-cr.
        */
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-open-ie w-cadsim 
PROCEDURE pi-open-ie :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER p-site AS CHAR FORMAT "x(100)".

    DEF VAR c-arq-java AS CHAR.
    DEF VAR c-comando AS CHAR.                                

    ASSIGN c-arq-java = SESSION:TEMP-DIRECTORY + "abrir.js".

    OUTPUT TO VALUE(c-arq-java).
        PUT 'var oIE = new ActiveXObject("InternetExplorer.Application");' SKIP
            'oIE.Navigate2("' + p-site + '");' FORMAT "x(150)" SKIP     
            'oIE.Visible = true;' SKIP.
    OUTPUT CLOSE.

    ASSIGN c-comando = 'wscript.exe ' + c-arq-java.
  
    OS-COMMAND SILENT VALUE(c-comando).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-processa w-cadsim 
PROCEDURE pi-processa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN utp/ut-acomp.p PERSISTENT SET h-acomp.

   EMPTY TEMP-TABLE tt-cheques.
   EMPTY TEMP-TABLE tt-titulos.
   EMPTY TEMP-TABLE tt-clientes.

   ASSIGN gr-emitente = ?
          rw-cliente = ?.

   RUN pi-clientes.
   RUN pi-titulos.
   RUN pi-cheques.

   {utp/ut-liter.i Calculando_Conta_Corrente *}
   RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).
   FOR EACH tt-clientes BY tt-clientes.nome-abrev.

       RUN pi-acompanhar IN h-acomp (INPUT "Cliente: " + tt-clientes.nome-abrev).

       ASSIGN tt-clientes.lim-cred = tt-clientes.lim-cred + tt-clientes.lim-adicional.

       ASSIGN tt-clientes.saldo = tt-clientes.lim-cred - tt-clientes.tot-pend - tt-clientes.tot-aceitos -
                                  tt-clientes.tit-vencidos - tt-clientes.tot-aprov - tt-clientes.tit-a-vencer + tt-clientes.cheques-compsar -
                                  tt-clientes.cheques-dev.

       ASSIGN de-tot-pend = de-tot-pend + tt-clientes.tot-pend
              de-tot-aceitos = de-tot-aceitos + tt-clientes.tot-aceitos.
       
       RUN esapi/esapi0180.p (INPUT tt-clientes.cod-emit, 
                              OUTPUT tt-clientes.restricao,
                              OUTPUT tt-clientes.motivo).
       

       CASE tt-clientes.ind-cre-cli.
           WHEN  5 THEN
               ASSIGN tt-clientes.restricao = YES
                      tt-clientes.motivo = IF tt-clientes.motivo = ""
                                           THEN "Pagamento ∑ VISTA"   
                                           ELSE "Pagamento ∑ VISTA // " + tt-clientes.motivo.
           WHEN  4 THEN
               ASSIGN tt-clientes.restricao = YES
                      tt-clientes.motivo = IF tt-clientes.motivo = ""
                                           THEN "CrÇdito SUSPENSO // " + tt-clientes.observacoes
                                           ELSE "CrÇdito SUSPENSO // " + tt-clientes.observacoes + " // " + tt-clientes.motivo.
       END.

       IF tt-clientes.saldo <= 0 THEN
          ASSIGN tt-clientes.motivo = IF tt-clientes.motivo = ""
                                      THEN "Excedeu Limite CrÇdito"
                                      ELSE "Excedeu Limite CrÇdito // " + tt-clientes.motivo.

       IF tt-clientes.saldo > 0 AND 
          NOT tt-clientes.restricao THEN
          ASSIGN tt-clientes.motivo = "Credito OK".

       IF tt-clientes.desc-bloq-cr <> "" THEN
          ASSIGN tt-clientes.motivo = tt-clientes.motivo + " // " + tt-clientes.desc-bloq-cr.
   END.

   RUN pi-finalizar IN h-acomp.

   {&OPEN-QUERY-br-clientes}
   APPLY 'value-changed' TO br-clientes IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-sintegra w-cadsim 
PROCEDURE pi-sintegra :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER p-estado AS CHAR.

    CASE p-estado.
       WHEN 'AC' THEN
           RUN pi-open-ie (INPUT "http://www.sefaz.ac.gov.br:8080/portalsefaz/servlet/hpfsinco").
       WHEN 'AL' THEN
           RUN pi-open-ie (INPUT "http://www.sefaz.al.gov.br/asp/sintegra/sintegra.asp?estado=AL").
       WHEN 'AP' THEN
           RUN pi-open-ie (INPUT "http://www.sintegra.ap.gov.br/").     
       WHEN 'AM' THEN
           RUN pi-open-ie (INPUT "http://www.sefaz.am.gov.br/sintegra/sintegra0.asp").     
       WHEN 'BA' THEN
           RUN pi-open-ie (INPUT "http://www.sefaz.ba.gov.br/Sintegra/sintegra.asp?estado=BA").     
       WHEN 'CE' THEN
           RUN pi-open-ie (INPUT "http://www.sefaz.ce.gov.br/Sintegra/Sintegra.Asp?estado=CE").
       WHEN 'DF' THEN
           RUN pi-open-ie (INPUT "http://www.fazenda.df.gov.br/area.cfm?id_area=110").
       WHEN 'ES' THEN
           RUN pi-open-ie (INPUT "http://www.sintegra.es.gov.br/").
       WHEN 'GO' THEN
           RUN pi-open-ie (INPUT "http://www.sefaz.go.gov.br/sintegra/sintegra.asp").     
       WHEN 'MA' THEN
           RUN pi-open-ie (INPUT "http://www.sefaz.ma.gov.br/sintegra/sintegra.asp").
       WHEN 'MT' THEN
           RUN pi-open-ie (INPUT "http://www.sefaz.mt.gov.br/sintegra").
       WHEN 'MS' THEN
           RUN pi-open-ie (INPUT "http://www.sintegra.ms.gov.br/sintegra.asp?estado=ms").
       WHEN 'MG' THEN
           RUN pi-open-ie (INPUT "http://www.sintegra.fazenda.mg.gov.br/").
       WHEN 'PA' THEN
           RUN pi-open-ie (INPUT "http://www.sefa.pa.gov.br/sintegra/consultapublica/index.cfm?fuseaction=inicio").     
       WHEN 'PB' THEN
           RUN pi-open-ie (INPUT "http://sintegra.receita.pb.gov.br/sintegra/sintegra.asp?estado=pb").     
       WHEN 'PR' THEN
           RUN pi-open-ie (INPUT "http://www.fazenda.pr.gov.br/sintegra/").     
       WHEN 'PE' THEN
           RUN pi-open-ie (INPUT "http://www.sintegra.sefaz.pe.gov.br").     
       WHEN 'PI' THEN
           RUN pi-open-ie (INPUT "http://web.sintegra.sefaz.pi.gov.br").     
       WHEN 'RJ' THEN
           RUN pi-open-ie (INPUT "http://p1-webapp.sef.rj.gov.br/app/cps/index.jsp").
       WHEN 'RN' THEN
           RUN pi-open-ie (INPUT "http://ww3.set.rn.gov.br/sintegra").     
       WHEN 'RS' THEN
           RUN pi-open-ie (INPUT "http://www.sefaz.rs.gov.br/SEF_root/inf/sintegra_entrada.asp").     
       WHEN 'RO' THEN
           RUN pi-open-ie (INPUT "http://www.sefin.ro.gov.br/sint_consul.asp").
       WHEN 'RR' THEN
           RUN pi-open-ie (INPUT "http://sintegra.sefaz.rr.gov.br/").     
       WHEN 'SC' THEN
           RUN pi-open-ie (INPUT "http://sistemas.sef.sc.gov.br/sintegra").     
       WHEN 'SP' THEN
           RUN pi-open-ie (INPUT "http://pfeserv1.fazenda.sp.gov.br/sintegrapfe/sintegra.html").
       WHEN 'SE' THEN
           RUN pi-open-ie (INPUT "http://www.sefaz.se.gov.br/sintegra").     
       WHEN 'TO' THEN
           RUN pi-open-ie (INPUT "http://sintegra.sefaz.to.gov.br").     
       OTHERWISE
           RUN pi-open-ie (INPUT "http://sintegra.gov.br").     
    END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-titulos w-cadsim 
PROCEDURE pi-titulos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {utp/ut-liter.i Selecionando_Titulos *}
    RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

    FOR EACH tt-clientes EXCLUSIVE-LOCK.
        FOR EACH titulo WHERE 
                 titulo.cod-emit = tt-clientes.cod-emit AND
                 titulo.cod-esp = 'DP' AND
                 titulo.vl-saldo <> 0 NO-LOCK USE-INDEX emitente.
                                                                                            
            /* Ignora duplicatas substituidas */
            IF CAN-FIND(FIRST mov-tit OF titulo WHERE mov-tit.baixa-subs) THEN NEXT.
                                                                                           
            RUN pi-acompanhar IN h-acomp (INPUT "Cliente: " + titulo.nome-abrev).
    
            IF titulo.dt-vencimen < TODAY THEN  
               ASSIGN tt-clientes.tit-vencidos = tt-clientes.tit-vencidos + titulo.vl-saldo.
            ELSE /* A Vencer*/
               ASSIGN tt-clientes.tit-a-vencer = tt-clientes.tit-a-vencer + titulo.vl-saldo.
                                                                                                       CREATE tt-titulos.
            BUFFER-COPY titulo TO tt-titulos.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-cadsim  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-clientes"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-cadsim 
PROCEDURE state-changed :
/*:T -----------------------------------------------------------
  Purpose:     Manuseia trocas de estado dos SmartObjects
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.

  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-modalidade w-cadsim 
FUNCTION fn-modalidade RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  IF tt-clientes.modalidade <> 0 THEN DO.
     {esinc/i-dsrb.i tt-clientes.modalidade tt-clientes.modalidade c-modalidade}.
  END.
  ELSE
     ASSIGN c-modalidade = "".

  RETURN c-modalidade.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-sit-cli w-cadsim 
FUNCTION fn-sit-cli RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
   IF tt-clientes.ind-sit-emitente <> 0 THEN DO.
      {esinc/i-dsrb.i tt-clientes.ind-sit-emitente tt-clientes.ind-sit-emitente c-sit-cli}.
   END.
   ELSE
      ASSIGN c-sit-cli = "".

   RETURN c-sit-cli.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

