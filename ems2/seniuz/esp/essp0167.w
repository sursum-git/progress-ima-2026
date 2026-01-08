&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESSP0167 2.04.00.999}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
/* INICIO DA DEFINI€ÇO DAS TABELAS TEMPORARIAS; PARA A CHAMADA DO RELATORIO ESPD0002RP.P */

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
/* FIM DAS DEFINI€åES DO RELATORIO ESPD0002RP.P */

/* Global Variable Definitions ---                                       */
DEF NEW GLOBAL SHARED VAR gr-ped-venda  AS ROWID NO-UNDO.
DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

/* Temp Table Definitions ---                                           */
DEF TEMP-TABLE tt-ped-venda LIKE ped-venda
    FIELD tipo-pedido  LIKE ped-venda-ext.tp-pedido
    FIELD dt-reserva   LIKE reserva.dt-reserva
    FIELD dt-emis-nota LIKE nota-fiscal.dt-emis-nota
    FIELD dt-saida-nf  LIKE nota-fiscal.dt-saida 
    FIELD dri AS INT FORMAT ">>9"          
    FIELD dfi AS INT FORMAT ">>9"            
    FIELD dfr AS INT FORMAT ">>9"          
    FIELD dei AS INT FORMAT ">>9"              
    FIELD der AS INT FORMAT ">>9"                  
    FIELD dif AS INT FORMAT ">>9".          

/* Local Variable Definitions ---                                       */
DEF VAR h-acomp           AS HANDLE NO-UNDO.

DEF VAR h-query           AS HANDLE.
DEF VAR v-row-table       AS ROWID.
DEF VAR c-busca           AS CHAR.

DEF VAR c-pedidos         AS CHAR.
DEF VAR i-ct              AS INT.
DEF VAR c-dia             AS CHAR.
DEF VAR da-dt-entrega-ini AS DATE.
DEF VAR da-dt-entrega-fin AS DATE.
DEF VAR i-lin AS INT.
DEF VAR i-pag AS INT.
DEF VAR da-reserva  AS DATE.


/* Variaveis Usadas Na Gera‡Æo da Planilha Excel */
DEF VAR c-empresa LIKE empresa.razao-social.
DEFINE VAR i-canal     AS INTEGER.
DEFINE VAR sys         AS INTEGER.
DEFINE VAR c-lin       AS CHARACTER FORMAT "x(500)".
DEFINE VAR aux-command AS CHAR FORMAT "x(100)".
DEFINE VAR arq-saida   AS CHAR FORMAT "x(45)".

/* Variaveis da Rotina de ImpressÆo */
DEFINE VAR i-saida             AS INT.
DEFINE VAR c-saida             AS CHAR.
DEFINE VAR i-num-copias        AS INT.




/* Variavies de Parƒmetros */
DEFINE VAR c-dt-limite-ini     AS CHAR.
DEFINE VAR c-dt-limite-fin     AS CHAR.
DEFINE VAR c-nr-pedcli-ini     LIKE ped-item-ext.nr-pedcli INIT "".
DEFINE VAR c-nr-pedcli-fin     LIKE ped-item-ext.nr-pedcli INIT "ZZZZZZZZ".  
DEFINE VAR c-nome-abrev-ini    LIKE ped-venda.nome-abrev. 
DEFINE VAR c-nome-abrev-fin    LIKE ped-venda.nome-abrev INIT "ZZZZZZZZZZZZ".
DEFINE VAR c-no-ab-reppri-ini  LIKE ped-venda.no-ab-reppri. 
DEFINE VAR c-no-ab-reppri-fin  LIKE ped-venda.no-ab-reppri INIT 'ZZZZZZZZZZZZ'.
DEFINE VAR i-reserva           AS INT INIT 1.
DEFINE VAR l-ok                AS LOG.



DEFINE VAR c-nr-pedcli         LIKE ped-venda.nr-pedcli.
/* DEFINI€ÇO DE VARIAVEIS PARA A CHAMADA DO RELATORIO ESPD0002RP.P   pre-nota */
/* Variaveis para a include i-rprun.i */
DEF VAR h-prog AS HANDLE.
DEFINE VAR raw-param   AS RAW NO-UNDO.
DEFINE VAR rs-execucao AS INTEGER INITIAL 1. 
DEFINE FRAME f-pg-imp rs-execucao AT ROW 5.75 COL 3 NO-LABEL.
DEFINE VAR c-programa-mg97 AS CHAR INIT "essp0155". 
DEFINE VAR c-versao-mg97 AS CHAR.
/* FIM DAS DEFINI€åES DE VARIAVEIS PARA CHAMADA DO RELATORIO ESPD0002RP.P */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME br-pedidos

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-ped-venda

/* Definitions for BROWSE br-pedidos                                    */
&Scoped-define FIELDS-IN-QUERY-br-pedidos tt-ped-venda.nr-pedcli tt-ped-venda.nome-abrev tt-ped-venda.tipo-pedido tt-ped-venda.dt-implant tt-ped-venda.dt-reserva tt-ped-venda.dri tt-ped-venda.dt-emis-nota tt-ped-venda.dfi tt-ped-venda.dfr tt-ped-venda.dt-saida tt-ped-venda.dei tt-ped-venda.der tt-ped-venda.dif tt-ped-venda.nr-pedrep tt-ped-venda.no-ab-reppri   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-pedidos   
&Scoped-define SELF-NAME br-pedidos
&Scoped-define OPEN-QUERY-br-pedidos RUN pi-medias. OPEN QUERY {&SELF-NAME} FOR EACH tt-ped-venda NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-pedidos tt-ped-venda
&Scoped-define FIRST-TABLE-IN-QUERY-br-pedidos tt-ped-venda


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-br-pedidos}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-4 rt-buttom br-pedidos bt-param ~
bt-vapara bt-consulta bt-imprime BUTTON-1 bt-excel bt-exit bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS fi-dfi fi-dfr fi-dei fi-der fi-def fi-dri 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-6 bt-excel 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ajuda 
     IMAGE-UP FILE "image/im-hel.bmp":U
     LABEL "&Ajuda" 
     SIZE 4.86 BY 1.29 TOOLTIP "Help on Line"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-consulta AUTO-GO 
     IMAGE-UP FILE "image/im-det.bmp":U
     LABEL "" 
     SIZE 5 BY 1.29 TOOLTIP "Detalhar Pedido"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-excel 
     IMAGE-UP FILE "image/excel.bmp":U
     LABEL "Button 2" 
     SIZE 5 BY 1.29.

DEFINE BUTTON bt-exit AUTO-END-KEY 
     IMAGE-UP FILE "image/im-exi.bmp":U
     LABEL "" 
     SIZE 4.86 BY 1.29 TOOLTIP "Fechar Programa"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-imprime AUTO-GO 
     IMAGE-UP FILE "image/im-prigr.bmp":U
     LABEL "" 
     SIZE 5 BY 1.29 TOOLTIP "Imprimir Pedido"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-param AUTO-GO 
     IMAGE-UP FILE "image/im-fil.bmp":U
     LABEL "&Sair" 
     SIZE 5 BY 1.25 TOOLTIP "Parƒmetros"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-vapara AUTO-GO 
     IMAGE-UP FILE "image/im-vapra.bmp":U
     LABEL "" 
     SIZE 5 BY 1.29 TOOLTIP "Posicionar no Pedido"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "image/im-printer.bmp":U
     LABEL "" 
     SIZE 5 BY 1.29 TOOLTIP "Imprimir a Carteira de Pedidos Selecionados".

DEFINE VARIABLE fi-def AS DECIMAL FORMAT ">>9.99":U INITIAL 0 
     LABEL "DEF" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dei AS DECIMAL FORMAT ">>9.99":U INITIAL 0 
     LABEL "DEI" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE fi-der AS DECIMAL FORMAT ">>9.99":U INITIAL 0 
     LABEL "DER" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dfi AS DECIMAL FORMAT ">>9.99":U INITIAL 0 
     LABEL "DFI" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dfr AS DECIMAL FORMAT ">>9.99":U INITIAL 0 
     LABEL "DFR" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dri AS DECIMAL FORMAT ">>9.99":U INITIAL 0 
     LABEL "DRI" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 111 BY 1.75
     BGCOLOR 9 FGCOLOR 15 .

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 7 BY 16.75
     BGCOLOR 8 FGCOLOR 0 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-pedidos FOR 
      tt-ped-venda SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-pedidos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-pedidos C-Win _FREEFORM
  QUERY br-pedidos NO-LOCK DISPLAY
      tt-ped-venda.nr-pedcli    COLUMN-LABEL "Pedido" FORMAT "x(12)":U  WIDTH 8    

tt-ped-venda.nome-abrev   FORMAT "x(12)":U  WIDTH 14
tt-ped-venda.tipo-pedido  COLUMN-LABEL "Tipo Pedido" FORMAT "x(12)":U  WIDTH 10
tt-ped-venda.dt-implant   COLUMN-LABEL "Implanta‡Æo"
tt-ped-venda.dt-reserva   COLUMN-LABEL "Reserva"
tt-ped-venda.dri          COLUMN-LABEL "DRI"      COLUMN-FGCOLOR 12
tt-ped-venda.dt-emis-nota COLUMN-LABEL "Faturamento"
tt-ped-venda.dfi          COLUMN-LABEL "DFI"
tt-ped-venda.dfr          COLUMN-LABEL "DFR"
tt-ped-venda.dt-saida     COLUMN-LABEL "Dt.Entrega"
tt-ped-venda.dei          COLUMN-LABEL "DEI"
tt-ped-venda.der          COLUMN-LABEL "DER"
tt-ped-venda.dif          COLUMN-LABEL "DEF"
tt-ped-venda.nr-pedrep    FORMAT "x(12)":U  WIDTH 10
tt-ped-venda.no-ab-reppri COLUMN-LABEL "Representante" FORMAT "x(12)":U  WIDTH 14
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 103 BY 16.75
         FONT 6
         TITLE "Carteira de Pedidos Selecionada" ROW-HEIGHT-CHARS .46.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     br-pedidos AT ROW 1.25 COL 2.14
     bt-param AT ROW 1.54 COL 107
     bt-vapara AT ROW 2.83 COL 107.14
     bt-consulta AT ROW 4.17 COL 107.14
     bt-imprime AT ROW 5.5 COL 107.14
     BUTTON-1 AT ROW 6.83 COL 107
     bt-excel AT ROW 8.21 COL 107
     bt-exit AT ROW 15 COL 107
     bt-ajuda AT ROW 16.33 COL 107
     fi-dfi AT ROW 18.25 COL 23.57 COLON-ALIGNED
     fi-dfr AT ROW 18.25 COL 41.43 COLON-ALIGNED
     fi-dei AT ROW 18.25 COL 59.29 COLON-ALIGNED
     fi-der AT ROW 18.25 COL 76.72 COLON-ALIGNED
     fi-def AT ROW 18.25 COL 93.43 COLON-ALIGNED
     fi-dri AT ROW 18.29 COL 7 COLON-ALIGNED
     "DFI" VIEW-AS TEXT
          SIZE 4 BY .67 AT ROW 20.75 COL 3.86
          BGCOLOR 9 FGCOLOR 6 
     "Dias Faturamento / Reserva" VIEW-AS TEXT
          SIZE 26 BY .67 AT ROW 19.92 COL 47.57
          BGCOLOR 9 FGCOLOR 15 FONT 6
     "Dias Entrega / Faturamento" VIEW-AS TEXT
          SIZE 24 BY .67 AT ROW 20.75 COL 86.14
          BGCOLOR 9 FGCOLOR 15 FONT 6
     "DEI" VIEW-AS TEXT
          SIZE 4 BY .67 AT ROW 20.75 COL 42.57
          BGCOLOR 9 FGCOLOR 6 
     "DER" VIEW-AS TEXT
          SIZE 4 BY .67 AT ROW 19.92 COL 81.14
          BGCOLOR 9 FGCOLOR 6 
     "DEF" VIEW-AS TEXT
          SIZE 4 BY .67 AT ROW 20.75 COL 81.14
          BGCOLOR 9 FGCOLOR 6 
     "Dias Entrega / Implanta‡Æo" VIEW-AS TEXT
          SIZE 24 BY .67 AT ROW 20.75 COL 47.57
          BGCOLOR 9 FGCOLOR 15 FONT 6
     "Dias Entrega / Reserva" VIEW-AS TEXT
          SIZE 24 BY .67 AT ROW 19.92 COL 86.14
          BGCOLOR 9 FGCOLOR 15 FONT 6
     "Dias Reserva / Implanta‡Æo" VIEW-AS TEXT
          SIZE 26 BY .67 AT ROW 19.92 COL 8.86
          BGCOLOR 9 FGCOLOR 15 FONT 6
     "Dias Faturamento / Implanta‡Æo" VIEW-AS TEXT
          SIZE 26 BY .67 AT ROW 20.75 COL 8.86
          BGCOLOR 9 FGCOLOR 15 FONT 6
     "DRI" VIEW-AS TEXT
          SIZE 4 BY .67 AT ROW 19.92 COL 3.86
          BGCOLOR 9 FGCOLOR 6 
     "DFR" VIEW-AS TEXT
          SIZE 4 BY .67 AT ROW 19.92 COL 42.57
          BGCOLOR 9 FGCOLOR 6 
     RECT-4 AT ROW 19.75 COL 2
     rt-buttom AT ROW 1.25 COL 106
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 112.72 BY 20.63.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Lead-Time dos Pedido de Vendas"
         COLUMN             = 12.43
         ROW                = 7.83
         HEIGHT             = 20.63
         WIDTH              = 112.72
         MAX-HEIGHT         = 29.79
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 29.79
         VIRTUAL-WIDTH      = 146.29
         RESIZE             = no
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



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB br-pedidos rt-buttom DEFAULT-FRAME */
/* SETTINGS FOR BUTTON bt-excel IN FRAME DEFAULT-FRAME
   6                                                                    */
/* SETTINGS FOR FILL-IN fi-def IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-dei IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-der IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-dfi IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-dfr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-dri IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-pedidos
/* Query rebuild information for BROWSE br-pedidos
     _START_FREEFORM
RUN pi-medias.
OPEN QUERY {&SELF-NAME} FOR EACH tt-ped-venda NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "mgmov.ped-venda.cod-sit-ped = 1"
     _Query            is OPENED
*/  /* BROWSE br-pedidos */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Lead-Time dos Pedido de Vendas */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Lead-Time dos Pedido de Vendas */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-pedidos
&Scoped-define SELF-NAME br-pedidos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-pedidos C-Win
ON ANY-KEY OF br-pedidos IN FRAME DEFAULT-FRAME /* Carteira de Pedidos Selecionada */
DO:
  IF INDEX("1234567890",KEYFUNCTION(LASTKEY)) <> 0 THEN DO.
     ASSIGN c-busca = c-busca + KEYFUNCTION(LASTKEY).
     IF SUBSTR(STRING(tt-ped-venda.nr-pedcli),1,LENGTH(c-busca)) <> c-busca THEN DO.
        ASSIGN v-row-table = ROWID(tt-ped-venda).
        FIND FIRST tt-ped-venda WHERE
                   tt-ped-venda.nr-pedcli BEGINS c-busca NO-LOCK NO-ERROR.
        IF AVAIL tt-ped-venda THEN DO.
           h-query:REPOSITION-TO-ROWID(ROWID(tt-ped-venda)) NO-ERROR.   
           APPLY 'value-changed' TO SELF.
        END.
        ELSE DO.
           ASSIGN c-busca = "".
           FIND tt-ped-venda WHERE 
                ROWID(tt-ped-venda) = v-row-table NO-LOCK NO-ERROR.
        END.
     END.
     IF AVAIL tt-ped-venda THEN DO.
        IF tt-ped-venda.nr-pedcli = c-busca THEN
           ASSIGN c-busca = "".
     END.
  END.
  ELSE 
     ASSIGN c-busca = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-pedidos C-Win
ON MOUSE-SELECT-DBLCLICK OF br-pedidos IN FRAME DEFAULT-FRAME /* Carteira de Pedidos Selecionada */
DO:
  APPLY 'choose' TO bt-consulta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-pedidos C-Win
ON RETURN OF br-pedidos IN FRAME DEFAULT-FRAME /* Carteira de Pedidos Selecionada */
DO:
  APPLY 'choose' TO bt-consulta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-pedidos C-Win
ON ROW-DISPLAY OF br-pedidos IN FRAME DEFAULT-FRAME /* Carteira de Pedidos Selecionada */
DO:
    IF tt-ped-venda.dei > 5 THEN
       tt-ped-venda.dei:FGCOLOR IN BROWSE br-pedidos = 12.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda C-Win
ON CHOOSE OF bt-ajuda IN FRAME DEFAULT-FRAME /* Ajuda */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-consulta C-Win
ON CHOOSE OF bt-consulta IN FRAME DEFAULT-FRAME
DO:
   FIND ped-venda WHERE
        ped-venda.nr-pedcli = tt-ped-venda.nr-pedcli AND
        ped-venda.nome-abrev = tt-ped-venda.nome-abrev NO-LOCK NO-ERROR.
   ASSIGN gr-ped-venda = ROWID(ped-venda).

   ASSIGN c-win:SENSITIVE = NO.
   RUN esp\espd4000.w (INPUT "Consultar").
   ASSIGN c-win:SENSITIVE = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-excel C-Win
ON CHOOSE OF bt-excel IN FRAME DEFAULT-FRAME /* Button 2 */
DO:
   RUN esdlg/d01essp0167.w (OUTPUT arq-saida).
   IF arq-saida <> "" THEN DO:
      RUN pi-gera-excel (INPUT arq-saida).
      MESSAGE "O arquivo foi gerado em " TRIM(arq-saida) + "." SKIP 
              "Para acess -lo,  abra-o atrav‚s do Excel."
          VIEW-AS ALERT-BOX INFO BUTTONS OK. 
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-exit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-exit C-Win
ON CHOOSE OF bt-exit IN FRAME DEFAULT-FRAME
DO:
   APPLY 'CLOSE':U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-imprime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-imprime C-Win
ON CHOOSE OF bt-imprime IN FRAME DEFAULT-FRAME
DO:
  CREATE tt-param.
  ASSIGN tt-param.usuario          = c-seg-usuario
         tt-param.data-exec        = TODAY
         tt-param.hora-exec        = TIME
         tt-param.destino          = 3
         tt-param.classifica       = 1
         tt-param.arquivo          = SESSION:TEMP-DIRECTORY + c-programa-mg97 + ".tmp"
         tt-param.pedido-ini       = INT(tt-ped-venda.nr-pedcli)
         tt-param.pedido-fin       = INT(tt-ped-venda.nr-pedcli)
         tt-param.dt-emissao-ini   = 01.01.0001     
         tt-param.dt-emissao-fin   = 12.31.9999
         tt-param.dt-entrega-ini   = 01.01.0001      
         tt-param.dt-entrega-fin   = 12.31.9999
         tt-param.cod-emit-ini     = 0
         tt-param.cod-emit-fin     = 999999
         tt-param.no-ab-reppri-ini = ''   
         tt-param.no-ab-reppri-fin = 'ZZZZZZZZZZZZZZ'   
         tt-param.nome-transp-ini  = ''                  
         tt-param.nome-transp-fin  = 'ZZZZZZZZZZZZZZ'    
         tt-param.corte-comerc-ini = ''
         tt-param.corte-comerc-fin = 'ZZZ'
         tt-param.so-indigo        = NO    
         tt-param.exc-indigo       = NO
         tt-param.it-codigo        = ''
         tt-param.sit-total        = YES
         tt-param.sit-aberto       = YES   
         tt-param.sit-parcial      = YES    
         tt-param.sit-pendentes    = YES    
         tt-param.sit-suspensos    = YES    
         tt-param.sit-cancelados   = YES
         tt-param.cond-credito     = "T"   
         tt-param.cond-pagto       = "T"    
         tt-param.mercado          = "A"    
         tt-param.tp-pedido        = ""    
         tt-param.aceita-parc      = "T"    
         tt-param.qtd-minima       = 0    
         tt-param.perc-minres      = 0    
         tt-param.min-it-ares      = 0    
         tt-param.max-it-ares      = 9999    
         tt-param.it-reservados    = YES.

  SESSION:SET-WAIT-STATE("general":U).

  RUN pi-carrega-dados.
  {include/i-rprun.i esrp/espd0002rp.p}

  IF tt-param.destino = 3 THEN DO.
     RUN utp/ut-utils.p PERSISTENT SET h-prog.
     RUN EXECUTE IN h-prog(INPUT "notepad.exe", 
                           INPUT tt-param.arquivo).
     DELETE PROCEDURE h-prog.
  END.

  SESSION:SET-WAIT-STATE("":U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-param
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-param C-Win
ON CHOOSE OF bt-param IN FRAME DEFAULT-FRAME /* Sair */
DO:
   ASSIGN c-win:SENSITIVE = NO.
   RUN esp/essp0167a.w (INPUT-OUTPUT c-dt-limite-ini,   
                        INPUT-OUTPUT c-dt-limite-fin,   
                        INPUT-OUTPUT c-nr-pedcli-ini,   
                        INPUT-OUTPUT c-nr-pedcli-fin,   
                        INPUT-OUTPUT c-nome-abrev-ini,
                        INPUT-OUTPUT c-nome-abrev-fin,
                        INPUT-OUTPUT c-no-ab-reppri-ini,
                        INPUT-OUTPUT c-no-ab-reppri-fin,
                        INPUT-OUTPUT i-reserva,
                        INPUT-OUTPUT l-ok). 
   IF l-ok THEN                                     
      RUN pi-popula-browse.
   ASSIGN c-win:SENSITIVE = YES.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-vapara
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-vapara C-Win
ON CHOOSE OF bt-vapara IN FRAME DEFAULT-FRAME
DO:
   RUN esdlg/d01essp0155.w (OUTPUT c-nr-pedcli).

   IF c-nr-pedcli <> "" THEN DO:
      FIND FIRST tt-ped-venda WHERE
                 tt-ped-venda.nr-pedcli = c-nr-pedcli NO-LOCK NO-ERROR.

      IF NOT AVAIL tt-ped-venda THEN DO.
         MESSAGE "Pedido nÆo est  contido na sele‡Æo!"
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
         RETURN NO-APPLY.
      END.

      h-query:REPOSITION-TO-ROWID(ROWID(tt-ped-venda)) NO-ERROR. 
      APPLY 'VALUE-CHANGED' TO br-pedidos.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 C-Win
ON CHOOSE OF BUTTON-1 IN FRAME DEFAULT-FRAME
DO:
  RUN pi-imprime.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

ON 'F5':U OF br-pedidos DO:
   {&OPEN-QUERY-br-pedidos}
   APPLY 'value-changed' TO br-pedidos.
END.

ASSIGN h-query = br-pedidos:QUERY.
br-pedidos:NUM-LOCKED-COLUMNS = 3.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

   RUN enable_UI.

   ASSIGN c-dt-limite-ini = '010001'
          c-dt-limite-fin = STRING(MONTH(TODAY),'99') + STRING(YEAR(TODAY),'9999').


   APPLY 'choose' TO bt-param.

   IF NOT THIS-PROCEDURE:PERSISTENT THEN
      WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY fi-dfi fi-dfr fi-dei fi-der fi-def fi-dri 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-4 rt-buttom br-pedidos bt-param bt-vapara bt-consulta bt-imprime 
         BUTTON-1 bt-excel bt-exit bt-ajuda 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-abre-excel C-Win 
PROCEDURE pi-abre-excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF INPUT PARAMETER p-arquivo AS CHAR.

 DEF VAR h-prog AS HANDLE NO-UNDO.
 RUN utp/ut-utils.p PERSISTENT SET h-prog.

 RUN EXECUTE IN h-prog(INPUT "EXCEL.EXE", INPUT p-arquivo).

 DELETE PROCEDURE h-prog.
 PAUSE 5 NO-MESSAGE.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-carrega-dados C-Win 
PROCEDURE pi-carrega-dados :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 {include/i-rprun.i esrp/espd0002arp.p}    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cor C-Win 
PROCEDURE pi-cor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*
    DEF INPUT PARAMETER p-cor AS INT.
    tt-ped-venda.nr-pedcli:FGCOLOR IN BROWSE {&browse-name} = p-cor.
    tt-ped-venda.nr-pedrep:FGCOLOR IN BROWSE {&browse-name} = p-cor.
    tt-ped-venda.nome-abrev:FGCOLOR IN BROWSE {&browse-name} = p-cor.
    tt-ped-venda.dt-entrega:FGCOLOR IN BROWSE {&browse-name} = p-cor.
    tt-ped-venda.qt-pedida:FGCOLOR IN BROWSE {&browse-name} = p-cor.
    tt-ped-venda.qt-faturada:FGCOLOR IN BROWSE {&browse-name} = p-cor.
    tt-ped-venda.qt-reservada:FGCOLOR IN BROWSE {&browse-name} = p-cor.
    tt-ped-venda.qt-cancelada:FGCOLOR IN BROWSE {&browse-name} = p-cor.
    tt-ped-venda.qt-aberto:FGCOLOR IN BROWSE {&browse-name} = p-cor.
    tt-ped-venda.situacao:FGCOLOR IN BROWSE {&browse-name} = p-cor. 
    tt-ped-venda.dt-implant:FGCOLOR IN BROWSE {&browse-name} = p-cor.
    tt-ped-venda.dt-apr-cred:FGCOLOR IN BROWSE {&browse-name} = p-cor.
    */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-gera-excel C-Win 
PROCEDURE pi-gera-excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF INPUT PARAMETER p-arq-saida AS CHAR FORMAT "x(45)".
    
 HIDE FRAME f-main.

 DEFINE FRAME frm_excel WITH SIZE 2 BY 2 ROW 1 COLUMN 1.
 ENABLE ALL WITH FRAME frm_excel.
   
 /* bloco principal do programa */
 find first param-global no-lock no-error.
 find first empresa
      where empresa.ep-codigo = param-global.empresa-prin no-lock no-error. 
    
 ASSIGN c-empresa = (IF AVAIL empresa THEN empresa.razao-social ELSE "").
    
 RUN pi-abre-excel (INPUT "").
 PAUSE 3 NO-MESSAGE.

 DDE INITIATE sys FRAME FRAME frm_excel:HANDLE APPLICATION "EXCEL" TOPIC "System".
 DDE INITIATE i-canal FRAME FRAME frm_excel:HANDLE APPLICATION "EXCEL" TOPIC "Pasta1".

 RUN pi-monta-planilha.

 OS-DELETE VALUE(p-arq-saida).
 DDE EXECUTE   sys COMMAND '[save.as("' + p-arq-saida + '")]'.
    
 DDE EXECUTE   sys COMMAND "[close(0)]". 
 DDE EXECUTE   sys COMMAND "[quit()]". 
   
 DDE TERMINATE sys.
    
 HIDE FRAME frm_excel.
 CLEAR FRAME frm_excel.
 DISABLE ALL WITH FRAME frm_excel.

 VIEW FRAME f-main.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imp-cabec C-Win 
PROCEDURE pi-imp-cabec :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    PUT c-empresa  FORMAT "X(40)"                 AT   1
        "DATA: "                                  AT  58
        STRING(TODAY,"99/99/9999") FORMAT "X(10)" AT  64
        "HORA: "                                  AT  85
        STRING(TIME,"hh:mm:ss")                   AT  91
        "PAG:"                                    AT 125
        i-pag FORMAT ">>"                         AT 130
        SKIP(1).
    CASE i-reserva:
        WHEN 1 THEN
            PUT "LEAD-TIME DOS PEDIDOS DE VENDAS COM RESERVAS" AT 51 SKIP(1).
        WHEN 2 THEN
            PUT "LEAD-TIME DOS PEDIDOS DE VENDAS SEM RESERVAS" AT 51 SKIP(1).
        WHEN 3 THEN
            PUT "LEAD-TIME DE TODOS OS PEDIDOS DE VENDAS" AT 51 SKIP(1).
    END CASE.
    PUT "Pedido Cliente      Tipo Pedido  Pedido Repr. Representante Dt.Implant Dt.Reserva DRI Dt.Faturam DFI   DFR Dt.entrega DEI   DER   DEF" AT 1.
    PUT "------ ------------ ------------ ------------ ------------- ---------- ---------- --- ---------- ---   --- ---------- ---   ---   ---" AT 1.
    ASSIGN i-pag = i-pag + 1.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imprime C-Win 
PROCEDURE pi-imprime :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR h-prog AS HANDLE NO-UNDO.
  DEF VAR i-ct   AS INT.

  RUN utp/ut-utils.p PERSISTENT SET h-prog.


  RUN esapi/saida-imp.p (OUTPUT i-saida,
                         OUTPUT c-saida,
                         OUTPUT i-num-copias,
                         OUTPUT l-ok).

  CASE i-saida:
      WHEN 1 THEN DO.
          OUTPUT TO PRINTER CONVERT TARGET "ibm850" PAGED PAGE-SIZE 61.
          PUT CONTROL "~033E~033(s18H".    
      END.
      WHEN 2 THEN
          OUTPUT TO VALUE(c-saida).
      WHEN 3 THEN DO.
          ASSIGN c-saida = SESSION:TEMP-DIRECTORY + "essp0167.tmp".
          OUTPUT TO VALUE(c-saida).
      END.
  END CASE.

  DO i-ct = 1 TO i-num-copias.
     ASSIGN fi-dri =  0
            fi-dfi =  0
            fi-dfr =  0
            fi-dei =  0
            fi-der =  0
            fi-def =  0
            i-ct   =  0
            i-pag  =  1
            i-lin  = 99.

     FOR EACH tt-ped-venda NO-LOCK.
         IF i-lin > 61 THEN DO:
           RUN pi-imp-cabec.
           ASSIGN i-lin = 7.
         END.
         PUT tt-ped-venda.nr-pedcli     FORMAT "x(6)"        AT   1
            tt-ped-venda.nome-abrev    FORMAT "x(12)"       AT   8
            tt-ped-venda.tp-pedido                          AT  21
            tt-ped-venda.nr-pedrep                          AT  34
            tt-ped-venda.no-ab-reppri                       AT  47
            tt-ped-venda.dt-implant                         AT  61
            tt-ped-venda.dt-reserva                         AT  72
            tt-ped-venda.dri                                AT  83
            tt-ped-venda.dt-emis-nota                       AT  87
            tt-ped-venda.dfi                                AT  98
            tt-ped-venda.dfr                                AT 104
            tt-ped-venda.dt-saida                           AT 108
            tt-ped-venda.dei                                AT 119
            tt-ped-venda.der                                AT 125
            tt-ped-venda.dif                                AT 131.

         ASSIGN fi-dri = fi-dri + tt-ped-venda.dri
                fi-dfi = fi-dfi + tt-ped-venda.dfi
                fi-dfr = fi-dfr + tt-ped-venda.dfr
                fi-dei = fi-dei + tt-ped-venda.dei
                fi-der = fi-der + tt-ped-venda.der
                fi-def = fi-def + tt-ped-venda.dif
                i-ct   = i-ct  + 1
                i-lin  = i-lin + 1.

     END.
     ASSIGN fi-dri = IF fi-dri > 0 THEN fi-dri / i-ct
                              ELSE fi-dri
       fi-dfi = IF fi-dfi > 0 THEN fi-dfi / i-ct 
                              ELSE fi-dfi
       fi-dfr = IF fi-dfr > 0 THEN fi-dfr / i-ct 
                              ELSE fi-dfr
       fi-dei = IF fi-dei > 0 THEN fi-dei / i-ct
                              ELSE fi-dei
       fi-der = IF fi-der > 0 THEN fi-der / i-ct
                              ELSE fi-der
       fi-def = IF fi-def > 0 THEN fi-def / i-ct
                              ELSE fi-def.
     IF i-lin > 61 THEN DO:
        RUN pi-imp-cabec.
        ASSIGN i-lin = 7.
      END.

     IF i-lin <> 7 THEN
        PUT "-----          ----- -----          ----- ----- -----"   AT 81.

     PUT "M‚dias.......:"  AT 66.
     PUT fi-dri FORMAT ">9.99" AT  81
     fi-dfi FORMAT ">9.99" AT  96
     fi-dfr FORMAT ">9.99" AT 102 
     fi-dei FORMAT ">9.99" AT 117
     fi-der FORMAT ">9.99" AT 123
     fi-def FORMAT ">9.99" AT 129.

     IF i-saida = 1 THEN DO:
        PAGE.
        PUT "" AT 1.
     END.
  END.
  IF i-saida = 3 THEN DO.
     RUN EXECUTE IN h-prog(INPUT "notepad.exe",
                           INPUT c-saida).
     DELETE PROCEDURE h-prog.
  END.
  OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-medias C-Win 
PROCEDURE pi-medias :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN fi-dri = 0
         fi-dfi = 0
         fi-dfr = 0
         fi-dei = 0
         fi-der = 0
         fi-def = 0
         i-ct   = 0.
  FOR EACH tt-ped-venda NO-LOCK.
      ASSIGN fi-dri = fi-dri + tt-ped-venda.dri
             fi-dfi = fi-dfi + tt-ped-venda.dfi
             fi-dfr = fi-dfr + tt-ped-venda.dfr
             fi-dei = fi-dei + tt-ped-venda.dei
             fi-der = fi-der + tt-ped-venda.der
             fi-def = fi-def + tt-ped-venda.dif
             i-ct   = i-ct + 1.
  END.

  ASSIGN fi-dri = IF fi-dri > 0 THEN fi-dri / i-ct
                                ELSE fi-dri
         fi-dfi = IF fi-dfi > 0 THEN fi-dfi / i-ct 
                                ELSE fi-dfi
         fi-dfr = IF fi-dfr > 0 THEN fi-dfr / i-ct 
                                ELSE fi-dfr
         fi-dei = IF fi-dei > 0 THEN fi-dei / i-ct
                                ELSE fi-dei
         fi-der = IF fi-der > 0 THEN fi-der / i-ct
                                ELSE fi-der
         fi-def = IF fi-def > 0 THEN fi-def / i-ct
                                ELSE fi-def.





  /*
  ASSIGN fi-tot-saldo:FGCOLOR IN FRAME {&FRAME-NAME} = ?.
  IF fi-tot-saldo < 0 THEN
     ASSIGN fi-tot-saldo:FGCOLOR IN FRAME {&FRAME-NAME} = 12.
  */
/*
  ASSIGN fi-dri:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(fi-dri / i-ct)  
         fi-dfi:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(fi-dfi / i-ct)
         fi-dfr:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(fi-dfr / i-ct)  
         fi-dei:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(fi-dei / i-ct)
         fi-der:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(fi-der / i-ct)  
         fi-def:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(fi-def / i-ct).
*/

  DISP fi-dri
       fi-dfi
       fi-dfr
       fi-dei      
       fi-der 
       fi-def
       WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-monta-planilha C-Win 
PROCEDURE pi-monta-planilha :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

 /* Cabe‡alho  da Planilha */
 ASSIGN c-Lin = c-empresa + "             " + " LEAD-TIME PEDIDOS VENDAS DE: "  + STRING(da-dt-entrega-ini, "99/99/9999") + " A " + STRING(da-dt-entrega-fin, "99/99/9999"). 
 DDE SEND i-canal SOURCE c-Lin ITEM "L1C1".
 DDE EXECUTE i-canal COMMAND '[select("L1C1:L1C10")]'.
 /* Fonte, Tamanho, Negrito, It lico, Sublinhado, Atachado, Cor(0=Autom tico, 1=Preto, 2=Branco 3=Vermelho...) */
 DDE EXECUTE sys COMMAND "[format.font(~"Verdana~",10,True,False,False,False,3)]".

 /* Cabe‡alho dos Dados */
                          
 DDE SEND i-canal SOURCE "PED.CLIENTE"   ITEM "L3C1".
 DDE SEND i-canal SOURCE "CLIENTE"       ITEM "L3C2".
 DDE SEND i-canal SOURCE "TIPO PEDIDO"   ITEM "L3C3".
 DDE SEND i-canal SOURCE "PED.REPRES."   ITEM "L3C4".
 DDE SEND i-canal SOURCE "REPRESENTANTE" ITEM "L3C5".
 DDE SEND i-canal SOURCE "DT IMPLANT"    ITEM "L3C6".
 DDE SEND i-canal SOURCE "DT RESERVA"    ITEM "L3C7".
 DDE SEND i-canal SOURCE "DRI"           ITEM "L3C8".
 DDE SEND i-canal SOURCE "DT EMISSÇO"    ITEM "L3C9".
 DDE SEND i-canal SOURCE "DFI"           ITEM "L3C10".
 DDE SEND i-canal SOURCE "DFR"           ITEM "L3c11".
 DDE SEND i-canal SOURCE "DT SAIDA"      ITEM "L3C12".
 DDE SEND i-canal SOURCE "DEI"           ITEM "L3C13".
 DDE SEND i-canal SOURCE "DER"           ITEM "L3C14".
 DDE SEND i-canal SOURCE "DIF"           ITEM "L3C15".
 /* Formata‡Æo das Celulas do Cabe‡alho de Dados */
 DDE EXECUTE i-canal COMMAND '[select("L3C1:L3C15")]'.
 /* Fonte, Tamanho, Negrito, It lico, Sublinhado, Atachado, Cor(0=Autom tico, 1=Preto, 2=Branco 3=Vermelho...) */
 DDE EXECUTE sys COMMAND "[format.font(~"Verdana~",8,True,False,False,False,5)]".

 DDE EXECUTE i-canal COMMAND "[select(~"C1~")]". 
 DDE EXECUTE sys     COMMAND "[column.width(12.00)]".

 DDE EXECUTE i-canal COMMAND "[select(~"C2~")]". 
 DDE EXECUTE sys     COMMAND "[column.width(13.00)]".

 DDE EXECUTE i-canal COMMAND "[select(~"C3~")]". 
 DDE EXECUTE sys     COMMAND "[column.width(18.00)]". 

 DDE EXECUTE i-canal COMMAND "[select(~"C4~")]". 
 DDE EXECUTE sys     COMMAND "[column.width(12.00)]".

 DDE EXECUTE i-canal COMMAND "[select(~"C5~")]". 
 DDE EXECUTE sys     COMMAND "[column.width(16.00)]".

 DDE EXECUTE i-canal COMMAND "[select(~"C6~")]". 
 DDE EXECUTE sys     COMMAND "[column.width(12.00)]".

 DDE EXECUTE i-canal COMMAND "[select(~"C7~")]". 
 DDE EXECUTE sys     COMMAND "[column.width(11.00)]".

 DDE EXECUTE i-canal COMMAND "[select(~"C8~")]". 
 DDE EXECUTE sys     COMMAND "[column.width(7.00)]".
 DDE EXECUTE sys     COMMAND "[format.number(~"###0,00~")]".
 DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]". 

 DDE EXECUTE i-canal COMMAND "[select(~"C9~")]". 
 DDE EXECUTE sys     COMMAND "[column.width(11.00)]".

 DDE EXECUTE i-canal COMMAND "[select(~"C10~")]". 
 DDE EXECUTE sys     COMMAND "[column.width(7.00)]".
 DDE EXECUTE sys     COMMAND "[format.number(~"###0,00~")]".
 DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]". 

 DDE EXECUTE i-canal COMMAND "[select(~"C11~")]". 
 DDE EXECUTE sys     COMMAND "[column.width(7.00)]".
 DDE EXECUTE sys     COMMAND "[format.number(~"###0,00~")]".
 DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]". 

 DDE EXECUTE i-canal COMMAND "[select(~"C12~")]". 
 DDE EXECUTE sys     COMMAND "[column.width(10.00)]".

 DDE EXECUTE i-canal COMMAND "[select(~"C13~")]". 
 DDE EXECUTE sys     COMMAND "[column.width(7.00)]".
 DDE EXECUTE sys     COMMAND "[format.number(~"###0,00~")]".
 DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]". 
 
 DDE EXECUTE i-canal COMMAND "[select(~"C14~")]". 
 DDE EXECUTE sys     COMMAND "[column.width(7.00)]".
 DDE EXECUTE sys     COMMAND "[format.number(~"###0,00~")]".
 DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]".  

 DDE EXECUTE i-canal COMMAND "[select(~"C15~")]". 
 DDE EXECUTE sys     COMMAND "[column.width(7.00)]".
 DDE EXECUTE sys     COMMAND "[format.number(~"###0,00~")]".
 DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]". 
 
 /* Montagem das Celulas de Dados */
 ASSIGN fi-dri = 0
        fi-dfi = 0
        fi-dfr = 0
        fi-dei = 0
        fi-der = 0
        fi-def = 0
        i-ct   = 0
        i-Lin  = 4.
 FOR EACH tt-ped-venda NO-LOCK.
     DDE SEND i-canal SOURCE STRING(tt-ped-venda.nr-pedcli)    ITEM "L" + TRIM(STRING(i-Lin)) + "C1". 
     DDE SEND i-canal SOURCE STRING(tt-ped-venda.nome-abrev)   ITEM "L" + TRIM(STRING(i-Lin)) + "C2".
     DDE SEND i-canal SOURCE STRING(tt-ped-venda.tipo-pedido)  ITEM "L" + TRIM(STRING(i-Lin)) + "C3".
     DDE SEND i-canal SOURCE STRING(tt-ped-venda.nr-pedrep)    ITEM "L" + TRIM(STRING(i-Lin)) + "C4".
     DDE SEND i-canal SOURCE STRING(tt-ped-venda.no-ab-reppri) ITEM "L" + TRIM(STRING(i-Lin)) + "C5".
     IF tt-ped-venda.dt-implant <> ? THEN
        DDE SEND i-canal SOURCE STRING(tt-ped-venda.dt-implant)   ITEM "L" + TRIM(STRING(i-Lin)) + "C6".
     IF tt-ped-venda.dt-reserva <> ? THEN
        DDE SEND i-canal SOURCE STRING(tt-ped-venda.dt-reserva)   ITEM "L" + TRIM(STRING(i-Lin)) + "C7".
     DDE SEND i-canal SOURCE STRING(tt-ped-venda.dri)          ITEM "L" + TRIM(STRING(i-Lin)) + "C8".
     IF tt-ped-venda.dt-emis-nota <> ? THEN
        DDE SEND i-canal SOURCE STRING(tt-ped-venda.dt-emis-nota) ITEM "L" + TRIM(STRING(i-Lin)) + "C9".
     DDE SEND i-canal SOURCE STRING(tt-ped-venda.dfi)          ITEM "L" + TRIM(STRING(i-Lin)) + "C10".
     DDE SEND i-canal SOURCE STRING(tt-ped-venda.dfr)          ITEM "L" + TRIM(STRING(i-Lin)) + "C11".
     IF tt-ped-venda.dt-saida <> ? THEN
        DDE SEND i-canal SOURCE STRING(tt-ped-venda.dt-saida)     ITEM "L" + TRIM(STRING(i-Lin)) + "C12".
     DDE SEND i-canal SOURCE STRING(tt-ped-venda.dei)          ITEM "L" + TRIM(STRING(i-Lin)) + "C13".
     DDE SEND i-canal SOURCE STRING(tt-ped-venda.der)          ITEM "L" + TRIM(STRING(i-Lin)) + "C14".
     DDE SEND i-canal SOURCE STRING(tt-ped-venda.dif)          ITEM "L" + TRIM(STRING(i-Lin)) + "C15".  
     
     ASSIGN aux-command = '[select("L' + TRIM(STRING(i-Lin)) + 'C1:L' + TRIM(STRING(i-Lin)) + 'C15")]'.
     DDE EXECUTE i-canal COMMAND aux-command.
     /* Fonte, Tamanho, Negrito, It lico, Sublinhado, Atachado, Cor(0=Autom tico, 1=Preto, 2=Branco 3=Vermelho...) */
     DDE EXECUTE sys COMMAND "[format.font(~"Verdana~",8,False,False,False,False,0)]".

     ASSIGN i-Lin = i-Lin + 1
            i-ct  = i-ct  + 1
            fi-dri = fi-dri + tt-ped-venda.dri
            fi-dfi = fi-dfi + tt-ped-venda.dfi
            fi-dfr = fi-dfr + tt-ped-venda.dfr
            fi-dei = fi-dei + tt-ped-venda.dei
            fi-der = fi-der + tt-ped-venda.der
            fi-def = fi-def + tt-ped-venda.dif.
 END.
 ASSIGN fi-dri = IF fi-dri > 0 THEN fi-dri / i-ct
                               ELSE fi-dri
        fi-dfi = IF fi-dfi > 0 THEN fi-dfi / i-ct 
                               ELSE fi-dfi
        fi-dfr = IF fi-dfr > 0 THEN fi-dfr / i-ct 
                               ELSE fi-dfr
        fi-dei = IF fi-dei > 0 THEN fi-dei / i-ct
                               ELSE fi-dei
        fi-der = IF fi-der > 0 THEN fi-der / i-ct
                               ELSE fi-der
        fi-def = IF fi-def > 0 THEN fi-def / i-ct
                               ELSE fi-def.

 DDE SEND i-canal SOURCE "Total Geral"  ITEM "L" + TRIM(STRING(i-Lin)) + "C6".
 DDE SEND i-canal SOURCE STRING(fi-dri) ITEM "L" + TRIM(STRING(i-Lin)) + "C8".
 DDE SEND i-canal SOURCE STRING(fi-dfi) ITEM "L" + TRIM(STRING(i-Lin)) + "C10".
 DDE SEND i-canal SOURCE STRING(fi-dfr) ITEM "L" + TRIM(STRING(i-Lin)) + "C11".
 DDE SEND i-canal SOURCE STRING(fi-dei) ITEM "L" + TRIM(STRING(i-Lin)) + "C13".
 DDE SEND i-canal SOURCE STRING(fi-der) ITEM "L" + TRIM(STRING(i-Lin)) + "C14".
 DDE SEND i-canal SOURCE STRING(fi-def) ITEM "L" + TRIM(STRING(i-Lin)) + "C15".
 ASSIGN aux-command = '[select("L' + TRIM(STRING(i-Lin)) + 'C1:L' + TRIM(STRING(i-Lin)) + 'C15")]'.
 DDE EXECUTE i-canal COMMAND aux-command.
 /* Fonte, Tamanho, Negrito, It lico, Sublinhado, Atachado, Cor(0=Autom tico, 1=Preto, 2=Branco 3=Vermelho...) */
 DDE EXECUTE sys COMMAND "[format.font(~"Verdana~",8,True,False,False,False,0)]".
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-popula-browse C-Win 
PROCEDURE pi-popula-browse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
   {utp/ut-liter.i Selecionando_Pedidos *}
   RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

   EMPTY TEMP-TABLE tt-ped-venda.

     /* Busca Nome da Empresa */
   FIND FIRST param-global NO-LOCK NO-ERROR.
   FIND FIRST empresa WHERE
              empresa.ep-codigo = param-global.empresa-prin NO-LOCK NO-ERROR. 
   ASSIGN c-empresa = (IF AVAIL empresa THEN empresa.razao-social ELSE "").
   
   ASSIGN fi-dri = 0
          fi-dfi = 0
          fi-dfr = 0
          fi-dei = 0
          fi-der = 0
          fi-def = 0.

   RUN esapi/ret-udm.p (INPUT c-dt-limite-fin, OUTPUT c-dia).
   ASSIGN da-dt-entrega-ini = DATE('01' + SUBSTR(c-dt-limite-ini,1,2) + SUBSTR(c-dt-limite-ini,3,4))
          da-dt-entrega-fin = DATE(c-dia + SUBSTR(c-dt-limite-fin,1,2) + SUBSTR(c-dt-limite-fin,3,4)).


   RUN pi-separa-pedidos.

   RUN pi-finalizar in h-acomp.
   
   DISP fi-dri
        fi-dfi
        fi-dfr
        fi-dei
        fi-der
        fi-def
        WITH FRAME {&FRAME-NAME}.
   
   {&OPEN-QUERY-br-pedidos}
   APPLY 'value-changed' TO br-pedidos.
   APPLY 'entry' TO br-pedidos.
   RETURN NO-APPLY.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-separa-pedidos C-Win 
PROCEDURE pi-separa-pedidos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH ped-venda WHERE
             ped-venda.dt-implant >= da-dt-entrega-ini AND
             ped-venda.dt-implant <= da-dt-entrega-fin AND
             ped-venda.cod-sit-ped = 3 AND 
             ped-venda.nr-pedcli  >= c-nr-pedcli-ini AND
             ped-venda.nr-pedcli  <= c-nr-pedcli-fin AND 
             ped-venda.nome-abrev >= c-nome-abrev-ini AND
             ped-venda.nome-abrev <= c-nome-abrev-fin AND
             ped-venda.no-ab-reppri >= c-no-ab-reppri-ini AND   
             ped-venda.no-ab-reppri <= c-no-ab-reppri-fin  NO-LOCK
             USE-INDEX ch-implant.

        RUN pi-acompanhar IN h-acomp (INPUT "Data: " + STRING(ped-venda.dt-entrega) + "   " + 
                                            "Pedido: " + ped-venda.nr-pedcli).

        ASSIGN da-reserva = ?.
        FIND ped-venda-ext WHERE
             ped-venda-ext.nr-pedido = ped-venda.nr-pedido NO-LOCK NO-ERROR.
        IF AVAIL ped-venda-ext AND ped-venda-ext.num-reserva <> 0 THEN DO.
           IF i-reserva = 2  THEN NEXT. /* Sele‡Æo ‚ de Pedidos sem Reserva */
           FOR LAST ped-item-res WHERE
                    ped-item-res.nome-abrev = ped-venda.nome-abrev AND
                    ped-item-res.nr-pedcli =  ped-venda.nr-pedcli NO-LOCK
                    BY ped-item-res.dt-trans.
               ASSIGN da-reserva = ped-item-res.dt-trans.
           END.
        END.
        ELSE DO.
            IF i-reserva = 1  THEN NEXT. /* Sele‡Æo ‚ de Pedidos com Reserva */
            FOR LAST ped-item-res WHERE
                     ped-item-res.nome-abrev = ped-venda.nome-abrev AND
                     ped-item-res.nr-pedcli =  ped-venda.nr-pedcli NO-LOCK
                     BY ped-item-res.dt-trans.
                ASSIGN da-reserva = ped-item-res.dt-trans.
            END.
        END.

        IF da-reserva = ? THEN NEXT.
        
        FIND LAST nota-fiscal WHERE
                  nota-fiscal.nome-ab-cli = ped-venda.nome-abrev AND
                  nota-fiscal.nr-pedcli   = ped-venda.nr-pedcli AND 
                  nota-fiscal.dt-cancela  = ? NO-LOCK NO-ERROR.

        FIND tt-ped-venda WHERE
             tt-ped-venda.nr-pedcli  = ped-venda.nr-pedcli AND
             tt-ped-venda.nome-abrev = ped-venda.nome-abrev NO-ERROR.
        IF NOT AVAIL tt-ped-venda THEN DO:
           CREATE tt-ped-venda.
           BUFFER-COPY ped-venda TO tt-ped-venda.
           
           ASSIGN tt-ped-venda.dt-emis-nota = nota-fiscal.dt-emis-nota
                  tt-ped-venda.dt-saida-nf  = nota-fiscal.dt-saida
                  tt-ped-venda.dri = ABS(ped-venda.dt-implant - da-reserva)                
                  tt-ped-venda.dfi = ABS(ped-venda.dt-implant - nota-fiscal.dt-emis-nota)  
                  tt-ped-venda.dfr = ABS(nota-fiscal.dt-emis-nota - da-reserva)            
                  tt-ped-venda.dei = IF nota-fiscal.dt-saida <> ? THEN ABS(nota-fiscal.dt-saida - ped-venda.dt-implant)      
                                                                  ELSE ABS(TODAY - ped-venda.dt-implant)
                  tt-ped-venda.der = IF nota-fiscal.dt-saida <> ? THEN ABS(nota-fiscal.dt-saida - da-reserva)                
                                                                  ELSE ABS(TODAY - da-reserva)
                  tt-ped-venda.dif = IF nota-fiscal.dt-saida <> ? THEN ABS(nota-fiscal.dt-saida - nota-fiscal.dt-emis-nota) 
                                                                  ELSE ABS(TODAY - nota-fiscal.dt-emis-nota)
                  tt-ped-venda.tipo-pedido  = IF AVAIL ped-venda-ext THEN ped-venda-ext.tp-pedido
                                                                     ELSE ""
                  tt-ped-venda.dt-reserva   = da-reserva.

        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

