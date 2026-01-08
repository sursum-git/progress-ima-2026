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
{include/i-prgvrs.i ESSP0169 2.04.00.999}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
/* Global Variable Definitions ---                                       */

/* Temp Table Definitions ---                                           */
DEF TEMP-TABLE tt-carga
    FIELD nome-transp  LIKE transporte.nome
    FIELD nome-abrev   LIKE transporte.nome-abrev
    FIELD cod-transp   LIKE transporte.cod-trans
    FIELD carga-kgs    AS DEC  EXTENT 5
    FIELD carga-mts    AS DEC  EXTENT 5
    INDEX indice1 IS PRIMARY cod-transp.

DEF TEMP-TABLE tt-detalhe
    FIELD dia-semana   AS CHAR
    FIELD cap-diaria   AS DEC  
    FIELD tot-dia-kg   AS DEC
    FIELD saldo-kg     AS DEC
    FIELD realiz-kg    AS DEC
    INDEX indice1 IS PRIMARY dia-semana.

DEF TEMP-TABLE tt-notas
    FIELD cod-estabel LIKE nota-fiscal.nr-nota-fis
    FIELD serie       LIKE nota-fiscal.serie
    FIELD nr-nota-fis LIKE nota-fiscal.nr-nota-fis
    FIELD dia-semana  AS CHAR 
    FIELD dia         AS INT
    FIELD carga-kgs   AS DEC 
    FIELD carga-mts   AS DEC
    INDEX indice1 IS PRIMARY cod-estabel serie nr-nota-fis.

/* Local Variable Definitions ---                                       */
DEF VAR h-acomp       AS HANDLE NO-UNDO.
DEF VAR h-query       AS HANDLE.
DEF VAR c-empresa     AS CHAR.
DEF VAR c-transp      AS CHAR.
DEF VAR c-semana      AS CHAR INIT "DOMINGO,2¶ FEIRA,3¶ FEIRA,4¶ FEIRA,5¶ FEIRA,6¶ FEIRA,SABADO".
DEF VAR de-mts        AS DEC.
DEF VAR de-capacidade AS DEC.
DEF VAR i-dia         AS INT.


/* Variavies de ParÉmetros */
DEFINE VAR da-dt-saida       AS DATE.
DEFINE VAR c-nome-transp-ini AS CHAR INIT "".
DEFINE VAR c-nome-transp-fin AS CHAR INIT "ZZZZZZZZZZZZ".
DEFINE VAR de-cap-diaria01   AS DEC INIT 30000.
DEFINE VAR de-cap-diaria02   AS DEC INIT 30000.
DEFINE VAR de-cap-diaria03   AS DEC INIT 30000.
DEFINE VAR de-cap-diaria04   AS DEC INIT 30000.
DEFINE VAR de-cap-diaria05   AS DEC INIT 30000.
DEFINE VAR c-cod-estabel     LIKE estabelec.cod-estabel INIT "2".
DEFINE VAR l-ok              AS LOG.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME br-carga

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-carga tt-detalhe

/* Definitions for BROWSE br-carga                                      */
&Scoped-define FIELDS-IN-QUERY-br-carga tt-carga.cod-transp /* */ tt-carga.nome-abrev /* */ tt-carga.carga-kgs[1] /* */ tt-carga.carga-mts[1] /* */ tt-carga.carga-kgs[2] /* */ tt-carga.carga-mts[2] /* */ tt-carga.carga-kgs[3] /* */ tt-carga.carga-mts[3] /* */ tt-carga.carga-kgs[4] /* */ tt-carga.carga-mts[4] /* */ tt-carga.carga-kgs[5] /* */ tt-carga.carga-mts[5] /* */   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-carga   
&Scoped-define SELF-NAME br-carga
&Scoped-define QUERY-STRING-br-carga FOR EACH tt-carga NO-LOCK
&Scoped-define OPEN-QUERY-br-carga OPEN QUERY {&SELF-NAME} FOR EACH tt-carga NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-carga tt-carga
&Scoped-define FIRST-TABLE-IN-QUERY-br-carga tt-carga


/* Definitions for BROWSE br-detalhe                                    */
&Scoped-define FIELDS-IN-QUERY-br-detalhe tt-detalhe.dia-semana tt-detalhe.cap-diaria tt-detalhe.tot-dia-kg tt-detalhe.saldo-kg tt-detalhe.realiz-kg   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-detalhe   
&Scoped-define SELF-NAME br-detalhe
&Scoped-define OPEN-QUERY-br-detalhe RUN pi-totais. OPEN QUERY {&SELF-NAME} FOR EACH tt-detalhe NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-detalhe tt-detalhe
&Scoped-define FIRST-TABLE-IN-QUERY-br-detalhe tt-detalhe


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-br-carga}~
    ~{&OPEN-QUERY-br-detalhe}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-buttom bt-param br-carga bt-vapara ~
bt-consulta bt-modifica bt-imprime bt-excel br-detalhe bt-exit 
&Scoped-Define DISPLAYED-OBJECTS fi-titulo-brw fi-cab-01 fi-cab-02 ~
fi-cab-03 fi-cab-04 fi-cab-05 fi-cab-06 fi-cap-diaria fi-tot-dia fi-saldo ~
fi-realizado 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-6 bt-excel 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-dia-carga C-Win 
FUNCTION fn-dia-carga RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-dia-mes C-Win 
FUNCTION fn-dia-mes RETURNS CHARACTER
  (INPUT da-dt-saida AS DATE) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-consulta AUTO-GO 
     IMAGE-UP FILE "image/im-det.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Detalhar Notas Fiscais por Transportadora"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-excel 
     IMAGE-UP FILE "image/excel.bmp":U
     LABEL "Button 2" 
     SIZE 5 BY 1.29 TOOLTIP "Gerar Planilha Excel".

DEFINE BUTTON bt-exit AUTO-END-KEY 
     IMAGE-UP FILE "image/im-exi.bmp":U
     LABEL "" 
     SIZE 4.86 BY 1.29 TOOLTIP "Fechar Programa"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-imprime 
     IMAGE-UP FILE "image/im-printer.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Imprimir".

DEFINE BUTTON bt-modifica AUTO-END-KEY 
     IMAGE-UP FILE "image/im-carga.bmp":U
     LABEL "&Sair" 
     SIZE 4.86 BY 1.25 TOOLTIP "Detalhar Notas Fiscais por Data Saida"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-param AUTO-GO 
     IMAGE-UP FILE "image/im-fil.bmp":U
     LABEL "&Sair" 
     SIZE 5 BY 1.25 TOOLTIP "ParÉmetros"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-vapara AUTO-GO 
     IMAGE-UP FILE "image/im-vapra.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Posicionar na Transportadora"
     BGCOLOR 8 FONT 10.

DEFINE VARIABLE fi-cab-01 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 23.29 BY .75
     BGCOLOR 8  NO-UNDO.

DEFINE VARIABLE fi-cab-02 AS CHARACTER FORMAT "X(256)":C27 
     VIEW-AS FILL-IN 
     SIZE 21.43 BY .75
     BGCOLOR 8 FONT 1 NO-UNDO.

DEFINE VARIABLE fi-cab-03 AS CHARACTER FORMAT "X(256)":C27 
     VIEW-AS FILL-IN 
     SIZE 21.43 BY .75
     BGCOLOR 8 FONT 1 NO-UNDO.

DEFINE VARIABLE fi-cab-04 AS CHARACTER FORMAT "X(256)":C27 
     VIEW-AS FILL-IN 
     SIZE 21.29 BY .75
     BGCOLOR 8 FONT 1 NO-UNDO.

DEFINE VARIABLE fi-cab-05 AS CHARACTER FORMAT "X(256)":C27 
     VIEW-AS FILL-IN 
     SIZE 21.29 BY .75
     BGCOLOR 8 FONT 1 NO-UNDO.

DEFINE VARIABLE fi-cab-06 AS CHARACTER FORMAT "X(256)":C27 
     VIEW-AS FILL-IN 
     SIZE 21.72 BY .75
     BGCOLOR 8 FONT 1 NO-UNDO.

DEFINE VARIABLE fi-cap-diaria AS DECIMAL FORMAT "->>,>>>,>>9.99":R48 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 25 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-realizado AS DECIMAL FORMAT "->>,>>>,>>9.99":R38 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 22 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-saldo AS DECIMAL FORMAT "->>,>>>,>>9.99":R38 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 22 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-titulo-brw AS CHARACTER FORMAT "X(256)":C180 
     VIEW-AS FILL-IN 
     SIZE 129.29 BY .75
     BGCOLOR 1 FGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE fi-tot-dia AS DECIMAL FORMAT "->>,>>>,>>9.99":R48 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 25 BY .88
     FONT 1 NO-UNDO.

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 7 BY 20
     BGCOLOR 8 FGCOLOR 0 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-carga FOR 
      tt-carga SCROLLING.

DEFINE QUERY br-detalhe FOR 
      tt-detalhe SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-carga
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-carga C-Win _FREEFORM
  QUERY br-carga NO-LOCK DISPLAY
      tt-carga.cod-transp   COLUMN-LABEL "COD." FORMAT ">>>,>>9"       /* LABEL-FGCOLOR  15 LABEL-BGCOLOR 9 */ WIDTH  5
      tt-carga.nome-abrev   COLUMN-LABEL "TRANSPORTADORA"              /* LABEL-FGCOLOR  15 LABEL-BGCOLOR 9 */ WIDTH 17 
      tt-carga.carga-kgs[1] COLUMN-LABEL "KGS"  FORMAT ">>>>,>>9.99":U /* LABEL-FGCOLOR  15 LABEL-BGCOLOR 9 */ WIDTH 10
      tt-carga.carga-mts[1] COLUMN-LABEL "MTS"  FORMAT ">>>>,>>9.99":U /* LABEL-FGCOLOR  15 LABEL-BGCOLOR 9 */ WIDTH 10
      tt-carga.carga-kgs[2] COLUMN-LABEL "KGS"  FORMAT ">>>>,>>9.99":U /* LABEL-FGCOLOR  15 LABEL-BGCOLOR 9 */ WIDTH 10
      tt-carga.carga-mts[2] COLUMN-LABEL "MTS"  FORMAT ">>>>,>>9.99":U /* LABEL-FGCOLOR  15 LABEL-BGCOLOR 9 */ WIDTH 10
      tt-carga.carga-kgs[3] COLUMN-LABEL "KGS"  FORMAT ">>>>,>>9.99":U /* LABEL-FGCOLOR  15 LABEL-BGCOLOR 9 */ WIDTH 10
      tt-carga.carga-mts[3] COLUMN-LABEL "MTS"  FORMAT ">>>>,>>9.99":U /* LABEL-FGCOLOR  15 LABEL-BGCOLOR 9 */ WIDTH 10
      tt-carga.carga-kgs[4] COLUMN-LABEL "KGS"  FORMAT ">>>>,>>9.99":U /* LABEL-FGCOLOR  15 LABEL-BGCOLOR 9 */ WIDTH 10
      tt-carga.carga-mts[4] COLUMN-LABEL "MTS"  FORMAT ">>>>,>>9.99":U /* LABEL-FGCOLOR  15 LABEL-BGCOLOR 9 */ WIDTH 10
      tt-carga.carga-kgs[5] COLUMN-LABEL "KGS"  FORMAT ">>>>,>>9.99":U /* LABEL-FGCOLOR  15 LABEL-BGCOLOR 9 */ WIDTH 10
      tt-carga.carga-mts[5] COLUMN-LABEL "MTS"  FORMAT ">>>>,>>9.99":U /* LABEL-FGCOLOR  15 LABEL-BGCOLOR 9 */ WIDTH 10
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-BOX NO-ROW-MARKERS SEPARATORS SIZE 131.72 BY 11.79
         FONT 1 ROW-HEIGHT-CHARS .46.

DEFINE BROWSE br-detalhe
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-detalhe C-Win _FREEFORM
  QUERY br-detalhe NO-LOCK DISPLAY
      tt-detalhe.dia-semana  COLUMN-LABEL "D I A"                    FORMAT "x(20)"            WIDTH 16 
      tt-detalhe.cap-diaria  COLUMN-LABEL "CAPACIDADE DIARIA (KGS)"  FORMAT ">>>,>>>,>>9.99":U WIDTH 30
      tt-detalhe.tot-dia-kg  COLUMN-LABEL "TOTAL DIA (KGS)"          FORMAT ">>>,>>>,>>9.99":U WIDTH 30
      tt-detalhe.saldo-kg    COLUMN-LABEL "SALDO (KGS)"              FORMAT ">>>,>>>,>>9.99":U WIDTH 25
      tt-detalhe.realiz-kg   COLUMN-LABEL "REALIZADO (KGS)"          FORMAT ">>>,>>>,>>9.99":U WIDTH 25
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 131.72 BY 6.25
         FONT 1
         TITLE "DETALHAMENTO DIARIO GERAL" ROW-HEIGHT-CHARS .46.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fi-titulo-brw AT ROW 1.08 COL 1.72 NO-LABEL WIDGET-ID 16
     bt-param AT ROW 1.54 COL 134.72
     fi-cab-01 AT ROW 1.75 COL 1.72 NO-LABEL WIDGET-ID 2
     fi-cab-02 AT ROW 1.75 COL 22.72 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     fi-cab-03 AT ROW 1.75 COL 43.86 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     fi-cab-04 AT ROW 1.75 COL 65.14 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     fi-cab-05 AT ROW 1.75 COL 86.14 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     fi-cab-06 AT ROW 1.75 COL 107.29 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     br-carga AT ROW 2.46 COL 1.72
     bt-vapara AT ROW 2.83 COL 134.72
     bt-consulta AT ROW 4.67 COL 134.72
     bt-modifica AT ROW 6.5 COL 134.72
     bt-imprime AT ROW 9.33 COL 134.72
     bt-excel AT ROW 13.21 COL 134.72
     br-detalhe AT ROW 14.33 COL 1.72 WIDGET-ID 100
     bt-exit AT ROW 19.54 COL 134.72
     fi-cap-diaria AT ROW 20.75 COL 21.86 COLON-ALIGNED NO-LABEL WIDGET-ID 18
     fi-tot-dia AT ROW 20.75 COL 52.86 COLON-ALIGNED NO-LABEL WIDGET-ID 20
     fi-saldo AT ROW 20.75 COL 81.43 COLON-ALIGNED NO-LABEL WIDGET-ID 22
     fi-realizado AT ROW 20.75 COL 106.57 COLON-ALIGNED NO-LABEL WIDGET-ID 24
     rt-buttom AT ROW 1.25 COL 133.72
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 140 BY 20.79
         FONT 1.


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
         TITLE              = "Controle Diario de Carregamento"
         COLUMN             = 1.57
         ROW                = 7.96
         HEIGHT             = 20.79
         WIDTH              = 140
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
         FONT               = 1
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
/* BROWSE-TAB br-carga fi-cab-06 DEFAULT-FRAME */
/* BROWSE-TAB br-detalhe bt-excel DEFAULT-FRAME */
/* SETTINGS FOR BUTTON bt-excel IN FRAME DEFAULT-FRAME
   6                                                                    */
/* SETTINGS FOR FILL-IN fi-cab-01 IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN fi-cab-02 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-cab-03 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-cab-04 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-cab-05 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-cab-06 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-cap-diaria IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-realizado IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-saldo IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-titulo-brw IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN fi-tot-dia IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-carga
/* Query rebuild information for BROWSE br-carga
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-carga NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "mgmov.ped-venda.cod-sit-ped = 1"
     _Query            is OPENED
*/  /* BROWSE br-carga */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-detalhe
/* Query rebuild information for BROWSE br-detalhe
     _START_FREEFORM
RUN pi-totais.
OPEN QUERY {&SELF-NAME} FOR EACH tt-detalhe NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "mgmov.ped-venda.cod-sit-ped = 1"
     _Query            is OPENED
*/  /* BROWSE br-detalhe */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Controle Diario de Carregamento */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Controle Diario de Carregamento */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-carga
&Scoped-define SELF-NAME br-carga
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-carga C-Win
ON ROW-DISPLAY OF br-carga IN FRAME DEFAULT-FRAME
DO:
    /*
  IF tt-ob-etiqueta.dif-kg < 0 AND tt-ob-etiqueta.peso-bruto <> 0 THEN
     tt-ob-etiqueta.dif-kg:FGCOLOR IN BROWSE br-ob-etiquetas = 12.
  IF tt-ob-etiqueta.gramatura-ideal > tt-ob-etiqueta.gramatura  THEN
         tt-ob-etiqueta.gramatura-ideal:FGCOLOR IN BROWSE br-ob-etiquetas = 12.
*/         
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-detalhe
&Scoped-define SELF-NAME br-detalhe
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-detalhe C-Win
ON ROW-DISPLAY OF br-detalhe IN FRAME DEFAULT-FRAME /* DETALHAMENTO DIARIO GERAL */
DO:
    /*
  IF tt-ob-etiqueta.dif-kg < 0 AND tt-ob-etiqueta.peso-bruto <> 0 THEN
     tt-ob-etiqueta.dif-kg:FGCOLOR IN BROWSE br-ob-etiquetas = 12.
  IF tt-ob-etiqueta.gramatura-ideal > tt-ob-etiqueta.gramatura  THEN
         tt-ob-etiqueta.gramatura-ideal:FGCOLOR IN BROWSE br-ob-etiquetas = 12.
*/         
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-consulta C-Win
ON CHOOSE OF bt-consulta IN FRAME DEFAULT-FRAME
DO:
   ASSIGN c-win:SENSITIVE = NO.
   RUN esp/essp0197b.p (INPUT TABLE tt-notas,
                        INPUT tt-carga.nome-abrev).
   ASSIGN c-win:SENSITIVE = YES.
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


&Scoped-define SELF-NAME bt-modifica
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-modifica C-Win
ON CHOOSE OF bt-modifica IN FRAME DEFAULT-FRAME /* Sair */
DO:
    /*
  FIND corte-comerc WHERE
       corte-comerc.codigo = tt-ob-etiqueta.corte-comerc NO-LOCK NO-ERROR.
  ASSIGN gr-corte-comerc = ROWID(corte-comerc).
      
  ASSIGN c-win:SENSITIVE = NO.
  RUN esp\essp0125.w.
  ASSIGN c-win:SENSITIVE = YES.
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-param
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-param C-Win
ON CHOOSE OF bt-param IN FRAME DEFAULT-FRAME /* Sair */
DO:
   ASSIGN c-win:SENSITIVE = NO.
   RUN esp/essp0197a.w (INPUT-OUTPUT da-dt-saida,   
                        INPUT-OUTPUT c-nome-transp-ini,   
                        INPUT-OUTPUT c-nome-transp-fin,   
                        INPUT-OUTPUT de-cap-diaria01, 
                        INPUT-OUTPUT de-cap-diaria02, 
                        INPUT-OUTPUT de-cap-diaria03, 
                        INPUT-OUTPUT de-cap-diaria04, 
                        INPUT-OUTPUT de-cap-diaria05, 
                        INPUT-OUTPUT c-cod-estabel,
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
  
   RUN esdlg/d01essp0197.w (OUTPUT c-transp). 

   IF c-transp <> ? THEN DO:
      FIND tt-carga WHERE
           tt-carga.cod-transp   = INT(c-transp) NO-LOCK NO-ERROR.

      IF NOT AVAIL tt-carga THEN DO.
         FIND tt-carga WHERE
              tt-carga.nome-abrev = c-transp NO-LOCK NO-ERROR.

         IF NOT AVAIL tt-carga THEN DO:
            MESSAGE "Codigo ou Nome Abreviado n∆o est† contido na seleá∆o!"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN NO-APPLY.
         END.
      END.

      h-query:REPOSITION-TO-ROWID(ROWID(tt-carga)) NO-ERROR. 
      APPLY 'VALUE-CHANGED' TO br-carga.
   END.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-carga
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

ON 'F5':U OF br-carga DO:
   {&OPEN-QUERY-br-carga}
   APPLY 'value-changed' TO br-carga.
END.

ASSIGN h-query = br-carga:QUERY.
br-carga:NUM-LOCKED-COLUMNS = 3.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

   RUN enable_UI.

   ASSIGN da-dt-saida = TODAY.

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
  DISPLAY fi-titulo-brw fi-cab-01 fi-cab-02 fi-cab-03 fi-cab-04 fi-cab-05 
          fi-cab-06 fi-cap-diaria fi-tot-dia fi-saldo fi-realizado 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE rt-buttom bt-param br-carga bt-vapara bt-consulta bt-modifica 
         bt-imprime bt-excel br-detalhe bt-exit 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-popula-browse C-Win 
PROCEDURE pi-popula-browse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
   {utp/ut-liter.i Selecionando_Notas_Fiscais *}
   RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

   FOR EACH tt-carga.
       DELETE tt-carga.
   END.

   /* Busca Nome da Empresa */
   FIND FIRST param-global NO-LOCK NO-ERROR.
   FIND FIRST empresa WHERE
              empresa.ep-codigo = param-global.empresa-prin NO-LOCK NO-ERROR. 
   ASSIGN c-empresa = (IF AVAIL empresa THEN empresa.razao-social ELSE "").

   ASSIGN fi-cab-02:SCREEN-VALUE IN FRAME {&FRAME-NAME} = fn-dia-mes(INPUT da-dt-saida)     + ENTRY(WEEKDAY(da-dt-saida),     c-semana,",") 
          fi-cab-03:SCREEN-VALUE IN FRAME {&FRAME-NAME} = fn-dia-mes(INPUT da-dt-saida + 1) + ENTRY(WEEKDAY(da-dt-saida + 1), c-semana,",") 
          fi-cab-04:SCREEN-VALUE IN FRAME {&FRAME-NAME} = fn-dia-mes(INPUT da-dt-saida + 2) + ENTRY(WEEKDAY(da-dt-saida + 2), c-semana,",") 
          fi-cab-05:SCREEN-VALUE IN FRAME {&FRAME-NAME} = fn-dia-mes(INPUT da-dt-saida + 3) + ENTRY(WEEKDAY(da-dt-saida + 3), c-semana,",") 
          fi-cab-06:SCREEN-VALUE IN FRAME {&FRAME-NAME} = fn-dia-mes(INPUT da-dt-saida + 4) + ENTRY(WEEKDAY(da-dt-saida + 4), c-semana,",")
          fi-titulo-brw:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "PAINEL CONTROLE DIARIO DE CARREGAMENTO - DATA DE SAIDA " +
                                                              STRING(da-dt-saida, "99/99/9999").

   RUN pi-separa-notas. 

   RUN pi-finalizar in h-acomp.

   {&OPEN-QUERY-br-carga}
   {&OPEN-QUERY-br-detalhe}

   APPLY 'value-changed' TO br-carga IN FRAME {&FRAME-NAME}.
   APPLY 'entry' TO br-carga IN FRAME {&FRAME-NAME}.
   RETURN NO-APPLY.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-separa-notas C-Win 
PROCEDURE pi-separa-notas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 FOR EACH nota-fiscal WHERE
          nota-fiscal.cod-estabel    = c-cod-estabel AND
          nota-fiscal.dt-emis-nota  >= (da-dt-saida - 5) AND
          nota-fiscal.dt-emis-nota  <= (da-dt-saida + 5) AND
          nota-fiscal.dt-cancela     = ? NO-LOCK. 

     RUN pi-acompanhar IN h-acomp (INPUT "Data: "    + STRING(nota-fiscal.dt-emis-nota) +
                                         " Nota Fiscal: " + nota-fiscal.nr-nota-fis).


     IF nota-fiscal.nome-transp < c-nome-transp-ini OR
        nota-fiscal.nome-transp > c-nome-transp-fin THEN NEXT.

     IF nota-fiscal.dt-saida < da-dt-saida THEN NEXT.
     IF nota-fiscal.dt-saida > da-dt-saida + 4 THEN NEXT.

     FIND natur-oper WHERE
          natur-oper.nat-operacao = nota-fiscal.nat-operacao NO-LOCK NO-ERROR.
     IF NOT AVAIL natur-oper THEN NEXT.

     IF natur-oper.tipo = 1 THEN NEXT. /* Movto de Entr */
     IF natur-oper.cod-esp <> "DP" THEN NEXT. /* Ped. que Geraram Dupl */

     FIND transporte WHERE
          transporte.nome-abrev = nota-fiscal.nome-transp NO-LOCK NO-ERROR.
     IF NOT AVAIL transporte THEN NEXT.

     IF nota-fiscal.dt-saida = ? THEN NEXT.

     ASSIGN de-mts = 0.
     FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK.
         ASSIGN de-mts = de-mts + it-nota-fisc.qt-faturada[1].
     END.
     IF de-mts = 0 THEN NEXT.

     ASSIGN i-dia = fn-dia-carga().

     FIND tt-carga WHERE
          tt-carga.cod-transp = transporte.cod-transp NO-ERROR.
     IF NOT AVAIL tt-carga THEN DO:
        CREATE tt-carga.
        ASSIGN tt-carga.cod-transp  = transporte.cod-transp
               tt-carga.nome-transp = transporte.nome
               tt-carga.nome-abrev  = transporte.nome-abrev.
     END.
     ASSIGN tt-carga.carga-kgs[i-dia] = tt-carga.carga-kgs[i-dia] + nota-fiscal.peso-bru-tot
            tt-carga.carga-mts[i-dia] = tt-carga.carga-mts[i-dia] + de-mts.

     FIND tt-notas WHERE
          tt-notas.cod-estabel = nota-fiscal.cod-estabel AND
          tt-notas.serie       = nota-fiscal.serie       AND
          tt-notas.nr-nota-fis = nota-fiscal.nr-nota-fis NO-ERROR.
     IF NOT AVAIL tt-notas THEN DO:
        CREATE tt-notas.
        ASSIGN tt-notas.cod-estabel = nota-fiscal.cod-estabel
               tt-notas.serie       = nota-fiscal.serie      
               tt-notas.nr-nota-fis = nota-fiscal.nr-nota-fis
               tt-notas.dia         = i-dia.
     END.
     ASSIGN tt-notas.carga-kgs  = nota-fiscal.peso-bru-tot
            tt-notas.carga-mts  = de-mts.
     CASE i-dia:
         WHEN 1 THEN
             ASSIGN tt-notas.dia-semana = TRIM(fi-cab-02:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
         WHEN 2 THEN
             ASSIGN tt-notas.dia-semana = TRIM(fi-cab-03:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
         WHEN 3 THEN
             ASSIGN tt-notas.dia-semana = TRIM(fi-cab-04:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
         WHEN 4 THEN
             ASSIGN tt-notas.dia-semana = TRIM(fi-cab-05:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
         WHEN 5 THEN
             ASSIGN tt-notas.dia-semana = TRIM(fi-cab-06:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
     END CASE.

 END.

 FOR EACH tt-notas.
     FIND tt-detalhe WHERE
          tt-detalhe.dia-semana = tt-notas.dia-semana NO-ERROR.
     IF NOT AVAIL tt-detalhe THEN DO:
        CREATE tt-detalhe.
        ASSIGN tt-detalhe.dia-semana   = tt-notas.dia-semana.
        CASE tt-notas.dia:
            WHEN 1 THEN
                ASSIGN tt-detalhe.cap-diaria = de-cap-diaria01.
            WHEN 2 THEN
                ASSIGN tt-detalhe.cap-diaria = de-cap-diaria02.
            WHEN 3 THEN
                ASSIGN tt-detalhe.cap-diaria = de-cap-diaria03.
            WHEN 4 THEN
                ASSIGN tt-detalhe.cap-diaria = de-cap-diaria04.
            WHEN 5 THEN
                ASSIGN tt-detalhe.cap-diaria = de-cap-diaria05.
        END CASE.
     END.
     ASSIGN tt-detalhe.tot-dia-kg = tt-detalhe.tot-dia-kg + tt-notas.carga-kgs.
 END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-totais C-Win 
PROCEDURE pi-totais :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

ASSIGN fi-cap-diaria = 0
       fi-tot-dia    = 0
       fi-saldo      = 0
       fi-realizado  = 0.
FOR EACH tt-detalhe NO-LOCK.
    ASSIGN fi-cap-diaria = fi-cap-diaria + tt-detalhe.cap-diaria
           fi-tot-dia    = fi-tot-dia    + tt-detalhe.tot-dia-kg
           fi-saldo      = fi-saldo      + tt-detalhe.saldo-kg  
           fi-realizado  = fi-realizado  + tt-detalhe.realiz-kg.
END.
DISP fi-cap-diaria
     fi-tot-dia   
     fi-saldo     
     fi-realizado 
     WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-dia-carga C-Win 
FUNCTION fn-dia-carga RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR i-dia AS INT.

  IF nota-fiscal.dt-saida = da-dt-saida THEN
     ASSIGN i-dia = 1.

  IF nota-fiscal.dt-saida = da-dt-saida + 1 THEN
     ASSIGN i-dia = 2.

  IF nota-fiscal.dt-saida = da-dt-saida + 2 THEN
     ASSIGN i-dia = 3.

  IF nota-fiscal.dt-saida = da-dt-saida + 3 THEN
     ASSIGN i-dia = 4.

  IF nota-fiscal.dt-saida = da-dt-saida + 4 THEN
     ASSIGN i-dia = 5.

  RETURN i-dia.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-dia-mes C-Win 
FUNCTION fn-dia-mes RETURNS CHARACTER
  (INPUT da-dt-saida AS DATE):
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR c-dia-mes AS CHAR INIT "".

  ASSIGN c-dia-mes = "(" + STRING(DAY(da-dt-saida), "99") +
                     "/" + STRING(MONTH(da-dt-saida), "99") + ")  ".

  RETURN c-dia-mes.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

