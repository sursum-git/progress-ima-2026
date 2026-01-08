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
{include/i-prgvrs.i ESSP0168 2.04.00.999}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
/* Temp Table Definitions ---                                           */

DEF TEMP-TABLE tt-ped-reserva LIKE ped-reserva.

DEF TEMP-TABLE tt-etq-reservadas LIKE ob-etiqueta.

DEF TEMP-TABLE tt-item-disp LIKE ped-reserva-it.

DEF TEMP-TABLE tt-ped-reserva-etq LIKE ped-reserva-etq.

/* Local Variable Definitions ---                                       */
DEF NEW GLOBAL SHARED VAR gr-ob-etiqueta AS ROWID NO-UNDO.
DEF NEW GLOBAL SHARED VAR c-seg-usuario  AS CHAR NO-UNDO.

DEF VAR h-acomp           AS HANDLE NO-UNDO.
DEF VAR h-query           AS HANDLE.
DEF VAR v-row-table       AS ROWID.
DEF VAR c-dia             AS CHAR.
DEF VAR da-dt-entrega-ini AS DATE.
DEF VAR da-dt-entrega-fin AS DATE.
DEF VAR de-tot-res        AS DEC.
DEF VAR de-ger-res        AS DEC.
DEF VAR i-lin             AS INT.
DEF VAR i-pag             AS INT.
DEF VAR c-qualid   AS CHAR FORMAT "x(13)".
DEF VAR c-emitente AS CHAR FORMAT "x(12)".
DEF VAR c-repres   AS CHAR FORMAT "x(12)".
DEF VAR c-tear     AS CHAR.
DEF VAR c-corte    AS CHAR.
DEF VAR c-sit      AS CHAR FORMAT "x(9)".
DEF VAR c-empresa  AS CHAR.
DEF VAR i-num-reserva LIKE ped-reserva.num-reserva.
DEF VAR i-sit-ini  AS INT.
DEF VAR i-sit-fin  AS INT.


/* Variavies de ParÉmetros */
DEFINE VAR c-dt-limite-ini     AS CHAR.
DEFINE VAR c-dt-limite-fin     AS CHAR.
DEFINE VAR c-cod-estabel-ini   AS CHAR.
DEFINE VAR c-cod-estabel-fin   AS CHAR.
DEFINE VAR i-num-reserva-ini   LIKE ped-reserva.num-reserva.
DEFINE VAR i-num-reserva-fin   LIKE ped-reserva.num-reserva INIT 999999999.  
DEFINE VAR i-cod-emit-ini      LIKE ped-reserva.cod-emitente. 
DEFINE VAR i-cod-emit-fin      LIKE ped-reserva.cod-emitente INIT 999999999.
DEFINE VAR i-cod-rep-ini       LIKE ped-reserva.cod-rep. 
DEFINE VAR i-cod-rep-fin       LIKE ped-reserva.cod-rep INIT 999999.
DEFINE VAR c-it-codigo-ini     LIKE ped-reserva-it.it-codigo INIT "".                 
DEFINE VAR c-it-codigo-fin     LIKE ped-reserva-it.it-codigo INIT "ZZZZZZZZZZZZZZZ".  
DEFINE VAR c-cod-refer-ini     LIKE ped-reserva-it.cod-refer.
DEFINE VAR c-cod-refer-fin     LIKE ped-reserva-it.cod-refer INIT "ZZZZZZZZZZ".   
DEFINE VAR i-reserva           AS INT INIT 1.
DEFINE VAR l-ok                AS LOG.

/* Variaveis da Rotina de Impress∆o */
DEFINE VAR i-saida             AS INT.
DEFINE VAR c-saida             AS CHAR.
DEFINE VAR i-num-copias        AS INT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME br-etq-reservadas

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-etq-reservadas tt-item-disp ~
tt-ped-reserva

/* Definitions for BROWSE br-etq-reservadas                             */
&Scoped-define FIELDS-IN-QUERY-br-etq-reservadas tt-etq-reservadas.localizacao tt-etq-reservadas.num-etiqueta tt-etq-reservadas.nuance fn-qualid('tt-etq-reservadas') @ c-qualid fn-tear('tt-etq-reservadas') @ c-tear tt-etq-reservadas.quantidade   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-etq-reservadas   
&Scoped-define SELF-NAME br-etq-reservadas
&Scoped-define OPEN-QUERY-br-etq-reservadas RUN pi-soma-res. OPEN QUERY {&SELF-NAME} FOR EACH tt-etq-reservadas  NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-etq-reservadas tt-etq-reservadas
&Scoped-define FIRST-TABLE-IN-QUERY-br-etq-reservadas tt-etq-reservadas


/* Definitions for BROWSE br-item-disp                                  */
&Scoped-define FIELDS-IN-QUERY-br-item-disp tt-item-disp.nr-sequencia tt-item-disp.it-codigo tt-item-disp.cod-refer fn-corte() @ c-corte tt-item-disp.nr-lote   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-item-disp   
&Scoped-define SELF-NAME br-item-disp
&Scoped-define OPEN-QUERY-br-item-disp RUN pi-soma-item. OPEN QUERY {&SELF-NAME} FOR EACH tt-item-disp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-item-disp tt-item-disp
&Scoped-define FIRST-TABLE-IN-QUERY-br-item-disp tt-item-disp


/* Definitions for BROWSE br-reservas                                   */
&Scoped-define FIELDS-IN-QUERY-br-reservas tt-ped-reserva.num-reserva fn-emitente() @ c-emitente fn-repres() @ c-repres tt-ped-reserva.dt-reserva tt-ped-reserva.dt-validade UPPER(tt-ped-reserva.usuario) fn-situacao() @ c-sit   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-reservas   
&Scoped-define SELF-NAME br-reservas
&Scoped-define QUERY-STRING-br-reservas FOR EACH tt-ped-reserva NO-LOCK
&Scoped-define OPEN-QUERY-br-reservas OPEN QUERY {&SELF-NAME} FOR EACH tt-ped-reserva NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-reservas tt-ped-reserva
&Scoped-define FIRST-TABLE-IN-QUERY-br-reservas tt-ped-reserva


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-br-etq-reservadas}~
    ~{&OPEN-QUERY-br-item-disp}~
    ~{&OPEN-QUERY-br-reservas}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-buttom br-reservas bt-param bt-imprime ~
bt-vapara br-item-disp br-etq-reservadas bt-exit bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS fi-tot-res-disp fi-tot-reservado 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-4 bt-det 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-corte C-Win 
FUNCTION fn-corte RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-emitente C-Win 
FUNCTION fn-emitente RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-qualid C-Win 
FUNCTION fn-qualid RETURNS CHARACTER
  (INPUT c-tabela AS CHAR) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-repres C-Win 
FUNCTION fn-repres RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-situacao C-Win 
FUNCTION fn-situacao RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-tear C-Win 
FUNCTION fn-tear RETURNS CHARACTER
  (INPUT c-tabela AS CHAR) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ajuda 
     IMAGE-UP FILE "image/im-hel.bmp":U
     LABEL "&Ajuda" 
     SIZE 5 BY 1.29 TOOLTIP "Help on Line"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-det 
     IMAGE-UP FILE "image/im-det.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-det.bmp":U
     LABEL "" 
     SIZE 5 BY 1.29 TOOLTIP "Detalha Etiquetas Reservadas".

DEFINE BUTTON bt-exit AUTO-END-KEY 
     IMAGE-UP FILE "image/im-exi.bmp":U
     LABEL "" 
     SIZE 5 BY 1.29 TOOLTIP "Fechar Programa"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-imprime 
     IMAGE-UP FILE "image/im-printer.bmp":U
     LABEL "" 
     SIZE 5 BY 1.29 TOOLTIP "Imprimir a Reserva Selecionada".

DEFINE BUTTON bt-param AUTO-GO 
     IMAGE-UP FILE "image/im-fil.bmp":U
     LABEL "&Sair" 
     SIZE 5 BY 1.29 TOOLTIP "ParÉmetros"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-vapara 
     IMAGE-UP FILE "image/im-vapra.bmp":U
     LABEL "Button 2" 
     SIZE 5 BY 1.29 TOOLTIP "Va para uma Reserva".

DEFINE VARIABLE fi-tot-res-disp AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Total Reservado" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE fi-tot-reservado AS DECIMAL FORMAT ">>>,>>9.99":U INITIAL 0 
     LABEL "Reservado" 
     VIEW-AS FILL-IN 
     SIZE 10.57 BY .88
     FGCOLOR 2 FONT 6 NO-UNDO.

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 7 BY 18.75
     BGCOLOR 8 FGCOLOR 0 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-etq-reservadas FOR 
      tt-etq-reservadas SCROLLING.

DEFINE QUERY br-item-disp FOR 
      tt-item-disp SCROLLING.

DEFINE QUERY br-reservas FOR 
      tt-ped-reserva SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-etq-reservadas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-etq-reservadas C-Win _FREEFORM
  QUERY br-etq-reservadas NO-LOCK DISPLAY
      tt-etq-reservadas.localizacao  FORMAT "999/999":U    COLUMN-LABEL "Local"  WIDTH 6

tt-etq-reservadas.num-etiqueta FORMAT "999999999":U  COLUMN-LABEL "Etiqueta"     WIDTH 8

tt-etq-reservadas.nuance       FORMAT " X(2)":U      COLUMN-LABEL "Nuance"       WIDTH 6

fn-qualid('tt-etq-reservadas') @ c-qualid         WIDTH 11              COLUMN-LABEL "Qualidade"

fn-tear('tt-etq-reservadas')   @ c-tear           WIDTH 7               COLUMN-LABEL "Tear"

tt-etq-reservadas.quantidade   FORMAT ">>9.99":U     COLUMN-LABEL "Qtde (m)"     WIDTH 10
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 54 BY 9
         FONT 1
         TITLE "Peáas Reservadas" ROW-HEIGHT-CHARS .63.

DEFINE BROWSE br-item-disp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-item-disp C-Win _FREEFORM
  QUERY br-item-disp NO-LOCK DISPLAY
      tt-item-disp.nr-sequencia  FORMAT ">>>9":U     COLUMN-LABEL "Seq. "     WIDTH 5      
tt-item-disp.it-codigo FORMAT "x(8)":U WIDTH 8
tt-item-disp.cod-refer FORMAT "x(8)":U  COLUMN-LABEL "Referância"   WIDTH 8
fn-corte() @ c-corte     COLUMN-LABEL "Corte  "                       WIDTH 11 
tt-item-disp.nr-lote FORMAT "X(4)":U                                WIDTH 4
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 42 BY 9
         FONT 1
         TITLE "Item Disponivel" ROW-HEIGHT-CHARS .54.

DEFINE BROWSE br-reservas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-reservas C-Win _FREEFORM
  QUERY br-reservas NO-LOCK DISPLAY
      tt-ped-reserva.num-reserva  COLUMN-LABEL "Nß Reserva" WIDTH 10
fn-emitente() @ c-emitente  WIDTH 14  COLUMN-LABEL "Cliente" 
fn-repres()   @ c-repres    WIDTH 14  COLUMN-LABEL "Representante" 
tt-ped-reserva.dt-reserva  COLUMN-LABEL "Data Reserva"  WIDTH 12
tt-ped-reserva.dt-validade COLUMN-LABEL "Data Validade"    WIDTH 12
UPPER(tt-ped-reserva.usuario)     COLUMN-LABEL "Implantador"  WIDTH 14
fn-situacao()   @ c-sit    WIDTH 14  COLUMN-LABEL "Situacao"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 96.86 BY 9.5
         FONT 6
         TITLE "Reservas" ROW-HEIGHT-CHARS .67.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     br-reservas AT ROW 1.25 COL 2.14
     bt-param AT ROW 1.54 COL 100.72
     bt-imprime AT ROW 2.96 COL 100.72
     bt-vapara AT ROW 4.42 COL 100.72
     br-item-disp AT ROW 11 COL 2
     br-etq-reservadas AT ROW 11.04 COL 45
     bt-det AT ROW 12.5 COL 100.72
     bt-exit AT ROW 16.96 COL 100.72
     bt-ajuda AT ROW 18.29 COL 100.72
     fi-tot-res-disp AT ROW 20.13 COL 16.43 COLON-ALIGNED
     fi-tot-reservado AT ROW 20.13 COL 84 COLON-ALIGNED
     rt-buttom AT ROW 1.25 COL 99.72
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 112.72 BY 20.17.


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
         TITLE              = "Consulta Reservas - ESSP0168"
         COLUMN             = 15.43
         ROW                = 6.79
         HEIGHT             = 20.08
         WIDTH              = 106.14
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
/* BROWSE-TAB br-reservas rt-buttom DEFAULT-FRAME */
/* BROWSE-TAB br-item-disp bt-vapara DEFAULT-FRAME */
/* BROWSE-TAB br-etq-reservadas br-item-disp DEFAULT-FRAME */
/* SETTINGS FOR BUTTON bt-det IN FRAME DEFAULT-FRAME
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-tot-res-disp IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-reservado IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-etq-reservadas
/* Query rebuild information for BROWSE br-etq-reservadas
     _START_FREEFORM
RUN pi-soma-res.
OPEN QUERY {&SELF-NAME} FOR EACH tt-etq-reservadas  NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-etq-reservadas */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-item-disp
/* Query rebuild information for BROWSE br-item-disp
     _START_FREEFORM
RUN pi-soma-item.
OPEN QUERY {&SELF-NAME} FOR EACH tt-item-disp NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-item-disp */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-reservas
/* Query rebuild information for BROWSE br-reservas
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-ped-reserva NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "mgmov.ped-venda.cod-sit-ped = 1"
     _Query            is OPENED
*/  /* BROWSE br-reservas */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Consulta Reservas - ESSP0168 */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Consulta Reservas - ESSP0168 */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-etq-reservadas
&Scoped-define SELF-NAME br-etq-reservadas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-etq-reservadas C-Win
ON END OF br-etq-reservadas IN FRAME DEFAULT-FRAME /* Peáas Reservadas */
DO:
  MESSAGE "end"
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-etq-reservadas C-Win
ON ENTRY OF br-etq-reservadas IN FRAME DEFAULT-FRAME /* Peáas Reservadas */
DO:
   APPLY 'value-changed' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-etq-reservadas C-Win
ON VALUE-CHANGED OF br-etq-reservadas IN FRAME DEFAULT-FRAME /* Peáas Reservadas */
DO:
    
   ASSIGN bt-det:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
   IF AVAIL tt-etq-reservadas THEN DO.
      ASSIGN bt-det:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
      FIND ob-etiqueta WHERE
           ob-etiqueta.cod-estabel = tt-ped-reserva.cod-estabel AND
           ob-etiqueta.num-etiqueta = tt-etq-reservadas.num-etiqueta 
           NO-LOCK NO-ERROR.
      IF AVAIL ob-etiqueta THEN
         ASSIGN gr-ob-etiqueta = ROWID(ob-etiqueta).
   END.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-item-disp
&Scoped-define SELF-NAME br-item-disp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-item-disp C-Win
ON VALUE-CHANGED OF br-item-disp IN FRAME DEFAULT-FRAME /* Item Disponivel */
DO:
    
   ASSIGN bt-det:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
   EMPTY TEMP-TABLE tt-etq-reservadas.
   FOR EACH ped-reserva-etq OF tt-item-disp NO-LOCK.
       FIND ob-etiqueta WHERE
            ob-etiqueta.cod-estabel = tt-ped-reserva.cod-estabel AND
            ob-etiqueta.num-etiqueta = ped-reserva-etq.num-etiqueta NO-LOCK NO-ERROR.
       IF AVAIL ob-etiqueta THEN DO:
          CREATE tt-etq-reservadas.
          BUFFER-COPY ob-etiqueta TO tt-etq-reservadas.
       END.
   END.
   
   {&OPEN-QUERY-br-etq-reservadas}
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-reservas
&Scoped-define SELF-NAME br-reservas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-reservas C-Win
ON VALUE-CHANGED OF br-reservas IN FRAME DEFAULT-FRAME /* Reservas */
DO:
  
  ASSIGN bt-det:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
  EMPTY TEMP-TABLE tt-item-disp.
  EMPTY TEMP-TABLE tt-ped-reserva-etq.

  FOR EACH ped-reserva-it OF tt-ped-reserva  WHERE
           ped-reserva-it.it-codigo >= c-it-codigo-ini AND
           ped-reserva-it.it-codigo <= c-it-codigo-fin AND
           ped-reserva-it.cod-refer >= c-cod-refer-ini AND
           ped-reserva-it.cod-refer <= c-cod-refer-fin NO-LOCK.
      CREATE tt-item-disp.
      BUFFER-COPY ped-reserva-it TO tt-item-disp.
  END.

  FOR EACH ped-reserva-it OF tt-ped-reserva  WHERE 
           ped-reserva-it.it-codigo >= c-it-codigo-ini AND
           ped-reserva-it.it-codigo <= c-it-codigo-fin AND
           ped-reserva-it.cod-refer >= c-cod-refer-ini AND
           ped-reserva-it.cod-refer <= c-cod-refer-fin NO-LOCK.
      FOR EACH ped-reserva-etq OF ped-reserva-it NO-LOCK.
          CREATE tt-ped-reserva-etq.
          BUFFER-COPY ped-reserva-etq TO tt-ped-reserva-etq.
      END.
  END.

  {&OPEN-QUERY-br-item-disp}
  APPLY 'value-changed' TO br-item-disp.
  APPLY 'entry' TO br-item-disp.
  
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


&Scoped-define SELF-NAME bt-det
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-det C-Win
ON CHOOSE OF bt-det IN FRAME DEFAULT-FRAME
DO:
   RUN esp/essp0146.p.  
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
  RUN pi-imprime.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-param
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-param C-Win
ON CHOOSE OF bt-param IN FRAME DEFAULT-FRAME /* Sair */
DO:
   ASSIGN c-win:SENSITIVE = NO.
   RUN esp/essp0168a.w (INPUT-OUTPUT c-dt-limite-ini,   
                        INPUT-OUTPUT c-dt-limite-fin, 
                        INPUT-OUTPUT c-cod-estabel-ini,
                        INPUT-OUTPUT c-cod-estabel-fin,
                        INPUT-OUTPUT i-num-reserva-ini,   
                        INPUT-OUTPUT i-num-reserva-fin,   
                        INPUT-OUTPUT i-cod-emit-ini,
                        INPUT-OUTPUT i-cod-emit-fin,
                        INPUT-OUTPUT i-cod-rep-ini,
                        INPUT-OUTPUT i-cod-rep-fin,
                        INPUT-OUTPUT c-it-codigo-ini,   
                        INPUT-OUTPUT c-it-codigo-fin,   
                        INPUT-OUTPUT c-cod-refer-ini,   
                        INPUT-OUTPUT c-cod-refer-fin,   
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
ON CHOOSE OF bt-vapara IN FRAME DEFAULT-FRAME /* Button 2 */
DO:
  RUN esdlg/d01essp0168.w (OUTPUT i-num-reserva).

   IF i-num-reserva <> 0 THEN DO:
      FIND FIRST tt-ped-reserva WHERE
                 tt-ped-reserva.num-reserva = i-num-reserva NO-LOCK NO-ERROR.

      IF NOT AVAIL tt-ped-reserva THEN DO.
         MESSAGE "Reserva n∆o est† contida na seleá∆o!"
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
         RETURN NO-APPLY.
      END.

      h-query:REPOSITION-TO-ROWID(ROWID(tt-ped-reserva)) NO-ERROR. 
      APPLY 'VALUE-CHANGED' TO br-reservas.
   END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-etq-reservadas
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

ON 'F5':U OF br-reservas DO:
   {&OPEN-QUERY-br-reservas}
   APPLY 'value-changed' TO br-reservas.
END.

ASSIGN h-query = br-reservas:QUERY.
br-reservas:NUM-LOCKED-COLUMNS = 3.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

   RUN enable_UI.

   RUN esapi/busca-estab-usuar.p (INPUT c-seg-usuario,
                                  OUTPUT c-cod-estabel-ini).
   IF c-cod-estabel-ini = '' THEN 
      ASSIGN c-cod-estabel-ini = '1'
             c-cod-estabel-fin = '2'.
   ELSE
      ASSIGN c-cod-estabel-fin = c-cod-estabel-ini.

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
  DISPLAY fi-tot-res-disp fi-tot-reservado 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE rt-buttom br-reservas bt-param bt-imprime bt-vapara br-item-disp 
         br-etq-reservadas bt-exit bt-ajuda 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cabec C-Win 
PROCEDURE pi-cabec :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 PUT c-empresa  FORMAT "X(40)"                 AT  1
     "DATA: "                                  AT 60
     STRING(TODAY,"99/99/9999") FORMAT "X(10)" AT 66
     "HORA: "                                  AT 86
     STRING(TIME,"hh:mm:ss")                   AT 92
     SKIP(1).
    
 PUT "RELATORIO DE DETALHE DA RESERVA" AT 40 SKIP(1).

 
 PUT "Reserva  Situacao  Dt.Reserva Cliente      Representante Item   Referencia Corte    Lote Quantidade" AT 1.
 PUT "-------- --------- ---------- ------------ ------------- ------ ---------- -------- ---- ----------" AT 1.

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
         OUTPUT TO VALUE(c-saida) CONVERT TARGET "ibm850" PAGED PAGE-SIZE 61.
         PUT CONTROL "~033E~033(s16H".    
     END.
     WHEN 2 THEN
         OUTPUT TO VALUE(c-saida).
     WHEN 3 THEN DO.
         ASSIGN c-saida = SESSION:TEMP-DIRECTORY + "essp0168.tmp".
         OUTPUT TO VALUE(c-saida).
     END.
 END CASE.
 DO i-ct = 1 TO i-num-copias.
    ASSIGN de-tot-res =  0
           de-ger-res =  0
           i-lin      = 99.

    FIND emitente WHERE
         emitente.cod-emitente = tt-ped-reserva.cod-emitente NO-LOCK NO-ERROR.
    FIND repres WHERE
         repres.cod-rep = tt-ped-reserva.cod-rep NO-LOCK NO-ERROR.

    FOR EACH tt-item-disp where
             tt-item-disp.num-reserva = tt-ped-reserva.num-reserva NO-LOCK
        BREAK BY tt-item-disp.num-reserva.
        ASSIGN de-tot-res = 0.
        
        FOR EACH  tt-ped-reserva-etq OF tt-item-disp NO-LOCK. 
            FIND ob-etiqueta WHERE 
                 ob-etiqueta.cod-estabel = tt-ped-reserva.cod-estabel AND
                 ob-etiqueta.num-etiqueta = tt-ped-reserva-etq.num-etiqueta NO-LOCK NO-ERROR.
            IF AVAIL ob-etiqueta THEN
               ASSIGN de-tot-res = de-tot-res + ob-etiqueta.quantidade.
        END.
        
        IF i-lin > 74 THEN DO:
           RUN pi-cabec.
           ASSIGN i-lin = 7.
        END.

        {esinc/i-dsrb.i tt-ped-reserva.situacao tt-ped-reserva.situacao c-sit}  

        FIND corte-comerc WHERE
             corte-comerc.codigo = tt-item-disp.corte-comerc NO-LOCK NO-ERROR.

        IF FIRST-OF(tt-item-disp.num-reserva) THEN
           PUT tt-ped-reserva.num-reserva FORMAT ">>>>>>>9" AT  1 
               c-sit                                        AT 10
               tt-ped-reserva.dt-reserva                    AT 20
               emitente.nome-abrev FORMAT "x(12)"           AT 31
               repres.nome-abrev   FORMAT "x(12)"           AT 44.

        PUT tt-item-disp.it-codigo FORMAT "x(6)"    AT 58
            tt-item-disp.cod-refer                  AT 65
            corte-comerc.descricao FORMAT "x(8)"    AT 76
            tt-item-disp.nr-lote FORMAT "x(2)"      AT 85
            de-tot-res FORMAT ">>>,>>9.99"          AT 90.

        ASSIGN de-ger-res = de-ger-res + de-tot-res.

    END.
    IF de-ger-res <> 0 THEN DO:
        PUT "----------"   AT 90 SKIP.
        PUT "TOTAL . . ."  AT 76.
        PUT de-ger-res FORMAT ">>>,>>9.99" AT 90.
    END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-popula-browse C-Win 
PROCEDURE pi-popula-browse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
   {utp/ut-liter.i Selecionando_Reservas *}
   RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

   EMPTY TEMP-TABLE tt-ped-reserva.
   EMPTY TEMP-TABLE tt-item-disp.
   EMPTY TEMP-TABLE tt-etq-reservadas.

   /* Busca Nome da Empresa */
   FIND FIRST param-global NO-LOCK NO-ERROR.
   FIND FIRST empresa WHERE
              empresa.ep-codigo = param-global.empresa-prin NO-LOCK NO-ERROR. 
   ASSIGN c-empresa = (IF AVAIL empresa THEN empresa.razao-social ELSE "").

   RUN esapi/ret-udm.p (INPUT c-dt-limite-fin, OUTPUT c-dia).
   ASSIGN da-dt-entrega-ini = DATE('01' + SUBSTR(c-dt-limite-ini,1,2) + SUBSTR(c-dt-limite-ini,3,4))
          da-dt-entrega-fin = DATE(c-dia + SUBSTR(c-dt-limite-fin,1,2) + SUBSTR(c-dt-limite-fin,3,4)).

   RUN pi-separa-reservas.

   RUN pi-finalizar in h-acomp.
   
   {&OPEN-QUERY-br-reservas}
   APPLY 'value-changed' TO br-reservas IN FRAME {&FRAME-NAME}.
   APPLY 'entry' TO br-reservas IN FRAME {&FRAME-NAME}.
   RETURN NO-APPLY.
     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-separa-reservas C-Win 
PROCEDURE pi-separa-reservas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN i-sit-ini = IF i-reserva = 4 
                       THEN 1
                       ELSE i-reserva.

    ASSIGN i-sit-fin = IF i-reserva = 4 
                       THEN 99
                       ELSE i-reserva .

    FOR EACH ped-reserva WHERE
             ped-reserva.situacao     >= i-sit-ini AND
             ped-reserva.situacao     <= i-sit-fin AND
             ped-reserva.num-reserva  >= i-num-reserva-ini AND
             ped-reserva.num-reserva  <= i-num-reserva-fin AND
             ped-reserva.cod-estabel  >= c-cod-estabel-ini AND
             ped-reserva.cod-estabel  <= c-cod-estabel-fin AND
             ped-reserva.dt-reserva   >= da-dt-entrega-ini AND
             ped-reserva.dt-reserva   <= da-dt-entrega-fin AND 
             ped-reserva.cod-emitente >= i-cod-emit-ini    AND
             ped-reserva.cod-emitente <= i-cod-emit-fin    AND
             ped-reserva.cod-rep      >= i-cod-rep-ini     AND
             ped-reserva.cod-rep      <= i-cod-rep-fin NO-LOCK,
        EACH ped-reserva-it WHERE
             ped-reserva-it.num-reserva  = ped-reserva.num-reserva AND
             ped-reserva-it.it-codigo   >= c-it-codigo-ini         AND
             ped-reserva-it.it-codigo   <= c-it-codigo-fin         AND
             ped-reserva-it.cod-refer   >= c-cod-refer-ini         AND
             ped-reserva-it.cod-refer   <= c-cod-refer-fin NO-LOCK.

        RUN pi-acompanhar IN h-acomp (INPUT "Reserva: " + STRING(ped-reserva.num-reserva) + "   " + 
                                            "Data: " + STRING(ped-reserva.dt-reserva)).

        FIND tt-ped-reserva WHERE
             tt-ped-reserva.num-reserva  = ped-reserva.num-reserva NO-ERROR.
        IF NOT AVAIL tt-ped-reserva THEN DO:
           CREATE tt-ped-reserva.
           BUFFER-COPY ped-reserva TO tt-ped-reserva.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-soma-item C-Win 
PROCEDURE pi-soma-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 ASSIGN fi-tot-res-disp = 0.
 FOR EACH tt-item-disp NO-LOCK.
     FOR EACH  ped-reserva-etq OF tt-item-disp NO-LOCK.
         FIND ob-etiqueta WHERE
              ob-etiqueta.cod-estabel = ped-reserva.cod-estabel AND
              ob-etiqueta.num-etiqueta = ped-reserva-etq.num-etiqueta NO-LOCK NO-ERROR.
         IF AVAIL ob-etiqueta THEN
            ASSIGN fi-tot-res-disp = fi-tot-res-disp + ob-etiqueta.quantidade.
     END.
 END.
 DISP fi-tot-res-disp WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-soma-res C-Win 
PROCEDURE pi-soma-res :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN fi-tot-reservado = 0.
    FOR EACH tt-etq-reservadas NO-LOCK.
        ASSIGN fi-tot-reservado = fi-tot-reservado + tt-etq-reservadas.quantidade.
    END.
    DISP fi-tot-reservado WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-corte C-Win 
FUNCTION fn-corte RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  FIND corte-comerc WHERE
       corte-comerc.codigo = tt-item-disp.corte-comerc 
       NO-LOCK NO-ERROR.

  IF AVAIL corte-comerc THEN 
     RETURN corte-comerc.descricao.  /* Function return value. */
  ELSE
     RETURN "N∆o Informado".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-emitente C-Win 
FUNCTION fn-emitente RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND emitente WHERE 
       emitente.cod-emit = tt-ped-reserva.cod-emitente NO-LOCK NO-ERROR.
  IF AVAIL emitente THEN 
     RETURN emitente.nome-abrev.   /* Function return value. */
  ELSE
     RETURN "N∆o Informada".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-qualid C-Win 
FUNCTION fn-qualid RETURNS CHARACTER
  (INPUT c-tabela AS CHAR):
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND qualid-tecido WHERE
       qualid-tecido.codigo =/* IF c-tabela = 'wt-etiquetas' 
                              THEN wt-etiquetas.cod-qualid
                              ELSE */ tt-etq-reservadas.cod-qualid
       NO-LOCK NO-ERROR.

  IF AVAIL qualid-tecido THEN 
     RETURN qualid-tecido.descricao.   /* Function return value. */
  ELSE
     RETURN "N∆o Informada".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-repres C-Win 
FUNCTION fn-repres RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  FIND repres WHERE 
       repres.cod-rep = tt-ped-reserva.cod-rep NO-LOCK NO-ERROR.
  IF AVAIL repres THEN 
     RETURN repres.nome-abrev.   /* Function return value. */
  ELSE
     RETURN "N∆o Informado".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-situacao C-Win 
FUNCTION fn-situacao RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  {esinc/i-dsrb.i tt-ped-reserva.situacao tt-ped-reserva.situacao c-sit}.

   RETURN UPPER(c-sit).
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-tear C-Win 
FUNCTION fn-tear RETURNS CHARACTER
  (INPUT c-tabela AS CHAR):
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
/*   IF c-tabela = 'wt-etiquetas' THEN
       FIND ordem-benefic OF wt-etiquetas
            NO-LOCK NO-ERROR.
   ELSE  */
/*
       FIND ordem-benefic OF tt-etq-reservadas
             NO-LOCK NO-ERROR.

  IF AVAIL ordem-benefic THEN 
     RETURN ordem-benefic.tipo-tear.   /* Function return value. */
  ELSE */
     RETURN "N∆o Informada".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

