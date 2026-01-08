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
{include/i-prgvrs.i ESSP0169 2.04.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
/* Global Variable Definitions ---                                       */
DEF NEW GLOBAL SHARED VAR gr-ob-etiqueta  AS ROWID NO-UNDO.
DEF NEW GLOBAL SHARED VAR gr-corte-comerc AS ROWID NO-UNDO.
DEF NEW GLOBAL SHARED VAR gr-item         AS ROWID NO-UNDO.
DEF NEW GLOBAL SHARED VAR c-seg-usuario   AS CHAR NO-UNDO.
DEFINE BUFFER empresa FOR mgcad.empresa.

/* Temp Table Definitions ---                                           */
DEF TEMP-TABLE tt-ob-etiqueta LIKE ob-etiqueta
    FIELD gramatura           LIKE item.peso-liquido
    FIELD peso-embalagem      AS DEC  FORMAT ">>9.99999"
    FIELD peso-liq-sist       AS DEC  FORMAT ">>9.999"
    FIELD peso-bruto-sist     AS DEC  FORMAT ">>9.999" 
    FIELD dif-kg              AS DEC  FORMAT "->>9.999"
    FIELD dif-metro           AS DEC  FORMAT "->>9.999"
    FIELD gramatura-ideal     AS DEC  FORMAT "->>9.99999"
    FIELD sit                 AS CHAR FORMAT "X(12)".

/* Local Variable Definitions ---                                       */
DEF VAR h-acomp           AS HANDLE NO-UNDO.
DEF VAR h-query           AS HANDLE.
DEF VAR c-empresa         AS CHAR.

DEF VAR c-situacao        AS CHAR FORMAT "x(12)".
DEF VAR c-lotes           AS CHAR.
DEF VAR da-dt-emissao     LIKE ob-etiqueta.dt-emissao.
DEF VAR i-num-etiqueta    LIKE ob-etiqueta.num-etiqueta.
DEF VAR i-etiqueta-ini    LIKE ob-etiqueta.num-etiqueta INIT 0.
DEF VAR i-etiqueta-fin    LIKE ob-etiqueta.num-etiqueta INIT 999999999.
DEF VAR l-sintetico       AS LOG INITIAL YES.
DEF VAR i-lin             AS INT.
DEF VAR i-pag             AS INT.

/* Variaveis da Rotina de Impress∆o */
DEFINE VAR i-saida             AS INT.
DEFINE VAR c-saida             AS CHAR.
DEFINE VAR i-num-copias        AS INT.

/* Variavies de ParÉmetros */
DEFINE VAR c-cod-estabel       LIKE ob-etiqueta.cod-estabel.
DEFINE VAR da-dt-emissao-ini   LIKE ob-etiqueta.dt-emissao.
DEFINE VAR da-dt-emissao-fin   LIKE ob-etiqueta.dt-emissao.
DEFINE VAR c-it-codigo-ini     LIKE ob-etiqueta.it-codigo INIT "".
DEFINE VAR c-it-codigo-fin     LIKE ob-etiqueta.it-codigo INIT "ZZZZZZ".
DEFINE VAR c-cod-refer-ini     LIKE ob-etiqueta.cod-refer INIT "". 
DEFINE VAR c-cod-refer-fin     LIKE ob-etiqueta.cod-refer INIT "ZZZZZZZ".
DEFINE VAR c-corte-comerc-ini  LIKE ob-etiqueta.corte-comerc INIT "".
DEFINE VAR c-corte-comerc-fin  LIKE ob-etiqueta.corte-comerc INIT "Z".
DEFINE VAR i-tp-tecido         AS INT INITIAL 3.
DEFINE VAR i-tipo-ordem        AS INT INITIAL 9.
DEFINE VAR l-erro-peso         AS LOG INITIAL NO.
DEFINE VAR l-lote-todos        AS LOG INIT YES.
DEFINE VAR l-lote-pp           AS LOG INIT NO.
DEFINE VAR l-lote-pd           AS LOG INIT NO.
DEFINE VAR l-lote-rp           AS LOG INIT NO.
DEFINE VAR l-lote-rd           AS LOG INIT NO.
DEFINE VAR l-lote-sc           AS LOG INIT NO.
DEFINE VAR l-lote-ca           AS LOG INIT NO.
DEFINE VAR l-dep-corte         AS LOG INIT NO.
DEFINE VAR i-opc-acabado       AS INT INIT 3.
DEFINE VAR l-ok                AS LOG.


/* Variaveis Usadas Na Geraá∆o da Planilha Excel */
DEFINE VAR i-canal     AS INTEGER.
DEFINE VAR sys         AS INTEGER.
DEFINE VAR c-lin       AS CHARACTER FORMAT "x(500)".
DEFINE VAR aux-command AS CHAR FORMAT "x(100)".
DEFINE VAR arq-saida   AS CHAR FORMAT "x(45)".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME br-ob-etiquetas

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-ob-etiqueta

/* Definitions for BROWSE br-ob-etiquetas                               */
&Scoped-define FIELDS-IN-QUERY-br-ob-etiquetas tt-ob-etiqueta.dt-emissao tt-ob-etiqueta.num-etiqueta tt-ob-etiqueta.it-codigo tt-ob-etiqueta.cod-refer tt-ob-etiqueta.nr-lote tt-ob-etiqueta.gramatura tt-ob-etiqueta.peso-embalagem tt-ob-etiqueta.quantidade tt-ob-etiqueta.peso-bruto-sist tt-ob-etiqueta.peso-bruto tt-ob-etiqueta.dif-kg tt-ob-etiqueta.dif-metro tt-ob-etiqueta.gramatura-ideal tt-ob-etiqueta.peso-liq-sist tt-ob-etiqueta.sit   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-ob-etiquetas   
&Scoped-define SELF-NAME br-ob-etiquetas
&Scoped-define OPEN-QUERY-br-ob-etiquetas RUN pi-totais. OPEN QUERY {&SELF-NAME} FOR EACH tt-ob-etiqueta USE-INDEX indice8 NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-ob-etiquetas tt-ob-etiqueta
&Scoped-define FIRST-TABLE-IN-QUERY-br-ob-etiquetas tt-ob-etiqueta


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-br-ob-etiquetas}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-buttom br-ob-etiquetas bt-param bt-vapara ~
bt-consulta bt-modifica bt-modifica-2 bt-imprime bt-excel bt-exit bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS fi-tot-metros fi-tot-media 

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
     SIZE 5 BY 1.25 TOOLTIP "Detalhar Etiqueta"
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

DEFINE BUTTON bt-imprime 
     IMAGE-UP FILE "image/im-printer.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Imprimir a Carteira de Pedidos Selecionados".

DEFINE BUTTON bt-modifica AUTO-END-KEY 
     IMAGE-UP FILE "image/im-mod.bmp":U
     LABEL "&Sair" 
     SIZE 4.86 BY 1.25 TOOLTIP "Modificar Corte Comercial"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-modifica-2 AUTO-END-KEY 
     IMAGE-UP FILE "image/im-modbm.bmp":U
     LABEL "&Sair" 
     SIZE 4.86 BY 1.25 TOOLTIP "Modificar Gramatura Item"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-param AUTO-GO 
     IMAGE-UP FILE "image/im-fil.bmp":U
     LABEL "&Sair" 
     SIZE 5 BY 1.25 TOOLTIP "ParÉmetros"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-vapara AUTO-GO 
     IMAGE-UP FILE "image/im-vapra.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Posicionar na Etiqueta"
     BGCOLOR 8 FONT 10.

DEFINE VARIABLE fi-tot-media AS DECIMAL FORMAT "->,>>>,>>9.99999":U INITIAL 0 
     LABEL "MÇdia Gramatura Ideal" 
     VIEW-AS FILL-IN 
     SIZE 10.57 BY .88
     FGCOLOR 12 FONT 1 NO-UNDO.

DEFINE VARIABLE fi-tot-metros AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Total Metragem" 
     VIEW-AS FILL-IN 
     SIZE 10.57 BY .88
     FONT 1 NO-UNDO.

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 7 BY 20
     BGCOLOR 8 FGCOLOR 0 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-ob-etiquetas FOR 
      tt-ob-etiqueta SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-ob-etiquetas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-ob-etiquetas C-Win _FREEFORM
  QUERY br-ob-etiquetas NO-LOCK DISPLAY
      tt-ob-etiqueta.dt-emissao      COLUMN-LABEL "Dt.Emiss∆o"    
tt-ob-etiqueta.num-etiqueta    COLUMN-LABEL "Etiqueta"    FORMAT ">>>,>>>,>>9" WIDTH 10
tt-ob-etiqueta.it-codigo       COLUMN-LABEL "Item"        FORMAT "x(6)":U  WIDTH 6
tt-ob-etiqueta.cod-refer       COLUMN-LABEL "Refer."  FORMAT "x(7)":U WIDTH 7
tt-ob-etiqueta.nr-lote         COLUMN-LABEL "Lote"  FORMAT "x(3)":U WIDTH 4
tt-ob-etiqueta.gramatura       COLUMN-LABEL "Gramatura" WIDTH 9
tt-ob-etiqueta.peso-embalagem  COLUMN-LABEL "Embal." WIDTH 7
tt-ob-etiqueta.quantidade      COLUMN-LABEL "Metro" WIDTH 6
tt-ob-etiqueta.peso-bruto-sist COLUMN-LABEL "P.Bruto"
tt-ob-etiqueta.peso-bruto      COLUMN-LABEL "Balanáa" WIDTH 8
tt-ob-etiqueta.dif-kg          COLUMN-LABEL "DIF. Kg"
tt-ob-etiqueta.dif-metro       COLUMN-LABEL "DIF.Metro"
tt-ob-etiqueta.gramatura-ideal COLUMN-LABEL "Gram.Ideal"
tt-ob-etiqueta.peso-liq-sist   COLUMN-LABEL "P.Liquido"
tt-ob-etiqueta.sit             COLUMN-LABEL "Situacao"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 103 BY 19
         FONT 6
         TITLE "Etiquetas Selecionadas" ROW-HEIGHT-CHARS .62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     br-ob-etiquetas AT ROW 1.25 COL 2.14
     bt-param AT ROW 1.54 COL 107
     bt-vapara AT ROW 2.83 COL 107
     bt-consulta AT ROW 4.13 COL 107
     bt-modifica AT ROW 5.42 COL 107
     bt-modifica-2 AT ROW 6.71 COL 107
     bt-imprime AT ROW 8 COL 107
     bt-excel AT ROW 9.58 COL 107
     bt-exit AT ROW 18.21 COL 107
     bt-ajuda AT ROW 19.54 COL 107
     fi-tot-metros AT ROW 20.33 COL 51.43 COLON-ALIGNED
     fi-tot-media AT ROW 20.33 COL 87.29 COLON-ALIGNED
     rt-buttom AT ROW 1.25 COL 106
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 112.72 BY 20.33.


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
         TITLE              = "Acompanhamento Peso Tecidos"
         COLUMN             = 22
         ROW                = 7
         HEIGHT             = 20.33
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
/* BROWSE-TAB br-ob-etiquetas rt-buttom DEFAULT-FRAME */
/* SETTINGS FOR BUTTON bt-excel IN FRAME DEFAULT-FRAME
   6                                                                    */
/* SETTINGS FOR FILL-IN fi-tot-media IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-metros IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-ob-etiquetas
/* Query rebuild information for BROWSE br-ob-etiquetas
     _START_FREEFORM
RUN pi-totais.
OPEN QUERY {&SELF-NAME} FOR EACH tt-ob-etiqueta USE-INDEX indice8 NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "mgmov.ped-venda.cod-sit-ped = 1"
     _Query            is OPENED
*/  /* BROWSE br-ob-etiquetas */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Acompanhamento Peso Tecidos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Acompanhamento Peso Tecidos */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-ob-etiquetas
&Scoped-define SELF-NAME br-ob-etiquetas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-ob-etiquetas C-Win
ON ROW-DISPLAY OF br-ob-etiquetas IN FRAME DEFAULT-FRAME /* Etiquetas Selecionadas */
DO:
  IF tt-ob-etiqueta.dif-kg < 0 AND tt-ob-etiqueta.peso-bruto <> 0 THEN
     tt-ob-etiqueta.dif-kg:FGCOLOR IN BROWSE br-ob-etiquetas = 12.
  IF tt-ob-etiqueta.gramatura-ideal > tt-ob-etiqueta.gramatura  THEN
         tt-ob-etiqueta.gramatura-ideal:FGCOLOR IN BROWSE br-ob-etiquetas = 12.
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
   FIND ob-etiqueta WHERE
        ob-etiqueta.cod-estabel  = c-cod-estabel AND
        ob-etiqueta.num-etiqueta = tt-ob-etiqueta.num-etiqueta NO-LOCK NO-ERROR.
   ASSIGN gr-ob-etiqueta = ROWID(ob-etiqueta).

   ASSIGN c-win:SENSITIVE = NO.
   RUN esp\essp0146.w.
   ASSIGN c-win:SENSITIVE = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-excel C-Win
ON CHOOSE OF bt-excel IN FRAME DEFAULT-FRAME /* Button 2 */
DO:
   RUN esdlg/d02essp0169.w (OUTPUT arq-saida).
   IF arq-saida <> "" THEN DO:
      RUN pi-gera-excel (INPUT arq-saida).
      MESSAGE "O arquivo foi gerado em " TRIM(arq-saida) + "." SKIP 
              "Para acess†-lo,  abra-o atravÇs do Excel."
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
   ASSIGN c-win:SENSITIVE = NO.
   RUN esp/essp0169b.w (INPUT-OUTPUT i-etiqueta-ini,   
                        INPUT-OUTPUT i-etiqueta-fin,   
                        INPUT-OUTPUT l-sintetico,   
                        INPUT-OUTPUT l-ok). 
   IF l-ok  THEN
      RUN pi-imprime.

   ASSIGN c-win:SENSITIVE = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-modifica
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-modifica C-Win
ON CHOOSE OF bt-modifica IN FRAME DEFAULT-FRAME /* Sair */
DO:
  FIND corte-comerc WHERE
       corte-comerc.codigo = tt-ob-etiqueta.corte-comerc NO-LOCK NO-ERROR.
  ASSIGN gr-corte-comerc = ROWID(corte-comerc).
      
  ASSIGN c-win:SENSITIVE = NO.
  RUN esp\essp0202.w.
  ASSIGN c-win:SENSITIVE = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-modifica-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-modifica-2 C-Win
ON CHOOSE OF bt-modifica-2 IN FRAME DEFAULT-FRAME /* Sair */
DO:
  FIND item WHERE
       item.it-codigo = tt-ob-etiqueta.it-codigo NO-LOCK NO-ERROR.
  ASSIGN gr-item = ROWID(item).
      
  ASSIGN c-win:SENSITIVE = NO.
  RUN esp\essp0203.w.
  ASSIGN c-win:SENSITIVE = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-param
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-param C-Win
ON CHOOSE OF bt-param IN FRAME DEFAULT-FRAME /* Sair */
DO:
   ASSIGN c-win:SENSITIVE = NO.
   RUN esp/essp0169a.w (INPUT-OUTPUT c-cod-estabel,
                        INPUT-OUTPUT da-dt-emissao-ini,   
                        INPUT-OUTPUT da-dt-emissao-fin,   
                        INPUT-OUTPUT c-it-codigo-ini,   
                        INPUT-OUTPUT c-it-codigo-fin,   
                        INPUT-OUTPUT c-cod-refer-ini,
                        INPUT-OUTPUT c-cod-refer-fin,
                        INPUT-OUTPUT c-corte-comerc-ini,
                        INPUT-OUTPUT c-corte-comerc-fin,
                        INPUT-OUTPUT i-tp-tecido,
                        INPUT-OUTPUT l-erro-peso,
                        INPUT-OUTPUT l-lote-todos,                 
                        INPUT-OUTPUT l-lote-rp,                    
                        INPUT-OUTPUT l-lote-rd,
                        INPUT-OUTPUT l-lote-sc,
                        INPUT-OUTPUT l-lote-ca,
                        INPUT-OUTPUT i-opc-acabado,
                        INPUT-OUTPUT i-tipo-ordem,
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
  
   RUN esdlg/d01essp0169.w (OUTPUT da-dt-emissao, 
                            OUTPUT i-num-etiqueta).

   IF da-dt-emissao <> ? THEN DO:
      FIND tt-ob-etiqueta WHERE
           tt-ob-etiqueta.dt-emissao   = da-dt-emissao  AND 
           tt-ob-etiqueta.num-etiqueta = i-num-etiqueta NO-LOCK NO-ERROR.

      IF NOT AVAIL tt-ob-etiqueta THEN DO.
         MESSAGE "Data/Etiquera n∆o est† contido na seleá∆o!"
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
         RETURN NO-APPLY.
      END.

      h-query:REPOSITION-TO-ROWID(ROWID(tt-ob-etiqueta)) NO-ERROR. 
      APPLY 'VALUE-CHANGED' TO br-ob-etiquetas.
   END.
   
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

ON 'F5':U OF br-ob-etiquetas DO:
   {&OPEN-QUERY-br-ob-etiquetas}
   APPLY 'value-changed' TO br-ob-etiquetas.
END.

ASSIGN h-query = br-ob-etiquetas:QUERY.
br-ob-etiquetas:NUM-LOCKED-COLUMNS = 3.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

   RUN enable_UI.

   RUN esapi/busca-estab-usuar.p (INPUT c-seg-usuario,
                                  OUTPUT c-cod-estabel).
   IF c-cod-estabel = '' THEN DO.
      MESSAGE 'Usuario ' c-seg-usuario ' n∆o relacionado Ö um Estabelecimento....'
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY "choose" TO bt-exit.
   END.

   ASSIGN da-dt-emissao-ini = DATE(MONTH(TODAY),1,YEAR(TODAY))
          da-dt-emissao-fin = TODAY.

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
  DISPLAY fi-tot-metros fi-tot-media 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE rt-buttom br-ob-etiquetas bt-param bt-vapara bt-consulta bt-modifica 
         bt-modifica-2 bt-imprime bt-excel bt-exit bt-ajuda 
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
        i-pag FORMAT ">>99999"                     AT 130
        SKIP(1).

    PUT "RELAT‡RIO DE ACOMPANHAMENTO PESO TECIDOS - PERIODO: " AT 31
        da-dt-emissao-ini AT 83
        "A"               AT 94
        da-dt-emissao-fin AT 96 SKIP(1).

    IF l-sintetico = NO THEN DO:
       PUT "Dt.Emiss∆ao   Etiqueta   Item  Refer. lote  Gramatura  Embal.  Metro P.Liquido P.Bruto Balanca  DIF. Kg  DIF.Metro Gram.Ideal Situacao" AT 1.
       PUT " ---------- ---------- ------ -------  ----- --------- ------- ------ --------- ------- -------  -------  --------- ---------- ------------" AT 1.
    END.
    /* ELSE DO:
       PUT "Item    Refer.  Gramat.  Embal. Quantidade(m)   Rolos  Peso Liquido    Peso Bruto   Peso Balanca   Diferenáa Kg Difenáa Metro Gram.Ideal" AT 1.
       PUT "------ -------  ------- ------- ------------- ------- ------------- ------------- --------------   ------------ ------------- ----------" AT 1.
    END. */
    ELSE DO:
       PUT "Item    Refer. lote  Gramat.  Embal. Quantidade(m)   Peso Liquido    Peso Bruto   Peso Balanca   Diferenáa Kg Difenáa Metro Gram.Ideal" AT 1.
       PUT "------ ------- ---- -------- ------- -------------   ------------- ------------- --------------   ------------ ------------- ----------" AT 1.
    END.
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




 DEF VAR h-prog    AS HANDLE NO-UNDO.
 DEF VAR i-ct      AS INT.
 DEF VAR de-totais AS DEC EXTENT 3.

 RUN utp/ut-utils.p PERSISTENT SET h-prog.

 &SCOPED-DEFINE BREAKBY tt-ob-etiqueta.it-codigo + tt-ob-etiqueta.cod-refer + tt-ob-etiqueta.nr-lote

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
         OUTPUT TO VALUE(c-saida) CONVERT SOURCE "ibm850".
     WHEN 3 THEN DO.
         ASSIGN c-saida = SESSION:TEMP-DIRECTORY + "essp0169.tmp".
         OUTPUT TO VALUE(c-saida) CONVERT SOURCE "ibm850".
     END.
 END CASE.

 ASSIGN i-lin     = 99
        i-pag     =  1
        de-totais =  0.

 DO i-ct = 1 TO i-num-copias.
    FOR EACH tt-ob-etiqueta USE-INDEX indice8 WHERE 
             tt-ob-etiqueta.num-etiqueta >= i-etiqueta-ini AND
             tt-ob-etiqueta.num-etiqueta <= i-etiqueta-fin 
             NO-LOCK
       BREAK BY {&BREAKBY}.
    
        IF i-lin > 61 THEN DO:
           RUN pi-imp-cabec.
           ASSIGN i-lin = 7.
        END.
        IF l-sintetico = NO THEN DO:
           PUT tt-ob-etiqueta.dt-emissao                          AT   1
               tt-ob-etiqueta.num-etiqueta    FORMAT ">>,>>>,>>9" AT  12
               tt-ob-etiqueta.it-codigo       FORMAT "x(6)"       AT  23
               tt-ob-etiqueta.cod-refer       FORMAT "X(7)"       AT  30
               tt-ob-etiqueta.nr-lote         FORMAT "X(2)"       AT  42
               tt-ob-etiqueta.gramatura       FORMAT ">>9.99999"  AT  45
               tt-ob-etiqueta.peso-embalagem  FORMAT "9.99999"    AT  56
               tt-ob-etiqueta.quantidade      FORMAT ">>9.99"     AT  64
               tt-ob-etiqueta.peso-liq-sist   FORMAT ">>9.999"    AT  73
               tt-ob-etiqueta.peso-bruto-sist FORMAT ">>9.999"    AT  80
               tt-ob-etiqueta.peso-bruto      FORMAT ">>9.999"    AT  88
               tt-ob-etiqueta.dif-kg          FORMAT "->9.999"    AT  97
               tt-ob-etiqueta.dif-metro       FORMAT "->>9.999"   AT 106
               tt-ob-etiqueta.gramatura-ideal FORMAT "->9.99999"  AT 116
               tt-ob-etiqueta.sit                                 AT 127.
           ASSIGN i-lin = i-lin + 1.
           ACCUMULATE tt-ob-etiqueta.quantidade      (TOTAL).
        END.
        ELSE DO:
           ACCUMULATE tt-ob-etiqueta.quantidade      (TOTAL BY {&BREAKBY}).
           ACCUMULATE tt-ob-etiqueta.peso-liq-sist   (TOTAL BY {&BREAKBY}).
           ACCUMULATE tt-ob-etiqueta.peso-bruto-sist (TOTAL BY {&BREAKBY}).
           ACCUMULATE tt-ob-etiqueta.peso-bruto      (TOTAL BY {&BREAKBY}).
           ACCUMULATE tt-ob-etiqueta.peso-embalagem  (TOTAL BY {&BREAKBY}).
           ACCUMULATE tt-ob-etiqueta.gramatura       (TOTAL BY {&BREAKBY}).
           ACCUMULATE tt-ob-etiqueta.num-etiqueta    (COUNT BY {&BREAKBY}).
        END.
    
        IF LAST-OF({&BREAKBY}) AND l-sintetico THEN DO:
           PUT  tt-ob-etiqueta.it-codigo       FORMAT "x(6)"       AT   1
                tt-ob-etiqueta.cod-refer       FORMAT "X(7)"       AT   8
                 tt-ob-etiqueta.nr-lote        FORMAT "X(2)"       AT  17
                tt-ob-etiqueta.gramatura       FORMAT "9.99999"    AT  21
                tt-ob-etiqueta.peso-embalagem  FORMAT "9.99999"    AT  30.
           PUT ACCUM TOTAL BY {&BREAKBY} tt-ob-etiqueta.quantidade      FORMAT ">>,>>>,>>9.99" AT 38.
      /*   PUT ACCUM COUNT BY {&BREAKBY} tt-ob-etiqueta.num-etiqueta    FORMAT ">>,>>9"        AT 48. */
           PUT ACCUM TOTAL BY {&BREAKBY} tt-ob-etiqueta.peso-liq-sist   FORMAT ">>,>>>,>>9.99" AT 54. 
           PUT ACCUM TOTAL BY {&BREAKBY} tt-ob-etiqueta.peso-bruto-sist FORMAT ">>,>>>,>>9.99" AT 68.
           PUT ACCUM TOTAL BY {&BREAKBY} tt-ob-etiqueta.peso-bruto      FORMAT ">>,>>>,>>9.99" AT 83.
   
           PUT ((ACCUM TOTAL BY {&BREAKBY} tt-ob-etiqueta.peso-bruto) - (ACCUM TOTAL BY {&BREAKBY} tt-ob-etiqueta.peso-bruto-sist)) FORMAT "->>>>,>>9.999" AT 98.
   
           PUT (((ACCUM TOTAL BY {&BREAKBY} tt-ob-etiqueta.peso-bruto) - (ACCUM TOTAL BY {&BREAKBY} tt-ob-etiqueta.peso-bruto-sist)) / (ACCUM TOTAL BY {&BREAKBY} tt-ob-etiqueta.gramatura)) FORMAT "->,>>>,>>9.99" AT 112.
   
           PUT (((ACCUM TOTAL BY {&BREAKBY} tt-ob-etiqueta.peso-bruto) - (ACCUM TOTAL BY {&BREAKBY} tt-ob-etiqueta.peso-embalagem)) / (ACCUM TOTAL BY {&BREAKBY} tt-ob-etiqueta.quantidade)) FORMAT "->>9.99999" AT 126.
           ASSIGN de-totais[1] = de-totais[1] + ACCUM TOTAL BY {&BREAKBY} tt-ob-etiqueta.quantidade 
                  de-totais[2] = de-totais[2] + ACCUM COUNT BY {&BREAKBY} tt-ob-etiqueta.num-etiqueta
                  de-totais[3] = de-totais[3] + ACCUM TOTAL BY {&BREAKBY} tt-ob-etiqueta.peso-bruto.
        END.
    END.
    IF l-sintetico AND de-totais[1] <> 0 THEN DO:
       PUT "-------------                                   ----------       " AT 38 SKIP.
       PUT de-totais[1] FORMAT ">>,>>>,>>9.99" AT 38.
      /* PUT de-totais[2] FORMAT ">>,>>9"        AT 48. */
       PUT de-totais[3] FORMAT ">>,>>>,>>9.99" AT 83.
    END.
    ELSE DO:
        PUT "------" AT 64 SKIP.
        PUT ACCUM TOTAL tt-ob-etiqueta.quantidade FORMAT ">>,>>>,>>9.99" AT 63.
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
 
 
/*

 DEF VAR h-prog    AS HANDLE NO-UNDO.
 DEF VAR i-ct      AS INT.
 DEF VAR de-totais AS DEC EXTENT 3.
 DEF VAR c-it-codigo AS CHAR.
 DEF VAR c-cod-refer AS CHAR.
 DEF VAR c-nr-lote AS CHAR.

 DEF VAR i-quantidade AS INT.    
 DEF VAR i-peso-liq-sist AS INT.  
 DEF VAR i-peso-bruto-sist AS INT.
 DEF VAR i-peso-bruto AS INT.    
 DEF VAR i-gramatura AS INT.
 DEF VAR i-peso-embalagem AS INT.

ASSIGN i-quantidade      = 0
       i-peso-liq-sist   = 0 
       i-peso-bruto-sist = 0
       i-peso-bruto      = 0
       i-gramatura       = 0
       i-peso-embalagem  = 0.





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
         OUTPUT TO VALUE(c-saida) CONVERT SOURCE "ibm850".
     WHEN 3 THEN DO.
         ASSIGN c-saida = SESSION:TEMP-DIRECTORY + "essp0169.tmp".
         OUTPUT TO VALUE(c-saida) CONVERT SOURCE "ibm850".
     END.
 END CASE.

 ASSIGN i-lin     = 140
        i-pag     =  1
        de-totais =  0.

 DO i-ct = 1 TO i-num-copias.
    FOR EACH tt-ob-etiqueta USE-INDEX indice8 WHERE 
             tt-ob-etiqueta.num-etiqueta >= i-etiqueta-ini AND
             tt-ob-etiqueta.num-etiqueta <= i-etiqueta-fin 
             NO-LOCK
       BREAK BY tt-ob-etiqueta.it-codigo.
    
        IF i-lin > 61 THEN DO:
           RUN pi-imp-cabec.
           ASSIGN i-lin = 7.
        END.
        IF l-sintetico = NO THEN DO:
           PUT tt-ob-etiqueta.dt-emissao                          AT   1
               tt-ob-etiqueta.num-etiqueta    FORMAT ">>,>>>,>>9" AT  12
               tt-ob-etiqueta.it-codigo       FORMAT "x(6)"       AT  23
               tt-ob-etiqueta.cod-refer       FORMAT "X(7)"       AT  30
               tt-ob-etiqueta.gramatura       FORMAT ">>9.99999"  AT  39
               tt-ob-etiqueta.peso-embalagem  FORMAT "9.99999"    AT  49
               tt-ob-etiqueta.quantidade      FORMAT ">>9.99"     AT  57
               tt-ob-etiqueta.peso-liq-sist   FORMAT ">>9.999"    AT  66
               tt-ob-etiqueta.peso-bruto-sist FORMAT ">>9.999"    AT  74
               tt-ob-etiqueta.peso-bruto      FORMAT ">>9.999"    AT  82
               tt-ob-etiqueta.dif-kg          FORMAT "->9.999"    AT  91
               tt-ob-etiqueta.dif-metro       FORMAT "->>9.999"   AT 100
               tt-ob-etiqueta.gramatura-ideal FORMAT "->9.99999"  AT 111
               tt-ob-etiqueta.sit                                 AT 121.
           ASSIGN i-lin = i-lin + 1.
            tt-ob-etiqueta.quantidade      .
        END.
        ELSE DO:

            ACCUMULATE tt-ob-etiqueta.quantidade      (TOTAL).
            ACCUMULATE tt-ob-etiqueta.peso-liq-sist   (TOTAL).
            ACCUMULATE tt-ob-etiqueta.peso-bruto-sist (TOTAL).
            ACCUMULATE tt-ob-etiqueta.peso-bruto      (TOTAL).
            ACCUMULATE tt-ob-etiqueta.peso-embalagem  (TOTAL).
            ACCUMULATE tt-ob-etiqueta.gramatura       (TOTAL).
            ACCUMULATE tt-ob-etiqueta.num-etiqueta    (COUNT).

        END.
    
        IF LAST-OF(tt-ob-etiqueta.it-codigo) AND l-sintetico THEN DO:



               ASSIGN i-quantidade      =   ACCUM TOTAL tt-ob-etiqueta.quantidade     
                      i-peso-liq-sist   =   ACCUM TOTAL tt-ob-etiqueta.peso-liq-sist  
                      i-peso-bruto-sist =   ACCUM TOTAL tt-ob-etiqueta.peso-bruto-sist
                      i-peso-bruto      =   ACCUM TOTAL tt-ob-etiqueta.peso-bruto     
                      i-gramatura       =   ACCUM TOTAL tt-ob-etiqueta.gramatura
                      i-peso-embalagem  =   ACCUM TOTAL tt-ob-etiqueta.peso-embalagem.

               
               PUT tt-ob-etiqueta.it-codigo       FORMAT "x(6)"       AT   1
                   tt-ob-etiqueta.cod-refer       FORMAT "X(7)"       AT   8
                   tt-ob-etiqueta.gramatura       FORMAT "->9.999"    AT  17
                   tt-ob-etiqueta.peso-embalagem  FORMAT "->9.999"    AT  25.
               PUT i-quantidade       FORMAT "->>,>>>,>>9.99" AT 33.
               PUT i-peso-liq-sist    FORMAT "->>,>>>,>>9.99" AT 48.
               PUT i-peso-bruto-sist  FORMAT "->>,>>>,>>9.99" AT 62.
               PUT i-peso-bruto       FORMAT "->>,>>>,>>9.99" AT 77.
                   
               PUT ((i-peso-bruto) - (i-peso-bruto-sist)) FORMAT "->>>>,>>9.999" AT 90.
       
               PUT (((i-peso-bruto) - (i-peso-bruto-sist)) / (i-gramatura)) FORMAT "->,>>>,>>9.99" AT 105.
                                                                                                                                                      
               PUT (((i-peso-bruto) - (i-peso-embalagem)) / (i-quantidade)) FORMAT "->>9.99999" AT 120.
               ASSIGN de-totais[1] = de-totais[1] + i-quantidade 
                      de-totais[2] = de-totais[2] + tt-ob-etiqueta.num-etiqueta
                      de-totais[3] = de-totais[3] + i-peso-bruto.
               
               ASSIGN i-quantidade      =  tt-ob-etiqueta.quantidade      
                      i-peso-liq-sist   =  tt-ob-etiqueta.peso-liq-sist  
                      i-peso-bruto-sist =  tt-ob-etiqueta.peso-bruto-sist
                      i-peso-bruto      =  tt-ob-etiqueta.peso-bruto     
                      i-gramatura       =  tt-ob-etiqueta.gramatura
                      i-peso-embalagem  =  tt-ob-etiqueta.peso-embalagem.

/*                MESSAGE i-quantidade                   */
/*                        i-peso-liq-sist                */
/*                        i-peso-bruto-sist              */
/*                        i-peso-bruto                   */
/*                        i-gramatura                    */
/*                    VIEW-AS ALERT-BOX INFO BUTTONS OK. */
            
        END.
        ELSE IF l-sintetico = NO THEN
           ASSIGN de-totais[1] = de-totais[1] +  tt-ob-etiqueta.quantidade.
    END.
    IF l-sintetico AND de-totais[1] <> 0 THEN DO:
       PUT "-------------                 --------------" AT 33 SKIP.
       PUT de-totais[1] FORMAT ">>,>>>,>>9.99" AT 33.
       PUT de-totais[3] FORMAT ">>,>>>,>>9.99" AT 62.
    END.
    ELSE DO:
        PUT "------" AT 57 SKIP.
        PUT ACCUM TOTAL tt-ob-etiqueta.quantidade FORMAT ">>>9.99" AT 56.
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
 
*/


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
 DEF VAR lin-col AS CHAR.

 /* Cabeáalho  da Planilha */
 ASSIGN c-Lin = c-empresa + "             " + " ACOMPANHAMENTO DE PESO TECIDOS - PERIODO: "  + STRING(da-dt-emissao-ini, "99/99/9999") + " A " + STRING(da-dt-emissao-fin, "99/99/9999"). 
 DDE SEND i-canal SOURCE c-Lin ITEM "L1C1".
 DDE EXECUTE i-canal COMMAND '[select("L1C1:L1C11")]'.
 /* Fonte, Tamanho, Negrito, It†lico, Sublinhado, Atachado, Cor(0=Autom†tico, 1=Preto, 2=Branco 3=Vermelho...) */
 DDE EXECUTE sys COMMAND "[format.font(~"Verdana~",10,True,False,False,False,3)]".

 /* Cabeáalho dos Dados */
                          
 DDE SEND i-canal SOURCE "DT EMISS«O" ITEM "L3C1".
 DDE SEND i-canal SOURCE "ETIQUETA"   ITEM "L3C2".
 DDE SEND i-canal SOURCE "ITEM"       ITEM "L3C3".
 DDE SEND i-canal SOURCE "REFER“NCIA" ITEM "L3C4".
 DDE SEND i-canal SOURCE "LOTE"       ITEM "L3C5".
 DDE SEND i-canal SOURCE "GRAMATURA"  ITEM "L3C6".
 DDE SEND i-canal SOURCE "EMBAL."     ITEM "L3C7".
 DDE SEND i-canal SOURCE "METRO"      ITEM "L3C8".
 DDE SEND i-canal SOURCE "P.LIQUIDO"  ITEM "L3C9".
 DDE SEND i-canal SOURCE "P.BRUTO"    ITEM "L3C10".
 DDE SEND i-canal SOURCE "BALANCA"    ITEM "L3C11".
 DDE SEND i-canal SOURCE "DIF. KG"    ITEM "L3c12".
 DDE SEND i-canal SOURCE "DIF.METRO"  ITEM "L3C13".
 DDE SEND i-canal SOURCE "GRAM.IDEAL" ITEM "L3C14".
 DDE SEND i-canal SOURCE "SITUAÄ«O"   ITEM "L3C15". 
 /* Formataá∆o das Celulas do Cabeáalho de Dados */
 DDE EXECUTE i-canal COMMAND '[select("L3C1:L3C15")]'.
 /* Fonte, Tamanho, Negrito, It†lico, Sublinhado, Atachado, Cor(0=Autom†tico, 1=Preto, 2=Branco 3=Vermelho...) */
 DDE EXECUTE sys COMMAND "[format.font(~"Verdana~",8,True,False,False,False,5)]".

 DDE EXECUTE i-canal COMMAND "[select(~"C1~")]". 
 DDE EXECUTE sys     COMMAND "[column.width(11.00)]".

 DDE EXECUTE i-canal COMMAND "[select(~"C2~")]". 
 DDE EXECUTE sys     COMMAND "[column.width(9.00)]".

 DDE EXECUTE i-canal COMMAND "[select(~"C3~")]". 
 DDE EXECUTE sys     COMMAND "[column.width(7.00)]". 

 DDE EXECUTE i-canal COMMAND "[select(~"C4~")]". 
 DDE EXECUTE sys     COMMAND "[column.width(12.00)]".

 DDE EXECUTE i-canal COMMAND "[select(~"C5~")]". 
 DDE EXECUTE sys     COMMAND "[column.width(8.00)]".

 DDE EXECUTE i-canal COMMAND "[select(~"C6~")]". 
 DDE EXECUTE sys     COMMAND "[column.width(13.00)]".
 DDE EXECUTE sys     COMMAND "[format.number(~"##0,00000~")]".
 DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]". 

 DDE EXECUTE i-canal COMMAND "[select(~"C7~")]". 
 DDE EXECUTE sys     COMMAND "[column.width(10.00)]".
 DDE EXECUTE sys     COMMAND "[format.number(~"##0,00000~")]".
 DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]". 

 DDE EXECUTE i-canal COMMAND "[select(~"C8~")]". 
 DDE EXECUTE sys     COMMAND "[column.width(11.00)]".
 DDE EXECUTE sys     COMMAND "[format.number(~"###.##0,00~")]".
 DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]". 

 DDE EXECUTE i-canal COMMAND "[select(~"C9~")]". 
 DDE EXECUTE sys     COMMAND "[column.width(11.00)]".
 DDE EXECUTE sys     COMMAND "[format.number(~"###.##0,00~")]".
 DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]". 

 DDE EXECUTE i-canal COMMAND "[select(~"C10~")]". 
 DDE EXECUTE sys     COMMAND "[column.width(10.00)]".
 DDE EXECUTE sys     COMMAND "[format.number(~"###.##0,00~")]".
 DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]". 

 DDE EXECUTE i-canal COMMAND "[select(~"C11~")]". 
 DDE EXECUTE sys     COMMAND "[column.width(10.00)]".
 DDE EXECUTE sys     COMMAND "[format.number(~"###.##0,00~")]".
 DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]". 

 DDE EXECUTE i-canal COMMAND "[select(~"C12~")]". 
 DDE EXECUTE sys     COMMAND "[column.width(10.00)]".
 DDE EXECUTE sys     COMMAND "[format.number(~"###.##0,00~")]".
 DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]". 

 DDE EXECUTE i-canal COMMAND "[select(~"C13~")]". 
 DDE EXECUTE sys     COMMAND "[column.width(11.00)]".
 DDE EXECUTE sys     COMMAND "[format.number(~"###.##0,00~")]".
 DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]". 

 DDE EXECUTE i-canal COMMAND "[select(~"C14~")]". 
 DDE EXECUTE sys     COMMAND "[column.width(12.00)]".
 DDE EXECUTE sys     COMMAND "[format.number(~"##0,00000~")]".
 DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]". 
 
 DDE EXECUTE i-canal COMMAND "[select(~"C15~")]". 
 DDE EXECUTE sys     COMMAND "[column.width(12.00)]".

 /* Montagem das Celulas de Dados */
 ASSIGN i-Lin  = 4.
 FOR EACH tt-ob-etiqueta NO-LOCK.
     DDE SEND i-canal SOURCE STRING(tt-ob-etiqueta.dt-emissao)             ITEM "L" + TRIM(STRING(i-Lin)) + "C1". 
     DDE SEND i-canal SOURCE STRING(tt-ob-etiqueta.num-etiqueta)           ITEM "L" + TRIM(STRING(i-Lin)) + "C2".
     DDE SEND i-canal SOURCE STRING(tt-ob-etiqueta.it-codigo)              ITEM "L" + TRIM(STRING(i-Lin)) + "C3".
     DDE SEND i-canal SOURCE STRING(tt-ob-etiqueta.cod-refer, "99 9999 9") ITEM "L" + TRIM(STRING(i-Lin)) + "C4".
     DDE SEND i-canal SOURCE STRING(tt-ob-etiqueta.nr-lote)       ITEM "L" + TRIM(STRING(i-Lin)) + "C5".
     DDE SEND i-canal SOURCE STRING(tt-ob-etiqueta.gramatura)       ITEM "L" + TRIM(STRING(i-Lin)) + "C6".
     DDE SEND i-canal SOURCE STRING(tt-ob-etiqueta.peso-embalagem)  ITEM "L" + TRIM(STRING(i-Lin)) + "C7".
     DDE SEND i-canal SOURCE STRING(tt-ob-etiqueta.quantidade)      ITEM "L" + TRIM(STRING(i-Lin)) + "C8".
     DDE SEND i-canal SOURCE STRING(tt-ob-etiqueta.peso-liq-sist)   ITEM "L" + TRIM(STRING(i-Lin)) + "C9".
     DDE SEND i-canal SOURCE STRING(tt-ob-etiqueta.peso-bruto-sist) ITEM "L" + TRIM(STRING(i-Lin)) + "C10".
     DDE SEND i-canal SOURCE STRING(tt-ob-etiqueta.peso-bruto)      ITEM "L" + TRIM(STRING(i-Lin)) + "C11".
     DDE SEND i-canal SOURCE STRING(tt-ob-etiqueta.dif-kg)          ITEM "L" + TRIM(STRING(i-Lin)) + "C12".
     DDE SEND i-canal SOURCE STRING(tt-ob-etiqueta.dif-metro)       ITEM "L" + TRIM(STRING(i-Lin)) + "C13".
     DDE SEND i-canal SOURCE STRING(tt-ob-etiqueta.gramatura-ideal) ITEM "L" + TRIM(STRING(i-Lin)) + "C14".
     DDE SEND i-canal SOURCE STRING(tt-ob-etiqueta.sit)             ITEM "L" + TRIM(STRING(i-Lin)) + "C15".
     
     ASSIGN aux-command = '[select("L' + TRIM(STRING(i-Lin)) + 'C1:L' + TRIM(STRING(i-Lin)) + 'C15")]'.
     DDE EXECUTE i-canal COMMAND aux-command.
     /* Fonte, Tamanho, Negrito, It†lico, Sublinhado, Atachado, Cor(0=Autom†tico, 1=Preto, 2=Branco 3=Vermelho...) */
     DDE EXECUTE sys COMMAND "[format.font(~"Verdana~",8,False,False,False,False,0)]".

     IF tt-ob-etiqueta.dif-kg < 0 THEN DO:
        DDE EXECUTE i-canal COMMAND '[select("L' + TRIM(STRING(i-Lin)) + 'C12:L' + TRIM(STRING(i-Lin)) + 'C12")]'.
        DDE EXECUTE sys COMMAND "[format.font(~"Verdana~",8,false,False,False,False,3)]".
     END.

     IF tt-ob-etiqueta.gramatura-ideal < 0 THEN DO:
        DDE EXECUTE i-canal COMMAND '[select("L' + TRIM(STRING(i-Lin)) + 'C14:L' + TRIM(STRING(i-Lin)) + 'C14")]'.
        DDE EXECUTE sys COMMAND "[format.font(~"Verdana~",8,false,False,False,False,3)]".
     END.
     
     ASSIGN i-Lin = i-Lin + 1.
 END.

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
   {utp/ut-liter.i Selecionando_Etiquetas *}
   RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

   EMPTY TEMP-TABLE tt-ob-etiqueta.

   ASSIGN c-lotes = "".
   IF l-lote-todos = YES THEN
      ASSIGN c-lotes = "rp,rd,sc,ca,".
   ELSE DO:
      ASSIGN c-lotes = c-lotes + IF l-lote-rp = YES THEN "rp," ELSE ",".
      ASSIGN c-lotes = c-lotes + IF l-lote-rd = YES THEN "rd," ELSE ",".
      ASSIGN c-lotes = c-lotes + IF l-lote-sc = YES THEN "sc," ELSE ",".
      ASSIGN c-lotes = c-lotes + IF l-lote-ca = YES THEN "ca," ELSE ",".
   END.

   /* Busca Nome da Empresa */
   FIND FIRST param-global NO-LOCK NO-ERROR.
   FIND FIRST empresa WHERE
              empresa.ep-codigo = param-global.empresa-prin NO-LOCK NO-ERROR. 
   ASSIGN c-empresa = (IF AVAIL empresa THEN empresa.razao-social ELSE "").


   RUN pi-separa-etiquetas.

   RUN pi-finalizar in h-acomp.

   {&OPEN-QUERY-br-ob-etiquetas}
   
   APPLY 'value-changed' TO br-ob-etiquetas IN FRAME {&FRAME-NAME}.
   APPLY 'entry' TO br-ob-etiquetas IN FRAME {&FRAME-NAME}.
   RETURN NO-APPLY.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-separa-etiquetas C-Win 
PROCEDURE pi-separa-etiquetas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
    FOR EACH ob-etiqueta USE-INDEX indice8 WHERE 
             ob-etiqueta.dt-emissao   >= da-dt-emissao-ini    AND 
             ob-etiqueta.dt-emissao   <= da-dt-emissao-fin    AND
             ob-etiqueta.it-codigo    >= c-it-codigo-ini      AND
             ob-etiqueta.it-codigo    <= c-it-codigo-fin      AND 
             ob-etiqueta.cod-refer    >= c-cod-refer-ini      AND
             ob-etiqueta.cod-refer    <= c-cod-refer-fin      AND
             ob-etiqueta.corte-comerc >= c-corte-comerc-ini   AND
             ob-etiqueta.corte-comerc <= c-corte-comerc-fin   AND
             ob-etiqueta.cod-estabel = c-cod-estabel /*AND
             LOOKUP(ob-etiqueta.nr-lote,c-lotes) <> 0  */ NO-LOCK:  
             
        /*
        IF i-opc-acabado = 1 AND SUBSTR(ob-etiqueta.cod-refer,7,1) <> '0' THEN NEXT.
        IF i-opc-acabado = 2 AND SUBSTR(ob-etiqueta.cod-refer,7,1) = '0' THEN NEXT.

        IF i-tipo-ordem <> 9 AND 
           ob-etiqueta.tipo-ordem <> i-tipo-ordem THEN NEXT.

        IF ob-etiqueta.situacao = 1 OR  /* Impressao */
           ob-etiqueta.situacao = 7 THEN NEXT. /* Consumo Corte */
        */

        /*
        IF ob-etiqueta.peso-bruto <= 0 AND l-erro-peso = NO THEN NEXT. /* Somente Rolos Pesados pela Balanáa */
        IF l-erro-peso AND ob-etiqueta.erro-peso = NO  THEN NEXT. /* Somente com Erro na Pesagem */
        IF l-erro-peso = NO AND ob-etiqueta.erro-peso = YES THEN NEXT. /* Somente Rolos sem erro Pesagem */
        */

        RUN pi-acompanhar IN h-acomp (INPUT "Data: " + STRING(ob-etiqueta.dt-emissao) + "   " + 
                                            "Etiqueta: " + trim(STRING(ob-etiqueta.num-etiqueta,">>>,>>>,>>9"))).

        FIND item-ext WHERE
             item-ext.it-codigo = ob-etiqueta.it-codigo NO-LOCK NO-ERROR.
        
        /*IF i-tp-tecido = 1 AND item-ext.indigo <> YES THEN NEXT. /* Somente Indigo */*/

        /*IF i-tp-tecido = 2 AND item-ext.indigo <> NO  THEN NEXT. /* Somente N∆o Indigo */*/
        
        FIND ITEM WHERE
             ITEM.it-codigo = ob-etiqueta.it-codigo NO-LOCK NO-ERROR.

        FIND corte-comerc WHERE
             corte-comerc.codigo = ob-etiqueta.corte-comerc NO-LOCK NO-ERROR.

        FIND FIRST ob-param NO-LOCK NO-ERROR.

        {esinc/i-dsrb.i ob-etiqueta.situacao ob-etiqueta.situacao c-situacao}  

        FIND tt-ob-etiqueta WHERE 
             tt-ob-etiqueta.dt-emissao   = ob-etiqueta.dt-emissao   AND
             tt-ob-etiqueta.num-etiqueta = ob-etiqueta.num-etiqueta NO-ERROR.
        IF NOT AVAIL tt-ob-etiqueta THEN DO:
           CREATE tt-ob-etiqueta.
           BUFFER-COPY ob-etiqueta TO tt-ob-etiqueta.
           
           ASSIGN tt-ob-etiqueta.gramatura       = ITEM.peso-liquido
                  tt-ob-etiqueta.peso-embalagem  = IF AVAIL corte-comerc 
                                                   THEN corte-comerc.peso-emb-outros
                                                   ELSE 0
                  tt-ob-etiqueta.peso-liq-sist   = tt-ob-etiqueta.quantidade * tt-ob-etiqueta.gramatura
                  tt-ob-etiqueta.peso-bruto-sist = tt-ob-etiqueta.peso-liq-sist + tt-ob-etiqueta.peso-embalagem
                  tt-ob-etiqueta.dif-kg          = tt-ob-etiqueta.peso-bruto - tt-ob-etiqueta.peso-bruto-sist
                  tt-ob-etiqueta.dif-metro       = (tt-ob-etiqueta.peso-bruto - tt-ob-etiqueta.peso-bruto-sist) /
                                                   tt-ob-etiqueta.gramatura
                  tt-ob-etiqueta.gramatura-ideal = (tt-ob-etiqueta.peso-bruto - tt-ob-etiqueta.peso-embalagem) /
                                                   tt-ob-etiqueta.quantidade
                  tt-ob-etiqueta.sit             = c-situacao.

           IF tt-ob-etiqueta.gramatura-ideal = ? THEN
              ASSIGN tt-ob-etiqueta.gramatura-ideal = 0.00.

        END. 

        
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
ASSIGN fi-tot-metros = 0.
FOR EACH tt-ob-etiqueta USE-INDEX indice8 NO-LOCK.
    ASSIGN fi-tot-metros = fi-tot-metros + tt-ob-etiqueta.quantidade.
    ACCUMULATE tt-ob-etiqueta.gramatura-ideal (AVERAGE).
END.
ASSIGN fi-tot-media = (ACCUM AVERAGE tt-ob-etiqueta.gramatura-ideal).
DISP fi-tot-metros
     fi-tot-media
     WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

