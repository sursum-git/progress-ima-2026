&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          espec            PROGRESS
*/
&Scoped-define WINDOW-NAME w-consim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-consim 
/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESSP0162 2.04.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEF {1} TEMP-TABLE wt-etiquetas LIKE ob-etiqueta.
DEF {1} VAR i-num-reserva LIKE ped-reserva.num-reserva.
DEF {1} VAR c-tp-acao   AS CHAR.
DEF {1} VAR de-qtd-disp AS DEC.
DEF {1} VAR c-it-codigo AS CHAR.
DEF {1} VAR c-cod-refer AS CHAR.   
DEF {1} VAR c-nr-lote   AS CHAR.
DEF {1} VAR c-corte-comerc AS CHAR.
DEF {1} VAR l-alterou-res  AS LOG.
DEF {1} VAR c-estab        AS CHAR.   

/* Local Variable Definitions ---                                       */
DEF TEMP-TABLE tt-etq-reservadas LIKE ob-etiqueta
    FIELD tp-acao                AS CHAR.

DEF TEMP-TABLE tt-item-disp LIKE ped-reserva-it.

DEF NEW GLOBAL SHARED VAR gr-ob-etiqueta AS ROWID NO-UNDO.

DEF VAR i-row    AS INT.
DEF VAR h-query  AS HANDLE.
DEF VAR c-qualid AS CHAR FORMAT "x(13)".
DEF VAR c-tear   AS CHAR.
DEF VAR c-corte  AS CHAR.
DEF VAR i-qtd-reservar AS DEC.
DEF VAR i-nr-sequencia AS INT.
DEF VAR r-item-disp AS ROWID.
DEF VAR i-tp-embal AS INT.

/* VARIAVEL DEFINIDA PARA O ZOOM */
def var wh-pesquisa as widget-handle.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-consim
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad
&Scoped-define BROWSE-NAME br-etq-estoque

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES wt-etiquetas tt-etq-reservadas tt-item-disp

/* Definitions for BROWSE br-etq-estoque                                */
&Scoped-define FIELDS-IN-QUERY-br-etq-estoque wt-etiquetas.localizacao wt-etiquetas.num-etiqueta wt-etiquetas.nuance fn-qualid('wt-etiquetas') @ c-qualid fn-tear('wt-etiquetas') @ c-tear wt-etiquetas.quantidade   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-etq-estoque   
&Scoped-define SELF-NAME br-etq-estoque
&Scoped-define OPEN-QUERY-br-etq-estoque RUN pi-soma-est. OPEN QUERY {&SELF-NAME} FOR EACH wt-etiquetas WHERE                                  wt-etiquetas.situacao = 3 NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-etq-estoque wt-etiquetas
&Scoped-define FIRST-TABLE-IN-QUERY-br-etq-estoque wt-etiquetas


/* Definitions for BROWSE br-etq-reservadas                             */
&Scoped-define FIELDS-IN-QUERY-br-etq-reservadas tt-etq-reservadas.localizacao tt-etq-reservadas.num-etiqueta tt-etq-reservadas.nuance fn-qualid('tt-etq-reservadas') @ c-qualid fn-tear('tt-etq-reservadas') @ c-tear tt-etq-reservadas.quantidade   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-etq-reservadas   
&Scoped-define SELF-NAME br-etq-reservadas
&Scoped-define OPEN-QUERY-br-etq-reservadas RUN pi-soma-res. OPEN QUERY {&SELF-NAME} FOR EACH tt-etq-reservadas WHERE                                  tt-etq-reservadas.tp-acao <> 'Del'                                  INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-etq-reservadas tt-etq-reservadas
&Scoped-define FIRST-TABLE-IN-QUERY-br-etq-reservadas tt-etq-reservadas


/* Definitions for BROWSE br-item-disp                                  */
&Scoped-define FIELDS-IN-QUERY-br-item-disp tt-item-disp.nr-sequencia tt-item-disp.it-codigo tt-item-disp.cod-refer fn-corte() @ c-corte tt-item-disp.nr-lote   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-item-disp   
&Scoped-define SELF-NAME br-item-disp
&Scoped-define OPEN-QUERY-br-item-disp RUN pi-soma-item. OPEN QUERY {&SELF-NAME} FOR EACH tt-item-disp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-item-disp tt-item-disp
&Scoped-define FIRST-TABLE-IN-QUERY-br-item-disp tt-item-disp


/* Definitions for FRAME f-cad                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-cad ~
    ~{&OPEN-QUERY-br-etq-estoque}~
    ~{&OPEN-QUERY-br-etq-reservadas}~
    ~{&OPEN-QUERY-br-item-disp}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-item-disp br-etq-estoque ~
br-etq-reservadas RECT-11 rt-button 
&Scoped-Define DISPLAYED-OBJECTS fi-tot-res-disp fi-de-qtd-disp ~
fi-nome-emitente fi-cod-emitente fi-nome-repres fi-cod-rep fi-dt-validade ~
fi-tot-estoque fi-tot-reservado 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-4 bt-det bt-add bt-del 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-corte w-consim 
FUNCTION fn-corte RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-qualid w-consim 
FUNCTION fn-qualid RETURNS CHARACTER
  (INPUT c-tabela AS CHAR) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-tear w-consim 
FUNCTION fn-tear RETURNS CHARACTER
  (INPUT c-tabela AS CHAR) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-consim AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU mi-arquivo 
       MENU-ITEM mi-primeiro    LABEL "&Primeiro"      ACCELERATOR "CTRL-HOME"
       MENU-ITEM mi-anterior    LABEL "An&terior"      ACCELERATOR "CTRL-CURSOR-LEFT"
       MENU-ITEM mi-proximo     LABEL "Pr&¢ximo"       ACCELERATOR "CTRL-CURSOR-RIGHT"
       MENU-ITEM mi-ultimo      LABEL "&Èltimo"        ACCELERATOR "CTRL-END"
       MENU-ITEM mi-va-para     LABEL "&V† para"       ACCELERATOR "CTRL-T"
       MENU-ITEM mi-pesquisa    LABEL "Pes&quisa"      ACCELERATOR "CTRL-F5"
       RULE
       MENU-ITEM mi-consultas   LABEL "Co&nsultas"     ACCELERATOR "CTRL-L"
       MENU-ITEM mi-imprimir    LABEL "&Relat¢rios"    ACCELERATOR "CTRL-P"
       RULE
       MENU-ITEM mi-sair        LABEL "&Sair"          ACCELERATOR "CTRL-X".

DEFINE SUB-MENU m_Ajuda 
       MENU-ITEM mi-conteudo    LABEL "&Conteudo"     
       MENU-ITEM mi-sobre       LABEL "&Sobre..."     .

DEFINE MENU m-cadastro MENUBAR
       SUB-MENU  mi-arquivo     LABEL "&Arquivo"      
       SUB-MENU  m_Ajuda        LABEL "&Ajuda"        .


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_p-exihel AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-navega AS HANDLE NO-UNDO.
DEFINE VARIABLE h_q01es016 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v01es016 AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-add 
     IMAGE-UP FILE "image/im-down.bmp":U
     LABEL "" 
     SIZE 5 BY 1.46.

DEFINE BUTTON bt-del 
     IMAGE-UP FILE "image/im-uptear.bmp":U
     LABEL "" 
     SIZE 5 BY 1.46.

DEFINE BUTTON bt-det 
     IMAGE-UP FILE "image/im-det.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-det.bmp":U
     LABEL "" 
     SIZE 4.86 BY 1.46 TOOLTIP "Detalha Etiquetas Reservadas".

DEFINE BUTTON bt-ok AUTO-GO 
     IMAGE-UP FILE "image/im-cq.bmp":U
     LABEL "OK" 
     SIZE 4 BY 1.21 TOOLTIP "Grava Reserva para o Cliente".

DEFINE VARIABLE fi-cod-emitente AS CHARACTER FORMAT "X(12)" 
     LABEL "Codigo Emitente" 
     VIEW-AS FILL-IN 
     SIZE 11.43 BY .88.

DEFINE VARIABLE fi-cod-rep AS INTEGER FORMAT ">>>,>>>" INITIAL 0 
     LABEL "Representante" 
     VIEW-AS FILL-IN 
     SIZE 9.14 BY .88.

DEFINE VARIABLE fi-de-qtd-disp AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Disponivel" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88
     FGCOLOR 12 FONT 6 NO-UNDO.

DEFINE VARIABLE fi-dt-validade AS DATE FORMAT "99/99/9999" 
     LABEL "Data de Validade" 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88.

DEFINE VARIABLE fi-nome-emitente AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 45.43 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nome-repres AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 43.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-tot-estoque AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Estoque Total" 
     VIEW-AS FILL-IN 
     SIZE 10.86 BY .88
     FGCOLOR 9 FONT 6 NO-UNDO.

DEFINE VARIABLE fi-tot-res-disp AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Total Reservado" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88
     FGCOLOR 4  NO-UNDO.

DEFINE VARIABLE fi-tot-reservado AS DECIMAL FORMAT ">>>,>>9.99":U INITIAL 0 
     LABEL "Reservado" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88
     FGCOLOR 2 FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 97 BY 2.5.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 97.43 BY 1.46
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-etq-estoque FOR 
      wt-etiquetas SCROLLING.

DEFINE QUERY br-etq-reservadas FOR 
      tt-etq-reservadas SCROLLING.

DEFINE QUERY br-item-disp FOR 
      tt-item-disp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-etq-estoque
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-etq-estoque w-consim _FREEFORM
  QUERY br-etq-estoque NO-LOCK DISPLAY
      wt-etiquetas.localizacao  FORMAT "999/999":U    COLUMN-LABEL "Localizaá∆o"  WIDTH 9
wt-etiquetas.num-etiqueta FORMAT "999999999":U  COLUMN-LABEL "Etiqueta"     WIDTH 9
wt-etiquetas.nuance       FORMAT " X(2)":U      COLUMN-LABEL "Nuance"       WIDTH 6
fn-qualid('wt-etiquetas') @ c-qualid    WIDTH 11    COLUMN-LABEL "Qualidade"
fn-tear('wt-etiquetas') @ c-tear        WIDTH 7    COLUMN-LABEL "Tear"
wt-etiquetas.quantidade   FORMAT ">>9.99":U     COLUMN-LABEL "Qtde (m)"     WIDTH 10
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 58.57 BY 5.08
         FONT 1
         TITLE "Estoque Dispon°vel" ROW-HEIGHT-CHARS .46.

DEFINE BROWSE br-etq-reservadas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-etq-reservadas w-consim _FREEFORM
  QUERY br-etq-reservadas NO-LOCK DISPLAY
      tt-etq-reservadas.localizacao  FORMAT "999/999":U    COLUMN-LABEL "Localizaá∆o"  WIDTH 9
tt-etq-reservadas.num-etiqueta FORMAT "999999999":U  COLUMN-LABEL "Etiqueta"     WIDTH 9
tt-etq-reservadas.nuance       FORMAT " X(2)":U      COLUMN-LABEL "Nuance"       WIDTH 6
fn-qualid('tt-etq-reservadas') @ c-qualid         WIDTH 11              COLUMN-LABEL "Qualidade"
fn-tear('tt-etq-reservadas')   @ c-tear           WIDTH 7               COLUMN-LABEL "Tear"

tt-etq-reservadas.quantidade   FORMAT ">>9.99":U     COLUMN-LABEL "Qtde (m)"     WIDTH 10
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 58.57 BY 6.25
         FONT 1
         TITLE "Peáas Reservadas" ROW-HEIGHT-CHARS .46.

DEFINE BROWSE br-item-disp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-item-disp w-consim _FREEFORM
  QUERY br-item-disp NO-LOCK DISPLAY
      tt-item-disp.nr-sequencia  FORMAT ">>>9":U     COLUMN-LABEL "Seq"     WIDTH 3      
tt-item-disp.it-codigo FORMAT "x(8)":U
tt-item-disp.cod-refer FORMAT "x(8)":U  COLUMN-LABEL "Referância"   WIDTH 8
fn-corte() @ c-corte     COLUMN-LABEL "Corte"                       WIDTH 11 
tt-item-disp.nr-lote FORMAT "X(4)":U                                WIDTH 3.5
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 37 BY 13.33
         FONT 1
         TITLE "Item Disponivel" ROW-HEIGHT-CHARS .46.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     fi-tot-res-disp AT ROW 21.38 COL 12.29 COLON-ALIGNED
     fi-de-qtd-disp AT ROW 21.38 COL 52 COLON-ALIGNED
     bt-ok AT ROW 1.42 COL 61
     fi-nome-emitente AT ROW 5.5 COL 26.72 COLON-ALIGNED NO-LABEL
     fi-cod-emitente AT ROW 5.54 COL 15 COLON-ALIGNED
     fi-nome-repres AT ROW 6.5 COL 24.43 COLON-ALIGNED NO-LABEL
     fi-cod-rep AT ROW 6.54 COL 15 COLON-ALIGNED
     fi-dt-validade AT ROW 6.5 COL 82 COLON-ALIGNED
     br-item-disp AT ROW 7.92 COL 2
     br-etq-estoque AT ROW 7.92 COL 40
     bt-det AT ROW 13.25 COL 40.72
     bt-add AT ROW 13.25 COL 51
     bt-del AT ROW 13.25 COL 57.29
     fi-tot-estoque AT ROW 13.63 COL 83.14 COLON-ALIGNED
     br-etq-reservadas AT ROW 15 COL 40
     fi-tot-reservado AT ROW 21.38 COL 83 COLON-ALIGNED
     RECT-11 AT ROW 5.25 COL 2
     rt-button AT ROW 1.21 COL 1.57
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 98.29 BY 21.54
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: w-consim
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-consim ASSIGN
         HIDDEN             = YES
         TITLE              = "Manutená∆o das Reservas para Clientes"
         HEIGHT             = 21.54
         WIDTH              = 98.29
         MAX-HEIGHT         = 28.21
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 28.21
         VIRTUAL-WIDTH      = 146.29
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU m-cadastro:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-consim 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-consim.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-consim
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-cad
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB br-item-disp fi-dt-validade f-cad */
/* BROWSE-TAB br-etq-estoque br-item-disp f-cad */
/* BROWSE-TAB br-etq-reservadas fi-tot-estoque f-cad */
/* SETTINGS FOR BUTTON bt-add IN FRAME f-cad
   NO-ENABLE 4                                                          */
/* SETTINGS FOR BUTTON bt-del IN FRAME f-cad
   NO-ENABLE 4                                                          */
/* SETTINGS FOR BUTTON bt-det IN FRAME f-cad
   NO-ENABLE 4                                                          */
/* SETTINGS FOR BUTTON bt-ok IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-cod-emitente IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-cod-rep IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-de-qtd-disp IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-dt-validade IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nome-emitente IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nome-repres IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-estoque IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-res-disp IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-reservado IN FRAME f-cad
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-consim)
THEN w-consim:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-etq-estoque
/* Query rebuild information for BROWSE br-etq-estoque
     _START_FREEFORM
RUN pi-soma-est.
OPEN QUERY {&SELF-NAME} FOR EACH wt-etiquetas WHERE
                                 wt-etiquetas.situacao = 3 NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "espec.ob-etiqueta.situacao = 3 AND
espec.ob-etiqueta.it-codigo = tt-itens.it-codigo AND
espec.ob-etiqueta.cod-refer = tt-itens.cod-refer AND
espec.ob-etiqueta.nr-lote = tt-positivo.lote AND
espec.ob-etiqueta.corte-comerc = tt-positivo.corte-comerc"
     _Query            is OPENED
*/  /* BROWSE br-etq-estoque */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-etq-reservadas
/* Query rebuild information for BROWSE br-etq-reservadas
     _START_FREEFORM
RUN pi-soma-res.
OPEN QUERY {&SELF-NAME} FOR EACH tt-etq-reservadas WHERE
                                 tt-etq-reservadas.tp-acao <> 'Del'
                                 INDEXED-REPOSITION.
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

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-consim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-consim w-consim
ON END-ERROR OF w-consim /* Manutená∆o das Reservas para Clientes */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-consim w-consim
ON WINDOW-CLOSE OF w-consim /* Manutená∆o das Reservas para Clientes */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-etq-estoque
&Scoped-define SELF-NAME br-etq-estoque
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-etq-estoque w-consim
ON VALUE-CHANGED OF br-etq-estoque IN FRAME f-cad /* Estoque Dispon°vel */
DO:
   ASSIGN bt-det:SENSITIVE IN FRAME {&FRAME-NAME} = NO
          bt-add:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
   IF AVAIL wt-etiquetas THEN DO.
      ASSIGN bt-det:SENSITIVE IN FRAME {&FRAME-NAME} = YES
             bt-add:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

      FIND ob-etiqueta WHERE
           ob-etiqueta.cod-estabel  = wt-etiquetas.cod-estabel AND
           ob-etiqueta.num-etiqueta = wt-etiquetas.num-etiqueta 
           NO-LOCK NO-ERROR.
      IF AVAIL ob-etiqueta THEN
         ASSIGN gr-ob-etiqueta = ROWID(ob-etiqueta).
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-etq-reservadas
&Scoped-define SELF-NAME br-etq-reservadas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-etq-reservadas w-consim
ON ENTRY OF br-etq-reservadas IN FRAME f-cad /* Peáas Reservadas */
DO:
   APPLY 'value-changed' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-etq-reservadas w-consim
ON MOUSE-SELECT-DBLCLICK OF br-etq-reservadas IN FRAME f-cad /* Peáas Reservadas */
DO:
   ASSIGN gr-ob-etiqueta = ROWID(tt-etq-reservadas).
   APPLY 'choose' TO bt-det.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-etq-reservadas w-consim
ON VALUE-CHANGED OF br-etq-reservadas IN FRAME f-cad /* Peáas Reservadas */
DO:
   ASSIGN bt-det:SENSITIVE IN FRAME {&FRAME-NAME} = NO
          bt-del:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
   IF AVAIL tt-etq-reservadas THEN DO.
      ASSIGN bt-det:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

      IF c-tp-acao <> '' THEN
         bt-del:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

      FIND ob-etiqueta WHERE
           ob-etiqueta.cod-estabel  = tt-etq-reservadas.cod-estabel AND
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-item-disp w-consim
ON VALUE-CHANGED OF br-item-disp IN FRAME f-cad /* Item Disponivel */
DO:
   FOR EACH tt-etq-reservadas.
       DELETE tt-etq-reservadas.
   END.
   FOR EACH ped-reserva-etq OF tt-item-disp NO-LOCK.
       FIND ped-reserva WHERE
            ped-reserva.num-reserva = ped-reserva-etq.num-reserva NO-LOCK NO-ERROR.
       IF NOT AVAIL ped-reserva THEN NEXT.
       FIND ob-etiqueta WHERE
            ob-etiqueta.cod-estabel  = ped-reserva.cod-estabel AND
            ob-etiqueta.num-etiqueta = ped-reserva-etq.num-etiqueta NO-LOCK NO-ERROR.
       IF AVAIL ob-etiqueta THEN DO:
          CREATE tt-etq-reservadas.
          BUFFER-COPY ob-etiqueta TO tt-etq-reservadas
                      ASSIGN tt-etq-reservadas.tp-acao = "Mod".
       END.
   END.
   {&OPEN-QUERY-br-etq-reservadas}
   IF AVAIL tt-item-disp THEN
      ASSIGN i-nr-sequencia = tt-item-disp.nr-sequencia.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-add w-consim
ON CHOOSE OF bt-add IN FRAME f-cad
DO:
   IF fi-cod-emitente:SCREEN-VALUE = '' THEN DO.
      MESSAGE 'Cliente deve ser Informado...'
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
       APPLY 'entry' TO fi-cod-emitente.
       RETURN NO-APPLY.
   END.

   ASSIGN i-qtd-reservar = 0.
   DO i-row = 1 TO br-etq-estoque:NUM-SELECTED-ROWS:
      FIND ob-etiqueta WHERE
           ob-etiqueta.cod-estabel  = wt-etiquetas.cod-estabel AND
           ob-etiqueta.num-etiqueta = wt-etiquetas.num-etiqueta NO-LOCK NO-ERROR.
      /*
      IF ob-etiqueta.corte-comerc <> "I" AND
         ob-etiqueta.localiz BEGINS '7' THEN NEXT.
      */

      IF br-etq-estoque:FETCH-SELECTED-ROW(i-row) THEN 
         ASSIGN i-qtd-reservar = i-qtd-reservar + wt-etiquetas.quantidade.
   END.

   IF fi-tot-reservado + i-qtd-reservar > fi-de-qtd-disp THEN DO.
      MESSAGE 'Reserva N∆o Permitida. Qtde Selecionada Superior a Disponivel...'
               VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      RETURN NO-APPLY.
   END.

   DO i-row = 1 TO br-etq-estoque:NUM-SELECTED-ROWS:
      IF br-etq-estoque:FETCH-SELECTED-ROW(i-row) THEN DO.
         FIND ob-etiqueta WHERE
              ob-etiqueta.cod-estabel  = wt-etiquetas.cod-estabel AND
              ob-etiqueta.num-etiqueta = wt-etiquetas.num-etiqueta NO-LOCK NO-ERROR.
         /*
         IF ob-etiqueta.corte-comerc <> "I" AND
            ob-etiqueta.localiz BEGINS '7' THEN DO.
            MESSAGE 'Etiqueta ' wt-etiquetas.num-etiqueta 'foi enviada para o Dep¢sito 700 (Corte)'
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            NEXT.
         END.
         IF ob-etiqueta.localizacao = '' THEN DO.
            MESSAGE 'Etiqueta ' wt-etiquetas.num-etiqueta ' est† sem localizá∆o'
                 VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            NEXT.
         END.

         IF i-tp-embal <> 0 THEN DO.
            FIND corte-comerc WHERE
                 corte-comerc.codigo = ob-etiqueta.corte-comerc NO-LOCK NO-ERROR.
            IF corte-comerc.tp-embal <> i-tp-embal THEN DO.
               MESSAGE 'Etiqueta ' wt-etiquetas.num-etiqueta ' Lote Ç diferente do Lote da Reserva'
                      VIEW-AS ALERT-BOX ERROR BUTTONS OK.
               NEXT.
            END.
         END.
         */

         FIND tt-etq-reservadas WHERE
              tt-etq-reservadas.cod-estabel  = wt-etiquetas.cod-estabel AND
              tt-etq-reservadas.num-etiqueta = wt-etiquetas.num-etiqueta NO-ERROR.
         IF NOT AVAIL tt-etq-reservadas THEN DO.
            CREATE tt-etq-reservadas.
            BUFFER-COPY wt-etiquetas TO tt-etq-reservadas.
         END.
         ASSIGN tt-etq-reservadas.tp-acao = 'Inc'.

         DELETE wt-etiquetas.
      END.
   END.

   {&OPEN-QUERY-br-etq-estoque}
   {&OPEN-QUERY-br-etq-reservadas}

    ASSIGN bt-add:SENSITIVE IN FRAME {&FRAME-NAME} = YES
           bt-del:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

    IF NUM-RESULTS("br-etq-estoque") = 0 THEN
       ASSIGN bt-add:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

    IF NUM-RESULTS("br-etq-reservadas") = 0 THEN
       ASSIGN bt-del:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-del
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-del w-consim
ON CHOOSE OF bt-del IN FRAME f-cad
DO:
   DO i-row = 1 TO br-etq-reservadas:NUM-SELECTED-ROWS.
      IF br-etq-reservadas:FETCH-SELECTED-ROW(i-row) THEN DO.

         FIND wt-etiquetas WHERE
              wt-etiquetas.cod-estabel  = tt-etq-reservadas.cod-estabel AND
              wt-etiquetas.num-etiqueta = tt-etq-reservadas.num-etiqueta NO-ERROR.
         IF NOT AVAIL wt-etiquetas THEN DO.
            CREATE wt-etiquetas.
            BUFFER-COPY tt-etq-reservadas TO wt-etiquetas.
         END.
         ASSIGN wt-etiquetas.situacao = 3
                tt-etq-reservadas.tp-acao = 'Del'.
      END.
   END.

   {&OPEN-QUERY-br-etq-estoque}
   {&OPEN-QUERY-br-etq-reservadas}

   ASSIGN bt-add:SENSITIVE IN FRAME {&FRAME-NAME} = YES
          bt-del:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

   IF NUM-RESULTS("br-etq-estoque") = 0 THEN
      ASSIGN bt-add:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

   IF NUM-RESULTS("br-etq-reservadas") = 0 THEN
      ASSIGN bt-del:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-det
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-det w-consim
ON CHOOSE OF bt-det IN FRAME f-cad
DO:
   RUN esp/essp0146.p.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-consim
ON CHOOSE OF bt-ok IN FRAME f-cad /* OK */
DO:
   FOR EACH tt-etq-reservadas.
       IF tt-etq-reservadas.tp-acao = 'Inc' THEN DO.
          FIND ob-etiqueta WHERE
               ob-etiqueta.cod-estabel  = tt-etq-reservadas.cod-estabel AND
               ob-etiqueta.num-etiqueta = tt-etq-reservadas.num-etiqueta NO-ERROR.

          IF ob-etiqueta.situacao <> 3 THEN DO.
             MESSAGE 'Etiqueta ' + STRING(ob-etiqueta.num-etiqueta) + ' j† foi Reservada por outro Usu†rio...'
                  VIEW-AS ALERT-BOX ERROR BUTTONS OK.
             APPLY 'entry' TO SELF.
             RETURN NO-APPLY.
          END.
       END.
   END.

   IF c-tp-acao = 'Mod' THEN DO:
      IF INPUT FRAME {&FRAME-NAME} fi-dt-validade > fi-dt-validade + 7 OR
         INPUT FRAME {&FRAME-NAME} fi-dt-validade < fi-dt-validade  THEN DO:
         MESSAGE 'Data de Validade Inval°da...'
             VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY 'entry' TO fi-dt-validade.
         RETURN NO-APPLY.
      END.
   END.
   ELSE DO:
      IF fi-cod-emitente:SCREEN-VALUE = '' THEN DO.
          MESSAGE 'Favor Informar o C¢digo do Emitente...'
              VIEW-AS ALERT-BOX ERROR BUTTONS OK.
          APPLY 'entry' TO fi-cod-emitente.
          RETURN NO-APPLY.
      END.

      FIND emitente WHERE 
           emitente.nome-abrev = INPUT FRAME {&FRAME-NAME} fi-cod-emitente
           USE-INDEX nome NO-LOCK NO-ERROR.
      IF NOT AVAIL emitente THEN
         FIND emitente WHERE 
              STRING(emitente.cod-emit) = INPUT FRAME {&FRAME-NAME} fi-cod-emitente 
              USE-INDEX codigo NO-LOCK NO-ERROR.
      IF NOT AVAIL emitente THEN DO.
         MESSAGE 'Cliente n∆o Cadastrado...'
             VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY 'entry' TO SELF.
         RETURN NO-APPLY.
      END.
      IF emitente.identific = 2 THEN DO:
          MESSAGE 'O c¢digo informado n∆o Ç de cliente...'
              VIEW-AS ALERT-BOX ERROR BUTTONS OK.
          APPLY 'entry' TO SELF.
          RETURN NO-APPLY.
      END.

      IF emitente.ind-cre-cli = 4 THEN DO.
         MESSAGE 'Cliente Suspenso para Implantaá∆o/Efetivaá∆o de Pedidos'
             VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY 'entry' TO SELF.
         RETURN NO-APPLY.
      END.

      IF emitente.ind-sit-emitente = 2 THEN DO.
         MESSAGE 'Cliente Bloqueado....'
             VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY 'entry' TO SELF.
         RETURN NO-APPLY.
      END.

      IF fi-cod-rep:SCREEN-VALUE = '' THEN DO:
          MESSAGE 'Favor Informar o C¢digo do Representante...'
              VIEW-AS ALERT-BOX ERROR BUTTONS OK.
          APPLY 'entry' TO fi-cod-emitente.
          RETURN NO-APPLY.
      END.
      IF fi-tot-reservado = 0 THEN DO:
          MESSAGE 'Favor Informar as Peáas a Serem Reservadas...'
              VIEW-AS ALERT-BOX ERROR BUTTONS OK.
          RETURN NO-APPLY.
      END.
   END.

   ASSIGN INPUT FRAME {&FRAME-NAME} fi-cod-emitente
          INPUT FRAME {&FRAME-NAME} fi-cod-rep
          INPUT FRAME {&FRAME-NAME} fi-dt-validade.
   
   ASSIGN l-alterou-res = YES.

   RUN pi-grava-dados IN h_v01es016 (INPUT TABLE tt-etq-reservadas,
                                     INPUT TABLE tt-item-disp,
                                     INPUT c-tp-acao,
                                     INPUT emitente.cod-emit,
                                     INPUT fi-cod-rep,
                                     INPUT fi-dt-validade,
                                     INPUT i-nr-sequencia,
                                     INPUT c-estab).
   RUN pi-sair IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-emitente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-emitente w-consim
ON ENTRY OF fi-cod-emitente IN FRAME f-cad /* Codigo Emitente */
DO: 
   IF SELF:SCREEN-VALUE <> "" THEN DO.
       FIND emitente WHERE 
            emitente.nome-abrev = INPUT FRAME {&FRAME-NAME} fi-cod-emitente
            USE-INDEX nome NO-LOCK NO-ERROR.
       IF NOT AVAIL emitente THEN
          FIND emitente WHERE 
               STRING(emitente.cod-emit) = INPUT FRAME {&FRAME-NAME} fi-cod-emitente 
               USE-INDEX codigo NO-LOCK NO-ERROR.
      IF NOT AVAIL emitente THEN DO.
         MESSAGE 'Cliente n∆o Cadastrado...'
             VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY 'entry' TO SELF.
         RETURN NO-APPLY.
      END.
      ASSIGN fi-nome-emitente:SCREEN-VALUE IN FRAME {&FRAME-NAME} = emitente.nome-emit.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-emitente w-consim
ON LEAVE OF fi-cod-emitente IN FRAME f-cad /* Codigo Emitente */
DO:
   IF KEYFUNCTION(LASTKEY) = 'BACK-TAB' THEN DO.
      APPLY 'ENTRY' TO bt-ok.
      RETURN NO-APPLY.
   END.

   FIND emitente WHERE 
        emitente.nome-abrev = INPUT FRAME {&FRAME-NAME} fi-cod-emitente
        USE-INDEX nome NO-LOCK NO-ERROR.
   IF NOT AVAIL emitente THEN
      FIND emitente WHERE 
           STRING(emitente.cod-emit) = INPUT FRAME {&FRAME-NAME} fi-cod-emitente 
           USE-INDEX codigo NO-LOCK NO-ERROR.
   IF AVAIL emitente THEN DO.
      ASSIGN fi-nome-emitente:SCREEN-VALUE IN FRAME {&FRAME-NAME} = emitente.nome-emit.
      FIND repres WHERE
           repres.cod-rep = emitente.cod-rep NO-LOCK NO-ERROR.
      IF AVAIL repres THEN
         ASSIGN fi-cod-rep:SCREEN-VALUE = STRING(repres.cod-rep)
                fi-nome-repres:SCREEN-VALUE = repres.nome. 
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-emitente w-consim
ON MOUSE-SELECT-DBLCLICK OF fi-cod-emitente IN FRAME f-cad /* Codigo Emitente */
DO:
    {include/zoomvar.i &prog-zoom=adzoom/z02ad098.w 
                       &campo=fi-cod-emitente
                       &campo2=fi-nome-emitente
                       &campozoom=cod-emitente
                       &campozoom2=nome-emit}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-rep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-rep w-consim
ON ENTRY OF fi-cod-rep IN FRAME f-cad /* Representante */
DO:
   IF SELF:SCREEN-VALUE <> "0" THEN DO.
      FIND repres WHERE
           repres.cod-rep = INPUT FRAME {&FRAME-NAME} fi-cod-rep
           NO-LOCK NO-ERROR.
      IF NOT AVAIL repres THEN DO.
         MESSAGE 'Representante n∆o Cadastrado...'
             VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY 'entry' TO SELF.
         RETURN NO-APPLY.
      END.
      ASSIGN fi-nome-repres:SCREEN-VALUE IN FRAME {&FRAME-NAME} = repres.nome.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-rep w-consim
ON LEAVE OF fi-cod-rep IN FRAME f-cad /* Representante */
DO:
  FIND repres WHERE
       repres.cod-rep = INPUT FRAME {&FRAME-NAME} fi-cod-rep
       NO-LOCK NO-ERROR.
  IF NOT AVAIL repres THEN DO.
     MESSAGE 'Representante n∆o Cadastrado...'
         VIEW-AS ALERT-BOX ERROR BUTTONS OK.
     APPLY 'entry' TO SELF.
     RETURN NO-APPLY.
  END.
  ASSIGN fi-nome-repres:SCREEN-VALUE IN FRAME {&FRAME-NAME} = repres.nome.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-rep w-consim
ON MOUSE-SELECT-DBLCLICK OF fi-cod-rep IN FRAME f-cad /* Representante */
DO:
    /*
    {include/zoomvar.i &prog-zoom=adzoom/z02ad098.w 
                       &campo=fi-cod-emitente
                       &campo2=fi-nome-emitente
                       &campozoom=cod-emitente
                       &campozoom2=nome-emit}


    */


   {include/zoomvar.i &prog-zoom  = adzoom/z01ad229.w
                      &campo      = fi-cod-rep
                      &campo2     = fi-nome-repres
                      &campozoom  = cod-rep
                      &campozoom2 = nome-repres}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-dt-validade
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-dt-validade w-consim
ON LEAVE OF fi-dt-validade IN FRAME f-cad /* Data de Validade */
DO:
  /*
   IF INPUT FRAME {&FRAME-NAME} fi-dt-validade > fi-dt-validade + 7 OR
      INPUT FRAME {&FRAME-NAME} fi-dt-validade < fi-dt-validade  THEN DO:
      MESSAGE 'Data de Validade Inval°da...'
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      APPLY 'entry' TO SELF.
      RETURN NO-APPLY.
   END.
   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-anterior
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-anterior w-consim
ON CHOOSE OF MENU-ITEM mi-anterior /* Anterior */
DO:
  RUN pi-anterior IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-arquivo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-arquivo w-consim
ON MENU-DROP OF MENU mi-arquivo /* Arquivo */
DO:
  run pi-disable-menu.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-consultas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-consultas w-consim
ON CHOOSE OF MENU-ITEM mi-consultas /* Consultas */
DO:
  RUN pi-consulta IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-conteudo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-conteudo w-consim
ON CHOOSE OF MENU-ITEM mi-conteudo /* Conteudo */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  RUN pi-ajuda IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-imprimir w-consim
ON CHOOSE OF MENU-ITEM mi-imprimir /* Relat¢rios */
DO:
  RUN pi-imprimir IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-pesquisa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-pesquisa w-consim
ON CHOOSE OF MENU-ITEM mi-pesquisa /* Pesquisa */
DO:
  RUN pi-pesquisa IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-primeiro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-primeiro w-consim
ON CHOOSE OF MENU-ITEM mi-primeiro /* Primeiro */
DO:
  RUN pi-primeiro IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-proximo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-proximo w-consim
ON CHOOSE OF MENU-ITEM mi-proximo /* Pr¢ximo */
DO:
  RUN pi-proximo IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sair
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sair w-consim
ON CHOOSE OF MENU-ITEM mi-sair /* Sair */
DO:
  RUN pi-sair IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sobre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sobre w-consim
ON CHOOSE OF MENU-ITEM mi-sobre /* Sobre... */
DO:
  {include/sobre.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-ultimo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-ultimo w-consim
ON CHOOSE OF MENU-ITEM mi-ultimo /* Èltimo */
DO:
  RUN pi-ultimo IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-va-para
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-va-para w-consim
ON CHOOSE OF MENU-ITEM mi-va-para /* V† para */
DO:
  RUN pi-vapara IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-etq-estoque
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-consim 


/* ***************************  Main Block  *************************** */
IF c-tp-acao <> ""  THEN DO:
   fi-cod-emitente:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.
   fi-cod-rep:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.

   ASSIGN fi-de-qtd-disp = de-qtd-disp
          l-alterou-res = NO.
END.

ASSIGN h-query = br-item-disp:QUERY.

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-consim  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  CASE adm-current-page: 

    WHEN 0 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panel/p-navega.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Edge-Pixels = 0,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-navega ).
       RUN set-position IN h_p-navega ( 1.33 , 2.14 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 24.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panel/p-exihel.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-exihel ).
       RUN set-position IN h_p-exihel ( 1.33 , 82.43 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'esvwr/v01es016.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v01es016 ).
       RUN set-position IN h_v01es016 ( 2.75 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.50 , 97.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'esqry/q01es016.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_q01es016 ).
       RUN set-position IN h_q01es016 ( 1.46 , 70.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.00 , 7.72 ) */

       /* Links to SmartViewer h_v01es016. */
       RUN add-link IN adm-broker-hdl ( h_q01es016 , 'Record':U , h_v01es016 ).

       /* Links to SmartQuery h_q01es016. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , h_q01es016 ).
       RUN add-link IN adm-broker-hdl ( h_p-navega , 'Navigation':U , h_q01es016 ).
       RUN add-link IN adm-broker-hdl ( h_p-navega , 'State':U , h_q01es016 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-navega ,
             fi-de-qtd-disp:HANDLE IN FRAME f-cad , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             h_p-navega , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v01es016 ,
             bt-ok:HANDLE IN FRAME f-cad , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-consim  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-consim  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-consim)
  THEN DELETE WIDGET w-consim.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-consim  _DEFAULT-ENABLE
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
  DISPLAY fi-tot-res-disp fi-de-qtd-disp fi-nome-emitente fi-cod-emitente 
          fi-nome-repres fi-cod-rep fi-dt-validade fi-tot-estoque 
          fi-tot-reservado 
      WITH FRAME f-cad IN WINDOW w-consim.
  ENABLE br-item-disp br-etq-estoque br-etq-reservadas RECT-11 rt-button 
      WITH FRAME f-cad IN WINDOW w-consim.
  {&OPEN-BROWSERS-IN-QUERY-f-cad}
  VIEW w-consim.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-apply-entry w-consim 
PROCEDURE local-apply-entry :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'apply-entry':U ) .

  /* Code placed here will execute AFTER standard behavior.    */


  CASE c-tp-acao.
      WHEN 'IncItem' THEN DO. /* Inclui novo item na reserva */
          ASSIGN fi-cod-emitente:SENSITIVE IN FRAME {&FRAME-NAME} = YES
                 fi-dt-validade:SENSITIVE IN FRAME {&FRAME-NAME}  = YES.

          RUN pi-add-record IN h_v01es016 (INPUT c-tp-acao).
      END.
      WHEN 'Inc' THEN DO. /* INCLUSAO DE NOVA RESERVA */
         RUN pi-add-record IN h_v01es016 (INPUT c-tp-acao).
         
         ASSIGN fi-cod-emitente:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

         ASSIGN fi-cod-emitente  = '' 
                fi-nome-emitente = ''
                fi-cod-rep       = 0 
                fi-nome-repres   = ''
                fi-dt-validade   = TODAY + 7.
      END.
      WHEN 'Mod' THEN DO. /* MODIFICA RESERVA */
         ASSIGN fi-cod-emitente:SENSITIVE IN FRAME {&FRAME-NAME} = YES
                fi-dt-validade:SENSITIVE IN FRAME {&FRAME-NAME}  = YES.
      END.
  END CASE.

  DISP fi-cod-emitente
       fi-nome-emitente
       fi-cod-rep
       fi-nome-repres
       fi-dt-validade
       WITH FRAME {&FRAME-NAME}.

  APPLY 'entry' TO fi-cod-emitente.
  RETURN NO-APPLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-consim 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-consim 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-consim 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  {include/win-size.i}

  run pi-before-initialize.

  {utp/ut9000.i "ESSP0162" "2.04.00.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  IF i-num-reserva <> 0 THEN DO.
     FIND ped-reserva WHERE
          ped-reserva.num-reserva = i-num-reserva
          NO-LOCK NO-ERROR.

     RUN pi-reposiciona-query IN h_q01es016 (INPUT ROWID(ped-reserva)).
  END.
  ELSE
     RUN dispatch IN h_q01es016 ( INPUT "get-first":U).

  RUN pi-after-initialize.

  IF c-tp-acao <> "" THEN
     ASSIGN bt-ok:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

  IF c-tp-acao = "IncItem" THEN
     RUN pi-disable-all IN h_p-navega.


  RUN local-apply-entry.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-monta-itens w-consim 
PROCEDURE pi-monta-itens :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER p-num-reserva LIKE ped-reserva.num-reserva.

    FIND ped-reserva WHERE 
         ped-reserva.num-reserva = p-num-reserva NO-LOCK NO-ERROR.

    FOR EACH tt-item-disp.
        DELETE tt-item-disp.
    END.

    FOR EACH ped-reserva-it OF ped-reserva NO-LOCK.
        CREATE tt-item-disp.
        BUFFER-COPY ped-reserva-it TO tt-item-disp.
    END.

    FIND LAST ped-reserva-it OF ped-reserva NO-LOCK NO-ERROR.
    ASSIGN i-tp-embal = 0.
    IF AVAIL ped-reserva-it THEN DO.
       FIND corte-comerc WHERE
            corte-comerc.codigo = ped-reserva-it.corte-comerc
            NO-LOCK NO-ERROR.
       IF AVAIL corte-comerc THEN
          ASSIGN i-tp-embal = corte-comerc.tp-embal.
    END.

    CREATE tt-item-disp.
    ASSIGN tt-item-disp.num-reserva   = p-num-reserva
           tt-item-disp.nr-sequencia  = IF AVAIL ped-reserva-it
                                        THEN ped-reserva-it.nr-sequencia + 10
                                        ELSE 10
           tt-item-disp.it-codigo     = c-it-codigo
           tt-item-disp.cod-refer     = c-cod-refer
           tt-item-disp.corte-comerc  = c-corte-comerc
           tt-item-disp.nr-lote       = c-nr-lote.

    ASSIGN r-item-disp = ROWID(tt-item-disp).

    {&OPEN-QUERY-br-item-disp}
    {&OPEN-QUERY-br-etq-estoque}

    IF AVAIL tt-item-disp THEN DO.
       h-query:REPOSITION-TO-ROWID(r-item-disp) NO-ERROR. 
       br-item-disp:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
    END.

    DISP fi-tot-res-disp WITH FRAME {&FRAME-NAME}.

    APPLY 'ENTRY' TO br-item-disp.
    APPLY 'VALUE-CHANGED' TO br-item-disp.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-mostra-cli w-consim 
PROCEDURE pi-mostra-cli :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER p-num-reserva AS INT.

    FIND ped-reserva WHERE
         ped-reserva.num-reserva = p-num-reserva NO-LOCK  NO-ERROR.
    IF AVAIL ped-reserva THEN DO:
       FIND emitente WHERE
            emitente.cod-emitente = ped-reserva.cod-emitente NO-LOCK NO-ERROR.
       FIND repres WHERE
            repres.cod-rep = ped-reserva.cod-rep NO-LOCK NO-ERROR.

       ASSIGN fi-cod-emitente  = STRING(ped-reserva.cod-emitente)
              fi-nome-emitente = IF AVAIL emitente
                                 THEN emitente.nome-emit
                                 ELSE ''
              fi-cod-rep       = ped-reserva.cod-rep
              fi-nome-repres   = IF AVAIL repres
                                 THEN repres.nome
                                 ELSE ''
              fi-dt-validade   = ped-reserva.dt-validade.

       DISP fi-cod-emitente 
            fi-nome-emitente
            fi-cod-rep 
            fi-nome-repres 
            fi-dt-validade 
            WITH FRAME {&FRAME-NAME}.
    END.

    ASSIGN bt-add:SENSITIVE IN FRAME {&FRAME-NAME} = YES
           bt-del:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

    FOR EACH tt-item-disp.
        DELETE tt-item-disp.
    END.
    FOR EACH ped-reserva-it OF ped-reserva NO-LOCK.
        CREATE tt-item-disp.
        BUFFER-COPY ped-reserva-it TO tt-item-disp.

    END.
    ASSIGN br-item-disp:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

    FIND tt-item-disp WHERE
         tt-item-disp.it-codigo = c-it-codigo AND
         tt-item-disp.cod-refer = c-cod-refer AND
         tt-item-disp.nr-lote = c-nr-lote AND
         tt-item-disp.corte-comerc = c-corte-comerc
         NO-LOCK NO-ERROR.

    IF NOT AVAIL tt-item-disp THEN
       FIND FIRST tt-item-disp NO-LOCK NO-ERROR.

    IF c-tp-acao = '' THEN DO.
       ASSIGN br-item-disp:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
       ASSIGN bt-add:SENSITIVE IN FRAME {&FRAME-NAME} = NO
              bt-del:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
    END.

    ASSIGN r-item-disp = ROWID(tt-item-disp).

    {&OPEN-QUERY-br-item-disp}
    {&OPEN-QUERY-br-etq-estoque}

    IF AVAIL tt-item-disp THEN DO.
       h-query:REPOSITION-TO-ROWID(r-item-disp) NO-ERROR. 
       br-item-disp:SELECT-FOCUSED-ROW().
    END.

    APPLY 'VALUE-CHANGED' TO br-item-disp.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-soma-est w-consim 
PROCEDURE pi-soma-est :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 ASSIGN fi-tot-estoque = 0.
 FOR EACH wt-etiquetas WHERE
          wt-etiquetas.situacao = 3 NO-LOCK.
     ASSIGN fi-tot-estoque = fi-tot-estoque + wt-etiquetas.quantidade.
 END.
 DISP fi-tot-estoque WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-soma-item w-consim 
PROCEDURE pi-soma-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 ASSIGN fi-tot-res-disp = 0.
 FOR EACH tt-item-disp NO-LOCK.
     FOR EACH  ped-reserva-etq OF tt-item-disp NO-LOCK.
         FIND ped-reserva WHERE
              ped-reserva.num-reserva = ped-reserva-etq.num-reserva NO-LOCK NO-ERROR.
         IF NOT AVAIL ped-reserva THEN NEXT.
         FIND ob-etiqueta WHERE
              ob-etiqueta.cod-estabel  = ped-reserva.cod-estabel AND
              ob-etiqueta.num-etiqueta = ped-reserva-etq.num-etiqueta NO-LOCK NO-ERROR.
         IF AVAIL ob-etiqueta THEN
            ASSIGN fi-tot-res-disp = fi-tot-res-disp + ob-etiqueta.quantidade.
     END.
 END.
 DISP fi-tot-res-disp WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-soma-res w-consim 
PROCEDURE pi-soma-res :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN fi-tot-reservado = 0.
    FOR EACH tt-etq-reservadas WHERE 
             tt-etq-reservadas.tp-acao <> 'Del' NO-LOCK.
        ASSIGN fi-tot-reservado = fi-tot-reservado + tt-etq-reservadas.quantidade.
    END.
    DISP fi-tot-reservado WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-consim  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "wt-etiquetas"}
  {src/adm/template/snd-list.i "tt-item-disp"}
  {src/adm/template/snd-list.i "tt-etq-reservadas"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-consim 
PROCEDURE state-changed :
/*:T -----------------------------------------------------------
  Purpose:     Manuseia trocas de estado dos SmartObjects
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-corte w-consim 
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
     RETURN "N∆o Informada".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-qualid w-consim 
FUNCTION fn-qualid RETURNS CHARACTER
  (INPUT c-tabela AS CHAR):
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND qualid-tecido WHERE
       qualid-tecido.codigo = IF c-tabela = 'wt-etiquetas' 
                              THEN wt-etiquetas.cod-qualid
                              ELSE tt-etq-reservadas.cod-qualid
       NO-LOCK NO-ERROR.

  IF AVAIL qualid-tecido THEN 
     RETURN qualid-tecido.descricao.   /* Function return value. */
  ELSE
     RETURN "N∆o Informada".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-tear w-consim 
FUNCTION fn-tear RETURNS CHARACTER
  (INPUT c-tabela AS CHAR):
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
   /*
   IF c-tabela = 'wt-etiquetas' THEN
       FIND ordem-benefic OF wt-etiquetas
            NO-LOCK NO-ERROR.
   ELSE 
       FIND ordem-benefic OF tt-etq-reservadas
             NO-LOCK NO-ERROR.

  IF AVAIL ordem-benefic THEN 
     RETURN ordem-benefic.tipo-tear.   /* Function return value. */
  ELSE  
  */
     RETURN "N∆o Informada".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

