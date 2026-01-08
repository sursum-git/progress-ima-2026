&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME w-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-digita 
/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESSP0149 2.04.00.000}
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: 
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Temp-Tables Definitions ---                                           */

DEF TEMP-TABLE tt-pedidos
    FIELD row-tt-itens    AS ROWID
    FIELD row-tt-negativo AS ROWID
    FIELD nr-pedcli       LIKE ped-venda.nr-pedcli
    FIELD nome-abrev      LIKE ped-venda.nome-abrev
    FIELD nr-sequencia    LIKE ped-item.nr-sequencia
    FIELD qt-pedida       LIKE ped-item.qt-pedida
    FIELD qt-reserva      LIKE ped-item.qt-pedida 
    INDEX indice1 IS PRIMARY nr-pedcli nr-sequencia row-tt-itens.

DEF TEMP-TABLE tt-itens
    FIELD it-codigo    LIKE ob-etiqueta.it-codigo
    FIELD cod-refer    LIKE ob-etiqueta.cod-refer
    FIELD lote         LIKE ob-etiqueta.nr-lote
    INDEX indice1 IS PRIMARY it-codigo cod-refer lote.

DEF TEMP-TABLE tt-etiquetas
    FIELD cod-estabel  LIKE ob-etiqueta.cod-estabel
    FIELD num-etiqueta LIKE ob-etiqueta.num-etiqueta
    FIELD quantidade   LIKE ob-etiqueta.quantidade
    FIELD it-codigo    LIKE ob-etiqueta.it-codigo
    FIELD cod-refer    LIKE ob-etiqueta.cod-refer
    FIELD nr-lote      LIKE ob-etiqueta.nr-lote
    FIELD nr-ob        LIKE ob-etiqueta.nr-ob
    FIELD nr-seq       LIKE ob-etiqueta.nr-seq.

DEF TEMP-TABLE tt-negativo
    FIELD row-tt-itens AS ROWID
    FIELD corte-comerc LIKE ob-etiqueta.corte-comerc
    FIELD qt-pedida    LIKE ped-item.qt-pedida      LABEL "Qt Pedida"
    FIELD qt-reserva   LIKE ped-item.qt-pedida      LABEL "Qt Reserva"
    FIELD qt-estoque   LIKE ob-etiqueta.quantidade  LABEL "Qt Estoq"
    FIELD qt-trf       LIKE ob-etiqueta.quantidade  LABEL "Qt Transf"
    FIELD qt-saldo     LIKE saldo-estoq.qtidade-atu LABEL "Qt Saldo"
    INDEX indice1 IS PRIMARY row-tt-itens corte-comerc.
    
DEF TEMP-TABLE tt-positivo LIKE tt-negativo
    FIELD row-tt-negativo AS ROWID
    FIELD lote            LIKE ob-etiqueta.nr-lote.

DEF TEMP-TABLE tt-etq-estoque
    FIELD row-tt-positivo AS ROWID
    FIELD num-etiqueta LIKE ob-etiqueta.num-etiqueta.

DEF BUFFER b-ped-item FOR ped-item.

/* Global Variable Definitions ---                                       */
DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.
DEF NEW GLOBAL SHARED VAR gr-ob-etiqueta AS ROWID NO-UNDO.
DEF NEW SHARED VAR i-num-trf AS INT.

/* Local Variable Definitions ---                                       */
DEF VAR rw-tt-itens   AS ROWID NO-UNDO.
DEF VAR h-acomp       AS HANDLE NO-UNDO.
DEF VAR h-query       AS HANDLE NO-UNDO.
DEF VAR c-dt-limite   AS CHAR FORMAT "99/9999".
DEF VAR c-dia         AS CHAR.
DEF VAR da-dt-entrega AS DATE.

DEF VAR c-cod-estabel AS CHAR.
DEF VAR c-it-codigo-ini    LIKE ped-item.it-codigo INIT '5'.
DEF VAR c-it-codigo-fin    LIKE ped-item.it-codigo INIT "5ZZZZZZZZZZZZZZZ".
DEF VAR c-cod-refer-ini    LIKE ped-item.cod-refer.
DEF VAR c-cod-refer-fin    LIKE ped-item.cod-refer INIT "ZZZZZZZZZZ".
DEF VAR c-cod-qualid-ini   LIKE ob-etiqueta.cod-qualid.
DEF VAR c-cod-qualid-fin   LIKE ob-etiqueta.cod-qualid     INIT "Z".
DEF VAR c-cod-obsoleto-ini AS CHAR.
DEF VAR c-cod-obsoleto-fin AS CHAR  INIT "Z".
DEF VAR c-cod-depos        LIKE saldo-estoq.cod-depos      INIT "EXP".
DEF VAR l-lote-todos       AS LOG INIT NO.
DEF VAR l-lote-pp          AS LOG INIT YES.
DEF VAR l-lote-pd          AS LOG INIT NO.
DEF VAR l-lote-rp          AS LOG INIT YES.
DEF VAR l-lote-rd          AS LOG INIT NO.
DEF VAR l-lote-sc          AS LOG INIT NO.
DEF VAR l-lote-ca          AS LOG INIT NO.
DEF VAR c-tp-artigo        AS CHAR INIT 'A'.
DEF VAR i-situacao         AS INT.
DEF VAR l-ok               AS LOG.
DEF VAR i-row              AS INT.
DEF VAR c-lotes            AS CHAR FORMAT "x(12)".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Digitacao
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-etiquetas

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-etq-estoque ob-etiqueta tt-itens ~
tt-pedidos item tt-negativo tt-positivo corte-comerc tt-etiquetas

/* Definitions for BROWSE br-etiquetas                                  */
&Scoped-define FIELDS-IN-QUERY-br-etiquetas ob-etiqueta.num-etiqueta ob-etiqueta.localizacao ob-etiqueta.nuance ob-etiqueta.quantidade   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-etiquetas   
&Scoped-define SELF-NAME br-etiquetas
&Scoped-define QUERY-STRING-br-etiquetas FOR EACH tt-etq-estoque WHERE                                  tt-etq-estoque.row-tt-positivo = ROWID(tt-positivo) NO-LOCK, ~
                                   EACH ob-etiqueta WHERE                                  ob-etiqueta.cod-estabel = c-cod-estabel AND                                  ob-etiqueta.num-etiqueta = tt-etq-estoque.num-etiqueta AND                                  ob-etiqueta.situacao = 3 AND                                  ob-etiqueta.localiz <> '700003' AND                                  ob-etiqueta.localiz <> '700004' NO-LOCK
&Scoped-define OPEN-QUERY-br-etiquetas OPEN QUERY {&SELF-NAME} FOR EACH tt-etq-estoque WHERE                                  tt-etq-estoque.row-tt-positivo = ROWID(tt-positivo) NO-LOCK, ~
                                   EACH ob-etiqueta WHERE                                  ob-etiqueta.cod-estabel = c-cod-estabel AND                                  ob-etiqueta.num-etiqueta = tt-etq-estoque.num-etiqueta AND                                  ob-etiqueta.situacao = 3 AND                                  ob-etiqueta.localiz <> '700003' AND                                  ob-etiqueta.localiz <> '700004' NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-etiquetas tt-etq-estoque ob-etiqueta
&Scoped-define FIRST-TABLE-IN-QUERY-br-etiquetas tt-etq-estoque
&Scoped-define SECOND-TABLE-IN-QUERY-br-etiquetas ob-etiqueta


/* Definitions for BROWSE br-itens                                      */
&Scoped-define FIELDS-IN-QUERY-br-itens tt-itens.it-codigo item.desc-item tt-itens.cod-refer tt-itens.lote   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-itens   
&Scoped-define SELF-NAME br-itens
&Scoped-define QUERY-STRING-br-itens FOR EACH tt-itens NO-LOCK, ~
                                   FIRST tt-pedidos WHERE                                   tt-pedidos.row-tt-itens = ROWID(tt-itens) NO-LOCK, ~
                                   FIRST item WHERE                                   item.it-codigo = tt-itens.it-codigo NO-LOCK
&Scoped-define OPEN-QUERY-br-itens OPEN QUERY {&SELF-NAME} FOR EACH tt-itens NO-LOCK, ~
                                   FIRST tt-pedidos WHERE                                   tt-pedidos.row-tt-itens = ROWID(tt-itens) NO-LOCK, ~
                                   FIRST item WHERE                                   item.it-codigo = tt-itens.it-codigo NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-itens tt-itens tt-pedidos item
&Scoped-define FIRST-TABLE-IN-QUERY-br-itens tt-itens
&Scoped-define SECOND-TABLE-IN-QUERY-br-itens tt-pedidos
&Scoped-define THIRD-TABLE-IN-QUERY-br-itens item


/* Definitions for BROWSE br-negativo                                   */
&Scoped-define FIELDS-IN-QUERY-br-negativo fn-corte-comerc() tt-negativo.qt-estoque tt-negativo.qt-pedida tt-negativo.qt-saldo   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-negativo   
&Scoped-define SELF-NAME br-negativo
&Scoped-define QUERY-STRING-br-negativo FOR EACH tt-negativo WHERE                                  tt-negativo.row-tt-itens =  ROWID(tt-itens) NO-LOCK                                  INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-negativo OPEN QUERY {&SELF-NAME} FOR EACH tt-negativo WHERE                                  tt-negativo.row-tt-itens =  ROWID(tt-itens) NO-LOCK                                  INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-negativo tt-negativo
&Scoped-define FIRST-TABLE-IN-QUERY-br-negativo tt-negativo


/* Definitions for BROWSE br-positivo                                   */
&Scoped-define FIELDS-IN-QUERY-br-positivo corte-comerc.descricao tt-positivo.lote tt-positivo.qt-estoque   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-positivo   
&Scoped-define SELF-NAME br-positivo
&Scoped-define QUERY-STRING-br-positivo FOR EACH tt-positivo WHERE                                  tt-positivo.row-tt-negativo = ROWID(tt-negativo)                                  NO-LOCK, ~
                                   FIRST corte-comerc WHERE                                   corte-comerc.codigo = tt-positivo.corte-comerc                                   NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-positivo OPEN QUERY {&SELF-NAME} FOR EACH tt-positivo WHERE                                  tt-positivo.row-tt-negativo = ROWID(tt-negativo)                                  NO-LOCK, ~
                                   FIRST corte-comerc WHERE                                   corte-comerc.codigo = tt-positivo.corte-comerc                                   NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-positivo tt-positivo corte-comerc
&Scoped-define FIRST-TABLE-IN-QUERY-br-positivo tt-positivo
&Scoped-define SECOND-TABLE-IN-QUERY-br-positivo corte-comerc


/* Definitions for BROWSE br-tt-etiquetas                               */
&Scoped-define FIELDS-IN-QUERY-br-tt-etiquetas tt-etiquetas.num-etiqueta tt-etiquetas.quantidade   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-tt-etiquetas   
&Scoped-define SELF-NAME br-tt-etiquetas
&Scoped-define QUERY-STRING-br-tt-etiquetas FOR EACH tt-etiquetas NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-tt-etiquetas OPEN QUERY {&SELF-NAME} FOR EACH tt-etiquetas NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-tt-etiquetas tt-etiquetas
&Scoped-define FIRST-TABLE-IN-QUERY-br-tt-etiquetas tt-etiquetas


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-br-etiquetas}~
    ~{&OPEN-QUERY-br-itens}~
    ~{&OPEN-QUERY-br-negativo}~
    ~{&OPEN-QUERY-br-positivo}~
    ~{&OPEN-QUERY-br-tt-etiquetas}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-3 RECT-6 br-itens br-negativo ~
br-positivo br-etiquetas br-tt-etiquetas bt-sel bt-add bt-del bt-cancelar 
&Scoped-Define DISPLAYED-OBJECTS fi-total-disp fi-total-sel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-corte-comerc w-digita 
FUNCTION fn-corte-comerc RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-digita AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-add 
     IMAGE-UP FILE "adeicon\next-au":U
     IMAGE-INSENSITIVE FILE "adeicon\next-ai":U
     LABEL "" 
     SIZE 5 BY 1.5.

DEFINE BUTTON bt-ajuda AUTO-GO 
     IMAGE-UP FILE "image/im-hel.bmp":U
     LABEL "OK" 
     SIZE 4.86 BY 1.13 TOOLTIP "Gera Transforma‡Æo".

DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     IMAGE-UP FILE "image/im-exi.bmp":U
     LABEL "Cancelar" 
     SIZE 4.86 BY 1.13.

DEFINE BUTTON bt-corte AUTO-GO 
     IMAGE-UP FILE "image/im-tes.bmp":U
     LABEL "OK" 
     SIZE 4.86 BY 1.13 TOOLTIP "Retira Cortes de Amostra".

DEFINE BUTTON bt-del 
     IMAGE-UP FILE "adeicon\prev-au":U
     IMAGE-INSENSITIVE FILE "adeicon\prev-ai":U
     LABEL "" 
     SIZE 5 BY 1.5.

DEFINE BUTTON bt-det 
     IMAGE-UP FILE "image/im-det.bmp":U
     LABEL "" 
     SIZE 4.86 BY 1.38 TOOLTIP "Detalha Etiqueta".

DEFINE BUTTON bt-ok AUTO-GO 
     IMAGE-UP FILE "image/im-trf.bmp":U
     LABEL "OK" 
     SIZE 4.86 BY 1.13 TOOLTIP "Gera Transforma‡Æo".

DEFINE BUTTON bt-pedidos 
     IMAGE-UP FILE "image/im-local.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-local.bmp":U
     LABEL "bt inclui 2" 
     SIZE 4.86 BY 1.13 TOOLTIP "Consulta/Seleciona Pedidos".

DEFINE BUTTON bt-sel 
     IMAGE-UP FILE "image/im-fil.bmp":U
     LABEL "Button 3" 
     SIZE 4.86 BY 1.13 TOOLTIP "Seleciona Itens".

DEFINE VARIABLE fi-total-disp AS DECIMAL FORMAT "-ZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11 BY .79
     FONT 2 NO-UNDO.

DEFINE VARIABLE fi-total-sel AS DECIMAL FORMAT "-ZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11 BY .79
     FONT 2 NO-UNDO.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 7 BY 8
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 75 BY 1.25
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-etiquetas FOR 
      tt-etq-estoque, 
      ob-etiqueta SCROLLING.

DEFINE QUERY br-itens FOR 
      tt-itens, 
      tt-pedidos, 
      item SCROLLING.

DEFINE QUERY br-negativo FOR 
      tt-negativo SCROLLING.

DEFINE QUERY br-positivo FOR 
      tt-positivo, 
      corte-comerc SCROLLING.

DEFINE QUERY br-tt-etiquetas FOR 
      tt-etiquetas SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-etiquetas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-etiquetas w-digita _FREEFORM
  QUERY br-etiquetas NO-LOCK DISPLAY
      ob-etiqueta.num-etiqueta COLUMN-LABEL "Etiqueta" FORMAT "999999999":U
      ob-etiqueta.localizacao FORMAT "999/999":U 
      ob-etiqueta.nuance FORMAT "X(2)":U 
      ob-etiqueta.quantidade COLUMN-LABEL "Qtde (m)" FORMAT ">>>,>>9.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 41 BY 6.5
         FONT 1
         TITLE "Pe‡as Dispon¡veis".

DEFINE BROWSE br-itens
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-itens w-digita _FREEFORM
  QUERY br-itens NO-LOCK DISPLAY
      tt-itens.it-codigo FORMAT "X(16)":U
      item.desc-item FORMAT "x(40)"
      tt-itens.cod-refer FORMAT "X(8)":U
      tt-itens.lote FORMAT "X(8)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 83 BY 4.5
         FONT 1.

DEFINE BROWSE br-negativo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-negativo w-digita _FREEFORM
  QUERY br-negativo NO-LOCK DISPLAY
      fn-corte-comerc()       FORMAT "x(15)"      COLUMN-LABEL "Corte Comercial"
      tt-negativo.qt-estoque  FORMAT ">>>,>>9.99"
      tt-negativo.qt-pedida   FORMAT ">>>>,>>9.99"
      tt-negativo.qt-saldo    FORMAT "->,>>>,>>9.99" COLUMN-LABEL "Saldo" WIDTH 9
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 41 BY 6
         FONT 1
         TITLE "Negativo".

DEFINE BROWSE br-positivo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-positivo w-digita _FREEFORM
  QUERY br-positivo NO-LOCK DISPLAY
      corte-comerc.descricao COLUMN-LABEL "Corte Comercial"
     tt-positivo.lote       COLUMN-LABEL "Lote" 
     tt-positivo.qt-estoque COLUMN-LABEL "Qt Estoque"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 36 BY 6
         FONT 1
         TITLE "Positivo".

DEFINE BROWSE br-tt-etiquetas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-tt-etiquetas w-digita _FREEFORM
  QUERY br-tt-etiquetas NO-LOCK DISPLAY
      tt-etiquetas.num-etiqueta FORMAT "999999999":U COLUMN-LABEL "Etiqueta"
      tt-etiquetas.quantidade FORMAT ">>>,>>9.99":U COLUMN-LABEL "Qtde (m)" WIDTH 10
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS NO-SCROLLBAR-VERTICAL SIZE 28 BY 6.5
         FONT 1
         TITLE "Pe‡as Selecionadas".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br-itens AT ROW 1.25 COL 2
     br-negativo AT ROW 6.25 COL 2
     br-positivo AT ROW 6.25 COL 49
     br-etiquetas AT ROW 12.75 COL 2
     br-tt-etiquetas AT ROW 12.75 COL 49
     bt-sel AT ROW 13 COL 79.14
     bt-det AT ROW 14 COL 43.57
     bt-pedidos AT ROW 14.17 COL 79.14
     bt-corte AT ROW 15.33 COL 79.14
     bt-add AT ROW 15.58 COL 43.43
     bt-ok AT ROW 16.5 COL 79.14
     bt-del AT ROW 17.33 COL 43.43
     bt-cancelar AT ROW 18.33 COL 79.14
     bt-ajuda AT ROW 19.5 COL 79.14
     fi-total-disp AT ROW 19.75 COL 25 COLON-ALIGNED NO-LABEL
     fi-total-sel AT ROW 19.75 COL 59.86 COLON-ALIGNED NO-LABEL
     "Selecionado:" VIEW-AS TEXT
          SIZE 10.72 BY .54 AT ROW 19.88 COL 50.72
          BGCOLOR 8 FONT 6
     "Dispon¡vel:" VIEW-AS TEXT
          SIZE 9.29 BY .54 AT ROW 19.88 COL 17.29
          BGCOLOR 8 FONT 6
     RECT-3 AT ROW 12.75 COL 78
     RECT-6 AT ROW 19.5 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 92.86 BY 22.79
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Digitacao
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
   Design Page: 1
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-digita ASSIGN
         HIDDEN             = YES
         TITLE              = "An lise de Transforma‡Æo"
         COLUMN             = 20.86
         ROW                = 8.08
         HEIGHT             = 19.83
         WIDTH              = 84.57
         MAX-HEIGHT         = 22.92
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 22.92
         VIRTUAL-WIDTH      = 114.29
         RESIZE             = no
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-digita 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-digit.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-digita
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
/* BROWSE-TAB br-itens RECT-6 F-Main */
/* BROWSE-TAB br-negativo br-itens F-Main */
/* BROWSE-TAB br-positivo br-negativo F-Main */
/* BROWSE-TAB br-etiquetas br-positivo F-Main */
/* BROWSE-TAB br-tt-etiquetas br-etiquetas F-Main */
/* SETTINGS FOR BUTTON bt-ajuda IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-corte IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-det IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-ok IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-pedidos IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-total-disp IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-total-sel IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-digita)
THEN w-digita:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-etiquetas
/* Query rebuild information for BROWSE br-etiquetas
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-etq-estoque WHERE
                                 tt-etq-estoque.row-tt-positivo = ROWID(tt-positivo) NO-LOCK,
                            EACH ob-etiqueta WHERE
                                 ob-etiqueta.cod-estabel = c-cod-estabel AND
                                 ob-etiqueta.num-etiqueta = tt-etq-estoque.num-etiqueta AND
                                 ob-etiqueta.situacao = 3 AND
                                 ob-etiqueta.localiz <> '700003' AND
                                 ob-etiqueta.localiz <> '700004' NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "espec.ob-etiqueta.situacao = 3 AND
espec.ob-etiqueta.it-codigo = tt-itens.it-codigo AND
espec.ob-etiqueta.cod-refer = tt-itens.cod-refer AND
espec.ob-etiqueta.nr-lote = tt-positivo.lote AND
espec.ob-etiqueta.corte-comerc = tt-positivo.corte-comerc"
     _Query            is OPENED
*/  /* BROWSE br-etiquetas */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-itens
/* Query rebuild information for BROWSE br-itens
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-itens NO-LOCK,
                            FIRST tt-pedidos WHERE
                                  tt-pedidos.row-tt-itens = ROWID(tt-itens) NO-LOCK,
                            FIRST item WHERE
                                  item.it-codigo = tt-itens.it-codigo NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-itens */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-negativo
/* Query rebuild information for BROWSE br-negativo
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-negativo WHERE
                                 tt-negativo.row-tt-itens =  ROWID(tt-itens) NO-LOCK
                                 INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-negativo */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-positivo
/* Query rebuild information for BROWSE br-positivo
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-positivo WHERE
                                 tt-positivo.row-tt-negativo = ROWID(tt-negativo)
                                 NO-LOCK,
                            FIRST corte-comerc WHERE
                                  corte-comerc.codigo = tt-positivo.corte-comerc
                                  NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-positivo */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-tt-etiquetas
/* Query rebuild information for BROWSE br-tt-etiquetas
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-etiquetas NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-tt-etiquetas */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-digita w-digita
ON END-ERROR OF w-digita /* An lise de Transforma‡Æo */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-digita w-digita
ON WINDOW-CLOSE OF w-digita /* An lise de Transforma‡Æo */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-etiquetas
&Scoped-define SELF-NAME br-etiquetas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-etiquetas w-digita
ON VALUE-CHANGED OF br-etiquetas IN FRAME F-Main /* Pe‡as Dispon¡veis */
DO:
   ASSIGN bt-det:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-itens
&Scoped-define SELF-NAME br-itens
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-itens w-digita
ON VALUE-CHANGED OF br-itens IN FRAME F-Main
DO:
   {&OPEN-QUERY-br-negativo}
   APPLY 'value-changed' TO br-negativo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-negativo
&Scoped-define SELF-NAME br-negativo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-negativo w-digita
ON VALUE-CHANGED OF br-negativo IN FRAME F-Main /* Negativo */
DO:
  {&OPEN-QUERY-br-positivo}
  APPLY 'VALUE-CHANGED' TO br-positivo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-positivo
&Scoped-define SELF-NAME br-positivo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-positivo w-digita
ON VALUE-CHANGED OF br-positivo IN FRAME F-Main /* Positivo */
DO:
  RUN pi-mata-sel.
  RUN pi-soma-disp.
  {&OPEN-QUERY-br-etiquetas}
  APPLY 'VALUE-CHANGED' TO br-etiquetas.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-add w-digita
ON CHOOSE OF bt-add IN FRAME F-Main
DO:
   DO i-row = 1 TO br-etiquetas:NUM-SELECTED-ROWS:
      IF br-etiquetas:FETCH-SELECTED-ROW(i-row) THEN DO.
         FIND tt-etiquetas WHERE
              tt-etiquetas.cod-estabel  = ob-etiqueta.cod-estabel AND
              tt-etiquetas.num-etiqueta = ob-etiqueta.num-etiqueta
              NO-ERROR.
         IF NOT AVAIL tt-etiquetas THEN DO.
            CREATE tt-etiquetas.
            ASSIGN tt-etiquetas.cod-estabel  = ob-etiqueta.cod-estabel
                   tt-etiquetas.num-etiqueta = ob-etiqueta.num-etiqueta
                   tt-etiquetas.quantidade   = ob-etiqueta.quantidade.
         END.
      END.

      ASSIGN bt-corte:SENSITIVE IN FRAME {&FRAME-NAME} = YES
             bt-ok:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
   END.
   RUN pi-soma-sel.
   {&OPEN-QUERY-br-tt-etiquetas}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancelar w-digita
ON CHOOSE OF bt-cancelar IN FRAME F-Main /* Cancelar */
DO:
  apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-corte
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-corte w-digita
ON CHOOSE OF bt-corte IN FRAME F-Main /* OK */
DO:
   FIND ob-etiqueta WHERE
        ob-etiqueta.cod-estabel  = tt-etiquetas.cod-estabel AND
        ob-etiqueta.num-etiqueta = tt-etiquetas.num-etiqueta
        NO-ERROR.

   IF AVAIL ob-etiqueta THEN DO.
      ASSIGN i-situacao = ob-etiqueta.situacao.
      ASSIGN gr-ob-etiqueta = ROWID(ob-etiqueta).
      RUN esp/essp0145.p.

      FIND CURRENT ob-etiqueta SHARE-LOCK NO-ERROR.
      IF ob-etiqueta.situacao <> 7 THEN
         ASSIGN ob-etiqueta.situacao = i-situacao.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-del
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-del w-digita
ON CHOOSE OF bt-del IN FRAME F-Main
DO:
   DO i-row = 1 TO br-tt-etiquetas:NUM-SELECTED-ROWS:
      IF br-tt-etiquetas:FETCH-SELECTED-ROW(i-row) THEN DO.
         DELETE tt-etiquetas.
      END.
   END.

   FIND FIRST tt-etiquetas NO-ERROR.
   IF NOT AVAIL tt-etiquetas THEN
      ASSIGN bt-corte:SENSITIVE IN FRAME {&FRAME-NAME} = NO
             bt-ok:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
   RUN pi-soma-sel.
   {&OPEN-QUERY-br-tt-etiquetas}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-det
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-det w-digita
ON CHOOSE OF bt-det IN FRAME F-Main
DO:
   ASSIGN gr-ob-etiqueta = ROWID(ob-etiqueta).
   RUN esp/essp0146.p.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-digita
ON CHOOSE OF bt-ok IN FRAME F-Main /* OK */
DO:
  FIND FIRST tt-etiquetas NO-LOCK NO-ERROR.
  IF AVAIL tt-etiquetas THEN DO.
     CREATE ob-trf.
     ASSIGN ob-trf.num-trf = NEXT-VALUE(seq-trf)
            ob-trf.dt-solic = TODAY
            ob-trf.it-codigo = tt-itens.it-codigo
            ob-trf.cod-refer = tt-itens.cod-refer
            ob-trf.nr-lote = tt-itens.lote
            ob-trf.corte-comerc = tt-negativo.corte-comerc
            ob-trf.usuario = c-seg-usuario
            SUBSTR(ob-trf.char-1,100,1) = tt-etiquetas.cod-estabel.

     FOR EACH tt-etiquetas.
         CREATE ob-etq-trf.
         ASSIGN ob-etq-trf.num-trf      = ob-trf.num-trf
                ob-etq-trf.cod-estabel  = tt-etiquetas.cod-estabel
                ob-etq-trf.num-etiqueta = tt-etiquetas.num-etiqueta.

         FIND ob-etiqueta WHERE
              ob-etiqueta.cod-estabel  = tt-etiquetas.cod-estabel AND
              ob-etiqueta.num-etiqueta = tt-etiquetas.num-etiqueta NO-ERROR.

         ASSIGN ob-etiqueta.situacao = 6.
         DELETE tt-etiquetas.
     END.
     {&OPEN-QUERY-br-tt-etiquetas}
     
     ASSIGN i-num-trf = ob-trf.num-trf.
     RUN esp/essp0153.w "shared".
  END.
  DELETE tt-negativo.

  IF NOT CAN-FIND(FIRST tt-negativo WHERE
                        tt-negativo.row-tt-itens = ROWID(tt-itens)) THEN DO.

      FOR EACH tt-pedidos WHERE
               tt-pedidos.row-tt-itens = ROWID(tt-itens) EXCLUSIVE-LOCK.
          DELETE tt-pedidos.
      END.
      DELETE tt-itens.
  END.

  IF NOT AVAIL tt-itens THEN DO.
     ASSIGN rw-tt-itens = ?.
     h-query:GET-NEXT(NO-LOCK).
     IF h-query:QUERY-OFF-END THEN DO.
        h-query:GET-LAST(NO-LOCK).
        h-query:GET-PREV(NO-LOCK).
     END.
     IF NOT h-query:QUERY-OFF-END THEN 
        ASSIGN rw-tt-itens = ROWID(tt-itens).
  END.
  ELSE
     ASSIGN rw-tt-itens = ROWID(tt-itens).

  {&OPEN-QUERY-br-itens}

   h-query:REPOSITION-TO-ROWID(rw-tt-itens) NO-ERROR.
   APPLY 'value-changed' TO br-itens IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-pedidos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-pedidos w-digita
ON CHOOSE OF bt-pedidos IN FRAME F-Main /* bt inclui 2 */
DO:
   
   RUN esp/essp0149b.p (INPUT TABLE tt-pedidos,
                        INPUT ROWID(tt-negativo)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-sel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-sel w-digita
ON CHOOSE OF bt-sel IN FRAME F-Main /* Button 3 */
DO:
   RUN esapi/busca-estab-usuar.p (INPUT c-seg-usuario,
                                  OUTPUT c-cod-estabel).

   ASSIGN w-digita:SENSITIVE = NO.
   RUN esp/essp0149a.w (INPUT-OUTPUT c-cod-estabel,
                        INPUT-OUTPUT c-dt-limite, 
                        INPUT-OUTPUT c-it-codigo-ini,
                        INPUT-OUTPUT c-it-codigo-fin,
                        INPUT-OUTPUT c-cod-refer-ini,
                        INPUT-OUTPUT c-cod-refer-fin,
                        INPUT-OUTPUT c-cod-qualid-ini,  
                        INPUT-OUTPUT c-cod-qualid-fin,  
                        INPUT-OUTPUT c-cod-obsoleto-ini,
                        INPUT-OUTPUT c-cod-obsoleto-fin,
                        INPUT-OUTPUT c-cod-depos,  
                        INPUT-OUTPUT c-tp-artigo,
                        INPUT-OUTPUT l-lote-todos, 
                        INPUT-OUTPUT l-lote-pp,         
                        INPUT-OUTPUT l-lote-pd,         
                        INPUT-OUTPUT l-lote-rp,         
                        INPUT-OUTPUT l-lote-rd, 
                        INPUT-OUTPUT l-lote-sc,
                        INPUT-OUTPUT l-lote-ca,
                        INPUT-OUTPUT l-ok).

   IF l-ok THEN DO.
      FOR EACH tt-pedidos.
          DELETE tt-pedidos.
      END.
      FOR EACH tt-itens.
          DELETE tt-itens.
      END.

      FOR EACH tt-etiquetas.
          DELETE tt-etiquetas.
      END.

      FOR EACH tt-positivo.
          DELETE tt-positivo.
      END.
      FOR EACH tt-negativo.
          DELETE tt-negativo.
      END.

      FOR EACH tt-etq-estoque.
          DELETE tt-etq-estoque.
      END.

      RUN pi-processa.
   END.
   ASSIGN w-digita:SENSITIVE = YES.
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-etiquetas
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-digita 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-digita  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  /* Select a Startup page. */
  IF adm-current-page eq 0 
  THEN RUN select-page IN THIS-PROCEDURE ( 1 ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-digita  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-digita  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-digita)
  THEN DELETE WIDGET w-digita.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-digita  _DEFAULT-ENABLE
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
  DISPLAY fi-total-disp fi-total-sel 
      WITH FRAME F-Main IN WINDOW w-digita.
  ENABLE RECT-3 RECT-6 br-itens br-negativo br-positivo br-etiquetas 
         br-tt-etiquetas bt-sel bt-add bt-del bt-cancelar 
      WITH FRAME F-Main IN WINDOW w-digita.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW w-digita.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-digita 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-digita 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-digita 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  {utp/ut9000.i "ESSP0149" "2.04.00.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN h-query = br-itens:QUERY IN FRAME {&FRAME-NAME}.


  RUN esapi/busca-estab-usuar.p (INPUT c-seg-usuario,
                                 OUTPUT c-cod-estabel).
  IF c-cod-estabel = '' OR c-cod-estabel = '0' THEN 
     ASSIGN c-cod-estabel = '1'.


  ASSIGN c-dt-limite = STRING(MONTH(TODAY),'99') + STRING(YEAR(TODAY),'9999').
  APPLY 'choose' TO bt-sel IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-mata-sel w-digita 
PROCEDURE pi-mata-sel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  FOR EACH tt-etiquetas.
      DELETE tt-etiquetas.
  END.
  {&OPEN-QUERY-br-tt-etiquetas}
  ASSIGN fi-total-sel = 0.
  DISP fi-total-sel WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-processa w-digita 
PROCEDURE pi-processa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
{utp/ut-liter.i Lendo_Carteira *}
RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

RUN esapi/ret-udm.p (INPUT c-dt-limite, OUTPUT c-dia).
ASSIGN da-dt-entrega = DATE(c-dia + SUBSTR(c-dt-limite,1,2) + SUBSTR(c-dt-limite,3,4)).

ASSIGN c-lotes = "".
IF l-lote-todos = YES THEN
   ASSIGN c-lotes = "pp pd rp rd sc ca ".
ELSE DO:
   ASSIGN c-lotes = c-lotes + IF l-lote-pp = YES THEN "pp " ELSE "   ".
   ASSIGN c-lotes = c-lotes + IF l-lote-pd = YES THEN "pd " ELSE "   ".
   ASSIGN c-lotes = c-lotes + IF l-lote-rp = YES THEN "rp " ELSE "   ".
   ASSIGN c-lotes = c-lotes + IF l-lote-rd = YES THEN "rd " ELSE "   ".
   ASSIGN c-lotes = c-lotes + IF l-lote-sc = YES THEN "sc " ELSE "   ".
   ASSIGN c-lotes = c-lotes + IF l-lote-ca = YES THEN "ca " ELSE "   ".
END.

FOR EACH ped-item WHERE        
         ped-item.cod-sit-item = 1 AND
         ped-item.it-codigo >= c-it-codigo-ini AND
         ped-item.it-codigo <= c-it-codigo-fin AND
         ped-item.cod-refer >= c-cod-refer-ini AND 
         ped-item.cod-refer <= c-cod-refer-fin NO-LOCK,
    FIRST ped-item-ext OF ped-item WHERE
         INDEX(c-lotes,SUBSTR(ped-item-ext.lote,1,2)) <> 0 NO-LOCK.

    FIND item-ext WHERE
         item-ext.it-codigo = ped-item.it-codigo NO-LOCK NO-ERROR.

    /*IF c-tp-artigo <> 'A' THEN 
       IF AVAIL item-ext AND
          (item-ext.indigo = YES AND c-tp-artigo <> "I") OR
          (item-ext.indigo = NO  AND c-tp-artigo <> "O") THEN NEXT.*/

    FIND ped-venda OF ped-item NO-LOCK NO-ERROR.

    IF ped-venda.cod-estab <> c-cod-estabel THEN NEXT.

    IF ped-venda.dt-entrega > da-dt-entrega THEN NEXT.

    RUN pi-acompanhar IN h-acomp (INPUT "Pedido: " + ped-item.nr-pedcli +
                                        "  Item: " + ped-item.it-codigo +
                                        "   Ref: " + ped-item.cod-refer).

    FIND tt-itens WHERE
         tt-itens.it-codigo = ped-item.it-codigo AND
         tt-itens.cod-refer = ped-item.cod-refer AND
         tt-itens.lote      = SUBSTR(ped-item-ext.lote,1,2)
         NO-LOCK NO-ERROR.

    IF NOT AVAIL tt-itens THEN DO.
       CREATE tt-itens.
       ASSIGN tt-itens.it-codigo = ped-item.it-codigo 
              tt-itens.cod-refer = ped-item.cod-refer
              tt-itens.lote      = UPPER(SUBSTR(ped-item-ext.lote,1,2)).
    END.

    FIND tt-negativo WHERE
         tt-negativo.row-tt-itens = ROWID(tt-itens) AND
         tt-negativo.corte-comerc = ped-item-ext.corte-comerc
         NO-LOCK NO-ERROR.

    IF NOT AVAIL tt-negativo THEN DO.

       CREATE tt-negativo.
       ASSIGN tt-negativo.row-tt-itens = ROWID(tt-itens)
              tt-negativo.corte-comerc = ped-item-ext.corte-comerc.

       FOR EACH ob-etiqueta WHERE
                ob-etiqueta.cod-estabel = c-cod-estabel AND
                (ob-etiqueta.situacao = 3 OR ob-etiqueta.situacao = 4) AND
                ob-etiqueta.it-codigo = ped-item.it-codigo AND
                ob-etiqueta.cod-refer = ped-item.cod-refer AND
                ob-etiqueta.nr-lote = tt-itens.lote AND 
                ob-etiqueta.corte-comerc = ped-item-ext.corte-comerc NO-LOCK.

           IF ob-etiqueta.localizacao = '' THEN NEXT.

           IF ob-etiqueta.situacao = 3 THEN
              ASSIGN tt-negativo.qt-estoque = tt-negativo.qt-estoque + ob-etiqueta.quantidade.
           ELSE DO.
              FIND ped-item-rom WHERE
                   ped-item-rom.cod-estabel  = ob-etiqueta.cod-estabel AND
                   ped-item-rom.num-etiqueta = ob-etiqueta.num-etiqueta
                   NO-LOCK NO-ERROR.

              IF NOT AVAIL ped-item-rom THEN NEXT.

              FIND b-ped-item WHERE       
                   b-ped-item.nr-pedcli = ped-item-rom.nr-pedcli AND
                   b-ped-item.nome-abrev = ped-item-rom.nome-abrev AND
                   b-ped-item.nr-sequencia = ped-item-rom.nr-sequencia AND
                   b-ped-item.cod-sit-item = 1 NO-LOCK NO-ERROR.

              IF NOT AVAIL b-ped-item THEN NEXT.

              FIND tt-pedidos WHERE
                   tt-pedidos.row-tt-negativo = ROWID(tt-negativo) AND
                   tt-pedidos.nr-pedcli       = ped-item-rom.nr-pedcli AND
                   tt-pedidos.nr-sequencia    = ped-item-rom.nr-sequencia NO-ERROR.
              IF NOT AVAIL tt-pedidos THEN DO.
                 CREATE tt-pedidos.
                 ASSIGN tt-pedidos.row-tt-negativo = ROWID(tt-negativo) 
                        tt-pedidos.row-tt-itens    = ROWID(tt-itens)
                        tt-pedidos.nr-sequencia    = ped-item-rom.nr-sequencia
                        tt-pedidos.nr-pedcli       = ped-item-rom.nr-pedcli
                        tt-pedidos.nome-abrev      = ped-item-rom.nome-abrev.
              END.
              ASSIGN tt-pedidos.qt-reserva = tt-pedidos.qt-reserva + ob-etiqueta.quantidade
                     tt-negativo.qt-reserva = tt-negativo.qt-reserva + ob-etiqueta.quantidade.
           END.
           ASSIGN tt-negativo.qt-saldo = tt-negativo.qt-saldo + ob-etiqueta.quantidade.
       END.
       
       FOR EACH ob-trf WHERE
                ob-trf.situacao = 1 AND
                ob-trf.it-codigo = tt-itens.it-codigo AND
                ob-trf.cod-refer = tt-itens.cod-refer AND
                ob-trf.nr-lote = tt-itens.lote AND 
                ob-trf.corte-comerc = tt-negativo.corte-comerc NO-LOCK,
           EACH ob-etq-trf OF ob-trf WHERE
                ob-etq-trf.cod-estabel = c-cod-estabel NO-LOCK.

           FIND ob-etiqueta WHERE
                ob-etiqueta.cod-estabel  = ob-etq-trf.cod-estabel AND
                ob-etiqueta.num-etiqueta = ob-etq-trf.num-etiqueta NO-LOCK NO-ERROR.

           ASSIGN tt-negativo.qt-trf = tt-negativo.qt-trf + ob-etiqueta.quantidade.
       END.
       ASSIGN tt-negativo.qt-saldo = tt-negativo.qt-saldo + tt-negativo.qt-trf.

    END. 
    ASSIGN tt-negativo.qt-pedida = tt-negativo.qt-pedida + ped-item.qt-pedida.

    ASSIGN tt-negativo.qt-saldo = tt-negativo.qt-saldo - ped-item.qt-pedida.

    FIND tt-pedidos WHERE
         tt-pedidos.row-tt-negativo = ROWID(tt-negativo) AND
         tt-pedidos.nr-pedcli       = ped-item.nr-pedcli AND 
         tt-pedidos.nr-sequencia    = ped-item.nr-sequencia  NO-ERROR.
    IF NOT AVAIL tt-pedidos THEN DO.
       CREATE tt-pedidos.
       ASSIGN tt-pedidos.row-tt-negativo = ROWID(tt-negativo) 
              tt-pedidos.row-tt-itens    = ROWID(tt-itens)
              tt-pedidos.nr-pedcli       = ped-item.nr-pedcli
              tt-pedidos.nome-abrev      = ped-item.nome-abrev
              tt-pedidos.nr-sequencia    = ped-item.nr-sequencia.
    END.
    ASSIGN tt-pedidos.qt-pedida = tt-pedidos.qt-pedida + ped-item.qt-pedida.
END.

                 
{utp/ut-liter.i Calculando_Positivo *}
RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

FOR EACH tt-pedidos.
    IF tt-pedidos.qt-pedida - tt-pedidos.qt-reserva <= 0 THEN
       DELETE tt-pedidos.
END.

FOR EACH tt-negativo EXCLUSIVE-LOCK.
    FIND tt-itens WHERE
         ROWID(tt-itens) = tt-negativo.row-tt-itens
         NO-LOCK NO-ERROR.

    RUN pi-acompanhar IN h-acomp (INPUT "Item: " + tt-itens.it-codigo +
                                        " Ref: " + tt-itens.cod-refer).

    ASSIGN tt-negativo.qt-pedida = tt-negativo.qt-pedida - tt-negativo.qt-reserva.

    IF tt-negativo.qt-saldo < 0 THEN DO.
       FIND corte-comerc WHERE
            corte-comerc.codigo = tt-negativo.corte-comerc NO-ERROR.

       FOR EACH ob-etiqueta WHERE
                ob-etiqueta.cod-estabel = c-cod-estabel AND
                ob-etiqueta.situacao = 3 AND
                ob-etiqueta.it-codigo = tt-itens.it-codigo AND
                ob-etiqueta.cod-refer = tt-itens.cod-refer NO-LOCK.

           IF ob-etiqueta.localizacao = '' THEN NEXT.

           IF LOOKUP(ob-etiqueta.corte-comerc,corte-comerc.corte-origem) = 0 THEN NEXT.

           IF corte-comerc.tp-embalag = 2 THEN DO.
              IF LOOKUP(SUBSTR(ob-etiqueta.nr-lote,1,1),"R,P") = 0 THEN NEXT.
              IF SUBSTR(ob-etiqueta.nr-lote,2,1) <> SUBSTR(tt-itens.lote,2,1) THEN NEXT.
           END.
           ELSE IF ob-etiqueta.nr-lote <> tt-itens.lote THEN NEXT.

           FIND tt-positivo WHERE
                tt-positivo.row-tt-negativo = ROWID(tt-negativo) AND 
                tt-positivo.lote = ob-etiqueta.nr-lote AND
                tt-positivo.corte-comerc = ob-etiqueta.corte-comerc
                NO-LOCK NO-ERROR.
           IF NOT AVAIL tt-positivo THEN DO.
              CREATE tt-positivo.
              ASSIGN tt-positivo.row-tt-negativo = ROWID(tt-negativo) 
                     tt-positivo.row-tt-itens = ROWID(tt-itens)
                     tt-positivo.corte-comerc = ob-etiqueta.corte-comerc
                     tt-positivo.lote = ob-etiqueta.nr-lote.
           END.
           ASSIGN tt-positivo.qt-estoque = tt-positivo.qt-estoque + ob-etiqueta.quantidade.

           CREATE tt-etq-estoque.
           ASSIGN tt-etq-estoque.row-tt-positivo = ROWID(tt-positivo)
                  tt-etq-estoque.num-etiqueta = ob-etiqueta.num-etiqueta.
       END.
    END.
END.

{utp/ut-liter.i Descontando_Carteira *}
RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

FOR EACH tt-positivo EXCLUSIVE-LOCK.
    FIND tt-itens WHERE
         ROWID(tt-itens) = tt-positivo.row-tt-itens NO-ERROR.

    RUN pi-acompanhar IN h-acomp (INPUT "Item: " + tt-itens.it-codigo + 
                                        " Ref: " + tt-itens.cod-refer).
    
    FIND corte-comerc WHERE
         corte-comerc.codigo = tt-positivo.corte-comerc 
         NO-LOCK NO-ERROR.

    FOR EACH ped-item WHERE
             ped-item.it-codigo = tt-itens.it-codigo AND
             ped-item.cod-refer = tt-itens.cod-refer AND
             ped-item.cod-sit-item = 1 NO-LOCK USE-INDEX ch-correcao,
        EACH ped-item-ext OF ped-item WHERE
             SUBSTR(ped-item-ext.lote,1,2) = tt-positivo.lote AND
             ped-item-ext.corte-comerc = tt-positivo.corte-comerc NO-LOCK.

        FIND ped-item-res OF ped-item NO-LOCK NO-ERROR.
        IF AVAIL ped-item-res THEN NEXT.
                                  
        ASSIGN tt-positivo.qt-estoque = tt-positivo.qt-estoque - ped-item.qt-pedida.
    END.
    
    IF tt-positivo.qt-estoque <= 0 OR
       ABS(tt-positivo.qt-estoque) < corte-comerc.compr-min THEN
       DELETE tt-positivo.
END.


FOR EACH tt-negativo EXCLUSIVE-LOCK.
    IF NOT CAN-FIND(FIRST tt-positivo WHERE
                          tt-positivo.row-tt-itens = tt-negativo.row-tt-itens) THEN DO.
       FIND tt-itens WHERE 
            ROWID(tt-itens) = tt-negativo.row-tt-itens NO-ERROR.

       FOR EACH tt-pedidos WHERE
                tt-pedidos.row-tt-itens = rowid(tt-itens) NO-LOCK.
           DELETE tt-pedidos.
       END.

       IF AVAIL tt-itens THEN
          DELETE tt-itens.
    END.

    IF NOT CAN-FIND(FIRST tt-positivo WHERE
                          tt-positivo.row-tt-negativo = ROWID(tt-negativo)) THEN DO.
       DELETE tt-negativo.
    END. 
END.

RUN pi-finalizar in h-acomp.

{&OPEN-QUERY-br-itens}

IF CAN-FIND(FIRST tt-pedidos) THEN
   ASSIGN bt-pedidos:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

APPLY 'value-changed' TO br-itens IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-soma-disp w-digita 
PROCEDURE pi-soma-disp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

 ASSIGN fi-total-disp = 0.
 FOR EACH ob-etiqueta WHERE ob-etiqueta.situacao     = 3                  AND
                            ob-etiqueta.it-codigo    = tt-itens.it-codigo AND
                            ob-etiqueta.cod-refer    = tt-itens.cod-refer AND
                            ob-etiqueta.nr-lote      = tt-positivo.lote   AND
                            ob-etiqueta.corte-comerc = tt-positivo.corte-comerc
                            NO-LOCK.

     IF ob-etiqueta.localiz = '' THEN NEXT.

     ASSIGN fi-total-disp =  fi-total-disp + ob-etiqueta.quantidade.
 END.
 DISP fi-total-disp WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-soma-sel w-digita 
PROCEDURE pi-soma-sel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

 ASSIGN fi-total-sel = 0.
 FOR EACH tt-etiquetas NO-LOCK.
     ASSIGN fi-total-sel =  fi-total-sel + tt-etiquetas.quantidade.
 END.
 DISP fi-total-sel WITH FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-digita  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-etiquetas"}
  {src/adm/template/snd-list.i "tt-positivo"}
  {src/adm/template/snd-list.i "corte-comerc"}
  {src/adm/template/snd-list.i "tt-negativo"}
  {src/adm/template/snd-list.i "tt-itens"}
  {src/adm/template/snd-list.i "tt-pedidos"}
  {src/adm/template/snd-list.i "item"}
  {src/adm/template/snd-list.i "tt-etq-estoque"}
  {src/adm/template/snd-list.i "ob-etiqueta"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-digita 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-corte-comerc w-digita 
FUNCTION fn-corte-comerc RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND corte-comerc WHERE
       corte-comerc.codigo = tt-negativo.corte-comerc NO-LOCK NO-ERROR.

  IF AVAIL corte-comerc THEN
     RETURN corte-comerc.descricao.   /* Function return value. */
  ELSE
     RETURN ''.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

