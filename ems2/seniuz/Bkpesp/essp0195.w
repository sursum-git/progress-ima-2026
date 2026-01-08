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
{include/i-prgvrs.i ESSP0195 2.04.00.000}

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
/* Temp-Tables Definitions ---                                          */

DEF TEMP-TABLE tt-digita
    FIELD opcao  AS CHAR
    FIELD campo AS CHAR
    FIELD valor AS CHAR.

DEF TEMP-TABLE tt-itens
    FIELD it-codigo    LIKE ob-etiqueta.it-codigo
    FIELD cod-refer    LIKE ob-etiqueta.cod-refer
    FIELD lote         LIKE ob-etiqueta.nr-lote
    FIELD corte-comerc LIKE corte-comerc.descricao
    FIELD estoque      AS DEC
    FIELD cod-corte-comerc LIKE corte-comerc.codigo
    INDEX indice1 IS PRIMARY it-codigo cod-refer lote.

DEF TEMP-TABLE tt-etiquetas LIKE ob-etiqueta
    FIELD corte-coml LIKE corte-comerc.descricao.

DEF TEMP-TABLE tt-historico
    FIELD dt-manut AS CHAR FORMAT "x(10)"
    FIELD usuario  AS CHAR FORMAT "x(12)"
    FIELD it-codigo    LIKE ob-etiqueta.it-codigo
    FIELD cod-refer    LIKE ob-etiqueta.cod-refer
    FIELD lote         LIKE ob-etiqueta.nr-lote
    FIELD corte-comerc LIKE corte-comerc.codigo.

DEF TEMP-TABLE tt-det-hist
    FIELD dt-manut     AS CHAR FORMAT "x(10)"
    FIELD usuario      AS CHAR FORMAT "x(12)"
    FIELD cod-estabel  AS CHAR
    FIELD num-etiqueta LIKE ob-etiqueta.num-etiqueta
    FIELD localiz-ant  LIKE ob-etiqueta.localizacao
    FIELD it-codigo    LIKE ob-etiqueta.it-codigo
    FIELD cod-refer    LIKE ob-etiqueta.cod-refer
    FIELD lote         LIKE ob-etiqueta.nr-lote
    FIELD corte-comerc LIKE corte-comerc.codigo.

DEF TEMP-TABLE tt-localiz LIKE ob-etiqueta
    FIELD corte-coml  LIKE corte-comerc.descricao
    FIELD localiz-ant LIKE ob-etiqueta.localizacao
    FIELD marca       AS CHAR.

/* Global Variable Definitions ---                                       */
DEF NEW GLOBAL SHARED VAR gr-ob-etiqueta AS ROWID NO-UNDO.

DEF BUFFER b-tt-etiquetas FOR tt-etiquetas.
DEF BUFFER b-tt-localiz   FOR tt-localiz.
DEFINE BUFFER empresa FOR mgcad.empresa.
/* Local Variable Definitions ---                                       */
DEF VAR c-lotes     AS CHAR FORMAT "x(12)".
DEF VAR de-total    AS DEC.
DEF VAR i-row       AS INT.
DEF VAR h-acomp     AS HANDLE NO-UNDO.
DEF VAR h-query     AS HANDLE NO-UNDO.
DEF VAR c-it-codigo LIKE ob-etiqueta.it-codigo.
DEF VAR c-cod-refer LIKE ob-etiqueta.cod-refer.
DEF VAR c-empresa   AS CHAR.

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

DEF NEW SHARED VAR p-it-codigo-150 AS CHAR.
DEF NEW SHARED VAR p-cod-refer-150 AS CHAR.
DEF NEW SHARED VAR p-lote-rp-150   AS LOG.


 /* Variaveis da Rotina de Impress∆o */
DEFINE VAR i-saida      AS INT.
DEFINE VAR c-saida      AS CHAR.
DEFINE VAR c-impressora LIKE imprsor_usuar.nom_disposit_so.
DEFINE VAR i-num-copias AS INT.
DEFINE VAR i-Lin        AS INT INIT 99.
DEFINE VAR i-pag        AS INT.
DEFINE VAR i-ct         AS INT.

/* Variaveis PARAMETROS */
DEF VAR c-cod-estabel      AS CHAR.
DEF VAR c-it-codigo-ini    LIKE ped-item.it-codigo INIT ''.
DEF VAR c-it-codigo-fin    LIKE ped-item.it-codigo INIT "ZZZZZZZZZZZZZZZ".
DEF VAR c-cod-refer-ini    LIKE ped-item.cod-refer.
DEF VAR c-cod-refer-fin    LIKE ped-item.cod-refer INIT "ZZZZZZZZZZ".
DEF VAR c-cod-qualid-ini   LIKE ob-etiqueta.cod-qualid.
DEF VAR c-cod-qualid-fin   LIKE ob-etiqueta.cod-qualid     INIT "Z".
DEF VAR c-cod-obsoleto-ini AS CHAR.
DEF VAR c-cod-obsoleto-fin AS CHAR INIT "Z".
DEF VAR c-corte-comerc-ini LIKE ob-etiqueta.corte-comerc   INIT "A".
DEF VAR c-corte-comerc-fin LIKE ob-etiqueta.corte-comerc   INIT "Z".
DEF VAR c-novo-local       AS CHAR.
DEF VAR l-lote-todos       AS LOG INIT NO.
DEF VAR l-lote-pp          AS LOG INIT NO.
DEF VAR l-lote-pd          AS LOG INIT NO.
DEF VAR l-lote-rp          AS LOG INIT YES.
DEF VAR l-lote-rd          AS LOG INIT YES.
DEF VAR l-lote-sc          AS LOG INIT NO.
DEF VAR l-lote-ca          AS LOG INIT NO.
DEF VAR c-tp-artigo        AS CHAR INIT 'A'.
DEF VAR l-ok               AS LOG.

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
&Scoped-define INTERNAL-TABLES tt-etiquetas tt-itens item tt-localiz

/* Definitions for BROWSE br-etiquetas                                  */
&Scoped-define FIELDS-IN-QUERY-br-etiquetas tt-etiquetas.num-etiqueta tt-etiquetas.localizacao tt-etiquetas.nuance tt-etiquetas.quantidade   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-etiquetas   
&Scoped-define SELF-NAME br-etiquetas
&Scoped-define OPEN-QUERY-br-etiquetas RUN pi-tot-etiquetas. OPEN QUERY {&SELF-NAME} FOR EACH tt-etiquetas WHERE                                  tt-etiquetas.it-codigo  = tt-itens.it-codigo AND                                  tt-etiquetas.cod-refer  = tt-itens.cod-refer AND                                  tt-etiquetas.nr-lote    = tt-itens.lote      AND                                  tt-etiquetas.corte-coml = tt-itens.corte-comerc                                  NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-etiquetas tt-etiquetas
&Scoped-define FIRST-TABLE-IN-QUERY-br-etiquetas tt-etiquetas


/* Definitions for BROWSE br-itens                                      */
&Scoped-define FIELDS-IN-QUERY-br-itens tt-itens.it-codigo item.desc-item tt-itens.cod-refer tt-itens.lote tt-itens.corte-comerc tt-itens.estoque   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-itens   
&Scoped-define SELF-NAME br-itens
&Scoped-define OPEN-QUERY-br-itens RUN pi-total-est. OPEN QUERY {&SELF-NAME} FOR EACH tt-itens NO-LOCK, ~
                                   FIRST item WHERE                                   item.it-codigo = tt-itens.it-codigo NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-itens tt-itens item
&Scoped-define FIRST-TABLE-IN-QUERY-br-itens tt-itens
&Scoped-define SECOND-TABLE-IN-QUERY-br-itens item


/* Definitions for BROWSE br-localiz                                    */
&Scoped-define FIELDS-IN-QUERY-br-localiz tt-localiz.num-etiqueta tt-localiz.localizacao tt-localiz.localiz-ant tt-localiz.quantidade   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-localiz tt-localiz.localizacao   
&Scoped-define ENABLED-TABLES-IN-QUERY-br-localiz tt-localiz
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br-localiz tt-localiz
&Scoped-define SELF-NAME br-localiz
&Scoped-define OPEN-QUERY-br-localiz RUN pi-tot-sel. OPEN QUERY {&SELF-NAME} FOR EACH tt-localiz WHERE                                  tt-localiz.it-codigo  = tt-itens.it-codigo AND                                  tt-localiz.cod-refer  = tt-itens.cod-refer AND                                  tt-localiz.nr-lote    = tt-itens.lote   AND                                  tt-localiz.corte-coml = tt-itens.corte-comerc                                  NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-localiz tt-localiz
&Scoped-define FIRST-TABLE-IN-QUERY-br-localiz tt-localiz


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-br-etiquetas}~
    ~{&OPEN-QUERY-br-itens}~
    ~{&OPEN-QUERY-br-localiz}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-3 RECT-6 br-itens br-etiquetas ~
br-localiz bt-sel bt-vapara bt-historico bt-anl-gerencial bt-add bt-desenho ~
bt-del bt-ok 
&Scoped-Define DISPLAYED-OBJECTS fi-total-est fi-total-disp fi-total-sel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
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

DEFINE BUTTON bt-anl-gerencial 
     IMAGE-UP FILE "image/im-150.bmp":U
     LABEL "" 
     SIZE 5 BY 1.13 TOOLTIP "Consulta Gerencial do Estoque"
     BGCOLOR 8 .

DEFINE BUTTON bt-confirma AUTO-GO 
     IMAGE-UP FILE "image/im-cq.bmp":U
     LABEL "OK" 
     SIZE 5 BY 1.13 TOOLTIP "Confirma alteraá‰es na localizaá∆o".

DEFINE BUTTON bt-del 
     IMAGE-UP FILE "adeicon\prev-au":U
     IMAGE-INSENSITIVE FILE "adeicon\prev-ai":U
     LABEL "" 
     SIZE 5 BY 1.5.

DEFINE BUTTON bt-desenho 
     IMAGE-UP FILE "image/im-show.bmp":U
     LABEL "" 
     SIZE 5 BY 1.13 TOOLTIP "Visualiza Imagem do Desenho"
     BGCOLOR 8 .

DEFINE BUTTON bt-det 
     IMAGE-UP FILE "image/im-det.bmp":U
     LABEL "" 
     SIZE 4.86 BY 1.38 TOOLTIP "Detalha Etiqueta".

DEFINE BUTTON bt-historico AUTO-GO 
     IMAGE-UP FILE "image/im-hist.bmp":U
     LABEL "" 
     SIZE 5 BY 1.13 TOOLTIP "Hist¢rico de Alteraá‰es da localizaá∆o"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-imprime AUTO-GO 
     IMAGE-UP FILE "image/im-pri.bmp":U
     LABEL "" 
     SIZE 5 BY 1.13 TOOLTIP "Imprime Programaá‰es"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-ok AUTO-END-KEY 
     IMAGE-UP FILE "image/im-exi.bmp":U
     LABEL "Cancelar" 
     SIZE 5 BY 1.13.

DEFINE BUTTON bt-sel 
     IMAGE-UP FILE "image/im-fil.bmp":U
     LABEL "Button 3" 
     SIZE 5 BY 1.13 TOOLTIP "Seleciona Itens".

DEFINE BUTTON bt-vapara AUTO-GO 
     IMAGE-UP FILE "image/im-vapra.bmp":U
     LABEL "" 
     SIZE 5 BY 1.13 TOOLTIP "Posicionar no Item"
     BGCOLOR 8 FONT 10.

DEFINE VARIABLE fi-total-disp AS DECIMAL FORMAT "-ZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11 BY .79
     FONT 2 NO-UNDO.

DEFINE VARIABLE fi-total-est AS DECIMAL FORMAT "-ZZ,ZZ9.99":R12 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12.86 BY .79
     FONT 2 NO-UNDO.

DEFINE VARIABLE fi-total-sel AS DECIMAL FORMAT "-ZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11 BY .79
     FONT 2 NO-UNDO.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 7 BY 9.96
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 75 BY 1.25
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-etiquetas FOR 
      tt-etiquetas SCROLLING.

DEFINE QUERY br-itens FOR 
      tt-itens, 
      item SCROLLING.

DEFINE QUERY br-localiz FOR 
      tt-localiz SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-etiquetas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-etiquetas w-digita _FREEFORM
  QUERY br-etiquetas NO-LOCK DISPLAY
      tt-etiquetas.num-etiqueta COLUMN-LABEL "Etiqueta"    FORMAT "999999999"  WIDTH  8
      tt-etiquetas.localizacao  COLUMN-LABEL "Localizaá∆o" FORMAT "999/999"    WIDTH  8.5
      tt-etiquetas.nuance       COLUMN-LABEL "Nuance"      FORMAT "X(6)"       WIDTH  5.5
      tt-etiquetas.quantidade   COLUMN-LABEL "Qtde (m)"    FORMAT ">>>,>>9.99" WIDTH  8
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 35 BY 8.63
         FONT 1
         TITLE "Etiquetas Dispon°veis".

DEFINE BROWSE br-itens
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-itens w-digita _FREEFORM
  QUERY br-itens NO-LOCK DISPLAY
      tt-itens.it-codigo     COLUMN-LABEL "Item"            FORMAT "X(6)"           WIDTH  6.0
      item.desc-item         COLUMN-LABEL "Descriá∆o"       FORMAT "x(40)"          WIDTH 35.0
      tt-itens.cod-refer     COLUMN-LABEL "Referància"      FORMAT "X(8)"           WIDTH  8.0
      tt-itens.lote          COLUMN-LABEL "Lote"            FORMAT "X(8)"           WIDTH  4.0
      tt-itens.corte-comerc  COLUMN-LABEL "Corte Comercial" FORMAT "X(15)"          WIDTH 12.0
      tt-itens.estoque       COLUMN-LABEL "Qtd Estoque"     FORMAT ">>>,>>>,>>9.99" WIDTH 12.0
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 83 BY 9.17
         FONT 1 ROW-HEIGHT-CHARS .46.

DEFINE BROWSE br-localiz
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-localiz w-digita _FREEFORM
  QUERY br-localiz NO-LOCK DISPLAY
      tt-localiz.num-etiqueta COLUMN-LABEL "Etiqueta"  FORMAT "999999999"  WIDTH  8
      tt-localiz.localizacao  COLUMN-LABEL "Local ATU" FORMAT "999/999"    WIDTH  8
      tt-localiz.localiz-ant  COLUMN-LABEL "Local ANT" FORMAT "999/999"    WIDTH  8
      tt-localiz.quantidade   COLUMN-LABEL "Qtde (m)"  FORMAT ">>>,>>9.99" WIDTH  8
      ENABLE
      tt-localiz.localizacao
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE NO-SCROLLBAR-VERTICAL SIZE 34.86 BY 8.67
         FONT 1
         TITLE "Etiquetas Para Corte".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br-itens AT ROW 1.25 COL 2
     fi-total-est AT ROW 10.58 COL 67.57 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     br-etiquetas AT ROW 11.54 COL 2
     br-localiz AT ROW 11.54 COL 43.14
     bt-sel AT ROW 11.71 COL 79.29
     bt-vapara AT ROW 12.83 COL 79.29
     bt-imprime AT ROW 13.96 COL 79.29
     bt-det AT ROW 14.79 COL 37.72
     bt-historico AT ROW 15.08 COL 79.29
     bt-anl-gerencial AT ROW 16.21 COL 79.29
     bt-add AT ROW 16.38 COL 37.57
     bt-desenho AT ROW 17.33 COL 79.29
     bt-del AT ROW 18.13 COL 37.57
     bt-confirma AT ROW 19.08 COL 79.29
     bt-ok AT ROW 20.21 COL 79.29
     fi-total-disp AT ROW 20.54 COL 22.57 COLON-ALIGNED NO-LABEL
     fi-total-sel AT ROW 20.54 COL 59.86 COLON-ALIGNED NO-LABEL
     "Dispon°vel:" VIEW-AS TEXT
          SIZE 9.29 BY .54 AT ROW 20.67 COL 14.86
          BGCOLOR 8 FONT 6
     "Selecionado:" VIEW-AS TEXT
          SIZE 10.72 BY .54 AT ROW 20.67 COL 50.72
          BGCOLOR 8 FONT 6
     "Estoque:" VIEW-AS TEXT
          SIZE 7.57 BY .54 AT ROW 10.67 COL 61.57 WIDGET-ID 4
          FONT 6
     RECT-3 AT ROW 11.54 COL 78.29
     RECT-6 AT ROW 20.29 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 84.86 BY 22.79
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
         TITLE              = "Manutená∆o da Localizaá∆o da Etiqueta"
         COLUMN             = 20.14
         ROW                = 7.46
         HEIGHT             = 20.71
         WIDTH              = 85
         MAX-HEIGHT         = 22.92
         MAX-WIDTH          = 123.57
         VIRTUAL-HEIGHT     = 22.92
         VIRTUAL-WIDTH      = 123.57
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
/* BROWSE-TAB br-etiquetas fi-total-est F-Main */
/* BROWSE-TAB br-localiz br-etiquetas F-Main */
/* SETTINGS FOR BUTTON bt-confirma IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-det IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-imprime IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-total-disp IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-total-est IN FRAME F-Main
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
RUN pi-tot-etiquetas.
OPEN QUERY {&SELF-NAME} FOR EACH tt-etiquetas WHERE
                                 tt-etiquetas.it-codigo  = tt-itens.it-codigo AND
                                 tt-etiquetas.cod-refer  = tt-itens.cod-refer AND
                                 tt-etiquetas.nr-lote    = tt-itens.lote      AND
                                 tt-etiquetas.corte-coml = tt-itens.corte-comerc
                                 NO-LOCK.
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
RUN pi-total-est.
OPEN QUERY {&SELF-NAME} FOR EACH tt-itens NO-LOCK,
                            FIRST item WHERE
                                  item.it-codigo = tt-itens.it-codigo NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-itens */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-localiz
/* Query rebuild information for BROWSE br-localiz
     _START_FREEFORM
RUN pi-tot-sel.
OPEN QUERY {&SELF-NAME} FOR EACH tt-localiz WHERE
                                 tt-localiz.it-codigo  = tt-itens.it-codigo AND
                                 tt-localiz.cod-refer  = tt-itens.cod-refer AND
                                 tt-localiz.nr-lote    = tt-itens.lote   AND
                                 tt-localiz.corte-coml = tt-itens.corte-comerc
                                 NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-localiz */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-digita w-digita
ON END-ERROR OF w-digita /* Manutená∆o da Localizaá∆o da Etiqueta */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-digita w-digita
ON WINDOW-CLOSE OF w-digita /* Manutená∆o da Localizaá∆o da Etiqueta */
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
ON VALUE-CHANGED OF br-etiquetas IN FRAME F-Main /* Etiquetas Dispon°veis */
DO:
   IF AVAIL tt-etiquetas THEN
      ASSIGN bt-det:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
   {&OPEN-QUERY-br-localiz}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-itens
&Scoped-define SELF-NAME br-itens
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-itens w-digita
ON VALUE-CHANGED OF br-itens IN FRAME F-Main
DO:
    FIND tt-historico WHERE
         tt-historico.it-codigo = tt-itens.it-codigo AND
         tt-historico.cod-refer = tt-itens.cod-refer AND
         tt-historico.lote = tt-itens.lote AND
         tt-historico.corte-comerc = tt-itens.cod-corte-comerc NO-LOCK NO-ERROR.

    IF AVAIL tt-historico THEN
        ASSIGN bt-historico:SENSITIVE = YES.
    ELSE
        ASSIGN bt-historico:SENSITIVE = NO.

    {&OPEN-QUERY-br-etiquetas}
    APPLY 'value-changed' TO br-etiquetas.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-add w-digita
ON CHOOSE OF bt-add IN FRAME F-Main
DO:
   DO i-row = 1 TO br-etiquetas:NUM-SELECTED-ROWS:
      IF br-etiquetas:FETCH-SELECTED-ROW(i-row) THEN DO.
         FIND tt-localiz WHERE
              tt-localiz.num-etiqueta = tt-etiquetas.num-etiqueta
              NO-ERROR.
         IF NOT AVAIL tt-localiz THEN DO.
            CREATE tt-localiz.
            BUFFER-COPY tt-etiquetas TO tt-localiz.
            ASSIGN tt-localiz.localiz-ant = tt-etiquetas.localizacao
                   tt-localiz.localizacao = c-novo-local.
            DELETE tt-etiquetas.
         END.
      END.
      ASSIGN bt-confirma:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
   END.
   {&OPEN-QUERY-br-etiquetas}
   {&OPEN-QUERY-br-localiz}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-anl-gerencial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-anl-gerencial w-digita
ON CHOOSE OF bt-anl-gerencial IN FRAME F-Main
DO:
   IF AVAIL tt-itens THEN DO.
      ASSIGN w-digita:SENSITIVE = NO.
      ASSIGN p-it-codigo-150 = tt-itens.it-codigo
             p-cod-refer-150 = tt-itens.cod-refer
             p-lote-rp-150 = YES.

      RUN esp/essp0150.w "SHARED".
      ASSIGN w-digita:SENSITIVE = YES.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-confirma
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-confirma w-digita
ON CHOOSE OF bt-confirma IN FRAME F-Main /* OK */
DO:
   FIND FIRST b-tt-localiz NO-LOCK NO-ERROR.
   IF NOT AVAIL b-tt-localiz THEN DO:
      MESSAGE "Favor selecionar Etiquetas para alteraá∆o da localizaá∆o ! ! !"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY 'entry' TO br-etiquetas.
      RETURN NO-APPLY.
   END.


   FOR EACH b-tt-localiz NO-LOCK. 
       FIND ob-etiqueta WHERE
            ob-etiqueta.cod-estabel  = c-cod-estabel AND
            ob-etiqueta.num-etiqueta = b-tt-localiz.num-etiqueta SHARE-LOCK NO-ERROR.
       IF NOT AVAIL ob-etiqueta THEN NEXT.
       IF ob-etiqueta.situacao = 3 THEN DO:
          ASSIGN ob-etiqueta.localizacao = b-tt-localiz.localizacao
                 SUBSTR(ob-etiqueta.char-1, 1, 10) = STRING(TODAY, "99/99/9999")
                 SUBSTR(ob-etiqueta.char-1,11, 12) = c-seg-usuario
                 SUBSTR(ob-etiqueta.char-1,23,  6) = b-tt-localiz.localiz-ant.
       END.
       ELSE
           MESSAGE "A Etiqueta: " STRING(ob-etiqueta.num-etiqueta) SKIP(1)
                   "A sua localizaá∆o n∆o pode ser mudada. Devido a mudanáa" SKIP
                   "na sua situaá∆o em ESTOQUE. Provavelmente foi RESERVADA" SKIP
                   "ou FATURADA"
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
   END.
   RUN pi-imprime.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-del
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-del w-digita
ON CHOOSE OF bt-del IN FRAME F-Main
DO:
   DO i-row = 1 TO br-localiz:NUM-SELECTED-ROWS:
      IF br-localiz:FETCH-SELECTED-ROW(i-row) THEN DO.
          FIND tt-etiquetas WHERE
               tt-etiquetas.num-etiqueta = tt-localiz.num-etiqueta
               NO-ERROR.
          IF NOT AVAIL tt-etiquetas THEN DO.
             CREATE tt-etiquetas.
             BUFFER-COPY tt-localiz TO tt-etiquetas.
          END.
         DELETE tt-localiz.
      END.
   END.
   FIND FIRST b-tt-localiz WHERE NO-LOCK NO-ERROR.
   IF NOT AVAIL b-tt-localiz THEN
      ASSIGN bt-confirma:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

   {&OPEN-QUERY-br-etiquetas}
   {&OPEN-QUERY-br-localiz}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-desenho
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-desenho w-digita
ON CHOOSE OF bt-desenho IN FRAME F-Main
DO:
   RUN esdlg/d01-desenho.w (INPUT tt-itens.cod-refer).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-det
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-det w-digita
ON CHOOSE OF bt-det IN FRAME F-Main
DO:
   FIND ob-etiqueta WHERE
        ob-etiqueta.cod-estab = c-cod-estabel AND
        ob-etiqueta.num-etiqueta = tt-etiquetas.num-etiqueta NO-LOCK NO-ERROR.
   IF AVAIL ob-etiqueta THEN DO:
      ASSIGN gr-ob-etiqueta = ROWID(ob-etiqueta).
      RUN esp/essp0146.p.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-historico
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-historico w-digita
ON CHOOSE OF bt-historico IN FRAME F-Main
DO:
   IF AVAIL tt-historico THEN DO: 
      ASSIGN w-digita:SENSITIVE = NO.
      RUN esp/essp0195b.w (INPUT TABLE tt-historico,
                           INPUT TABLE tt-det-hist,
                           INPUT tt-itens.it-codigo,
                           INPUT tt-itens.cod-refer,
                           INPUT tt-itens.lote,
                           INPUT tt-itens.cod-corte-comerc).
      ASSIGN w-digita:SENSITIVE = YES.
  END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-imprime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-imprime w-digita
ON CHOOSE OF bt-imprime IN FRAME F-Main
DO:
    RUN pi-imprime.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-digita
ON CHOOSE OF bt-ok IN FRAME F-Main /* Cancelar */
DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-sel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-sel w-digita
ON CHOOSE OF bt-sel IN FRAME F-Main /* Button 3 */
DO:
   IF AVAIL tt-localiz THEN DO:
       MESSAGE "                   A T E N Ä « O                " SKIP 
               "Existem Etiquetas para Corte, que n∆o foram Con-" SKIP
               "firmadas " SKIP(1)
               "Deseja Confirmar Estas Etiquetas ?"
                VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE l-conf AS LOG.

       IF l-conf THEN
          APPLY 'choose' TO bt-confirma.

   END.
   ASSIGN w-digita:SENSITIVE = NO.

   RUN esp/essp0195a.w (INPUT-OUTPUT c-cod-estabel,
                        INPUT-OUTPUT c-it-codigo-ini,
                        INPUT-OUTPUT c-it-codigo-fin,
                        INPUT-OUTPUT c-cod-refer-ini,
                        INPUT-OUTPUT c-cod-refer-fin,
                        INPUT-OUTPUT c-cod-qualid-ini,  
                        INPUT-OUTPUT c-cod-qualid-fin,  
                        INPUT-OUTPUT c-cod-obsoleto-ini,
                        INPUT-OUTPUT c-cod-obsoleto-fin,
                        INPUT-OUTPUT c-corte-comerc-ini,
                        INPUT-OUTPUT c-corte-comerc-fin,
                        INPUT-OUTPUT l-lote-todos, 
                        INPUT-OUTPUT l-lote-pp,         
                        INPUT-OUTPUT l-lote-pd,         
                        INPUT-OUTPUT l-lote-rp,         
                        INPUT-OUTPUT l-lote-rd, 
                        INPUT-OUTPUT l-lote-sc,
                        INPUT-OUTPUT l-lote-ca,
                        INPUT-OUTPUT c-tp-artigo,
                        INPUT-OUTPUT l-ok,
                        INPUT-OUTPUT TABLE tt-digita,
                        INPUT-OUTPUT c-novo-local).
   IF l-ok THEN DO.

   
      FOR EACH tt-itens.
          DELETE tt-itens.
      END.

      FOR EACH tt-etiquetas.
          DELETE tt-etiquetas.
      END.

      FOR EACH tt-localiz.
          DELETE tt-localiz.
      END.

      FOR EACH tt-historico.
          DELETE tt-historico.
      END.

      RUN pi-processa.

      {&OPEN-QUERY-br-itens}
      {&OPEN-QUERY-br-etiquetas}
      {&OPEN-QUERY-br-localiz}
   END.
   ASSIGN w-digita:SENSITIVE = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-vapara
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-vapara w-digita
ON CHOOSE OF bt-vapara IN FRAME F-Main
DO:
   RUN esdlg/d01essp0195.w (OUTPUT c-it-codigo,
                            OUTPUT c-cod-refer).
   IF c-it-codigo <> "" AND c-cod-refer = "" THEN DO:
      FIND FIRST tt-itens WHERE
                 tt-itens.it-codigo = c-it-codigo NO-LOCK NO-ERROR. 

      IF NOT AVAIL tt-itens THEN DO.
         MESSAGE "Item n∆o est† contido na seleá∆o!"
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
         RETURN NO-APPLY.
      END.
      h-query:REPOSITION-TO-ROWID(ROWID(tt-itens)) NO-ERROR.
      APPLY 'VALUE-CHANGED' TO br-itens.
   END.
   ELSE DO:
       IF c-cod-refer <> "" THEN DO:
          FIND FIRST tt-itens WHERE
                     tt-itens.it-codigo = c-it-codigo AND
                     tt-itens.cod-refer = c-cod-refer  NO-LOCK NO-ERROR. 

          IF NOT AVAIL tt-itens THEN DO.
             MESSAGE "Item/Referància n∆o est† contido na seleá∆o!"
                 VIEW-AS ALERT-BOX INFO BUTTONS OK.
             RETURN NO-APPLY.
          END.
          h-query:REPOSITION-TO-ROWID(ROWID(tt-itens)) NO-ERROR.
          APPLY 'VALUE-CHANGED' TO br-itens.
       END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-etiquetas
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-digita 


/* ***************************  Main Block  *************************** */

ASSIGN h-query = br-itens:QUERY.


/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}
STATUS INPUT OFF. /* Desliga Mensagem no RodapÇ da Tela */

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
  DISPLAY fi-total-est fi-total-disp fi-total-sel 
      WITH FRAME F-Main IN WINDOW w-digita.
  ENABLE RECT-3 RECT-6 br-itens br-etiquetas br-localiz bt-sel bt-vapara 
         bt-historico bt-anl-gerencial bt-add bt-desenho bt-del bt-ok 
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

  {utp/ut9000.i "ESSP0195" "2.04.00.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN h-query = br-itens:QUERY IN FRAME {&FRAME-NAME}.

  /* Busca Nome da Empresa */
  FIND FIRST param-global NO-LOCK NO-ERROR.
  FIND FIRST empresa WHERE
             empresa.ep-codigo = param-global.empresa-prin NO-LOCK NO-ERROR. 
  ASSIGN c-empresa = (IF AVAIL empresa THEN empresa.razao-social ELSE "").

  /* Busca Impressora Principal do Usuario */
  FOR EACH imprsor_usuar  WHERE 
           imprsor_usuar.cod_usuario       = c-seg-usuario AND 
           imprsor_usuar.log_imprsor_princ = YES NO-LOCK.
      ASSIGN c-impressora = imprsor_usuar.nom_disposit_so.
  END.

  APPLY 'choose' TO bt-sel IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imp-cabec w-digita 
PROCEDURE pi-imp-cabec :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  PUT c-empresa  FORMAT "X(40)"                 AT   1
      "DATA: "                                  AT  71
      STRING(TODAY,"99/99/9999") FORMAT "X(10)" AT  77
      "HORA: "                                  AT 104
      STRING(TIME,"hh:mm:ss")                   AT 110
      "PAG:"                                    AT 144
      i-pag FORMAT ">>>"                        AT 149
      SKIP(1).

  PUT "RELATORIO DA LOCALIZAÄ«O ETIQUETAS MANUAL " FORMAT "X(60)" AT 36 SKIP(1).


  PUT "ITEM    DESCRICAO                                REFERENCIA  LOTE CORTE COMERCIAL  ETIQUETA LOCAL ATU LOCAL ANT QUANTIDADE" AT 1.
  PUT "-----   ---------------------------------------  ----------  ---- --------------- --------- --------- --------- ----------" AT 1.
  ASSIGN i-pag = i-pag + 1.                                                                              

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imprime w-digita 
PROCEDURE pi-imprime :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF VAR h-prog AS HANDLE NO-UNDO.
 DEF VAR i-ct   AS INT.

 DEF VAR i-col AS INT INITIAL 0.


 RUN utp/ut-utils.p PERSISTENT SET h-prog.

 RUN esapi/saida-imp.p (OUTPUT i-saida,
                        OUTPUT c-saida,
                        OUTPUT i-num-copias,
                        OUTPUT l-ok).

 CASE i-saida:
     WHEN 1 THEN DO.
         OUTPUT TO VALUE(c-impressora) CONVERT TARGET "ibm850" PAGED PAGE-SIZE 62.
         PUT CONTROL "~033E~033(s18H".    
     END.
     WHEN 2 THEN
         OUTPUT TO VALUE(c-saida).
     WHEN 3 THEN DO.
         ASSIGN c-saida = SESSION:TEMP-DIRECTORY + "essp0195b.tmp".
         OUTPUT TO VALUE(c-saida) CONVERT SOURCE "ibm850".
     END.
 END CASE.

 DO i-ct = 1 TO i-num-copias.
    ASSIGN i-pag      =  1
           i-lin      = 99
           de-total   =  0.
    FOR EACH b-tt-localiz NO-LOCK
          BY b-tt-localiz.localiz-ant
          BY b-tt-localiz.it-codigo
          BY b-tt-localiz.num-etiqueta.

        FIND ob-etiqueta WHERE
             ob-etiqueta.cod-estabel  = c-cod-estabel AND
             ob-etiqueta.num-etiqueta = b-tt-localiz.num-etiqueta NO-LOCK NO-ERROR.

        IF ob-etiqueta.situacao <> 3 THEN NEXT.

        FIND ITEM WHERE
             ITEM.it-codigo = b-tt-localiz.it-codigo NO-LOCK NO-ERROR.

        IF i-lin > 62 THEN DO:
           RUN pi-imp-cabec.
           ASSIGN i-lin = 7.
        END.

        PUT b-tt-localiz.it-codigo            FORMAT "x(6)"       AT   1
            item.desc-item                    FORMAT "x(40)"      AT   9
            b-tt-localiz.cod-refer            FORMAT "x(7)"       AT  50 
            b-tt-localiz.nr-lote              FORMAT "x(2)"       AT  62 
            b-tt-localiz.corte-coml           FORMAT "x(15)"      AT  67 
            b-tt-localiz.num-etiqueta         FORMAT ">>>>>>>>9"  AT  83 
            b-tt-localiz.localizacao          FORMAT "999/999"    AT  93 
            SUBSTR(ob-etiqueta.char-1, 23, 6) FORMAT "999/999"    AT 103 
            b-tt-localiz.quantidade           FORMAT ">>>,>>9.99" AT 113. 

        ASSIGN i-lin = i-lin + 1
               de-total = de-total + ob-etiqueta.quantidade.

    END.
    IF i-lin <>  99 THEN DO:
       PUT "----------" AT 113.
       PUT "TOTAL GERAL......." AT 67.
       PUT de-total   FORMAT ">>>,>>9.99" AT 113.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-processa w-digita 
PROCEDURE pi-processa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
{utp/ut-liter.i Lendo_Etiquetas *}
RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).


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


FOR EACH ob-etiqueta WHERE   
         ob-etiqueta.cod-estabel = c-cod-estabel AND
         ob-etiqueta.situacao    = 3 AND   /* Em Estoque */
         ob-etiqueta.it-codigo  >= c-it-codigo-ini AND
         ob-etiqueta.it-codigo  <= c-it-codigo-fin AND
         ob-etiqueta.cod-refer  >= c-cod-refer-ini AND 
         ob-etiqueta.cod-refer  <= c-cod-refer-fin AND 
         INDEX(c-lotes,SUBSTR(ob-etiqueta.nr-lote,1,2)) <> 0 NO-LOCK.

    IF ob-etiqueta.localizacao = ""  THEN NEXT.

    RUN pi-ver-digita (INPUT "Item",
                       INPUT ob-etiqueta.it-codigo).
    IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

    RUN pi-ver-digita (INPUT "Referància",
                       INPUT ob-etiqueta.cod-refer).
    IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

    IF ob-etiqueta.corte-comerc < c-corte-comerc-ini OR  
       ob-etiqueta.corte-comerc > c-corte-comerc-fin THEN NEXT.
    RUN pi-ver-digita (INPUT "Corte_Comercial",
                       INPUT ob-etiqueta.corte-comerc).
    IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

    IF ob-etiqueta.cod-qualid < c-cod-qualid-ini OR
       ob-etiqueta.cod-qualid > c-cod-qualid-fin THEN NEXT.
    RUN pi-ver-digita (INPUT "Qualidade",
                       INPUT ob-etiqueta.cod-qualid).
    IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

    /*
    FIND ref-item-ext WHERE
         ref-item-ext.it-codigo = ob-etiqueta.it-codigo AND
         ref-item-ext.cod-refer = ob-etiqueta.cod-refer 
         NO-LOCK NO-ERROR.
    IF NOT AVAIL ref-item-ext AND
       c-cod-obsoleto-ini <> '' THEN NEXT.

    IF AVAIL ref-item-ext THEN DO.
       IF ref-item-ext.cod-obsoleto < c-cod-obsoleto-ini OR
          ref-item-ext.cod-obsoleto > c-cod-obsoleto-fin THEN NEXT.

       RUN pi-ver-digita (INPUT "Codigo_Obsoleto",
                          INPUT ref-item-ext.cod-obsoleto).
       IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.
    END.
    */
    RUN pi-acompanhar IN h-acomp (INPUT "Etiqueta: " + STRING(ob-etiqueta.num-etiqueta) +
                                        "  Item: " + ob-etiqueta.it-codigo +
                                        "   Ref: " + ob-etiqueta.cod-refer).

    FIND corte-comerc WHERE
         corte-comerc.codigo = ob-etiqueta.corte-comerc.

    IF ob-etiqueta.localizacao <> c-novo-local THEN DO:
       FIND tt-itens WHERE
            tt-itens.it-codigo = ob-etiqueta.it-codigo           AND
            tt-itens.cod-refer = ob-etiqueta.cod-refer           AND
            tt-itens.lote      = SUBSTR(ob-etiqueta.nr-lote,1,2) AND 
            tt-itens.corte-comerc = corte-comerc.descricao       NO-LOCK NO-ERROR.
       IF NOT AVAIL tt-itens THEN DO.
          CREATE tt-itens.
          ASSIGN tt-itens.it-codigo    = ob-etiqueta.it-codigo 
                 tt-itens.cod-refer    = ob-etiqueta.cod-refer
                 tt-itens.lote         = SUBSTR(ob-etiqueta.nr-lote,1,2)
                 tt-itens.corte-comerc = corte-comerc.descricao
                 tt-itens.cod-corte-comerc = corte-comerc.codigo.
       END.
        
       ASSIGN tt-itens.estoque = tt-itens.estoque + ob-etiqueta.quantidade.
       FIND tt-etiquetas WHERE
            tt-etiquetas.num-etiqueta = ob-etiqueta.num-etiqueta NO-LOCK NO-ERROR.
       IF NOT AVAIL tt-etiquetas THEN DO:
          CREATE tt-etiquetas.
          BUFFER-COPY ob-etiqueta TO tt-etiquetas.
          ASSIGN tt-etiquetas.corte-coml = corte-comerc.descricao.   
       END.
    END.
    ELSE DO:
       IF SUBSTR(ob-etiqueta.char-1, 1, 10) <> "" THEN DO:
          FIND tt-historico WHERE
               tt-historico.dt-manut = SUBSTR(ob-etiqueta.char-1, 1, 10) AND 
               tt-historico.usuario  = SUBSTR(ob-etiqueta.char-1,11, 12) NO-LOCK NO-ERROR.
          IF NOT AVAIL tt-historico THEN DO:

             CREATE tt-historico.
             ASSIGN tt-historico.dt-manut = SUBSTR(ob-etiqueta.char-1, 1, 10)
                    tt-historico.usuario  = SUBSTR(ob-etiqueta.char-1,11, 12)
                    tt-historico.it-codigo    = ob-etiqueta.it-codigo    
                    tt-historico.cod-refer    = ob-etiqueta.cod-refer    
                    tt-historico.lote         = SUBSTR(ob-etiqueta.nr-lote,1,2)
                    tt-historico.corte-comerc = corte-comerc.codigo.
          END.
          FIND tt-det-hist WHERE
               tt-det-hist.dt-manut     = SUBSTR(ob-etiqueta.char-1, 1, 10) AND 
               tt-det-hist.usuario      = SUBSTR(ob-etiqueta.char-1,11, 12) AND 
               tt-det-hist.cod-estabel  = ob-etiqueta.cod-estabel  AND
               tt-det-hist.num-etiqueta = ob-etiqueta.num-etiqueta AND
               tt-det-hist.it-codigo    = ob-etiqueta.it-codigo    AND
               tt-det-hist.cod-refer    = ob-etiqueta.cod-refer    AND
               tt-det-hist.lote         = SUBSTR(ob-etiqueta.nr-lote,1,2) AND
               tt-det-hist.corte-comerc = corte-comerc.codigo NO-LOCK NO-ERROR.

          IF NOT AVAIL tt-det-hist THEN DO:
             CREATE tt-det-hist.
             ASSIGN tt-det-hist.dt-manut     = SUBSTR(ob-etiqueta.char-1, 1, 10)
                    tt-det-hist.usuario      = SUBSTR(ob-etiqueta.char-1,11, 12)
                    tt-det-hist.cod-estabel  = ob-etiqueta.cod-estabel
                    tt-det-hist.num-etiqueta = ob-etiqueta.num-etiqueta
                    tt-det-hist.it-codigo    = ob-etiqueta.it-codigo 
                    tt-det-hist.cod-refer    = ob-etiqueta.cod-refer
                    tt-det-hist.lote         = SUBSTR(ob-etiqueta.nr-lote,1,2)
                    tt-det-hist.corte-comerc = corte-comerc.codigo
                    tt-det-hist.localiz-ant  = SUBSTR(ob-etiqueta.char-1,23,  6).
          END. 
       END.
    END.
END.
RUN pi-finalizar in h-acomp.

{&OPEN-QUERY-br-itens}

APPLY 'value-changed' TO br-itens IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-tot-etiquetas w-digita 
PROCEDURE pi-tot-etiquetas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

 ASSIGN fi-total-disp = 0.
 FOR EACH tt-etiquetas WHERE
          tt-etiquetas.it-codigo  = tt-itens.it-codigo AND
          tt-etiquetas.cod-refer  = tt-itens.cod-refer AND 
          tt-etiquetas.nr-lote    = tt-itens.lote      AND
          tt-etiquetas.corte-coml = tt-itens.corte-comerc
          NO-LOCK.
     ASSIGN fi-total-disp =  fi-total-disp + tt-etiquetas.quantidade.
 END.
 DISP fi-total-disp WITH FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-tot-sel w-digita 
PROCEDURE pi-tot-sel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 ASSIGN fi-total-sel = 0.
 FOR EACH tt-localiz WHERE
          tt-localiz.it-codigo  = tt-itens.it-codigo AND
          tt-localiz.cod-refer  = tt-itens.cod-refer AND 
          tt-localiz.nr-lote    = tt-itens.lote      AND
          tt-localiz.corte-coml = tt-itens.corte-comerc
          NO-LOCK.
     ASSIGN fi-total-sel =  fi-total-sel + tt-localiz.quantidade.
 END.
 DISP fi-total-sel WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-total-est w-digita 
PROCEDURE pi-total-est :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN fi-total-est = 0.
  FOR EACH tt-itens NO-LOCK.
      ASSIGN fi-total-est =  fi-total-est + tt-itens.estoque.
  END.
  DISP fi-total-est WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-ver-digita w-digita 
PROCEDURE pi-ver-digita :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF INPUT PARAMETER p-campo AS CHAR.
 DEF INPUT PARAMETER p-valor AS CHAR.

 IF CAN-FIND(FIRST tt-digita WHERE
                   tt-digita.opcao = 'D' AND
                   tt-digita.campo = p-campo) AND
    NOT CAN-FIND(FIRST tt-digita WHERE
                       tt-digita.opcao = 'D' AND
                       tt-digita.campo = p-campo AND
                       tt-digita.valor = p-valor) THEN RETURN 'ADM-ERROR'.
 ELSE
   IF CAN-FIND(FIRST tt-digita WHERE
                     tt-digita.opcao = 'E' AND
                     tt-digita.campo = p-campo AND
                     tt-digita.valor = p-valor) THEN RETURN 'ADM-ERROR'.
   ELSE
      RETURN 'OK'.

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
  {src/adm/template/snd-list.i "tt-localiz"}
  {src/adm/template/snd-list.i "tt-itens"}
  {src/adm/template/snd-list.i "item"}
  {src/adm/template/snd-list.i "tt-etiquetas"}

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

