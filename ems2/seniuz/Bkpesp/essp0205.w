&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          ems2cad          PROGRESS
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

DEF BUFFER empresa FOR mgcad.empresa.

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
    FIELD qtidade-atu  LIKE saldo-estoq.qtidade-atu
    FIELD cod-corte-comerc LIKE corte-comerc.codigo
    INDEX indice1 IS PRIMARY it-codigo cod-refer lote.

DEF TEMP-TABLE tt-etiquetas LIKE ob-etiqueta
    FIELD corte-coml LIKE corte-comerc.descricao.

DEF TEMP-TABLE tt-etq-baixar LIKE ob-etiqueta
    FIELD marca       AS CHAR.

/* Global Variable Definitions ---                                       */
DEF NEW GLOBAL SHARED VAR gr-ob-etiqueta AS ROWID NO-UNDO.

DEF BUFFER b-tt-etiquetas FOR tt-etiquetas.
DEF BUFFER b-tt-etq-baixar   FOR tt-etq-baixar.

/* Local Variable Definitions ---                                       */
DEF VAR c-lotes     AS CHAR FORMAT "x(12)".
DEF VAR de-total    AS DEC.
DEF VAR i-row       AS INT.
DEF VAR h-acomp     AS HANDLE NO-UNDO.
DEF VAR h-query     AS HANDLE NO-UNDO.
DEF VAR c-it-codigo LIKE ob-etiqueta.it-codigo.
DEF VAR c-cod-refer LIKE ob-etiqueta.cod-refer.
DEF VAR c-empresa   AS CHAR.
DEF VAR l-erro      AS LOG.

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

DEF NEW SHARED VAR p-it-codigo-150 AS CHAR.
DEF NEW SHARED VAR p-cod-refer-150 AS CHAR.
DEF NEW SHARED VAR p-lote-rp-150   AS LOG.

 /* Variaveis da Rotina de ImpressÆo */
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
DEF VAR l-lote-rp          AS LOG INIT NO.
DEF VAR l-lote-rd          AS LOG INIT NO.
DEF VAR l-lote-sc          AS LOG INIT NO.
DEF VAR l-lote-ca          AS LOG INIT YES.
DEF VAR c-tp-artigo        AS CHAR INIT 'A'.
DEF VAR l-ok               AS LOG.

/* Includes para gerar Movto Estoq */
{cdp/cd0666.i}
{cep/ceapi001.i}
/*{cep/ceapi002.i}*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Digitacao
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-baixa

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-etq-baixar tt-etiquetas tt-itens item

/* Definitions for BROWSE br-baixa                                      */
&Scoped-define FIELDS-IN-QUERY-br-baixa tt-etq-baixar.num-etiqueta tt-etq-baixar.localiz tt-etq-baixar.quantidade   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-baixa   
&Scoped-define SELF-NAME br-baixa
&Scoped-define OPEN-QUERY-br-baixa RUN pi-tot-sel. OPEN QUERY {&SELF-NAME} FOR EACH tt-etq-baixar WHERE                                  tt-etq-baixar.it-codigo  = tt-itens.it-codigo                                  NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-baixa tt-etq-baixar
&Scoped-define FIRST-TABLE-IN-QUERY-br-baixa tt-etq-baixar


/* Definitions for BROWSE br-etiquetas                                  */
&Scoped-define FIELDS-IN-QUERY-br-etiquetas tt-etiquetas.num-etiqueta tt-etiquetas.cod-refer tt-etiquetas.nr-lote tt-etiquetas.localiz tt-etiquetas.quantidade   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-etiquetas   
&Scoped-define SELF-NAME br-etiquetas
&Scoped-define OPEN-QUERY-br-etiquetas RUN pi-tot-etiquetas. OPEN QUERY {&SELF-NAME} FOR EACH tt-etiquetas WHERE                                  tt-etiquetas.it-codigo = tt-itens.it-codigo AND                                  tt-etiquetas.cod-refer = tt-itens.cod-refer                                  NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-etiquetas tt-etiquetas
&Scoped-define FIRST-TABLE-IN-QUERY-br-etiquetas tt-etiquetas


/* Definitions for BROWSE br-itens                                      */
&Scoped-define FIELDS-IN-QUERY-br-itens tt-itens.it-codigo item.desc-item tt-itens.cod-refer tt-itens.estoque tt-itens.qtidade-atu   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-itens   
&Scoped-define SELF-NAME br-itens
&Scoped-define OPEN-QUERY-br-itens RUN pi-total-est. OPEN QUERY {&SELF-NAME} FOR EACH tt-itens NO-LOCK, ~
                                   FIRST item WHERE                                   item.it-codigo = tt-itens.it-codigo NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-itens tt-itens item
&Scoped-define FIRST-TABLE-IN-QUERY-br-itens tt-itens
&Scoped-define SECOND-TABLE-IN-QUERY-br-itens item


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-br-baixa}~
    ~{&OPEN-QUERY-br-etiquetas}~
    ~{&OPEN-QUERY-br-itens}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-3 RECT-6 br-itens br-etiquetas br-baixa ~
bt-sel bt-anl-gerencial bt-desenho bt-add bt-del bt-ok 
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
     SIZE 5 BY 1.58 TOOLTIP "Consulta Gerencial do Estoque"
     BGCOLOR 8 .

DEFINE BUTTON bt-confirma AUTO-GO 
     IMAGE-UP FILE "image/im-cq.bmp":U
     LABEL "OK" 
     SIZE 5 BY 1.71 TOOLTIP "Confirma altera‡äes na localiza‡Æo".

DEFINE BUTTON bt-del 
     IMAGE-UP FILE "adeicon\prev-au":U
     IMAGE-INSENSITIVE FILE "adeicon\prev-ai":U
     LABEL "" 
     SIZE 5 BY 1.5.

DEFINE BUTTON bt-desenho 
     IMAGE-UP FILE "image/im-show.bmp":U
     LABEL "" 
     SIZE 5 BY 1.58 TOOLTIP "Visualiza Imagem do Desenho"
     BGCOLOR 8 .

DEFINE BUTTON bt-det 
     IMAGE-UP FILE "image/im-det.bmp":U
     LABEL "" 
     SIZE 4.86 BY 1.38 TOOLTIP "Detalha Etiqueta".

DEFINE BUTTON bt-ok AUTO-END-KEY 
     IMAGE-UP FILE "image/im-exi.bmp":U
     LABEL "Cancelar" 
     SIZE 5 BY 1.71.

DEFINE BUTTON bt-sel 
     IMAGE-UP FILE "image/im-fil.bmp":U
     LABEL "Button 3" 
     SIZE 5 BY 1.75 TOOLTIP "Seleciona Itens".

DEFINE VARIABLE fi-total-disp AS DECIMAL FORMAT "-ZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11 BY .79
     FONT 2 NO-UNDO.

DEFINE VARIABLE fi-total-est AS DECIMAL FORMAT "-ZZ,ZZ9.99":R12 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.43 BY .88
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
     SIZE 76 BY 1.25
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-baixa FOR 
      tt-etq-baixar SCROLLING.

DEFINE QUERY br-etiquetas FOR 
      tt-etiquetas SCROLLING.

DEFINE QUERY br-itens FOR 
      tt-itens, 
      item SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-baixa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-baixa w-digita _FREEFORM
  QUERY br-baixa NO-LOCK DISPLAY
      tt-etq-baixar.num-etiqueta COLUMN-LABEL "Etiqueta"  FORMAT "999999999"  WIDTH  8
      tt-etq-baixar.localiz      COLUMN-LABEL "Local ATU" FORMAT "999/999"    WIDTH  8
      tt-etq-baixar.quantidade   COLUMN-LABEL "Qtde (m)"  FORMAT ">>>,>>9.99" WIDTH  8
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE NO-SCROLLBAR-VERTICAL SIZE 30 BY 8.67
         FONT 1
         TITLE "Etiquetas Para Baixar".

DEFINE BROWSE br-etiquetas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-etiquetas w-digita _FREEFORM
  QUERY br-etiquetas NO-LOCK DISPLAY
      tt-etiquetas.num-etiqueta COLUMN-LABEL "Etiqueta"    FORMAT "999999999"  WIDTH  9
      tt-etiquetas.cod-refer    COLUMN-LABEL "Refer" 
      tt-etiquetas.nr-lote      COLUMN-LABEL "Lote" 
      tt-etiquetas.localiz      COLUMN-LABEL "Localiz"    FORMAT "999/999"     WIDTH  7
      tt-etiquetas.quantidade   COLUMN-LABEL "Qtde (m)"   FORMAT ">>9.99"      WIDTH  5
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 39 BY 8.63
         FONT 1
         TITLE "Etiquetas Dispon¡veis".

DEFINE BROWSE br-itens
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-itens w-digita _FREEFORM
  QUERY br-itens NO-LOCK DISPLAY
      tt-itens.it-codigo     COLUMN-LABEL "Item"            FORMAT "X(8)"           WIDTH 8
      item.desc-item         COLUMN-LABEL "Descri‡Æo"       FORMAT "x(40)"          WIDTH 40
      tt-itens.cod-refer     COLUMN-LABEL "Referˆncia"      FORMAT "X(8)"           WIDTH 8
      tt-itens.estoque       COLUMN-LABEL "Qtd Estoque"     FORMAT ">,>>>,>>9.99"   WIDTH 10
      tt-itens.qtidade-atu   COLUMN-LABEL "Sld Contabil"    FORMAT ">,>>>,>>9.99"   WIDTH 10
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 83 BY 9
         FONT 1 ROW-HEIGHT-CHARS .46.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br-itens AT ROW 1.25 COL 2
     fi-total-est AT ROW 10.46 COL 66 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     br-etiquetas AT ROW 11.54 COL 2
     br-baixa AT ROW 11.54 COL 48
     bt-sel AT ROW 11.83 COL 79.43
     bt-anl-gerencial AT ROW 14.13 COL 79.43
     bt-det AT ROW 14.79 COL 42.14
     bt-desenho AT ROW 15.75 COL 79.43
     bt-add AT ROW 16.38 COL 42
     bt-confirma AT ROW 17.83 COL 79.29
     bt-del AT ROW 18.13 COL 42
     bt-ok AT ROW 19.63 COL 79.29
     fi-total-disp AT ROW 20.54 COL 22.57 COLON-ALIGNED NO-LABEL
     fi-total-sel AT ROW 20.54 COL 59.86 COLON-ALIGNED NO-LABEL
     "Selecionado:" VIEW-AS TEXT
          SIZE 10.72 BY .54 AT ROW 20.67 COL 50.72
          BGCOLOR 8 FONT 6
     "Dispon¡vel:" VIEW-AS TEXT
          SIZE 9.29 BY .54 AT ROW 20.67 COL 14.86
          BGCOLOR 8 FONT 6
     "Estoque:" VIEW-AS TEXT
          SIZE 7.57 BY .54 AT ROW 10.58 COL 60.14 WIDGET-ID 4
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
         TITLE              = "Manuten‡Æo da Localiza‡Æo da Etiqueta"
         COLUMN             = 21.86
         ROW                = 6.96
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
/* BROWSE-TAB br-baixa br-etiquetas F-Main */
/* SETTINGS FOR BUTTON bt-confirma IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-det IN FRAME F-Main
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

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-baixa
/* Query rebuild information for BROWSE br-baixa
     _START_FREEFORM
RUN pi-tot-sel.
OPEN QUERY {&SELF-NAME} FOR EACH tt-etq-baixar WHERE
                                 tt-etq-baixar.it-codigo  = tt-itens.it-codigo
                                 NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-baixa */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-etiquetas
/* Query rebuild information for BROWSE br-etiquetas
     _START_FREEFORM
RUN pi-tot-etiquetas.
OPEN QUERY {&SELF-NAME} FOR EACH tt-etiquetas WHERE
                                 tt-etiquetas.it-codigo = tt-itens.it-codigo AND
                                 tt-etiquetas.cod-refer = tt-itens.cod-refer
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

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-digita w-digita
ON END-ERROR OF w-digita /* Manuten‡Æo da Localiza‡Æo da Etiqueta */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-digita w-digita
ON WINDOW-CLOSE OF w-digita /* Manuten‡Æo da Localiza‡Æo da Etiqueta */
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
ON VALUE-CHANGED OF br-etiquetas IN FRAME F-Main /* Etiquetas Dispon¡veis */
DO:
   IF AVAIL tt-etiquetas THEN
      ASSIGN bt-det:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

   {&OPEN-QUERY-br-baixa}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-itens
&Scoped-define SELF-NAME br-itens
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-itens w-digita
ON VALUE-CHANGED OF br-itens IN FRAME F-Main
DO:
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
         FIND tt-etq-baixar WHERE
              tt-etq-baixar.num-etiqueta = tt-etiquetas.num-etiqueta
              NO-LOCK NO-ERROR.
         IF NOT AVAIL tt-etq-baixar THEN DO.
            CREATE tt-etq-baixar.
            BUFFER-COPY tt-etiquetas TO tt-etq-baixar.
            DELETE tt-etiquetas.
         END.
      END.
      ASSIGN bt-confirma:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
   END.
   {&OPEN-QUERY-br-etiquetas}
   {&OPEN-QUERY-br-baixa}
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
   FIND FIRST b-tt-etq-baixar NO-LOCK NO-ERROR.
   IF NOT AVAIL b-tt-etq-baixar THEN DO:
      MESSAGE "Favor selecionar Etiquetas para Baixar ! ! !"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY 'entry' TO br-etiquetas.
      RETURN NO-APPLY.
   END.

   ASSIGN l-erro = NO.
   FOR EACH b-tt-etq-baixar NO-LOCK. 
       FIND ob-etiqueta WHERE
            ob-etiqueta.cod-estabel  = c-cod-estabel AND
            ob-etiqueta.num-etiqueta = b-tt-etq-baixar.num-etiqueta SHARE-LOCK NO-ERROR.
       IF NOT AVAIL ob-etiqueta THEN NEXT.

       IF ob-etiqueta.situacao <> 3 THEN DO:
          MESSAGE "A Etiqueta: " STRING(ob-etiqueta.num-etiqueta) SKIP(1)
                  "NÆo Poder  ser Baixada. Devido a mudan‡a" SKIP
                  "na sua situa‡Æo em ESTOQUE. Provavelmente foi RESERVADA" SKIP
                  "ou FATURADA"
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          NEXT.
       END.

       FIND saldo-estoq WHERE
            saldo-estoq.cod-estab = ob-etiqueta.cod-estabel AND
            saldo-estoq.it-codigo = ob-etiqueta.it-codigo AND
            saldo-estoq.cod-refer = ob-etiqueta.cod-refer AND
            saldo-estoq.lote = ob-etiqueta.nr-lote + ob-etiqueta.cod-refer
            SHARE-LOCK NO-ERROR.

       IF NOT AVAIL saldo-estoq OR
          (AVAIL saldo-estoq AND saldo-estoq.qtidade-atu <= 0) THEN DO.
          FIND saldo-estoq WHERE
               saldo-estoq.cod-estab = ob-etiqueta.cod-estabel AND
               saldo-estoq.it-codigo = ob-etiqueta.it-codigo AND
               saldo-estoq.cod-refer = ob-etiqueta.cod-refer AND
               saldo-estoq.lote = ob-etiqueta.cod-refer AND
               saldo-estoq.qtidade-atu > 0
               SHARE-LOCK NO-ERROR.
       END.

       IF AVAIL saldo-estoq THEN DO.
          IF saldo-estoq.qt-alocada >= ob-etiqueta.quantidade THEN
             ASSIGN saldo-estoq.qt-alocada = saldo-estoq.qt-alocada - ob-etiqueta.quantidade.
           
          RUN pi-baixa-estoq (INPUT saldo-estoq.it-codigo,
                              INPUT saldo-estoq.cod-refer,
                              INPUT saldo-estoq.lote, 
                              INPUT ob-etiqueta.quantidade).
          
          IF RETURN-VALUE = 'OK' THEN
             ASSIGN ob-etiqueta.situacao = 7. /* Consumida em Corte */
          ELSE
             ASSIGN l-erro = YES.
             
       END.

       FIND CURRENT saldo-estoq NO-LOCK NO-ERROR.

   END.

   EMPTY TEMP-TABLE tt-etq-baixar.

   IF NOT l-erro THEN
      MESSAGE 'Etiquetas Baixadas com Sucesso...'
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-del
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-del w-digita
ON CHOOSE OF bt-del IN FRAME F-Main
DO:
   DO i-row = 1 TO br-baixa:NUM-SELECTED-ROWS:
      IF br-baixa:FETCH-SELECTED-ROW(i-row) THEN DO.
          FIND tt-etiquetas WHERE
               tt-etiquetas.num-etiqueta = tt-etq-baixar.num-etiqueta
               NO-LOCK NO-ERROR.
          IF NOT AVAIL tt-etiquetas THEN DO.
             CREATE tt-etiquetas.
             BUFFER-COPY tt-etq-baixar TO tt-etiquetas.
          END.
         DELETE tt-etq-baixar.
      END.
   END.
   FIND FIRST b-tt-etq-baixar WHERE NO-LOCK NO-ERROR.
   IF NOT AVAIL b-tt-etq-baixar THEN
      ASSIGN bt-confirma:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

   {&OPEN-QUERY-br-etiquetas}
   {&OPEN-QUERY-br-baixa}

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
   IF AVAIL tt-etq-baixar THEN DO:
       MESSAGE "                   A T E N € Ç O                " SKIP 
               "Existem Etiquetas para Corte, que nÆo foram Con-" SKIP
               "firmadas " SKIP(1)
               "Deseja Confirmar Estas Etiquetas ?"
                VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE l-conf AS LOG.

       IF l-conf THEN
          APPLY 'choose' TO bt-confirma.

   END.
   ASSIGN w-digita:SENSITIVE = NO.

   RUN esp/essp0205a.w (INPUT-OUTPUT c-cod-estabel,
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

   
      FOR EACH tt-itens NO-LOCK.
          DELETE tt-itens.
      END.

      FOR EACH tt-etiquetas NO-LOCK.
          DELETE tt-etiquetas.
      END.

      FOR EACH tt-etq-baixar NO-LOCK.
          DELETE tt-etq-baixar.
      END.

      RUN pi-processa.

      {&OPEN-QUERY-br-itens}
      {&OPEN-QUERY-br-etiquetas}
      {&OPEN-QUERY-br-baixa}
   END.
   ASSIGN w-digita:SENSITIVE = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-baixa
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-digita 


/* ***************************  Main Block  *************************** */

ASSIGN h-query = br-itens:QUERY.


/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}
STATUS INPUT OFF. /* Desliga Mensagem no Rodap‚ da Tela */

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
  ENABLE RECT-3 RECT-6 br-itens br-etiquetas br-baixa bt-sel bt-anl-gerencial 
         bt-desenho bt-add bt-del bt-ok 
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

  {utp/ut9000.i "ESSP0205" "2.04.00.000"}

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

  APPLY 'choose' TO bt-sel IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-baixa-estoq w-digita 
PROCEDURE pi-baixa-estoq :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER p-it-codigo  AS CHAR.
    DEF INPUT PARAMETER p-cod-refer  AS CHAR.
    DEF INPUT PARAMETER p-lote       AS CHAR.
    DEF INPUT PARAMETER p-qtde       AS DEC.

    DEF VAR c-erro     AS CHAR FORMAT "x(100)".

    RUN esapi/cria-movto-estoq.p (INPUT ob-etiqueta.cod-estabel,
                                  INPUT p-it-codigo,
                                  INPUT p-cod-refer,
                                  INPUT p-lote, 
                                  INPUT p-qtde,
                                  INPUT 30,   
                                  INPUT 2,  
                                  INPUT "Consumo de Quantidade",
                                  OUTPUT c-erro). 

    IF RETURN-VALUE = 'ADM-ERROR' THEN DO.
       MESSAGE "Erro ao Baixar a Etiqueta " SKIP
               c-erro
               VIEW-AS ALERT-BOX ERROR.

       RUN esapi/esapi002.p (INPUT "imatextil@imatextil.com.br", /* e-mail remetente */
                             INPUT "angelo.panzera@imatextil.com.br", /* e-mail destinat rio */
                             INPUT "Erro ao Consumir Quantidade da Etiqueta: " + STRING(ob-etiqueta.num-etiqueta), /* Assunto */
                             INPUT c-erro, /* Mensagem */
                             INPUT "", /*arquivo anexo*/
                             INPUT NO). /* Mostra Erros */
       RETURN 'ADM-ERROR'.
    END.
    RETURN 'OK'.
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

    RUN pi-ver-digita (INPUT "Item",
                       INPUT ob-etiqueta.it-codigo).
    IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

    RUN pi-ver-digita (INPUT "Referˆncia",
                       INPUT ob-etiqueta.cod-refer).
    IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

    IF NOT l-lote-todos THEN DO.
       FIND ob-localiz WHERE
            ob-localiz.cod-localiz = ob-etiqueta.localizacao NO-LOCK NO-ERROR.
       IF NOT AVAIL ob-localiz THEN NEXT.
       IF ob-localiz.tipo <> 2 THEN NEXT.
    END.

    RUN pi-acompanhar IN h-acomp (INPUT "Etiqueta: " + STRING(ob-etiqueta.num-etiqueta) +
                                        "  Item: " + ob-etiqueta.it-codigo +
                                        "   Ref: " + ob-etiqueta.cod-refer).


    FIND tt-itens WHERE
         tt-itens.it-codigo = ob-etiqueta.it-codigo AND 
         tt-itens.cod-refer = ob-etiqueta.cod-refer NO-LOCK NO-ERROR.
    IF NOT AVAIL tt-itens THEN DO.
       CREATE tt-itens.
       ASSIGN tt-itens.it-codigo = ob-etiqueta.it-codigo
              tt-itens.cod-refer = ob-etiqueta.cod-refer.

       FOR EACH saldo-estoq WHERE
                saldo-estoq.it-codigo = tt-itens.it-codigo AND
                saldo-estoq.cod-refer = tt-itens.cod-refer NO-LOCK.
           ASSIGN tt-itens.qtidade-atu = tt-itens.qtidade-atu + saldo-estoq.qtidade-atu.
       END.

    END.
    ASSIGN tt-itens.estoque = tt-itens.estoque + ob-etiqueta.quantidade.

    FIND tt-etiquetas WHERE
         tt-etiquetas.num-etiqueta = ob-etiqueta.num-etiqueta NO-LOCK NO-ERROR.
    IF NOT AVAIL tt-etiquetas THEN DO:
       CREATE tt-etiquetas.
       BUFFER-COPY ob-etiqueta TO tt-etiquetas.
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
 FOR EACH tt-etq-baixar WHERE
          tt-etq-baixar.it-codigo  = tt-itens.it-codigo AND
          tt-etq-baixar.cod-refer  = tt-itens.cod-refer AND 
          tt-etq-baixar.nr-lote    = tt-itens.lote 
          NO-LOCK.
     ASSIGN fi-total-sel =  fi-total-sel + tt-etq-baixar.quantidade.
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
  {src/adm/template/snd-list.i "tt-itens"}
  {src/adm/template/snd-list.i "item"}
  {src/adm/template/snd-list.i "tt-etiquetas"}
  {src/adm/template/snd-list.i "tt-etq-baixar"}

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

