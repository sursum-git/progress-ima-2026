&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          ems206           PROGRESS
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

DEF BUFFER empresa FOR mgcad.empresa.

DEF TEMP-TABLE tt-ped-venda
    FIELD row-tt-ped-item    AS ROWID
    FIELD row-tt-negativo AS ROWID
    FIELD cod-estabel     LIKE ped-venda.cod-estabel
    FIELD nr-pedcli       LIKE ped-venda.nr-pedcli
    FIELD nome-abrev      LIKE ped-venda.nome-abrev
    FIELD no-ab-reppri    LIKE ped-venda.no-ab-reppri
    FIELD cod-cond-pag    LIKE ped-venda.cod-cond-pag
    FIELD nome-transp     LIKE ped-venda.nome-transp
    FIELD nr-sequencia    LIKE ped-item.nr-sequencia
    FIELD qt-pedida       LIKE ped-item.qt-pedida
    FIELD qt-reserva      LIKE ped-item.qt-pedida 
    FIELD l-impresso      AS   LOG
    FIELD observ          LIKE ped-venda.observ
    INDEX indice1 IS PRIMARY nr-pedcli nr-sequencia row-tt-ped-item.

DEF TEMP-TABLE tt-ped-item
    FIELD nr-pedcli    LIKE ped-venda.nr-pedcli
    FIELD nr-sequencia LIKE ped-item.nr-sequencia
    FIELD it-codigo    LIKE ob-etiqueta.it-codigo
    FIELD cod-refer    LIKE ob-etiqueta.cod-refer
    FIELD lote         LIKE ob-etiqueta.nr-lote
    FIELD qt-pedida    LIKE ped-item.qt-pedida
    FIELD observacao   AS   CHAR FORMAT "x(20)" 
    INDEX indice1 IS PRIMARY nr-pedcli nr-sequencia.

DEF TEMP-TABLE tt-etiquetas LIKE ob-etiqueta.

/* Global Variable Definitions ---                                       */
DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.
DEF NEW GLOBAL SHARED VAR gr-ob-etiqueta AS ROWID NO-UNDO.
DEF NEW GLOBAL SHARED VAR gr-ped-venda  AS ROWID NO-UNDO.

/* Local Variable Definitions ---                                       */
DEF VAR rw-tt-ped-item   AS ROWID NO-UNDO.
DEF VAR h-acomp          AS HANDLE NO-UNDO.
DEF VAR h-query          AS HANDLE NO-UNDO.
DEF VAR c-dia            AS CHAR.
DEF VAR i-lin            AS INT.
DEF VAR i-pag            AS INT.
DEF VAR c-nr-pedcli      AS CHAR.
DEF VAR l-vrf-preco      AS LOG.
DEF VAR c-docas          AS CHAR.
DEF VAR i-handle         AS INT.
DEF VAR c-arq-bat        AS CHAR.
DEF VAR c-arq-saida      AS CHAR.

/* ParÉmetros */
DEF VAR c-cod-estabel AS CHAR.
DEF VAR c-nr-pedcli-ini    LIKE ped-item.nr-pedcli INIT "".
DEF VAR c-nr-pedcli-fin    LIKE ped-item.nr-pedcli INIT "ZZZZZZZZZZZZ".
DEF VAR c-it-codigo-ini    LIKE ped-item.it-codigo INIT ''.
DEF VAR c-it-codigo-fin    LIKE ped-item.it-codigo INIT "ZZZZZZZZZZZZZZZ".
DEF VAR c-cod-refer-ini    LIKE ped-item.cod-refer.
DEF VAR c-cod-refer-fin    LIKE ped-item.cod-refer INIT "ZZZZZZZZZZ".
DEF VAR c-cod-qualid-ini   LIKE ob-etiqueta.cod-qualid.
DEF VAR c-cod-qualid-fin   LIKE ob-etiqueta.cod-qualid     INIT "Z".
DEF VAR c-cod-depos        LIKE saldo-estoq.cod-depos      INIT "ARM".
DEF VAR l-lote-todos       AS LOG INIT YES.
DEF VAR l-lote-pp          AS LOG INIT NO.
DEF VAR l-lote-pd          AS LOG INIT NO.
DEF VAR l-lote-rp          AS LOG INIT NO.
DEF VAR l-lote-rd          AS LOG INIT NO.
DEF VAR l-lote-sc          AS LOG INIT NO.
DEF VAR l-lote-ca          AS LOG INIT NO.
DEF VAR i-situacao         AS INT.
DEF VAR l-ok               AS LOG.
DEF VAR i-row              AS INT.
DEF VAR c-lotes            AS CHAR FORMAT "x(12)".

/* Variaveis da Rotina de Impress∆o */
DEFINE VAR c-empresa LIKE empresa.razao-social.
DEFINE VAR i-saida             AS INT.
DEFINE VAR c-saida             AS CHAR.
DEFINE VAR i-num-copias        AS INT.

/* Includes Editor */
{include/tt-edit.i}
{include/pi-edit.i}

/* ***************************  Definitions  ************************** */
/* INICIO DA DEFINIÄ«O DAS TABELAS TEMPORARIAS; PARA A CHAMADA DO RELATORIO ESPD0002RP.P */
{esinc/espd0002.i}

DEF TEMP-TABLE tt-raw-digita
    FIELD raw-digita      AS RAW.

DEFINE TEMP-TABLE tt-digita NO-UNDO 
       FIELD nr-sequencia     AS   INT FORMAT '>>9'
       FIELD it-codigo        LIKE ped-item.it-codigo
       FIELD cod-refer        LIKE ped-item.cod-refer
       FIELD desc-item        AS   CHAR FORMAT "x(25)"
       FIELD qt-pedida        AS   DEC FORMAT ">>>,>>9.99" 
       FIELD qt-reserva       AS   DEC FORMAT ">>>,>>9.99" 
       FIELD sit-prog         AS   CHAR FORMAT "x(7)"
       INDEX seqped nr-sequencia.

DEFINE VAR raw-param   AS RAW NO-UNDO.
DEFINE VAR rs-execucao AS INTEGER INITIAL 1. 
DEFINE FRAME f-pg-imp rs-execucao AT ROW 5.75 COL 3 NO-LABEL.

/* FIM DAS DEFINIÄÂES DO RELATORIO ESPD0002RP.P */

PROCEDURE FindWindowA EXTERNAL "USER32.DLL":
    DEFINE INPUT  PARAMETER intClassName AS LONG.
    DEFINE INPUT  PARAMETER chrCaption   AS CHARACTER.
    DEFINE RETURN PARAMETER intHandle    AS LONG.
END PROCEDURE.

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
&Scoped-define INTERNAL-TABLES tt-etiquetas tt-ped-item item tt-ped-venda

/* Definitions for BROWSE br-etiquetas                                  */
&Scoped-define FIELDS-IN-QUERY-br-etiquetas tt-etiquetas.num-etiqueta tt-etiquetas.localizacao tt-etiquetas.quantidade   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-etiquetas   
&Scoped-define SELF-NAME br-etiquetas
&Scoped-define QUERY-STRING-br-etiquetas FOR EACH tt-etiquetas NO-LOCK
&Scoped-define OPEN-QUERY-br-etiquetas OPEN QUERY {&SELF-NAME} FOR EACH tt-etiquetas NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-etiquetas tt-etiquetas
&Scoped-define FIRST-TABLE-IN-QUERY-br-etiquetas tt-etiquetas


/* Definitions for BROWSE br-itens                                      */
&Scoped-define FIELDS-IN-QUERY-br-itens tt-ped-item.nr-sequencia tt-ped-item.it-codigo item.desc-item tt-ped-item.cod-refer tt-ped-item.qt-pedida   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-itens   
&Scoped-define SELF-NAME br-itens
&Scoped-define QUERY-STRING-br-itens FOR EACH tt-ped-item WHERE                                  tt-ped-item.nr-pedcli = tt-ped-venda.nr-pedcli NO-LOCK, ~
                                   FIRST item WHERE                                   item.it-codigo = tt-ped-item.it-codigo NO-LOCK                             BY tt-ped-item.nr-pedcli                             by tt-ped-item.nr-sequencia
&Scoped-define OPEN-QUERY-br-itens OPEN QUERY {&SELF-NAME} FOR EACH tt-ped-item WHERE                                  tt-ped-item.nr-pedcli = tt-ped-venda.nr-pedcli NO-LOCK, ~
                                   FIRST item WHERE                                   item.it-codigo = tt-ped-item.it-codigo NO-LOCK                             BY tt-ped-item.nr-pedcli                             by tt-ped-item.nr-sequencia.
&Scoped-define TABLES-IN-QUERY-br-itens tt-ped-item item
&Scoped-define FIRST-TABLE-IN-QUERY-br-itens tt-ped-item
&Scoped-define SECOND-TABLE-IN-QUERY-br-itens item


/* Definitions for BROWSE br-pedidos                                    */
&Scoped-define FIELDS-IN-QUERY-br-pedidos tt-ped-venda.nr-pedcli tt-ped-venda.nome-abrev   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-pedidos   
&Scoped-define SELF-NAME br-pedidos
&Scoped-define QUERY-STRING-br-pedidos FOR EACH tt-ped-venda NO-LOCK
&Scoped-define OPEN-QUERY-br-pedidos OPEN QUERY {&SELF-NAME} FOR EACH tt-ped-venda NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-pedidos tt-ped-venda
&Scoped-define FIRST-TABLE-IN-QUERY-br-pedidos tt-ped-venda


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-br-etiquetas}~
    ~{&OPEN-QUERY-br-itens}~
    ~{&OPEN-QUERY-br-pedidos}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-7 RECT-8 RECT-50 br-pedidos br-itens ~
br-etiquetas bt-par bt-det bt-imprime bt-libera bt-coletor bt-ok 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-4 bt-log bt-vapara bt-consulta bt-imprime-2 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-desc-item w-digita 
FUNCTION fn-desc-item RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-digita AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-coletor 
     IMAGE-UP FILE "image/im-calc3.bmp":U
     LABEL "" 
     SIZE 4.86 BY 1.38 TOOLTIP "Envia Etiquetas para Coletor".

DEFINE BUTTON bt-consulta AUTO-GO 
     IMAGE-UP FILE "image/im-det.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Detalhar Pedido"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-det 
     IMAGE-UP FILE "image/im-det.bmp":U
     LABEL "" 
     SIZE 4.86 BY 1.38 TOOLTIP "Detalha Etiqueta".

DEFINE BUTTON bt-imprime 
     IMAGE-UP FILE "image/im-pri.bmp":U
     LABEL "" 
     SIZE 4.86 BY 1.29 TOOLTIP "Imprime Pedido para Separaá∆o (Bloqueia Pedido)".

DEFINE BUTTON bt-imprime-2 AUTO-GO 
     IMAGE-UP FILE "image/im-prigr.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Imprimir Pedido"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-libera 
     IMAGE-UP FILE "image/im-lib1.bmp":U
     LABEL "" 
     SIZE 4.86 BY 1.29 TOOLTIP "Libera o Pedido para Alteraá∆o".

DEFINE BUTTON bt-log AUTO-GO 
     IMAGE-UP FILE "image/im-log.bmp":U NO-FOCUS
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Alteraá‰es do Pedido"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-ok AUTO-END-KEY 
     IMAGE-UP FILE "image/im-exi.bmp":U
     LABEL "Cancelar" 
     SIZE 4.86 BY 1.13.

DEFINE BUTTON bt-par 
     IMAGE-UP FILE "image/im-fil.bmp":U
     LABEL "Button 3" 
     SIZE 4.86 BY 1.13 TOOLTIP "Seleciona Itens".

DEFINE BUTTON bt-vapara AUTO-GO 
     IMAGE-UP FILE "image/im-vapra.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Posicionar no Pedido"
     BGCOLOR 8 FONT 10.

DEFINE RECTANGLE RECT-50
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 27 BY 1.63.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 7 BY 8.25
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 90 BY 18.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-etiquetas FOR 
      tt-etiquetas SCROLLING.

DEFINE QUERY br-itens FOR 
      tt-ped-item, 
      item SCROLLING.

DEFINE QUERY br-pedidos FOR 
      tt-ped-venda SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-etiquetas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-etiquetas w-digita _FREEFORM
  QUERY br-etiquetas NO-LOCK DISPLAY
      tt-etiquetas.num-etiqueta COLUMN-LABEL "Etiqueta" FORMAT "999999999":U
      tt-etiquetas.localizacao FORMAT "999/999":U WIDTH 10 
      tt-etiquetas.quantidade COLUMN-LABEL "Qtde (m)" FORMAT ">>>,>>9.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 52 BY 8.17
         FONT 1
         TITLE "Peáas Dispon°veis" ROW-HEIGHT-CHARS .46.

DEFINE BROWSE br-itens
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-itens w-digita _FREEFORM
  QUERY br-itens NO-LOCK DISPLAY
      tt-ped-item.nr-sequencia
      tt-ped-item.it-codigo     FORMAT "X(8)":U       WIDTH 7
      item.desc-item            FORMAT "x(40)"        WIDTH 25
      tt-ped-item.cod-refer     FORMAT "X(8)":U
      tt-ped-item.qt-pedida
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 60 BY 9
         FONT 1.

DEFINE BROWSE br-pedidos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-pedidos w-digita _FREEFORM
  QUERY br-pedidos NO-LOCK DISPLAY
      tt-ped-venda.nr-pedcli  FORMAT "x(12)":U       WIDTH 9
      tt-ped-venda.nome-abrev FORMAT "x(12)":U       WIDTH 13.5
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 27 BY 15.75
         FONT 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     bt-log AT ROW 17.38 COL 23 WIDGET-ID 12
     br-pedidos AT ROW 1.25 COL 2 WIDGET-ID 100
     br-itens AT ROW 1.25 COL 30
     br-etiquetas AT ROW 10.5 COL 30
     bt-par AT ROW 10.75 COL 84
     bt-det AT ROW 11.92 COL 84
     bt-imprime AT ROW 13.33 COL 84 WIDGET-ID 4
     bt-libera AT ROW 14.67 COL 84 WIDGET-ID 22
     bt-coletor AT ROW 16 COL 84 WIDGET-ID 20
     bt-vapara AT ROW 17.38 COL 2.86 WIDGET-ID 14
     bt-consulta AT ROW 17.38 COL 8.29 WIDGET-ID 8
     bt-imprime-2 AT ROW 17.38 COL 13.57 WIDGET-ID 10
     bt-ok AT ROW 17.46 COL 84
     RECT-7 AT ROW 10.5 COL 83 WIDGET-ID 2
     RECT-8 AT ROW 1 COL 1 WIDGET-ID 6
     RECT-50 AT ROW 17.17 COL 2 WIDGET-ID 16
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 102.29 BY 22.79
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
         TITLE              = "Libera Pedido para Separaá∆o"
         COLUMN             = 37.57
         ROW                = 9.33
         HEIGHT             = 18.21
         WIDTH              = 90
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
/* BROWSE-TAB br-pedidos bt-log F-Main */
/* BROWSE-TAB br-itens br-pedidos F-Main */
/* BROWSE-TAB br-etiquetas br-itens F-Main */
/* SETTINGS FOR BUTTON bt-consulta IN FRAME F-Main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR BUTTON bt-imprime-2 IN FRAME F-Main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR BUTTON bt-log IN FRAME F-Main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR BUTTON bt-vapara IN FRAME F-Main
   NO-ENABLE 4                                                          */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-digita)
THEN w-digita:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-etiquetas
/* Query rebuild information for BROWSE br-etiquetas
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-etiquetas NO-LOCK.
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
OPEN QUERY {&SELF-NAME} FOR EACH tt-ped-item WHERE
                                 tt-ped-item.nr-pedcli = tt-ped-venda.nr-pedcli NO-LOCK,
                            FIRST item WHERE
                                  item.it-codigo = tt-ped-item.it-codigo NO-LOCK
                            BY tt-ped-item.nr-pedcli
                            by tt-ped-item.nr-sequencia.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-itens */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-pedidos
/* Query rebuild information for BROWSE br-pedidos
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-ped-venda NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-pedidos */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-digita w-digita
ON END-ERROR OF w-digita /* Libera Pedido para Separaá∆o */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-digita w-digita
ON WINDOW-CLOSE OF w-digita /* Libera Pedido para Separaá∆o */
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
ON VALUE-CHANGED OF br-etiquetas IN FRAME F-Main /* Peáas Dispon°veis */
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
   EMPTY TEMP-TABLE tt-etiquetas.
   FOR EACH ob-etiqueta WHERE
            ob-etiqueta.cod-estabel = c-cod-estabel AND
            ob-etiqueta.situacao = 3 AND
            ob-etiqueta.it-codigo = tt-ped-item.it-codigo AND
            ob-etiqueta.cod-refer = tt-ped-item.cod-refer AND 
            ob-etiqueta.quantidade > 0 NO-LOCK.

        IF ob-etiqueta.localizacao <> '' THEN DO.
           FIND ob-localiz WHERE
                ob-localiz.cod-localiz = ob-etiqueta.localizacao NO-LOCK NO-ERROR.
           IF AVAIL ob-localiz THEN DO.
              IF (ob-localiz.tipo = 2 OR /* Amostra */
                  ob-localiz.tipo = 4 OR /* Desenho Exclusivo */ 
                  ob-localiz.tipo = 7 OR /* Bloqueado*/ 
                  ob-localiz.tipo = 9)   /* Pilotagem */
                  THEN NEXT.
           END.
        END.

        CREATE tt-etiquetas.
        ASSIGN tt-etiquetas.num-etiqueta = ob-etiqueta.num-etiqueta
               tt-etiquetas.quantidade = ob-etiqueta.quantidade
               tt-etiquetas.localiz = ob-etiqueta.localiz.

        IF tt-ped-item.qt-pedida > 30 THEN
           ASSIGN tt-ped-item.observacao = IF tt-ped-item.observacao = ''
                                           THEN 'ESTOQUE' ELSE tt-ped-item.observacao.
        ELSE
           IF ob-etiqueta.quantidade > (tt-ped-item.qt-pedida - (tt-ped-item.qt-pedida * 10 / 100)) AND
              ob-etiqueta.quantidade < (tt-ped-item.qt-pedida + (tt-ped-item.qt-pedida * 50 / 100)) THEN
              ASSIGN tt-ped-item.observacao = 'ESTOQUE'.
           ELSE
              ASSIGN tt-ped-item.observacao = 'TRANSFORMAR'.
   END.

   {&OPEN-QUERY-br-etiquetas}
   APPLY 'value-changed' TO br-etiquetas.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-pedidos
&Scoped-define SELF-NAME br-pedidos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-pedidos w-digita
ON CURSOR-DOWN OF br-pedidos IN FRAME F-Main
DO:
   br-pedidos:DESELECT-ROWS().

   br-pedidos:SELECT-FOCUSED-ROW().
   APPLY 'VALUE-CHANGED' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-pedidos w-digita
ON CURSOR-UP OF br-pedidos IN FRAME F-Main
DO:
    br-pedidos:DESELECT-ROWS().

    br-pedidos:SELECT-FOCUSED-ROW().
    APPLY 'VALUE-CHANGED' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-pedidos w-digita
ON ROW-DISPLAY OF br-pedidos IN FRAME F-Main
DO:
    ASSIGN tt-ped-venda.nr-pedcli:FONT IN BROWSE br-pedidos = 1  
           tt-ped-venda.nome-abrev:FONT IN BROWSE br-pedidos = 1
           tt-ped-venda.nr-pedcli:FGCOLOR IN BROWSE br-pedidos = ?  
           tt-ped-venda.nome-abrev:FGCOLOR IN BROWSE br-pedidos = ?.

    IF tt-ped-venda.l-impresso THEN
       ASSIGN tt-ped-venda.nr-pedcli:FONT IN BROWSE br-pedidos = 6  
              tt-ped-venda.nome-abrev:FONT IN BROWSE br-pedidos = 6
              tt-ped-venda.nr-pedcli:FGCOLOR IN BROWSE br-pedidos = 12  
              tt-ped-venda.nome-abrev:FGCOLOR IN BROWSE br-pedidos = 12.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-pedidos w-digita
ON VALUE-CHANGED OF br-pedidos IN FRAME F-Main
DO:
    {&OPEN-QUERY-br-itens}
    APPLY 'value-changed' TO br-itens.

    DISABLE {&list-4} WITH FRAME {&FRAME-NAME}.
    IF AVAIL tt-ped-venda THEN
       ENABLE {&list-4} WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-coletor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-coletor w-digita
ON CHOOSE OF bt-coletor IN FRAME F-Main
DO:
  MESSAGE "Deseja Enviar Dados para o Coletor ?" SKIP
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL UPDATE l-enviar AS LOG.

   IF l-enviar THEN DO.
       OUTPUT TO m:/ems206/especificos/seniuz/coletor/enviar/sep.txt.

       DO i-row = 1 TO br-pedidos:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}:
          IF br-pedidos:FETCH-SELECTED-ROW(i-row) THEN DO.

             FOR EACH tt-ped-item WHERE
                      tt-ped-item.nr-pedcli = tt-ped-venda.nr-pedcli NO-LOCK.

                 PUT UNFORMATTED 
                     "*,"
                     tt-ped-venda.nr-pedcli "," 
                     tt-ped-item.nr-sequencia ","
                     tt-ped-item.it-codigo "," 
                     tt-ped-item.cod-refer "," 
                     TRIM(STRING(100 * (tt-ped-item.qt-pedida - (tt-ped-item.qt-pedida * 5 / 100)),">>>,>>9,99")) "," 
                     TRIM(STRING(100 * (tt-ped-item.qt-pedida + (tt-ped-item.qt-pedida * 10 / 100)),">>>,>>9,99")) 
                     SKIP.
    
                 ASSIGN c-docas = ''.
                 FOR EACH ob-etiqueta WHERE
                          ob-etiqueta.cod-estabel = c-cod-estabel AND
                          ob-etiqueta.situacao = 3 AND
                          ob-etiqueta.it-codigo = tt-ped-item.it-codigo AND
                          ob-etiqueta.cod-refer = tt-ped-item.cod-refer AND
                          ob-etiqueta.quantidade > 0 NO-LOCK.

                     PUT UNFORMATTED
                         "#,"  
                         ob-etiqueta.num-etiqueta "," 
                         TRIM(STRING(100 * ob-etiqueta.quantidade,">,>>9,99"))
                         SKIP.
            
                     IF ob-etiqueta.localiz <> '' AND
                        LOOKUP(STRING(ob-etiqueta.localiz,"999/999"),c-docas) = 0 THEN
                        ASSIGN c-docas = IF c-docas = ''
                                         THEN STRING(ob-etiqueta.localiz,"999/999")
                                         ELSE c-docas + ',' + STRING(ob-etiqueta.localiz,"999/999").
                 END.
                 PUT UNFORMATTED 
                     "@," 
                     c-docas
                     SKIP.
             END.
          END.
       END.
       OUTPUT CLOSE.

       IF SESSION:SET-WAIT-STATE("general":U) THEN.
    
       OS-DELETE SILENT VALUE(c-arq-saida).
       IF SEARCH("C:\IMPROTEC\P220\P220.EXE") <> ? THEN DO.
          ASSIGN c-arq-bat = SESSION:TEMP-DIRECTORY + "p220.bat".
          OUTPUT TO VALUE(c-arq-bat).
              PUT "c:" SKIP
                  "cd " SESSION:TEMP-DIRECTORY SKIP
                  "C:\IMPROTEC\P220\P220.EXE E 2 3 "
                  c-arq-saida FORMAT "x(40)" SKIP.
          OUTPUT CLOSE.

          IF SEARCH(c-arq-bat) <> ? THEN DO.
             OS-COMMAND SILENT VALUE(c-arq-bat).
             PAUSE 3 NO-MESSAGE.
             REPEAT. 
                RUN FindWindowA (0, "IMODEM", OUTPUT i-handle).
                IF i-handle = 0 THEN LEAVE.
             END. 
             /*OS-DELETE SILENT VALUE(c-arq-bat). */
          END.
       END.
       IF SESSION:SET-WAIT-STATE("":U) THEN.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-consulta w-digita
ON CHOOSE OF bt-consulta IN FRAME F-Main
DO:
   FIND ped-venda WHERE
        ped-venda.nr-pedcli = tt-ped-venda.nr-pedcli AND
        ped-venda.nome-abrev = tt-ped-venda.nome-abrev NO-LOCK NO-ERROR.
   ASSIGN gr-ped-venda = ROWID(ped-venda).

   ASSIGN w-digita:SENSITIVE = NO.
   RUN esp\espd4000.w (INPUT "Consultar").
   ASSIGN w-digita:SENSITIVE = YES.
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


&Scoped-define SELF-NAME bt-imprime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-imprime w-digita
ON CHOOSE OF bt-imprime IN FRAME F-Main
DO:
  RUN pi-imprime.
  br-pedidos:REFRESH().
  APPLY 'value-changed' TO br-pedidos IN FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-imprime-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-imprime-2 w-digita
ON CHOOSE OF bt-imprime-2 IN FRAME F-Main
DO:
  CREATE tt-param.
  ASSIGN tt-param.usuario          = c-seg-usuario
         tt-param.data-exec        = TODAY
         tt-param.hora-exec        = TIME
         tt-param.destino          = 3
         tt-param.classifica       = 1
         tt-param.arquivo          = SESSION:TEMP-DIRECTORY + c-programa-mg97 + ".tmp"
         tt-param.estabel-ini      = tt-ped-venda.cod-estabel
         tt-param.estabel-fin      = tt-ped-venda.cod-estabel
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
         tt-param.sit-total        = NO
         tt-param.sit-aberto       = YES   
         tt-param.sit-parcial      = YES    
         tt-param.sit-pendentes    = YES    
         tt-param.sit-suspensos    = YES    
         tt-param.sit-cancelados   = NO
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


&Scoped-define SELF-NAME bt-libera
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-libera w-digita
ON CHOOSE OF bt-libera IN FRAME F-Main
DO:
  
  FIND ped-venda-ext WHERE
       ped-venda-ext.cod-estabel = tt-ped-venda.cod-estabel AND   /*  daf  */  
       ped-venda-ext.nr-pedido = INT(tt-ped-venda.nr-pedcli)
       SHARE-LOCK NO-ERROR.
  IF AVAIL ped-venda-ext THEN DO.
     ASSIGN ped-venda-ext.l-etiqueta = NO
            tt-ped-venda.l-impresso = NO.

     RUN esapi/cria-log-pedvenda.p (INPUT tt-ped-venda.nr-pedcli,
                                    INPUT tt-ped-venda.nome-abrev,
                                    INPUT "Liberado para Alteraá∆o", 
                                    INPUT YES).
  END.
  FIND CURRENT ped-venda-ext NO-LOCK NO-ERROR.

  br-pedidos:REFRESH().
  APPLY 'value-changed' TO br-pedidos IN FRAME {&FRAME-NAME}.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-log
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-log w-digita
ON CHOOSE OF bt-log IN FRAME F-Main
DO:
   RUN esp/essp0155b.p (INPUT tt-ped-venda.nr-pedcli).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-digita
ON CHOOSE OF bt-ok IN FRAME F-Main /* Cancelar */
DO:
  apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-par
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-par w-digita
ON CHOOSE OF bt-par IN FRAME F-Main /* Button 3 */
DO:

   ASSIGN w-digita:SENSITIVE = NO.
   RUN esp/essp0204a.w (INPUT-OUTPUT c-cod-estabel,
                        INPUT-OUTPUT c-nr-pedcli-ini,
                        INPUT-OUTPUT c-nr-pedcli-fin,
                        INPUT-OUTPUT c-it-codigo-ini,
                        INPUT-OUTPUT c-it-codigo-fin,
                        INPUT-OUTPUT c-cod-refer-ini,
                        INPUT-OUTPUT c-cod-refer-fin,
                        INPUT-OUTPUT c-cod-qualid-ini,
                        INPUT-OUTPUT c-cod-qualid-fin,
                        INPUT-OUTPUT c-cod-depos,  
                        INPUT-OUTPUT l-lote-todos, 
                        INPUT-OUTPUT l-lote-pp,         
                        INPUT-OUTPUT l-lote-pd,         
                        INPUT-OUTPUT l-lote-rp,         
                        INPUT-OUTPUT l-lote-rd, 
                        INPUT-OUTPUT l-lote-sc,
                        INPUT-OUTPUT l-lote-ca,
                        INPUT-OUTPUT l-ok).

   IF l-ok THEN DO.
      FOR EACH tt-ped-venda.
          DELETE tt-ped-venda.
      END.
      FOR EACH tt-ped-item.
          DELETE tt-ped-item.
      END.

      FOR EACH tt-etiquetas.
          DELETE tt-etiquetas.
      END.

      RUN pi-processa.
   END.
   ASSIGN w-digita:SENSITIVE = YES.
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-vapara
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-vapara w-digita
ON CHOOSE OF bt-vapara IN FRAME F-Main
DO:
  RUN esp/essp0154b.w (OUTPUT c-nr-pedcli).

  IF c-nr-pedcli <> "" THEN DO:
     FIND FIRST tt-ped-venda WHERE
                tt-ped-venda.nr-pedcli = c-nr-pedcli NO-LOCK NO-ERROR.
      IF AVAIL tt-ped-venda THEN DO.
         h-query:REPOSITION-TO-ROWID(ROWID(tt-ped-venda)) NO-ERROR. 
         APPLY 'VALUE-CHANGED' TO br-pedidos IN FRAME {&FRAME-NAME}.
      END.
      ELSE
         MESSAGE "Pedido n∆o est† contido na seleá∆o!"
                 VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.
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
  ENABLE RECT-7 RECT-8 RECT-50 br-pedidos br-itens br-etiquetas bt-par bt-det 
         bt-imprime bt-libera bt-coletor bt-ok 
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

  {utp/ut9000.i "ESSP0204" "2.04.00.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN h-query = br-itens:QUERY IN FRAME {&FRAME-NAME}.

  FIND FIRST para-ped NO-LOCK NO-ERROR.
  ASSIGN c-cod-estabel = para-ped.estab-padrao.

  APPLY 'choose' TO bt-par IN FRAME {&FRAME-NAME}.

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
        "DATA: "                                  AT  63
        STRING(TODAY,"99/99/9999") FORMAT "X(10)" AT  69
        "HORA: "                                  AT  97
        STRING(TIME,"hh:mm:ss")                   AT 103
        "PAG:"                                    AT 126
        i-pag FORMAT ">>>"                        AT 131
        SKIP(1).

    PUT "RELAT‡RIO TRANSFORMAÄ«O PARA CORTES " AT 38
        SKIP(1).


    PUT "Pedido   Cliente      Repres.      Transp.      Item     Descriá∆o                      Refer Qt.Pedida  Quantidade Observaá∆o" AT 1.
    PUT "-------- ------------ ------------ ------------ -------- ------------------------------ ----- ---------- ---------- ----------------" AT 1.
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
  DEF VAR de-qtd-tot AS DEC FORMAT ">>>,>>9.99". 

  RUN utp/ut-utils.p PERSISTENT SET h-prog.

  RUN esapi/saida-imp.p (OUTPUT i-saida,
                         OUTPUT c-saida,
                         OUTPUT i-num-copias,
                         OUTPUT l-ok).

  CASE i-saida:
      WHEN 1 THEN DO.
          OUTPUT TO VALUE(c-saida) CONVERT TARGET "ibm850" PAGED PAGE-SIZE 63.
          PUT CONTROL "~033E~033(s19H".    
      END.
      WHEN 2 THEN
          OUTPUT TO VALUE(c-saida).
      WHEN 3 THEN DO.
          ASSIGN c-saida = SESSION:TEMP-DIRECTORY + "essp0204.tmp".
          OUTPUT TO VALUE(c-saida) CONVERT SOURCE "ibm850".
      END.
  END CASE.

  DO i-ct = 1 TO i-num-copias.
     ASSIGN i-lin = 99
            i-pag =  1.


     DO i-row = 1 TO br-pedidos:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}:
        IF br-pedidos:FETCH-SELECTED-ROW(i-row) THEN DO.

           RUN pi-imp-cabec.
           ASSIGN i-lin = 7.
           
           FIND ped-venda WHERE 
                ped-venda.nr-pedcli  = tt-ped-venda.nr-pedcli AND 
                ped-venda.nome-abrev = tt-ped-venda.nome-abrev NO-LOCK NO-ERROR.
           IF ped-venda.cod-sit-com <> 2 THEN DO.
              MESSAGE "Preáo n∆o Aprovado para pedido " + tt-ped-venda.nr-pedcli 
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
              NEXT.          
           END.

           FIND ped-venda-ext WHERE
                ped-venda-ext.cod-estabel = tt-ped-venda.cod-estabel AND  /*  daf  */   
                ped-venda-ext.nr-pedido = INT(tt-ped-venda.nr-pedcli)
                SHARE-LOCK NO-ERROR.
           IF AVAIL ped-venda-ext THEN DO.
              /* 
              IF ped-venda-ext.l-bloqueio THEN DO.
                 MESSAGE 'Pedido Bloqueado, Favor Confirmar Pagamento do Pedido e Desbloquear'
                     VIEW-AS ALERT-BOX INFO BUTTONS OK.
                 NEXT.
              END.
              */
              ASSIGN ped-venda-ext.l-etiqueta = YES
                     tt-ped-venda.l-impresso = YES.

              RUN esapi/cria-log-pedvenda.p (INPUT tt-ped-venda.nr-pedcli,
                                             INPUT tt-ped-venda.nome-abrev,
                                             INPUT "Pedido em Processo de Separaá∆o", 
                                             INPUT YES).
           END.
           FIND CURRENT ped-venda-ext NO-LOCK NO-ERROR.

           PUT tt-ped-venda.nr-pedcli    FORMAT "x(8)"  AT 1
               tt-ped-venda.nome-abrev   FORMAT "x(12)" AT 10
               tt-ped-venda.no-ab-reppri FORMAT "x(12)" AT 23
               tt-ped-venda.nome-transp  FORMAT "x(12)" AT 36.
               

           FOR EACH tt-ped-item WHERE
                    tt-ped-item.nr-pedcli = tt-ped-venda.nr-pedcli NO-LOCK
                    BREAK BY tt-ped-item.nr-sequencia. 
        
               IF i-lin > 63 THEN DO:
                  RUN pi-imp-cabec.
                  ASSIGN i-lin = 7.
               END.
        
               PUT tt-ped-item.it-codigo     FORMAT "x(6)"       AT 49
                   fn-desc-item()            FORMAT "x(25)"      AT 58
                   tt-ped-item.cod-refer     FORMAT "x(5)"       AT 89
                   tt-ped-item.qt-pedida     FORMAT ">>>,>>9.99" AT 95.
        
               ASSIGN de-qtd-tot = 0
                      c-docas = ''.
               FOR EACH ob-etiqueta WHERE
                        ob-etiqueta.cod-estabel = c-cod-estabel AND
                        ob-etiqueta.situacao = 3 AND
                        ob-etiqueta.it-codigo = tt-ped-item.it-codigo AND
                        ob-etiqueta.cod-refer = tt-ped-item.cod-refer AND 
                        ob-etiqueta.quantidade > 0 NO-LOCK.

                   IF ob-etiqueta.localizacao <> '' THEN DO.
                      FIND ob-localiz WHERE
                           ob-localiz.cod-localiz = ob-etiqueta.localizacao NO-LOCK NO-ERROR.
                      IF AVAIL ob-localiz THEN DO.
                         IF (ob-localiz.tipo = 2 OR /* Amostra */
                             ob-localiz.tipo = 4 OR /* Desenho Exclusivo */ 
                             ob-localiz.tipo = 7 OR /* Bloqueado*/ 
                             ob-localiz.tipo = 9)   /* Pilotagem */
                             THEN NEXT.
                      END.
                   END.

                   ASSIGN de-qtd-tot = de-qtd-tot + ob-etiqueta.quantidade.

                   IF tt-ped-item.qt-pedida > 30 THEN
                      ASSIGN tt-ped-item.observacao = IF tt-ped-item.observacao = ''
                                                      THEN 'ESTOQUE' ELSE tt-ped-item.observacao.
                   ELSE DO.
                      IF ob-etiqueta.quantidade > (tt-ped-item.qt-pedida - (tt-ped-item.qt-pedida * 10 / 100)) AND
                         ob-etiqueta.quantidade < (tt-ped-item.qt-pedida + (tt-ped-item.qt-pedida * 50 / 100)) THEN
                         ASSIGN tt-ped-item.observacao = 'ESTOQUE'.
                      ELSE
                         ASSIGN tt-ped-item.observacao = 'TRANSFORMAR'.
                   END.

                   IF ob-etiqueta.localiz <> '' AND
                      LOOKUP(STRING(ob-etiqueta.localiz,"999/999"),c-docas) = 0 THEN
                      ASSIGN c-docas = IF c-docas = ''
                                       THEN STRING(ob-etiqueta.localiz,"999/999")
                                       ELSE c-docas + ',' + STRING(ob-etiqueta.localiz,"999/999").
               END.

               PUT de-qtd-tot AT 106
                   tt-ped-item.observacao AT 117
                   SKIP.
               ASSIGN i-lin = i-lin + 1.

               PUT c-docas FORMAT "x(100)" AT 49
                   SKIP(1).

               ASSIGN i-lin = i-lin + 2.
           END.
           

           IF tt-ped-venda.cod-cond-pag = 1 THEN DO.  /* Somente Ö Vista */
              PUT SKIP(1).
              ASSIGN i-lin = i-lin + 1.

              PUT "COND. PAGTO.: A VISTA"
                   SKIP.
              ASSIGN i-lin = i-lin + 1.
           END.


           PUT SKIP(1).
           ASSIGN i-lin = i-lin + 1.

           PUT "  OBSERVAÄ«O: ".
           RUN pi-print-editor(INPUT REPLACE(REPLACE(tt-ped-venda.observ, CHR(13), " "), CHR(10), " "), INPUT 120).
           FOR EACH tt-editor:
               PUT tt-editor.conteudo FORMAT "x(120)" 
                   SKIP.
               ASSIGN i-lin = i-lin + 1.
           END.

           PUT SKIP(1).
           ASSIGN i-lin = i-lin + 1.

           PUT SKIP(63 - i-lin).

     /*    PUT FILL("-",132) FORMAT "x(132)".    */

           ASSIGN i-lin = 99.
        END.
     END.

     IF i-saida = 3 THEN DO.
        RUN EXECUTE IN h-prog(INPUT "notepad.exe",
                              INPUT c-saida).
        DELETE PROCEDURE h-prog.
     END.
     OUTPUT CLOSE.
  END.
  
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
    
    FOR EACH ped-venda WHERE
             ped-venda.cod-sit-ped = 1 AND
             ped-venda.nr-pedcli  >= c-nr-pedcli-ini AND
             ped-venda.nr-pedcli  <= c-nr-pedcli-fin NO-LOCK,
        EACH ped-item OF ped-venda WHERE
             ped-item.cod-sit-item = 1 AND
             ped-item.it-codigo >= c-it-codigo-ini AND
             ped-item.it-codigo <= c-it-codigo-fin AND
             ped-item.cod-refer >= c-cod-refer-ini AND 
             ped-item.cod-refer <= c-cod-refer-fin NO-LOCK,
       FIRST ped-item-ext WHERE
             ped-item-ext.cod-estabel = ped-venda.cod-estabel AND
             ped-item-ext.nome-abrev = ped-venda.nome-abrev AND
             ped-item-ext.nr-pedcli = ped-venda.nr-pedcli AND 
             ped-item-ext.nr-sequencia = ped-item.nr-sequencia NO-LOCK. 
     
        RUN pi-acompanhar IN h-acomp (INPUT "Pedido: " + ped-item.nr-pedcli +
                                            "  Item: " + ped-item.it-codigo +
                                            "   Ref: " + ped-item.cod-refer).
    
        FIND ped-venda-ext WHERE
             ped-venda-ext.cod-estabel = ped-venda.cod-estabel AND 
             ped-venda-ext.nr-pedido = ped-venda.nr-pedido
             NO-LOCK NO-ERROR.
        IF NOT AVAIL ped-venda-ext THEN NEXT.
        
        IF ped-venda.cod-estabel <> c-cod-estabel THEN NEXT.
    
        ASSIGN l-vrf-preco = YES.
        IF ped-venda-ext.tp-pedido = "Exportaá∆o" OR 
           ped-venda-ext.tp-pedido = "Bonificaá∆o" OR
           ped-venda-ext.tp-pedido = "Doaá∆o" THEN
           ASSIGN l-vrf-preco = NO.

        IF ped-venda-ext.tp-pedido = "Amostra" OR
           ped-venda-ext.tp-pedido = "Amostra Exportaá∆o" THEN DO.
           FIND natur-oper WHERE
                natur-oper.nat-operacao = ped-venda.nat-oper NO-LOCK NO-ERROR.
           IF natur-oper.emite-duplic = NO THEN
              ASSIGN l-vrf-preco = NO.
        END.

        IF l-vrf-preco THEN DO.
           IF ped-venda.cod-sit-com <> 2 THEN NEXT.  /* Preco n∆o Aprovado */

           IF ped-venda.completo = YES AND
              ped-venda.cod-sit-aval <> 2 AND
              ped-venda.cod-sit-aval <> 3 THEN NEXT.
        
           IF ped-venda-ext.l-nao-aprovar THEN NEXT.
        END.

        IF ped-venda.completo = NO AND
           ped-venda-ext.l-etiqueta = NO THEN NEXT.  /* N∆o foi Impresso */

        FIND tt-ped-venda WHERE
             tt-ped-venda.nr-pedcli = ped-venda.nr-pedcli NO-LOCK NO-ERROR.
        IF NOT AVAIL tt-ped-venda THEN DO.
           CREATE tt-ped-venda.
           ASSIGN tt-ped-venda.cod-estabel = ped-venda.cod-estabel
                  tt-ped-venda.nr-pedcli  = ped-item.nr-pedcli
                  tt-ped-venda.nome-abrev = ped-item.nome-abrev
                  tt-ped-venda.no-ab-reppri = ped-venda.no-ab-reppri
                  tt-ped-venda.cod-cond-pag = ped-venda.cod-cond-pag
                  tt-ped-venda.nome-transp = ped-venda.nome-transp
                  tt-ped-venda.observ = ped-venda.observ
                  tt-ped-venda.l-impresso = ped-venda-ext.l-etiqueta.
        END.
        ASSIGN tt-ped-venda.qt-pedida = tt-ped-venda.qt-pedida + ped-item.qt-pedida.
    
        IF tt-ped-venda.l-impresso = NO THEN DO.
           FOR EACH ped-item-res WHERE
                    ped-item-res.cod-estabel = ped-venda.cod-estabel AND
                    ped-item-res.nome-abrev = ped-venda.nome-abrev AND
                    ped-item-res.nr-pedcli = ped-venda.nr-pedcli NO-LOCK.
               IF ped-item-res.qt-pedida > 0 THEN
                  ASSIGN tt-ped-venda.l-impresso = YES.
           END.
        END.


        FIND item-ext WHERE
             item-ext.it-codigo = ped-item.it-codigo NO-LOCK NO-ERROR.
    
        FIND tt-ped-item WHERE
             tt-ped-item.nr-pedcli = ped-venda.nr-pedcli AND
             tt-ped-item.nr-sequencia = ped-item.nr-sequencia
             NO-LOCK NO-ERROR.
    
        IF NOT AVAIL tt-ped-item THEN DO.
           CREATE tt-ped-item.
           ASSIGN tt-ped-item.nr-pedcli = ped-venda.nr-pedcli 
                  tt-ped-item.nr-sequencia = ped-item.nr-sequencia
                  tt-ped-item.it-codigo = ped-item.it-codigo 
                  tt-ped-item.cod-refer = ped-item.cod-refer
                  tt-ped-item.qt-pedida = ped-item.qt-pedida.
        END.
    END.

    
    RUN pi-finalizar IN h-acomp.

    {&OPEN-QUERY-br-pedidos}
    APPLY 'VALUE-CHANGED' TO br-pedidos IN FRAME {&FRAME-NAME}.

    IF NUM-RESULTS("br-pedidos") > 0 THEN
       br-pedidos:SELECT-ROW(1).

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
  {src/adm/template/snd-list.i "tt-ped-venda"}
  {src/adm/template/snd-list.i "tt-ped-item"}
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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-desc-item w-digita 
FUNCTION fn-desc-item RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND ITEM WHERE
       ITEM.it-codigo = tt-ped-item.it-codigo NO-LOCK NO-ERROR.

  RETURN item.desc-item.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

