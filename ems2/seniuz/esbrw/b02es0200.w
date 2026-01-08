&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.


{include/tt-edit.i}
{include/pi-edit.i}


/* ***************************  Definitions  ************************** */
DEFINE BUFFER empresa FOR mgcad.empresa.

/* Temp Table Definitions ---                                           */
DEF TEMP-TABLE tt-itens-sel
    FIELD marca       AS CHAR
    FIELD nr-nota-fis LIKE it-nota-fisc.nr-nota-fis
    FIELD nome-abrev   LIKE nota-fiscal.nome-ab-cli
    FIELD dt-emis     LIKE it-nota-fisc.dt-emis-nota
    FIELD it-codigo   LIKE it-nota-fisc.it-codigo
    FIELD cod-refer   LIKE ITEM.cod-refer
    FIELD nr-lote     LIKE ob-etiqueta.nr-lote
    FIELD qtde-item   AS DECIMAL FORMAT ">,>>>,>>>,>>9.99"
    FIELD vlr-unit    AS DECIMAL FORMAT ">,>>>,>>>,>>9.99"
    FIELD vlr-total   AS DECIMAL FORMAT ">,>>>,>>>,>>9.99"
    FIELD nr-pedcli   LIKE it-nota-fisc.nr-pedcli
    FIELD descricao   AS CHAR FORMAT "x(30)"
    FIELD nr-seq-ped  AS INT
    FIELD manual      AS CHAR
    INDEX indice1 it-codigo cod-refer nr-lote.

DEF TEMP-TABLE tt-nota-fisc LIKE nota-fiscal.
DEF TEMP-TABLE tt-it-nota-fisc LIKE it-nota-fisc.

DEF TEMP-TABLE wt-notas-geradas
    FIELD rw-nota-fiscal AS ROWID.
    
/* Definicao da tabela temporaria tt-notas-geradas, include {dibo/bodi317ef.i1} */
DEF TEMP-TABLE tt-notas-geradas NO-UNDO
    FIELD rw-nota-fiscal AS   ROWID
    FIELD nr-nota        LIKE nota-fiscal.nr-nota-fis
    FIELD seq-wt-docto   LIKE wt-docto.seq-wt-docto.

/* Variaveis do Recebimento Fiscal ---                                  */
DEF TEMP-TABLE tt-docum-est NO-UNDO LIKE docum-est 
    FIELD r-rowid AS ROWID.

DEF TEMP-TABLE tt-item-doc-est NO-UNDO LIKE item-doc-est
    FIELD r-rowid AS ROWID.
DEF TEMP-TABLE wt-item-doc-est NO-UNDO LIKE item-doc-est
    FIELD r-rowid AS ROWID.

DEF TEMP-TABLE tt-dupli-apagar NO-UNDO LIKE dupli-apagar
    FIELD r-rowid AS ROWID.

DEF TEMP-TABLE tt-rat-lote NO-UNDO LIKE rat-lote
    FIELD r-rowid AS ROWID.
DEF TEMP-TABLE wt-rat-lote NO-UNDO LIKE rat-lote
    FIELD r-rowid AS ROWID.

DEF TEMP-TABLE tt-item-devol-cli no-undo
    FIELD rw-it-nota-fisc   AS ROWID
    FIELD quant-devol       LIKE item-doc-est.quantidade
    FIELD preco-devol       LIKE item-doc-est.preco-total EXTENT 0
    FIELD cod-depos         LIKE item-doc-est.cod-depos
    FIELD reabre-pd         LIKE item-doc-est.reabre-pd
    FIELD vl-desconto       LIKE item-doc-est.pr-total-cmi
    FIELD nat-of            AS CHARACTER.

DEF TEMP-TABLE tt-etq-fat NO-UNDO LIKE ob-etiqueta
    FIELD nr-nota-fis         LIKE it-nota-fisc.nr-nota-fis
    INDEX indice1 localizacao ASCENDING num-etiqueta DESCENDING.
    
DEF TEMP-TABLE tt-etq-dev NO-UNDO LIKE ob-etiqueta
    FIELD nr-nota-fis         LIKE it-nota-fisc.nr-nota-fis
    INDEX indice1 localizacao ASCENDING num-etiqueta DESCENDING.

DEF BUFFER b-tt-notas-geradas FOR tt-notas-geradas.
DEF BUFFER b-tt-etq-dev FOR tt-etq-dev.
DEF BUFFER b-tt-itens-sel FOR tt-itens-sel.

/* Temp-Table tt-erro Definitions ---*/
{method/dbotterr.i}

/* Global Variable Definitions ---                                       */
DEF NEW GLOBAL SHARED VAR h-essp0204     AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-b01es0204    AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-b02es0204    AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR gr-nota-fiscal AS ROWID NO-UNDO.
DEF NEW GLOBAL SHARED VAR gr-ped-venda   AS ROWID NO-UNDO.
DEF NEW GLOBAL SHARED VAR gr-emitente    AS ROWID NO-UNDO.
DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.
DEF NEW GLOBAL SHARED VAR l-implanta AS LOGICAL INIT NO.
DEF NEW GLOBAL SHARED VAR wh-window  AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR adm-broker-hdl AS HANDLE NO-UNDO.

/* Local Variable Definitions ---                                        */
DEF VAR wh-pesquisa   AS WIDGET-HANDLE.
DEF VAR i-item        AS INT.
DEF VAR c-nr-nota-fis AS CHAR.
DEF VAR c-nat-oper-nf AS CHAR.
DEF VAR c-nf-cli      AS CHAR.
DEF VAR c-serie       AS CHAR.
DEF VAR c-chave       AS CHAR.

/* Variaveis para o Excel */
DEF VAR chExcelApp  AS COM-HANDLE NO-UNDO.
DEF VAR chWorkbook  AS COM-HANDLE NO-UNDO.
DEF VAR chworksheet AS COM-HANDLE NO-UNDO.
DEF VAR c-lin       AS CHAR FORMAT "x(500)".
DEF VAR arq-saida   AS CHAR FORMAT "x(50)".

/* Variaveis da Rotina de Impress∆o */
DEF VAR l-ok         AS LOG.
DEF VAR i-saida      AS INT.
DEF VAR c-saida      AS CHAR.
DEF VAR i-num-copias AS INT.

DEF VAR c-cod-estabel     AS CHAR.
DEF VAR c-serie-orig      AS CHAR.
DEF VAR de-total-qtd      AS DEC.
DEF VAR de-total-vlr      AS DEC.
DEF VAR c-nat-oper        AS CHAR NO-UNDO.
DEF VAR l-snfd            AS LOG.
DEF VAR c-lotes           AS CHAR NO-UNDO.
DEF VAR c-empresa         AS CHAR NO-UNDO.
DEF VAR c-titulo          AS CHAR NO-UNDO.
DEF VAR i-Lin             AS INTEGER.
DEF VAR l-sintetico       AS LOG.
DEF VAR l-enviou-email    AS LOG.

DEF VAR h-prog         AS HANDLE NO-UNDO.
DEF VAR i-ct           AS INT.
DEF VAR i-pag          AS INT.
DEF VAR c-corte-comerc AS CHAR.

DEF VAR c-result        AS CHAR.
DEF VAR i-cont          AS INT.
DEF VAR de-qtd-devol-it AS DEC.
DEF VAR de-vlr-devol-it AS DEC.

DEF VAR c-texto-msg AS CHAR.
DEF VAR c-doca-msg AS CHAR.
DEF VAR c-autorizacao AS CHAR.
DEF VAR w-livre       AS WIDGET-HANDLE NO-UNDO.
DEF VAR h-acomp       AS HANDLE.
DEF VAR d-qtde-trf    AS DECIMAL.
DEF VAR i-ult-seq     AS INT.
DEF VAR i-resto       AS INT.
DEF VAR i-nr-seq-ped  AS INT.
DEF VAR i-nr-seq      AS INTEGER.
DEF VAR c-mensagem    AS CHAR.
DEF VAR c-arq-email   AS CHAR FORMAT "x(45)".
DEF VAR i-row         AS INTEGER.
DEF VAR d-qtd-tot-sel AS DECIMAL.
DEF VAR d-vlr-tot-sel AS DECIMAL.
DEF VAR d-qtd-tot-dev AS DECIMAL.
DEF VAR d-vlr-tot-dev AS DECIMAL.
DEF VAR de-tot-etq-dev AS DECIMAL.
DEF VAR de-tot-valor  AS DEC.
DEF VAR de-tot-qtde   AS DEC.
DEF VAR l-erro        AS LOG.

DEF VAR h-boin090     AS HANDLE.
DEF VAR h-boin176     AS HANDLE.
DEF VAR h-boin367     AS HANDLE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-etq-dev

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-etq-dev tt-etq-fat tt-itens-sel

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br-etq-dev                                    */
&Scoped-define FIELDS-IN-QUERY-br-etq-dev tt-etq-dev.localizacao tt-etq-dev.num-etiqueta tt-etq-dev.nuance tt-etq-dev.cod-qualid tt-etq-dev.quantidade   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-etq-dev   
&Scoped-define SELF-NAME br-etq-dev
&Scoped-define OPEN-QUERY-br-etq-dev RUN pi-totais-etq-dev. OPEN QUERY {&SELF-NAME} FOR EACH tt-etq-dev WHERE                                  tt-etq-dev.nr-nota-fis = tt-itens-sel.nr-nota-fis AND                                  tt-etq-dev.it-codigo = tt-itens-sel.it-codigo AND                                  tt-etq-dev.cod-refer = tt-itens-sel.cod-refer AND                                  tt-etq-dev.nr-lote = tt-itens-sel.nr-lote                                  NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-etq-dev tt-etq-dev
&Scoped-define FIRST-TABLE-IN-QUERY-br-etq-dev tt-etq-dev


/* Definitions for BROWSE br-etq-fat                                    */
&Scoped-define FIELDS-IN-QUERY-br-etq-fat tt-etq-fat.localizacao tt-etq-fat.num-etiqueta tt-etq-fat.nuance tt-etq-fat.cod-qualid tt-etq-fat.quantidade   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-etq-fat   
&Scoped-define SELF-NAME br-etq-fat
&Scoped-define QUERY-STRING-br-etq-fat FOR EACH tt-etq-fat NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-etq-fat OPEN QUERY {&SELF-NAME} FOR EACH tt-etq-fat NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-etq-fat tt-etq-fat
&Scoped-define FIRST-TABLE-IN-QUERY-br-etq-fat tt-etq-fat


/* Definitions for BROWSE br-it-sel                                     */
&Scoped-define FIELDS-IN-QUERY-br-it-sel tt-itens-sel.nr-nota-fis tt-itens-sel.dt-emis tt-itens-sel.it-codigo tt-itens-sel.cod-refer tt-itens-sel.nr-lote tt-itens-sel.qtd tt-itens-sel.vlr-unit tt-itens-sel.vlr-tot tt-itens-sel.nr-pedcli tt-itens-sel.descricao   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-it-sel   
&Scoped-define SELF-NAME br-it-sel
&Scoped-define OPEN-QUERY-br-it-sel RUN pi-totais-sel. OPEN QUERY {&SELF-NAME} FOR EACH tt-itens-sel NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-it-sel tt-itens-sel
&Scoped-define FIRST-TABLE-IN-QUERY-br-it-sel tt-itens-sel


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-br-etq-dev}~
    ~{&OPEN-QUERY-br-etq-fat}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-it-sel fi-cod-devol bt-nf bt-fornec ~
bt-imp-item ebt-excel bt-devolucao br-etq-fat bt-email fi-cod-transp ~
cb-frete bt-status bt-right bt-left RECT-65 RECT-68 RECT-69 br-etq-dev 
&Scoped-Define DISPLAYED-OBJECTS fi-cod-devol fi-sel-vlr-total ~
fi-sel-qtd-total fi-cod-transp fi-nome-transp cb-frete fi-motivo ~
fi-etq-dev-qtd-total 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-6 ebt-excel 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" B-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS>
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "",
     Keys-Supplied = ""':U).
/**************************
</EXECUTING-CODE> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Advanced Query Options" B-table-Win _INLINE
/* Actions: ? adm/support/advqedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<SORTBY-OPTIONS>
</SORTBY-OPTIONS> 
<SORTBY-RUN-CODE>
************************
* Set attributes related to SORTBY-OPTIONS */
RUN set-attribute-list (
    'SortBy-Options = ""':U).
/************************
</SORTBY-RUN-CODE> 
<FILTER-ATTRIBUTES>
</FILTER-ATTRIBUTES> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-devolucao 
     IMAGE-UP FILE "image/im-desc.bmp":U
     LABEL "" 
     SIZE 8 BY 2.42 TOOLTIP "Relatorio das Etiquetas Devolvidas"
     BGCOLOR 8 .

DEFINE BUTTON bt-email 
     IMAGE-UP FILE "image/im-email.bmp":U
     LABEL "Button 6" 
     SIZE 8 BY 2 TOOLTIP "Gerar Emails".

DEFINE BUTTON bt-fornec 
     IMAGE-UP FILE "image/im-usu.bmp":U
     LABEL "Button 5" 
     SIZE 8 BY 1.5 TOOLTIP "Consulta Cliente".

DEFINE BUTTON bt-imp-item 
     IMAGE-UP FILE "image/im-pri.bmp":U
     LABEL "" 
     SIZE 8 BY 1.5 TOOLTIP "Imprimir Relat¢rio".

DEFINE BUTTON bt-left 
     IMAGE-UP FILE "image/im-pre.bmp":U
     LABEL "Button 2" 
     SIZE 6 BY 1.33.

DEFINE BUTTON bt-nf AUTO-GO 
     IMAGE-UP FILE "image/im-det.bmp":U
     LABEL "" 
     SIZE 8 BY 2.04 TOOLTIP "Consulta Pedido de Venda da Devoluá∆o"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-right 
     IMAGE-UP FILE "image\im-nex.bmp":U
     LABEL "Button 1" 
     SIZE 6 BY 1.38.

DEFINE BUTTON bt-status AUTO-GO 
     IMAGE-UP FILE "image/imt-chck1.bmp":U
     LABEL "" 
     SIZE 8 BY 3.5 TOOLTIP "Confirma Devoluá∆o"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON ebt-excel 
     IMAGE-UP FILE "image/excel.bmp":U
     LABEL "Button 2" 
     SIZE 8 BY 1.75 TOOLTIP "Gerar Planilha Excel".

DEFINE VARIABLE cb-frete AS INTEGER FORMAT "9":U INITIAL 0 
     LABEL "Frete" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "",0,
                     "Cif",1,
                     "Fob",2,
                     "Outros",3
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cod-devol AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Cod. Devoluá∆o" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-transp AS CHARACTER FORMAT "X(256)":U 
     LABEL "Transportadora" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 NO-UNDO.

DEFINE VARIABLE fi-etq-dev-qtd-total AS DECIMAL FORMAT "->,>>>,>>9.99":U INITIAL 0 
     LABEL "Quant. Total" 
     VIEW-AS FILL-IN 
     SIZE 10.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-motivo AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 84.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nome-transp AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 43.43 BY .88 NO-UNDO.

DEFINE VARIABLE fi-sel-qtd-total AS DECIMAL FORMAT "->,>>>,>>9.99":U INITIAL 0 
     LABEL "Quant. Total" 
     VIEW-AS FILL-IN 
     SIZE 10.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-sel-vlr-total AS DECIMAL FORMAT "->,>>>,>>9.99":U INITIAL 0 
     LABEL "Valor Total" 
     VIEW-AS FILL-IN 
     SIZE 10.57 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-65
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 10 BY 8
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-68
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 118 BY 3.

DEFINE RECTANGLE RECT-69
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 10 BY 12.75
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-etq-dev FOR 
      tt-etq-dev SCROLLING.

DEFINE QUERY br-etq-fat FOR 
      tt-etq-fat SCROLLING.

DEFINE QUERY br-it-sel FOR 
      tt-itens-sel SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-etq-dev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-etq-dev B-table-Win _FREEFORM
  QUERY br-etq-dev NO-LOCK DISPLAY
      tt-etq-dev.localizacao  FORMAT "999/999":U   COLUMN-LABEL "Localiz"  WIDTH 8
      tt-etq-dev.num-etiqueta FORMAT "999999999":U COLUMN-LABEL "Etiqueta" WIDTH 11
      tt-etq-dev.nuance       FORMAT " X(2)":U     COLUMN-LABEL "Nc"       WIDTH 5
      tt-etq-dev.cod-qualid   FORMAT "X(2)":U      COLUMN-LABEL "Qld"      WIDTH 7 
      tt-etq-dev.quantidade   FORMAT ">>9.99":U    COLUMN-LABEL "Qtde (m)" WIDTH 12
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 54 BY 8.25
         FONT 1
         TITLE "Etiquetas Devolvidas" ROW-HEIGHT-CHARS .58.

DEFINE BROWSE br-etq-fat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-etq-fat B-table-Win _FREEFORM
  QUERY br-etq-fat NO-LOCK DISPLAY
      tt-etq-fat.localizacao  FORMAT "999/999":U   COLUMN-LABEL "Localiz"  WIDTH 8
      tt-etq-fat.num-etiqueta FORMAT "999999999":U COLUMN-LABEL "Etiqueta" WIDTH 11
      tt-etq-fat.nuance       FORMAT " X(2)":U     COLUMN-LABEL "Nc"       WIDTH 5
      tt-etq-fat.cod-qualid   FORMAT "X(2)":U      COLUMN-LABEL "Qld"      WIDTH 7 
      tt-etq-fat.quantidade   FORMAT ">>9.99":U    COLUMN-LABEL "Qtde (m)" WIDTH 12
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 56 BY 8.25
         FONT 1
         TITLE "Etiquetas Faturadas" ROW-HEIGHT-CHARS .58.

DEFINE BROWSE br-it-sel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-it-sel B-table-Win _FREEFORM
  QUERY br-it-sel NO-LOCK DISPLAY
      tt-itens-sel.nr-nota-fis COLUMN-LABEL "N.F. Fiscal" WIDTH 10
      tt-itens-sel.dt-emis     COLUMN-LABEL "Dt. Emiss∆o" FORMAT "99/99/9999":U
      tt-itens-sel.it-codigo   COLUMN-LABEL "Item"        FORMAT "x(16)":U WIDTH 10
      tt-itens-sel.cod-refer   COLUMN-LABEL "Refer."
      tt-itens-sel.nr-lote     COLUMN-LABEL "Lote"
      tt-itens-sel.qtd         COLUMN-LABEL "Quantidade"  WIDTH 12
      tt-itens-sel.vlr-unit    COLUMN-LABEL "Valor. Unit" WIDTH 9
      tt-itens-sel.vlr-tot     COLUMN-LABEL "Valor Total" WIDTH 9
      tt-itens-sel.nr-pedcli   COLUMN-LABEL "Pedido"
      tt-itens-sel.descricao   COLUMN-LABEL "Descriá∆o"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 119 BY 8.08
         FONT 1
         TITLE "Itens Selecionados para Devoluá∆o" ROW-HEIGHT-CHARS .58.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br-it-sel AT ROW 1.25 COL 1 WIDGET-ID 200
     fi-cod-devol AT ROW 22.25 COL 19 COLON-ALIGNED WIDGET-ID 118
     bt-nf AT ROW 1.5 COL 122 WIDGET-ID 4
     bt-fornec AT ROW 3.75 COL 122 WIDGET-ID 20
     bt-imp-item AT ROW 7.5 COL 122 WIDGET-ID 6
     ebt-excel AT ROW 5.5 COL 122 WIDGET-ID 10
     fi-sel-vlr-total AT ROW 9.42 COL 67.86 COLON-ALIGNED WIDGET-ID 38
     fi-sel-qtd-total AT ROW 9.46 COL 47.14 COLON-ALIGNED WIDGET-ID 36
     bt-devolucao AT ROW 11.25 COL 122 WIDGET-ID 16
     br-etq-fat AT ROW 10.75 COL 2 WIDGET-ID 400
     bt-email AT ROW 17 COL 122 WIDGET-ID 22
     fi-cod-transp AT ROW 21.25 COL 19 COLON-ALIGNED WIDGET-ID 54
     fi-nome-transp AT ROW 21.25 COL 28.43 COLON-ALIGNED NO-LABEL WIDGET-ID 56
     cb-frete AT ROW 21.25 COL 94 COLON-ALIGNED WIDGET-ID 62
     fi-motivo AT ROW 22.25 COL 25.43 COLON-ALIGNED NO-LABEL WIDGET-ID 60
     bt-status AT ROW 19.75 COL 122 WIDGET-ID 2
     bt-right AT ROW 13.75 COL 59 WIDGET-ID 112
     bt-left AT ROW 15.21 COL 59 WIDGET-ID 114
     fi-etq-dev-qtd-total AT ROW 19.25 COL 104 COLON-ALIGNED WIDGET-ID 116
     br-etq-dev AT ROW 10.75 COL 66 WIDGET-ID 500
     "Dados Complementares" VIEW-AS TEXT
          SIZE 20 BY .75 AT ROW 20.17 COL 5 WIDGET-ID 76
          FGCOLOR 12 FONT 13
     RECT-65 AT ROW 1.25 COL 121 WIDGET-ID 26
     RECT-68 AT ROW 20.5 COL 2 WIDGET-ID 52
     RECT-69 AT ROW 10.75 COL 121 WIDGET-ID 80
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 1 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 22.92
         WIDTH              = 130.72.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit Custom                            */
/* BROWSE-TAB br-it-sel 1 F-Main */
/* BROWSE-TAB br-etq-fat bt-devolucao F-Main */
/* BROWSE-TAB br-etq-dev RECT-69 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON ebt-excel IN FRAME F-Main
   6                                                                    */
/* SETTINGS FOR FILL-IN fi-etq-dev-qtd-total IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-motivo IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nome-transp IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-sel-qtd-total IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-sel-vlr-total IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-etq-dev
/* Query rebuild information for BROWSE br-etq-dev
     _START_FREEFORM
RUN pi-totais-etq-dev.
OPEN QUERY {&SELF-NAME} FOR EACH tt-etq-dev WHERE
                                 tt-etq-dev.nr-nota-fis = tt-itens-sel.nr-nota-fis AND
                                 tt-etq-dev.it-codigo = tt-itens-sel.it-codigo AND
                                 tt-etq-dev.cod-refer = tt-itens-sel.cod-refer AND
                                 tt-etq-dev.nr-lote = tt-itens-sel.nr-lote
                                 NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-etq-dev */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-etq-fat
/* Query rebuild information for BROWSE br-etq-fat
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-etq-fat NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-etq-fat */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-it-sel
/* Query rebuild information for BROWSE br-it-sel
     _START_FREEFORM
RUN pi-totais-sel.
OPEN QUERY {&SELF-NAME} FOR EACH tt-itens-sel NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Query            is NOT OPENED
*/  /* BROWSE br-it-sel */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br-it-sel
&Scoped-define SELF-NAME br-it-sel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-it-sel B-table-Win
ON VALUE-CHANGED OF br-it-sel IN FRAME F-Main /* Itens Selecionados para Devoluá∆o */
DO:
   EMPTY TEMP-TABLE tt-etq-fat.

   IF AVAIL tt-itens-sel  THEN DO.
      FIND FIRST nota-fiscal WHERE
                 nota-fiscal.cod-estabel = c-cod-estabel AND
                 nota-fiscal.serie = c-serie-orig AND
                 nota-fiscal.nr-nota-fis = tt-itens-sel.nr-nota-fis  NO-LOCK NO-ERROR.
      IF AVAIL nota-fiscal THEN DO.
         ASSIGN gr-nota-fiscal = ROWID(nota-fiscal).
         FIND FIRST emitente WHERE
                    emitente.cod-emit = nota-fiscal.cod-emit NO-LOCK NO-ERROR.
         IF AVAIL emitente THEN
            ASSIGN gr-emitente = ROWID(emitente).
      END.

      FIND ped-venda WHERE
           ped-venda.nome-abrev = nota-fiscal.nome-ab-cli AND
           ped-venda.nr-pedcli = nota-fiscal.nr-pedcli  NO-LOCK NO-ERROR.

      FIND ped-item OF ped-venda WHERE
           ped-item.it-codigo = tt-itens-sel.it-codigo  AND
           ped-item.cod-refer = tt-itens-sel.cod-refer NO-LOCK NO-ERROR.

      FOR EACH ped-item-rom WHERE
               ped-item-rom.nome-abrev   = ped-item.nome-abrev AND
               ped-item-rom.nr-pedcli    = ped-item.nr-pedcli  AND
               ped-item-rom.nr-sequencia = ped-item.nr-sequencia NO-LOCK.

          FIND ob-etiqueta WHERE
               ob-etiqueta.cod-estabel  = ped-item-rom.cod-estabel AND
               ob-etiqueta.num-etiqueta = ped-item-rom.num-etiqueta
               NO-LOCK NO-ERROR.

          FIND tt-etq-dev WHERE
               tt-etq-dev.num-etiqueta = ped-item-rom.num-etiqueta NO-ERROR.
          IF AVAIL tt-etq-dev THEN NEXT.

          CREATE tt-etq-fat.
          BUFFER-COPY ob-etiqueta TO tt-etq-fat.
      END.
   END.
   {&OPEN-QUERY-br-etq-fat}
   {&OPEN-QUERY-br-etq-dev}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-devolucao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-devolucao B-table-Win
ON CHOOSE OF bt-devolucao IN FRAME F-Main
DO:
  RUN pi-imprime-etq.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-email
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-email B-table-Win
ON CHOOSE OF bt-email IN FRAME F-Main /* Button 6 */
DO:                                            
   DEF VAR c-mensagem   AS CHAR.
   DEF VAR c-destinatario AS CHAR.
   DEF VAR c-tipo-frete AS CHAR.

   MESSAGE "Deseja Comunicar a Devoluá∆o Recebida Via E-MAIL ?"
       VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE choice AS LOGICAL.

   ASSIGN INPUT FRAME {&FRAME-NAME} fi-motivo.

   ASSIGN c-destinatario = "ti.seniuz@gmail.com".

   IF fi-cod-transp:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" THEN DO.
      MESSAGE 'Favor Informar um Transportadora ! ! !'
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY "ENTRY":U TO fi-cod-transp IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.
   FIND transporte WHERE 
        transporte.nome-abrev = fi-cod-transp:SCREEN-VALUE IN FRAME {&FRAME-NAME} NO-LOCK NO-ERROR.
   IF NOT AVAIL transporte THEN DO.
      MESSAGE 'Transportadora n∆o Cadastrada ! ! ! '
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      APPLY 'ENTRY' TO fi-cod-transp.
      RETURN NO-APPLY.
   END.

   IF cb-frete:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ? THEN DO.
      MESSAGE 'Favor Informar o tipo Frete ! ! !'
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY "ENTRY":U TO cb-frete IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.
   

   IF fi-motivo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" THEN DO.
      MESSAGE 'Favor Informar um Motivo da Devoluá∆o ! ! !'
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY "ENTRY":U TO fi-motivo IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.
   IF LENGTH(fi-motivo) < 10  THEN DO.
      MESSAGE 'Favor Informar um Texto de no m°nimo 10 Caracteres' SKIP
              'Justificando a Alteraá∆o...' 
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY "ENTRY":U TO fi-motivo IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.

   FIND repres WHERE
        repres.cod-rep = emitente.cod-rep NO-LOCK NO-ERROR.

   CASE choice:
      WHEN FALSE THEN 
         RETURN NO-APPLY.
      WHEN TRUE THEN DO:
           RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
           {utp/ut-liter.i Enviando_Emails *}
           RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

           CASE cb-frete:SCREEN-VALUE IN FRAME {&FRAME-NAME}:
               WHEN '1' THEN
                   ASSIGN c-tipo-frete = 'CIF'.
               WHEN '2' THEN
                   ASSIGN c-tipo-frete = 'FOB'.
               WHEN '3' THEN
                   ASSIGN c-tipo-frete = 'OUTROS'.
           END CASE.

           ASSIGN c-mensagem = "Prezados(as) Senhores(as)," + CHR(13) + CHR(13) +
                  "Em anexo arquivo com dados da devoluá∆o recebida do cliente " +
                   UPPER(TRIM(emitente.nome-emit)) + "." +  CHR(13) + CHR(13) +
                  "Expediá∆o PARAOPEBA" + CHR(13) +
                  "MED TEXTIL IMPORTAÄ«O E EXPORTAÄ«O LTDA".
        
           ASSIGN  i-lin = 99
                   i-pag =  1.
        
           ASSIGN c-arq-email = SESSION:TEMP-DIRECTORY + "Devolucao Cliente " + STRING(emitente.cod-emit, ">>>,>>9") + ".txt".
           OUTPUT TO VALUE(c-arq-email) CONVERT SOURCE "ibm850".
           FOR EACH b-tt-itens-sel NO-LOCK
               BREAK BY b-tt-itens-sel.nr-nota-fis.
        
               FIND FIRST ITEM WHERE
                          ITEM.it-codigo = b-tt-itens-sel.it-codigo NO-LOCK NO-ERROR.

               RUN pi-acompanhar IN h-acomp (INPUT "Nota Fiscal: " + b-tt-itens-sel.nr-nota-fis + "   " + 
                                                   "Itens: "       + b-tt-itens-sel.it-codigo).
        
               /*
               IF FIRST-OF(b-tt-itens-sel.nr-nota-fis) THEN DO:
                  ASSIGN c-arq-email = SESSION:TEMP-DIRECTORY + "Devolucao Cliente " + STRING(emitente.cod-emit, ">>>,>>9") + ".txt".
                  OUTPUT TO VALUE(c-arq-email) CONVERT SOURCE "ibm850".
               END.
               */
               IF i-lin > 61 THEN DO:
                  RUN pi-imp-cabec-email.
                  ASSIGN i-lin = 10.
               END.
        
               PUT STRING(b-tt-itens-sel.nr-nota-fis)                          AT   1 
                   b-tt-itens-sel.dt-emis     FORMAT "99/99/9999"              AT  10
                   b-tt-itens-sel.it-codigo   FORMAT "x(6)"                    AT  22
                   b-tt-itens-sel.descricao   FORMAT "x(48)"                   AT  29
                   STRING(b-tt-itens-sel.cod-refer, "99.9999-9") FORMAT "X(9)" AT  78
                   b-tt-itens-sel.nr-lote     FORMAT "x(4)"                    AT  89
                   b-tt-itens-sel.qtde-item   FORMAT ">>>,>>9.99"              AT  94
                   b-tt-itens-sel.vlr-unit    FORMAT ">>>,>>9.99"              AT 106
                   b-tt-itens-sel.vlr-total   FORMAT ">>>,>>9.99"              AT 118
                   b-tt-itens-sel.nr-pedcli   FORMAT "99999999"                AT 129.
                  
               ASSIGN i-lin = i-lin + 1.
               ACCUMULATE b-tt-itens-sel.qtde-item (TOTAL BY b-tt-itens-sel.nr-nota-fis).
               ACCUMULATE b-tt-itens-sel.vlr-total (TOTAL BY b-tt-itens-sel.nr-nota-fis).

               PUT SKIP.
               ASSIGN i-lin = i-lin + 1.
              
               IF LAST-OF(b-tt-itens-sel.nr-nota-fis) THEN DO:
                  IF i-lin > 61 THEN DO:
                     RUN pi-imp-cabec-email.
                     ASSIGN i-lin = 10.
                  END.

                  PUT "----------             -----------"  AT 94.
                  PUT "TOTAL DA NOTA FISCAL.............:" FORMAT "x(34)" AT 59.
                  PUT ACCUM TOTAL BY b-tt-itens-sel.nr-nota-fis b-tt-itens-sel.qtde-item FORMAT ">>>,>>9.99"  AT  94.
                  PUT ACCUM TOTAL BY b-tt-itens-sel.nr-nota-fis b-tt-itens-sel.vlr-total FORMAT ">>>,>>9.99"  AT 118.
                  PUT SKIP.
                  ASSIGN de-total-qtd = de-total-qtd + (ACCUM TOTAL BY b-tt-itens-sel.nr-nota-fis b-tt-itens-sel.qtde-item)
                         de-total-vlr = de-total-vlr + (ACCUM TOTAL BY b-tt-itens-sel.nr-nota-fis b-tt-itens-sel.vlr-total)
                         i-lin = i-lin + 1.
               END.
           END.
           PUT SKIP(1).
           ASSIGN i-lin = i-lin + 1.
           PUT "TOTAL GERAL DA DEVOLUÄ«O.........:" FORMAT "x(34)" AT 59.
           PUT de-total-qtd FORMAT ">>>,>>9.99"  AT  94.
           PUT de-total-vlr FORMAT ">>>,>>9.99"  AT 118.
           PUT SKIP(1).

           PUT "TRANSPORTADORA..: " + fi-cod-transp:SCREEN-VALUE IN FRAME {&FRAME-NAME} + " - " 
                                    + fi-nome-transp:SCREEN-VALUE IN FRAME {&FRAME-NAME} FORMAT "x(60)" AT 1.
           PUT "TIPO DO FRETE...: " + c-tipo-frete FORMAT "x(50)" AT 1.
           PUT "MOTIVO DEVOLUCAO: " FORMAT "x(17)" AT 1.
           RUN pi-print-editor(INPUT REPLACE(REPLACE(TRIM(fi-motivo:SCREEN-VALUE IN FRAME {&FRAME-NAME}),CHR(13)," "),CHR(10)," "), INPUT 110). 
           FOR EACH tt-editor:
               PUT tt-editor.conteudo AT 19 SKIP.
               ASSIGN i-lin = i-lin + 1.
           END.
           OUTPUT CLOSE.

           RUN esapi/esapi002.p (INPUT "imatextil@imatextil.com.br", /* e-mail remetente */
                                 INPUT c-destinatario,  /* e-mail destinat†rio */ 
                                 INPUT "DEVOLUCAO DE CLIENTE" ,        /* Assunto  */
                                 INPUT c-mensagem,                     /* Mensagem */
                                 INPUT c-arq-email,                    /*arquivo anexo */
                                 INPUT YES).                           /* Mostra Erros */

           ASSIGN i-lin          = 99
                  l-enviou-email = YES.
                  i-pag          =  1.

           RUN pi-finalizar IN h-acomp.
           MESSAGE 'Email(s) Enviado(s) com Sucesso....'
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
           APPLY "CLOSE":U TO THIS-PROCEDURE.
      END.
   END CASE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-fornec
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-fornec B-table-Win
ON CHOOSE OF bt-fornec IN FRAME F-Main /* Button 5 */
DO:
  ASSIGN CURRENT-WINDOW:SENSITIVE = NO.
     RUN cdp/cd1022.w.
  ASSIGN CURRENT-WINDOW:SENSITIVE = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-imp-item
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-imp-item B-table-Win
ON CHOOSE OF bt-imp-item IN FRAME F-Main
DO:
  RUN pi-imp-item-sel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-left
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-left B-table-Win
ON CHOOSE OF bt-left IN FRAME F-Main /* Button 2 */
DO:
  DO i-row = 1 TO br-etq-dev:NUM-SELECTED-ROWS:
     IF br-etq-dev:FETCH-SELECTED-ROW(i-row) THEN DO.
        CREATE tt-etq-fat.
        BUFFER-COPY tt-etq-dev TO tt-etq-fat.
        DELETE tt-etq-dev.       
     END.
  END.
  {&OPEN-QUERY-br-etq-dev}
  {&OPEN-QUERY-br-etq-fat}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-nf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-nf B-table-Win
ON CHOOSE OF bt-nf IN FRAME F-Main
DO:
    FIND ped-venda WHERE
         ped-venda.nr-pedcli = tt-itens-sel.nr-pedcli NO-LOCK NO-ERROR.
    ASSIGN gr-ped-venda = ROWID(ped-venda).

    IF AVAIL ped-venda THEN 
       RUN esp\espd4000.w (INPUT "consultar").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-right
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-right B-table-Win
ON CHOOSE OF bt-right IN FRAME F-Main /* Button 1 */
DO:
  DO i-row = 1 TO  br-etq-fat:NUM-SELECTED-ROWS.
     IF br-etq-fat:FETCH-SELECTED-ROW(i-row) THEN DO.
        FIND tt-etq-dev WHERE
             tt-etq-dev.num-etiqueta = tt-etq-fat.num-etiqueta NO-ERROR.
        IF NOT AVAIL tt-etq-dev THEN DO.
           CREATE tt-etq-dev.
           BUFFER-COPY tt-etq-fat TO tt-etq-dev
               ASSIGN tt-etq-dev.nr-nota-fis = tt-itens-sel.nr-nota-fis 
                      tt-etq-dev.it-codigo = tt-itens-sel.it-codigo 
                      tt-etq-dev.cod-refer = tt-itens-sel.cod-refer 
                      tt-etq-dev.nr-lote = tt-itens-sel.nr-lote.
        END.
        DELETE tt-etq-fat.
     END.
  END.
  {&OPEN-QUERY-br-etq-fat}
  {&OPEN-QUERY-br-etq-dev}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-status
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-status B-table-Win
ON CHOOSE OF bt-status IN FRAME F-Main
DO:
  ASSIGN INPUT FRAME {&FRAME-NAME} fi-sel-vlr-total   
                                   fi-sel-qtd-total  
                                   fi-etq-dev-qtd-total
                                   fi-nome-transp
                                   fi-cod-transp
                                   cb-frete
                                   fi-cod-devol
                                   fi-motivo.
  RUN pi-validate.
  IF RETURN-VALUE = 'ADM-ERROR' THEN
     RETURN NO-APPLY.

  DO TRANSACTION:
     RUN pi-recebimento.
     IF RETURN-VALUE = "ADM-ERROR" THEN 
        UNDO, RETURN NO-APPLY.
  END.

  FIND docum-est WHERE
       docum-est.ce-atual = NO AND
       docum-est.cod-emit = emitente.cod-emit AND
       docum-est.serie-docto = c-serie  AND
       docum-est.cod-observ = 3 
       USE-INDEX atual NO-LOCK NO-ERROR.

  IF NOT AVAIL docum-est THEN RETURN NO-APPLY.
  ASSIGN docum-est.int-2 = fi-cod-devol.

  RUN pi-atualiza-recto.
  RUN pi-retorna-etq. 
  RUN pi-limpa.
  RUN pi-select-page IN h-essp0204 (INPUT 1).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ebt-excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ebt-excel B-table-Win
ON CHOOSE OF ebt-excel IN FRAME F-Main /* Button 2 */
DO:
   ASSIGN arq-saida = SESSION:TEMP-DIRECTORY + "Analise Devoluá∆o Itens.XLS".
   RUN esdlg/d01essp0200.w (INPUT-OUTPUT arq-saida).

   IF arq-saida <> "" THEN DO:
      RUN pi-gera-excel (INPUT arq-saida).
      MESSAGE "O arquivo foi gerado em " TRIM(arq-saida) + "." SKIP 
              "Para acess†-lo,  abra-o atravÇs do Excel."
          VIEW-AS ALERT-BOX INFO BUTTONS OK. 
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-devol
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-devol B-table-Win
ON LEAVE OF fi-cod-devol IN FRAME F-Main /* Cod. Devoluá∆o */
DO:
  FIND cod-rejeicao WHERE 
       cod-rejeicao.codigo-rejei = INPUT FRAME {&FRAME-NAME} fi-cod-devol NO-LOCK NO-ERROR.
  IF NOT AVAIL cod-rejeicao THEN DO:
     MESSAGE "C¢digo de Rejeiá∆o n∆o Cadastrado..."
             VIEW-AS ALERT-BOX ERROR BUTTONS OK.
     APPLY 'ENTRY' TO SELF.
     RETURN NO-APPLY.
  END.
  ASSIGN fi-motivo:SCREEN-VALUE = UPPER(cod-rejeicao.descricao).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-devol B-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-cod-devol IN FRAME F-Main /* Cod. Devoluá∆o */
DO:
  {include/zoomvar.i &prog-zoom=inzoom/z01in047.r
                     &campo     = fi-cod-devol
                     &campozoom = codigo-rejei
                     &campo2    = fi-motivo
                     &campozoom2 = descricao}
                     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-transp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-transp B-table-Win
ON LEAVE OF fi-cod-transp IN FRAME F-Main /* Transportadora */
DO:
   FIND transporte WHERE 
        transporte.nome-abrev = INPUT FRAME {&FRAME-NAME} fi-cod-transp NO-LOCK NO-ERROR.
   IF NOT AVAIL transporte THEN
      FIND transporte WHERE 
           STRING(transporte.cod-transp) = INPUT FRAME {&FRAME-NAME} fi-cod-transp NO-LOCK NO-ERROR.

   IF AVAIL transporte THEN 
      ASSIGN fi-cod-transp:SCREEN-VALUE IN FRAME {&FRAME-NAME} = transporte.nome-abrev
             fi-nome-transp:SCREEN-VALUE IN FRAME {&FRAME-NAME} = transporte.nome.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-transp B-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-cod-transp IN FRAME F-Main /* Transportadora */
DO:
  {include/zoomvar.i &prog-zoom = adzoom\z01ad268.w
                     &campo     = fi-cod-transp
                     &campozoom = nome-abrev}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-etq-dev
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

fi-cod-transp:LOAD-MOUSE-POINTER("image/lupa.cur").
fi-cod-devol:LOAD-MOUSE-POINTER("image/lupa.cur").

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-open-query-cases B-table-Win 
PROCEDURE adm-open-query-cases :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

{&OPEN-QUERY-{&BROWSE-NAME}}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available B-table-Win  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI B-table-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-abre-excel B-table-Win 
PROCEDURE pi-abre-excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
CREATE "Excel.Application" chExcelApp NO-ERROR.
 IF chExcelApp <> ? THEN /* Cria a Planilha */
    ASSIGN chExcelApp:VISIBLE     = TRUE  /* A Planilha Ficar† Visivel */
           chExcelApp:SheetsInNewWorkbook = 1 /* Nı PLANILHAS A SEREM CRIADAS */
           chWorkbook             = chExcelApp:Workbooks:ADD() /* Cria Planilha */
           chworksheet            = chExcelapp:sheets:ITEM(1)
           chworksheet:PageSetup:PrintGridlines = TRUE              /* Imprimir Linhas de Grade */
           chworksheet:PageSetup:CenterHorizontally = TRUE          /* Centraliza Linhas Horizontais */
           chworksheet:PageSetup:CenterVertically   = FALSE         /* Centraliza Linhas Verticais */
           chworksheet:PageSetup:rightheader = "&d - &t" + "  Pagina: &9&P De &N" 
           chworksheet:pagesetup:ORIENTATION = 2                    /* PAISAGEM */
           chworksheet:pagesetup:printTitleRows = "$1:$2"           /* Imprimir Sempre em cada Pagina as linhas 1 a 3*/
           chworksheet:pagesetup:zoom = 90.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-atualiza-recto B-table-Win 
PROCEDURE pi-atualiza-recto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    IF NOT VALID-HANDLE(h-boin090) OR
       h-boin090:TYPE      <> "PROCEDURE":U OR
       h-boin090:FILE-NAME <> "inbo/boin090.p":U THEN
       RUN inbo/boin090.p PERSISTENT SET h-boin090.

    EMPTY TEMP-TABLE tt-docum-est.

    /* Cria Temp-Table para o Recebimento*/
    CREATE tt-docum-est.
    BUFFER-COPY docum-est TO tt-docum-est
           ASSIGN tt-docum-est.r-rowid = ROWID(docum-est).

    /* Atualiza Documento */
    RUN openQueryStatic IN h-boin090 (INPUT "Main":U).
    RUN emptyRowErrors IN h-boin090.
    RUN repositionRecord IN h-boin090 (input tt-docum-est.r-rowid).
    RUN AtualizaDocumento IN h-boin090.
    RUN getRowErrors IN h-boin090 (OUTPUT TABLE RowErrors).
    IF CAN-FIND(FIRST RowErrors 
               WHERE RowErrors.ErrorSubType = "ERROR":U  ) THEN DO:

       FOR EACH rowerrors WHERE
                RowErrors.ErrorSubType = "ERROR":U:
           MESSAGE "Erro ao Atualizar o Documento" SKIP
                    rowerrors.errordescription 
                   VIEW-AS ALERT-BOX.
       END.
       IF VALID-HANDLE(h-boin090) THEN
          DELETE PROCEDURE h-boin090.

       RETURN 'ADM-ERROR'.
    END.

    IF VALID-HANDLE(h-boin090) THEN
       DELETE PROCEDURE h-boin090.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cabec-item-sel B-table-Win 
PROCEDURE pi-cabec-item-sel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
      PUT c-empresa  FORMAT "X(40)"                   AT   1
      "DATA: "                                        AT  60
      STRING(TODAY,"99/99/9999") FORMAT "X(10)"       AT  66
      "HORA: "                                        AT  88
      STRING(TIME,"hh:mm:ss")                         AT  94
      "PAG:"                                          AT 127
      i-pag FORMAT ">>>"                              AT 132
      SKIP(1).
    
  PUT "RELATORIO DE ITENS SELECIONADOS" FORMAT "X(35)" AT 44 SKIP(1).

  PUT "CLIENTE: " + TRIM(STRING(emitente.cod-emit)) +  "  " + emitente.nome-emit FORMAT "x(60)" AT 1 SKIP(1).

  PUT "N.FISCAL DT.NFISCAL ITEM   DESCRIÄ«O                                         REFER     LOTE QUANTIDADE VL.UNITARIO VALOR TOTAL  PEDIDO" AT 1.
  PUT "-------- ---------- ------ ------------------------------------------------- --------- ---- ---------- ----------- ----------- -------" AT 1. 
  ASSIGN i-pag = i-pag + 1. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-gera-excel B-table-Win 
PROCEDURE pi-gera-excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER arq-saida AS CHAR FORMAT "x(50)".

RUN pi-abre-excel.
  IF chExcelApp = ? THEN DO:
     MESSAGE "O Aplicativo EXCEL n∆o foi encontrado. N∆o Ç possivel a execuá∆o do programa."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     ASSIGN arq-saida = "".
     RETURN.
  END.

  RUN pi-monta-planilha.

  /* Posiciona o Foco no Inicio da Planilha */
  chExcelApp:Range("A1"):SELECT.
  chExcelApp:Range("A:A"):EntireColumn:AutoFit.

  /* Posiciona na Planilha 1, Salva e Fecha */
  chWorkSheet = chExcelapp:Sheets:ITEM(1).
  chWorkbook:Worksheets(1):activate.

  /* Salva e Fecha Planilha */
  IF chExcelApp:Version < "12":U THEN DO.
     ASSIGN arq-saida = SUBSTR(arq-saida,1,LENGTH(arq-saida) - 3) + "xls".
     OS-DELETE VALUE(arq-saida).
     chWorkBook:SaveAs(arq-saida,39,,,,,,,TRUE). /* Salva em formato mais Antigo EXCEL97 */ 
  END.
  ELSE DO:
     ASSIGN arq-saida = SUBSTR(arq-saida,1,LENGTH(arq-saida) - 3) + "xlsx".
     OS-DELETE VALUE(arq-saida).
     chWorkBook:SaveAs(arq-saida,,,,,,,).  /* Salva na Vers∆o da planilha da ESTAÄ«O */
  END.

  chWorkBook:CLOSE().
  chExcelApp:QUIT().
  RELEASE OBJECT chExcelApp. 
  RELEASE OBJECT chworkBook.
  RELEASE OBJECT chworksheet.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imp-cabec-email B-table-Win 
PROCEDURE pi-imp-cabec-email :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

 PUT c-empresa  FORMAT "x(40)"                 AT   1
     "DATA: "                                  AT  71
     STRING(TODAY,"99/99/9999") FORMAT "x(10)" AT  77
     "HORA: "                                  AT 104
     STRING(TIME,"hh:mm:ss")                   AT 110
     "PAG:"                                    AT 150
     i-pag FORMAT ">>>"                        AT 155
     SKIP(1).

 PUT "DEVOLUÄ«O DE CLIENTE" FORMAT "x(25)" AT 44  SKIP(1).

 PUT "CLIENTE: " + STRING(emitente.cod-emit, ">>>>>>>9") + " - " + emitente.nome-emit FORMAT "x(36)" AT 1
     "Nß DOCTO: "              FORMAT "x(10)"  AT 47
      c-nr-nota-fis            FORMAT "x(11)"  AT 57 
      "DATA ENTRADA: "         FORMAT "x(14)"  AT 71
      TRIM(STRING(TODAY, "99/99/9999")) FORMAT "x(10)" AT 85
      "QTDE DEV.: "                FORMAT "x(11)"    AT  97
      TRIM(STRING(fi-sel-qtd-total, ">>>>,>>9.99")) FORMAT "x(11)" AT 108
      "VALOR: "                    FORMAT "X(7)"    AT 128
      TRIM(STRING(fi-sel-vlr-total, ">>>>,>>9.99")) FORMAT "x(11)" AT 135.
 PUT "REPRES.: " FORMAT "x(9)"   AT  1
      repres.nome FORMAT "x(40)" AT 10
      SKIP(1).

 PUT "N.FISCAL DATA DA NF   ITEM DESCRIÄ«O                                         REFERENCIA LOTE QUANTIDADE VL.UNITARIO VALOR TOTAL PEDIDO" AT 1.
 PUT "-------- ---------- ------ ------------------------------------------------- ---------- ---- ---------- ----------- ----------- ------" AT 1.
 ASSIGN i-pag = i-pag + 1.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imp-cabec-etq B-table-Win 
PROCEDURE pi-imp-cabec-etq :
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
     "PAG:"                                    AT 150
     i-pag FORMAT ">>>"                        AT 155
     SKIP(1).

 PUT "RELATORIO DE RECEBIMENTO DA DEVOLUÄ«O DE CLIENTE" FORMAT "X(50)" AT 60 SKIP(1).

 PUT "CLIENTE: " + TRIM(STRING(emitente.cod-emit, ">>>>>>>9")) + " - " + emitente.nome-emit FORMAT "x(60)" AT 1 SKIP(1).

 PUT "N.FISCAL DATA DA NF ITEM   DESCRIÄ«O                REFER LOTE QUANTIDADE VL.UNITARIO VALOR TOTAL PEDIDO  LOCALIZ. ETIQUETA   NUANCE QUALIDADE TIPO QUANTIDADE" AT 1.
 PUT "-------- ---------- ------ -------------------- --------- ---- ---------- ----------- ----------- ------- -------- ---------  ------ --------- ---- ----------" AT 1. 
 ASSIGN i-pag = i-pag + 1.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imp-item-sel B-table-Win 
PROCEDURE pi-imp-item-sel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  &SCOPED-DEFINE NOTAS b-tt-itens-sel.nr-nota-fis 

  DEF VAR h-prog AS HANDLE NO-UNDO.
  DEF VAR i-ct   AS INT.
  RUN utp/ut-utils.p PERSISTENT SET h-prog.

  RUN esapi/saida-imp.p (OUTPUT i-saida,
                         OUTPUT c-saida,
                         OUTPUT i-num-copias,
                         OUTPUT l-ok).

  CASE i-saida:
      WHEN 1 THEN DO.
          OUTPUT TO VALUE(c-saida) CONVERT TARGET "ibm850" PAGED PAGE-SIZE 62.
          PUT CONTROL "~033E~033(s18H".    
      END.
      WHEN 2 THEN
          OUTPUT TO VALUE(c-saida).
      WHEN 3 THEN DO.
          ASSIGN c-saida = SESSION:TEMP-DIRECTORY + "essp0204.tmp".
          OUTPUT TO VALUE(c-saida) CONVERT SOURCE "ibm850".
      END.
  END CASE.
  DO i-ct = 1 TO i-num-copias:
     ASSIGN i-pag      =  1
            i-lin      = 99.

     FOR EACH b-tt-itens-sel NO-LOCK  
         BREAK BY {&NOTAS}.

         IF i-lin > 62 THEN DO:
            RUN pi-cabec-item-sel.
            ASSIGN i-lin = 11.
         END.

         PUT b-tt-itens-sel.nr-nota-fis FORMAT "99999999"      AT   1     
             b-tt-itens-sel.dt-emis     FORMAT "99/99/9999"    AT  10
             b-tt-itens-sel.it-codigo   FORMAT "x(6)"          AT  21
             b-tt-itens-sel.descricao   FORMAT "x(49)"         AT  28
             b-tt-itens-sel.cod-refer   FORMAT "99.9999-9"     AT  78
             b-tt-itens-sel.nr-lote     FORMAT "x(4)"          AT  88 
             b-tt-itens-sel.qtde-item   FORMAT ">>>,>>9.99"    AT  93 
             b-tt-itens-sel.vlr-unit    FORMAT ">>>,>>9.99"    AT 105 
             b-tt-itens-sel.vlr-total   FORMAT ">>>,>>9.99"    AT 117 
             b-tt-itens-sel.nr-pedcli   FORMAT "X(6)"          AT 129.

         ASSIGN i-lin = i-lin + 1.

         ACCUMULATE b-tt-itens-sel.qtde-item (TOTAL).
         ACCUMULATE b-tt-itens-sel.vlr-total (TOTAL).

    END.
    
    IF (ACCUM TOTAL b-tt-itens-sel.qtde-item) <> 0 OR
       (ACCUM TOTAL b-tt-itens-sel.vlr-total) <> 0 THEN DO:
       IF i-lin > 57 THEN DO:
          RUN pi-cabec-item-sel.
          ASSIGN i-lin = 11.
       END.
       PUT "----------             -----------" AT 93.
       PUT "TOTAL GERAL" FORMAT "x(11)" AT 78.
       PUT ACCUM TOTAL b-tt-itens-sel.qtde-item FORMAT ">>>,>>9.99"   AT  93.
       PUT ACCUM TOTAL b-tt-itens-sel.vlr-total FORMAT ">>>,>>9.99"   AT 117.
       
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imprime-etq B-table-Win 
PROCEDURE pi-imprime-etq :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  &SCOPED-DEFINE NOTA-ITEM b-tt-itens-sel.nr-nota-fis + b-tt-itens-sel.it-codigo + b-tt-itens-sel.cod-refer 

  DEF VAR de-qtd-item AS DECIMAL NO-UNDO.
  DEF VAR de-tot-item AS DECIMAL NO-UNDO.
  DEF VAR de-tot-etq  AS DECIMAL NO-UNDO.
  DEF VAR de-qtd-ger  AS DECIMAL NO-UNDO.
  DEF VAR de-tot-ger  AS DECIMAL NO-UNDO.
  DEF VAR de-etq-ger  AS DECIMAL NO-UNDO.

  RUN utp/ut-utils.p PERSISTENT SET h-prog.

  RUN esapi/saida-imp.p (OUTPUT i-saida,
                         OUTPUT c-saida,
                         OUTPUT i-num-copias,
                         OUTPUT l-ok).
  CASE i-saida:
      WHEN 1 THEN DO.
          OUTPUT TO VALUE(c-saida) CONVERT TARGET "ibm850" PAGED PAGE-SIZE 39.
          PUT CONTROL "~033&l1O~033(s16H". /* ORIENTAÄ«O PAISAGEM & COMPACTA */ 
      END.
      WHEN 2 THEN
          OUTPUT TO VALUE(c-saida) CONVERT SOURCE "ibm850".
      WHEN 3 THEN DO.
          ASSIGN c-saida = SESSION:TEMP-DIRECTORY + "essp0204.txt".
          OUTPUT TO VALUE(c-saida) CONVERT SOURCE "ibm850".
      END.
  END CASE.

  DO i-ct = 1 TO i-num-copias.
     ASSIGN i-pag        =  1
            i-lin        = 99
            de-qtd-item  =  0 
            de-tot-item  =  0.
     FOR EACH b-tt-itens-sel NO-LOCK
         BREAK BY {&NOTA-ITEM}.

         IF i-lin > 39 THEN DO:
            RUN pi-imp-cabec-etq.
            ASSIGN i-lin = 7.
         END.

         PUT b-tt-itens-sel.nr-nota-fis FORMAT "99999999"      AT   1     
             b-tt-itens-sel.dt-emis     FORMAT "99/99/9999"    AT  10
             b-tt-itens-sel.it-codigo   FORMAT "x(6)"          AT  21
             b-tt-itens-sel.descricao   FORMAT "x(20)"         AT  28
             b-tt-itens-sel.cod-refer   FORMAT "99.9999-9"     AT  49
             b-tt-itens-sel.nr-lote     FORMAT "x(3)"          AT  59 
             b-tt-itens-sel.qtde-item   FORMAT ">>>,>>9.99"    AT  64 
             b-tt-itens-sel.vlr-unit    FORMAT ">>>>,>>9.99"   AT  75 
             b-tt-itens-sel.vlr-total   FORMAT ">>>>,>>9.99"   AT  87 
             b-tt-itens-sel.nr-pedcli   FORMAT "x(7)"          AT  99. 
              
         ASSIGN de-qtd-item = de-qtd-item + b-tt-itens-sel.qtde-item
                de-tot-item = de-tot-item + b-tt-itens-sel.vlr-total.

         /*  E T I Q U E T A S */
         ASSIGN de-tot-etq = 0.
         FOR EACH b-tt-etq-dev NO-LOCK.

             FIND ob-etiqueta WHERE 
                  ob-etiqueta.cod-estabel  = c-cod-estabel AND 
                  ob-etiqueta.num-etiqueta = b-tt-etq-dev.num-etiqueta NO-LOCK NO-ERROR.
             IF NOT AVAIL ob-etiqueta THEN NEXT.

             IF ob-etiqueta.it-codigo = b-tt-itens-sel.it-codigo AND
                ob-etiqueta.cod-refer = b-tt-itens-sel.cod-refer AND
                ob-etiqueta.nr-lote   = b-tt-itens-sel.nr-lote   THEN DO:

                IF ob-etiqueta.it-codigo <> b-tt-itens-sel.it-codigo OR
                   ob-etiqueta.cod-refer <> b-tt-itens-sel.cod-refer THEN NEXT.

                FIND ped-item-rom WHERE
                     ped-item-rom.cod-estabel  = ob-etiqueta.cod-estabel AND
                     ped-item-rom.num-etiqueta = ob-etiqueta.num-etiqueta NO-LOCK NO-ERROR.
                IF NOT AVAIL ped-item-rom THEN NEXT.

                FIND ped-item WHERE
                     ped-item.nr-pedcli = ped-item-rom.nr-pedcli   AND
                     ped-item.nome-abrev = ped-item-rom.nome-abrev AND
                     ped-item.nr-sequencia = ped-item-rom.nr-sequencia NO-LOCK NO-ERROR.

                FIND ped-item-res OF ped-item NO-LOCK NO-ERROR.
                IF AVAIL ped-item-res THEN DO.
                   IF b-tt-itens-sel.nr-nota-fis <> STRING(ped-item-res.nr-nota-fis,"9999999") THEN NEXT.
                END.

                PUT b-tt-etq-dev.localizacao  FORMAT "999/999":U    AT 107
                    b-tt-etq-dev.num-etiqueta FORMAT "999999999":U  AT 116
                    b-tt-etq-dev.nuance       FORMAT "X(4)":U       AT 127
                    b-tt-etq-dev.cod-qualid   FORMAT "X(4)":U       AT 134
                    b-tt-etq-dev.quantidade   FORMAT ">>>,>>9.99":U AT 144.

                ASSIGN i-lin = i-lin + 1.

                ASSIGN de-tot-etq = de-tot-etq + b-tt-etq-dev.quantidade.
             END.
         END.

         IF i-lin > 39 THEN DO:
            RUN pi-imp-cabec-etq.
            ASSIGN i-lin = 7.
         END.


         PUT "----------             -----------                                                   ----------" AT 64.
         ASSIGN i-lin = i-lin + 1.
         PUT "TOTAL ITEM :"                   AT  39
             de-qtd-item FORMAT ">>>,>>9.99"  AT  64 
             de-tot-item FORMAT ">>>>,>>9.99" AT  87 
             de-tot-etq  FORMAT ">>>,>>9.99"  AT 149.
         ASSIGN de-qtd-ger = de-qtd-ger + de-qtd-item
                de-tot-ger = de-tot-ger + de-tot-item
                de-etq-ger = de-etq-ger + de-tot-etq
                i-lin      = i-lin + 1. 
         ASSIGN de-qtd-item = 0 de-tot-item = 0 de-tot-etq = 0.
         PUT "--------------------------------------------------------------------------------------------------------------------------------------------------------------" AT 1.
         ASSIGN i-lin = i-lin + 1.
     END.

     PUT "" SKIP(1).
     PUT "TOTAL GERAL:"                   AT  39
         de-qtd-ger  FORMAT ">>>,>>9.99"  AT  64 
         de-tot-ger  FORMAT ">>>>,>>9.99" AT  87 
         de-etq-ger  FORMAT ">>>,>>9.99"  AT 149.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-limpa B-table-Win 
PROCEDURE pi-limpa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   EMPTY TEMP-TABLE tt-nota-fisc.
   EMPTY TEMP-TABLE tt-it-nota-fisc.
   EMPTY TEMP-TABLE wt-notas-geradas.
   EMPTY TEMP-TABLE tt-notas-geradas.
   EMPTY TEMP-TABLE tt-docum-est.
   EMPTY TEMP-TABLE tt-item-doc-est.
   EMPTY TEMP-TABLE wt-item-doc-est.
   EMPTY TEMP-TABLE tt-dupli-apagar.
   EMPTY TEMP-TABLE tt-rat-lote.
   EMPTY TEMP-TABLE tt-item-devol-cli.

   ASSIGN fi-cod-transp:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
          fi-nome-transp:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
          cb-frete:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ' '
          fi-motivo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
          fi-sel-qtd-total:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0"
          fi-sel-vlr-total:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0".

   ASSIGN c-texto-msg = ""
          c-doca-msg = "".
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-monta-planilha B-table-Win 
PROCEDURE pi-monta-planilha :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 /* Nomear Aba da Planilha */
 chWorkSheet = chExcelapp:Sheets:ITEM(1).
 chWorkSheet:NAME = "Espelho".
 chWorkSheet:TAB:ColorIndex = 19.

 /* Ativar a Planilha */
 chWorkSheet = chExcelapp:Sheets:ITEM(1).
 chWorkbook:Worksheets(1):activate.
 chExcelApp:ActiveWindow:Zoom = 100.

 ASSIGN chworksheet:range("B1"):VALUE = "RESUMO POR ITENS DA DEVOLUÄ«O (ITENS SELECIONADO)".

 /* Configura Alinhamento Horizontal do Titulo da Planilha */
 ChWorkSheet:range("B1:J1"):SELECT().
 ChWorksheet:range("B1:J1"):Merge.
 Chworksheet:Range("B1:J1"):HorizontalAlignment = 3. /* Centralizado */
 Chworksheet:Range("B1:J1"):VerticalAlignment   = 2. /* Centralizado */

 /* Colorir Titulo da Planilha */
 chWorkSheet:Range("A1:J1"):FONT:ColorIndex     = 18. /* Avermelhado */
 chWorkSheet:Range("A1:J1"):Interior:ColorIndex = 2. /* Branco */

 /* Configura a Linha do Titulo da Planilha */
 ASSIGN chWorkSheet:Rows("1:1"):RowHeight = 40
        chWorkSheet:Rows("1:1"):FONT:SIZE = 18
        chWorkSheet:Rows("1:1"):FONT:bold = FALSE.

 /* Inserir Logotipo e Alinhar Via Tamanho e Altura Logotipo */
 ChWorkSheet:range("A1"):SELECT().
 ChWorkSheet:Pictures:INSERT("image\med-dup.jpg"):SELECT. 
 chExcelApp:SELECTION:ShapeRange:ScaleWidth(1.9,FALSE,FALSE).
 chExcelApp:SELECTION:ShapeRange:ScaleHeight(0.61,FALSE,FALSE). 
 chExcelApp:SELECTION:ShapeRange:IncrementLeft(-49.5).
 chExcelApp:SELECTION:ShapeRange:IncrementTop( -48.75).

 /* Titulo das Colunas */
 ASSIGN chworksheet:range("A2"):VALUE = "N.FISCAL"
        chworksheet:range("B2"):VALUE = "DT.EMISSAO"    
        chworksheet:range("C2"):VALUE = "ITEM"  
        chworksheet:range("D2"):VALUE = "DESCRIÄ«O"
        chworksheet:range("E2"):VALUE = "REFERENCIA" 
        chworksheet:range("F2"):VALUE = "LOTE"  
        chworksheet:range("G2"):VALUE = "QUANTIDADE"
        chworksheet:range("H2"):VALUE = "VLR UNITARIO" 
        chworksheet:range("I2"):VALUE = "VALOR TOTAL"            
        chworksheet:range("J2"):VALUE = "PEDIDO".                 

 /* Ajustar o Tamanho Dentro da Celula */     
 ASSIGN chworksheet:range("A2"):ShrinkToFit = TRUE    
        chworksheet:range("B2"):ShrinkToFit = TRUE    
        chworksheet:range("C2"):ShrinkToFit = TRUE
        chworksheet:range("D2"):ShrinkToFit = TRUE
        chworksheet:range("E2"):ShrinkToFit = TRUE
        chworksheet:range("F2"):ShrinkToFit = TRUE
        chworksheet:range("G2"):ShrinkToFit = TRUE
        chworksheet:range("H2"):ShrinkToFit = TRUE
        chworksheet:range("I2"):ShrinkToFit = TRUE
        chworksheet:range("J2"):ShrinkToFit = TRUE.

 /* Tamanho das Colunas */
 ASSIGN chWorkSheet:Columns("A"):ColumnWidth = 11
        chWorkSheet:Columns("B"):ColumnWidth = 13
        chWorkSheet:Columns("C"):ColumnWidth =  7
        chWorkSheet:Columns("D"):ColumnWidth = 40
        chWorkSheet:Columns("E"):ColumnWidth = 13
        chWorkSheet:Columns("F"):ColumnWidth =  6
        chWorkSheet:Columns("G"):ColumnWidth = 14
        chWorkSheet:Columns("H"):ColumnWidth = 14
        chWorkSheet:Columns("I"):ColumnWidth = 14
        chWorkSheet:Columns("J"):ColumnWidth =  8.

 /* Configura as Colunas da Planilha */
 ASSIGN chworksheet:range("A:F"):NumberFormat = "@"
        chworksheet:range("G:I"):NumberFormat = "###.###.##0,00"
        Chworksheet:range("G:I"):HorizontalAlignment = 4  /* Alinhamento a Direita */
        chworksheet:range("J:J"):NumberFormat = "@".

 /* Configura Cabeáalho das Colunas */
 chWorkSheet:Range("A2:J2"):SELECT().
 ASSIGN chExcelApp:SELECTION:FONT:NAME               = "Consolas"
        chExcelApp:SELECTION:FONT:SIZE               = 12
        chExcelApp:SELECTION:FONT:Bold               = FALSE 
        chExcelApp:SELECTION:Interior:ColorIndex     = 37
        chExcelApp:SELECTION:FONT:ColorIndex         = 11.

 ASSIGN i-Lin    = 3.
 FOR EACH tt-itens-sel WHERE NO-LOCK.
     ASSIGN chworksheet:range("A" + STRING(i-lin)):VALUE = STRING(tt-itens-sel.nr-nota-fis)     
            chworksheet:range("B" + STRING(i-lin)):VALUE = STRING(tt-itens-sel.dt-emis, "99/99/9999")     
            chworksheet:range("C" + STRING(i-lin)):VALUE = tt-itens-sel.it-codigo   
            chworksheet:range("D" + STRING(i-lin)):VALUE = tt-itens-sel.descricao
            chworksheet:range("E" + STRING(i-lin)):VALUE = STRING(tt-itens-sel.cod-refer,"99.9999-9")   
            chworksheet:range("F" + STRING(i-lin)):VALUE = tt-itens-sel.nr-lote      
            chworksheet:range("G" + STRING(i-lin)):VALUE = tt-itens-sel.qtde-item    
            chworksheet:range("H" + STRING(i-lin)):VALUE = tt-itens-sel.vlr-unit     
            chworksheet:range("I" + STRING(i-lin)):VALUE = tt-itens-sel.vlr-total    
            chworksheet:range("J" + STRING(i-lin)):VALUE = tt-itens-sel.nr-pedcli.

     /*  Configura Tamanho da Fonte */
     ASSIGN c-lin = STRING(i-lin) + ":" + STRING(i-lin)
            chworksheet:Rows(c-lin):FONT:SIZE = 9.

     ASSIGN i-lin = i-lin + 1.
     ACCUMULATE tt-itens-sel.qtde-item (TOTAL).
     ACCUMULATE tt-itens-sel.vlr-total (TOTAL).
 END.
 IF i-lin <> 4 THEN DO:
     ASSIGN chworksheet:range("D" + STRING(i-lin)):VALUE = "T O T A L . . . "
            chworksheet:range("G" + STRING(i-lin)):VALUE = (ACCUM TOTAL tt-itens-sel.qtde-item)
            chworksheet:range("I" + STRING(i-lin)):VALUE = (ACCUM TOTAL tt-itens-sel.vlr-total).
     /* Colorir a Linha / Negrito */
     ASSIGN chWorkSheet:Range("A" + STRING(i-lin) + ":J" + STRING(i-lin)):Interior:ColorIndex = 14
            chWorkSheet:Range("A" + STRING(i-lin) + ":J" + STRING(i-lin)):FONT:ColorIndex     =  2
            chWorkSheet:Range("A" + STRING(i-lin) + ":J" + STRING(i-lin)):FONT:Bold           = TRUE.
     ASSIGN i-lin = i-lin + 1.
 END.

 ASSIGN i-lin = i-lin + 3.

 /* Inserir Logotipo e Alinhar Via Tamanho e Altura Logotipo */
 chWorkSheet:Range("A" + STRING(i-lin + 3) + ":A" + STRING(i-lin + 3)):SELECT().
 ChWorkSheet:Pictures:INSERT("image\med-dup.jpg"):SELECT. 
 chExcelApp:SELECTION:ShapeRange:ScaleWidth(1.9,FALSE,FALSE).
 chExcelApp:SELECTION:ShapeRange:ScaleHeight(0.61,FALSE,FALSE). 
 chExcelApp:SELECTION:ShapeRange:IncrementLeft(-49.5).
 chExcelApp:SELECTION:ShapeRange:IncrementTop( -48.75).

 ASSIGN chworksheet:range("B" + STRING(i-lin)):VALUE = "RESUMO POR ITENS DA DEVOLUÄ«O (ITENS DEVOLVIDOS)".

 /* Configura Alinhamento Horizontal do Titulo da Planilha */
 chWorkSheet:Range("B" + STRING(i-lin) + ":J" + STRING(i-lin)):SELECT().
 chWorkSheet:Range("B" + STRING(i-lin) + ":J" + STRING(i-lin)):Merge.
 chWorkSheet:Range("B" + STRING(i-lin) + ":J" + STRING(i-lin)):HorizontalAlignment = 3.
 chWorkSheet:Range("B" + STRING(i-lin) + ":J" + STRING(i-lin)):VerticalAlignment   = 2.

 /* Colorir Titulo da Planilha */
 chWorkSheet:Range("A" + STRING(i-lin) + ":J" + STRING(i-lin)):FONT:ColorIndex     = 18. 
 chWorkSheet:Range("A" + STRING(i-lin) + ":J" + STRING(i-lin)):Interior:ColorIndex =  2. /* Branco */

 /* Configura a Linha do Titulo da Planilha */
 ASSIGN chWorkSheet:Rows(STRING(i-lin) + ':' + STRING(i-lin)):RowHeight = 40
        chWorkSheet:Rows(STRING(i-lin) + ':' + STRING(i-lin)):FONT:SIZE = 18
        chWorkSheet:Rows(STRING(i-lin) + ':' + STRING(i-lin)):FONT:bold = FALSE.


 ASSIGN i-lin = i-lin + 1.
 /* Titulo das Colunas */
 ASSIGN chworksheet:range("A" + STRING(i-lin)):VALUE = "N.FISCAL"
        chworksheet:range("B" + STRING(i-lin)):VALUE = "DT.EMISSAO"    
        chworksheet:range("C" + STRING(i-lin)):VALUE = "ITEM"  
        chworksheet:range("D" + STRING(i-lin)):VALUE = "DESCRIÄ«O"
        chworksheet:range("E" + STRING(i-lin)):VALUE = "REFERENCIA" 
        chworksheet:range("F" + STRING(i-lin)):VALUE = "LOTE"  
        chworksheet:range("G" + STRING(i-lin)):VALUE = "QUANTIDADE"
        chworksheet:range("H" + STRING(i-lin)):VALUE = "VLR UNITARIO" 
        chworksheet:range("I" + STRING(i-lin)):VALUE = "VALOR TOTAL"            
        chworksheet:range("J" + STRING(i-lin)):VALUE = "PEDIDO".                 


 /* Configura Cabeáalho das Colunas */
 chWorkSheet:Range("A" + STRING(i-lin) + ":J" + STRING(i-lin)):SELECT().
 ASSIGN chExcelApp:SELECTION:FONT:NAME               = "Consolas"
        chExcelApp:SELECTION:FONT:SIZE               = 12
        chExcelApp:SELECTION:FONT:Bold               = FALSE 
        chExcelApp:SELECTION:Interior:ColorIndex     = 37
        chExcelApp:SELECTION:FONT:ColorIndex         = 11.

 ASSIGN i-lin = i-lin + 1.
 FOR EACH tt-itens-sel WHERE NO-LOCK.
     ASSIGN chworksheet:range("A" + STRING(i-lin)):VALUE = STRING(tt-itens-sel.nr-nota-fis)     
            chworksheet:range("B" + STRING(i-lin)):VALUE = STRING(tt-itens-sel.dt-emis, "99/99/9999")     
            chworksheet:range("C" + STRING(i-lin)):VALUE = tt-itens-sel.it-codigo   
            chworksheet:range("D" + STRING(i-lin)):VALUE = tt-itens-sel.descricao
            chworksheet:range("E" + STRING(i-lin)):VALUE = STRING(tt-itens-sel.cod-refer,"99.9999-9")   
            chworksheet:range("F" + STRING(i-lin)):VALUE = tt-itens-sel.nr-lote      
            chworksheet:range("G" + STRING(i-lin)):VALUE = tt-itens-sel.qtde-item    
            chworksheet:range("H" + STRING(i-lin)):VALUE = tt-itens-sel.vlr-unit     
            chworksheet:range("I" + STRING(i-lin)):VALUE = tt-itens-sel.vlr-total    
            chworksheet:range("J" + STRING(i-lin)):VALUE = tt-itens-sel.nr-pedcli.

     /*  Configura Tamanho da Fonte */
     ASSIGN c-lin = STRING(i-lin) + ":" + STRING(i-lin)
            chworksheet:Rows(c-lin):FONT:SIZE = 9.

     ASSIGN i-lin = i-lin + 1.
     ACCUMULATE tt-itens-sel.qtde-item (TOTAL).
     ACCUMULATE tt-itens-sel.vlr-total (TOTAL).
 END.
 IF i-lin <> 4 THEN DO:
     ASSIGN chworksheet:range("D" + STRING(i-lin)):VALUE = "T O T A L . . . "
            chworksheet:range("G" + STRING(i-lin)):VALUE = (ACCUM TOTAL tt-itens-sel.qtde-item)
            chworksheet:range("I" + STRING(i-lin)):VALUE = (ACCUM TOTAL tt-itens-sel.vlr-total).
     /* Colorir a Linha / Negrito */
     ASSIGN chWorkSheet:Range("A" + STRING(i-lin) + ":J" + STRING(i-lin)):Interior:ColorIndex = 14
            chWorkSheet:Range("A" + STRING(i-lin) + ":J" + STRING(i-lin)):FONT:ColorIndex     =  2
            chWorkSheet:Range("A" + STRING(i-lin) + ":J" + STRING(i-lin)):FONT:Bold           = TRUE.
     ASSIGN i-lin = i-lin + 1.
 END.
 ASSIGN  chWorkSheet:PageSetup:PrintArea = "$A$1:$J$" + STRING(i-lin).  /* Marcar Area de Impressao */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-processa B-table-Win 
PROCEDURE pi-processa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER TABLE FOR tt-itens-sel.
    DEFINE INPUT PARAMETER TABLE FOR tt-etq-dev.
    DEFINE INPUT PARAMETER p-cod-cliente      AS CHAR.
    DEFINE INPUT PARAMETER p-cod-estabel      AS CHAR.
    DEFINE INPUT PARAMETER p-nr-nota-fis      AS CHAR.
    DEFINE INPUT PARAMETER p-serie            AS CHAR.
    DEFINE INPUT PARAMETER p-nat-oper         AS CHAR.
    DEFINE INPUT PARAMETER p-snfd             AS LOG.
    DEFINE INPUT PARAMETER p-serie-propria    AS CHAR.
    DEFINE INPUT PARAMETER p-nat-oper-propria AS CHAR.         
    DEFINE INPUT PARAMETER p-nf-cli           AS CHAR.
    DEFINE INPUT PARAMETER p-cod-chave-aces-nf-eletro AS CHAR.
    DEFINE INPUT PARAMETER p-serie-cliente    AS CHAR.
    DEFINE INPUT PARAMETER p-nat-oper-cli     AS CHAR.  
        
    IF p-nf-cli <> ''  THEN DO.  // Nota do Cliente
       ASSIGN c-nr-nota-fis = p-nf-cli
              c-nat-oper-nf = p-nat-oper-cli
              c-serie       = p-serie-cliente
              c-chave       = p-cod-chave-aces-nf-eletro.
    END.
    ELSE IF p-serie-propria <> "" THEN DO.
       ASSIGN c-nr-nota-fis = p-nr-nota-fis
              c-nat-oper-nf = p-nat-oper-propria
              c-serie       = p-serie-propria.
    END.

    ASSIGN bt-devolucao:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           bt-email:SENSITIVE IN FRAME {&FRAME-NAME}     = YES.

    /* Busca Nome da Empresa */
    FIND FIRST param-global NO-LOCK NO-ERROR.
    FIND FIRST empresa
         WHERE empresa.ep-codigo = param-global.empresa-prin NO-LOCK NO-ERROR. 
    ASSIGN c-empresa = (IF AVAIL empresa THEN empresa.razao-social ELSE "").

    FIND FIRST param-dis NO-LOCK NO-ERROR.

    ASSIGN c-cod-estabel     = p-cod-estabel
           c-nf-cli          = p-nf-cli
           c-serie-orig      = p-serie
           l-snfd            = p-snfd.

    FIND nota-fiscal WHERE
         nota-fiscal.cod-estabel = c-cod-estabel AND
         nota-fiscal.serie       = c-serie-orig AND
         nota-fiscal.nr-nota-fis = p-nr-nota-fis  NO-LOCK NO-ERROR.

    FIND emitente WHERE
         emitente.nome-abrev = p-cod-cliente NO-LOCK NO-ERROR.

    RUN pi-limpa.

    FIND FIRST tt-itens-sel NO-LOCK NO-ERROR.
    IF AVAIL tt-itens-sel THEN
       ASSIGN bt-devolucao:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

    {&OPEN-QUERY-br-it-sel}
    APPLY 'value-changed' TO br-it-sel IN FRAME {&FRAME-NAME}.
    
    APPLY 'ENTRY' TO fi-cod-transp.
    RETURN NO-APPLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-recebimento B-table-Win 
PROCEDURE pi-recebimento :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-----------------------------------------------------------------------------*/
    RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
    {utp/ut-liter.i Criando_Recebimento *}
    RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

    RUN pi-acompanhar IN h-acomp (INPUT "Aguarde").
 
    IF NOT VALID-HANDLE(h-boin090) OR
       h-boin090:TYPE      <> "PROCEDURE":U OR
       h-boin090:FILE-NAME <> "inbo/boin090.p":U THEN
       RUN inbo/boin090.p PERSISTENT SET h-boin090.
    
    IF NOT VALID-HANDLE(h-boin176) OR 
       h-boin176:TYPE      <> "PROCEDURE":U OR
       h-boin176:FILE-NAME <> "inbo/boin176.p":U THEN
       RUN inbo/boin176.p PERSISTENT SET h-boin176.

    IF NOT VALID-HANDLE(h-boin367) OR 
       h-boin367:TYPE      <> "PROCEDURE":U OR
       h-boin367:FILE-NAME <> "inbo/boin367.p":U THEN
       RUN inbo/boin367.p PERSISTENT SET h-boin367.

    EMPTY TEMP-TABLE tt-docum-est.
    EMPTY TEMP-TABLE tt-item-doc-est. 
    EMPTY TEMP-TABLE tt-dupli-apagar. 
    EMPTY TEMP-TABLE tt-rat-lote.

    FIND estabelec WHERE
         estabelec.cod-estabel = c-cod-estabel NO-LOCK NO-ERROR.

    FIND natur-oper WHERE 
         natur-oper.nat-operacao = c-nat-oper-nf  NO-LOCK NO-ERROR.

    /* Cria Temp-Table para o Recebimento*/
    CREATE tt-docum-est.
    ASSIGN tt-docum-est.cod-emit = emitente.cod-emit
           tt-docum-est.serie-docto = c-serie       
           tt-docum-est.nro-docto =  c-nr-nota-fis       
           tt-docum-est.nat-operacao = c-nat-oper-nf        
           tt-docum-est.cod-observ = 3    /* Devoluá∆o de Cliente */
           tt-docum-est.cod-estabel = c-cod-estabel
           tt-docum-est.nome-transp = transporte.nome-abrev
           tt-docum-est.mod-frete = cb-frete  
           tt-docum-est.ct-transit = '19000008' 
           tt-docum-est.dt-emissao = TODAY
           tt-docum-est.dt-trans = TODAY
           tt-docum-est.ok-observ = c-autorizacao
           tt-docum-est.cod-chave-aces-nf-eletro = c-chave
           tt-docum-est.int-2 = fi-cod-devol.

    IF l-snfd THEN
        ASSIGN tt-docum-est.observacao = "Devoluá∆o Sem Nota Origem Informada // " + fi-motivo.
    ELSE
        ASSIGN tt-docum-est.observacao = "Devoluá∆o Ref. Ö NF " + nota-fiscal.nr-nota-fis +  
                                         "de " + STRING(nota-fiscal.dt-emis,"99/99/9999") +
                                         "no Valor de " + STRING(nota-fiscal.vl-tot-nota) +
                                         " // " + fi-motivo                                         .
    ASSIGN de-tot-valor = 0
           de-tot-qtde = 0.

    FOR EACH tt-itens-sel NO-LOCK 
             BY tt-itens-sel.it-codigo.

        RUN pi-acompanhar IN h-acomp (INPUT "Criando Itens: " + tt-itens-sel.it-codigo).

        IF NOT l-snfd THEN DO.
           FIND nota-fiscal WHERE
                nota-fiscal.cod-estabel = c-cod-estabel AND
                nota-fiscal.serie = c-serie-orig AND
                nota-fiscal.nr-nota-fis = tt-itens-sel.nr-nota-fis NO-LOCK NO-ERROR.
    
           FIND it-nota-fisc OF nota-fiscal WHERE
                it-nota-fisc.nr-seq-ped = tt-itens-sel.nr-seq-ped NO-LOCK NO-ERROR.
    
           IF NOT AVAIL it-nota-fisc THEN DO.
              MESSAGE 'Item ' tt-itens-sel.it-codigo ' Seq: ' it-nota-fisc.nr-seq-ped ' N∆o Encontrado na Nota Fiscal' SKIP
                      'Ser† Desconsiderado...'
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
              NEXT.
           END.
    
           /* Itens das Devoluá‰es */
           CREATE tt-item-devol-cli.
           ASSIGN tt-item-devol-cli.rw-it-nota-fisc = ROWID(it-nota-fisc)
                  tt-item-devol-cli.quant-devol     = tt-itens-sel.qtd /* it-nota-fisc.qt-faturada[1] */
                  tt-item-devol-cli.preco-devol     = tt-itens-sel.vlr-tot /* it-nota-fisc.vl-tot-item */
                  tt-item-devol-cli.cod-depos       = 'EXP' 
                  tt-item-devol-cli.reabre-pd       = NO
                  tt-item-devol-cli.vl-desconto     = 0.
        END.
        ELSE DO.
            /* Cria os Itens do Recebimento */
            CREATE tt-item-doc-est.
            ASSIGN tt-item-doc-est.nro-docto      = tt-docum-est.nro-docto
                   tt-item-doc-est.serie-docto    = tt-docum-est.serie-docto
                   tt-item-doc-est.cod-emitente   = tt-docum-est.cod-emit
                   tt-item-doc-est.nat-operacao   = tt-docum-est.nat-operacao
                   tt-item-doc-est.sequencia      = tt-itens-sel.nr-seq
                   tt-item-doc-est.cod-depos      = 'EXP'
                   tt-item-doc-est.it-codigo      = tt-itens-sel.it-codigo
                   tt-item-doc-est.cod-refer      = tt-itens-sel.cod-refer
                   tt-item-doc-est.lote           = tt-itens-sel.nr-lote + tt-itens-sel.cod-refer
                   tt-item-doc-est.qt-do-forn     = ROUND(tt-itens-sel.qtd,4)
                   tt-item-doc-est.preco-total[1] = tt-itens-sel.vlr-tot
                   tt-item-doc-est.preco-unit[1]  = tt-itens-sel.vlr-unit
                   tt-item-doc-est.peso-liquido   = ROUND(tt-itens-sel.qtd,4)
                   tt-item-doc-est.dec-1          = ROUND(tt-itens-sel.qtd,4)
                   tt-item-doc-est.base-icm       = tt-itens-sel.vlr-tot
                   tt-item-doc-est.base-pis       = tt-itens-sel.vlr-tot
                   tt-item-doc-est.base-cof       = tt-itens-sel.vlr-tot
                   tt-item-doc-est.aliquota-icm   = natur-oper.aliquota-icm
                   tt-item-doc-est.valor-icm      = tt-itens-sel.vlr-tot * natur-oper.aliquota-icm / 100
                   tt-item-doc-est.base-ipi       = tt-itens-sel.vlr-tot
                   tt-item-doc-est.dt-vali-lote   = 12.31.9999.
        END.

        ASSIGN de-tot-valor = de-tot-valor + tt-itens-sel.vlr-tot /* it-nota-fisc.vl-tot-item */
               de-tot-qtde = de-tot-qtde + tt-itens-sel.qtd /* it-nota-fisc.qt-faturada[1] */.
    END.
    ASSIGN tt-docum-est.tot-valor = fi-sel-vlr-total
           tt-docum-est.valor-mercad = fi-sel-vlr-total
           tt-docum-est.base-icm = fi-sel-vlr-total
           tt-docum-est.icm-deb-cre = fi-sel-vlr-total * natur-oper.aliquota-icm / 100
           tt-docum-est.base-ipi = fi-sel-vlr-total
           tt-docum-est.despesa-nota = 0.

    IF l-snfd THEN
       ASSIGN tt-docum-est.tot-peso = de-tot-qtde
              SUBSTR(tt-docum-est.char-1,157,10) = STRING(de-tot-qtde).
           
    RUN pi-acompanhar IN h-acomp (INPUT "Criando Recebimento").
  
    /* Cria Recebimento Fiscal */
    RUN openQueryStatic IN h-boin090 (INPUT "Main":U).
    RUN emptyRowErrors IN h-boin090.
    RUN setRecord IN h-boin090 (INPUT TABLE tt-docum-est).
    RUN createRecord IN h-boin090.
    RUN getRowErrors IN h-boin090 (OUTPUT TABLE RowErrors).

    IF CAN-FIND(FIRST RowErrors 
               WHERE RowErrors.ErrorSubType = "ERROR":U  ) THEN DO:

       RUN pi-finalizar IN h-acomp.
     
       FOR EACH rowerrors WHERE
                RowErrors.ErrorSubType = "ERROR":U:
           MESSAGE "(" + STRING(RowErrors.ErrorNumber) +
                   ") Erro ao Gerar o Recebimento" SKIP
                    rowerrors.errordescription 
                   VIEW-AS ALERT-BOX.
       END.
       IF VALID-HANDLE(h-boin090) THEN
          DELETE PROCEDURE h-boin090.

       IF VALID-HANDLE(h-boin176) THEN
          DELETE PROCEDURE h-boin176.

       IF VALID-HANDLE(h-boin367) THEN
          DELETE PROCEDURE h-boin367.

       RETURN 'ADM-ERROR'.
    END.

    RUN pi-acompanhar IN h-acomp (INPUT "Criando Itens Recebimento").
    
    /* Cria Itens do Recebimento*/
    RUN openQueryStatic IN h-boin176 (INPUT "Main":U).
    RUN emptyRowErrors IN h-boin176.

    FIND FIRST tt-item-devol-cli NO-LOCK NO-ERROR.
    IF AVAIL tt-item-devol-cli THEN
       RUN createItemOfNotaFiscal IN h-boin176 (INPUT h-boin090,
                                                INPUT TABLE tt-item-devol-cli ).
    ELSE DO.
       FOR EACH tt-item-doc-est:
           EMPTY TEMP-TABLE wt-item-doc-est.
           CREATE wt-item-doc-est.
           BUFFER-COPY tt-item-doc-est TO wt-item-doc-est.

           RUN setRecord IN h-boin176 (INPUT TABLE wt-item-doc-est).
           RUN createRecord IN h-boin176.
       END.
    END.

    RUN getRowErrors IN h-boin176 (OUTPUT TABLE RowErrors).
    IF CAN-FIND(FIRST RowErrors 
              WHERE RowErrors.ErrorSubType = "ERROR":U  ) THEN DO:
   
       RUN pi-finalizar IN h-acomp.
   
       FOR EACH rowerrors WHERE
                RowErrors.ErrorSubType = "ERROR":U:
           MESSAGE "Erro ao Gerar os Itens do Recebimento" SKIP
                   rowerrors.errordescription
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
       END.
       IF VALID-HANDLE(h-boin090) THEN
          DELETE PROCEDURE h-boin090.
   
       IF VALID-HANDLE(h-boin176) THEN
          DELETE PROCEDURE h-boin176.
   
       IF VALID-HANDLE(h-boin367) THEN
          DELETE PROCEDURE h-boin367.

       RETURN 'ADM-ERROR'.
    END.

    RUN pi-finalizar IN h-acomp.

    IF VALID-HANDLE(h-boin090) THEN
       DELETE PROCEDURE h-boin090.
    
    IF VALID-HANDLE(h-boin176) THEN
       DELETE PROCEDURE h-boin176.

    IF VALID-HANDLE(h-boin367) THEN
       DELETE PROCEDURE h-boin367.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-retorna-etq B-table-Win 
PROCEDURE pi-retorna-etq :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH tt-etq-dev NO-LOCK.
        FIND ob-etiqueta WHERE 
             ob-etiqueta.cod-estabel = c-cod-estabel AND 
             ob-etiqueta.num-etiqueta = tt-etq-dev.num-etiqueta
             SHARE-LOCK NO-ERROR.
        
        IF AVAIL ob-etiqueta THEN DO.
           CREATE movto-etq.
           ASSIGN movto-etq.cod-estabel = ob-etiqueta.cod-estabel
                  movto-etq.dt-trans = TODAY
                  movto-etq.esp-docto = "DEV"
                  movto-etq.nro-docto = tt-etq-dev.nr-nota-fis
                  movto-etq.num-etiqueta = ob-etiqueta.num-etiqueta 
                  movto-etq.quantidade = ob-etiqueta.quantidade
                  movto-etq.tipo-trans = YES
                  movto-etq.char-1 = "Usuario: " + c-seg-usuario + FILL(" ",10) +
                                     "Hora: " + STRING(TIME,"HH:MM:SS") + FILL(" ",10) +
                                     "Sit Ant: " + "5"  + FILL(" ",5) +
                                     "Sit Nova: " + "3" + FILL(" ",5) +
                                     "Programa: " + "ESSP0200".

           ASSIGN ob-etiqueta.situacao = 3
                  ob-etiqueta.localizacao = c-doca-msg.

           FIND ped-item-rom WHERE
                ped-item-rom.cod-estabel = ob-etiqueta.cod-estabel AND
                ped-item-rom.num-etiqueta = ob-etiqueta.num-etiqueta EXCLUSIVE-LOCK.

           CREATE dev-item-rom.
           BUFFER-COPY ped-item-rom TO dev-item-rom.
           DELETE ped-item-rom.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-totais-etq-dev B-table-Win 
PROCEDURE pi-totais-etq-dev :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN fi-etq-dev-qtd-total = 0.
    FOR EACH tt-etq-dev NO-LOCK.
        ASSIGN fi-etq-dev-qtd-total = fi-etq-dev-qtd-total + tt-etq-dev.quantidade.
    END.
    DISP fi-etq-dev-qtd-total
         WITH FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-totais-sel B-table-Win 
PROCEDURE pi-totais-sel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN fi-sel-qtd-total = 0
           fi-sel-vlr-total = 0.
    FOR EACH tt-itens-sel NO-LOCK.
        ASSIGN fi-sel-qtd-total = fi-sel-qtd-total + tt-itens-sel.qtde-item
               fi-sel-vlr-total = fi-sel-vlr-total + tt-itens-sel.vlr-total.
    END.

    DISP fi-sel-qtd-total
         fi-sel-vlr-total 
         WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-validate B-table-Win 
PROCEDURE pi-validate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF fi-cod-transp  = "" THEN DO.
       MESSAGE 'Favor Informar a Transportadora correta !'
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
       APPLY "ENTRY":U TO fi-cod-transp IN FRAME {&FRAME-NAME}.
       RETURN 'ADM-ERROR'.
    END.

    IF cb-frete = ? THEN DO.
       MESSAGE 'Favor Informar o tipo Frete!'
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
       APPLY "ENTRY":U TO cb-frete IN FRAME {&FRAME-NAME}.
       RETURN 'ADM-ERROR'.
    END.

    IF fi-cod-devol = 0 THEN DO.
       MESSAGE 'Favor Informar o C¢digo da Devoluá∆o '
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
       APPLY "ENTRY":U TO fi-cod-devol IN FRAME {&FRAME-NAME}.
       RETURN 'ADM-ERROR'.
    END.

    FIND transporte WHERE 
         transporte.nome-abrev = fi-cod-transp NO-LOCK NO-ERROR.
    IF NOT AVAIL transporte THEN DO.
       MESSAGE 'Transportadora n∆o Cadastrada....'
           VIEW-AS ALERT-BOX ERROR BUTTONS OK.
       APPLY 'ENTRY' TO fi-cod-transp.
       RETURN 'ADM-ERROR'.
    END.

    FIND param-re WHERE
         param-re.usuario = c-seg-usuario NO-LOCK NO-ERROR.

    IF NOT AVAIL param-re THEN DO:
       MESSAGE "Usu†rio n∆o Cadastrado no Recebimento..."
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
       RETURN 'ADM-ERROR'.
    END.

    IF AVAIL param-re AND 
       param-re.baixa-estoq = NO THEN DO:
       MESSAGE "Usu†rio n∆o atualiza estoque automaticamente..."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
       RETURN 'ADM-ERROR'.
    END.

    IF fi-sel-qtd-total <> fi-etq-dev-qtd-total THEN DO.
       MESSAGE "Quantidade Total dos Itens, n∆o confere com o Total das Etiquetas Devolvidas" 
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
       ASSIGN l-erro = YES.
    END.

    /* Consiste Itens Devolvidos com ss Etiquetas Devolvidas */
    ASSIGN l-erro = NO.
    FOR EACH b-tt-itens-sel NO-LOCK.
        ASSIGN de-tot-etq-dev = 0.
        FOR EACH tt-etq-dev WHERE
                 tt-etq-dev.nr-nota-fis = b-tt-itens-sel.nr-nota-fis AND
                 tt-etq-dev.it-codigo = b-tt-itens-sel.it-codigo AND
                 tt-etq-dev.cod-refer = b-tt-itens-sel.cod-refer AND
                 tt-etq-dev.nr-lote = b-tt-itens-sel.nr-lote NO-LOCK.
            ASSIGN de-tot-etq-dev = de-tot-etq-dev + tt-etq-dev.quantidade.
        END.

        IF b-tt-itens-sel.qtd <> de-tot-etq-dev THEN DO.
           MESSAGE "Quantidade do Item " b-tt-itens-sel.it-codigo " , n∆o confere com as Etiquetas Devolvidas" 
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
           ASSIGN l-erro = YES.
        END.
    END.

    IF l-erro THEN   
       RETURN 'ADM-ERROR'.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records B-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-itens-sel"}
  {src/adm/template/snd-list.i "tt-etq-fat"}
  {src/adm/template/snd-list.i "tt-etq-dev"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed B-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/bstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

