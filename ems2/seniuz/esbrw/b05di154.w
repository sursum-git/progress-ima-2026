&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r11 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i B99XX999 9.99.99.999}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEF TEMP-TABLE tt-itens 
    FIELD it-codigo    LIKE ped-item.it-codigo
    FIELD qt-lida      LIKE ped-item.qt-pedida COLUMN-LABEL "Qt Lida" 
    FIELD qt-reservada LIKE ped-item.qt-pedida COLUMN-LABEL "Qt Reservada"
    FIELD qt-aberta    LIKE ped-item.qt-pedida COLUMN-LABEL "Qt Aberta"
    FIELD qt-regra     LIKE ped-item.qt-pedida COLUMN-LABEL "Qt Regra"
    FIELD qt-fila      LIKE ped-item.qt-pedida COLUMN-LABEL "Qt Fila"
    FIELD qt-crivada   LIKE ped-item.qt-pedida COLUMN-LABEL "Qt Crivada"
    INDEX indice1 IS PRIMARY it-codigo.

DEF TEMP-TABLE tt-refer
    FIELD it-codigo    LIKE ped-item.it-codigo
    FIELD cod-refer    LIKE ped-item.cod-refer 
    FIELD lote         LIKE ped-item-ext.lote 
    FIELD corte-comerc LIKE ped-item-ext.corte-comerc
    FIELD qt-pedida    LIKE ped-item.qt-pedida
    FIELD qt-reservada LIKE ped-item.qt-pedida
    FIELD qt-crivada   LIKE ped-item.qt-pedida
    FIELD qt-regra     LIKE ped-item.qt-pedida 
    FIELD qt-fila      LIKE ped-item.qt-pedida
    INDEX indice1 it-codigo cod-refer lote corte-comerc.

DEF TEMP-TABLE tt-saldo-item
    FIELD it-codigo         LIKE ped-item.it-codigo
    FIELD cod-refer         LIKE ped-item.cod-refer 
    FIELD lote              LIKE ped-item-ext.lote 
    FIELD corte-comerc      LIKE ped-item-ext.corte-comerc
    FIELD vrf-tecelagem     AS   LOG
    FIELD qt-sulzer         LIKE ob-etiqueta.quantidade
    FIELD qt-outras-tecelag LIKE ob-etiqueta.quantidade
    FIELD qtidade-atu       LIKE ob-etiqueta.quantidade
    FIELD qt-reservada      LIKE ob-etiqueta.quantidade
    FIELD qt-sem-localiz    LIKE ob-etiqueta.quantidade
    FIELD qt-sem-qualid     LIKE ob-etiqueta.quantidade
    FIELD qt-sem-emb-neutra LIKE ob-etiqueta.quantidade
    INDEX indice1 it-codigo cod-refer lote corte-comerc.

DEF TEMP-TABLE tt-itens-ped LIKE ped-item
    FIELD cod-estabel  LIKE ped-venda.cod-estabel
    FIELD lote         LIKE ped-item-ext.lote 
    FIELD corte-comerc LIKE ped-item-ext.corte-comerc
    FIELD qt-reservada LIKE ped-item.qt-pedida
    FIELD qt-crivada   LIKE ped-item.qt-pedida
    FIELD qt-regra     LIKE ped-item.qt-pedida 
    FIELD qt-fila      LIKE ped-item.qt-pedida 
    FIELD nome-transp  LIKE ped-venda.nome-transp
    FIELD tecelagem    LIKE ped-venda-ext.tecelagem
    FIELD emb-neutra   LIKE ped-venda-ext.l-emb-neutra
    FIELD regra        AS CHAR
    FIELD res-completa AS LOG INIT NO
    FIELD atende       AS LOG INIT NO
    FIELD exportacao   AS LOG INIT NO
    FIELD visualiza    AS LOG INIT NO.

DEF TEMP-TABLE tt-etiquetas
    FIELD cod-estabel  LIKE ob-etiqueta.cod-estabel
    FIELD nr-pedcli    LIKE ped-item.nr-pedcli 
    FIELD nr-sequencia LIKE ped-item.nr-sequencia
    FIELD num-etiqueta LIKE ob-etiqueta.num-etiqueta
    FIELD visualiza    AS   LOG
    INDEX indice1 IS PRIMARY nr-pedcli nr-sequencia num-etiqueta
    INDEX indice2 num-etiqueta.

DEF TEMP-TABLE wt-docas
    FIELD localiz       LIKE ob-etiqueta.localizacao
    FIELD it-codigo     LIKE ped-item.it-codigo
    FIELD cod-refer     LIKE ped-item.cod-refer 
    FIELD lote          LIKE ped-item-ext.lote 
    FIELD corte-comerc  LIKE ped-item-ext.corte-comerc
    FIELD tecelagem     AS   CHAR
    FIELD metragem-tot  LIKE ob-etiqueta.quantidade
    FIELD metragem-disp LIKE ob-etiqueta.quantidade.

DEF TEMP-TABLE wt-etiquetas
    FIELD cod-estabel  LIKE ob-etiqueta.cod-estabel
    FIELD it-codigo    LIKE ped-item.it-codigo
    FIELD cod-refer    LIKE ped-item.cod-refer 
    FIELD lote         LIKE ped-item-ext.lote 
    FIELD corte-comerc LIKE ped-item-ext.corte-comerc
    FIELD reservada    AS   LOG INIT NO
    FIELD num-etiqueta LIKE ob-etiqueta.num-etiqueta
    FIELD localizacao  LIKE ob-etiqueta.localizacao
    FIELD quantidade   LIKE ob-etiqueta.quantidade
    FIELD tipo-tear    AS CHAR
    INDEX indice1 IS PRIMARY localizacao ASCENDING num-etiqueta DESCENDING
    INDEX indice2 quantidade
    INDEX indice3 num-etiqueta.

DEF TEMP-TABLE tt-digita
    FIELD opcao AS CHAR
    FIELD campo AS CHAR
    FIELD valor AS CHAR.

DEF BUFFER b-wt-etiquetas FOR wt-etiquetas.
DEF BUFFER b-tt-itens-ped FOR tt-itens-ped.
DEF BUFFER b-wt-docas     FOR wt-docas.

DEF TEMP-TABLE tt-ped-item NO-UNDO LIKE ped-item
    FIELD r-rowid AS ROWID.

DEF VAR c-desc-item   LIKE ITEM.desc-item.

DEF VAR h-query       AS HANDLE.
DEF VAR h-acomp       AS HANDLE NO-UNDO.
DEF VAR da-dt-entrega AS DATE.
DEF VAR c-dia         AS CHAR.
DEF VAR c-lotes       AS CHAR.
DEF VAR c-desc-dentro AS CHAR. 
DEF VAR c-empresa     AS CHAR.

DEF VAR de-sulzer          LIKE ob-etiqueta.quantidade.
DEF VAR de-outras-tecelag  LIKE ob-etiqueta.quantidade.
DEF VAR c-lst-tecelag      AS CHAR.
DEF VAR l-vrf-tecelagem    AS LOG.

DEF VAR c-cod-estabel      AS CHAR.      
DEF VAR c-dt-limite        AS CHAR.      
DEF VAR c-it-codigo-ini    AS CHAR.      
DEF VAR c-it-codigo-fin    AS CHAR.      
DEF VAR c-cod-refer-ini    AS CHAR.      
DEF VAR c-cod-refer-fin    AS CHAR.      
DEF VAR c-nr-pedcli-ini    AS CHAR.      
DEF VAR c-nr-pedcli-fin    AS CHAR.      
DEF VAR i-nr-seq-ini       AS INT.       
DEF VAR i-nr-seq-fin       AS INT.       
DEF VAR c-localiz-ini      LIKE ob-etiqueta.localiz.
DEF VAR c-localiz-fin      LIKE ob-etiqueta.localiz.
DEF VAR c-corte-comerc-ini LIKE ob-etiqueta.corte-comerc.
DEF VAR c-corte-comerc-fin LIKE ob-etiqueta.corte-comerc.
DEF VAR c-cod-obsoleto-ini AS CHAR.      
DEF VAR c-cod-obsoleto-fin AS CHAR.      
DEF VAR de-perc-min        AS DEC.       
DEF VAR de-perc-max        AS DEC.       
DEF VAR l-lote-todos       AS LOG.
DEF VAR l-lote-pp          AS LOG.
DEF VAR l-lote-pd          AS LOG.
DEF VAR l-lote-rp          AS LOG.
DEF VAR l-lote-rd          AS LOG.
DEF VAR l-lote-sc          AS LOG.
DEF VAR l-lote-ca          AS LOG.
DEF VAR c-tp-artigo        AS CHAR.
DEF VAR l-res-crivo        AS LOG.
DEF VAR l-acomp            AS LOG.

DEF VAR c-cod-refer  LIKE ped-item.it-codigo.

DEF VAR l-reservar   AS LOG.
DEF VAR l-fila-ativa AS LOG.
DEF VAR l-ok         AS LOG.
DEF VAR de-lida      AS DEC.
DEF VAR de-reservada AS DEC.
DEF VAR de-aberta    AS DEC.
DEF VAR de-regra     AS DEC.
DEF VAR de-fila      AS DEC.
DEF VAR de-crivada   AS DEC.
DEF VAR i-lin        AS INT.
DEF VAR i-pag        AS INT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-itens

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-itens tt-refer

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br-itens                                      */
&Scoped-define FIELDS-IN-QUERY-br-itens tt-itens.it-codigo fn-desc-item() @ c-desc-item   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-itens   
&Scoped-define SELF-NAME br-itens
&Scoped-define OPEN-QUERY-br-itens RUN pi-totais. OPEN QUERY {&SELF-NAME} FOR EACH tt-itens WHERE NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-itens tt-itens
&Scoped-define FIRST-TABLE-IN-QUERY-br-itens tt-itens


/* Definitions for BROWSE br-refer                                      */
&Scoped-define FIELDS-IN-QUERY-br-refer tt-refer.cod-refer tt-refer.lote tt-refer.corte-comerc tt-refer.qt-pedida tt-refer.qt-reservada tt-refer.qt-regra tt-refer.qt-fila tt-refer.qt-crivada   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-refer   
&Scoped-define SELF-NAME br-refer
&Scoped-define QUERY-STRING-br-refer FOR EACH tt-refer WHERE                                  NO-LOCK BY tt-refer.cod-refer
&Scoped-define OPEN-QUERY-br-refer OPEN QUERY {&SELF-NAME} FOR EACH tt-refer WHERE                                  NO-LOCK BY tt-refer.cod-refer.
&Scoped-define TABLES-IN-QUERY-br-refer tt-refer
&Scoped-define FIRST-TABLE-IN-QUERY-br-refer tt-refer


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-br-refer}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-itens br-refer RECT-3 RECT-5 bt-reserva ~
bt-reserva-all bt-all-regra bt-regra bt-fila bt-crivado bt-vapra ~
bt-etiquetas bt-imprime 
&Scoped-Define DISPLAYED-OBJECTS fi-tot-lida-it fi-tot-lida fi-tot-res-it ~
fi-tot-res fi-tot-regra-it fi-tot-regra fi-tot-fila-it fi-tot-fila ~
fi-tot-crivada-it fi-tot-crivada 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-4 bt-reserva bt-regra bt-fila bt-crivado 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" B-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<FOREIGN-KEYS>
nr-ord-produ||y|mgmov.ped-item.nr-ord-produ
nr-programa||y|mgmov.ped-item.nr-programa
it-codigo||y|mgmov.ped-item.it-codigo
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "nr-ord-produ,nr-programa,it-codigo"':U).

/* Tell the ADM to use the OPEN-QUERY-CASES. */
&Scoped-define OPEN-QUERY-CASES RUN dispatch ('open-query-cases':U).
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

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-desc-item B-table-Win 
FUNCTION fn-desc-item RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-all-regra 
     IMAGE-UP FILE "image/imt-all-bloq.bmp":U
     LABEL "" 
     SIZE 4 BY 1.29 TOOLTIP "TODOS Pedidos N∆o Reservados por Regras"
     BGCOLOR 8 .

DEFINE BUTTON bt-crivado 
     IMAGE-UP FILE "image/im-aval.bmp":U
     LABEL "" 
     SIZE 4 BY 1.29 TOOLTIP "Pedidos N∆o Reservados por CrÇdito"
     BGCOLOR 8 .

DEFINE BUTTON bt-etiquetas 
     IMAGE-UP FILE "image/im-local.bmp":U
     LABEL "" 
     SIZE 4 BY 1.29 TOOLTIP "Detalha Item Selecionado"
     BGCOLOR 8 .

DEFINE BUTTON bt-fila 
     IMAGE-UP FILE "image/im-aloca.bmp":U
     IMAGE-INSENSITIVE FILE "image/im-aloci.bmp":U
     LABEL "" 
     SIZE 4 BY 1.29 TOOLTIP "Pedidos N∆o Reservados por Fila"
     BGCOLOR 8 .

DEFINE BUTTON bt-imprime 
     IMAGE-UP FILE "image/im-pri.bmp":U
     LABEL "&Imprimir" 
     SIZE 4 BY 1.29 TOOLTIP "Relat¢rio de Itens Separados"
     BGCOLOR 8 .

DEFINE BUTTON bt-ok AUTO-GO 
     IMAGE-UP FILE "image/im-cq.bmp":U
     LABEL "OK" 
     SIZE 4 BY 1.25 TOOLTIP "Salva Reserva Autom†tica para os Itens".

DEFINE BUTTON bt-regra 
     IMAGE-UP FILE "image/im-bloq.bmp":U
     IMAGE-INSENSITIVE FILE "image/im-bloqi.bmp":U
     LABEL "" 
     SIZE 4 BY 1.29 TOOLTIP "Pedidos N∆o Reservados por Regra"
     BGCOLOR 8 .

DEFINE BUTTON bt-reserva 
     IMAGE-UP FILE "image/imt-res.bmp":U
     LABEL "" 
     SIZE 4 BY 1.29 TOOLTIP "Pedidos Reservados da Referància Marcada"
     BGCOLOR 8 .

DEFINE BUTTON bt-reserva-all 
     IMAGE-UP FILE "image/im-todos.bmp":U
     LABEL "" 
     SIZE 4 BY 1.29 TOOLTIP "TODOS os Pedidos Reservados"
     BGCOLOR 8 .

DEFINE BUTTON bt-vapra 
     IMAGE-UP FILE "image/im-vapra.bmp":U
     LABEL "" 
     SIZE 4 BY 1.29 TOOLTIP "V† para o Item"
     BGCOLOR 8 .

DEFINE VARIABLE fi-tot-crivada AS DECIMAL FORMAT "z,zzz,zz9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.29 BY .88
     FONT 2 NO-UNDO.

DEFINE VARIABLE fi-tot-crivada-it AS DECIMAL FORMAT "z,zzz,zz9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.29 BY .88
     FONT 2 NO-UNDO.

DEFINE VARIABLE fi-tot-fila AS DECIMAL FORMAT "z,zzz,zz9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.29 BY .88
     FONT 2 NO-UNDO.

DEFINE VARIABLE fi-tot-fila-it AS DECIMAL FORMAT "z,zzz,zz9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.29 BY .88
     FONT 2 NO-UNDO.

DEFINE VARIABLE fi-tot-lida AS DECIMAL FORMAT "Z,ZZZ,ZZ9.99":R INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.29 BY .88
     FONT 2 NO-UNDO.

DEFINE VARIABLE fi-tot-lida-it AS DECIMAL FORMAT "Z,ZZZ,ZZ9.99":R INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.29 BY .88
     FONT 2 NO-UNDO.

DEFINE VARIABLE fi-tot-regra AS DECIMAL FORMAT "z,zzz,zz9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.29 BY .88
     FONT 2 NO-UNDO.

DEFINE VARIABLE fi-tot-regra-it AS DECIMAL FORMAT "z,zzz,zz9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.29 BY .88
     FONT 2 NO-UNDO.

DEFINE VARIABLE fi-tot-res AS DECIMAL FORMAT "z,zzz,zz9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.29 BY .88
     FONT 2 NO-UNDO.

DEFINE VARIABLE fi-tot-res-it AS DECIMAL FORMAT "z,zzz,zz9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.29 BY .88
     FONT 2 NO-UNDO.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 65 BY 1.75
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 42 BY 6.25
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-itens FOR 
      tt-itens SCROLLING.

DEFINE QUERY br-refer FOR 
      tt-refer SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-itens
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-itens B-table-Win _FREEFORM
  QUERY br-itens NO-LOCK DISPLAY
      tt-itens.it-codigo FORMAT "x(8)":U                    WIDTH 8
      fn-desc-item() @ c-desc-item COLUMN-LABEL "Descriá∆o" WIDTH 30
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 42 BY 10.25
         FONT 1
         TITLE "Itens Processados" ROW-HEIGHT-CHARS .63.

DEFINE BROWSE br-refer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-refer B-table-Win _FREEFORM
  QUERY br-refer NO-LOCK DISPLAY
      tt-refer.cod-refer                                    WIDTH 7  COLUMN-LABEL "Refer"
      tt-refer.lote                                         WIDTH 3  COLUMN-LABEL "Lote" 
      tt-refer.corte-comerc                                          COLUMN-LABEL "Corte"     
      tt-refer.qt-pedida           FORMAT ">,>>>,>>9.99"    WIDTH 09 COLUMN-LABEL "Qt Pedida"
      tt-refer.qt-reservada        FORMAT ">,>>>,>>9.99"    WIDTH 10 COLUMN-LABEL "Qt Reservada"
      tt-refer.qt-regra            FORMAT ">,>>>,>>9.99"    WIDTH 08 COLUMN-LABEL "Qt Regra"   
      tt-refer.qt-fila             FORMAT ">,>>>,>>9.99"    WIDTH 09 COLUMN-LABEL "Qt Fila"    
      tt-refer.qt-crivada          FORMAT ">,>>>,>>9.99"    WIDTH 08 COLUMN-LABEL "Qt Crivada"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 65 BY 15
         FONT 1
         TITLE "Referàncias do Item" ROW-HEIGHT-CHARS .63.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br-itens AT ROW 1 COL 2
     br-refer AT ROW 1 COL 45
     fi-tot-lida-it AT ROW 12.75 COL 11 COLON-ALIGNED NO-LABEL
     fi-tot-lida AT ROW 12.75 COL 26 COLON-ALIGNED NO-LABEL
     fi-tot-res-it AT ROW 13.75 COL 11 COLON-ALIGNED NO-LABEL
     fi-tot-res AT ROW 13.75 COL 26 COLON-ALIGNED NO-LABEL
     fi-tot-regra-it AT ROW 14.75 COL 11 COLON-ALIGNED NO-LABEL
     fi-tot-regra AT ROW 14.75 COL 26 COLON-ALIGNED NO-LABEL
     fi-tot-fila-it AT ROW 15.75 COL 11 COLON-ALIGNED NO-LABEL
     fi-tot-fila AT ROW 15.75 COL 26 COLON-ALIGNED NO-LABEL
     bt-reserva AT ROW 16.5 COL 47
     bt-reserva-all AT ROW 16.5 COL 54.29
     bt-all-regra AT ROW 16.5 COL 58.43
     bt-regra AT ROW 16.5 COL 66.57
     bt-fila AT ROW 16.5 COL 70.72
     bt-crivado AT ROW 16.5 COL 74.86
     bt-vapra AT ROW 16.5 COL 85.14
     bt-etiquetas AT ROW 16.5 COL 89.14
     bt-imprime AT ROW 16.5 COL 93.14
     bt-ok AT ROW 16.5 COL 104.72
     fi-tot-crivada-it AT ROW 16.75 COL 11 COLON-ALIGNED NO-LABEL
     fi-tot-crivada AT ROW 16.75 COL 26 COLON-ALIGNED NO-LABEL
     "ITEM" VIEW-AS TEXT
          SIZE 6 BY .54 AT ROW 12.13 COL 13
          BGCOLOR 8 FONT 6
     "Crivada:" VIEW-AS TEXT
          SIZE 7.29 BY .54 AT ROW 16.88 COL 11.86 RIGHT-ALIGNED
          BGCOLOR 8 FGCOLOR 9 FONT 6
     "Fila:" VIEW-AS TEXT
          SIZE 3.14 BY .54 AT ROW 15.88 COL 11.14 RIGHT-ALIGNED
          BGCOLOR 8 FGCOLOR 9 FONT 6
     "Lida:" VIEW-AS TEXT
          SIZE 4.57 BY .54 AT ROW 12.88 COL 11.86 RIGHT-ALIGNED
          BGCOLOR 8 FGCOLOR 9 FONT 6
     "Reservada:" VIEW-AS TEXT
          SIZE 10 BY .54 AT ROW 13.88 COL 12 RIGHT-ALIGNED
          BGCOLOR 8 FGCOLOR 9 FONT 6
     "Regra:" VIEW-AS TEXT
          SIZE 5.14 BY .54 AT ROW 14.88 COL 11.14 RIGHT-ALIGNED
          BGCOLOR 8 FGCOLOR 9 FONT 6
     " Totais" VIEW-AS TEXT
          SIZE 9 BY .75 AT ROW 11.42 COL 3.29
          BGCOLOR 8 FGCOLOR 12 FONT 0
     "GERAL" VIEW-AS TEXT
          SIZE 7 BY .54 AT ROW 12.13 COL 28
          BGCOLOR 8 FONT 6
     RECT-3 AT ROW 16.25 COL 45
     RECT-5 AT ROW 11.75 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 FONT 1.


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
         HEIGHT             = 17.17
         WIDTH              = 109.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{include/c-browse.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB br-itens TEXT-7 F-Main */
/* BROWSE-TAB br-refer br-itens F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON bt-crivado IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR BUTTON bt-fila IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR BUTTON bt-ok IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-regra IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR BUTTON bt-reserva IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR FILL-IN fi-tot-crivada IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-crivada-it IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-fila IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-fila-it IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-lida IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-lida-it IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-regra IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-regra-it IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-res IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-res-it IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TEXT-LITERAL "Lida:"
          SIZE 4.57 BY .54 AT ROW 12.88 COL 11.86 RIGHT-ALIGNED         */

/* SETTINGS FOR TEXT-LITERAL "Reservada:"
          SIZE 10 BY .54 AT ROW 13.88 COL 12 RIGHT-ALIGNED              */

/* SETTINGS FOR TEXT-LITERAL "Regra:"
          SIZE 5.14 BY .54 AT ROW 14.88 COL 11.14 RIGHT-ALIGNED         */

/* SETTINGS FOR TEXT-LITERAL "Fila:"
          SIZE 3.14 BY .54 AT ROW 15.88 COL 11.14 RIGHT-ALIGNED         */

/* SETTINGS FOR TEXT-LITERAL "Crivada:"
          SIZE 7.29 BY .54 AT ROW 16.88 COL 11.86 RIGHT-ALIGNED         */

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-itens
/* Query rebuild information for BROWSE br-itens
     _START_FREEFORM
RUN pi-totais.
OPEN QUERY {&SELF-NAME} FOR EACH tt-itens WHERE NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Query            is NOT OPENED
*/  /* BROWSE br-itens */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-refer
/* Query rebuild information for BROWSE br-refer
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-refer WHERE
                                 NO-LOCK BY tt-refer.cod-refer.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-refer */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br-itens
&Scoped-define SELF-NAME br-itens
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-itens B-table-Win
ON VALUE-CHANGED OF br-itens IN FRAME F-Main /* Itens Processados */
DO:
    ASSIGN fi-tot-lida-it = 0
           fi-tot-lida-it = 0
           fi-tot-res-it =  0
           fi-tot-regra-it = 0
           fi-tot-fila-it = 0
           fi-tot-crivada-it = 0.

    IF AVAIL tt-itens THEN DO.
       ASSIGN fi-tot-lida-it = tt-itens.qt-lida
              fi-tot-res-it =  tt-itens.qt-reservada
              fi-tot-regra-it = tt-itens.qt-regra    
              fi-tot-fila-it = tt-itens.qt-fila     
              fi-tot-crivada-it = tt-itens.qt-crivada.
    
       EMPTY TEMP-TABLE tt-refer.
       FOR EACH tt-itens-ped WHERE
                tt-itens-ped.it-codigo = tt-itens.it-codigo NO-LOCK.
    
           FIND tt-refer WHERE
                tt-refer.it-codigo = tt-itens-ped.it-codigo AND
                tt-refer.cod-refer = tt-itens-ped.cod-refer AND  
                tt-refer.lote = tt-itens-ped.lote AND
                tt-refer.corte-comerc = tt-itens-ped.corte-comerc
                NO-LOCK NO-ERROR.
           IF NOT AVAIL tt-refer THEN DO.
              CREATE tt-refer.
              ASSIGN tt-refer.it-codigo = tt-itens-ped.it-codigo
                     tt-refer.cod-refer = tt-itens-ped.cod-refer
                     tt-refer.lote = tt-itens-ped.lote     
                     tt-refer.corte-comerc = tt-itens-ped.corte-comerc.
           END.
           ASSIGN tt-refer.qt-pedida = tt-refer.qt-pedida + tt-itens-ped.qt-pedida       
                  tt-refer.qt-reservada = tt-refer.qt-reservada + tt-itens-ped.qt-reservada 
                  tt-refer.qt-crivada = tt-refer.qt-crivada + tt-itens-ped.qt-crivada   
                  tt-refer.qt-regra = tt-refer.qt-regra + tt-itens-ped.qt-regra     
                  tt-refer.qt-fila = tt-refer.qt-fila + tt-itens-ped.qt-fila.
       END.
    END.
    
    DISPLAY fi-tot-lida-it
            fi-tot-res-it
            fi-tot-regra-it
            fi-tot-fila-it
            fi-tot-crivada-it
            WITH FRAME {&FRAME-NAME}.

    {&OPEN-QUERY-br-refer}
    APPLY 'value-changed' TO br-refer.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-refer
&Scoped-define SELF-NAME br-refer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-refer B-table-Win
ON VALUE-CHANGED OF br-refer IN FRAME F-Main /* Referàncias do Item */
DO:
   DISABLE {&list-4} WITH FRAME {&FRAME-NAME}.
   IF AVAIL tt-refer THEN DO.
      IF tt-refer.qt-reservada > 0 THEN
         ASSIGN bt-reserva:SENSITIVE = YES.

      IF tt-refer.qt-crivada > 0 THEN
         ASSIGN bt-crivado:SENSITIVE = YES.

      IF tt-refer.qt-regra > 0 THEN
         ASSIGN bt-regra:SENSITIVE = YES.

      IF tt-refer.qt-fila > 0 THEN
         ASSIGN bt-fila:SENSITIVE = YES.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-all-regra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-all-regra B-table-Win
ON CHOOSE OF bt-all-regra IN FRAME F-Main
DO:
    FOR EACH tt-itens-ped.
        ASSIGN tt-itens-ped.visualiza = NO.
    END.
    FOR EACH tt-etiquetas.
        ASSIGN tt-etiquetas.visualiza = NO.
    END.

    FOR EACH tt-itens WHERE
             tt-itens.qt-regra > 0 NO-LOCK.

        FOR EACH tt-itens-ped WHERE
                 tt-itens-ped.it-codigo = tt-itens.it-codigo AND 
                 tt-itens-ped.qt-regra > 0 EXCLUSIVE-LOCK.

            ASSIGN tt-itens-ped.visualiza = YES.

            FOR EACH tt-etiquetas WHERE
                     tt-etiquetas.nr-pedcli = tt-itens-ped.nr-pedcli AND 
                     tt-etiquetas.nr-sequencia = tt-itens-ped.nr-sequencia EXCLUSIVE-LOCK.
                ASSIGN tt-etiquetas.visualiza = YES.
            END.
        END.
    END.

    RUN esp/essp0160f.p (INPUT TABLE tt-itens-ped,
                         INPUT TABLE tt-etiquetas).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-crivado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-crivado B-table-Win
ON CHOOSE OF bt-crivado IN FRAME F-Main
DO:
    FOR EACH tt-itens-ped.
        ASSIGN tt-itens-ped.visualiza = NO.
    END.
    FOR EACH tt-itens-ped WHERE
             tt-itens-ped.qt-crivada > 0 AND
             tt-itens-ped.it-codigo = tt-refer.it-codigo AND
             tt-itens-ped.cod-refer = tt-refer.cod-refer AND
             tt-itens-ped.lote = tt-refer.lote AND
             tt-itens-ped.corte-comerc = tt-refer.corte-comerc EXCLUSIVE-LOCK.
        ASSIGN tt-itens-ped.visualiza = YES.
    END.
    RUN esp/essp0160b.p (INPUT TABLE tt-itens-ped,
                         INPUT TABLE tt-etiquetas).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-etiquetas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-etiquetas B-table-Win
ON CHOOSE OF bt-etiquetas IN FRAME F-Main
DO:
  RUN pi-imprime (INPUT NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-fila
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-fila B-table-Win
ON CHOOSE OF bt-fila IN FRAME F-Main
DO:
    FOR EACH tt-itens-ped.
        ASSIGN tt-itens-ped.visualiza = NO.
    END.
    FOR EACH tt-etiquetas.
        ASSIGN tt-etiquetas.visualiza = NO.
    END.

    FOR EACH tt-itens-ped WHERE
             tt-itens-ped.qt-fila > 0 AND
             tt-itens-ped.it-codigo = tt-refer.it-codigo AND
             tt-itens-ped.cod-refer = tt-refer.cod-refer AND
             tt-itens-ped.lote = tt-refer.lote AND
             tt-itens-ped.corte-comerc = tt-refer.corte-comerc EXCLUSIVE-LOCK.
        ASSIGN tt-itens-ped.visualiza = YES.
        FOR EACH tt-etiquetas WHERE
                 tt-etiquetas.nr-pedcli = tt-itens-ped.nr-pedcli AND 
                 tt-etiquetas.nr-sequencia = tt-itens-ped.nr-sequencia EXCLUSIVE-LOCK.
            ASSIGN tt-etiquetas.visualiza = YES.
        END.
    END.

    RUN esp/essp0160e.p (INPUT TABLE tt-itens-ped,
                         INPUT TABLE tt-etiquetas).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-imprime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-imprime B-table-Win
ON CHOOSE OF bt-imprime IN FRAME F-Main /* Imprimir */
DO:
  RUN pi-imprime (INPUT YES).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok B-table-Win
ON CHOOSE OF bt-ok IN FRAME F-Main /* OK */
DO:
    DO TRANSACTION:
       FOR EACH tt-itens-ped WHERE
                tt-itens-ped.qt-reservada > 0 NO-LOCK.

           FIND ped-item OF tt-itens-ped EXCLUSIVE-LOCK NO-ERROR.

           IF ped-item.cod-sit-item > 2 THEN DO.
              MESSAGE 'Sequencia' tt-itens-ped.nr-sequencia ' n∆o est† mais disponivel para ser Reservada...' 
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
              NEXT.
           END.

           /* Verifica se todas as Etiquetas selecionadas ainda est∆o dispon°ves */
           ASSIGN l-reservar = YES.
           FOR EACH tt-etiquetas WHERE
                    tt-etiquetas.nr-pedcli = tt-itens-ped.nr-pedcli AND
                    tt-etiquetas.nr-sequencia = tt-itens-ped.nr-sequencia NO-LOCK.
    
               FIND ob-etiqueta WHERE
                    ob-etiqueta.cod-estabel = tt-etiquetas.cod-estabel AND
                    ob-etiqueta.num-etiqueta = tt-etiquetas.num-etiqueta 
                    SHARE-LOCK NO-ERROR.

               IF ob-etiqueta.situacao <> 3 OR
                  ob-etiqueta.localizacao = '' THEN
                  ASSIGN l-reservar = NO.
           END.
           IF l-reservar = NO THEN DO.
              MESSAGE 'Pedido: ' tt-itens-ped.nr-pedcli
                      'Seq: ' tt-itens-ped.nr-sequencia SKIP
                      'Item :' tt-itens-ped.it-codigo
                      'Ref :' tt-itens-ped.cod-refer
                      'Lote :' tt-itens-ped.lote
                      'Corte: ' tt-itens-ped.corte-comerc SKIP(1)
                      'Existem Etiquetas selecionadas que j† foram Reservadas para' SKIP 
                      'outro Cliente/Pedido, reserva SERµ DESCONSIDERADA !!!'
                  VIEW-AS ALERT-BOX ERROR BUTTONS OK.
              NEXT.
           END.

           FIND corte-comerc WHERE
                corte-comerc.codigo = tt-itens-ped.corte-comerc 
                NO-LOCK NO-ERROR.
    
           {esinc/i-dsrb.i corte-comerc.tp-embalag corte-comerc.tp-embalag c-desc-dentro}.
           ASSIGN c-desc-dentro = UPPER(c-desc-dentro) + 'S'.
            
           CREATE ped-item-res.
           ASSIGN ped-item-res.cod-estabel  = tt-itens-ped.cod-estabel
                  ped-item-res.nome-abrev   = tt-itens-ped.nome-abrev
                  ped-item-res.nr-pedcli    = tt-itens-ped.nr-pedcli
                  ped-item-res.nr-sequencia = tt-itens-ped.nr-sequencia
                  ped-item-res.it-codigo    = tt-itens-ped.it-codigo
                  ped-item-res.cod-refer    = tt-itens-ped.cod-refer
                  ped-item-res.nome-transp  = tt-itens-ped.nome-transp
                  ped-item-res.sigla-emb    = tt-itens-ped.lote
                  ped-item-res.desc-dentro  = c-desc-dentro
                  ped-item-res.qt-pedida    = tt-itens-ped.qt-reservada
                  ped-item-res.dt-trans     = TODAY
                  ped-item-res.hr-trans     = STRING(TIME,"HH:MM:SS") + " Prog: ESSP0160  Usuario:" + c-seg-usuario
                  ped-item-res.lote         = tt-itens-ped.lote + tt-itens-ped.cod-refer.
    
           FOR EACH tt-etiquetas WHERE
                    tt-etiquetas.nr-pedcli = tt-itens-ped.nr-pedcli AND
                    tt-etiquetas.nr-sequencia = tt-itens-ped.nr-sequencia NO-LOCK.
    
               FIND ob-etiqueta WHERE
                    ob-etiqueta.cod-estabel = tt-etiquetas.cod-estabel AND
                    ob-etiqueta.num-etiqueta = tt-etiquetas.num-etiqueta 
                    SHARE-LOCK NO-ERROR.
    
               FIND ped-item-rom WHERE
                    ped-item-rom.cod-estabel = ob-etiqueta.cod-estabel AND
                    ped-item-rom.num-etiqueta = ob-etiqueta.num-etiqueta NO-LOCK NO-ERROR.
    
               IF NOT AVAIL ped-item-rom THEN DO.
                  CREATE ped-item-rom.
                  ASSIGN ped-item-rom.cod-estabel = ob-etiqueta.cod-estabel
                         ped-item-rom.nome-abrev = ped-item-res.nome-abrev
                         ped-item-rom.nr-pedcli = ped-item-res.nr-pedcli
                         ped-item-rom.nr-sequencia = ped-item-res.nr-sequencia
                         ped-item-rom.num-etiqueta = ob-etiqueta.num-etiqueta
                         ped-item-rom.nr-ob = ob-etiqueta.nr-ob
                         ped-item-rom.nr-seq-etq = ob-etiqueta.nr-sequencia
                         ped-item-rom.quantidade = ob-etiqueta.quantidade.
               END.
               ASSIGN ob-etiqueta.situacao = 4.
           END.
    
           IF tt-itens-ped.qt-pedida <> tt-itens-ped.qt-reservada THEN DO.
              EMPTY TEMP-TABLE tt-ped-item.
              CREATE tt-ped-item.
              BUFFER-COPY tt-itens-ped TO tt-ped-item
                          ASSIGN tt-ped-item.qt-pedida = tt-itens-ped.qt-reservada.

              RUN esapi/altera-peditem-v2.p (INPUT TABLE tt-ped-item).

              IF RETURN-VALUE = 'NOK' THEN DO.
                 MESSAGE "Erro ao Alterar a Quantidade do Item no Pedido" SKIP
                         " Pedido:" tt-itens-ped.nr-pedcli
                         " Cliente:" tt-itens-ped.nome-abrev
                         " Item:" tt-itens-ped.it-codigo
                         " Refer:" tt-itens-ped.cod-refer
                         " Qtd:" tt-itens-ped.qt-pedida SKIP(2)
                         "Reserva N«O ser† Efetuada..."
                     VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                 UNDO, NEXT.
              END.
              RUN esapi/completa-pedvenda.p (INPUT tt-itens-ped.nr-pedcli).
           END.
       END.
    END.
    ASSIGN bt-ok:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-regra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-regra B-table-Win
ON CHOOSE OF bt-regra IN FRAME F-Main
DO:
    FOR EACH tt-itens-ped.
        ASSIGN tt-itens-ped.visualiza = NO.
    END.
    FOR EACH tt-etiquetas.
        ASSIGN tt-etiquetas.visualiza = NO.
    END.

    FOR EACH tt-itens-ped WHERE
             tt-itens-ped.qt-regra > 0 AND
             tt-itens-ped.it-codigo = tt-refer.it-codigo AND
             tt-itens-ped.cod-refer = tt-refer.cod-refer AND
             tt-itens-ped.lote = tt-refer.lote AND
             tt-itens-ped.corte-comerc = tt-refer.corte-comerc EXCLUSIVE-LOCK.
        ASSIGN tt-itens-ped.visualiza = YES.

        FOR EACH tt-etiquetas WHERE
                 tt-etiquetas.nr-pedcli = tt-itens-ped.nr-pedcli AND 
                 tt-etiquetas.nr-sequencia = tt-itens-ped.nr-sequencia EXCLUSIVE-LOCK.
            ASSIGN tt-etiquetas.visualiza = YES.
        END.
    END.

    RUN esp/essp0160d.p (INPUT TABLE tt-itens-ped,
                         INPUT TABLE tt-etiquetas).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-reserva
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-reserva B-table-Win
ON CHOOSE OF bt-reserva IN FRAME F-Main
DO:
    FOR EACH tt-itens-ped.
        ASSIGN tt-itens-ped.visualiza = NO.
    END.
    FOR EACH tt-etiquetas.
        ASSIGN tt-etiquetas.visualiza = NO.
    END.

    FOR EACH tt-itens-ped WHERE
             tt-itens-ped.qt-reservada > 0 AND
             tt-itens-ped.it-codigo = tt-refer.it-codigo AND
             tt-itens-ped.cod-refer = tt-refer.cod-refer AND
             tt-itens-ped.lote = tt-refer.lote AND
             tt-itens-ped.corte-comerc = tt-refer.corte-comerc EXCLUSIVE-LOCK.
        ASSIGN tt-itens-ped.visualiza = YES.

        FOR EACH tt-etiquetas WHERE
                 tt-etiquetas.nr-pedcli = tt-itens-ped.nr-pedcli AND 
                 tt-etiquetas.nr-sequencia = tt-itens-ped.nr-sequencia EXCLUSIVE-LOCK.
            ASSIGN tt-etiquetas.visualiza = YES.
        END.
    END.
    RUN esp/essp0160a.p (INPUT TABLE tt-itens-ped,
                         INPUT TABLE tt-etiquetas).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-reserva-all
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-reserva-all B-table-Win
ON CHOOSE OF bt-reserva-all IN FRAME F-Main
DO:
    FOR EACH tt-itens-ped.
        ASSIGN tt-itens-ped.visualiza = NO.
    END.
    FOR EACH tt-etiquetas.
        ASSIGN tt-etiquetas.visualiza = NO.
    END.

    FOR EACH tt-itens-ped WHERE
             tt-itens-ped.qt-reservada > 0 EXCLUSIVE-LOCK.

        ASSIGN tt-itens-ped.visualiza = YES.

        FOR EACH tt-etiquetas WHERE
                 tt-etiquetas.nr-pedcli = tt-itens-ped.nr-pedcli AND 
                 tt-etiquetas.nr-sequencia = tt-itens-ped.nr-sequencia EXCLUSIVE-LOCK.
            ASSIGN tt-etiquetas.visualiza = YES.
        END.
    END.
    RUN esp/essp0160a.p (INPUT TABLE tt-itens-ped,
                         INPUT TABLE tt-etiquetas).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-vapra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-vapra B-table-Win
ON CHOOSE OF bt-vapra IN FRAME F-Main
DO:
  RUN esdlg/d01essp0160.w (OUTPUT c-cod-refer).
  IF c-cod-refer <> "" THEN DO:
     FIND FIRST tt-refer WHERE
                tt-refer.cod-refer = c-cod-refer NO-LOCK NO-ERROR.
     IF AVAIL tt-refer THEN
        h-query:REPOSITION-TO-ROWID(ROWID(tt-refer)) NO-ERROR. 
     ELSE
        MESSAGE "Item n∆o est† contido na seleá∆o!"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-itens
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
br-itens:NUM-LOCKED-COLUMNS = 2.

ASSIGN h-query = br-itens:QUERY.

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-open-query-cases B-table-Win  adm/support/_adm-opn.p
PROCEDURE adm-open-query-cases :
/*------------------------------------------------------------------------------
  Purpose:     Opens different cases of the query based on attributes
               such as the 'Key-Name', or 'SortBy-Case'
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* No Foreign keys are accepted by this SmartObject. */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imp-cabec B-table-Win 
PROCEDURE pi-imp-cabec :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    PUT c-empresa  FORMAT "X(40)"                 AT   1
        "DATA: "                                  AT  66
        STRING(TODAY,"99/99/9999") FORMAT "X(10)" AT  72
        "HORA: "                                  AT  88
        STRING(TIME,"hh:mm:ss")                   AT  94
        "PAGINA:"                                 AT 122
        i-pag FORMAT "999"                        AT 130
        SKIP(1).
        
    PUT "RELATORIO DA SEPARACAO AUTOMATICA" AT 51 SKIP(1).

    PUT "ITEM   DESCRICAO                             LIDA       RESERVADA           ABERTA           REGRA            FILA         CRIVADA" AT 1.
    PUT "------ ----------------------------- ------------    ------------     ------------    ------------    ------------    ------------" AT 1.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imprime B-table-Win 
PROCEDURE pi-imprime :
/*------------------------------F------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER p-imprime AS LOG.
   
    IF p-imprime = YES  THEN DO:
       SYSTEM-DIALOG PRINTER-SETUP UPDATE l-ok.
      /* OUTPUT TO PRINTER CONVERT TARGET "ISO8859-1" PAGED PAGE-SIZE 62.  */
      /*  OUTPUT TO PRINTER PAGED PAGE-SIZE 56. */
       OUTPUT TO PRINTER.
       PUT "~033(s18H".
    END.
    ELSE DO:
       OUTPUT TO "C:\TEMP\ESSP0160.LST".
       ASSIGN l-ok = YES.
    END.
       
    IF l-ok THEN DO:
       RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
       {utp/ut-liter.i Impress∆o *}
       RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).
          
       ASSIGN i-lin        = 99
              i-pag        =  1
              de-lida      =  0
              de-reservada =  0
              de-aberta    =  0
              de-regra     =  0
              de-fila      =  0
              de-crivada   =  0.
       
       FOR EACH tt-itens NO-LOCK BY tt-itens.it-codigo.
           RUN pi-acompanhar IN h-acomp (INPUT "Item: " + tt-itens.it-codigo).
   
           IF i-lin > 56 THEN DO:
              RUN pi-imp-cabec.
              ASSIGN i-lin = 7
                     i-pag = i-pag + 1.
           END.
   
           FIND ITEM WHERE
                ITEM.it-codigo = tt-itens.it-codigo NO-LOCK NO-ERROR.
   
           PUT tt-itens.it-codigo     FORMAT "x(6)"            AT   1
               ITEM.desc-item         FORMAT "x(29)"           AT   8
               tt-itens.qt-lida       FORMAT ">,>>>,>>9.99"    AT  38
               tt-itens.qt-reservada  FORMAT ">,>>>,>>9.99"    AT  54
               tt-itens.qt-aberta     FORMAT ">,>>>,>>9.99"    AT  71
               tt-itens.qt-regra      FORMAT ">,>>>,>>9.99"    AT  87
               tt-itens.qt-fila       FORMAT ">,>>>,>>9.99"    AT 103
               tt-itens.qt-crivada    FORMAT ">,>>>,>>9.99"    AT 119.
       
           ASSIGN i-lin        = i-lin        + 1
                  de-lida      = de-lida      + tt-itens.qt-lida
                  de-reservada = de-reservada + tt-itens.qt-reservada
                  de-aberta    = de-aberta    + tt-itens.qt-aberta
                  de-regra     = de-regra     + tt-itens.qt-regra
                  de-fila      = de-fila      + tt-itens.qt-fila
                  de-crivada   = de-crivada   + tt-itens.qt-crivada.
           FOR EACH tt-itens-ped WHERE tt-itens-ped.it-codigo = tt-itens.it-codigo NO-LOCK.
               IF i-lin > 56 THEN DO:
                  RUN pi-imp-cabec.
                  ASSIGN i-lin = 7
                         i-pag = i-pag + 1.
               END.
               PUT tt-itens-ped.nr-pedcli  FORMAT "x(6)"               AT  10
                   tt-itens-ped.nome-abrev FORMAT "x(11)"              AT  17
                   tt-itens-ped.cod-refer                              AT  31
                   tt-itens-ped.qt-pedida     FORMAT ">,>>>,>>9.99"    AT  40
                   tt-itens-ped.qt-reservada  FORMAT ">,>>>,>>9.99"    AT  56
                   tt-itens-ped.qt-pedida - tt-itens-ped.qt-reservada  FORMAT "->,>>>,>>9.99"    AT  72
                   tt-itens-ped.qt-regra      FORMAT ">,>>>,>>9.99"    AT  89
                   tt-itens-ped.qt-crivada    FORMAT ">,>>>,>>9.99"    AT 121.
               ASSIGN i-lin = i-lin + 1.
               FOR EACH tt-etiquetas WHERE tt-etiquetas.nr-pedcli    = tt-itens-ped.nr-pedcli 
                                       AND tt-etiquetas.nr-sequencia = tt-itens-ped.nr-sequencia NO-LOCK. 
                   FIND ob-etiqueta WHERE
                        ob-etiqueta.cod-estabel  = tt-etiquetas.cod-estabel AND
                        ob-etiqueta.num-etiqueta = tt-etiquetas.num-etiqueta NO-LOCK NO-ERROR.
                   IF NOT AVAIL ob-etiqueta THEN NEXT.
                   /* FIND ordem-benefic WHERE
                           ordem-benefic.nr-ob = ob-etiqueta.nr-ob  NO-LOCK NO-ERROR.
                   IF NOT AVAIL ordem-benefic THEN NEXT. */
                   IF i-lin > 56 THEN DO:
                      RUN pi-imp-cabec.
                      ASSIGN i-lin = 7
                             i-pag = i-pag + 1.
                   END.
                   PUT ob-etiqueta.num-etiqueta  FORMAT "999999999"        AT  12
                   /*  ordem-benefic.tipo-tear                             AT  22 */
                       ob-etiqueta.localizacao FORMAT "XXX/XXX"            AT  30
                       ob-etiqueta.quantidade  FORMAT ">>9.99"             AT  64.
                   ASSIGN i-lin = i-lin + 1.
               END.
           END.
           PUT "" AT 1.
           ASSIGN i-lin = i-lin + 1.
       END.
       IF de-lida      <> 0 OR  de-reservada <> 0 OR
          de-aberta    <> 0 OR  de-regra     <> 0 OR
          de-fila      <> 0 OR  de-crivada   <> 0 THEN DO:
          PUT "------------    ------------     ------------    ------------    ------------    ------------"   AT 38.
          PUT "T O T A I S  . . . ."  AT 08
              de-lida       FORMAT ">,>>>,>>9.99" AT  38
              de-reservada  FORMAT ">,>>>,>>9.99" AT  54 
              de-aberta     FORMAT ">,>>>,>>9.99" AT  71 
              de-regra      FORMAT ">,>>>,>>9.99" AT  87 
              de-fila       FORMAT ">,>>>,>>9.99" AT 103
              de-crivada    FORMAT ">,>>>,>>9.99" AT 119.
       END.
       OUTPUT CLOSE.
       RUN pi-finalizar in h-acomp.
    END.
       
    IF p-imprime = NO THEN DO. /* CONSULTA */
       RUN utp/ut-utils.p PERSISTENT SET h-prog.
       RUN EXECUTE IN h-prog(INPUT "notepad.exe", 
                             INPUT "C:\TEMP\ESSP0160.LST").
       DELETE PROCEDURE h-prog.
    END.
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
     FOR EACH tt-itens.
         DELETE tt-itens.
     END.
     RUN adm-open-query-cases.
     ASSIGN bt-ok:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-marca-pedidos B-table-Win 
PROCEDURE pi-marca-pedidos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR de-tot-itens AS DEC.
    FOR EACH b-tt-itens-ped WHERE
             b-tt-itens-ped.it-codigo = tt-itens-ped.it-codigo AND
             b-tt-itens-ped.cod-refer = tt-itens-ped.cod-refer AND
             b-tt-itens-ped.lote = tt-itens-ped.lote AND
             b-tt-itens-ped.corte-comerc = tt-itens-ped.corte-comerc EXCLUSIVE-LOCK.

        ASSIGN de-tot-itens = de-tot-itens + tt-itens-ped.qt-pedida.

        IF de-tot-itens + b-tt-itens-ped.qt-pedida > de-sulzer + de-outras-tecelag THEN 
           LEAVE.

        ASSIGN b-tt-itens-ped.atende = YES.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-proc-regra B-table-Win 
PROCEDURE pi-proc-regra :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF l-acomp THEN
       MESSAGE 'Pedido: ' tt-itens-ped.nr-pedcli
               'Seq: ' tt-itens-ped.nr-sequencia SKIP
               'Vou Calcular a Regra... ' SKIP
               'Saldo N∆o Reservado: ' tt-saldo-item.qtidade-atu SKIP
               'Etiquetas Sem Localiz: ' tt-saldo-item.qt-sem-localiz SKIP
               'Etiquetas Qualid A: ' tt-saldo-item.qt-sem-qualid SKIP
               'Qt Pedida: ' tt-itens-ped.qt-pedida
               VIEW-AS ALERT-BOX INFO BUTTONS OK.

    IF tt-saldo-item.qtidade-atu + tt-saldo-item.qt-sem-localiz + 
       tt-saldo-item.qt-sem-qualid + tt-saldo-item.qt-sem-emb-neutra <= tt-itens-ped.qt-pedida - (tt-itens-ped.qt-pedida * (de-perc-min / 100)) THEN 
       RETURN 'METRAGEM'.

    IF tt-saldo-item.vrf-tecelagem THEN DO.
       CASE tt-itens-ped.tecelagem.
           WHEN 'Todas' THEN DO.
               IF tt-saldo-item.qt-sulzer + tt-saldo-item.qt-outras-tecelag >= tt-itens-ped.qt-pedida - (tt-itens-ped.qt-pedida * (de-perc-min / 100)) AND
                  tt-saldo-item.qt-sulzer < tt-itens-ped.qt-pedida - (tt-itens-ped.qt-pedida * (de-perc-min / 100)) AND
                  tt-saldo-item.qt-outras-tecelag < tt-itens-ped.qt-pedida - (tt-itens-ped.qt-pedida * (de-perc-min / 100)) THEN
                  RETURN 'TECELAGEM'.
           END.
           WHEN 'SULZER' THEN
               IF tt-saldo-item.qt-sulzer < tt-itens-ped.qt-pedida - (tt-itens-ped.qt-pedida * (de-perc-min / 100)) THEN 
                  RETURN 'TECELAGEM'.
           OTHERWISE
               IF tt-saldo-item.qt-outras-tecelag < tt-itens-ped.qt-pedida - (tt-itens-ped.qt-pedida * (de-perc-min / 100)) THEN
                  RETURN 'TECELAGEM'.
       END.
    END.

    IF tt-itens-ped.exportacao AND
       tt-saldo-item.qtidade-atu + tt-saldo-item.qt-sem-qualid > tt-itens-ped.qt-pedida - (tt-itens-ped.qt-pedida * (de-perc-min / 100)) THEN 
       RETURN "QUALIDADE".

    IF tt-itens-ped.emb-neutra AND
       tt-saldo-item.qtidade-atu + tt-saldo-item.qt-sem-emb-neutra > tt-itens-ped.qt-pedida - (tt-itens-ped.qt-pedida * (de-perc-min / 100)) THEN 
       RETURN "EMBALAGEM NEUTRA".

    IF tt-saldo-item.qtidade-atu < tt-itens-ped.qt-pedida - (tt-itens-ped.qt-pedida * (de-perc-min / 100)) AND
       tt-saldo-item.qt-sem-localiz <> 0 AND
       tt-saldo-item.qtidade-atu + tt-saldo-item.qt-sem-localiz + tt-saldo-item.qt-sem-qualid >= tt-itens-ped.qt-pedida - (tt-itens-ped.qt-pedida * (de-perc-min / 100)) THEN 
       RETURN "LOCALIZAÄ«O".

    RETURN 'TOLER∂NCIA'.
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
    DEF INPUT PARAMETER p-cod-estabel      AS CHAR.
    DEF INPUT PARAMETER p-dt-limite        AS CHAR.
    DEF INPUT PARAMETER p-it-codigo-ini    AS CHAR.
    DEF INPUT PARAMETER p-it-codigo-fin    AS CHAR.
    DEF INPUT PARAMETER p-cod-refer-ini    AS CHAR.
    DEF INPUT PARAMETER p-cod-refer-fin    AS CHAR.
    DEF INPUT PARAMETER p-nr-pedcli-ini    AS CHAR.
    DEF INPUT PARAMETER p-nr-pedcli-fin    AS CHAR.
    DEF INPUT PARAMETER p-nr-seq-ini       AS INT. 
    DEF INPUT PARAMETER p-nr-seq-fin       AS INT.
    DEF INPUT PARAMETER p-localiz-ini      LIKE ob-etiqueta.localiz.
    DEF INPUT PARAMETER p-localiz-fin      LIKE ob-etiqueta.localiz.
    DEF INPUT PARAMETER p-corte-comerc-ini LIKE ob-etiqueta.corte-comerc.
    DEF INPUT PARAMETER p-corte-comerc-fin LIKE ob-etiqueta.corte-comerc.
    DEF INPUT PARAMETER p-cod-obsoleto-ini AS CHAR.                              
    DEF INPUT PARAMETER p-cod-obsoleto-fin AS CHAR.
    DEF INPUT PARAMETER p-perc-min         AS DEC.
    DEF INPUT PARAMETER p-perc-max         AS DEC.
    DEF INPUT PARAMETER p-lote-todos       AS LOG.
    DEF INPUT PARAMETER p-lote-pp          AS LOG.
    DEF INPUT PARAMETER p-lote-pd          AS LOG.
    DEF INPUT PARAMETER p-lote-rp          AS LOG.
    DEF INPUT PARAMETER p-lote-rd          AS LOG.
    DEF INPUT PARAMETER p-lote-sc          AS LOG.
    DEF INPUT PARAMETER p-lote-ca          AS LOG.
    DEF INPUT PARAMETER p-tp-artigo        AS CHAR.
    DEF INPUT PARAMETER p-res-crivo        AS LOG.
    DEF INPUT PARAMETER p-acomp            AS LOG.
    DEF INPUT PARAMETER TABLE FOR tt-digita.  

    &SCOPED-DEFINE BREAK-BY tt-itens-ped.it-codigo + tt-itens-ped.cod-refer + tt-itens-ped.lote + tt-itens-ped.corte-comerc

    ASSIGN c-cod-estabel      = p-cod-estabel
           c-dt-limite        = p-dt-limite
           c-it-codigo-ini    = p-it-codigo-ini
           c-it-codigo-fin    = p-it-codigo-fin
           c-cod-refer-ini    = p-cod-refer-ini
           c-cod-refer-fin    = p-cod-refer-fin
           c-nr-pedcli-ini    = p-nr-pedcli-ini
           c-nr-pedcli-fin    = p-nr-pedcli-fin
           i-nr-seq-ini       = p-nr-seq-ini 
           i-nr-seq-fin       = p-nr-seq-fin 
           c-localiz-ini      = p-localiz-ini
           c-localiz-fin      = p-localiz-fin
           c-corte-comerc-ini = p-corte-comerc-ini
           c-corte-comerc-fin = p-corte-comerc-fin
           c-cod-obsoleto-ini = p-cod-obsoleto-ini 
           c-cod-obsoleto-fin = p-cod-obsoleto-fin 
           de-perc-min        = p-perc-min 
           de-perc-max        = p-perc-max 
           l-lote-todos       = p-lote-todos
           l-lote-pp          = p-lote-pp  
           l-lote-pd          = p-lote-pd  
           l-lote-rp          = p-lote-rp  
           l-lote-rd          = p-lote-rd  
           l-lote-sc          = p-lote-sc  
           l-lote-ca          = p-lote-ca  
           c-tp-artigo        = p-tp-artigo
           l-res-crivo        = p-res-crivo
           l-acomp            = p-acomp.

    EMPTY TEMP-TABLE tt-itens.
    EMPTY TEMP-TABLE tt-refer.
    EMPTY TEMP-TABLE tt-itens-ped.
    EMPTY TEMP-TABLE tt-etiquetas.
    EMPTY TEMP-TABLE wt-etiquetas.
    EMPTY TEMP-TABLE tt-saldo-item.

    /* Busca Nome da Empresa */
    FIND FIRST param-global NO-LOCK NO-ERROR.
    FIND FIRST empresa
         WHERE empresa.ep-codigo = param-global.empresa-prin NO-LOCK NO-ERROR. 
    ASSIGN c-empresa = (IF AVAIL empresa THEN empresa.razao-social ELSE "").

    RUN utp/ut-acomp.p PERSISTENT SET h-acomp.

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

    {utp/ut-liter.i Calculando_Carteira *}
    RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

    RUN pi-separa-pedidos.

    {utp/ut-liter.i Separando_Etiquetas *}
    RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

    RUN pi-separa-etiquetas.

    /* Separa quais sequàncias poder∆o ser atendidas */
    FOR EACH tt-itens-ped WHERE
             (tt-itens-ped.qt-crivada <> 0 AND l-res-crivo OR tt-itens-ped.qt-crivada = 0)
             BREAK BY {&BREAK-BY}
                   BY tt-itens-ped.dt-entrega
                   BY tt-itens-ped.nr-pedcli
                   BY tt-itens-ped.nr-sequencia.

        IF l-acomp THEN
           MESSAGE 'Pedido: ' tt-itens-ped.nr-pedcli
                   'Seq: ' tt-itens-ped.nr-sequencia SKIP
                   'Item :' tt-itens-ped.it-codigo
                   'Ref :' tt-itens-ped.cod-refer
                   'Lote :' tt-itens-ped.lote
                   'Corte: ' tt-itens-ped.corte-comerc SKIP
                   'Dt Entrega :' tt-itens-ped.dt-entrega
                   'Quantidade: ' tt-itens-ped.qt-pedida
                   VIEW-AS ALERT-BOX INFO BUTTONS OK.

        IF FIRST-OF({&BREAK-BY}) THEN   /* se primeiro item,refer,lote,corte... separa etiquetas */
           ASSIGN l-fila-ativa = NO.

        FIND tt-saldo-item WHERE
             tt-saldo-item.it-codigo = tt-itens-ped.it-codigo AND
             tt-saldo-item.cod-refer = tt-itens-ped.cod-refer AND  
             tt-saldo-item.lote = tt-itens-ped.lote AND
             tt-saldo-item.corte-comerc = tt-itens-ped.corte-comerc NO-ERROR.

        IF tt-saldo-item.qtidade-atu >= tt-saldo-item.qt-reservada + 
                                        (tt-itens-ped.qt-pedida - (tt-itens-ped.qt-pedida * (de-perc-min / 100))) AND
           l-fila-ativa = NO THEN
           ASSIGN tt-saldo-item.qt-reservada = tt-saldo-item.qt-reservada + tt-itens-ped.qt-pedida
                  tt-itens-ped.atende = YES.
        ELSE
           ASSIGN l-fila-ativa = YES.

        IF l-acomp THEN
           MESSAGE "Quantidade Atual: " tt-saldo-item.qtidade-atu
                   VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.

    /* Dentro dos pedidos poss°veis de serem atendidos, atende primeiro os com tecelagem Espec°fica */
    FOR EACH tt-itens-ped WHERE
             tt-itens-ped.atende = YES AND
             tt-itens-ped.tecelagem <> 'Todas' NO-LOCK,
       FIRST tt-saldo-item WHERE
             tt-saldo-item.it-codigo = tt-itens-ped.it-codigo AND
             tt-saldo-item.cod-refer = tt-itens-ped.cod-refer AND
             tt-saldo-item.lote = tt-itens-ped.lote AND
             tt-saldo-item.corte-comerc = tt-itens-ped.corte-comerc 
             NO-LOCK
        BREAK BY {&BREAK-BY}
              BY tt-itens-ped.dt-entrega
              BY tt-itens-ped.nr-pedcli
              BY tt-itens-ped.nr-sequencia.

        IF FIRST-OF({&BREAK-BY}) THEN   /* se primeiro item,refer,lote,corte... separa etiquetas */
           ASSIGN l-fila-ativa = NO.

        IF l-fila-ativa THEN DO.
           IF l-acomp THEN
              MESSAGE 'Pedido: ' tt-itens-ped.nr-pedcli
                      'Seq: ' tt-itens-ped.nr-sequencia SKIP(2)
                      'Parado na FILA...'
                       VIEW-AS ALERT-BOX INFO BUTTONS OK.

           ASSIGN tt-itens-ped.qt-fila = tt-itens-ped.qt-pedida.
           NEXT.
        END.

        IF tt-itens-ped.tecelagem = 'SULZER' THEN DO. /* Cliente quer apenas Sulzer */
           IF tt-saldo-item.qt-sulzer < tt-itens-ped.qt-pedida - (tt-itens-ped.qt-pedida * (de-perc-min / 100)) THEN DO.
              RUN pi-proc-regra.
              ASSIGN tt-itens-ped.regra = RETURN-VALUE
                     l-fila-ativa = YES.
           END.
           ELSE
              ASSIGN c-lst-tecelag = tt-itens-ped.tecelagem.
        END.
        ELSE DO.
           IF tt-saldo-item.qt-outras-tecelag < tt-itens-ped.qt-pedida - (tt-itens-ped.qt-pedida * (de-perc-min / 100)) THEN DO.
              RUN pi-proc-regra.
              ASSIGN tt-itens-ped.regra = RETURN-VALUE
                     l-fila-ativa = YES.
           END.
           ELSE
              ASSIGN c-lst-tecelag = 'NISSAN,PICANOL'.
        END.

        IF tt-itens-ped.regra <> '' THEN DO.
           IF l-acomp THEN
              MESSAGE 'Pedido: ' tt-itens-ped.nr-pedcli
                      'Seq:' tt-itens-ped.nr-sequencia
                      'Qt Pedida: ' tt-itens-ped.qt-pedida
                      'Parado por REGRA'
                       VIEW-AS ALERT-BOX ERROR BUTTONS OK.

           ASSIGN tt-itens-ped.qt-regra = tt-itens-ped.qt-pedida.
           NEXT.
        END.

        IF l-acomp THEN
           MESSAGE 'Pedido: ' tt-itens-ped.nr-pedcli
                   'Seq: ' tt-itens-ped.nr-sequencia SKIP
                   'Vou Atender a Tecelagem: ' c-lst-tecelag  
                   VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RUN pi-separa-docas.
    END.

    /* Dentro dos pedidos poss°veis de serem atendidos, atende os pedidos que aceitam qualquer tecelagem  */
    FOR EACH tt-itens-ped WHERE
             tt-itens-ped.atende = YES AND
             tt-itens-ped.tecelagem = 'Todas' NO-LOCK,
       FIRST tt-saldo-item WHERE
             tt-saldo-item.it-codigo = tt-itens-ped.it-codigo AND
             tt-saldo-item.cod-refer = tt-itens-ped.cod-refer AND
             tt-saldo-item.lote = tt-itens-ped.lote AND
             tt-saldo-item.corte-comerc = tt-itens-ped.corte-comerc NO-LOCK
        BREAK BY {&BREAK-BY}
              BY tt-itens-ped.dt-entrega
              BY tt-itens-ped.nr-pedcli
              BY tt-itens-ped.nr-sequencia.

        IF FIRST-OF({&BREAK-BY}) THEN   /* se primeiro item,refer,lote,corte... separa etiquetas */
           ASSIGN l-fila-ativa = NO.

        IF l-fila-ativa THEN DO.
           IF l-acomp THEN
              MESSAGE 'Pedido: ' tt-itens-ped.nr-pedcli
                      'Seq: ' tt-itens-ped.nr-sequencia SKIP(2)
                      'Parado na FILA...'
                       VIEW-AS ALERT-BOX INFO BUTTONS OK.

           ASSIGN tt-itens-ped.qt-fila = tt-itens-ped.qt-pedida.
           NEXT.
        END.

        ASSIGN c-lst-tecelag = 'NISSAN,PICANOL,SULZER,'.
        IF tt-saldo-item.vrf-tecelagem = YES THEN DO.
           ASSIGN c-lst-tecelag = 'NISSAN,PICANOL'.
           IF tt-saldo-item.qt-sulzer > tt-saldo-item.qt-outras-tecelag THEN
              ASSIGN c-lst-tecelag = 'SULZER'.

           IF l-acomp THEN
              MESSAGE 'Pedido: ' tt-itens-ped.nr-pedcli
                      'Seq: ' tt-itens-ped.nr-sequencia SKIP
                      'Comeáar com a Tecelagem: ' c-lst-tecelag  
                      VIEW-AS ALERT-BOX INFO BUTTONS OK.
        END.

        RUN pi-separa-docas.
        IF NOT tt-itens-ped.res-completa THEN DO.
           IF tt-saldo-item.vrf-tecelagem THEN DO.
              ASSIGN c-lst-tecelag = IF c-lst-tecelag = 'SULZER'
                                     THEN 'NISSAN,PICANOL'
                                     ELSE 'SULZER'.

              RUN pi-separa-docas.

              /* Se Completou a Reserva com outra Tecelagem, pr¢ximo item */
              IF tt-itens-ped.res-completa THEN NEXT. 
           END.

           IF l-acomp THEN
              MESSAGE 'Pedido: ' tt-itens-ped.nr-pedcli
                      'Seq:' tt-itens-ped.nr-sequencia
                      'Qt Pedida: ' tt-itens-ped.qt-pedida
                      'Parado por REGRA'
                       VIEW-AS ALERT-BOX ERROR BUTTONS OK.

           ASSIGN tt-itens-ped.qt-regra = tt-itens-ped.qt-pedida.

           /* Se chegou aqui Ç porque o pedido parou por alguma regra, ent∆o vamos
              descobrir qual foi */
    
           RUN pi-proc-regra.
           ASSIGN tt-itens-ped.regra = RETURN-VALUE
                  l-fila-ativa = YES.
        END.
    END.  

    FOR EACH tt-itens-ped WHERE
            (tt-itens-ped.qt-crivada <> 0 AND l-res-crivo OR tt-itens-ped.qt-crivada = 0) AND
             tt-itens-ped.res-completa = NO AND
             tt-itens-ped.qt-regra = 0
        BREAK BY {&BREAK-BY}
              BY tt-itens-ped.dt-entrega
              BY tt-itens-ped.nr-pedcli
              BY tt-itens-ped.nr-sequencia.

        FIND tt-saldo-item WHERE
             tt-saldo-item.it-codigo = tt-itens-ped.it-codigo AND
             tt-saldo-item.cod-refer = tt-itens-ped.cod-refer AND
             tt-saldo-item.lote = tt-itens-ped.lote AND
             tt-saldo-item.corte-comerc = tt-itens-ped.corte-comerc NO-ERROR.

        ASSIGN tt-itens-ped.qt-fila = tt-itens-ped.qt-pedida.
        IF FIRST-OF({&BREAK-BY}) THEN DO.
           FIND FIRST b-tt-itens-ped WHERE
                      b-tt-itens-ped.it-codigo = tt-itens-ped.it-codigo AND
                      b-tt-itens-ped.cod-refer = tt-itens-ped.cod-refer AND
                      b-tt-itens-ped.lote = tt-itens-ped.lote AND
                      b-tt-itens-ped.corte-comerc = tt-itens-ped.corte-comerc AND
                      b-tt-itens-ped.qt-regra > 0 NO-ERROR.
           IF NOT AVAIL b-tt-itens-ped THEN DO.
              ASSIGN tt-itens-ped.qt-regra = tt-itens-ped.qt-pedida
                     tt-itens-ped.qt-fila = 0.

              RUN pi-proc-regra.
              ASSIGN tt-itens-ped.regra = RETURN-VALUE.
           END.
        END.
    END.

    FOR EACH tt-itens-ped.
        FIND tt-itens WHERE
             tt-itens.it-codigo = tt-itens-ped.it-codigo
             NO-LOCK NO-ERROR.

        IF NOT AVAIL tt-itens THEN DO.
           CREATE tt-itens.
           ASSIGN tt-itens.it-codigo = tt-itens-ped.it-codigo.
        END.
        ASSIGN tt-itens.qt-lida = tt-itens.qt-lida + tt-itens-ped.qt-pedida
               tt-itens.qt-reservada = tt-itens.qt-reservada + tt-itens-ped.qt-reservada
               tt-itens.qt-regra = tt-itens.qt-regra + tt-itens-ped.qt-regra
               tt-itens.qt-fila = tt-itens.qt-fila + tt-itens-ped.qt-fila
               tt-itens.qt-crivada = tt-itens.qt-crivada + tt-itens-ped.qt-crivada
               tt-itens.qt-aberta = tt-itens.qt-lida - tt-itens.qt-reservada.

        IF tt-itens.qt-aberta < 0 THEN
           ASSIGN tt-itens.qt-aberta = 0.

    END.
    ASSIGN bt-ok:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
    IF CAN-FIND(FIRST tt-itens WHERE
                      tt-itens.qt-reservada > 0) THEN
       ASSIGN bt-ok:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

    RUN pi-finalizar in h-acomp.
    RUN adm-open-query-cases.

    APPLY 'VALUE-CHANGED' TO br-itens.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-separa-docas B-table-Win 
PROCEDURE pi-separa-docas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF l-acomp THEN
       MESSAGE 'Pedido: ' tt-itens-ped.nr-pedcli
               'Seq: ' tt-itens-ped.nr-sequencia SKIP
               'Iniciar Separaá∆o das DOCAS' SKIP(2)
               'Sulzer : ' tt-saldo-item.qt-sulzer 
               'Outras : ' tt-saldo-item.qt-outras-tecelag SKIP
               "Qt Item: " tt-itens-ped.qt-pedida SKIP
               'Separar Tecelagem: ' c-lst-tecelag 
               VIEW-AS ALERT-BOX INFO BUTTONS OK.

    SEPARAR-DOCAS:
    FOR EACH wt-docas WHERE
             LOOKUP(wt-docas.tecelagem,c-lst-tecelag) > 0 AND
             wt-docas.it-codigo = tt-itens-ped.it-codigo AND
             wt-docas.cod-refer = tt-itens-ped.cod-refer AND
             wt-docas.lote = tt-itens-ped.lote AND
             wt-docas.corte-comerc = tt-itens-ped.corte-comerc AND
             wt-docas.metragem-disp > 0 NO-LOCK,
        EACH wt-etiquetas WHERE
             wt-etiquetas.cod-estabel = tt-itens-ped.cod-estabel AND
             wt-etiquetas.it-codigo = tt-itens-ped.it-codigo AND
             wt-etiquetas.cod-refer = tt-itens-ped.cod-refer AND
             wt-etiquetas.lote = tt-itens-ped.lote AND
             wt-etiquetas.corte-comerc = tt-itens-ped.corte-comerc AND
             wt-etiquetas.localiz = wt-docas.localiz AND
             wt-etiquetas.reservada = NO AND
             wt-etiquetas.tipo-tear = wt-docas.tecelagem 
             BY wt-docas.metragem-tot DESCENDING
             BY wt-etiquetas.num-etiqueta DESCENDING.

        IF l-acomp THEN
           MESSAGE 'Pedido: ' tt-itens-ped.nr-pedcli
                   'Seq: ' tt-itens-ped.nr-sequencia SKIP
                   'Doca: ' wt-docas.localiz SKIP
                   'Metragem: ' wt-docas.metragem-disp SKIP
                   'Tecelgem: ' wt-docas.tecelagem SKIP
                   VIEW-AS ALERT-BOX INFO BUTTONS OK.

        IF l-acomp THEN
           MESSAGE 'Pedido: ' tt-itens-ped.nr-pedcli
                   'Seq: ' tt-itens-ped.nr-sequencia SKIP
                   'Vou Tentar Reservar'
                   'Etiqueta: ' wt-etiquetas.num-etiqueta
                   'Localiz: ' wt-etiquetas.localiz
                   'QT Etiquenta: ' wt-etiquetas.quantidade SKIP(1)
                   'Qt Pedida: ' tt-itens-ped.qt-pedida
                   'Qt Reservada: ' tt-itens-ped.qt-reservada
                   VIEW-AS ALERT-BOX INFO BUTTONS OK.

        ASSIGN wt-etiquetas.reservada = YES
               tt-itens-ped.qt-reservada = tt-itens-ped.qt-reservada + wt-etiquetas.quantidade
               wt-docas.metragem-disp = wt-docas.metragem-disp - wt-etiquetas.quantidade.

        IF l-res-crivo AND
           tt-itens-ped.qt-crivada <> 0 THEN 
           ASSIGN tt-itens-ped.qt-crivada = tt-itens-ped.qt-crivada - wt-etiquetas.quantidade.

        IF tt-saldo-item.vrf-tecelagem THEN DO.
           IF wt-etiquetas.tipo-tear = 'SULZER' THEN
              ASSIGN tt-saldo-item.qt-sulzer = tt-saldo-item.qt-sulzer - wt-etiquetas.quantidade.
           ELSE
              ASSIGN tt-saldo-item.qt-outras-tecelag = tt-saldo-item.qt-outras-tecelag - wt-etiquetas.quantidade.
        END.
        ASSIGN tt-saldo-item.qtidade-atu = tt-saldo-item.qtidade-atu - wt-etiquetas.quantidade.

        FIND tt-etiquetas WHERE
             tt-etiquetas.cod-estabel = wt-etiquetas.cod-estabel AND
             tt-etiquetas.nr-pedcli = tt-itens-ped.nr-pedcli AND
             tt-etiquetas.nr-sequencia = tt-itens-ped.nr-sequencia AND
             tt-etiquetas.num-etiqueta = wt-etiquetas.num-etiqueta
             NO-ERROR.
        IF NOT AVAIL tt-etiquetas THEN DO.
           CREATE tt-etiquetas.
           ASSIGN tt-etiquetas.cod-estabel = wt-etiquetas.cod-estabel
                  tt-etiquetas.nr-pedcli = tt-itens-ped.nr-pedcli 
                  tt-etiquetas.nr-sequencia = tt-itens-ped.nr-sequencia 
                  tt-etiquetas.num-etiqueta = wt-etiquetas.num-etiqueta.
        END.

        IF l-acomp THEN
           MESSAGE 'Pedido: ' tt-itens-ped.nr-pedcli
                   'Seq: ' tt-itens-ped.nr-sequencia SKIP
                   'Reservei' SKIP
                   'Etiqueta: ' wt-etiquetas.num-etiqueta
                   'Localiz: ' wt-etiquetas.localiz
                   'QT Etiquenta: ' wt-etiquetas.quantidade
                   'Qt Pedida: ' tt-itens-ped.qt-pedida
                   'Qt Reservada: ' tt-itens-ped.qt-reservada
                   VIEW-AS ALERT-BOX INFO BUTTONS OK.
            
        IF tt-itens-ped.qt-reservada = tt-itens-ped.qt-pedida THEN DO.
           IF l-acomp THEN
              MESSAGE 'Pedido: ' tt-itens-ped.nr-pedcli
                      'Seq: ' tt-itens-ped.nr-sequencia SKIP
                      'Reserva Completa' SKIP
                      'Seq: ' tt-itens-ped.nr-sequencia
                      'Qt Pedida: ' tt-itens-ped.qt-pedida
                      'Qt Reservada: ' tt-itens-ped.qt-reservada
                   VIEW-AS ALERT-BOX INFO BUTTONS OK.
           ASSIGN tt-itens-ped.res-completa = YES.

           LEAVE SEPARAR-DOCAS.
        END.

        FIND corte-comerc WHERE
             corte-comerc.codigo = wt-etiquetas.corte-comerc NO-LOCK NO-ERROR.

        IF tt-itens-ped.qt-reservada > tt-itens-ped.qt-pedida THEN DO.
           IF l-acomp THEN
              MESSAGE 'Pedido: ' tt-itens-ped.nr-pedcli
                      'Seq: ' tt-itens-ped.nr-sequencia SKIP
                      'vou verificar o %Max e a media do corte comerc...'
                      VIEW-AS ALERT-BOX INFO BUTTONS OK.

           IF tt-itens-ped.qt-reservada <= tt-itens-ped.qt-pedida + (tt-itens-ped.qt-pedida * (de-perc-max / 100)) AND
              ABS(tt-itens-ped.qt-pedida - tt-itens-ped.qt-reservada) <= corte-comerc.compr-med THEN DO.
              IF l-acomp THEN
                 MESSAGE 'Pedido: ' tt-itens-ped.nr-pedcli
                         'Seq: ' tt-itens-ped.nr-sequencia SKIP
                         'Reserva Completa' SKIP
                         'Qt Pedida: ' tt-itens-ped.qt-pedida
                         'Qt Reservada: ' tt-itens-ped.qt-reservada
                     VIEW-AS ALERT-BOX INFO BUTTONS OK.

              ASSIGN tt-itens-ped.res-completa = YES.

              LEAVE SEPARAR-DOCAS.
           END.

           FIND FIRST tt-etiquetas WHERE
                      tt-etiquetas.num-etiqueta = wt-etiquetas.num-etiqueta
                      NO-ERROR.

           IF AVAIL tt-etiquetas THEN DO.
              IF l-acomp THEN
                 MESSAGE 'Pedido: ' tt-itens-ped.nr-pedcli
                         'Seq: ' tt-itens-ped.nr-sequencia SKIP
                         'Vou Retirar a Reserva' SKIP
                         'Etiqueta: ' wt-etiquetas.num-etiqueta
                         'Localiz: ' wt-etiquetas.localiz
                         'QT Etiquenta: ' wt-etiquetas.quantidade SKIP
                         'Qt Pedida: ' tt-itens-ped.qt-pedida
                         'Qt Reservada: ' tt-itens-ped.qt-reservada
                         VIEW-AS ALERT-BOX INFO BUTTONS OK.

              FIND FIRST b-wt-etiquetas WHERE
                         b-wt-etiquetas.num-etiqueta = tt-etiquetas.num-etiqueta 
                         USE-INDEX indice3 NO-ERROR.

              FIND b-wt-docas WHERE
                   b-wt-docas.localiz = b-wt-etiquetas.localizacao AND 
                   b-wt-docas.it-codigo = b-wt-etiquetas.it-codigo AND
                   b-wt-docas.cod-refer = b-wt-etiquetas.cod-refer AND
                   b-wt-docas.lote = b-wt-etiquetas.lote AND
                   b-wt-docas.corte-comerc = b-wt-etiquetas.corte-comerc AND
                   b-wt-docas.tecelagem = b-wt-etiquetas.tipo-tear NO-ERROR.

              ASSIGN b-wt-etiquetas.reservada = NO
                     tt-itens-ped.qt-reservada = tt-itens-ped.qt-reservada - b-wt-etiquetas.quantidade
                     b-wt-docas.metragem-disp = b-wt-docas.metragem-disp + b-wt-etiquetas.quantidade.

              IF tt-saldo-item.vrf-tecelagem THEN DO.
                 IF b-wt-etiquetas.tipo-tear = 'SULZER' THEN
                    ASSIGN tt-saldo-item.qt-sulzer = tt-saldo-item.qt-sulzer + b-wt-etiquetas.quantidade.
                 ELSE
                    ASSIGN tt-saldo-item.qt-outras-tecelag = tt-saldo-item.qt-outras-tecelag + b-wt-etiquetas.quantidade.
              END.
              ASSIGN tt-saldo-item.qtidade-atu = tt-saldo-item.qtidade-atu + b-wt-etiquetas.quantidade.

              DELETE tt-etiquetas.
           END.

           IF l-acomp THEN
              MESSAGE 'Pedido: ' tt-itens-ped.nr-pedcli
                      'Seq: ' tt-itens-ped.nr-sequencia SKIP
                      'Vou olhar %MIN' SKIP
                       tt-itens-ped.qt-reservada SKIP
                       tt-itens-ped.qt-pedida - (tt-itens-ped.qt-pedida * (de-perc-min / 100))
                       VIEW-AS ALERT-BOX INFO BUTTONS OK.

           IF tt-itens-ped.qt-reservada >= tt-itens-ped.qt-pedida - (tt-itens-ped.qt-pedida * (de-perc-min / 100)) AND 
              ABS(tt-itens-ped.qt-pedida - tt-itens-ped.qt-reservada) <= corte-comerc.compr-med THEN DO.
              IF l-acomp THEN
                 MESSAGE 'Pedido: ' tt-itens-ped.nr-pedcli
                         'Seq: ' tt-itens-ped.nr-sequencia SKIP
                         'Reserva Completa....' SKIP
                         'Qt Pedida: ' tt-itens-ped.qt-pedida
                         'Qt Reservada: ' tt-itens-ped.qt-reservada
                     VIEW-AS ALERT-BOX INFO BUTTONS OK.

              ASSIGN tt-itens-ped.res-completa = YES.
              LEAVE SEPARAR-DOCAS.
           END.
        END.
    END.

    IF NOT tt-itens-ped.res-completa THEN DO.
       FIND corte-comerc WHERE
            corte-comerc.codigo = tt-itens-ped.corte-comerc NO-LOCK NO-ERROR.

       IF l-acomp THEN 
          MESSAGE 'Pedido: ' tt-itens-ped.nr-pedcli
                  'Seq: ' tt-itens-ped.nr-sequencia SKIP
                  'N∆o Completou a Reserva, Vou olhar %MIN' SKIP
                   tt-itens-ped.qt-reservada SKIP
                   tt-itens-ped.qt-pedida - (tt-itens-ped.qt-pedida * (de-perc-min / 100)) SKIP
                   "Saldo: " ABS(tt-itens-ped.qt-pedida - tt-itens-ped.qt-reservada)
                   "Comp Medio: " corte-comerc.compr-med
                   VIEW-AS ALERT-BOX INFO BUTTONS OK.

       IF tt-itens-ped.qt-reservada >= tt-itens-ped.qt-pedida - (tt-itens-ped.qt-pedida * (de-perc-min / 100)) AND 
          ABS(tt-itens-ped.qt-pedida - tt-itens-ped.qt-reservada) <= corte-comerc.compr-med THEN DO.
          IF l-acomp THEN
             MESSAGE 'Pedido: ' tt-itens-ped.nr-pedcli
                     'Seq: ' tt-itens-ped.nr-sequencia SKIP
                     'Reserva Completa....' SKIP
                     'Qt Pedida: ' tt-itens-ped.qt-pedida
                     'Qt Reservada: ' tt-itens-ped.qt-reservada
                 VIEW-AS ALERT-BOX INFO BUTTONS OK.

          ASSIGN tt-itens-ped.res-completa = YES.
          RETURN.
       END.

       IF l-acomp THEN
          MESSAGE 'Pedido: ' tt-itens-ped.nr-pedcli
                  'Seq: ' tt-itens-ped.nr-sequencia SKIP
                  'Vou Cancelar as Etiquetas Reservadas para a Seqencia' SKIP
                  'Qt Pedida: ' tt-itens-ped.qt-pedida
                  'Qt Reservada: ' tt-itens-ped.qt-reservada 
                   VIEW-AS ALERT-BOX ERROR BUTTONS OK.

       ASSIGN tt-itens-ped.qt-reservada = 0.
       FOR EACH tt-etiquetas WHERE
                tt-etiquetas.nr-pedcli = tt-itens-ped.nr-pedcli AND
                tt-etiquetas.nr-sequencia = tt-itens-ped.nr-sequencia 
                EXCLUSIVE-LOCK.  

           FIND FIRST wt-etiquetas WHERE
                      wt-etiquetas.num-etiqueta = tt-etiquetas.num-etiqueta 
                      USE-INDEX indice3 NO-ERROR.

           IF l-acomp THEN
                MESSAGE 'Pedido: ' tt-itens-ped.nr-pedcli
                        'Seq: ' tt-itens-ped.nr-sequencia SKIP
                        'Vou Retirar Reserva'
                        'Etiqueta: ' tt-etiquetas.num-etiqueta
                        'Qtde: ' wt-etiquetas.quantidade SKIP(2)
                        'Qt Pedida: ' tt-itens-ped.qt-pedida
                        'Qt Reservada: ' tt-itens-ped.qt-reservada
                   VIEW-AS ALERT-BOX ERROR BUTTONS OK.

            FIND wt-docas WHERE
                 wt-docas.localiz = wt-etiquetas.localizacao AND 
                 wt-docas.it-codigo = wt-etiquetas.it-codigo AND
                 wt-docas.cod-refer = wt-etiquetas.cod-refer AND
                 wt-docas.lote = wt-etiquetas.lote AND
                 wt-docas.corte-comerc = wt-etiquetas.corte-comerc AND
                 wt-docas.tecelagem = wt-etiquetas.tipo-tear NO-ERROR.

            ASSIGN wt-etiquetas.reservada = NO
                   wt-docas.metragem-disp = wt-docas.metragem-disp + wt-etiquetas.quantidade.

            IF tt-saldo-item.vrf-tecelagem THEN DO.
               IF wt-etiquetas.tipo-tear = 'SULZER' THEN
                  ASSIGN tt-saldo-item.qt-sulzer = tt-saldo-item.qt-sulzer + wt-etiquetas.quantidade.
               ELSE
                  ASSIGN tt-saldo-item.qt-outras-tecelag = tt-saldo-item.qt-outras-tecelag + wt-etiquetas.quantidade.
            END.
            ASSIGN tt-saldo-item.qtidade-atu = tt-saldo-item.qtidade-atu + wt-etiquetas.quantidade.

            DELETE tt-etiquetas.
       END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-separa-etiquetas B-table-Win 
PROCEDURE pi-separa-etiquetas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /* Separa as Etiquetas e Calcula o saldo para cada item separado por Tecelagem */
    FOR EACH tt-itens-ped NO-LOCK.
        FIND tt-saldo-item WHERE
             tt-saldo-item.it-codigo = tt-itens-ped.it-codigo AND
             tt-saldo-item.cod-refer = tt-itens-ped.cod-refer AND  
             tt-saldo-item.lote = tt-itens-ped.lote AND
             tt-saldo-item.corte-comerc = tt-itens-ped.corte-comerc
             NO-LOCK NO-ERROR.

        IF NOT AVAIL tt-saldo-item THEN DO.
           CREATE tt-saldo-item.
           ASSIGN tt-saldo-item.it-codigo = tt-itens-ped.it-codigo
                  tt-saldo-item.cod-refer = tt-itens-ped.cod-refer
                  tt-saldo-item.lote = tt-itens-ped.lote     
                  tt-saldo-item.corte-comerc = tt-itens-ped.corte-comerc.

         IF l-acomp THEN
              MESSAGE tt-itens-ped.cod-estabel SKIP
                      tt-itens-ped.it-codigo
                      tt-itens-ped.cod-refer
                      tt-itens-ped.lote
                      tt-itens-ped.corte-comerc
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.

           FOR EACH ob-etiqueta WHERE
                    ob-etiqueta.cod-estabel = tt-itens-ped.cod-estabel AND
                    ob-etiqueta.situacao  = 3 AND
                    ob-etiqueta.it-codigo = tt-itens-ped.it-codigo AND
                    ob-etiqueta.cod-refer = tt-itens-ped.cod-refer AND
                    ob-etiqueta.nr-lote   = tt-itens-ped.lote AND
                    ob-etiqueta.corte-comerc = tt-itens-ped.corte-comerc NO-LOCK. 
    
               IF l-acomp THEN
                  MESSAGE ob-etiqueta.num-etiqueta
                      VIEW-AS ALERT-BOX INFO BUTTONS OK.


               RUN pi-acompanhar IN h-acomp (INPUT "Etiqueta: " + STRING(ob-etiqueta.num-etiqueta,"999999999")).
    
               /* FIND ordem-benefic WHERE
                    ordem-benefic.cod-estabel = ob-etiqueta.cod-estabel AND
                    ordem-benefic.dt-ob = ob-etiqueta.dt-ob AND
                    ordem-benefic.nr-ob = ob-etiqueta.nr-ob AND
                    ordem-benefic.nr-carro = ob-etiqueta.nr-carro NO-LOCK NO-ERROR. */
    
               IF ob-etiqueta.localizacao = '' THEN DO.
                  ASSIGN tt-saldo-item.qt-sem-localiz = tt-saldo-item.qt-sem-localiz +
                                                        ob-etiqueta.quantidade.
                  NEXT.
               END.

               IF ob-etiqueta.localizacao < c-localiz-ini OR
                  ob-etiqueta.localizacao > c-localiz-fin THEN NEXT.

               IF l-acomp THEN
                   MESSAGE "Aqui" SKIP 
                            ob-etiqueta.nr-lote SKIP
                            ob-etiqueta.localizacao
                       VIEW-AS ALERT-BOX INFO BUTTONS OK.


               ASSIGN l-reservar = NO.
               IF ob-etiqueta.nr-lote <> 'CA' THEN DO.
                  ASSIGN l-reservar = YES.

                  IF (ob-etiqueta.localizacao BEGINS '6' OR
                      ob-etiqueta.localizacao BEGINS '7') THEN
                      ASSIGN l-reservar = NO.

                  IF (ob-etiqueta.localizacao = '700006' OR
                      ob-etiqueta.localizacao = '700007' OR
                      ob-etiqueta.localizacao = '700010') THEN 
                      ASSIGN l-reservar = YES.
               END.
                 
               IF l-reservar = NO THEN NEXT.

               RUN pi-ver-digita (INPUT "Localizaá∆o",
                                  INPUT ob-etiqueta.localizacao).
               IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

               FIND item-ext WHERE
                    item-ext.it-codigo = tt-itens-ped.it-codigo NO-LOCK NO-ERROR.
    
               /*IF AVAIL item-ext AND
                  ((item-ext.indigo = YES AND SUBSTR(ob-etiqueta.cod-refer,3,4) <> "0520") OR
                  (ob-etiqueta.it-codigo = '501871' AND ob-etiqueta.cod-refer = '010101')) AND
                   ob-etiqueta.nuance = '' THEN NEXT.*/

               IF tt-itens-ped.exportacao = YES AND
                  ob-etiqueta.cod-qualid <> 'A' THEN DO.
                  ASSIGN tt-saldo-item.qt-sem-qualid = tt-saldo-item.qt-sem-qualid + ob-etiqueta.quantidade.
                  NEXT.
               END.

               IF tt-itens-ped.emb-neutra = YES AND
                  ob-etiqueta.emb-neutra  = NO THEN DO.
                  ASSIGN tt-saldo-item.qt-sem-emb-neutra = tt-saldo-item.qt-sem-emb-neutra + ob-etiqueta.quantidade.
                  NEXT.
               END.

               CREATE wt-etiquetas.
               ASSIGN wt-etiquetas.cod-estabel = ob-etiqueta.cod-estabel
                      wt-etiquetas.it-codigo = ob-etiqueta.it-codigo
                      wt-etiquetas.cod-refer = ob-etiqueta.cod-refer
                      wt-etiquetas.lote = ob-etiqueta.nr-lote
                      wt-etiquetas.corte-comerc = ob-etiqueta.corte-comerc
                      wt-etiquetas.num-etiqueta = ob-etiqueta.num-etiqueta
                      wt-etiquetas.quantidade = ob-etiqueta.quantidade
                      wt-etiquetas.localiz = ob-etiqueta.localiz
                      wt-etiquetas.tipo-tear = /* IF AVAIL ordem-benefic 
                                               THEN ordem-benefic.tipo-tear
                                               ELSE "" */ ""
                      wt-etiquetas.corte-comerc = ob-etiqueta.corte-comerc.

               FIND wt-docas WHERE
                    wt-docas.localiz = wt-etiquetas.localizacao AND 
                    wt-docas.it-codigo = wt-etiquetas.it-codigo AND
                    wt-docas.cod-refer = wt-etiquetas.cod-refer AND
                    wt-docas.lote = wt-etiquetas.lote AND
                    wt-docas.corte-comerc = wt-etiquetas.corte-comerc AND
                    wt-docas.tecelagem = wt-etiquetas.tipo-tear NO-ERROR.

               IF NOT AVAIL wt-docas THEN DO.
                  CREATE wt-docas.
                  ASSIGN wt-docas.localiz = wt-etiquetas.localizacao
                         wt-docas.it-codigo = wt-etiquetas.it-codigo 
                         wt-docas.cod-refer = wt-etiquetas.cod-refer 
                         wt-docas.lote = wt-etiquetas.lote 
                         wt-docas.corte-comerc = wt-etiquetas.corte-comerc 
                         wt-docas.tecelagem = wt-etiquetas.tipo-tear.
               END.
               ASSIGN wt-docas.metragem-tot = wt-docas.metragem-tot + wt-etiquetas.quantidade
                      wt-docas.metragem-disp = wt-docas.metragem-disp + wt-etiquetas.quantidade.

               ASSIGN tt-saldo-item.vrf-tecelagem = NO.
               /*IF item-ext.indigo = YES AND                           /* Se for ÷ndigo,        */
                  tt-itens-ped.lote = 'RP' AND                        /* com Rolos Perfeitos e */
                  LOOKUP(tt-itens-ped.corte-comerc,"A,B,C") = 0 THEN  /* Cortes <> de A,B,C....*/
                  ASSIGN tt-saldo-item.vrf-tecelagem = YES.           /* Verifica Tecelagem   */ */

               /* Se n∆o for verificar a Tecelagem, define tecelagem do Pedido como "Todas" */
               IF tt-saldo-item.vrf-tecelagem = NO THEN
                  ASSIGN tt-itens-ped.tecelagem = 'Todas'.

               IF tt-saldo-item.vrf-tecelagem = YES THEN DO.
                  IF wt-etiquetas.tipo-tear = 'SULZER' THEN
                     ASSIGN tt-saldo-item.qt-sulzer = tt-saldo-item.qt-sulzer + wt-etiquetas.quantidade.
                  ELSE
                     ASSIGN tt-saldo-item.qt-outras-tecelag = tt-saldo-item.qt-outras-tecelag + wt-etiquetas.quantidade.
               END.

               ASSIGN tt-saldo-item.qtidade-atu = tt-saldo-item.qtidade-atu + wt-etiquetas.quantidade.
           END.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-separa-itens B-table-Win 
PROCEDURE pi-separa-itens :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH ped-item WHERE        
             ped-item.it-codigo >= c-it-codigo-ini AND
             ped-item.it-codigo <= c-it-codigo-fin AND
             ped-item.cod-refer >= c-cod-refer-ini AND 
             ped-item.cod-refer <= c-cod-refer-fin AND
             ped-item.nr-sequencia >= i-nr-seq-ini AND
             ped-item.nr-sequencia <= i-nr-seq-fin  AND
             (ped-item.cod-sit-item = 1 OR
              ped-item.cod-sit-item = 2 OR
              ped-item.cod-sit-item = 5) NO-LOCK, 
       FIRST ped-venda OF ped-item WHERE
             ped-venda.cod-estabel = c-cod-estabel AND
            (ped-venda.cod-sit-ped = 1 OR
             ped-venda.cod-sit-ped = 2 OR
             ped-venda.cod-sit-ped = 5) AND
             ped-venda.nr-pedcli >= c-nr-pedcli-ini AND
             ped-venda.nr-pedcli <= c-nr-pedcli-fin AND
             ped-venda.dt-entrega <= da-dt-entrega NO-LOCK, 
       FIRST ped-item-ext OF ped-item WHERE
             INDEX(c-lotes,SUBSTR(ped-item-ext.lote,1,2)) <> 0  AND 
             ped-item-ext.corte-comerc >= c-corte-comerc-ini AND
             ped-item-ext.corte-comerc <= c-corte-comerc-fin NO-LOCK
             BY ped-venda.nr-pedcli
             BY ped-item.nr-sequencia.

        FIND ped-venda-ext WHERE
             ped-venda-ext.nr-pedido = INT(ped-venda.nr-pedcli) NO-LOCK NO-ERROR.
              
        IF NOT AVAIL ped-venda-ext THEN NEXT.

        RUN pi-ver-digita (INPUT "Pedido_de_Venda",
                           INPUT ped-venda.nr-pedcli).
        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

        RUN pi-ver-digita (INPUT "Item",
                           INPUT ped-item.it-codigo).
        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

        RUN pi-ver-digita (INPUT "Referància",
                           INPUT ped-item.cod-refer).
        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

        RUN pi-ver-digita (INPUT "Sequància",
                           INPUT ped-item.nr-sequencia).
        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

        RUN pi-ver-digita (INPUT "Corte_Comercial",
                           INPUT ped-item-ext.corte-comerc).
        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

        FIND ped-item-res WHERE
             ped-item-res.nome-abrev   = ped-item.nome-abrev   AND
             ped-item-res.nr-pedcli    = ped-item.nr-pedcli    AND
             ped-item-res.nr-sequencia = ped-item.nr-sequencia NO-LOCK NO-ERROR.
        IF AVAIL ped-item-res THEN NEXT.

        /*FIND item-ext WHERE
             item-ext.it-codigo = ped-item.it-codigo NO-LOCK NO-ERROR.
        IF c-tp-artigo <> 'A' THEN 
           IF AVAIL item-ext AND 
              (item-ext.indigo = YES AND c-tp-artigo <> "I") OR
              (item-ext.indigo = NO  AND c-tp-artigo <> "O") THEN NEXT.*/
        /*
        FIND ref-item-ext WHERE
             ref-item-ext.it-codigo = ped-item.it-codigo AND
             ref-item-ext.cod-refer = ped-item.cod-refer NO-LOCK NO-ERROR.

        IF NOT AVAIL ref-item-ext THEN NEXT.

        IF AVAIL ref-item-ext AND 
           (ref-item-ext.cod-obsoleto < c-cod-obsoleto-ini OR
            ref-item-ext.cod-obsoleto > c-cod-obsoleto-fin) THEN NEXT.     
        
       
        RUN pi-ver-digita (INPUT "Codigo_Obsoleto",
                           INPUT ref-item-ext.cod-obsoleto).
        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.
        */
        
        RUN pi-acompanhar IN h-acomp (INPUT "Pedido: " + ped-item.nr-pedcli +
                                            "  Item: " + ped-item.it-codigo +
                                            "   Ref: " + ped-item.cod-refer).


        FIND emitente WHERE
             emitente.cod-emitente = ped-venda.cod-emitente NO-LOCK NO-ERROR.

        FIND tt-itens-ped WHERE
             tt-itens-ped.nr-pedcli = ped-item.nr-pedcli AND
             tt-itens-ped.nr-sequencia = ped-item.nr-sequencia 
             NO-LOCK NO-ERROR.

        IF NOT AVAIL tt-itens-ped THEN DO.
           CREATE tt-itens-ped.
           BUFFER-COPY ped-item TO tt-itens-ped.
           
           ASSIGN tt-itens-ped.cod-estabel = ped-venda.cod-estabel
                  tt-itens-ped.lote = UPPER(SUBSTR(ped-item-ext.lote,1,2))
                  tt-itens-ped.corte-comerc = ped-item-ext.corte-comerc
                  tt-itens-ped.nome-transp = UPPER(ped-venda.nome-transp)
                  tt-itens-ped.tecelagem = ped-venda-ext.tecelagem
                  tt-itens-ped.emb-neutra = ped-venda-ext.l-emb-neutra
                  tt-itens-ped.exportacao = IF ped-venda-ext.tp-pedido = 'Exportaá∆o'
                                            THEN YES ELSE NO.
        END.

        IF NOT l-res-crivo THEN DO.
           FIND cond-pagto OF ped-venda NO-LOCK NO-ERROR.
           IF AVAIL cond-pagto THEN DO.
              IF (cond-pagto.cod-vencto < 2 OR cond-pagto.cod-vencto > 3) AND  
                 (ped-venda.cod-sit-aval < 2 OR ped-venda.cod-sit-aval > 3) THEN 
                 ASSIGN tt-itens-ped.qt-crivada = tt-itens-ped.qt-crivada + ped-item.qt-pedida.
           END.
           ELSE
              IF ped-venda.cod-sit-aval < 2 OR ped-venda.cod-sit-aval > 3 THEN
                 ASSIGN tt-itens-ped.qt-crivada = tt-itens-ped.qt-crivada + ped-item.qt-pedida.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-separa-pedidos B-table-Win 
PROCEDURE pi-separa-pedidos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FOR EACH ped-venda WHERE
            ped-venda.cod-sit-ped = 1 NO-LOCK,
       EACH ped-item OF ped-venda WHERE        
            ped-item.it-codigo >= c-it-codigo-ini AND
            ped-item.it-codigo <= c-it-codigo-fin AND
            ped-item.cod-refer >= c-cod-refer-ini AND 
            ped-item.cod-refer <= c-cod-refer-fin AND
            ped-item.nr-sequencia >= i-nr-seq-ini AND
            ped-item.nr-sequencia <= i-nr-seq-fin NO-LOCK,
       FIRST ped-item-ext OF ped-item WHERE
             INDEX(c-lotes,SUBSTR(ped-item-ext.lote,1,2)) <> 0 AND
             ped-item-ext.corte-comerc >= c-corte-comerc-ini AND
             ped-item-ext.corte-comerc <= c-corte-comerc-fin NO-LOCK
             BY ped-venda.nr-pedcli
             BY ped-item.nr-sequencia.

        RUN pi-acompanhar IN h-acomp (INPUT "Pedido: " + ped-item.nr-pedcli +
                                            "  Item: " + ped-item.it-codigo +
                                            "   Ref: " + ped-item.cod-refer).

       IF ped-venda.cod-estabel <> c-cod-estabel THEN NEXT.
       IF ped-venda.nr-pedcli < c-nr-pedcli-ini OR
          ped-venda.nr-pedcli > c-nr-pedcli-fin OR
          ped-venda.dt-entrega > da-dt-entrega THEN NEXT.


        FIND ped-venda-ext WHERE
             ped-venda-ext.nr-pedido = INT(ped-venda.nr-pedcli) NO-LOCK NO-ERROR.
              
        IF NOT AVAIL ped-venda-ext THEN NEXT.

        IF ped-venda-ext.l-nao-aprovar THEN NEXT.

        RUN pi-ver-digita (INPUT "Pedido_de_Venda",
                           INPUT ped-venda.nr-pedcli).
        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

        RUN pi-ver-digita (INPUT "Item",
                           INPUT ped-item.it-codigo).
        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

        RUN pi-ver-digita (INPUT "Referància",
                           INPUT ped-item.cod-refer).
        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

        RUN pi-ver-digita (INPUT "Sequància",
                           INPUT ped-item.nr-sequencia).
        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

        RUN pi-ver-digita (INPUT "Corte_Comercial",
                           INPUT ped-item-ext.corte-comerc).
        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.


        FIND ped-item-res WHERE
             ped-item-res.nome-abrev   = ped-item.nome-abrev   AND
             ped-item-res.nr-pedcli    = ped-item.nr-pedcli    AND
             ped-item-res.nr-sequencia = ped-item.nr-sequencia NO-LOCK NO-ERROR.
        IF AVAIL ped-item-res THEN NEXT.

        /*FIND item-ext WHERE
             item-ext.it-codigo = ped-item.it-codigo NO-LOCK NO-ERROR.
        IF c-tp-artigo <> 'A' THEN 
           IF AVAIL item-ext AND
              (item-ext.indigo = YES AND c-tp-artigo <> "I") OR
              (item-ext.indigo = NO  AND c-tp-artigo <> "O") THEN NEXT.*/
        /*
        FIND ref-item-ext WHERE
             ref-item-ext.it-codigo = ped-item.it-codigo AND
             ref-item-ext.cod-refer = ped-item.cod-refer NO-LOCK NO-ERROR.

        IF NOT AVAIL ref-item-ext THEN NEXT.
        IF AVAIL ref-item-ext AND 
           (ref-item-ext.cod-obsoleto < c-cod-obsoleto-ini OR
            ref-item-ext.cod-obsoleto > c-cod-obsoleto-fin) THEN NEXT.
                          
        RUN pi-ver-digita (INPUT "Codigo_Obsoleto",
                           INPUT ref-item-ext.cod-obsoleto).
        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.
        */

        FIND emitente WHERE
             emitente.cod-emitente = ped-venda.cod-emitente NO-LOCK NO-ERROR.

        FIND tt-itens-ped WHERE
             tt-itens-ped.nr-pedcli = ped-item.nr-pedcli AND
             tt-itens-ped.nr-sequencia = ped-item.nr-sequencia 
             NO-LOCK NO-ERROR.

        IF NOT AVAIL tt-itens-ped THEN DO.
           CREATE tt-itens-ped.
           BUFFER-COPY ped-item TO tt-itens-ped.
           
           ASSIGN tt-itens-ped.cod-estabel = ped-venda.cod-estabel
                  tt-itens-ped.lote = UPPER(SUBSTR(ped-item-ext.lote,1,2))
                  tt-itens-ped.corte-comerc = ped-item-ext.corte-comerc
                  tt-itens-ped.nome-transp = UPPER(ped-venda.nome-transp)
                  tt-itens-ped.tecelagem = ped-venda-ext.tecelagem
                  tt-itens-ped.emb-neutra = ped-venda-ext.l-emb-neutra
                  tt-itens-ped.exportacao = IF ped-venda-ext.tp-pedido = 'Exportaá∆o'
                                            THEN YES ELSE NO.
        END.

        IF NOT l-res-crivo THEN DO.
           FIND cond-pagto OF ped-venda NO-LOCK NO-ERROR.
           IF AVAIL cond-pagto THEN DO.
              IF (cond-pagto.cod-vencto < 2 OR cond-pagto.cod-vencto > 3) AND  
                 (ped-venda.cod-sit-aval < 2 OR ped-venda.cod-sit-aval > 3) THEN 
                 ASSIGN tt-itens-ped.qt-crivada = tt-itens-ped.qt-crivada + ped-item.qt-pedida.
           END.
           ELSE
              IF ped-venda.cod-sit-aval < 2 OR ped-venda.cod-sit-aval > 3 THEN
                 ASSIGN tt-itens-ped.qt-crivada = tt-itens-ped.qt-crivada + ped-item.qt-pedida.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-totais B-table-Win 
PROCEDURE pi-totais :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN fi-tot-lida    = 0
           fi-tot-res     = 0
           fi-tot-regra   = 0
           fi-tot-fila    = 0
           fi-tot-crivada = 0.

    FOR EACH tt-itens NO-LOCK.
        ASSIGN fi-tot-lida    = fi-tot-lida    + tt-itens.qt-lida
               fi-tot-res     = fi-tot-res     + tt-itens.qt-reservada
               fi-tot-regra   = fi-tot-regra   + tt-itens.qt-regra
               fi-tot-crivada = fi-tot-crivada + tt-itens.qt-crivada
               fi-tot-fila    = fi-tot-fila    + tt-itens.qt-fila.
    END.
    DISPLAY fi-tot-lida     
            fi-tot-res
            fi-tot-regra    
            fi-tot-fila     
            fi-tot-crivada
            WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-ver-digita B-table-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key B-table-Win  adm/support/_key-snd.p
PROCEDURE send-key :
/*------------------------------------------------------------------------------
  Purpose:     Sends a requested KEY value back to the calling
               SmartObject.
  Parameters:  <see adm/template/sndkytop.i>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/sndkytop.i}

  /* Return the key value associated with each key case.             */
  {src/adm/template/sndkycas.i "nr-ord-produ" "ped-item" "nr-ord-produ"}
  {src/adm/template/sndkycas.i "nr-programa" "ped-item" "nr-programa"}
  {src/adm/template/sndkycas.i "it-codigo" "ped-item" "it-codigo"}

  /* Close the CASE statement and end the procedure.                 */
  {src/adm/template/sndkyend.i}

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
  {src/adm/template/snd-list.i "tt-refer"}
  {src/adm/template/snd-list.i "tt-itens"}

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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-desc-item B-table-Win 
FUNCTION fn-desc-item RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND ITEM WHERE
       ITEM.it-codigo = tt-itens.it-codigo NO-LOCK NO-ERROR.

  RETURN item.desc-item.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

