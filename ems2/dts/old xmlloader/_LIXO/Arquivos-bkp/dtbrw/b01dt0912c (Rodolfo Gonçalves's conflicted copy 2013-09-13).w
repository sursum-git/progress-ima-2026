&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r11 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          xmlloader        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*:T *******************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
**
**  ACRA 016 - 25/01/2013 
**           - INCLUS«O DO CAMPO LOCALIZAÄ«O
**           - AJUSTE NA L‡GICA DE CARGA DOS DADOS PARA VALIDAR FIFO DE ORDEM
**             DE COMPRA E DE ORDEM DE PRODUÄ«O;
**           - AJUSTE NA L‡GICA DE CARGA DOS DADOS PARA BUSCAR, ALêM DO DEP‡SITO,
**             A LOCALIZAÄ«O
**  ACRA 017 - 15/02/2013 
**           - AJUSTE NA VALIDAÄ«O DE DEP‡SITO PARA ITENS COM ORDEM DE COMPRA            
**  ACRA 018 - 25/03/2013 
**           - RETIRADO O RETORNO DE ZOOM DA ORDEM DE COMPRA PARA O CAMPO DEP‡SITO;
**           - ALTERADO LABEL DO BOT«O bt-rateio PARA RATEIO LOTES/LOCALIZAÄ«O;
**           - HABILITADO O BOT«O DE RATEIO DE LOTES PARA TRATAR ITENS QUE N«O POSSUEM
**             CONTROLE POR LOTE PARA EFETUAR RATEIO DE DEP‡SITOS E LOCALIZAÄÂES.
**           - COLOCADA A GRAVAÄ«O DO CAMPO DA dt-item-docum-est.quantidade NA PROCEDURE
**             pi-carrega-dados. ANTES ESTAVA FAZENDO SOMENTE NO LEAVE DO CAMPO item-ems
**             DO BROWSE, ACARRETANDO ERROS NO MOMENTO DE RATEIO DO LOTE
**  ACRA 019 - 25/05/2013 
**           - CRIADO BOT«O DE "GERA ESTRUTURA"
**           - INSERIDA L‡GICA DE GRAVAÄ«O DA tt-movto-pend CASO O USUµRIO SELECIONE O 
**             REGISTRO COMO AGREGADO E SAIA DO PROGRAMA SEM PROCESSAR
**  ACRA 020 - 06/06/2013 
**           - AJUSTE GRAVAÄ«O DO DOCUMENTO DE ORIGEM NA PROCEDURE pi-valida 
**             QUANDO O MESMO ê RETORNO DE BENEFICIAMENTO - 
**             TRATAMENTO QUANDO A NOTA FISCAL DE TERCEIROS
**             GERA NOTA NO FATURAMENTO (natur-oper.imp-nota).
**  ACRA 021 - 03/08/2013 
**           - AJUSTE PARA TRATAMENTO RECEBIMENTO F÷SICO - pi-valida-fisico
**  ACRA 022 - 09/08/2013 
**           - AJUSTE LIMPEZA DA TABELA TEMPORµRIA tt-dt-item-doc-est - ESTAVA DUPLICANDO 
**             O REGISTRO QUANDO RETORNAVA ERRO NA EXECUÄ«O E FAZIA UMA NOVA EFETIVAÄ«O
**             PROCEDURE - pi-valida

*******************************************************************************/
/* {include/i-buffer.i} */
{include/i-prgvrs.i B01DT0912C 2.06.00.022}
{dtp/dts0101.i}
{inbo/boin366.i tt-rat-docum }     /* Definiá∆o TT2-RAT-DOCUM */
{inbo/boin366.i tt2-rat-docum }     /* Definiá∆o TT2-RAT-DOCUM */
{inbo/boin367.i tt-rat-lote }
{inbo/boin367.i tt2-rat-lote }
{inbo/boin366.i1 tt-imposto}        /* Definiá∆o TT-IMPOSTO */
{inbo/boin176.i4 tt-item-devol-cli}
{utp/ut-glob.i}
/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEF TEMP-TABLE tt-item-terc NO-UNDO
    FIELD item-xml  LIKE ITEM.it-codigo
    FIELD item-ems  LIKE ITEM.it-codigo
    FIELD sequencia LIKE item-doc-est.sequencia
    FIELD cod-estab LIKE docum-est.cod-estabel
    FIELD serie     LIKE docum-est.serie
    FIELD nro-docto LIKE docum-est.nro-docto
    FIELD nat-oper  LIKE docum-est.nat-operacao.

DEFINE TEMP-TABLE tt-nf-devol-cli NO-UNDO
    FIELD serie-nf      AS CHAR
    FIELD nr-nf         AS CHAR
    FIELD reabre-pedido AS LOGICAL
    FIELD cons-ender    AS LOGICAL 
    FIELD rRowid        AS ROWID.

DEFINE TEMP-TABLE tt-movto-pend NO-UNDO LIKE movto-pend
    field r-rowid as rowid.

DEFINE VARIABLE wh-pesquisa                      AS HANDLE                  NO-UNDO.
DEFINE VARIABLE cDescricao                       AS CHAR FORMAT "x(60)"     NO-UNDO.
DEFINE VARIABLE pPedidoCompr                     AS INTEGER                 NO-UNDO.
DEFINE VARIABLE vNatureza                        LIKE natur-oper.nat-operacao.
DEFINE VARIABLE cFornec                          AS CHAR                    NO-UNDO.
DEFINE VARIABLE iFornec                          LIKE emitente.cod-emitente NO-UNDO.
DEFINE VARIABLE pEstabelec                       LIKE estabelec.cod-estabel NO-UNDO.
DEFINE VARIABLE lHabilitaLote                    AS LOGICAL INITIAL NO      NO-UNDO.
DEFINE VARIABLE de-indice                        AS DECIMAL                 NO-UNDO.
DEFINE VARIABLE pRetorno                         AS LOGICAL INITIAL NO      NO-UNDO .
DEFINE VARIABLE h-prog-aux                       as handle                  no-undo.
DEFINE VARIABLE hProgramZoom                     AS HANDLE                  NO-UNDO.
define variable pTipoCompra                      as char                    no-undo.
define variable i-tipo-nota                      as integer                 no-undo.
DEFINE VARIABLE cTipoRecebimento                 AS CHAR                    NO-UNDO.




DEFINE NEW GLOBAL SHARED VARIABLE l-nat-terc     AS LOGICAL NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE l-nota-imp     AS LOGICAL NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE l-nat-cte      AS LOGICAL NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE l-nat-devol    AS LOGICAL NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE l-nat-nft      AS LOGICAL NO-UNDO. /* transferància */
DEFINE NEW GLOBAL SHARED VARIABLE vCabec         AS HANDLE  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE c-seg-usuario  AS CHARACTER  FORMAT "X(12)"  NO-UNDO.
DEFINE TEMP-TABLE tt2-dt-it-docum-est NO-UNDO LIKE dt-it-docum-est 
    FIELD pRowid AS ROWID.
DEFINE TEMP-TABLE tt2-dt-docum-est    NO-UNDO LIKE dt-docum-est .
DEFINE TEMP-TABLE tt2-devol-cli       NO-UNDO LIKE tt-nf-devol-cli.
DEFINE BUFFER bDtItDocumEst FOR dt-it-docum-est.
DEFINE BUFFER bDtDocEst     FOR dt-docum-est.
DEFINE BUFFER bItem         FOR ITEM.
DEFINE BUFFER bDtEmpresa    FOR dt-empresa.
DEFINE BUFFER bNaturOper    FOR natur-oper.
DEFINE buffer b-doc         for doc-fisico.
DEFINE buffer b-emit-estab  for emitente.
define buffer bf-estabelec  for estabelec.

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

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES dt-docum-est
&Scoped-define FIRST-EXTERNAL-TABLE dt-docum-est


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR dt-docum-est.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES dt-it-docum-est

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br-itens                                      */
&Scoped-define FIELDS-IN-QUERY-br-itens dt-it-docum-est.log-1 dt-it-docum-est.nat-operacao dt-it-docum-est.sequencia dt-it-docum-est.it-codigo dt-it-docum-est.char-2 dt-it-docum-est.item-ems dt-it-docum-est.cod-depos dt-it-docum-est.cod-localiz fcDescr(dt-it-docum-est.item-ems) @ cDescricao dt-it-docum-est.un dt-it-docum-est.qt-do-forn dt-it-docum-est.dec-1 dt-it-docum-est.lote dt-it-docum-est.dt-vali-lote dt-it-docum-est.numero-ordem fnPedidoCompr(dt-it-docum-est.numero-ordem) @ pPedidoCompr dt-it-docum-est.serie-comp dt-it-docum-est.nro-comp dt-it-docum-est.nat-comp dt-it-docum-est.seq-comp dt-it-docum-est.nr-ord-produ dt-it-docum-est.narrativa   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-itens dt-it-docum-est.nat-operacao ~
 ~
  dt-it-docum-est.item-ems ~
 ~
 dt-it-docum-est.cod-depos ~
 ~
 dt-it-docum-est.cod-localiz ~
 ~
 dt-it-docum-est.lote ~
 ~
 dt-it-docum-est.narrativa ~
 ~
 dt-it-docum-est.numero-ordem ~
 ~
 dt-it-docum-est.dt-vali-lote ~
 ~
 dt-it-docum-est.nro-comp ~
 ~
 dt-it-docum-est.nr-ord-produ   
&Scoped-define ENABLED-TABLES-IN-QUERY-br-itens dt-it-docum-est
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br-itens dt-it-docum-est
&Scoped-define SELF-NAME br-itens
&Scoped-define QUERY-STRING-br-itens FOR EACH dt-it-docum-est                             WHERE dt-it-docum-est.nro-docto    = dt-docum-est.nro-docto                               AND dt-it-docum-est.serie-docto  = dt-docum-est.serie-docto                               AND dt-it-docum-est.cod-emitente = dt-docum-est.cod-emitente                               /* AND dt-it-docum-est.nat-operacao = dt-docum-est.nat-operacao */                               AND dt-it-docum-est.nome-arq     = dt-docum-est.nome-arq  SHARE-LOCK                               BY dt-it-docum-est.sequencia
&Scoped-define OPEN-QUERY-br-itens OPEN QUERY {&SELF-NAME} FOR EACH dt-it-docum-est                             WHERE dt-it-docum-est.nro-docto    = dt-docum-est.nro-docto                               AND dt-it-docum-est.serie-docto  = dt-docum-est.serie-docto                               AND dt-it-docum-est.cod-emitente = dt-docum-est.cod-emitente                               /* AND dt-it-docum-est.nat-operacao = dt-docum-est.nat-operacao */                               AND dt-it-docum-est.nome-arq     = dt-docum-est.nome-arq  SHARE-LOCK                               BY dt-it-docum-est.sequencia.
&Scoped-define TABLES-IN-QUERY-br-itens dt-it-docum-est
&Scoped-define FIRST-TABLE-IN-QUERY-br-itens dt-it-docum-est


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-itens bt-lote bt-gera-estrut bt-rateio-oc ~
bt-rateio-op 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" B-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS
><EXECUTING-CODE>
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

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fcDescr B-table-Win 
FUNCTION fcDescr RETURNS CHARACTER
  ( INPUT pItemEMS AS CHAR /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnPedidoCompr B-table-Win 
FUNCTION fnPedidoCompr RETURNS INTEGER
  ( INPUT pNumOrdem AS INTEGER /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-agregado 
     LABEL "Agregado" 
     SIZE 15 BY 1.

DEFINE BUTTON bt-devol 
     LABEL "Associar Devoluá∆o" 
     SIZE 14.86 BY 1.

DEFINE BUTTON bt-gera-estrut 
     LABEL "Gerar Estrutura" 
     SIZE 15 BY 1 TOOLTIP "Gerar Retorno Materia-prima por Estrutura".

DEFINE BUTTON bt-gerar 
     LABEL "Gerar Devoluá∆o" 
     SIZE 13 BY 1.

DEFINE BUTTON bt-it-agregado  NO-CONVERT-3D-COLORS
     LABEL "Item Agregado" 
     SIZE 12.29 BY 1 TOOLTIP "Marcar o Item Agregado".

DEFINE BUTTON bt-lote 
     LABEL "Rateio Lotes/Localizaá∆o" 
     SIZE 18 BY 1.

DEFINE BUTTON bt-rateio 
     LABEL "Rateio CTe" 
     SIZE 9.72 BY 1 TOOLTIP "Rateio de Conhecimento de Transporte".

DEFINE BUTTON bt-rateio-nat-op  NO-CONVERT-3D-COLORS
     LABEL "Rateio Operaá∆o" 
     SIZE 12.29 BY 1 TOOLTIP "Rateio de Ordem de Compra".

DEFINE BUTTON bt-rateio-oc 
     LABEL "Rateio Compra" 
     SIZE 11.29 BY 1 TOOLTIP "Rateio de Ordem de Compra".

DEFINE BUTTON bt-rateio-op 
     LABEL "Rateio Produá∆o" 
     SIZE 12.14 BY 1 TOOLTIP "Rateio de Ordem de Produá∆o".

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-itens FOR 
      dt-it-docum-est SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-itens
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-itens B-table-Win _FREEFORM
  QUERY br-itens SHARE-LOCK NO-WAIT DISPLAY
      dt-it-docum-est.log-1         COLUMN-LABEL "Agregado" FORMAT "*/"
      dt-it-docum-est.nat-operacao  COLUMN-LABEL "Natureza"
      dt-it-docum-est.sequencia     COLUMN-LABEL "Sequencia"
      dt-it-docum-est.it-codigo     COLUMN-LABEL "Item XML"         FORMAT "x(22)":U
      dt-it-docum-est.char-2        COLUMN-LABEL "Descriá∆o"        FORMAT "x(40)":U
      dt-it-docum-est.item-ems      COLUMN-LABEL "Item EMS"
      dt-it-docum-est.cod-depos     COLUMN-LABEL "Dep¢sito"
      dt-it-docum-est.cod-localiz   COLUMN-LABEL "Localizaá∆o"
      fcDescr(dt-it-docum-est.item-ems) @ cDescricao COLUMN-LABEL "Descriá∆o EMS" FORMAT "x(40)"
      dt-it-docum-est.un            COLUMN-LABEL "UN"
      dt-it-docum-est.qt-do-forn    COLUMN-LABEL "Quantidade"       FORMAT ">>>>>>,>>9.9999" 
      dt-it-docum-est.dec-1         COLUMN-LABEL "Quant!Convertida" FORMAT ">>>>>>,>>9.9999" 
      dt-it-docum-est.lote          COLUMN-LABEL "Lote/!Referencia" FORMAT "X(10)"
      dt-it-docum-est.dt-vali-lote  COLUMN-LABEL "Dt Validade!Lote" FORMAT "99/99/9999"
      dt-it-docum-est.numero-ordem  COLUMN-LABEL "Ordem Compra"
      fnPedidoCompr(dt-it-docum-est.numero-ordem) @ pPedidoCompr COLUMN-LABEL "Ped.Compra"
      dt-it-docum-est.serie-comp    COLUMN-LABEL "Ser Terc"
      dt-it-docum-est.nro-comp      COLUMN-LABEL "Doc Terc"
      dt-it-docum-est.nat-comp      COLUMN-LABEL "Nat Terc"
      dt-it-docum-est.seq-comp      COLUMN-LABEL "Seq Terc"
      dt-it-docum-est.nr-ord-produ  COLUMN-LABEL "Ord Prod"
      dt-it-docum-est.narrativa                                     FORMAT "X(60)"
      
          ENABLE dt-it-docum-est.nat-operacao 
                 dt-it-docum-est.item-ems
                 dt-it-docum-est.cod-depos
                 dt-it-docum-est.cod-localiz
                 dt-it-docum-est.lote
                 dt-it-docum-est.narrativa
                 dt-it-docum-est.numero-ordem
                 dt-it-docum-est.dt-vali-lote
                 dt-it-docum-est.nro-comp
                 dt-it-docum-est.nr-ord-produ
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 114 BY 14.5
         FONT 1 ROW-HEIGHT-CHARS .54 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br-itens AT ROW 1 COL 1
     bt-rateio-nat-op AT ROW 12.25 COL 69 WIDGET-ID 18
     bt-agregado AT ROW 13 COL 39 WIDGET-ID 4
     bt-lote AT ROW 15.5 COL 1.29 WIDGET-ID 2
     bt-gera-estrut AT ROW 15.5 COL 25.72 HELP
          "Gerar Retorno pela Estrutura - somente para itens agregados" WIDGET-ID 22
     bt-devol AT ROW 15.5 COL 40.86 WIDGET-ID 8
     bt-gerar AT ROW 15.5 COL 55.86 WIDGET-ID 10
     bt-it-agregado AT ROW 15.5 COL 69.14 WIDGET-ID 20
     bt-rateio-oc AT ROW 15.5 COL 81.57 WIDGET-ID 16
     bt-rateio-op AT ROW 15.5 COL 93 WIDGET-ID 14
     bt-rateio AT ROW 15.5 COL 105 WIDGET-ID 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 FONT 1 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   External Tables: xmlloader.dt-docum-est
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
         HEIGHT             = 15.54
         WIDTH              = 114.
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
/* BROWSE-TAB br-itens 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       br-itens:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE
       br-itens:COLUMN-RESIZABLE IN FRAME F-Main       = TRUE
       br-itens:COLUMN-MOVABLE IN FRAME F-Main         = TRUE
       br-itens:ROW-RESIZABLE IN FRAME F-Main          = TRUE.

/* SETTINGS FOR BUTTON bt-agregado IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       bt-agregado:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR BUTTON bt-devol IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-gerar IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-it-agregado IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-rateio IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-rateio-nat-op IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       bt-rateio-nat-op:HIDDEN IN FRAME F-Main           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-itens
/* Query rebuild information for BROWSE br-itens
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH dt-it-docum-est
                            WHERE dt-it-docum-est.nro-docto    = dt-docum-est.nro-docto
                              AND dt-it-docum-est.serie-docto  = dt-docum-est.serie-docto
                              AND dt-it-docum-est.cod-emitente = dt-docum-est.cod-emitente
                              /* AND dt-it-docum-est.nat-operacao = dt-docum-est.nat-operacao */
                              AND dt-it-docum-est.nome-arq     = dt-docum-est.nome-arq  SHARE-LOCK
                              BY dt-it-docum-est.sequencia
     _END_FREEFORM
     _Options          = "SHARE-LOCK KEY-PHRASE SORTBY-PHRASE"
     _JoinCode[1]      = "xmlloader.dt-it-docum-est.cod-estabel = xmlloader.dt-docum-est.cod-estabel
  AND xmlloader.dt-it-docum-est.serie-docto = xmlloader.dt-docum-est.serie-docto
  AND xmlloader.dt-it-docum-est.nro-docto = xmlloader.dt-docum-est.nro-docto
  AND xmlloader.dt-it-docum-est.nome-arq = xmlloader.dt-docum-est.nome-arq"
     _Query            is NOT OPENED
*/  /* BROWSE br-itens */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br-itens
&Scoped-define SELF-NAME br-itens
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-itens B-table-Win
ON ENTRY OF br-itens IN FRAME F-Main
DO:
/*   APPLY "leave" TO dt-it-docum-est.item-ems     IN BROWSE br-itens. */
/*   apply "leave" to dt-it-docum-est.lote         in browse br-itens. */
/*   apply "leave" to dt-it-docum-est.nr-ord-produ in browse br-itens. */
/*   apply "leave" to dt-it-docum-est.nro-comp     in browse br-itens. */
/*   apply "leave" to dt-it-docum-est.numero-ordem in browse br-itens. */



END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-itens B-table-Win
ON MOUSE-SELECT-CLICK OF br-itens IN FRAME F-Main
DO:
    IF dt-it-docum-est.log-1 THEN
        ASSIGN bt-it-agregado:TOOLTIP IN FRAME {&FRAME-NAME} = "Desmarcar Item Agregado".
    ELSE
        ASSIGN bt-it-agregado:TOOLTIP IN FRAME {&FRAME-NAME} = "Marcar Item Agregado".
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-itens B-table-Win
ON ROW-DISPLAY OF br-itens IN FRAME F-Main
DO:
  IF dt-it-docum-est.log-1   THEN
      ASSIGN dt-it-docum-est.log-1       :bgcolor in browse br-itens = 10 
             dt-it-docum-est.nat-operacao:bgcolor in browse br-itens = 10 
             dt-it-docum-est.sequencia   :bgcolor in browse br-itens = 10 
             dt-it-docum-est.it-codigo   :bgcolor in browse br-itens = 10 
             dt-it-docum-est.char-2      :bgcolor in browse br-itens = 10 
             dt-it-docum-est.item-ems    :bgcolor in browse br-itens = 10 
             dt-it-docum-est.cod-depos   :bgcolor in browse br-itens = 10 
             dt-it-docum-est.cod-localiz :bgcolor in browse br-itens = 10 
             cDescricao                  :bgcolor in browse br-itens = 10
             pPedidoCompr                :bgcolor in browse br-itens = 10
             dt-it-docum-est.un          :bgcolor in browse br-itens = 10 
             dt-it-docum-est.qt-do-forn  :bgcolor in browse br-itens = 10 
             dt-it-docum-est.dec-1       :bgcolor in browse br-itens = 10 
             dt-it-docum-est.lote        :bgcolor in browse br-itens = 10 
             dt-it-docum-est.dt-vali-lote:bgcolor in browse br-itens = 10 
             dt-it-docum-est.numero-ordem:bgcolor in browse br-itens = 10 
             dt-it-docum-est.serie-comp  :bgcolor in browse br-itens = 10 
             dt-it-docum-est.nro-comp    :bgcolor in browse br-itens = 10 
             dt-it-docum-est.nat-comp    :bgcolor in browse br-itens = 10 
             dt-it-docum-est.seq-comp    :bgcolor in browse br-itens = 10 
             dt-it-docum-est.nr-ord-produ:bgcolor in browse br-itens = 10 
             dt-it-docum-est.narrativa   :bgcolor in browse br-itens = 10.
  ELSE 
      ASSIGN dt-it-docum-est.log-1       :bgcolor in browse br-itens = ? 
             dt-it-docum-est.nat-operacao:bgcolor in browse br-itens = ? 
             dt-it-docum-est.sequencia   :bgcolor in browse br-itens = ? 
             dt-it-docum-est.it-codigo   :bgcolor in browse br-itens = ? 
             dt-it-docum-est.char-2      :bgcolor in browse br-itens = ? 
             dt-it-docum-est.item-ems    :bgcolor in browse br-itens = ? 
             dt-it-docum-est.cod-depos   :bgcolor in browse br-itens = ? 
             dt-it-docum-est.cod-localiz :bgcolor in browse br-itens = ?  
             cDescricao                  :bgcolor in browse br-itens = ?
             pPedidoCompr                :bgcolor in browse br-itens = ?
             dt-it-docum-est.un          :bgcolor in browse br-itens = ? 
             dt-it-docum-est.qt-do-forn  :bgcolor in browse br-itens = ? 
             dt-it-docum-est.dec-1       :bgcolor in browse br-itens = ? 
             dt-it-docum-est.lote        :bgcolor in browse br-itens = ? 
             dt-it-docum-est.dt-vali-lote:bgcolor in browse br-itens = ? 
             dt-it-docum-est.numero-ordem:bgcolor in browse br-itens = ? 
             dt-it-docum-est.serie-comp  :bgcolor in browse br-itens = ? 
             dt-it-docum-est.nro-comp    :bgcolor in browse br-itens = ? 
             dt-it-docum-est.nat-comp    :bgcolor in browse br-itens = ? 
             dt-it-docum-est.seq-comp    :bgcolor in browse br-itens = ? 
             dt-it-docum-est.nr-ord-produ:bgcolor in browse br-itens = ? 
             dt-it-docum-est.narrativa   :bgcolor in browse br-itens = ?.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-itens B-table-Win
ON ROW-ENTRY OF br-itens IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-itens B-table-Win
ON ROW-LEAVE OF br-itens IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   {src/adm/template/brsleave.i}
       APPLY 'value-changed' TO br-itens IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-itens B-table-Win
ON VALUE-CHANGED OF br-itens IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
     {src/adm/template/brschnge.i}
      FIND FIRST ITEM
          WHERE item.it-codigo = dt-it-docum-est.item-ems:SCREEN-VALUE IN BROWSE br-itens NO-LOCK NO-ERROR.
      IF AVAIL item THEN DO: 
          ASSIGN cDescricao:SCREEN-VALUE IN BROWSE br-itens = item.desc-item.
                /* item.un:SCREEN-VALUE IN BROWSE br-itens          = item.un */
                 /* dt-it-docum-est.cod-depos:SCREEN-VALUE IN BROWSE br-itens = item.deposito-pad. */

          ASSIGN dt-it-docum-est.lote        :READ-ONLY IN BROWSE br-itens = NOT(item.tipo-con-est <> 1)
                 dt-it-docum-est.dt-vali-lote:READ-ONLY IN BROWSE br-itens = not(can-do("3,4",string(item.tipo-con-est)))
                 lHabilitaLote    = IF lHabilitaLote = NO THEN item.tipo-con-est <> 1 ELSE lHabilitaLote.



      END.
      ELSE DO:
          ASSIGN dt-it-docum-est.lote        :READ-ONLY IN BROWSE br-itens = YES
                 dt-it-docum-est.dt-vali-lote:READ-ONLY IN BROWSE br-itens = YES.

      END.
      ASSIGN dt-it-docum-est.item-ems = dt-it-docum-est.item-ems:SCREEN-VALUE IN BROWSE br-itens.
      ASSIGN dt-it-docum-est.nro-comp:READ-ONLY IN BROWSE br-itens = not(l-nat-terc).
/*              dt-it-docum-est.nr-ord-produ:READ-ONLY IN BROWSE br-itens = not(l-nat-terc). */
      /* ASSIGN bt-lote:SENSITIVE IN FRAME {&FRAME-NAME} =  lHabilitaLote. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-agregado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-agregado B-table-Win
ON CHOOSE OF bt-agregado IN FRAME F-Main /* Agregado */
DO:
  IF l-nat-terc THEN DO:
      FIND FIRST tt-movto-pend NO-ERROR.
      IF NOT AVAIL tt-movto-pend THEN DO:
          CREATE tt-movto-pend.
          ASSIGN tt-movto-pend.serie-docto  = dt-docum-est.serie-docto
                 tt-movto-pend.nro-docto    = dt-docum-est.nro-docto 
                 tt-movto-pend.cod-emitente = dt-docum-est.cod-emitente 
                 tt-movto-pend.nat-operacao = dt-docum-est.nat-operacao.
      END.
      RUN dtp/dts0912j.w(INPUT-OUTPUT TABLE tt-movto-pend,
                         INPUT dt-docum-est.cod-estabel).
      FIND FIRST tt-movto-pend NO-LOCK NO-ERROR.
  END.
/*   EMPTY TEMP-TABLE tt2-dt-it-docum-est.                                   */
/*   FOR EACH dt-it-docum-est                                                */
/*      WHERE dt-it-docum-est.nro-docto    = dt-docum-est.nro-docto          */
/*        AND dt-it-docum-est.serie-docto  = dt-docum-est.serie-docto        */
/*        AND dt-it-docum-est.cod-emitente = dt-docum-est.cod-emitente       */
/*        AND dt-it-docum-est.nat-operacao = dt-docum-est.nat-operacao       */
/*        AND dt-it-docum-est.nome-arq     = dt-docum-est.nome-arq NO-LOCK:  */
/*       CREATE tt2-dt-it-docum-est.                                         */
/*       BUFFER-COPY dt-it-docum-est TO tt2-dt-it-docum-est.                 */
/*   END.                                                                    */
/*   IF VALID-HANDLE(vCabec) THEN                                            */
/*       RUN pi-retorna-campo IN vCabec(INPUT "nat-oper", OUTPUT vNatureza). */
/*   RUN dtp/dts0912h(INPUT vNatureza,                                       */
/*                    INPUT TABLE tt2-dt-it-docum-est,                       */
/*                    INPUT-OUTPUT TABLE tt-rat-lote).                       */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-devol
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-devol B-table-Win
ON CHOOSE OF bt-devol IN FRAME F-Main /* Associar Devoluá∆o */
DO:
  ASSIGN pRetorno = NO.
  IF l-nat-devol THEN DO:
      EMPTY TEMP-TABLE tt-nf-devol-cli.
      EMPTY TEMP-TABLE tt2-devol-cli.    
      EMPTY TEMP-TABLE tt2-dt-it-docum-est.
      FOR EACH dt-it-docum-est
         WHERE dt-it-docum-est.nro-docto    = dt-docum-est.nro-docto
           AND dt-it-docum-est.serie-docto  = dt-docum-est.serie-docto
           AND dt-it-docum-est.cod-emitente = dt-docum-est.cod-emitente
           /* AND dt-it-docum-est.nat-operacao = dt-docum-est.nat-operacao */
           AND dt-it-docum-est.nome-arq     = dt-docum-est.nome-arq NO-LOCK:
          CREATE tt2-dt-it-docum-est.
          BUFFER-COPY dt-it-docum-est TO tt2-dt-it-docum-est.
          ASSIGN tt2-dt-it-docum-est.pRowid = ROWID(dt-it-docum-est).
      END.
      IF AVAIL dt-docum-est THEN DO:
          CREATE tt2-dt-docum-est.
          BUFFER-COPY dt-docum-est TO tt2-dt-docum-est.
      END.
      IF VALID-HANDLE(vCabec) THEN DO:
          RUN pi-retorna-campo IN vCabec(INPUT "nat-oper", OUTPUT vNatureza).
          RUN pi-retorna-campo IN vCabec(INPUT "cod-emitente", OUTPUT cFornec).
          RUN pi-retorna-campo IN vCabec(INPUT "cod-estabel", OUTPUT pEstabelec).
          RUN pi-retorna-devol-cli IN vCabec(OUTPUT TABLE tt-nf-devol-cli).
      END.
      ASSIGN iFornec = INTEGER(cFornec).
      FIND FIRST tt-nf-devol-cli NO-LOCK NO-ERROR.
      IF AVAIL tt-nf-devol-cli THEN DO:
          RUN dtp/dts0912m.w(INPUT vNatureza,
                             INPUT tt-nf-devol-cli.rRowid, /* ROWID DA NOTA-FISCAL */ 
                             INPUT-OUTPUT TABLE tt2-dt-it-docum-est,
                             INPUT-OUTPUT TABLE tt-item-devol-cli,
                             INPUT tt-nf-devol-cli.reabre-pedido,
                             OUTPUT pRetorno).
          
      END.
      IF pRetorno THEN
        RUN piAtualizaDadosDev.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-gera-estrut
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-gera-estrut B-table-Win
ON CHOOSE OF bt-gera-estrut IN FRAME F-Main /* Gerar Estrutura */
DO:
    def var lEscolha as logical initial yes no-undo.
    EMPTY TEMP-TABLE tt2-dt-it-docum-est.
    if can-find (first dt-it-docum-est
                 WHERE dt-it-docum-est.nro-docto    = dt-docum-est.nro-docto
                   AND dt-it-docum-est.serie-docto  = dt-docum-est.serie-docto
                   AND dt-it-docum-est.cod-emitente = dt-docum-est.cod-emitente
                   AND dt-it-docum-est.nome-arq     = dt-docum-est.nome-arq
                   and dt-it-docum-est.log-2        = yes) then do:
        MESSAGE "Ao Gerar Retorno de MatÇria-prima pela Estrutura os retornos j† gerados para esta Nota ser∆o eliminados!" skip
                "Deseja gerar novamente o retorno pela Estrutura?"
            VIEW-AS ALERT-BOX question BUTTONS YES-NO TITLE "Gerar Estrutura" UPDATE lChoice AS LOGICAL.
        assign lEscolha = lChoice.
        if lChoice then do:
            for each dt-it-docum-est
                 where dt-it-docum-est.nro-docto    = dt-docum-est.nro-docto
                   AND dt-it-docum-est.serie-docto  = dt-docum-est.serie-docto
                   AND dt-it-docum-est.cod-emitente = dt-docum-est.cod-emitente
                   AND dt-it-docum-est.nome-arq     = dt-docum-est.nome-arq
                   and dt-it-docum-est.log-2        = yes exclusive-lock:
                delete dt-it-docum-est.
                 
            end.
        end.
    end.
    if lEscolha then do:
        FOR EACH dt-it-docum-est
           WHERE dt-it-docum-est.nro-docto    = dt-docum-est.nro-docto
             AND dt-it-docum-est.serie-docto  = dt-docum-est.serie-docto
             AND dt-it-docum-est.cod-emitente = dt-docum-est.cod-emitente
             AND dt-it-docum-est.nome-arq     = dt-docum-est.nome-arq
             and dt-it-docum-est.log-1        = yes NO-LOCK:
            CREATE tt2-dt-it-docum-est.
            BUFFER-COPY dt-it-docum-est TO tt2-dt-it-docum-est.
            ASSIGN tt2-dt-it-docum-est.pRowid = ROWID(dt-it-docum-est).
        END.
        IF VALID-HANDLE(vCabec) THEN
            RUN pi-retorna-campo IN vCabec(INPUT "nat-oper", OUTPUT vNatureza).
        RUN dtp/dts0912r.w(INPUT vNatureza,
                           input-output TABLE tt2-dt-it-docum-est,
                           OUTPUT pRetorno).
        if pRetorno then do:
            for each tt2-dt-it-docum-est 
                where tt2-dt-it-docum-est.log-2 = yes:
                create dt-it-docum-est.
                buffer-copy tt2-dt-it-docum-est to dt-it-docum-est.
                find first item
                    where item.it-codigo = dt-it-docum-est.item-ems no-lock no-error.
                if avail item then
                    assign dt-it-docum-est.char-2 = item.desc-item.

            end.
        end.
        {&OPEN-QUERY-br-itens}
        /* run dtp/dts0912r.w(). */
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-gerar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-gerar B-table-Win
ON CHOOSE OF bt-gerar IN FRAME F-Main /* Gerar Devoluá∆o */
DO:
  ASSIGN pRetorno = NO.
  IF l-nat-devol THEN DO:
      EMPTY TEMP-TABLE tt-nf-devol-cli.
      EMPTY TEMP-TABLE tt2-devol-cli.    
      EMPTY TEMP-TABLE tt2-dt-it-docum-est.
      FOR EACH dt-it-docum-est
         WHERE dt-it-docum-est.nro-docto    = dt-docum-est.nro-docto
           AND dt-it-docum-est.serie-docto  = dt-docum-est.serie-docto
           AND dt-it-docum-est.cod-emitente = dt-docum-est.cod-emitente
           AND dt-it-docum-est.nat-operacao = dt-docum-est.nat-operacao
           AND dt-it-docum-est.nome-arq     = dt-docum-est.nome-arq NO-LOCK:
          CREATE tt2-dt-it-docum-est.
          BUFFER-COPY dt-it-docum-est TO tt2-dt-it-docum-est.
          ASSIGN tt2-dt-it-docum-est.pRowid = ROWID(dt-it-docum-est).
      END.
/*       RUN dtp/dts0912h.w(INPUT vNatureza,                 */
/*                          INPUT TABLE tt2-dt-it-docum-est, */
/*                          INPUT-OUTPUT TABLE tt-rat-lote). */
      

      IF AVAIL dt-docum-est THEN DO:
          CREATE tt2-dt-docum-est.
          BUFFER-COPY dt-docum-est TO tt2-dt-docum-est.
      END.
      IF VALID-HANDLE(vCabec) THEN DO:
          RUN pi-retorna-campo IN vCabec(INPUT "nat-oper", OUTPUT vNatureza).
          RUN pi-retorna-campo IN vCabec(INPUT "cod-emitente", OUTPUT cFornec).
          RUN pi-retorna-campo IN vCabec(INPUT "cod-estabel", OUTPUT pEstabelec).
      END.
      ASSIGN iFornec = INTEGER(cFornec).
      RUN dtp/dts0912l.w (INPUT pEstabelec,
                          INPUT iFornec,
                          OUTPUT TABLE tt-nf-devol-cli).
      FIND FIRST tt-nf-devol-cli NO-LOCK NO-ERROR.
      IF AVAIL tt-nf-devol-cli THEN DO:
          RUN dtp/dts0912m.w(INPUT vNatureza,
                             INPUT tt-nf-devol-cli.rRowid, /* ROWID DA NOTA-FISCAL */ 
                             INPUT-OUTPUT TABLE tt2-dt-it-docum-est,
                             INPUT-OUTPUT TABLE tt-item-devol-cli,
                             INPUT tt-nf-devol-cli.reabre-pedido,
                             OUTPUT pRetorno).

      END.
      
/*      RUN dtp/dts0912k.w  (INPUT vNatureza,
                           INPUT TABLE tt2-dt-docum-est,
                           INPUT-OUTPUT TABLE tt-rat-docum,
                           OUTPUT TABLE tt-imposto) NO-ERROR . */
      IF pRetorno THEN
        RUN piAtualizaDadosDev.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-it-agregado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-it-agregado B-table-Win
ON CHOOSE OF bt-it-agregado IN FRAME F-Main /* Item Agregado */
DO:
    IF dt-it-docum-est.log-1 THEN
        ASSIGN dt-it-docum-est.log-1 = NO.
    ELSE DO:
        ASSIGN dt-it-docum-est.log-1 = YES.
        IF l-nat-terc THEN DO:
            FIND FIRST tt-movto-pend NO-ERROR.
            IF NOT AVAIL tt-movto-pend THEN DO:
                CREATE tt-movto-pend.
                ASSIGN tt-movto-pend.serie-docto    = dt-docum-est.serie-docto
                       tt-movto-pend.nro-docto      = dt-docum-est.nro-docto 
                       tt-movto-pend.cod-emitente   = dt-docum-est.cod-emitente 
                       tt-movto-pend.nat-operacao   = dt-it-docum-est.nat-operacao.
                assign tt-movto-pend.nro-comp       = string(dt-docum-est.nro-docto ,"9999999")
                       tt-movto-pend.nat-comp       = dt-it-docum-est.nat-operacao        
                       tt-movto-pend.serie-comp     = dt-docum-est.serie-docto 
                       tt-movto-pend.tipo           = 1
                       tt-movto-pend.nr-ord-produ   = dt-it-docum-est.nr-ord-prod
                       tt-movto-pend.it-codigo      = dt-it-docum-est.ITEM-ems
                       tt-movto-pend.quantidade     = dt-it-docum-est.dec-1.
                find FIRST item-uni-estab
                    WHERE item-uni-estab.cod-estabel = dt-docum-est.cod-estabel
                      AND item-uni-estab.it-codigo   = dt-it-docum-est.item-ems NO-LOCK NO-ERROR.
                IF AVAIL item-uni-estab THEN
                    ASSIGN tt-movto-pend.cod-depos = item-uni-estab.deposito-pad.
                for first estab-mat
                    fields ( cod-estabel-prin   conta-fornec )        
                    where estab-mat.cod-estabel = dt-docum-est.cod-estabel no-lock:
                end.

                assign tt-movto-pend.conta-contabil = if  avail estab-mat 
                                                                  then estab-mat.conta-fornec
                                                                  else "".

            END.
/*             RUN dtp/dts0912j.w(INPUT-OUTPUT TABLE tt-movto-pend, */
/*                                INPUT dt-docum-est.cod-estabel).  */
            FIND FIRST tt-movto-pend NO-LOCK NO-ERROR.
        END.
    END.
        

  br-itens:REFRESH().
  /* {&OPEN-QUERY-br-itens} */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-lote
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-lote B-table-Win
ON CHOOSE OF bt-lote IN FRAME F-Main /* Rateio Lotes/Localizaá∆o */
DO:
  EMPTY TEMP-TABLE tt2-dt-it-docum-est.
/*  MESSAGE dt-docum-est.nat-operacao
      VIEW-AS ALERT-BOX INFO BUTTONS OK. */
  FOR EACH dt-it-docum-est
     WHERE dt-it-docum-est.nro-docto    = dt-docum-est.nro-docto
       AND dt-it-docum-est.serie-docto  = dt-docum-est.serie-docto
       AND dt-it-docum-est.cod-emitente = dt-docum-est.cod-emitente
       /* AND dt-it-docum-est.nat-operacao = dt-docum-est.nat-operacao */
       AND dt-it-docum-est.nome-arq     = dt-docum-est.nome-arq NO-LOCK:
      CREATE tt2-dt-it-docum-est.
      BUFFER-COPY dt-it-docum-est TO tt2-dt-it-docum-est.
  END.
  IF VALID-HANDLE(vCabec) THEN
      RUN pi-retorna-campo IN vCabec(INPUT "nat-oper", OUTPUT vNatureza).
  RUN dtp/dts0912h.w(INPUT vNatureza,
                     INPUT TABLE tt2-dt-it-docum-est,
                     INPUT-OUTPUT TABLE tt-rat-lote).
/*   for each tt-rat-lote no-lock:            */
/*       message tt-rat-lote.it-codigo   skip */
/*               tt-rat-lote.cod-depos   skip */
/*               tt-rat-lote.cod-localiz skip */
/*               tt-rat-lote.lote        skip */
/*               tt-rat-lote.quantidade  skip */
/*           view-as alert-box.               */
/*   end.                                     */


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-rateio
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-rateio B-table-Win
ON CHOOSE OF bt-rateio IN FRAME F-Main /* Rateio CTe */
DO:
  IF l-nat-cte THEN DO:
      EMPTY TEMP-TABLE tt2-dt-docum-est.
      
      IF AVAIL dt-docum-est THEN DO:
          CREATE tt2-dt-docum-est.
          BUFFER-COPY dt-docum-est TO tt2-dt-docum-est.
      END.
      IF VALID-HANDLE(vCabec) THEN
          RUN pi-retorna-campo IN vCabec(INPUT "nat-oper", OUTPUT vNatureza).
      RUN dtp/dts0912k.w  (INPUT vNatureza,
                           INPUT TABLE tt2-dt-docum-est,
                           INPUT-OUTPUT TABLE tt-rat-docum,
                           OUTPUT TABLE tt-imposto) NO-ERROR .
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-rateio-nat-op
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-rateio-nat-op B-table-Win
ON CHOOSE OF bt-rateio-nat-op IN FRAME F-Main /* Rateio Operaá∆o */
DO:
  ASSIGN pRetorno = NO.
  EMPTY TEMP-TABLE tt2-dt-it-docum-est.
  FIND CURRENT dt-it-docum-est NO-LOCK NO-ERROR.
  IF AVAIL dt-it-docum-est THEN DO:
      CREATE tt2-dt-it-docum-est.
      BUFFER-COPY dt-it-docum-est TO tt2-dt-it-docum-est.
      ASSIGN tt2-dt-it-docum-est.pRowid = ROWID(dt-it-docum-est).
  END.
  IF VALID-HANDLE(vCabec) THEN
      RUN pi-retorna-campo IN vCabec(INPUT "nat-oper", OUTPUT vNatureza).
  RUN dtp/dts0912p.w(INPUT vNatureza,
                     INPUT TABLE tt2-dt-it-docum-est,
                     INPUT-OUTPUT TABLE tt-dt-rat-nat-operacao,
                     OUTPUT pRetorno).

  FIND FIRST tt-dt-rat-nat-operacao EXCLUSIVE-LOCK NO-ERROR.
  IF AVAIL tt-dt-rat-nat-operacao AND pRetorno THEN DO:
      FIND FIRST tt2-dt-it-docum-est NO-LOCK NO-ERROR.
      IF AVAIL tt2-dt-it-docum-est THEN DO:
          FIND FIRST dt-it-docum-est
              WHERE ROWID(dt-it-docum-est) = tt2-dt-it-docum-est.pRowid EXCLUSIVE-LOCK NO-ERROR.
          IF AVAIL dt-it-docum-est THEN
              DELETE dt-it-docum-est.
          FOR EACH tt-dt-rat-nat-operacao
              WHERE tt-dt-rat-nat-operacao.cod-emitente = tt2-dt-it-docum-est.cod-emitente
                and tt-dt-rat-nat-operacao.serie-docto  = tt2-dt-it-docum-est.serie-docto
                and tt-dt-rat-nat-operacao.nro-docto    = tt2-dt-it-docum-est.nro-docto
                and tt-dt-rat-nat-operacao.nat-operacao = tt2-dt-it-docum-est.nat-operacao
                and tt-dt-rat-nat-operacao.item-ems    = tt2-dt-it-docum-est.item-ems
                NO-LOCK.
              CREATE dt-it-docum-est.
              BUFFER-COPY tt-dt-rat-nat-operacao TO dt-it-docum-est.

          END.
          FOR EACH dt-it-docum-est 
              WHERE dt-it-docum-est.cod-emitente = tt2-dt-it-docum-est.cod-emitente
                and dt-it-docum-est.serie-docto  = tt2-dt-it-docum-est.serie-docto
                and dt-it-docum-est.nro-docto    = tt2-dt-it-docum-est.nro-docto
                and dt-it-docum-est.nat-operacao = tt2-dt-it-docum-est.nat-operacao
                and dt-it-docum-est.item-ems     = tt2-dt-it-docum-est.item-ems EXCLUSIVE-LOCK:
              ASSIGN dt-it-docum-est.qt-do-forn  = ((dt-it-docum-est.quantidade * tt2-dt-it-docum-est.qt-do-forn) / tt2-dt-it-docum-est.quantidade)
                     dt-it-docum-est.dec-1       = ((dt-it-docum-est.quantidade * tt2-dt-it-docum-est.dec-1) / tt2-dt-it-docum-est.quantidade)
                     dt-it-docum-est.base-cofins = ((dt-it-docum-est.quantidade * tt2-dt-it-docum-est.base-cofins) / tt2-dt-it-docum-est.quantidade)
                     dt-it-docum-est.base-icm    = ((dt-it-docum-est.quantidade * tt2-dt-it-docum-est.base-icm) / tt2-dt-it-docum-est.quantidade)
                     dt-it-docum-est.base-ipi    = ((dt-it-docum-est.quantidade * tt2-dt-it-docum-est.base-ipi) / tt2-dt-it-docum-est.quantidade)
                     dt-it-docum-est.base-iss    = ((dt-it-docum-est.quantidade * tt2-dt-it-docum-est.base-iss) / tt2-dt-it-docum-est.quantidade)
                     dt-it-docum-est.base-pis    = ((dt-it-docum-est.quantidade * tt2-dt-it-docum-est.base-pis) / tt2-dt-it-docum-est.quantidade)
                     dt-it-docum-est.base-subs   = ((dt-it-docum-est.quantidade * tt2-dt-it-docum-est.base-subs) / tt2-dt-it-docum-est.quantidade)
                     dt-it-docum-est.preco-total = ((dt-it-docum-est.quantidade * tt2-dt-it-docum-est.preco-total) / tt2-dt-it-docum-est.quantidade)
                     dt-it-docum-est.valor-icm   = ((dt-it-docum-est.quantidade * tt2-dt-it-docum-est.valor-icm) / tt2-dt-it-docum-est.quantidade)
                     dt-it-docum-est.valor-ipi   = ((dt-it-docum-est.quantidade * tt2-dt-it-docum-est.valor-ipi) / tt2-dt-it-docum-est.quantidade)
                     dt-it-docum-est.valor-iss   = ((dt-it-docum-est.quantidade * tt2-dt-it-docum-est.valor-iss) / tt2-dt-it-docum-est.quantidade)
                     dt-it-docum-est.valor-subs  = ((dt-it-docum-est.quantidade * tt2-dt-it-docum-est.valor-subs) / tt2-dt-it-docum-est.quantidade).
          END.
      END.
      EMPTY TEMP-TABLE tt-dt-rat-nat-operacao.
  END.
  {&OPEN-QUERY-br-itens}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-rateio-oc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-rateio-oc B-table-Win
ON CHOOSE OF bt-rateio-oc IN FRAME F-Main /* Rateio Compra */
DO:
  ASSIGN pRetorno = NO.
  EMPTY TEMP-TABLE tt2-dt-it-docum-est.
  FIND CURRENT dt-it-docum-est NO-LOCK NO-ERROR.
  IF AVAIL dt-it-docum-est THEN DO:
      CREATE tt2-dt-it-docum-est.
      BUFFER-COPY dt-it-docum-est TO tt2-dt-it-docum-est.
      ASSIGN tt2-dt-it-docum-est.pRowid = ROWID(dt-it-docum-est).
  END.
  IF VALID-HANDLE(vCabec) THEN
      RUN pi-retorna-campo IN vCabec(INPUT "nat-oper", OUTPUT vNatureza).
  RUN dtp/dts0912o.w(INPUT vNatureza,
                     INPUT TABLE tt2-dt-it-docum-est,
                     INPUT-OUTPUT TABLE tt-dt-rat-ord-compra,
                     OUTPUT pRetorno).

  FIND FIRST tt-dt-rat-ord-compra EXCLUSIVE-LOCK NO-ERROR.
  IF AVAIL tt-dt-rat-ord-compra AND pRetorno THEN DO:
      FIND FIRST tt2-dt-it-docum-est NO-LOCK NO-ERROR.
      IF AVAIL tt2-dt-it-docum-est THEN DO:
          FIND FIRST dt-it-docum-est
              WHERE ROWID(dt-it-docum-est) = tt2-dt-it-docum-est.pRowid EXCLUSIVE-LOCK NO-ERROR.
          IF AVAIL dt-it-docum-est THEN
              DELETE dt-it-docum-est.
          FOR EACH tt-dt-rat-ord-compra
              WHERE tt-dt-rat-ord-compra.cod-emitente = tt2-dt-it-docum-est.cod-emitente
                and tt-dt-rat-ord-compra.serie-docto  = tt2-dt-it-docum-est.serie-docto
                and tt-dt-rat-ord-compra.nro-docto    = tt2-dt-it-docum-est.nro-docto
                and tt-dt-rat-ord-compra.nat-operacao = tt2-dt-it-docum-est.nat-operacao
                and tt-dt-rat-ord-compra.item-ems    = tt2-dt-it-docum-est.item-ems
                AND tt-dt-rat-ord-compra.int-2 <> 1  NO-LOCK.
              CREATE dt-it-docum-est.
              BUFFER-COPY tt-dt-rat-ord-compra TO dt-it-docum-est.

          END.
          FOR EACH dt-it-docum-est 
              WHERE dt-it-docum-est.cod-emitente = tt2-dt-it-docum-est.cod-emitente
                and dt-it-docum-est.serie-docto  = tt2-dt-it-docum-est.serie-docto
                and dt-it-docum-est.nro-docto    = tt2-dt-it-docum-est.nro-docto
                and dt-it-docum-est.nat-operacao = tt2-dt-it-docum-est.nat-operacao
                and dt-it-docum-est.item-ems     = tt2-dt-it-docum-est.item-ems EXCLUSIVE-LOCK:
              ASSIGN dt-it-docum-est.qt-do-forn  = ((dt-it-docum-est.quantidade * tt2-dt-it-docum-est.qt-do-forn) / tt2-dt-it-docum-est.quantidade)
                     dt-it-docum-est.dec-1       = ((dt-it-docum-est.quantidade * tt2-dt-it-docum-est.dec-1) / tt2-dt-it-docum-est.quantidade)
                     dt-it-docum-est.base-cofins = ((dt-it-docum-est.quantidade * tt2-dt-it-docum-est.base-cofins) / tt2-dt-it-docum-est.quantidade)
                     dt-it-docum-est.base-icm    = ((dt-it-docum-est.quantidade * tt2-dt-it-docum-est.base-icm) / tt2-dt-it-docum-est.quantidade)
                     dt-it-docum-est.base-ipi    = ((dt-it-docum-est.quantidade * tt2-dt-it-docum-est.base-ipi) / tt2-dt-it-docum-est.quantidade)
                     dt-it-docum-est.base-iss    = ((dt-it-docum-est.quantidade * tt2-dt-it-docum-est.base-iss) / tt2-dt-it-docum-est.quantidade)
                     dt-it-docum-est.base-pis    = ((dt-it-docum-est.quantidade * tt2-dt-it-docum-est.base-pis) / tt2-dt-it-docum-est.quantidade)
                     dt-it-docum-est.base-subs   = ((dt-it-docum-est.quantidade * tt2-dt-it-docum-est.base-subs) / tt2-dt-it-docum-est.quantidade)
                     dt-it-docum-est.preco-total = ((dt-it-docum-est.quantidade * tt2-dt-it-docum-est.preco-total) / tt2-dt-it-docum-est.quantidade)
                     dt-it-docum-est.valor-icm   = ((dt-it-docum-est.quantidade * tt2-dt-it-docum-est.valor-icm) / tt2-dt-it-docum-est.quantidade)
                     dt-it-docum-est.valor-ipi   = ((dt-it-docum-est.quantidade * tt2-dt-it-docum-est.valor-ipi) / tt2-dt-it-docum-est.quantidade)
                     dt-it-docum-est.valor-iss   = ((dt-it-docum-est.quantidade * tt2-dt-it-docum-est.valor-iss) / tt2-dt-it-docum-est.quantidade)
                     dt-it-docum-est.valor-subs  = ((dt-it-docum-est.quantidade * tt2-dt-it-docum-est.valor-subs) / tt2-dt-it-docum-est.quantidade).
          END.
      END.
      EMPTY TEMP-TABLE tt-dt-rat-ord-compra.
  END.
  {&OPEN-QUERY-br-itens}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-rateio-op
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-rateio-op B-table-Win
ON CHOOSE OF bt-rateio-op IN FRAME F-Main /* Rateio Produá∆o */
DO:
  ASSIGN pRetorno = NO.
  EMPTY TEMP-TABLE tt2-dt-it-docum-est.
  FIND CURRENT dt-it-docum-est NO-LOCK NO-ERROR.
  IF AVAIL dt-it-docum-est THEN DO:
      CREATE tt2-dt-it-docum-est.
      BUFFER-COPY dt-it-docum-est TO tt2-dt-it-docum-est.
      ASSIGN tt2-dt-it-docum-est.pRowid = ROWID(dt-it-docum-est).
  END.
  IF VALID-HANDLE(vCabec) THEN
      RUN pi-retorna-campo IN vCabec(INPUT "nat-oper", OUTPUT vNatureza).
  RUN dtp/dts0912n.w(INPUT vNatureza,
                     INPUT TABLE tt2-dt-it-docum-est,
                     INPUT-OUTPUT TABLE tt-dt-rat-ord-prod,
                     OUTPUT pRetorno).

  FIND FIRST tt-dt-rat-ord-prod EXCLUSIVE-LOCK NO-ERROR.
  IF AVAIL tt-dt-rat-ord-prod AND pRetorno THEN DO:
      FIND FIRST tt2-dt-it-docum-est NO-LOCK NO-ERROR.
      IF AVAIL tt2-dt-it-docum-est THEN DO:
          FIND FIRST dt-it-docum-est
              WHERE ROWID(dt-it-docum-est) = tt2-dt-it-docum-est.pRowid EXCLUSIVE-LOCK NO-ERROR.
          IF AVAIL dt-it-docum-est THEN
              DELETE dt-it-docum-est.
          FOR EACH tt-dt-rat-ord-prod
              WHERE tt-dt-rat-ord-prod.cod-emitente = tt2-dt-it-docum-est.cod-emitente
                and tt-dt-rat-ord-prod.serie-docto  = tt2-dt-it-docum-est.serie-docto
                and tt-dt-rat-ord-prod.nro-docto    = tt2-dt-it-docum-est.nro-docto
                and tt-dt-rat-ord-prod.nat-operacao = tt2-dt-it-docum-est.nat-operacao
                and tt-dt-rat-ord-prod.item-ems    = tt2-dt-it-docum-est.item-ems
                AND tt-dt-rat-ord-prod.int-2 <> 1  NO-LOCK.
              CREATE dt-it-docum-est.
              BUFFER-COPY tt-dt-rat-ord-prod TO dt-it-docum-est.

          END.
          FOR EACH dt-it-docum-est 
              WHERE dt-it-docum-est.cod-emitente = tt2-dt-it-docum-est.cod-emitente
                and dt-it-docum-est.serie-docto  = tt2-dt-it-docum-est.serie-docto
                and dt-it-docum-est.nro-docto    = tt2-dt-it-docum-est.nro-docto
                and dt-it-docum-est.nat-operacao = tt2-dt-it-docum-est.nat-operacao
                and dt-it-docum-est.item-ems     = tt2-dt-it-docum-est.item-ems EXCLUSIVE-LOCK:
              ASSIGN dt-it-docum-est.qt-do-forn  = ((dt-it-docum-est.quantidade * tt2-dt-it-docum-est.qt-do-forn) / tt2-dt-it-docum-est.quantidade)
                     dt-it-docum-est.dec-1       = ((dt-it-docum-est.quantidade * tt2-dt-it-docum-est.dec-1) / tt2-dt-it-docum-est.quantidade)
                     dt-it-docum-est.base-cofins = ((dt-it-docum-est.quantidade * tt2-dt-it-docum-est.base-cofins) / tt2-dt-it-docum-est.quantidade)
                     dt-it-docum-est.base-icm    = ((dt-it-docum-est.quantidade * tt2-dt-it-docum-est.base-icm) / tt2-dt-it-docum-est.quantidade)
                     dt-it-docum-est.base-ipi    = ((dt-it-docum-est.quantidade * tt2-dt-it-docum-est.base-ipi) / tt2-dt-it-docum-est.quantidade)
                     dt-it-docum-est.base-iss    = ((dt-it-docum-est.quantidade * tt2-dt-it-docum-est.base-iss) / tt2-dt-it-docum-est.quantidade)
                     dt-it-docum-est.base-pis    = ((dt-it-docum-est.quantidade * tt2-dt-it-docum-est.base-pis) / tt2-dt-it-docum-est.quantidade)
                     dt-it-docum-est.base-subs   = ((dt-it-docum-est.quantidade * tt2-dt-it-docum-est.base-subs) / tt2-dt-it-docum-est.quantidade)
                     dt-it-docum-est.preco-total = ((dt-it-docum-est.quantidade * tt2-dt-it-docum-est.preco-total) / tt2-dt-it-docum-est.quantidade)
                     dt-it-docum-est.valor-icm   = ((dt-it-docum-est.quantidade * tt2-dt-it-docum-est.valor-icm) / tt2-dt-it-docum-est.quantidade)
                     dt-it-docum-est.valor-ipi   = ((dt-it-docum-est.quantidade * tt2-dt-it-docum-est.valor-ipi) / tt2-dt-it-docum-est.quantidade)
                     dt-it-docum-est.valor-iss   = ((dt-it-docum-est.quantidade * tt2-dt-it-docum-est.valor-iss) / tt2-dt-it-docum-est.quantidade)
                     dt-it-docum-est.valor-subs  = ((dt-it-docum-est.quantidade * tt2-dt-it-docum-est.valor-subs) / tt2-dt-it-docum-est.quantidade).
          END.
      END.
      EMPTY TEMP-TABLE tt-dt-rat-ord-prod.
  END.
  {&OPEN-QUERY-br-itens}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

    &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
    &ENDIF
    dt-it-docum-est.item-ems:LOAD-MOUSE-POINTER("image/lupa.cur").
    dt-it-docum-est.numero-ordem:LOAD-MOUSE-POINTER("image/lupa.cur").
    dt-it-docum-est.cod-depos:LOAD-MOUSE-POINTER("image/lupa.cur").
    dt-it-docum-est.cod-localiz:LOAD-MOUSE-POINTER("image/lupa.cur").
    dt-it-docum-est.nro-comp:LOAD-MOUSE-POINTER("image/lupa.cur").
    dt-it-docum-est.nr-ord-produ:LOAD-MOUSE-POINTER("image/lupa.cur").
    dt-it-docum-est.nat-operacao:LOAD-MOUSE-POINTER("image/lupa.cur").
    ON 'mouse-select-dblclick' OF dt-it-docum-est.item-ems DO:
       DEF VAR hProgramZoom AS HANDLE NO-UNDO.
       {include/zoomvar.i &prog-zoom=inzoom/z02in172.w
                          &campo=dt-it-docum-est.item-ems
                          &campozoom=it-codigo
                          &browse=br-itens}   
    END.
    ON 'F5':U OF dt-it-docum-est.nat-operacao  DO:
        {method/zoomfields.i &ProgramZoom="inzoom/z04in245.w"
                              &FieldZoom1="nat-operacao"
                              &FieldScreen1="dt-it-docum-est.nat-operacao"
                              &browse1="br-itens"                                                  
                              &EnableImplant="NO"}  

    END.
    ON 'mouse-select-dblclick':U OF dt-it-docum-est.nat-operacao  DO:
        {method/zoomfields.i &ProgramZoom="inzoom/z04in245.w"
                              &FieldZoom1="nat-operacao"
                              &FieldScreen1="dt-it-docum-est.nat-operacao"
                              &browse1="br-itens"                                                  
                              &EnableImplant="NO"}  
    END.
    ON 'F5' OF dt-it-docum-est.item-ems DO:
        DEF VAR hProgramZoom AS HANDLE NO-UNDO.
        {include/zoomvar.i &prog-zoom=inzoom/z02in172.w
                           &campo=dt-it-docum-est.item-ems
                           &campozoom=it-codigo
                           &browse=br-itens}   
    END.
    ON 'F5' OF dt-it-docum-est.cod-depos DO:
        {include/zoomvar.i &prog-zoom=inzoom/z01in084.w
                           &campo=dt-it-docum-est.cod-depos
                           &campozoom=cod-depos
                           &browse=br-itens} 
       APPLY "leave" TO dt-it-docum-est.cod-depos IN BROWSE br-itens.
    END. 
    ON 'mouse-select-dblclick' OF dt-it-docum-est.cod-depos DO:
        {include/zoomvar.i &prog-zoom=inzoom/z01in084.w
                           &campo=dt-it-docum-est.cod-depos
                           &campozoom=cod-depos
                           &browse=br-itens}
        APPLY "leave" TO dt-it-docum-est.cod-depos IN BROWSE br-itens.
    END.  
    ON 'leave' OF dt-it-docum-est.cod-depos DO:
        ASSIGN dt-it-docum-est.cod-depos = dt-it-docum-est.cod-depos:SCREEN-VALUE IN BROWSE br-itens.
    END. 
    ON 'mouse-select-dblclick' OF dt-it-docum-est.cod-localiz DO:
        {include/zoomvar.i &prog-zoom="inzoom/z02in189.w"
                           &campo=dt-it-docum-est.cod-localiz
                           &campozoom=cod-localiz
                           &browse=br-itens}
         APPLY "leave" TO dt-it-docum-est.cod-localiz IN BROWSE br-itens.
    END. 
    ON 'F5' OF dt-it-docum-est.cod-localiz DO:
        {include/zoomvar.i &prog-zoom="inzoom/z02in189.w"
                           &campo=dt-it-docum-est.cod-localiz
                           &campozoom=cod-localiz
                           &browse=br-itens}
         APPLY "leave" TO dt-it-docum-est.cod-localiz IN BROWSE br-itens.
    END. 
    ON 'leave' OF dt-it-docum-est.cod-localiz DO:
        ASSIGN dt-it-docum-est.cod-localiz = dt-it-docum-est.cod-localiz:SCREEN-VALUE IN BROWSE br-itens.
    END. 


    ON 'mouse-select-dblclick' OF dt-it-docum-est.numero-ordem DO:

       {include/zoomvar.i &prog-zoom=dtzoom/z02dt0912.w
                          &campo=dt-it-docum-est.item-ems
                          &campozoom=it-codigo
                          &browse=br-itens
                          &campo2=dt-it-docum-est.numero-ordem
                          &campozoom2=numero-ordem
                          &browse=br-itens
                          &campo3=dt-it-docum-est.narrativa
                          &campozoom3=narrativa
                          &browse=br-itens}
         APPLY "leave" TO  dt-it-docum-est.numero-ordem  IN BROWSE br-itens.                   
        /*APPLY 'LEAVE' TO dt-it-docum-est.item-ems.                   */
    END.  
    ON 'mouse-select-dblclick':U OF dt-it-docum-est.nro-comp DO:
        IF l-nat-terc THEN DO:
            {include/zoomvar.i &prog-zoom=dtzoom/z03dt0912.w
                               &campo=dt-it-docum-est.serie-comp
                               &campozoom=serie
                               &browse=br-itens
                               &campo2=dt-it-docum-est.nat-comp    
                               &campozoom2=nat-operacao                
                               &browse=br-itens                        
                               &campo3=dt-it-docum-est.nro-comp       
                               &campozoom3=nro-docto                   
                               &browse=br-itens                        
                               &campo4=dt-it-docum-est.seq-comp       
                               &campozoom4=sequencia                   
                               &browse=br-itens                        
                               &campo5=dt-it-docum-est.nr-ord-produ       
                               &campozoom5=nr-ord-produ                   
                               &browse=br-itens   
                               &campo6=dt-it-docum-est.item-ems
                               &campozoom6=it-codigo
                               &browse=br-itens   
                               &parametros="run pi-faixa in wh-pesquisa(INPUT dt-it-docum-est.item-ems, INPUT dt-it-docum-est.cod-emitente ,INPUT vNatureza, input dt-it-docum-est.quantidade )." }
            APPLY "leave" TO  dt-it-docum-est.nro-comp  IN BROWSE br-itens.
        END.
    END.
    ON 'F5':U OF dt-it-docum-est.nro-comp DO:
        IF l-nat-terc THEN DO:
            {include/zoomvar.i &prog-zoom=dtzoom/z03dt0912.w
                               &campo=dt-it-docum-est.serie-comp
                               &campozoom=serie
                               &browse=br-itens
                               &campo2=dt-it-docum-est.nat-comp    
                               &campozoom2=nat-operacao                
                               &browse=br-itens                        
                               &campo3=dt-it-docum-est.nro-comp       
                               &campozoom3=nro-docto                   
                               &browse=br-itens                        
                               &campo4=dt-it-docum-est.seq-comp       
                               &campozoom4=sequencia                   
                               &browse=br-itens                        
                               &campo5=dt-it-docum-est.nr-ord-produ       
                               &campozoom5=nr-ord-produ                   
                               &browse=br-itens                        
                               &campo6=dt-it-docum-est.item-ems
                               &campozoom6=it-codigo
                               &browse=br-itens   
                               &parametros="run pi-faixa in wh-pesquisa(INPUT dt-it-docum-est.item-ems, INPUT dt-it-docum-est.cod-emitente ,INPUT vNatureza, input dt-it-docum-est.quantidade )." }
            APPLY "leave" TO  dt-it-docum-est.nro-comp  IN BROWSE br-itens.
        END.
    END.
    ON 'F5' OF dt-it-docum-est.numero-ordem DO:
       {include/zoomvar.i &prog-zoom=dtzoom/z02dt0912.w
                          &campo=dt-it-docum-est.item-ems
                          &campozoom=it-codigo
                          &browse=br-itens
                          &campo2=dt-it-docum-est.numero-ordem
                          &campozoom2=numero-ordem
                          &browse=br-itens
                          &campo3=dt-it-docum-est.narrativa
                          &campozoom3=narrativa
                          &browse=br-itens}
        APPLY "leave" TO  dt-it-docum-est.numero-ordem  IN BROWSE br-itens.                   
    END.
    ON 'F5':U OF dt-it-docum-est.nr-ord-produ DO:
        {include/zoomvar.i &prog-zoom=inzoom/z01in271.w
                           &campo=dt-it-docum-est.nr-ord-produ
                           &campozoom=nr-ord-produ
                           &browse=br-itens}
        APPLY "leave" TO  dt-it-docum-est.nr-ord-produ  IN BROWSE br-itens.                   
    END.
    ON 'mouse-select-dblclick':U OF dt-it-docum-est.nr-ord-produ DO:
        {include/zoomvar.i &prog-zoom=inzoom/z01in271.w
                           &campo=dt-it-docum-est.nr-ord-produ
                           &campozoom=nr-ord-produ
                           &browse=br-itens}
        APPLY "leave" TO  dt-it-docum-est.nr-ord-produ  IN BROWSE br-itens.                   
    END.
    ON 'leave'  OF dt-it-docum-est.item-ems DO:
        IF dt-it-docum-est.item-ems <> dt-it-docum-est.item-ems:SCREEN-VALUE IN BROWSE br-itens THEN DO:
            FOR EACH tt-rat-lote
                WHERE tt-rat-lote.cod-emitente = dt-it-docum-est.cod-emitente
                  and tt-rat-lote.serie-docto  = dt-it-docum-est.serie-docto
                  and tt-rat-lote.nro-docto    = dt-it-docum-est.nro-docto
                  and tt-rat-lote.nat-operacao = dt-it-docum-est.nat-operacao
                  and tt-rat-lote.sequencia    = dt-it-docum-est.sequencia
                  AND tt-rat-lote.it-codigo    = dt-it-docum-est.item-ems  EXCLUSIVE-LOCK.
                ASSIGN tt-rat-lote.it-codigo    = dt-it-docum-est.item-ems:SCREEN-VALUE IN BROWSE br-itens.
            END.
        END.
        ASSIGN dt-it-docum-est.item-ems = dt-it-docum-est.item-ems:SCREEN-VALUE IN BROWSE br-itens.
        FIND FIRST ITEM
            WHERE item.it-codigo = dt-it-docum-est.item-ems:SCREEN-VALUE IN BROWSE br-itens NO-LOCK NO-ERROR.
        IF AVAIL item THEN DO: 
            ASSIGN cDescricao:SCREEN-VALUE IN BROWSE br-itens = item.desc-item.
                  /* item.un:SCREEN-VALUE IN BROWSE br-itens          = item.un */
                   /*dt-it-docum-est.cod-depos:SCREEN-VALUE IN BROWSE br-itens = item.deposito-pad. */

            ASSIGN dt-it-docum-est.lote        :READ-ONLY IN BROWSE br-itens = NOT(item.tipo-con-est <> 1)
                   dt-it-docum-est.dt-vali-lote:READ-ONLY IN BROWSE br-itens = not(can-do("3,4",string(item.tipo-con-est))).
             IF item.un <> substring(dt-it-docum-est.un,1,2) THEN DO:
                  FIND item-fornec
                       WHERE item-fornec.it-codigo = dt-it-docum-est.item-ems
                         AND item-fornec.cod-emite = dt-it-docum-est.cod-emitente NO-LOCK NO-ERROR.
                  IF AVAILABLE item-fornec 
                  THEN ASSIGN de-indice = item-fornec.fator-conver / EXP(10,item-fornec.num-casa-dec).
                  ELSE DO:
                      ASSIGN de-indice = 1.
/*                       RUN utp/ut-msgs.p (INPUT 'SHOW':U,                                                                                                                                                                                                                                   */
/*                                          INPUT 17006,                                                                                                                                                                                                                                      */
/*                                          INPUT 'Fator de Convers∆o Unidade de Medida' + "~~" + 'N∆o cadastrado Fator de Convers∆o para Unidade de Medida ' + dt-it-docum-est.un + ' para o Item: ' + dt-it-docum-est.item-ems + ' e Fornecedor: ' + STRING(dt-it-docum-est.cod-emitente)). */
/*                       APPLY "entry" TO dt-it-docum-est.item-ems IN BROWSE br-itens.                                                                                                                                                                                                        */
                  END.
                  ASSIGN dt-it-docum-est.dec-1      = dt-it-docum-est.qt-do-forn / de-indice.
                         dt-it-docum-est.quantidade = dt-it-docum-est.dec-1. 
            END.
            ELSE DO:
                ASSIGN dt-it-docum-est.dec-1 = dt-it-docum-est.qt-do-forn
                       dt-it-docum-est.quantidade = dt-it-docum-est.dec-1.
            END.
        END.
        ELSE DO:
            ASSIGN dt-it-docum-est.lote        :READ-ONLY IN BROWSE br-itens = YES
                   dt-it-docum-est.dt-vali-lote:READ-ONLY IN BROWSE br-itens = YES.
        END.
        ASSIGN dt-it-docum-est.item-ems = dt-it-docum-est.item-ems:SCREEN-VALUE IN BROWSE br-itens.
        ASSIGN dt-it-docum-est.nro-comp    :READ-ONLY IN BROWSE br-itens = not(l-nat-terc)       .
               /* dt-it-docum-est.nr-ord-produ:READ-ONLY IN BROWSE br-itens = not(l-nat-terc). */
        br-itens:REFRESH() IN FRAME {&FRAME-NAME}.
    END.
    ON 'any-key':u OF dt-it-docum-est.lote  DO:
        FIND FIRST tt-rat-lote
            WHERE tt-rat-lote.cod-emitente = dt-it-docum-est.cod-emitente
              and tt-rat-lote.serie-docto  = dt-it-docum-est.serie-docto
              and tt-rat-lote.nro-docto    = dt-it-docum-est.nro-docto
              and tt-rat-lote.nat-operacao = dt-it-docum-est.nat-operacao
              and tt-rat-lote.sequencia    = dt-it-docum-est.sequencia
              AND tt-rat-lote.it-codigo    = dt-it-docum-est.item-ems  EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAIL tt-rat-lote THEN DO:
            CREATE tt-rat-lote.
            ASSIGN tt-rat-lote.cod-emitente = dt-it-docum-est.cod-emitente
                   tt-rat-lote.serie-docto  = dt-it-docum-est.serie-docto
                   tt-rat-lote.nro-docto    = dt-it-docum-est.nro-docto
                   tt-rat-lote.nat-operacao = dt-it-docum-est.nat-operacao
                   tt-rat-lote.sequencia    = dt-it-docum-est.sequencia
                   tt-rat-lote.it-codigo    = dt-it-docum-est.item-ems
                   tt-rat-lote.cod-depos    = dt-it-docum-est.cod-depos
                   tt-rat-lote.cod-localiz  = dt-it-docum-est.cod-localiz.
        END.
        ELSE DO:
            ASSIGN tt-rat-lote.cod-depos    = dt-it-docum-est.cod-depos
                   tt-rat-lote.cod-localiz  = dt-it-docum-est.cod-localiz.
        END.
        find first item where item.it-codigo = dt-it-docum-est.item-ems no-lock no-error.
        if item.tipo-con-est <> 1 then do:
            if item.tipo-con-est = 4 then do:
                ASSIGN tt-rat-lote.cod-refer    = dt-it-docum-est.lote:SCREEN-VALUE IN BROWSE br-itens
                       tt-rat-lote.dt-vali-lote = date(dt-it-docum-est.dt-vali-lote:SCREEN-VALUE IN BROWSE br-itens)
                       tt-rat-lote.quantidade   = dt-it-docum-est.dec-1
                       tt-rat-lote.cod-depos    = dt-it-docum-est.cod-depos
                       tt-rat-lote.cod-localiz  = dt-it-docum-est.cod-localiz.
            end.
            else do:
                ASSIGN tt-rat-lote.lote         = dt-it-docum-est.lote:SCREEN-VALUE IN BROWSE br-itens
                       tt-rat-lote.dt-vali-lote = date(dt-it-docum-est.dt-vali-lote:SCREEN-VALUE IN BROWSE br-itens)
                       tt-rat-lote.quantidade   = dt-it-docum-est.dec-1
                       tt-rat-lote.cod-depos    = dt-it-docum-est.cod-depos
                       tt-rat-lote.cod-localiz  = dt-it-docum-est.cod-localiz.
            END.
        end.
    END.
    ON 'any-key':u OF dt-it-docum-est.nr-ord-produ DO:
        ASSIGN dt-it-docum-est.nr-ord-produ =  INT(dt-it-docum-est.nr-ord-produ:SCREEN-VALUE IN BROWSE br-itens).
    END.
    ON 'leave':u OF dt-it-docum-est.nr-ord-produ DO:
        ASSIGN dt-it-docum-est.nr-ord-produ =  INT(dt-it-docum-est.nr-ord-produ:SCREEN-VALUE IN BROWSE br-itens).
    END.
    ON 'leave':u OF dt-it-docum-est.nro-comp  DO:
        ASSIGN  dt-it-docum-est.serie-comp   =  dt-it-docum-est.serie-comp:screen-value in browse br-itens
                dt-it-docum-est.nat-comp     =  dt-it-docum-est.nat-comp  :screen-value in browse br-itens
                dt-it-docum-est.nro-comp     =  dt-it-docum-est.nro-comp  :screen-value in browse br-itens
                dt-it-docum-est.seq-comp     =  int(dt-it-docum-est.seq-comp:screen-value in browse br-itens)
                dt-it-docum-est.nr-ord-produ =  INT(dt-it-docum-est.nr-ord-produ:SCREEN-VALUE IN BROWSE br-itens) WHEN dt-it-docum-est.nr-ord-produ:SCREEN-VALUE IN BROWSE br-itens <> ""
                dt-it-docum-est.item-ems     =  dt-it-docum-est.item-ems  :SCREEN-VALUE IN BROWSE br-itens .
        br-itens:REFRESH() IN FRAME {&FRAME-NAME}.
    END.
    ON 'leave':U OF dt-it-docum-est.nat-operacao 
    DO:
        IF dt-it-docum-est.nat-operacao:screen-value in browse br-itens <> vNatureza THEN DO:
            RUN pi-valida-natop(INPUT dt-it-docum-est.nat-operacao:screen-value in browse br-itens).
            IF RETURN-VALUE = "OK" THEN DO:
                ASSIGN dt-it-docum-est.nat-operaca = dt-it-docum-est.nat-operacao:screen-value in browse br-itens.
                br-itens:REFRESH() IN FRAME {&FRAME-NAME}.
                RUN pi-altera-natureza.
            END.
            ELSE 
                APPLY "entry" TO dt-it-docum-est.nat-operacao IN BROWSE br-itens.
        END.
    END.
    ON 'leave':u OF dt-it-docum-est.numero-ordem  DO:
        FIND FIRST ordem-compra
            WHERE ordem-compra.numero-ordem = INT(dt-it-docum-est.numero-ordem:SCREEN-VALUE IN BROWSE br-itens) NO-LOCK NO-ERROR.
        IF AVAIL ordem-compra THEN DO:
            ASSIGN dt-it-docum-est.item-ems:SCREEN-VALUE IN BROWSE br-itens  = ordem-compra.it-codigo
                   dt-it-docum-est.narrativa:SCREEN-VALUE IN BROWSE br-itens = ordem-compra.narrativa.
/*             if dt-it-docum-est.cod-depos:SCREEN-VALUE IN BROWSE br-itens = "" then                           */
/*                 assign dt-it-docum-est.cod-depos:SCREEN-VALUE IN BROWSE br-itens = ordem-compra.dep-almoxar. */
            FIND FIRST item-uni-estab NO-LOCK
                 WHERE item-uni-estab.it-codigo   = dt-it-docum-est.item-ems
                 AND   item-uni-estab.cod-estabel = dt-docum-est.cod-estabel NO-ERROR.
            IF AVAIL item-uni-estab THEN
               ASSIGN dt-it-docum-est.cod-depos:SCREEN-VALUE IN BROWSE br-itens   = item-uni-estab.deposito-pad
                      dt-it-docum-est.cod-localiz:SCREEN-VALUE IN BROWSE br-itens = item-uni-estab.cod-localiz
                      dt-it-docum-est.cod-depos                                   = dt-it-docum-est.cod-depos:SCREEN-VALUE IN BROWSE br-itens
                      dt-it-docum-est.cod-localiz                                 = dt-it-docum-est.cod-localiz:SCREEN-VALUE IN BROWSE br-itens.

        END.
        ASSIGN dt-it-docum-est.numero-ordem = INT(dt-it-docum-est.numero-ordem:SCREEN-VALUE IN BROWSE br-itens)
               dt-it-docum-est.item-ems     = dt-it-docum-est.item-ems:SCREEN-VALUE IN BROWSE br-itens
/*                dt-it-docum-est.cod-depos    = dt-it-docum-est.cod-depos:SCREEN-VALUE IN BROWSE br-itens */
               dt-it-docum-est.narrativa    = dt-it-docum-est.narrativa:SCREEN-VALUE IN BROWSE br-itens .
        APPLY "leave" TO  dt-it-docum-est.item-ems  IN BROWSE br-itens. 
        br-itens:REFRESH() IN FRAME {&FRAME-NAME}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "dt-docum-est"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "dt-docum-est"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AtualizaNaturOper B-table-Win 
PROCEDURE AtualizaNaturOper :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pNatureza AS CHAR.

ASSIGN /* bt-agregado:SENSITIVE IN FRAME {&FRAME-NAME} =  l-nat-terc  */
       vNatureza = pNatureza.
ASSIGN dt-it-docum-est.nro-comp    :READ-ONLY IN BROWSE br-itens     = not(l-nat-terc)
       /* dt-it-docum-est.nr-ord-produ:READ-ONLY IN BROWSE br-itens     = not(l-nat-terc) */
       bt-rateio-oc                :SENSITIVE IN FRAME {&FRAME-NAME} = NOT(l-nat-terc). 
ASSIGN bt-rateio     :SENSITIVE IN FRAME {&FRAME-NAME} = l-nat-cte
       bt-devol      :SENSITIVE IN FRAME {&FRAME-NAME} = l-nat-devol
       bt-gerar      :SENSITIVE IN FRAME {&FRAME-NAME} = l-nat-devol
       bt-it-agregado:SENSITIVE IN FRAME {&FRAME-NAME} = l-nat-terc
       bt-gera-estrut:SENSITIVE IN FRAME {&FRAME-NAME} = l-nat-terc.
FIND FIRST bDtDocEst 
   WHERE bDtDocEst.nro-docto    = dt-docum-est.nro-docto
     AND bDtDocEst.serie-docto  = dt-docum-est.serie-docto
     AND bDtDocEst.cod-emitente = dt-docum-est.cod-emitente 
     AND bDtDocEst.cod-estabel  =  dt-docum-est.cod-estabel EXCLUSIVE-LOCK NO-ERROR.
IF AVAIL bDtDocEst THEN DO:
    ASSIGN bDtDocEst.nat-operacao = vNatureza.
    RELEASE bDtDocEst.
END.

FOR EACH bDtItDocumEst
   WHERE bDtItDocumEst.nro-docto    = dt-docum-est.nro-docto
     AND bDtItDocumEst.serie-docto  = dt-docum-est.serie-docto
     AND bDtItDocumEst.cod-emitente = dt-docum-est.cod-emitente
     AND bDtItDocumEst.nome-arq     = dt-docum-est.nome-arq   
     AND bDtItDocumEst.cod-estabel  = dt-docum-est.cod-estabel EXCLUSIVE-LOCK:
    
    IF bDtItDocumEst.nat-operacao = "1" OR 
       bDtItDocumEst.nat-operacao = ""  OR 
       (bDtItDocumEst.log-1        = NO AND 
        bDtItDocumEst.nat-operacao = "1" )  THEN 
        ASSIGN bDtItDocumEst.nat-operacao = vNatureza.
END.
IF VALID-HANDLE(vCabec) THEN DO:
    RUN pi-retorna-campo IN vCabec (INPUT 'tipo-rec'   , OUTPUT pTipoCompra).
END.
IF pTipoCompra = "2" THEN DO:
    FOR EACH bDtItDocumEst
       WHERE bDtItDocumEst.nro-docto    = dt-docum-est.nro-docto
         AND bDtItDocumEst.serie-docto  = dt-docum-est.serie-docto
         AND bDtItDocumEst.cod-emitente = dt-docum-est.cod-emitente
         AND bDtItDocumEst.nome-arq     = dt-docum-est.nome-arq   
         AND bDtItDocumEst.cod-estabel  = dt-docum-est.cod-estabel EXCLUSIVE-LOCK:
         ASSIGN bDtItDocumEst.nat-operacao = vNatureza.
    END.

END.

IF l-nat-devol THEN
    ASSIGN dt-it-docum-est.serie-comp:LABEL IN BROWSE br-itens = "Ser Devol" 
           dt-it-docum-est.nro-comp  :LABEL IN BROWSE br-itens = "Doc Devol" 
           dt-it-docum-est.nat-comp  :LABEL IN BROWSE br-itens = "Nat Devol" 
           dt-it-docum-est.seq-comp  :LABEL IN BROWSE br-itens = "Seq Devol" .
ELSE
    ASSIGN dt-it-docum-est.serie-comp:LABEL IN BROWSE br-itens = "Ser Terc" 
           dt-it-docum-est.nro-comp  :LABEL IN BROWSE br-itens = "Doc Terc" 
           dt-it-docum-est.nat-comp  :LABEL IN BROWSE br-itens = "Nat Terc" 
           dt-it-docum-est.seq-comp  :LABEL IN BROWSE br-itens = "Seq Terc" .




br-itens:REFRESH() IN FRAME {&FRAME-NAME}.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-altera-natureza B-table-Win 
PROCEDURE pi-altera-natureza :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH tt-rat-lote 
   WHERE tt-rat-lote.cod-emitente = dt-it-docum-est.cod-emitente
     AND tt-rat-lote.serie-docto  = dt-it-docum-est.serie-docto
     AND tt-rat-lote.nro-docto    = dt-it-docum-est.nro-docto
     /* AND tt-rat-lote.nat-operacao = dt-it-docum-est.nat-operacao */
     AND tt-rat-lote.sequencia    = dt-it-docum-est.sequencia  EXCLUSIVE-LOCK:
    ASSIGN tt-rat-lote.nat-operacao = dt-it-docum-est.nat-operacao.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-busca-lote-nf-origem B-table-Win 
PROCEDURE pi-busca-lote-nf-origem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FIND FIRST bf-estabelec NO-LOCK
        WHERE bf-estabelec.cod-emitente = dt-docum-est.cod-emitente NO-ERROR.

   IF AVAIL bf-estabelec THEN DO:

        FOR FIRST fat-ser-lote FIELDS(nr-serlote
                                      dt-vali-lote) 
            where fat-ser-lote.cod-estabel = bf-estabelec.cod-estabel
              and fat-ser-lote.serie       = dt-it-docum-est.serie-docto
              and fat-ser-lote.nr-nota-fis = dt-it-docum-est.nro-docto
              and fat-ser-lote.nr-seq-fat  > 0 /*dt-it-docum-est.sequencia*/ 
              AND fat-ser-lote.it-codigo   = dt-it-docum-est.it-codigo NO-LOCK.
        END.

        IF AVAIL fat-ser-lote THEN DO.
           ASSIGN dt-it-docum-est.lote         = fat-ser-lote.nr-serlote
                  dt-it-docum-est.dt-vali-lote = fat-ser-lote.dt-vali-lote.
        END.

   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-carrega-dados B-table-Win 
PROCEDURE pi-carrega-dados :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE i-seq           AS INTEGER              NO-UNDO.
/*     ASSIGN br-itens:HIDDEN IN FRAME {&FRAME-NAME} = YES. */
    ASSIGN lHabilitaLote = NO.
    EMPTY TEMP-TABLE tt-rat-docum.
    EMPTY TEMP-TABLE tt-imposto.
    FOR EACH dt-it-docum-est
       WHERE dt-it-docum-est.nro-docto    = dt-docum-est.nro-docto
         AND dt-it-docum-est.serie-docto  = dt-docum-est.serie-docto
         AND dt-it-docum-est.cod-emitente = dt-docum-est.cod-emitente
         AND dt-it-docum-est.nat-operacao = dt-docum-est.nat-operacao
         AND dt-it-docum-est.nome-arq     = dt-docum-est.nome-arq EXCLUSIVE-LOCK:
/*         ASSIGN i-seq = i-seq + 1.                 */
/*         ASSIGN dt-it-docum-est.sequencia = i-seq. */

        FIND FIRST item
             WHERE item.it-codigo = dt-it-docum-est.it-codigo NO-LOCK NO-ERROR.
        IF  AVAIL item 
        THEN ASSIGN dt-it-docum-est.item-ems    = ITEM.it-codigo
                    dt-it-docum-est.cod-depos   = item.deposito-pad
                    dt-it-docum-est.cod-localiz = ITEM.cod-localiz. 

        FIND FIRST item-uni-estab NO-LOCK
             WHERE item-uni-estab.it-codigo   = dt-it-docum-est.item-ems
             AND   item-uni-estab.cod-estabel = dt-docum-est.cod-estabel NO-ERROR.
        IF AVAIL item-uni-estab THEN 
           ASSIGN dt-it-docum-est.cod-depos   = item-uni-estab.deposito-pad
                  dt-it-docum-est.cod-localiz = ITEM-uni-estab.cod-localiz.

        IF AVAIL ITEM AND 
           ITEM.tipo-con-est <> 1 AND
           dt-it-docum-est.lote = "" THEN
           RUN pi-busca-lote-nf-origem.

/*         ELSE ASSIGN dt-it-docum-est.item-ems  = "". */
        RELEASE ITEM.
        
        FIND FIRST item-fornec-estab
            WHERE item-fornec-estab.cod-emitente = dt-it-docum-est.cod-emitente
              AND item-fornec-estab.item-do-forn = dt-it-docum-est.it-codigo
              AND item-fornec-estab.cod-estabel  = dt-docum-est.cod-estabel NO-LOCK NO-ERROR.

        IF AVAIL item-fornec-estab 
        THEN ASSIGN dt-it-docum-est.item-ems = item-fornec-estab.it-codigo.

        IF dt-it-docum-est.item-ems = ""  
        THEN DO:
             FIND FIRST item-fornec
                  WHERE item-fornec.cod-emitente = dt-it-docum-est.cod-emitente
                    AND item-fornec.item-do-forn = dt-it-docum-est.it-codigo NO-LOCK NO-ERROR.
    
             IF AVAIL item-fornec 
             THEN DO:
                  ASSIGN dt-it-docum-est.item-ems     = item-fornec.it-codigo.
                  FIND FIRST ITEM
                       WHERE ITEM.it-codigo = item-fornec.it-codigo NO-LOCK NO-ERROR.
                  IF AVAIL ITEM and dt-it-docum-est.cod-localiz <> ""
                  THEN ASSIGN dt-it-docum-est.cod-localiz = ITEM.cod-localiz.
             END.
        END.
        ELSE DO:
             FIND FIRST ITEM
                 WHERE ITEM.it-codigo = dt-it-docum-est.item-ems NO-LOCK NO-ERROR.
             IF AVAIL ITEM THEN DO:
                ASSIGN /* dt-it-docum-est.cod-localiz                               = ITEM.cod-localiz */
                       dt-it-docum-est.lote        :READ-ONLY IN BROWSE br-itens = NOT(item.tipo-con-est <> 1)
                       dt-it-docum-est.dt-vali-lote:READ-ONLY IN BROWSE br-itens = not(can-do("3,4",string(item.tipo-con-est)))
                       lHabilitaLote                                             = IF lHabilitaLote = NO THEN item.tipo-con-est <> 1 ELSE lHabilitaLote.
                IF item.tipo-con-est <> 1 THEN DO:
                    IF dt-it-docum-est.lote <> "" THEN DO:
                        FIND FIRST tt-rat-lote
                            WHERE tt-rat-lote.cod-emitente = dt-it-docum-est.cod-emitente
                              and tt-rat-lote.serie-docto  = dt-it-docum-est.serie-docto
                              and tt-rat-lote.nro-docto    = dt-it-docum-est.nro-docto
                              and tt-rat-lote.nat-operacao = dt-it-docum-est.nat-operacao
                              and tt-rat-lote.sequencia    = dt-it-docum-est.sequencia    
                              AND tt-rat-lote.it-codigo    = tt-dt-it-docum-est.item-ems  EXCLUSIVE-LOCK NO-ERROR.
                        IF NOT AVAIL tt-rat-lote THEN DO:
                            CREATE tt-rat-lote.
                            ASSIGN tt-rat-lote.cod-emitente = dt-it-docum-est.cod-emitente 
                                   tt-rat-lote.serie-docto  = dt-it-docum-est.serie-docto  
                                   tt-rat-lote.nro-docto    = dt-it-docum-est.nro-docto    
                                   tt-rat-lote.nat-operacao = dt-it-docum-est.nat-operacao 
                                   tt-rat-lote.sequencia    = dt-it-docum-est.sequencia  
                                   tt-rat-lote.it-codigo    = dt-it-docum-est.item-ems
                                   tt-rat-lote.cod-depos    = dt-it-docum-est.cod-depos
                                   tt-rat-lote.cod-localiz  = dt-it-docum-est.cod-localiz.  


                        END.
                        if item.tipo-con-est = 4 then 
                            assign tt-rat-lote.cod-refer         = dt-it-docum-est.lote.
                        else 
                            ASSIGN tt-rat-lote.lote         = dt-it-docum-est.lote       .

                        assign tt-rat-lote.dt-vali-lote = dt-it-docum-est.dt-vali-lote
                               tt-rat-lote.quantidade   = dt-it-docum-est.dec-1
                               tt-rat-lote.cod-depos    = dt-it-docum-est.cod-depos
                               tt-rat-lote.cod-localiz  = dt-it-docum-est.cod-localiz.  

                    END.
                END.
             END.
             ELSE DO:
                 ASSIGN dt-it-docum-est.lote        :READ-ONLY IN BROWSE br-itens = YES
                        dt-it-docum-est.dt-vali-lote:READ-ONLY IN BROWSE br-itens = YES.
             END.



        END.
        IF dt-it-docum-est.cod-depos = "" THEN DO:
            FIND FIRST item-uni-estab NO-LOCK
                 WHERE item-uni-estab.it-codigo   = dt-it-docum-est.item-ems
                 AND   item-uni-estab.cod-estabel = dt-docum-est.cod-estabel NO-ERROR.
            IF AVAIL item-uni-estab THEN 
               ASSIGN dt-it-docum-est.cod-depos   = item-uni-estab.deposito-pad
                      dt-it-docum-est.cod-localiz = ITEM-uni-estab.cod-localiz.

            IF NOT AVAIL item-uni-estab THEN DO.
               FIND FIRST item
                    WHERE item.it-codigo = dt-it-docum-est.item-ems NO-LOCK NO-ERROR.
               IF AVAIL item THEN
                  ASSIGN dt-it-docum-est.cod-depos   = item.deposito-pad
                         dt-it-docum-est.cod-localiz = item.cod-localiz.
            END.

            /*ELSE DO.
               FIND FIRST param-estoq NO-LOCK NO-ERROR.
               IF AVAIL param-estoq 
               THEN ASSIGN dt-it-docum-est.cod-depos = param-estoq.depos-pad.
            END.*/

/*             FIND FIRST param-estoq NO-LOCK NO-ERROR.                               */
/*             IF AVAIL param-estoq                                                   */
/*             THEN ASSIGN dt-it-docum-est.cod-depos = param-estoq.depos-pad.         */
/*                                                                                    */
/*             FIND FIRST item                                                        */
/*                  WHERE item.it-codigo = dt-it-docum-est.item-ems NO-LOCK NO-ERROR. */
/*             IF AVAIL item                                                          */
/*                 THEN ASSIGN dt-it-docum-est.cod-depos   = item.deposito-pad        */
/*                             dt-it-docum-est.cod-localiz = item.cod-localiz.        */
            
        END.
        /* FIFO Ordem de Compra */
/*        FIND FIRST bDtEmpresa WHERE bDtEmpresa.ep-codigo = dt-docum-est.ep-codigo NO-LOCK NO-ERROR.
        IF bDtEmpresa.l-fifo-ordem-compra THEN DO:
            FOR LAST ordem-compra
                WHERE ordem-compra.cod-estabel  = dt-docum-est.cod-estabel
                  AND ordem-compra.it-codigo    = dt-it-docum-est.item-ems
                  AND ordem-compra.situacao     = 2
                  AND ordem-compra.cod-emitente = dt-docum-est.cod-emitente NO-LOCK ,
                FIRST prazo-compra OF ordem-compra
                WHERE ( prazo-compra.quant-saldo - prazo-compra.dec-1 ) >= dt-it-docum-est.quantidade NO-LOCK:
    
                ASSIGN dt-it-docum-est.numero-ordem = ordem-compra.numero-ordem.
            END.
            
        END.
        */
        FIND FIRST ITEM
            WHERE ITEM.it-codigo = dt-it-docum-est.item-ems NO-LOCK NO-ERROR.

        IF AVAIL ITEM THEN DO:
             IF item.un <> dt-it-docum-est.un THEN DO:
/*                  find first item-fornec                                                              */
/*                      where item-fornec.item-do-forn = dt-it-docum-est.it-codigo                      */
/*                        and item-fornec.cod-emitente = dt-it-docum-est.cod-emitente no-lock no-error. */
                  FIND item-fornec
                       WHERE item-fornec.it-codigo = dt-it-docum-est.item-ems
                         AND item-fornec.cod-emite = dt-it-docum-est.cod-emitente NO-LOCK NO-ERROR.
                  IF AVAILABLE item-fornec  THEN  do:

                      ASSIGN de-indice = item-fornec.fator-conver / EXP(10,item-fornec.num-casa-dec).
                  end.
                  ELSE ASSIGN de-indice = 1.
                  ASSIGN dt-it-docum-est.dec-1      = dt-it-docum-est.qt-do-forn / de-indice
                         dt-it-docum-est.quantidade = dt-it-docum-est.dec-1.
/*                   MESSAGE item-fornec.it-codigo skip     */
/*                           de-indice skip                 */
/*                       VIEW-AS ALERT-BOX INFO BUTTONS OK. */
            END.
            ELSE DO:
                ASSIGN dt-it-docum-est.dec-1 = dt-it-docum-est.qt-do-forn.

            END.
        END.


        
    END.
    /* ASSIGN bt-lote:SENSITIVE IN FRAME {&FRAME-NAME} =  lHabilitaLote. */
           /* bt-agregado:SENSITIVE IN FRAME {&FRAME-NAME} =  l-nat-terc. */
    IF VALID-HANDLE(vCabec) THEN DO:
        RUN pi-retorna-campo IN vCabec(INPUT "nat-oper", OUTPUT vNatureza).
        RUN pi-retorna-campo IN vCabec(INPUT 'tipo-rec', OUTPUT cTipoRecebimento).
    END.
    IF cTipoRecebimento = '2' THEN
        ASSIGN bt-gera-estrut:SENSITIVE IN FRAME {&FRAME-NAME} = NO
               bt-it-agregado:SENSITIVE IN FRAME {&FRAME-NAME} = NO
               bt-rateio     :SENSITIVE IN FRAME {&FRAME-NAME} = NO.
        
    {&OPEN-QUERY-br-itens}
    APPLY "value-changed" TO br-itens in frame {&frame-name}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-terceiros B-table-Win 
PROCEDURE pi-terceiros :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH tt-item-terc:
    FOR FIRST dt-it-docum-est
        WHERE dt-it-docum-est.sequencia = tt-item-terc.sequencia:
        ASSIGN dt-it-docum-est.item-ems = tt-item-terc.item-ems.
    END.
END.

{&open-query-br-itens}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-valida B-table-Win 
PROCEDURE pi-valida :
DEFINE OUTPUT PARAM TABLE FOR tt-dt-docum-est.
    DEFINE OUTPUT PARAM TABLE FOR tt-dt-it-docum-est.
    DEFINE OUTPUT PARAM TABLE FOR tt-rat-lote.
    DEFINE OUTPUT PARAM TABLE FOR tt-movto-pend.
    DEFINE OUTPUT PARAM TABLE FOR tt-rat-docum.
    DEFINE OUTPUT PARAM TABLE FOR tt-imposto.
    EMPTY TEMP-TABLE tt-dt-docum-est.
    EMPTY TEMP-TABLE tt-dt-it-docum-est.
    DEFINE VARIABLE deQuantAcum     AS DEC     NO-UNDO.
    DEFINE VARIABLE deSaldoOrdem    AS DEC     NO-UNDO.
    DEFINE VARIABLE iSequencia      AS INTEGER NO-UNDO.
    DEFINE VARIABLE l-pendente      as logical no-undo.
    
    FIND FIRST param-compra NO-LOCK NO-ERROR.
    IF l-nat-terc THEN DO:
        IF NOT CAN-FIND(FIRST bDtItDocumEst 
                        WHERE bDtItDocumEst.serie-docto  = dt-docum-est.serie-docto 
                          and bDtItDocumEst.nro-docto    = dt-docum-est.nro-docto   
                          and bDtItDocumEst.cod-emitente = dt-docum-est.cod-emitente
                          AND bDtItDocumEst.nat-operacao <> dt-docum-est.nat-operacao ) THEN do:
            FIND FIRST dt-it-docum-est 
                 WHERE dt-it-docum-est.serie-docto   = dt-docum-est.serie-docto 
                   AND dt-it-docum-est.nro-docto     = dt-docum-est.nro-docto   
                   AND dt-it-docum-est.cod-emitente  = dt-docum-est.cod-emitente
                   AND dt-it-docum-est.nro-comp      = ''  no-lock no-error.
        end.
        else do:
            FIND FIRST dt-it-docum-est 
                 WHERE dt-it-docum-est.serie-docto   = dt-docum-est.serie-docto 
                   AND dt-it-docum-est.nro-docto     = dt-docum-est.nro-docto   
                   AND dt-it-docum-est.cod-emitente  = dt-docum-est.cod-emitente
                   AND dt-it-docum-est.nro-comp      = ''  
                   and dt-it-docum-est.nat-operacao  = dt-docum-est.nat-operacao no-lock no-error.
        end.
        IF AVAIL dt-it-docum-est THEN DO:
             RUN utp/ut-msgs.p (INPUT 'SHOW':U,
                                INPUT 17006,
                                INPUT 'Retorno de Terceiros' + "~~" + 'Natureza de Operaá∆o de Retorno de Terceiros e n∆o foi informada a nota de sa°da para o item - ' + dt-it-docum-est.item-ems).
             RETURN 'NOK':U.
        END.
        IF NOT CAN-FIND(FIRST bDtItDocumEst 
                        WHERE bDtItDocumEst.serie-docto  = dt-docum-est.serie-docto 
                          and bDtItDocumEst.nro-docto    = dt-docum-est.nro-docto   
                          and bDtItDocumEst.cod-emitente = dt-docum-est.cod-emitente
                          AND bDtItDocumEst.nat-operacao <> dt-docum-est.nat-operacao) then do:
            FIND FIRST dt-it-docum-est 
                 WHERE dt-it-docum-est.serie-docto   = dt-docum-est.serie-docto 
                   AND dt-it-docum-est.nro-docto     = dt-docum-est.nro-docto   
                   AND dt-it-docum-est.cod-emitente  = dt-docum-est.cod-emitente
                   AND dt-it-docum-est.nr-ord-produ  = 0  
                   AND dt-it-docum-est.log-1         = YES no-lock no-error.
        end.
        else do:
            FIND FIRST dt-it-docum-est 
                 WHERE dt-it-docum-est.serie-docto   = dt-docum-est.serie-docto 
                   AND dt-it-docum-est.nro-docto     = dt-docum-est.nro-docto   
                   AND dt-it-docum-est.cod-emitente  = dt-docum-est.cod-emitente
                   AND dt-it-docum-est.nr-ord-produ  = 0  
                   AND dt-it-docum-est.log-1         = YES 
                   and dt-it-docum-est.nat-operacao  = dt-docum-est.nat-operacao no-lock no-error.
        end.
        IF AVAIL dt-it-docum-est THEN DO:
            IF NOT CAN-FIND (FIRST tt-dt-rat-ord-prod 
                             WHERE tt-dt-rat-ord-prod.serie-docto   = dt-docum-est.serie-docto 
                               AND tt-dt-rat-ord-prod.nro-docto     = dt-docum-est.nro-docto   
                               AND tt-dt-rat-ord-prod.cod-emitente  = dt-docum-est.cod-emitente
                               AND tt-dt-rat-ord-prod.nat-operacao  = dt-it-docum-est.nat-operacao
                               AND tt-dt-rat-ord-prod.item-ems      = dt-it-docum-est.item-ems NO-LOCK ) THEN DO:
                RUN utp/ut-msgs.p (INPUT 'SHOW':U,
                                   INPUT 17006,
                                   INPUT 'Retorno de Terceiros' + "~~" + 'Natureza de Operaá∆o de Retorno de Terceiros e n∆o foi informada Ordem de Produá∆o para o item - ' + dt-it-docum-est.item-ems).
            END.
        END.
        RUN pi-valida-var-terc.
        IF RETURN-VALUE <> "OK" THEN
            RETURN 'NOK':U.
        if not can-find(first tt-movto-pend) then do:
            run piGeraMovtoPend.
        end.
    END. /* fim IF l-nat-terc */ 
    IF l-nat-devol THEN DO: /* DEVOLUÄ«O */
        FIND FIRST dt-it-docum-est 
             WHERE dt-it-docum-est.serie-docto   = dt-docum-est.serie-docto 
               AND dt-it-docum-est.nro-docto     = dt-docum-est.nro-docto   
               AND dt-it-docum-est.cod-emitente  = dt-docum-est.cod-emitente
               AND dt-it-docum-est.nat-operacao  = dt-docum-est.nat-operacao
               AND dt-it-docum-est.nro-comp      = ''  NO-LOCK NO-ERROR.
        IF AVAIL dt-it-docum-est THEN DO:
             RUN utp/ut-msgs.p (INPUT 'SHOW':U,
                                INPUT 17006,
                                INPUT 'Devoluá∆o de Mercadoria' + "~~" + 'Natureza de Operaá∆o de Devoluá∆o de Mercadoria e n∆o foi informada a nota de sa°da para o item - ' + dt-it-docum-est.item-ems).
             RETURN 'NOK':U.
        END.
    END. /* fim IF l-nat-devol */
    FOR EACH dt-it-docum-est
       WHERE dt-it-docum-est.serie-docto   = dt-docum-est.serie-docto 
         AND dt-it-docum-est.nro-docto     = dt-docum-est.nro-docto   
         AND dt-it-docum-est.cod-emitente  = dt-docum-est.cod-emitente NO-LOCK:
        IF dt-it-docum-est.nat-operacao <> dt-docum-est.nat-operacao THEN
            RUN pi-valida-natop(INPUT dt-it-docum-est.nat-operacao) .
        IF RETURN-VALUE = 'NOK' THEN
            RETURN 'NOK':U.
        FIND FIRST ITEM
            WHERE item.it-codigo = dt-it-docum-est.item-ems NO-LOCK NO-ERROR.
        IF AVAIL item THEN DO: 
            IF item.tipo-con-est <> 1 THEN DO:
                IF NOT CAN-FIND (FIRST tt-rat-lote 
                                 WHERE tt-rat-lote.cod-emitente = dt-it-docum-est.cod-emitente
                                   AND tt-rat-lote.serie-docto  = dt-it-docum-est.serie-docto
                                   AND tt-rat-lote.nro-docto    = dt-it-docum-est.nro-docto
                                   AND tt-rat-lote.sequencia    = dt-it-docum-est.sequencia  NO-LOCK ) THEN DO:
                    RUN utp/ut-msgs.p (INPUT 'SHOW':U,
                                       INPUT 17006,
                                       INPUT 'Lote' + "~~" + "Item - " + dt-it-docum-est.item-ems + " - possui controle por lote e o mesmo n∆o foi informado.").
                    RETURN 'NOK':U.
                END.
                ELSE DO:
                    FOR EACH tt-rat-lote 
                       WHERE tt-rat-lote.cod-emitente = dt-it-docum-est.cod-emitente
                         AND tt-rat-lote.serie-docto   = dt-it-docum-est.serie-docto
                         AND tt-rat-lote.nro-docto     = dt-it-docum-est.nro-docto
                         /* AND tt-rat-lote.nat-operacao  = dt-it-docum-est.nat-operacao */
                         AND tt-rat-lote.sequencia     = dt-it-docum-est.sequencia  NO-LOCK :
                        ASSIGN deQuantAcum = deQuantAcum + tt-rat-lote.quantidade.
                    END.
                    IF deQuantAcum < dt-it-docum-est.quantidade THEN DO:
                        RUN utp/ut-msgs.p (INPUT 'SHOW':U,
                                           INPUT 17006,
                                           INPUT 'Lote' + "~~" + 'Item - ' + dt-it-docum-est.item-ems + " - possui quantidade menor do que a quantidade recebida. Deve-se preencher a quantidade total recebida com lotes.").
                        RETURN 'NOK':U.
                    END.
                END.
            END.
        END. /* if avail item */ 
        ELSE DO:
            RUN utp/ut-msgs.p (INPUT 'SHOW':U,
                               INPUT 17006,
                               INPUT 'Item ' + "~~" + "Item do EMS - " + dt-it-docum-est.item-ems + " n∆o cadastrado.":U).
            RETURN 'NOK':U.
        END.
        /**** verificando ordem de compra */
        IF dt-it-docum-est.numero-ordem <> 0 THEN DO:
            ASSIGN deSaldoOrdem = 0.
            FIND FIRST ordem-compra
                WHERE ordem-compra.numero-ordem = dt-it-docum-est.numero-ordem NO-LOCK NO-ERROR.
            IF AVAIL ordem-compra THEN DO: 
                if  avail param-compra
                and param-compra.log-1 then do:
                    run cdp/cdapi172.p (input 4, input rowid(ordem-compra), output l-pendente).
                    if not l-pendente then
                        run cdp/cdapi172.p (input 6, input rowid(ordem-compra), output l-pendente).
                    if l-pendente then do:
                        run utp/ut-msgs.p (input "show":U, 
                                           input 17006, 
                                           input "Ordem est† pendente de aprovaá∆o - " + "~~" + string(dt-it-docum-est.numero-ordem) + "O documento n∆o pode ser recebido pois possui pendància de aprovaá∆o.").
                        RETURN 'NOK':U.
                    end.
                end. 
                CASE ordem-compra.situacao:
                    WHEN 1 THEN DO:
                        RUN utp/ut-msgs.p (INPUT 'SHOW':U,
                                           INPUT 17006,
                                           INPUT 'Ordem de Compra' + "~~" + 'Situaá∆o da Ordem de Compra ' + string(dt-it-docum-est.numero-ordem) + ' - do Item - ' + dt-it-docum-est.item-ems + " n∆o permite recebimento - Ordem N∆o Confirmada!"  ).
                        RETURN 'NOK':U.
                    END.
                    WHEN 2 THEN DO:
                        IF  ordem-compra.num-pedido = 0 THEN DO:
                            RUN utp/ut-msgs.p (INPUT 'SHOW':U,
                                               INPUT 5766,
                                               INPUT ' Ordem de Compra do Item - ' + dt-it-docum-est.item-ems + " n∆o possui Pedido de Compra!"  ).
                            RETURN 'NOK':U.
                        END.
                        IF  ordem-compra.it-codigo <> dt-it-docum-est.item-ems THEN DO:
                            RUN utp/ut-msgs.p (INPUT 'SHOW':U,
                                               INPUT 4564,
                                               INPUT 'N∆o' + "~~" + ""  ).
                            RETURN 'NOK':U.
                        END.
                        IF  param-compra.pend-aprov = 2 AND 
                            ordem-compra.pend-aprov = 2 THEN DO:
                            RUN utp/ut-msgs.p (INPUT 'SHOW':U,
                                               INPUT 17006,
                                               INPUT "Ordem de Compra" + string(dt-it-docum-est.numero-ordem) + "~~" + "Pendente de Aprovaá∆o!" ).
                            RETURN 'NOK':U.
                        END.
                        FIND pedido-compr WHERE pedido-compr.num-pedido = ordem-compra.num-pedido NO-LOCK NO-ERROR.
                        IF NOT AVAIL pedido-compr THEN DO:
                            RUN utp/ut-msgs.p (INPUT 'SHOW':U,
                                               INPUT 6363,
                                               INPUT ' Pedido de Compra ' + STRING(ordem-compra.num-pedido) + ' do Item - ' + dt-it-docum-est.item-ems + " n∆o cadastrado!"  ).
                            RETURN 'NOK':U.
                        END.
                        ELSE DO:
                            if  pedido-compr.situacao = 3 THEN DO: /* Pedido de compra eliminado */
                                RUN utp/ut-msgs.p (INPUT 'SHOW':U,
                                                   INPUT 7169,
                                                   INPUT ' Pedido de Compra ' + STRING(ordem-compra.num-pedido) + ' do Item - ' + dt-it-docum-est.item-ems + " n∆o cadastrado!"  ).
                                RETURN 'NOK':U.
                            END.
                        END.
                    END. 
                    WHEN 3 THEN DO:
                        RUN utp/ut-msgs.p (INPUT 'SHOW':U,
                                           INPUT 17006,
                                           INPUT 'Ordem de Compra' + "~~" + 'Situaá∆o da Ordem de Compra do Item - ' + dt-it-docum-est.item-ems + " n∆o permite recebimento - Ordem Cotada!"  ).
                        RETURN 'NOK':U.
                    END.
                    WHEN 4 THEN DO:
                        RUN utp/ut-msgs.p (INPUT 'SHOW':U,
                                           INPUT 17006,
                                           INPUT 'Ordem de Compra' + "~~" + 'Situaá∆o da Ordem de Compra do Item - ' + dt-it-docum-est.item-ems + " n∆o permite recebimento - Ordem Eliminada!"  ).
                        RETURN 'NOK':U.
                    END.
                    WHEN 5 THEN DO:
                        RUN utp/ut-msgs.p (INPUT 'SHOW':U,
                                           INPUT 17006,
                                           INPUT 'Ordem de Compra' + "~~" + 'Situaá∆o da Ordem de Compra do Item - ' + dt-it-docum-est.item-ems + " n∆o permite recebimento - Ordem Em Cotaá∆o!"  ).
                        RETURN 'NOK':U.
                    END.
                    WHEN 6 THEN DO:
                        FOR EACH prazo-compra OF ordem-compra NO-LOCK:
                            ASSIGN deSaldoOrdem = deSaldoOrdem + prazo-compra.quant-saldo.
                        END.
                        IF dt-it-docum-est.quantidade > deSaldoOrdem THEN DO:
                            RUN utp/ut-msgs.p (INPUT 'SHOW':U,
                                               INPUT 17006,
                                               INPUT 'Ordem de Compra' + "~~" + 'Saldo da Ordem de Compra do Item - ' + dt-it-docum-est.item-ems + " menor que a quantidade informada!"  ).
                            RETURN 'NOK':U.
                        END.
                    END.
                END CASE.
            END.
        END. /* fim ordem de compra */
        /* verificando ordem de produá∆o */
        IF dt-it-docum-est.nr-ord-produ <> 0 THEN DO:
            FIND FIRST ord-prod
                WHERE ord-prod.nr-ord-produ = dt-it-docum-est.nr-ord-produ NO-LOCK NO-ERROR.
            IF NOT AVAIL ord-prod THEN DO:
                RUN utp/ut-msgs.p (INPUT 'SHOW':U,
                                   INPUT 17006,
                                   INPUT 'Ordem de Produá∆o' + "~~" + 'Ordem de Produá∆o do Item - ' + dt-it-docum-est.item-ems + " n∆o cadastrada!"  ).
                RETURN 'NOK':U.
            END.
            IF ord-prod.estado = 7 OR ord-prod.estado = 8 THEN DO: 
                RUN utp/ut-msgs.p (INPUT 'SHOW':U,
                                   INPUT 17006,
                                   INPUT 'Ordem de Produá∆o' + "~~" + 'Ordem de Produá∆o do Item - ' + dt-it-docum-est.item-ems + ' Finalizada ou Terminada!'  ).
                RETURN 'NOK':U.
            END.
            IF ord-prod.cod-estabel <> dt-docum-est.cod-estabel THEN DO:
                RUN utp/ut-msgs.p (input 'SHOW':U,
                                   input 15189,
                                   input 'Ordem de Produá∆o do Item - ' + dt-it-docum-est.item-ems + ' n∆o Ç do mesmo estabelecimento do Documento!').
                RETURN 'NOK':U.
            END.
        END.
    END.
    IF CAN-FIND (FIRST dt-it-docum-est 
                 WHERE dt-it-docum-est.serie-docto   = dt-docum-est.serie-docto 
                   AND dt-it-docum-est.nro-docto     = dt-docum-est.nro-docto   
                   AND dt-it-docum-est.cod-emitente  = dt-docum-est.cod-emitente
                   AND dt-it-docum-est.item-ems = ''  NO-LOCK ) THEN DO:
         RUN utp/ut-msgs.p (INPUT 'SHOW':U,
                            INPUT 17006,
                            INPUT 'Item ' + "~~" + 'Item do EMS n∆o Informado.':U).
         RETURN 'NOK':U.
    END.
    /* Modificaá∆o para Precon - N∆o validar ordem de compra para notas
    ** de devoluá∆o e transferància - Abr/2013 */ 
    IF NOT(l-nat-terc)  AND
       NOT(l-nat-devol) AND
       NOT(l-nat-nft)   THEN DO:
        FIND FIRST param-re
            WHERE param-re.usuario = c-seg-usuario NO-LOCK NO-ERROR.
        IF AVAIL param-re AND NOT(param-re.sem-pedido) THEN DO:
            FIND FIRST dt-it-docum-est 
                         WHERE dt-it-docum-est.serie-docto   = dt-docum-est.serie-docto 
                           AND dt-it-docum-est.nro-docto     = dt-docum-est.nro-docto   
                           AND dt-it-docum-est.cod-emitente  = dt-docum-est.cod-emitente
                           AND dt-it-docum-est.numero-ordem  = 0  NO-LOCK NO-ERROR.
             IF AVAIL dt-it-docum-est THEN DO:
                 RUN utp/ut-msgs.p (INPUT 'SHOW':U,
                                    INPUT 17006,
                                    INPUT "Item " + "~~" + "Item " + dt-it-docum-est.item-ems + " n∆o possui Ordem de Compra Informada.":U).
                 RETURN 'NOK':U.
            END.
        END.
    END.
    /* TRATAMENTO DE CLASSIFICAÄ«O FISCAL */
    FOR EACH dt-it-docum-est
       WHERE dt-it-docum-est.serie-docto   = dt-docum-est.serie-docto 
         AND dt-it-docum-est.nro-docto     = dt-docum-est.nro-docto   
         AND dt-it-docum-est.cod-emitente  = dt-docum-est.cod-emitente 
         AND (IF NOT CAN-FIND(FIRST bDtItDocumEst 
                             WHERE bDtItDocumEst.serie-docto  = dt-docum-est.serie-docto 
                               and bDtItDocumEst.nro-docto    = dt-docum-est.nro-docto   
                               and bDtItDocumEst.cod-emitente = dt-docum-est.cod-emitente
                               AND bDtItDocumEst.nat-operacao <> dt-docum-est.nat-operacao ) THEN TRUE ELSE dt-it-docum-est.nat-operacao  = dt-docum-est.nat-operacao) NO-LOCK:
        FIND FIRST ITEM WHERE ITEM.it-codigo = dt-it-docum-est.item-ems NO-LOCK NO-ERROR.
        IF AVAIL ITEM AND ITEM.class-fiscal <> dt-it-docum-est.class-fiscal THEN DO:
/*             RUN piAjustaClassFiscal.     */
/*             IF RETURN-VALUE = 'NOK' THEN */
/*                 RETURN 'NOK':U.          */
        END.
    END.
    CREATE tt-dt-docum-est.
    BUFFER-COPY dt-docum-est TO tt-dt-docum-est.
    FOR EACH dt-it-docum-est 
        WHERE dt-it-docum-est.serie-docto   = dt-docum-est.serie-docto 
          AND dt-it-docum-est.nro-docto     = dt-docum-est.nro-docto   
          AND dt-it-docum-est.cod-emitente  = dt-docum-est.cod-emitente EXCLUSIVE-LOCK:
        FIND FIRST tt-dt-rat-ord-prod
            WHERE  tt-dt-rat-ord-prod.serie-docto   = dt-docum-est.serie-docto
              AND  tt-dt-rat-ord-prod.nro-docto     = dt-docum-est.nro-docto
              AND  tt-dt-rat-ord-prod.cod-emitente  = dt-docum-est.cod-emitente
              AND  tt-dt-rat-ord-prod.nat-operacao  = dt-docum-est.nat-operacao
              AND  tt-dt-rat-ord-prod.item-ems      = dt-it-docum-est.item-ems NO-LOCK NO-ERROR.
        IF NOT AVAIL tt-dt-rat-ord-prod THEN DO:
            CREATE tt-dt-it-docum-est.
            BUFFER-COPY dt-it-docum-est TO tt-dt-it-docum-est.
            IF l-nat-terc AND dt-it-docum-est.nat-operacao <> dt-docum-est.nat-operacao THEN DO:
                find first bNaturOper
                    where bNaturOper.nat-operacao = dt-docum-est.nat-operacao no-lock no-error.
                if not(bNaturOper.imp-nota) then
                    ASSIGN dt-it-docum-est.char-1 = dt-docum-est.serie-docto + "|" + dt-docum-est.nro-docto + "|" + STRING(dt-docum-est.cod-emitente) + "|" + dt-docum-est.nat-operacao + "|".
            END.
        END.
    END.
    FOR EACH tt-dt-rat-ord-prod NO-LOCK:
        CREATE tt-dt-it-docum-est.
        BUFFER-COPY tt-dt-rat-ord-prod TO tt-dt-it-docum-est.
    END.
    IF CAN-FIND (FIRST tt-movto-pend NO-LOCK) THEN DO:
        IF NOT(l-nat-terc) THEN
            EMPTY TEMP-TABLE tt-movto-pend.
    END.
    RETURN 'OK':U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-valida-capa-fisico B-table-Win 
PROCEDURE pi-valida-capa-fisico :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR l-erro          AS LOG     NO-UNDO.
    DEF VAR c-label         AS CHAR    NO-UNDO.
    def var c-cod-estabel   as char    no-undo.
    def var c-cod-emitente  as char    no-undo.
   
    &if defined (bf_mat_bloqueio_fornec) &then
        def var i-situacao     as int  no-undo.  
        def var dt-vig-ini     as date no-undo.  
        def var dt-vig-fim     as date no-undo.  
    &endif
    
    if not can-find (first emitente 
                     where emitente.cod-emitente = dt-docum-est.cod-emitente ) then do:
        run utp/ut-msgs.p (input "show":U, input 2, input return-value).
        /* apply "entry":U to dt-docum-est.cod-emitente in frame {&frame-name}. */
        return 'ADM-ERROR':U.
    end.
    
    if not can-find(first estabelec 
                    where estabelec.cod-estabel = dt-docum-est.cod-estabel) then do:
        run utp/ut-msgs.p (input "show":U, input 2, input return-value).
        /* apply "entry":U to doc-fisico.cod-estabel in frame {&frame-name}. */
        return 'ADM-ERROR':U.
    end.
   
    if not can-find(first serie 
                    where serie.serie = TRIM(dt-docum-est.serie-docto)) then do:
        run utp/ut-msgs.p (input "show":U, input 2, input return-value).
        /* apply "entry":U to doc-fisico.serie-docto in frame {&frame-name}. */
        return 'ADM-ERROR':U.
     END.
    
    if dt-docum-est.cod-emitente = 0 then do:
        run utp/ut-msgs.p (input "show":U, input 6189, input "":U).
        /* apply "entry":U to doc-fisico.cod-emitente in frame {&frame-name}. */
        return 'ADM-ERROR':U.
    end.
    
    if  dt-docum-est.nro-docto = "0000000":U 
    or  dt-docum-est.nro-docto = "":U then do:
        run utp/ut-msgs.p (input "show":U, input 16686, input "":U).
        /* apply "entry":U to i-nro-docto in frame {&frame-name}. */
        return 'ADM-ERROR':U.
    end.
    ASSIGN c-cod-estabel = dt-docum-est.cod-estabel.
    IF  {cdp/cd0066.i2 c-cod-estabel} AND i-tipo-nota = 7 THEN DO:
       RUN utp/ut-msgs.p (INPUT "show":U, INPUT 35097, INPUT "":U).
       
       RETURN 'ADM-ERROR':U.
    END.

    &if defined (bf_mat_bloqueio_fornec) &then
    
        /**Valida a situacao fornecedor qndo nao for (remessa benef ou retorno benef ou dev.consig)**/
        if (i-tipo-nota <> 2 AND    /* Devoluá∆o */
            i-tipo-nota <> 4 and    /* Entrada Benef */
            i-tipo-nota <> 5 and    /* Retorno Benef */
            i-tipo-nota <> 8)       /* Devoluá∆o Consig */
        then do:
            run cdp/cdapi029.p (input c-seg-usuario,
                                input 2,
                                input dt-docum-est.dt-trans ,
                                input dt-docum-est.cod-emitente,
                                output i-situacao,  
                                output dt-vig-ini,  
                                output dt-vig-fim,  
                                output table tt-erro).
            if return-value = "nok":U then do:
                if can-find (first tt-erro) then do:
                    run cdp/cd0669.w (table tt-erro).
                    return 'ADM-ERROR':U.
                end.
            end.
            else if i-situacao = 3 then do:
                run utp/ut-msgs.p (input "show":U, input 27886, input "para Recebimento").
                assign l-grava-consist-nota = yes.
            end.
        end.
    &endif

    if can-find(first b-doc 
                where b-doc.cod-emitente = dt-docum-est.cod-emitente 
                  and b-doc.tipo-nota    = i-tipo-nota                                       
                  and b-doc.serie-docto  = dt-docum-est.serie-docto  
                  and b-doc.nro-docto    = TRIM(string(dt-docum-est.nro-docto,">>>9999999":U)) ) then do:
        run utp/ut-msgs.p (input "show":U, input 7, input return-value).
        return 'ADM-ERROR':U.
    end.
    /*   VALIDAR ISSO -------
    if i-tipo-nota = 3 then do with frame {&FRAME-NAME}: /*entrada de transferencia*/
    
        if  input doc-fisico.estab-de-or = input doc-fisico.cod-estabel then do:
            /**  Estabelecimento Origem deve ser o Estabelecimento Emitente. **/
            {include/i-vldprg.i}
            run utp/ut-msgs.p (input "show":U, input 7298, input "":U).
            apply "entry":U to doc-fisico.estab-de-or in frame {&frame-name}.
            return 'ADM-ERROR':U.
        end.
        
        if not can-find(first estabelec 
                        where estabelec.cod-estabel = input doc-fisico.estab-de-or) then do:
            /**  Estabelecimento de origem nao cadastrado **/
            {include/i-vldprg.i}
            {utp/ut-field.i mgind doc-fisico estab-de-or 01}
            run utp/ut-msgs.p (input "show",input 47,input return-value).
            apply 'entry':U to doc-fisico.estab-de-or in frame {&frame-name} .
            return 'adm-error':U.
        end.

        &IF '{&bf_mat_versao_ems}' >= '2.062' &THEN
            /** Emitente Origem x Estabelecimento Origem **/
            if not can-find(first estabelec 
                            where estabelec.cod-estabel  = input frame {&FRAME-NAME} doc-fisico.estab-de-or 
                            and   estabelec.cod-emitente = input frame {&FRAME-NAME} doc-fisico.cod-emitente) then do:
                assign c-cod-estabel  = doc-fisico.estab-de-or:screen-value in frame {&FRAME-NAME}
                       c-cod-emitente = doc-fisico.cod-emitente:screen-value in frame {&FRAME-NAME}.
                
                {include/i-vldprg.i}
                {utp/ut-liter.i origem *}
                run utp/ut-msgs.p (input "show",
                                   input 34320,
                                   input RETURN-VALUE + "~~" + c-cod-estabel + "~~" + c-cod-emitente).
                apply 'entry':U to doc-fisico.estab-de-or in frame {&frame-name} .
                return 'adm-error':U.
            end.
            else do:
                /** Emitente Destino x Estabelecimento Destino **/
                for first bf-estabelec fields (cod-emitente cod-estabel)
                    where bf-estabelec.cod-estabel = input frame {&FRAME-NAME} doc-fisico.cod-estabel no-lock:
                    for first saldo-terc fields (emite-comp)
                        where saldo-terc.serie-docto   = input doc-fisico.serie-docto 
                          and saldo-terc.nro-docto     = string(input i-nro-docto,"9999999":U) 
                          and saldo-terc.cod-emitente  = input doc-fisico.cod-emitente 
                          and saldo-terc.cod-estabel   = input doc-fisico.estab-de-or
                          and saldo-terc.tipo-sal-terc = 3:
                        if saldo-terc.emite-comp <> bf-estabelec.cod-emitente then do:
                            {include/i-vldprg.i}
                            {utp/ut-liter.i destino *}
                            run utp/ut-msgs.p (input "show",
                                               input 34320,
                                               input RETURN-VALUE + "~~" + bf-estabelec.cod-estabel + "~~" + string(saldo-terc.emite-comp)).
                            apply 'entry':U to doc-fisico.cod-estabel in frame {&frame-name} .
                            return 'adm-error':U.
                        end.
                    end.
                end.
            end.
        &else
            /** Emitente Destino x Estabelecimento Destino **/
            if not can-find(first estabelec 
                            where estabelec.cod-estabel  = input frame {&FRAME-NAME} doc-fisico.cod-estabel 
                            and   estabelec.cod-emitente = input frame {&FRAME-NAME} doc-fisico.cod-emitente) then do:
                assign c-cod-estabel  = doc-fisico.cod-estabel:screen-value in frame {&FRAME-NAME}
                       c-cod-emitente = doc-fisico.cod-emitente:screen-value in frame {&FRAME-NAME}.

                {include/i-vldprg.i}
                {utp/ut-liter.i destino *}
                run utp/ut-msgs.p (input "show",
                                   input 34320,
                                   input RETURN-VALUE + "~~" + c-cod-estabel + "~~" + c-cod-emitente).
                apply 'entry':U to doc-fisico.cod-emitente in frame {&frame-name} .
                return 'adm-error':U.
            end.
        &ENDIF

        if not can-find(first saldo-terc
                        where saldo-terc.serie-docto   = input doc-fisico.serie-docto 
                          and saldo-terc.nro-docto     = string(input i-nro-docto,"9999999":U) 
                          and saldo-terc.cod-emitente  = input doc-fisico.cod-emitente 
                          and saldo-terc.cod-estabel   = input doc-fisico.estab-de-or 
                          and saldo-terc.quantidade    > 0 
                          and saldo-terc.tipo-sal-terc = 3) then do:
            /**  Nao Existe Documento de Saida. Usuario Devera Informar Documento Manualmente. **/
            {include/i-vldprg.i}
            run utp/ut-msgs.p (input "show":U, input 5128, input "":U).
            apply "entry":U to doc-fisico.estab-de-or in frame {&frame-name}.
            return 'ADM-ERROR':U.
        end.
        
    end.
    */
    if dt-docum-est.dt-emissao < (today - param-re.var-emis) then do:
        run utp/ut-msgs.p (input "show":U, input 8824, input return-value).
        return 'ADM-ERROR':U.
    end.
    if dt-docum-est.dt-trans > today then do:
        run utp/ut-msgs.p (input "show":U, input 1788, input "":U).
        return 'ADM-ERROR':U.
    end.
/*     if dt-docum-est.dt-trans  < dt-docum-est.dt-emissao then do:                             */
/*         {utp/ut-field.i xmlloader dt-docum-est dt-emissao 2}                                 */
/*         assign c-label = return-value.                                                       */
/*         run utp/ut-msgs.p (input "show":U, input 89, input return-value + "~~":U + c-label). */
/*         return 'ADM-ERROR':U.                                                                */
/*     end.                                                                                     */


    &IF "{&bf_mat_versao_ems}" >= "2.04" &THEN
        if  param-estoq.tp-fech = 2 then do:
            find estab-mat where 
                 estab-mat.cod-estabel = dt-docum-est.cod-estabel no-lock no-error.
            if  avail estab-mat and 
                dt-docum-est.dt-trans <= estab-mat.mensal-ate then DO:
                run utp/ut-msgs.p (input "show":U, input 1586, input "":U).
                return 'ADM-ERROR':U.

            END.
        end.
        else 
           if  param-estoq.log-1 and dt-docum-est.dt-trans <= param-estoq.mensal-ate then DO:
               run utp/ut-msgs.p (input "show":U, input 1586, input "":U).
               return 'ADM-ERROR':U.
           END.
    &else
       if  param-estoq.log-1 and dt-docum-est.dt-trans <= param-estoq.mensal-ate then DO:
           run utp/ut-msgs.p (input "show":U, input 1586, input "":U).
           return 'ADM-ERROR':U.
       END.
    &endif
    
    if dt-docum-est.dt-trans < (today - param-re.var-atual)  then do:
        run utp/ut-msgs.p (input "show":U, input 8824, input return-value).
        return 'ADM-ERROR':U.
    end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-valida-fisico B-table-Win 
PROCEDURE pi-valida-fisico :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAM TABLE FOR tt-dt-docum-est.
    DEFINE OUTPUT PARAM TABLE FOR tt-dt-it-docum-est.
    DEFINE OUTPUT PARAM TABLE FOR tt-rat-lote.
    DEFINE OUTPUT PARAM TABLE FOR tt-movto-pend.
    DEFINE OUTPUT PARAM TABLE FOR tt-rat-docum.
    DEFINE OUTPUT PARAM TABLE FOR tt-imposto.
    DEFINE VARIABLE deQuantAcum     AS DEC     NO-UNDO.
    DEFINE VARIABLE deSaldoOrdem    AS DEC     NO-UNDO.
    DEFINE VARIABLE iSequencia      AS INTEGER NO-UNDO.
    FIND FIRST param-compra NO-LOCK NO-ERROR.
    FIND param-re WHERE
        param-re.usuario = c-seg-usuario
        NO-LOCK NO-ERROR.
    find first param-estoq  no-lock no-error.
    CREATE tt-dt-docum-est.
    BUFFER-COPY dt-docum-est TO tt-dt-docum-est.
    IF VALID-HANDLE(vCabec) THEN DO:
        RUN pi-retorna-campo IN vCabec (INPUT 'estab-ori-dest', OUTPUT tt-dt-docum-est.estab-de-or).
        RUN pi-retorna-campo IN vCabec (INPUT 'tipo-rec'      , OUTPUT tt-dt-docum-est.tipo-receb).
        RUN pi-retorna-campo IN vCabec (INPUT 'tipo-compra'   , OUTPUT pTipoCompra).

    END.
    ASSIGN tt-dt-docum-est.cod-observa = INT(pTipoCompra).
    FIND FIRST bDtDocEst 
       WHERE bDtDocEst.nro-docto    = dt-docum-est.nro-docto
         AND bDtDocEst.serie-docto  = dt-docum-est.serie-docto
         AND bDtDocEst.cod-emitente = dt-docum-est.cod-emitente
         AND bDtDocEst.cod-estabel  = dt-docum-est.cod-estabel EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL bDtDocEst THEN DO:
        ASSIGN bDtDocEst.cod-observa = INT(pTipoCompra)
               bDtDocEst.estab-de-or = tt-dt-docum-est.estab-de-or
               bDtDocEst.tipo-receb  = tt-dt-docum-est.tipo-receb.
        ASSIGN bDtDocEst.nat-operacao = vNatureza.
        RELEASE bDtDocEst.
    END.

    if valid-handle(vCabec) then
        

    assign i-tipo-nota = int(pTipoCompra).
    run pi-valida-capa-fisico.
    if RETURN-VALUE = 'ADM-ERROR':U then
        return 'NOK':U.
    FOR EACH dt-it-docum-est
       WHERE dt-it-docum-est.serie-docto   = dt-docum-est.serie-docto 
         AND dt-it-docum-est.nro-docto     = dt-docum-est.nro-docto   
         AND dt-it-docum-est.cod-emitente  = dt-docum-est.cod-emitente 
         AND dt-it-docum-est.cod-estabel   = dt-docum-est.cod-estabel NO-LOCK:
        
        FIND FIRST ITEM
            WHERE item.it-codigo = dt-it-docum-est.item-ems NO-LOCK NO-ERROR.
        IF AVAIL item THEN DO: 
            IF item.tipo-con-est <> 1 THEN DO:
                IF NOT CAN-FIND (FIRST tt-rat-lote 
                                 WHERE tt-rat-lote.cod-emitente = dt-it-docum-est.cod-emitente
                                   AND tt-rat-lote.serie-docto  = dt-it-docum-est.serie-docto
                                   AND tt-rat-lote.nro-docto    = dt-it-docum-est.nro-docto
                                   AND tt-rat-lote.sequencia    = dt-it-docum-est.sequencia  NO-LOCK ) THEN DO:
                    RUN utp/ut-msgs.p (INPUT 'SHOW':U,
                                       INPUT 17006,
                                       INPUT 'Lote' + "~~" + "Item - " + dt-it-docum-est.item-ems + " - possui controle por lote e o mesmo n∆o foi informado.").
                    RETURN 'NOK':U.
                END.
                ELSE DO:
                    FOR EACH tt-rat-lote 
                       WHERE tt-rat-lote.cod-emitente = dt-it-docum-est.cod-emitente
                         AND tt-rat-lote.serie-docto   = dt-it-docum-est.serie-docto
                         AND tt-rat-lote.nro-docto     = dt-it-docum-est.nro-docto
                         /* AND tt-rat-lote.nat-operacao  = dt-it-docum-est.nat-operacao */
                         AND tt-rat-lote.sequencia     = dt-it-docum-est.sequencia  NO-LOCK :
                        ASSIGN deQuantAcum = deQuantAcum + tt-rat-lote.quantidade.
                    END.
                    IF deQuantAcum < dt-it-docum-est.quantidade THEN DO:
                        RUN utp/ut-msgs.p (INPUT 'SHOW':U,
                                           INPUT 17006,
                                           INPUT 'Lote' + "~~" + 'Item - ' + dt-it-docum-est.item-ems + " - possui quantidade menor do que a quantidade recebida. Deve-se preencher a quantidade total recebida com lotes.").
                        RETURN 'NOK':U.
                    END.
                END.
            END.
        END. /* if avail item */ 
        ELSE DO:
            RUN utp/ut-msgs.p (INPUT 'SHOW':U,
                               INPUT 17006,
                               INPUT 'Item ' + "~~" + "Item do EMS - " + dt-it-docum-est.item-ems + " n∆o cadastrado.":U).
            RETURN 'NOK':U.
        END.
        /**** verificando ordem de compra */
        IF dt-it-docum-est.numero-ordem <> 0 THEN DO:
            ASSIGN deSaldoOrdem = 0.
            FIND FIRST ordem-compra
                WHERE ordem-compra.numero-ordem = dt-it-docum-est.numero-ordem NO-LOCK NO-ERROR.
            IF AVAIL ordem-compra THEN DO: 
                CASE ordem-compra.situacao:
                    WHEN 1 THEN DO:
                        RUN utp/ut-msgs.p (INPUT 'SHOW':U,
                                           INPUT 17006,
                                           INPUT 'Ordem de Compra' + "~~" + 'Situaá∆o da Ordem de Compra do Item - ' + dt-it-docum-est.item-ems + " n∆o permite recebimento - Ordem N∆o Confirmada!"  ).
                        RETURN 'NOK':U.
                    END.
                    WHEN 2 THEN DO:
                        IF  ordem-compra.num-pedido = 0 THEN DO:
                            RUN utp/ut-msgs.p (INPUT 'SHOW':U,
                                               INPUT 5766,
                                               INPUT ' Ordem de Compra do Item - ' + dt-it-docum-est.item-ems + " n∆o possui Pedido de Compra!"  ).
                            RETURN 'NOK':U.
                        END.
                        IF  ordem-compra.it-codigo <> dt-it-docum-est.item-ems THEN DO:
                            RUN utp/ut-msgs.p (INPUT 'SHOW':U,
                                               INPUT 4564,
                                               INPUT 'N∆o' + "~~" + ""  ).
                            RETURN 'NOK':U.
                        END.
                        IF  param-compra.pend-aprov = 2 AND 
                            ordem-compra.pend-aprov = 2 THEN DO:
                            RUN utp/ut-msgs.p (INPUT 'SHOW':U,
                                               INPUT 8813,
                                               INPUT '' + "~~" + ""  ).
                            RETURN 'NOK':U.
                        END.
                        FIND pedido-compr WHERE pedido-compr.num-pedido = ordem-compra.num-pedido NO-LOCK NO-ERROR.
                        IF NOT AVAIL pedido-compr THEN DO:
                            RUN utp/ut-msgs.p (INPUT 'SHOW':U,
                                               INPUT 6363,
                                               INPUT ' Pedido de Compra ' + STRING(ordem-compra.num-pedido) + ' do Item - ' + dt-it-docum-est.item-ems + " n∆o cadastrado!"  ).
                            RETURN 'NOK':U.
                        END.
                        ELSE DO:
                            if  pedido-compr.situacao = 3 THEN DO: /* Pedido de compra eliminado */
                                RUN utp/ut-msgs.p (INPUT 'SHOW':U,
                                                   INPUT 7169,
                                                   INPUT ' Pedido de Compra ' + STRING(ordem-compra.num-pedido) + ' do Item - ' + dt-it-docum-est.item-ems + " n∆o cadastrado!"  ).
                                RETURN 'NOK':U.
                            END.
                        END.
                    END. 
                    WHEN 3 THEN DO:
                        RUN utp/ut-msgs.p (INPUT 'SHOW':U,
                                           INPUT 17006,
                                           INPUT 'Ordem de Compra' + "~~" + 'Situaá∆o da Ordem de Compra do Item - ' + dt-it-docum-est.item-ems + " n∆o permite recebimento - Ordem Cotada!"  ).
                        RETURN 'NOK':U.
                    END.
                    WHEN 4 THEN DO:
                        RUN utp/ut-msgs.p (INPUT 'SHOW':U,
                                           INPUT 17006,
                                           INPUT 'Ordem de Compra' + "~~" + 'Situaá∆o da Ordem de Compra do Item - ' + dt-it-docum-est.item-ems + " n∆o permite recebimento - Ordem Eliminada!"  ).
                        RETURN 'NOK':U.
                    END.
                    WHEN 5 THEN DO:
                        RUN utp/ut-msgs.p (INPUT 'SHOW':U,
                                           INPUT 17006,
                                           INPUT 'Ordem de Compra' + "~~" + 'Situaá∆o da Ordem de Compra do Item - ' + dt-it-docum-est.item-ems + " n∆o permite recebimento - Ordem Em Cotaá∆o!"  ).
                        RETURN 'NOK':U.
                    END.
                    WHEN 6 THEN DO:
                        FOR EACH prazo-compra OF ordem-compra NO-LOCK:
                            ASSIGN deSaldoOrdem = deSaldoOrdem + prazo-compra.quant-saldo.
                        END.
                        IF dt-it-docum-est.quantidade > deSaldoOrdem THEN DO:
                            RUN utp/ut-msgs.p (INPUT 'SHOW':U,
                                               INPUT 17006,
                                               INPUT 'Ordem de Compra' + "~~" + 'Saldo da Ordem de Compra do Item - ' + dt-it-docum-est.item-ems + " menor que a quantidade informada!"  ).
                            RETURN 'NOK':U.
                        END.
                    END.
                END CASE.
            END.
        END. /* fim ordem de compra */
        /* verificando ordem de produá∆o */
        IF dt-it-docum-est.nr-ord-produ <> 0 THEN DO:
            FIND FIRST ord-prod
                WHERE ord-prod.nr-ord-produ = dt-it-docum-est.nr-ord-produ NO-LOCK NO-ERROR.
            IF NOT AVAIL ord-prod THEN DO:
                RUN utp/ut-msgs.p (INPUT 'SHOW':U,
                                   INPUT 17006,
                                   INPUT 'Ordem de Produá∆o' + "~~" + 'Ordem de Produá∆o do Item - ' + dt-it-docum-est.item-ems + " n∆o cadastrada!"  ).
                RETURN 'NOK':U.
            END.
            IF ord-prod.estado = 7 OR ord-prod.estado = 8 THEN DO: 
                RUN utp/ut-msgs.p (INPUT 'SHOW':U,
                                   INPUT 17006,
                                   INPUT 'Ordem de Produá∆o' + "~~" + 'Ordem de Produá∆o do Item - ' + dt-it-docum-est.item-ems + ' Finalizada ou Terminada!'  ).
                RETURN 'NOK':U.
            END.
            IF ord-prod.cod-estabel <> dt-docum-est.cod-estabel THEN DO:
                RUN utp/ut-msgs.p (input 'SHOW':U,
                                   input 15189,
                                   input 'Ordem de Produá∆o do Item - ' + dt-it-docum-est.item-ems + ' n∆o Ç do mesmo estabelecimento do Documento!').
                RETURN 'NOK':U.
            END.
        END.
    END.
    IF CAN-FIND (FIRST dt-it-docum-est 
                 WHERE dt-it-docum-est.serie-docto   = dt-docum-est.serie-docto 
                   AND dt-it-docum-est.nro-docto     = dt-docum-est.nro-docto   
                   AND dt-it-docum-est.cod-emitente  = dt-docum-est.cod-emitente
                   /* AND dt-it-docum-est.nat-operacao  = dt-docum-est.nat-operacao */
                   AND dt-it-docum-est.item-ems = ''  NO-LOCK ) THEN DO:
         RUN utp/ut-msgs.p (INPUT 'SHOW':U,
                            INPUT 17006,
                            INPUT 'Item ' + "~~" + 'Item do EMS n∆o Informado.':U).
         RETURN 'NOK':U.
    END.
    IF NOT(l-nat-terc) THEN DO:
        FIND FIRST param-re
            WHERE param-re.usuario = c-seg-usuario NO-LOCK NO-ERROR.
        IF AVAIL param-re AND NOT(param-re.sem-pedido) THEN DO:
            FIND FIRST dt-it-docum-est 
                         WHERE dt-it-docum-est.serie-docto   = dt-docum-est.serie-docto 
                           AND dt-it-docum-est.nro-docto     = dt-docum-est.nro-docto   
                           AND dt-it-docum-est.cod-emitente  = dt-docum-est.cod-emitente
                           /* AND dt-it-docum-est.nat-operacao  = dt-docum-est.nat-operacao */
                           AND dt-it-docum-est.numero-ordem  = 0  NO-LOCK NO-ERROR.
             IF AVAIL dt-it-docum-est THEN DO:
                 RUN utp/ut-msgs.p (INPUT 'SHOW':U,
                                    INPUT 17006,
                                    INPUT "Item " + "~~" + "Item " + dt-it-docum-est.item-ems + " n∆o possui Ordem de Compra Informada.":U).
                 RETURN 'NOK':U.
            END.
        END.
    END.
    /* TRATAMENTO DE CLASSIFICAÄ«O FISCAL */
    FOR EACH dt-it-docum-est 
        WHERE dt-it-docum-est.serie-docto   = dt-docum-est.serie-docto 
          AND dt-it-docum-est.nro-docto     = dt-docum-est.nro-docto   
          AND dt-it-docum-est.cod-emitente  = dt-docum-est.cod-emitente EXCLUSIVE-LOCK:
        FIND FIRST tt-dt-rat-ord-prod
            WHERE  tt-dt-rat-ord-prod.serie-docto   = dt-docum-est.serie-docto
              AND  tt-dt-rat-ord-prod.nro-docto     = dt-docum-est.nro-docto
              AND  tt-dt-rat-ord-prod.cod-emitente  = dt-docum-est.cod-emitente
              AND  tt-dt-rat-ord-prod.nat-operacao  = dt-docum-est.nat-operacao
              AND  tt-dt-rat-ord-prod.item-ems      = dt-it-docum-est.item-ems NO-LOCK NO-ERROR.
        IF NOT AVAIL tt-dt-rat-ord-prod THEN DO:
            
            CREATE tt-dt-it-docum-est.
            BUFFER-COPY dt-it-docum-est TO tt-dt-it-docum-est.
            ASSIGN tt-dt-it-docum-est.conta-contabil = substring(param-estoq.char-1,5,17). /* conta transit¢ria de recebimento f°sico RE0103 */
        END.
    END.
    FOR EACH tt-dt-rat-ord-prod NO-LOCK:
        CREATE tt-dt-it-docum-est.
        BUFFER-COPY tt-dt-rat-ord-prod TO tt-dt-it-docum-est.
    END.
    IF CAN-FIND (FIRST tt-movto-pend NO-LOCK) THEN DO:
        IF NOT(l-nat-terc) THEN
            EMPTY TEMP-TABLE tt-movto-pend.
    END.
    RETURN 'OK':U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-valida-natop B-table-Win 
PROCEDURE pi-valida-natop :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER pNaturPesq LIKE dt-it-docum-est.nat-operacao.
    DEFINE VARIABLE l-erro AS LOGICAL NO-UNDO.
    
    FIND FIRST natur-oper
         WHERE natur-oper.nat-operacao = pNaturPesq NO-LOCK NO-ERROR.
    IF AVAIL natur-oper THEN DO:
        IF natur-oper.imp-nota AND NOT(l-nota-imp) and not(l-nat-terc) THEN DO:
            RUN utp/ut-msgs.p (INPUT 'SHOW':U,
                               INPUT 17006,
                               INPUT 'Natureza de Operaá∆o' + "~~" + 'Natureza de Operaá∆o principal n∆o gera nota no faturamento e a informada gera. As naturezas devem ser compat°veis').
            RETURN "NOK":U.
        END.
        IF l-nota-imp AND NOT(natur-oper.imp-nota) and not(l-nat-terc) THEN DO:
            RUN utp/ut-msgs.p (INPUT 'SHOW':U,
                               INPUT 17006,
                               INPUT 'Natureza de Operaá∆o' + "~~" + 'Natureza de Operaá∆o principal gera nota no faturamento e a informada n∆o gera. As naturezas devem ser compat°veis').
            RETURN "NOK":U.

        END.
        FOR FIRST estabelec FIELDS ( estado ) 
            WHERE estabelec.cod-estabel = dt-it-docum-est.cod-estabel NO-LOCK: 
        END.
        FOR FIRST emitente 
            WHERE emitente.cod-emitente = dt-it-docum-est.cod-emitente NO-LOCK: 
        END.
        IF AVAIL estabelec THEN DO:    
             IF  dt-docum-est.uf = estabelec.estado 
                 AND (  natur-oper.nat-operacao BEGINS "1" OR natur-oper.nat-operacao BEGINS "5") 
             THEN ASSIGN l-erro = NO.
             ELSE IF  dt-docum-est.uf <> estabelec.estado
                      AND emitente.natureza = 3 /*"E"*/
                      AND (  natur-oper.nat-operacao BEGINS "3" OR natur-oper.nat-operacao BEGINS "7") 
             THEN ASSIGN l-erro = NO.
             ELSE IF  dt-docum-est.uf <> estabelec.estado
                      AND emitente.natureza <> 3 /*"E"*/
                      AND (  natur-oper.nat-operacao BEGINS "2" OR natur-oper.nat-operacao BEGINS "6") 
             THEN ASSIGN l-erro = NO.
             ELSE IF dt-docum-est.uf <> estabelec.estado
                      AND emitente.natureza = 2 /*"Juridica "*/
                      AND (  natur-oper.nat-operacao BEGINS "3" OR natur-oper.nat-operacao BEGINS "6")
             THEN ASSIGN l-erro = NO.
             IF l-erro 
             THEN DO:
                  RUN utp/ut-msgs.p (INPUT "show":U,
                                     INPUT  19235,
                                     INPUT "").
             END.                            
        END.
        
/*         IF natur-oper.terceiros                                                                                                                                                 */
/*            AND ( natur-oper.tp-oper-terc = 2 OR                                                                                                                                 */
/*                  natur-oper.tp-oper-terc = 4 OR                                                                                                                                 */
/*                  natur-oper.tp-oper-terc = 5 OR                                                                                                                                 */
/*                  natur-oper.tp-oper-terc = 6) THEN DO:                                                                                                                          */
/*             IF NOT(l-nat-terc)  THEN DO:                                                                                                                                        */
/*                 RUN utp/ut-msgs.p (INPUT 'SHOW':U,                                                                                                                              */
/*                                    INPUT 17006,                                                                                                                                 */
/*                                    INPUT 'Natureza de Operaá∆o' + "~~" + 'Natureza de Operaá∆o principal n∆o Ç Terceiros e a informada Ç. As naturezas devem ser compat°veis'). */
/*                 RETURN "NOK":U.                                                                                                                                                 */
/*             END.                                                                                                                                                                */
/*          END.                                                                                                                                                                   */
/*          ELSE DO:                                                                                                                                                               */
/*              IF l-nat-terc THEN DO:                                                                                                                                             */
/*                  RUN utp/ut-msgs.p (INPUT 'SHOW':U,                                                                                                                             */
/*                                     INPUT 17006,                                                                                                                                */
/*                                     INPUT 'Natureza de Operaá∆o' + "~~" + 'Natureza de Operaá∆o principal Ç Terceiros e a informada n∆o. As naturezas devem ser compat°veis').  */
/*                  RETURN "NOK":U.                                                                                                                                                */
/*                                                                                                                                                                                 */
/*              END.                                                                                                                                                               */
/*          END.                                                                                                                                                                   */
         IF natur-oper.nota-rateio THEN DO:
             IF NOT(l-nat-cte) THEN DO:
                 RUN utp/ut-msgs.p (INPUT 'SHOW':U,
                                    INPUT 17006,
                                    INPUT 'Natureza de Operaá∆o' + "~~" + 'Natureza de Operaá∆o principal n∆o Ç Conhecimento de Transporte e a informada Ç. As naturezas devem ser compat°veis').
                 RETURN "NOK":U.
             END.
         END.
         ELSE DO:
             IF l-nat-cte THEN DO:
                 RUN utp/ut-msgs.p (INPUT 'SHOW':U,
                                    INPUT 17006,
                                    INPUT 'Natureza de Operaá∆o' + "~~" + 'Natureza de Operaá∆o principal Ç Conhecimento de Transporte e a informada n∆o. As naturezas devem ser compat°veis').
                 RETURN "NOK":U.
             END.

         END.
         IF natur-oper.tipo-compra = 3 THEN DO:
             IF not(l-nat-devol) THEN DO:
                 RUN utp/ut-msgs.p (INPUT 'SHOW':U,
                                    INPUT 17006,
                                    INPUT 'Natureza de Operaá∆o' + "~~" + 'Natureza de Operaá∆o principal n∆o Ç de Devoluá∆o e a informada Ç. As naturezas devem ser compat°veis').
                 RETURN "NOK":U.
             END.
         END.
         ELSE DO:
             IF l-nat-devol THEN DO:
                 RUN utp/ut-msgs.p (INPUT 'SHOW':U,
                                    INPUT 17006,
                                    INPUT 'Natureza de Operaá∆o' + "~~" + 'Natureza de Operaá∆o principal Ç de Devoluá∆o e a informada n∆o. As naturezas devem ser compat°veis').
                 RETURN "NOK":U.
             END.

         END.
    END.
    ELSE DO: 
        RUN utp/ut-msgs.p (INPUT 'SHOW':U,
                           INPUT 17006,
                           INPUT 'Natureza de Operaá∆o' + "~~" + 'Natureza de Operaá∆o n∆o encontrada.').
        RETURN "NOK":U.

    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-valida-var-terc B-table-Win 
PROCEDURE pi-valida-var-terc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /* verificaá∆o de saldo terceiros */
    DEFINE VARIABLE h-boin404re     AS HANDLE  NO-UNDO.
    DEFINE VARIABLE de-vl-merc-liq  AS DECIMAL format ">>>>,>>>,>>9.99" NO-UNDO.  
    DEFINE VARIABLE de-vl-desc      AS DECIMAL format ">>>>,>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE de-var-max      AS DECIMAL format ">>>>,>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE de-var-min      AS DECIMAL format ">>>>,>>>,>>9.99" NO-UNDO.
    define variable dt-trans-doc    as date no-undo.
    define variable lAchou          as logical initial no.
    DEFINE BUFFER bSaldoTerc FOR saldo-terc.   
        IF VALID-HANDLE(vCabec) THEN
            RUN pi-retorna-campo IN vCabec(INPUT "dt-trans", OUTPUT dt-trans-doc).
    blk_sld_terc:
    FOR EACH dt-it-docum-est
        WHERE dt-it-docum-est.serie-docto   = dt-docum-est.serie-docto 
          AND dt-it-docum-est.nro-docto     = dt-docum-est.nro-docto   
          AND dt-it-docum-est.cod-emitente  = dt-docum-est.cod-emitente
          AND dt-it-docum-est.nro-comp      <> "" no-lock:
        IF CAN-FIND(FIRST bDtItDocumEst                                                                                                                                                     
                    WHERE bDtItDocumEst.serie-docto  = dt-docum-est.serie-docto                                                                              
                      and bDtItDocumEst.nro-docto    = dt-docum-est.nro-docto                                                                                
                      and bDtItDocumEst.cod-emitente = dt-docum-est.cod-emitente                                                                             
                      AND bDtItDocumEst.nat-operacao <> dt-docum-est.nat-operacao ) 
            and dt-it-docum-est.nat-operacao  <> dt-docum-est.nat-operacao then next blk_sld_terc.



/*           AND (IF NOT CAN-FIND(FIRST bDtItDocumEst                                                                                                                      */
/*                               WHERE bDtItDocumEst.serie-docto  = dt-docum-est.serie-docto                                                                               */
/*                                 and bDtItDocumEst.nro-docto    = dt-docum-est.nro-docto                                                                                 */
/*                                 and bDtItDocumEst.cod-emitente = dt-docum-est.cod-emitente                                                                              */
/*                                 AND bDtItDocumEst.nat-operacao <> dt-docum-est.nat-operacao ) THEN TRUE ELSE dt-it-docum-est.nat-operacao  = dt-docum-est.nat-operacao) */

          /* AND dt-it-docum-est.nat-operacao  = dt-docum-est.nat-operacao */
          /* NO-LOCK: */
        assign de-vl-merc-liq = 0
               de-vl-desc     = 0.
        if not(lAchou) then
            assign lAchou = yes.
        FIND FIRST bSaldoTerc
            WHERE bSaldoTerc.cod-emitente  = dt-docum-est.cod-emitente
              AND bSaldoTerc.serie-docto   = dt-it-docum-est.serie-comp 
              AND bSaldoTerc.nro-docto     = dt-it-docum-est.nro-comp   
              AND bSaldoTerc.nat-operacao  = dt-it-docum-est.nat-comp   
              AND bSaldoTerc.sequencia     = dt-it-docum-est.seq-comp   
              AND bSaldoTerc.it-codigo     = dt-it-docum-est.item-ems NO-LOCK NO-ERROR.
        IF NOT AVAIL bSaldoTerc THEN DO:
            RUN utp/ut-msgs.p (INPUT 'SHOW':U,
                               INPUT 17006,
                               INPUT 'Retorno de Terceiros' + "~~" + "Nota de sa°da para o item - " + dt-it-docum-est.item-ems + " -  n∆o encontrada.").
            RETURN 'NOK':U.
        END.
        ELSE DO:
            if  not valid-handle(h-boin404re) then
                run inbo/boin404re.p persistent set h-boin404re.
            run findNaturOper in h-boin404re ( dt-it-docum-est.nat-operacao ).
            find first natur-oper where natur-oper.nat-operacao = dt-it-docum-est.nat-operacao no-lock no-error.
            run findSaldoTerc in h-boin404re ( dt-docum-est.cod-emitente,
                                               dt-it-docum-est.serie-comp,
                                               dt-it-docum-est.nro-comp,
                                               dt-it-docum-est.nat-comp,
                                               dt-it-docum-est.seq-comp,
                                               dt-it-docum-est.item-ems,
                                               "" ).
            if  return-value <> "OK":U then DO:
                RUN utp/ut-msgs.p (INPUT 'SHOW':U,
                                   INPUT 2,
                                   INPUT 'Saldo em Poder de Terceiros' ).
                if valid-handle(h-boin404re) then do:
                   run destroy in h-boin404re.
                   assign h-boin404re = ?.                
                end.
                return "NOK":U.
            END.
            find first natur-oper where natur-oper.nat-operacao = dt-it-docum-est.nat-operacao no-lock no-error.
            run getValuesTerceiros in h-boin404re ( input dt-trans-doc,
                                                    input 1,
                                                    input dt-it-docum-est.dec-1,
                                                    output de-vl-merc-liq,
                                                    output de-vl-desc ).
            assign de-var-max = de-vl-merc-liq 
                              + (de-vl-merc-liq * natur-oper.dec-1 / 100)
                   de-var-min = de-vl-merc-liq 
                              - (de-vl-merc-liq * natur-oper.dec-1 / 100).
/*             if dt-it-docum-est.item-ems = "PC22PR0000" then              */
/*                 MESSAGE "Item:  "  dt-it-docum-est.item-ems skip         */
/*                         "Vlr Dt Item: " dt-it-docum-est.preco-total skip */
/*                         "Mercadoria: " de-vl-merc-liq skip               */
/*                         "Nat Oper:   " natur-oper.dec-1 skip             */
/*                         "Max: " de-var-max skip                          */
/*                         "Min: " de-var-min                               */
/*                     VIEW-AS ALERT-BOX INFO BUTTONS OK.                   */
            assign de-vl-merc-liq = dt-it-docum-est.preco-total .
            if  de-vl-merc-liq > de-var-max 
            or  de-vl-merc-liq < de-var-min then do: 
                RUN utp/ut-msgs.p (INPUT 'SHOW':U,
                                   INPUT 17006,
                                   INPUT 'Retorno de Terceiros' + "~~" + "Variaá∆o para o item - " + dt-it-docum-est.item-ems + " -  fora da variaá∆o permitida." + CHR(10) + 
                                         "Valor Retorno  : R$ " + trim(STRING(dt-it-docum-est.preco-total, ">>>,>>>,>>9.99")) + CHR(10) + 
                                         "Valor Variaá∆o : R$ " + TRIM(STRING(de-var-max, ">>>,>>>,>>9.99")) + " atÇ " + trim(STRING(de-var-min, ">>>,>>>,>>9.99")) ).
                if valid-handle(h-boin404re) then do:
                    run destroy in h-boin404re.
                    assign h-boin404re = ?.
                end.
                RETURN 'NOK':U.
            END.
            if valid-handle(h-boin404re) then do:
                run destroy in h-boin404re.
                assign h-boin404re = ?.
            end.
        END.
    END.
    return "OK".

/*    MESSAGE "aqui 7"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
  */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piAjustaClassFiscal B-table-Win 
PROCEDURE piAjustaClassFiscal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE lChoice         AS LOGICAL INITIAL NO NO-UNDO.
    MESSAGE "Classificaá∆o Fiscal do Fornecedor diferente da Cadastrada para o Item " dt-it-docum-est.item-ems  SKIP
            "Deseja cadastrar a Classificaá∆o Fiscal do Fornecedor para o Item?" 
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lChoice.
    CASE lChoice:
        WHEN YES THEN DO:
            FIND FIRST classif-fisc 
                WHERE classif-fisc.class-fiscal = dt-it-docum-est.class-fiscal NO-LOCK NO-ERROR.
            IF NOT AVAIL classif-fisc THEN DO:
                RUN utp/ut-msgs.p (INPUT 'SHOW':U,
                                   INPUT 17006,
                                   INPUT 'Classificaá∆o Fiscal n∆o Cadastrada' + "~~" + 'Antes de alterar para a Classificaá∆o Fiscal do Fornecedor, a classificaá∆o ' + dt-it-docum-est.class-fiscal
                                         + " deve ser cadastrada no programa CD0603.").
                RETURN "NOK":U.
            END.
            ELSE DO:
                FOR FIRST bItem
                    WHERE bItem.it-codigo = dt-it-docum-est.item-ems EXCLUSIVE-LOCK:
                    ASSIGN bItem.class-fiscal = dt-it-docum-est.class-fiscal.
                END.
                RELEASE bItem.
                RETURN "OK":U.

            END.
        END.
        OTHERWISE RETURN "OK":U.
    END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piAtualizaDadosDev B-table-Win 
PROCEDURE piAtualizaDadosDev :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH tt2-dt-it-docum-est NO-LOCK:
    FIND FIRST dt-it-docum-est
        WHERE ROWID(dt-it-docum-est) = tt2-dt-it-docum-est.pRowid EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL dt-it-docum-est THEN DO:
        ASSIGN dt-it-docum-est.item-ems    = tt2-dt-it-docum-est.item-ems           
               dt-it-docum-est.serie-comp  = tt2-dt-it-docum-est.serie-comp         
               dt-it-docum-est.nro-comp    = tt2-dt-it-docum-est.nro-comp           
               dt-it-docum-est.nat-comp    = tt2-dt-it-docum-est.nat-comp           
               dt-it-docum-est.seq-comp    = tt2-dt-it-docum-est.seq-comp           
               dt-it-docum-est.class-fiscal= tt2-dt-it-docum-est.class-fiscal
               dt-it-docum-est.narrativa   = dt-it-docum-est.narrativa + " Ref. NF de Faturamento: "  +  tt2-dt-it-docum-est.nro-comp  .    
    END.
END.
{&OPEN-QUERY-br-itens}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piGeraMovtoPend B-table-Win 
PROCEDURE piGeraMovtoPend :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
        def var iNumSeq as integer no-undo.
        assign iNumSeq = 10.
        for each dt-it-docum-est 
             WHERE dt-it-docum-est.serie-docto   = dt-docum-est.serie-docto 
               AND dt-it-docum-est.nro-docto     = dt-docum-est.nro-docto   
               AND dt-it-docum-est.cod-emitente  = dt-docum-est.cod-emitente
               AND dt-it-docum-est.log-1         = YES  no-lock:

            CREATE tt-movto-pend.
            ASSIGN tt-movto-pend.serie-docto    = dt-docum-est.serie-docto
                   tt-movto-pend.nro-docto      = dt-docum-est.nro-docto 
                   tt-movto-pend.cod-emitente   = dt-docum-est.cod-emitente 
                   tt-movto-pend.nat-operacao   = dt-it-docum-est.nat-operacao.
            assign tt-movto-pend.nro-comp       = string(dt-docum-est.nro-docto ,"9999999")
                   tt-movto-pend.nat-comp       = dt-it-docum-est.nat-operacao        
                   tt-movto-pend.serie-comp     = dt-docum-est.serie-docto 
                   tt-movto-pend.tipo           = 1
                   tt-movto-pend.nr-ord-produ   = dt-it-docum-est.nr-ord-prod
                   tt-movto-pend.it-codigo      = dt-it-docum-est.ITEM-ems
                   tt-movto-pend.quantidade     = dt-it-docum-est.dec-1
                   tt-movto-pend.sequencia      = iNumSeq.
            find FIRST item-uni-estab
                WHERE item-uni-estab.cod-estabel = dt-docum-est.cod-estabel
                  AND item-uni-estab.it-codigo   = dt-it-docum-est.item-ems NO-LOCK NO-ERROR.
            IF AVAIL item-uni-estab THEN
                ASSIGN tt-movto-pend.cod-depos = item-uni-estab.deposito-pad.
            for first estab-mat
                fields ( cod-estabel-prin   conta-fornec )        
                where estab-mat.cod-estabel = dt-docum-est.cod-estabel no-lock:
            end.

            assign tt-movto-pend.conta-contabil = if  avail estab-mat 
                                                              then estab-mat.conta-fornec
                                                              else "".
            assign iNumSeq = iNumSeq + 10.

        end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piValidaConversao B-table-Win 
PROCEDURE piValidaConversao :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*         FOR EACH dt-it-docum-est                                                                                                                                                                                                                                                             */
/*              WHERE dt-it-docum-est.serie-docto   = dt-docum-est.serie-docto                                                                                                                                                                                                                  */
/*                AND dt-it-docum-est.nro-docto     = dt-docum-est.nro-docto                                                                                                                                                                                                                    */
/*                AND dt-it-docum-est.cod-emitente  = dt-docum-est.cod-emitente                                                                                                                                                                                                                 */
/*                AND IF NOT CAN-FIND(FIRST bDtItDocumEst                                                                                                                                                                                                                                       */
/*                                    WHERE bDtItDocumEst.serie-docto  = dt-docum-est.serie-docto                                                                                                                                                                                               */
/*                                      and bDtItDocumEst.nro-docto    = dt-docum-est.nro-docto                                                                                                                                                                                                 */
/*                                      and bDtItDocumEst.cod-emitente = dt-docum-est.cod-emitente                                                                                                                                                                                              */
/*                                      AND bDtItDocumEst.nat-operacao <> dt-docum-est.nat-operacao ) THEN TRUE ELSE dt-it-docum-est.nat-operacao  = dt-docum-est.nat-operacao NO-LOCK:                                                                                                         */
/*           FIND FIRST ITEM                                                                                                                                                                                                                                                                    */
/*               WHERE item.it-codigo = dt-it-docum-est.item-ems NO-LOCK NO-ERROR.                                                                                                                                                                                                              */
/*           IF AVAIL item THEN DO:                                                                                                                                                                                                                                                             */
/*                IF item.un <> substring(dt-it-docum-est.un,1,2) THEN DO:                                                                                                                                                                                                                      */
/*                     FIND item-fornec                                                                                                                                                                                                                                                         */
/*                          WHERE item-fornec.it-codigo = dt-it-docum-est.item-ems                                                                                                                                                                                                              */
/*                            AND item-fornec.cod-emite = dt-it-docum-est.cod-emitente NO-LOCK NO-ERROR.                                                                                                                                                                                        */
/*                     IF AVAILABLE item-fornec                                                                                                                                                                                                                                                 */
/*                     THEN DO:                                                                                                                                                                                                                                                                 */
/*                         RETURN "OK":U.                                                                                                                                                                                                                                                       */
/*                     END.                                                                                                                                                                                                                                                                     */
/*                     ELSE DO:                                                                                                                                                                                                                                                                 */
/*                         RUN utp/ut-msgs.p (INPUT 'SHOW':U,                                                                                                                                                                                                                                   */
/*                                            INPUT 17006,                                                                                                                                                                                                                                      */
/*                                            INPUT 'Fator de Convers∆o Unidade de Medida' + "~~" + 'N∆o cadastrado Fator de Convers∆o para Unidade de Medida ' + dt-it-docum-est.un + ' para o Item: ' + dt-it-docum-est.item-ems + ' e Fornecedor: ' + STRING(dt-it-docum-est.cod-emitente)). */
/*                         RETURN "NOK":U.                                                                                                                                                                                                                                                      */
/*                     END.                                                                                                                                                                                                                                                                     */
/*               END.                                                                                                                                                                                                                                                                           */
/*               ELSE DO:                                                                                                                                                                                                                                                                       */
/*                   RETURN "OK":U.                                                                                                                                                                                                                                                             */
/*               END.                                                                                                                                                                                                                                                                           */
/*           END.                                                                                                                                                                                                                                                                               */
/*                                                                                                                                                                                                                                                                                              */
/*         END.                                                                                                                                                                                                                                                                                 */
    RETURN "OK":U.

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
  {src/adm/template/snd-list.i "dt-docum-est"}
  {src/adm/template/snd-list.i "dt-it-docum-est"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fcDescr B-table-Win 
FUNCTION fcDescr RETURNS CHARACTER
  ( INPUT pItemEMS AS CHAR /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND FIRST ITEM
      WHERE ITEM.it-codigo = pItemEMS NO-LOCK NO-ERROR.
  IF AVAIL ITEM THEN DO:
      RETURN item.desc-item .
  END.
      
  ELSE RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnPedidoCompr B-table-Win 
FUNCTION fnPedidoCompr RETURNS INTEGER
  ( INPUT pNumOrdem AS INTEGER /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE BUFFER bOrdCompra FOR ordem-compra.
  FIND FIRST bOrdCompra
      WHERE bOrdCompra.numero-ordem = pNumOrdem NO-LOCK NO-ERROR.
  IF AVAIL bOrdCompra THEN
      RETURN bOrdCompra.num-pedido.
  ELSE
    RETURN 0.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

