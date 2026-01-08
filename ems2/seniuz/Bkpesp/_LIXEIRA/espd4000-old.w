&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          ems2med          PROGRESS
*/
&Scoped-define WINDOW-NAME w-window
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-window 
/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESPD4000 2.04.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
DEF BUFFER moeda FOR mgcad.moeda.
DEF BUFFER unid-feder FOR mgcad.unid-feder.

/* Parameters Definitions ---                                           */
DEF INPUT PARAMETER p-acao AS CHAR.

/* Global Variable Definitions ---                                      */
DEF NEW GLOBAL SHARED VAR gr-ped-venda  AS ROWID NO-UNDO. 
DEF NEW GLOBAL SHARED VAR gr-emitente   AS ROWID NO-UNDO. 
DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

/* Temp Tables  Definitions ---         */
DEF TEMP-TABLE tt-itens-ped NO-UNDO LIKE ped-item
    FIELD tp-acao      AS   CHAR
    FIELD qt-reserva   LIKE ped-item.qt-pedida
    FIELD nr-nota-fis  LIKE nota-fiscal.nr-nota-fis
    FIELD dt-emis-nf   AS   DATE FORMAT "99/99/9999" 
    FIELD dt-saida-nf  LIKE nota-fiscal.dt-saida
    FIELD bloqueio-fat AS LOG 
    FIELD motivo       AS CHAR
    FIELD cod-estabel  LIKE ped-venda.cod-estabel
    FIELD vl-pre-calc  LIKE ped-item.vl-pretab
    INDEX indice-1 nr-sequencia 
    INDEX ch-item-ped IS PRIMARY UNIQUE nome-abrev nr-pedcli nr-sequencia it-codigo cod-refer.

DEF TEMP-TABLE tt-ped-venda NO-UNDO LIKE ped-venda 
     FIELD r-rowid AS ROWID.

DEF TEMP-TABLE tt-ped-item NO-UNDO LIKE ped-item
     FIELD r-rowid AS ROWID.

DEF TEMP-TABLE tt-ped-repre NO-UNDO LIKE ped-repre
    FIELD r-rowid AS ROWID.
DEF TEMP-TABLE wt-ped-repre  NO-UNDO LIKE ped-repre
    FIELD r-rowid AS ROWID.

DEF TEMP-TABLE tt-cond-ped NO-UNDO LIKE cond-ped
    FIELD r-rowid AS ROWID.
DEF TEMP-TABLE wt-cond-ped NO-UNDO LIKE cond-ped
    FIELD r-rowid AS ROWID.

DEFINE TEMP-TABLE tt-ref-item
       FIELD cod-refer   LIKE saldo-estoq.cod-refer
       FIELD qtidade-atu LIKE saldo-estoq.qtidade-atu
       FIELD preco-un    LIKE preco-item.preco-venda
       FIELD qt-pedida   LIKE ped-item.qt-pedida
       FIELD vl-tot-ref  AS   DEC FORMAT ">>>,>>>,>>9.9999"
       FIELD cod-depos   LIKE saldo-estoq.cod-depos
       INDEX indice1 cod-refer.

DEFINE TEMP-TABLE RowErrors NO-UNDO
       FIELD ErrorSequence    AS INTEGER
       FIELD ErrorNumber      AS INTEGER
       FIELD ErrorDescription AS CHARACTER
       FIELD ErrorParameters  AS CHARACTER
       FIELD ErrorType        AS CHARACTER
       FIELD ErrorHelp        AS CHARACTER
       FIELD ErrorSubType     AS CHARACTER.

DEF TEMP-TABLE tt-estoque
    FIELD it-codigo      LIKE ob-etiqueta.it-codigo
    FIELD cod-refer      LIKE ob-etiqueta.cod-refer
    FIELD corte-comerc   LIKE ob-etiqueta.corte-comerc LABEL "Corte Comercial"
    FIELD qt-estoque     LIKE ob-etiqueta.quantidade   LABEL "Estoque"
    FIELD qt-res-antc    LIKE ob-etiqueta.quantidade   LABEL "Res.Antec."
    FIELD qt-ped-reserva LIKE ob-etiqueta.quantidade   LABEL "Ped.Reserva"
    FIELD qt-trf         AS   DEC FORMAT "->>>,>>9.99" LABEL "Transform"
    FIELD qt-benefic     LIKE ob-etiqueta.quantidade   LABEL "Benefic"
    FIELD qt-carteira    LIKE ped-item.qt-pedida       LABEL "Carteira"
    FIELD qt-res-cart    LIKE ob-etiqueta.quantidade   LABEL "Cart.Forn."
    FIELD qt-saldo       LIKE saldo-estoq.qtidade-atu  LABEL "SALDO"
    FIELD visualiza      AS   LOG INIT NO
    INDEX indice1 IS PRIMARY it-codigo cod-refer corte-comerc.

/* Buffer Definitions ---                                       */
DEF BUFFER b-itens-ped    FOR tt-itens-ped.
DEF BUFFER b-ped-venda    FOR ped-venda.
DEF BUFFER b-ped-item     FOR ped-item.
DEF BUFFER b-ped-item-res FOR ped-item-res.
DEF BUFFER b-emitente     FOR emitente.

/* Local Variable Definitions ---                               */
DEF VAR r-row-peditem      AS ROWID.

DEF VAR h-bodi018          AS HANDLE.
DEF VAR h-bodi157          AS HANDLE.
                           
DEF VAR h-menu             AS HANDLE.
DEF VAR h-menu-item        AS HANDLE.

DEF VAR h-essp0150         AS HANDLE.

DEF VAR c-obsoleto         AS CHAR.
DEF VAR c-situacao         AS CHAR.
DEF VAR c-desc-dentro      AS CHAR.
                           
DEF VAR c-motivo           AS CHAR FORMAT "x(60)".
DEF VAR da-dt-trans        AS DATE.
DEF VAR c-finalidade       AS CHAR.
DEF VAR c-tipo-trans       AS CHAR.
                           
DEF VAR i-nr-seq-div       AS INT.
DEF VAR c-corte-div        LIKE ped-item-ext.corte-comerc.
DEF VAR de-qtd-div         LIKE ped-item.qt-pedida.
DEF VAR c-novo-corte       LIKE ped-item-ext.corte-comerc.
DEF VAR l-ok               AS LOG.
DEF VAR c-desc-bloq-cr     LIKE ped-venda.desc-bloq-cr.
DEF VAR l-desaprovar-cred  AS LOG.
DEF VAR i-cod-sit-ped      LIKE ped-venda.cod-sit-ped.                           
DEF VAR c-texto-msg        AS CHAR.
DEF VAR c-pedidos          AS CHAR.
DEF VAR i-tp-embal         AS INT.
DEF VAR c-tipos-ped        AS CHAR.
DEF VAR c-tab-preco        AS CHAR.
DEF VAR c-tb-preco-pad     AS CHAR.
DEF VAR de-ind-finan       AS DEC.
DEF VAR de-tot-prazo       LIKE cond-ped.nr-dias-venc.
DEF VAR i-prazo-medio      AS INTEGER.
DEF VAR de-qt-media        AS DEC.
DEF VAR de-qtidade-atu     LIKE saldo-estoq.qtidade-atu.
DEF VAR c-dia              AS CHAR.
DEF VAR i-ult-seq          AS INT.
DEF VAR i-resto            AS INT.
DEF VAR l-criou-pedido     AS LOG.
DEF VAR l-tem-acesso       AS LOG.
DEF VAR i-ct               AS INT.
DEF VAR c-results          AS CHAR.
DEF VAR de-tot-perc        AS DEC.
DEF VAR c-lst-prioridade   AS CHAR INIT "10,18".
DEF VAR c-lst-preposto     AS CHAR.
DEF VAR c-clas-prod        AS CHAR EXTENT 5 
                           INIT ["0 - Lancamento","1 - Fora de Producao","2 - Em Produá∆o","3 - Retalho","4 - Exclusividade"].
DEF VAR c-tpped-cred-aut   AS CHAR 
                           INIT "Exportaá∆o,Amostra,Amostra Exportaá∆o,Bonificaá∆o,Doaá∆o".
DEF VAR c-texto-log        AS   CHAR FORMAT "x(100)".
DEF VAR c-nr-pedcli        LIKE ped-item.nr-pedcli.
DEF VAR l-todos-itens      AS LOG INIT YES.
DEF VAR l-copia-reservas   AS LOG.
DEF VAR l-aberto           AS LOG.
DEF VAR l-atendido-parcial AS LOG.
DEF VAR l-atendido-total   AS LOG.
DEF VAR l-pendente         AS LOG.
DEF VAR l-suspenso         AS LOG.
DEF VAR l-cancelado        AS LOG.
DEF VAR l-fat-balcao       AS LOG.
DEF VAR c-it-container     AS CHAR.
DEF VAR c-it-codigo-ini    AS CHAR.                              
DEF VAR c-it-codigo-fin    AS CHAR.                              
DEF VAR c-cod-refer-ini    AS CHAR.                              
DEF VAR c-cod-refer-fin    AS CHAR.
DEF VAR l-copia-observ     AS LOG.
DEF VAR l-preco-alterado   AS LOG.
DEF VAR l-incluiu-item     AS LOG.
DEF VAR l-nat-oper         AS LOG.

DEF VAR h-acomp            AS HANDLE NO-UNDO.

DEF VAR h-objeto AS HANDLE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE JanelaDetalhe
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-ped-item

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-itens-ped ITEM

/* Definitions for BROWSE br-ped-item                                   */
&Scoped-define FIELDS-IN-QUERY-br-ped-item tt-itens-ped.nr-sequencia tt-itens-ped.it-codigo item.desc-item tt-itens-ped.cod-refer tt-itens-ped.qt-pedida tt-itens-ped.dec-2 tt-itens-ped.qt-log-aloca tt-itens-ped.vl-preori tt-itens-ped.vl-pre-calc fn-situacao() @ c-situacao tt-itens-ped.qt-reserva tt-itens-ped.nr-nota-fis tt-itens-ped.dt-emis-nf tt-itens-ped.dt-saida   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-ped-item tt-itens-ped.it-codigo   tt-itens-ped.cod-refer   tt-itens-ped.qt-pedida   tt-itens-ped.vl-preori   
&Scoped-define ENABLED-TABLES-IN-QUERY-br-ped-item tt-itens-ped
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br-ped-item tt-itens-ped
&Scoped-define SELF-NAME br-ped-item
&Scoped-define QUERY-STRING-br-ped-item FOR EACH tt-itens-ped WHERE                                  tt-itens-ped.tp-acao <> 'eliminar', ~
                                   FIRST ITEM OF tt-itens-ped                             BY tt-itens-ped.nr-sequencia
&Scoped-define OPEN-QUERY-br-ped-item OPEN QUERY {&SELF-NAME} FOR EACH tt-itens-ped WHERE                                  tt-itens-ped.tp-acao <> 'eliminar', ~
                                   FIRST ITEM OF tt-itens-ped                             BY tt-itens-ped.nr-sequencia.
&Scoped-define TABLES-IN-QUERY-br-ped-item tt-itens-ped ITEM
&Scoped-define FIRST-TABLE-IN-QUERY-br-ped-item tt-itens-ped
&Scoped-define SECOND-TABLE-IN-QUERY-br-ped-item ITEM


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-br-ped-item}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-ped-item cb-prioridade cb-tp-pedido ~
fi-cod-estabel fi-cliente fi-pedido-cli fi-fat-max fi-cod-rep fi-ped-repres ~
fi-natur-oper fi-perc-comis fi-cod-cond-pag fi-nr-pedido cb-tab-preco ~
cb-tp-entrega fi-nome-transp fi-nome-tr-red bt-log fi-cod-rota cb-tp-frete ~
fi-data-base ed-obs fi-observ-nf bt-inc bt-sair bt-cons-cliente-tri RECT-1 ~
RECT-2 RECT-3 RECT-4 
&Scoped-Define DISPLAYED-OBJECTS cb-prioridade tg-nao-aprovar ~
fi-nome-estabel fi-dt-implant cb-tp-pedido fi-cod-estabel cb-ext-tp-pedido ~
fi-reserva fi-cliente fi-pedido-cli fi-cliente-tri fi-sit-cred fi-fat-max ~
fi-cod-rep fi-nome-rep fi-ped-repres cb-preposto fi-natur-oper ~
fi-denominacao fi-perc-comis fi-moeda fi-desc-moeda fi-cod-cond-pag ~
fi-nr-pedido fi-desc-cond-pag cb-tp-preco cb-tab-preco cb-tp-entrega ~
fi-dt-entrega cb-tipo-pagto fi-nome-transp fi-nome-tr-red fi-cod-rota ~
fi-desc-rota cb-tp-frete fi-data-base fi-nr-container ed-obs tg-bloqueio ~
fi-observ-nf fi-tot-qtd-ped fi-tot-qtd-res fi-tot-qtd-fat fi-tot-vlr-ped ~
fi-tot-vlr-fat fi-tot-vlr-abe fi-completo fi-sit-preco fi-cidade-cli ~
fi-cidade-cli-tri fi-uf fi-uf-tri 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-4 fi-pedido-cli fi-fat-max fi-cod-rep fi-ped-repres ~
fi-natur-oper fi-perc-comis fi-moeda fi-cod-cond-pag cb-tp-entrega ~
fi-nome-transp fi-nome-tr-red fi-cod-rota cb-tp-frete ed-obs bt-msg ~
bt-copia-it-ped fi-observ-nf bt-inc bt-can 
&Scoped-define List-5 cb-tp-pedido fi-cod-estabel cb-ext-tp-pedido ~
fi-cliente 
&Scoped-define List-6 tg-nao-aprovar cb-tipo-pagto fi-data-base 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-obsoleto w-window 
FUNCTION fn-obsoleto RETURNS CHARACTER
  ( INPUT p-it-codigo AS CHAR, INPUT p-cod-refer AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-situacao w-window 
FUNCTION fn-situacao RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-window AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-bloq AUTO-GO 
     IMAGE-UP FILE "image/im-bloq.bmp":U
     IMAGE-INSENSITIVE FILE "image/im-bloqi.bmp":U
     LABEL "" 
     SIZE 4.86 BY 1.17 TOOLTIP "Bloqueia o Faturamento do Item"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-can AUTO-GO 
     IMAGE-UP FILE "image/im-cance.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-cance.bmp":U
     LABEL "" 
     SIZE 4.86 BY 1.17 TOOLTIP "Cancelar o Item do Pedido"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-cond-esp 
     IMAGE-UP FILE "image/im-open.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-open.bmp":U
     LABEL "Button 2" 
     SIZE 4 BY 1 TOOLTIP "Condiá‰es Especiais de Pagto".

DEFINE BUTTON bt-cons-cliente-tri AUTO-GO 
     IMAGE-UP FILE "image/im-det.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Detalha Cliente Triangular"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-copia-it-ped 
     IMAGE-UP FILE "image/im-cop2.bmp":U
     LABEL "Button 1" 
     SIZE 4.86 BY 1.17 TOOLTIP "Copia Itens de um Pedido".

DEFINE BUTTON bt-del 
     IMAGE-UP FILE "image/im-era.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-era.bmp":U
     LABEL "bt inclui 2" 
     SIZE 4.86 BY 1.13 TOOLTIP "Elimina Item do Pedido".

DEFINE BUTTON bt-imp-res 
     IMAGE-UP FILE "image/im-plin.bmp":U
     LABEL "Button 2" 
     SIZE 4.86 BY 1.17 TOOLTIP "Importa Itens Reservados".

DEFINE BUTTON bt-inc 
     IMAGE-UP FILE "image/im-add.bmp":U
     LABEL "Button 3" 
     SIZE 4.86 BY 1.17 TOOLTIP "Inclui Novo Item".

DEFINE BUTTON bt-log AUTO-GO 
     IMAGE-UP FILE "image/im-log.bmp":U
     LABEL "" 
     SIZE 4.29 BY 1.75 TOOLTIP "Alteraá‰es do Pedido"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-mod 
     IMAGE-UP FILE "image/im-mod.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-mod.bmp":U
     LABEL "bt inclui 2" 
     SIZE 4.86 BY 1.17 TOOLTIP "Modifica Item".

DEFINE BUTTON bt-msg AUTO-GO 
     IMAGE-UP FILE "image/gr-zoo.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Escolhe Mensagem"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-ok AUTO-GO 
     IMAGE-UP FILE "image/im-sav.bmp":U
     LABEL "OK" 
     SIZE 4.86 BY 1.17 TOOLTIP "Salva Alteraá‰es".

DEFINE BUTTON bt-sair 
     IMAGE-UP FILE "image/im-exi.bmp":U
     LABEL "Cancelar" 
     SIZE 4.86 BY 1.17 TOOLTIP "Cancela e Sai".

DEFINE VARIABLE cb-ext-tp-pedido AS CHARACTER FORMAT "X(256)":U INITIAL "PE" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "PE","PI" 
     DROP-DOWN-LIST
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE cb-preposto AS CHARACTER FORMAT "X(256)":U 
     LABEL "Preposto" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE cb-prioridade AS CHARACTER 
     LABEL "Prioridade" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "10" 
     DROP-DOWN
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE cb-tab-preco AS CHARACTER 
     LABEL "Tabela Preáo" 
     VIEW-AS COMBO-BOX INNER-LINES 7
     DROP-DOWN AUTO-COMPLETION UNIQUE-MATCH
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE cb-tipo-pagto AS CHARACTER FORMAT "X(256)":U INITIAL "Normal" 
     LABEL "Tipo Pagto" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Normal","Caixa ","Vendor" 
     DROP-DOWN-LIST
     SIZE 16.72 BY 1 NO-UNDO.

DEFINE VARIABLE cb-tp-entrega AS CHARACTER FORMAT "X(256)":U INITIAL "A Partir da Data" 
     LABEL "Tipo Entrega" 
     VIEW-AS COMBO-BOX INNER-LINES 6
     LIST-ITEMS "1¶ Quinzena","2¶ Quinzena","No Màs","Na Data","A Partir da Data","AtÇ a Data","Imediata" 
     DROP-DOWN-LIST
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE cb-tp-frete AS CHARACTER FORMAT "X(256)":U 
     LABEL "Tipo Frete" 
     VIEW-AS COMBO-BOX INNER-LINES 6
     LIST-ITEMS "","Cif Total","Cif atÇ Redesp","Cif Destaque NF","Fob Total","Fob atÇ Redesp" 
     DROP-DOWN-LIST
     SIZE 14.72 BY 1 NO-UNDO.

DEFINE VARIABLE cb-tp-pedido AS CHARACTER FORMAT "X(256)":U INITIAL "Normal" 
     LABEL "Tipo Pedido" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEMS "Normal","Produá∆o","Exportaá∆o","Reserva","Amostra","∑ Vista","Operaá∆o Triangular","Bonificaá∆o","Doaá∆o","Bancado","Refaturamento","Amostra Exportaá∆o","Rem.Industrializacao","Venda Confec." 
     DROP-DOWN-LIST
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE cb-tp-preco AS CHARACTER FORMAT "X(256)":U INITIAL "1" 
     LABEL "Tipo Preáo" 
     VIEW-AS COMBO-BOX INNER-LINES 3
     LIST-ITEM-PAIRS "Informado","1",
                     "Dia da Implantaá∆o","2",
                     "Dia do Faturamento","3"
     DROP-DOWN-LIST
     SIZE 16.86 BY 1 NO-UNDO.

DEFINE VARIABLE ed-obs AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 59 BY 2 NO-UNDO.

DEFINE VARIABLE fi-cidade-cli AS CHARACTER FORMAT "X(256)":U 
     LABEL "Cidade" 
     VIEW-AS FILL-IN 
     SIZE 23 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cidade-cli-tri AS CHARACTER FORMAT "X(256)":U 
     LABEL "Cidade" 
     VIEW-AS FILL-IN 
     SIZE 23 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cliente AS CHARACTER FORMAT "x(12)" 
     LABEL "Cliente":R9 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cliente-tri AS CHARACTER FORMAT "x(12)" 
     LABEL "Cli Rem Tri" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-cond-pag AS INTEGER FORMAT ">>9" INITIAL 0 
     LABEL "Cond. Pagto" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-estabel AS CHARACTER FORMAT "X(256)":U 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-informado AS CHARACTER FORMAT "X(4)":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 TOOLTIP "C¢digo Informado" NO-UNDO.

DEFINE VARIABLE fi-cod-rep AS CHARACTER FORMAT "X(12)" 
     LABEL "Representante" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-rota AS CHARACTER FORMAT "X(256)":U 
     LABEL "Rota" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-completo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sit Com." 
     VIEW-AS FILL-IN 
     SIZE 14.14 BY .88
     BGCOLOR 2 FGCOLOR 15 FONT 12 NO-UNDO.

DEFINE VARIABLE fi-data-base AS DATE FORMAT "99/99/9999":U 
     LABEL "Dt Base Fat." 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE fi-denominacao AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 36.43 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-cond-pag AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 27.43 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-moeda AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 10.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-rota AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 12.29 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-entrega AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-implant AS DATE FORMAT "99/99/9999" 
     LABEL "Dt Implantaá∆o" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fat-max AS DECIMAL FORMAT ">>>,>>9.99":U INITIAL 0 
     LABEL "Vlr Max por NF" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-moeda AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Moeda" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-natur-oper AS CHARACTER FORMAT "X(256)":U 
     LABEL "Natur Oper" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nome-estabel AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 44.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nome-rep AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 30.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nome-tr-red AS CHARACTER FORMAT "X(256)":U 
     LABEL "Redesp." 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nome-transp AS CHARACTER FORMAT "X(256)":U 
     LABEL "Transportador" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nr-container AS INTEGER FORMAT ">>>,>>9" INITIAL 0 
     LABEL "Container" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88.

DEFINE VARIABLE fi-nr-pedido AS INTEGER FORMAT ">>>>>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.38
     BGCOLOR 2 FGCOLOR 15 FONT 20 NO-UNDO.

DEFINE VARIABLE fi-observ-nf AS CHARACTER FORMAT "X(200)":U 
     LABEL "Observ. NF" 
     VIEW-AS FILL-IN 
     SIZE 55 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ped-repres AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ped. Repres." 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-pedido-cli AS CHARACTER FORMAT "X(256)":U INITIAL "0" 
     LABEL "Ped. Cliente" 
     VIEW-AS FILL-IN 
     SIZE 9.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-perc-comis AS DECIMAL FORMAT ">>9.99":U INITIAL 0 
     LABEL "%Comis" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE VARIABLE fi-reserva AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Reserva" 
     VIEW-AS FILL-IN 
     SIZE 9.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-sit-cred AS CHARACTER FORMAT "X(256)":U 
     LABEL "Cred." 
     VIEW-AS FILL-IN 
     SIZE 10.72 BY .79
     BGCOLOR 2 FGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE fi-sit-preco AS CHARACTER FORMAT "X(256)":U 
     LABEL "Preáo" 
     VIEW-AS FILL-IN 
     SIZE 10.72 BY .79
     BGCOLOR 2 FGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE fi-tot-qtd-fat AS DECIMAL FORMAT ">>>,>>9.99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE fi-tot-qtd-ped AS DECIMAL FORMAT ">>>,>>9.99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE fi-tot-qtd-res AS DECIMAL FORMAT ">>>,>>9.99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-tot-vlr-abe AS DECIMAL FORMAT "->,>>>,>>9.99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88
     BGCOLOR 8 FGCOLOR 2 FONT 6 NO-UNDO.

DEFINE VARIABLE fi-tot-vlr-fat AS DECIMAL FORMAT ">,>>>,>>9.99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88
     BGCOLOR 8 FGCOLOR 12  NO-UNDO.

DEFINE VARIABLE fi-tot-vlr-ped AS DECIMAL FORMAT ">,>>>,>>9.99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88
     BGCOLOR 8 FGCOLOR 9 FONT 6 NO-UNDO.

DEFINE VARIABLE fi-uf AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-uf-tri AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 94 BY 1.83
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 7 BY 11.17
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 102 BY 8.67.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 102 BY 4.

DEFINE VARIABLE tg-bloqueio AS LOGICAL INITIAL no 
     LABEL "Bloq. Fat." 
     VIEW-AS TOGGLE-BOX
     SIZE 10 BY .54 NO-UNDO.

DEFINE VARIABLE tg-nao-aprovar AS LOGICAL INITIAL no 
     LABEL "N∆o Aprovar" 
     VIEW-AS TOGGLE-BOX
     SIZE 12 BY .58 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-ped-item FOR 
      tt-itens-ped, 
      ITEM SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-ped-item
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-ped-item w-window _FREEFORM
  QUERY br-ped-item DISPLAY
      tt-itens-ped.nr-sequencia WIDTH 4
 tt-itens-ped.it-codigo     FORMAT "x(8)"  WIDTH 8
 item.desc-item             FORMAT "x(25)" WIDTH 25
 tt-itens-ped.cod-refer     FORMAT "x(7)"  WIDTH 5
 tt-itens-ped.qt-pedida     FORMAT ">>>,>>9.99" WIDTH 8
 tt-itens-ped.dec-2         FORMAT ">>>,>>9.99" COLUMN-LABEL "Qt Estoque" WIDTH 8
 tt-itens-ped.qt-log-aloca  FORMAT ">>>,>>9.99" COLUMN-LABEL "Qt Alocada" WIDTH 8
 tt-itens-ped.vl-preori     FORMAT ">,>>9.99"   COLUMN-LABEL "Preáo"      WIDTH 7
 tt-itens-ped.vl-pre-calc   FORMAT ">,>>9.99"   COLUMN-LABEL "Preáo TAB"  WIDTH 8
 fn-situacao() @ c-situacao FORMAT "x(4)"       COLUMN-LABEL "Sit" WIDTH 4
 tt-itens-ped.qt-reserva    FORMAT ">>>,>>9.99" WIDTH 8 COLUMN-LABEL "Qt Reserva"
 tt-itens-ped.nr-nota-fis   COLUMN-LABEL "Nota Fiscal"
 tt-itens-ped.dt-emis-nf    COLUMN-LABEL "Dt Emis NF"
 tt-itens-ped.dt-saida      COLUMN-LABEL "Dt Saida NF"
 ENABLE 
     tt-itens-ped.it-codigo
     tt-itens-ped.cod-refer
     tt-itens-ped.qt-pedida
     tt-itens-ped.vl-preori
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS DROP-TARGET SIZE 94 BY 9.33
         FONT 1
         TITLE "Itens do Pedido".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br-ped-item AT ROW 13.92 COL 2
     cb-prioridade AT ROW 8.38 COL 86.14 COLON-ALIGNED WIDGET-ID 56
     tg-nao-aprovar AT ROW 13.04 COL 88 WIDGET-ID 42
     fi-nome-estabel AT ROW 1.17 COL 34.29 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     fi-dt-implant AT ROW 2.13 COL 29 COLON-ALIGNED
     cb-tp-pedido AT ROW 2.13 COL 52 COLON-ALIGNED
     fi-cod-estabel AT ROW 1.17 COL 29 COLON-ALIGNED WIDGET-ID 6 NO-TAB-STOP 
     cb-ext-tp-pedido AT ROW 2.13 COL 73.14 COLON-ALIGNED NO-LABEL WIDGET-ID 52
     fi-reserva AT ROW 2.13 COL 91.14 COLON-ALIGNED
     fi-cliente AT ROW 3.08 COL 29 COLON-ALIGNED
     fi-pedido-cli AT ROW 3.08 COL 91.29 COLON-ALIGNED
     fi-cliente-tri AT ROW 4.04 COL 29 COLON-ALIGNED
     fi-sit-cred AT ROW 4.08 COL 2.86
     fi-fat-max AT ROW 4.04 COL 91.14 COLON-ALIGNED
     fi-cod-rep AT ROW 5.38 COL 12 COLON-ALIGNED HELP
          "C¢digo do representante direto"
     fi-nome-rep AT ROW 5.38 COL 24.29 COLON-ALIGNED NO-LABEL
     fi-ped-repres AT ROW 5.33 COL 66 COLON-ALIGNED
     cb-preposto AT ROW 5.33 COL 85.86 COLON-ALIGNED WIDGET-ID 54
     fi-natur-oper AT ROW 6.38 COL 12 COLON-ALIGNED
     fi-denominacao AT ROW 6.38 COL 21.57 COLON-ALIGNED NO-LABEL
     fi-perc-comis AT ROW 6.33 COL 65.86 COLON-ALIGNED WIDGET-ID 16
     fi-moeda AT ROW 6.38 COL 86 COLON-ALIGNED
     fi-desc-moeda AT ROW 6.38 COL 90.43 COLON-ALIGNED NO-LABEL
     fi-cod-cond-pag AT ROW 7.38 COL 12 COLON-ALIGNED HELP
          "C¢digo da condiá∆o de pagamento"
     bt-cond-esp AT ROW 7.33 COL 19.14
     fi-nr-pedido AT ROW 1.71 COL 1.14 COLON-ALIGNED HELP
          "C¢digo do cliente" NO-LABEL NO-TAB-STOP 
     fi-desc-cond-pag AT ROW 7.38 COL 21.57 COLON-ALIGNED NO-LABEL
     cb-tp-preco AT ROW 7.38 COL 58.14 COLON-ALIGNED WIDGET-ID 14
     cb-tab-preco AT ROW 7.38 COL 86 COLON-ALIGNED WIDGET-ID 24
     cb-tp-entrega AT ROW 8.38 COL 12 COLON-ALIGNED
     fi-dt-entrega AT ROW 8.38 COL 27.43 COLON-ALIGNED NO-LABEL
     cb-tipo-pagto AT ROW 8.38 COL 58.29 COLON-ALIGNED
     fi-cod-informado AT ROW 8.38 COL 95 COLON-ALIGNED NO-LABEL WIDGET-ID 22
     fi-nome-transp AT ROW 9.38 COL 12 COLON-ALIGNED
     fi-nome-tr-red AT ROW 9.38 COL 36.14 COLON-ALIGNED
     bt-log AT ROW 3.17 COL 18.29 NO-TAB-STOP 
     fi-cod-rota AT ROW 9.38 COL 58.29 COLON-ALIGNED WIDGET-ID 2
     fi-desc-rota AT ROW 9.38 COL 62.72 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     cb-tp-frete AT ROW 9.38 COL 86 COLON-ALIGNED
     fi-data-base AT ROW 10.38 COL 86 COLON-ALIGNED
     fi-nr-container AT ROW 11.33 COL 86 COLON-ALIGNED WIDGET-ID 46
     ed-obs AT ROW 10.38 COL 14 NO-LABEL
     bt-msg AT ROW 12.58 COL 69.29
     tg-bloqueio AT ROW 12.38 COL 88
     bt-imp-res AT ROW 14.08 COL 98.14
     bt-copia-it-ped AT ROW 15.25 COL 98.14
     fi-observ-nf AT ROW 12.63 COL 12 COLON-ALIGNED NO-TAB-STOP 
     bt-inc AT ROW 16.63 COL 98.14
     bt-mod AT ROW 17.79 COL 98.14
     bt-can AT ROW 20.17 COL 98.14
     bt-bloq AT ROW 21.29 COL 98.14 WIDGET-ID 10
     bt-ok AT ROW 22.63 COL 98.14
     bt-sair AT ROW 23.79 COL 98.14
     fi-tot-qtd-ped AT ROW 24.08 COL 2.43 COLON-ALIGNED HELP
          "C¢digo do cliente" NO-LABEL
     fi-tot-qtd-res AT ROW 24.08 COL 17.57 COLON-ALIGNED HELP
          "C¢digo do cliente" NO-LABEL
     fi-tot-qtd-fat AT ROW 24.08 COL 33.43 COLON-ALIGNED HELP
          "C¢digo do cliente" NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 103.14 BY 24.33
         FONT 1.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     fi-tot-vlr-ped AT ROW 24.08 COL 48.57 COLON-ALIGNED HELP
          "C¢digo do cliente" NO-LABEL
     fi-tot-vlr-fat AT ROW 24.08 COL 64.43 COLON-ALIGNED HELP
          "C¢digo do cliente" NO-LABEL
     fi-tot-vlr-abe AT ROW 24.08 COL 80 COLON-ALIGNED HELP
          "C¢digo do cliente" NO-LABEL
     bt-del AT ROW 18.96 COL 98.29 WIDGET-ID 28
     fi-completo AT ROW 1.17 COL 82.57 WIDGET-ID 38
     fi-sit-preco AT ROW 3.21 COL 2.43 WIDGET-ID 40
     bt-cons-cliente-tri AT ROW 3.96 COL 43.57 NO-TAB-STOP 
     fi-cidade-cli AT ROW 3.08 COL 52 COLON-ALIGNED WIDGET-ID 30 NO-TAB-STOP 
     fi-cidade-cli-tri AT ROW 4.04 COL 52 COLON-ALIGNED WIDGET-ID 32 NO-TAB-STOP 
     fi-uf AT ROW 3.08 COL 75.14 COLON-ALIGNED NO-LABEL WIDGET-ID 34 NO-TAB-STOP 
     fi-uf-tri AT ROW 4.04 COL 75.14 COLON-ALIGNED NO-LABEL WIDGET-ID 36 NO-TAB-STOP 
     "Observaá∆o:" VIEW-AS TEXT
          SIZE 9 BY .54 AT ROW 10.33 COL 4.72
     "Qt Tot Faturada" VIEW-AS TEXT
          SIZE 11 BY .54 AT ROW 23.46 COL 35.43
          BGCOLOR 8 
     "Vl Tot Desconto" VIEW-AS TEXT
          SIZE 12.57 BY .54 AT ROW 23.5 COL 66.43
          BGCOLOR 8 
     "Vl Tot Liquido" VIEW-AS TEXT
          SIZE 11 BY .54 AT ROW 23.5 COL 82
          BGCOLOR 8 
     "Nß Pedido" VIEW-AS TEXT
          SIZE 7 BY .54 AT ROW 1.13 COL 3.29
     "Qt Total Pedido" VIEW-AS TEXT
          SIZE 11 BY .54 AT ROW 23.46 COL 4.43
          BGCOLOR 8 
     "Vl Total Pedido" VIEW-AS TEXT
          SIZE 11 BY .54 AT ROW 23.5 COL 50.57
          BGCOLOR 8 
     "Qt Tot Reservada" VIEW-AS TEXT
          SIZE 14 BY .54 AT ROW 23.46 COL 19.57
          BGCOLOR 8 
     RECT-1 AT ROW 23.29 COL 2
     RECT-2 AT ROW 13.92 COL 97
     RECT-3 AT ROW 5.08 COL 2
     RECT-4 AT ROW 1 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 103.14 BY 24.33
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: JanelaDetalhe
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-window ASSIGN
         HIDDEN             = YES
         TITLE              = "Implantaá∆o de Pedidos de Venda"
         COLUMN             = 17.14
         ROW                = 5.04
         HEIGHT             = 24.33
         WIDTH              = 103.14
         MAX-HEIGHT         = 29
         MAX-WIDTH          = 182.86
         VIRTUAL-HEIGHT     = 29
         VIRTUAL-WIDTH      = 182.86
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-window 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-window.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-window
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB br-ped-item 1 F-Main */
/* SETTINGS FOR BUTTON bt-bloq IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-can IN FRAME F-Main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR BUTTON bt-cond-esp IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-copia-it-ped IN FRAME F-Main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR BUTTON bt-del IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-imp-res IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-inc IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR BUTTON bt-mod IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-msg IN FRAME F-Main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR BUTTON bt-ok IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX cb-ext-tp-pedido IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR COMBO-BOX cb-preposto IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX cb-tipo-pagto IN FRAME F-Main
   NO-ENABLE 6                                                          */
/* SETTINGS FOR COMBO-BOX cb-tp-entrega IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR COMBO-BOX cb-tp-frete IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR COMBO-BOX cb-tp-pedido IN FRAME F-Main
   5                                                                    */
/* SETTINGS FOR COMBO-BOX cb-tp-preco IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR ed-obs IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR FILL-IN fi-cidade-cli IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-cidade-cli-tri IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-cliente IN FRAME F-Main
   5                                                                    */
/* SETTINGS FOR FILL-IN fi-cliente-tri IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-cod-cond-pag IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR FILL-IN fi-cod-estabel IN FRAME F-Main
   5                                                                    */
/* SETTINGS FOR FILL-IN fi-cod-informado IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       fi-cod-informado:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN fi-cod-rep IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR FILL-IN fi-cod-rota IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR FILL-IN fi-completo IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN fi-data-base IN FRAME F-Main
   6                                                                    */
/* SETTINGS FOR FILL-IN fi-denominacao IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-cond-pag IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-moeda IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-rota IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-dt-entrega IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-dt-implant IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-fat-max IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR FILL-IN fi-moeda IN FRAME F-Main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-natur-oper IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR FILL-IN fi-nome-estabel IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nome-rep IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nome-tr-red IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR FILL-IN fi-nome-transp IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR FILL-IN fi-nr-container IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-observ-nf IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR FILL-IN fi-ped-repres IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR FILL-IN fi-pedido-cli IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR FILL-IN fi-perc-comis IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR FILL-IN fi-reserva IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-sit-cred IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN fi-sit-preco IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN fi-tot-qtd-fat IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-qtd-ped IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-qtd-res IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-vlr-abe IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-vlr-fat IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-vlr-ped IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-uf IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-uf-tri IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-bloqueio IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-nao-aprovar IN FRAME F-Main
   NO-ENABLE 6                                                          */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-window)
THEN w-window:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-ped-item
/* Query rebuild information for BROWSE br-ped-item
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-itens-ped WHERE
                                 tt-itens-ped.tp-acao <> 'eliminar',
                            FIRST ITEM OF tt-itens-ped
                            BY tt-itens-ped.nr-sequencia.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-ped-item */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-window
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON END-ERROR OF w-window /* Implantaá∆o de Pedidos de Venda */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON WINDOW-CLOSE OF w-window /* Implantaá∆o de Pedidos de Venda */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  FIND CURRENT ped-venda NO-LOCK NO-ERROR.
  FIND CURRENT ped-item NO-LOCK NO-ERROR.

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN c-pedidos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-ped-item
&Scoped-define SELF-NAME br-ped-item
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-ped-item w-window
ON END-ERROR OF br-ped-item IN FRAME F-Main /* Itens do Pedido */
ANYWHERE 
DO:
   IF br-ped-item:NEW-ROW IN FRAME {&FRAME-NAME} THEN DO:
      IF AVAIL tt-itens-ped THEN
         DELETE tt-itens-ped.
      IF br-ped-item:DELETE-CURRENT-ROW() IN FRAME {&FRAME-NAME} THEN. 
   END.                                                               

   {&OPEN-QUERY-br-ped-item}

   tt-itens-ped.it-codigo:READ-ONLY IN BROWSE br-ped-item = YES.
   tt-itens-ped.cod-refer:READ-ONLY IN BROWSE br-ped-item = YES.
   tt-itens-ped.qt-pedida:READ-ONLY IN BROWSE br-ped-item = YES.
   tt-itens-ped.vl-preori:READ-ONLY IN BROWSE br-ped-item = YES.

   ASSIGN bt-inc:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
   IF p-acao = 'Incluir' AND 
      fi-reserva:INPUT-VALUE <> 0 THEN 
      ASSIGN bt-inc:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

   APPLY 'VALUE-CHANGED' TO br-ped-item.
   APPLY 'entry' TO bt-inc.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-ped-item w-window
ON LEAVE OF br-ped-item IN FRAME F-Main /* Itens do Pedido */
DO:
   IF p-acao <> 'Consultar' THEN DO.
      /*
      FIND FIRST b-itens-ped WHERE
                 b-itens-ped.tp-acao <> '' NO-ERROR.
      IF AVAIL b-itens-ped THEN */
         ASSIGN bt-ok:SENSITIVE = YES.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-ped-item w-window
ON OFF-END OF br-ped-item IN FRAME F-Main /* Itens do Pedido */
DO:
   APPLY 'entry':U TO bt-inc IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-ped-item w-window
ON RETURN OF br-ped-item IN FRAME F-Main /* Itens do Pedido */
ANYWHERE
DO:
   APPLY 'tab':U TO SELF.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-ped-item w-window
ON ROW-DISPLAY OF br-ped-item IN FRAME F-Main /* Itens do Pedido */
DO:
    tt-itens-ped.nr-sequencia:FGCOLOR IN BROWSE br-ped-item = ?.
    tt-itens-ped.it-codigo:FGCOLOR IN BROWSE br-ped-item = ?.
    item.desc-item:FGCOLOR IN BROWSE br-ped-item = ?.
    tt-itens-ped.cod-refer:FGCOLOR IN BROWSE br-ped-item = ?.
    tt-itens-ped.qt-pedida:FGCOLOR IN BROWSE br-ped-item = ?.
    tt-itens-ped.vl-preori:FGCOLOR IN BROWSE br-ped-item = ?.
    tt-itens-ped.qt-reserva:FGCOLOR IN BROWSE br-ped-item = ?.
    c-situacao:FGCOLOR IN BROWSE br-ped-item = ?.
    tt-itens-ped.nr-nota-fis:FGCOLOR IN BROWSE br-ped-item = ?.
    tt-itens-ped.dt-emis-nf:FGCOLOR IN BROWSE br-ped-item = ?.
    tt-itens-ped.dt-saida:FGCOLOR IN BROWSE br-ped-item = ?.
    
    IF tt-itens-ped.cod-sit-item = 6 OR
       tt-itens-ped.bloqueio-fat THEN DO.
       tt-itens-ped.nr-sequencia:FGCOLOR IN BROWSE br-ped-item = 12.
       tt-itens-ped.it-codigo:FGCOLOR IN BROWSE br-ped-item = 12.
       item.desc-item:FGCOLOR IN BROWSE br-ped-item = 12.
       tt-itens-ped.cod-refer:FGCOLOR IN BROWSE br-ped-item = 12.
       tt-itens-ped.qt-pedida:FGCOLOR IN BROWSE br-ped-item = 12.
       tt-itens-ped.vl-preori:FGCOLOR IN BROWSE br-ped-item = 12.
       tt-itens-ped.qt-reserva:FGCOLOR IN BROWSE br-ped-item = 12.
       c-situacao:FGCOLOR IN BROWSE br-ped-item = 12.
       tt-itens-ped.nr-nota-fis:FGCOLOR IN BROWSE br-ped-item = 12.
       tt-itens-ped.dt-emis-nf:FGCOLOR IN BROWSE br-ped-item = 12.
       tt-itens-ped.dt-saida:FGCOLOR IN BROWSE br-ped-item = 12.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-ped-item w-window
ON ROW-ENTRY OF br-ped-item IN FRAME F-Main /* Itens do Pedido */
DO:
  IF AVAIL tt-itens-ped AND tt-itens-ped.cod-sit-item <> 1 THEN DO.
     APPLY 'entry' TO bt-mod.
     RETURN NO-APPLY.
  END.

  IF br-ped-item:NEW-ROW IN FRAME {&FRAME-NAME} THEN DO.
     ASSIGN tt-itens-ped.nr-sequencia:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(i-ult-seq).

     FIND LAST b-itens-ped USE-INDEX indice-1 NO-LOCK NO-ERROR.
     IF AVAIL b-itens-ped THEN DO.
        ASSIGN tt-itens-ped.it-codigo:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(b-itens-ped.it-codigo)
               tt-itens-ped.qt-pedida:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(b-itens-ped.qt-pedida)
               tt-itens-ped.vl-preori:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(b-itens-ped.vl-preori).

        FIND ITEM WHERE
             ITEM.it-codigo = b-itens-ped.it-codigo NO-LOCK NO-ERROR.
        IF AVAIL ITEM THEN
           DISP ITEM.desc-item WITH BROWSE {&browse-name}.
     END.
  END.
  /*
  IF AVAIL tt-itens-ped AND
     tt-itens-ped.vl-preori = 0 THEN DO.
     FIND LAST b-itens-ped WHERE
               b-itens-ped.nr-sequencia <  tt-itens-ped.nr-sequencia AND
               b-itens-ped.it-codigo = tt-itens-ped.it-codigo
               NO-LOCK NO-ERROR.
     IF AVAIL b-itens-ped THEN
        ASSIGN tt-itens-ped.vl-preori:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(b-itens-ped.vl-preori).
  END.
  */

  ASSIGN bt-del:SENSITIVE IN FRAME {&FRAME-NAME} = NO
         bt-inc:SENSITIVE IN FRAME {&FRAME-NAME} = NO 
         bt-mod:SENSITIVE IN FRAME {&FRAME-NAME} = NO
         bt-can:SENSITIVE IN FRAME {&FRAME-NAME} = NO
         bt-bloq:SENSITIVE IN FRAME {&FRAME-NAME} = NO
         bt-ok:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-ped-item w-window
ON ROW-LEAVE OF br-ped-item IN FRAME F-Main /* Itens do Pedido */
DO:
   IF LOOKUP(KEYFUNCTION(LAST-KEY),"TAB,RETURN") > 0 THEN DO. 
      IF p-acao = 'Modificar' AND 
         NOT br-ped-item:NEW-ROW IN FRAME {&FRAME-NAME} THEN DO.

        FIND ped-item-res WHERE
             ped-item-res.cod-estabel = ped-venda.cod-estabel AND  
             ped-item-res.nome-abrev = ped-venda.nome-abrev AND   
             ped-item-res.nr-pedcli = ped-venda.nr-pedcli AND    
             ped-item-res.nr-sequencia = tt-itens-ped.nr-sequencia 
             NO-LOCK NO-ERROR. 
         IF AVAIL ped-item-res AND
            ped-item-res.qt-pedida > 0 AND
            INPUT BROWSE br-ped-item tt-itens-ped.qt-pedida <> tt-itens-ped.qt-pedida THEN DO.

            MESSAGE "J† Existem Etiquetas Reservadas para essa Sequància," SKIP
                    "Modificaá∆o s¢ ser† permitida se Cancelar a Reserva." SKIP
                    "Deseja Cancelar a Reserva ?"
                    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                    TITLE "" UPDATE l-opcao AS LOGICAL.
            IF l-opcao = NO THEN DO.
               ASSIGN tt-itens-ped.qt-pedida:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(tt-itens-ped.qt-pedida).

               APPLY 'ENTRY' TO tt-itens-ped.qt-pedida.
               RETURN NO-APPLY.
            END.
            RUN pi-cancela-reserva.
            ASSIGN SUBSTR(tt-itens-ped.char-2,1550,1) = "S".
         END.
      END.
    
      IF p-acao <> 'Consultar' THEN 
         ASSIGN bt-ok:SENSITIVE = YES.

      tt-itens-ped.it-codigo:READ-ONLY IN BROWSE br-ped-item = YES.
      tt-itens-ped.cod-refer:READ-ONLY IN BROWSE br-ped-item = YES.
      tt-itens-ped.qt-pedida:READ-ONLY IN BROWSE br-ped-item = YES.
      tt-itens-ped.vl-preori:READ-ONLY IN BROWSE br-ped-item = YES.
    
      IF br-ped-item:NEW-ROW IN FRAME {&FRAME-NAME} THEN
         DO TRANSACTION ON ERROR UNDO, RETURN NO-APPLY:
    
         IF LOOKUP(KEYFUNCTION(LAST-KEY),"TAB,RETURN") > 0 THEN DO.
            CREATE tt-itens-ped.
            ASSIGN INPUT BROWSE br-ped-item tt-itens-ped.nr-sequencia
                   INPUT BROWSE br-ped-item tt-itens-ped.it-codigo
                   INPUT BROWSE br-ped-item tt-itens-ped.cod-refer
                   INPUT BROWSE br-ped-item tt-itens-ped.qt-pedida
                   INPUT BROWSE br-ped-item tt-itens-ped.vl-preori
                   INPUT BROWSE br-ped-item tt-itens-ped.vl-pre-calc.
    
            ASSIGN fi-tot-qtd-ped = fi-tot-qtd-ped + tt-itens-ped.qt-pedida
                   fi-tot-vlr-ped = fi-tot-vlr-ped + (tt-itens-ped.vl-preori * tt-itens-ped.qt-pedida).

            ASSIGN tt-itens-ped.tp-acao = 'incluir'.
        
            br-ped-item:CREATE-RESULT-LIST-ENTRY() IN FRAME {&FRAME-NAME}.
            RELEASE tt-itens-ped.
    
            ASSIGN fi-tot-qtd-ped:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(fi-tot-qtd-ped)
                   fi-tot-vlr-ped:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(fi-tot-vlr-ped)
                   fi-tot-vlr-abe:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(fi-tot-vlr-ped).

            APPLY 'CHOOSE' TO bt-inc.
            RETURN NO-APPLY.
         END.
         ELSE
            APPLY 'END-ERROR' TO br-ped-item.
      END.
      ELSE DO TRANSACTION ON ERROR UNDO, RETURN NO-APPLY:
          ASSIGN fi-tot-qtd-ped = fi-tot-qtd-ped - tt-itens-ped.qt-pedida
                 fi-tot-vlr-ped = fi-tot-vlr-ped - (tt-itens-ped.vl-preori * tt-itens-ped.qt-pedida).

         ASSIGN INPUT BROWSE br-ped-item tt-itens-ped.nr-sequencia
                INPUT BROWSE br-ped-item tt-itens-ped.it-codigo
                INPUT BROWSE br-ped-item tt-itens-ped.cod-refer
                INPUT BROWSE br-ped-item tt-itens-ped.qt-pedida
                INPUT BROWSE br-ped-item tt-itens-ped.vl-preori.
    
         ASSIGN fi-tot-qtd-ped = fi-tot-qtd-ped + tt-itens-ped.qt-pedida
                fi-tot-vlr-ped = fi-tot-vlr-ped + (tt-itens-ped.vl-preori * tt-itens-ped.qt-pedida).

         ASSIGN fi-tot-qtd-ped:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(fi-tot-qtd-ped)
                fi-tot-vlr-ped:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(fi-tot-vlr-ped)
                fi-tot-vlr-abe:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(fi-tot-vlr-ped).

         ASSIGN tt-itens-ped.tp-acao = IF tt-itens-ped.tp-acao = ''
                                       THEN 'modificar' ELSE tt-itens-ped.tp-acao.
    
         ASSIGN bt-del:SENSITIVE IN FRAME {&FRAME-NAME}  = NO
                bt-inc:SENSITIVE IN FRAME {&FRAME-NAME}  = NO
                bt-mod:SENSITIVE IN FRAME {&FRAME-NAME}  = YES
                bt-can:SENSITIVE IN FRAME {&FRAME-NAME}  = YES
                bt-ok:SENSITIVE IN FRAME {&FRAME-NAME}  = YES.
    
         IF p-acao = 'Incluir' AND
            fi-reserva:INPUT-VALUE <> 0 THEN 
            ASSIGN bt-del:SENSITIVE IN FRAME {&FRAME-NAME}  = NO
                   bt-can:SENSITIVE IN FRAME {&FRAME-NAME}  = NO.
    
         IF CURRENT-RESULT-ROW("br-ped-item") = NUM-RESULTS("br-ped-item") THEN
            APPLY 'ENTRY' TO bt-mod.
         ELSE 
            APPLY 'CHOOSE' TO bt-mod.
      END.
   END. 
   ELSE
       APPLY 'END-ERROR' TO br-ped-item.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-ped-item w-window
ON VALUE-CHANGED OF br-ped-item IN FRAME F-Main /* Itens do Pedido */
DO:
  ASSIGN bt-inc:SENSITIVE IN FRAME {&FRAME-NAME} = NO
         bt-mod:SENSITIVE IN FRAME {&FRAME-NAME} = NO
         bt-del:SENSITIVE IN FRAME {&FRAME-NAME} = NO
         bt-can:SENSITIVE IN FRAME {&FRAME-NAME} = NO
         bt-bloq:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

  IF p-acao <> 'Consultar' AND
     fi-reserva:INPUT-VALUE = 0 THEN 
     ASSIGN bt-inc:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

  IF AVAIL tt-itens-ped AND 
     p-acao <> 'consultar' AND
     tt-itens-ped.cod-sit-item = 1 THEN 
     ASSIGN bt-mod:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            bt-del:SENSITIVE IN FRAME {&FRAME-NAME} = NOT CAN-FIND (FIRST ped-item OF tt-itens-ped)
            bt-can:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            bt-bloq:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

  IF p-acao = 'Incluir' AND
     fi-reserva:INPUT-VALUE <> 0 THEN 
     ASSIGN bt-inc:SENSITIVE IN FRAME {&FRAME-NAME} = NO
            bt-del:SENSITIVE IN FRAME {&FRAME-NAME} = NOT CAN-FIND (FIRST ped-item OF tt-itens-ped)
            bt-can:SENSITIVE IN FRAME {&FRAME-NAME} = NO
            bt-bloq:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
  
  IF AVAIL tt-itens-ped THEN DO.
     IF tt-itens-ped.bloqueio-fat = YES THEN DO.
        bt-bloq:LOAD-IMAGE-UP("image/im-lib1.bmp").
        bt-bloq:TOOLTIP = "Desbloqueia o Faturamento do Item".
     END.
     ELSE DO.
        bt-bloq:LOAD-IMAGE("image/im-bloq.bmp").
        bt-bloq:TOOLTIP = "Bloqueia o Faturamento do Item".
     END.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-bloq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-bloq w-window
ON CHOOSE OF bt-bloq IN FRAME F-Main
DO:
   RUN pi-ver-permissao.
   IF NOT l-tem-acesso THEN DO.
      MESSAGE "Usu†rio: " + c-seg-usuario + ", n∆o tem permiss∆o para essa rotina."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
   END.

   IF tt-itens-ped.tp-acao <> '' THEN DO.
      MESSAGE 'Existem alteraá‰es pendentes para o Item, favor confirmar e depois Bloquear...'
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
   END.

   GET CURRENT br-ped-item.

   ASSIGN tt-itens-ped.tp-acao = 'Modificar'.
   IF tt-itens-ped.bloqueio-fat = NO THEN
      ASSIGN tt-itens-ped.bloqueio-fat = YES.
   ELSE
      ASSIGN tt-itens-ped.bloqueio-fat = NO.

   br-ped-item:REFRESH().
   APPLY 'VALUE-CHANGED' TO br-ped-item.

   ASSIGN bt-ok:SENSITIVE = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-can
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-can w-window
ON CHOOSE OF bt-can IN FRAME F-Main
DO:
   RUN pi-ver-permissao.
   IF NOT l-tem-acesso THEN
      MESSAGE "Usu†rio: " + c-seg-usuario + ", n∆o tem permiss∆o para essa rotina."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
   ELSE DO:
      FIND ped-item-res WHERE
           ped-item-res.cod-estabel = ped-venda.cod-estabel AND  
           ped-item-res.nome-abrev = ped-venda.nome-abrev AND   
           ped-item-res.nr-pedcli = ped-venda.nr-pedcli AND    
           ped-item-res.nr-sequencia = tt-itens-ped.nr-sequencia 
           NO-LOCK NO-ERROR. 

      IF AVAIL ped-item-res AND ped-item-res.qt-pedida > 0 THEN DO.
         MESSAGE "J† Existem Etiquetas Reservadas para essa Sequància," SKIP
                 "Cancelamento s¢ ser† permitido se Cancelar a Reserva." SKIP
                 "Deseja Cancelar a Reserva ?"
                 VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                 TITLE "" UPDATE l-opcao AS LOGICAL.
         IF l-opcao = NO THEN
            RETURN NO-APPLY.
      
         RUN pi-cancela-reserva.
      END.
      
      IF br-ped-item:NUM-SELECTED-ROWS > 0 THEN DO ON ERROR UNDO, RETURN NO-APPLY:
         ASSIGN c-finalidade = 'Cancelamento'.
         RUN pdp/pd4000a.p (INPUT c-finalidade,
                            OUTPUT c-tipo-trans,
                            OUTPUT c-motivo,
                            OUTPUT da-dt-trans,
                            OUTPUT l-ok).
      
         IF l-ok THEN DO.
            GET CURRENT br-ped-item.
            ASSIGN tt-itens-ped.tp-acao = 'cancelar'
                   tt-itens-ped.cod-sit-item = 6
                   tt-itens-ped.motivo = c-motivo.
      
            ASSIGN bt-ok:SENSITIVE = YES.

            ASSIGN fi-tot-qtd-ped = fi-tot-qtd-ped - tt-itens-ped.qt-pedida
                   fi-tot-vlr-ped = fi-tot-vlr-ped - (tt-itens-ped.vl-preori * tt-itens-ped.qt-pedida).

            ASSIGN fi-tot-qtd-ped:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(fi-tot-qtd-ped)
                   fi-tot-vlr-ped:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(fi-tot-vlr-ped)
                   fi-tot-vlr-abe:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(fi-tot-vlr-ped).
         END.
      END.
      ASSIGN r-row-peditem = ROWID(tt-itens-ped).
      {&OPEN-QUERY-br-ped-item}
      br-ped-item:QUERY:REPOSITION-TO-ROWID(r-row-peditem).
      APPLY 'VALUE-CHANGED' TO br-ped-item.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cond-esp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cond-esp w-window
ON CHOOSE OF bt-cond-esp IN FRAME F-Main /* Button 2 */
DO:
   IF p-acao <> 'Incluir' THEN DO.
      FIND FIRST tt-cond-ped NO-ERROR.
      IF NOT AVAIL tt-cond-ped THEN DO.
         FOR EACH cond-ped OF ped-venda NO-LOCK.
             CREATE tt-cond-ped.
             BUFFER-COPY cond-ped TO tt-cond-ped.
         END.
      END.
   END.

   RUN esp/espd4000a.p (INPUT-OUTPUT TABLE tt-cond-ped, 
                        INPUT fi-cod-cond-pag:INPUT-VALUE,
                        INPUT p-acao).

   IF p-acao <> 'Consultar' THEN DO.
      FIND FIRST tt-cond-ped NO-LOCK NO-ERROR.
      IF NOT AVAIL tt-cond-ped THEN DO.
         APPLY 'ENTRY' TO fi-cod-cond-pag.
         RETURN NO-APPLY.
      END.
    
      ASSIGN de-tot-perc = 0.
      FOR EACH tt-cond-ped.
          ASSIGN de-tot-perc = de-tot-perc + tt-cond-ped.perc-pagto.
      END.
      IF de-tot-perc <> 100 THEN DO.
         MESSAGE 'Percentual de Pagamento' de-tot-perc 'deve ser igual Ö 100%'
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
         APPLY 'ENTRY' TO fi-cod-cond-pag.
         RETURN NO-APPLY.
      END.
    
      ASSIGN fi-cod-cond-pag:SCREEN-VALUE = '0' 
             fi-desc-cond-pag:SCREEN-VALUE = "E S P E C I A L".
    
      ASSIGN de-tot-prazo = 0
             i-ct = 0.

      FOR EACH tt-cond-ped.
          ASSIGN tt-cond-ped.nr-pedido = INPUT FRAME {&FRAME-NAME} fi-nr-pedido.

          ASSIGN de-tot-prazo = de-tot-prazo + tt-cond-ped.nr-dias-venc.
          ASSIGN i-ct = i-ct + 1.
      END.
      ASSIGN i-prazo-medio = de-tot-prazo / i-ct.

      FIND FIRST tab-finan WHERE 
                 tab-finan.dt-ini-val <= TODAY AND 
                 tab-finan.dt-fim-val >= TODAY NO-LOCK NO-ERROR.

      DO i-ct = 1 TO EXTENT(tab-finan.tab-dia-fin).
         IF tab-finan.tab-dia-fin[i-ct] >= i-prazo-medio THEN
            LEAVE. 
      END.

      IF i-ct > EXTENT(tab-finan.tab-ind-fin) THEN
         ASSIGN i-ct = EXTENT(tab-finan.tab-ind-fin).

      ASSIGN de-ind-finan = tab-finan.tab-ind-fin[i-ct].

      APPLY 'LEAVE' TO SELF.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cons-cliente-tri
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cons-cliente-tri w-window
ON CHOOSE OF bt-cons-cliente-tri IN FRAME F-Main
DO:
    IF fi-cliente-tri:SCREEN-VALUE <> '' THEN DO.
       FIND emitente WHERE 
            emitente.nome-abrev = fi-cliente-tri:SCREEN-VALUE NO-LOCK NO-ERROR.

       ASSIGN gr-emitente = ROWID(emitente).

       RUN cdp/cd1022.p.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-copia-it-ped
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-copia-it-ped w-window
ON CHOOSE OF bt-copia-it-ped IN FRAME F-Main /* Button 1 */
DO:
   RUN pi-ver-permissao.
   IF NOT l-tem-acesso THEN DO.
      MESSAGE "Usu†rio: " + c-seg-usuario + ", n∆o tem permiss∆o para essa rotina..."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
   END.

   IF fi-reserva:INPUT-VALUE <> 0 THEN DO.
      MESSAGE "N∆o permitido C¢pia de Itens em Pedidos de Reserva..."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
   END.

   RUN esp/espd4000b.w (OUTPUT c-nr-pedcli,
                        OUTPUT l-todos-itens,
                        OUTPUT l-copia-reservas,
                        OUTPUT l-aberto,
                        OUTPUT l-atendido-parcial,
                        OUTPUT l-atendido-total,
                        OUTPUT l-pendente,
                        OUTPUT l-suspenso,
                        OUTPUT l-cancelado,
                        OUTPUT l-fat-balcao,
                        OUTPUT c-it-codigo-ini,                              
                        OUTPUT c-it-codigo-fin,                              
                        OUTPUT c-cod-refer-ini,                              
                        OUTPUT c-cod-refer-fin,
                        OUTPUT l-copia-observ,
                        OUTPUT l-ok).

   IF l-ok THEN DO.
      RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
      {utp/ut-liter.i Copiando_Pedido *}
      RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

      ASSIGN fi-tot-qtd-ped = 0     fi-tot-qtd-res = 0
             fi-tot-qtd-fat = 0     fi-tot-vlr-ped = 0
             fi-tot-vlr-fat = 0     fi-tot-vlr-abe = 0.

      FIND LAST tt-itens-ped NO-LOCK NO-ERROR.
      IF AVAIL tt-itens-ped THEN
         ASSIGN i-ult-seq = tt-itens-ped.nr-sequencia.
      ELSE
         ASSIGN i-ult-seq = 10.

      FIND ped-venda WHERE
           ped-venda.nr-pedcli = c-nr-pedcli NO-LOCK NO-ERROR.

      FOR EACH ped-item OF ped-venda NO-LOCK.
          RUN pi-acompanhar IN h-acomp (INPUT "Copiando Item: " + ped-item.it-codigo + " Ref.: " + ped-item.cod-refer).

          IF NOT l-todos-itens AND
             (ped-item.it-codigo < c-it-codigo-ini OR
              ped-item.it-codigo > c-it-codigo-fin OR
              ped-item.cod-refer < c-cod-refer-ini OR
              ped-item.cod-refer > c-cod-refer-fin) THEN NEXT.

          IF (NOT l-aberto AND ped-item.cod-sit-item = 1) OR
             (NOT l-atendido-parcial AND ped-item.cod-sit-item = 2) OR
             (NOT l-atendido-total AND ped-item.cod-sit-item = 3) OR
             (NOT l-pendente AND ped-item.cod-sit-item = 4) OR
             (NOT l-suspenso AND ped-item.cod-sit-item = 5) OR
             (NOT l-cancelado AND ped-item.cod-sit-item = 6) OR
             (NOT l-fat-balcao AND ped-item.cod-sit-item = 7) THEN NEXT.

          FIND ped-item-ext WHERE
               ped-item-ext.cod-estabel = ped-venda.cod-estabel AND
               ped-item-ext.nr-pedcli = ped-item.nr-pedcli AND
               ped-item-ext.nome-abrev = ped-item.nome-abrev AND
               ped-item-ext.nr-sequencia = ped-item.nr-sequencia NO-LOCK.

          ASSIGN i-ult-seq = i-ult-seq + 10.
          CREATE tt-itens-ped.
          BUFFER-COPY ped-item TO tt-itens-ped
                      ASSIGN tt-itens-ped.cod-sit-item = 1
                             tt-itens-ped.nome-abrev   = fi-cliente:INPUT-VALUE
                             tt-itens-ped.cod-estabel  = fi-cod-estabel:INPUT-VALUE
                             tt-itens-ped.nr-pedcli    = fi-nr-pedido:INPUT-VALUE
                             tt-itens-ped.nr-sequencia = i-ult-seq.

          ASSIGN fi-tot-qtd-ped = fi-tot-qtd-ped + ped-item.qt-pedida
                 fi-tot-vlr-ped = fi-tot-vlr-ped + ped-item.vl-tot-it.

          ASSIGN tt-itens-ped.tp-acao = 'Incluir'.
      END.

      /* Copia Observaá∆o */
      IF l-copia-observ THEN 
         ASSIGN ed-obs:SCREEN-VALUE = ped-venda.observ.

      /* Copia Reservas */
      IF l-copia-reservas THEN DO.
         FOR EACH b-ped-item-res WHERE
                  b-ped-item-res.nome-abrev = ped-venda.nome-abrev AND
                  b-ped-item-res.nr-pedcli = ped-venda.nr-pedcli AND
                  b-ped-item-res.faturado  = NO EXCLUSIVE-LOCK.

             RUN pi-acompanhar IN h-acomp (INPUT "Copiando Reservas: " + b-ped-item-res.it-codigo +
                                                 " Ref.: " + b-ped-item-res.cod-refer).

             IF NOT l-todos-itens AND
                (b-ped-item-res.it-codigo < c-it-codigo-ini OR
                 b-ped-item-res.it-codigo > c-it-codigo-fin OR
                 b-ped-item-res.cod-refer < c-cod-refer-ini OR
                 b-ped-item-res.cod-refer > c-cod-refer-fin) THEN NEXT.

             FIND tt-itens-ped WHERE
                  tt-itens-ped.nome-abrev   = fi-cliente:INPUT-VALUE AND
                  tt-itens-ped.nr-pedcli    = fi-nr-pedido:INPUT-VALUE AND
                  tt-itens-ped.cod-estabel  = fi-cod-estabel:INPUT-VALUE AND
                  tt-itens-ped.nr-sequencia = b-ped-item-res.nr-sequencia AND
                  tt-itens-ped.it-codigo    = b-ped-item-res.it-codigo AND
                  tt-itens-ped.cod-refer    = b-ped-item-res.cod-refer
                  NO-ERROR.

             IF AVAIL tt-itens-ped THEN 
                ASSIGN tt-itens-ped.qt-reserva = b-ped-item-res.qt-pedida
                       fi-tot-qtd-res = fi-tot-qtd-res + b-ped-item-res.qt-pedida.
         END.
      END.
      RUN pi-finalizar in h-acomp.      

      {&OPEN-QUERY-br-ped-item}

      APPLY 'value-changed' TO br-ped-item.

      DISP fi-tot-qtd-ped
           fi-tot-qtd-res
           fi-tot-qtd-fat
           fi-tot-vlr-ped
           fi-tot-vlr-fat
           fi-tot-vlr-abe
           WITH FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-del
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-del w-window
ON CHOOSE OF bt-del IN FRAME F-Main /* bt inclui 2 */
DO:
  IF br-ped-item:NUM-SELECTED-ROWS > 0 THEN DO ON ERROR UNDO, RETURN NO-APPLY:
     ASSIGN fi-tot-qtd-ped = fi-tot-qtd-ped - tt-itens-ped.qt-pedida
            fi-tot-vlr-ped = fi-tot-vlr-ped - (tt-itens-ped.vl-preori * tt-itens-ped.qt-pedida).

     ASSIGN fi-tot-qtd-ped:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(fi-tot-qtd-ped)
            fi-tot-vlr-ped:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(fi-tot-vlr-ped)
            fi-tot-vlr-abe:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(fi-tot-vlr-ped).

     DELETE tt-itens-ped.
     IF br-ped-item:DELETE-CURRENT-ROW() IN FRAME {&FRAME-NAME} THEN.
  END.

  IF NUM-RESULTS("br-ped-item":U) = 0 THEN
     ASSIGN bt-mod:SENSITIVE IN FRAME {&FRAME-NAME} = NO 
            bt-del:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

  RUN adm-open-query-cases.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-imp-res
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-imp-res w-window
ON CHOOSE OF bt-imp-res IN FRAME F-Main /* Button 2 */
DO:
    FIND ped-reserva WHERE
         ped-reserva.num-reserva = INPUT FRAME {&FRAME-NAME} fi-reserva
         NO-LOCK NO-ERROR.

    IF NOT AVAIL ped-reserva THEN DO.
       MESSAGE "Reserva N∆o Cadastrada..."
           VIEW-AS ALERT-BOX ERROR BUTTONS OK.
       APPLY 'entry' TO SELF.
       RETURN NO-APPLY.
    END.

    FOR EACH tt-itens-ped.
        DELETE tt-itens-ped.
    END.

    ASSIGN fi-tot-qtd-ped = 0     fi-tot-qtd-res = 0
           fi-tot-qtd-fat = 0     fi-tot-vlr-ped = 0
           fi-tot-vlr-fat = 0     fi-tot-vlr-abe = 0.

    FOR EACH ped-reserva-it WHERE
             ped-reserva-it.num-reserva = INPUT FRAME {&FRAME-NAME} fi-reserva NO-LOCK.

        CREATE tt-itens-ped.
        ASSIGN tt-itens-ped.nr-sequencia = ped-reserva-it.nr-sequencia
               tt-itens-ped.it-codigo = ped-reserva-it.it-codigo
               tt-itens-ped.cod-refer = ped-reserva-it.cod-refer
               tt-itens-ped.tp-acao = 'Incluir'.

        FIND corte-comerc WHERE
             corte-comerc.codigo = ped-reserva-it.corte-comerc NO-LOCK NO-ERROR.

        FOR EACH ped-reserva-etq OF ped-reserva-it NO-LOCK.
            FIND ob-etiqueta WHERE
                 ob-etiqueta.cod-estabel = ped-reserva.cod-estabel AND
                 ob-etiqueta.num-etiqueta = ped-reserva-etq.num-etiqueta
                 NO-LOCK NO-ERROR.
            ASSIGN tt-itens-ped.qt-pedida = tt-itens-ped.qt-pedida + ob-etiqueta.quantidade.
        END.
        ASSIGN tt-itens-ped.qt-reserva = tt-itens-ped.qt-pedida
               fi-tot-qtd-res = fi-tot-qtd-res + tt-itens-ped.qt-pedida
               fi-tot-qtd-ped = fi-tot-qtd-ped + tt-itens-ped.qt-pedida.
    END.

    {&OPEN-QUERY-br-ped-item}

    APPLY 'value-changed' TO br-ped-item.

    DISP fi-tot-qtd-ped
         fi-tot-qtd-res
         fi-tot-qtd-fat
         fi-tot-vlr-ped
         fi-tot-vlr-fat
         fi-tot-vlr-abe
         WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-inc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-inc w-window
ON CHOOSE OF bt-inc IN FRAME F-Main /* Button 3 */
DO:
   RUN pi-ver-permissao.
   IF NOT l-tem-acesso THEN DO.
      MESSAGE "Usu†rio: " + c-seg-usuario + ", n∆o tem permiss∆o para essa rotina."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
   END.

   IF fi-reserva:INPUT-VALUE <> 0 THEN DO.
      MESSAGE "N∆o permitido inclus∆o de Itens em Pedidos de Reserva..."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
   END.

   IF cb-tp-preco:SCREEN-VALUE = "1" THEN
      ASSIGN tt-itens-ped.vl-preori:READ-ONLY IN BROWSE {&browse-name} = NO.
   ELSE 
      ASSIGN tt-itens-ped.vl-preori:READ-ONLY IN BROWSE {&browse-name} = YES.

   tt-itens-ped.it-codigo:READ-ONLY IN BROWSE br-ped-item = NO.
   tt-itens-ped.cod-refer:READ-ONLY IN BROWSE br-ped-item = NO.
   tt-itens-ped.qt-pedida:READ-ONLY IN BROWSE br-ped-item = NO.

   RUN pi-add-item.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-log
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-log w-window
ON CHOOSE OF bt-log IN FRAME F-Main
DO:
   RUN esp/essp0155b.p (INPUT fi-nr-pedido:SCREEN-VALUE).
                        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-mod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-mod w-window
ON CHOOSE OF bt-mod IN FRAME F-Main /* bt inclui 2 */
DO:
   RUN pi-ver-permissao.
   IF NOT l-tem-acesso THEN DO.
      MESSAGE "Usu†rio: " + c-seg-usuario + ", n∆o tem permiss∆o para essa rotina."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
   END.

   IF ped-venda.tp-pedido = 'PI' THEN DO.
      ASSIGN de-qtidade-atu = 0.
      FOR EACH pp-it-container WHERE
               pp-it-container.nr-container   = ped-venda-ext.nr-container AND
               pp-it-container.it-comprado    = tt-itens-ped.it-codigo AND
               pp-it-container.ref-comprada   = tt-itens-ped.cod-refer 
               NO-LOCK.
           ASSIGN de-qtidade-atu = de-qtidade-atu + (pp-it-container.qt-pedida - pp-it-container.qt-vendida).
       END.
   END.
   ELSE DO.
       ASSIGN de-qtidade-atu = 0.
       FOR EACH saldo-estoq WHERE
                saldo-estoq.cod-estabel = ped-venda.cod-estabel  AND 
                saldo-estoq.it-codigo   = tt-itens-ped.it-codigo  AND
                saldo-estoq.cod-refer   = tt-itens-ped.cod-refer AND 
                saldo-estoq.cod-localiz = '' NO-LOCK.
           ASSIGN de-qtidade-atu = de-qtidade-atu + saldo-estoq.qtidade-atu. 
       END.

       /* Subtrai Pedidos em Aberto */
       FOR EACH b-ped-venda WHERE
                b-ped-venda.cod-sit-ped = 1 NO-LOCK,
           EACH ped-item OF b-ped-venda WHERE
                ped-item.cod-sit-item = 1 AND
                ped-item.it-codigo = tt-itens-ped.it-codigo AND
                ped-item.cod-refer = tt-itens-ped.cod-refer
                NO-LOCK.

           IF b-ped-venda.tp-pedido = 'PI' AND
              b-ped-venda.dt-entrega > TODAY THEN NEXT.

           ASSIGN de-qtidade-atu = de-qtidade-atu - ped-item.qt-pedida.
       END.

       /* Subtrai Notas em Aberto */ 
       FOR EACH nota-fiscal WHERE
                nota-fiscal.cod-estabe = ped-venda.cod-estabel AND
                nota-fiscal.serie = para-fat.serie-pad AND
                nota-fiscal.dt-cancela = ? AND
                nota-fiscal.dt-confirma = ? NO-LOCK,
           EACH it-nota-fisc OF nota-fiscal WHERE
                it-nota-fisc.it-codigo = tt-itens-ped.it-codigo AND
                it-nota-fisc.cod-refer = tt-itens-ped.cod-refer NO-LOCK.
        
           ASSIGN de-qtidade-atu = de-qtidade-atu - it-nota-fisc.qt-faturada[1].
        END.
   END.

   IF de-qtidade-atu < 0 THEN
      ASSIGN de-qtidade-atu = 0.

   ASSIGN tt-itens-ped.dec-2 = de-qtidade-atu.
   DISP tt-itens-ped.dec-2 
        WITH BROWSE {&browse-name}.

   IF cb-tp-preco:SCREEN-VALUE = "1" THEN
      ASSIGN tt-itens-ped.vl-preori:READ-ONLY IN BROWSE {&browse-name} = NO.
   ELSE 
      ASSIGN tt-itens-ped.vl-preori:READ-ONLY IN BROWSE {&browse-name} = YES.

   IF fi-reserva:INPUT-VALUE <> 0 THEN 
      APPLY 'entry':U TO tt-itens-ped.vl-preori IN BROWSE br-ped-item. 
   ELSE DO.
      IF NOT INPUT FRAME {&FRAME-NAME} cb-tp-pedido MATCHES "*Amostra*" THEN DO.
         tt-itens-ped.qt-pedida:READ-ONLY IN BROWSE br-ped-item = NO.
         APPLY 'entry':U TO tt-itens-ped.qt-pedida IN BROWSE br-ped-item. 
      END.
      ELSE DO.
         tt-itens-ped.qt-pedida:READ-ONLY IN BROWSE br-ped-item = NO.
         APPLY 'entry':U TO tt-itens-ped.qt-pedida IN BROWSE br-ped-item. 
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-msg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-msg w-window
ON CHOOSE OF bt-msg IN FRAME F-Main
DO:
   ASSIGN c-texto-msg = "".
   RUN esp/espd4000d.p (OUTPUT c-texto-msg).

   IF c-texto-msg <> "" THEN
      ASSIGN fi-observ-nf:SCREEN-VALUE IN FRAME {&FRAME-NAME} = c-texto-msg.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-window
ON CHOOSE OF bt-ok IN FRAME F-Main /* OK */
DO:
   APPLY 'ENTRY' TO cb-tp-pedido.

   RUN pi-validate.
   IF RETURN-VALUE = 'ADM-ERROR' THEN
      RETURN NO-APPLY.

   DO TRANSACTION:
      RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
      {utp/ut-liter.i Atualizando *}
      RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

      ASSIGN l-criou-pedido = NO.
      RUN pi-pedvenda.

      IF RETURN-VALUE <> 'ADM-ERROR' THEN DO.
         ASSIGN l-criou-pedido = YES.

         IF p-acao = 'Incluir' AND fi-reserva:INPUT-VALUE <> 0 THEN DO.
            FIND ped-reserva WHERE
                 ped-reserva.num-reserva = INPUT FRAME {&FRAME-NAME} fi-reserva NO-ERROR.
            
            FOR EACH ped-reserva-it OF ped-reserva NO-LOCK.
                FIND tt-itens-ped WHERE
                     tt-itens-ped.nr-sequencia = ped-reserva-it.nr-sequencia 
                     NO-LOCK NO-ERROR.

                CREATE ped-item-res.
                ASSIGN ped-item-res.cod-estabel  = ped-venda.cod-estabel
                       ped-item-res.nome-abrev   = ped-venda.nome-abrev
                       ped-item-res.nr-pedcli    = ped-venda.nr-pedcli
                       ped-item-res.nome-transp  = ped-venda.nome-transp
                       ped-item-res.nr-sequencia = tt-itens-ped.nr-sequencia
                       ped-item-res.it-codigo    = tt-itens-ped.it-codigo
                       ped-item-res.cod-refer    = tt-itens-ped.cod-refer
                       ped-item-res.sigla-emb    = 'RP'
                       ped-item-res.desc-dentro  = c-desc-dentro
                       ped-item-res.qt-pedida    = tt-itens-ped.qt-pedida
                       ped-item-res.dt-trans     = TODAY
                       ped-item-res.hr-trans     = STRING(TIME,"HH:MM:SS") + "Prog: ESPD4000"
                       ped-item-res.lote         = 'RP' + tt-itens-ped.cod-refer.
                
                FOR EACH ped-reserva-etq OF ped-reserva-it NO-LOCK.
                    FIND ob-etiqueta WHERE
                         ob-etiqueta.cod-estabel = ped-reserva.cod-estabel AND
                         ob-etiqueta.num-etiqueta = ped-reserva-etq.num-etiqueta 
                         SHARE-LOCK NO-ERROR.

                    FIND ped-item-rom WHERE
                         ped-item-rom.cod-estabel = ped-venda.cod-estabel AND
                         ped-item-rom.num-etiqueta = ob-etiqueta.num-etiqueta NO-LOCK NO-ERROR.

                    IF NOT AVAIL ped-item-rom THEN DO.
                       CREATE ped-item-rom.
                       ASSIGN ped-item-rom.cod-estabel = ped-venda.cod-estabel
                              ped-item-rom.nome-abrev = ped-item-res.nome-abrev
                              ped-item-rom.nr-pedcli = ped-item-res.nr-pedcli
                              ped-item-rom.nr-sequencia = ped-item-res.nr-sequencia
                              ped-item-rom.num-etiqueta = ob-etiqueta.num-etiqueta
                              ped-item-rom.nr-ob = ob-etiqueta.nr-ob
                              ped-item-rom.nr-seq-etq = ob-etiqueta.nr-sequencia
                              ped-item-rom.quantidade = ob-etiqueta.quantidade.
                    END.
                END.
            END.
            ASSIGN ped-reserva.situacao = 3.
         END.

         IF c-nr-pedcli <> '' AND
            l-copia-reservas = YES THEN DO.  /* Ç para copiar as reservas */

            /* Procura pedido copiado e là suas reservas */
            FIND ped-venda WHERE
                 ped-venda.nr-pedcli = c-nr-pedcli NO-LOCK NO-ERROR.

            FOR EACH b-ped-item-res WHERE
                     b-ped-item-res.nome-abrev = ped-venda.nome-abrev AND
                     b-ped-item-res.nr-pedcli = ped-venda.nr-pedcli AND
                     b-ped-item-res.faturado  = NO EXCLUSIVE-LOCK.

                IF NOT l-todos-itens AND
                   (b-ped-item-res.it-codigo < c-it-codigo-ini OR
                    b-ped-item-res.it-codigo > c-it-codigo-fin OR
                    b-ped-item-res.cod-refer < c-cod-refer-ini OR
                    b-ped-item-res.cod-refer > c-cod-refer-fin) THEN NEXT.

                FIND tt-itens-ped WHERE
                     tt-itens-ped.nome-abrev   = fi-cliente:INPUT-VALUE AND
                     tt-itens-ped.nr-pedcli    = fi-nr-pedido:INPUT-VALUE AND
                     tt-itens-ped.cod-estabel  = fi-cod-estabel:INPUT-VALUE AND
                     tt-itens-ped.nr-sequencia = b-ped-item-res.nr-sequencia AND
                     tt-itens-ped.it-codigo    = b-ped-item-res.it-codigo AND
                     tt-itens-ped.cod-refer    = b-ped-item-res.cod-refer
                     NO-ERROR.

                IF NOT AVAIL tt-itens-ped THEN NEXT.

                CREATE ped-item-res.
                BUFFER-COPY b-ped-item-res TO ped-item-res
                            ASSIGN ped-item-res.nome-abrev = fi-cliente:INPUT-VALUE
                                   ped-item-res.nr-pedcli  = fi-nr-pedido:INPUT-VALUE.

                /* Altera Romaneio */
                FOR EACH ped-item-rom WHERE
                         ped-item-rom.nome-abrev = b-ped-item-res.nome-abrev AND
                         ped-item-rom.nr-pedcli = b-ped-item-res.nr-pedcli AND
                         ped-item-rom.nr-sequencia = b-ped-item-res.nr-sequencia
                         EXCLUSIVE-LOCK.
    
                    ASSIGN ped-item-rom.nome-abrev = ped-item-res.nome-abrev
                           ped-item-rom.nr-pedcli = ped-item-res.nr-pedcli.
                END.

                /* Elimina reservas do pedido copiado */
                DELETE b-ped-item-res.
            END.
         END.
      END.
       
      RUN pi-finalizar in h-acomp.
   END.

   IF p-acao = "Incluir" AND l-criou-pedido THEN DO.
      ASSIGN c-pedidos = IF c-pedidos = ''
                         THEN fi-nr-pedido:SCREEN-VALUE IN FRAME {&FRAME-NAME}
                         ELSE c-pedidos + ',' + fi-nr-pedido:SCREEN-VALUE IN FRAME {&FRAME-NAME}
             cb-tp-pedido:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Normal"
             fi-nr-pedido:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "?".

      APPLY 'VALUE-CHANGED' TO cb-tp-pedido.
      APPLY 'ENTRY' TO cb-tp-pedido.
      RETURN NO-APPLY.
   END.
   ELSE DO.
      IF ped-venda.completo THEN DO.
         bt-ok:LOAD-IMAGE("image/im-ok.gif").

         MESSAGE 'Manutená∆o Efetuada com SUCESSO...'
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
      END.
      ELSE DO.
         bt-ok:LOAD-IMAGE("image/im-sav.gif").

         MESSAGE 'Manutená∆o Efetuada, Pedido IMCOMPLETO...'
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
      END.
      FIND CURRENT ped-item NO-LOCK NO-ERROR.
      FIND CURRENT ped-venda NO-LOCK NO-ERROR.

      APPLY 'VALUE-CHANGED' TO br-ped-item.
      APPLY 'entry' TO bt-mod.
      RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-sair
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-sair w-window
ON CHOOSE OF bt-sair IN FRAME F-Main /* Cancelar */
DO:
  FIND CURRENT ped-venda NO-LOCK NO-ERROR.
  FIND CURRENT ped-item NO-LOCK NO-ERROR.

  apply "close":U to this-procedure.
  RETURN c-pedidos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-ext-tp-pedido
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-ext-tp-pedido w-window
ON VALUE-CHANGED OF cb-ext-tp-pedido IN FRAME F-Main
DO:
   CASE SELF:SCREEN-VALUE.
       WHEN 'PI' THEN 
            ASSIGN fi-nr-container:SENSITIVE = YES
                   tg-nao-aprovar:SCREEN-VALUE = 'YES'.
       WHEN 'PE' THEN
            ASSIGN fi-nr-container:SENSITIVE = NO
                   fi-nr-container:SCREEN-VALUE = ""
                   tg-nao-aprovar:SCREEN-VALUE = 'NO'.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-tab-preco
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-tab-preco w-window
ON LEAVE OF cb-tab-preco IN FRAME F-Main /* Tabela Preáo */
DO:
   IF cb-tab-preco:SCREEN-VALUE <> cb-tab-preco AND
      cb-tp-preco:SCREEN-VALUE <> "1" THEN DO.
      ASSIGN de-ind-finan = IF de-ind-finan = 0 THEN 1 ELSE de-ind-finan.
      FOR EACH tt-ped-item.
          FIND preco-item WHERE
               preco-item.nr-tabpre = cb-tab-preco:SCREEN-VALUE AND
               preco-item.it-codigo = tt-ped-item.it-codigo AND
               preco-item.cod-refer = tt-ped-item.cod-refer NO-LOCK NO-ERROR.
          IF AVAIL preco-item THEN
             ASSIGN tt-ped-item.vl-preuni = preco-item.preco-venda * de-ind-finan.
      END.
      RUN adm-open-query-cases.
   END.
   ASSIGN c-tab-preco = cb-tab-preco:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-tab-preco w-window
ON VALUE-CHANGED OF cb-tab-preco IN FRAME F-Main /* Tabela Preáo */
DO:
   FIND tb-preco WHERE
        tb-preco.nr-tabpre = SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
   IF AVAIL tb-preco THEN DO.
      FIND moeda WHERE
           moeda.mo-codigo = tb-preco.mo-codigo NO-LOCK NO-ERROR.
      IF AVAIL moeda THEN
         ASSIGN fi-moeda:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(tb-preco.mo-codigo)
                fi-desc-moeda:SCREEN-VALUE = moeda.descricao.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-tp-entrega
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-tp-entrega w-window
ON LEAVE OF cb-tp-entrega IN FRAME F-Main /* Tipo Entrega */
DO:
    IF SELF:SCREEN-VALUE = 'Imediata' AND
       cb-tp-pedido:SCREEN-VALUE <> 'Reserva' THEN DO.
       MESSAGE 'Tipo de Entrega indevido para Tipo de Pedido...'
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
       APPLY 'ENTRY' TO SELF.
       RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-tp-entrega w-window
ON VALUE-CHANGED OF cb-tp-entrega IN FRAME F-Main /* Tipo Entrega */
DO:
   ASSIGN fi-dt-entrega:SCREEN-VALUE = ?.
   CASE SELF:SCREEN-VALUE.
       WHEN "Na Data" OR WHEN "A Partir da Data" OR WHEN "AtÇ a Data" THEN
             ASSIGN fi-dt-entrega:SENSITIVE = YES.
       WHEN 'No Màs' THEN DO.
             RUN esapi/ret-udm.p (INPUT STRING(MONTH(TODAY),"99") + STRING(YEAR(TODAY),"9999"), 
                                  OUTPUT c-dia).
             ASSIGN fi-dt-entrega:SENSITIVE = YES
                    fi-dt-entrega:SCREEN-VALUE = c-dia + '/' + STRING(MONTH(TODAY),'99') + '/' + STRING(YEAR(TODAY),'9999').
       END.
       WHEN '1¶ Quinzena' THEN 
           ASSIGN fi-dt-entrega:SENSITIVE = YES
                  fi-dt-entrega:SCREEN-VALUE = '15' + '/' + STRING(MONTH(TODAY),'99') + '/' + STRING(YEAR(TODAY),'9999').
       WHEN '2¶ Quinzena' THEN DO.
           RUN esapi/ret-udm.p (INPUT STRING(MONTH(TODAY),"99") + STRING(YEAR(TODAY),"9999"), 
                                OUTPUT c-dia).
           ASSIGN fi-dt-entrega:SENSITIVE = YES
                  fi-dt-entrega:SCREEN-VALUE = c-dia + '/' + STRING(MONTH(TODAY),'99') + '/' + STRING(YEAR(TODAY),'9999').
       END.
       WHEN 'Imediata' THEN   /* aceitar apenas para pedidos do tipo RESERVA */
           ASSIGN fi-dt-entrega:SENSITIVE = NO
                  fi-dt-entrega:SCREEN-VALUE = STRING(TODAY) .
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-tp-frete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-tp-frete w-window
ON LEAVE OF cb-tp-frete IN FRAME F-Main /* Tipo Frete */
DO:
   IF KEYFUNCTION(LASTKEY) = 'back-tab' THEN DO.
      APPLY 'BACK-TAB' TO SELF.
      RETURN NO-APPLY.
   END.

   IF cb-tp-frete:SCREEN-VALUE = ? OR 
      cb-tp-frete:SCREEN-VALUE =  '' THEN DO.
      MESSAGE 'Tipo de Frete deve ser Informado....'
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      APPLY 'ENTRY' TO cb-tp-frete.
      RETURN NO-APPLY.
   END.

   IF cb-tp-frete:SCREEN-VALUE = 'Cif atÇ Redesp' AND
      fi-nome-tr-red:SCREEN-VALUE = '' THEN DO.
      MESSAGE 'Tipo de Frete deve possuir Transportadora de Redespacho'
             VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      APPLY 'ENTRY' TO cb-tp-frete.
      RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-tp-pedido
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-tp-pedido w-window
ON VALUE-CHANGED OF cb-tp-pedido IN FRAME F-Main /* Tipo Pedido */
DO:
   ASSIGN fi-reserva:SENSITIVE = NO
          fi-cliente-tri:SENSITIVE = NO
          bt-imp-res:SENSITIVE = NO
          bt-cond-esp:SENSITIVE = YES
          bt-msg:SENSITIVE = YES
          fi-cod-cond-pag:SENSITIVE = YES
          cb-tab-preco:SENSITIVE = YES
          bt-inc:SENSITIVE = YES
          bt-copia-it-ped:SENSITIVE = YES
          fi-cliente:SENSITIVE = YES
          fi-cod-rep:SENSITIVE = YES
          tg-nao-aprovar:SENSITIVE = YES
          fi-cod-rota:SENSITIVE = NO.

   ASSIGN fi-reserva:SCREEN-VALUE = '0'
          fi-cliente:SCREEN-VALUE = ''
          fi-cidade-cli:SCREEN-VALUE  = ''
          fi-uf:SCREEN-VALUE = ''
          fi-cliente-tri:SCREEN-VALUE  = ''
          fi-cidade-cli-tri:SCREEN-VALUE  = ''
          fi-uf-tri:SCREEN-VALUE = ''
          fi-fat-max:SCREEN-VALUE  = '0,00'
          fi-cod-cond-pag:SCREEN-VALUE  = ''
          fi-desc-cond-pag:SCREEN-VALUE = ''
          cb-tp-preco:SCREEN-VALUE = '2' 
          cb-tab-preco:SCREEN-VALUE = ' ' 
          cb-tipo-pagto:SCREEN-VALUE = 'Normal'
          /*tg-nao-aprovar:SCREEN-VALUE = 'NO'*/
          tg-bloqueio:SCREEN-VALUE = 'NO'
          fi-cod-rep:SCREEN-VALUE = ''
          fi-nome-rep:SCREEN-VALUE = ''
          fi-ped-repres:SCREEN-VALUE = ''
          fi-pedido-cli:SCREEN-VALUE = ''
          fi-perc-comis:SCREEN-VALUE = ''
          fi-natur-oper:SCREEN-VALUE = ''
          fi-denominacao:SCREEN-VALUE = ''
          fi-cod-cond-pag:SCREEN-VALUE = ''
          fi-desc-cond-pag:SCREEN-VALUE = ''
          fi-moeda:SCREEN-VALUE = '0'
          fi-desc-moeda:SCREEN-VALUE = 'Real'
          fi-nome-transp:SCREEN-VALUE = ''
          fi-nome-tr-red:SCREEN-VALUE = ''
          cb-tp-frete:SCREEN-VALUE = 'Cif Total'
          fi-data-base:SCREEN-VALUE = ''
          cb-tp-entrega:SCREEN-VALUE = "A Partir da Data"
          fi-dt-entrega:SCREEN-VALUE = STRING(TODAY)
          fi-cod-rota:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ''
          fi-desc-rota:SCREEN-VALUE = ''
          ed-obs:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ''
          fi-observ-nf:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ''.

   FOR EACH tt-itens-ped.
       DELETE tt-itens-ped.
   END.

   ASSIGN fi-tot-qtd-ped = 0     fi-tot-qtd-res = 0
          fi-tot-qtd-fat = 0     fi-tot-vlr-ped = 0
          fi-tot-vlr-fat = 0     fi-tot-vlr-abe = 0.

   {&OPEN-QUERY-br-ped-item}
   APPLY 'value-changed' TO br-ped-item.

   DISP fi-tot-qtd-ped
        fi-tot-qtd-res
        fi-tot-qtd-fat
        fi-tot-vlr-ped
        fi-tot-vlr-fat
        fi-tot-vlr-abe
        WITH FRAME {&FRAME-NAME}.

   IF SELF:SCREEN-VALUE = 'Reserva' OR
      SELF:SCREEN-VALUE = 'Exportaá∆o' OR
      SELF:SCREEN-VALUE = 'Operaá∆o Triangular' OR
      SELF:SCREEN-VALUE = '∑ Vista' OR
      SELF:SCREEN-VALUE = 'Rem.Industrializacao' OR
      SELF:SCREEN-VALUE = 'Bonificaá∆o' OR
      SELF:SCREEN-VALUE = 'Doaá∆o' THEN
      ASSIGN fi-reserva:SENSITIVE = YES.

   CASE SELF:SCREEN-VALUE.
       WHEN "Produá∆o" THEN DO.
           ASSIGN cb-ext-tp-pedido:SCREEN-VALUE = ""
                  cb-ext-tp-pedido:SENSITIVE = NO.
       END.
       WHEN "Reserva" THEN DO.
           ASSIGN cb-tp-entrega:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Imediata"
                  fi-dt-entrega:SCREEN-VALUE = STRING(TODAY).
       END.
       WHEN 'Exportaá∆o' THEN DO.
           ASSIGN tg-bloqueio:SENSITIVE = NO
                  fi-cod-rota:SENSITIVE = YES.

           ASSIGN tg-bloqueio:SCREEN-VALUE = 'YES'.

           FIND moeda WHERE
                moeda.mo-codigo = 3 NO-LOCK NO-ERROR.
           IF AVAIL moeda THEN
              ASSIGN fi-moeda:SCREEN-VALUE = '3'
                     fi-desc-moeda:SCREEN-VALUE = moeda.descricao.
       END.
       WHEN "Operaá∆o Triangular" THEN DO.
             ASSIGN fi-cliente-tri:SENSITIVE = YES.
       END.
       WHEN "∑ Vista" THEN DO.
             ASSIGN fi-cod-cond-pag:SENSITIVE = NO
                    bt-cond-esp:SENSITIVE = NO
                    tg-bloqueio:SENSITIVE = NO
                    fi-cliente-tri:SENSITIVE = YES.

             ASSIGN tg-bloqueio:SCREEN-VALUE = 'YES'.

             FIND cond-pagto WHERE
                  cond-pagto.cod-cond-pag = 1 NO-LOCK NO-ERROR.
             IF AVAIL cond-pagto THEN 
                ASSIGN fi-cod-cond-pag:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(cond-pagto.cod-cond-pag)
                       fi-desc-cond-pag:SCREEN-VALUE = cond-pagto.descricao.
       END.
       WHEN 'Amostra' THEN DO.
          ASSIGN fi-cod-cond-pag:SENSITIVE = NO
                 bt-cond-esp:SENSITIVE = NO.
       END.
       WHEN 'Amostra Exportaá∆o' THEN DO.
          ASSIGN fi-cod-cond-pag:SENSITIVE = NO
                 bt-cond-esp:SENSITIVE = NO
                 fi-cod-rota:SENSITIVE = YES.

          ASSIGN tg-bloqueio:SCREEN-VALUE = 'YES'.

          FIND moeda WHERE
               moeda.mo-codigo = 3 NO-LOCK NO-ERROR.
          IF AVAIL moeda THEN
             ASSIGN fi-moeda:SCREEN-VALUE = '3'
                    fi-desc-moeda:SCREEN-VALUE = moeda.descricao.
       END.
   END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-tp-preco
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-tp-preco w-window
ON LEAVE OF cb-tp-preco IN FRAME F-Main /* Tipo Preáo */
DO:
   IF cb-tp-preco:SCREEN-VALUE <> cb-tp-preco THEN DO.
      IF SELF:SCREEN-VALUE = "1" THEN DO.
         FOR EACH tt-ped-item.
             ASSIGN tt-ped-item.vl-preuni = 0.
         END.
         ASSIGN c-tab-preco = ''.
      END.
      RUN adm-open-query-cases. 
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-tp-preco w-window
ON VALUE-CHANGED OF cb-tp-preco IN FRAME F-Main /* Tipo Preáo */
DO:
  ASSIGN fi-moeda:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0"
         fi-desc-moeda:SCREEN-VALUE = "Real".
     
  IF INPUT FRAME {&FRAME-NAME} fi-cod-estabel = "1" THEN DO.
     FIND unid-feder WHERE 
          unid-feder.estado = emitente.estado NO-LOCK NO-ERROR.
     IF unid-feder.char-2 = 'SUL' OR
        (unid-feder.char-2 = 'SUDESTE' AND unid-feder.estado <> "ES") THEN
        FIND im-param WHERE
             im-param.cod-param = "TABELA_IMA12" NO-LOCK NO-ERROR.
     ELSE
        FIND im-param WHERE
             im-param.cod-param = "TABELA_IMA07" NO-LOCK NO-ERROR.
  END.
  ELSE
     FIND im-param WHERE
          im-param.cod-param = "TABELA_MED" NO-LOCK NO-ERROR.

  IF AVAIL im-param THEN
     ASSIGN c-tb-preco-pad = im-param.val-param.

  IF SELF:SCREEN-VALUE = "1" THEN DO.
     ASSIGN cb-tab-preco:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ' '.
     ASSIGN cb-tab-preco:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

     ASSIGN tt-itens-ped.vl-preori:READ-ONLY IN BROWSE {&browse-name} = NO.
  END.
  ELSE DO.
     ASSIGN cb-tab-preco:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

     IF AVAIL emitente AND emitente.nr-tabpre <> '' THEN
        ASSIGN cb-tab-preco:SCREEN-VALUE IN FRAME {&FRAME-NAME} = emitente.nr-tabpre.
     ELSE
        ASSIGN cb-tab-preco:SCREEN-VALUE IN FRAME {&FRAME-NAME} = c-tb-preco-pad.

     ASSIGN tt-itens-ped.vl-preori:READ-ONLY IN BROWSE {&browse-name} = YES.
  END.

  ASSIGN c-tab-preco = cb-tab-preco:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cliente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cliente w-window
ON ENTRY OF fi-cliente IN FRAME F-Main /* Cliente */
DO:
  IF SELF:SCREEN-VALUE <> '' THEN DO.
     FIND emitente WHERE 
          emitente.nome-abrev = INPUT FRAME {&FRAME-NAME} fi-cliente NO-LOCK NO-ERROR.
     IF NOT AVAIL emitente THEN
        FIND emitente WHERE 
             STRING(emitente.cod-emit) = INPUT FRAME {&FRAME-NAME} fi-cliente NO-LOCK NO-ERROR.
    
     IF AVAIL emitente THEN
        ASSIGN fi-cidade-cli:SCREEN-VALUE IN FRAME {&FRAME-NAME} = emitente.cidade
               fi-uf:SCREEN-VALUE = emitente.estado.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cliente w-window
ON F5 OF fi-cliente IN FRAME F-Main /* Cliente */
DO:
    {include/zoomvar.i &prog-zoom=adzoom\z02ad098.w
                     &campo=fi-cliente
                     &campozoom=nome-abrev}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cliente w-window
ON LEAVE OF fi-cliente IN FRAME F-Main /* Cliente */
DO:
   ASSIGN fi-cidade-cli:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ''
          fi-uf:SCREEN-VALUE = ''
          fi-cod-rep:SCREEN-VALUE = ''
          fi-nome-rep:SCREEN-VALUE = ''
          fi-perc-comis:SCREEN-VALUE = ''
          fi-natur-oper:SCREEN-VALUE = ''
          fi-denominacao:SCREEN-VALUE = ''
          bt-cond-esp:SENSITIVE = NO
          fi-nome-transp:SCREEN-VALUE = ''
          fi-nome-tr-red:SCREEN-VALUE = ''.

   IF KEYFUNCTION(LASTKEY) = 'back-tab' THEN DO.
      APPLY 'BACK-TAB' TO SELF.
      RETURN NO-APPLY.
   END.

   IF SELF:SCREEN-VALUE = '' THEN DO.
      MESSAGE 'Cliente deve ser Informado....'
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      APPLY 'ENTRY' TO SELF.
      RETURN NO-APPLY.
   END.

   FIND emitente WHERE 
        emitente.nome-abrev = INPUT FRAME {&FRAME-NAME} fi-cliente NO-LOCK NO-ERROR.
   IF NOT AVAIL emitente THEN
      FIND emitente WHERE 
           emitente.cod-emit = INTEGER(INPUT FRAME {&FRAME-NAME} fi-cliente) NO-LOCK NO-ERROR.

   IF NOT AVAIL emitente THEN DO.
      MESSAGE 'Cliente n∆o Cadastrado....'
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      APPLY 'ENTRY' TO SELF.
      RETURN NO-APPLY.
   END.

   ASSIGN SELF:SCREEN-VALUE = emitente.nome-abrev
          fi-cidade-cli:SCREEN-VALUE IN FRAME {&FRAME-NAME} = emitente.cidade
          fi-uf:SCREEN-VALUE IN FRAME {&FRAME-NAME} = emitente.estado
          fi-nome-tr-red:SCREEN-VALUE = emitente.nome-tr-red.

   IF emitente.identific = 2 THEN DO:
      MESSAGE "C¢digo informado Ç de um Fornecedor, Favor alter†-lo para Cliente..."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY 'ENTRY' TO SELF.
      RETURN NO-APPLY.
   END.

   IF emitente.ind-cre-cli = 4 THEN DO.
      MESSAGE emitente.observacoes
          VIEW-AS ALERT-BOX ERROR BUTTONS OK
          TITLE 'Cliente Suspenso para Implantaá∆o/Efetivaá∆o de Pedidos'.

      /*RUN pdp/pd0806a.p (INPUT-OUTPUT emitente.observacoes).*/

      APPLY 'entry' TO SELF.
      RETURN NO-APPLY.
   END.

   IF emitente.ind-sit-emitente = 2 THEN DO.
      MESSAGE 'Cliente Bloqueado....'
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      APPLY 'entry' TO SELF.
      RETURN NO-APPLY.
   END.


   IF cb-tp-pedido:SCREEN-VALUE MATCHES '*Exportaá∆o*' AND
      emitente.pais = 'Brasil' THEN DO.
      MESSAGE 'Tipo de Pedido indevido para Clientes de Mercado Interno...'
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      APPLY 'ENTRY' TO SELF.
      RETURN NO-APPLY.
   END.

   IF NOT cb-tp-pedido:SCREEN-VALUE MATCHES '*Exportaá∆o*' AND
      emitente.pais <> 'Brasil' THEN DO.
      MESSAGE 'Para Clientes de Mercado Externo, Pedidos devem ser de Exportaá∆o...'
           VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      APPLY 'ENTRY' TO SELF.
      RETURN NO-APPLY.
   END.

   FIND FIRST unid-feder WHERE
              unid-feder.estado = emitente.estado NO-LOCK NO-ERROR.

   FIND repres WHERE
        repres.cod-rep = emitente.cod-rep NO-LOCK NO-ERROR.
   IF AVAIL repres THEN
      ASSIGN fi-cod-rep:SCREEN-VALUE = repres.nome-abrev
             fi-nome-rep:SCREEN-VALUE = repres.nome
             fi-perc-comis:SCREEN-VALUE = STRING(repres.comis-direta).

   IF emitente.natureza = 1 THEN
      ASSIGN cb-prioridade:SCREEN-VALUE = "12" 
             cb-prioridade:SENSITIVE = NO.

   IF cb-tp-preco:SCREEN-VALUE = '1' THEN
      ASSIGN c-tab-preco = ' '.
   ELSE DO.
      FIND tb-preco WHERE
           tb-preco.nr-tabpre = emitente.nr-tabpre AND
           tb-preco.situacao = 1 NO-LOCK NO-ERROR.
      IF AVAIL tb-preco THEN
         ASSIGN c-tab-preco = emitente.nr-tabpre.
   END.
   ASSIGN cb-tab-preco:SCREEN-VALUE = c-tab-preco.

   ASSIGN fi-natur-oper = IF emitente.estado <> estabelec.estado 
                          THEN emitente.nat-ope-ext
                          ELSE emitente.nat-operacao.

   IF emitente.estado <> estabelec.estado THEN
      OVERLAY(fi-natur-oper,1,1) = '6'.
   ELSE
      OVERLAY(fi-natur-oper,1,1) = '5'.

   ASSIGN fi-natur-oper:SCREEN-VALUE = fi-natur-oper.

   /* Calcula a Natureza de Operaá∆o  */
   IF cb-tp-pedido:SCREEN-VALUE <> 'Operaá∆o Triangular' THEN DO.
      ASSIGN fi-natur-oper:SCREEN-VALUE = emitente.nat-oper.
      RUN pi-calc-natur-oper.  
   END.
   

   IF cb-tp-pedido:SCREEN-VALUE <> '∑ Vista' THEN DO.
      FIND cond-pagto WHERE
           cond-pagto.cod-cond-pag = emitente.cod-cond-pag NO-LOCK NO-ERROR.
      IF AVAIL cond-pagto THEN
         ASSIGN bt-cond-esp:SENSITIVE = IF cond-pagto.cod-cond-pag > 1 THEN YES ELSE NO
                fi-cod-cond-pag:SCREEN-VALUE = STRING(cond-pagto.cod-cond-pag)
                fi-desc-cond-pag:SCREEN-VALUE = cond-pagto.descricao.
   END.

   FIND natur-oper WHERE
        natur-oper.nat-operacao = fi-natur-oper:SCREEN-VALUE NO-LOCK NO-ERROR.
   IF AVAIL natur-oper THEN DO.
      ASSIGN fi-denominacao:SCREEN-VALUE = natur-oper.denominacao.

      IF natur-oper.emite-duplic = NO THEN
         ASSIGN fi-cod-cond-pag:SCREEN-VALUE = ''
                fi-desc-cond-pag:SCREEN-VALUE = ''
                fi-cod-cond-pag:SENSITIVE = NO
                bt-cond-esp:SENSITIVE = NO.
   END.

   FIND LAST ped-venda WHERE
             ped-venda.nome-abrev = emitente.nome-abrev 
             USE-INDEX ch-implant NO-LOCK NO-ERROR.
   IF AVAIL ped-venda THEN DO:
      ASSIGN fi-nome-transp:SCREEN-VALUE = ped-venda.nome-transp.
   END.
   ELSE DO:
      FIND transporte WHERE
           transporte.cod-transp = emitente.cod-transp NO-LOCK NO-ERROR.
      IF AVAIL transporte THEN
         ASSIGN fi-nome-transp:SCREEN-VALUE = transporte.nome-abrev.
      
      FIND estab-cli WHERE
           estab-cli.cod-estabel = para-ped.estab-padrao AND
           estab-cli.nome-abrev = emitente.nome-abrev NO-LOCK NO-ERROR.
      IF AVAIL estab-cli THEN
         ASSIGN fi-nome-transp:SCREEN-VALUE = estab-cli.nome-transp.
   END.

   IF fi-nr-pedido:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '?' THEN DO.
      ASSIGN fi-nr-pedido:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(NEXT-VALUE(seq-nr-pedido))
             fi-sit-cred:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "N∆o Avaliado".

      FOR EACH tt-cond-ped.
          DELETE tt-cond-ped.
      END.
   END.

   FOR LAST b-ped-venda WHERE
            b-ped-venda.cod-sit-ped <= 3 AND
            b-ped-venda.nome-abrev = SELF:SCREEN-VALUE NO-LOCK
         BY b-ped-venda.nr-pedido.
      ASSIGN ed-obs:SCREEN-VALUE = b-ped-venda.observacoes.
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cliente w-window
ON MOUSE-SELECT-DBLCLICK OF fi-cliente IN FRAME F-Main /* Cliente */
DO:
   APPLY 'F5' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cliente-tri
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cliente-tri w-window
ON ENTRY OF fi-cliente-tri IN FRAME F-Main /* Cli Rem Tri */
DO:
  FIND b-emitente WHERE 
       b-emitente.nome-abrev = INPUT FRAME {&FRAME-NAME} fi-cliente-tri NO-LOCK NO-ERROR.
  IF NOT AVAIL b-emitente THEN
     FIND b-emitente WHERE 
          STRING(b-emitente.cod-emit) = INPUT FRAME {&FRAME-NAME} fi-cliente-tri NO-LOCK NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cliente-tri w-window
ON LEAVE OF fi-cliente-tri IN FRAME F-Main /* Cli Rem Tri */
DO:
   IF KEYFUNCTION(LASTKEY) = 'back-tab' THEN
      APPLY 'LEAVE' TO SELF.

   IF cb-tp-pedido:SCREEN-VALUE = 'Operaá∆o Triangular' AND
      SELF:SCREEN-VALUE = '' THEN DO.
      MESSAGE 'Cliente Remessa Triangular deve ser Informado....'
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      APPLY 'ENTRY' TO SELF.
      RETURN NO-APPLY.
   END.

   IF SELF:SCREEN-VALUE <> '' THEN DO.
      FIND b-emitente WHERE 
           b-emitente.nome-abrev = INPUT FRAME {&FRAME-NAME} fi-cliente-tri NO-LOCK NO-ERROR.
      IF NOT AVAIL b-emitente THEN
         FIND b-emitente WHERE 
              b-emitente.cod-emit = INTEGER(INPUT FRAME {&FRAME-NAME} fi-cliente-tri) NO-LOCK NO-ERROR.
    
      IF NOT AVAIL b-emitente THEN DO.
         MESSAGE 'Cliente Remessa Triangular n∆o Cadastrado....'
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY 'ENTRY' TO SELF.
         RETURN NO-APPLY.
      END.
      ASSIGN SELF:SCREEN-VALUE = UPPER(b-emitente.nome-abrev)
             fi-cidade-cli-tri:SCREEN-VALUE IN FRAME {&FRAME-NAME} = b-emitente.cidade
             fi-uf-tri:SCREEN-VALUE IN FRAME {&FRAME-NAME} = b-emitente.estado.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cliente-tri w-window
ON MOUSE-SELECT-DBLCLICK OF fi-cliente-tri IN FRAME F-Main /* Cli Rem Tri */
DO:
  {include/zoomvar.i &prog-zoom=adzoom\z02ad098.w
                     &campo=fi-cliente-tri
                     &campozoom=nome-abrev}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-cond-pag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-cond-pag w-window
ON LEAVE OF fi-cod-cond-pag IN FRAME F-Main /* Cond. Pagto */
DO:
    IF KEYFUNCTION(LASTKEY) = 'back-tab' THEN DO.
       APPLY 'BACK-TAB' TO SELF.
       RETURN NO-APPLY.
    END.

    IF fi-cod-cond-pag <> fi-cod-cond-pag:INPUT-VALUE THEN DO. /* Alterou Condiá∆o de Pagamento */
       FOR EACH tt-cond-ped.
           DELETE tt-cond-ped.
       END.
    END.
    ASSIGN fi-desc-cond-pag:SCREEN-VALUE = ''.

    FIND emitente WHERE 
         emitente.nome-abrev = INPUT FRAME {&FRAME-NAME} fi-cliente NO-LOCK NO-ERROR.

    FIND cond-pagto WHERE
         cond-pagto.cod-cond-pag = INPUT FRAME {&FRAME-NAME} fi-cod-cond-pag
         NO-LOCK NO-ERROR.

    IF AVAIL cond-pagto THEN DO.
       IF (cb-tp-pedido:SCREEN-VALUE = '∑ Vista' AND cond-pagto.cod-cond-pag >= 3) OR
          (LOOKUP(cb-tp-pedido:SCREEN-VALUE,"∑ Vista,Operaá∆o Triangular,Exportaá∆o,Amostra,Amostra Exportaá∆o") = 0 AND
           cond-pagto.cod-cond-pag < 3) THEN DO.
          MESSAGE "Condiá∆o de Pagamento Indevida para Tipo de Pedido... "
                  VIEW-AS ALERT-BOX ERROR BUTTONS OK.
          APPLY 'ENTRY' TO SELF.
          RETURN NO-APPLY.
       END.

       IF cond-pagto.ind-situacao = 2 THEN DO.
          MESSAGE 'Condiá∆o de Pagamento INATIVA...'
              VIEW-AS ALERT-BOX ERROR BUTTONS OK.
          APPLY 'ENTRY' TO SELF.
          RETURN NO-APPLY.
       END.
      
       IF emitente.ind-cre-cli = 5 AND
          cond-pagto.cod-cond-pag > 3 OR cond-pagto.cod-cond-pag = 0 THEN DO.
          MESSAGE 'Cliente suspenso para Vendas Ö Prazo...'
              VIEW-AS ALERT-BOX ERROR BUTTONS OK.
          APPLY 'entry' TO SELF.
          RETURN NO-APPLY.
       END.

       ASSIGN cb-tipo-pagto:SCREEN-VALUE = 'Normal'.
       IF cond-pagto.log-2 THEN
          ASSIGN cb-tipo-pagto:SCREEN-VALUE = 'Vendor'.

       ASSIGN bt-cond-esp:SENSITIVE = IF cond-pagto.cod-cond-pag > 3 AND
                                         user-coml.inf-condesp
                                      THEN YES ELSE NO
              fi-desc-cond-pag:SCREEN-VALUE = cond-pagto.descricao.

       ASSIGN de-ind-finan = 1.
       IF cond-pagto.nr-tab-finan <> 0 AND
          cond-pagto.nr-ind-finan <> 0 THEN DO.
          FIND tab-finan WHERE
               tab-finan.nr-tab-finan = cond-pagto.nr-tab-finan NO-LOCK NO-ERROR.
          IF tab-finan.tab-ind-fin[cond-pagto.nr-ind-finan] <> 0 THEN
             ASSIGN de-ind-finan = tab-finan.tab-ind-fin[cond-pagto.nr-ind-finan].
       END.
    END.
    ELSE DO. 
       IF user-coml.inf-condesp THEN DO.
          IF emitente.ind-cre-cli = 5 THEN DO.
             MESSAGE 'Cliente suspenso para Vendas Ö Prazo...'
                 VIEW-AS ALERT-BOX ERROR BUTTONS OK.
             APPLY 'entry' TO SELF.
             RETURN NO-APPLY.
          END.

          ASSIGN bt-cond-esp:SENSITIVE = YES.
          APPLY 'choose' TO bt-cond-esp.

          FIND FIRST tt-cond-ped NO-ERROR.
          IF NOT AVAIL tt-cond-ped THEN DO.
             APPLY 'ENTRY' TO SELF.
             RETURN NO-APPLY.
          END.
       END.
       ELSE DO.
          MESSAGE 'Condiá∆o de Pagamento n∆o Cadastrada...'
              VIEW-AS ALERT-BOX ERROR BUTTONS OK.
          APPLY 'ENTRY' TO SELF.
          RETURN NO-APPLY.
       END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-cond-pag w-window
ON MOUSE-SELECT-DBLCLICK OF fi-cod-cond-pag IN FRAME F-Main /* Cond. Pagto */
DO:
  {include/zoomvar.i &prog-zoom = adzoom\z01ad039.w
                     &campo     = fi-cod-cond-pag
                     &campozoom = cod-cond-pag}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-cond-pag w-window
ON VALUE-CHANGED OF fi-cod-cond-pag IN FRAME F-Main /* Cond. Pagto */
DO:
   ASSIGN bt-cond-esp:SENSITIVE = NO
          fi-desc-cond-pag:SCREEN-VALUE = ''.
   IF SELF:INPUT-VALUE <> 0 AND SELF:INPUT-VALUE <> 1 THEN DO.
      FIND cond-pagto WHERE
           cond-pagto.cod-cond-pag = INPUT FRAME {&FRAME-NAME} fi-cod-cond-pag
           NO-LOCK NO-ERROR.
      IF AVAIL cond-pagto THEN 
         ASSIGN bt-cond-esp:SENSITIVE = YES
                fi-desc-cond-pag:SCREEN-VALUE = cond-pagto.descricao.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-estabel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-estabel w-window
ON LEAVE OF fi-cod-estabel IN FRAME F-Main /* Estabelecimento */
DO:
  FIND estabelec WHERE
       estabelec.cod-estabel = INPUT FRAME {&FRAME-NAME} fi-cod-estabel 
       NO-LOCK NO-ERROR.
  IF NOT AVAIL estabelec THEN DO.
     MESSAGE 'Estabelecimento n∆o Cadastrado....'
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
     APPLY 'ENTRY' TO SELF.
     RETURN NO-APPLY.
  END.
  ASSIGN fi-nome-estabel:SCREEN-VALUE = estabelec.nome.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-estabel w-window
ON MOUSE-SELECT-DBLCLICK OF fi-cod-estabel IN FRAME F-Main /* Estabelecimento */
DO:
  {include/zoomvar.i &prog-zoom=adzoom\z02ad098.w
                     &campo=fi-cod-estabel
                     &campozoom=cod-estabel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-informado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-informado w-window
ON LEAVE OF fi-cod-informado IN FRAME F-Main
DO:
   IF SELF:INPUT-VALUE = '' THEN DO.
      MESSAGE 'Para prioridade 99, o C¢digo deve ser Informado...'
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY 'entry' TO SELF.
      RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-rep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-rep w-window
ON LEAVE OF fi-cod-rep IN FRAME F-Main /* Representante */
DO:
   IF KEYFUNCTION(LASTKEY) = 'back-tab' THEN DO.
      APPLY 'BACK-TAB' TO SELF.
      RETURN NO-APPLY.
   END.

   ASSIGN fi-nome-rep:SCREEN-VALUE = ''.

   FIND repres WHERE
        repres.cod-rep = INTEGER(INPUT FRAME {&FRAME-NAME} fi-cod-rep) NO-LOCK NO-ERROR.
   IF NOT AVAIL repres THEN
      FIND repres WHERE
           repres.nome-abrev = INPUT FRAME {&FRAME-NAME} fi-cod-rep NO-LOCK NO-ERROR.

   IF NOT AVAIL repres THEN DO.
      MESSAGE 'Representante n∆o Cadastrado....'
           VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      APPLY 'ENTRY' TO SELF.
      RETURN NO-APPLY.
   END.
   ASSIGN fi-cod-rep:SCREEN-VALUE = repres.nome-abrev
          fi-nome-rep:SCREEN-VALUE = repres.nome
          fi-perc-comis:SCREEN-VALUE = STRING(repres.comis-direta).

   ASSIGN c-lst-preposto = "".
   FOR EACH user-web WHERE
            user-web.usuario = repres.nome-abrev AND
            user-web.tp-usuario = 5 /*Preposto*/
            NO-LOCK.
       ASSIGN c-lst-preposto = IF c-lst-preposto = ""
                               THEN user-web.nome
                               ELSE c-lst-preposto + "," + user-web.nome.
   END.
   ASSIGN cb-preposto:LIST-ITEMS = "," + c-lst-preposto.

   IF c-lst-preposto <> '' THEN
      ASSIGN cb-preposto:SENSITIVE = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-rep w-window
ON MOUSE-SELECT-DBLCLICK OF fi-cod-rep IN FRAME F-Main /* Representante */
DO:
  {include/zoomvar.i &prog-zoom = adzoom\z01ad229.w
                     &campo     = fi-cod-rep
                     &campozoom = nome-abrev}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-rota
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-rota w-window
ON LEAVE OF fi-cod-rota IN FRAME F-Main /* Rota */
DO:
  IF fi-cod-rota:SCREEN-VALUE <> '' THEN DO.
     FIND rota WHERE 
          rota.cod-rota = INPUT FRAME {&FRAME-NAME} fi-cod-rota NO-LOCK NO-ERROR.

     IF NOT AVAIL rota THEN DO.
        MESSAGE 'Rota n∆o Cadastrada....'
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        APPLY 'ENTRY' TO SELF.
        RETURN NO-APPLY.
     END.

     FIND FIRST unid-feder WHERE
                unid-feder.estado = SUBSTR(rota.roteiro,1,2) NO-LOCK NO-ERROR.
     IF NOT AVAIL unid-feder THEN DO.
        MESSAGE 'Roteiro da Rota est† Inv†lido (CD0706)....'
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        APPLY 'ENTRY' TO SELF.
        RETURN NO-APPLY.
     END.
     ASSIGN fi-desc-rota:SCREEN-VALUE IN FRAME {&FRAME-NAME} = SUBSTR(rota.roteiro,1,2) + "-" + 
                                                               rota.descricao.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-rota w-window
ON MOUSE-SELECT-DBLCLICK OF fi-cod-rota IN FRAME F-Main /* Rota */
DO:
  {include/zoomvar.i &prog-zoom = dizoom\z01di181.w
                     &campo     = fi-cod-rota
                     &campozoom = cod-rota}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-data-base
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-data-base w-window
ON LEAVE OF fi-data-base IN FRAME F-Main /* Dt Base Fat. */
DO:
   IF KEYFUNCTION(LASTKEY) = 'BACK-TAB' THEN DO.
      APPLY 'ENTRY' TO fi-dt-entrega.
      RETURN NO-APPLY.
   END.

   IF SELF:SCREEN-VALUE <> '' AND 
      SELF:INPUT-VALUE < TODAY THEN DO.
      MESSAGE 'Data Base n∆o Pode ser menor que a Data da Inclus∆o'
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      APPLY 'ENTRY' TO SELF.
      RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-dt-entrega
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-dt-entrega w-window
ON LEAVE OF fi-dt-entrega IN FRAME F-Main
DO:
   IF SELF:INPUT-VALUE = ? OR 
      KEYFUNCTION(LASTKEY) = 'BACK-TAB' THEN DO.
      APPLY 'ENTRY' TO cb-tp-entrega.
      RETURN NO-APPLY.
   END.

   IF INPUT FRAME {&FRAME-NAME} fi-dt-entrega < TODAY THEN DO.
      MESSAGE 'Data de Entrega n∆o Pode ser menor que a Data da Inclus∆o'
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      APPLY 'ENTRY' TO SELF.
      RETURN NO-APPLY.
   END.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-moeda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-moeda w-window
ON ENTRY OF fi-moeda IN FRAME F-Main /* Moeda */
DO:
   FIND moeda WHERE
        moeda.mo-codigo = INPUT FRAME {&FRAME-NAME} fi-moeda NO-LOCK NO-ERROR.
   IF AVAIL moeda THEN
      ASSIGN fi-desc-moeda:SCREEN-VALUE = moeda.descricao.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-moeda w-window
ON LEAVE OF fi-moeda IN FRAME F-Main /* Moeda */
DO:
   FIND moeda WHERE
        moeda.mo-codigo = INPUT FRAME {&FRAME-NAME} fi-moeda NO-LOCK NO-ERROR.
   IF NOT AVAIL moeda THEN DO.
      MESSAGE 'Moeda n∆o Cadastrada...'
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      APPLY 'ENTRY' TO SELF.
      RETURN NO-APPLY.
   END.

   ASSIGN fi-desc-moeda:SCREEN-VALUE = moeda.descricao.

   IF cb-tp-pedido:SCREEN-VALUE MATCHES '*Exportaá∆o*' AND
      INPUT FRAME {&FRAME-NAME} fi-moeda = 0 THEN DO.
      MESSAGE 'Moeda indevida para Tipo de Pedido...'
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      APPLY 'ENTRY' TO SELF.
      RETURN NO-APPLY.
   END.

   IF NOT cb-tp-pedido:SCREEN-VALUE MATCHES '*Exportaá∆o*' AND
      INPUT FRAME {&FRAME-NAME} cb-ext-tp-pedido <> 'PI' AND
      INPUT FRAME {&FRAME-NAME} fi-moeda <> 0 THEN DO.

      MESSAGE 'Moeda indevida para Tipo de Pedido...'
           VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      APPLY 'ENTRY' TO SELF.
      RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-natur-oper
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-natur-oper w-window
ON LEAVE OF fi-natur-oper IN FRAME F-Main /* Natur Oper */
DO:
   IF KEYFUNCTION(LASTKEY) = 'back-tab' THEN DO.
      APPLY 'BACK-TAB' TO SELF.
      RETURN NO-APPLY.
   END.

   IF SELF:SCREEN-VALUE = '' THEN DO.
      MESSAGE 'Natureza de Operaá∆o deve ser Informada....'
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      APPLY 'ENTRY' TO SELF.
      RETURN NO-APPLY.
   END.

   ASSIGN fi-denominacao:SCREEN-VALUE = ''.
   FIND natur-oper WHERE
        natur-oper.nat-operacao = INPUT FRAME {&FRAME-NAME} fi-natur-oper NO-LOCK NO-ERROR.

   IF NOT AVAIL natur-oper THEN DO.
      MESSAGE 'Natureza de Operaá∆o n∆o Cadastrada....'
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      APPLY 'ENTRY' TO SELF.
      RETURN NO-APPLY.
   END.
   ASSIGN fi-denominacao:SCREEN-VALUE = natur-oper.denominacao.

   IF (fi-cliente-tri:SCREEN-VALUE <> '' AND natur-oper.log-oper-triang = NO) OR
      (fi-cliente-tri:SCREEN-VALUE = '' AND natur-oper.log-oper-triang = YES) THEN DO.
      MESSAGE 'Natureza de Operaá∆o Indevida....' SKIP
              'Favor Verificar o Tipo de Pedido e/ou Operaá∆o Tringular'
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      APPLY 'ENTRY' TO SELF.
      RETURN NO-APPLY.
   END.

   IF cb-tp-pedido:SCREEN-VALUE MATCHES 'Rem.Industrializacao' AND 
      natur-oper.terceiros = NO THEN DO.
      MESSAGE 'Natureza de Operaá∆o Indevida para Tipo de Pedido....'
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      APPLY 'ENTRY' TO SELF.
      RETURN NO-APPLY.
   END.

   IF cb-tp-pedido:SCREEN-VALUE MATCHES '*Exportaá∆o*' AND
      NOT INPUT FRAME {&FRAME-NAME} fi-natur-oper BEGINS "7" THEN DO.
      MESSAGE 'Natureza de Operaá∆o indevida para Tipo de Pedido...'
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      APPLY 'ENTRY' TO SELF.
      RETURN NO-APPLY.
   END.

   IF NOT cb-tp-pedido:SCREEN-VALUE MATCHES '*Exportaá∆o*' AND
      INPUT FRAME {&FRAME-NAME} fi-natur-oper BEGINS "7" THEN DO.
      MESSAGE 'Natureza de Operaá∆o indevida para Tipo de Pedido...'
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      APPLY 'ENTRY' TO SELF.
      RETURN NO-APPLY.
   END.
   ASSIGN fi-cod-cond-pag:SENSITIVE = YES.

   IF cb-tp-pedido:SCREEN-VALUE = '∑ Vista' THEN
      ASSIGN fi-cod-cond-pag:SENSITIVE = NO.

   IF natur-oper.emite-duplic = NO THEN
      ASSIGN fi-cod-cond-pag:SCREEN-VALUE = ''
             fi-cod-cond-pag:SENSITIVE = NO
             fi-desc-cond-pag:SCREEN-VALUE = ''.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-natur-oper w-window
ON MOUSE-SELECT-DBLCLICK OF fi-natur-oper IN FRAME F-Main /* Natur Oper */
DO:
  {include/zoomvar.i &prog-zoom = inzoom\z01in245.w
                     &campo     = fi-natur-oper
                     &campozoom = nat-operacao}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-nome-tr-red
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nome-tr-red w-window
ON LEAVE OF fi-nome-tr-red IN FRAME F-Main /* Redesp. */
DO:
   IF fi-nome-tr-red:SCREEN-VALUE <> '' THEN DO.
      FIND transporte WHERE 
           transporte.nome-abrev = INPUT FRAME {&FRAME-NAME} fi-nome-tr-red NO-LOCK NO-ERROR.
      IF NOT AVAIL transporte THEN
         FIND transporte WHERE 
              STRING(transporte.cod-transp) = INPUT FRAME {&FRAME-NAME} fi-nome-tr-red NO-LOCK NO-ERROR.
    
      IF NOT AVAIL transporte THEN DO.
         MESSAGE 'Transportadora n∆o Cadastrada....'
             VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY 'ENTRY' TO SELF.
         RETURN NO-APPLY.
      END.
      ASSIGN fi-nome-tr-red:SCREEN-VALUE IN FRAME {&FRAME-NAME} = transporte.nome-abrev.

      ASSIGN cb-tp-frete:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Cif atÇ Redesp".
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nome-tr-red w-window
ON MOUSE-SELECT-DBLCLICK OF fi-nome-tr-red IN FRAME F-Main /* Redesp. */
DO:
  {include/zoomvar.i &prog-zoom = adzoom\z01ad268.w
                     &campo     = fi-nome-tr-red
                     &campozoom = nome-abrev}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-nome-transp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nome-transp w-window
ON LEAVE OF fi-nome-transp IN FRAME F-Main /* Transportador */
DO:
   IF KEYFUNCTION(LASTKEY) = 'back-tab' THEN DO.
      APPLY 'BACK-TAB' TO SELF.
      RETURN NO-APPLY.
   END.

   IF SELF:SCREEN-VALUE = '' THEN DO.
      MESSAGE 'Transportadora deve ser Informada....'
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      APPLY 'ENTRY' TO SELF.
      RETURN NO-APPLY.
   END.

   FIND transporte WHERE 
        transporte.nome-abrev = INPUT FRAME {&FRAME-NAME} fi-nome-transp NO-LOCK NO-ERROR.
   IF NOT AVAIL transporte THEN
      FIND transporte WHERE 
           STRING(transporte.cod-transp) = INPUT FRAME {&FRAME-NAME} fi-nome-transp NO-LOCK NO-ERROR.

   IF NOT AVAIL transporte THEN DO.
      MESSAGE 'Transportadora n∆o Cadastrada....'
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      APPLY 'ENTRY' TO SELF.
      RETURN NO-APPLY.
   END.
   ASSIGN fi-nome-transp:SCREEN-VALUE IN FRAME {&FRAME-NAME} = transporte.nome-abrev.

   IF estabelec.cod-estabel = '1' AND
      transporte.estado <> 'MG' THEN DO.
      MESSAGE "                   A T E N Ä « O !!!! " SKIP(1)
              "A Transportadora " transporte.nome-abrev " n∆o Ç do Estado de Minas Gerais," SKIP
              "indicamos contrat†-la como Redespacho..." SKIP(1)
              "Confirma Contratataá∆o dessa Transportadora ?"
              VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                      TITLE "Transportadora Indevida !!!" UPDATE l-transp-ima AS LOGICAL.

      IF NOT l-transp-ima THEN DO.
         APPLY 'ENTRY' TO SELF.
         RETURN NO-APPLY.
      END.
   END.

   IF estabelec.cod-estabel = '5' AND
      transporte.estado <> 'ES' THEN DO.
      MESSAGE "                   A T E N Ä « O !!!! " SKIP(1)
              "A Transportadora " transporte.nome-abrev " n∆o Ç do Estado do Esperito Santo," SKIP
              "indicamos contrat†-la como Redespacho..." SKIP(1)
              "Confirma Contratataá∆o dessa Transportadora ?"
              VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                      TITLE "Transportadora Indevida !!!" UPDATE l-transp-med AS LOGICAL.

      IF NOT l-transp-med THEN DO.
         APPLY 'ENTRY' TO SELF.
         RETURN NO-APPLY.
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nome-transp w-window
ON MOUSE-SELECT-DBLCLICK OF fi-nome-transp IN FRAME F-Main /* Transportador */
DO:
  {include/zoomvar.i &prog-zoom = adzoom\z01ad268.w
                     &campo     = fi-nome-transp
                     &campozoom = nome-abrev}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-nr-container
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nr-container w-window
ON LEAVE OF fi-nr-container IN FRAME F-Main /* Container */
DO:
   IF KEYFUNCTION(LASTKEY) = 'back-tab' THEN DO.
      APPLY 'BACK-TAB' TO SELF.
      RETURN NO-APPLY.
   END.

   IF SELF:SCREEN-VALUE = '0' THEN DO.
      MESSAGE 'Container deve ser Informada....'
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      APPLY 'ENTRY' TO SELF.
      RETURN NO-APPLY.
   END.

   FIND pp-container WHERE
        pp-container.nr-container = INPUT FRAME {&FRAME-NAME} fi-nr-container AND
        pp-container.cod-estabel  = INPUT FRAME {&FRAME-NAME} fi-cod-estabel
        NO-LOCK NO-ERROR.

   IF NOT AVAIL pp-container THEN DO.
      MESSAGE "Container n∆o Cadastrado para esse Estabelecimento..." SKIP
              VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      APPLY 'entry' TO fi-nr-container.
      RETURN NO-APPLY.
   END.

   CASE pp-container.situacao.
      WHEN 3 THEN DO.
         MESSAGE "Container j† foi FECHADO, Verifique..."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
         APPLY 'entry' TO fi-nr-container.
         RETURN NO-APPLY.
      END.
      WHEN 2 THEN DO.
          MESSAGE "Container est† SUSPENSO para venda....."
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
          APPLY 'entry' TO fi-nr-container.
          RETURN NO-APPLY.
      END.
   END CASE.

   FIND FIRST pp-it-container OF pp-container NO-LOCK NO-ERROR.
   IF NOT AVAIL pp-it-container THEN DO:
      MESSAGE "Container n∆o possui itens para venda"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY 'entry' TO fi-nr-container.
      RETURN NO-APPLY.
   END.

   ASSIGN fi-dt-entrega:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(pp-container.dt-prev-chegada).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nr-container w-window
ON MOUSE-SELECT-DBLCLICK OF fi-nr-container IN FRAME F-Main /* Container */
DO:
  {include/zoomvar.i &prog-zoom=eszoom\z02pp001.w
                     &campo=fi-nr-container
                     &campozoom=nr-container}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-perc-comis
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-perc-comis w-window
ON LEAVE OF fi-perc-comis IN FRAME F-Main /* %Comis */
DO:
   IF SELF:INPUT-VALUE > repres.comis-direta THEN DO.
      MESSAGE 'Comiss∆o Superior a Parametrizada para Representante...'
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY 'ENTRY' TO SELF.
      RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-reserva
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-reserva w-window
ON LEAVE OF fi-reserva IN FRAME F-Main /* Reserva */
DO:
   IF KEYFUNCTION(LASTKEY) = 'back-tab' OR
      (cb-tp-pedido:SCREEN-VALUE = 'Reserva' AND
      SELF:INPUT-VALUE = 0) THEN DO.
      APPLY 'ENTRY' TO cb-tp-pedido.
      RETURN NO-APPLY.
   END.

   IF SELF:INPUT-VALUE <> 0 THEN DO.
      ASSIGN bt-imp-res:SENSITIVE IN FRAME {&FRAME-NAME} = NO
             bt-copia-it-ped:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
    
      FIND ped-reserva WHERE
           ped-reserva.num-reserva = INPUT FRAME {&FRAME-NAME} fi-reserva 
           NO-LOCK NO-ERROR.
    
      IF NOT AVAIL ped-reserva THEN DO.
         MESSAGE "Reserva N∆o Cadastrada..."
             VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY 'entry' TO SELF.
         RETURN NO-APPLY.
      END.
    
      IF ped-reserva.situacao <> 1 THEN DO.
         MESSAGE "Reserva j† foi Encerrada, n∆o poder† ser Utilizada..."
             VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY 'entry' TO SELF.
         RETURN NO-APPLY.
      END.

      IF ped-reserva.cod-estabel <> INPUT FRAME {&FRAME-NAME} fi-cod-estabel THEN DO.
         MESSAGE "Estabelecimento da Reseva Ç diferente do Informado..."
               VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY 'entry' TO SELF.
         RETURN NO-APPLY.
      END.
    
      FIND emitente WHERE
           emitente.cod-emit = ped-reserva.cod-emit NO-LOCK NO-ERROR.
      IF AVAIL emitente THEN
         ASSIGN fi-cliente:SCREEN-VALUE IN FRAME {&FRAME-NAME} = emitente.nome-abrev
                fi-cidade-cli:SCREEN-VALUE IN FRAME {&FRAME-NAME} = emitente.cidade
                fi-uf:SCREEN-VALUE = emitente.estado.
    
      ASSIGN fi-nome-tr-red:SCREEN-VALUE = emitente.nome-tr-red
             /*tg-fat-parcial:SCREEN-VALUE = STRING(emitente.ind-fat-par) */.

      IF cb-tp-pedido:SCREEN-VALUE MATCHES '*Exportaá∆o*' AND
         emitente.pais = 'Brasil' THEN DO.
         MESSAGE 'N∆o Ç Permitido Tipo de Pedido Exportaá∆o para Clientes de Mercado Interno.' SKIP
             VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY 'ENTRY' TO SELF.
         RETURN NO-APPLY.
      END.

      IF NOT cb-tp-pedido:SCREEN-VALUE MATCHES '*Exportaá∆o*' AND
         emitente.pais <> 'Brasil' THEN DO.
         MESSAGE 'Cliente da Reserva Ç de Mercado Externo e o Tipo de Pedido n∆o Ç Exportaá∆o,' SKIP
                 'Imposs°vel utilizar essa reserva...'
              VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY 'ENTRY' TO SELF.
         RETURN NO-APPLY.
      END.

      FIND repres WHERE
           repres.cod-rep = ped-reserva.cod-rep NO-LOCK NO-ERROR.
      IF AVAIL repres THEN
         ASSIGN fi-cod-rep:SCREEN-VALUE IN FRAME {&FRAME-NAME} = repres.nome-abrev
                fi-nome-rep:SCREEN-VALUE IN FRAME {&FRAME-NAME} = repres.nome
                fi-ped-repres:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ''
                fi-perc-comis:SCREEN-VALUE = STRING(repres.comis-direta).

      FIND cond-pagto WHERE
           cond-pagto.cod-cond-pag = emitente.cod-cond-pag NO-LOCK NO-ERROR.
      IF AVAIL cond-pagto THEN
         ASSIGN bt-cond-esp:SENSITIVE = IF cond-pagto.cod-cond-pag > 1 THEN YES ELSE NO
                fi-cod-cond-pag:SCREEN-VALUE = STRING(cond-pagto.cod-cond-pag)
                fi-desc-cond-pag:SCREEN-VALUE = cond-pagto.descricao.

      IF cb-tp-pedido:SCREEN-VALUE <> 'Operaá∆o Triangular' THEN DO.
         FIND natur-oper WHERE
              natur-oper.nat-operacao = emitente.nat-oper NO-LOCK NO-ERROR.
         IF AVAIL natur-oper THEN
            ASSIGN fi-natur-oper:SCREEN-VALUE = natur-oper.nat-operacao
                   fi-denominacao:SCREEN-VALUE = natur-oper.denominacao.

         IF natur-oper.emite-duplic = NO THEN
            ASSIGN fi-cod-cond-pag:SCREEN-VALUE = ''
                   fi-desc-cond-pag:SCREEN-VALUE = ''
                   fi-cod-cond-pag:SENSITIVE = NO
                   bt-cond-esp:SENSITIVE = NO.
      END.

      FIND transporte WHERE
           transporte.cod-transp = emitente.cod-transp NO-LOCK NO-ERROR.
      IF AVAIL transporte THEN
         ASSIGN fi-nome-transp:SCREEN-VALUE = transporte.nome-abrev.

      FIND estab-cli WHERE
           estab-cli.cod-estabel = para-ped.estab-padrao AND
           estab-cli.nome-abrev = emitente.nome-abrev NO-LOCK NO-ERROR.
      IF AVAIL estab-cli THEN
         ASSIGN fi-nome-transp:SCREEN-VALUE = estab-cli.nome-transp.

      IF fi-nr-pedido:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '?' THEN
         ASSIGN fi-nr-pedido:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(NEXT-VALUE(seq-nr-pedido))
                fi-sit-cred:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "N∆o Avaliado".

      ASSIGN fi-cliente:SENSITIVE = NO
             fi-cod-rep:SENSITIVE = NO
             bt-imp-res:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
    
      APPLY 'choose' TO bt-imp-res.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-reserva w-window
ON MOUSE-SELECT-DBLCLICK OF fi-reserva IN FRAME F-Main /* Reserva */
DO:
  {include/zoomvar.i &prog-zoom = eszoom\z01es016.w
                     &campo     = fi-reserva
                     &campozoom = num-reserva}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-window 


/* ***************************  Main Block  *************************** */

ASSIGN c-tab-preco = ''.
FOR EACH tb-preco WHERE 
         tb-preco.situacao = 1 NO-LOCK.
    ASSIGN c-tab-preco = IF c-tab-preco = ''
                         THEN " ," + tb-preco.nr-tabpre
                         ELSE c-tab-preco + "," + tb-preco.nr-tabpre.
END.
ASSIGN cb-tab-preco:LIST-ITEMS = c-tab-preco.

/* Substitui TAB por ENTER */
ON 'RETURN':U ANYWHERE DO:
    APPLY 'TAB' TO SELF.
    RETURN NO-APPLY.
END.

ON 'F5':U, 'mouse-select-dblclick':U OF tt-itens-ped.it-codigo IN BROWSE {&browse-name} DO:
    IF INPUT FRAME {&FRAME-NAME} cb-ext-tp-pedido = 'PI' THEN DO.
       RUN esp/espp002b.w (OUTPUT c-it-container, 
                           INPUT INPUT FRAME {&FRAME-NAME} fi-cod-estabel, 
                           INPUT INPUT FRAME {&FRAME-NAME} fi-nr-container).
       IF c-it-container = "" THEN DO.
          APPLY 'ENTRY' TO SELF.
          RETURN NO-APPLY.
       END.
       ASSIGN tt-itens-ped.it-codigo:SCREEN-VALUE IN BROWSE {&browse-name} = c-it-container.
    END.
    ELSE
       RUN pi-zoom-item.
END.

ON 'leave':U OF tt-itens-ped.it-codigo IN BROWSE {&browse-name} DO:
    IF KEYFUNCTION(LASTKEY) = 'back-tab' THEN DO.
       APPLY 'END-ERROR' TO br-ped-item IN FRAME {&FRAME-NAME}.
       RETURN NO-APPLY.
    END.

    RUN pi-leave-item.
    IF RETURN-VALUE = 'ADM-ERROR' THEN
       RETURN NO-APPLY.
END.

ON 'leave':U OF tt-itens-ped.cod-refer IN BROWSE {&browse-name} DO:
    IF KEYFUNCTION(LASTKEY) = 'back-tab' THEN DO.
       APPLY 'BACK-TAB' TO SELF.
       RETURN NO-APPLY.
    END.

    RUN pi-leave-refer.
    IF RETURN-VALUE = 'ADM-ERROR' THEN
       RETURN NO-APPLY.
END.

ON 'F5':U, 'mouse-select-dblclick':U OF tt-itens-ped.cod-refer IN BROWSE {&browse-name} DO:
   RUN pi-zoom-refer.
END.

ON 'leave':U OF tt-itens-ped.qt-pedida IN BROWSE {&browse-name} DO:
    IF KEYFUNCTION(LASTKEY) = 'back-tab' THEN DO.
       APPLY 'BACK-TAB' TO SELF.
       RETURN NO-APPLY.
    END.

    RUN pi-leave-qtde.
    IF RETURN-VALUE = 'ADM-ERROR' THEN
       RETURN NO-APPLY.
END.

ON 'leave':U OF tt-itens-ped.vl-preori IN BROWSE {&browse-name} DO:
    IF KEYFUNCTION(LASTKEY) = 'back-tab' THEN DO.
       APPLY 'BACK-TAB' TO SELF.
       RETURN NO-APPLY.
    END.

    IF DEC(SELF:SCREEN-VALUE) = 0 THEN DO.
       MESSAGE 'Preáo Deve ser Informado...'
           VIEW-AS ALERT-BOX ERROR BUTTONS OK.
       APPLY 'entry' TO SELF.
       RETURN NO-APPLY.
    END.
END.

fi-cod-estabel:LOAD-MOUSE-POINTER("image/lupa.cur").
fi-cliente:LOAD-MOUSE-POINTER("image/lupa.cur").
fi-cliente-tri:LOAD-MOUSE-POINTER("image/lupa.cur").
fi-cod-rep:LOAD-MOUSE-POINTER("image/lupa.cur").
fi-natur-oper:LOAD-MOUSE-POINTER("image/lupa.cur").
fi-nome-transp:LOAD-MOUSE-POINTER("image/lupa.cur").
fi-nome-tr-red:LOAD-MOUSE-POINTER("image/lupa.cur").
fi-cod-rota:LOAD-MOUSE-POINTER("image/lupa.cur").
fi-cod-cond-pag:LOAD-MOUSE-POINTER("image/lupa.cur").
fi-moeda:LOAD-MOUSE-POINTER("image/lupa.cur").
fi-reserva:LOAD-MOUSE-POINTER("image/lupa.cur").
fi-nr-container:LOAD-MOUSE-POINTER("image/lupa.cur").

tt-itens-ped.it-codigo:LOAD-MOUSE-POINTER("image/lupa.cur") IN BROWSE {&browse-name}.
tt-itens-ped.cod-refer:LOAD-MOUSE-POINTER("image/lupa.cur") IN BROWSE {&browse-name}.

br-ped-item:NUM-LOCKED-COLUMNS = 4.

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-window  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-open-query-cases w-window 
PROCEDURE adm-open-query-cases :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
    /*
    ASSIGN fi-qtd-acum = 0
           fi-vlr-acum = 0.
    FOR EACH tt-ped-item.
        ASSIGN fi-qtd-acum = fi-qtd-acum + tt-ped-item.qt-pedida
               fi-vlr-acum = fi-vlr-acum + (TRUNC(tt-ped-item.vl-preuni,2) * tt-ped-item.qt-pedida).
    END.
    ASSIGN fi-qtd-acum:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(fi-qtd-acum)
           fi-vlr-acum:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(fi-vlr-acum).

    {&OPEN-QUERY-br-ped-item}
    */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-window  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-window  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-window)
  THEN DELETE WIDGET w-window.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-window  _DEFAULT-ENABLE
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
  DISPLAY cb-prioridade tg-nao-aprovar fi-nome-estabel fi-dt-implant 
          cb-tp-pedido fi-cod-estabel cb-ext-tp-pedido fi-reserva fi-cliente 
          fi-pedido-cli fi-cliente-tri fi-sit-cred fi-fat-max fi-cod-rep 
          fi-nome-rep fi-ped-repres cb-preposto fi-natur-oper fi-denominacao 
          fi-perc-comis fi-moeda fi-desc-moeda fi-cod-cond-pag fi-nr-pedido 
          fi-desc-cond-pag cb-tp-preco cb-tab-preco cb-tp-entrega fi-dt-entrega 
          cb-tipo-pagto fi-nome-transp fi-nome-tr-red fi-cod-rota fi-desc-rota 
          cb-tp-frete fi-data-base fi-nr-container ed-obs tg-bloqueio 
          fi-observ-nf fi-tot-qtd-ped fi-tot-qtd-res fi-tot-qtd-fat 
          fi-tot-vlr-ped fi-tot-vlr-fat fi-tot-vlr-abe fi-completo fi-sit-preco 
          fi-cidade-cli fi-cidade-cli-tri fi-uf fi-uf-tri 
      WITH FRAME F-Main IN WINDOW w-window.
  ENABLE br-ped-item cb-prioridade cb-tp-pedido fi-cod-estabel fi-cliente 
         fi-pedido-cli fi-fat-max fi-cod-rep fi-ped-repres fi-natur-oper 
         fi-perc-comis fi-cod-cond-pag fi-nr-pedido cb-tab-preco cb-tp-entrega 
         fi-nome-transp fi-nome-tr-red bt-log fi-cod-rota cb-tp-frete 
         fi-data-base ed-obs fi-observ-nf bt-inc bt-sair bt-cons-cliente-tri 
         RECT-1 RECT-2 RECT-3 RECT-4 
      WITH FRAME F-Main IN WINDOW w-window.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW w-window.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-window 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-window 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-window 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  {include/win-size.i}
  
  /*{utp/ut9000.i "ESPD4000" "2.04.00.000"} */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  FIND FIRST param-global NO-LOCK NO-ERROR.
  FIND FIRST para-fat NO-LOCK NO-ERROR.

  FIND FIRST para-ped NO-LOCK NO-ERROR.
  IF AVAIL para-ped THEN DO.
     FIND estabelec WHERE
          estabelec.cod-estab = para-ped.estab-padrao NO-LOCK NO-ERROR.
     ASSIGN fi-cod-estabel:SCREEN-VALUE IN FRAME {&FRAME-NAME} = estabelec.cod-estab
            fi-nome-estabel:SCREEN-VALUE IN FRAME {&FRAME-NAME} = estabelec.nome.
  END.

  FIND estabelec WHERE
       estabelec.cod-estabel = para-ped.estab-padrao NO-LOCK NO-ERROR.

  FIND user-coml WHERE 
       user-coml.usuario = c-seg-usuario NO-LOCK NO-ERROR.

  IF NOT AVAIL user-coml THEN DO.
     MESSAGE 'Usuario Comercial n∆o Cadastrado para ' c-seg-usuario
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     RETURN 'ADM-ERROR'.
  END.

  ASSIGN cb-tp-preco:SENSITIVE IN FRAME {&FRAME-NAME} = user-coml.inf-tppreco
         cb-ext-tp-pedido:SENSITIVE IN FRAME {&FRAME-NAME} = user-coml.inf-tpped.

  IF user-coml.inf-moeda THEN DO:
     APPLY "leave" TO fi-moeda IN FRAME {&FRAME-NAME}.
     ASSIGN fi-moeda:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
  END.
  ELSE DO.
     FIND moeda WHERE
          moeda.mo-codigo = 0 NO-LOCK NO-ERROR.

     IF AVAIL moeda THEN
        ASSIGN fi-moeda:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '0'. /* Real */
               fi-desc-moeda:SCREEN-VALUE = moeda.descricao.
  END.

  ASSIGN c-lst-prioridade = cb-prioridade:LIST-ITEMS.
  IF user-coml.inf-condesp THEN
     ASSIGN bt-cond-esp:SENSITIVE = YES
            c-lst-prioridade = "10,12,15,16,17,18,99".
  ASSIGN cb-prioridade:LIST-ITEMS = c-lst-prioridade
         cb-prioridade:SCREEN-VALUE = ENTRY(1,c-lst-prioridade).

  tt-itens-ped.it-codigo:READ-ONLY IN BROWSE br-ped-item = YES.
  tt-itens-ped.cod-refer:READ-ONLY IN BROWSE br-ped-item = YES.
  tt-itens-ped.qt-pedida:READ-ONLY IN BROWSE br-ped-item = YES.
  tt-itens-ped.vl-preori:READ-ONLY IN BROWSE br-ped-item = YES.

  ENABLE {&list-4}
         {&list-5}
         {&list-6}
         WITH FRAME {&FRAME-NAME}.

  ASSIGN tg-bloqueio:SENSITIVE = NO.
  FIND im-param WHERE
       im-param.cod-param = "USR_BLOQ_FAT" NO-LOCK NO-ERROR.

  IF AVAIL im-param THEN 
     IF LOOKUP(c-seg-usuario,im-param.val-param) > 0 THEN
        ASSIGN tg-bloqueio:SENSITIVE = YES.

  CASE p-acao:
      WHEN 'Incluir' THEN DO.
         RUN esapi/ret-udm.p (INPUT STRING(MONTH(TODAY),"99") + STRING(YEAR(TODAY),"9999"), 
                              OUTPUT c-dia).

         ASSIGN fi-cod-estabel:SCREEN-VALUE IN FRAME {&FRAME-NAME} = para-ped.estab-padrao
                fi-nome-estabel:SCREEN-VALUE IN FRAME {&FRAME-NAME} = estabelec.nome
                cb-tp-pedido:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Normal"
                fi-nr-pedido:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "?"
                fi-dt-implant:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY,"99/99/9999").

         IF SUBSTR(user-coml.char-1,1250,2) = 'PI' THEN
            ASSIGN cb-ext-tp-pedido:SCREEN-VALUE = 'PI'
                   fi-nr-container:SENSITIVE = YES
                   tg-nao-aprovar:SCREEN-VALUE = 'YES'.

         APPLY 'VALUE-CHANGED' TO cb-tp-pedido.
         ASSIGN bt-inc:SENSITIVE = YES
                bt-ok:SENSITIVE = YES.
      END.
      WHEN 'Modificar' THEN DO.
          FIND ped-venda WHERE
               ROWID(ped-venda) = gr-ped-venda EXCLUSIVE-LOCK NO-ERROR.

          RUN pi-display-fields.

          ASSIGN i-cod-sit-ped = ped-venda.cod-sit-ped
                 c-tab-preco = ped-venda.nr-tabpre.

          ASSIGN de-ind-finan = 1.
          IF AVAIL cond-pagto THEN DO.
             IF cond-pagto.nr-tab-finan <> 0 AND
                cond-pagto.nr-ind-finan <> 0 THEN DO.
                FIND tab-finan WHERE
                     tab-finan.nr-tab-finan = cond-pagto.nr-tab-finan NO-LOCK NO-ERROR.
                IF tab-finan.tab-ind-fin[cond-pagto.nr-ind-finan] <> 0 THEN
                   ASSIGN de-ind-finan = tab-finan.tab-ind-fin[cond-pagto.nr-ind-finan].
             END.
          END.
          ELSE DO.
             ASSIGN de-tot-prazo = 0
                    i-ct = 0.
             FOR EACH cond-ped WHERE
                      cond-ped.nr-pedido = ped-venda.nr-pedido NO-LOCK.
                 ASSIGN de-tot-prazo = de-tot-prazo + cond-ped.nr-dias-venc.
                 ASSIGN i-ct = i-ct + 1.
             END.
             ASSIGN i-prazo-medio = de-tot-prazo / i-ct.

             IF i-prazo-medio > 90 THEN DO.
                FIND tab-finan WHERE
                     tab-finan.nr-tab-finan = 1 NO-LOCK NO-ERROR.
                IF tab-finan.tab-ind-fin[1] <> 0 THEN
                   ASSIGN de-ind-finan = tab-finan.tab-ind-fin[4].
             END.
             ELSE DO.
                 FIND tab-finan WHERE
                      tab-finan.nr-tab-finan = 1 NO-LOCK NO-ERROR.
                 IF tab-finan.tab-ind-fin[1] <> 0 THEN
                    ASSIGN de-ind-finan = tab-finan.tab-ind-fin[4].
             END.
          END.

          IF de-ind-finan = 0 THEN
             ASSIGN de-ind-finan = 1.

          IF ped-venda.cod-cond-pag = 0 THEN
             ASSIGN bt-cond-esp:SENSITIVE = YES.

          DISABLE {&list-5} WITH FRAME {&FRAME-NAME}.

          IF ped-venda.cod-sit-ped = 1 THEN
             ASSIGN fi-cod-estabel:SENSITIVE = YES.

          FIND usuar_grp_usuar WHERE 
               usuar_grp_usuar.cod_usuar     = c-seg-usuario AND
               usuar_grp_usuar.cod_grp_usuar = "EP0" 
               NO-LOCK NO-ERROR.
          IF AVAIL usuar_grp_usuar THEN DO.
             DISABLE {&list-4} WITH FRAME {&FRAME-NAME}.

             FIND usuar_grp_usuar WHERE 
                  usuar_grp_usuar.cod_usuar     = c-seg-usuario AND
                  usuar_grp_usuar.cod_grp_usuar = "VD0" 
                  NO-LOCK NO-ERROR.
             IF AVAIL usuar_grp_usuar THEN 
                ENABLE {&list-4} WITH FRAME {&FRAME-NAME}.
          END.

          IF ped-venda.tp-preco = 1 THEN
             ASSIGN cb-tp-preco:SENSITIVE IN FRAME {&FRAME-NAME} = YES.


          IF ped-venda-ext.tp-pedido = 'Operaá∆o Triangular' THEN
             ASSIGN fi-cliente-tri:SENSITIVE = YES.

          APPLY 'value-changed' TO br-ped-item.
          ASSIGN bt-ok:SENSITIVE = YES.
      END.
      WHEN 'Consultar' THEN DO.
          DISABLE {&list-4}
                  {&list-5}
                  {&list-6} WITH FRAME {&FRAME-NAME}.
          FIND ped-venda WHERE
               ROWID(ped-venda) = gr-ped-venda NO-LOCK NO-ERROR.
          RUN pi-display-fields.

          IF ped-venda.cod-cond-pag = 0 THEN
             ASSIGN bt-cond-esp:SENSITIVE = YES.

          IF ped-venda.nome-abrev-tri = '' THEN
             ASSIGN bt-cons-cliente-tri:SENSITIVE = NO.

          ASSIGN ed-obs:SENSITIVE = YES.
          ASSIGN ed-obs:READ-ONLY = YES.
      END.
  END CASE.
  APPLY 'ENTRY' TO cb-tp-pedido.
  RETURN NO-APPLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-add-item w-window 
PROCEDURE pi-add-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN i-ult-seq = 10.
    IF NUM-RESULTS("br-ped-item") > 0 THEN DO.
       br-ped-item:QUERY:GET-LAST() IN FRAME {&FRAME-NAME}.
       br-ped-item:QUERY:REPOSITION-TO-ROWID(ROWID(tt-itens-ped)).
       br-ped-item:SELECT-FOCUSED-ROW().

       ASSIGN i-resto = 10 - (tt-itens-ped.nr-sequencia MODULO 10).

       IF i-resto <> 10 THEN
          ASSIGN i-ult-seq = tt-itens-ped.nr-sequencia + i-resto.
       ELSE
          ASSIGN i-ult-seq = tt-itens-ped.nr-sequencia + 10.
    END.

    br-ped-item:INSERT-ROW("after":U) IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-calc-natur-oper w-window 
PROCEDURE pi-calc-natur-oper :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   CASE cb-tp-pedido:SCREEN-VALUE IN FRAME {&FRAME-NAME}:
       WHEN 'Operaá∆o Triangular' OR WHEN '∑ Vista' THEN DO.
          IF AVAIL b-emitente THEN DO.
             IF (emitente.ins-estadual = '' OR emitente.ins-estadual = 'ISENTO') THEN DO. 
                IF emitente.estado = 'MG' THEN
                   ASSIGN fi-natur-oper:SCREEN-VALUE = IF b-emitente.estado = 'MG'
                                                       THEN '51208' ELSE ''.
                ELSE
                   ASSIGN fi-natur-oper:SCREEN-VALUE = IF b-emitente.estado = 'MG'
                                                       THEN '61902' ELSE '61903'.
                END.
             ELSE DO.
                IF emitente.estado = 'MG' THEN
                   ASSIGN fi-natur-oper:SCREEN-VALUE = IF b-emitente.estado = 'MG'
                                                       THEN '51203' ELSE '51204'.
                ELSE
                IF emitente.estado = 'RJ' OR
                   emitente.estado = 'SP' OR
                   unid-feder.char-2 = 'SUL' THEN /* Regi∆o */
                   ASSIGN fi-natur-oper:SCREEN-VALUE = IF b-emitente.estado = 'MG' 
                                                       THEN '61204' ELSE '61203'.
                ELSE
                   ASSIGN fi-natur-oper:SCREEN-VALUE = IF b-emitente.estado = 'MG' 
                                                       THEN '61205' ELSE '61206'.
             END.
          END.
       END.
       WHEN 'Amostra' THEN DO.
          IF emitente.estado = 'MG' THEN 
             ASSIGN fi-natur-oper:SCREEN-VALUE = IF emitente.ins-estadual = '' OR
                                                    emitente.ins-estadual = 'ISENTO'
                                                 THEN '59930' ELSE '59930'.
          ELSE DO.
             IF emitente.ins-estadual = '' OR 
                emitente.ins-estadual = 'ISENTO' THEN
                ASSIGN fi-natur-oper:SCREEN-VALUE = '69930'.
             ELSE DO.
                IF emitente.estado = 'RJ' OR
                   emitente.estado = 'SP' OR
                   unid-feder.char-2 = 'SUL' THEN  /* Regi∆o */
                   ASSIGN fi-natur-oper:SCREEN-VALUE = '69930'.
                ELSE
                   ASSIGN fi-natur-oper:SCREEN-VALUE = '69930'.
             END.
          END.
       END.
       WHEN 'Amostra Exportaá∆o' THEN
          ASSIGN fi-natur-oper:SCREEN-VALUE = '79902'.
       WHEN 'Rem.Industrializacao' THEN DO.
          IF emitente.estado = 'MG' THEN 
             ASSIGN fi-natur-oper:SCREEN-VALUE = '59301'.
          ELSE
             ASSIGN fi-natur-oper:SCREEN-VALUE = '69301'.
       END.
       WHEN 'Bonificaá∆o' THEN DO.
          IF emitente.estado = 'MG' THEN 
             ASSIGN fi-natur-oper:SCREEN-VALUE = IF emitente.ins-estadual = '' OR
                                                    emitente.ins-estadual = 'ISENTO'
                                                 THEN '59929' ELSE '59929'.
          ELSE DO.
             IF emitente.ins-estadual = '' OR 
                emitente.ins-estadual = 'ISENTO' THEN
                ASSIGN fi-natur-oper:SCREEN-VALUE = '69929'.
             ELSE DO.
                 IF emitente.estado = 'RJ' OR
                    emitente.estado = 'SP' OR
                    unid-feder.char-2 = 'SUL' THEN  /* Regi∆o */
                    ASSIGN fi-natur-oper:SCREEN-VALUE = '69929'.
                 ELSE
                    ASSIGN fi-natur-oper:SCREEN-VALUE = '69929'.
             END.
          END.
       END.
       WHEN 'Doaá∆o' THEN DO.
           IF emitente.estado = 'MG' THEN DO.
              ASSIGN fi-natur-oper:SCREEN-VALUE = IF emitente.ins-estadual = '' OR
                                                     emitente.ins-estadual = 'ISENTO'
                                                  THEN '59907' ELSE '59907'.
           END.
           ELSE DO.
              IF emitente.ins-estadual = '' OR 
                 emitente.ins-estadual = 'ISENTO' THEN
                 ASSIGN fi-natur-oper:SCREEN-VALUE = '69907'.
              ELSE DO.
                  IF emitente.estado = 'RJ' OR
                     emitente.estado = 'SP' OR
                     unid-feder.char-2 = 'SUL' THEN  /* Regi∆o */
                     ASSIGN fi-natur-oper:SCREEN-VALUE = '69907'.
                  ELSE
                     ASSIGN fi-natur-oper:SCREEN-VALUE = '69907'.
              END.
           END.
       END.
       WHEN 'Venda Confec.' THEN DO.
           IF emitente.estado = 'MG' THEN
              ASSIGN fi-natur-oper:SCREEN-VALUE = ''.
           ELSE DO.
              IF emitente.estado = 'RJ' OR
                 emitente.estado = 'SP' OR
                 unid-feder.char-2 = 'SUL' THEN  /* Regi∆o */
                 ASSIGN fi-natur-oper:SCREEN-VALUE = ''.
              ELSE
                 ASSIGN fi-natur-oper:SCREEN-VALUE = ''.
           END.
       END.
   END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-calc-seq w-window 
PROCEDURE pi-calc-seq :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN i-nr-seq-div = tt-itens-ped.nr-sequencia + 1.
    REPEAT.
        FIND b-itens-ped WHERE
             b-itens-ped.nr-sequencia = i-nr-seq-div
             NO-LOCK NO-ERROR.
        IF NOT AVAIL b-itens-ped THEN LEAVE.

        ASSIGN i-nr-seq-div = i-nr-seq-div + 1.
    END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cancela-reserva w-window 
PROCEDURE pi-cancela-reserva :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FIND ped-item-res WHERE
         ped-item-res.cod-estabel = ped-venda.cod-estabel AND  
         ped-item-res.nr-pedcli = ped-venda.nr-pedcli AND
         ped-item-res.nome-abrev = ped-venda.nome-abrev AND
         ped-item-res.nr-sequencia = tt-itens-ped.nr-sequencia
         SHARE-LOCK NO-ERROR.

    IF AVAIL ped-item-res THEN
       DELETE ped-item-res.

    ASSIGN tt-itens-ped.qt-reserva = 0.
    DISP tt-itens-ped.qt-reserva WITH BROWSE {&browse-name}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cond-esp w-window 
PROCEDURE pi-cond-esp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH cond-ped OF ped-venda NO-LOCK.
        RUN esapi/elimina-cond-esp.p (INPUT cond-ped.nr-pedido,
                                      INPUT cond-ped.nr-sequencia).
    END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cria-log w-window 
PROCEDURE pi-cria-log :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN l-nat-oper = NO.
    DO i-ct = 1 TO NUM-ENTRIES(c-results).
       CASE ENTRY(i-ct,c-results).
            WHEN 'dt-entrega' THEN DO.
                RUN esapi/cria-log-pedvenda.p (INPUT tt-ped-venda.nr-pedcli,
                                               INPUT tt-ped-venda.nome-abrev,
                                               INPUT "Alterado Data de Entrega, para: " + STRING(tt-ped-venda.dt-entrega),
                                               INPUT NO).
                FOR EACH tt-itens-ped WHERE
                         tt-itens-ped.cod-sit-item = 1.
                    ASSIGN tt-itens-ped.tp-acao = 'Modificar'
                           tt-itens-ped.dt-entrega = tt-ped-venda.dt-entrega.
                END.
            END.
            WHEN 'nat-operacao' THEN DO.
                 RUN esapi/cria-log-pedvenda.p (INPUT tt-ped-venda.nr-pedcli,
                                                INPUT tt-ped-venda.nome-abrev,
                                                INPUT "Alterado Natureza de Operaá∆o, para: " + tt-ped-venda.nat-operacao,
                                                INPUT NO).
                 ASSIGN l-nat-oper = YES.
            END.
            WHEN 'no-ab-rep' THEN DO.
                 RUN esapi/elimina-repres.p (INPUT ped-venda.nr-pedcli,
                                             INPUT ped-venda.no-ab-rep).
                 RUN esapi/cria-log-pedvenda.p (INPUT tt-ped-venda.nr-pedcli,
                                                INPUT tt-ped-venda.nome-abrev,
                                                INPUT "Alterado Representante, para: " + tt-ped-venda.no-ab-rep,
                                                INPUT NO).
            END.
            WHEN 'cod-cond-pag' THEN DO.
                 RUN esapi/cria-log-pedvenda.p (INPUT tt-ped-venda.nr-pedcli,
                                                INPUT tt-ped-venda.nome-abrev,
                                                INPUT "Alterado Condiá∆o de Pagamento, para: " + STRING(tt-ped-venda.cod-cond-pag),
                                                INPUT NO).
                 ASSIGN l-desaprovar-cred = YES.
            END.
            WHEN 'nome-transp' THEN
                 RUN esapi/cria-log-pedvenda.p (INPUT tt-ped-venda.nr-pedcli,
                                                INPUT tt-ped-venda.nome-abrev,
                                                INPUT "Alterado Transportadora, para: " + tt-ped-venda.nome-transp,
                                                INPUT NO).
            WHEN 'nome-tr-red' THEN
                 RUN esapi/cria-log-pedvenda.p (INPUT tt-ped-venda.nr-pedcli,
                                                INPUT tt-ped-venda.nome-abrev,
                                                INPUT "Alterado Redepacho, para: " + tt-ped-venda.nome-tr-red,
                                                INPUT NO).
            WHEN 'dt-base-ft' THEN
                 RUN esapi/cria-log-pedvenda.p (INPUT tt-ped-venda.nr-pedcli,
                                                INPUT tt-ped-venda.nome-abrev,
                                                INPUT "Alterado Data Base, para: " + STRING(tt-ped-venda.dt-base-ft),
                                                INPUT NO).
       END CASE.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-desaprova-cred w-window 
PROCEDURE pi-desaprova-cred :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FIND CURRENT ped-venda EXCLUSIVE-LOCK NO-ERROR.

    FIND emitente WHERE
         emitente.nome-abrev = ped-venda.nome-abrev NO-LOCK NO-ERROR.

    IF AVAIL ped-venda-ext THEN DO.
       IF LOOKUP(ped-venda-ext.tp-pedido,c-tpped-cred-aut) > 0 THEN DO.
          IF (ped-venda-ext.tp-pedido = "Amostra" OR
              ped-venda-ext.tp-pedido = "Amostra Exportaá∆o") THEN DO.
              FIND natur-oper WHERE
                   natur-oper.nat-operacao = ped-venda.nat-oper NO-LOCK NO-ERROR.

              IF natur-oper.emite-duplic = NO THEN RETURN.
          END.
          ELSE RETURN.
       END.

       IF emitente.ind-cre-cli <> 2 THEN DO.
          ASSIGN ped-venda.desc-bloq-cr = c-desc-bloq-cr
                 ped-venda.dt-apr-cred = ?
                 ped-venda.cod-sit-aval = IF ped-venda.cod-sit-aval = 2 OR
                                             ped-venda.cod-sit-aval = 3
                                          THEN 4
                                          ELSE ped-venda.cod-sit-aval
                 ped-venda.quem-aprovou = c-seg-usuario
                 ped-venda.dsp-pre-fat = NO.
    
          /* Grava LOG */
          ASSIGN c-texto-log = "Pedido Retornado para Re-Avaliaá∆o de CrÇdito // " + ped-venda.desc-bloq-cr.
          RUN esapi/cria-log-pedvenda.p (INPUT ped-venda.nr-pedcli,
                                         INPUT ped-venda.nome-abrev,
                                         INPUT c-texto-log,
                                         INPUT NO).
       END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-display-fields w-window 
PROCEDURE pi-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF AVAIL ped-venda THEN DO.
      FIND estabelec WHERE
           estabelec.cod-estabel = ped-venda.cod-estabel NO-LOCK NO-ERROR.

      ASSIGN fi-cod-estabel:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ped-venda.cod-estabel
             fi-nome-estabel:SCREEN-VALUE IN FRAME {&FRAME-NAME} = estabelec.nome.

      FIND ped-venda-ext WHERE
           ped-venda-ext.cod-estabel = ped-venda.cod-estabel AND
           ped-venda-ext.nr-pedido = ped-venda.nr-pedido NO-LOCK NO-ERROR.

      ASSIGN fi-nr-pedido:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ped-venda.nr-pedido)
             fi-dt-implant:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ped-venda.dt-implant).

      /*{esinc/i-dsrb.i ped-venda.cod-sit-ped ped-venda.cod-sit-ped fi-situacao:SCREEN-VALUE}*/
      CASE ped-venda.cod-sit-ped.
          WHEN 1 OR WHEN 2 OR WHEN 4 THEN ASSIGN fi-nr-pedido:BGCOLOR = 2.
          WHEN 3 OR WHEN 5 OR WHEN 6 THEN ASSIGN fi-nr-pedido:BGCOLOR = 12.
      END CASE.

      {esinc/i-dsrb.i ped-venda.cod-sit-aval ped-venda.cod-sit-aval fi-sit-cred:SCREEN-VALUE}
      CASE ped-venda.cod-sit-aval.
          WHEN 1 THEN ASSIGN fi-sit-cred:BGCOLOR = 20.
          WHEN 2 THEN ASSIGN fi-sit-cred:BGCOLOR = 2.
          WHEN 3 THEN ASSIGN fi-sit-cred:BGCOLOR = 2.
          WHEN 4 THEN ASSIGN fi-sit-cred:BGCOLOR = 12.
      END CASE.

      {esinc/i-dsrb.i ped-venda.cod-sit-com ped-venda.cod-sit-com fi-sit-preco:SCREEN-VALUE}
      CASE ped-venda.cod-sit-com.
          WHEN 1 THEN ASSIGN fi-sit-preco:BGCOLOR = 20.
          WHEN 2 THEN ASSIGN fi-sit-preco:BGCOLOR = 2.
          WHEN 3 THEN ASSIGN fi-sit-preco:BGCOLOR = 12.
      END CASE.

      ASSIGN fi-cliente:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ped-venda.nome-abrev).
      FIND emitente WHERE 
           emitente.nome-abrev = ped-venda.nome-abrev NO-LOCK NO-ERROR.
      IF AVAIL emitente THEN 
         ASSIGN fi-cidade-cli:SCREEN-VALUE IN FRAME {&FRAME-NAME} = emitente.cidade
                fi-uf:SCREEN-VALUE = emitente.estado.

      ASSIGN fi-pedido-cli:SCREEN-VALUE IN FRAME {&FRAME-NAME} = IF AVAIL ped-venda-ext
                                                                 THEN ped-venda-ext.nr-pedcli
                                                                 ELSE ''
             fi-cliente-tri:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ped-venda.nome-abrev-tri).

      IF ped-venda.nome-abrev-tri <> '' THEN DO.
         FIND emitente WHERE 
              emitente.nome-abrev = ped-venda.nome-abrev-tri NO-LOCK NO-ERROR.

         IF AVAIL emitente THEN 
            ASSIGN fi-cidade-cli-tri:SCREEN-VALUE IN FRAME {&FRAME-NAME} = emitente.cidade
                   fi-uf-tri:SCREEN-VALUE = emitente.estado.
      END.

      ASSIGN fi-cod-rep:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ped-venda.no-ab-reppri.
      FIND repres WHERE
           repres.nome-abrev = ped-venda.no-ab-reppri NO-LOCK NO-ERROR.
      IF AVAIL repres THEN
         ASSIGN fi-nome-rep:SCREEN-VALUE IN FRAME {&FRAME-NAME} = repres.nome
                fi-perc-comis:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(repres.comis-direta).

      ASSIGN fi-ped-repres:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ped-venda.nr-pedrep.
      FIND ped-repre OF ped-venda WHERE 
           ped-repre.nome-ab-rep = ped-venda.no-ab-reppri NO-LOCK NO-ERROR.
      IF AVAIL ped-repre THEN
         ASSIGN fi-perc-comis:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ped-repre.perc-comis).

      ASSIGN fi-natur-oper:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ped-venda.nat-operacao).
      FIND natur-oper WHERE
           natur-oper.nat-operacao = ped-venda.nat-operacao NO-LOCK NO-ERROR.
      IF AVAIL natur-oper THEN
         ASSIGN fi-denominacao:SCREEN-VALUE IN FRAME {&FRAME-NAME} = natur-oper.denominacao.

      ASSIGN fi-cod-cond-pag:SCREEN-VALUE = STRING(ped-venda.cod-cond-pag).
      FIND cond-pagto WHERE
           cond-pagto.cod-cond-pag = ped-venda.cod-cond-pag NO-LOCK NO-ERROR.
      IF AVAIL cond-pagto THEN DO.
         ASSIGN fi-desc-cond-pag:SCREEN-VALUE = cond-pagto.descricao.

         ASSIGN de-ind-finan = 1.
         IF cond-pagto.nr-tab-finan <> 0 AND
            cond-pagto.nr-ind-finan <> 0 THEN DO.
            FIND tab-finan WHERE
                 tab-finan.nr-tab-finan = cond-pagto.nr-tab-finan NO-LOCK NO-ERROR.
            IF tab-finan.tab-ind-fin[cond-pagto.nr-ind-finan] <> 0 THEN
               ASSIGN de-ind-finan = tab-finan.tab-ind-fin[cond-pagto.nr-ind-finan].
         END.
      END.
      ELSE DO.
         ASSIGN fi-desc-cond-pag:SCREEN-VALUE = 'E S P E C I A L'.

         ASSIGN de-tot-prazo = 0
                i-ct = 0.
         FOR EACH cond-ped WHERE
                  cond-ped.nr-pedido = ped-venda.nr-pedido NO-LOCK.
             ASSIGN de-tot-prazo = de-tot-prazo + cond-ped.nr-dias-venc.
             ASSIGN i-ct = i-ct + 1.
         END.
         ASSIGN i-prazo-medio = de-tot-prazo / i-ct.

         FIND FIRST tab-finan WHERE 
                    tab-finan.dt-ini-val <= TODAY AND 
                    tab-finan.dt-fim-val >= TODAY NO-LOCK NO-ERROR.
         
         DO i-ct = 1 TO EXTENT(tab-finan.tab-dia-fin).
            IF tab-finan.tab-dia-fin[i-ct] >= i-prazo-medio THEN
               LEAVE. 
         END.
         IF i-ct > EXTENT(tab-finan.tab-ind-fin) THEN
            ASSIGN i-ct = EXTENT(tab-finan.tab-ind-fin).

         ASSIGN de-ind-finan = tab-finan.tab-ind-fin[i-ct].
      END.

      IF de-ind-finan = 0 THEN
         ASSIGN de-ind-finan = 1.

      ASSIGN cb-tab-preco:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ped-venda.nr-tabpre
             cb-prioridade:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ped-venda.cod-priori)
             cb-tp-preco:SCREEN-VALUE = STRING(ped-venda.tp-preco) 
             fi-cod-informado:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ped-venda.des-pct-desconto-inform.
        
      ASSIGN cb-tp-entrega:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Na Data"
             fi-dt-entrega:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ped-venda.dt-entrega)
             fi-nome-transp:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ped-venda.nome-transp)
             fi-nome-tr-red:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ped-venda.nome-tr-red).

      ASSIGN fi-cod-rota:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ped-venda.cod-rota).
      FIND rota WHERE 
           rota.cod-rota = ped-venda.cod-rota NO-LOCK NO-ERROR.

      IF AVAIL rota THEN 
         ASSIGN fi-desc-rota:SCREEN-VALUE IN FRAME {&FRAME-NAME} = SUBSTR(rota.roteiro,1,2) + "-" + 
                                                                   rota.descricao.

      ASSIGN fi-moeda:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ped-venda.mo-codigo).
      FIND moeda WHERE
           moeda.mo-codigo = ped-venda.mo-codigo NO-LOCK NO-ERROR.
      IF AVAIL moeda THEN
         ASSIGN fi-desc-moeda:SCREEN-VALUE = moeda.descricao.

      ASSIGN fi-data-base:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ped-venda.dt-base-ft)
             ed-obs:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ped-venda.obs.

      /* Dados Complementares */
      IF AVAIL ped-venda-ext THEN 
         ASSIGN cb-tp-pedido:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ped-venda-ext.tp-pedido 
                cb-ext-tp-pedido:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ped-venda.tp-pedido 
                cb-tp-entrega:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ped-venda-ext.tp-entrega
                cb-tp-frete:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ped-venda-ext.tp-frete
                cb-tipo-pagto:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ped-venda-ext.tp-pagto
                fi-reserva:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ped-venda-ext.num-reserva)
                fi-fat-max:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ped-venda-ext.fat-max-nf)
                fi-nr-container:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ped-venda-ext.nr-container)
                tg-bloqueio:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ped-venda-ext.l-bloqueio)
                tg-nao-aprovar:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ped-venda-ext.l-nao-aprovar) 
                fi-observ-nf:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ped-venda-ext.compl-observ).

      IF ped-venda.completo THEN DO.
         bt-ok:LOAD-IMAGE("image/im-ok.gif").
         ASSIGN fi-completo:SCREEN-VALUE = 'Completo'.
         ASSIGN fi-completo:BGCOLOR = 2.
      END.
      ELSE DO.
         bt-ok:LOAD-IMAGE("image/im-sav.gif").
         ASSIGN fi-completo:SCREEN-VALUE = 'Incompleto'.
         ASSIGN fi-completo:BGCOLOR = 12.
      END.

      FOR EACH cond-ped OF ped-venda NO-LOCK.
          CREATE tt-cond-ped.
          BUFFER-COPY cond-ped TO tt-cond-ped.
      END.

      RUN pi-popula-browse.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-envia-email w-window 
PROCEDURE pi-envia-email :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR c-mensagem     AS CHAR.
    DEF VAR l-env-e-mail   AS LOG.
    DEF VAR i-pos          AS INT.
    DEF VAR i-tam          AS INT.
    DEF VAR c-remetente    LIKE usuar_mestre.cod_e_mail_local.
    DEF VAR c-destinatario LIKE param-dis.destinatario.

    FIND FIRST espec.param-dis NO-LOCK NO-ERROR.

    ASSIGN l-env-e-mail = NO.
    FOR EACH usuar_grp_usuar WHERE
             usuar_grp_usuar.cod_usuario = c-seg-usuario NO-LOCK:
        IF INDEX(espec.param-dis.grp-remetente,usuar_grp_usuar.cod_grp_usuar) <> 0 THEN DO:
           ASSIGN l-env-e-mail = YES.
           LEAVE.
        END.
    END.

    FIND usuar_mestre WHERE 
         usuar_mestre.cod_usuario = c-seg-usuario NO-LOCK NO-ERROR.

    IF AVAIL usuar_mestre AND usuar_mestre.cod_e_mail_local <> "" THEN DO:
       ASSIGN c-remetente = usuar_mestre.cod_e_mail_local
              i-pos = INDEX(param-dis.destinatario,usuar_mestre.cod_e_mail_local)
              i-tam = LENGTH(usuar_mestre.cod_e_mail_local).
       IF SUBSTR(param-dis.destinatario,i-pos + i-tam,1) = ";" THEN
          ASSIGN i-tam = i-tam + 1.

       IF i-pos = 1 THEN
          ASSIGN c-destinatario = SUBSTR(param-dis.destinatario, i-tam + 1, LENGTH(param-dis.destinatario) - i-tam).
       ELSE
          ASSIGN c-destinatario = SUBSTR(PARAM-dis.destinatario,1,i-pos - 1) + SUBSTR(param-dis.destinatario, i-pos + 
                                                 i-tam,LENGTH(param-dis.destinatario) - (i-pos + i-tam - 1)).
    END.
    ELSE
       ASSIGN c-remetente    = "teartextil@teartextil.com.br"
              c-destinatario = param-dis.destinatario.

    ASSIGN c-mensagem = "Pedido: " + ped-item-ext.nr-pedcli + " Seq.: " + TRIM(STRING(ped-item-ext.nr-sequencia,">>>9")) + " Cliente: " + 
                        ped-item-ext.nome-abrev + CHR(13) +
                        "Usu†rio: " + c-seg-usuario + " Data: " + STRING(TODAY,"99/99/9999") + " Hora: " + STRING(TIME,"HH:MM") + 
                        CHR(13) + CHR(13).

    ASSIGN c-mensagem = c-mensagem + c-texto-msg.

    RUN esapi/esapi002.p (INPUT c-remetente, /* e-mail remetente */
                          INPUT c-destinatario, /* e-mail destinat†rio */
                          INPUT "Alteraá∆o no Pedido de Venda: " + ped-item-ext.nr-pedcli, /* Assunto */
                          INPUT c-mensagem, /* Mensagem */
                          INPUT "", /*arquivo anexo*/
                          INPUT YES). /* Mostra Erros */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-estoque w-window 
PROCEDURE pi-estoque :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER p-cod-estabel AS CHAR.
    DEF INPUT PARAMETER p-it-codigo AS CHAR.
    DEF INPUT PARAMETER p-cod-refer AS CHAR.
    DEF INPUT PARAMETER p-nr-lote AS CHAR.
    
    RUN esp/essp0150.p PERSISTENT SET h-essp0150.

    FOR EACH tt-estoque.
        DELETE tt-estoque.
    END.

    RUN pi-retorna-temp-table IN h-essp0150 (INPUT-OUTPUT TABLE tt-estoque,
                                             INPUT p-cod-estabel,
                                             INPUT p-it-codigo,
                                             INPUT p-cod-refer,
                                             INPUT p-nr-lote,
                                             INPUT '129999'). 
    DELETE OBJECT h-essp0150.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-leave-item w-window 
PROCEDURE pi-leave-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF INPUT BROWSE {&browse-name} tt-itens-ped.it-codigo = '' THEN DO.
       MESSAGE 'Item deve ser Informado...'
           VIEW-AS ALERT-BOX ERROR BUTTONS OK.
       APPLY 'entry' TO SELF.
       RETURN 'ADM-ERROR'.
    END.

    FIND ITEM WHERE
         ITEM.it-codigo = INPUT BROWSE {&browse-name} tt-itens-ped.it-codigo NO-LOCK NO-ERROR.
    IF NOT AVAIL item THEN DO.
       MESSAGE 'Item n∆o Cadastrado...'
           VIEW-AS ALERT-BOX ERROR BUTTONS OK.
       APPLY 'entry' TO SELF.
       RETURN 'ADM-ERROR'.
    END.
    DISP ITEM.desc-item WITH BROWSE {&browse-name}.

    IF INPUT FRAME {&FRAME-NAME} cb-ext-tp-pedido = 'PI' THEN DO.
       FIND FIRST pp-it-container WHERE
                  pp-it-container.nr-container = INPUT FRAME {&FRAME-NAME} fi-nr-container AND
                  pp-it-container.it-codigo = INPUT BROWSE {&browse-name} tt-itens-ped.it-codigo
                  NO-LOCK NO-ERROR.
       IF NOT AVAIL pp-it-container THEN DO.
          MESSAGE 'Item n∆o Dispon°vel no Container ' 
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
          APPLY 'entry' TO SELF.
          RETURN 'ADM-ERROR'.
       END.
    END.

    FIND LAST b-itens-ped WHERE
              b-itens-ped.nr-sequencia < INPUT BROWSE {&browse-name} tt-itens-ped.nr-sequencia AND 
              b-itens-ped.it-codigo = INPUT BROWSE {&browse-name} tt-itens-ped.it-codigo
              NO-LOCK NO-ERROR.

    IF AVAIL b-itens-ped THEN
       ASSIGN tt-itens-ped.vl-preori:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(b-itens-ped.vl-preori).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-leave-qtde w-window 
PROCEDURE pi-leave-qtde :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF DEC(SELF:SCREEN-VALUE) = 0 THEN DO.
       MESSAGE 'Quantidade Deve ser Informada...'
           VIEW-AS ALERT-BOX ERROR BUTTONS OK.
       APPLY 'entry' TO SELF.
       RETURN 'ADM-ERROR'.
    END.
                          
    IF INPUT FRAME {&FRAME-NAME} cb-ext-tp-pedido = 'PI' THEN DO.
       FIND tt-itens-ped WHERE 
            tt-itens-ped.nr-sequencia = INPUT BROWSE {&browse-name} tt-itens-ped.nr-sequencia
            NO-ERROR.
       IF NOT AVAIL tt-itens-ped THEN DO:
          IF INPUT BROWSE {&browse-name} tt-itens-ped.qt-pedida > de-qtidade-atu THEN DO.
             MESSAGE 'Quantidade Pedida maior que Dispon°vel no Container...'
                 VIEW-AS ALERT-BOX INFO BUTTONS OK.
             APPLY 'entry' TO SELF.
             RETURN 'ADM-ERROR'.
          END.
       END.
       ELSE DO: /* est† alterando */
          IF tt-itens-ped.qt-pedida <> INPUT BROWSE {&BROWSE-NAME} tt-itens-ped.qt-pedida AND
             INPUT BROWSE {&BROWSE-NAME} tt-itens-ped.qt-pedida > (tt-itens-ped.qt-pedida + de-qtidade-atu) THEN DO. 
             MESSAGE 'Quantidade Pedida maior que Dispon°vel no Container...'
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
             APPLY 'entry' TO SELF.
             RETURN 'ADM-ERROR'.
          END.
       END.
    END.
    ELSE DO.
       IF tt-itens-ped.it-codigo:READ-ONLY IN BROWSE br-ped-item = NO THEN DO. /* Esta incluindo */
          IF INPUT BROWSE {&BROWSE-NAME} tt-itens-ped.qt-pedida > de-qtidade-atu THEN DO.
             MESSAGE 'Quantidade Informada Maior que a Dispon°vel...'
                  VIEW-AS ALERT-BOX ERROR BUTTONS OK.
             APPLY 'entry' TO SELF.
             RETURN 'ADM-ERROR'.
          END.
       END.
       ELSE DO.
          IF tt-itens-ped.qt-pedida <> INPUT BROWSE {&BROWSE-NAME} tt-itens-ped.qt-pedida AND
             INPUT BROWSE {&BROWSE-NAME} tt-itens-ped.qt-pedida > (tt-itens-ped.qt-pedida + de-qtidade-atu) THEN DO.
             MESSAGE 'Quantidade Informada Maior que a Dispon°vel...'
                  VIEW-AS ALERT-BOX ERROR BUTTONS OK.
             APPLY 'entry' TO SELF.
             RETURN 'ADM-ERROR'.
          END.
       END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-leave-refer w-window 
PROCEDURE pi-leave-refer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF item.tipo-con-est <> 4 AND 
       INPUT BROWSE {&BROWSE-NAME} tt-itens-ped.cod-refer <> "" THEN DO:
       MESSAGE "Item n∆o Ç controlado por Referància. Referància deve ser BRANCO." 
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
       APPLY 'entry' TO SELF.
       RETURN 'ADM-ERROR'.
    END.

    IF item.tipo-con-est = 4 THEN DO:
       FIND referencia WHERE 
            referencia.cod-refer = INPUT BROWSE {&BROWSE-NAME} tt-itens-ped.cod-refer
            NO-LOCK NO-ERROR.

       IF NOT AVAIL referencia THEN DO:
           MESSAGE 'Referància n∆o Cadastrada...'
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
           APPLY 'entry' TO SELF.
           RETURN 'ADM-ERROR'.
       END.

       FIND ref-item WHERE
            ref-item.cod-refer = INPUT BROWSE {&BROWSE-NAME} tt-itens-ped.cod-refer AND
            ref-item.it-codigo = ITEM.it-codigo NO-LOCK NO-ERROR.

       IF NOT AVAIL ref-item THEN DO.
          MESSAGE "Referància n∆o Vinculada ao Item..."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          APPLY 'entry' TO SELF.
          RETURN 'ADM-ERROR'.
       END.

       IF br-ped-item:NEW-ROW IN FRAME {&FRAME-NAME} THEN DO.
          FIND b-itens-ped WHERE
               b-itens-ped.it-codigo = ITEM.it-codigo AND
               b-itens-ped.cod-refer = INPUT BROWSE {&BROWSE-NAME} tt-itens-ped.cod-refer AND
               b-itens-ped.cod-sit-item <> 6
               NO-LOCK NO-ERROR.
           IF AVAIL b-itens-ped THEN DO.
              MESSAGE "Referància j† Cadastrada para o Item..."
                  VIEW-AS ALERT-BOX ERROR BUTTONS OK.
              APPLY 'entry' TO SELF.
              RETURN 'ADM-ERROR'.
           END.
       END.
    END.

    IF INPUT FRAME {&FRAME-NAME} cb-ext-tp-pedido = 'PI' THEN DO.
       FIND pp-it-container WHERE 
            pp-it-container.nr-container = INPUT FRAME {&FRAME-NAME} fi-nr-container AND
            pp-it-container.it-comprado  = INPUT BROWSE {&BROWSE-NAME} tt-itens-ped.it-codigo AND
            pp-it-container.ref-comprada = INPUT BROWSE {&BROWSE-NAME} tt-itens-ped.cod-refer
            NO-LOCK NO-ERROR.
       IF NOT AVAIL pp-it-container THEN DO.
          MESSAGE "Referància n∆o Cadastrada no Container..." 
              VIEW-AS ALERT-BOX ERROR BUTTONS OK.
          APPLY 'entry' TO SELF.
          RETURN 'ADM-ERROR'.
       END.
    
       IF br-ped-item:NEW-ROW IN FRAME {&FRAME-NAME} THEN DO.
          ASSIGN de-qtidade-atu = 0.
          FOR EACH pp-it-container WHERE
                   pp-it-container.nr-container   = INPUT FRAME {&FRAME-NAME} fi-nr-container AND
                   pp-it-container.it-comprado    = INPUT BROWSE {&BROWSE-NAME} tt-itens-ped.it-codigo AND
                   pp-it-container.ref-comprada   = INPUT BROWSE {&BROWSE-NAME} tt-itens-ped.cod-refer 
                   NO-LOCK.
              ASSIGN de-qtidade-atu = de-qtidade-atu + (pp-it-container.qt-pedida - pp-it-container.qt-vendida).
          END.

          IF de-qtidade-atu <= 0 THEN DO.
             MESSAGE 'Referància sem Saldo no Container'
                   VIEW-AS ALERT-BOX ERROR BUTTONS OK.
             APPLY 'entry' TO SELF.
             RETURN 'ADM-ERROR'.
          END.
       END.
    
       IF c-tab-preco <> '' THEN DO.
          FIND preco-item WHERE
               preco-item.nr-tabpre = c-tab-preco AND
               preco-item.it-codigo = ITEM.it-codigo AND
               preco-item.cod-refer = INPUT BROWSE {&BROWSE-NAME} tt-itens-ped.cod-refer
               NO-LOCK NO-ERROR.
          IF NOT AVAIL preco-item THEN DO.
             MESSAGE 'Referencia n∆o est† Cadastrada na Tabela de Preáos'
                 VIEW-AS ALERT-BOX ERROR BUTTONS OK.
             APPLY 'entry' TO SELF.
             RETURN 'ADM-ERROR'.
          END.
          DISP (preco-item.preco-venda * de-ind-finan) @ tt-itens-ped.vl-preori
               WITH BROWSE {&browse-name}.
       END.
       ELSE
          IF AVAIL pp-it-container AND
             tt-itens-ped.vl-preori:SCREEN-VALUE IN BROWSE br-ped-item = '0' THEN
             ASSIGN tt-itens-ped.vl-preori:SCREEN-VALUE IN BROWSE br-ped-item = STRING(pp-it-container.preco-venda).
    END.
    ELSE DO.
       IF c-tab-preco <> '' THEN DO.
          FIND preco-item WHERE
               preco-item.nr-tabpre = c-tab-preco AND
               preco-item.it-codigo = item.it-codigo AND
               preco-item.cod-refer = INPUT BROWSE {&BROWSE-NAME} tt-itens-ped.cod-refer AND
               preco-item.cod-unid-med = item.un
               NO-LOCK NO-ERROR.
            
          IF NOT AVAIL preco-item THEN DO.
             MESSAGE 'Referencia n∆o est† Cadastrada na Tabela de Preáos'
                  VIEW-AS ALERT-BOX ERROR BUTTONS OK.
             APPLY 'entry' TO SELF.
             RETURN 'ADM-ERROR'.
          END.
    
          DISP (preco-item.preco-venda * de-ind-finan) @ tt-itens-ped.vl-preori
               WITH BROWSE {&browse-name}.
       END.
    
       IF c-tb-preco-pad <> '' THEN DO.
          FIND preco-item WHERE
               preco-item.nr-tabpre = c-tb-preco-pad AND
               preco-item.it-codigo = item.it-codigo AND
               preco-item.cod-refer = INPUT BROWSE {&BROWSE-NAME} tt-itens-ped.cod-refer AND
               preco-item.cod-unid-med = item.un
               NO-LOCK NO-ERROR.
            
          IF AVAIL preco-item THEN
             DISP (preco-item.preco-venda * de-ind-finan) @ tt-itens-ped.vl-pre-calc
                   WITH BROWSE {&browse-name}.
       END.
    
       ASSIGN de-qtidade-atu = 0.
       FOR EACH saldo-estoq WHERE
                saldo-estoq.cod-estabel = INPUT FRAME {&FRAME-NAME} fi-cod-estabel           AND 
                saldo-estoq.it-codigo   = item.it-codigo                                     AND
                saldo-estoq.cod-refer   = INPUT BROWSE {&BROWSE-NAME} tt-itens-ped.cod-refer AND 
                saldo-estoq.cod-localiz = '' NO-LOCK.
           ASSIGN de-qtidade-atu = de-qtidade-atu + saldo-estoq.qtidade-atu. 
       END.

       /* Subtrai Pedidos PI */
       FOR EACH ped-venda WHERE
                ped-venda.cod-sit-ped = 1 NO-LOCK,
           EACH ped-item OF ped-venda WHERE
                ped-item.cod-sit-item = 1 AND
                ped-item.it-codigo = ITEM.it-codigo AND
                ped-item.cod-refer = INPUT BROWSE {&BROWSE-NAME} tt-itens-ped.cod-refer
                NO-LOCK.

           IF ped-venda.tp-pedido = 'PI' AND
              ped-venda.dt-entrega > TODAY THEN NEXT.

           ASSIGN de-qtidade-atu = de-qtidade-atu - ped-item.qt-pedida.
       END.

       /* Subtrai Notas em Aberto */ 
       FOR EACH nota-fiscal WHERE
                nota-fiscal.cod-estabe = INPUT FRAME {&FRAME-NAME} fi-cod-estabel AND
                nota-fiscal.serie = para-fat.serie-pad AND
                nota-fiscal.dt-cancela = ? AND
                nota-fiscal.dt-confirma = ? NO-LOCK,
           EACH it-nota-fisc OF nota-fiscal WHERE
                it-nota-fisc.it-codigo = item.it-codigo AND
                it-nota-fisc.cod-refer = INPUT BROWSE {&BROWSE-NAME} tt-itens-ped.cod-refer 
                NO-LOCK.
           ASSIGN de-qtidade-atu = de-qtidade-atu - it-nota-fisc.qt-faturada[1].
        END.

    END.
    
    IF de-qtidade-atu < 0 THEN
       ASSIGN de-qtidade-atu = 0.
    
    DISP de-qtidade-atu @ tt-itens-ped.dec-2 
         WITH BROWSE {&browse-name}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-logs w-window 
PROCEDURE pi-logs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF p-acao = 'Modificar' THEN DO.
       IF ped-venda-ext.tp-entrega <> INPUT FRAME {&FRAME-NAME} cb-tp-entrega THEN
          RUN esapi/cria-log-pedvenda.p (INPUT ped-venda.nr-pedcli,
                                         INPUT ped-venda.nome-abrev,
                                         INPUT "Alterado Tipo de Entrega, para: " + INPUT FRAME {&FRAME-NAME} cb-tp-entrega,
                                         INPUT NO).

       IF ped-venda-ext.l-nao-aprovar <> INPUT FRAME {&FRAME-NAME} tg-nao-aprovar THEN DO.
          RUN esapi/cria-log-pedvenda.p (INPUT ped-venda.nr-pedcli,
                                         INPUT ped-venda.nome-abrev,
                                         INPUT "Alterado N«O APROVAR, para: " + INPUT FRAME {&FRAME-NAME} tg-nao-aprovar,
                                         INPUT NO).
          ASSIGN l-desaprovar-cred = YES.
       END.

       IF ped-venda-ext.l-bloqueio <> INPUT FRAME {&FRAME-NAME} tg-bloqueio THEN
          RUN esapi/cria-log-pedvenda.p (INPUT ped-venda.nr-pedcli,
                                         INPUT ped-venda.nome-abrev,
                                         INPUT "Alterado Bloqueio de Faturamento, para: " + INPUT FRAME {&FRAME-NAME} tg-bloqueio,
                                         INPUT NO).

       IF ped-venda-ext.tp-pagto <> INPUT FRAME {&FRAME-NAME} cb-tipo-pagto THEN
          RUN esapi/cria-log-pedvenda.p (INPUT ped-venda.nr-pedcli,
                                         INPUT ped-venda.nome-abrev,
                                         INPUT "Alterado Tipo de Pagamento, para: " + INPUT FRAME {&FRAME-NAME} cb-tipo-pagto,
                                         INPUT NO).

       IF ped-venda-ext.tp-frete <> INPUT FRAME {&FRAME-NAME} cb-tp-frete THEN
          RUN esapi/cria-log-pedvenda.p (INPUT ped-venda.nr-pedcli,
                                         INPUT ped-venda.nome-abrev,
                                         INPUT "Alterado Tipo de Frete, para: " + INPUT FRAME {&FRAME-NAME} cb-tp-frete,
                                         INPUT NO).
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-peditem w-window 
PROCEDURE pi-peditem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR l-erro AS LOG INIT NO.
    DEF VAR l-desaprova-cred AS LOG INIT NO.
    DEF VAR de-preco-ant LIKE ped-item.vl-preori.
    DEF VAR de-qtd-ant LIKE ped-item.qt-pedida.

    ASSIGN l-incluiu-item = NO.

    FOR EACH tt-itens-ped WHERE
             tt-itens-ped.tp-acao <> '' 
             BREAK BY tt-itens-ped.tp-acao DESCENDING.

        ASSIGN c-desc-bloq-cr = "".

        CASE tt-itens-ped.tp-acao:
            WHEN 'Incluir' THEN DO.

               RUN esapi/cria-peditem.p (INPUT tt-itens-ped.nr-pedcli,
                                         INPUT tt-itens-ped.nr-sequencia,
                                         INPUT tt-itens-ped.it-codigo,
                                         INPUT tt-itens-ped.cod-refer,
                                         INPUT tt-itens-ped.qt-pedida,
                                         INPUT tt-itens-ped.vl-preori).

               IF RETURN-VALUE = 'ADM-ERROR' THEN DO.

                  ASSIGN l-erro = YES.
                  NEXT.
               END.

               IF p-acao = 'Modificar' AND  /* Modificando o Pedido */
                  tt-itens-ped.nr-sequencia MODULO 10 = 0 THEN
                  ASSIGN l-desaprova-cred = YES
                         c-desc-bloq-cr = c-desc-bloq-cr + "Inclu°do novo Item".
 
               CREATE ped-item-ext.
               ASSIGN ped-item-ext.cod-estabel  = tt-itens-ped.cod-estabel  /* daf */
                      ped-item-ext.nome-abrev   = tt-itens-ped.nome-abrev
                      ped-item-ext.nr-pedcli    = tt-itens-ped.nr-pedcli
                      ped-item-ext.nr-sequencia = tt-itens-ped.nr-sequencia
                      ped-item-ext.it-codigo    = tt-itens-ped.it-codigo
                      ped-item-ext.cod-refer    = tt-itens-ped.cod-refer
                      ped-item-ext.reservado    = NO
                      ped-item-ext.lote         = 'RP' + tt-itens-ped.cod-refer.

               FIND CURRENT ped-item-ext NO-LOCK NO-ERROR.

               ASSIGN l-incluiu-item = YES.

               IF p-acao = 'Modificar' THEN
                  RUN esapi/cria-log-pedvenda.p (INPUT tt-itens-ped.nr-pedcli,
                                                 INPUT tt-itens-ped.nome-abrev,
                                                 INPUT "Sequencia " + TRIM(STRING(tt-itens-ped.nr-sequencia,">>>9")) + ": Inclu°da",
                                                 INPUT YES).
            END.
            
            WHEN 'Modificar' THEN DO.
               FIND ped-item OF tt-itens-ped NO-LOCK NO-ERROR.
               IF AVAIL ped-item THEN DO.

                  IF tt-itens-ped.vl-preori > ped-item.vl-preori THEN
                     ASSIGN l-desaprova-cred = YES
                            c-desc-bloq-cr = c-desc-bloq-cr + "Alterado Preáo".

                  IF tt-itens-ped.qt-pedida > ped-item.qt-pedida THEN DO.
                     IF tt-itens-ped.qt-pedida > ped-item.qt-pedida + (ped-item.qt-pedida * (10 / 100)) THEN
                        ASSIGN l-desaprova-cred = YES
                               c-desc-bloq-cr = c-desc-bloq-cr + "Alterado Quantidade".
                  END.

                  BUFFER-COMPARE ped-item TO tt-itens-ped SAVE RESULT IN c-results.
                  IF c-results <> '' THEN DO.

                     FOR EACH tt-ped-item.
                         DELETE tt-ped-item.
                     END.

                     CREATE tt-ped-item.
                     BUFFER-COPY tt-itens-ped TO tt-ped-item.

                     ASSIGN de-preco-ant = ped-item.vl-preori
                            de-qtd-ant = ped-item.qt-pedida.

                     RUN esapi/altera-peditem-v2.p (INPUT TABLE tt-ped-item).
                     DELETE tt-ped-item.

                     IF RETURN-VALUE = 'ADM-ERROR' THEN DO.
                        ASSIGN l-erro = YES.
                        NEXT.
                     END.

                     FIND CURRENT ped-item NO-LOCK NO-ERROR. /* Reposiciona no Iem do Pedido */

                     ASSIGN l-preco-alterado = NO.
                     DO i-ct = 1 TO NUM-ENTRIES(c-results).
                        CASE ENTRY(i-ct,c-results).
                             WHEN 'qt-pedida' THEN
                                  RUN esapi/cria-log-pedvenda.p (INPUT tt-itens-ped.nr-pedcli,
                                                                 INPUT tt-itens-ped.nome-abrev,
                                                                 INPUT "Sequencia " + TRIM(STRING(tt-itens-ped.nr-sequencia,">>>9")) +
                                                                       ": Alterada a Quantidade Pedida, De: " + TRIM(STRING(de-qtd-ant,">>>,>>9.99")) +
                                                                       " Para: " + TRIM(STRING(tt-itens-ped.qt-pedida,">>>,>>9.99")),
                                                                 INPUT YES).
                             WHEN 'vl-preori' THEN DO.
                                  IF de-preco-ant <> ped-item.vl-preori THEN DO.
                                     RUN esapi/cria-log-pedvenda.p (INPUT tt-itens-ped.nr-pedcli,
                                                                    INPUT tt-itens-ped.nome-abrev,
                                                                    INPUT "Sequencia " + TRIM(STRING(tt-itens-ped.nr-sequencia,">>>9")) +
                                                                          ": Alterado o Preáo, De: " + TRIM(STRING(de-preco-ant,">>>,>>9.99")) +
                                                                          " Para: " + TRIM(STRING(tt-itens-ped.vl-preori,">>>,>>9.99")),
                                                                    INPUT YES).
                                     ASSIGN l-preco-alterado = YES.
                                  END.
                             END.
                        END CASE.
                     END.
                  END.
 

                  IF SUBSTR(tt-itens-ped.char-2,1550,1) = "S" THEN DO.
                     RUN pi-cancela-reserva.
                     ASSIGN SUBSTR(tt-itens-ped.char-2,1550,1) = "".
                  END.

                  FIND ped-item-ext WHERE
                       ped-item-ext.cod-estabel  = tt-itens-ped.cod-estabel AND
                       ped-item-ext.nome-abrev   = tt-itens-ped.nome-abrev AND
                       ped-item-ext.nr-pedcli    = tt-itens-ped.nr-pedcli AND
                       ped-item-ext.nr-sequencia = tt-itens-ped.nr-sequencia 
                       SHARE-LOCK NO-ERROR.
    
                  ASSIGN ped-item-ext.bloqueio-fat = tt-itens-ped.bloqueio-fat.
                  IF l-preco-alterado THEN
                     ASSIGN ped-item-ext.preco-alterado = l-preco-alterado.

                  FIND CURRENT ped-item-ext NO-LOCK NO-ERROR.
               END.
            END.
            WHEN 'Cancelar' THEN DO.
               RUN esapi/cancela-peditem.p (INPUT tt-itens-ped.nr-pedcli,
                                            INPUT tt-itens-ped.nr-sequencia,
                                            INPUT tt-itens-ped.motivo).
               IF RETURN-VALUE = 'ADM-ERROR' THEN DO.
                  ASSIGN l-erro = YES.
                  NEXT.
               END.
            END.
            WHEN 'Eliminar' THEN DO.
               FIND ped-item OF tt-itens-ped NO-LOCK NO-ERROR.

               RUN esapi/elimina-peditem.p (INPUT tt-itens-ped.nr-pedcli,
                                            INPUT tt-itens-ped.nr-sequencia).
               IF RETURN-VALUE = 'ADM-ERROR' THEN DO.
                  ASSIGN l-erro = YES.
                  NEXT.
               END.
               RUN esapi/cria-log-pedvenda.p (INPUT tt-itens-ped.nr-pedcli,
                                              INPUT tt-itens-ped.nome-abrev,
                                              INPUT "Sequencia " + TRIM(STRING(tt-itens-ped.nr-sequencia,">>>9")) + ": Eliminada",
                                              INPUT YES).
            END.
        END CASE.
        FIND CURRENT ped-item NO-LOCK NO-ERROR.


        IF l-desaprova-cred THEN
           RUN pi-desaprova-cred.

        ASSIGN tt-itens-ped.tp-acao = ''
               l-desaprova-cred = NO.
    END.

    IF l-erro THEN
       RETURN 'ADM-ERROR'.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-pedvenda w-window 
PROCEDURE pi-pedvenda :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN pi-acompanhar IN h-acomp (INPUT "Definindo BO's...").

    IF NOT VALID-HANDLE(h-bodi018) OR 
       h-bodi018:TYPE      <> "PROCEDURE":U OR
       h-bodi018:FILE-NAME <> "dibo/bodi018.p":U THEN
       RUN dibo/bodi018.p PERSISTENT SET h-bodi018.

    IF NOT VALID-HANDLE(h-bodi157) OR 
       h-bodi157:TYPE      <> "PROCEDURE":U OR
       h-bodi157:FILE-NAME <> "dibo/bodi157.p":U THEN
       RUN dibo/bodi157.p PERSISTENT SET h-bodi157.

    DO TRANSACTION:
        FIND emitente WHERE 
             emitente.nome-abrev = INPUT FRAME {&FRAME-NAME} fi-cliente NO-LOCK NO-ERROR.
        FIND repres WHERE
             repres.nome-abrev = INPUT FRAME {&FRAME-NAME} fi-cod-rep NO-LOCK NO-ERROR.
        FIND cond-pagto WHERE
             cond-pagto.cod-cond-pag = INPUT FRAME {&FRAME-NAME} fi-cod-cond-pag 
             NO-LOCK NO-ERROR.
        RUN pi-acompanhar IN h-acomp (INPUT "Manutená∆o Pedido de Venda...").
        FOR EACH tt-ped-venda.
            DELETE tt-ped-venda.
        END.
        CREATE tt-ped-venda.
        FIND ped-venda WHERE
             ped-venda.nr-pedido = INPUT FRAME {&FRAME-NAME} fi-nr-pedido NO-LOCK NO-ERROR.
        IF AVAIL ped-venda THEN DO.
           BUFFER-COPY ped-venda TO tt-ped-venda.
        END.
        ELSE DO.
           ASSIGN tt-ped-venda.nome-abrev = emitente.nome-abrev
                  tt-ped-venda.nr-pedido = INPUT FRAME {&FRAME-NAME} fi-nr-pedido
                  tt-ped-venda.nr-pedcli = STRING(INPUT FRAME {&FRAME-NAME} fi-nr-pedido)
                  tt-ped-venda.cod-portador = emitente.portador
                  tt-ped-venda.modalidade = emitente.modalidade
                  tt-ped-venda.per-max-canc = 99
                  tt-ped-venda.cod-entrega = emitente.cod-entrega
                  tt-ped-venda.estado = emitente.estado
                  tt-ped-venda.dt-userimp = TODAY
                  tt-ped-venda.char-1 = STRING(TODAY,"99/99/9999") + STRING(TIME,"HH:MM") + 'DIGITAÄ«O RµPIDA'.
        END.

        ASSIGN tt-ped-venda.cidade = emitente.cidade
               tt-ped-venda.nr-tabpre = c-tab-preco
               tt-ped-venda.tp-preco = INTEGER(INPUT FRAME {&FRAME-NAME} cb-tp-preco)
               tt-ped-venda.des-pct-desconto-inform = INPUT FRAME {&FRAME-NAME} fi-cod-informado
               tt-ped-venda.cod-priori = INTEGER(INPUT FRAME {&FRAME-NAME} cb-prioridade).

        ASSIGN tt-ped-venda.cod-estabel = INPUT FRAME {&FRAME-NAME} fi-cod-estabel
               tt-ped-venda.nome-abrev-tri = INPUT FRAME {&FRAME-NAME} fi-cliente-tri
               tt-ped-venda.nat-operacao = INPUT FRAME {&FRAME-NAME} fi-natur-oper
               tt-ped-venda.cod-mensagem = natur-oper.cod-mensagem
               tt-ped-venda.cod-cond-pag = INPUT FRAME {&FRAME-NAME} fi-cod-cond-pag
               tt-ped-venda.mo-codigo = INPUT FRAME {&FRAME-NAME} fi-moeda
               tt-ped-venda.no-ab-reppri = repres.nome-abrev
               tt-ped-venda.nr-pedrep = INPUT FRAME {&FRAME-NAME} fi-ped-repres
               tt-ped-venda.nr-tab-finan = IF AVAIL cond-pagto 
                                           THEN cond-pagto.nr-tab-fin ELSE 1
               tt-ped-venda.nr-ind-finan = IF AVAIL cond-pagto
                                           THEN cond-pagto.nr-ind-fin ELSE 1
               tt-ped-venda.no-ab-rep = repres.nome-abrev
               tt-ped-venda.tp-pedido = INPUT FRAME {&FRAME-NAME} cb-ext-tp-pedido
               tt-ped-venda.nome-transp = INPUT FRAME {&FRAME-NAME} fi-nome-transp
               tt-ped-venda.nome-tr-red = INPUT FRAME {&FRAME-NAME} fi-nome-tr-red
               tt-ped-venda.cod-rota = INPUT FRAME {&FRAME-NAME} fi-cod-rota
               tt-ped-venda.observacoes = INPUT FRAME {&FRAME-NAME} ed-obs
               tt-ped-venda.ind-fat-par = NO
               tt-ped-venda.dt-base-ft = INPUT FRAME {&FRAME-NAME} fi-data-base
               tt-ped-venda.dt-entrega = INPUT FRAME {&FRAME-NAME} fi-dt-entrega
               tt-ped-venda.ind-lib-nota = YES.

        IF INPUT FRAME {&FRAME-NAME} cb-tp-frete = 'Cif Total' THEN 
           ASSIGN tt-ped-venda.cidade-cif = tt-ped-venda.cidade.
        ELSE
           ASSIGN tt-ped-venda.cidade-cif = ''.

        FOR EACH tt-itens-ped.
            ASSIGN tt-itens-ped.nome-abrev = tt-ped-venda.nome-abrev
                   tt-itens-ped.nr-pedcli = tt-ped-venda.nr-pedcli
                   tt-itens-ped.cod-estabel = tt-ped-venda.cod-estabel
                   tt-itens-ped.nat-operacao = tt-ped-venda.nat-operacao
                   tt-itens-ped.cod-entrega = tt-ped-venda.cod-entrega
                   tt-itens-ped.nr-tabpre = tt-ped-venda.nr-tabpre
                   tt-itens-ped.vl-preori = tt-itens-ped.vl-preori
                   tt-itens-ped.vl-liq-abe = tt-itens-ped.vl-preori
                   tt-itens-ped.dec-2 = 0
                   tt-itens-ped.char-1 = 'DIGITAÄ«O RµPIDA'.
        END.

        IF NOT AVAIL ped-venda THEN DO.
           RUN esapi/cria-pedvenda.p (INPUT TABLE tt-ped-venda).
           IF RETURN-VALUE = 'ADM-ERROR' THEN
              RETURN 'ADM-ERROR'.

           FIND ped-venda WHERE
                ped-venda.nr-pedcli = tt-ped-venda.nr-pedcli AND ped-venda.nome-abrev = tt-ped-venda.nome-abrev NO-ERROR.
           IF NOT AVAIL ped-venda THEN DO.
              MESSAGE 'N∆o foi Poss°vel Criar o Pedido de Venda ' tt-ped-venda.nr-pedcli  
                      'Favor Verificar...'
                      VIEW-AS ALERT-BOX INFO BUTTONS OK.
              RETURN 'ADM-ERROR'.
           END.
           RUN esapi/cria-log-pedvenda.p (INPUT tt-ped-venda.nr-pedcli,
                                          INPUT tt-ped-venda.nome-abrev,
                                          INPUT "Implantado o Pedido: " + tt-ped-venda.nr-pedcli + "   Cliente:" + tt-ped-venda.nome-abrev,
                                          INPUT NO).
           FIND ped-venda-ext WHERE
                ped-venda-ext.cod-estabel = tt-ped-venda.cod-estabel AND 
                ped-venda-ext.nr-pedido = tt-ped-venda.nr-pedido SHARE-LOCK NO-ERROR.
           IF NOT AVAIL ped-venda-ext THEN DO.
              CREATE ped-venda-ext.
              ASSIGN ped-venda-ext.cod-estabel = tt-ped-venda.cod-estabel 
                     ped-venda-ext.nr-pedido = tt-ped-venda.nr-pedido      
                     ped-venda-ext.nome-abrev = tt-ped-venda.nome-abrev
                     ped-venda-ext.l-etiqueta = NO.
           END.
           ASSIGN ped-venda-ext.tp-pedido = INPUT FRAME {&FRAME-NAME} cb-tp-pedido.

           FIND CURRENT ped-venda-ext NO-LOCK NO-ERROR.
        END.
        ELSE DO. 
            ASSIGN l-desaprovar-cred = NO.
            BUFFER-COMPARE ped-venda TO tt-ped-venda SAVE RESULT IN c-results.

            IF c-results <> '' THEN DO.
               RUN esapi/altera-pedvenda.p (INPUT TABLE tt-ped-venda).
               IF RETURN-VALUE = 'ADM-ERROR' THEN
                  RETURN 'ADM-ERROR'.

               RUN pi-cria-log.
            END.
        END.
        FIND ped-venda-ext WHERE
             ped-venda-ext.cod-estabel = tt-ped-venda.cod-estabel AND 
             ped-venda-ext.nr-pedido = tt-ped-venda.nr-pedido NO-LOCK NO-ERROR.  

        RUN pi-logs.

        FIND CURRENT ped-venda-ext SHARE-LOCK NO-ERROR.
        ASSIGN ped-venda-ext.cod-estabel = INPUT FRAME {&FRAME-NAME} fi-cod-estabel
               ped-venda-ext.nr-pedcli = INPUT FRAME {&FRAME-NAME} fi-pedido-cli
               ped-venda-ext.tp-entrega = INPUT FRAME {&FRAME-NAME} cb-tp-entrega
               ped-venda-ext.num-reserva = INPUT FRAME {&FRAME-NAME} fi-reserva
               ped-venda-ext.fat-max-nf = INPUT FRAME {&FRAME-NAME} fi-fat-max
               ped-venda-ext.tp-frete = INPUT FRAME {&FRAME-NAME} cb-tp-frete
               ped-venda-ext.tp-pagto = INPUT FRAME {&FRAME-NAME} cb-tipo-pagto
               ped-venda-ext.nr-container = INPUT FRAME {&FRAME-NAME} fi-nr-container
               ped-venda-ext.tecelagem = "Todas" 
               ped-venda-ext.l-nao-aprovar = INPUT FRAME {&FRAME-NAME} tg-nao-aprovar
               ped-venda-ext.l-bloqueio = INPUT FRAME {&FRAME-NAME} tg-bloqueio
               ped-venda-ext.compl-observ = INPUT FRAME {&FRAME-NAME} fi-observ-nf.
        FIND CURRENT ped-venda-ext NO-LOCK NO-ERROR.

        IF ped-venda-ext.tp-pagto = "Caixa" THEN DO.
           FIND ped-venda WHERE
                ped-venda.nome-abrev = tt-ped-venda.nome-abrev AND 
                ped-venda.nr-pedcli = tt-ped-venda.nr-pedcli SHARE-LOCK NO-ERROR.
           ASSIGN ped-venda.cod-portador = 99
                  ped-venda.modalidade = 6.
           FIND CURRENT ped-venda NO-LOCK NO-ERROR.
        END.

        /* Manutená∆o os Itens do Pedido */
        IF ped-venda-ext.l-etiqueta = YES THEN DO.  /* Pedido j† foi Separado */
           MESSAGE "Os Itens do Pedido n∆o Poder∆o ser Alterados neste momento pois, " SKIP
                   "este Pedido est† em Processo de Separaá∆o !!!" SKIP(1)
                   "EVENTUAIS ALTERAÄÂES NOS ITENS FORAM DESCONSIDERADAS"
                   VIEW-AS ALERT-BOX INFO BUTTONS OK.    

           IF l-nat-oper THEN DO.   /* Alterou Natureza de Operaá∆o, tem que repassar para os Itens */
              EMPTY TEMP-TABLE tt-ped-item.
              FOR EACH ped-item OF tt-ped-venda.
                  CREATE tt-ped-item.
                  BUFFER-COPY ped-item TO tt-ped-item
                       ASSIGN tt-ped-item.nat-operacao = tt-ped-venda.nat-operacao.
    
                  RUN esapi/altera-peditem-v2.p (INPUT TABLE tt-ped-item).
                  DELETE tt-ped-item.
              END.
           END.
        END.
        ELSE DO.  /* Permite Alterar apenas Pedidos que n∆o est∆o sendo Separados */
           RUN pi-acompanhar IN h-acomp (INPUT "Mantendo Itens...").
           RUN pi-peditem.
           IF RETURN-VALUE = 'ADM-ERROR' THEN
              RETURN 'ADM-ERROR'.
        END.       

        /* Cria Condiá∆o de Pagamento Especial */
        RUN pi-cond-esp. /* Elimina Condiá‰es de Pagamento Especial (se tiver) */
        FIND FIRST tt-cond-ped NO-ERROR.
        IF AVAIL tt-cond-ped THEN DO.
           IF tt-ped-venda.cod-cond-pag = 0 THEN DO.
              RUN pi-acompanhar IN h-acomp (INPUT "Criando Condiá∆o Especial de Pagamento...").
              RUN openQueryStatic IN h-bodi018 (INPUT "Main":U).
              RUN emptyRowErrors IN h-bodi018.
              FOR EACH tt-cond-ped NO-LOCK.
                  EMPTY TEMP-TABLE wt-cond-ped.
                  CREATE wt-cond-ped.
                  BUFFER-COPY tt-cond-ped TO wt-cond-ped.
                  RUN setRecord IN h-bodi018 (INPUT TABLE wt-cond-ped).
                  RUN createRecord IN h-bodi018.
              END.
              RUN getRowErrors IN h-bodi018 (OUTPUT TABLE RowErrors).
              IF CAN-FIND(FIRST RowErrors 
                          WHERE RowErrors.ErrorSubType = "ERROR":U  ) THEN DO:
                 FOR EACH rowerrors WHERE
                          RowErrors.ErrorSubType = "ERROR":U:
                     MESSAGE "Erro ao Criar Condiá∆o de Pagamento Especial" SKIP
                             rowerrors.errordescription 
                             VIEW-AS ALERT-BOX.
                 END.
                 RETURN 'ADM-ERROR':U.
              END.
           END.
        END.

        /* Cria Representantes do Pedido */
        RUN pi-acompanhar IN h-acomp (INPUT "Mantendo Representantes...").
        RUN pi-repres.
        IF RETURN-VALUE = 'ADM-ERROR' THEN
           RETURN 'ADM-ERROR'.

        RUN pi-acompanhar IN h-acomp (INPUT "Finalizando Pedido...").
        /* Se o m¢dulo de exportaá∆o estiver implantado,
           Cria Processo de Exportaá∆o para Pedidos do Tipo Exportaá∆o do Pedido */
        IF param-global.modulo-ex AND
           INPUT FRAME {&FRAME-NAME} cb-tp-pedido = 'Exportaá∆o' THEN
           RUN esapi/cria-pedvenda-cex.p (INPUT tt-ped-venda.nr-pedcli,
                                          INPUT tt-ped-venda.nome-abrev).
        FOR EACH ped-ent OF tt-ped-venda SHARE-LOCK.
            ASSIGN ped-ent.tipo-atend = 2.
        END. 
        FIND FIRST ped-ent OF tt-ped-venda NO-LOCK NO-ERROR.

        /*Completa o Pedido */
        IF ped-venda.tp-pedido = "PE" THEN DO.
           RUN pi-acompanhar IN h-acomp (INPUT "Completando o Pedido...").
           IF NOT ped-venda.completo THEN DO.
              RUN esapi/completa-pedvenda.p (INPUT tt-ped-venda.nr-pedido).
              IF ped-venda.completo THEN
                 RUN esapi/cria-log-pedvenda.p (INPUT ped-venda.nr-pedcli,
                                                INPUT ped-venda.nome-abrev,
                                                INPUT "Pedido Completo",
                                                INPUT NO).
           END.
        END.

        /* Valida CrÇdito do Pedido*/
        RUN esapi/credito-pedvenda.p.
        IF l-desaprovar-cred THEN DO.
           ASSIGN c-desc-bloq-cr = "Alterado Cond. Pagto".
           RUN pi-desaprova-cred.
        END.

        /* Valida Preáos */
        RUN pi-acompanhar IN h-acomp (INPUT "Validando Preáos...").
        IF p-acao = 'Incluir' OR 
           l-preco-alterado = YES OR
           l-incluiu-item = YES THEN DO.
           FIND ped-venda WHERE 
                ped-venda.nome-abrev = tt-ped-venda.nome-abrev AND
                ped-venda.nr-pedcli = tt-ped-venda.nr-pedcli SHARE-LOCK NO-ERROR.

           ASSIGN ped-venda.log-ped-bonif-pendente = NO
                  ped-venda.cod-sit-com = 2.

           IF ped-venda.tp-preco = 1 THEN DO.
              RUN esapi/valida-preco.p (INPUT ped-venda.nr-pedcli,
                                        OUTPUT l-ok).
              IF l-ok = NO THEN DO.
                 RUN esapi/cria-log-pedvenda.p (INPUT ped-venda.nr-pedcli,
                                                INPUT ped-venda.nome-abrev,
                                                INPUT "Preáo Inv†lido, Pedido Requer Aprovaá∆o",
                                                INPUT NO).
              END.
           END.
        END.

        IF p-acao = 'Incluir' THEN DO.
           IF NOT LOOKUP(ped-venda-ext.tp-pedido,c-tpped-cred-aut) > 0 AND  /* fora da lista */ 
              (ped-venda.cod-sit-aval = 1 OR ped-venda.cod-sit-aval = 4) THEN
               ASSIGN ped-venda.cod-sit-ped = 1
                      ped-venda.quem-aprovou = ""
                      ped-venda.dt-apr-cred = ?.

           IF (ped-venda-ext.tp-pedido = "Amostra" OR
               ped-venda-ext.tp-pedido = "Amostra Exportaá∆o") AND
              (ped-venda.cod-sit-aval = 1 OR ped-venda.cod-sit-aval = 4) THEN DO.
              FIND natur-oper WHERE
                   natur-oper.nat-operacao = ped-venda.nat-oper NO-LOCK NO-ERROR.
              IF natur-oper.emite-duplic = YES THEN
                 ASSIGN ped-venda.cod-sit-ped = 1.
           END.

           FIND proc-ped-venda WHERE
                proc-ped-venda.nome-abrev = tt-ped-venda.nome-abrev AND
                proc-ped-venda.nr-pedcli = tt-ped-venda.nr-pedcli
                NO-LOCK NO-ERROR.
           IF AVAIL proc-ped-venda THEN
              RUN utp/ut-msgs.p (INPUT "show",
                                 INPUT 28530,
                                 INPUT proc-ped-venda.nr-proc-exp).
        END.
        ELSE
           ASSIGN ped-venda.cod-sit-ped = IF ped-venda.cod-sit-ped <> 3
                                          THEN i-cod-sit-ped
                                          ELSE ped-venda.cod-sit-ped. 

        IF VALID-HANDLE(h-bodi018) THEN
           DELETE PROCEDURE h-bodi018.
        IF VALID-HANDLE(h-bodi157) THEN
           DELETE PROCEDURE h-bodi157.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-popula-browse w-window 
PROCEDURE pi-popula-browse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH tt-itens-ped.
        DELETE tt-itens-ped.
    END.

    ASSIGN fi-tot-qtd-ped = 0     fi-tot-qtd-res = 0
           fi-tot-qtd-fat = 0     fi-tot-vlr-ped = 0
           fi-tot-vlr-fat = 0     fi-tot-vlr-abe = 0.

    IF ped-venda.cod-estabel = "1" THEN DO.  /* Ima */
       FIND unid-feder WHERE 
            unid-feder.estado = emitente.estado NO-LOCK NO-ERROR.
       IF unid-feder.char-2 = 'SUL' OR
          (unid-feder.char-2 = 'SUDESTE' AND unid-feder.estado <> "ES") THEN
          FIND im-param WHERE
               im-param.cod-param = "TABELA_IMA12" NO-LOCK NO-ERROR.
       ELSE
          FIND im-param WHERE
               im-param.cod-param = "TABELA_IMA07" NO-LOCK NO-ERROR.
    END.
    ELSE
       FIND im-param WHERE
            im-param.cod-param = "TABELA_MED" NO-LOCK NO-ERROR.

    IF AVAIL im-param THEN
       ASSIGN c-tb-preco-pad = im-param.val-param.

    FOR EACH ped-item OF ped-venda NO-LOCK,
        EACH ped-item-ext WHERE
             ped-item-ext.cod-estabel = ped-venda.cod-estabel AND
             ped-item-ext.nr-pedcli = ped-item.nr-pedcli AND 
             ped-item-ext.nome-abrev = ped-item.nome-abrev AND
             ped-item-ext.nr-sequencia = ped-item.nr-sequencia NO-LOCK. 

        FIND ITEM WHERE
             ITEM.it-codigo = ped-item.it-codigo NO-LOCK NO-ERROR.

        CREATE tt-itens-ped.
        BUFFER-COPY ped-item TO tt-itens-ped
               ASSIGN tt-itens-ped.cod-estabel = ped-venda.cod-estabel.

        IF ped-item.cod-sit-item <> 6 THEN  /* N∆o Soma Itens Cancelados */
           ASSIGN fi-tot-qtd-ped = fi-tot-qtd-ped + ped-item.qt-pedida
                  fi-tot-vlr-ped = fi-tot-vlr-ped + (ped-item.qt-pedida * ped-item.vl-preori).

        ASSIGN tt-itens-ped.bloqueio-fat = ped-item-ext.bloqueio-fat.

        ASSIGN tt-itens-ped.vl-pre-calc = 0.
        IF c-tb-preco-pad <> '' THEN DO.
           FIND preco-item WHERE
                preco-item.nr-tabpre = c-tb-preco-pad AND
                preco-item.it-codigo = ped-item.it-codigo AND
                preco-item.cod-refer = ped-item.cod-refer AND
                preco-item.cod-unid-med = item.un
                NO-LOCK NO-ERROR.

           IF AVAIL preco-item THEN
              ASSIGN tt-itens-ped.vl-pre-calc = (preco-item.preco-venda * de-ind-finan).
        END.

        FOR EACH nota-fiscal WHERE
                 nota-fiscal.nome-ab-cli = ped-venda.nome-abrev AND
                 nota-fiscal.nr-pedcli = ped-venda.nr-pedcli AND
                 nota-fiscal.dt-cancela  = ? NO-LOCK.
            FIND it-nota-fisc OF nota-fiscal WHERE
                 it-nota-fisc.nr-seq-ped = ped-item.nr-sequencia NO-LOCK NO-ERROR.
            IF AVAIL it-nota-fisc THEN
               ASSIGN tt-itens-ped.nr-nota-fis = nota-fiscal.nr-nota-fis
                      tt-itens-ped.dt-emis-nf = nota-fiscal.dt-emis
                      tt-itens-ped.dt-saida-nf = nota-fiscal.dt-saida
                      fi-tot-qtd-fat = fi-tot-qtd-fat + it-nota-fisc.qt-faturada[1]
                      fi-tot-vlr-fat = fi-tot-vlr-fat + it-nota-fisc.vl-tot-item.
        END.


        ASSIGN tt-itens-ped.dec-2:VISIBLE IN BROWSE br-ped-item = NO.
        IF p-acao <> 'Consultar' THEN DO.
           ASSIGN tt-itens-ped.dec-2:VISIBLE IN BROWSE br-ped-item = YES.
           /*
           RUN pi-estoque (INPUT ped-venda.cod-estabel,
                           INPUT tt-itens-ped.it-codigo,
                           INPUT tt-itens-ped.cod-refer,
                           INPUT SUBSTR(ped-item-ext.lote,1,2)).
    
           ASSIGN de-qtidade-atu = 0.
           FOR EACH tt-estoque NO-LOCK.
               ASSIGN de-qtidade-atu = de-qtidade-atu + tt-estoque.qt-saldo.
           END.
            
           IF de-qtidade-atu < 0 THEN
              ASSIGN de-qtidade-atu = 0.
    
           ASSIGN tt-itens-ped.dec-2 = de-qtidade-atu.
           */
           ASSIGN tt-itens-ped.dec-2 = ?.
        END.

        FIND ped-item-res OF ped-item NO-LOCK NO-ERROR.        /*  daf  */

      /*  FIND ped-item-res WHERE
             ped-item-res.cod-estabel = ped-venda.cod-estabel AND  
             ped-item-res.nome-abrev = ped-item.nome-abrev AND   
             ped-item-res.nr-pedcli = ped-item.nr-pedcli AND    
             ped-item-res.nr-sequencia = ped-item.nr-sequencia AND   
             ped-item-res.it-codigo = ped-item.it-codigo AND  
             ped-item-res.cod-refer = ped-item.cod-refer 
             NO-LOCK NO-ERROR.  */

        IF AVAIL ped-item-res THEN
            ASSIGN tt-itens-ped.qt-reserva = ped-item-res.qt-pedida
                   fi-tot-qtd-res = fi-tot-qtd-res + ped-item-res.qt-pedida.
    END.
    {&OPEN-QUERY-br-ped-item}

    ASSIGN fi-tot-vlr-abe = fi-tot-vlr-ped - fi-tot-vlr-fat.
    DISP fi-tot-qtd-ped
         fi-tot-qtd-res
         fi-tot-qtd-fat
         fi-tot-vlr-ped
         fi-tot-vlr-fat
         fi-tot-vlr-abe
         WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-repres w-window 
PROCEDURE pi-repres :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN pi-acompanhar IN h-acomp (INPUT "Criando Representante...").

    ASSIGN INPUT FRAME {&FRAME-NAME} fi-perc-comis.

    FIND ped-repre WHERE
         ped-repre.nr-pedido = INT(tt-ped-venda.nr-pedcli) AND
         ped-repre.nome-ab-rep = tt-ped-venda.no-ab-reppri NO-LOCK NO-ERROR.

    IF NOT AVAIL ped-repre THEN DO.
       RUN esapi/cria-repres.p (INPUT INT(tt-ped-venda.nr-pedcli),
                                INPUT tt-ped-venda.no-ab-reppri,
                                INPUT fi-perc-comis).

       IF RETURN-VALUE = 'ADM-ERROR' THEN 
          RETURN 'ADM-ERROR'.
    END.
    ELSE DO.
       IF ped-repre.perc-comis <> fi-perc-comis THEN DO.
          RUN esapi/altera-repres.p (INPUT INT(tt-ped-venda.nr-pedcli),
                                     INPUT tt-ped-venda.no-ab-reppri,
                                     INPUT fi-perc-comis).
          IF RETURN-VALUE = 'ADM-ERROR' THEN
             RETURN 'ADM-ERROR'.

          RUN esapi/cria-log-pedvenda.p (INPUT tt-ped-venda.nr-pedcli,
                                         INPUT tt-ped-venda.nome-abrev,
                                         INPUT "Alterada a Comiss∆o do Representante, Para: " + TRIM(STRING(fi-perc-comis,">>9.99")),
                                         INPUT YES).
       END.
    END.
    
    /* Verificar Desconto Informado */ 
    FIND ped-repre WHERE
         ped-repre.nr-pedido = INT(tt-ped-venda.nr-pedcli) AND
         ped-repre.nome-ab-rep = 'Fulano' NO-LOCK NO-ERROR.
    
    IF AVAIL ped-repre THEN
       RUN esapi/elimina-repres.p (INPUT INT(tt-ped-venda.nr-pedcli),
                                   INPUT 'Fulano').
    
    IF tt-ped-venda.des-pct-desconto-inform <> "" THEN DO:   /* Cria Fulano */
       FIND ped-repre WHERE
            ped-repre.nr-pedido = INT(tt-ped-venda.nr-pedcli) AND
            ped-repre.nome-ab-rep = 'Fulano' NO-LOCK NO-ERROR.

       RUN esapi/cria-repres.p (INPUT INT(tt-ped-venda.nr-pedcli),
                                INPUT 'Fulano',
                                INPUT DEC(tt-ped-venda.des-pct-desconto-inform) ).

       IF RETURN-VALUE = 'ADM-ERROR' THEN 
          RETURN 'ADM-ERROR'.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-validate w-window 
PROCEDURE pi-validate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
    IF INPUT FRAME {&FRAME-NAME} fi-cliente = '' THEN DO.
       MESSAGE 'Cliente deve ser Informado....'
           VIEW-AS ALERT-BOX ERROR BUTTONS OK.
       APPLY 'ENTRY' TO fi-cliente.
       RETURN 'ADM-ERROR'.
    END.

    FIND repres WHERE
         repres.nome-abrev = INPUT FRAME {&FRAME-NAME} fi-cod-rep NO-LOCK NO-ERROR.
    IF NOT AVAIL repres THEN DO.
       MESSAGE 'Representante n∆o Cadastrado....'
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
       APPLY 'ENTRY' TO fi-cod-rep.
       RETURN 'ADM-ERROR'.
    END.

    FIND natur-oper WHERE
         natur-oper.nat-operacao = INPUT FRAME {&FRAME-NAME} fi-natur-oper NO-LOCK NO-ERROR.

    IF NOT AVAIL natur-oper THEN DO.
       MESSAGE 'Natureza de Operaá∆o n∆o Cadastrada....'
           VIEW-AS ALERT-BOX ERROR BUTTONS OK.
       APPLY 'ENTRY' TO fi-natur-oper.
       RETURN 'ADM-ERROR'.
    END.

    FIND FIRST tt-itens-ped NO-LOCK NO-ERROR.
    IF NOT AVAIL tt-itens-ped THEN DO.
       MESSAGE 'Para Gravar o Pedido Ç necess†rio pelo menos 1 (um) Item...'
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
       APPLY 'ENTRY' TO bt-inc.
       RETURN 'ADM-ERROR'.
    END.

    IF cb-tp-pedido:SCREEN-VALUE = 'Operaá∆o Triangular' AND 
       INPUT FRAME {&FRAME-NAME} fi-cliente-tri = '' THEN DO.
        MESSAGE 'Pedido do Tipo Operaá∆o Triangular, deve ser Informado o Cliente de Remessa...'
             VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        APPLY 'ENTRY' TO fi-cliente-tri.
        RETURN 'ADM-ERROR'.
    END.

    IF cb-tp-pedido:SCREEN-VALUE = 'Venda Confec.' AND
       INPUT FRAME {&FRAME-NAME} fi-observ-nf = '' THEN DO.
        MESSAGE 'Para pedidos do Tipo Venda Confeccionada, deve ser Informado uma mensagem...'
             VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        APPLY 'ENTRY' TO fi-observ-nf.
        RETURN 'ADM-ERROR'.
    END.

    IF fi-nome-transp:SCREEN-VALUE = "" THEN DO.
       MESSAGE 'Transportadora deve ser Informada....'
             VIEW-AS ALERT-BOX ERROR BUTTONS OK.
       APPLY 'ENTRY' TO fi-nome-transp.
       RETURN 'ADM-ERROR'.
    END.
    
    IF cb-tp-pedido:SCREEN-VALUE MATCHES '*Exportaá∆o*' AND
       INPUT FRAME {&FRAME-NAME} fi-cod-rota = '' THEN DO.
       MESSAGE 'Para Pedidos do Tipo Exportaá∆o, deve ser Informado uma Rota...'
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
       APPLY 'ENTRY' TO fi-cod-rota.
       RETURN 'ADM-ERROR'.
    END.

    IF p-acao = 'Incluir' AND
       (cb-tp-frete:SCREEN-VALUE = ? OR 
        cb-tp-frete:SCREEN-VALUE =  '') THEN DO.
       MESSAGE 'Tipo de Frete deve ser Informado....'
             VIEW-AS ALERT-BOX ERROR BUTTONS OK.
       APPLY 'ENTRY' TO cb-tp-frete.
       RETURN 'ADM-ERROR'.
    END.


    IF INPUT FRAME {&FRAME-NAME} fi-moeda <> 0 THEN DO: 
       MESSAGE "A moeda escolhida para este pedido n∆o Ç o REAL ! ! !" SKIP
               "Deseja prosseguir com este pedido?"
           VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                   TITLE "" UPDATE choice AS LOGICAL.
       IF choice = FALSE THEN
          RETURN 'ADM-ERROR'.
    END.

    FIND FIRST tt-itens-ped NO-LOCK NO-ERROR.
    IF NOT AVAIL tt-itens-ped THEN DO.
       MESSAGE 'N∆o foi Encontrado Itens para o Pedido....'
              VIEW-AS ALERT-BOX ERROR BUTTONS OK.
       APPLY 'ENTRY' TO bt-inc.
       RETURN 'ADM-ERROR'.
    END.
    
    FOR EACH tt-itens-ped WHERE
             tt-itens-ped.tp-acao <> '' NO-LOCK.

        FIND ped-item OF tt-itens-ped NO-LOCK NO-ERROR.
        IF AVAIL ped-item AND
           ped-item.cod-sit-item <> 1 THEN DO.
           MESSAGE "Situaá∆o do Item " tt-itens-ped.nr-sequencia " N∆o Permite alteraá∆o...."
                  VIEW-AS ALERT-BOX ERROR BUTTONS OK.
           APPLY 'ENTRY' TO br-ped-item.
           RETURN 'ADM-ERROR'.
        END.

        FIND b-itens-ped WHERE
             b-itens-ped.it-codigo = tt-itens-ped.it-codigo AND
             b-itens-ped.cod-refer = tt-itens-ped.cod-refer AND
             b-itens-ped.cod-sit-item <> 6
             NO-LOCK NO-ERROR.
         IF AMBIGUOUS b-itens-ped THEN DO.
            MESSAGE "Item/Referencia: " tt-itens-ped.it-codigo tt-itens-ped.cod-refer
                    "Em Duplicidade, favor verificar..."
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            APPLY 'entry' TO SELF.
            RETURN NO-APPLY.
         END.
        
        FIND ped-item-res WHERE
             ped-item-res.cod-estabel = ped-venda.cod-estabel AND  
             ped-item-res.nome-abrev = ped-item.nome-abrev AND   
             ped-item-res.nr-pedcli = ped-item.nr-pedcli AND    
             ped-item-res.nr-sequencia = ped-item.nr-sequencia AND   
             ped-item-res.it-codigo = ped-item.it-codigo AND  
             ped-item-res.cod-refer = ped-item.cod-refer 
             NO-LOCK NO-ERROR.  
        IF AVAIL ped-item-res AND 
           ped-item-res.qt-pedida > 0 AND
           SUBSTR(tt-itens-ped.char-2,1550,1) = "S" THEN DO.

           MESSAGE "Ainda Existem Etiquetas Reservadas para essa Sequància," SKIP
                   "Alteraá‰es e Cancelamentos s¢ ser∆o permitidos se Cancelar a Reserva." SKIP
                   "Deseja Cancelar a Reserva ?"
                   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                   TITLE "" UPDATE l-opcao AS LOGICAL.
           IF l-opcao = NO THEN
              RETURN 'ADM-ERROR'.

           RUN pi-cancela-reserva.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-ver-permissao w-window 
PROCEDURE pi-ver-permissao :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR c-grupos AS CHAR FORMAT "x(20)".
    
    ASSIGN l-tem-acesso = NO.
    FIND FIRST espec.param-dis NO-LOCK NO-ERROR.

    CASE SELF:NAME.
        WHEN "bt-imp-res" THEN
           ASSIGN c-grupos = espec.param-dis.grp-imp-res.
        WHEN "bt-copia-it-ped" THEN
           ASSIGN c-grupos = espec.param-dis.grp-cop-item.
        WHEN "bt-inc" THEN
           ASSIGN c-grupos = espec.param-dis.grp-inc-item.
        WHEN "bt-mod" THEN
           ASSIGN c-grupos = espec.param-dis.grp-alt-item.
        WHEN "bt-del" THEN
           ASSIGN c-grupos = espec.param-dis.grp-div-item.
        WHEN "bt-can" THEN
           ASSIGN c-grupos = espec.param-dis.grp-can-item.
        WHEN "bt-bloq" THEN
           ASSIGN c-grupos = espec.param-dis.grp-can-item.
    END CASE.

    FOR EACH usuar_grp_usuar WHERE usuar_grp_usuar.cod_usuario = c-seg-usuario NO-LOCK:
        IF INDEX(c-grupos,usuar_grp_usuar.cod_grp_usuar) <> 0 THEN DO:
           ASSIGN l-tem-acesso = YES.
           LEAVE.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-zoom-item w-window 
PROCEDURE pi-zoom-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {include/zoomvar.i &prog-zoom=inzoom/z01in172.w
                       &campo=tt-itens-ped.it-codigo
                       &campozoom=it-codigo
                       &BROWSE=br-ped-item}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-zoom-refer w-window 
PROCEDURE pi-zoom-refer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF INPUT FRAME {&FRAME-NAME} cb-ext-tp-pedido = 'PI' THEN DO.
      FOR EACH tt-itens-ped WHERE
               tt-itens-ped.it-codigo = item.it-codigo  NO-LOCK.
          CREATE tt-ref-item.
          ASSIGN tt-ref-item.cod-refer  = tt-itens-ped.cod-refer
                 tt-ref-item.preco-un   = tt-itens-ped.vl-preuni
                 tt-ref-item.qt-pedida  = tt-itens-ped.qt-pedida
                 tt-ref-item.vl-tot-ref = tt-itens-ped.vl-preuni * tt-itens-ped.qt-pedida.
      END.
       
      RUN esp/espp002a.w (INPUT-OUTPUT TABLE tt-ref-item,
                          INPUT item.it-codigo,
                          INPUT c-tab-preco,
                          INPUT de-ind-finan,
                          INPUT INPUT FRAME {&FRAME-NAME} fi-cod-estabel, 
                          INPUT INPUT FRAME {&FRAME-NAME} fi-nr-container,
                          INPUT INPUT FRAME {&FRAME-NAME} cb-tp-preco).
    
      FIND FIRST tt-ref-item NO-LOCK NO-ERROR.
      IF AVAIL tt-ref-item THEN DO.
         RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
         {utp/ut-liter.i Importando_Referencias *}
         RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

         FOR EACH tt-itens-ped WHERE
                  tt-itens-ped.it-codigo = item.it-codigo NO-LOCK.
             DELETE tt-itens-ped.
         END.
    
         FOR EACH tt-ref-item WHERE
                  tt-ref-item.qt-pedida > 0 NO-LOCK.

             RUN pi-acompanhar IN h-acomp (INPUT "Referencia: " + tt-ref-item.cod-refer).

             ASSIGN de-qtidade-atu = 0.
             FOR EACH pp-it-container WHERE
                      pp-it-container.it-comprado = item.it-codigo AND
                      pp-it-container.ref-comprada = tt-ref-item.cod-refer NO-LOCK.
                 ASSIGN de-qtidade-atu = de-qtidade-atu + (pp-it-container.qt-pedida - pp-it-container.qt-vendida - tt-ref-item.qt-pedida).
             END.

             IF tt-ref-item.qt-pedida > de-qtidade-atu THEN NEXT.

             FIND LAST b-itens-ped USE-INDEX indice-1 NO-LOCK NO-ERROR.

             CREATE tt-itens-ped.
             ASSIGN tt-itens-ped.tp-acao = 'Incluir'
                    tt-itens-ped.nr-sequencia = IF AVAIL b-itens-ped
                                                THEN b-itens-ped.nr-sequencia + 10
                                                ELSE 10
                    tt-itens-ped.it-codigo = item.it-codigo
                    tt-itens-ped.cod-refer = tt-ref-item.cod-refer
                    tt-itens-ped.qt-pedida = tt-ref-item.qt-pedida
                    tt-itens-ped.vl-preuni = ROUND(tt-ref-item.preco-un,2)
                    tt-itens-ped.vl-preori = ROUND(tt-ref-item.preco-un,2).

             ASSIGN tt-itens-ped.dec-2 = de-qtidade-atu.
         END.
         RUN pi-finalizar in h-acomp.

         ASSIGN bt-del:SENSITIVE IN FRAME {&FRAME-NAME} = YES
                bt-inc:SENSITIVE IN FRAME {&FRAME-NAME} = YES
                bt-mod:SENSITIVE IN FRAME {&FRAME-NAME} = YES
                bt-can:SENSITIVE IN FRAME {&FRAME-NAME} = YES
                bt-ok:SENSITIVE IN FRAME {&FRAME-NAME}  = YES.

         RUN adm-open-query-cases. 
         APPLY 'ROW-LEAVE' TO br-ped-item IN FRAME {&FRAME-NAME}.
      END.
   END.
   ELSE DO.
       EMPTY TEMP-TABLE tt-ref-item.
       FOR EACH tt-itens-ped WHERE
                tt-itens-ped.it-codigo = item.it-codigo NO-LOCK.
           CREATE tt-ref-item.
           ASSIGN tt-ref-item.cod-refer   = tt-itens-ped.cod-refer
                  tt-ref-item.cod-depos   = 'ARM'
                  tt-ref-item.preco-un    = tt-itens-ped.vl-preuni
                  tt-ref-item.qt-pedida   = tt-itens-ped.qt-pedida
                  tt-ref-item.vl-tot-ref  = tt-itens-ped.vl-preuni * tt-itens-ped.qt-pedida.
       END.
    
       RUN esp/espd4000f.w (INPUT-OUTPUT TABLE tt-ref-item,
                            INPUT item.it-codigo,
                            INPUT c-tab-preco,
                            INPUT de-ind-finan,
                            INPUT fi-cod-estabel:SCREEN-VALUE IN FRAME {&FRAME-NAME},
                            INPUT 'ARM').
    
       FIND FIRST tt-ref-item NO-LOCK NO-ERROR.
       IF AVAIL tt-ref-item THEN DO.
          RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
          {utp/ut-liter.i Importando_Referencias *}
          RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

          FOR EACH tt-itens-ped WHERE
                   tt-itens-ped.it-codigo = item.it-codigo NO-LOCK.
              DELETE tt-itens-ped.
          END.
    
          FOR EACH tt-ref-item WHERE
                   tt-ref-item.qt-pedida > 0 NO-LOCK.
    
              RUN pi-acompanhar IN h-acomp (INPUT "Referencia: " + tt-ref-item.cod-refer).

              IF tt-ref-item.qt-pedida = 0 THEN NEXT.

              ASSIGN de-qtidade-atu = 0.
              FOR EACH saldo-estoq WHERE
                       saldo-estoq.cod-estabel = INPUT FRAME {&FRAME-NAME} fi-cod-estabel AND 
                       saldo-estoq.it-codigo = item.it-codigo AND
                       saldo-estoq.cod-refer = tt-ref-item.cod-refer AND 
                       saldo-estoq.cod-depos = 'ARM' AND
                       saldo-estoq.cod-localiz = '' NO-LOCK.
                  ASSIGN de-qtidade-atu = de-qtidade-atu + saldo-estoq.qtidade-atu.
              END.
    
              /* Subtrai Pedidos PI */
              FOR EACH ped-venda WHERE
                       ped-venda.cod-sit-ped = 1 NO-LOCK,
                  EACH ped-item OF ped-venda WHERE
                       ped-item.cod-sit-item = 1 AND
                       ped-item.it-codigo = item.it-codigo AND
                       ped-item.cod-refer = tt-ref-item.cod-refer NO-LOCK.

                  IF ped-venda.tp-pedido = 'PI' AND
                     ped-venda.dt-entrega > TODAY THEN NEXT.

                  ASSIGN de-qtidade-atu = de-qtidade-atu - ped-item.qt-pedida.
              END.

              /* Subtrai Notas em Aberto */ 
              FOR EACH nota-fiscal WHERE
                       nota-fiscal.cod-estabe = INPUT FRAME {&FRAME-NAME} fi-cod-estabel AND
                       nota-fiscal.serie = para-fat.serie-pad AND
                       nota-fiscal.dt-cancela = ? AND
                       nota-fiscal.dt-confirma = ? NO-LOCK,
                  EACH it-nota-fisc OF nota-fiscal WHERE
                       it-nota-fisc.it-codigo = item.it-codigo AND
                       it-nota-fisc.cod-refer = tt-ref-item.cod-refer 
                       NO-LOCK.
                  ASSIGN de-qtidade-atu = de-qtidade-atu - it-nota-fisc.qt-faturada[1].
              END.

              IF tt-ref-item.qt-pedida > de-qtidade-atu THEN NEXT.

              FIND LAST b-itens-ped USE-INDEX indice-1 NO-LOCK NO-ERROR.
    
              CREATE tt-itens-ped.
              ASSIGN tt-itens-ped.tp-acao = 'Incluir'
                     tt-itens-ped.nr-sequencia = IF AVAIL b-itens-ped
                                                 THEN b-itens-ped.nr-sequencia + 10
                                                 ELSE 10
                     tt-itens-ped.it-codigo = item.it-codigo
                     tt-itens-ped.cod-refer = tt-ref-item.cod-refer
                     tt-itens-ped.qt-pedida = tt-ref-item.qt-pedida
                     tt-itens-ped.vl-preuni = ROUND(tt-ref-item.preco-un,2)
                     tt-itens-ped.vl-preori = ROUND(tt-ref-item.preco-un,2).
    
              ASSIGN tt-itens-ped.dec-2 = de-qtidade-atu.
          END.
          RUN pi-finalizar in h-acomp.

          ASSIGN bt-del:SENSITIVE IN FRAME {&FRAME-NAME} = YES
                 bt-inc:SENSITIVE IN FRAME {&FRAME-NAME} = YES
                 bt-mod:SENSITIVE IN FRAME {&FRAME-NAME} = YES
                 bt-can:SENSITIVE IN FRAME {&FRAME-NAME} = YES
                 bt-ok:SENSITIVE IN FRAME {&FRAME-NAME}  = YES.
    
          RUN adm-open-query-cases. 
          APPLY 'ROW-LEAVE' TO br-ped-item IN FRAME {&FRAME-NAME}.
       END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-window  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-itens-ped"}
  {src/adm/template/snd-list.i "ITEM"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-window 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-obsoleto w-window 
FUNCTION fn-obsoleto RETURNS CHARACTER
  ( INPUT p-it-codigo AS CHAR, INPUT p-cod-refer AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  /*
  FIND ref-item-ext WHERE
       ref-item-ext.it-codigo = p-it-codigo AND
       ref-item-ext.cod-refer = p-cod-refer
       NO-LOCK NO-ERROR.

  IF AVAIL ref-item-ext THEN DO.
     CASE ref-item-ext.cod-obsoleto.
         WHEN '0' THEN RETURN 'Lanáamento'.
         WHEN '1' THEN RETURN 'Fora de Produá∆o'.
         WHEN '2' THEN RETURN 'Em Produá∆o'.
         WHEN '3' THEN RETURN 'Retalho'.
         WHEN '4' THEN RETURN 'Excluisividade'.
     END CASE.
  END.
  ELSE
     RETURN "N∆o Definido".  
   */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-situacao w-window 
FUNCTION fn-situacao RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
   IF tt-itens-ped.bloqueio-fat THEN
      RETURN "BLQ".

   CASE tt-itens-ped.cod-sit-item.
         WHEN 1 THEN RETURN 'ABE'.
         WHEN 2 THEN RETURN 'ATP'.
         WHEN 3 THEN RETURN 'ATT'.
         WHEN 4 THEN RETURN 'PEN'.
         WHEN 5 THEN RETURN 'SUS'.
         WHEN 6 THEN RETURN 'CAN'.
   END CASE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

