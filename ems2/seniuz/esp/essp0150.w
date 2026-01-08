&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          ems2med          PROGRESS
*/
&Scoped-define WINDOW-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-livre 
/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESSP0150 2.04.00.999}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

DEF TEMP-TABLE tt-itens
    FIELD it-codigo    LIKE ob-etiqueta.it-codigo
    FIELD cod-refer    LIKE ob-etiqueta.cod-refer
    FIELD lote         LIKE ob-etiqueta.nr-lote
    FIELD qt-estoque   LIKE ob-etiqueta.quantidade
    FIELD qt-pedida    LIKE ob-etiqueta.quantidade
    FIELD qt-benefic   LIKE ob-etiqueta.quantidade
    FIELD visualiza    AS   LOG INIT NO
    INDEX indice1 IS PRIMARY it-codigo cod-refer lote.

DEF TEMP-TABLE tt-detalhe
    FIELD corte-comerc LIKE ob-etiqueta.corte-comerc
    FIELD qt-estoque   LIKE ob-etiqueta.quantidade  LABEL "Qt Estoq"
    INDEX indice1 IS PRIMARY corte-comerc.
    
DEF TEMP-TABLE tt-movto
    FIELD row-tt-itens   AS ROWID
    FIELD lote           LIKE ob-etiqueta.nr-lote
    FIELD qt-estoque     LIKE ob-etiqueta.quantidade   LABEL "Estoque"
    FIELD qt-res-antc    LIKE ob-etiqueta.quantidade   LABEL "Ped.Pend."
    FIELD qt-ped-reserva LIKE ob-etiqueta.quantidade   LABEL "Ped.Reserva"
    FIELD qt-trf         AS   DEC FORMAT "->>>,>>9.99" LABEL "Transform"
    FIELD qt-benefic     LIKE ob-etiqueta.quantidade   LABEL "Benefic"
    FIELD qt-carteira    LIKE ped-item.qt-pedida       LABEL "Carteira"
    FIELD qt-res-cart    LIKE ob-etiqueta.quantidade   LABEL "Cart.Forn."
    FIELD qt-saldo       LIKE saldo-estoq.qtidade-atu  LABEL "SALDO"
    FIELD visualiza      AS   LOG INIT NO
    INDEX indice1 IS PRIMARY row-tt-itens lote.

DEF TEMP-TABLE tt-etiquetas
    FIELD cod-estabel  AS   CHAR
    FIELD row-tt-movto AS ROWID
    FIELD num-etiqueta LIKE ob-etiqueta.num-etiqueta
    FIELD visualiza   AS  LOG INIT NO
    INDEX indice1 IS PRIMARY row-tt-movto num-etiqueta.

DEF TEMP-TABLE tt-pedidos
    FIELD row-tt-movto AS ROWID
    FIELD nr-pedcli    LIKE ped-venda.nr-pedcli
    FIELD nome-abrev   LIKE ped-venda.nome-abrev
    FIELD dt-entrega   LIKE ped-venda.dt-entrega
    FIELD qt-pedida    LIKE ped-item.qt-pedida
    FIELD qt-reservada LIKE ped-item.qt-pedida
    FIELD visualiza    AS   LOG
    INDEX indice1 IS PRIMARY row-tt-movto nr-pedcli.

DEF TEMP-TABLE tt-ped-item
    FIELD row-tt-movto AS ROWID
    FIELD nr-pedcli    LIKE ped-venda.nr-pedcli
    FIELD nome-abrev   LIKE ped-venda.nome-abrev
    FIELD nr-sequencia LIKE ped-item.nr-sequencia
    FIELD qt-pedida    LIKE ob-etiqueta.quantidade  LABEL "Qt Estoq"
    FIELD qt-reservada LIKE ped-item.qt-pedida
    FIELD visualiza    AS   LOG
    INDEX indice1 IS PRIMARY nr-pedcli nome-abrev nr-sequencia.

DEF TEMP-TABLE tt-trf
    FIELD row-tt-movto AS ROWID
    FIELD num-trf      LIKE ob-trf.num-trf
    FIELD visualiza    AS   LOG
    INDEX indice1 IS PRIMARY row-tt-movto num-trf.

DEF TEMP-TABLE tt-ped-reserva
    FIELD row-tt-movto AS ROWID
    FIELD num-reserva  LIKE ped-reserva.num-reserva
    FIELD visualiza    AS   LOG
    INDEX indice1 IS PRIMARY row-tt-movto num-reserva.

DEF NEW SHARED TEMP-TABLE wt-etiquetas
    FIELD num-etiqueta LIKE ob-etiqueta.num-etiqueta.

DEF TEMP-TABLE tt-digita
    FIELD opcao  AS CHAR
    FIELD campo AS CHAR
    FIELD valor AS CHAR.

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

/* Buffer Definitions */
DEF BUFFER b-estrutura FOR estrutura.              
DEF BUFFER b-tt-itens FOR tt-itens.

/* Global Variable Definitions ---                                       */
DEF NEW GLOBAL SHARED VAR gr-ob-etiqueta AS ROWID NO-UNDO.
DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.
DEF NEW SHARED VAR p-cod-estabel-182 AS CHAR.
DEF NEW SHARED VAR p-it-codigo-182 AS CHAR.
DEF NEW SHARED VAR p-cod-refer-182 AS CHAR.
DEF NEW SHARED VAR p-manut-182 AS LOG.

/* Variaveis de Parametro Enviada pelo programa ESSP0170.W */
/*                                                         */
DEF {1} VAR p-it-codigo-150 AS CHAR.
DEF {1} VAR p-cod-refer-150 AS CHAR.
DEF {1} VAR p-lote-rp-150   AS LOG.

/* Local Variable Definitions ---                                       */
DEF VAR h-acomp            AS HANDLE NO-UNDO.
DEF VAR r-tt-movto         AS ROWID.
DEF VAR c-work             AS CHAR.

DEF VAR c-cod-estabel      AS CHAR.
DEF VAR c-dt-limite        AS CHAR FORMAT "99/9999".
DEF VAR c-it-codigo-ini    LIKE ped-item.it-codigo         INIT ''.
DEF VAR c-it-codigo-fin    LIKE ped-item.it-codigo         INIT "ZZZZZZZZZZZZZZZ".
DEF VAR c-cod-refer-ini    LIKE ped-item.cod-refer.
DEF VAR c-cod-refer-fin    LIKE ped-item.cod-refer         INIT "ZZZZZZZZZZ".
DEF VAR c-cod-qualid-ini   LIKE ob-etiqueta.cod-qualid.
DEF VAR c-cod-qualid-fin   LIKE ob-etiqueta.cod-qualid     INIT "Z".
DEF VAR c-cod-obsoleto-ini AS CHAR FORMAT "x".
DEF VAR c-cod-obsoleto-fin AS CHAR FORMAT "x"         INIT "Z".
DEF VAR c-corte-comerc-ini LIKE ob-etiqueta.corte-comerc   INIT "A".
DEF VAR c-corte-comerc-fin LIKE ob-etiqueta.corte-comerc   INIT "Z".
DEF VAR c-emitente-ini     LIKE ped-item.nome-abrev.
DEF VAR c-emitente-fin     LIKE ped-item.nome-abrev        INIT "ZZZZZZZZZZZZ".
DEF VAR c-cod-depos        LIKE saldo-estoq.cod-depos      INIT "ARM".
DEF VAR l-lote-todos       AS LOG INIT NO.
DEF VAR l-lote-pp          AS LOG INIT YES.
DEF VAR l-lote-pd          AS LOG INIT YES.
DEF VAR l-lote-rp          AS LOG INIT YES.
DEF VAR l-lote-rd          AS LOG INIT YES.
DEF VAR l-lote-sc          AS LOG INIT NO.
DEF VAR l-lote-ca          AS LOG INIT NO.
DEF VAR l-dep-corte        AS LOG INIT NO.
DEF VAR c-tp-artigo        AS CHAR INIT 'A'.
DEF VAR i-opc-acabado      AS INT INIT 3.
DEF VAR l-itens-relac      AS LOG INIT NO.
DEF VAR i-situacao         AS INT.
DEF VAR l-batch            AS LOG INIT NO.
DEF VAR l-ok               AS LOG.
DEF VAR l-recalc-res       AS LOG.
DEF VAR c-obsoleto         AS CHAR.
DEF VAR da-dt-entrega      AS DATE FORMAT "99/99/9999".
DEF VAR c-dia              AS CHAR.
DEF VAR arq-saida          AS CHAR FORMAT "x(45)".
DEF VAR c-lotes            AS CHAR FORMAT "x(18)".
DEF VAR de-qtd-prog        AS DEC.
DEF VAR de-qtd-proc        AS DEC.
DEF VAR de-qtd-pron        AS DEC.

/* Variaveis para o Excel */
DEF VAR chExcelApp  AS COM-HANDLE NO-UNDO.
DEF VAR chWorkbook  AS COM-HANDLE NO-UNDO.
DEF VAR chworksheet AS COM-HANDLE NO-UNDO.
DEFINE VAR i-Lin       AS INTEGER.
DEFINE VAR c-lin       AS CHARACTER FORMAT "x(500)".

DEFINE TEMP-TABLE tt-fotos
       FIELD arq-image AS CHAR.

DEF VAR c-arq-image AS CHAR.
DEF VAR c-comando AS CHAR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-livre
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad
&Scoped-define BROWSE-NAME br-itens

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-itens item tt-movto

/* Definitions for BROWSE br-itens                                      */
&Scoped-define FIELDS-IN-QUERY-br-itens tt-itens.it-codigo item.desc-item ITEM.un tt-itens.cod-refer tt-itens.lote   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-itens   
&Scoped-define SELF-NAME br-itens
&Scoped-define QUERY-STRING-br-itens FOR EACH tt-itens WHERE                                  tt-itens.visualiza = YES NO-LOCK, ~
                                   FIRST item WHERE                                   item.it-codigo = tt-itens.it-codigo NO-LOCK                                   INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-itens OPEN QUERY {&SELF-NAME} FOR EACH tt-itens WHERE                                  tt-itens.visualiza = YES NO-LOCK, ~
                                   FIRST item WHERE                                   item.it-codigo = tt-itens.it-codigo NO-LOCK                                   INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-itens tt-itens item
&Scoped-define FIRST-TABLE-IN-QUERY-br-itens tt-itens
&Scoped-define SECOND-TABLE-IN-QUERY-br-itens item


/* Definitions for BROWSE br-movto                                      */
&Scoped-define FIELDS-IN-QUERY-br-movto tt-movto.lote tt-movto.qt-estoque tt-movto.qt-res-antc tt-movto.qt-ped-reserva tt-movto.qt-trf fn-benefic() @ tt-movto.qt-benefic fn-carteira() @ tt-movto.qt-carteira tt-movto.qt-res-cart tt-movto.qt-saldo   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-movto   
&Scoped-define SELF-NAME br-movto
&Scoped-define OPEN-QUERY-br-movto RUN pi-totais. OPEN QUERY {&SELF-NAME} FOR EACH tt-movto WHERE                                  tt-movto.row-tt-itens = ROWID(tt-itens) AND                                  tt-movto.visualiza = YES NO-LOCK                                   INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-movto tt-movto
&Scoped-define FIRST-TABLE-IN-QUERY-br-movto tt-movto


/* Definitions for FRAME f-cad                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-cad ~
    ~{&OPEN-QUERY-br-itens}~
    ~{&OPEN-QUERY-br-movto}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-3 RECT-4 rt-button bt-par rs-tp-calculo ~
br-itens bt-det-estoq br-movto bt-trf bt-exit 
&Scoped-Define DISPLAYED-OBJECTS rs-tp-calculo fi-estoque-sel ~
fi-disponivel-sel fi-carteira-sel fi-negativo-sel fi-tot-estoque ~
fi-tot-res-antc fi-tot-reserva fi-tot-trf fi-tot-benefic fi-tot-carteira ~
fi-tot-res-cart fi-tot-saldo 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-4 bt-pedido-res 
&Scoped-define List-6 bt-det-estoq bt-det-disponivel bt-det-carteira ~
bt-det-negativo bt-excel 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-benefic w-livre 
FUNCTION fn-benefic RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-carteira w-livre 
FUNCTION fn-carteira RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-obsoleto w-livre 
FUNCTION fn-obsoleto RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-livre AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU mi-programa 
       MENU-ITEM mi-consultas   LABEL "Co&nsultas"     ACCELERATOR "CTRL-L"
       MENU-ITEM mi-imprimir    LABEL "&Relat¢rios"    ACCELERATOR "CTRL-P"
       RULE
       MENU-ITEM mi-sair        LABEL "&Sair"          ACCELERATOR "CTRL-X".

DEFINE SUB-MENU m_Ajuda 
       MENU-ITEM mi-conteudo    LABEL "&Conteudo"     
       MENU-ITEM mi-sobre       LABEL "&Sobre..."     .

DEFINE MENU m-livre MENUBAR
       SUB-MENU  mi-programa    LABEL "&Nome-do-Programa"
       SUB-MENU  m_Ajuda        LABEL "&Ajuda"        .


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_p-exihel AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-carteira 
     IMAGE-UP FILE "image/im-local.bmp":U
     LABEL "" 
     SIZE 4.86 BY 1.13 TOOLTIP "Carteira".

DEFINE BUTTON bt-desenho 
     IMAGE-UP FILE "image/imt-show-des.bmp":U
     LABEL "Des" 
     SIZE 4.86 BY 6.63 TOOLTIP "Visualiza Imagem do Desenho".

DEFINE BUTTON bt-det-carteira 
     IMAGE-UP FILE "image/im-open.bmp":U
     LABEL "Button 3" 
     SIZE 3.57 BY 1.

DEFINE BUTTON bt-det-disponivel 
     IMAGE-UP FILE "image/im-open.bmp":U
     LABEL "Button 2" 
     SIZE 3.57 BY 1.

DEFINE BUTTON bt-det-estoq 
     IMAGE-UP FILE "image/im-open.bmp":U
     LABEL "Button 1" 
     SIZE 3.57 BY 1.

DEFINE BUTTON bt-det-negativo 
     IMAGE-UP FILE "image/im-open.bmp":U
     LABEL "Button 4" 
     SIZE 3.57 BY 1.

DEFINE BUTTON bt-estoq 
     IMAGE-UP FILE "image/im-chest.bmp":U
     LABEL "" 
     SIZE 4.86 BY 1.13 TOOLTIP "Etiquetas em Estoque".

DEFINE BUTTON bt-estoq-res 
     IMAGE-UP FILE "image/im-bt-grupo-maq.bmp":U
     LABEL "" 
     SIZE 4.86 BY 1.13 TOOLTIP "Beneficiamento - Programado, Processo, Pronto".

DEFINE BUTTON bt-excel 
     IMAGE-UP FILE "image/excel.bmp":U
     LABEL "Button 2" 
     SIZE 7.14 BY 1.13.

DEFINE BUTTON bt-exit AUTO-END-KEY 
     IMAGE-UP FILE "image/im-exi.bmp":U
     LABEL "" 
     SIZE 7.14 BY 1.13 TOOLTIP "Fechar Programa"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-par 
     IMAGE-UP FILE "image/im-fil.bmp":U
     LABEL "Button 3" 
     SIZE 4.29 BY 1.21 TOOLTIP "ParÉmetros".

DEFINE BUTTON bt-pedido-res 
     IMAGE-UP FILE "image/imt-res.bmp":U
     LABEL "" 
     SIZE 4.86 BY 1.29 TOOLTIP "Reserva de Pedidos"
     BGCOLOR 8 .

DEFINE BUTTON bt-res-cart 
     IMAGE-UP FILE "image/im-crit.bmp":U
     LABEL "" 
     SIZE 4.86 BY 1.13 TOOLTIP "Carteira Resrevada".

DEFINE BUTTON bt-trf 
     IMAGE-UP FILE "image/im-trf.bmp":U
     LABEL "" 
     SIZE 4.86 BY 1.13 TOOLTIP "Composiá∆o da Transformaá∆o".

DEFINE VARIABLE fi-carteira-sel AS DECIMAL FORMAT ">>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi-disponivel-sel AS DECIMAL FORMAT ">>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi-estoque-sel AS DECIMAL FORMAT ">>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi-negativo-sel AS DECIMAL FORMAT "->>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1
     FGCOLOR 12 FONT 6 NO-UNDO.

DEFINE VARIABLE fi-tot-benefic AS DECIMAL FORMAT "ZZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-tot-carteira AS DECIMAL FORMAT "->>,>>>,>>9.99":R18 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10.57 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-tot-estoque AS DECIMAL FORMAT ">>>,>>9.99":R12 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9.14 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-tot-res-antc AS DECIMAL FORMAT ">>>,>>9.99":R16 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-tot-res-cart AS DECIMAL FORMAT "ZZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8.14 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-tot-reserva AS DECIMAL FORMAT "Z,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-tot-saldo AS DECIMAL FORMAT "->>>,>>9.99":R16 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10.72 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-tot-trf AS DECIMAL FORMAT "-ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE rs-tp-calculo AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Estoque Total", 1,
"Estoque Dispon°vel", 2,
"Carteira", 3,
"Negativo Ö Produzir", 4
     SIZE 73 BY 1
     FGCOLOR 9 FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 7 BY 8
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 27 BY 6.58
     BGCOLOR 8 .

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 99 BY 1.46
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-itens FOR 
      tt-itens, 
      item SCROLLING.

DEFINE QUERY br-movto FOR 
      tt-movto SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-itens
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-itens w-livre _FREEFORM
  QUERY br-itens NO-LOCK DISPLAY
      tt-itens.it-codigo FORMAT "X(9)":U 
      item.desc-item     FORMAT "x(55)"
      ITEM.un            FORMAT "x(5)" 
      tt-itens.cod-refer FORMAT "X(8)":U
      tt-itens.lote      FORMAT "X(5)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 66.57 BY 6.63
         FONT 1
         TITLE "Itens Selecionados".

DEFINE BROWSE br-movto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-movto w-livre _FREEFORM
  QUERY br-movto NO-LOCK DISPLAY
      tt-movto.lote  FORMAT "x(20)"
      tt-movto.qt-estoque 
      tt-movto.qt-res-antc
      tt-movto.qt-ped-reserva 
      tt-movto.qt-trf     
      fn-benefic() @ tt-movto.qt-benefic  COLUMN-LABEL "Benefic" 
      fn-carteira() @ tt-movto.qt-carteira COLUMN-LABEL "Carteira" 
      tt-movto.qt-res-cart 
      tt-movto.qt-saldo WIDTH 10
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 90.43 BY 9.08
         FONT 1
         TITLE "Movimentaá∆o".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     bt-par AT ROW 1.17 COL 77
     rs-tp-calculo AT ROW 1.25 COL 2 NO-LABEL
     br-itens AT ROW 2.63 COL 1.43
     bt-desenho AT ROW 2.63 COL 68.14
     fi-estoque-sel AT ROW 4.5 COL 80.57 COLON-ALIGNED NO-LABEL
     bt-det-estoq AT ROW 4.54 COL 95.14
     fi-disponivel-sel AT ROW 5.67 COL 80.57 COLON-ALIGNED NO-LABEL
     bt-det-disponivel AT ROW 5.71 COL 95.14
     fi-carteira-sel AT ROW 6.83 COL 80.57 COLON-ALIGNED NO-LABEL
     bt-det-carteira AT ROW 6.88 COL 95.14
     fi-negativo-sel AT ROW 8 COL 80.57 COLON-ALIGNED NO-LABEL
     bt-det-negativo AT ROW 8.04 COL 95.14
     br-movto AT ROW 9.42 COL 1.57
     bt-estoq AT ROW 9.67 COL 94.14
     bt-estoq-res AT ROW 10.92 COL 94.14
     bt-pedido-res AT ROW 12.21 COL 94.14
     bt-trf AT ROW 13.63 COL 94.14
     bt-carteira AT ROW 14.88 COL 94.14
     bt-res-cart AT ROW 16.08 COL 94.14
     bt-excel AT ROW 17.54 COL 93
     bt-exit AT ROW 18.75 COL 93 WIDGET-ID 2
     fi-tot-estoque AT ROW 18.79 COL 23.86 RIGHT-ALIGNED NO-LABEL
     fi-tot-res-antc AT ROW 18.79 COL 32.29 RIGHT-ALIGNED NO-LABEL
     fi-tot-reserva AT ROW 18.79 COL 32 COLON-ALIGNED NO-LABEL
     fi-tot-trf AT ROW 18.79 COL 40.43 COLON-ALIGNED NO-LABEL
     fi-tot-benefic AT ROW 18.79 COL 48.86 COLON-ALIGNED NO-LABEL
     fi-tot-carteira AT ROW 18.79 COL 58 COLON-ALIGNED NO-LABEL
     fi-tot-res-cart AT ROW 18.79 COL 68.86 COLON-ALIGNED NO-LABEL
     fi-tot-saldo AT ROW 18.79 COL 77.29 COLON-ALIGNED NO-LABEL
     "Totais:" VIEW-AS TEXT
          SIZE 8 BY .54 AT ROW 18.96 COL 7
          FONT 0
     "            Total Geral" VIEW-AS TEXT
          SIZE 25 BY .58 AT ROW 3 COL 74
          BGCOLOR 1 FGCOLOR 15 FONT 6
     "Carteira:" VIEW-AS TEXT
          SIZE 5.86 BY .54 AT ROW 7.08 COL 81.15 RIGHT-ALIGNED
          BGCOLOR 8 FGCOLOR 9 
     "Estoque:" VIEW-AS TEXT
          SIZE 6 BY .54 AT ROW 4.79 COL 81.14 RIGHT-ALIGNED
          BGCOLOR 8 FGCOLOR 9 
     "Negativo:" VIEW-AS TEXT
          SIZE 7 BY .54 AT ROW 8.25 COL 81.43 RIGHT-ALIGNED
          BGCOLOR 8 FGCOLOR 9 
     "Dispon°vel:" VIEW-AS TEXT
          SIZE 8 BY .54 AT ROW 5.92 COL 81.29 RIGHT-ALIGNED
          BGCOLOR 8 FGCOLOR 9 
     "    dos Itens Selecionados" VIEW-AS TEXT
          SIZE 25 BY .58 AT ROW 3.58 COL 74
          BGCOLOR 1 FGCOLOR 15 FONT 6
     RECT-3 AT ROW 9.42 COL 93
     RECT-4 AT ROW 2.67 COL 73
     rt-button AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 99.57 BY 18.96
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: w-livre
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-livre ASSIGN
         HIDDEN             = YES
         TITLE              = "An†lise Gerencial do Estoque"
         HEIGHT             = 19
         WIDTH              = 99.72
         MAX-HEIGHT         = 19.75
         MAX-WIDTH          = 106.57
         VIRTUAL-HEIGHT     = 19.75
         VIRTUAL-WIDTH      = 106.57
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU m-livre:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-livre 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-livre.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-livre
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-cad
   FRAME-NAME L-To-R                                                    */
/* BROWSE-TAB br-itens rs-tp-calculo f-cad */
/* BROWSE-TAB br-movto bt-det-negativo f-cad */
/* SETTINGS FOR BUTTON bt-carteira IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-desenho IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-det-carteira IN FRAME f-cad
   NO-ENABLE 6                                                          */
/* SETTINGS FOR BUTTON bt-det-disponivel IN FRAME f-cad
   NO-ENABLE 6                                                          */
/* SETTINGS FOR BUTTON bt-det-estoq IN FRAME f-cad
   6                                                                    */
/* SETTINGS FOR BUTTON bt-det-negativo IN FRAME f-cad
   NO-ENABLE 6                                                          */
/* SETTINGS FOR BUTTON bt-estoq IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-estoq-res IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-excel IN FRAME f-cad
   NO-ENABLE 6                                                          */
/* SETTINGS FOR BUTTON bt-pedido-res IN FRAME f-cad
   NO-ENABLE 4                                                          */
/* SETTINGS FOR BUTTON bt-res-cart IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-carteira-sel IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-disponivel-sel IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-estoque-sel IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-negativo-sel IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-benefic IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-carteira IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-estoque IN FRAME f-cad
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN fi-tot-res-antc IN FRAME f-cad
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN fi-tot-res-cart IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-reserva IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-saldo IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-trf IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR TEXT-LITERAL "Estoque:"
          SIZE 6 BY .54 AT ROW 4.79 COL 81.14 RIGHT-ALIGNED             */

/* SETTINGS FOR TEXT-LITERAL "Dispon°vel:"
          SIZE 8 BY .54 AT ROW 5.92 COL 81.29 RIGHT-ALIGNED             */

/* SETTINGS FOR TEXT-LITERAL "Carteira:"
          SIZE 5.86 BY .54 AT ROW 7.08 COL 81.15 RIGHT-ALIGNED          */

/* SETTINGS FOR TEXT-LITERAL "Negativo:"
          SIZE 7 BY .54 AT ROW 8.25 COL 81.43 RIGHT-ALIGNED             */

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-itens
/* Query rebuild information for BROWSE br-itens
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-itens WHERE
                                 tt-itens.visualiza = YES NO-LOCK,
                            FIRST item WHERE
                                  item.it-codigo = tt-itens.it-codigo NO-LOCK
                                  INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-itens */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-movto
/* Query rebuild information for BROWSE br-movto
     _START_FREEFORM
RUN pi-totais.
OPEN QUERY {&SELF-NAME} FOR EACH tt-movto WHERE
                                 tt-movto.row-tt-itens = ROWID(tt-itens) AND
                                 tt-movto.visualiza = YES NO-LOCK
                                  INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-movto */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON END-ERROR OF w-livre /* An†lise Gerencial do Estoque */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON WINDOW-CLOSE OF w-livre /* An†lise Gerencial do Estoque */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-itens
&Scoped-define SELF-NAME br-itens
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-itens w-livre
ON VALUE-CHANGED OF br-itens IN FRAME f-cad /* Itens Selecionados */
DO:
   IF AVAIL tt-itens THEN DO.
       ASSIGN c-arq-image = SESSION:TEMP-DIRECTORY + tt-itens.cod-refer + '.txt'.
              c-comando = 'DIR /b ' + param-dis.dir-img-item + '\*' + tt-itens.cod-refer + '* >' +
                          c-arq-image.
       OS-COMMAND SILENT VALUE(c-comando).
    
       EMPTY TEMP-TABLE tt-fotos.

       INPUT FROM VALUE(c-arq-image).
       REPEAT.
          CREATE tt-fotos.
          IMPORT tt-fotos.
       END.
       INPUT CLOSE.
    
       FOR EACH tt-fotos.
           IF tt-fotos.arq-image MATCHES '*' + tt-itens.cod-refer + '*' THEN DO.
              ASSIGN tt-fotos.arq-image = param-dis.dir-img-item + "\" + tt-fotos.arq-image.
              NEXT.
           END.
           ELSE
              DELETE tt-fotos.
       END.
    
       FIND FIRST tt-fotos NO-ERROR.
       ASSIGN bt-desenho:SENSITIVE = NO.
       IF AVAIL tt-fotos THEN
          ASSIGN bt-desenho:SENSITIVE = YES.

       OS-DELETE VALUE(c-arq-image) NO-ERROR.
   END.
   {&OPEN-QUERY-br-movto}
   APPLY 'VALUE-CHANGED' TO br-movto.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-movto
&Scoped-define SELF-NAME br-movto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-movto w-livre
ON ROW-DISPLAY OF br-movto IN FRAME f-cad /* Movimentaá∆o */
DO:
   ASSIGN tt-movto.qt-saldo:FGCOLOR IN BROWSE br-movto = ?.
   IF tt-movto.qt-saldo < 0 THEN
       ASSIGN tt-movto.qt-saldo:FGCOLOR IN BROWSE br-movto = 12.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-movto w-livre
ON VALUE-CHANGED OF br-movto IN FRAME f-cad /* Movimentaá∆o */
DO:
    IF AVAIL tt-movto THEN DO.
       ASSIGN bt-estoq:SENSITIVE IN FRAME {&FRAME-NAME} = NO
              bt-estoq-res:SENSITIVE IN FRAME {&FRAME-NAME} = NO
              bt-carteira:SENSITIVE IN FRAME {&FRAME-NAME} = NO
              bt-res-cart:SENSITIVE IN FRAME {&FRAME-NAME} = NO
              bt-pedido-res:SENSITIVE IN FRAME {&FRAME-NAME} = NO
              bt-trf:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
    
       IF tt-movto.qt-estoque > 0 OR
          tt-movto.qt-res-cart > 0 THEN 
          ASSIGN bt-estoq:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
       
       IF tt-itens.qt-benefic > 0 THEN 
          ASSIGN bt-estoq-res:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

       IF tt-movto.qt-carteira - tt-movto.qt-res-cart > 0 OR
          tt-movto.qt-res-antc > 0 OR
          tt-movto.qt-res-cart > 0 THEN
          ASSIGN bt-carteira:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

       IF tt-movto.qt-trf > 0 THEN 
          ASSIGN bt-trf:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

       IF tt-itens.lote <> 'SC' AND
          (INPUT FRAME {&FRAME-NAME} rs-tp-calculo = 2 OR
          tt-movto.qt-ped-reserva > 0) THEN 
          ASSIGN bt-pedido-res:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-carteira
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-carteira w-livre
ON CHOOSE OF bt-carteira IN FRAME f-cad
DO:
    FOR EACH tt-ped-item.
        ASSIGN tt-ped-item.visualiza = NO.
    END.

    FOR EACH tt-pedidos WHERE
             tt-pedidos.row-tt-movto = ROWID(tt-movto) NO-LOCK,
        EACH tt-ped-item WHERE
             tt-ped-item.row-tt-movto = ROWID(tt-movto) AND
             tt-ped-item.nr-pedcli = tt-pedidos.nr-pedcli AND
             tt-ped-item.nome-abrev = tt-pedidos.nome-abrev EXCLUSIVE-LOCK.

        ASSIGN tt-ped-item.visualiza = YES.
    END.

    RUN esp/essp0150f.p (INPUT TABLE tt-pedidos,
                         INPUT TABLE tt-ped-item). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-desenho
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-desenho w-livre
ON CHOOSE OF bt-desenho IN FRAME f-cad /* Des */
DO:
   RUN esdlg/d01-desenho.w (INPUT tt-itens.cod-refer).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-det-carteira
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-det-carteira w-livre
ON CHOOSE OF bt-det-carteira IN FRAME f-cad /* Button 3 */
DO:
    FOR EACH tt-pedidos.
        ASSIGN tt-pedidos.visualiza = YES.
    END.
    RUN esp/essp0150e.p (INPUT TABLE tt-pedidos).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-det-disponivel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-det-disponivel w-livre
ON CHOOSE OF bt-det-disponivel IN FRAME f-cad /* Button 2 */
DO:
    RUN esp/essp0150b.p (INPUT TABLE tt-detalhe).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-det-estoq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-det-estoq w-livre
ON CHOOSE OF bt-det-estoq IN FRAME f-cad /* Button 1 */
DO:
   RUN esp/essp0150b.p (INPUT TABLE tt-detalhe).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-det-negativo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-det-negativo w-livre
ON CHOOSE OF bt-det-negativo IN FRAME f-cad /* Button 4 */
DO:
    RUN esp/essp0150b.p (INPUT TABLE tt-detalhe).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-estoq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-estoq w-livre
ON CHOOSE OF bt-estoq IN FRAME f-cad
DO:
   RUN esp/essp0150d.p (INPUT TABLE tt-etiquetas,
                        INPUT ROWID(tt-movto)).
                        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-estoq-res
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-estoq-res w-livre
ON CHOOSE OF bt-estoq-res IN FRAME f-cad
DO:
   ASSIGN p-cod-estabel-182 = c-cod-estabel
          p-it-codigo-182 = tt-itens.it-codigo
          p-cod-refer-182 = tt-itens.cod-refer
          p-manut-182 = NO.

   RUN esp/essp0182.w "SHARED".

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-excel w-livre
ON CHOOSE OF bt-excel IN FRAME f-cad /* Button 2 */
DO:
    RUN esdlg/d01essp0150.w (INPUT rs-tp-calculo, OUTPUT arq-saida).

    IF arq-saida <> "" THEN DO:
       IF rs-tp-calculo = 1 THEN /* Estoque Total */
          RUN pi-gera-excel (INPUT "TOT", INPUT-OUTPUT arq-saida).

       IF rs-tp-calculo = 2 THEN /* Estoque Dispon°vel */
          RUN pi-gera-excel (INPUT "DSP", INPUT-OUTPUT arq-saida).

       IF rs-tp-calculo = 4 THEN /* Negativo a Produzir */
          RUN pi-gera-excel (INPUT "NEG", INPUT-OUTPUT arq-saida).

       IF arq-saida <> "" THEN
          MESSAGE "O arquivo foi gerado em " TRIM(arq-saida) + "." SKIP 
                  "Para acess†-lo,  abra-o atravÇs do Excel."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-exit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-exit w-livre
ON CHOOSE OF bt-exit IN FRAME f-cad
DO:
   APPLY 'CLOSE':U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-par
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-par w-livre
ON CHOOSE OF bt-par IN FRAME f-cad /* Button 3 */
DO:
  RUN esapi/busca-estab-usuar.p (INPUT c-seg-usuario,
                                 OUTPUT c-cod-estabel).
  IF c-cod-estabel = '' OR c-cod-estabel = '0' THEN
     ASSIGN c-cod-estabel = '1'.

  EMPTY TEMP-TABLE tt-digita.

  ASSIGN w-livre:SENSITIVE = NO.

  IF p-it-codigo-150 = "" THEN DO:
     RUN esp/essp0150a.w (INPUT-OUTPUT c-cod-estabel,
                          INPUT-OUTPUT TABLE tt-digita,
                          INPUT-OUTPUT c-dt-limite,                  
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
                          INPUT-OUTPUT c-emitente-ini,               
                          INPUT-OUTPUT c-emitente-fin,               
                          INPUT-OUTPUT c-cod-depos,                  
                          INPUT-OUTPUT l-lote-todos,                 
                          INPUT-OUTPUT l-lote-pp,                    
                          INPUT-OUTPUT l-lote-pd,                    
                          INPUT-OUTPUT l-lote-rp,                    
                          INPUT-OUTPUT l-lote-rd,
                          INPUT-OUTPUT l-lote-sc,
                          INPUT-OUTPUT l-lote-ca,
                          INPUT-OUTPUT c-tp-artigo,
                          INPUT-OUTPUT i-opc-acabado,
                          INPUT-OUTPUT l-itens-relac,
                          INPUT-OUTPUT l-dep-corte,
                          INPUT-OUTPUT l-ok).

  END.
  ELSE
     ASSIGN c-it-codigo-ini = p-it-codigo-150
            c-it-codigo-fin = p-it-codigo-150
            c-cod-refer-ini = p-cod-refer-150
            c-cod-refer-fin = p-cod-refer-150
            l-lote-rp       = p-lote-rp-150
            l-lote-todos    = NOT p-lote-rp-150
            l-ok            = YES.

  IF l-ok THEN DO.
     FOR EACH tt-itens.
         DELETE tt-itens.
     END.
     FOR EACH tt-movto.
         DELETE tt-movto.
     END.

     RUN pi-processa.
     APPLY 'value-changed' TO rs-tp-calculo.
  END.
  ASSIGN w-livre:SENSITIVE = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-pedido-res
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-pedido-res w-livre
ON CHOOSE OF bt-pedido-res IN FRAME f-cad
DO:
    ASSIGN w-livre:SENSITIVE = NO.

    FOR EACH tt-etiquetas.
        ASSIGN tt-etiquetas.visualiza = NO.
    END.

    FOR EACH tt-etiquetas WHERE
             tt-etiquetas.row-tt-movto = ROWID(tt-movto) EXCLUSIVE-LOCK.
        ASSIGN tt-etiquetas.visualiza = YES.
    END.

    FOR EACH tt-ped-reserva WHERE
             tt-ped-reserva.row-tt-movto = ROWID(tt-movto) EXCLUSIVE-LOCK.
        ASSIGN tt-ped-reserva.visualiza = YES.
    END.

    RUN esp/essp0150h.p (INPUT TABLE tt-etiquetas,
                         INPUT tt-movto.qt-saldo,
                         INPUT tt-itens.it-codigo,
                         INPUT tt-itens.cod-refer,
                         INPUT tt-itens.lote,
                         INPUT tt-movto.lote,
                         INPUT rs-tp-calculo:INPUT-VALUE,
                         INPUT c-cod-estabel,
                         OUTPUT l-recalc-res).

    ASSIGN w-livre:SENSITIVE = YES.

    IF l-recalc-res THEN DO.
       ASSIGN tt-movto.qt-saldo = tt-movto.qt-saldo + tt-movto.qt-ped-reserva
              tt-movto.qt-ped-reserva = 0.

       FOR EACH ped-reserva-it WHERE
                ped-reserva-it.it-codigo = tt-itens.it-codigo AND
                ped-reserva-it.cod-refer = tt-itens.cod-refer AND
                ped-reserva-it.nr-lot    = tt-itens.lote AND
                ped-reserva-it.corte-comerc = tt-movto.lote NO-LOCK.

           FIND ped-reserva OF ped-reserva-it NO-LOCK NO-ERROR.
           IF ped-reserva.situacao <> 1 THEN NEXT.

           FOR EACH ped-reserva-etq OF ped-reserva-it NO-LOCK.
               FIND ob-etiqueta WHERE
                    ob-etiqueta.cod-estabel  = ped-reserva.cod-estabel AND 
                    ob-etiqueta.num-etiqueta = ped-reserva-etq.num-etiqueta
                    NO-LOCK NO-ERROR.
               IF NOT AVAIL ob-etiqueta THEN NEXT.

               ASSIGN tt-movto.qt-ped-reserva = tt-movto.qt-ped-reserva + ob-etiqueta.quantidade
                      tt-movto.qt-saldo = tt-movto.qt-saldo - ob-etiqueta.quantidade.

               FIND tt-ped-reserva WHERE
                    tt-ped-reserva.row-tt-movto = ROWID(tt-movto) AND
                    tt-ped-reserva.num-reserva = ped-reserva-it.num-reserva 
                    NO-ERROR.
               IF NOT AVAIL tt-ped-reserva THEN DO.
                  CREATE tt-ped-reserva.
                  ASSIGN tt-ped-reserva.row-tt-movto = ROWID(tt-movto)
                         tt-ped-reserva.num-reserva = ped-reserva-it.num-reserva.
               END.
           END.
       END.

       IF rs-tp-calculo:INPUT-VALUE = 2 AND
          tt-movto.qt-saldo <= 0 THEN
          ASSIGN tt-movto.visualiza = NO.

       ASSIGN r-tt-movto = ROWID(tt-movto).
       APPLY 'VALUE-CHANGED' TO br-itens.

       FIND tt-movto WHERE
            ROWID(tt-movto) = r-tt-movto NO-ERROR.

       IF tt-movto.visualiza THEN DO.
          br-movto:QUERY:REPOSITION-TO-ROWID(r-tt-movto).
          APPLY 'VALUE-CHANGED' TO br-movto.
       END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-res-cart
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-res-cart w-livre
ON CHOOSE OF bt-res-cart IN FRAME f-cad
DO:
  FOR EACH tt-ped-item.
      ASSIGN tt-ped-item.visualiza = NO.
  END.

  FOR EACH tt-pedidos WHERE
           tt-pedidos.row-tt-movto = ROWID(tt-movto) NO-LOCK,
      EACH tt-ped-item WHERE
           tt-ped-item.row-tt-movto = ROWID(tt-movto) AND
           tt-ped-item.nr-pedcli = tt-pedidos.nr-pedcli AND
           tt-ped-item.nome-abrev = tt-pedidos.nome-abrev EXCLUSIVE-LOCK.

      IF tt-ped-item.qt-reservada > 0 THEN
         ASSIGN tt-ped-item.visualiza = YES.
      ELSE
         ASSIGN tt-ped-item.visualiza = NO.
  END.

  RUN esp/essp0150f.p (INPUT TABLE tt-pedidos,
                       INPUT TABLE tt-ped-item). 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-trf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-trf w-livre
ON CHOOSE OF bt-trf IN FRAME f-cad
DO:
   FOR EACH tt-trf.
       ASSIGN tt-trf.visualiza = NO.
   END.

   FOR EACH tt-trf WHERE
            tt-trf.row-tt-movto = ROWID(tt-movto) EXCLUSIVE-LOCK.
       ASSIGN tt-trf.visualiza = YES.
   END.

   RUN esp/essp0150c.p (INPUT TABLE tt-trf).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-consultas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-consultas w-livre
ON CHOOSE OF MENU-ITEM mi-consultas /* Consultas */
DO:
  RUN pi-consulta IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-conteudo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-conteudo w-livre
ON CHOOSE OF MENU-ITEM mi-conteudo /* Conteudo */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  RUN pi-ajuda IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-imprimir w-livre
ON CHOOSE OF MENU-ITEM mi-imprimir /* Relat¢rios */
DO:
  RUN pi-imprimir IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-programa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-programa w-livre
ON MENU-DROP OF MENU mi-programa /* Nome-do-Programa */
DO:
  run pi-disable-menu.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sair
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sair w-livre
ON CHOOSE OF MENU-ITEM mi-sair /* Sair */
DO:
  RUN pi-sair IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sobre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sobre w-livre
ON CHOOSE OF MENU-ITEM mi-sobre /* Sobre... */
DO:
  {include/sobre.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-tp-calculo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-tp-calculo w-livre
ON VALUE-CHANGED OF rs-tp-calculo IN FRAME f-cad
DO:
    ASSIGN INPUT FRAME {&FRAME-NAME} rs-tp-calculo.

    ASSIGN fi-estoque-sel = 0
           fi-carteira-sel = 0
           fi-disponivel-sel = 0
           fi-negativo-sel = 0.

    DISP fi-estoque-sel 
         fi-carteira-sel
         fi-disponivel-sel
         fi-negativo-sel
         WITH FRAME {&FRAME-NAME}.

    DISABLE {&list-6} WITH FRAME {&FRAME-NAME}.

    FOR EACH tt-detalhe.
        DELETE tt-detalhe.
    END.

    CASE rs-tp-calculo.
        WHEN 1 THEN DO.
            FOR EACH tt-itens.
                ASSIGN tt-itens.visualiza = NO.
                FOR EACH tt-movto WHERE
                         tt-movto.row-tt-itens = ROWID(tt-itens).
                    ASSIGN tt-movto.visualiza = YES
                           tt-itens.visualiza = YES
                           fi-estoque-sel = fi-estoque-sel + tt-movto.qt-estoque.

                    FIND tt-detalhe WHERE
                         tt-detalhe.corte-comerc = tt-movto.lote
                         NO-LOCK NO-ERROR.
                    IF NOT AVAIL tt-detalhe THEN DO.
                       CREATE tt-detalhe.
                       ASSIGN tt-detalhe.corte-comerc = tt-movto.lote.
                    END.
                    ASSIGN tt-detalhe.qt-estoque = tt-detalhe.qt-estoque + tt-movto.qt-estoque.
                END.
            END.
            IF fi-estoque-sel > 0 THEN DO.
               DISP fi-estoque-sel 
                    fi-carteira-sel
                    fi-disponivel-sel
                    fi-negativo-sel
                    WITH FRAME {&FRAME-NAME}.
               ASSIGN bt-det-estoq:SENSITIVE IN FRAME {&FRAME-NAME} = YES
                      bt-excel:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
            END.
        END.

        WHEN 2 THEN DO.
            FOR EACH tt-itens.
                ASSIGN tt-itens.visualiza = NO.
                FOR EACH tt-movto WHERE
                         tt-movto.row-tt-itens = ROWID(tt-itens).
                    ASSIGN tt-movto.visualiza = NO.
                    IF tt-movto.qt-saldo > 0 THEN DO.
                       ASSIGN tt-movto.visualiza = YES
                              tt-itens.visualiza = YES
                              fi-disponivel-sel = fi-disponivel-sel + tt-movto.qt-saldo.
                       FIND tt-detalhe WHERE
                            tt-detalhe.corte-comerc = tt-movto.lote
                            NO-LOCK NO-ERROR.
                       IF NOT AVAIL tt-detalhe THEN DO.
                          CREATE tt-detalhe.
                          ASSIGN tt-detalhe.corte-comerc = tt-movto.lote.
                       END.
                       ASSIGN tt-detalhe.qt-estoque = tt-detalhe.qt-estoque + tt-movto.qt-saldo.
                    END.
                END.
            END.
            IF fi-disponivel-sel > 0 THEN DO.
               DISP fi-estoque-sel 
                    fi-carteira-sel
                    fi-disponivel-sel
                    fi-negativo-sel
                    WITH FRAME {&FRAME-NAME}.
               ASSIGN bt-det-disponivel:SENSITIVE IN FRAME {&FRAME-NAME} = YES
                      bt-excel:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
            END.
        END.

        WHEN 3 THEN DO.
            FOR EACH tt-itens.
                ASSIGN tt-itens.visualiza = NO.
                FOR EACH tt-movto WHERE
                         tt-movto.row-tt-itens = ROWID(tt-itens).
                    ASSIGN tt-movto.visualiza = NO.
                    IF tt-movto.qt-carteira > 0 OR
                       tt-movto.qt-res-cart > 0 THEN
                       ASSIGN tt-movto.visualiza = YES
                              tt-itens.visualiza = YES
                              fi-carteira-sel = fi-carteira-sel + tt-movto.qt-carteira.
                END.
            END.
            IF fi-carteira-sel > 0  THEN DO.
               DISP fi-estoque-sel 
                    fi-carteira-sel
                    fi-disponivel-sel
                    fi-negativo-sel
                    WITH FRAME {&FRAME-NAME}.
               ASSIGN bt-det-carteira:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
            END.
        END.

        WHEN 4 THEN DO.
            FOR EACH tt-itens.
                ASSIGN tt-itens.visualiza = NO.
                FOR EACH tt-movto WHERE
                         tt-movto.row-tt-itens = ROWID(tt-itens).
                    ASSIGN tt-movto.visualiza = NO.
                    IF tt-movto.qt-saldo < 0 THEN DO.
                       ASSIGN tt-movto.visualiza = YES
                              tt-itens.visualiza = YES
                              fi-negativo-sel = fi-negativo-sel + tt-movto.qt-saldo.

                       FIND tt-detalhe WHERE
                            tt-detalhe.corte-comerc = tt-movto.lote
                            NO-LOCK NO-ERROR.
                       IF NOT AVAIL tt-detalhe THEN DO.
                          CREATE tt-detalhe.
                          ASSIGN tt-detalhe.corte-comerc = tt-movto.lote.
                       END.
                       ASSIGN tt-detalhe.qt-estoque = tt-detalhe.qt-estoque + tt-movto.qt-saldo.
                    END.
                END.
            END.
            IF fi-negativo-sel < 0 THEN DO.
               DISP fi-estoque-sel 
                    fi-carteira-sel
                    fi-disponivel-sel
                    fi-negativo-sel
                    WITH FRAME {&FRAME-NAME}.
               ASSIGN bt-det-negativo:SENSITIVE IN FRAME {&FRAME-NAME} = YES
                      bt-excel:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
            END.
        END.
    END CASE.

    {&OPEN-QUERY-br-itens}

    APPLY 'value-changed' TO br-itens IN FRAME {&FRAME-NAME}.
    APPLY 'ENTRY' TO br-itens IN FRAME {&FRAME-NAME}.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-itens
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-livre 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-livre  _ADM-CREATE-OBJECTS
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
             INPUT  'panel/p-exihel.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-exihel ).
       RUN set-position IN h_p-exihel ( 1.13 , 83.14 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             bt-par:HANDLE IN FRAME f-cad , 'BEFORE':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-livre  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-livre  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
  THEN DELETE WIDGET w-livre.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-livre  _DEFAULT-ENABLE
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
  DISPLAY rs-tp-calculo fi-estoque-sel fi-disponivel-sel fi-carteira-sel 
          fi-negativo-sel fi-tot-estoque fi-tot-res-antc fi-tot-reserva 
          fi-tot-trf fi-tot-benefic fi-tot-carteira fi-tot-res-cart fi-tot-saldo 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE RECT-3 RECT-4 rt-button bt-par rs-tp-calculo br-itens bt-det-estoq 
         br-movto bt-trf bt-exit 
      WITH FRAME f-cad IN WINDOW w-livre.
  {&OPEN-BROWSERS-IN-QUERY-f-cad}
  VIEW w-livre.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-livre 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-livre 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-livre 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  run pi-before-initialize.

  {include/win-size.i}

  {utp/ut9000.i "ESSP0150" "2.04.00.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  run pi-after-initialize.

  FIND FIRST param-dis NO-LOCK NO-ERROR.

  ASSIGN c-dt-limite = '129999' /* STRING(MONTH(TODAY),'99') + STRING(YEAR(TODAY),'9999') */.
  APPLY 'choose' TO bt-par IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-abre-excel w-livre 
PROCEDURE pi-abre-excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 CREATE "Excel.Application" chExcelApp NO-ERROR.
 IF chExcelApp <> ? THEN /* Cria a Planilha */
    ASSIGN chExcelApp:VISIBLE     = TRUE  /* A Planilha Ficar† Visivel */
           chWorkbook             = chExcelApp:Workbooks:ADD() /* Cria Planilha */
           chworksheet            = chExcelapp:sheets:ITEM(1).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-benefic w-livre 
PROCEDURE pi-benefic :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /* Processa Programaá∆o*/
    /*
    FOR EACH ref-item-ext WHERE
             ref-item-ext.it-codigo >= c-it-codigo-ini AND
             ref-item-ext.it-codigo <= c-it-codigo-fin AND
             ref-item-ext.cod-refer >= c-cod-refer-ini AND 
             ref-item-ext.cod-refer <= c-cod-refer-fin NO-LOCK.

        RUN pi-ver-digita (INPUT "Item",
                           INPUT ref-item-ext.it-codigo).
        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

        RUN pi-ver-digita (INPUT "Referància",
                           INPUT ref-item-ext.cod-refer).
        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

        FIND FIRST tt-itens WHERE
                   tt-itens.it-codigo = ref-item-ext.it-codigo AND
                   tt-itens.cod-refer = ref-item-ext.cod-refer NO-ERROR.

        IF NOT AVAIL tt-itens THEN DO.
           CREATE tt-itens.
           ASSIGN tt-itens.it-codigo = ref-item-ext.it-codigo 
                  tt-itens.cod-refer = ref-item-ext.cod-refer
                  tt-itens.lote = 'RP'.
        END.

        FOR EACH tt-itens WHERE
                 tt-itens.it-codigo = ref-item-ext.it-codigo AND
                 tt-itens.cod-refer = ref-item-ext.cod-refer NO-LOCK.

            IF tt-itens.lote <> 'PP' AND
               tt-itens.lote <> 'RP' THEN NEXT.

            ASSIGN tt-itens.qt-benefic = tt-itens.qt-benefic + ref-item-ext.qtd-prog +
                                         ref-item-ext.qtd-proc + ref-item-ext.qtd-pron.
        END.
    END.
    */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-carteira w-livre 
PROCEDURE pi-carteira :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER p-situacao AS INT.

    FOR EACH ped-venda WHERE
             ped-venda.cod-sit-ped = p-situacao NO-LOCK,
        EACH ped-item OF ped-venda WHERE        
             ped-item.cod-sit-item = 1 AND
             ped-item.it-codigo >= c-it-codigo-ini AND
             ped-item.it-codigo <= c-it-codigo-fin AND
             ped-item.cod-refer >= c-cod-refer-ini AND 
             ped-item.cod-refer <= c-cod-refer-fin NO-LOCK,
       FIRST ped-item-ext OF ped-item 
             WHERE LOOKUP(SUBSTR(ped-item-ext.lote,1,2),c-lotes) <> 0  
             NO-LOCK.
                        
        IF NOT l-batch THEN
           RUN pi-acompanhar IN h-acomp (INPUT "Pedido: " + ped-item.nr-pedcli +
                                           "  Item: " + ped-item.it-codigo +
                                           "   Ref: " + ped-item.cod-refer).

        RUN pi-ver-digita (INPUT "Item",
                           INPUT ped-item.it-codigo).
        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

        RUN pi-ver-digita (INPUT "Referància",
                           INPUT ped-item.cod-refer).
        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

        IF ped-venda.cod-estabel <> c-cod-estabel THEN NEXT.

        IF ped-venda.dt-entrega > da-dt-entrega THEN NEXT.

        IF ped-item.nome-abrev < c-emitente-ini OR
           ped-item.nome-abrev > c-emitente-fin THEN NEXT.

        IF i-opc-acabado = 1 AND SUBSTR(ped-item.cod-refer,7,1) <> '0' THEN NEXT.
        IF i-opc-acabado = 2 AND SUBSTR(ped-item.cod-refer,7,1) = '0' THEN NEXT.

        RUN pi-ver-digita (INPUT "Cliente",
                           INPUT ped-item.nome-abrev).
        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

        FIND item WHERE
             item.it-codigo = ped-item.it-codigo NO-LOCK NO-ERROR.

        FIND item-ext WHERE
             item-ext.it-codigo = item.it-codigo NO-LOCK NO-ERROR.

        FIND tt-itens WHERE
             tt-itens.it-codigo = ped-item.it-codigo AND
             tt-itens.cod-refer = ped-item.cod-refer 
             NO-LOCK NO-ERROR.

        IF NOT AVAIL tt-itens THEN DO.
           CREATE tt-itens.
           ASSIGN tt-itens.it-codigo = ped-item.it-codigo 
                  tt-itens.cod-refer = ped-item.cod-refer.
        END.
        ASSIGN tt-itens.qt-pedida = tt-itens.qt-pedida + ped-item.qt-pedida.

        
        FIND tt-movto WHERE
             tt-movto.row-tt-itens = ROWID(tt-itens) AND
             tt-movto.lote = SUBSTR(ped-item-ext.lote,1,2)
             NO-LOCK NO-ERROR.


        IF NOT AVAIL tt-movto THEN DO.
           CREATE tt-movto.
           ASSIGN tt-movto.row-tt-itens = ROWID(tt-itens) 
                  tt-movto.lote = SUBSTR(ped-item-ext.lote,1,2).
        END.
        
        IF ped-venda.cod-sit-ped = 4 THEN
           ASSIGN tt-movto.qt-res-antc = tt-movto.qt-res-antc + ped-item.qt-pedida.
        ELSE
           ASSIGN tt-movto.qt-carteira = tt-movto.qt-carteira + ped-item.qt-pedida.

        ASSIGN tt-movto.qt-saldo = tt-movto.qt-saldo - ped-item.qt-pedida.

        FIND tt-pedidos WHERE
             tt-pedidos.row-tt-movto = ROWID(tt-movto) AND
             tt-pedidos.nr-pedcli = ped-item.nr-pedcli
             NO-ERROR.
        IF NOT AVAIL tt-pedidos THEN DO.
           CREATE tt-pedidos.
           ASSIGN tt-pedidos.row-tt-movto = ROWID(tt-movto)
                  tt-pedidos.nr-pedcli = ped-item.nr-pedcli
                  tt-pedidos.nome-abrev = ped-item.nome-abrev
                  tt-pedidos.dt-entrega = ped-venda.dt-entrega.
        END.
        ASSIGN tt-pedidos.qt-pedida = tt-pedidos.qt-pedida + ped-item.qt-pedida.

        FIND tt-ped-item WHERE
             tt-ped-item.row-tt-movto = ROWID(tt-movto)   AND
             tt-ped-item.nr-pedcli = tt-pedidos.nr-pedcli AND
             tt-ped-item.nome-abrev = tt-pedidos.nome-abrev AND
             tt-ped-item.nr-sequencia = ped-item.nr-sequencia
             NO-LOCK NO-ERROR.

        IF NOT AVAIL tt-ped-item THEN DO.
           CREATE tt-ped-item.
           ASSIGN tt-ped-item.row-tt-movto = ROWID(tt-movto)
                  tt-ped-item.nr-pedcli = tt-pedidos.nr-pedcli 
                  tt-ped-item.nome-abrev = tt-pedidos.nome-abrev 
                  tt-ped-item.nr-sequencia = ped-item.nr-sequencia.
        END.
        ASSIGN tt-ped-item.qt-pedida = tt-ped-item.qt-pedida + ped-item.qt-pedida.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-etiquetas w-livre 
PROCEDURE pi-etiquetas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER p-situacao AS INT.

    FOR EACH ob-etiqueta WHERE
             ob-etiqueta.situacao = p-situacao AND
             ob-etiqueta.it-codigo  >= c-it-codigo-ini AND
             ob-etiqueta.it-codigo  <= c-it-codigo-fin AND
             ob-etiqueta.cod-refer  >= c-cod-refer-ini AND
             ob-etiqueta.cod-refer  <= c-cod-refer-fin AND
             LOOKUP(ob-etiqueta.nr-lote,c-lotes) <> 0  NO-LOCK:

        IF ob-etiqueta.cod-estabel <> c-cod-estabel THEN NEXT.
 
        RUN pi-ver-digita (INPUT "Item",
                           INPUT ob-etiqueta.it-codigo).
        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

        RUN pi-ver-digita (INPUT "Referància",
                           INPUT ob-etiqueta.cod-refer).
        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

        FIND item WHERE
             item.it-codigo = ob-etiqueta.it-codigo NO-LOCK NO-ERROR.

        FIND item-ext WHERE
             item-ext.it-codigo = item.it-codigo NO-LOCK NO-ERROR.

        IF NOT l-batch THEN
           RUN pi-acompanhar IN h-acomp (INPUT "Etiqueta: " + STRING(ob-etiqueta.num-etiqueta) + 
                                               "    Item: " + ob-etiqueta.it-codigo +
                                               "     Ref: " + ob-etiqueta.cod-refer).

        FIND tt-itens WHERE
             tt-itens.it-codigo = ob-etiqueta.it-codigo AND
             tt-itens.cod-refer = ob-etiqueta.cod-refer
             NO-LOCK NO-ERROR.

        IF NOT AVAIL tt-itens THEN DO.
           CREATE tt-itens.
           ASSIGN tt-itens.it-codigo = ob-etiqueta.it-codigo 
                  tt-itens.cod-refer = ob-etiqueta.cod-refer.
        END.
        ASSIGN tt-itens.qt-estoque = tt-itens.qt-estoque + ob-etiqueta.quantidade.

        FIND tt-movto WHERE
             tt-movto.row-tt-itens = ROWID(tt-itens) AND
             tt-movto.lote = ob-etiqueta.nr-lote 
             NO-LOCK NO-ERROR.

        IF NOT AVAIL tt-movto THEN DO.
           CREATE tt-movto.
           ASSIGN tt-movto.row-tt-itens = ROWID(tt-itens)
                  tt-movto.lote = ob-etiqueta.nr-lote.
        END.
        ASSIGN tt-movto.qt-estoque = tt-movto.qt-estoque + ob-etiqueta.quantidade
               tt-movto.qt-saldo = tt-movto.qt-saldo + ob-etiqueta.quantidade.
               
        IF ob-etiqueta.situacao = 4 THEN DO.

           FIND ped-item-rom WHERE
                ped-item-rom.num-etiqueta = ob-etiqueta.num-etiqueta AND 
                ped-item-rom.cod-estabel  = ob-etiqueta.cod-estabel 
                NO-LOCK NO-ERROR.
           IF AVAIL ped-item-rom THEN DO.
              FIND ped-venda WHERE
                   ped-venda.nr-pedcli = ped-item-rom.nr-pedcli AND
                   ped-venda.nome-abrev = ped-item-rom.nome-abrev
                   NO-LOCK NO-ERROR.

              FIND ped-item WHERE
                   ped-item.nr-pedcli = ped-item-rom.nr-pedcli AND
                   ped-item.nome-abrev = ped-item-rom.nome-abrev AND
                   ped-item.nr-sequencia = ped-item-rom.nr-sequencia
                   NO-LOCK NO-ERROR.
              IF NOT AVAIL ped-item THEN NEXT.

              ASSIGN tt-movto.qt-res-cart = tt-movto.qt-res-cart + ob-etiqueta.quantidade.

              FIND tt-pedidos WHERE
                   tt-pedidos.row-tt-movto = ROWID(tt-movto) AND
                   tt-pedidos.nr-pedcli = ped-item-rom.nr-pedcli
                   NO-ERROR.
              IF NOT AVAIL tt-pedidos THEN DO.
                 CREATE tt-pedidos.
                 ASSIGN tt-pedidos.row-tt-movto = ROWID(tt-movto)
                        tt-pedidos.nr-pedcli = ped-item-rom.nr-pedcli
                        tt-pedidos.nome-abrev = ped-item-rom.nome-abrev
                        tt-pedidos.dt-entrega = ped-venda.dt-entrega.
              END.
              ASSIGN tt-pedidos.qt-reservada = tt-pedidos.qt-reservada + ob-etiqueta.quantidade.

              FIND tt-ped-item WHERE
                   tt-ped-item.row-tt-movto = ROWID(tt-movto) AND
                   tt-ped-item.nr-pedcli = tt-pedidos.nr-pedcli AND
                   tt-ped-item.nome-abrev = tt-pedidos.nome-abrev AND
                   tt-ped-item.nr-sequencia = ped-item.nr-sequencia
                   NO-LOCK NO-ERROR.

              IF NOT AVAIL tt-ped-item THEN DO.
                 CREATE tt-ped-item.
                 ASSIGN tt-ped-item.row-tt-movto = ROWID(tt-movto)
                        tt-ped-item.nr-pedcli = tt-pedidos.nr-pedcli 
                        tt-ped-item.nome-abrev = tt-pedidos.nome-abrev 
                        tt-ped-item.nr-sequencia = ped-item.nr-sequencia.
              END.
              ASSIGN tt-ped-item.qt-reservada = tt-ped-item.qt-reservada + ob-etiqueta.quantidade.
           END.
        END.

        FIND tt-etiquetas WHERE
             tt-etiquetas.row-tt-movto = ROWID(tt-movto) AND
             tt-etiquetas.cod-estabel = ob-etiqueta.cod-estabel AND
             tt-etiquetas.num-etiqueta = ob-etiqueta.num-etiqueta
             NO-ERROR.
        IF NOT AVAIL tt-etiquetas THEN DO.
           CREATE tt-etiquetas.
           ASSIGN tt-etiquetas.row-tt-movto = ROWID(tt-movto)
                  tt-etiquetas.cod-estabel = ob-etiqueta.cod-estabel 
                  tt-etiquetas.num-etiqueta = ob-etiqueta.num-etiqueta.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-excel-disp w-livre 
PROCEDURE pi-excel-disp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN chworksheet:range("A1"):VALUE = " ÷TENS DISPON÷VEIS PARA VENDA - DATA: " + STRING(TODAY,"99/99/9999").
   
    /* Configura Alinhamento Horizontal do Titulo da Planilha */
    ChWorkSheet:range("A1:J1"):SELECT().
    ChWorksheet:range("A1:J1"):Merge.
    Chworksheet:Range("A1:J1"):HorizontalAlignment =  3.

    /* Colorir Titulo da Planilha */
    chWorkSheet:Range("A1:J1"):FONT:ColorIndex     = 19.
    chWorkSheet:Range("A1:J1"):Interior:ColorIndex = 18.

    /* Configura a Linha do Titulo da Planilha */
    ASSIGN chWorkSheet:Rows("1:1"):RowHeight = 25
           chWorkSheet:Rows("2:2"):RowHeight =  4
           chWorkSheet:Rows("1:1"):FONT:SIZE = 19
           chWorkSheet:Rows("1:1"):FONT:bold = TRUE.

   /* Cabeáalho das Colunas */
    ASSIGN chworksheet:range("A3"):VALUE = "ITEM"
           chworksheet:range("B3"):VALUE = "DESCRIC«O"
           chworksheet:range("C3"):VALUE = "UN"   
           chworksheet:range("D3"):VALUE = "REFER“NCIA"
           chworksheet:range("E3"):VALUE = "QUANTIDADE".

    /* Configura as Colunas da Planilha */
    ASSIGN chworksheet:range("A:D"):NumberFormat        = "@".
    ASSIGN chworksheet:range("E:E"):NumberFormat        = "###.###.##0,00"
           Chworksheet:range("E:E"):HorizontalAlignment = 4.

    /* Tamanho das Colunas */
    ASSIGN chWorkSheet:Columns("A"):ColumnWidth =  7
           chWorkSheet:Columns("B"):ColumnWidth = 35
           chWorkSheet:Columns("C"):ColumnWidth =  4
           chWorkSheet:Columns("D"):ColumnWidth = 15
           chWorkSheet:Columns("E"):ColumnWidth = 15.

    /* Configura Cabeáalho das Colunas */
    chWorkSheet:Range("A3:J3"):SELECT().
    ASSIGN chExcelApp:SELECTION:FONT:NAME               = "Consolas"
           chExcelApp:SELECTION:FONT:SIZE               = 14
           chExcelApp:SELECTION:FONT:Bold               = TRUE 
           chExcelApp:SELECTION:Interior:ColorIndex     = 23
           chExcelApp:SELECTION:FONT:ColorIndex         = 20.

    ASSIGN i-Lin = 4.
    
    FOR EACH tt-itens WHERE tt-itens.visualiza = YES NO-LOCK,
        EACH tt-movto WHERE tt-movto.row-tt-itens = ROWID(tt-itens) 
                        AND tt-movto.visualiza    = YES NO-LOCK
        BREAK BY tt-itens.it-codigo
              BY tt-itens.cod-refer
              BY tt-itens.lote
              BY tt-movto.lote:
        
        IF FIRST-OF(tt-itens.it-codigo) THEN DO:
           FIND ITEM WHERE ITEM.it-codigo = tt-itens.it-codigo NO-LOCK NO-ERROR.
           ASSIGN chworksheet:range("A" + STRING(i-lin)):VALUE = tt-itens.it-codigo
                  chworksheet:range("B" + STRING(i-lin)):VALUE = item.desc-item
                  chworksheet:range("C" + STRING(i-lin)):VALUE = item.un.
        END.
        
        ACCUMULATE tt-movto.qt-saldo (TOTAL BY tt-itens.it-codigo).
        ACCUMULATE tt-movto.qt-saldo (TOTAL BY tt-itens.cod-refer).
        ACCUMULATE tt-movto.qt-saldo (TOTAL).
       
        IF LAST-OF(tt-itens.cod-refer) THEN DO.
           ASSIGN chworksheet:range("D" + STRING(i-lin)):VALUE = STRING(tt-itens.cod-refer).
           ASSIGN chworksheet:range("E" + STRING(i-lin)):VALUE = (ACCUM TOTAL BY tt-itens.cod-refer tt-movto.qt-saldo).

           ASSIGN i-Lin = i-Lin + 1.
        END.

        /* Colorir a Linha */
        /*
        ASSIGN c-work = "A" + STRING(i-lin) + ":" + "E" + STRING(i-lin).
        chWorkSheet:Range(c-work):SELECT().
        ASSIGN chExcelApp:SELECTION:Interior:ColorIndex     = 35
               chExcelApp:SELECTION:FONT:ColorIndex         = 51.
        */
        //  ASSIGN i-Lin = i-Lin + 1.
    END.
    
    IF (ACCUM TOTAL tt-movto.qt-saldo) <> 0 THEN DO:
       ASSIGN chworksheet:range("B" + STRING(i-lin)):VALUE = "TOTAL GERAL".
       ASSIGN chworksheet:range("E" + STRING(i-lin)):VALUE = (ACCUM TOTAL tt-movto.qt-saldo).

       /* Colorir a Linha / Negrito */
       
       chWorkSheet:Range("A" + STRING(i-lin) + ":E" + STRING(i-lin)):Interior:ColorIndex = 14.
       chWorkSheet:Range("A" + STRING(i-lin) + ":E" + STRING(i-lin)):FONT:ColorIndex     =  2.
       chWorkSheet:Range("A" + STRING(i-lin) + ":E" + STRING(i-lin)):FONT:Bold           = TRUE.
       
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-excel-neg w-livre 
PROCEDURE pi-excel-neg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN chworksheet:range("A1"):VALUE = " ÷TENS NEGATIVOS A PRODUZIR - DATA: " + STRING(TODAY,"99/99/9999").
   
    /* Configura Alinhamento Horizontal do Titulo da Planilha */
    ChWorkSheet:range("A1:K1"):SELECT().
    ChWorksheet:range("A1:K1"):Merge.
    Chworksheet:Range("A1:K1"):HorizontalAlignment =  3.

    /* Colorir Titulo da Planilha */
    chWorkSheet:Range("A1:K1"):FONT:ColorIndex     = 19.
    chWorkSheet:Range("A1:K1"):Interior:ColorIndex = 18.

    /* Configura a Linha do Titulo da Planilha */
    ASSIGN chWorkSheet:Rows("1:1"):RowHeight = 25
           chWorkSheet:Rows("2:2"):RowHeight =  4
           chWorkSheet:Rows("1:1"):FONT:SIZE = 19
           chWorkSheet:Rows("1:1"):FONT:bold = TRUE.

   /* Cabeáalho das Colunas */
    ASSIGN chworksheet:range("A3"):VALUE = "ITEM"
           chworksheet:range("B3"):VALUE = "DESCRIC«O"
           chworksheet:range("C3"):VALUE = "UN"   
           chworksheet:range("D3"):VALUE = "REFER“NCIA"
           chworksheet:range("E3"):VALUE = "LOTE"
           chworksheet:range("F3"):VALUE = "COR/DESENHO"
           chworksheet:range("G3"):VALUE = "QUANTIDADE"
           chworksheet:range("H3"):VALUE = "ACUM.REFER"
           chworksheet:range("I3"):VALUE = "PROGRAMADO"
           chworksheet:range("J3"):VALUE = "PROCESSO"
           chworksheet:range("K3"):VALUE = "PRONTO".

    /* Configura Formato das Colunas */
    ASSIGN chworksheet:range("A:F"):NumberFormat        = "@".
    ASSIGN chworksheet:range("G:L"):NumberFormat        = "###.###.##0,00"
           Chworksheet:range("G:L"):HorizontalAlignment = 4.

    /* Tamanho das Colunas */
    ASSIGN chWorkSheet:Columns("A"):ColumnWidth =  7
           chWorkSheet:Columns("B"):ColumnWidth = 35
           chWorkSheet:Columns("C"):ColumnWidth =  4
           chWorkSheet:Columns("D"):ColumnWidth = 15
           chWorkSheet:Columns("E"):ColumnWidth =  6
           chWorkSheet:Columns("F"):ColumnWidth = 18
           chWorkSheet:Columns("G"):ColumnWidth = 15
           chWorkSheet:Columns("H"):ColumnWidth = 15
           chWorkSheet:Columns("I"):ColumnWidth = 14
           chWorkSheet:Columns("J"):ColumnWidth = 14
           chWorkSheet:Columns("K"):ColumnWidth = 14.

    /* Configura Cabeáalho das Colunas */
    chWorkSheet:Range("A3:K3"):SELECT().
    ASSIGN chExcelApp:SELECTION:FONT:NAME               = "Consolas"
           chExcelApp:SELECTION:FONT:SIZE               = 14
           chExcelApp:SELECTION:FONT:Bold               = TRUE 
           chExcelApp:SELECTION:Interior:ColorIndex     = 23
           chExcelApp:SELECTION:FONT:ColorIndex         = 20.

    ASSIGN i-Lin = 4.
    
    FOR EACH tt-itens WHERE
             tt-itens.visualiza = YES NO-LOCK,
        EACH tt-movto WHERE 
             tt-movto.row-tt-itens = ROWID(tt-itens) AND
             tt-movto.visualiza    = YES NO-LOCK
        BREAK BY tt-itens.it-codigo
              BY tt-itens.cod-refer
              BY tt-itens.lote:
        
        IF FIRST-OF(tt-itens.it-codigo) THEN DO:
           FIND ITEM WHERE ITEM.it-codigo = tt-itens.it-codigo NO-LOCK NO-ERROR.
           
           ASSIGN chworksheet:range("A" + STRING(i-lin)):VALUE = tt-itens.it-codigo
                  chworksheet:range("B" + STRING(i-lin)):VALUE = item.desc-item
                  chworksheet:range("C" + STRING(i-lin)):VALUE = item.un.
        END.
        
        IF FIRST-OF(tt-itens.cod-refer) THEN
           ASSIGN chworksheet:range("D" + STRING(i-lin)):VALUE = STRING(tt-itens.cod-refer, "99.9999.9").
        
        IF FIRST-OF(tt-itens.lote) THEN DO:
           /*
           FIND ref-item-ext WHERE
                ref-item-ext.it-codigo = tt-itens.it-codigo AND
                ref-item-ext.cod-refer = tt-itens.cod-refer NO-LOCK NO-ERROR.
           IF AVAIL ref-item-ext THEN
              ASSIGN chworksheet:range("E" + STRING(i-lin)):VALUE = tt-itens.lote.
           IF AVAIL ref-item-ext THEN
              ASSIGN chworksheet:range("F" + STRING(i-lin)):VALUE = ref-item-ext.cor.
            */  
        END.
    
        ACCUMULATE tt-movto.qt-saldo (TOTAL BY tt-itens.it-codigo).
        ACCUMULATE tt-movto.qt-saldo (TOTAL BY tt-itens.cod-refer).
        ACCUMULATE tt-movto.qt-saldo (TOTAL BY tt-itens.lote).
        ACCUMULATE tt-movto.qt-saldo (TOTAL).
    
        IF LAST-OF(tt-itens.lote) THEN
           ASSIGN chworksheet:range("G" + STRING(i-lin)):VALUE = (ACCUM TOTAL BY tt-itens.lote tt-movto.qt-saldo).

        IF LAST-OF(tt-itens.cod-refer) THEN DO:
           ASSIGN de-qtd-prog = 0
                  de-qtd-proc = 0
                  de-qtd-pron = 0.
           /*
           FOR EACH ob-pcp WHERE
                    ob-pcp.situacao = 1 AND
                    ob-pcp.it-codigo = tt-itens.it-codigo NO-LOCK,
               EACH ob-pcp-ref OF ob-pcp WHERE
                    ob-pcp-ref.cod-refer = tt-itens.cod-refer NO-LOCK.

               ASSIGN de-qtd-prog = de-qtd-prog + ob-pcp-ref.qtd-sld-prog
                      de-qtd-proc = de-qtd-proc + ob-pcp-ref.qtd-proc
                      de-qtd-pron = de-qtd-pron + ob-pcp-ref.qtd-pron.
           END.
           */
           ASSIGN chworksheet:range("H" + STRING(i-lin)):VALUE = (ACCUM TOTAL BY tt-itens.cod-refer tt-movto.qt-saldo).
           ASSIGN chworksheet:range("I" + STRING(i-lin)):VALUE = de-qtd-prog.
           ASSIGN chworksheet:range("J" + STRING(i-lin)):VALUE = de-qtd-proc.
           ASSIGN chworksheet:range("K" + STRING(i-lin)):VALUE = de-qtd-pron.

           /* Colorir a Linha */
           ASSIGN c-work = "A" + STRING(i-lin) + ":" + "K" + STRING(i-lin).
           chWorkSheet:Range(c-work):SELECT().
           ASSIGN chExcelApp:SELECTION:Interior:ColorIndex     = 35
                  chExcelApp:SELECTION:FONT:ColorIndex         = 51.
           ASSIGN i-Lin = i-Lin + 1.
        END.

        IF LAST-OF(tt-itens.it-codigo) THEN DO:
           ASSIGN chworksheet:range("B" + STRING(i-lin)):VALUE = "TOTAL DO ITEM".
           ASSIGN chworksheet:range("G" + STRING(i-lin)):VALUE = (ACCUM TOTAL BY tt-itens.it-codigo tt-movto.qt-saldo).
           /* Colorir Linha / Negrito */
           chWorkSheet:Range("A" + STRING(i-lin) + ":K" + STRING(i-lin)):Interior:ColorIndex = 22.
           chWorkSheet:Range("A" + STRING(i-lin) + ":K" + STRING(i-lin)):FONT:ColorIndex     = 20.
           chWorkSheet:Range("A" + STRING(i-lin) + ":K" + STRING(i-lin)):FONT:Italic         = TRUE.
           ASSIGN i-Lin = i-Lin + 1.
        END.
    END.
    
    IF (ACCUM TOTAL tt-movto.qt-saldo) <> 0 THEN DO:
        ASSIGN chworksheet:range("B" + STRING(i-lin)):VALUE = "TOTAL GERAL".
        ASSIGN chworksheet:range("G" + STRING(i-lin)):VALUE = (ACCUM TOTAL tt-movto.qt-saldo).
         /* Colorir Linha / Negrito */
        chWorkSheet:Range("A" + STRING(i-lin) + ":K" + STRING(i-lin)):Interior:ColorIndex = 14.
        chWorkSheet:Range("A" + STRING(i-lin) + ":K" + STRING(i-lin)):FONT:ColorIndex     =  2.
        chWorkSheet:Range("A" + STRING(i-lin) + ":K" + STRING(i-lin)):FONT:Bold           = TRUE.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-excel-tot w-livre 
PROCEDURE pi-excel-tot :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN chworksheet:range("A1"):VALUE = " ESTOQUE TOTAL - DATA: " + STRING(TODAY,"99/99/9999").
   
    /* Configura Alinhamento Horizontal do Titulo da Planilha */
    ChWorkSheet:range("A1:I1"):SELECT().
    ChWorksheet:range("A1:I1"):Merge.
    Chworksheet:Range("A1:I1"):HorizontalAlignment =  3.

    /* Colorir Titulo da Planilha */
    chWorkSheet:Range("A1:I1"):FONT:ColorIndex     = 19.
    chWorkSheet:Range("A1:I1"):Interior:ColorIndex = 18.

    /* Configura a Linha do Titulo da Planilha */
    ASSIGN chWorkSheet:Rows("1:1"):RowHeight = 25
           chWorkSheet:Rows("2:2"):RowHeight =  4
           chWorkSheet:Rows("1:1"):FONT:SIZE = 19
           chWorkSheet:Rows("1:1"):FONT:bold = TRUE.

    /* Cabeáalho das Colunas */
    ASSIGN chworksheet:range("A3"):VALUE = "ITEM"
           chworksheet:range("B3"):VALUE = "DESCRIC«O"
           chworksheet:range("C3"):VALUE = "UN"   
           chworksheet:range("D3"):VALUE = "REFER“NCIA"
           chworksheet:range("E3"):VALUE = "LOTE"
           chworksheet:range("F3"):VALUE = "COR/DESENHO"
           chworksheet:range("G3"):VALUE = "CORTE COML"
           chworksheet:range("H3"):VALUE = "QUANTIDADE"
           chworksheet:range("I3"):VALUE = "ACUM.REFER".

    /* Configura Formato das Colunas */
    ASSIGN chworksheet:range("A:G"):NumberFormat        = "@".
    ASSIGN chworksheet:range("H:I"):NumberFormat        = "###.###.##0,00"
           Chworksheet:range("H:I"):HorizontalAlignment = 4.

    /* Tamanho das Colunas */
    ASSIGN chWorkSheet:Columns("A"):ColumnWidth =  7
           chWorkSheet:Columns("B"):ColumnWidth = 35
           chWorkSheet:Columns("C"):ColumnWidth =  4
           chWorkSheet:Columns("D"):ColumnWidth = 15
           chWorkSheet:Columns("E"):ColumnWidth =  6
           chWorkSheet:Columns("F"):ColumnWidth = 18
           chWorkSheet:Columns("G"):ColumnWidth = 14
           chWorkSheet:Columns("H"):ColumnWidth = 15
           chWorkSheet:Columns("I"):ColumnWidth = 15.

    /* Configura Cabeáalho das Colunas */
    chWorkSheet:Range("A3:I3"):SELECT().
    ASSIGN chExcelApp:SELECTION:FONT:NAME               = "Consolas"
           chExcelApp:SELECTION:FONT:SIZE               = 14
           chExcelApp:SELECTION:FONT:Bold               = TRUE 
           chExcelApp:SELECTION:Interior:ColorIndex     = 23
           chExcelApp:SELECTION:FONT:ColorIndex         = 20.

    ASSIGN i-Lin = 4.
    
    FOR EACH tt-itens WHERE tt-itens.visualiza = YES NO-LOCK,
        EACH tt-movto WHERE tt-movto.row-tt-itens = ROWID(tt-itens) 
                        AND tt-movto.visualiza    = YES NO-LOCK
        BREAK BY tt-itens.it-codigo
              BY tt-itens.cod-refer
              BY tt-itens.lote
              BY tt-movto.lote:
        
        IF FIRST-OF(tt-itens.it-codigo) THEN DO:
           FIND ITEM WHERE ITEM.it-codigo = tt-itens.it-codigo NO-LOCK NO-ERROR.

           ASSIGN chworksheet:range("A" + STRING(i-lin)):VALUE = tt-itens.it-codigo
                  chworksheet:range("B" + STRING(i-lin)):VALUE = item.desc-item
                  chworksheet:range("C" + STRING(i-lin)):VALUE = item.un.
        END.
        
        IF FIRST-OF(tt-itens.cod-refer) THEN
           ASSIGN chworksheet:range("D" + STRING(i-lin)):VALUE = STRING(tt-itens.cod-refer, "99.9999.9").
        
        IF FIRST-OF(tt-itens.lote) THEN DO:
           /*
           FIND ref-item-ext WHERE
                ref-item-ext.it-codigo = tt-itens.it-codigo AND
                ref-item-ext.cod-refer = tt-itens.cod-refer NO-LOCK NO-ERROR.
           IF AVAIL ref-item-ext THEN
              ASSIGN chworksheet:range("E" + STRING(i-lin)):VALUE = tt-itens.lote.
           IF AVAIL ref-item-ext THEN
             ASSIGN chworksheet:range("F" + STRING(i-lin)):VALUE = ref-item-ext.cor.
           */
        END.
    
        FIND corte-comerc WHERE 
            corte-comerc.codigo = tt-movto.lote NO-LOCK NO-ERROR.
        ASSIGN c-lin = IF AVAIL corte-comerc THEN corte-comerc.descricao
                                             ELSE "".

        ASSIGN chworksheet:range("G" + STRING(i-lin)):VALUE = c-lin.
        ASSIGN chworksheet:range("H" + STRING(i-lin)):VALUE = tt-movto.qt-estoque.

        ACCUMULATE tt-movto.qt-estoque (TOTAL BY tt-itens.it-codigo).
        ACCUMULATE tt-movto.qt-estoque (TOTAL BY tt-itens.cod-refer).
        ACCUMULATE tt-movto.qt-estoque (TOTAL).
    
        IF LAST-OF(tt-itens.cod-refer) THEN
           ASSIGN chworksheet:range("I" + STRING(i-lin)):VALUE = (ACCUM TOTAL BY tt-itens.cod-refer tt-movto.qt-estoque).
        
        /* Colorir a Linha */
        ASSIGN c-work = "A" + STRING(i-lin) + ":" + "I" + STRING(i-lin).
        chWorkSheet:Range(c-work):SELECT().
        ASSIGN chExcelApp:SELECTION:Interior:ColorIndex     = 35
               chExcelApp:SELECTION:FONT:ColorIndex         = 51.


        ASSIGN i-Lin = i-Lin + 1.
    
        IF LAST-OF(tt-itens.it-codigo) THEN DO:
           ASSIGN chworksheet:range("B" + STRING(i-lin)):VALUE = "TOTAL DO ITEM".
           ASSIGN chworksheet:range("H" + STRING(i-lin)):VALUE = (ACCUM TOTAL BY tt-itens.it-codigo tt-movto.qt-estoque).
           /* Colorir Linha / Negrito */
           chWorkSheet:Range("A" + STRING(i-lin) + ":I" + STRING(i-lin)):Interior:ColorIndex = 22.
           chWorkSheet:Range("A" + STRING(i-lin) + ":I" + STRING(i-lin)):FONT:ColorIndex     = 20.
           chWorkSheet:Range("A" + STRING(i-lin) + ":I" + STRING(i-lin)):FONT:Italic         = TRUE.
           ASSIGN i-Lin = i-Lin + 1.
        END.
    END.
    IF (ACCUM TOTAL tt-movto.qt-estoque) <> 0 THEN DO:
       ASSIGN chworksheet:range("B" + STRING(i-lin)):VALUE = "TOTAL GERAL".
       ASSIGN chworksheet:range("H" + STRING(i-lin)):VALUE = (ACCUM TOTAL tt-movto.qt-estoque).
        /* Colorir Linha / Negrito */
       chWorkSheet:Range("A" + STRING(i-lin) + ":I" + STRING(i-lin)):Interior:ColorIndex = 14.
       chWorkSheet:Range("A" + STRING(i-lin) + ":I" + STRING(i-lin)):FONT:ColorIndex     =  2.
       chWorkSheet:Range("A" + STRING(i-lin) + ":I" + STRING(i-lin)):FONT:Bold           = TRUE.
    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-gera-excel w-livre 
PROCEDURE pi-gera-excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER p-selecao          AS CHAR FORMAT "x(3)".
    DEF INPUT-OUTPUT PARAMETER p-arq-saida AS CHAR FORMAT "x(45)".
    
    
    RUN pi-abre-excel.
    IF chExcelApp = ? THEN DO:
       MESSAGE "O Aplicativo EXCEL n∆o foi encontrado. Favor instalar."
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
       ASSIGN p-arq-saida = "".
       RETURN.
    END.
    
    CASE p-selecao:
        WHEN "DSP" THEN RUN pi-excel-disp.
        WHEN "TOT" THEN RUN pi-excel-tot.
        WHEN "NEG" THEN RUN pi-excel-neg.
    END CASE.

    /* Posiciona o Foco no Inicio da Planilha */
    chExcelApp:Range("A1"):SELECT.
    chExcelApp:Range("A:A"):EntireColumn:AutoFit.

    /* Salva e Fecha Planilha */
    OS-DELETE VALUE(p-arq-saida).

    /*chWorkBook:SaveAs(p-arq-saida,,,,,,,).*/
    IF chExcelApp:Version BEGINS "8":U THEN 
       chWorkBook:SaveAs(p-arq-saida,39,,,,,,,TRUE). /* Salva em formato mais Antigo EXCEL97 */ 
    ELSE 
       chWorkBook:SaveAs(p-arq-saida,,,,,,,).  /* Salva na Vers∆o da planilha da ESTAÄ«O */

    chWorkBook:CLOSE().
    chExcelApp:QUIT().
    RELEASE OBJECT chExcelApp. 
    RELEASE OBJECT chworkBook.
    RELEASE OBJECT chworksheet.
    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-pcp w-livre 
PROCEDURE pi-pcp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /* Processa Programaá∆o*/
    /*
    FOR EACH ob-pcp WHERE
             ob-pcp.situacao = 1 AND
             ob-pcp.it-codigo >= c-it-codigo-ini AND
             ob-pcp.it-codigo <= c-it-codigo-fin AND 
             ob-pcp.cod-estabel = c-cod-estabel NO-LOCK,
        EACH ob-pcp-ref OF ob-pcp WHERE
             ob-pcp-ref.cod-refer >= c-cod-refer-ini AND
             ob-pcp-ref.cod-refer <= c-cod-refer-fin NO-LOCK.

        IF i-opc-acabado = 1 AND SUBSTR(ob-pcp-ref.cod-refer,7,1) <> '0' THEN NEXT.
        IF i-opc-acabado = 2 AND SUBSTR(ob-pcp-ref.cod-refer,7,1) = '0' THEN NEXT.

        RUN pi-ver-digita (INPUT "Item",
                           INPUT ob-pcp.it-codigo).
        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

        RUN pi-ver-digita (INPUT "Referància",
                           INPUT ob-pcp-ref.cod-refer).
        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

        FIND FIRST tt-itens WHERE
                   tt-itens.it-codigo = ob-pcp.it-codigo AND
                   tt-itens.cod-refer = ob-pcp-ref.cod-refer NO-ERROR.

        IF NOT AVAIL tt-itens THEN DO.
           CREATE tt-itens.
           ASSIGN tt-itens.it-codigo = ob-pcp.it-codigo 
                  tt-itens.cod-refer = ob-pcp-ref.cod-refer
                  tt-itens.lote = 'RP'.
        END.

        FOR EACH tt-itens WHERE
                 tt-itens.it-codigo = ob-pcp.it-codigo AND
                 tt-itens.cod-refer = ob-pcp-ref.cod-refer NO-LOCK.

            IF tt-itens.lote <> 'PP' AND
               tt-itens.lote <> 'RP' THEN NEXT.

            ASSIGN tt-itens.qt-benefic = tt-itens.qt-benefic + ob-pcp-ref.qtd-sld-prog +
                                         ob-pcp-ref.qtd-proc + ob-pcp-ref.qtd-pron.
        END.
    END.
    */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-popula-browse w-livre 
PROCEDURE pi-popula-browse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH tt-pedidos.
        DELETE tt-pedidos.
    END.

    FOR EACH tt-ped-item.
        DELETE tt-ped-item.
    END.

    FOR EACH tt-movto.
        DELETE tt-movto.
    END.

    FOR EACH tt-itens.
        DELETE tt-itens.
    END.

    FOR EACH tt-etiquetas.
        DELETE tt-etiquetas.
    END.

    RUN esapi/ret-udm.p (INPUT c-dt-limite, OUTPUT c-dia).
    ASSIGN da-dt-entrega = DATE(c-dia + SUBSTR(c-dt-limite,1,2) + SUBSTR(c-dt-limite,3,4)).

    ASSIGN c-lotes = "".
    IF l-lote-todos = YES THEN
       ASSIGN c-lotes = "rp,rd,pp,pd,sc,ca,".
    ELSE DO:
       ASSIGN c-lotes = c-lotes + IF l-lote-pp = YES THEN "pp," ELSE ",".
       ASSIGN c-lotes = c-lotes + IF l-lote-pd = YES THEN "pd," ELSE ",".
       ASSIGN c-lotes = c-lotes + IF l-lote-rp = YES THEN "rp," ELSE ",".
       ASSIGN c-lotes = c-lotes + IF l-lote-rd = YES THEN "rd," ELSE ",".
       ASSIGN c-lotes = c-lotes + IF l-lote-sc = YES THEN "sc," ELSE ",".
       ASSIGN c-lotes = c-lotes + IF l-lote-ca = YES THEN "ca," ELSE ",".
    END.

    IF NOT l-batch THEN DO.
       {utp/ut-liter.i Calculando_Estoque *}
       RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).
    END.

    RUN pi-etiquetas (INPUT 3).             
    RUN pi-etiquetas (INPUT 4).             

    IF NOT l-batch THEN DO.
       {utp/ut-liter.i Calculando_Carteira *}
       RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).
    END.
    
    RUN pi-carteira (INPUT 1).
    RUN pi-carteira (INPUT 2).
    RUN pi-carteira (INPUT 4).
    RUN pi-carteira (INPUT 5).

    IF NOT l-batch THEN DO.
       {utp/ut-liter.i Calculando_Reservas *}
       RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).
    END.
    RUN pi-reserva.

    IF NOT l-batch THEN DO.
       {utp/ut-liter.i Calculando_Transformaá‰es *}
       RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).
    END.
    RUN pi-trf.

    IF NOT l-batch THEN DO.
       {utp/ut-liter.i Calculando_Programaá‰es *}
       RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).
    END.
    RUN pi-pcp.
    
    IF l-itens-relac THEN DO.
       FOR EACH tt-itens BREAK BY tt-itens.it-codigo.
           IF FIRST-OF(tt-itens.it-codigo) THEN DO.
              FOR EACH estrutura WHERE
                       estrutura.it-codigo = tt-itens.it-codigo NO-LOCK.
                  RUN pi-ver-estrutura (INPUT estrutura.es-codigo).
              END.

              FOR EACH estrutura WHERE
                       estrutura.es-codigo = tt-itens.it-codigo NO-LOCK.
                  RUN pi-ver-estrutura (INPUT estrutura.it-codigo).

                  FOR EACH b-estrutura WHERE
                           b-estrutura.it-codigo = estrutura.it-codigo AND 
                           b-estrutura.es-codigo <> tt-itens.it-codigo NO-LOCK.
                      RUN pi-ver-estrutura (INPUT b-estrutura.es-codigo).
                  END.
              END.
           END.
       END.
    END.

    FOR EACH tt-movto.
        FIND corte-comerc WHERE
             corte-comerc.codigo = tt-movto.lote 
             NO-LOCK NO-ERROR.

        IF AVAIL corte-comerc AND
           ABS(tt-movto.qt-saldo) < corte-comerc.compr-min THEN
           ASSIGN tt-movto.qt-saldo = 0.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-processa w-livre 
PROCEDURE pi-processa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
     {utp/ut-liter.i Calculando_Estoque *}
     RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).
    
     RUN pi-popula-browse.
    
     RUN pi-finalizar in h-acomp.
     {&OPEN-QUERY-br-itens}
     APPLY 'value-changed' TO br-itens IN FRAME {&FRAME-NAME}.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-reserva w-livre 
PROCEDURE pi-reserva :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /* Processa Pedidos de Reserva */
    FOR EACH tt-ped-reserva.
        DELETE tt-ped-reserva.
    END.
    
    FOR EACH ped-reserva WHERE
             ped-reserva.cod-estab = c-cod-estabel AND
             ped-reserva.situacao = 1 NO-LOCK,
        EACH ped-reserva-it OF ped-reserva WHERE
             ped-reserva-it.it-codigo >= c-it-codigo-ini AND
             ped-reserva-it.it-codigo <= c-it-codigo-fin AND
             ped-reserva-it.cod-refer >= c-cod-refer-ini AND
             ped-reserva-it.cod-refer <= c-cod-refer-fin AND
             LOOKUP(ped-reserva-it.nr-lote,c-lotes) <> 0 NO-LOCK.

        /*
        IF i-opc-acabado = 1 AND SUBSTR(ped-reserva-it.cod-refer,7,1) <> '0' THEN NEXT.
        IF i-opc-acabado = 2 AND SUBSTR(ped-reserva-it.cod-refer,7,1) = '0' THEN NEXT.
        */

        RUN pi-ver-digita (INPUT "Item",
                           INPUT ped-reserva-it.it-codigo).
        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

        RUN pi-ver-digita (INPUT "Referància",
                           INPUT ped-reserva-it.cod-refer).
        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

        /*
        IF ped-reserva-it.corte-comerc < c-corte-comerc-ini OR  
           ped-reserva-it.corte-comerc > c-corte-comerc-fin THEN NEXT.

        RUN pi-ver-digita (INPUT "Corte_Comercial",
                           INPUT ped-reserva-it.corte-comerc).
        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.
        */
        
        FIND item WHERE
             item.it-codigo = ped-reserva-it.it-codigo NO-LOCK NO-ERROR.
        /*IF item.deposito-pad <> c-cod-depos THEN NEXT.*/
    
        FIND item-ext WHERE
             item-ext.it-codigo = item.it-codigo NO-LOCK NO-ERROR.
    
       /* IF c-tp-artigo <> 'A' AND AVAIL item-ext THEN
           IF (item-ext.indigo = YES AND c-tp-artigo <> "I") OR
              (item-ext.indigo = NO  AND c-tp-artigo <> "O") THEN NEXT. */
        /*
        FIND ref-item-ext WHERE
             ref-item-ext.it-codigo = ped-reserva-it.it-codigo AND
             ref-item-ext.cod-refer = ped-reserva-it.cod-refer 
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

        FIND tt-itens WHERE
             tt-itens.it-codigo = ped-reserva-it.it-codigo AND
             tt-itens.cod-refer = ped-reserva-it.cod-refer AND
             tt-itens.lote      = ped-reserva-it.nr-lote NO-ERROR.

        IF NOT AVAIL tt-itens THEN DO.
           CREATE tt-itens.
           ASSIGN tt-itens.it-codigo = ped-reserva-it.it-codigo 
                  tt-itens.cod-refer = ped-reserva-it.cod-refer
                  tt-itens.lote      = ped-reserva-it.nr-lote.
        END.

        FOR EACH ped-reserva-etq OF ped-reserva-it NO-LOCK.
            FIND ob-etiqueta WHERE
                 ob-etiqueta.cod-estabel  = ped-reserva.cod-estabel AND
                 ob-etiqueta.num-etiqueta = ped-reserva-etq.num-etiqueta
                 NO-LOCK NO-ERROR.

            IF NOT AVAIL ob-etiqueta THEN NEXT.

            FIND tt-movto WHERE
                 tt-movto.row-tt-itens = ROWID(tt-itens) AND
                 tt-movto.lote = ob-etiqueta.nr-lote
                 NO-LOCK NO-ERROR.

            IF NOT AVAIL tt-movto THEN DO.
               CREATE tt-movto.
               ASSIGN tt-movto.row-tt-itens = ROWID(tt-itens)
                      tt-movto.lote = ob-etiqueta.nr-lote.
            END.
            ASSIGN tt-movto.qt-ped-reserva = tt-movto.qt-ped-reserva + ob-etiqueta.quantidade
                   tt-movto.qt-saldo = tt-movto.qt-saldo - ob-etiqueta.quantidade.

            FIND tt-ped-reserva WHERE
                 tt-ped-reserva.row-tt-movto = ROWID(tt-movto) AND
                 tt-ped-reserva.num-reserva = ped-reserva-it.num-reserva 
                 NO-ERROR.
            IF NOT AVAIL tt-ped-reserva THEN DO.
               CREATE tt-ped-reserva.
               ASSIGN tt-ped-reserva.row-tt-movto = ROWID(tt-movto)
                      tt-ped-reserva.num-reserva = ped-reserva-it.num-reserva.
            END.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-retorna-temp-table w-livre 
PROCEDURE pi-retorna-temp-table :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tt-estoque. 
    DEFINE INPUT PARAMETER p-cod-estab AS   CHAR.
    DEFINE INPUT PARAMETER p-it-codigo LIKE ob-etiqueta.it-codigo.
    DEFINE INPUT PARAMETER p-cod-refer LIKE ob-etiqueta.cod-refer.
    DEFINE INPUT PARAMETER p-nr-lote LIKE ob-etiqueta.nr-lote.
    DEFINE INPUT PARAMETER p-dt-limite AS CHAR.

    ASSIGN c-dt-limite        = p-dt-limite 
           c-cod-estabel      = p-cod-estab
           c-it-codigo-ini    = p-it-codigo
           c-it-codigo-fin    = p-it-codigo
           c-cod-refer-ini    = p-cod-refer
           c-cod-refer-fin    = p-cod-refer
           c-corte-comerc-ini = ""
           c-corte-comerc-fin = "ZZ"
           l-itens-relac      = NO 
           l-dep-corte        = NO
           l-lote-todos       = NO
           l-lote-pp          = NO
           l-lote-pd          = NO
           l-lote-rp          = NO
           l-lote-rd          = NO
           l-lote-ca          = NO
           l-lote-sc          = NO
           l-batch            = YES.

    ASSIGN l-lote-pp = LOOKUP('pp',p-nr-lote) > 0.
    ASSIGN l-lote-pd = LOOKUP('pd',p-nr-lote) > 0.
    ASSIGN l-lote-rp = LOOKUP('rp',p-nr-lote) > 0.
    ASSIGN l-lote-rd = LOOKUP('rd',p-nr-lote) > 0.
    ASSIGN l-lote-ca = LOOKUP('ca',p-nr-lote) > 0.
    ASSIGN l-lote-sc = LOOKUP('sc',p-nr-lote) > 0.

    /*                        
    CASE p-nr-lote.
        WHEN 'pp' THEN ASSIGN l-lote-pp = YES
                              l-lote-rp = YES.
        WHEN 'pd' THEN ASSIGN l-lote-pd = YES
                              l-lote-rd = YES.
        WHEN 'rp' THEN ASSIGN l-lote-rp = YES.
        WHEN 'rd' THEN ASSIGN l-lote-rd = YES.
        WHEN 'ca' THEN ASSIGN l-lote-ca = YES.
        WHEN 'sc' THEN ASSIGN l-lote-sc = YES.
    END.
    */


    RUN pi-popula-browse.  
   
    FOR EACH tt-movto NO-LOCK.

        FIND tt-itens WHERE
            ROWID(tt-itens) = tt-movto.row-tt-itens NO-LOCK NO-ERROR.

        IF AVAIL tt-itens THEN DO:
           FIND tt-estoque WHERE
                tt-estoque.it-codigo    = tt-itens.it-codigo AND
                tt-estoque.cod-refer    = tt-itens.cod-refer AND
                tt-estoque.corte-comerc = tt-movto.lote NO-LOCK NO-ERROR.
           IF NOT AVAIL tt-estoque THEN DO:
              CREATE tt-estoque.
              ASSIGN tt-estoque.it-codigo      = tt-itens.it-codigo 
                     tt-estoque.cod-refer      = tt-itens.cod-refer 
                     tt-estoque.corte-comerc   = tt-movto.lote.
           END.
           ASSIGN tt-estoque.qt-estoque = tt-estoque.qt-estoque + tt-movto.qt-estoque    
                  tt-estoque.qt-res-antc = tt-estoque.qt-res-antc + tt-movto.qt-res-antc   
                  tt-estoque.qt-ped-reserva = tt-estoque.qt-ped-reserva + tt-movto.qt-ped-reserva
                  tt-estoque.qt-trf = tt-estoque.qt-trf + tt-movto.qt-trf        
                  tt-estoque.qt-benefic = tt-estoque.qt-benefic + tt-itens.qt-benefic    
                  tt-estoque.qt-carteira = tt-estoque.qt-carteira + tt-movto.qt-carteira   
                  tt-estoque.qt-res-cart = tt-estoque.qt-res-cart + tt-movto.qt-res-cart   
                  tt-estoque.qt-saldo = tt-estoque.qt-saldo + tt-movto.qt-saldo. 
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-totais w-livre 
PROCEDURE pi-totais :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN fi-tot-estoque  = 0
           fi-tot-res-antc = 0
           fi-tot-reserva  = 0
           fi-tot-trf      = 0
           fi-tot-benefic  = 0
           fi-tot-carteira = 0
           fi-tot-res-cart = 0
           fi-tot-saldo    = 0.

    FOR EACH tt-movto WHERE
             tt-movto.row-tt-itens = ROWID(tt-itens) AND
             tt-movto.visualiza = YES NO-LOCK.
        ASSIGN fi-tot-estoque  = fi-tot-estoque + tt-movto.qt-estoque
               fi-tot-res-antc = fi-tot-res-antc + tt-movto.qt-res-antc
               fi-tot-reserva  = fi-tot-reserva + tt-movto.qt-ped-reserva
               fi-tot-trf      = fi-tot-trf + tt-movto.qt-trf
               fi-tot-benefic  = fi-tot-benefic + tt-itens.qt-benefic
               fi-tot-carteira = fi-tot-carteira + 
                                 IF tt-movto.qt-carteira - tt-movto.qt-res-cart >= 0
                                 THEN tt-movto.qt-carteira - tt-movto.qt-res-cart
                                 ELSE 0
               fi-tot-res-cart = fi-tot-res-cart + tt-movto.qt-res-cart
               fi-tot-saldo    = fi-tot-saldo + tt-movto.qt-saldo.
    END.
    IF fi-tot-carteira < 0 THEN
       ASSIGN fi-tot-carteira = 0.

    ASSIGN fi-tot-saldo:FGCOLOR IN FRAME {&FRAME-NAME} = ?.
    IF fi-tot-saldo < 0 THEN
       ASSIGN fi-tot-saldo:FGCOLOR IN FRAME {&FRAME-NAME} = 12.

    DISP fi-tot-estoque 
         fi-tot-res-antc
         fi-tot-reserva 
         fi-tot-trf     
         fi-tot-benefic 
         fi-tot-carteira
         fi-tot-res-cart
         fi-tot-saldo   
         WITH FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-trf w-livre 
PROCEDURE pi-trf :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH ob-trf WHERE 
             ob-trf.situacao = 1 AND
             ob-trf.it-codigo >= c-it-codigo-ini AND
             ob-trf.it-codigo <= c-it-codigo-fin AND
             ob-trf.cod-refer >= c-cod-refer-ini AND 
             ob-trf.cod-refer <= c-cod-refer-fin AND
             LOOKUP(ob-trf.nr-lote,c-lotes) <> 0 NO-LOCK.

        RUN pi-ver-digita (INPUT "Item",
                           INPUT ob-trf.it-codigo).
        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

        RUN pi-ver-digita (INPUT "Referància",
                           INPUT ob-trf.cod-refer).
        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

        IF i-opc-acabado = 1 AND SUBSTR(ob-trf.cod-refer,7,1) <> '0' THEN NEXT.
        IF i-opc-acabado = 2 AND SUBSTR(ob-trf.cod-refer,7,1) = '0' THEN NEXT.

        /*
        IF ob-trf.corte-comerc < c-corte-comerc-ini OR  
           ob-trf.corte-comerc > c-corte-comerc-fin THEN NEXT.
    
        RUN pi-ver-digita (INPUT "Corte_Comercial",
                           INPUT ob-trf.corte-comerc).
        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.
        */
          
        FIND item WHERE
             item.it-codigo = ob-trf.it-codigo NO-LOCK NO-ERROR.
        /*IF item.deposito-pad <> c-cod-depos THEN NEXT.*/
    
        FIND item-ext WHERE
             item-ext.it-codigo = item.it-codigo NO-LOCK NO-ERROR.
    
        /* IF c-tp-artigo <> 'A' THEN 
           IF AVAIL item-ext AND
              (item-ext.indigo = YES AND c-tp-artigo <> "I") OR
              (item-ext.indigo = NO  AND c-tp-artigo <> "O") THEN NEXT. */
        /*
        FIND ref-item-ext WHERE
             ref-item-ext.it-codigo = ob-trf.it-codigo AND
             ref-item-ext.cod-refer = ob-trf.cod-refer 
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
        FIND tt-itens WHERE
             tt-itens.it-codigo = ob-trf.it-codigo AND
             tt-itens.cod-refer = ob-trf.cod-refer AND
             tt-itens.lote      = ob-trf.nr-lote NO-ERROR.

        IF NOT AVAIL tt-itens THEN DO.
           CREATE tt-itens.
           ASSIGN tt-itens.it-codigo = ob-trf.it-codigo 
                  tt-itens.cod-refer = ob-trf.cod-refer
                  tt-itens.lote      = ob-trf.nr-lote.
        END.

        FOR EACH ob-etq-trf OF ob-trf NO-LOCK.
            FIND ob-etiqueta WHERE
                 ob-etiqueta.cod-estabel  = c-cod-estabel AND
                 ob-etiqueta.num-etiqueta = ob-etq-trf.num-etiqueta
                 NO-LOCK NO-ERROR.
            IF NOT AVAIL ob-etiqueta THEN NEXT.

            FIND tt-movto WHERE
                 tt-movto.row-tt-itens = ROWID(tt-itens) AND
                 tt-movto.lote = 'Z'
                 NO-LOCK NO-ERROR.

            IF NOT AVAIL tt-movto THEN DO.
               CREATE tt-movto.
               ASSIGN tt-movto.row-tt-itens = ROWID(tt-itens)
                      tt-movto.lote = 'Z'.
            END.
            ASSIGN tt-movto.qt-trf = tt-movto.qt-trf + ob-etiqueta.quantidade
                   tt-movto.qt-saldo = tt-movto.qt-saldo + ob-etiqueta.quantidade.

            FIND tt-trf WHERE
                 tt-trf.row-tt-movto = ROWID(tt-movto) AND
                 tt-trf.num-trf = ob-trf.num-trf 
                 NO-ERROR.
            IF NOT AVAIL tt-trf THEN DO.
               CREATE tt-trf.
               ASSIGN tt-trf.row-tt-movto = ROWID(tt-movto)
                      tt-trf.num-trf = ob-trf.num-trf.
            END.

            FIND tt-movto WHERE
                 tt-movto.row-tt-itens = ROWID(tt-itens) AND
                 tt-movto.lote = 'Z'
                 NO-LOCK NO-ERROR.

            IF NOT AVAIL tt-movto THEN DO.
               CREATE tt-movto.
               ASSIGN tt-movto.row-tt-itens = ROWID(tt-itens)
                      tt-movto.lote = 'Z'.
            END.
            
            ASSIGN tt-movto.qt-estoque = tt-movto.qt-estoque + ob-etiqueta.quantidade 
                   tt-movto.qt-trf = tt-movto.qt-trf - ob-etiqueta.quantidade.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-ver-digita w-livre 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-ver-estrutura w-livre 
PROCEDURE pi-ver-estrutura :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER p-it-codigo AS CHAR.
    FIND FIRST b-tt-itens WHERE
               b-tt-itens.it-codigo = p-it-codigo NO-LOCK NO-ERROR.
    IF NOT AVAIL b-tt-itens THEN DO.
       ASSIGN c-it-codigo-ini = p-it-codigo
              c-it-codigo-fin = p-it-codigo.

       IF NOT l-batch THEN DO.
          {utp/ut-liter.i Calculando_Estoque *}
          RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).
       END.

       RUN pi-etiquetas (INPUT 3).             
       RUN pi-etiquetas (INPUT 4).             

       IF NOT l-batch THEN DO.
          {utp/ut-liter.i Calculando_Carteira *}
          RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).
       END.

       RUN pi-carteira (INPUT 1).
       RUN pi-carteira (INPUT 2).
       RUN pi-carteira (INPUT 4).
       RUN pi-carteira (INPUT 5).

       IF NOT l-batch THEN DO.
          {utp/ut-liter.i Calculando_Reservas *}
          RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).
       END.
       RUN pi-reserva.

       IF NOT l-batch THEN DO.
          {utp/ut-liter.i Calculando_Transformaá‰es *}
          RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).
       END.
       RUN pi-trf.

       IF NOT l-batch THEN DO.
          {utp/ut-liter.i Calculando_Programaá‰es *}
          RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).
       END.
       RUN pi-pcp.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-livre  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-movto"}
  {src/adm/template/snd-list.i "tt-itens"}
  {src/adm/template/snd-list.i "item"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-livre 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-benefic w-livre 
FUNCTION fn-benefic RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

   DEF VAR de-benefic AS DEC.
   
   ASSIGN de-benefic = 0.
   IF CURRENT-RESULT-ROW("br-movto") = 1 THEN
      ASSIGN de-benefic = tt-itens.qt-benefic.
 
   RETURN de-benefic.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-carteira w-livre 
FUNCTION fn-carteira RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
   DEF VAR de-carteira AS DEC.
   
   ASSIGN de-carteira = tt-movto.qt-carteira - tt-movto.qt-res-cart.
   IF de-carteira < 0 THEN
      ASSIGN de-carteira = 0.

   RETURN de-carteira.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-obsoleto w-livre 
FUNCTION fn-obsoleto RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  /*
  FIND ref-item-ext WHERE
       ref-item-ext.it-codigo = tt-itens.it-codigo AND
       ref-item-ext.cod-refer = tt-itens.cod-refer
       NO-LOCK NO-ERROR.

  IF AVAIL ref-item-ext THEN DO.
     CASE ref-item-ext.cod-obsoleto.
         WHEN '0' THEN RETURN 'Lanáamento'.
         WHEN '1' THEN RETURN 'Fora de Produá∆o'.
         WHEN '2' THEN RETURN 'Em Produá∆o'.
         WHEN '3' THEN RETURN 'Retalho'.
         WHEN '4' THEN RETURN 'Exclusividade'.
         WHEN '5' THEN RETURN 'Exportaá∆o'.
     END CASE.
  END.
  ELSE */
     RETURN "N∆o Definido".  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

