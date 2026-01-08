&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
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
{include/i-prgvrs.i XX9999 9.99.99.999}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEF BUFFER empresa FOR mgadm.empresa.

/* Local Variable Definitions ---                                       */
DEF TEMP-TABLE tt-repres LIKE repres
    FIELD visualiza      AS LOG.

DEF TEMP-TABLE tt-ped-item LIKE ped-item.

DEF TEMP-TABLE tt-ped-venda LIKE ped-venda
    FIELD visualiza         AS LOG
    FIELD tipo-pedido       LIKE ped-venda-ext.tp-pedido
    FIELD qt-pedida         LIKE ped-item.qt-pedida
    FIELD qt-reservada      LIKE ped-item.qt-pedida
    FIELD qt-aberto         AS   DEC FORMAT "->,>>>,>>9.99"
    FIELD vl-reservado      LIKE ped-venda.vl-tot-ped
    FIELD vl-aberto         LIKE ped-venda.vl-tot-ped
    FIELD vl-solic          LIKE ped-venda.vl-tot-ped
    FIELD dt-solic          AS DATE
    FIELD qt-solic          LIKE ped-venda.vl-tot-ped
    FIELD usuario           AS CHAR
    FIELD marca             AS CHAR
    FIELD imprime           AS CHAR
    FIELD dt-atraso         AS DATE
    FIELD nr-embarque       LIKE pre-fatur.nr-embarque
    FIELD verifica-envio    AS LOG
    INDEX indice1 IS PRIMARY dt-entrega.

DEF TEMP-TABLE tt-ped-parcela LIKE ped-parcela
    FIELD faturado            AS LOG
    FIELD nova-sequencia      AS LOG
    FIELD sequencias          AS CHAR
    FIELD dias-atraso         AS INT
    FIELD nome-abrev          LIKE ped-venda.nome-abrev.


DEF BUFFER b-tt-repres FOR tt-repres.
DEF BUFFER b-tt-ped-venda FOR tt-ped-venda.
DEF BUFFER b-tt-ped-parcela FOR tt-ped-parcela.

DEF NEW GLOBAL SHARED VAR gr-ob-etiqueta AS ROWID NO-UNDO.
DEF VAR h-acomp               AS HANDLE.
DEF VAR h-query               AS HANDLE. 
DEF VAR h-repres              AS HANDLE.
DEF VAR c-dia                 AS CHAR.
DEF VAR da-dt-entrega-ini     AS DATE.
DEF VAR da-dt-entrega-fin     AS DATE.
DEF VAR c-lotes               AS CHAR.
DEF VAR c-situacao            AS CHAR.
DEF VAR de-qt-pedida          AS DEC.
DEF VAR de-qt-reservada       AS DEC.
DEF VAR de-qt-aberto          AS DEC.
DEF VAR de-vl-reservado       AS DEC.
DEF VAR de-vl-aberto          AS DEC.
DEF VAR de-vl-desconto        AS DEC.
DEF VAR de-vlr-solic          AS DEC.
DEF VAR de-qtd-solic          AS DEC.
DEF VAR de-qt-solicitado      AS DEC.
DEF VAR de-tot-vlr-todos      AS DEC.
DEF VAR i-hr-solic            AS INT.
DEF VAR c-nr-pedcli           LIKE ped-venda.nr-pedcli.
DEF VAR l-gestao              AS LOG.
DEF VAR l-canc-remessa        AS LOG.
DEF VAR d-date-email          AS DATE.
DEF VAR d-atraso              AS INT.

DEF VAR i-lin          AS INT INITIAL 99.
DEF VAR i-pag          AS INT INITIAL  1.
DEF VAR c-mensagem     AS CHAR.
DEF VAR c-arq-email    AS CHAR FORMAT "x(45)".
DEF VAR de-tot-qtd     AS DEC.
DEF VAR de-tot-vlr     AS DEC.
DEF VAR de-tot-ger     AS DEC.
DEF VAR de-tot-repres  AS DEC.
DEF VAR c-empresa      AS CHAR.


/* Variaveis da Rotina de Impress∆o */
DEFINE VAR i-saida             AS INT.
DEFINE VAR c-saida             AS CHAR.
DEFINE VAR i-num-copias        AS INT.

/* Global Variable Definitions ---                                       */
DEF NEW GLOBAL SHARED VAR gr-ped-venda  AS ROWID NO-UNDO.
DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

/* Variavies de ParÉmetros */

DEFINE VAR c-cod-estabel       AS CHAR.
DEFINE VAR c-dt-limite-ini     AS CHAR.
DEFINE VAR c-dt-limite-fin     AS CHAR.
DEFINE VAR c-nr-pedcli-ini     LIKE ped-item-ext.nr-pedcli INIT "".
DEFINE VAR c-nr-pedcli-fin     LIKE ped-item-ext.nr-pedcli INIT "ZZZZZZZZ".  
DEFINE VAR c-it-codigo-ini     LIKE ped-item-ext.it-codigo INIT "".                 
DEFINE VAR c-it-codigo-fin     LIKE ped-item-ext.it-codigo INIT "ZZZZZZZZZZZZZZZ".  
DEFINE VAR c-cod-refer-ini     LIKE ped-item-ext.cod-refer.
DEFINE VAR c-cod-refer-fin     LIKE ped-item-ext.cod-refer INIT "ZZZZZZZZZZ".   
DEFINE VAR c-nome-abrev-ini    LIKE ped-venda.nome-abrev. 
DEFINE VAR c-nome-abrev-fin    LIKE ped-venda.nome-abrev INIT "ZZZZZZZZZZZZ".
DEFINE VAR c-no-ab-reppri-ini  LIKE ped-venda.no-ab-reppri. 
DEFINE VAR c-no-ab-reppri-fin  LIKE ped-venda.no-ab-reppri INIT 'ZZZZZZZZZZZZ'.
DEFINE VAR c-cod-obsoleto-ini  AS CHAR.
DEFINE VAR c-cod-obsoleto-fin  AS CHAR INIT "Z".
DEFINE VAR c-corte-comerc-ini  LIKE ob-etiqueta.corte-comerc INIT "A".   
DEFINE VAR c-corte-comerc-fin  LIKE ob-etiqueta.corte-comerc INIT "Z".   
DEFINE VAR c-cod-depos         AS CHAR INIT "EXP".
DEFINE VAR l-lote-todos        AS LOG  INIT YES.
DEFINE VAR l-lote-pp           AS LOG  INIT NO.
DEFINE VAR l-lote-pd           AS LOG  INIT NO.
DEFINE VAR l-lote-rp           AS LOG  INIT NO.
DEFINE VAR l-lote-rd           AS LOG  INIT NO.
DEFINE VAR l-lote-sc           AS LOG  INIT NO.
DEFINE VAR l-lote-ca           AS LOG  INIT NO.
DEFINE VAR c-opc-artigo        AS CHAR INIT 'A'.
DEFINE VAR c-bloqueio          AS CHAR INIT 'T'.
DEFINE VAR l-sit-todas         AS LOG  INIT NO.
DEFINE VAR l-sit-abe           AS LOG  INIT YES.
DEFINE VAR l-sit-atp           AS LOG  INIT YES.
DEFINE VAR l-sit-att           AS LOG  INIT NO.
DEFINE VAR l-sit-pen           AS LOG  INIT NO.
DEFINE VAR l-sit-sus           AS LOG  INIT YES.
DEFINE VAR l-sit-can           AS LOG  INIT NO.
DEFINE VAR i-credito           AS INT  INIT 2.
DEFINE VAR l-ok                AS LOG.

/* ***************************  Definitions  ************************** */
/* INICIO DA DEFINIÄ«O DAS TABELAS TEMPORARIAS; PARA A CHAMADA DO RELATORIO ESPD0002RP.P */

{esinc/espd0002.i}

DEF TEMP-TABLE tt-raw-digita
    FIELD raw-digita      AS RAW.

DEF TEMP-TABLE tt-digita
    FIELD opcao  AS CHAR
    FIELD campo AS CHAR
    FIELD valor AS CHAR.

DEFINE VAR raw-param   AS RAW NO-UNDO.
DEFINE VAR rs-execucao AS INTEGER INITIAL 1. 
DEFINE FRAME f-pg-imp rs-execucao AT ROW 5.75 COL 3 NO-LABEL.
/* FIM DAS DEFINIÄÂES DO RELATORIO ESPD0002RP.P */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-livre
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad
&Scoped-define BROWSE-NAME br-parcelas

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-ped-parcela tt-ped-venda tt-repres

/* Definitions for BROWSE br-parcelas                                   */
&Scoped-define FIELDS-IN-QUERY-br-parcelas tt-ped-parcela.nome-abrev tt-ped-parcela.dt-solic tt-ped-parcela.dt-pagto tt-ped-parcela.dias-atraso tt-ped-parcela.usuario tt-ped-parcela.qtd-solic tt-ped-parcela.vlr-solic   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-parcelas   
&Scoped-define SELF-NAME br-parcelas
&Scoped-define OPEN-QUERY-br-parcelas RUN pi-tot-parcela. OPEN QUERY {&SELF-NAME} FOR EACH tt-ped-parcela WHERE                                  tt-ped-parcela.cod-estabel = tt-ped-venda.cod-estabel AND                                  tt-ped-parcela.nr-pedido = tt-ped-venda.nr-pedido AND                                  tt-ped-parcela.faturado  = NO                                  NO-LOCK                               BY tt-ped-parcela.dt-solic                               BY tt-ped-parcela.hr-solic.
&Scoped-define TABLES-IN-QUERY-br-parcelas tt-ped-parcela
&Scoped-define FIRST-TABLE-IN-QUERY-br-parcelas tt-ped-parcela


/* Definitions for BROWSE br-pedidos                                    */
&Scoped-define FIELDS-IN-QUERY-br-pedidos tt-ped-venda.nr-pedcli tt-ped-venda.nome-abrev tt-ped-venda.nome-transp /* tt-ped-venda.dt-entrega */ tt-ped-venda.nr-embarque tt-ped-venda.dt-implant tt-ped-venda.qt-pedida tt-ped-venda.qt-reservada tt-ped-venda.vl-aberto tt-ped-venda.vl-desconto tt-ped-venda.vl-tot-ped /* FIND pre-fatur OF ped-venda NO-LOCK NO-ERROR. */   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-pedidos   
&Scoped-define SELF-NAME br-pedidos
&Scoped-define OPEN-QUERY-br-pedidos RUN pi-totais. OPEN QUERY {&SELF-NAME} FOR EACH tt-ped-venda WHERE                                  tt-ped-venda.no-ab-reppri = tt-repres.nome-abrev  AND                                  tt-ped-venda.visualiza = YES NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-pedidos tt-ped-venda
&Scoped-define FIRST-TABLE-IN-QUERY-br-pedidos tt-ped-venda


/* Definitions for BROWSE br-repres                                     */
&Scoped-define FIELDS-IN-QUERY-br-repres tt-repres.nome-abrev   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-repres   
&Scoped-define SELF-NAME br-repres
&Scoped-define QUERY-STRING-br-repres FOR EACH tt-repres  WHERE                                  tt-repres.visualiza = YES NO-LOCK
&Scoped-define OPEN-QUERY-br-repres OPEN QUERY {&SELF-NAME} FOR EACH tt-repres  WHERE                                  tt-repres.visualiza = YES NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-repres tt-repres
&Scoped-define FIRST-TABLE-IN-QUERY-br-repres tt-repres


/* Definitions for FRAME f-cad                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-cad ~
    ~{&OPEN-QUERY-br-parcelas}~
    ~{&OPEN-QUERY-br-pedidos}~
    ~{&OPEN-QUERY-br-repres}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-10 rt-button rs-solic-pag bt-param ~
br-repres br-pedidos bt-desmarca bt-marca bt-todos bt-nenhum bt-vapara ~
bt-imprime-3 bt-consulta bt-imprime br-parcelas ed-obs-pedido bt-modifica ~
bt-ok bt-log 
&Scoped-Define DISPLAYED-OBJECTS rs-solic-pag fi-qt-pedida fi-qt-reservada ~
fi-vl-faturar fi-desconto fi-vl-total ed-obs-pedido fi-qt-parcela ~
fi-vl-parcela 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-4 bt-elimina bt-consulta bt-imprime bt-modifica bt-log 

/* _UIB-PREPROCESSOR-BLOCK-END */
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
DEFINE BUTTON bt-consulta AUTO-GO 
     IMAGE-UP FILE "image/im-det.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Detalhar Pedido"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-desmarca AUTO-GO 
     IMAGE-UP FILE "image/im-cllps.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Desmarca Pedido"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-elimina AUTO-GO 
     IMAGE-UP FILE "image/gr-eli.bmp":U NO-FOCUS
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Elimina Parcela J† Solicitada"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-imprime AUTO-GO 
     IMAGE-UP FILE "image/im-prigr.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Imprimir Pedido"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-imprime-3 
     IMAGE-UP FILE "image/im-pri.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Imprimir as Solicitaá‰es de Valores Pedidos a Vista.".

DEFINE BUTTON bt-log AUTO-GO 
     IMAGE-UP FILE "image/im-log.bmp":U NO-FOCUS
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Alteraá‰es do Pedido"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-marca AUTO-GO 
     IMAGE-UP FILE "image/im-expan.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Marca Pedido"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-modifica AUTO-GO 
     IMAGE-UP FILE "image/im-mod.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Modificar Pedido"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-nenhum AUTO-GO 
     IMAGE-UP FILE "image/im-nenhum.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Desmarca TODOS os Pedidos"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-ok AUTO-GO 
     IMAGE-UP FILE "image/im-cq.bmp":U
     LABEL "OK" 
     SIZE 5 BY 1.25 TOOLTIP "Confirma Solicitaá∆o de Valores".

DEFINE BUTTON bt-param 
     IMAGE-UP FILE "image/im-fil.bmp":U
     LABEL "Button 3" 
     SIZE 4.29 BY 1.21 TOOLTIP "ParÉmetros".

DEFINE BUTTON bt-todos AUTO-GO 
     IMAGE-UP FILE "image\im-ran_a.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Marca TODOS os Pedidos"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-vapara AUTO-GO 
     IMAGE-UP FILE "image/im-vapra.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Posicionar no Pedido"
     BGCOLOR 8 FONT 10.

DEFINE VARIABLE ed-obs-pedido AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 34 BY 7.25 NO-UNDO.

DEFINE VARIABLE fi-desconto AS DECIMAL FORMAT "->>,>>9.99":R16 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10.72 BY .79 NO-UNDO.

DEFINE VARIABLE fi-qt-parcela AS DECIMAL FORMAT "->>>,>>9.99":R20 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.29 BY .79 NO-UNDO.

DEFINE VARIABLE fi-qt-pedida AS DECIMAL FORMAT "->>>,>>9.99":R16 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9.57 BY .79 NO-UNDO.

DEFINE VARIABLE fi-qt-reservada AS DECIMAL FORMAT "->>>,>>9.99":R16 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10.43 BY .79 NO-UNDO.

DEFINE VARIABLE fi-vl-faturar AS DECIMAL FORMAT "->>>,>>9.99":R16 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10.43 BY .79 NO-UNDO.

DEFINE VARIABLE fi-vl-parcela AS DECIMAL FORMAT "->>>,>>9.99":R20 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY .79 NO-UNDO.

DEFINE VARIABLE fi-vl-total AS DECIMAL FORMAT "->>>,>>9.99":R16 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9.86 BY .79 NO-UNDO.

DEFINE VARIABLE rs-solic-pag AS INTEGER INITIAL 3 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Pedidos Solicitados", 1,
"Pedidos N∆o Solicitados", 2,
"Todos Pedidos", 3
     SIZE 114.29 BY 1.17
     FGCOLOR 9 FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 6 BY 16.92
     BGCOLOR 8 .

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 140 BY 1.46
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-parcelas FOR 
      tt-ped-parcela SCROLLING.

DEFINE QUERY br-pedidos FOR 
      tt-ped-venda SCROLLING.

DEFINE QUERY br-repres FOR 
      tt-repres SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-parcelas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-parcelas w-livre _FREEFORM
  QUERY br-parcelas NO-LOCK DISPLAY
      tt-ped-parcela.nome-abrev                COLUMN-LABEL "Cliente"          FORMAT "x(15)"      WIDTH 17
      tt-ped-parcela.dt-solic                  COLUMN-LABEL "Dt.Solicitaá∆o"   FORMAT "99/99/9999" WIDTH 13
      tt-ped-parcela.dt-pagto                  COLUMN-LABEL "Dt. Limite Pgto"  FORMAT "99/99/9999" WIDTH 15
      tt-ped-parcela.dias-atraso               COLUMN-LABEL "Dias Atraso"      FORMAT ">>9"        WIDTH 13
      tt-ped-parcela.usuario                   COLUMN-LABEL "Usuario"          FORMAT "x(15)"      WIDTH 16
      tt-ped-parcela.qtd-solic                 COLUMN-LABEL "Qtde Solicitada"  FORMAT ">>>,>>9.99" WIDTH 16
      tt-ped-parcela.vlr-solic                 COLUMN-LABEL "Valor Solicitado" FORMAT ">>>,>>9.99" WIDTH 14
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 76 BY 7.42
         FONT 1
         TITLE "Pagamentos Solicitados" ROW-HEIGHT-CHARS .54.

DEFINE BROWSE br-pedidos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-pedidos w-livre _FREEFORM
  QUERY br-pedidos NO-LOCK DISPLAY
      tt-ped-venda.nr-pedcli       COLUMN-LABEL "Pedido"         FORMAT "x(6)"       WIDTH 7   
      tt-ped-venda.nome-abrev      COLUMN-LABEL "Cliente"        FORMAT "x(12)"      WIDTH 12
      tt-ped-venda.nome-transp     COLUMN-LABEL "Transportadora" FORMAT "x(12)"      WIDTH 13
    /*  tt-ped-venda.dt-entrega      COLUMN-LABEL "Dt.Entrega"     FORMAT "99/99/9999" WIDTH 10  */
      tt-ped-venda.nr-embarque     COLUMN-LABEL "NßEmbarque"     FORMAT ">>>>,>>9"   WIDTH 10
      tt-ped-venda.dt-implant      COLUMN-LABEL "Dt.Implant"     FORMAT "99/99/9999" WIDTH 10
      tt-ped-venda.qt-pedida       COLUMN-LABEL "Qt.Pedida"      FORMAT ">>>,>>9.99" WIDTH 9
      tt-ped-venda.qt-reservada    COLUMN-LABEL "Qt.Reservada"   FORMAT ">>>,>>9.99" WIDTH 10      
      tt-ped-venda.vl-aberto       COLUMN-LABEL "Valor Faturar"  FORMAT ">>>,>>9.99" WIDTH 10
      tt-ped-venda.vl-desconto     COLUMN-LABEL "Valor Desconto" FORMAT ">>>,>>9.99" WIDTH 11
      tt-ped-venda.vl-tot-ped      COLUMN-LABEL "Valor Total"    FORMAT ">>>,>>9.99" WIDTH 10

        /*  FIND pre-fatur OF ped-venda NO-LOCK NO-ERROR. */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 111.43 BY 8.25
         FONT 1
         TITLE "Pedidos" ROW-HEIGHT-CHARS .5.

DEFINE BROWSE br-repres
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-repres w-livre _FREEFORM
  QUERY br-repres NO-LOCK DISPLAY
      tt-repres.nome-abrev COLUMN-LABEL "Representante" FORMAT "x(33)" WIDTH 34
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 21.43 BY 17
         FONT 6
         TITLE "Representantes" ROW-HEIGHT-CHARS .57.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     bt-elimina AT ROW 16.67 COL 136.14
     rs-solic-pag AT ROW 1.13 COL 2.14 NO-LABEL
     bt-param AT ROW 1.17 COL 121
     br-repres AT ROW 2.75 COL 1.57
     br-pedidos AT ROW 2.75 COL 24
     bt-desmarca AT ROW 2.92 COL 136.14
     bt-marca AT ROW 4.17 COL 136.14
     bt-todos AT ROW 5.42 COL 136.14
     bt-nenhum AT ROW 6.75 COL 136.14
     bt-vapara AT ROW 8.08 COL 136.14
     bt-imprime-3 AT ROW 9.42 COL 136.29
     bt-consulta AT ROW 10.79 COL 136.14
     fi-qt-pedida AT ROW 11.29 COL 87.57 RIGHT-ALIGNED NO-LABEL
     fi-qt-reservada AT ROW 11.29 COL 98.15 RIGHT-ALIGNED NO-LABEL
     fi-vl-faturar AT ROW 11.29 COL 108.72 RIGHT-ALIGNED NO-LABEL
     fi-desconto AT ROW 11.29 COL 120.15 RIGHT-ALIGNED NO-LABEL WIDGET-ID 4
     fi-vl-total AT ROW 11.29 COL 130.72 RIGHT-ALIGNED NO-LABEL WIDGET-ID 2
     bt-imprime AT ROW 12.17 COL 136.14
     br-parcelas AT ROW 12.33 COL 24
     ed-obs-pedido AT ROW 12.5 COL 101 NO-LABEL WIDGET-ID 8
     bt-modifica AT ROW 14.96 COL 136.14
     bt-ok AT ROW 18.21 COL 136.14
     fi-qt-parcela AT ROW 19.83 COL 80.86 RIGHT-ALIGNED NO-LABEL
     bt-log AT ROW 13.54 COL 136.14
     fi-vl-parcela AT ROW 19.83 COL 96 RIGHT-ALIGNED NO-LABEL
     "Totais:" VIEW-AS TEXT
          SIZE 9.57 BY 1 AT ROW 19.75 COL 57.14 WIDGET-ID 6
          FONT 8
     "Totais:" VIEW-AS TEXT
          SIZE 9.57 BY 1 AT ROW 11.21 COL 69.14
          FONT 8
     RECT-10 AT ROW 2.75 COL 135.72
     rt-button AT ROW 1 COL 1.72
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.04
         SIZE 145.29 BY 20.46
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
         TITLE              = "Analise e Gerenciamento Pedidos a Vista"
         COLUMN             = 14.29
         ROW                = 7.04
         HEIGHT             = 19.96
         WIDTH              = 141.14
         MAX-HEIGHT         = 28.13
         MAX-WIDTH          = 146.86
         VIRTUAL-HEIGHT     = 28.13
         VIRTUAL-WIDTH      = 146.86
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
/* BROWSE-TAB br-repres bt-param f-cad */
/* BROWSE-TAB br-pedidos br-repres f-cad */
/* BROWSE-TAB br-parcelas bt-imprime f-cad */
/* SETTINGS FOR BUTTON bt-consulta IN FRAME f-cad
   4                                                                    */
/* SETTINGS FOR BUTTON bt-elimina IN FRAME f-cad
   NO-ENABLE 4                                                          */
/* SETTINGS FOR BUTTON bt-imprime IN FRAME f-cad
   4                                                                    */
/* SETTINGS FOR BUTTON bt-log IN FRAME f-cad
   4                                                                    */
/* SETTINGS FOR BUTTON bt-modifica IN FRAME f-cad
   4                                                                    */
/* SETTINGS FOR FILL-IN fi-desconto IN FRAME f-cad
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN fi-qt-parcela IN FRAME f-cad
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN fi-qt-pedida IN FRAME f-cad
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN fi-qt-reservada IN FRAME f-cad
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN fi-vl-faturar IN FRAME f-cad
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN fi-vl-parcela IN FRAME f-cad
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN fi-vl-total IN FRAME f-cad
   NO-ENABLE ALIGN-R                                                    */
ASSIGN 
       rs-solic-pag:HIDDEN IN FRAME f-cad           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-parcelas
/* Query rebuild information for BROWSE br-parcelas
     _START_FREEFORM
RUN pi-tot-parcela.
OPEN QUERY {&SELF-NAME} FOR EACH tt-ped-parcela WHERE
                                 tt-ped-parcela.cod-estabel = tt-ped-venda.cod-estabel AND
                                 tt-ped-parcela.nr-pedido = tt-ped-venda.nr-pedido AND
                                 tt-ped-parcela.faturado  = NO
                                 NO-LOCK
                              BY tt-ped-parcela.dt-solic
                              BY tt-ped-parcela.hr-solic.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-parcelas */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-pedidos
/* Query rebuild information for BROWSE br-pedidos
     _START_FREEFORM
RUN pi-totais.
OPEN QUERY {&SELF-NAME} FOR EACH tt-ped-venda WHERE
                                 tt-ped-venda.no-ab-reppri = tt-repres.nome-abrev  AND
                                 tt-ped-venda.visualiza = YES NO-LOCK
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-pedidos */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-repres
/* Query rebuild information for BROWSE br-repres
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-repres  WHERE
                                 tt-repres.visualiza = YES NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-repres */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON END-ERROR OF w-livre /* Analise e Gerenciamento Pedidos a Vista */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON WINDOW-CLOSE OF w-livre /* Analise e Gerenciamento Pedidos a Vista */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-parcelas
&Scoped-define SELF-NAME br-parcelas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-parcelas w-livre
ON ROW-DISPLAY OF br-parcelas IN FRAME f-cad /* Pagamentos Solicitados */
DO:
   
    IF tt-ped-parcela.dias-atraso > 0 THEN
       ASSIGN  tt-ped-parcela.nome-abrev:FONT IN BROWSE br-parcelas = 6
               tt-ped-parcela.dt-solic:FONT IN BROWSE br-parcelas = 6
               tt-ped-parcela.dias-atraso:FONT IN BROWSE br-parcelas = 6 
               tt-ped-parcela.usuario:FONT IN BROWSE br-parcelas = 6
               tt-ped-parcela.qtd-solic:FONT IN BROWSE br-parcelas = 6
               tt-ped-parcela.vlr-solic:FONT IN BROWSE br-parcelas = 6
               tt-ped-parcela.dt-pagto:FONT IN BROWSE br-parcelas = 6
               tt-ped-parcela.nome-abrev:FGCOLOR IN BROWSE br-parcelas = 12
               tt-ped-parcela.dt-solic:FGCOLOR IN BROWSE br-parcelas = 12
               tt-ped-parcela.dias-atraso:FGCOLOR IN BROWSE br-parcelas = 12
               tt-ped-parcela.usuario:FGCOLOR IN BROWSE br-parcelas = 12
               tt-ped-parcela.qtd-solic:FGCOLOR IN BROWSE br-parcelas = 12
               tt-ped-parcela.vlr-solic:FGCOLOR IN BROWSE br-parcelas = 12
               tt-ped-parcela.dt-pagto:FGCOLOR IN BROWSE br-parcelas = 12.     




END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-parcelas w-livre
ON VALUE-CHANGED OF br-parcelas IN FRAME f-cad /* Pagamentos Solicitados */
DO:
  ASSIGN bt-elimina:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
  IF AVAIL tt-ped-parcela THEN DO:
     IF tt-ped-parcela.vlr-solic > 0 AND l-gestao THEN
        ASSIGN bt-elimina:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
  
     /*IF tt-ped-venda.verifica-envio = YES AND 
        tt-ped-venda.vl-tot-ped = tt-ped-parcela.vlr-solic AND
        tt-ped-venda.vl-desconto = tt-ped-parcela.vl-desconto THEN
        ASSIGN bt-marca:SENSITIVE IN FRAME {&FRAME-NAME} = NO 
               bt-todos:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
       
     ELSE
        ASSIGN bt-marca:SENSITIVE IN FRAME {&FRAME-NAME} = YES
               bt-todos:SENSITIVE IN FRAME {&FRAME-NAME} = YES.  */
  END.
  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-pedidos
&Scoped-define SELF-NAME br-pedidos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-pedidos w-livre
ON ROW-DISPLAY OF br-pedidos IN FRAME f-cad /* Pedidos */
DO:
    
    IF tt-ped-venda.verifica-envio = YES THEN 
        ASSIGN tt-ped-venda.nr-pedcli:FGCOLOR IN BROWSE br-pedidos = 7
               tt-ped-venda.nome-abrev:FGCOLOR IN BROWSE br-pedidos = 7
               tt-ped-venda.nome-transp:FGCOLOR IN BROWSE br-pedidos = 7
             /*  tt-ped-venda.dt-entrega:FGCOLOR IN BROWSE br-pedidos = 7  */
               tt-ped-venda.nr-embarque:FGCOLOR IN BROWSE br-pedidos = 7
               tt-ped-venda.dt-implant:FGCOLOR IN BROWSE br-pedidos = 7
               tt-ped-venda.qt-pedida:FGCOLOR IN BROWSE br-pedidos = 7
               tt-ped-venda.qt-reservada:FGCOLOR IN BROWSE br-pedidos = 7
               tt-ped-venda.vl-aberto:FGCOLOR IN BROWSE br-pedidos = 7
               tt-ped-venda.vl-desconto:FGCOLOR IN BROWSE br-pedidos = 7
               tt-ped-venda.vl-tot-ped:FGCOLOR IN BROWSE br-pedidos = 7     
               tt-ped-venda.nr-pedcli:FONT IN BROWSE br-pedidos = 2 
               tt-ped-venda.nome-abrev:FONT IN BROWSE br-pedidos = 2
               tt-ped-venda.nome-transp:FONT IN BROWSE br-pedidos = 2
             /*  tt-ped-venda.dt-entrega:FONT IN BROWSE br-pedidos = 2   */
               tt-ped-venda.nr-embarque:FONT IN BROWSE br-pedidos = 2
               tt-ped-venda.dt-implant:FONT IN BROWSE br-pedidos = 2
               tt-ped-venda.qt-pedida:FONT IN BROWSE br-pedidos = 2
               tt-ped-venda.qt-reservada:FONT IN BROWSE br-pedidos = 2
               tt-ped-venda.vl-aberto:FONT IN BROWSE br-pedidos = 2
               tt-ped-venda.vl-desconto:FONT IN BROWSE br-pedidos = 2
               tt-ped-venda.vl-tot-ped:FONT IN BROWSE br-pedidos = 2.    

    IF tt-ped-venda.marca = "*" THEN
       ASSIGN  tt-ped-venda.nr-pedcli:FONT IN BROWSE br-pedidos = 6
               tt-ped-venda.nome-abrev:FONT IN BROWSE br-pedidos = 6
               tt-ped-venda.nome-transp:FONT IN BROWSE br-pedidos = 6
             /*  tt-ped-venda.dt-entrega:FONT IN BROWSE br-pedidos = 6  */
               tt-ped-venda.nr-embarque:FONT IN BROWSE br-pedidos = 6
               tt-ped-venda.dt-implant:FONT IN BROWSE br-pedidos = 6
               tt-ped-venda.qt-pedida:FONT IN BROWSE br-pedidos = 6
               tt-ped-venda.qt-reservada:FONT IN BROWSE br-pedidos = 6
               tt-ped-venda.vl-aberto:FONT IN BROWSE br-pedidos = 6
               tt-ped-venda.vl-desconto:FONT IN BROWSE br-pedidos = 6
               tt-ped-venda.vl-tot-ped:FONT IN BROWSE br-pedidos = 6.


 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-pedidos w-livre
ON VALUE-CHANGED OF br-pedidos IN FRAME f-cad /* Pedidos */
DO: 
    
    IF AVAIL tt-ped-venda THEN DO.
        ASSIGN ed-obs-pedido:SCREEN-VALUE = tt-ped-venda.observacoes.
       /* IF tt-ped-venda.verifica-envio = YES THEN 
           ASSIGN bt-marca:SENSITIVE IN FRAME {&FRAME-NAME} = NO
                  bt-todos:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
           
        ELSE
           ASSIGN bt-marca:SENSITIVE IN FRAME {&FRAME-NAME} = YES
                  bt-todos:SENSITIVE IN FRAME {&FRAME-NAME} = YES. */
        
        EMPTY TEMP-TABLE tt-ped-parcela.
    
        FOR EACH ped-parcela WHERE
                 ped-parcela.cod-estabel = tt-ped-venda.cod-estabel AND    /*  daf  */
                 ped-parcela.nr-pedido = tt-ped-venda.nr-pedido NO-LOCK.
            
            CREATE tt-ped-parcela.
            BUFFER-COPY ped-parcela TO tt-ped-parcela.
            ASSIGN tt-ped-parcela.nome-abrev = tt-ped-venda.nome-abrev.
                   
            RUN esapi/calc-dias-uteis.p (INPUT ped-parcela.dt-pagto + 1,           /* data inicial */
                                         INPUT TODAY,                              /*  data final  */
                                         OUTPUT d-atraso).                         /* data calculada */ 
            ASSIGN tt-ped-parcela.dias-atraso = d-atraso.
       
        END.
    END.

    {&OPEN-QUERY-br-parcelas}
    APPLY 'value-changed' TO br-parcelas.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-repres
&Scoped-define SELF-NAME br-repres
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-repres w-livre
ON VALUE-CHANGED OF br-repres IN FRAME f-cad /* Representantes */
DO:
   
   {&OPEN-QUERY-br-pedidos}
   APPLY 'value-changed' TO br-pedidos.


   APPLY 'entry' TO br-pedidos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-consulta w-livre
ON CHOOSE OF bt-consulta IN FRAME f-cad
DO:
   FIND ped-venda WHERE
        ped-venda.nr-pedcli = tt-ped-venda.nr-pedcli AND
        ped-venda.nome-abrev = tt-ped-venda.nome-abrev NO-LOCK NO-ERROR.
   ASSIGN gr-ped-venda = ROWID(ped-venda).

   ASSIGN w-livre:SENSITIVE = NO.
   RUN esp\espd4000.w (INPUT "Consultar").
   ASSIGN w-livre:SENSITIVE = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-desmarca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-desmarca w-livre
ON CHOOSE OF bt-desmarca IN FRAME f-cad
DO:
  IF AVAIL tt-ped-venda AND tt-ped-venda.marca = '*' THEN DO:
     ASSIGN tt-ped-venda.dt-solic     = ?
            tt-ped-venda.usuario      = ""
            tt-ped-venda.marca        = "".

      br-pedidos:REFRESH().
      APPLY 'value-changed' TO br-pedidos IN FRAME {&FRAME-NAME}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-elimina
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-elimina w-livre
ON CHOOSE OF bt-elimina IN FRAME f-cad
DO:
  MESSAGE "Tem Certeza que deseja Cancelar a Solicitaá∆o de Remessa de Valor"  SKIP 
          "R$ " + TRIM(STRING(tt-ped-parcela.vlr-solic, ">>>,>>>,>>9.99"))  +
          " do Pedido " + TRIM(STRING(tt-ped-parcela.nr-pedido, ">>>,>>>"))  +
          " das Sequencias " + tt-ped-parcela.sequencias 
          SKIP(1)
          "Confirma a Exclus∆o desta Solicitaá∆o de Remessa ?"
           VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE l-opc AS LOG.

  IF l-opc THEN DO:
     ASSIGN l-canc-remessa = NO.
     FOR EACH ped-parcela WHERE
              ped-parcela.cod-estabel = tt-ped-parcela.cod-estabel AND
              ped-parcela.nr-pedido = tt-ped-parcela.nr-pedido SHARE-LOCK.

         DELETE ped-parcela.
         ASSIGN l-canc-remessa = YES.
     END.

     IF l-canc-remessa THEN DO:
        FIND ped-venda WHERE
             ped-venda.nr-pedido = tt-ped-parcela.nr-pedido NO-LOCK NO-ERROR.
        RUN esapi/cria-log-pedvenda.p (INPUT ped-venda.nr-pedcli,
                                       INPUT ped-venda.nome-abrev,
                                       INPUT "A Solicitaá∆o de Remessa, no Valor R$ " + TRIM(STRING(tt-ped-parcela.vlr-solic, ">>>,>>>,>>9.99")) +
                                             " Foi CANCELADA em: " + TRIM(STRING(TODAY,"99/99/9999")),
                                       INPUT NO).

        DELETE tt-ped-parcela.
        {&OPEN-QUERY-br-parcelas}
        APPLY 'value-changed' TO br-parcelas.

     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-imprime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-imprime w-livre
ON CHOOSE OF bt-imprime IN FRAME f-cad
DO:
  CREATE tt-param.
  ASSIGN tt-param.usuario          = c-seg-usuario
         tt-param.data-exec        = TODAY
         tt-param.hora-exec        = TIME
         tt-param.destino          = 3
         tt-param.classifica       = 1
         tt-param.arquivo          = SESSION:TEMP-DIRECTORY + c-programa-mg97 + ".tmp"
         tt-param.estabel-ini      = tt-ped-venda.cod-estab
         tt-param.estabel-fin      = tt-ped-venda.cod-estab
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


&Scoped-define SELF-NAME bt-imprime-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-imprime-3 w-livre
ON CHOOSE OF bt-imprime-3 IN FRAME f-cad
DO:
  RUN pi-imprime.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-log
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-log w-livre
ON CHOOSE OF bt-log IN FRAME f-cad
DO:
   RUN esp/essp0155b.p (INPUT tt-ped-venda.nr-pedcli).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-marca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-marca w-livre
ON CHOOSE OF bt-marca IN FRAME f-cad
/*DO:
  IF tt-ped-venda.cod-sit-ped < 3 AND tt-ped-venda.vl-reservado > 0 THEN DO:
     ASSIGN  tt-ped-venda.dt-solic = TODAY
             tt-ped-venda.usuario  = c-seg-usuario
             tt-ped-venda.marca    = "*".
     APPLY 'value-changed' TO br-pedidos.
     br-pedidos:REFRESH().
     RETURN NO-APPLY.
   END.
END.*/


    
        
            
                
DO:
     IF tt-ped-venda.cod-sit-ped < 3 THEN DO:                    
     ASSIGN  tt-ped-venda.dt-solic = TODAY
             tt-ped-venda.usuario  = c-seg-usuario
             tt-ped-venda.marca    = "*".

     APPLY 'value-changed' TO br-pedidos.
     br-pedidos:REFRESH().
     RETURN NO-APPLY.
  END.                                                     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-modifica
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-modifica w-livre
ON CHOOSE OF bt-modifica IN FRAME f-cad
DO:
   FIND ped-venda WHERE
        ped-venda.nr-pedcli = tt-ped-venda.nr-pedcli AND
        ped-venda.nome-abrev = tt-ped-venda.nome-abrev NO-LOCK NO-ERROR.
   ASSIGN gr-ped-venda = ROWID(ped-venda).

   ASSIGN w-livre:SENSITIVE = NO.
   RUN esp\espd4000.w (INPUT "Modificar").
   ASSIGN w-livre:SENSITIVE = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-nenhum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-nenhum w-livre
ON CHOOSE OF bt-nenhum IN FRAME f-cad
DO:
    FOR EACH tt-ped-venda WHERE                                            
             tt-ped-venda.no-ab-reppri = tt-repres.nome-abrev  AND         
             tt-ped-venda.marca        = "*" NO-LOCK.
        ASSIGN tt-ped-venda.dt-solic     = ?   
               tt-ped-venda.usuario      = ""
               tt-ped-venda.marca        = "".
    END.
    br-pedidos:REFRESH().
    APPLY 'value-changed' TO br-pedidos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-livre
ON CHOOSE OF bt-ok IN FRAME f-cad /* OK */
DO:
   FIND FIRST b-tt-ped-venda WHERE
              b-tt-ped-venda.marca = "*" NO-LOCK NO-ERROR.
   IF NOT AVAIL b-tt-ped-venda THEN DO:
        MESSAGE "Favor marcar pedido(s) para solicitaá∆o de valores ! ! !"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.   
      RETURN NO-APPLY.
   END.

   FOR EACH b-tt-ped-venda WHERE                                            /*  daf   */
            b-tt-ped-venda.qt-pedida <> b-tt-ped-venda.qt-reservada.
         IF b-tt-ped-venda.marca = "*" THEN DO: 
         MESSAGE "A quantidade pedida n∆o corresponde com a quantidade reservada ! ! !" + CHR(13) + CHR(13) +
                 "Deseja continuar a solicitaá∆o de valores?"
           VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                    TITLE "" UPDATE choice AS LOGICAL.
         CASE choice:
         WHEN TRUE THEN /* YES */
          NEXT.         
         WHEN FALSE THEN /* NO */
          RETURN NO-APPLY.
         END CASE.
         END.
   END.                                                                             /*  daf  */

   FOR EACH b-tt-ped-venda WHERE                                            
            b-tt-ped-venda.marca = "*"
            NO-LOCK
       BREAK  BY b-tt-ped-venda.no-ab-reppri                                     
              BY b-tt-ped-venda.nr-pedcli.                                       

       /* Grava Novas Sequencias 'Ped-Parcela' */
       RUN esapi/cv-hora.p (INPUT STRING(TIME,"hh:mm:ss"), OUTPUT i-hr-solic).
       
   END.

   /* Chama Programa Impress∆o e Envio de E-mails */
   RUN esp/essp0183b.p (INPUT TABLE tt-ped-venda).

   /* Limpa do Browse Pedidos Marcados que Foram Impressos ou Enviado E-mails */
   
   
   
   

   FOR EACH b-tt-ped-venda WHERE       
            b-tt-ped-venda.marca = "*". 
       ASSIGN b-tt-ped-venda.marca = "".

       FIND FIRST ped-parcela WHERE
                  ped-parcela.cod-estabel = b-tt-ped-venda.cod-estabel AND
                  ped-parcela.nr-pedido = b-tt-ped-venda.nr-pedido NO-LOCK NO-ERROR.
       IF AVAIL ped-parcela THEN
          ASSIGN b-tt-ped-venda.verifica-envio = YES.

   END.  
   

   APPLY 'value-changed' TO rs-solic-pag IN FRAME {&FRAME-NAME}.

 END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-param
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-param w-livre
ON CHOOSE OF bt-param IN FRAME f-cad /* Button 3 */
DO:
    EMPTY TEMP-TABLE tt-digita.
    ASSIGN rs-solic-pag:SCREEN-VALUE = '3'.
    ASSIGN w-livre:SENSITIVE = NO.

    RUN esp/essp0183a.w (INPUT-OUTPUT c-cod-estabel,
                         INPUT-OUTPUT TABLE tt-digita,
                         INPUT-OUTPUT c-dt-limite-ini,   
                         INPUT-OUTPUT c-dt-limite-fin,
                         INPUT-OUTPUT c-nr-pedcli-ini,   
                         INPUT-OUTPUT c-nr-pedcli-fin,   
                         INPUT-OUTPUT c-it-codigo-ini,   
                         INPUT-OUTPUT c-it-codigo-fin,   
                         INPUT-OUTPUT c-cod-refer-ini,   
                         INPUT-OUTPUT c-cod-refer-fin,   
                         INPUT-OUTPUT c-nome-abrev-ini,
                         INPUT-OUTPUT c-nome-abrev-fin,
                         INPUT-OUTPUT c-no-ab-reppri-ini,
                         INPUT-OUTPUT c-no-ab-reppri-fin,
                         INPUT-OUTPUT c-cod-obsoleto-ini,
                         INPUT-OUTPUT c-cod-obsoleto-fin,
                         INPUT-OUTPUT c-corte-comerc-ini,
                         INPUT-OUTPUT c-corte-comerc-fin,
                         INPUT-OUTPUT c-cod-depos,       
                         INPUT-OUTPUT l-lote-todos,                 
                         INPUT-OUTPUT l-lote-pp,                    
                         INPUT-OUTPUT l-lote-pd,                    
                         INPUT-OUTPUT l-lote-rp,                    
                         INPUT-OUTPUT l-lote-rd,
                         INPUT-OUTPUT l-lote-sc,
                         INPUT-OUTPUT l-lote-ca,
                         INPUT-OUTPUT c-opc-artigo, 
                         INPUT-OUTPUT c-bloqueio,
                         INPUT-OUTPUT l-sit-todas,       
                         INPUT-OUTPUT l-sit-abe,         
                         INPUT-OUTPUT l-sit-atp,         
                         INPUT-OUTPUT l-sit-att,         
                         INPUT-OUTPUT l-sit-pen,         
                         INPUT-OUTPUT l-sit-sus,         
                         INPUT-OUTPUT l-sit-can, 
                         INPUT-OUTPUT i-credito,
                         INPUT-OUTPUT l-ok). 
    IF l-ok THEN                                     
       RUN pi-processa.

  ASSIGN w-livre:SENSITIVE = YES.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-todos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-todos w-livre
ON CHOOSE OF bt-todos IN FRAME f-cad
DO:
   FOR EACH tt-ped-venda WHERE                                            
            tt-ped-venda.no-ab-reppri = tt-repres.nome-abrev  AND         
            tt-ped-venda.visualiza = YES NO-LOCK.     
       
       IF tt-ped-venda.cod-sit-ped < 3 AND 
          tt-ped-venda.verifica-envio = NO THEN DO:
          ASSIGN  tt-ped-venda.dt-solic = TODAY
                  tt-ped-venda.usuario  = c-seg-usuario
                  tt-ped-venda.marca    = "*".
       END.
   END.
   br-pedidos:REFRESH().
   APPLY 'value-changed' TO br-pedidos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-vapara
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-vapara w-livre
ON CHOOSE OF bt-vapara IN FRAME f-cad
DO:
   RUN esdlg/d01essp0155.w (OUTPUT c-nr-pedcli).

   IF c-nr-pedcli <> "" THEN DO:
      FIND FIRST tt-ped-venda WHERE
                 tt-ped-venda.nr-pedcli = c-nr-pedcli AND 
                 tt-ped-venda.visualiza = YES NO-LOCK NO-ERROR.

      IF NOT AVAIL tt-ped-venda THEN DO.
         MESSAGE "Pedido n∆o est† contido na seleá∆o!"
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
         RETURN NO-APPLY.
      END.

      FIND tt-repres WHERE
           tt-repres.nome-abrev = tt-ped-venda.no-ab-reppri NO-LOCK NO-ERROR.
      h-repres:REPOSITION-TO-ROWID(ROWID(tt-repres)) NO-ERROR. 
      APPLY 'VALUE-CHANGED' TO br-repres.

      FIND FIRST tt-ped-venda WHERE
                 tt-ped-venda.nr-pedcli = c-nr-pedcli AND 
                 tt-ped-venda.visualiza = YES NO-LOCK NO-ERROR.
      h-query:REPOSITION-TO-ROWID(ROWID(tt-ped-venda)) NO-ERROR. 
      APPLY 'VALUE-CHANGED' TO br-pedidos.
   END.
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


&Scoped-define SELF-NAME rs-solic-pag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-solic-pag w-livre
ON VALUE-CHANGED OF rs-solic-pag IN FRAME f-cad
DO:
  ASSIGN INPUT FRAME {&FRAME-NAME} rs-solic-pag.
  
  CASE rs-solic-pag.
      WHEN 1 THEN DO.
          FOR EACH tt-repres.
              ASSIGN tt-repres.visualiza = NO.
              FOR EACH tt-ped-venda WHERE 
                       tt-ped-venda.no-ab-reppri = tt-repres.nome-abrev.
                  ASSIGN tt-ped-venda.visualiza = NO.
                  IF tt-ped-venda.verifica-envio = YES THEN
                     ASSIGN tt-repres.visualiza    = YES
                            tt-ped-venda.visualiza = YES.
              END.
          END.
      END.
      WHEN 2 THEN DO.
          FOR EACH tt-repres.
              ASSIGN tt-repres.visualiza = NO.
              FOR EACH tt-ped-venda WHERE 
                       tt-ped-venda.no-ab-reppri = tt-repres.nome-abrev.
                  ASSIGN tt-ped-venda.visualiza = NO.
                  IF tt-ped-venda.verifica-envio = NO THEN
                     ASSIGN tt-repres.visualiza    = YES
                            tt-ped-venda.visualiza = YES.
              END.
          END.
      END.
      WHEN 3 THEN DO.
          FOR EACH tt-repres. 
              ASSIGN tt-repres.visualiza = YES.
              FOR EACH tt-ped-venda WHERE 
                       tt-ped-venda.no-ab-reppri = tt-repres.nome-abrev.
                  ASSIGN tt-ped-venda.visualiza = YES.
              END.
          END.
      END.
  END CASE.

  {&OPEN-QUERY-br-repres} 

  APPLY 'value-changed' TO br-repres IN FRAME {&FRAME-NAME}.
  APPLY 'entry'         TO br-repres IN FRAME {&FRAME-NAME}.
  APPLY 'value-changed' TO br-pedidos IN FRAME {&FRAME-NAME}.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-parcelas
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-livre 


/* ***************************  Main Block  *************************** */
ASSIGN h-query  = br-pedidos:QUERY
       h-repres = br-repres:QUERY.

ASSIGN c-dt-limite-ini = '010001'
       c-dt-limite-fin = STRING(MONTH(TODAY),"99") + STRING(YEAR(TODAY),"9999").
/*
FIND usuar_grp_usuar WHERE 
     usuar_grp_usuar.cod_usuar     = c-seg-usuario AND
     usuar_grp_usuar.cod_grp_usuar = "VD0" NO-LOCK NO-ERROR.
ASSIGN l-gestao = AVAIL usuar_grp_usuar.
*/

FIND im-param WHERE
     im-param.cod-param = "GESTAO_PEDIDO_AVISTA" NO-LOCK NO-ERROR.
IF LOOKUP(c-seg-usuario,im-param.val-param) > 0 THEN
   ASSIGN l-gestao = YES.

/*
IF c-seg-usuario = 'super' OR
   c-seg-usuario = 'sfreitas' OR
   c-seg-usuario = 'rparreiras' OR
   c-seg-usuario = 'gdamasceno' THEN
   ASSIGN l-gestao = YES.
*/

RUN esapi/busca-estab-usuar.p (INPUT c-seg-usuario,
                               OUTPUT c-cod-estabel).
/* IF c-cod-estabel = '' OR c-cod-estabel = '0' THEN 
   ASSIGN c-cod-estabel = ''.  */

  FIND FIRST ped-venda NO-LOCK NO-ERROR.
  ASSIGN c-cod-estabel = ped-venda.cod-estabel.



/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

STATUS INPUT OFF. /* Desliga Mensagem no RodapÇ da Tela */

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
       RUN set-position IN h_p-exihel ( 1.13 , 125.43 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             rs-solic-pag:HANDLE IN FRAME f-cad , 'AFTER':U ).
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
  DISPLAY rs-solic-pag fi-qt-pedida fi-qt-reservada fi-vl-faturar fi-desconto 
          fi-vl-total ed-obs-pedido fi-qt-parcela fi-vl-parcela 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE RECT-10 rt-button rs-solic-pag bt-param br-repres br-pedidos 
         bt-desmarca bt-marca bt-todos bt-nenhum bt-vapara bt-imprime-3 
         bt-consulta bt-imprime br-parcelas ed-obs-pedido bt-modifica bt-ok 
         bt-log 
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

  {utp/ut9000.i "ESSP0183" "2.04.00.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  run pi-after-initialize.

  APPLY 'choose' TO bt-param.

  APPLY 'entry' TO br-repres.
  RETURN NO-APPLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-carrega-dados w-livre 
PROCEDURE pi-carrega-dados :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 {include/i-rprun.i esrp/espd0002arp.p}    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imp-cabec w-livre 
PROCEDURE pi-imp-cabec :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 PUT c-empresa  FORMAT "X(40)"                 AT   1
     "DATA: "                                  AT  47
     STRING(TODAY,"99/99/9999") FORMAT "X(10)" AT  53
     "HORA: "                                  AT  79
     STRING(TIME,"hh:mm:ss")                   AT  85
     "PAG:"                                    AT 114
     i-pag FORMAT ">>>"                        AT 119
     SKIP(1).

 PUT "RELATORIO DE SOLICITAÄ«O DE VALORES A VISTA " AT 38 SKIP(1). 
 FIND repres WHERE
      repres.nome-abrev = tt-ped-venda.no-ab-reppri NO-LOCK NO-ERROR.
 IF AVAIL repres THEN DO:
    PUT "Representante: "    AT 1.
    PUT  repres.nome FORMAT "X(40)" AT 16 SKIP(1).
 END.


 PUT "Pedido Cliente      Ped.Repres.  Dt.Implant Data Solic Solicitante  Item   Refer.  Seq Metros a Faturar Valores a Faturar" AT 1.   
 PUT "------ ------------ ------------ ---------- ---------- ------------ ------ ------- --- ---------------- -----------------" AT 1.
 ASSIGN i-pag = i-pag + 1.                                                                              

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imprime w-livre 
PROCEDURE pi-imprime :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR h-prog AS HANDLE NO-UNDO.
DEF VAR i-ct   AS INT.

  RUN utp/ut-utils.p PERSISTENT SET h-prog.

  
  RUN esapi/saida-imp.p (OUTPUT i-saida,
                         OUTPUT c-saida,
                         OUTPUT i-num-copias,
                         OUTPUT l-ok).

  CASE i-saida:
      WHEN 1 THEN DO.
          OUTPUT TO PRINTER CONVERT TARGET "ibm850" PAGED PAGE-SIZE 61.
          PUT CONTROL "~033E~033(s18H".    
      END.
      WHEN 2 THEN
          OUTPUT TO VALUE(c-saida).
      WHEN 3 THEN DO.
          ASSIGN c-saida = SESSION:TEMP-DIRECTORY + "essp0183.tmp".
          OUTPUT TO VALUE(c-saida) CONVERT SOURCE "ibm850".
      END.
  END CASE.



  DO i-ct = 1 TO i-num-copias.
     ASSIGN i-lin = 99
            i-pag =  1.

     FOR EACH tt-repres WHERE 
              tt-repres.visualiza = YES NO-LOCK. 

   /*  FOR EACH tt-ped-venda WHERE                                            
              tt-ped-venda.imprime = "*" NO-LOCK,                             */
       FOR EACH tt-ped-venda WHERE
                  tt-ped-venda.no-ab-reppri = tt-repres.nome-abrev NO-LOCK
   /*    FOR EACH ped-venda-ext WHERE                                           
              ped-venda-ext.nr-pedido = tt-ped-venda.nr-pedido NO-LOCK      */
         BREAK  BY tt-ped-venda.no-ab-reppri                                     
                BY tt-ped-venda.nr-pedcli.                                       

         IF i-lin > 61 THEN DO:
            RUN pi-imp-cabec.
            ASSIGN i-lin = 7.
         END.
  
         PUT tt-ped-venda.nr-pedcli    FORMAT "x(6)"           AT  1
             tt-ped-venda.nome-abrev   FORMAT "x(12)"          AT  8
             tt-ped-venda.nr-pedrep    FORMAT "x(12)"          AT 21
             tt-ped-venda.dt-implant   FORMAT "99/99/9999"     AT 34
             ped-venda-ext.dt-solic    FORMAT "99/99/9999"     AT 45
             ped-venda-ext.usuario     FORMAT "x(12)"          AT 56.
         ASSIGN i-lin = i-lin + 1.


         PUT SKIP.
         ASSIGN i-lin = i-lin + 1.
         PUT SKIP.                                /*  daf   */
         ASSIGN i-lin = i-lin + 1.                /*  daf   */

         ASSIGN de-tot-qtd = 0
                de-vl-desconto = 0
                de-tot-vlr = 0.
         FOR EACH ped-item WHERE
                  ped-item.nr-pedcli  = tt-ped-venda.nr-pedcli AND
                  ped-item.nome-abrev = tt-ped-venda.nome-abrev AND 
                  ped-item.cod-sit-item = 1 NO-LOCK
               BY ped-item.it-codigo
               BY ped-item.nr-sequencia
               BY ped-item.cod-refer.

             RUN pi-ver-digita (INPUT "Item",
                                INPUT ped-item.it-codigo).
             IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

             RUN pi-ver-digita (INPUT "Referància",
                                INPUT ped-item.cod-refer).
             IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

 
    /*       IF ped-item-ext.corte-comerc < c-corte-comerc-ini OR  
                ped-item-ext.corte-comerc > c-corte-comerc-fin THEN NEXT.       

             RUN pi-ver-digita (INPUT "Corte_Comercial",
                                INPUT ped-item-ext.corte-comerc).
             IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.                              */
   
             FIND item WHERE
                  item.it-codigo = ped-item.it-codigo NO-LOCK NO-ERROR.

             FIND item-ext WHERE
                  item-ext.it-codigo = item.it-codigo NO-LOCK NO-ERROR.

   
 /*            FIND ped-item-res OF ped-item WHERE
                  ped-item-res.faturado = NO NO-LOCK NO-ERROR.
             IF NOT AVAIL ped-item-res THEN  NEXT.                     */

             /* Despreza Sequencias (nr-sequencia) que ja foram solicitadas Anteriormente 
                e que est∆o em Aberto ou Atendido Parcial */
/*              FIND tt-work WHERE                                       */
/*                   tt-work.nr-pedido      = tt-ped-venda.nr-pedido AND */
/*                   tt-work.nr-sequencia   = ped-item.nr-sequencia  AND */
/*                   tt-work.nova-sequencia = NO NO-LOCK NO-ERROR.       */
/*              IF AVAIL tt-work THEN NEXT.                              */

            /* IF de-tot-qtd + de-tot-vlr = 0 THEN
                RUN pi-cabec-item.  */

             PUT ped-item.it-codigo     FORMAT "x(6)"                              AT  07
                 item.desc-item         FORMAT "x(34)"                             AT  14
                 ITEM.un                FORMAT "x(3)"                              AT  50
                 ped-item.cod-refer                                                AT  54
                 ped-item.nr-sequencia  FORMAT ">>9"                               AT  65
                 ped-item.qt-pedida     FORMAT ">>>,>>>,>>9.99"                    AT  71
                 ped-item.vl-preuni     FORMAT ">>>,>>>,>>9.99"                    AT  86
                 ped-item.val-desconto-total + (ped-item.qt-pedida * ped-item.vl-preuni) FORMAT ">,>>>,>>>,>>9.99" AT 102.
          
             ASSIGN i-lin = i-lin + 1.

             ASSIGN de-tot-qtd = de-tot-qtd + ped-item.qt-pedida
                    de-tot-vlr = de-tot-vlr + ped-item.val-desconto-total + (ped-item.qt-pedida * ped-item.vl-preuni).


         END.
         PUT SKIP(2).
         ASSIGN i-lin = i-lin + 2.
       

         PUT "Total do Pedido......................: " AT  30.
         PUT de-tot-qtd  FORMAT ">>>,>>>,>>9.99"       AT  71
             de-tot-vlr  FORMAT ">,>>>,>>>,>>9.99"     AT 102.
         PUT SKIP(2).
         ASSIGN i-lin = i-lin + 3.

         ASSIGN de-tot-repres = tt-ped-venda.vl-tot-ped.
   /*    ACCUMULATE tt-ped-venda.vl-tot-ped (TOTAL BY tt-ped-venda.no-ab-reppri).   */  
         ACCUMULATE de-tot-repres (TOTAL BY tt-ped-venda.no-ab-reppri).               


         IF LAST-OF(tt-ped-venda.no-ab-reppri) THEN DO:
            PUT SKIP(2).
            PUT "TOTAL REPRESENTANTE .................: " AT 30.
      /*    PUT ACCUM TOTAL BY tt-ped-venda.no-ab-reppri tt-ped-venda.vl-tot-ped FORMAT ">,>>>,>>>,>>9.99" AT 102.    */
            PUT ACCUM TOTAL BY tt-ped-venda.no-ab-reppri de-tot-repres FORMAT ">,>>>,>>>,>>9.99" AT 102.                       
            
            PAGE.
            ASSIGN i-lin = 99.
         END.
       END.
   
     IF i-saida = 1 THEN DO:
        PAGE.
        PUT "" AT 1.
     END.

  END.
  

  FIND LAST tt-repres WHERE 
            tt-repres.visualiza = YES NO-LOCK.    
         PUT SKIP(3).
         PUT "VALOR TOTAL DE PEDIDOS A VISTA ........: " AT 30.
     PUT de-tot-ger TOTAL FORMAT ">,>>>,>>>,>>9.99" AT 102.   
     PAGE.


  OUTPUT CLOSE.
  IF i-saida = 3 THEN DO.
     RUN EXECUTE IN h-prog(INPUT "notepad.exe",
                           INPUT c-saida).
     DELETE PROCEDURE h-prog.
  END.
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
   {utp/ut-liter.i Selecionando_Pedidos *}
   RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

   EMPTY TEMP-TABLE tt-ped-venda.
   EMPTY TEMP-TABLE tt-repres.
   EMPTY TEMP-TABLE tt-ped-parcela.


   /* Busca Nome da Empresa */
   FIND FIRST param-global NO-LOCK NO-ERROR.
   FIND FIRST empresa WHERE
              empresa.ep-codigo = param-global.empresa-prin NO-LOCK NO-ERROR. 
   ASSIGN c-empresa = (IF AVAIL empresa THEN empresa.razao-social ELSE "").

   ASSIGN bt-ok:SENSITIVE IN FRAME {&FRAME-NAME}       = YES
          bt-desmarca:SENSITIVE IN FRAME {&FRAME-NAME} = YES
          bt-marca:SENSITIVE IN FRAME {&FRAME-NAME}    = YES
          bt-todos:SENSITIVE IN FRAME {&FRAME-NAME}    = YES
          bt-nenhum:SENSITIVE IN FRAME {&FRAME-NAME}   = YES.


   IF NOT l-gestao THEN DO:
      ASSIGN bt-ok:SENSITIVE IN FRAME {&FRAME-NAME}       = NO
             bt-desmarca:SENSITIVE IN FRAME {&FRAME-NAME} = NO
             bt-marca:SENSITIVE IN FRAME {&FRAME-NAME}    = NO
             bt-todos:SENSITIVE IN FRAME {&FRAME-NAME}    = NO
             bt-nenhum:SENSITIVE IN FRAME {&FRAME-NAME}   = NO.
   END.
   ELSE DO:
      IF i-credito <> 2 THEN 
         ASSIGN bt-ok:SENSITIVE IN FRAME {&FRAME-NAME}       = NO
                bt-desmarca:SENSITIVE IN FRAME {&FRAME-NAME} = NO
                bt-marca:SENSITIVE IN FRAME {&FRAME-NAME}    = NO
                bt-todos:SENSITIVE IN FRAME {&FRAME-NAME}    = NO
                bt-nenhum:SENSITIVE IN FRAME {&FRAME-NAME}   = NO.
   END.

   IF l-lote-todos = YES THEN
      ASSIGN c-lotes = "pp,pd,rp,rd,sc,ca,".
   ELSE DO:
      ASSIGN c-lotes = c-lotes + IF l-lote-pp = YES THEN "pp," ELSE ",".
      ASSIGN c-lotes = c-lotes + IF l-lote-pd = YES THEN "pd," ELSE ",".
      ASSIGN c-lotes = c-lotes + IF l-lote-rp = YES THEN "rp," ELSE ",".
      ASSIGN c-lotes = c-lotes + IF l-lote-rd = YES THEN "rd," ELSE ",".
      ASSIGN c-lotes = c-lotes + IF l-lote-sc = YES THEN "sc," ELSE ",".
      ASSIGN c-lotes = c-lotes + IF l-lote-ca = YES THEN "ca," ELSE ",".
   END.

   RUN esapi/ret-udm.p (INPUT c-dt-limite-fin, OUTPUT c-dia).
   ASSIGN da-dt-entrega-ini = DATE('01'  + SUBSTR(c-dt-limite-ini,1,2) + SUBSTR(c-dt-limite-ini,3,4))
          da-dt-entrega-fin = DATE(c-dia + SUBSTR(c-dt-limite-fin,1,2) + SUBSTR(c-dt-limite-fin,3,4)).

   IF l-sit-todas = YES THEN  
      ASSIGN c-situacao = '1,2,3,4,5,6,'.
   ELSE DO:
      IF l-sit-abe = YES THEN ASSIGN c-situacao = '1,'.
      IF l-sit-atp = YES THEN ASSIGN c-situacao = c-situacao + '2,'.
      IF l-sit-att = YES THEN ASSIGN c-situacao = c-situacao + '3,'.
      IF l-sit-pen = YES THEN ASSIGN c-situacao = c-situacao + '4,'.
      IF l-sit-sus = YES THEN ASSIGN c-situacao = c-situacao + '5,'.
      IF l-sit-can = YES THEN ASSIGN c-situacao = c-situacao + '6,'.
   END. 
   OVERLAY(c-situacao,LENGTH(c-situacao),1) = ''.
    
   RUN pi-separa-pedidos.
   RUN pi-finalizar IN h-acomp.

   {&OPEN-QUERY-br-repres} 
   APPLY 'value-changed' TO br-repres IN FRAME {&FRAME-NAME}.

   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-separa-pedidos w-livre 
PROCEDURE pi-separa-pedidos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH ped-venda WHERE
             ped-venda.cod-sit-ped = 1 AND
             ped-venda.cod-estabel   = c-cod-estabel      AND
             ped-venda.nr-pedcli    >= c-nr-pedcli-ini    AND
             ped-venda.nr-pedcli    <= c-nr-pedcli-fin    AND       
             ped-venda.nome-abrev   >= c-nome-abrev-ini   AND
             ped-venda.nome-abrev   <= c-nome-abrev-fin   AND                                                   
             ped-venda.dt-entrega   >= da-dt-entrega-ini  AND                                                   
             ped-venda.dt-entrega   <= da-dt-entrega-fin  NO-LOCK                                                   
             USE-INDEX ch-nr-pedcli.                                                                             
    
        IF ped-venda.cod-sit-com <> 2 THEN NEXT.  /* Frete n∆o Aprovado */
        IF ped-venda.cod-sit-preco <> 2 THEN NEXT.  /* Preco n∆o Aprovado */
    
        IF ped-venda.completo = YES AND
           ped-venda.cod-sit-aval <> 2 AND
           ped-venda.cod-sit-aval <> 3 THEN NEXT.
    
        FIND ped-venda-ext WHERE
             ped-venda-ext.cod-estabel = ped-venda.cod-estabel AND 
             ped-venda-ext.nr-pedido = ped-venda.nr-pedido NO-LOCK NO-ERROR.
       
        IF NOT AVAIL ped-venda-ext THEN NEXT.
    
        IF ped-venda-ext.l-nao-aprovar THEN NEXT.
       
        FIND cond-pagto WHERE
             cond-pagto.cod-cond-pag = ped-venda.cod-cond-pag NO-LOCK NO-ERROR.
        IF AVAIL cond-pagto AND 
           cond-pagto.cod-vencto <> 2 AND
           cond-pagto.cod-vencto <> 3 THEN NEXT. /* A Vista e Antecipado */
    
        IF c-bloqueio <> 'T' THEN 
           IF (ped-venda-ext.l-bloqueio = YES AND c-bloqueio <> "B") OR
              (ped-venda-ext.l-bloqueio = NO  AND c-bloqueio <> "S") THEN NEXT.
    
        FIND repres WHERE
             repres.nome-abrev = ped-venda.no-ab-reppri NO-LOCK NO-ERROR.
        IF NOT AVAIL repres THEN NEXT.
    
        RUN pi-ver-digita (INPUT "Pedido_de_Venda",                                                             
                           INPUT ped-venda.nr-pedcli).                                                          
        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.                                                                
    
        RUN pi-ver-digita (INPUT "Representante",                                                               
                           INPUT ped-venda.no-ab-reppri).                                                       
        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.                                                                
    
        RUN pi-ver-digita (INPUT "Cliente",
                           INPUT ped-venda.nome-abrev).
        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.
    
        RUN pi-acompanhar IN h-acomp (INPUT "Pedido: " + ped-venda.nr-pedcli).
    
        ASSIGN de-qt-pedida    = 0
               de-qt-reservada = 0
               de-vl-aberto = 0
               de-vl-desconto = 0.
    
        FOR EACH ped-item OF ped-venda WHERE
                 ped-item.it-codigo >= c-it-codigo-ini AND
                 ped-item.it-codigo <= c-it-codigo-fin AND
                 ped-item.cod-refer >= c-cod-refer-ini AND 
                 ped-item.cod-refer <= c-cod-refer-fin AND
                 LOOKUP(STRING(ped-item.cod-sit-item),TRIM(c-situacao)) > 0 NO-LOCK,
           FIRST ped-item-ext WHERE
                 ped-item-ext.cod-estabel = ped-venda.cod-estabel AND
                 ped-item-ext.nr-pedcli   = ped-venda.nr-pedcli AND
                 ped-item-ext.nome-abrev  = ped-venda.nome-abrev AND
                 LOOKUP(SUBSTR(ped-item-ext.lote,1,2),c-lotes) <> 0 
                 NO-LOCK. 
    
            RUN pi-ver-digita (INPUT "Item",
                               INPUT ped-item.it-codigo).
            IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.
    
            RUN pi-ver-digita (INPUT "Referància",
                               INPUT ped-item.cod-refer).
            IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.
    
            RUN pi-ver-digita (INPUT "Corte_Comercial",
                               INPUT ped-item-ext.corte-comerc).
            IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.
    
            FIND item WHERE
                 item.it-codigo = ped-item.it-codigo NO-LOCK NO-ERROR.
    
            FIND item-ext WHERE
                 item-ext.it-codigo = item.it-codigo NO-LOCK NO-ERROR.
    
            FIND ped-item-res OF ped-item WHERE
                 ped-item-res.faturado = NO NO-LOCK NO-ERROR.    
    
            ASSIGN de-vl-aberto = de-vl-aberto + (ped-item.qt-pedida * ped-item.vl-preuni)
                   de-vl-desconto = de-vl-desconto + ped-item.val-desconto-total.
    
            /* Implementei a l¢gica abaixo porque em alguns casos, o valor
               unit†rio, vl-preuni n∆o est† abatendo o desconto */
            IF ped-item.val-desconto-total > 0 THEN
               ASSIGN de-vl-aberto = de-vl-aberto - ped-item.val-desconto-total.
    
            IF AVAIL ped-item-res THEN
               ASSIGN de-qt-reservada  = de-qt-reservada + ped-item-res.qt-pedida 
                      de-vl-reservado  = de-vl-reservado + (ped-item-res.qt-pedida * ped-item.vl-preuni).
    
            ASSIGN de-qt-pedida = de-qt-pedida + ped-item.qt-pedida.
        END.
       
        IF de-qt-pedida = 0 THEN NEXT.
    
        IF i-credito = 1 AND  /* Pedidos Solicitados */
           de-vlr-solic = 0 THEN NEXT.
    
        FIND tt-ped-venda WHERE
             tt-ped-venda.cod-estabel = ped-venda.cod-estabel AND
             tt-ped-venda.nr-pedcli   = ped-venda.nr-pedcli AND
             tt-ped-venda.nome-abrev  = ped-venda.nome-abrev NO-LOCK NO-ERROR.
       
        IF NOT AVAIL tt-ped-venda THEN DO:
           CREATE tt-ped-venda.
           BUFFER-COPY ped-venda TO tt-ped-venda.
           ASSIGN tt-ped-venda.visualiza    = YES
                  tt-ped-venda.qt-pedida    = de-qt-pedida
                  tt-ped-venda.qt-aberto    = de-qt-pedida - de-qt-reservada
                  tt-ped-venda.vl-aberto    = de-vl-aberto
                  tt-ped-venda.vl-reservado = de-vl-reservado 
                  tt-ped-venda.qt-reservada = de-qt-reservada
                  tt-ped-venda.vl-desconto  = de-vl-desconto 
                  tt-ped-venda.vl-tot-ped   = de-vl-aberto + de-vl-desconto.
        END.
    
        FIND pre-fatur OF ped-venda NO-LOCK NO-ERROR.  
        IF AVAIL pre-fatur THEN 
           ASSIGN tt-ped-venda.nr-embarque  = pre-fatur.nr-embarque.
    
        FIND tt-repres WHERE
             tt-repres.nome-abrev = ped-venda.no-ab-reppri NO-LOCK NO-ERROR.
        IF NOT AVAIL tt-repres THEN DO:
           CREATE tt-repres.
           BUFFER-COPY repres TO tt-repres.
           ASSIGN tt-repres.visualiza = YES.
        END.
    
        FIND FIRST ped-parcela WHERE
             ped-parcela.cod-estabel = ped-venda.cod-estabel AND
             ped-parcela.nr-pedido = ped-venda.nr-pedido NO-LOCK NO-ERROR.
        IF AVAIL ped-parcela THEN
           ASSIGN tt-ped-venda.verifica-envio = YES.
    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-tot-parcela w-livre 
PROCEDURE pi-tot-parcela :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
ASSIGN fi-vl-parcela = 0
       fi-qt-parcela = 0.
FOR EACH tt-ped-parcela WHERE                                      
         tt-ped-parcela.cod-estabel = tt-ped-venda.cod-estabel AND
         tt-ped-parcela.nr-pedido = tt-ped-venda.nr-pedido AND 
         tt-ped-parcela.faturado  = NO NO-LOCK.   
    ASSIGN fi-vl-parcela = fi-vl-parcela + tt-ped-parcela.vlr-solic
           fi-qt-parcela = fi-qt-parcela + tt-ped-parcela.qtd-solic.
END.
DISP fi-qt-parcela
     fi-vl-parcela
     WITH FRAME {&FRAME-NAME}.

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
ASSIGN fi-qt-pedida    = 0
       fi-qt-reservada = 0
       fi-vl-faturar   = 0
       fi-desconto     = 0
       fi-vl-total     = 0.

FOR EACH tt-ped-venda WHERE                                      
         tt-ped-venda.no-ab-reppri = tt-repres.nome-abrev  AND   
         tt-ped-venda.visualiza = YES NO-LOCK.
    ASSIGN fi-qt-pedida    = fi-qt-pedida    + tt-ped-venda.qt-pedida
           fi-qt-reservada = fi-qt-reservada + tt-ped-venda.qt-reservada
           fi-vl-faturar   = fi-vl-faturar   + tt-ped-venda.vl-aberto
           fi-desconto     = fi-desconto     + tt-ped-venda.vl-desconto
           fi-vl-total     = fi-vl-total     + tt-ped-venda.vl-tot-ped.
END.
DISP fi-qt-pedida
     fi-qt-reservada
     fi-vl-faturar
     fi-desconto
     fi-vl-total
     WITH FRAME {&FRAME-NAME}.

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
                   tt-digita.opcao = 'D'      AND
                   tt-digita.campo = p-campo) AND
    NOT CAN-FIND(FIRST tt-digita WHERE
                       tt-digita.opcao = 'D'      AND
                       tt-digita.campo = p-campo  AND
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-verfica-envio w-livre 
PROCEDURE pi-verfica-envio :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/





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
  {src/adm/template/snd-list.i "tt-repres"}
  {src/adm/template/snd-list.i "tt-ped-venda"}
  {src/adm/template/snd-list.i "tt-ped-parcela"}

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

