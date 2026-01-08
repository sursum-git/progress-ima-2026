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
{include/i-prgvrs.i B02di134 2.04.00.000}

{utp/utapi011.i} /* Geraá∆o de Graficos */


/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */


DEFINE TEMP-TABLE tt-work  NO-UNDO 
       FIELD dia     AS CHAR FORMAT "x(2)"
       FIELD mes     AS CHAR FORMAT "x(3)"
       FIELD qtd-vda AS DEC
       FIELD vlr-vda AS DEC
       FIELD qtd-can AS DEC
       FIELD vlr-can AS DEC
       FIELD qtd-fat AS DEC
       FIELD vlr-fat AS DEC
       INDEX indice1 dia.

DEFINE TEMP-TABLE tt-lojas  NO-UNDO 
       FIELD loja    AS CHAR FORMAT "x(12)"
       FIELD qtd-vda AS DEC
       FIELD vlr-vda AS DEC
       FIELD qtd-can AS DEC
       FIELD vlr-can AS DEC
       FIELD qtd-fat AS DEC
       FIELD vlr-fat AS DEC
       INDEX indice1 loja.

DEFINE TEMP-TABLE tt-vda  NO-UNDO 
       FIELD tipo         AS CHAR
       FIELD dia          AS CHAR
       FIELD nr-pedcli    LIKE ped-venda.nr-pedcli
       FIELD nr-pedrep    LIKE ped-venda.nr-pedrep
       FIELD nome-abrev   LIKE ped-venda.nome-abrev
       FIELD no-ab-reppri LIKE ped-venda.no-ab-reppri
       FIELD dt-implant   LIKE ped-venda.dt-implant
       FIELD dt-entrega   LIKE ped-venda.dt-entrega
       FIELD Sit          AS CHAR
       FIELD qtde         AS DEC
       FIELD valor        AS DEC
       INDEX indice1 tipo dia nr-pedcli.

DEFINE TEMP-TABLE tt-can  NO-UNDO 
       FIELD tipo         AS CHAR
       FIELD dia          AS CHAR
       FIELD nr-pedcli    LIKE ped-venda.nr-pedcli
       FIELD nr-pedrep    LIKE ped-venda.nr-pedrep
       FIELD nome-abrev   LIKE ped-venda.nome-abrev
       FIELD no-ab-reppri LIKE ped-venda.no-ab-reppri
       FIELD dt-cancela   LIKE ped-item.dt-canseq
       FIELD dt-implant   LIKE ped-venda.dt-entrega
       FIELD Sit          AS CHAR
       FIELD qtde         AS DEC
       FIELD valor        AS DEC
       FIELD motivo       AS CHAR
       INDEX indice1 tipo dia nr-pedcli.

DEFINE TEMP-TABLE tt-fat  NO-UNDO 
       FIELD tipo         AS CHAR
       FIELD dia          AS CHAR
       FIELD nr-nota-fis  LIKE nota-fiscal.nr-nota-fis
       FIELD dt-emis-nota LIKE nota-fiscal.dt-emis-nota
       FIELD dt-saida     LIKE nota-fiscal.dt-saida
       FIELD nome-ab-cli  LIKE nota-fiscal.nome-ab-cli
       FIELD nr-pedcli    LIKE nota-fiscal.nr-pedcli
       FIELD no-ab-reppri LIKE nota-fiscal.no-ab-reppri
       FIELD qtde         AS DEC
       FIELD valor        AS DEC
       INDEX indice1 tipo dia nr-nota-fis.

/* Variaveis do parametro */
DEF VAR c-dt-limite        AS CHAR.
DEF VAR c-cod-estabel-ini  AS CHAR.
DEF VAR c-cod-estabel-fin  AS CHAR.
DEF VAR c-it-codigo-ini    AS CHAR.                              
DEF VAR c-it-codigo-fin    AS CHAR.                              
DEF VAR c-cod-refer-ini    AS CHAR.                              
DEF VAR c-cod-refer-fin    AS CHAR.
DEF VAR c-nome-abrev-ini   AS CHAR.                              
DEF VAR c-nome-abrev-fin   AS CHAR.
DEF VAR c-no-ab-reppri-ini AS CHAR.                              
DEF VAR c-no-ab-reppri-fin AS CHAR.
DEF VAR c-corte-comerc-ini AS CHAR.
DEF VAR c-corte-comerc-fin AS CHAR.
DEF VAR i-tp-relat         AS INT.
DEF VAR c-tp-pedido        AS CHAR.
DEF VAR l-lote-todos       AS LOG.
DEF VAR l-lote-pp          AS LOG.                              
DEF VAR l-lote-pd          AS LOG.
DEF VAR l-lote-rp          AS LOG.
DEF VAR l-lote-rd          AS LOG.
DEF VAR l-lote-sc          AS LOG.
DEF VAR l-lote-ca          AS LOG.
DEF VAR c-tp-merc          AS CHAR.
DEF VAR c-opc-artigo       AS CHAR.

/* Variaveis Usadas Na Geraá∆o da Planilha Excel */
DEFINE VAR i-canal     AS INTEGER.
DEFINE VAR sys         AS INTEGER.
DEFINE VAR c-lin       AS CHARACTER FORMAT "x(500)".
DEFINE VAR aux-command AS CHAR FORMAT "x(100)".
DEFINE VAR arq-saida   AS CHAR FORMAT "x(45)".

/* Variaveis da Rotina de Impress∆o */
DEFINE VAR l-ok                AS LOG.
DEFINE VAR i-saida             AS INT.
DEFINE VAR c-saida             AS CHAR.
DEFINE VAR i-num-copias        AS INT.


/* Variaveis normais do Programa */
{include/i-vrtab.i emitente}
{include/i-vrtab.i docum-est}
DEF NEW GLOBAL SHARED VAR gr-ped-venda   AS ROWID NO-UNDO.
DEF NEW GLOBAL SHARED VAR gr-nota-fiscal AS ROWID NO-UNDO.
DEF NEW GLOBAL SHARED VAR gr-docum-est   AS ROWID NO-UNDO.
DEF NEW GLOBAL SHARED VAR gr-emitente    AS ROWID NO-UNDO.


DEF VAR h-query            AS HANDLE.
DEF VAR h-acomp            AS HANDLE NO-UNDO.
DEF VAR da-dt-implant-ini  AS DATE.
DEF VAR da-dt-implant-fin  AS DATE.
DEF VAR c-dia              AS CHAR.
DEF VAR c-tipo             AS CHAR.
DEF VAR c-data-per         AS CHAR.
DEF VAR c-lotes            AS CHAR.
DEF VAR da-dt-cancel       AS DATE.
DEF VAR c-avalia-credito   AS CHAR.
DEF VAR c-codigo           AS CHAR.
DEF VAR c-motivo           AS CHAR FORMAT "x(70)".
DEF VAR de-total           AS DEC.
DEF VAR de-qtd-conv        AS DEC.
DEF VAR l-achei-item-can   AS LOG.
DEF VAR i-lin              AS INT.
DEF VAR i-pag              AS INT.
DEF VAR l-barra-vendidos   AS LOG.
DEF VAR l-barra-cancelados AS LOG.
DEF VAR l-barra-faturados  AS LOG.
DEF VAR c-empresa          AS CHAR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-work

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-work

/* Definitions for BROWSE br-work                                       */
&Scoped-define FIELDS-IN-QUERY-br-work tt-work.dia tt-work.mes tt-work.qtd-vda tt-work.vlr-vda tt-work.qtd-can tt-work.vlr-can tt-work.qtd-fat tt-work.vlr-fat   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-work   
&Scoped-define SELF-NAME br-work
&Scoped-define OPEN-QUERY-br-work RUN pi-totais. OPEN QUERY {&SELF-NAME} FOR EACH tt-work NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-work tt-work
&Scoped-define FIRST-TABLE-IN-QUERY-br-work tt-work


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-br-work}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-work RECT-60 bt-imprime bt-vendas ~
bt-cancelado bt-faturado bt-vapra bt-excel bt-Grafico-qtd bt-Grafico-vlr ~
bt-vendas-tot bt-cancelado-tot bt-faturado-tot bt-faturado-tot-2 
&Scoped-Define DISPLAYED-OBJECTS fi-qtd-vda fi-vlr-vda fi-qtd-can ~
fi-vlr-can fi-qtd-fat fi-vlr-fat fi-qtd-vda-ljs fi-vlr-vda-ljs ~
fi-qtd-can-ljs fi-vlr-can-ljs fi-qtd-fat-ljs fi-vlr-fat-ljs fi-qtd-vda-ind ~
fi-vlr-vda-ind fi-qtd-can-ind fi-vlr-can-ind fi-qtd-fat-ind fi-vlr-fat-ind ~
fi-qtd-vda-out fi-vlr-vda-out fi-qtd-can-out fi-vlr-can-out fi-qtd-fat-out ~
fi-vlr-fat-out fi-qtd-vda-geral fi-vlr-vda-geral fi-qtd-can-geral ~
fi-vlr-can-geral fi-qtd-fat-geral fi-vlr-fat-geral 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-4 bt-vendas bt-cancelado bt-faturado bt-vendas-tot 
&Scoped-define List-6 bt-imprime bt-vapra bt-excel bt-Grafico-qtd ~
bt-Grafico-vlr bt-vendas-tot bt-cancelado-tot bt-faturado-tot ~
bt-faturado-tot-2 

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
DEFINE BUTTON bt-cancelado 
     IMAGE-UP FILE "image/im-cance.bmp":U
     LABEL "" 
     SIZE 5 BY 1.21 TOOLTIP "Detalhar Cancelamentos do Dia".

DEFINE BUTTON bt-cancelado-tot 
     IMAGE-UP FILE "image/im-canc1.bmp":U
     LABEL "" 
     SIZE 5 BY 1.21 TOOLTIP "Detalhar Cancelamentos Total do Periodo".

DEFINE BUTTON bt-excel 
     IMAGE-UP FILE "image/excel.bmp":U
     LABEL "Button 2" 
     SIZE 5 BY 1.21 TOOLTIP "Gerar Planilha".

DEFINE BUTTON bt-faturado AUTO-GO 
     IMAGE-UP FILE "image/im-raneg.bmp":U
     LABEL "" 
     SIZE 5 BY 1.21 TOOLTIP "Detalhar Faturamento do Dia"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-faturado-tot AUTO-GO 
     IMAGE-UP FILE "image/im-pcust.bmp":U
     LABEL "" 
     SIZE 5 BY 1.21 TOOLTIP "Detalhar Faturamento Total do Periodo"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-faturado-tot-2 AUTO-GO 
     IMAGE-UP FILE "image/gr-tot.bmp":U
     LABEL "" 
     SIZE 5 BY 1.21 TOOLTIP "Detalhar Faturamento Total das Lojas"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-Grafico-qtd 
     IMAGE-UP FILE "image/im-grf.bmp":U
     LABEL "Grafico" 
     SIZE 5 BY 1.21 TOOLTIP "Visualizar o Grafico das Metragens".

DEFINE BUTTON bt-Grafico-vlr 
     IMAGE-UP FILE "image/im-grfcp.bmp":U
     LABEL "Grafico" 
     SIZE 5 BY 1.21 TOOLTIP "Visualizar o Grafico de Valores".

DEFINE BUTTON bt-imprime 
     IMAGE-UP FILE "image/im-pri.bmp":U
     LABEL "&Imprimir" 
     SIZE 5 BY 1.21 TOOLTIP "Relat¢rio"
     BGCOLOR 8 .

DEFINE BUTTON bt-vapra 
     IMAGE-UP FILE "image/im-vapra.bmp":U
     LABEL "" 
     SIZE 5 BY 1.21 TOOLTIP "V† para o Dia"
     BGCOLOR 8 .

DEFINE BUTTON bt-vendas AUTO-GO 
     IMAGE-UP FILE "image/im-ran_a.bmp":U
     LABEL "" 
     SIZE 5 BY 1.21 TOOLTIP "Detalhar Vendas do Dia"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-vendas-tot AUTO-GO 
     IMAGE-UP FILE "image/im-nenhum.bmp":U
     LABEL "" 
     SIZE 5 BY 1.21 TOOLTIP "Detalhar Vendas Total do Periodo"
     BGCOLOR 8 FONT 10.

DEFINE VARIABLE fi-qtd-can AS DECIMAL FORMAT "      -ZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.57 BY .88
     FGCOLOR 6  NO-UNDO.

DEFINE VARIABLE fi-qtd-can-geral AS DECIMAL FORMAT "      -ZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.57 BY .88
     FGCOLOR 6  NO-UNDO.

DEFINE VARIABLE fi-qtd-can-ind AS DECIMAL FORMAT "       -ZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.57 BY .88
     FGCOLOR 6  NO-UNDO.

DEFINE VARIABLE fi-qtd-can-ljs AS DECIMAL FORMAT "       -ZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.57 BY .88
     FGCOLOR 6  NO-UNDO.

DEFINE VARIABLE fi-qtd-can-out AS DECIMAL FORMAT "       -ZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.57 BY .88
     FGCOLOR 6  NO-UNDO.

DEFINE VARIABLE fi-qtd-fat AS DECIMAL FORMAT "   -ZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE fi-qtd-fat-geral AS DECIMAL FORMAT "   -ZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE fi-qtd-fat-ind AS DECIMAL FORMAT "    -ZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE fi-qtd-fat-ljs AS DECIMAL FORMAT "    -ZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE fi-qtd-fat-out AS DECIMAL FORMAT "    -ZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE fi-qtd-vda AS DECIMAL FORMAT "   -ZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12.43 BY .88
     FGCOLOR 4  NO-UNDO.

DEFINE VARIABLE fi-qtd-vda-geral AS DECIMAL FORMAT "   -ZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12.43 BY .88
     FGCOLOR 4  NO-UNDO.

DEFINE VARIABLE fi-qtd-vda-ind AS DECIMAL FORMAT "    -ZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12.43 BY .88
     FGCOLOR 4  NO-UNDO.

DEFINE VARIABLE fi-qtd-vda-ljs AS DECIMAL FORMAT "    -ZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12.43 BY .88
     FGCOLOR 4  NO-UNDO.

DEFINE VARIABLE fi-qtd-vda-out AS DECIMAL FORMAT "    -ZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12.43 BY .88
     FGCOLOR 4  NO-UNDO.

DEFINE VARIABLE fi-vlr-can AS DECIMAL FORMAT "      -ZZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.43 BY .88
     FGCOLOR 6  NO-UNDO.

DEFINE VARIABLE fi-vlr-can-geral AS DECIMAL FORMAT "      -ZZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.43 BY .88
     FGCOLOR 6  NO-UNDO.

DEFINE VARIABLE fi-vlr-can-ind AS DECIMAL FORMAT "       -ZZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.43 BY .88
     FGCOLOR 6  NO-UNDO.

DEFINE VARIABLE fi-vlr-can-ljs AS DECIMAL FORMAT "       -ZZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.43 BY .88
     FGCOLOR 6  NO-UNDO.

DEFINE VARIABLE fi-vlr-can-out AS DECIMAL FORMAT "       -ZZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.43 BY .88
     FGCOLOR 6  NO-UNDO.

DEFINE VARIABLE fi-vlr-fat AS DECIMAL FORMAT "   -ZZZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.29 BY .88
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE fi-vlr-fat-geral AS DECIMAL FORMAT "   -ZZZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.29 BY .88
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE fi-vlr-fat-ind AS DECIMAL FORMAT "   -ZZZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.29 BY .88
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE fi-vlr-fat-ljs AS DECIMAL FORMAT "   -ZZZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.29 BY .88
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE fi-vlr-fat-out AS DECIMAL FORMAT "   -ZZZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.29 BY .88
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE fi-vlr-vda AS DECIMAL FORMAT "      -ZZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.29 BY .88
     FGCOLOR 4  NO-UNDO.

DEFINE VARIABLE fi-vlr-vda-geral AS DECIMAL FORMAT "      -ZZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.29 BY .88
     FGCOLOR 4  NO-UNDO.

DEFINE VARIABLE fi-vlr-vda-ind AS DECIMAL FORMAT "       -ZZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.29 BY .88
     FGCOLOR 4  NO-UNDO.

DEFINE VARIABLE fi-vlr-vda-ljs AS DECIMAL FORMAT "       -ZZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.29 BY .88
     FGCOLOR 4  NO-UNDO.

DEFINE VARIABLE fi-vlr-vda-out AS DECIMAL FORMAT "       -ZZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.29 BY .88
     FGCOLOR 4  NO-UNDO.

DEFINE RECTANGLE RECT-60
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 6.57 BY 15.75.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-work FOR 
      tt-work SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-work
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-work B-table-Win _FREEFORM
  QUERY br-work NO-LOCK DISPLAY
      tt-work.dia      COLUMN-LABEL "Dia" WIDTH 3
      tt-work.mes      COLUMN-LABEL "Mes" WIDTH 4
      tt-work.qtd-vda  COLUMN-LABEL "Metros Vendidos"    FORMAT ">,>>>,>>9.99"   WIDTH 13    COLUMN-FGCOLOR 4
      tt-work.vlr-vda  COLUMN-LABEL "Valores Vendidos"   FORMAT ">>>,>>>,>>9.99" WIDTH 14    COLUMN-FGCOLOR 4
      tt-work.qtd-can  COLUMN-LABEL "Metros Cancelados"  FORMAT ">,>>>,>>9.99"   WIDTH 13.2  COLUMN-FGCOLOR 6
      tt-work.vlr-can  COLUMN-LABEL "Valores Cancelados" FORMAT ">>>,>>>,>>9.99" WIDTH 13.4  COLUMN-FGCOLOR 6
      tt-work.qtd-fat  COLUMN-LABEL "Metros Faturados"   FORMAT ">,>>>,>>9.99"   WIDTH 12    COLUMN-FGCOLOR 9
      tt-work.vlr-fat  COLUMN-LABEL "Valores Faturados"  FORMAT ">>>,>>>,>>9.99" WIDTH 13    COLUMN-FGCOLOR 9
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 90.43 BY 10.46
         FONT 1
         TITLE "" ROW-HEIGHT-CHARS .71.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br-work AT ROW 1.08 COL 2
     bt-imprime AT ROW 1.21 COL 93.72
     bt-vendas AT ROW 2.38 COL 93.72
     bt-cancelado AT ROW 3.58 COL 93.72
     bt-faturado AT ROW 4.79 COL 93.72
     bt-vapra AT ROW 6 COL 93.72
     bt-excel AT ROW 7.21 COL 93.72
     bt-Grafico-qtd AT ROW 8.42 COL 93.72
     bt-Grafico-vlr AT ROW 9.63 COL 93.72
     bt-vendas-tot AT ROW 10.88 COL 93.72
     fi-qtd-vda AT ROW 11.75 COL 5.29 COLON-ALIGNED NO-LABEL
     fi-vlr-vda AT ROW 11.75 COL 17.72 COLON-ALIGNED NO-LABEL
     fi-qtd-can AT ROW 11.75 COL 32.14 COLON-ALIGNED NO-LABEL
     fi-vlr-can AT ROW 11.75 COL 45.86 COLON-ALIGNED NO-LABEL
     fi-qtd-fat AT ROW 11.75 COL 60.43 COLON-ALIGNED NO-LABEL
     fi-vlr-fat AT ROW 11.75 COL 73.43 COLON-ALIGNED NO-LABEL
     bt-cancelado-tot AT ROW 12.08 COL 93.72
     fi-qtd-vda-ljs AT ROW 12.75 COL 5.29 COLON-ALIGNED NO-LABEL
     fi-vlr-vda-ljs AT ROW 12.75 COL 17.72 COLON-ALIGNED NO-LABEL
     fi-qtd-can-ljs AT ROW 12.75 COL 32.14 COLON-ALIGNED NO-LABEL
     fi-vlr-can-ljs AT ROW 12.75 COL 45.86 COLON-ALIGNED NO-LABEL
     fi-qtd-fat-ljs AT ROW 12.75 COL 60.43 COLON-ALIGNED NO-LABEL
     fi-vlr-fat-ljs AT ROW 12.75 COL 73.43 COLON-ALIGNED NO-LABEL
     bt-faturado-tot AT ROW 13.25 COL 93.72
     fi-qtd-vda-ind AT ROW 13.75 COL 5.29 COLON-ALIGNED NO-LABEL
     fi-vlr-vda-ind AT ROW 13.75 COL 17.72 COLON-ALIGNED NO-LABEL
     fi-qtd-can-ind AT ROW 13.75 COL 32.14 COLON-ALIGNED NO-LABEL
     fi-vlr-can-ind AT ROW 13.75 COL 45.86 COLON-ALIGNED NO-LABEL
     fi-qtd-fat-ind AT ROW 13.75 COL 60.43 COLON-ALIGNED NO-LABEL
     fi-vlr-fat-ind AT ROW 13.75 COL 73.43 COLON-ALIGNED NO-LABEL
     bt-faturado-tot-2 AT ROW 14.46 COL 93.72
     fi-qtd-vda-out AT ROW 14.75 COL 5.29 COLON-ALIGNED NO-LABEL
     fi-vlr-vda-out AT ROW 14.75 COL 17.72 COLON-ALIGNED NO-LABEL
     fi-qtd-can-out AT ROW 14.75 COL 32.14 COLON-ALIGNED NO-LABEL
     fi-vlr-can-out AT ROW 14.75 COL 45.86 COLON-ALIGNED NO-LABEL
     fi-qtd-fat-out AT ROW 14.75 COL 60.43 COLON-ALIGNED NO-LABEL
     fi-vlr-fat-out AT ROW 14.75 COL 73.43 COLON-ALIGNED NO-LABEL
     fi-qtd-vda-geral AT ROW 15.75 COL 5.29 COLON-ALIGNED NO-LABEL
     fi-vlr-vda-geral AT ROW 15.75 COL 17.72 COLON-ALIGNED NO-LABEL
     fi-qtd-can-geral AT ROW 15.75 COL 32.14 COLON-ALIGNED NO-LABEL
     fi-vlr-can-geral AT ROW 15.75 COL 45.86 COLON-ALIGNED NO-LABEL
     fi-qtd-fat-geral AT ROW 15.75 COL 60.43 COLON-ALIGNED NO-LABEL
     fi-vlr-fat-geral AT ROW 15.75 COL 73.43 COLON-ALIGNED NO-LABEL
     "Outros" VIEW-AS TEXT
          SIZE 4.57 BY .54 AT ROW 14.96 COL 2.43
     "Industria" VIEW-AS TEXT
          SIZE 5.72 BY .54 AT ROW 14.08 COL 1.14
     "Normal" VIEW-AS TEXT
          SIZE 4.86 BY .54 AT ROW 11.96 COL 2.29
     "Geral" VIEW-AS TEXT
          SIZE 4.14 BY .54 AT ROW 15.79 COL 2.72
     "Lojas" VIEW-AS TEXT
          SIZE 4.14 BY .54 AT ROW 13.04 COL 2.72
     RECT-60 AT ROW 1 COL 92.86
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
         HEIGHT             = 15.92
         WIDTH              = 98.86.
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
/* BROWSE-TAB br-work TEXT-2 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON bt-cancelado IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR BUTTON bt-cancelado-tot IN FRAME F-Main
   6                                                                    */
/* SETTINGS FOR BUTTON bt-excel IN FRAME F-Main
   6                                                                    */
/* SETTINGS FOR BUTTON bt-faturado IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR BUTTON bt-faturado-tot IN FRAME F-Main
   6                                                                    */
/* SETTINGS FOR BUTTON bt-faturado-tot-2 IN FRAME F-Main
   6                                                                    */
/* SETTINGS FOR BUTTON bt-Grafico-qtd IN FRAME F-Main
   6                                                                    */
/* SETTINGS FOR BUTTON bt-Grafico-vlr IN FRAME F-Main
   6                                                                    */
/* SETTINGS FOR BUTTON bt-imprime IN FRAME F-Main
   6                                                                    */
/* SETTINGS FOR BUTTON bt-vapra IN FRAME F-Main
   6                                                                    */
/* SETTINGS FOR BUTTON bt-vendas IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR BUTTON bt-vendas-tot IN FRAME F-Main
   4 6                                                                  */
/* SETTINGS FOR FILL-IN fi-qtd-can IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qtd-can-geral IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qtd-can-ind IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qtd-can-ljs IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qtd-can-out IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qtd-fat IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qtd-fat-geral IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qtd-fat-ind IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qtd-fat-ljs IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qtd-fat-out IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qtd-vda IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qtd-vda-geral IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qtd-vda-ind IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qtd-vda-ljs IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qtd-vda-out IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-vlr-can IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-vlr-can-geral IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-vlr-can-ind IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-vlr-can-ljs IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-vlr-can-out IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-vlr-fat IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-vlr-fat-geral IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-vlr-fat-ind IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-vlr-fat-ljs IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-vlr-fat-out IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-vlr-vda IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-vlr-vda-geral IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-vlr-vda-ind IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-vlr-vda-ljs IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-vlr-vda-out IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-work
/* Query rebuild information for BROWSE br-work
     _START_FREEFORM
RUN pi-totais.
OPEN QUERY {&SELF-NAME} FOR EACH tt-work NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-work */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br-work
&Scoped-define SELF-NAME br-work
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-work B-table-Win
ON VALUE-CHANGED OF br-work IN FRAME F-Main
DO:
  IF AVAIL tt-work THEN DO:
     DISABLE {&list-4} WITH FRAME {&FRAME-NAME}.
     ENABLE {&list-6} WITH FRAME {&FRAME-NAME}.
     IF tt-work.qtd-vda <> 0 OR tt-work.vlr-vda <> 0 THEN
        bt-vendas:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

     IF tt-work.qtd-can <> 0 OR tt-work.vlr-can <> 0 THEN
        bt-cancelado:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

     IF tt-work.qtd-fat <> 0 OR tt-work.vlr-fat <> 0 THEN
        bt-faturado:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cancelado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancelado B-table-Win
ON CHOOSE OF bt-cancelado IN FRAME F-Main
DO:
    RUN esp/essp0174b.p (INPUT TABLE tt-can,
                         INPUT tt-work.dia,
                         INPUT c-dt-limite,
                         INPUT i-tp-relat).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cancelado-tot
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancelado-tot B-table-Win
ON CHOOSE OF bt-cancelado-tot IN FRAME F-Main
DO:
    RUN esp/essp0174e.p (INPUT TABLE tt-can,
                         INPUT c-dt-limite,
                         INPUT i-tp-relat).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-excel B-table-Win
ON CHOOSE OF bt-excel IN FRAME F-Main /* Button 2 */
DO:
   
   RUN esdlg/d02essp0174.w (OUTPUT arq-saida).
   IF arq-saida <> "" THEN DO:
      RUN pi-gera-excel (INPUT arq-saida).
      MESSAGE "O arquivo foi gerado em " TRIM(arq-saida) + "." SKIP 
              "Para acess†-lo,  abra-o atravÇs do Excel."
          VIEW-AS ALERT-BOX INFO BUTTONS OK. 
   END.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-faturado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-faturado B-table-Win
ON CHOOSE OF bt-faturado IN FRAME F-Main
DO:  
    RUN esp/essp0174c.p (INPUT TABLE tt-fat,
                         INPUT tt-work.dia,
                         INPUT c-dt-limite,
                         INPUT i-tp-relat).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-faturado-tot
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-faturado-tot B-table-Win
ON CHOOSE OF bt-faturado-tot IN FRAME F-Main
DO:  
    RUN esp/essp0174f.p (INPUT TABLE tt-fat,
                         INPUT c-dt-limite,
                         INPUT i-tp-relat).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-faturado-tot-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-faturado-tot-2 B-table-Win
ON CHOOSE OF bt-faturado-tot-2 IN FRAME F-Main
DO:  
    RUN esp/essp0174g.p (INPUT TABLE tt-lojas,
                         INPUT c-dt-limite,
                         INPUT fi-qtd-vda-ind,
                         INPUT fi-vlr-vda-ind,
                         INPUT fi-qtd-can-ind,
                         INPUT fi-vlr-can-ind,
                         INPUT fi-qtd-fat-ind,
                         INPUT fi-vlr-fat-ind,
                         INPUT fi-qtd-vda-out,
                         INPUT fi-vlr-vda-out,
                         INPUT fi-qtd-can-out,
                         INPUT fi-vlr-can-out,
                         INPUT fi-qtd-fat-out,
                         INPUT fi-vlr-fat-out,
                         INPUT i-tp-relat).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-Grafico-qtd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-Grafico-qtd B-table-Win
ON CHOOSE OF bt-Grafico-qtd IN FRAME F-Main /* Grafico */
DO:
    RUN esdlg/d04essp0174.w (OUTPUT l-barra-vendidos,
                             OUTPUT l-barra-cancelados,
                             OUTPUT l-barra-faturados).
    IF l-barra-vendidos   = NO AND 
       l-barra-cancelados = NO AND 
       l-barra-faturados  = NO THEN DO:
          MESSAGE "N∆o Existe Barra(s) para a Montagem do Grafico. Favor selecionar ! ! !"
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
    ELSE DO:
       RUN pi-grafico-qtd.
       FIND FIRST tt-work NO-LOCK NO-ERROR.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-Grafico-vlr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-Grafico-vlr B-table-Win
ON CHOOSE OF bt-Grafico-vlr IN FRAME F-Main /* Grafico */
DO:
    RUN esdlg/d04essp0174.w (OUTPUT l-barra-vendidos,
                             OUTPUT l-barra-cancelados,
                             OUTPUT l-barra-faturados).
    IF l-barra-vendidos   = NO AND 
       l-barra-cancelados = NO AND 
       l-barra-faturados  = NO THEN DO:
          MESSAGE "N∆o Existe Barra(s) para a Montagem do Grafico. Favor selecionar ! ! !"
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
    ELSE DO:
       RUN pi-grafico-vlr.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-imprime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-imprime B-table-Win
ON CHOOSE OF bt-imprime IN FRAME F-Main /* Imprimir */
DO:
  RUN pi-imprime.
  FIND FIRST tt-work.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-vapra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-vapra B-table-Win
ON CHOOSE OF bt-vapra IN FRAME F-Main
DO:
  
 RUN esdlg/d01essp0174.w (OUTPUT c-dia).
 IF c-dia <> "" THEN DO:
    FIND FIRST tt-work WHERE
               tt-work.dia = c-dia NO-LOCK NO-ERROR.
    IF AVAIL tt-work THEN
       h-query:REPOSITION-TO-ROWID(ROWID(tt-work)) NO-ERROR. 
    ELSE
       MESSAGE "O Dia do Periodo " c-dia ", n∆o est† contido na seleá∆o!"
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
 END.
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-vendas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-vendas B-table-Win
ON CHOOSE OF bt-vendas IN FRAME F-Main
DO:
   RUN esp/essp0174a.p (INPUT TABLE tt-vda,
                        INPUT tt-work.dia,
                        INPUT c-dt-limite,
                        INPUT i-tp-relat).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-vendas-tot
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-vendas-tot B-table-Win
ON CHOOSE OF bt-vendas-tot IN FRAME F-Main
DO:
   RUN esp/essp0174d.p (INPUT TABLE tt-vda,
                        INPUT c-dt-limite,
                        INPUT i-tp-relat).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
/*
br-itens:NUM-LOCKED-COLUMNS = 2.
*/

ASSIGN br-work:TITLE IN FRAME {&FRAME-NAME} = "RELAT‡RIO DE VENDAS / FATURAMENTO DO PERIODO" +
                                              " " + SUBSTR(c-dt-limite,1,2) + "/" + SUBSTR(c-dt-limite,3,4).

ASSIGN h-query = br-work:QUERY.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-abre-excel B-table-Win 
PROCEDURE pi-abre-excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF INPUT PARAMETER p-arquivo AS CHAR.

 DEF VAR h-prog AS HANDLE NO-UNDO.
 RUN utp/ut-utils.p PERSISTENT SET h-prog.

 RUN EXECUTE IN h-prog(INPUT "EXCEL.EXE", INPUT p-arquivo).

 DELETE PROCEDURE h-prog.
 PAUSE 5 NO-MESSAGE.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cancelados B-table-Win 
PROCEDURE pi-cancelados :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH ped-item USE-INDEX planejamento WHERE
         SUBSTR(ped-item.it-codigo,1,1) >= "5"  AND
         ped-item.it-codigo >= c-it-codigo-ini  AND
         ped-item.it-codigo <= c-it-codigo-fin  AND
         ped-item.cod-sit-item           = 6   NO-LOCK. 
      
      RUN pi-acompanhar IN h-acomp (INPUT "  Item: " + ped-item.it-codigo +
                                          "   Ref: " + ped-item.cod-refer).

      IF ped-item.dt-canseq < da-dt-implant-ini OR 
         ped-item.dt-canseq > da-dt-implant-fin THEN NEXT. 

      IF ped-item.cod-refer < c-cod-refer-ini OR  
         ped-item.cod-refer > c-cod-refer-fin THEN NEXT.
      
      FIND ped-item-ext OF ped-item NO-LOCK NO-ERROR.
      IF NOT AVAIL ped-item-ext THEN NEXT.
      IF LOOKUP(SUBSTR(ped-item-ext.lote,1,2),c-lotes) = 0 THEN NEXT.
      IF ped-item-ext.corte-comerc < c-corte-comerc-ini OR
         ped-item-ext.corte-comerc > c-corte-comerc-fin THEN NEXT.

      FIND item-ext WHERE
           item-ext.it-codigo = ped-item.it-codigo NO-LOCK NO-ERROR.
      /*IF c-opc-artigo <> 'A' THEN 
         IF AVAIL item-ext AND
            (item-ext.indigo = YES AND c-opc-artigo <> "I") OR
            (item-ext.indigo = NO  AND c-opc-artigo <> "O") THEN NEXT.*/

      FIND ped-venda OF ped-item NO-LOCK NO-ERROR.
      IF NOT AVAIL ped-venda THEN NEXT.
      IF ped-venda.cod-estabel < c-cod-estabel-ini OR 
         ped-venda.cod-estabel > c-cod-estabel-fin THEN NEXT.
      IF ped-venda.nome-abrev < c-nome-abrev-ini OR 
         ped-venda.nome-abrev > c-nome-abrev-fin THEN NEXT.
      IF ped-venda.no-ab-reppri < c-no-ab-reppri-ini OR 
         ped-venda.no-ab-reppri > c-no-ab-reppri-fin THEN NEXT.

      FIND ped-venda-ext WHERE 
           ped-venda-ext.nr-pedido = ped-venda.nr-pedido NO-LOCK NO-ERROR.
      IF NOT AVAIL ped-venda-ext THEN NEXT.

      FIND natur-oper WHERE
           natur-oper.nat-operacao = ped-venda.nat-operacao NO-LOCK NO-ERROR.
      IF NOT AVAIL natur-oper THEN NEXT.

      IF c-tp-pedido <> "Todos" AND c-tp-pedido <> ped-venda-ext.tp-pedido THEN NEXT.

      IF c-tp-pedido = "Todos" AND c-tp-merc = "I" AND natur-oper.cod-esp <> "DP" AND
         LOOKUP(ped-venda-ext.tp-pedido,"AMOSTRA,AMOSTRA EXPORTAÄ«O,DOAÄ«O,BONIFICAÄ«O") <> 0 THEN NEXT.

      IF (c-tp-merc = "E" AND ped-venda-ext.tp-pedido <> "EXPORTACAO") OR
         (c-tp-merc = "I" AND ped-venda-ext.tp-pedido  = "EXPORTACAO")  THEN NEXT.

      ASSIGN c-dia  = STRING(DAY(ped-item.dt-canseq), "99")
             c-tipo = "Normal".
      IF i-tp-relat = 2 THEN
         ASSIGN c-dia = STRING(MONTH(ped-item.dt-canseq), "99").

      IF natur-oper.cod-esp <> "DP" THEN DO: /* Lojas Tear e Rem.Industrializaá∆o */
         IF ped-venda-ext.tp-pedido <> "Rem.Industrializaá∆o" THEN DO:
            IF LOOKUP(ped-venda-ext.tp-pedido,"Doaá∆o,bonificaá∆o") <> 0 THEN DO:
               ASSIGN fi-qtd-can-out = fi-qtd-can-out + ped-item.qt-pedida
                      fi-vlr-can-out = fi-vlr-can-out + (ped-item.qt-pedida * ped-item.vl-preori)
                      c-tipo         = "Outros".
            END.
            ELSE DO:

               IF ped-venda-ext.tp-pedido = "Normal" OR
                  ped-venda-ext.tp-pedido = "Reserva" THEN DO:
                  ASSIGN fi-qtd-can-ljs = fi-qtd-can-ljs + ped-item.qt-pedida
                         fi-vlr-can-ljs = fi-vlr-can-ljs + (ped-item.qt-pedida * ped-item.vl-preori)
                         c-tipo         = "Loja".
                  FIND tt-lojas WHERE
                       tt-lojas.loja  = ped-venda.nome-abrev NO-ERROR.
                  IF NOT AVAIL tt-lojas THEN DO:
                     CREATE tt-lojas.
                     ASSIGN tt-lojas.loja  = ped-venda.nome-abrev.
                  END.
                  ASSIGN tt-lojas.qtd-can = tt-lojas.qtd-can + ped-item.qt-pedida
                         tt-lojas.vlr-can = tt-lojas.vlr-can + (ped-item.qt-pedida * ped-item.vl-preori).
               END.
            END.
         END.
         ELSE
            ASSIGN fi-qtd-can-ind = fi-qtd-can-ind + ped-item.qt-pedida
                   fi-vlr-can-ind = fi-vlr-can-ind + (ped-item.qt-pedida * ped-item.vl-preori)
                   c-tipo         = "Indl".
      END.
      /* Gera Acumulados NORMAIS */
      /*                         */
      IF c-tipo = "Normal" THEN DO:
         FIND tt-work WHERE
              tt-work.dia  = c-dia NO-ERROR.
         IF NOT AVAIL tt-work THEN DO:
            CREATE tt-work.
            ASSIGN tt-work.dia  = c-dia
                   tt-work.mes  = SUBSTR("JANFEVMARABRMAIJUNJULAGOSETOUTNOVDEZ",INT(c-dia) * 3 - 2,3).
         END.
         ASSIGN tt-work.qtd-can = tt-work.qtd-can + ped-item.qt-pedida
                tt-work.vlr-can = tt-work.vlr-can + (ped-item.qt-pedida * ped-item.vl-preori).
      END.

      /* Grava Detalhamento dos Cancelamentos por Dia */
      /*                                              */
      FIND tt-can WHERE
           tt-can.tipo      = c-tipo AND 
           tt-can.dia       = c-dia  AND
           tt-can.nr-pedcli = ped-venda.nr-pedcli NO-ERROR.
      IF NOT AVAIL tt-can THEN DO:
         CREATE tt-can.
         ASSIGN tt-can.tipo         = c-tipo
                tt-can.dia          = c-dia
                tt-can.nr-pedcli    = ped-venda.nr-pedcli
                tt-can.nr-pedrep    = ped-venda.nr-pedrep
                tt-can.nome-abrev   = ped-venda.nome-abrev
                tt-can.no-ab-reppri = ped-venda.no-ab-reppri
                tt-can.dt-cancela   = ped-item.dt-canseq
                tt-can.motivo       = ped-item.desc-cancela
                tt-can.dt-implant   = ped-venda.dt-implant.
        CASE ped-venda.cod-sit-ped.
             WHEN 1 THEN ASSIGN tt-can.sit = 'ABE'.
             WHEN 2 THEN ASSIGN tt-can.sit = 'ATP'.
             WHEN 3 THEN ASSIGN tt-can.sit = 'ATT'.
             WHEN 4 THEN ASSIGN tt-can.sit = 'PEN'.
             WHEN 5 THEN ASSIGN tt-can.sit = 'SUS'.
             WHEN 6 THEN ASSIGN tt-can.sit = 'CAN'.
        END CASE.
      END.
      ASSIGN tt-can.qtde  = tt-can.qtde  + ped-item.qt-pedida
             tt-can.valor = tt-can.valor + (ped-item.qt-pedida * ped-item.vl-preori).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-faturados B-table-Win 
PROCEDURE pi-faturados :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 FOR EACH nota-fiscal USE-INDEX ch-distancia WHERE
          nota-fiscal.dt-emis-nota >= da-dt-implant-ini AND 
          nota-fiscal.dt-emis-nota <= da-dt-implant-fin AND
          nota-fiscal.cod-estabel  >= c-cod-estabel-ini AND
          nota-fiscal.cod-estabel  <= c-cod-estabel-fin AND
          nota-fiscal.dt-cancela    = ? NO-LOCK. 

     RUN pi-acompanhar IN h-acomp (INPUT "Data: "    + STRING(nota-fiscal.dt-emis-nota) +
                                         " Nota Fiscal: " + nota-fiscal.nr-nota-fis).

     FIND ped-venda WHERE
          ped-venda.nr-pedcli  = nota-fiscal.nr-pedcli  AND
          ped-venda.nome-abrev = nota-fiscal.nome-ab-cl NO-LOCK NO-ERROR.
     IF NOT AVAIL ped-venda THEN NEXT.

     IF nota-fiscal.nome-ab-cli  < c-nome-abrev-ini OR
        nota-fiscal.nome-ab-cli  > c-nome-abrev-fin  THEN NEXT.
     
     IF nota-fiscal.no-ab-reppri < c-no-ab-reppri-ini OR
        nota-fiscal.no-ab-reppri > c-no-ab-reppri-fin THEN NEXT.

     FIND natur-oper WHERE
          natur-oper.nat-operacao = nota-fiscal.nat-operacao NO-LOCK NO-ERROR.
     IF NOT AVAIL natur-oper THEN NEXT.

     FIND ped-venda-ext WHERE 
          ped-venda-ext.nr-pedido = ped-venda.nr-pedido NO-LOCK NO-ERROR.
     IF NOT AVAIL ped-venda-ext THEN NEXT.


     IF c-tp-pedido <> "Todos" AND c-tp-pedido <> ped-venda-ext.tp-pedido THEN NEXT.

     IF c-tp-pedido = "Todos" AND natur-oper.cod-esp <> "DP" AND 
        LOOKUP(ped-venda-ext.tp-pedido,"AMOSTRA,AMOSTRA EXPORTAÄ«O") <> 0 THEN NEXT.

     IF (c-tp-merc = "E" AND ped-venda-ext.tp-pedido <> "EXPORTACAO") OR
        (c-tp-merc = "I" AND ped-venda-ext.tp-pedido  = "EXPORTACAO")  THEN NEXT.
     
     ASSIGN de-qtd-conv = 0.
     FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK.
         IF it-nota-fisc.it-codigo < c-it-codigo-ini OR 
            it-nota-fisc.it-codigo > c-it-codigo-fin  THEN NEXT.

         IF it-nota-fisc.cod-refer < c-cod-refer-ini OR  
            it-nota-fisc.cod-refer > c-cod-refer-fin THEN NEXT.

         FIND ped-item-ext WHERE 
              ped-item-ext.nome-abrev   = it-nota-fisc.nome-ab-cli AND
              ped-item-ext.nr-pedcli    = it-nota-fisc.nr-pedcli   AND
              ped-item-ext.nr-sequencia = it-nota-fisc.nr-seq-ped  AND
              ped-item-ext.it-codigo    = it-nota-fisc.it-codigo   AND
              ped-item-ext.cod-refer    = it-nota-fisc.cod-refer NO-LOCK NO-ERROR.
         IF NOT AVAIL ped-item-ext THEN NEXT.
         IF LOOKUP(SUBSTR(ped-item-ext.lote,1,2),c-lotes) = 0 THEN NEXT.
         IF ped-item-ext.corte-comerc < c-corte-comerc-ini OR
            ped-item-ext.corte-comerc > c-corte-comerc-fin THEN NEXT.

         FIND item-ext WHERE
              item-ext.it-codigo = it-nota-fisc.it-codigo NO-LOCK NO-ERROR.
        /* IF c-opc-artigo <> 'A' THEN 
            IF AVAIL item-ext AND
               (item-ext.indigo = YES AND c-opc-artigo <> "I") OR
               (item-ext.indigo = NO  AND c-opc-artigo <> "O") THEN NEXT.*/

         FIND item WHERE
              item.it-codigo = it-nota-fisc.it-codigo  NO-LOCK NO-ERROR.
         IF AVAIL item THEN DO:
            IF item.un = "kg" THEN DO:
               IF AVAIL item-ext THEN
                  ASSIGN de-qtd-conv = de-qtd-conv + (it-nota-fisc.qt-faturada[1] /* *  item-ext.fator-conv */).
               ELSE
                  ACCUMULATE it-nota-fisc.qt-faturada[1] (TOTAL).
            END.                          
            ELSE                              
               ACCUMULATE it-nota-fisc.qt-faturada[1] (TOTAL).
            ACCUMULATE it-nota-fisc.vl-tot-item    (TOTAL).
         END.
         /*
         ACCUMULATE it-nota-fisc.qt-faturada[1] (TOTAL).
         ACCUMULATE it-nota-fisc.vl-tot-item    (TOTAL). */
     END.
     IF (ACCUM TOTAL it-nota-fisc.qt-faturada[1]) + (ACCUM TOTAL it-nota-fisc.vl-tot-item) + de-qtd-conv = 0 THEN NEXT.

     ASSIGN c-dia  = STRING(DAY(nota-fiscal.dt-emis-nota), "99")
            c-tipo = "Normal".
     IF i-tp-relat = 2 THEN
        ASSIGN c-dia = STRING(MONTH(nota-fiscal.dt-emis-nota), "99").

     IF natur-oper.cod-esp <> "DP" THEN DO: /* Lojas Tear e Rem.Industrializaá∆o */
        IF ped-venda-ext.tp-pedido <> "Rem.Industrializaá∆o" THEN DO:
           IF LOOKUP(ped-venda-ext.tp-pedido,"Doaá∆o,bonificaá∆o") <> 0 THEN DO:
              ASSIGN fi-qtd-fat-out = fi-qtd-fat-out + de-qtd-conv + ACCUM TOTAL it-nota-fisc.qt-faturada[1]
                     fi-vlr-fat-out = fi-vlr-fat-out + ACCUM TOTAL it-nota-fisc.vl-tot-item
                     c-tipo         = "Outros".
           END.
           ELSE DO:
              IF ped-venda-ext.tp-pedido = "Normal" OR    
                 ped-venda-ext.tp-pedido = "Reserva" THEN DO:
                 ASSIGN fi-qtd-fat-ljs = fi-qtd-fat-ljs + de-qtd-conv + ACCUM TOTAL it-nota-fisc.qt-faturada[1]
                        fi-vlr-fat-ljs = fi-vlr-fat-ljs + ACCUM TOTAL it-nota-fisc.vl-tot-item
                        c-tipo         = "Loja".
                 FIND tt-lojas WHERE
                      tt-lojas.loja  = nota-fiscal.nome-ab-cli NO-ERROR.
                 IF NOT AVAIL tt-lojas THEN DO:
                    CREATE tt-lojas.
                    ASSIGN tt-lojas.loja  = nota-fiscal.nome-ab-cli.
                 END.
                 ASSIGN tt-lojas.qtd-fat = tt-lojas.qtd-fat + de-qtd-conv + ACCUM TOTAL it-nota-fisc.qt-faturada[1]
                        tt-lojas.vlr-fat = tt-lojas.vlr-fat + ACCUM TOTAL it-nota-fisc.vl-tot-item.
              END.
           END.
        END.
        ELSE DO:
            ASSIGN fi-qtd-fat-ind = fi-qtd-fat-ind + de-qtd-conv + ACCUM TOTAL it-nota-fisc.qt-faturada[1]
                   fi-vlr-fat-ind = fi-vlr-fat-ind + ACCUM TOTAL it-nota-fisc.vl-tot-item
                   c-tipo         = "Indl".
        END.
     END.
     /* Gera Acumulados NORMAIS */
     /*                         */
     IF c-tipo = "Normal" THEN DO:
        FIND tt-work WHERE
             tt-work.dia  = c-dia NO-ERROR.
        IF NOT AVAIL tt-work THEN DO:
           CREATE tt-work.
           ASSIGN tt-work.dia  = c-dia
                  tt-work.mes  = SUBSTR("JANFEVMARABRMAIJUNJULAGOSETOUTNOVDEZ",INT(c-dia) * 3 - 2,3).
        END.
        ASSIGN tt-work.qtd-fat = tt-work.qtd-fat + de-qtd-conv + ACCUM TOTAL it-nota-fisc.qt-faturada[1]
               tt-work.vlr-fat = tt-work.vlr-fat + ACCUM TOTAL it-nota-fisc.vl-tot-item.
     END.

     /* Grava Detalhamento do Faturamento por Dia */
     /*                                           */
     FIND tt-fat WHERE
          tt-fat.tipo        = c-tipo AND
          tt-fat.dia         = c-dia  AND
          tt-fat.nr-nota-fis = nota-fiscal.nr-nota-fis NO-ERROR.
     IF NOT AVAIL tt-fat THEN DO:
        CREATE tt-fat.
        ASSIGN tt-fat.tipo         = c-tipo
               tt-fat.dia          = c-dia
               tt-fat.nr-nota-fis  = nota-fiscal.nr-nota-fis
               tt-fat.dt-emis-nota = nota-fiscal.dt-emis-nota
               tt-fat.dt-saida     = nota-fiscal.dt-saida
               tt-fat.nome-ab-cli  = nota-fiscal.nome-ab-cli
               tt-fat.nr-pedcli    = nota-fiscal.nr-pedcli
               tt-fat.no-ab-reppri = nota-fiscal.no-ab-reppri.
     END.
     ASSIGN tt-fat.qtde  = tt-fat.qtde  + de-qtd-conv + ACCUM TOTAL it-nota-fisc.qt-faturada[1]
            tt-fat.valor = tt-fat.valor + ACCUM TOTAL it-nota-fisc.vl-tot-item.
 END.

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
 DEF INPUT PARAMETER p-arq-saida AS CHAR FORMAT "x(45)".
    
 HIDE FRAME f-main.

 DEFINE FRAME frm_excel WITH SIZE 2 BY 2 ROW 1 COLUMN 1.
 ENABLE ALL WITH FRAME frm_excel.
   
 RUN pi-abre-excel (INPUT "").
 PAUSE 3 NO-MESSAGE.

 DDE INITIATE sys FRAME FRAME frm_excel:HANDLE APPLICATION "EXCEL" TOPIC "System".
 DDE INITIATE i-canal FRAME FRAME frm_excel:HANDLE APPLICATION "EXCEL" TOPIC "Pasta1".

 RUN pi-monta-planilha.

 OS-DELETE VALUE(p-arq-saida).
 DDE EXECUTE   sys COMMAND '[save.as("' + p-arq-saida + '")]'.
    
 DDE EXECUTE   sys COMMAND "[close(0)]". 
 DDE EXECUTE   sys COMMAND "[quit()]". 
   
 DDE TERMINATE sys.
    
 HIDE FRAME frm_excel.
 CLEAR FRAME frm_excel.
 DISABLE ALL WITH FRAME frm_excel.

 VIEW FRAME f-main.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-grafico-qtd B-table-Win 
PROCEDURE pi-grafico-qtd :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

 DEF VAR i-point AS INT INITIAL 1.

 DEF VAR i-numsets AS INT INITIAL 0.
 DEF VAR c-titulo-grafico AS CHAR.

 IF l-barra-vendidos = YES THEN
    ASSIGN c-titulo-grafico = "Metros Vendidos".

 IF l-barra-cancelados = YES THEN
    IF c-titulo-grafico = "" THEN
       ASSIGN c-titulo-grafico = "Metros Cancelados".
    ELSE
       ASSIGN c-titulo-grafico = c-titulo-grafico + " X Metros Cancelados".

 IF l-barra-faturados = YES THEN
    IF c-titulo-grafico = "" THEN
       ASSIGN c-titulo-grafico = "Metros Faturados".
    ELSE
       ASSIGN c-titulo-grafico = c-titulo-grafico + " X Metros Faturados ".

 ASSIGN c-titulo-grafico = c-titulo-grafico + " Em ".
 EMPTY TEMP-TABLE tt-atributos.
 EMPTY TEMP-TABLE tt-sets.
 EMPTY TEMP-TABLE tt-points-2.
 EMPTY TEMP-TABLE tt-dados.
 EMPTY TEMP-TABLE tt-erros.

 /* Configuraá∆o Geral do Grafico */
 /*                               */
 CREATE tt-atributos.
 IF i-tp-relat = 1 THEN
    ASSIGN tt-atributos.cod-versao-integracao = 3
           tt-atributos.graphtype             = 4
           tt-atributos.graphtitle            = c-titulo-grafico + " " + 
                                                SUBSTR(c-dt-limite,1,2) + "/" + SUBSTR(c-dt-limite,3,4) + '.'
           tt-atributos.lefttitle             = 'Quantidade em METROS.'
           tt-atributos.lefttitlestyle        = 2
           tt-atributos.bottomtitle           = 'D I A S'
           tt-atributos.numgraph              = 1.
 ELSE
     ASSIGN tt-atributos.cod-versao-integracao = 3
            tt-atributos.graphtype             = 4
            tt-atributos.graphtitle            = c-titulo-grafico + " " + SUBSTR(c-dt-limite,3,4) + '.'
            tt-atributos.lefttitle             = 'Quantidade em METROS.'
            tt-atributos.lefttitlestyle        = 2
            tt-atributos.bottomtitle           = 'M E S E S'
            tt-atributos.numgraph              = 1.


 /* Configuraá∆o das Variantes do Grafico (Linhas ou  Barras */
 /*                                                          */
 ASSIGN i-numsets = 1.
 IF l-barra-vendidos = YES THEN DO:
    CREATE tt-sets.
    ASSIGN tt-sets.NumSet   = i-numsets 
           tt-sets.NumGraph = 1
           tt-sets.ColorSet = 1
           tt-sets.legendText = "Metros Vendidos".
    ASSIGN i-numsets = i-numsets + 1.
 END.

 IF l-barra-cancelados = YES THEN DO:
    CREATE tt-sets.
    ASSIGN tt-sets.NumSet   = i-numsets
           tt-sets.NumGraph = 1
           tt-sets.ColorSet = 4
           tt-sets.legendText = "Metros Cancelados".
    ASSIGN i-numsets = i-numsets + 1.
 END.

 IF l-barra-faturados = YES THEN DO:
    CREATE tt-sets.
    ASSIGN tt-sets.NumSet   = i-numsets
           tt-sets.NumGraph = 1
           tt-sets.ColorSet = 2
           tt-sets.legendText = "Metros Faturados".
 END.

 FOR EACH tt-work WHERE NO-LOCK.

    /* Valores do EIXO X (DIAS) */
    CREATE tt-points-2.
    IF i-tp-relat = 1 THEN
       ASSIGN tt-points-2.NumPoint  = i-point
              tt-points-2.NumGraph  = 1
              tt-points-2.labeltext = tt-work.dia.
    ELSE
        ASSIGN tt-points-2.NumPoint  = i-point
               tt-points-2.NumGraph  = 1
               tt-points-2.labeltext = tt-work.mes.


    ASSIGN i-numsets = 1.
    IF l-barra-vendidos = YES  THEN DO:
       /* Valores do EIXO Y (Metros Vendidos) */ 
       CREATE tt-dados.
       ASSIGN tt-dados.NumPoint   = i-point
              tt-dados.NumSet     = i-numsets
              tt-dados.NumGraph   = 1
              tt-dados.graphdata  = tt-work.qtd-vda.
       ASSIGN i-numsets = i-numsets + 1.
    END.

    IF l-barra-cancelados = YES THEN DO:
       /* Valores do EIXO Y (Metros Cancelados) */
       CREATE tt-dados.
       ASSIGN tt-dados.NumPoint   = i-point
              tt-dados.NumSet     = i-numsets
              tt-dados.NumGraph   = 1
              tt-dados.graphdata  = tt-work.qtd-can.
       ASSIGN i-numsets = i-numsets + 1.
    END.

    IF l-barra-faturados = YES THEN DO:
       /* Valores do EIXI Y (Metros Faturados) */
       CREATE tt-dados.
       ASSIGN tt-dados.NumPoint   = i-point
              tt-dados.NumSet     = i-numsets
              tt-dados.NumGraph   = 1
              tt-dados.graphdata  = tt-work.qtd-fat.
    END.

    ASSIGN i-point = i-point + 1.

 END.

 DEF VAR h-utapi011 AS HANDLE NO-UNDO.

 RUN utp/utapi011.p PERSISTENT SET h-utapi011.

 RUN pi-execute IN h-utapi011 (INPUT  TABLE tt-atributos,
                               INPUT  TABLE tt-points-2,
                               INPUT  TABLE tt-sets,
                               INPUT  TABLE tt-dados,
                               INPUT  TABLE tt-ylabels,
                               OUTPUT TABLE tt-erros).

 IF RETURN-VALUE = "NOK" THEN DO: 
    FOR EACH tt-erros: 
        DISP cod-erro desc-erro FORMAT "x(100)" WITH 1 COL WIDTH 500. 
    END.
 END.                  

 DELETE PROCEDURE h-utapi011.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-grafico-vlr B-table-Win 
PROCEDURE pi-grafico-vlr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF VAR i-point AS INT INITIAL 1.

 DEF VAR i-numsets AS INT INITIAL 0.
 DEF VAR c-titulo-grafico AS CHAR.

 IF l-barra-vendidos = YES THEN
    ASSIGN c-titulo-grafico = "Valores Vendidos".

 IF l-barra-cancelados = YES THEN
    IF c-titulo-grafico = "" THEN
       ASSIGN c-titulo-grafico = "Valores Cancelados".
    ELSE
       ASSIGN c-titulo-grafico = c-titulo-grafico + " X Valores Cancelados".

 IF l-barra-faturados = YES THEN
    IF c-titulo-grafico = "" THEN
       ASSIGN c-titulo-grafico = "Valores Faturados".
    ELSE
       ASSIGN c-titulo-grafico = c-titulo-grafico + " X Valores Faturados ".

 ASSIGN c-titulo-grafico = c-titulo-grafico + "Em ".
 EMPTY TEMP-TABLE tt-atributos.
 EMPTY TEMP-TABLE tt-sets.
 EMPTY TEMP-TABLE tt-points-2.
 EMPTY TEMP-TABLE tt-dados.
 EMPTY TEMP-TABLE tt-erros.

 /* Configuraá∆o Geral do Grafico */
 /*                               */
 CREATE tt-atributos.
 IF i-tp-relat = 1 THEN
    ASSIGN tt-atributos.cod-versao-integracao = 3
           tt-atributos.graphtype             = 4
           tt-atributos.graphtitle            = c-titulo-grafico + " " +
                                                SUBSTR(c-dt-limite,1,2) + "/" + SUBSTR(c-dt-limite,3,4) + '.'
           tt-atributos.lefttitle             = 'Valores em REAL.'
           tt-atributos.lefttitlestyle        = 2
           tt-atributos.bottomtitle           = 'D I A S'
           tt-atributos.numgraph              = 1.
 ELSE
     ASSIGN tt-atributos.cod-versao-integracao = 3
            tt-atributos.graphtype             = 4
            tt-atributos.graphtitle            = c-titulo-grafico + " " + SUBSTR(c-dt-limite,3,4) + '.'
            tt-atributos.lefttitle             = 'Valores em REAL.'
            tt-atributos.lefttitlestyle        = 2
            tt-atributos.bottomtitle           = 'M E S E S'
            tt-atributos.numgraph              = 1.


 /* Configuraá∆o das Variantes do Grafico (Linhas ou  Barras */
 /*                                                          */
 ASSIGN i-numsets = 1.
 IF l-barra-vendidos = YES THEN DO:
    CREATE tt-sets.
    ASSIGN tt-sets.NumSet   = i-numsets 
           tt-sets.NumGraph = 1
           tt-sets.ColorSet = 1
           tt-sets.legendText = "Valores Vendidos".
    ASSIGN i-numsets = i-numsets + 1.
 END.

 IF l-barra-cancelados = YES THEN DO:
    CREATE tt-sets.
    ASSIGN tt-sets.NumSet   = i-numsets
           tt-sets.NumGraph = 1
           tt-sets.ColorSet = 4
           tt-sets.legendText = "Valores Cancelados".
    ASSIGN i-numsets = i-numsets + 1.
 END.

 IF l-barra-faturados = YES THEN DO:
    CREATE tt-sets.
    ASSIGN tt-sets.NumSet   = i-numsets 
           tt-sets.NumGraph = 1
           tt-sets.ColorSet = 2
           tt-sets.legendText = "Valores Faturados".
 END.

 FOR EACH tt-work WHERE NO-LOCK.

    /* Valores do EIXO X (DIAS) */
    CREATE tt-points-2.
    IF i-tp-relat = 1 THEN
       ASSIGN tt-points-2.NumPoint  = i-point
              tt-points-2.NumGraph  = 1
              tt-points-2.labeltext = tt-work.dia.
    ELSE
       ASSIGN tt-points-2.NumPoint  = i-point
              tt-points-2.NumGraph  = 1
              tt-points-2.labeltext = tt-work.mes.

    ASSIGN i-numsets = 1.
    IF l-barra-vendidos = YES THEN DO:
       /* Valores do EIXO Y (Metros Vendidos) */ 
       CREATE tt-dados.
       ASSIGN tt-dados.NumPoint   = i-point
              tt-dados.NumSet     = i-numsets
              tt-dados.NumGraph   = 1
              tt-dados.graphdata  = tt-work.vlr-vda.
       ASSIGN i-numsets = i-numsets + 1.
    END.

    IF l-barra-cancelados = YES THEN DO:
       /* Valores do EIXO Y (Metros Cancelados) */
       CREATE tt-dados.
       ASSIGN tt-dados.NumPoint   = i-point
              tt-dados.NumSet     = i-numsets
              tt-dados.NumGraph   = 1
              tt-dados.graphdata  = tt-work.vlr-can.
       ASSIGN i-numsets = i-numsets + 1.
    END.

    IF l-barra-faturados THEN DO:
       /* Valores do EIXI Y (Metros Faturados) */
       CREATE tt-dados.
       ASSIGN tt-dados.NumPoint   = i-point
              tt-dados.NumSet     = i-numsets
              tt-dados.NumGraph   = 1
              tt-dados.graphdata  = tt-work.vlr-fat.
    END.
    ASSIGN i-point = i-point + 1.

 END.

 DEF VAR h-utapi011 AS HANDLE NO-UNDO.

 RUN utp/utapi011.p PERSISTENT SET h-utapi011.

 RUN pi-execute IN h-utapi011 ( INPUT  TABLE tt-atributos,
                                INPUT  TABLE tt-points-2,
                                INPUT  TABLE tt-sets,
                                INPUT  TABLE tt-dados,
                                INPUT  TABLE tt-ylabels,
                                OUTPUT TABLE tt-erros).

 IF RETURN-VALUE = "NOK" THEN DO: 
    FOR EACH tt-erros: 
        DISP cod-erro desc-erro FORMAT "x(100)" WITH 1 COL WIDTH 500. 
    END.
 END.                  

 DELETE PROCEDURE h-utapi011.


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
        "DATA: "                                  AT  52
        STRING(TODAY,"99/99/9999") FORMAT "X(10)" AT  58
        "HORA: "                                  AT  80
        STRING(TIME,"hh:mm:ss")                   AT  86
        "PAG:"                                    AT 115
        i-pag FORMAT ">>>"                        AT 120
        SKIP(1).



    IF i-tp-relat = 1 THEN DO:
       PUT "RELATORIO DE VENDAS / FATURAMENTO DO PERIODO" AT 31.
       PUT SUBSTR(c-dt-limite,1,2) + "/" + SUBSTR(c-dt-limite,3,4) AT 76 SKIP(1).
    END.
    ELSE DO:
        PUT "RELATORIO DE VENDAS / FATURAMENTO DO EXERCICIO: " AT 31.
        PUT SUBSTR(c-dt-limite,3,4) AT 79 SKIP(1).
    END.

    PUT "DATA        Metros Vendidos  Valores Vendidos  Metros Cancelados  Valores Cancelados  Metros Faturados   Valores Faturados" AT 1.  
    PUT "----------  ---------------  ----------------  -----------------  ------------------  ----------------   -----------------" AT 1.  
    ASSIGN i-pag = i-pag + 1.


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
          PUT CONTROL "~033E~033(s16H".    
      END.
      WHEN 2 THEN
          OUTPUT TO VALUE(c-saida).
      WHEN 3 THEN DO.
          ASSIGN c-saida = SESSION:TEMP-DIRECTORY + "essp0183.tmp".
          OUTPUT TO VALUE(c-saida) CONVERT SOURCE "ibm850".
      END.
  END CASE.
  DO i-ct = 1 TO i-num-copias:
     ASSIGN i-pag      =  1
            i-lin      = 99.
     FOR EACH tt-work WHERE NO-LOCK.
      
         IF i-lin > 62 THEN DO:
            RUN pi-imp-cabec.
            ASSIGN i-lin = 7.
         END.

         ASSIGN c-data-per = tt-work.dia + "/" + SUBSTR(c-dt-limite,1,2) + "/" + SUBSTR(c-dt-limite,3,4).
         IF i-tp-relat = 2 THEN
           ASSIGN c-data-per = tt-work.mes  + "/" + SUBSTR(c-dt-limite,3,4).



         PUT c-data-per     FORMAT "X(10)"            AT   1
             tt-work.qtd-vda  FORMAT ">>>,>>>,>>9.99" AT  14
             tt-work.vlr-vda  FORMAT ">>>,>>>,>>9.99" AT  32
             tt-work.qtd-can  FORMAT ">>>,>>>,>>9.99" AT  51
             tt-work.vlr-can  FORMAT ">>>,>>>,>>9.99" AT  71
             tt-work.qtd-fat  FORMAT ">>>,>>>,>>9.99" AT  89
             tt-work.vlr-fat  FORMAT ">>>,>>>,>>9.99" AT 109.
         ASSIGN i-lin = i-lin + 1.
         ACCUMULATE tt-work.qtd-vda (TOTAL).
         ACCUMULATE tt-work.vlr-vda (TOTAL).
         ACCUMULATE tt-work.qtd-can (TOTAL).
         ACCUMULATE tt-work.vlr-can (TOTAL).
         ACCUMULATE tt-work.qtd-fat (TOTAL).
         ACCUMULATE tt-work.vlr-fat (TOTAL).
    END.
    IF (ACCUM TOTAL tt-work.qtd-vda) <> 0 OR
       (ACCUM TOTAL tt-work.vlr-vda) <> 0 OR
       (ACCUM TOTAL tt-work.qtd-can) <> 0 OR 
       (ACCUM TOTAL tt-work.vlr-can) <> 0 OR 
       (ACCUM TOTAL tt-work.qtd-fat) <> 0 OR 
       (ACCUM TOTAL tt-work.vlr-fat) <> 0 THEN DO:
       IF i-lin > 59 THEN DO:
          RUN pi-imp-cabec.
          ASSIGN i-lin = 7.
       END.
       PUT "---------------  ----------------  -----------------  ------------------  ----------------   -----------------" AT 13 SKIP.
       PUT "NORMAL...." AT 1.
       PUT ACCUM TOTAL tt-work.qtd-vda FORMAT ">>>>,>>>,>>9.99" AT  13.
       PUT ACCUM TOTAL tt-work.vlr-vda FORMAT ">>>>,>>>,>>9.99" AT  31.
       PUT ACCUM TOTAL tt-work.qtd-can FORMAT ">>>>,>>>,>>9.99" AT  50.
       PUT ACCUM TOTAL tt-work.vlr-can FORMAT ">>>>,>>>,>>9.99" AT  70.
       PUT ACCUM TOTAL tt-work.qtd-fat FORMAT ">>>>,>>>,>>9.99" AT  88.
       PUT ACCUM TOTAL tt-work.vlr-fat FORMAT ">>>>,>>>,>>9.99" AT 108 SKIP.

       PUT "LOJAS....." AT 1.
       PUT fi-qtd-vda-ljs FORMAT ">>>>,>>>,>>9.99" AT  13.
       PUT fi-vlr-vda-ljs FORMAT ">>>>,>>>,>>9.99" AT  31.
       PUT fi-qtd-can-ljs FORMAT ">>>>,>>>,>>9.99" AT  50.
       PUT fi-vlr-can-ljs FORMAT ">>>>,>>>,>>9.99" AT  70.
       PUT fi-qtd-fat-ljs FORMAT ">>>>,>>>,>>9.99" AT  88.
       PUT fi-vlr-fat-ljs FORMAT ">>>>,>>>,>>9.99" AT 108 SKIP.

       PUT "INDUSTRIA." AT 1.
       PUT fi-qtd-vda-ind FORMAT ">>>>,>>>,>>9.99" AT  13.
       PUT fi-vlr-vda-ind FORMAT ">>>>,>>>,>>9.99" AT  31.
       PUT fi-qtd-can-ind FORMAT ">>>>,>>>,>>9.99" AT  50.
       PUT fi-vlr-can-ind FORMAT ">>>>,>>>,>>9.99" AT  70.
       PUT fi-qtd-fat-ind FORMAT ">>>>,>>>,>>9.99" AT  88.
       PUT fi-vlr-fat-ind FORMAT ">>>>,>>>,>>9.99" AT 108 SKIP.

       PUT "OUTROS...." AT 1.
       PUT fi-qtd-vda-out FORMAT ">>>>,>>>,>>9.99" AT  13.
       PUT fi-vlr-vda-out FORMAT ">>>>,>>>,>>9.99" AT  31.
       PUT fi-qtd-can-out FORMAT ">>>>,>>>,>>9.99" AT  50.
       PUT fi-vlr-can-out FORMAT ">>>>,>>>,>>9.99" AT  70.
       PUT fi-qtd-fat-out FORMAT ">>>>,>>>,>>9.99" AT  88.
       PUT fi-vlr-fat-out FORMAT ">>>>,>>>,>>9.99" AT 108 SKIP.

       PUT "TOT.GERAL." AT 1.
       PUT fi-qtd-vda-geral FORMAT ">>>>,>>>,>>9.99" AT  13.
       PUT fi-vlr-vda-geral FORMAT ">>>>,>>>,>>9.99" AT  31.
       PUT fi-qtd-can-geral FORMAT ">>>>,>>>,>>9.99" AT  50.
       PUT fi-vlr-can-geral FORMAT ">>>>,>>>,>>9.99" AT  70.
       PUT fi-qtd-fat-geral FORMAT ">>>>,>>>,>>9.99" AT  88.
       PUT fi-vlr-fat-geral FORMAT ">>>>,>>>,>>9.99" AT 108 SKIP.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-monta-planilha B-table-Win 
PROCEDURE pi-monta-planilha :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

 /* Cabeáalho  da Planilha */
 IF i-tp-relat = 1 THEN
    ASSIGN c-Lin = c-empresa + "             " + " VENDAS & FATURAMENTO DO PERIODO "  + SUBSTR(c-dt-limite,1,2) + "/" + SUBSTR(c-dt-limite,3,4). 
 ELSE
    ASSIGN c-Lin = c-empresa + "             " + " VENDAS & FATURAMENTO DO EXERCICIO "  + SUBSTR(c-dt-limite,3,4). 

 DDE SEND i-canal SOURCE c-Lin ITEM "L1C1".

 DDE EXECUTE i-canal COMMAND '[select("L1C1:L1C10")]'.
 /* Fonte, Tamanho, Negrito, It†lico, Sublinhado, Atachado, Cor(0=Autom†tico, 1=Preto, 2=Branco 3=Vermelho...) */
 DDE EXECUTE sys COMMAND "[format.font(~"Verdana~",10,True,False,False,False,3)]".

 /* Cabeáalho dos Dados */
 IF i-tp-relat = 1 THEN
    DDE SEND i-canal SOURCE "D I A S"         ITEM "L3C1".
 ELSE
    DDE SEND i-canal SOURCE " MESES "         ITEM "L3C1".

 DDE SEND i-canal SOURCE "METROS VENDIDOS"    ITEM "L3C2".
 DDE SEND i-canal SOURCE "VALORES VENDIDOS"   ITEM "L3C3".
 DDE SEND i-canal SOURCE "METROS CANCELADOS"  ITEM "L3C4".
 DDE SEND i-canal SOURCE "VALORES CANCELADOS" ITEM "L3C5".
 DDE SEND i-canal SOURCE "METROS FATURADOS"   ITEM "L3C6".
 DDE SEND i-canal SOURCE "VALORES FATURADOS"  ITEM "L3C7".

 DDE EXECUTE i-canal COMMAND '[select("L3C1:L3C7")]'.

 /* Formataá∆o das Celulas do Cabeáalho de Dados */
 DDE EXECUTE i-canal COMMAND '[select("L3C1:L3C7")]'.
 /* Fonte, Tamanho, Negrito, It†lico, Sublinhado, Atachado, Cor(0=Autom†tico, 1=Preto, 2=Branco 3=Vermelho...) */
 DDE EXECUTE sys COMMAND "[format.font(~"Verdana~",8,True,False,False,False,5)]".

 /* Horizontal(1=General 2=Left 3=Center 4=Right) Wrap(retorno autom†tico), Vertical(1=Top 2=Center 3=Bottom)
    Orientation(0=Horizontal 1=Vertical) */
 ASSIGN aux-command = '[select("L3C1:L3C1")]'.
 DDE EXECUTE i-canal COMMAND aux-command.
 DDE EXECUTE sys     COMMAND "[alignment(3,true,3,0)]".

 /* Horizontal(1=General 2=Left 3=Center 4=Right) Wrap(retorno autom†tico), Vertical(1=Top 2=Center 3=Bottom)
    Orientation(0=Horizontal 1=Vertical) */
 DDE EXECUTE i-canal COMMAND "[select(~"C1~")]". 
 DDE EXECUTE sys     COMMAND "[column.width(8.00)]".

 DDE EXECUTE i-canal COMMAND "[select(~"C2~")]". 
 DDE EXECUTE sys     COMMAND "[column.width(15.00)]".
 DDE EXECUTE sys     COMMAND "[format.number(~"###.###.##0,00~")]".
 DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]".

 DDE EXECUTE i-canal COMMAND "[select(~"C3~")]". 
 DDE EXECUTE sys     COMMAND "[column.width(15.00)]". 
 DDE EXECUTE sys     COMMAND "[format.number(~"###.###.##0,00~")]".
 DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]".

 DDE EXECUTE i-canal COMMAND "[select(~"C4~")]". 
 DDE EXECUTE sys     COMMAND "[column.width(15.00)]".
 DDE EXECUTE sys     COMMAND "[format.number(~"###.###.##0,00~")]".
 DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]".

 DDE EXECUTE i-canal COMMAND "[select(~"C5~")]". 
 DDE EXECUTE sys     COMMAND "[column.width(15.00)]".
 DDE EXECUTE sys     COMMAND "[format.number(~"###.###.##0,00~")]".
 DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]".

 DDE EXECUTE i-canal COMMAND "[select(~"C6~")]". 
 DDE EXECUTE sys     COMMAND "[column.width(15.00)]".
 DDE EXECUTE sys     COMMAND "[format.number(~"###.###.##0,00~")]".
 DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]".

 DDE EXECUTE i-canal COMMAND "[select(~"C7~")]". 
 DDE EXECUTE sys     COMMAND "[column.width(15.00)]".
 DDE EXECUTE sys     COMMAND "[format.number(~"###.###.##0,00~")]".
 DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]".

  /* Montagem das Celulas de Dados */
 ASSIGN i-Lin = 4.
 FOR EACH tt-work NO-LOCK.
     ASSIGN c-data-per = tt-work.dia + "/" + SUBSTR(c-dt-limite,1,2) + "/" + SUBSTR(c-dt-limite,3,4).
     IF i-tp-relat = 1 THEN
        DDE SEND i-canal SOURCE STRING(tt-work.dia)        ITEM "L" + TRIM(STRING(i-Lin)) + "C1". 
     ELSE
        DDE SEND i-canal SOURCE STRING(tt-work.mes)        ITEM "L" + TRIM(STRING(i-Lin)) + "C1". 
     DDE SEND i-canal SOURCE STRING(tt-work.qtd-vda)       ITEM "L" + TRIM(STRING(i-Lin)) + "C2". 
     DDE SEND i-canal SOURCE STRING(tt-work.vlr-vda)       ITEM "L" + TRIM(STRING(i-Lin)) + "C3".
     DDE SEND i-canal SOURCE STRING(tt-work.qtd-can)       ITEM "L" + TRIM(STRING(i-Lin)) + "C4".
     DDE SEND i-canal SOURCE STRING(tt-work.vlr-can)       ITEM "L" + TRIM(STRING(i-Lin)) + "C5".
     DDE SEND i-canal SOURCE STRING(tt-work.qtd-fat)       ITEM "L" + TRIM(STRING(i-Lin)) + "C6".
     DDE SEND i-canal SOURCE STRING(tt-work.vlr-fat)       ITEM "L" + TRIM(STRING(i-Lin)) + "C7".
     ASSIGN aux-command = '[select("L' + TRIM(STRING(i-Lin)) + 'C1:L' + TRIM(STRING(i-Lin)) + 'C7")]'.
     DDE EXECUTE i-canal COMMAND aux-command.
     /* Fonte, Tamanho, Negrito, It†lico, Sublinhado, Atachado, Cor(0=Autom†tico, 1=Preto, 2=Branco 3=Vermelho...) */
     DDE EXECUTE sys COMMAND "[format.font(~"Verdana~",8,False,False,False,False,0)]".


     /* Horizontal(1=General 2=Left 3=Center 4=Right) Wrap(retorno autom†tico), Vertical(1=Top 2=Center 3=Bottom)
      Orientation(0=Horizontal 1=Vertical) */
     ASSIGN aux-command = '[select("L' + TRIM(STRING(i-Lin)) + 'C1:L' + TRIM(STRING(i-Lin)) + 'C1")]'.
     DDE EXECUTE i-canal COMMAND aux-command.
     DDE EXECUTE sys     COMMAND "[alignment(3,true,3,0)]".

     ASSIGN i-Lin = i-Lin + 1.
     ACCUMULATE tt-work.qtd-vda (TOTAL).
     ACCUMULATE tt-work.vlr-vda (TOTAL).
     ACCUMULATE tt-work.qtd-can (TOTAL).
     ACCUMULATE tt-work.vlr-can (TOTAL).
     ACCUMULATE tt-work.qtd-fat (TOTAL).
     ACCUMULATE tt-work.vlr-fat (TOTAL).

 END.
 DDE SEND i-canal SOURCE "NORMAL" ITEM "L" + TRIM(STRING(i-Lin)) + "C1".
 DDE SEND i-canal SOURCE STRING((ACCUM TOTAL tt-work.qtd-vda)) ITEM "L" + TRIM(STRING(i-Lin)) + "C2".
 DDE SEND i-canal SOURCE STRING((ACCUM TOTAL tt-work.vlr-vda)) ITEM "L" + TRIM(STRING(i-Lin)) + "C3".
 DDE SEND i-canal SOURCE STRING((ACCUM TOTAL tt-work.qtd-can)) ITEM "L" + TRIM(STRING(i-Lin)) + "C4".
 DDE SEND i-canal SOURCE STRING((ACCUM TOTAL tt-work.vlr-can)) ITEM "L" + TRIM(STRING(i-Lin)) + "C5".
 DDE SEND i-canal SOURCE STRING((ACCUM TOTAL tt-work.qtd-fat)) ITEM "L" + TRIM(STRING(i-Lin)) + "C6".
 DDE SEND i-canal SOURCE STRING((ACCUM TOTAL tt-work.vlr-fat)) ITEM "L" + TRIM(STRING(i-Lin)) + "C7".
 ASSIGN aux-command = '[select("L' + TRIM(STRING(i-Lin)) + 'C1:L' + TRIM(STRING(i-Lin)) + 'C11")]'.
 DDE EXECUTE i-canal COMMAND aux-command.
 /* Fonte, Tamanho, Negrito, It†lico, Sublinhado, Atachado, Cor(0=Autom†tico, 1=Preto, 2=Branco 3=Vermelho...) */
 DDE EXECUTE sys COMMAND "[format.font(~"Verdana~",8,True,False,False,False,0)]".

 ASSIGN i-Lin = i-Lin + 1.
         
 DDE SEND i-canal SOURCE "LOJA" ITEM "L" + TRIM(STRING(i-Lin)) + "C1".
 DDE SEND i-canal SOURCE STRING(fi-qtd-vda-ljs) ITEM "L" + TRIM(STRING(i-Lin)) + "C2".
 DDE SEND i-canal SOURCE STRING(fi-vlr-vda-ljs) ITEM "L" + TRIM(STRING(i-Lin)) + "C3".
 DDE SEND i-canal SOURCE STRING(fi-qtd-can-ljs) ITEM "L" + TRIM(STRING(i-Lin)) + "C4".
 DDE SEND i-canal SOURCE STRING(fi-vlr-can-ljs) ITEM "L" + TRIM(STRING(i-Lin)) + "C5".
 DDE SEND i-canal SOURCE STRING(fi-qtd-fat-ljs) ITEM "L" + TRIM(STRING(i-Lin)) + "C6".
 DDE SEND i-canal SOURCE STRING(fi-vlr-fat-ljs) ITEM "L" + TRIM(STRING(i-Lin)) + "C7".
 ASSIGN aux-command = '[select("L' + TRIM(STRING(i-Lin)) + 'C1:L' + TRIM(STRING(i-Lin)) + 'C11")]'.
 DDE EXECUTE i-canal COMMAND aux-command.
 /* Fonte, Tamanho, Negrito, It†lico, Sublinhado, Atachado, Cor(0=Autom†tico, 1=Preto, 2=Branco 3=Vermelho...) */
 DDE EXECUTE sys COMMAND "[format.font(~"Verdana~",8,True,False,False,False,0)]".
 ASSIGN i-Lin = i-Lin + 1.

 DDE SEND i-canal SOURCE "INDL" ITEM "L" + TRIM(STRING(i-Lin)) + "C1".
 DDE SEND i-canal SOURCE STRING(fi-qtd-vda-ind) ITEM "L" + TRIM(STRING(i-Lin)) + "C2".
 DDE SEND i-canal SOURCE STRING(fi-vlr-vda-ind) ITEM "L" + TRIM(STRING(i-Lin)) + "C3".
 DDE SEND i-canal SOURCE STRING(fi-qtd-can-ind) ITEM "L" + TRIM(STRING(i-Lin)) + "C4".
 DDE SEND i-canal SOURCE STRING(fi-vlr-can-ind) ITEM "L" + TRIM(STRING(i-Lin)) + "C5".
 DDE SEND i-canal SOURCE STRING(fi-qtd-fat-ind) ITEM "L" + TRIM(STRING(i-Lin)) + "C6".
 DDE SEND i-canal SOURCE STRING(fi-vlr-fat-ind) ITEM "L" + TRIM(STRING(i-Lin)) + "C7".
 ASSIGN aux-command = '[select("L' + TRIM(STRING(i-Lin)) + 'C1:L' + TRIM(STRING(i-Lin)) + 'C11")]'.
 DDE EXECUTE i-canal COMMAND aux-command.
 /* Fonte, Tamanho, Negrito, It†lico, Sublinhado, Atachado, Cor(0=Autom†tico, 1=Preto, 2=Branco 3=Vermelho...) */
 DDE EXECUTE sys COMMAND "[format.font(~"Verdana~",8,True,False,False,False,0)]".
 ASSIGN i-Lin = i-Lin + 1.

 DDE SEND i-canal SOURCE "OUTROS" ITEM "L" + TRIM(STRING(i-Lin)) + "C1".
 DDE SEND i-canal SOURCE STRING(fi-qtd-vda-out) ITEM "L" + TRIM(STRING(i-Lin)) + "C2".
 DDE SEND i-canal SOURCE STRING(fi-vlr-vda-out) ITEM "L" + TRIM(STRING(i-Lin)) + "C3".
 DDE SEND i-canal SOURCE STRING(fi-qtd-can-out) ITEM "L" + TRIM(STRING(i-Lin)) + "C4".
 DDE SEND i-canal SOURCE STRING(fi-vlr-can-out) ITEM "L" + TRIM(STRING(i-Lin)) + "C5".
 DDE SEND i-canal SOURCE STRING(fi-qtd-fat-out) ITEM "L" + TRIM(STRING(i-Lin)) + "C6".
 DDE SEND i-canal SOURCE STRING(fi-vlr-fat-out) ITEM "L" + TRIM(STRING(i-Lin)) + "C7".
 ASSIGN aux-command = '[select("L' + TRIM(STRING(i-Lin)) + 'C1:L' + TRIM(STRING(i-Lin)) + 'C11")]'.
 DDE EXECUTE i-canal COMMAND aux-command.
 /* Fonte, Tamanho, Negrito, It†lico, Sublinhado, Atachado, Cor(0=Autom†tico, 1=Preto, 2=Branco 3=Vermelho...) */
 DDE EXECUTE sys COMMAND "[format.font(~"Verdana~",8,True,False,False,False,0)]".
 ASSIGN i-Lin = i-Lin + 1.

 DDE SEND i-canal SOURCE "GERAL" ITEM "L" + TRIM(STRING(i-Lin)) + "C1".
 DDE SEND i-canal SOURCE STRING(fi-qtd-vda-geral) ITEM "L" + TRIM(STRING(i-Lin)) + "C2".
 DDE SEND i-canal SOURCE STRING(fi-vlr-vda-geral) ITEM "L" + TRIM(STRING(i-Lin)) + "C3".
 DDE SEND i-canal SOURCE STRING(fi-qtd-can-geral) ITEM "L" + TRIM(STRING(i-Lin)) + "C4".
 DDE SEND i-canal SOURCE STRING(fi-vlr-can-geral) ITEM "L" + TRIM(STRING(i-Lin)) + "C5".
 DDE SEND i-canal SOURCE STRING(fi-qtd-fat-geral) ITEM "L" + TRIM(STRING(i-Lin)) + "C6".
 DDE SEND i-canal SOURCE STRING(fi-vlr-fat-geral) ITEM "L" + TRIM(STRING(i-Lin)) + "C7".
 ASSIGN aux-command = '[select("L' + TRIM(STRING(i-Lin)) + 'C1:L' + TRIM(STRING(i-Lin)) + 'C11")]'.
 DDE EXECUTE i-canal COMMAND aux-command.
 /* Fonte, Tamanho, Negrito, It†lico, Sublinhado, Atachado, Cor(0=Autom†tico, 1=Preto, 2=Branco 3=Vermelho...) */
 DDE EXECUTE sys COMMAND "[format.font(~"Verdana~",8,True,False,False,False,0)]".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-popula-browse B-table-Win 
PROCEDURE pi-popula-browse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* Monta Valores Vendidos */
/* ---------------------- */
{utp/ut-liter.i Calculando_Vendas *}
RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

RUN pi-vendas.

/* Monta Valores Cancelados */
/* ------------------------ */
{utp/ut-liter.i Calculando_Cancelamentos *}
RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

 RUN pi-cancelados. 

/* Monta Valores Faturados */
/* ----------------------- */
{utp/ut-liter.i Calculando_Faturamento *}
RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

 RUN pi-faturados. 

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
  DEF INPUT PARAMETER p-dt-limite        AS CHAR.
  DEF INPUT PARAMETER p-cod-estabel-ini  AS CHAR.
  DEF INPUT PARAMETER p-cod-estabel-fin  AS CHAR.
  DEF INPUT PARAMETER p-it-codigo-ini    AS CHAR.                              
  DEF INPUT PARAMETER p-it-codigo-fin    AS CHAR.                              
  DEF INPUT PARAMETER p-cod-refer-ini    AS CHAR.                              
  DEF INPUT PARAMETER p-cod-refer-fin    AS CHAR.
  DEF INPUT PARAMETER p-nome-abrev-ini   AS CHAR.                              
  DEF INPUT PARAMETER p-nome-abrev-fin   AS CHAR.
  DEF INPUT PARAMETER p-no-ab-reppri-ini AS CHAR.                              
  DEF INPUT PARAMETER p-no-ab-reppri-fin AS CHAR.
  DEF INPUT PARAMETER p-corte-comerc-ini AS CHAR.                              
  DEF INPUT PARAMETER p-corte-comerc-fin AS CHAR.
  DEF INPUT PARAMETER p-tp-relat         AS INT.
  DEF INPUT PARAMETER p-tp-pedido        AS CHAR.
  DEF INPUT PARAMETER p-lote-todos       AS LOG.
  DEF INPUT PARAMETER p-lote-pp          AS LOG.
  DEF INPUT PARAMETER p-lote-pd          AS LOG.
  DEF INPUT PARAMETER p-lote-rp          AS LOG.
  DEF INPUT PARAMETER p-lote-rd          AS LOG.
  DEF INPUT PARAMETER p-lote-sc          AS LOG.
  DEF INPUT PARAMETER p-lote-ca          AS LOG.
  DEF INPUT PARAMETER p-tp-merc          AS CHAR.
  DEF INPUT PARAMETER p-opc-artigo       AS CHAR.

  ASSIGN c-dt-limite          = p-dt-limite  
         c-cod-estabel-ini    = p-cod-estabel-ini
         c-cod-estabel-fin    = p-cod-estabel-fin
         c-it-codigo-ini      = p-it-codigo-ini    
         c-it-codigo-fin      = p-it-codigo-fin    
         c-cod-refer-ini      = p-cod-refer-ini    
         c-cod-refer-fin      = p-cod-refer-fin    
         c-nome-abrev-ini     = p-nome-abrev-ini   
         c-nome-abrev-fin     = p-nome-abrev-fin   
         c-no-ab-reppri-ini   = p-no-ab-reppri-ini 
         c-no-ab-reppri-fin   = p-no-ab-reppri-fin 
         c-corte-comerc-ini   = p-corte-comerc-ini 
         c-corte-comerc-fin   = p-corte-comerc-fin 
         i-tp-relat           = p-tp-relat
         c-tp-pedido          = p-tp-pedido
         l-lote-todos         = p-lote-todos       
         l-lote-pp            = p-lote-pp          
         l-lote-pd            = p-lote-pd          
         l-lote-rp            = p-lote-rp          
         l-lote-rd            = p-lote-rd          
         l-lote-sc            = p-lote-sc
         l-lote-ca            = p-lote-ca
         c-tp-merc            = p-tp-merc          
         c-opc-artigo         = p-opc-artigo.

 EMPTY TEMP-TABLE tt-work.
 EMPTY TEMP-TABLE tt-vda.
 EMPTY TEMP-TABLE tt-can.
 EMPTY TEMP-TABLE tt-fat.

 /* Busca Nome da Empresa */
 FIND FIRST param-global NO-LOCK NO-ERROR.
 FIND FIRST ems2cad.empresa
      WHERE empresa.ep-codigo = param-global.empresa-prin NO-LOCK NO-ERROR. 
 ASSIGN c-empresa = (IF AVAIL empresa THEN empresa.razao-social ELSE "").

 tt-work.mes:VISIBLE IN BROWSE br-work   = NO.  
 DISABLE {&list-6} WITH FRAME {&FRAME-NAME}.
 DISABLE {&list-4} WITH FRAME {&FRAME-NAME}.

 ASSIGN c-lotes = "".
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

 ASSIGN fi-qtd-vda-ljs   = 0
        fi-vlr-vda-ljs   = 0
        fi-qtd-can-ljs   = 0
        fi-vlr-can-ljs   = 0
        fi-qtd-fat-ljs   = 0
        fi-vlr-fat-ljs   = 0
        fi-qtd-vda-ind   = 0
        fi-vlr-vda-ind   = 0
        fi-qtd-can-ind   = 0
        fi-vlr-can-ind   = 0
        fi-qtd-fat-ind   = 0
        fi-vlr-fat-ind   = 0
        fi-qtd-vda-out   = 0
        fi-vlr-vda-out   = 0
        fi-qtd-can-out   = 0
        fi-vlr-can-out   = 0
        fi-qtd-fat-out   = 0
        fi-vlr-fat-out   = 0
        fi-qtd-vda-geral = 0
        fi-vlr-vda-geral = 0
        fi-qtd-can-geral = 0
        fi-vlr-can-geral = 0
        fi-qtd-fat-geral = 0
        fi-vlr-fat-geral = 0.


 RUN esapi/ret-udm.p (INPUT c-dt-limite, OUTPUT c-dia).
 ASSIGN da-dt-implant-ini = DATE('01' + SUBSTR(c-dt-limite,1,2) + SUBSTR(c-dt-limite,3,4))
        da-dt-implant-fin = DATE(c-dia + SUBSTR(c-dt-limite,1,2) + SUBSTR(c-dt-limite,3,4)).

 RUN utp/ut-acomp.p PERSISTENT SET h-acomp.

 IF i-tp-relat = 1 THEN DO:
    ASSIGN br-work:TITLE IN FRAME {&FRAME-NAME} = "RELAT‡RIO DE VENDAS / FATURAMENTO DO PERIODO" +
                                                  " " + SUBSTR(c-dt-limite,1,2) + "/" + SUBSTR(c-dt-limite,3,4).
    tt-work.dia:VISIBLE IN BROWSE br-work   = YES.  

 END.
 ELSE DO:
     ASSIGN br-work:TITLE IN FRAME {&FRAME-NAME} = "RELAT‡RIO DE VENDAS / FATURAMENTO DO EXERCICIO" +
                                                   " " + SUBSTR(c-dt-limite,3,4)
            da-dt-implant-ini = DATE('0101' + SUBSTR(c-dt-limite,3,4)).
     tt-work.dia:VISIBLE IN BROWSE br-work   = NO.  
     tt-work.mes:VISIBLE IN BROWSE br-work   = YES.  

 END.

/* br-work:TITLE-FGCOLOR IN FRAME {&FRAME-NAME} =  1.
 br-work:TITLE-BGCOLOR IN FRAME {&FRAME-NAME} = 15. */

 RUN pi-popula-browse.
 
 RUN pi-finalizar in h-acomp.
 RUN adm-open-query-cases.
 APPLY 'value-changed' TO br-work IN FRAME {&FRAME-NAME}.
 APPLY 'entry' TO br-work IN FRAME {&FRAME-NAME}.

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
  ASSIGN fi-qtd-vda = 0
         fi-vlr-vda = 0
         fi-qtd-can = 0
         fi-vlr-can = 0
         fi-qtd-fat = 0
         fi-vlr-fat = 0.
         
  FOR EACH tt-work NO-LOCK. 
      ASSIGN fi-qtd-vda = fi-qtd-vda + tt-work.qtd-vda
             fi-vlr-vda = fi-vlr-vda + tt-work.vlr-vda
             fi-qtd-can = fi-qtd-can + tt-work.qtd-can
             fi-vlr-can = fi-vlr-can + tt-work.vlr-can
             fi-qtd-fat = fi-qtd-fat + tt-work.qtd-fat
             fi-vlr-fat = fi-vlr-fat + tt-work.vlr-fat.
  END.

  ASSIGN fi-qtd-vda-geral = fi-qtd-vda + fi-qtd-vda-ljs + fi-qtd-vda-ind + fi-qtd-vda-out 
         fi-vlr-vda-geral = fi-vlr-vda + fi-vlr-vda-ljs + fi-vlr-vda-ind + fi-vlr-vda-out 
         fi-qtd-can-geral = fi-qtd-can + fi-qtd-can-ljs + fi-qtd-can-ind + fi-qtd-can-out 
         fi-vlr-can-geral = fi-vlr-can + fi-vlr-can-ljs + fi-vlr-can-ind + fi-vlr-can-out 
         fi-qtd-fat-geral = fi-qtd-fat + fi-qtd-fat-ljs + fi-qtd-fat-ind + fi-qtd-fat-out 
         fi-vlr-fat-geral = fi-vlr-fat + fi-vlr-fat-ljs + fi-vlr-fat-ind + fi-vlr-fat-out.

  DISP fi-qtd-vda 
       fi-vlr-vda 
       fi-qtd-can 
       fi-vlr-can       
       fi-qtd-fat  
       fi-vlr-fat 
       fi-qtd-vda-ljs
       fi-vlr-vda-ljs
       fi-qtd-can-ljs
       fi-vlr-can-ljs
       fi-qtd-fat-ljs
       fi-vlr-fat-ljs
       fi-qtd-vda-ind
       fi-vlr-vda-ind
       fi-qtd-can-ind
       fi-vlr-can-ind
       fi-qtd-fat-ind
       fi-vlr-fat-ind
       fi-qtd-vda-out
       fi-vlr-vda-out
       fi-qtd-can-out
       fi-vlr-can-out
       fi-qtd-fat-out
       fi-vlr-fat-out
       fi-qtd-vda-geral
       fi-vlr-vda-geral
       fi-qtd-can-geral
       fi-vlr-can-geral
       fi-qtd-fat-geral
       fi-vlr-fat-geral
       WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-vendas B-table-Win 
PROCEDURE pi-vendas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH ped-venda WHERE
         ped-venda.dt-implant   >= da-dt-implant-ini  AND
         ped-venda.dt-implant   <= da-dt-implant-fin  AND
         ped-venda.nome-abrev   >= c-nome-abrev-ini   AND
         ped-venda.nome-abrev   <= c-nome-abrev-fin   AND
         ped-venda.no-ab-reppri >= c-no-ab-reppri-ini AND
         ped-venda.no-ab-reppri <= c-no-ab-reppri-fin AND
         ped-venda.cod-sit-ped <> 5                   AND /* Exclui Suspensos e Cancelados */
         ped-venda.cod-sit-ped <> 6                   AND
         ped-venda.cod-estabel  >= c-cod-estabel-ini  AND
         ped-venda.cod-estabel  <= c-cod-estabel-fin NO-LOCK.

    RUN pi-acompanhar IN h-acomp (INPUT "   Data: " + STRING(ped-venda.dt-implant) +
                                        " Pedido: " + ped-venda.nr-pedcli).
    FIND ped-venda-ext WHERE 
         ped-venda-ext.nr-pedido = ped-venda.nr-pedido NO-LOCK NO-ERROR.
    IF NOT AVAIL ped-venda-ext THEN NEXT.

    FIND natur-oper WHERE
         natur-oper.nat-operacao = ped-venda.nat-operacao NO-LOCK NO-ERROR.
    IF NOT AVAIL natur-oper THEN NEXT.

    IF c-tp-pedido <> "Todos" AND c-tp-pedido <> ped-venda-ext.tp-pedido THEN NEXT.

    IF c-tp-pedido = "Todos" AND  natur-oper.cod-esp <> "DP" AND 
       LOOKUP(ped-venda-ext.tp-pedido,"AMOSTRA,AMOSTRA EXPORTAÄ«O") <> 0 THEN NEXT.

    IF (c-tp-merc = "E" AND ped-venda-ext.tp-pedido <> "EXPORTACAO") OR
       (c-tp-merc = "I" AND ped-venda-ext.tp-pedido  = "EXPORTACAO")  THEN NEXT.


    ASSIGN c-dia = STRING(DAY(ped-venda.dt-implant), "99")
           de-total = 0.
    IF i-tp-relat = 2 THEN
       ASSIGN c-dia = STRING(MONTH(ped-venda.dt-implant), "99").

    FOR EACH ped-item OF ped-venda WHERE 
             ped-item.it-codigo   >= c-it-codigo-ini AND
             ped-item.it-codigo   <= c-it-codigo-fin AND
             ped-item.cod-refer   >= c-cod-refer-ini AND 
             ped-item.cod-refer   <= c-cod-refer-fin AND
             ped-item.cod-sit-item < 5 NO-LOCK, /* Exclui 5-Suspenso/6-Cancelado/7-Outros */
       FIRST ped-item-ext OF ped-item WHERE
             LOOKUP(SUBSTR(ped-item-ext.lote,1,2),c-lotes) <> 0  AND 
             ped-item-ext.corte-comerc >= c-corte-comerc-ini     AND
             ped-item-ext.corte-comerc <= c-corte-comerc-fin NO-LOCK.

        FIND item-ext WHERE
             item-ext.it-codigo = ped-item.it-codigo NO-LOCK NO-ERROR.
        /*IF c-opc-artigo <> 'A' THEN 
           IF AVAIL item-ext AND
              (item-ext.indigo = YES AND c-opc-artigo <> "I") OR
              (item-ext.indigo = NO  AND c-opc-artigo <> "O") THEN NEXT.*/

        ACCUMULATE ped-item.qt-pedida (TOTAL).
        ASSIGN de-total = de-total + (ped-item.qt-pedida * ped-item.vl-preori).
    END.
    IF (de-total + ACCUM TOTAL ped-item.qt-pedida) = 0  THEN NEXT.

    ASSIGN c-tipo = "Normal".
    IF natur-oper.cod-esp <> "DP" THEN DO: /* Lojas Tear e Rem.Industrializaá∆o */
       IF ped-venda-ext.tp-pedido <> "Rem.Industrializaá∆o" THEN DO.
          IF LOOKUP(ped-venda-ext.tp-pedido,"Doaá∆o,bonificaá∆o") <> 0 THEN DO:
             ASSIGN fi-qtd-vda-out = fi-qtd-vda-out + ACCUM TOTAL ped-item.qt-pedida
                    fi-vlr-vda-out = fi-vlr-vda-out + de-total
                    c-tipo         = "Outros".
          END.
          ELSE DO:
             IF ped-venda-ext.tp-pedido = "Normal" OR
                ped-venda-ext.tp-pedido = "Reserva" THEN DO:
                ASSIGN fi-qtd-vda-ljs = fi-qtd-vda-ljs + ACCUM TOTAL ped-item.qt-pedida
                       fi-vlr-vda-ljs = fi-vlr-vda-ljs + de-total
                       c-tipo         = "Loja".
                FIND tt-lojas WHERE
                     tt-lojas.loja  = ped-venda.nome-abrev NO-ERROR.
                IF NOT AVAIL tt-lojas THEN DO:
                   CREATE tt-lojas.
                   ASSIGN tt-lojas.loja  = ped-venda.nome-abrev.
                END.
                ASSIGN tt-lojas.qtd-vda = tt-lojas.qtd-vda + ACCUM TOTAL ped-item.qt-pedida
                       tt-lojas.vlr-vda = tt-lojas.vlr-vda + de-total. 
             END.
          END.
       END.
       ELSE
          ASSIGN fi-qtd-vda-ind = fi-qtd-vda-ind + ACCUM TOTAL ped-item.qt-pedida
                 fi-vlr-vda-ind = fi-vlr-vda-ind + de-total
                 c-tipo         = "Indl".
    END.
    /* Gera Acumulados NORMAIS */
    /*                         */
    IF c-tipo = "Normal" THEN DO:
       FIND tt-work WHERE
            tt-work.dia  = c-dia NO-ERROR.
       IF NOT AVAIL tt-work THEN DO:
          CREATE tt-work.
          ASSIGN tt-work.dia  = c-dia
                 tt-work.mes  = SUBSTR("JANFEVMARABRMAIJUNJULAGOSETOUTNOVDEZ",INT(c-dia) * 3 - 2,3).
       END.
       ASSIGN tt-work.qtd-vda = tt-work.qtd-vda + ACCUM TOTAL ped-item.qt-pedida
              tt-work.vlr-vda = tt-work.vlr-vda + de-total.
    END.
    
    /* Grava Detalhamento das Vendas por Dia */
    /*                                       */
    FIND tt-vda WHERE
         tt-vda.tipo      = c-tipo AND
         tt-vda.dia       = c-dia  AND
         tt-vda.nr-pedcli = ped-venda.nr-pedcli NO-ERROR.
    IF NOT AVAIL tt-vda THEN DO:
       CREATE tt-vda.
       ASSIGN tt-vda.tipo         = c-tipo
              tt-vda.dia          = c-dia
              tt-vda.nr-pedcli    = ped-venda.nr-pedcli
              tt-vda.nr-pedrep    = ped-venda.nr-pedrep
              tt-vda.nome-abrev   = ped-venda.nome-abrev
              tt-vda.no-ab-reppri = ped-venda.no-ab-reppri
              tt-vda.dt-implant   = ped-venda.dt-implant
              tt-vda.dt-entrega   = ped-venda.dt-entrega.

       CASE ped-venda.cod-sit-ped.
            WHEN 1 THEN ASSIGN tt-vda.sit = 'ABE'.
            WHEN 2 THEN ASSIGN tt-vda.sit = 'ATP'.
            WHEN 3 THEN ASSIGN tt-vda.sit = 'ATT'.
            WHEN 4 THEN ASSIGN tt-vda.sit = 'PEN'.
            WHEN 5 THEN ASSIGN tt-vda.sit = 'SUS'.
            WHEN 6 THEN ASSIGN tt-vda.sit = 'CAN'.
       END CASE.
    END.
    ASSIGN tt-vda.qtde  = tt-vda.qtde  + ACCUM TOTAL ped-item.qt-pedida
           tt-vda.valor = tt-vda.valor + de-total.
END.


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
  {src/adm/template/snd-list.i "tt-work"}

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
/*
  FIND ITEM WHERE
       ITEM.it-codigo = tt-itens.it-codigo NO-LOCK NO-ERROR.

  RETURN item.desc-item.   /* Function return value. */
*/
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

