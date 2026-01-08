&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r11 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          mgcad            PROGRESS
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
DEF VAR c-empresa LIKE empresa.razao-social.
DEFINE VAR i-canal     AS INTEGER.
DEFINE VAR sys         AS INTEGER.
DEFINE VAR c-lin       AS CHARACTER FORMAT "x(500)".
DEFINE VAR aux-command AS CHAR FORMAT "x(100)".
DEFINE VAR arq-saida   AS CHAR FORMAT "x(45)".



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
DEF VAR l-ok               AS LOG.
DEF VAR i-lin              AS INT.
DEF VAR i-pag              AS INT.
DEF VAR l-barra-vendidos   AS LOG.
DEF VAR l-barra-cancelados AS LOG.
DEF VAR l-barra-faturados  AS LOG.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-work

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-work

/* Definitions for BROWSE br-work                                       */
&Scoped-define FIELDS-IN-QUERY-br-work tt-work.dia tt-work.qtd-vda tt-work.vlr-vda tt-work.qtd-can tt-work.vlr-can tt-work.qtd-fat tt-work.vlr-fat   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-work   
&Scoped-define SELF-NAME br-work
&Scoped-define OPEN-QUERY-br-work RUN pi-totais. OPEN QUERY {&SELF-NAME} FOR EACH tt-work NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-work tt-work
&Scoped-define FIRST-TABLE-IN-QUERY-br-work tt-work


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-br-work}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-work bt-imprime bt-excel bt-Grafico-qtd ~
bt-Grafico-vlr RECT-60 
&Scoped-Define DISPLAYED-OBJECTS fi-qtd-vda fi-vlr-vda fi-qtd-can ~
fi-vlr-can fi-qtd-fat fi-vlr-fat fi-qtd-vda-ljs fi-vlr-vda-ljs ~
fi-qtd-can-ljs fi-vlr-can-ljs fi-qtd-fat-ljs fi-vlr-fat-ljs fi-qtd-vda-ind ~
fi-vlr-vda-ind fi-qtd-can-ind fi-vlr-can-ind fi-qtd-fat-ind fi-vlr-fat-ind ~
fi-qtd-vda-out fi-vlr-vda-out fi-qtd-can-out fi-vlr-can-out fi-qtd-fat-out ~
fi-vlr-fat-out fi-qtd-vda-geral fi-vlr-vda-geral fi-qtd-can-geral ~
fi-vlr-can-geral fi-qtd-fat-geral fi-vlr-fat-geral 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-6 bt-imprime bt-excel bt-Grafico-qtd bt-Grafico-vlr 

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
DEFINE BUTTON bt-excel 
     IMAGE-UP FILE "image/excel.bmp":U
     LABEL "Button 2" 
     SIZE 5 BY 1.21 TOOLTIP "Gerar Planilha".

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

DEFINE VARIABLE fi-qtd-can AS DECIMAL FORMAT "       -ZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.57 BY .88
     FGCOLOR 6  NO-UNDO.

DEFINE VARIABLE fi-qtd-can-geral AS DECIMAL FORMAT "       -ZZ,ZZZ,ZZ9.99":U INITIAL 0 
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

DEFINE VARIABLE fi-qtd-fat AS DECIMAL FORMAT "    -ZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE fi-qtd-fat-geral AS DECIMAL FORMAT "    -ZZ,ZZZ,ZZ9.99":U INITIAL 0 
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

DEFINE VARIABLE fi-qtd-vda AS DECIMAL FORMAT "    -ZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12.43 BY .88
     FGCOLOR 4  NO-UNDO.

DEFINE VARIABLE fi-qtd-vda-geral AS DECIMAL FORMAT "    -ZZ,ZZZ,ZZ9.99":U INITIAL 0 
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

DEFINE VARIABLE fi-vlr-can AS DECIMAL FORMAT "       -ZZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.43 BY .88
     FGCOLOR 6  NO-UNDO.

DEFINE VARIABLE fi-vlr-can-geral AS DECIMAL FORMAT "       -ZZZ,ZZZ,ZZ9.99":U INITIAL 0 
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

DEFINE VARIABLE fi-vlr-vda AS DECIMAL FORMAT "       -ZZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.29 BY .88
     FGCOLOR 4  NO-UNDO.

DEFINE VARIABLE fi-vlr-vda-geral AS DECIMAL FORMAT "       -ZZZ,ZZZ,ZZ9.99":U INITIAL 0 
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
      tt-work.dia      COLUMN-LABEL "Mes" WIDTH 3
      tt-work.qtd-vda  COLUMN-LABEL "Metros Vendidos"    FORMAT ">,>>>,>>9.99"   WIDTH 13    COLUMN-FGCOLOR 4
      tt-work.vlr-vda  COLUMN-LABEL "Valores Vendidos"   FORMAT ">>>,>>>,>>9.99" WIDTH 14    COLUMN-FGCOLOR 4
      tt-work.qtd-can  COLUMN-LABEL "Metros Cancelados"  FORMAT ">,>>>,>>9.99"   WIDTH 13.2  COLUMN-FGCOLOR 6
      tt-work.vlr-can  COLUMN-LABEL "Valores Cancelados" FORMAT ">>>,>>>,>>9.99" WIDTH 14    COLUMN-FGCOLOR 6
      tt-work.qtd-fat  COLUMN-LABEL "Metros Faturados"   FORMAT ">,>>>,>>9.99"   WIDTH 12.3  COLUMN-FGCOLOR 9
      tt-work.vlr-fat  COLUMN-LABEL "Valores Faturados"  FORMAT ">>>,>>>,>>9.99" WIDTH 14.2  COLUMN-FGCOLOR 9
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 90.43 BY 10.46
         FONT 1
         TITLE "" ROW-HEIGHT-CHARS .71.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br-work AT ROW 1.08 COL 2
     bt-imprime AT ROW 1.21 COL 93.72
     bt-excel AT ROW 2.5 COL 93.72
     bt-Grafico-qtd AT ROW 3.71 COL 93.72
     bt-Grafico-vlr AT ROW 4.92 COL 93.72
     fi-qtd-vda AT ROW 11.75 COL 5.29 COLON-ALIGNED NO-LABEL
     fi-vlr-vda AT ROW 11.75 COL 17.72 COLON-ALIGNED NO-LABEL
     fi-qtd-can AT ROW 11.75 COL 32.14 COLON-ALIGNED NO-LABEL
     fi-vlr-can AT ROW 11.75 COL 45.86 COLON-ALIGNED NO-LABEL
     fi-qtd-fat AT ROW 11.75 COL 60.43 COLON-ALIGNED NO-LABEL
     fi-vlr-fat AT ROW 11.75 COL 73.43 COLON-ALIGNED NO-LABEL
     fi-qtd-vda-ljs AT ROW 12.75 COL 5.29 COLON-ALIGNED NO-LABEL
     fi-vlr-vda-ljs AT ROW 12.75 COL 17.72 COLON-ALIGNED NO-LABEL
     fi-qtd-can-ljs AT ROW 12.75 COL 32.14 COLON-ALIGNED NO-LABEL
     fi-vlr-can-ljs AT ROW 12.75 COL 45.86 COLON-ALIGNED NO-LABEL
     fi-qtd-fat-ljs AT ROW 12.75 COL 60.43 COLON-ALIGNED NO-LABEL
     fi-vlr-fat-ljs AT ROW 12.75 COL 73.43 COLON-ALIGNED NO-LABEL
     fi-qtd-vda-ind AT ROW 13.75 COL 5.29 COLON-ALIGNED NO-LABEL
     fi-vlr-vda-ind AT ROW 13.75 COL 17.72 COLON-ALIGNED NO-LABEL
     fi-qtd-can-ind AT ROW 13.75 COL 32.14 COLON-ALIGNED NO-LABEL
     fi-vlr-can-ind AT ROW 13.75 COL 45.86 COLON-ALIGNED NO-LABEL
     fi-qtd-fat-ind AT ROW 13.75 COL 60.43 COLON-ALIGNED NO-LABEL
     fi-vlr-fat-ind AT ROW 13.75 COL 73.43 COLON-ALIGNED NO-LABEL
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
     RECT-60 AT ROW 1 COL 92.86
     "Normal" VIEW-AS TEXT
          SIZE 4.86 BY .54 AT ROW 11.96 COL 2.29
     "Lojas" VIEW-AS TEXT
          SIZE 4.14 BY .54 AT ROW 13.04 COL 2.72
     "Industria" VIEW-AS TEXT
          SIZE 5.72 BY .54 AT ROW 14.08 COL 1.14
     "Geral" VIEW-AS TEXT
          SIZE 4.14 BY .54 AT ROW 15.79 COL 2.72
     "Outros" VIEW-AS TEXT
          SIZE 4.57 BY .54 AT ROW 14.96 COL 2.43
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
   NOT-VISIBLE Size-to-Fit                                              */
/* BROWSE-TAB br-work 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON bt-excel IN FRAME F-Main
   6                                                                    */
/* SETTINGS FOR BUTTON bt-Grafico-qtd IN FRAME F-Main
   6                                                                    */
/* SETTINGS FOR BUTTON bt-Grafico-vlr IN FRAME F-Main
   6                                                                    */
/* SETTINGS FOR BUTTON bt-imprime IN FRAME F-Main
   6                                                                    */
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
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-work
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
   
 /* bloco principal do programa */
 find first param-global no-lock no-error.
 find first empresa
      where empresa.ep-codigo = param-global.empresa-prin no-lock no-error. 
    
 ASSIGN c-empresa = (IF AVAIL empresa THEN empresa.razao-social ELSE "").
    
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

 EMPTY TEMP-TABLE tt-atributos.
 EMPTY TEMP-TABLE tt-sets.
 EMPTY TEMP-TABLE tt-points-2.
 EMPTY TEMP-TABLE tt-dados.
 EMPTY TEMP-TABLE tt-erros.

 /* Configuraá∆o Geral do Grafico */
 /*                               */
 CREATE tt-atributos.
 ASSIGN tt-atributos.cod-versao-integracao = 3
        tt-atributos.graphtype             = 3
        tt-atributos.graphtitle            = c-titulo-grafico + " " + 
                                             SUBSTR(c-dt-limite,1,2) + "/" + SUBSTR(c-dt-limite,3,4) + '.'
        tt-atributos.lefttitle             = 'Quantidade em METROS.'
        tt-atributos.lefttitlestyle        = 1
        tt-atributos.bottomtitle           = 'D I A S'
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
    ASSIGN tt-points-2.NumPoint  = i-point
           tt-points-2.NumGraph  = 1
           tt-points-2.labeltext = tt-work.dia.

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

 EMPTY TEMP-TABLE tt-atributos.
 EMPTY TEMP-TABLE tt-sets.
 EMPTY TEMP-TABLE tt-points-2.
 EMPTY TEMP-TABLE tt-dados.
 EMPTY TEMP-TABLE tt-erros.

 /* Configuraá∆o Geral do Grafico */
 /*                               */
 CREATE tt-atributos.
 ASSIGN tt-atributos.cod-versao-integracao = 3
        tt-atributos.graphtype             = 3
        tt-atributos.graphtitle            = c-titulo-grafico + " " +
                                             SUBSTR(c-dt-limite,1,2) + "/" + SUBSTR(c-dt-limite,3,4) + '.'
        tt-atributos.lefttitle             = 'Valores em REAL.'
        tt-atributos.lefttitlestyle        = 1

        tt-atributos.bottomtitle           = 'D I A S'
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
    ASSIGN tt-points-2.NumPoint  = i-point
           tt-points-2.NumGraph  = 1
           tt-points-2.labeltext = tt-work.dia.

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
    PUT "TEAR TEXTIL INDUSTRIA E COMERCIO LTDA."  AT   1
        "DATA: "                                  AT  52
        STRING(TODAY,"99/99/9999") FORMAT "X(10)" AT  58
        "HORA: "                                  AT  80
        STRING(TIME,"hh:mm:ss")                   AT  86
        "PAG:"                                    AT 115
        i-pag FORMAT ">>>"                        AT 120
        SKIP(1).

    PUT "RELATORIO DE VENDAS / FATURAMENTO DO PERIODO" AT 31.
    PUT SUBSTR(c-dt-limite,1,2) + "/" + SUBSTR(c-dt-limite,3,4) AT 76 SKIP(1).

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
 SYSTEM-DIALOG PRINTER-SETUP UPDATE l-ok.
 IF l-ok THEN DO:
    OUTPUT TO PRINTER CONVERT TARGET "ibm850" PAGED PAGE-SIZE 62.
    PUT CONTROL "~033E~033(s16H".    
    
    ASSIGN i-pag      =  1
           i-lin      = 99.
    FOR EACH tt-work WHERE NO-LOCK.
        
        IF i-lin > 62 THEN DO:
           RUN pi-imp-cabec.
           ASSIGN i-lin = 7.
        END.
        ASSIGN c-data-per = tt-work.dia + "/" + SUBSTR(c-dt-limite,1,2) + "/" + SUBSTR(c-dt-limite,3,4).

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

      PUT "TOT.GERAL." AT 1.
      PUT fi-qtd-vda-geral FORMAT ">>>>,>>>,>>9.99" AT  13.
      PUT fi-vlr-vda-geral FORMAT ">>>>,>>>,>>9.99" AT  31.
      PUT fi-qtd-can-geral FORMAT ">>>>,>>>,>>9.99" AT  50.
      PUT fi-vlr-can-geral FORMAT ">>>>,>>>,>>9.99" AT  70.
      PUT fi-qtd-fat-geral FORMAT ">>>>,>>>,>>9.99" AT  88.
      PUT fi-vlr-fat-geral FORMAT ">>>>,>>>,>>9.99" AT 108 SKIP.
   END.
   OUTPUT CLOSE.
 END.
  



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
 ASSIGN c-Lin = c-empresa + "             " + " VENDAS & FATURAMENTO DO PERIODO "  + SUBSTR(c-dt-limite,1,2) + "/" + SUBSTR(c-dt-limite,3,4). 
 DDE SEND i-canal SOURCE c-Lin ITEM "L1C1".

 DDE EXECUTE i-canal COMMAND '[select("L1C1:L1C10")]'.
 /* Fonte, Tamanho, Negrito, It†lico, Sublinhado, Atachado, Cor(0=Autom†tico, 1=Preto, 2=Branco 3=Vermelho...) */
 DDE EXECUTE sys COMMAND "[format.font(~"Verdana~",10,True,False,False,False,3)]".

 /* Cabeáalho dos Dados */
 DDE SEND i-canal SOURCE "D I A S"            ITEM "L3C1".
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
     DDE SEND i-canal SOURCE STRING(tt-work.dia)           ITEM "L" + TRIM(STRING(i-Lin)) + "C1". 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-processa B-table-Win 
PROCEDURE pi-processa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER p-dt-limite        AS CHAR.
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

 ASSIGN br-work:TITLE IN FRAME {&FRAME-NAME} = "RELAT‡RIO DE VENDAS / FATURAMENTO DO PERIODO" +
                                               " " + SUBSTR(c-dt-limite,1,2) + "/" + SUBSTR(c-dt-limite,3,4).

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

