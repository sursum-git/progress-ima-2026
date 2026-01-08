&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESSP0172 2.04.00.999}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
/* Global Variable Definitions ---                                       */
DEF NEW GLOBAL SHARED VAR gr-ob-etiqueta  AS ROWID NO-UNDO.
DEF NEW GLOBAL SHARED VAR gr-corte-comerc AS ROWID NO-UNDO.
DEF NEW GLOBAL SHARED VAR gr-item AS ROWID NO-UNDO.

/* Temp Table Definitions ---                                           */
DEF TEMP-TABLE tt-itens 
    FIELD it-codigo     LIKE ob-sl-estoq-per.it-codigo
    FIELD qtd-inicial   LIKE ob-sl-estoq-per.qtd-inicial    
    FIELD qtd-entr-est  LIKE ob-sl-estoq-per.qtd-entr-est      
    FIELD qtd-transf    LIKE ob-sl-estoq-per.qtd-transf    
    FIELD qtd-devolvida LIKE ob-sl-estoq-per.qtd-devolvida 
    FIELD qtd-faturada  LIKE ob-sl-estoq-per.qtd-faturada  
    FIELD qtd-final     LIKE ob-sl-estoq-per.qtd-final     
    FIELD qtd-real      LIKE ob-sl-estoq-per.qtd-real      
    INDEX indice1 IS PRIMARY it-codigo.

DEF TEMP-TABLE tt-movto
    FIELD cod-refer     LIKE ob-sl-estoq-per.cod-refer
    FIELD nr-lote       LIKE ob-sl-estoq-per.nr-lote
    FIELD qtd-inicial   LIKE ob-sl-estoq-per.qtd-inicial    
    FIELD qtd-entr-est  LIKE ob-sl-estoq-per.qtd-entr-est      
    FIELD qtd-transf    LIKE ob-sl-estoq-per.qtd-transf    
    FIELD qtd-devolvida LIKE ob-sl-estoq-per.qtd-devolvida 
    FIELD qtd-faturada  LIKE ob-sl-estoq-per.qtd-faturada  
    FIELD qtd-final     LIKE ob-sl-estoq-per.qtd-final     
    FIELD qtd-real      LIKE ob-sl-estoq-per.qtd-real      
    INDEX indice1 cod-refer nr-lote.

DEF TEMP-TABLE tt-corte
    FIELD corte-comerc  LIKE ob-sl-estoq-per.corte-comerc
    FIELD cod-qualid    LIKE ob-sl-estoq-per.cod-qualid
    FIELD qtd-inicial   LIKE ob-sl-estoq-per.qtd-inicial    
    FIELD qtd-entr-est  LIKE ob-sl-estoq-per.qtd-entr-est      
    FIELD qtd-transf    LIKE ob-sl-estoq-per.qtd-transf    
    FIELD qtd-devolvida LIKE ob-sl-estoq-per.qtd-devolvida 
    FIELD qtd-faturada  LIKE ob-sl-estoq-per.qtd-faturada  
    FIELD qtd-final     LIKE ob-sl-estoq-per.qtd-final     
    FIELD qtd-real      LIKE ob-sl-estoq-per.qtd-real      
    INDEX indice1 corte-comerc cod-qualid.

DEF TEMP-TABLE tt-work /* Relatorio */
    FIELD it-codigo     LIKE ob-sl-estoq-per.it-codigo
    FIELD cod-refer     LIKE ob-sl-estoq-per.cod-refer
    FIELD nr-lote       LIKE ob-sl-estoq-per.nr-lote
    FIELD qtd-inicial   LIKE ob-sl-estoq-per.qtd-inicial    
    FIELD qtd-entr-est  LIKE ob-sl-estoq-per.qtd-entr-est      
    FIELD qtd-transf    LIKE ob-sl-estoq-per.qtd-transf    
    FIELD qtd-devolvida LIKE ob-sl-estoq-per.qtd-devolvida 
    FIELD qtd-faturada  LIKE ob-sl-estoq-per.qtd-faturada  
    FIELD qtd-final     LIKE ob-sl-estoq-per.qtd-final     
    FIELD qtd-real      LIKE ob-sl-estoq-per.qtd-real    
    INDEX indice1 it-codigo nr-lote cod-refer. 

/* --- Local Variable Definitions --- */
DEF VAR h-acomp           AS HANDLE NO-UNDO.
DEF VAR h-query           AS HANDLE.
DEF VAR c-empresa         AS CHAR.
DEF BUFFER b-tt-itens FOR tt-itens.

DEF VAR c-desc-item   LIKE ITEM.desc-item.
DEF VAR c-desc-corte  LIKE corte-comerc.descricao.
DEF VAR c-it-codigo   LIKE ob-sl-estoq-per.it-codigo.
DEF VAR c-lotes       AS CHAR FORMAT "x(18)".
DEF VAR arq-saida     AS CHAR FORMAT "x(45)".

DEF VAR c-dia             AS CHAR.
DEF VAR da-dt-emissao-ini LIKE ob-etiqueta.dt-emissao.
DEF VAR da-dt-emissao-fin LIKE ob-etiqueta.dt-emissao.

DEF VAR i-lin      AS INT.
DEF VAR i-pag      AS INT.
DEF VAR c-item-ini AS CHAR FORMAT "x(6)" INITIAL "".
DEF VAR c-item-fin AS CHAR FORMAT "x(6)" INITIAL "ZZZZZZ".
DEF VAR i-tipo-rel AS INT INITIAL 2.
DEF VAR l-item     AS LOG INITIAL YES.

/* Variaveis da Rotina de ImpressÆo */
DEFINE VAR i-saida             AS INT.
DEFINE VAR c-saida             AS CHAR.
DEFINE VAR i-num-copias        AS INT.


/* Variavies de Parƒmetros */
DEFINE VAR c-periodo          AS CHAR.
DEFINE VAR c-it-codigo-ini    LIKE ob-etiqueta.it-codigo     INIT "".
DEFINE VAR c-it-codigo-fin    LIKE ob-etiqueta.it-codigo     INIT "ZZZZZZ".
DEFINE VAR c-cod-refer-ini    LIKE ob-etiqueta.cod-refer     INIT "". 
DEFINE VAR c-cod-refer-fin    LIKE ob-etiqueta.cod-refer     INIT "ZZZZZZZ".
DEFINE VAR c-cod-qualid-ini   LIKE ob-etiqueta.cod-qualid    INIT "".
DEFINE VAR c-cod-qualid-fin   LIKE ob-etiqueta.cod-qualid    INIT "Z".
DEFINE VAR c-cod-obsoleto-ini LIKE ref-item-ext.cod-obsoleto INIT "".
DEFINE VAR c-cod-obsoleto-fin LIKE ref-item-ext.cod-obsoleto INIT "Z".
DEFINE VAR c-corte-comerc-ini LIKE ob-etiqueta.corte-comerc  INIT "".
DEFINE VAR c-corte-comerc-fin LIKE ob-etiqueta.corte-comerc  INIT "Z".
DEFINE VAR c-cod-depos        LIKE saldo-estoq.cod-depos     INIT "EXP".
DEFINE VAR l-lote-todos       AS LOG INIT YES.
DEFINE VAR l-lote-pp          AS LOG INIT YES.
DEFINE VAR l-lote-pd          AS LOG INIT YES.
DEFINE VAR l-lote-rp          AS LOG INIT YES.
DEFINE VAR l-lote-rd          AS LOG INIT YES.
DEFINE VAR l-lote-sc          AS LOG INIT YES.
DEFINE VAR l-lote-ca          AS LOG INIT YES.
DEFINE VAR i-tp-tecido        AS INT INITIAL 3.
DEFINE VAR l-ok               AS LOG.

/* Variaveis para o Excel */
DEFINE VAR i-canal     AS INTEGER.
DEFINE VAR sys         AS INTEGER.
DEFINE VAR c-lin       AS CHARACTER FORMAT "x(500)".
DEFINE VAR aux-command AS CHAR FORMAT "x(100)".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME br-corte

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-corte tt-itens tt-movto

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br-corte                                      */
&Scoped-define FIELDS-IN-QUERY-br-corte fn-desc-corte() @ c-desc-corte tt-corte.cod-qualid tt-corte.qtd-inicial tt-corte.qtd-entr-est tt-corte.qtd-transf tt-corte.qtd-devolvida tt-corte.qtd-faturada tt-corte.qtd-final tt-corte.qtd-real   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-corte   
&Scoped-define SELF-NAME br-corte
&Scoped-define QUERY-STRING-br-corte FOR EACH tt-corte WHERE                                  NO-LOCK BY tt-corte.corte-comerc
&Scoped-define OPEN-QUERY-br-corte OPEN QUERY {&SELF-NAME} FOR EACH tt-corte WHERE                                  NO-LOCK BY tt-corte.corte-comerc.
&Scoped-define TABLES-IN-QUERY-br-corte tt-corte
&Scoped-define FIRST-TABLE-IN-QUERY-br-corte tt-corte


/* Definitions for BROWSE br-itens                                      */
&Scoped-define FIELDS-IN-QUERY-br-itens tt-itens.it-codigo fn-desc-item() @ c-desc-item   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-itens   
&Scoped-define SELF-NAME br-itens
&Scoped-define OPEN-QUERY-br-itens RUN pi-tot-itens. OPEN QUERY {&SELF-NAME} FOR EACH tt-itens WHERE NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-itens tt-itens
&Scoped-define FIRST-TABLE-IN-QUERY-br-itens tt-itens


/* Definitions for BROWSE br-movto                                      */
&Scoped-define FIELDS-IN-QUERY-br-movto tt-movto.cod-refer tt-movto.nr-lote tt-movto.qtd-inicial tt-movto.qtd-entr-est tt-movto.qtd-transf tt-movto.qtd-devolvida tt-movto.qtd-faturada tt-movto.qtd-final tt-movto.qtd-real (tt-movto.qtd-final - tt-movto.qtd-real)   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-movto   
&Scoped-define SELF-NAME br-movto
&Scoped-define QUERY-STRING-br-movto FOR EACH tt-movto WHERE                                  NO-LOCK BY tt-movto.cod-refer
&Scoped-define OPEN-QUERY-br-movto OPEN QUERY {&SELF-NAME} FOR EACH tt-movto WHERE                                  NO-LOCK BY tt-movto.cod-refer.
&Scoped-define TABLES-IN-QUERY-br-movto tt-movto
&Scoped-define FIRST-TABLE-IN-QUERY-br-movto tt-movto


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-br-corte}~
    ~{&OPEN-QUERY-br-movto}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-5 rt-buttom br-itens br-movto bt-param ~
bt-vapara bt-excel bt-imprime br-corte bt-exit bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS fi-tot-ini fi-tot-ini-ger fi-tot-ent ~
fi-tot-ent-ger fi-tot-transf fi-tot-transf-ger fi-tot-dev fi-tot-dev-ger ~
fi-tot-fat fi-tot-fat-ger fi-tot-saldo fi-tot-saldo-ger fi-tot-real ~
fi-tot-real-ger 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-6 bt-excel 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-desc-corte C-Win 
FUNCTION fn-desc-corte RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-desc-item C-Win 
FUNCTION fn-desc-item RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ajuda 
     IMAGE-UP FILE "image/im-hel.bmp":U
     LABEL "&Ajuda" 
     SIZE 4.86 BY 1.29 TOOLTIP "Help on Line"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-excel 
     IMAGE-UP FILE "image/excel.bmp":U
     LABEL "Button 2" 
     SIZE 5 BY 1.25.

DEFINE BUTTON bt-exit AUTO-END-KEY 
     IMAGE-UP FILE "image/im-exi.bmp":U
     LABEL "" 
     SIZE 4.86 BY 1.29 TOOLTIP "Fechar Programa"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-imprime 
     IMAGE-UP FILE "image/im-printer.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Imprimir a Evolu‡Æo do Estoque Acabado".

DEFINE BUTTON bt-param AUTO-GO 
     IMAGE-UP FILE "image/im-fil.bmp":U
     LABEL "&Sair" 
     SIZE 5 BY 1.25 TOOLTIP "Parƒmetros"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-vapara AUTO-GO 
     IMAGE-UP FILE "image/im-vapra.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Posicionar no Item"
     BGCOLOR 8 FONT 10.

DEFINE VARIABLE fi-tot-dev AS DECIMAL FORMAT "z,zzz,zz9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.29 BY .88
     FGCOLOR 4 FONT 2 NO-UNDO.

DEFINE VARIABLE fi-tot-dev-ger AS DECIMAL FORMAT "-z,zzz,zz9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.72 BY .88
     FONT 2 NO-UNDO.

DEFINE VARIABLE fi-tot-ent AS DECIMAL FORMAT "z,zzz,zz9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.29 BY .88
     FGCOLOR 4 FONT 2 NO-UNDO.

DEFINE VARIABLE fi-tot-ent-ger AS DECIMAL FORMAT "-z,zzz,zz9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.72 BY .88
     FONT 2 NO-UNDO.

DEFINE VARIABLE fi-tot-fat AS DECIMAL FORMAT "z,zzz,zz9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.29 BY .88
     FGCOLOR 4 FONT 2 NO-UNDO.

DEFINE VARIABLE fi-tot-fat-ger AS DECIMAL FORMAT "-z,zzz,zz9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.72 BY .88
     FONT 2 NO-UNDO.

DEFINE VARIABLE fi-tot-ini AS DECIMAL FORMAT "Z,ZZZ,ZZ9.99":R INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.29 BY .88
     FGCOLOR 4 FONT 2 NO-UNDO.

DEFINE VARIABLE fi-tot-ini-ger AS DECIMAL FORMAT "-Z,ZZZ,ZZ9.99":R INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.72 BY .88
     FONT 2 NO-UNDO.

DEFINE VARIABLE fi-tot-real AS DECIMAL FORMAT "z,zzz,zz9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.29 BY .88
     FGCOLOR 4 FONT 2 NO-UNDO.

DEFINE VARIABLE fi-tot-real-ger AS DECIMAL FORMAT "-z,zzz,zz9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.72 BY .88
     FONT 2 NO-UNDO.

DEFINE VARIABLE fi-tot-saldo AS DECIMAL FORMAT "z,zzz,zz9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.29 BY .88
     FGCOLOR 4 FONT 2 NO-UNDO.

DEFINE VARIABLE fi-tot-saldo-ger AS DECIMAL FORMAT "-z,zzz,zz9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.72 BY .88
     FONT 2 NO-UNDO.

DEFINE VARIABLE fi-tot-transf AS DECIMAL FORMAT "z,zzz,zz9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.29 BY .88
     FGCOLOR 4 FONT 2 NO-UNDO.

DEFINE VARIABLE fi-tot-transf-ger AS DECIMAL FORMAT "-z,zzz,zz9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.72 BY .88
     FONT 2 NO-UNDO.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 42 BY 9.25
     BGCOLOR 8 .

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 6.72 BY 20
     BGCOLOR 8 FGCOLOR 0 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-corte FOR 
      tt-corte SCROLLING.

DEFINE QUERY br-itens FOR 
      tt-itens SCROLLING.

DEFINE QUERY br-movto FOR 
      tt-movto SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-corte
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-corte C-Win _FREEFORM
  QUERY br-corte NO-LOCK DISPLAY
      fn-desc-corte() @ c-desc-corte       WIDTH 7 COLUMN-LABEL "Corte"
      tt-corte.cod-qualid    FORMAT "x(1)" WIDTH 2 COLUMN-LABEL "Q"
      tt-corte.qtd-inicial   FORMAT ">,>>>,>>9.99" COLUMN-LABEL "Estoque Inicial"
      tt-corte.qtd-entr-est  FORMAT ">,>>>,>>9.99" COLUMN-LABEL "Entrada"
      tt-corte.qtd-transf    FORMAT ">,>>>,>>9.99" COLUMN-LABEL "Transferencia"   
      tt-corte.qtd-devolvida FORMAT ">,>>>,>>9.99" COLUMN-LABEL "Devolu‡Æo"    
      tt-corte.qtd-faturada  FORMAT ">,>>>,>>9.99" COLUMN-LABEL "Faturamento"
      tt-corte.qtd-final     FORMAT ">,>>>,>>9.99" COLUMN-LABEL "Estoque Final"
      tt-corte.qtd-real      FORMAT ">,>>>,>>9.99" COLUMN-LABEL "Estoque Real"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 71.72 BY 9.29
         FONT 1
         TITLE "Cortes" ROW-HEIGHT-CHARS .63.

DEFINE BROWSE br-itens
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-itens C-Win _FREEFORM
  QUERY br-itens NO-LOCK DISPLAY
      tt-itens.it-codigo FORMAT "x(8)":U                    WIDTH 8
      fn-desc-item() @ c-desc-item COLUMN-LABEL "Descri‡Æo" WIDTH 30
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 42 BY 10.38
         FONT 1
         TITLE "Itens" ROW-HEIGHT-CHARS .63.

DEFINE BROWSE br-movto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-movto C-Win _FREEFORM
  QUERY br-movto NO-LOCK DISPLAY
      tt-movto.cod-refer                           WIDTH 06 COLUMN-LABEL "Refer"
      tt-movto.nr-lote                             WIDTH 02 COLUMN-LABEL "Lt" 
      tt-movto.qtd-inicial   FORMAT "->,>>>,>>9.99" COLUMN-LABEL "Estoque Inicial"
      tt-movto.qtd-entr-est  FORMAT "->,>>>,>>9.99" COLUMN-LABEL "Entrada"
      tt-movto.qtd-transf    FORMAT "->,>>>,>>9.99" COLUMN-LABEL "Transforma‡Æo" 
      tt-movto.qtd-devolvida FORMAT "->,>>>,>>9.99" COLUMN-LABEL "Devolu‡Æo"    
      tt-movto.qtd-faturada  FORMAT "->,>>>,>>9.99" COLUMN-LABEL "Faturamento"
      tt-movto.qtd-final     FORMAT "->,>>>,>>9.99" COLUMN-LABEL "Estoque Final"
      tt-movto.qtd-real      FORMAT "->,>>>,>>9.99" COLUMN-LABEL "Estoque Real"
      (tt-movto.qtd-final - tt-movto.qtd-real) FORMAT "->>>,>>9.99" COLUMN-LABEL "Desvio"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 71.86 BY 10.33
         FONT 1
         TITLE "Movimenta‡Æo" ROW-HEIGHT-CHARS .63.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     br-itens AT ROW 1.08 COL 1.57
     br-movto AT ROW 1.13 COL 44.14
     bt-param AT ROW 1.54 COL 117.43
     bt-vapara AT ROW 3 COL 117.43
     bt-excel AT ROW 4.46 COL 117.57
     bt-imprime AT ROW 5.92 COL 117.57
     br-corte AT ROW 11.71 COL 44.29
     fi-tot-ini AT ROW 13.29 COL 13.14 COLON-ALIGNED NO-LABEL
     fi-tot-ini-ger AT ROW 13.29 COL 27.29 COLON-ALIGNED NO-LABEL
     fi-tot-ent AT ROW 14.29 COL 13.14 COLON-ALIGNED NO-LABEL
     fi-tot-ent-ger AT ROW 14.29 COL 27.29 COLON-ALIGNED NO-LABEL
     fi-tot-transf AT ROW 15.29 COL 13.14 COLON-ALIGNED NO-LABEL
     fi-tot-transf-ger AT ROW 15.29 COL 27.29 COLON-ALIGNED NO-LABEL
     fi-tot-dev AT ROW 16.29 COL 13.14 COLON-ALIGNED NO-LABEL
     fi-tot-dev-ger AT ROW 16.29 COL 27.29 COLON-ALIGNED NO-LABEL
     fi-tot-fat AT ROW 17.29 COL 13.14 COLON-ALIGNED NO-LABEL
     fi-tot-fat-ger AT ROW 17.29 COL 27.29 COLON-ALIGNED NO-LABEL
     bt-exit AT ROW 18.21 COL 117.43
     fi-tot-saldo AT ROW 18.29 COL 13.14 COLON-ALIGNED NO-LABEL
     fi-tot-saldo-ger AT ROW 18.29 COL 27.29 COLON-ALIGNED NO-LABEL
     fi-tot-real AT ROW 19.29 COL 13.14 COLON-ALIGNED NO-LABEL
     fi-tot-real-ger AT ROW 19.29 COL 27.29 COLON-ALIGNED NO-LABEL
     bt-ajuda AT ROW 19.54 COL 117.43
     "Transforma‡Æo:" VIEW-AS TEXT
          SIZE 12.57 BY .54 AT ROW 15.42 COL 2.29
          BGCOLOR 8 FGCOLOR 9 FONT 6
     "ITEM" VIEW-AS TEXT
          SIZE 5 BY .54 AT ROW 12.67 COL 23.86
          BGCOLOR 8 FGCOLOR 4 FONT 6
     "GERAL" VIEW-AS TEXT
          SIZE 6 BY .54 AT ROW 12.67 COL 36.57
          BGCOLOR 8 FONT 6
     " Totais" VIEW-AS TEXT
          SIZE 9 BY .75 AT ROW 11.54 COL 3
          BGCOLOR 8 FGCOLOR 12 FONT 0
     "Estoque Real:" VIEW-AS TEXT
          SIZE 11.57 BY .54 AT ROW 19.42 COL 3.29
          BGCOLOR 8 FGCOLOR 9 FONT 6
     "Estoque Final:" VIEW-AS TEXT
          SIZE 11.57 BY .54 AT ROW 18.42 COL 3.14
          BGCOLOR 8 FGCOLOR 9 FONT 6
     "Entrada:" VIEW-AS TEXT
          SIZE 7.29 BY .54 AT ROW 14.42 COL 8
          BGCOLOR 8 FGCOLOR 9 FONT 6
     "Estoque Inicial:" VIEW-AS TEXT
          SIZE 12.57 BY .54 AT ROW 13.42 COL 2.29
          BGCOLOR 8 FGCOLOR 9 FONT 6
     "Faturamento:" VIEW-AS TEXT
          SIZE 11 BY .54 AT ROW 17.42 COL 4.14
          BGCOLOR 8 FGCOLOR 9 FONT 6
     "Devolu‡Æo:" VIEW-AS TEXT
          SIZE 9 BY .54 AT ROW 16.42 COL 13.57 RIGHT-ALIGNED
          BGCOLOR 8 FGCOLOR 9 FONT 6
     RECT-5 AT ROW 11.83 COL 1.72
     rt-buttom AT ROW 1.21 COL 116.72
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.04
         SIZE 123 BY 20.33.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Evolu‡Æo de Estoque Acabado"
         COLUMN             = 13.72
         ROW                = 6.83
         HEIGHT             = 20.25
         WIDTH              = 123
         MAX-HEIGHT         = 29.79
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 29.79
         VIRTUAL-WIDTH      = 146.29
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB br-itens rt-buttom DEFAULT-FRAME */
/* BROWSE-TAB br-movto br-itens DEFAULT-FRAME */
/* BROWSE-TAB br-corte bt-imprime DEFAULT-FRAME */
/* SETTINGS FOR BUTTON bt-excel IN FRAME DEFAULT-FRAME
   6                                                                    */
/* SETTINGS FOR FILL-IN fi-tot-dev IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-dev-ger IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-ent IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-ent-ger IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-fat IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-fat-ger IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-ini IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-ini-ger IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-real IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-real-ger IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-saldo IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-saldo-ger IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-transf IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-transf-ger IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TEXT-LITERAL "Devolu‡Æo:"
          SIZE 9 BY .54 AT ROW 16.42 COL 13.57 RIGHT-ALIGNED            */

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-corte
/* Query rebuild information for BROWSE br-corte
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-corte WHERE
                                 NO-LOCK BY tt-corte.corte-comerc.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-corte */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-itens
/* Query rebuild information for BROWSE br-itens
     _START_FREEFORM
RUN pi-tot-itens.
OPEN QUERY {&SELF-NAME} FOR EACH tt-itens WHERE NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Query            is NOT OPENED
*/  /* BROWSE br-itens */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-movto
/* Query rebuild information for BROWSE br-movto
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-movto WHERE
                                 NO-LOCK BY tt-movto.cod-refer.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-movto */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Evolu‡Æo de Estoque Acabado */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Evolu‡Æo de Estoque Acabado */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-corte
&Scoped-define SELF-NAME br-corte
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-corte C-Win
ON VALUE-CHANGED OF br-corte IN FRAME DEFAULT-FRAME /* Cortes */
DO:
 /* APPLY 'entry' TO br-itens. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-itens
&Scoped-define SELF-NAME br-itens
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-itens C-Win
ON VALUE-CHANGED OF br-itens IN FRAME DEFAULT-FRAME /* Itens */
DO:
    DEF VAR de-tot AS DEC. 
    ASSIGN fi-tot-ini    = 0
           fi-tot-ent    = 0
           fi-tot-transf = 0
           fi-tot-dev    = 0
           fi-tot-fat    = 0
           fi-tot-saldo  = 0
           fi-tot-real   = 0. 
    IF AVAIL tt-itens THEN DO.
       ASSIGN fi-tot-ini    = tt-itens.qtd-inicial
              fi-tot-ent    = tt-itens.qtd-entr-est
              fi-tot-transf = tt-itens.qtd-transf    
              fi-tot-dev    = tt-itens.qtd-devolvida     
              fi-tot-fat    = tt-itens.qtd-faturada
              fi-tot-saldo  = tt-itens.qtd-final
              fi-tot-real   = tt-itens.qtd-real.

       EMPTY TEMP-TABLE tt-movto.

       FOR EACH ob-sl-estoq-per WHERE
                ob-sl-estoq-per.periodo       = c-periodo          AND 
                ob-sl-estoq-per.it-codigo     = tt-itens.it-codigo AND
                ob-sl-estoq-per.cod-refer    >= c-cod-refer-ini    AND
                ob-sl-estoq-per.cod-refer    <= c-cod-refer-fin    AND
                ob-sl-estoq-per.corte-comerc >= c-corte-comerc-ini AND
                ob-sl-estoq-per.corte-comerc <= c-corte-comerc-fin AND
                ob-sl-estoq-per.cod-qualid   >= c-cod-qualid-ini   AND
                ob-sl-estoq-per.cod-qualid   <= c-cod-qualid-fin   AND
                LOOKUP(ob-sl-estoq-per.nr-lote,c-lotes) <> 0  NO-LOCK.
           
           FIND tt-movto WHERE
                tt-movto.cod-refer = ob-sl-estoq-per.cod-refer AND
                tt-movto.nr-lote   = ob-sl-estoq-per.nr-lote   NO-ERROR.
           IF NOT AVAIL tt-movto THEN DO:
              CREATE tt-movto.
              ASSIGN tt-movto.cod-refer = ob-sl-estoq-per.cod-refer
                     tt-movto.nr-lote   = ob-sl-estoq-per.nr-lote.
           END.
           ASSIGN tt-movto.qtd-inicial   = tt-movto.qtd-inicial   + ob-sl-estoq-per.qtd-inicial
                  tt-movto.qtd-entr-est  = tt-movto.qtd-entr-est  + ob-sl-estoq-per.qtd-entr-est
                  tt-movto.qtd-transf    = tt-movto.qtd-transf    + ob-sl-estoq-per.qtd-transf
                  tt-movto.qtd-devolvida = tt-movto.qtd-devolvida + ob-sl-estoq-per.qtd-devolvida
                  tt-movto.qtd-faturada  = tt-movto.qtd-faturada  + ob-sl-estoq-per.qtd-faturada
                  tt-movto.qtd-final     = tt-movto.qtd-final     + ob-sl-estoq-per.qtd-final
                  tt-movto.qtd-real      = tt-movto.qtd-real      + ob-sl-estoq-per.qtd-real.
       END.
    END.
    {&OPEN-QUERY-br-movto}
    APPLY 'value-changed' TO br-movto.

    DISPLAY fi-tot-ini
            fi-tot-ent
            fi-tot-transf
            fi-tot-dev
            fi-tot-fat
            fi-tot-saldo
            fi-tot-real
            WITH FRAME {&FRAME-NAME}.
     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-movto
&Scoped-define SELF-NAME br-movto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-movto C-Win
ON VALUE-CHANGED OF br-movto IN FRAME DEFAULT-FRAME /* Movimenta‡Æo */
DO:
  IF AVAIL tt-movto THEN DO.
     EMPTY TEMP-TABLE tt-corte.
/*     DISPLAY fi-tot-ini
             fi-tot-ent
             fi-tot-transf
             fi-tot-dev
             fi-tot-fat
             fi-tot-saldo
             fi-tot-real
             WITH FRAME {&FRAME-NAME}. */
     FOR EACH ob-sl-estoq-per WHERE
              ob-sl-estoq-per.periodo       = c-periodo          AND 
              ob-sl-estoq-per.it-codigo     = tt-itens.it-codigo AND 
              ob-sl-estoq-per.cod-refer     = tt-movto.cod-refer AND 
              ob-sl-estoq-per.nr-lote       = tt-movto.nr-lote   AND 
              ob-sl-estoq-per.corte-comerc >= c-corte-comerc-ini AND
              ob-sl-estoq-per.corte-comerc <= c-corte-comerc-fin AND
              ob-sl-estoq-per.cod-qualid   >= c-cod-qualid-ini   AND
              ob-sl-estoq-per.cod-qualid   <= c-cod-qualid-fin   NO-LOCK.
         FIND tt-corte WHERE
              tt-corte.corte-comerc = ob-sl-estoq-per.corte-comerc AND 
              tt-corte.cod-qualid   = ob-sl-estoq-per.cod-qualid NO-ERROR.
         IF NOT AVAIL tt-corte THEN DO:
            CREATE tt-corte.
            ASSIGN tt-corte.corte-comerc = ob-sl-estoq-per.corte-comerc
                   tt-corte.cod-qualid   = ob-sl-estoq-per.cod-qualid.
         END.
         ASSIGN tt-corte.qtd-inicial   = tt-corte.qtd-inicial   + ob-sl-estoq-per.qtd-inicial
                tt-corte.qtd-entr-est  = tt-corte.qtd-entr-est  + ob-sl-estoq-per.qtd-entr-est
                tt-corte.qtd-transf    = tt-corte.qtd-transf    + ob-sl-estoq-per.qtd-transf
                tt-corte.qtd-devolvida = tt-corte.qtd-devolvida + ob-sl-estoq-per.qtd-devolvida
                tt-corte.qtd-faturada  = tt-corte.qtd-faturada  + ob-sl-estoq-per.qtd-faturada
                tt-corte.qtd-final     = tt-corte.qtd-final     + ob-sl-estoq-per.qtd-final
                tt-corte.qtd-real      = tt-corte.qtd-real      + ob-sl-estoq-per.qtd-real.
     END.  
  END.
  {&OPEN-QUERY-br-corte}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda C-Win
ON CHOOSE OF bt-ajuda IN FRAME DEFAULT-FRAME /* Ajuda */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-excel C-Win
ON CHOOSE OF bt-excel IN FRAME DEFAULT-FRAME /* Button 2 */
DO:
   RUN esdlg/d02essp0172.w (INPUT c-periodo, OUTPUT arq-saida).
   IF arq-saida <> "" THEN DO:
      RUN pi-gera-excel (INPUT arq-saida).
      MESSAGE "O arquivo foi gerado em " TRIM(arq-saida) + "." SKIP 
              "Para acess -lo,  abra-o atrav‚s do Excel."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-exit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-exit C-Win
ON CHOOSE OF bt-exit IN FRAME DEFAULT-FRAME
DO:
   APPLY 'CLOSE':U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-imprime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-imprime C-Win
ON CHOOSE OF bt-imprime IN FRAME DEFAULT-FRAME
DO:
   ASSIGN c-win:SENSITIVE = NO.
   ASSIGN c-item-ini = tt-itens.it-codigo
          c-item-fin = tt-itens.it-codigo.
   RUN esp/essp0172b.w (INPUT-OUTPUT c-item-ini,   
                        INPUT-OUTPUT c-item-fin,   
                        INPUT-OUTPUT l-item, 
                        INPUT-OUTPUT i-tipo-rel,
                        INPUT-OUTPUT l-ok). 
   RUN pi-imprime. 
   ASSIGN c-win:SENSITIVE = YES.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-param
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-param C-Win
ON CHOOSE OF bt-param IN FRAME DEFAULT-FRAME /* Sair */
DO:
   ASSIGN c-win:SENSITIVE = NO.
   RUN esp/essp0172a.w (INPUT-OUTPUT c-periodo,   
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
                        INPUT-OUTPUT c-cod-depos,
                        INPUT-OUTPUT i-tp-tecido,
                        INPUT-OUTPUT l-lote-todos, 
                        INPUT-OUTPUT l-lote-pp,         
                        INPUT-OUTPUT l-lote-pd,         
                        INPUT-OUTPUT l-lote-rp,         
                        INPUT-OUTPUT l-lote-rd, 
                        INPUT-OUTPUT l-lote-sc,
                        INPUT-OUTPUT l-lote-ca,
                        INPUT-OUTPUT l-ok).
   IF l-ok THEN                                     
      RUN pi-popula-browse.
   ASSIGN c-win:SENSITIVE = YES.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-vapara
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-vapara C-Win
ON CHOOSE OF bt-vapara IN FRAME DEFAULT-FRAME
DO:
  
   RUN esdlg/d01essp0172.w (OUTPUT c-it-codigo).

   IF c-it-codigo <> ? THEN DO:
      FIND tt-itens WHERE
           tt-itens.it-codigo = c-it-codigo NO-LOCK NO-ERROR. 

      IF NOT AVAIL tt-itens THEN DO.
         MESSAGE "Item nÆo est  contido na sele‡Æo!"
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
         RETURN NO-APPLY.
      END.
       /* br-itens:QUERY:REPOSITION-TO-ROWID(ROWID(tt-itens)) NO-ERROR. */
      h-query:REPOSITION-TO-ROWID(ROWID(tt-itens)) NO-ERROR.
      APPLY 'VALUE-CHANGED' TO br-itens.
   END.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-corte
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/*
ON 'F5':U OF br-itens DO:
   {&OPEN-QUERY-br-itens}
   APPLY 'value-changed' TO br-itens.
END.

br-itens:NUM-LOCKED-COLUMNS = 3.
*/

ASSIGN h-query = br-itens:QUERY.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

   RUN enable_UI.

  ASSIGN c-periodo = STRING(MONTH(TODAY),'99') + STRING(YEAR(TODAY),'9999').

   APPLY 'choose' TO bt-param.

   IF NOT THIS-PROCEDURE:PERSISTENT THEN
      WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY fi-tot-ini fi-tot-ini-ger fi-tot-ent fi-tot-ent-ger fi-tot-transf 
          fi-tot-transf-ger fi-tot-dev fi-tot-dev-ger fi-tot-fat fi-tot-fat-ger 
          fi-tot-saldo fi-tot-saldo-ger fi-tot-real fi-tot-real-ger 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-5 rt-buttom br-itens br-movto bt-param bt-vapara bt-excel 
         bt-imprime br-corte bt-exit bt-ajuda 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-abre-excel C-Win 
PROCEDURE pi-abre-excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF INPUT PARAMETER p-arquivo AS CHAR.

   def var h-prog as handle no-undo.
   run utp/ut-utils.p persistent set h-prog.

   run Execute in h-prog(input "EXCEL.EXE", input p-arquivo).

   delete procedure h-prog.
   PAUSE 5 NO-MESSAGE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-gera-excel C-Win 
PROCEDURE pi-gera-excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER p-arq-saida AS CHAR FORMAT "x(45)".
    
    /*DEFINE FRAME frm_excel WITH SIZE 85.25 BY 15.42.*/
    DEFINE FRAME frm_excel WITH SIZE 2 BY 2 ROW 1 COLUMN 1.
    ENABLE ALL WITH FRAME frm_excel.
    
    RUN pi-abre-excel (INPUT "").
    PAUSE 3 NO-MESSAGE.

    DDE INITIATE sys FRAME FRAME frm_excel:HANDLE APPLICATION "EXCEL" TOPIC "System".
    DDE INITIATE i-canal FRAME FRAME frm_excel:HANDLE APPLICATION "EXCEL" TOPIC "Pasta1".

    /* Gerar Movimento Total na TEMP-TABLE */
    FOR EACH b-tt-itens NO-LOCK.
        RUN pi-grava-movto (INPUT b-tt-itens.it-codigo).
    END.
    RUN pi-monta-planilha.

    OS-DELETE VALUE(p-arq-saida).
    DDE EXECUTE   sys COMMAND '[save.as("' + p-arq-saida + '")]'.
    
    DDE EXECUTE   sys COMMAND "[close(0)]". 
    DDE EXECUTE   sys COMMAND "[quit()]". 
    
    DDE TERMINATE sys.
    
    HIDE FRAME frm_excel.
    CLEAR FRAME frm_excel.
    DISABLE ALL WITH FRAME frm_excel.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-grava-movto C-Win 
PROCEDURE pi-grava-movto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE INPUT PARAMETER p-it-codigo AS CHAR.

 FOR EACH ob-sl-estoq-per WHERE
          ob-sl-estoq-per.periodo       = c-periodo          AND 
          ob-sl-estoq-per.it-codigo     = p-it-codigo        AND
          ob-sl-estoq-per.cod-refer    >= c-cod-refer-ini    AND
          ob-sl-estoq-per.cod-refer    <= c-cod-refer-fin    AND
          ob-sl-estoq-per.corte-comerc >= c-corte-comerc-ini AND
          ob-sl-estoq-per.corte-comerc <= c-corte-comerc-fin AND
          ob-sl-estoq-per.cod-qualid   >= c-cod-qualid-ini   AND
          ob-sl-estoq-per.cod-qualid   <= c-cod-qualid-fin   AND
          LOOKUP(ob-sl-estoq-per.nr-lote,c-lotes) <> 0  NO-LOCK.

     FIND tt-work WHERE
          tt-work.it-codigo = p-it-codigo               AND
          tt-work.cod-refer = ob-sl-estoq-per.cod-refer AND
          tt-work.nr-lote   = ob-sl-estoq-per.nr-lote   NO-ERROR.
     IF NOT AVAIL tt-work THEN DO:
        CREATE tt-work.
        ASSIGN tt-work.it-codigo = p-it-codigo
               tt-work.cod-refer = ob-sl-estoq-per.cod-refer
               tt-work.nr-lote   = ob-sl-estoq-per.nr-lote.
     END.
     ASSIGN tt-work.qtd-inicial   = tt-work.qtd-inicial   + ob-sl-estoq-per.qtd-inicial
            tt-work.qtd-entr-est  = tt-work.qtd-entr-est  + ob-sl-estoq-per.qtd-entr-est
            tt-work.qtd-transf    = tt-work.qtd-transf    + ob-sl-estoq-per.qtd-transf
            tt-work.qtd-devolvida = tt-work.qtd-devolvida + ob-sl-estoq-per.qtd-devolvida
            tt-work.qtd-faturada  = tt-work.qtd-faturada  + ob-sl-estoq-per.qtd-faturada
            tt-work.qtd-final     = tt-work.qtd-final     + ob-sl-estoq-per.qtd-final
            tt-work.qtd-real      = tt-work.qtd-real      + ob-sl-estoq-per.qtd-real.
 END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imp-analitico C-Win 
PROCEDURE pi-imp-analitico :
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
         ASSIGN c-saida = SESSION:TEMP-DIRECTORY + "essp0172-ana.tmp".
         OUTPUT TO VALUE(c-saida).
     END.
 END CASE.

 DO i-ct = 1 TO i-num-copias.
    ASSIGN i-lin = 99
           i-pag =  1.
    
    EMPTY TEMP-TABLE tt-work.
    
    IF l-item = YES THEN  /* Imprime Somente Item Marcado */
       RUN pi-grava-movto (INPUT tt-itens.it-codigo).
    ELSE DO:
        FOR EACH b-tt-itens WHERE
                 b-tt-itens.it-codigo >= c-item-ini AND
                 b-tt-itens.it-codigo <= c-item-fin NO-LOCK.
            RUN pi-grava-movto (INPUT b-tt-itens.it-codigo).
        END.
    END.
    
    FOR EACH tt-work NO-LOCK
       BREAK BY tt-work.it-codigo:
           
        IF FIRST-OF(tt-work.it-codigo) THEN DO:
            IF i-lin > 61 THEN DO:
               RUN pi-imp-cabec.
               ASSIGN i-lin = 7.
            END.
            PUT tt-work.it-codigo FORMAT "x(6)" AT  1.
        END.
           
        IF i-lin > 61 THEN DO:
           RUN pi-imp-cabec.
           ASSIGN i-lin = 7.
        END.
    
        PUT tt-work.cod-refer                      FORMAT "X(7)"           AT   8
            tt-work.nr-lote                        FORMAT "X(2)"           AT  19
            tt-work.qtd-inicial                    FORMAT "->>,>>>,>>9.99" AT  24
            tt-work.qtd-entr-est                   FORMAT "->>,>>>,>>9.99" AT  39
            tt-work.qtd-transf                     FORMAT "->>,>>>,>>9.99" AT  54
            tt-work.qtd-devolvida                  FORMAT "->>,>>>,>>9.99" AT  69
            tt-work.qtd-faturada                   FORMAT "->>,>>>,>>9.99" AT  84
            tt-work.qtd-final                      FORMAT "->>,>>>,>>9.99" AT  99
            tt-work.qtd-real                       FORMAT "->>,>>>,>>9.99" AT 113.
        PUT (tt-work.qtd-final - tt-work.qtd-real) FORMAT "->>,>>9.99"     AT 127.
        ASSIGN i-lin = i-lin + 1.
    
        ACCUMULATE tt-work.qtd-inicial   (TOTAL BY tt-work.it-codigo). /* Acumula Total por ITEM */
        ACCUMULATE tt-work.qtd-entr-est  (TOTAL BY tt-work.it-codigo).
        ACCUMULATE tt-work.qtd-transf    (TOTAL BY tt-work.it-codigo).
        ACCUMULATE tt-work.qtd-devolvida (TOTAL BY tt-work.it-codigo).
        ACCUMULATE tt-work.qtd-faturada  (TOTAL BY tt-work.it-codigo).
        ACCUMULATE tt-work.qtd-final     (TOTAL BY tt-work.it-codigo).
        ACCUMULATE tt-work.qtd-real      (TOTAL BY tt-work.it-codigo).
    
        ACCUMULATE tt-work.qtd-inicial   (TOTAL). /* Acumula Total Geral */
        ACCUMULATE tt-work.qtd-entr-est  (TOTAL).
        ACCUMULATE tt-work.qtd-transf    (TOTAL).
        ACCUMULATE tt-work.qtd-devolvida (TOTAL).
        ACCUMULATE tt-work.qtd-faturada  (TOTAL).
        ACCUMULATE tt-work.qtd-final     (TOTAL).
        ACCUMULATE tt-work.qtd-real      (TOTAL).
    
        IF LAST-OF(tt-work.it-codigo) THEN DO:
           IF i-lin > 61 THEN DO:
              RUN pi-imp-cabec.
              ASSIGN i-lin = 7.
           END.
           IF i-lin <> 7 THEN DO:
              PUT "-------------- -------------- -------------- -------------- -------------- -------------- ------------- ---------" AT 24.
              ASSIGN i-lin = i-lin + 1.
           END.
           PUT ACCUM TOTAL BY tt-work.it-codigo tt-work.qtd-inicial     FORMAT "->>,>>>,>>9.99" AT  24
               ACCUM TOTAL BY tt-work.it-codigo tt-work.qtd-entr-est    FORMAT "->>,>>>,>>9.99" AT  39
               ACCUM TOTAL BY tt-work.it-codigo tt-work.qtd-transf      FORMAT "->>,>>>,>>9.99" AT  54
               ACCUM TOTAL BY tt-work.it-codigo tt-work.qtd-devolvida   FORMAT "->>,>>>,>>9.99" AT  69
               ACCUM TOTAL BY tt-work.it-codigo tt-work.qtd-faturada    FORMAT "->>,>>>,>>9.99" AT  84
               ACCUM TOTAL BY tt-work.it-codigo tt-work.qtd-final       FORMAT "->>,>>>,>>9.99" AT  99
               ACCUM TOTAL BY tt-work.it-codigo tt-work.qtd-real        FORMAT "->>,>>>,>>9.99" AT 113.
           PUT ((ACCUM TOTAL BY tt-work.it-codigo tt-work.qtd-final) -
                (ACCUM TOTAL BY tt-work.it-codigo tt-work.qtd-real)) FORMAT "->>,>>9.99" AT 127.
           ASSIGN i-lin = i-lin + 1.
           PUT SKIP.
           ASSIGN i-lin = i-lin + 1.
        END.
    END.
    PUT SKIP.
    PUT "TOTAL GERAL..........:" AT 1.
    PUT ACCUM TOTAL tt-work.qtd-inicial     FORMAT "->>,>>>,>>9.99" AT  24
        ACCUM TOTAL tt-work.qtd-entr-est    FORMAT "->>,>>>,>>9.99" AT  39
        ACCUM TOTAL tt-work.qtd-transf      FORMAT "->>,>>>,>>9.99" AT  54
        ACCUM TOTAL tt-work.qtd-devolvida   FORMAT "->>,>>>,>>9.99" AT  69
        ACCUM TOTAL tt-work.qtd-faturada    FORMAT "->>,>>>,>>9.99" AT  84
        ACCUM TOTAL tt-work.qtd-final       FORMAT "->>,>>>,>>9.99" AT  99
        ACCUM TOTAL tt-work.qtd-real        FORMAT "->>,>>>,>>9.99" AT 113.
    PUT ((ACCUM TOTAL tt-work.qtd-final) - 
         (ACCUM TOTAL tt-work.qtd-real)) FORMAT "->>,>>9.99" AT 127 SKIP(1).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imp-cabec C-Win 
PROCEDURE pi-imp-cabec :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    PUT c-empresa  FORMAT "X(40)"                 AT   1
        "DATA: "                                  AT  58
        STRING(TODAY,"99/99/9999") FORMAT "X(10)" AT  64
        "HORA: "                                  AT  85
        STRING(TIME,"hh:mm:ss")                   AT  91
        "PAG:"                                    AT 125
        i-pag FORMAT ">>"                         AT 130
        SKIP(1).

    PUT "RELATORIO DA EVOLUCAO DE ESTOQUE ITENS ACABADO" AT 52 SKIP(1).
    PUT "Item   Referencia Lote Estoq. Inicial       Entrada   Transformacao      Devolucao    Faturamendo  Estoque Final  Estoque Real    Desvio" AT 1.
    PUT "------ ---------- ---- -------------- -------------- -------------- -------------- -------------- -------------- ------------- ---------" AT 1.
    ASSIGN i-pag = i-pag + 1.                                                                              

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imp-sintetico C-Win 
PROCEDURE pi-imp-sintetico :
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
         ASSIGN c-saida = SESSION:TEMP-DIRECTORY + "essp0172-sint.tmp".
         OUTPUT TO VALUE(c-saida).
     END.
 END CASE.

 DO i-ct = 1 TO i-num-copias.
    ASSIGN i-lin = 99
           i-pag =  1.
    
    EMPTY TEMP-TABLE tt-work.
    
    FOR EACH b-tt-itens WHERE
             b-tt-itens.it-codigo >= c-item-ini AND
             b-tt-itens.it-codigo <= c-item-fin NO-LOCK.
        RUN pi-grava-movto (INPUT b-tt-itens.it-codigo).
    END.
    
    FOR EACH tt-work NO-LOCK
       BREAK BY tt-work.it-codigo
             BY tt-work.nr-lote:
           
          IF FIRST-OF(tt-work.it-codigo) THEN DO:
              IF i-lin > 61 THEN DO:
                 RUN pi-imp-cabec.
                 ASSIGN i-lin = 7.
              END.
              PUT tt-work.it-codigo FORMAT "x(6)" AT  1.
          END.
    
          ACCUMULATE tt-work.qtd-inicial   (TOTAL BY tt-work.nr-lote).
          ACCUMULATE tt-work.qtd-entr-est  (TOTAL BY tt-work.nr-lote).
          ACCUMULATE tt-work.qtd-transf    (TOTAL BY tt-work.nr-lote).
          ACCUMULATE tt-work.qtd-devolvida (TOTAL BY tt-work.nr-lote).
          ACCUMULATE tt-work.qtd-faturada  (TOTAL BY tt-work.nr-lote).
          ACCUMULATE tt-work.qtd-final     (TOTAL BY tt-work.nr-lote).
          ACCUMULATE tt-work.qtd-real      (TOTAL BY tt-work.nr-lote).
    
          ACCUMULATE tt-work.qtd-inicial   (TOTAL BY tt-work.it-codigo). /* Acumula Total por ITEM */
          ACCUMULATE tt-work.qtd-entr-est  (TOTAL BY tt-work.it-codigo).
          ACCUMULATE tt-work.qtd-transf    (TOTAL BY tt-work.it-codigo).
          ACCUMULATE tt-work.qtd-devolvida (TOTAL BY tt-work.it-codigo).
          ACCUMULATE tt-work.qtd-faturada  (TOTAL BY tt-work.it-codigo).
          ACCUMULATE tt-work.qtd-final     (TOTAL BY tt-work.it-codigo).
          ACCUMULATE tt-work.qtd-real      (TOTAL BY tt-work.it-codigo).
    
          ACCUMULATE tt-work.qtd-inicial   (TOTAL). /* Acumula Total Geral */
          ACCUMULATE tt-work.qtd-entr-est  (TOTAL).
          ACCUMULATE tt-work.qtd-transf    (TOTAL).
          ACCUMULATE tt-work.qtd-devolvida (TOTAL).
          ACCUMULATE tt-work.qtd-faturada  (TOTAL).
          ACCUMULATE tt-work.qtd-final     (TOTAL).
          ACCUMULATE tt-work.qtd-real      (TOTAL).
    
          IF LAST-OF(tt-work.nr-lote) THEN DO:
              IF i-lin > 61 THEN DO:
                 RUN pi-imp-cabec.
                 ASSIGN i-lin = 7.
              END.
              PUT tt-work.nr-lote                                         FORMAT "X(2)"           AT  19
                  ACCUM TOTAL BY tt-work.nr-lote tt-work.qtd-inicial      FORMAT "->>,>>>,>>9.99" AT  24   
                  ACCUM TOTAL BY tt-work.nr-lote tt-work.qtd-entr-est     FORMAT "->>,>>>,>>9.99" AT  39   
                  ACCUM TOTAL BY tt-work.nr-lote tt-work.qtd-transf       FORMAT "->>,>>>,>>9.99" AT  54   
                  ACCUM TOTAL BY tt-work.nr-lote tt-work.qtd-devolvida    FORMAT "->>,>>>,>>9.99" AT  69   
                  ACCUM TOTAL BY tt-work.nr-lote tt-work.qtd-faturada     FORMAT "->>,>>>,>>9.99" AT  84   
                  ACCUM TOTAL BY tt-work.nr-lote tt-work.qtd-final        FORMAT "->>,>>>,>>9.99" AT  99   
                  ACCUM TOTAL BY tt-work.nr-lote tt-work.qtd-real         FORMAT "->>,>>>,>>9.99" AT 113.  
              PUT (ACCUM TOTAL BY tt-work.nr-lote tt-work.qtd-final)  -                                    
                  (ACCUM TOTAL BY tt-work.nr-lote tt-work.qtd-real)       FORMAT "->>,>>9.99"     AT 127.      
              ASSIGN i-Lin = i-Lin + 1.
          END.
          IF LAST-OF(tt-work.it-codigo) THEN DO:
             IF i-lin > 61 THEN DO:
                RUN pi-imp-cabec.
                ASSIGN i-lin = 7.
             END.
             IF i-lin <> 7 THEN DO:
                PUT "-------------- -------------- -------------- -------------- -------------- -------------- ------------- ---------" AT 24.
                ASSIGN i-lin = i-lin + 1.
             END.
             PUT ACCUM TOTAL BY tt-work.it-codigo tt-work.qtd-inicial     FORMAT "->>,>>>,>>9.99" AT  24
                 ACCUM TOTAL BY tt-work.it-codigo tt-work.qtd-entr-est    FORMAT "->>,>>>,>>9.99" AT  39
                 ACCUM TOTAL BY tt-work.it-codigo tt-work.qtd-transf      FORMAT "->>,>>>,>>9.99" AT  54
                 ACCUM TOTAL BY tt-work.it-codigo tt-work.qtd-devolvida   FORMAT "->>,>>>,>>9.99" AT  69
                 ACCUM TOTAL BY tt-work.it-codigo tt-work.qtd-faturada    FORMAT "->>,>>>,>>9.99" AT  84
                 ACCUM TOTAL BY tt-work.it-codigo tt-work.qtd-final       FORMAT "->>,>>>,>>9.99" AT  99
                 ACCUM TOTAL BY tt-work.it-codigo tt-work.qtd-real        FORMAT "->>,>>>,>>9.99" AT 113.
             PUT ((ACCUM TOTAL BY tt-work.it-codigo tt-work.qtd-final) -
                  (ACCUM TOTAL BY tt-work.it-codigo tt-work.qtd-real))    FORMAT "->>,>>9.99" AT 127.
             ASSIGN i-lin = i-lin + 1.
             PUT SKIP.
          END.
   
    END.
    PUT SKIP.
    PUT "TOTAL GERAL..........:" AT 1.
    PUT ACCUM TOTAL tt-work.qtd-inicial     FORMAT "->>,>>>,>>9.99" AT  24
        ACCUM TOTAL tt-work.qtd-entr-est    FORMAT "->>,>>>,>>9.99" AT  39
        ACCUM TOTAL tt-work.qtd-transf      FORMAT "->>,>>>,>>9.99" AT  54
        ACCUM TOTAL tt-work.qtd-devolvida   FORMAT "->>,>>>,>>9.99" AT  69
        ACCUM TOTAL tt-work.qtd-faturada    FORMAT "->>,>>>,>>9.99" AT  84
        ACCUM TOTAL tt-work.qtd-final       FORMAT "->>,>>>,>>9.99" AT  99
        ACCUM TOTAL tt-work.qtd-real        FORMAT "->>,>>>,>>9.99" AT 113.
    PUT ((ACCUM TOTAL tt-work.qtd-final) - 
         (ACCUM TOTAL tt-work.qtd-real)) FORMAT "->>,>>9.99" AT 127 SKIP(1).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imprime C-Win 
PROCEDURE pi-imprime :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 IF i-tipo-rel = 1 THEN
    RUN pi-imp-sintetico.
 ELSE
    RUN pi-imp-analitico.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-monta-planilha C-Win 
PROCEDURE pi-monta-planilha :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    /* Cabe‡alho  da Planilha */
    ASSIGN c-Lin = c-empresa + "             " + " EVOLU€ÇO DO ESTOQUE NO PERIODO: "  + SUBSTR(c-periodo,1,2) + "/" + SUBSTR(c-periodo,3,4). 
    DDE SEND i-canal SOURCE c-Lin ITEM "L1C1".
    DDE EXECUTE i-canal COMMAND '[select("L1C1:L1C11")]'.
    /* Fonte, Tamanho, Negrito, It lico, Sublinhado, Atachado, Cor(0=Autom tico, 1=Preto, 2=Branco 3=Vermelho...) */
    DDE EXECUTE sys COMMAND "[format.font(~"Verdana~",12,True,False,False,False,3)]".

    /* Cabe‡alho dos Dados */
    DDE SEND i-canal SOURCE "ITEM"          ITEM "L3C1".
    DDE SEND i-canal SOURCE "LOTE"          ITEM "L3C2".
    DDE SEND i-canal SOURCE "REFERÒNCIA"    ITEM "L3C3".
    DDE SEND i-canal SOURCE "QTD INICIAL"   ITEM "L3C4".
    DDE SEND i-canal SOURCE "ENT.EST"       ITEM "L3C5".
    DDE SEND i-canal SOURCE "TRANSFORMA€ÇO" ITEM "L3C6".
    DDE SEND i-canal SOURCE "DEVOLVIDA"     ITEM "L3C7".
    DDE SEND i-canal SOURCE "FATURADA"      ITEM "L3C8".
    DDE SEND i-canal SOURCE "EST.FINAL"     ITEM "L3C9".
    DDE SEND i-canal SOURCE "REAL"          ITEM "L3C10".
    DDE SEND i-canal SOURCE "DESVIO"        ITEM "L3C11".
    

    /* Formata‡Æo das Celulas do Cabe‡alho de Dados */
    DDE EXECUTE i-canal COMMAND '[select("L3C1:L3C11")]'.
    /* Fonte, Tamanho, Negrito, It lico, Sublinhado, Atachado, Cor(0=Autom tico, 1=Preto, 2=Branco 3=Vermelho...) */
    DDE EXECUTE sys COMMAND "[format.font(~"Verdana~",8,True,False,False,False,5)]".
    
    DDE EXECUTE i-canal COMMAND "[select(~"C1~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(6.00)]".
    
    DDE EXECUTE i-canal COMMAND "[select(~"C2~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(5.00)]".
    
    DDE EXECUTE i-canal COMMAND "[select(~"C3~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(12.00)]". 

    DDE EXECUTE i-canal COMMAND "[select(~"C4~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(13.86)]".
    DDE EXECUTE sys     COMMAND "[format.number(~"###.###.##0,00~")]".
    DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]".

    DDE EXECUTE i-canal COMMAND "[select(~"C5~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(13.86)]".
    DDE EXECUTE sys     COMMAND "[format.number(~"###.###.##0,00~")]".
    DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]".

    DDE EXECUTE i-canal COMMAND "[select(~"C6~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(17.00)]".
    DDE EXECUTE sys     COMMAND "[format.number(~"###.###.##0,00~")]".
    DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]".

    DDE EXECUTE i-canal COMMAND "[select(~"C7~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(13.86)]".
    DDE EXECUTE sys     COMMAND "[format.number(~"###.###.##0,00~")]".
    DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]".

    DDE EXECUTE i-canal COMMAND "[select(~"C8~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(13.86)]".
    DDE EXECUTE sys     COMMAND "[format.number(~"###.###.##0,00~")]".
    DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]".

    DDE EXECUTE i-canal COMMAND "[select(~"C9~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(13.86)]".
    DDE EXECUTE sys     COMMAND "[format.number(~"###.###.##0,00~")]".
    DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]".

    DDE EXECUTE i-canal COMMAND "[select(~"C10~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(13.86)]".
    DDE EXECUTE sys     COMMAND "[format.number(~"###.###.##0,00~")]".
    DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]".

    DDE EXECUTE i-canal COMMAND "[select(~"C11~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(13.86)]".
    DDE EXECUTE sys     COMMAND "[format.number(~"###.###.##0,00~")]".
    DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]".
    
    
    
    /* Montagem das Celulas de Dados */
    ASSIGN i-Lin = 4.
    FOR EACH tt-work NO-LOCK
       BREAK BY tt-work.it-codigo 
             BY tt-work.nr-lote
             BY tt-work.cod-refer: 

        IF FIRST-OF(tt-work.it-codigo) THEN 
           DDE SEND i-canal SOURCE tt-work.it-codigo ITEM "L" + TRIM(STRING(i-Lin)) + "C1". 

        IF FIRST-OF(tt-work.nr-lote) THEN 
           DDE SEND i-canal SOURCE tt-work.nr-lote   ITEM "L" + TRIM(STRING(i-Lin)) + "C2".

        IF FIRST-OF(tt-work.cod-refer) THEN 
           DDE SEND i-canal SOURCE STRING(tt-work.cod-refer,"99 9999 9") ITEM "L" + TRIM(STRING(i-Lin)) + "C3".
        

        DDE SEND i-canal SOURCE STRING(tt-work.qtd-inicial)                    ITEM "L" + TRIM(STRING(i-Lin)) + "C4".
        DDE SEND i-canal SOURCE STRING(tt-work.qtd-entr-est)                   ITEM "L" + TRIM(STRING(i-Lin)) + "C5".
        DDE SEND i-canal SOURCE STRING(tt-work.qtd-transf)                     ITEM "L" + TRIM(STRING(i-Lin)) + "C6".
        DDE SEND i-canal SOURCE STRING(tt-work.qtd-devolvida)                  ITEM "L" + TRIM(STRING(i-Lin)) + "C7".
        DDE SEND i-canal SOURCE STRING(tt-work.qtd-faturada)                   ITEM "L" + TRIM(STRING(i-Lin)) + "C8".
        DDE SEND i-canal SOURCE STRING(tt-work.qtd-final)                      ITEM "L" + TRIM(STRING(i-Lin)) + "C9".
        DDE SEND i-canal SOURCE STRING(tt-work.qtd-real)                       ITEM "L" + TRIM(STRING(i-Lin)) + "C10".
        IF (tt-work.qtd-final - tt-work.qtd-real) <> 0 THEN
           DDE SEND i-canal SOURCE STRING((tt-work.qtd-final - tt-work.qtd-real)) ITEM "L" + TRIM(STRING(i-Lin)) + "C11".

        ASSIGN aux-command = '[select("L' + TRIM(STRING(i-Lin)) + 'C1:L' + TRIM(STRING(i-Lin)) + 'C11")]'.
        DDE EXECUTE i-canal COMMAND aux-command.
        /* Fonte, Tamanho, Negrito, It lico, Sublinhado, Atachado, Cor(0=Autom tico, 1=Preto, 2=Branco 3=Vermelho...) */
        DDE EXECUTE sys COMMAND "[format.font(~"Verdana~",8,False,False,False,False,0)]".
        
        ASSIGN i-Lin = i-Lin + 1.

        ACCUMULATE tt-work.qtd-inicial   (TOTAL).
        ACCUMULATE tt-work.qtd-entr-est  (TOTAL).
        ACCUMULATE tt-work.qtd-transf    (TOTAL).
        ACCUMULATE tt-work.qtd-devolvida (TOTAL).
        ACCUMULATE tt-work.qtd-faturada  (TOTAL).
        ACCUMULATE tt-work.qtd-final     (TOTAL).
        ACCUMULATE tt-work.qtd-real      (TOTAL).

        ACCUMULATE tt-work.qtd-inicial   (TOTAL BY tt-work.it-codigo).
        ACCUMULATE tt-work.qtd-entr-est  (TOTAL BY tt-work.it-codigo).
        ACCUMULATE tt-work.qtd-transf    (TOTAL BY tt-work.it-codigo).
        ACCUMULATE tt-work.qtd-devolvida (TOTAL BY tt-work.it-codigo).
        ACCUMULATE tt-work.qtd-faturada  (TOTAL BY tt-work.it-codigo).
        ACCUMULATE tt-work.qtd-final     (TOTAL BY tt-work.it-codigo).
        ACCUMULATE tt-work.qtd-real      (TOTAL BY tt-work.it-codigo).

        ACCUMULATE tt-work.qtd-inicial   (TOTAL BY tt-work.nr-lote).
        ACCUMULATE tt-work.qtd-entr-est  (TOTAL BY tt-work.nr-lote).
        ACCUMULATE tt-work.qtd-transf    (TOTAL BY tt-work.nr-lote).
        ACCUMULATE tt-work.qtd-devolvida (TOTAL BY tt-work.nr-lote).
        ACCUMULATE tt-work.qtd-faturada  (TOTAL BY tt-work.nr-lote).
        ACCUMULATE tt-work.qtd-final     (TOTAL BY tt-work.nr-lote).
        ACCUMULATE tt-work.qtd-real      (TOTAL BY tt-work.nr-lote).

        IF LAST-OF(tt-work.nr-lote) THEN DO:
            DDE SEND i-canal SOURCE "Lote" ITEM "L" + TRIM(STRING(i-Lin)) + "C2".
            DDE SEND i-canal SOURCE STRING((ACCUM TOTAL BY tt-work.nr-lote tt-work.qtd-inicial))   ITEM "L" + TRIM(STRING(i-Lin)) + "C4".
            DDE SEND i-canal SOURCE STRING((ACCUM TOTAL BY tt-work.nr-lote tt-work.qtd-entr-est))  ITEM "L" + TRIM(STRING(i-Lin)) + "C5".
            DDE SEND i-canal SOURCE STRING((ACCUM TOTAL BY tt-work.nr-lote tt-work.qtd-transf))    ITEM "L" + TRIM(STRING(i-Lin)) + "C6".
            DDE SEND i-canal SOURCE STRING((ACCUM TOTAL BY tt-work.nr-lote tt-work.qtd-devolvida)) ITEM "L" + TRIM(STRING(i-Lin)) + "C7".
            DDE SEND i-canal SOURCE STRING((ACCUM TOTAL BY tt-work.nr-lote tt-work.qtd-faturada))  ITEM "L" + TRIM(STRING(i-Lin)) + "C8".
            DDE SEND i-canal SOURCE STRING((ACCUM TOTAL BY tt-work.nr-lote tt-work.qtd-final))     ITEM "L" + TRIM(STRING(i-Lin)) + "C9".
            DDE SEND i-canal SOURCE STRING((ACCUM TOTAL BY tt-work.nr-lote tt-work.qtd-real))      ITEM "L" + TRIM(STRING(i-Lin)) + "C10".
            IF ((ACCUM TOTAL BY tt-work.nr-lote tt-work.qtd-final) - (ACCUM TOTAL BY tt-work.nr-lote tt-work.qtd-real)) <> 0 THEN
               DDE SEND i-canal SOURCE STRING(((ACCUM TOTAL BY tt-work.nr-lote tt-work.qtd-final) - (ACCUM TOTAL BY tt-work.nr-lote tt-work.qtd-real))) ITEM "L" + TRIM(STRING(i-Lin)) + "C11".
            ASSIGN aux-command = '[select("L' + TRIM(STRING(i-Lin)) + 'C1:L' + TRIM(STRING(i-Lin)) + 'C11")]'.
            DDE EXECUTE i-canal COMMAND aux-command.
            /* Fonte, Tamanho, Negrito, It lico, Sublinhado, Atachado, Cor(0=Autom tico, 1=Preto, 2=Branco 3=Vermelho...) */
            DDE EXECUTE sys COMMAND "[format.font(~"Verdana~",8,True,False,False,False,0)]".
            ASSIGN i-Lin = i-Lin + 1.
        END.

        IF LAST-OF(tt-work.it-codigo) THEN DO:
            DDE SEND i-canal SOURCE "Item" ITEM "L" + TRIM(STRING(i-Lin)) + "C2".
            DDE SEND i-canal SOURCE STRING((ACCUM TOTAL BY tt-work.it-codigo tt-work.qtd-inicial))   ITEM "L" + TRIM(STRING(i-Lin)) + "C4".
            DDE SEND i-canal SOURCE STRING((ACCUM TOTAL BY tt-work.it-codigo tt-work.qtd-entr-est))  ITEM "L" + TRIM(STRING(i-Lin)) + "C5".
            DDE SEND i-canal SOURCE STRING((ACCUM TOTAL BY tt-work.it-codigo tt-work.qtd-transf))    ITEM "L" + TRIM(STRING(i-Lin)) + "C6".
            DDE SEND i-canal SOURCE STRING((ACCUM TOTAL BY tt-work.it-codigo tt-work.qtd-devolvida)) ITEM "L" + TRIM(STRING(i-Lin)) + "C7".
            DDE SEND i-canal SOURCE STRING((ACCUM TOTAL BY tt-work.it-codigo tt-work.qtd-faturada))  ITEM "L" + TRIM(STRING(i-Lin)) + "C8".
            DDE SEND i-canal SOURCE STRING((ACCUM TOTAL BY tt-work.it-codigo tt-work.qtd-final))     ITEM "L" + TRIM(STRING(i-Lin)) + "C9".
            DDE SEND i-canal SOURCE STRING((ACCUM TOTAL BY tt-work.it-codigo tt-work.qtd-real))      ITEM "L" + TRIM(STRING(i-Lin)) + "C10".
            IF ((ACCUM TOTAL BY tt-work.it-codigo tt-work.qtd-final) - (ACCUM TOTAL BY tt-work.it-codigo tt-work.qtd-real)) <> 0 THEN
               DDE SEND i-canal SOURCE STRING(((ACCUM TOTAL BY tt-work.it-codigo tt-work.qtd-final) - (ACCUM TOTAL BY tt-work.it-codigo tt-work.qtd-real)))      ITEM "L" + TRIM(STRING(i-Lin)) + "C10".
            ASSIGN aux-command = '[select("L' + TRIM(STRING(i-Lin)) + 'C1:L' + TRIM(STRING(i-Lin)) + 'C11")]'.
            DDE EXECUTE i-canal COMMAND aux-command.
            /* Fonte, Tamanho, Negrito, It lico, Sublinhado, Atachado, Cor(0=Autom tico, 1=Preto, 2=Branco 3=Vermelho...) */
            DDE EXECUTE sys COMMAND "[format.font(~"Verdana~",8,True,False,False,False,0)]".
            ASSIGN i-Lin = i-Lin + 1.
        END.

    END.
    DDE SEND i-canal SOURCE "Total Geral" ITEM "L" + TRIM(STRING(i-Lin)) + "C2".
    DDE SEND i-canal SOURCE STRING((ACCUM TOTAL tt-work.qtd-inicial))   ITEM "L" + TRIM(STRING(i-Lin)) + "C4".
    DDE SEND i-canal SOURCE STRING((ACCUM TOTAL tt-work.qtd-entr-est))  ITEM "L" + TRIM(STRING(i-Lin)) + "C5".
    DDE SEND i-canal SOURCE STRING((ACCUM TOTAL tt-work.qtd-transf))    ITEM "L" + TRIM(STRING(i-Lin)) + "C6".
    DDE SEND i-canal SOURCE STRING((ACCUM TOTAL tt-work.qtd-devolvida)) ITEM "L" + TRIM(STRING(i-Lin)) + "C7".
    DDE SEND i-canal SOURCE STRING((ACCUM TOTAL tt-work.qtd-faturada))  ITEM "L" + TRIM(STRING(i-Lin)) + "C8".
    DDE SEND i-canal SOURCE STRING((ACCUM TOTAL tt-work.qtd-final))     ITEM "L" + TRIM(STRING(i-Lin)) + "C9".
    DDE SEND i-canal SOURCE STRING((ACCUM TOTAL tt-work.qtd-real))      ITEM "L" + TRIM(STRING(i-Lin)) + "C10".
    IF ((ACCUM TOTAL tt-work.qtd-final) - (ACCUM TOTAL tt-work.qtd-real)) <> 0 THEN
       DDE SEND i-canal SOURCE STRING(((ACCUM TOTAL tt-work.qtd-final) - (ACCUM TOTAL tt-work.qtd-real)))  ITEM "L" + TRIM(STRING(i-Lin)) + "C10".
    ASSIGN aux-command = '[select("L' + TRIM(STRING(i-Lin)) + 'C1:L' + TRIM(STRING(i-Lin)) + 'C11")]'.
    DDE EXECUTE i-canal COMMAND aux-command.
    /* Fonte, Tamanho, Negrito, It lico, Sublinhado, Atachado, Cor(0=Autom tico, 1=Preto, 2=Branco 3=Vermelho...) */
    DDE EXECUTE sys COMMAND "[format.font(~"Verdana~",8,True,False,False,False,0)]".
    ASSIGN i-Lin = i-Lin + 2.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-popula-browse C-Win 
PROCEDURE pi-popula-browse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
   {utp/ut-liter.i Selecionando_Evolu‡Æo_do_Estoque *}
   RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

   EMPTY TEMP-TABLE tt-itens.
   EMPTY TEMP-TABLE tt-movto.
   EMPTY TEMP-TABLE tt-corte.

   /* Busca Nome da Empresa */
   FIND FIRST param-global NO-LOCK NO-ERROR.
   FIND FIRST empresa WHERE
              empresa.ep-codigo = param-global.empresa-prin NO-LOCK NO-ERROR. 
   ASSIGN c-empresa = (IF AVAIL empresa THEN empresa.razao-social ELSE "").

   RUN esapi/ret-udm.p (INPUT c-periodo, OUTPUT c-dia).
   ASSIGN da-dt-emissao-ini = DATE('01'  + SUBSTR(c-periodo,1,2) + SUBSTR(c-periodo,3,4))
          da-dt-emissao-fin = DATE(c-dia + SUBSTR(c-periodo,1,2) + SUBSTR(c-periodo,3,4)).

   RUN pi-separa-itens.

   RUN pi-finalizar in h-acomp.

   {&OPEN-QUERY-br-itens}

   APPLY 'value-changed' TO br-itens IN FRAME {&FRAME-NAME}.
   APPLY 'entry' TO br-itens IN FRAME {&FRAME-NAME}.
   RETURN NO-APPLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-separa-itens C-Win 
PROCEDURE pi-separa-itens :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    ASSIGN c-lotes           = ""
           fi-tot-ini-ger    = 0
           fi-tot-ent-ger    = 0
           fi-tot-transf-ger = 0
           fi-tot-dev-ger    = 0
           fi-tot-fat-ger    = 0
           fi-tot-saldo-ger  = 0
           fi-tot-real-ger   = 0.

    IF l-lote-todos = YES THEN
       ASSIGN c-lotes = "PP,PD,RP,RD,SC,CA,".
    ELSE DO:
       ASSIGN c-lotes = c-lotes + IF l-lote-pp = YES THEN "PP," ELSE ",".
       ASSIGN c-lotes = c-lotes + IF l-lote-pd = YES THEN "PD," ELSE ",".
       ASSIGN c-lotes = c-lotes + IF l-lote-rp = YES THEN "RP," ELSE ",".
       ASSIGN c-lotes = c-lotes + IF l-lote-rd = YES THEN "RD," ELSE ",".
       ASSIGN c-lotes = c-lotes + IF l-lote-sc = YES THEN "SC," ELSE ",".
       ASSIGN c-lotes = c-lotes + IF l-lote-ca = YES THEN "CA," ELSE ",".
    END.

    FOR EACH ob-sl-estoq-per WHERE 
             ob-sl-estoq-per.periodo       = c-periodo          AND 
             ob-sl-estoq-per.it-codigo    >= c-it-codigo-ini    AND
             ob-sl-estoq-per.it-codigo    <= c-it-codigo-fin    AND 
             ob-sl-estoq-per.cod-refer    >= c-cod-refer-ini    AND
             ob-sl-estoq-per.cod-refer    <= c-cod-refer-fin    AND
             ob-sl-estoq-per.corte-comerc >= c-corte-comerc-ini AND
             ob-sl-estoq-per.corte-comerc <= c-corte-comerc-fin AND
             ob-sl-estoq-per.cod-qualid   >= c-cod-qualid-ini   AND
             ob-sl-estoq-per.cod-qualid   <= c-cod-qualid-fin   AND
             LOOKUP(ob-sl-estoq-per.nr-lote,c-lotes) <> 0 
             NO-LOCK.

        FIND ITEM WHERE
             ITEM.it-codigo = ob-sl-estoq-per.it-codigo NO-LOCK NO-ERROR.
        IF item.deposito-pad <> c-cod-depos THEN NEXT.

        /*FIND item-ext WHERE
             item-ext.it-codigo = ob-sl-estoq-per.it-codigo NO-LOCK NO-ERROR.
        IF AVAIL item-ext THEN DO:
           IF i-tp-tecido = 1 AND item-ext.indigo <> YES THEN NEXT. /* Somente Indigo */
           IF i-tp-tecido = 2 AND item-ext.indigo <> NO  THEN NEXT. /* Somente NÆo Indigo */
        END.*/

        FIND ref-item-ext WHERE
             ref-item-ext.it-codigo = ob-sl-estoq-per.it-codigo AND
             ref-item-ext.cod-refer = ob-sl-estoq-per.cod-refer NO-LOCK NO-ERROR.
        IF AVAIL ref-item-ext AND
           (ref-item-ext.cod-obsoleto < c-cod-obsoleto-ini OR
            ref-item-ext.cod-obsoleto > c-cod-obsoleto-fin) THEN NEXT.

        RUN pi-acompanhar IN h-acomp (INPUT "Item: " + ob-sl-estoq-per.it-codigo + " " + 
                                            "Referencia: " + ob-sl-estoq-per.cod-refer).
        /* Gera Itens */
        FIND tt-itens WHERE 
             tt-itens.it-codigo  = ob-sl-estoq-per.it-codigo NO-ERROR.
        IF NOT AVAIL tt-itens THEN DO:
           CREATE tt-itens.
           ASSIGN tt-itens.it-codigo = ob-sl-estoq-per.it-codigo.
        END.
        ASSIGN tt-itens.qtd-inicial   = tt-itens.qtd-inicial   + ob-sl-estoq-per.qtd-inicial
               tt-itens.qtd-entr-est  = tt-itens.qtd-entr-est  + ob-sl-estoq-per.qtd-entr-est
               tt-itens.qtd-transf    = tt-itens.qtd-transf    + ob-sl-estoq-per.qtd-transf
               tt-itens.qtd-devolvida = tt-itens.qtd-devolvida + ob-sl-estoq-per.qtd-devolvida
               tt-itens.qtd-faturada  = tt-itens.qtd-faturada  + ob-sl-estoq-per.qtd-faturada
               tt-itens.qtd-final     = tt-itens.qtd-final     + ob-sl-estoq-per.qtd-final
               tt-itens.qtd-real      = tt-itens.qtd-real      + ob-sl-estoq-per.qtd-real.

        ASSIGN fi-tot-ini-ger    = fi-tot-ini-ger    + ob-sl-estoq-per.qtd-inicial
               fi-tot-ent-ger    = fi-tot-ent-ger    + ob-sl-estoq-per.qtd-entr-est
               fi-tot-transf-ger = fi-tot-transf-ger + ob-sl-estoq-per.qtd-transf
               fi-tot-dev-ger    = fi-tot-dev-ger    + ob-sl-estoq-per.qtd-devolvida
               fi-tot-fat-ger    = fi-tot-fat-ger    + ob-sl-estoq-per.qtd-faturada
               fi-tot-saldo-ger  = fi-tot-saldo-ger  + ob-sl-estoq-per.qtd-final
               fi-tot-real-ger   = fi-tot-real-ger   + ob-sl-estoq-per.qtd-real.
    END.
    
    DISPLAY fi-tot-ini-ger
            fi-tot-ent-ger
            fi-tot-transf-ger
            fi-tot-dev-ger
            fi-tot-fat-ger
            fi-tot-saldo-ger
            fi-tot-real-ger
            WITH FRAME {&FRAME-NAME}.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-tot-itens C-Win 
PROCEDURE pi-tot-itens :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*
   ASSIGN fi-tot-ini    = tt-itens.qtd-inicial
          fi-tot-ent    = tt-itens.qtd-entr-est
          fi-tot-transf = tt-itens.qtd-transf    
          fi-tot-dev    = tt-itens.qtd-devolvida     
          fi-tot-fat    = tt-itens.qtd-faturada
          fi-tot-saldo  = tt-itens.qtd-final
          fi-tot-real   = tt-itens.qtd-real.
 
   MESSAGE tt-itens.it-codigo
       VIEW-AS ALERT-BOX INFO BUTTONS OK.


   DISPLAY fi-tot-ini
           fi-tot-ent
           fi-tot-transf
           fi-tot-dev
           fi-tot-fat
           fi-tot-saldo
           fi-tot-real
           WITH FRAME {&FRAME-NAME}.
 */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-desc-corte C-Win 
FUNCTION fn-desc-corte RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  FIND corte-comerc WHERE
       corte-comerc.codigo = tt-corte.corte-comerc NO-LOCK NO-ERROR.

  RETURN trim(corte-comerc.descricao).  /* Function return value. */


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-desc-item C-Win 
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

