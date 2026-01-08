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

/* Local Variable Definitions ---                                       */

DEF TEMP-TABLE tt-pcp 
    FIELD num-progr     LIKE ob-pcp.num-progr
    FIELD dt-progr      LIKE ob-pcp.dt-progr
    FIELD it-codigo     LIKE ob-pcp.it-codigo
    FIELD situacao      LIKE ob-pcp.situacao
    FIELD l-prog        AS   LOG
    FIELD l-proc        AS   LOG
    FIELD l-pron        AS   LOG.

DEF TEMP-TABLE tt-pcp-ref LIKE ob-pcp-ref.

DEF TEMP-TABLE tt-digita
    FIELD opcao  AS CHAR
    FIELD campo AS CHAR
    FIELD valor AS CHAR.

DEFINE TEMP-TABLE tt-fotos NO-UNDO
       FIELD arq-image AS CHAR.

DEF VAR c-arq-image AS CHAR.
DEF VAR c-comando AS CHAR.

/* --- Local Variable Definitions --- */
DEF VAR h-acomp       AS HANDLE NO-UNDO.
DEF VAR h-qry-ref     AS HANDLE.

DEF VAR c-desc-item   LIKE ITEM.desc-item.
DEF VAR c-desc-sit    AS CHAR FORMAT "x(14)".
DEF VAR i-num-progr   LIKE ob-pcp.num-progr.
DEF VAR c-lotes       AS CHAR FORMAT "x(18)".
DEF VAR arq-saida     AS CHAR FORMAT "x(45)".
DEF VAR c-arq-email   AS CHAR FORMAT "x(45)".

DEF VAR c-dia           AS CHAR.
DEF VAR da-dt-progr-ini LIKE ob-pcp.dt-progr.
DEF VAR da-dt-progr-fin LIKE ob-pcp.dt-progr.
DEF VAR i-lin           AS INT.
DEF VAR i-pag           AS INT.
DEF VAR de-tot-it-prog  AS DEC.
DEF VAR de-tot-it-proc  AS DEC.
DEF VAR de-tot-it-pron  AS DEC.
DEF VAR de-tot-ger-prog AS DEC.
DEF VAR de-tot-ger-proc AS DEC.
DEF VAR de-tot-ger-pron AS DEC.
DEF VAR de-tot-des-prog AS DEC.
DEF VAR de-tot-des-proc AS DEC.
DEF VAR de-tot-des-pron AS DEC.
DEF VAR c-cod-refer     LIKE ob-pcp-ref.cod-refer.
DEF VAR c-mensagem      AS CHAR.
DEF VAR c-empresa       AS CHAR.

DEF VAR i-sit-ini AS INT.
DEF VAR i-sit-fin AS INT.

/* Variavies de Parƒmetros */
DEFINE VAR c-cod-estabel       AS CHAR.
DEFINE VAR c-periodo-ini       AS CHAR.
DEFINE VAR c-periodo-fin       AS CHAR.
DEFINE VAR i-num-progr-ini     LIKE ob-pcp.num-progr.
DEFINE VAR i-num-progr-fin     LIKE ob-pcp.num-progr          INIT "9999999".
DEFINE VAR c-it-codigo-ini     LIKE ob-etiqueta.it-codigo     INIT "".
DEFINE VAR c-it-codigo-fin     LIKE ob-etiqueta.it-codigo     INIT "ZZZZZZ".
DEFINE VAR c-cod-refer-ini     LIKE ob-etiqueta.cod-refer     INIT "". 
DEFINE VAR c-cod-refer-fin     LIKE ob-etiqueta.cod-refer     INIT "ZZZZZZZ".
DEFINE VAR c-cod-obsoleto-ini  LIKE ref-item-ext.cod-obsoleto INIT "".
DEFINE VAR c-cod-obsoleto-fin  LIKE ref-item-ext.cod-obsoleto INIT "Z".
DEFINE VAR i-tp-acab           AS INT INITIAL 3.
DEFINE VAR i-tp-tecido         AS INT INITIAL 3.
DEFINE VAR i-situacao          AS INT INITIAL 3.

DEFINE VAR l-ok                AS LOG.
DEFINE VAR i-saida             AS INT.
DEFINE VAR c-saida             AS CHAR.
DEFINE VAR i-num-copias        AS INT.

DEF {1} VAR p-cod-estabel-182  AS CHAR.
DEF {1} VAR p-it-codigo-182    AS CHAR.
DEF {1} VAR p-cod-refer-182    AS CHAR.
DEF {1} VAR p-manut-182        AS LOG.

DEF NEW SHARED VAR p-progr-ini-166 LIKE ob-pcp.num-progr.
DEF NEW SHARED VAR p-progr-fin-166 LIKE ob-pcp.num-progr.

DEF NEW SHARED VAR p-it-codigo-150 AS CHAR.
DEF NEW SHARED VAR p-cod-refer-150 AS CHAR.
DEF NEW SHARED VAR p-lote-rp-150   AS LOG.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-livre
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad
&Scoped-define BROWSE-NAME br-pcp

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-pcp tt-pcp-ref

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br-pcp                                        */
&Scoped-define FIELDS-IN-QUERY-br-pcp tt-pcp.num-progr tt-pcp.it-codigo fn-desc-item() @ c-desc-item   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-pcp   
&Scoped-define SELF-NAME br-pcp
&Scoped-define QUERY-STRING-br-pcp FOR EACH tt-pcp WHERE                                  (tt-pcp.l-prog AND rs-ppp = 1) OR                                  (tt-pcp.l-proc AND rs-ppp = 2) OR                                  (tt-pcp.l-pron AND rs-ppp = 3) OR                                  rs-ppp = 4 NO-LOCK
&Scoped-define OPEN-QUERY-br-pcp OPEN QUERY {&SELF-NAME} FOR EACH tt-pcp WHERE                                  (tt-pcp.l-prog AND rs-ppp = 1) OR                                  (tt-pcp.l-proc AND rs-ppp = 2) OR                                  (tt-pcp.l-pron AND rs-ppp = 3) OR                                  rs-ppp = 4 NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-pcp tt-pcp
&Scoped-define FIRST-TABLE-IN-QUERY-br-pcp tt-pcp


/* Definitions for BROWSE br-pcp-ref                                    */
&Scoped-define FIELDS-IN-QUERY-br-pcp-ref tt-pcp-ref.cod-refer fn-fundo() tt-pcp-ref.qtd-progr   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-pcp-ref   
&Scoped-define SELF-NAME br-pcp-ref
&Scoped-define QUERY-STRING-br-pcp-ref FOR EACH tt-pcp-ref WHERE                                  tt-pcp-ref.num-progr = tt-pcp.num-progr AND                                  ((tt-pcp-ref.qtd-sld-prog > 0 AND rs-ppp = 1) OR                                   (tt-pcp-ref.qtd-proc > 0     AND rs-ppp = 2) OR                                   (tt-pcp-ref.qtd-pron > 0     AND rs-ppp = 3) OR                                   rs-ppp = 4) NO-LOCK                                  BY tt-pcp-ref.cod-refer
&Scoped-define OPEN-QUERY-br-pcp-ref OPEN QUERY {&SELF-NAME} FOR EACH tt-pcp-ref WHERE                                  tt-pcp-ref.num-progr = tt-pcp.num-progr AND                                  ((tt-pcp-ref.qtd-sld-prog > 0 AND rs-ppp = 1) OR                                   (tt-pcp-ref.qtd-proc > 0     AND rs-ppp = 2) OR                                   (tt-pcp-ref.qtd-pron > 0     AND rs-ppp = 3) OR                                   rs-ppp = 4) NO-LOCK                                  BY tt-pcp-ref.cod-refer.
&Scoped-define TABLES-IN-QUERY-br-pcp-ref tt-pcp-ref
&Scoped-define FIRST-TABLE-IN-QUERY-br-pcp-ref tt-pcp-ref


/* Definitions for FRAME f-cad                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-cad ~
    ~{&OPEN-QUERY-br-pcp-ref}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-11 RECT-12 RECT-13 RECT-14 rt-button ~
bt-param bt-consulta bt-elim-prog rs-ppp br-pcp br-pcp-ref bt-vapra ~
bt-desenho bt-anl-gerencial bt-imprime fi-qtd-work edt_msg 
&Scoped-Define DISPLAYED-OBJECTS rs-ppp fi-usr-ult-proc fi-dt-ult-proc ~
fi-usr-ult-prog fi-usr-ult-pron fi-qtd-acum-proc fi-dt-ult-prog ~
fi-dt-ult-pron fi-qtd-sld-prog fi-qtd-acum-pron fi-qtd-work edt_msg 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-desc-item w-livre 
FUNCTION fn-desc-item RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-desc-situacao w-livre 
FUNCTION fn-desc-situacao RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-fundo w-livre 
FUNCTION fn-fundo RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-situacao w-livre 
FUNCTION fn-situacao RETURNS CHARACTER
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
DEFINE BUTTON bt-anl-gerencial 
     IMAGE-UP FILE "image/im-150.bmp":U
     LABEL "" 
     SIZE 4 BY 1.21 TOOLTIP "Consulta Gerencial do Estoque"
     BGCOLOR 8 .

DEFINE BUTTON bt-cancel-prog 
     IMAGE-UP FILE "image/img-elim.bmp":U
     LABEL "Button 1" 
     SIZE 12 BY 1.13 TOOLTIP "Elimina Programa‡ao do Desenho".

DEFINE BUTTON bt-cancel-pron 
     IMAGE-UP FILE "image/img-era.bmp":U
     LABEL "" 
     SIZE 12 BY 1.13 TOOLTIP "Cancela Saldo Pronto".

DEFINE BUTTON bt-consulta AUTO-GO 
     IMAGE-UP FILE "image/im-mens.bmp":U
     LABEL "" 
     SIZE 4 BY 1.21 TOOLTIP "ImpressÆo e Mensagens da Programa‡Æo"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-desenho 
     IMAGE-UP FILE "image/im-show.bmp":U
     LABEL "" 
     SIZE 4 BY 1.29 TOOLTIP "Visualiza Imagem do Desenho"
     BGCOLOR 8 .

DEFINE BUTTON bt-elim-prog 
     IMAGE-UP FILE "image/im-era.bmp":U
     LABEL "" 
     SIZE 4 BY 1.21 TOOLTIP "Elimina a Programa‡Æo e TODOS os Desenhos".

DEFINE BUTTON bt-imprime AUTO-GO 
     IMAGE-UP FILE "image/im-pri.bmp":U
     LABEL "" 
     SIZE 4 BY 1.29 TOOLTIP "Imprime Programa‡äes"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-mod-prog 
     IMAGE-UP FILE "image/img-mod.bmp":U
     LABEL "bt cancel prog 2" 
     SIZE 12 BY 1.13 TOOLTIP "Modifica Saldo Programado".

DEFINE BUTTON bt-param AUTO-GO 
     IMAGE-UP FILE "image/im-fil.bmp":U
     LABEL "&Sair" 
     SIZE 4 BY 1.21 TOOLTIP "Parƒmetros"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-proc AUTO-GO 
     IMAGE-UP FILE "image/im-down2.bmp":U
     LABEL "" 
     SIZE 5.43 BY 1.25 TOOLTIP "Retorna Quantidade para Processo"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-prog AUTO-GO 
     IMAGE-UP FILE "image/im-nex1.bmp":U
     LABEL "" 
     SIZE 5.43 BY 1.25 TOOLTIP "Retorna Quantidade para Processo"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-pron AUTO-GO 
     IMAGE-UP FILE "image/im-pre.bmp":U
     LABEL "" 
     SIZE 5.43 BY 1.25 TOOLTIP "Retorna Quantidade para Processo"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-vapra 
     IMAGE-UP FILE "image/im-vapra.bmp":U
     LABEL "" 
     SIZE 4 BY 1.29 TOOLTIP "V  para Referˆncia"
     BGCOLOR 8 .

DEFINE VARIABLE edt_msg AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 97 BY 5.5
     BGCOLOR 8 FGCOLOR 12 FONT 11 NO-UNDO.

DEFINE VARIABLE fi-dt-ult-proc AS DATE FORMAT "99/99/9999" 
     LABEL "Ult. Data" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 TOOLTIP "Data da £ltima altera‡Æo em Processo." NO-UNDO.

DEFINE VARIABLE fi-dt-ult-prog AS DATE FORMAT "99/99/9999" 
     LABEL "Data" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 TOOLTIP "Data da £ltima altera‡Æo em Processo." NO-UNDO.

DEFINE VARIABLE fi-dt-ult-pron AS DATE FORMAT "99/99/9999" 
     LABEL "Ult. Data" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 TOOLTIP "Data da £ltima altera‡Æo em Pronto." NO-UNDO.

DEFINE VARIABLE fi-qtd-acum-proc AS INTEGER FORMAT "->,>>>,>>9" INITIAL 0 
     LABEL "Qtde Acumulada" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88
     FGCOLOR 16 FONT 0 NO-UNDO.

DEFINE VARIABLE fi-qtd-acum-pron AS INTEGER FORMAT ">,>>>,>>9" INITIAL 0 
     LABEL "Qtde Acumulada" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88
     FGCOLOR 2 FONT 0 NO-UNDO.

DEFINE VARIABLE fi-qtd-sld-prog AS INTEGER FORMAT "->,>>>,>>9" INITIAL 0 
     LABEL "Saldo" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88
     FGCOLOR 12 FONT 0 NO-UNDO.

DEFINE VARIABLE fi-qtd-work AS INTEGER FORMAT ">,>>>,>>>":R17 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 20.29 BY 1.5
     FONT 10 NO-UNDO.

DEFINE VARIABLE fi-usr-ult-proc AS CHARACTER FORMAT "X(12)" 
     LABEL "Ult. Usu rio" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 TOOLTIP "Usu rio que fez a £ltima altera‡Æo em Processo." NO-UNDO.

DEFINE VARIABLE fi-usr-ult-prog AS CHARACTER FORMAT "X(12)" 
     LABEL "Usu rio" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 TOOLTIP "Usu rio que fez a £ltima altera‡Æo em Processo." NO-UNDO.

DEFINE VARIABLE fi-usr-ult-pron AS CHARACTER FORMAT "X(12)" 
     LABEL "Ult. Usuario" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 TOOLTIP "Usu rio que fez a £ltima altera‡Æo em Pronto." NO-UNDO.

DEFINE VARIABLE rs-ppp AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Programado", 1,
"Processo", 2,
"Pronto", 3,
"Todos", 4
     SIZE 64 BY 1
     BGCOLOR 8 FGCOLOR 9 FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 8  NO-FILL   
     SIZE 32 BY 4
     BGCOLOR 16 FGCOLOR 16 .

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 8  NO-FILL   
     SIZE 32 BY 7.08
     BGCOLOR 2 .

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 8  NO-FILL   
     SIZE 29 BY 7
     BGCOLOR 12 FGCOLOR 12 .

DEFINE RECTANGLE RECT-14
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 5 BY 6.75
     BGCOLOR 8 .

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 97 BY 1.46
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-pcp FOR 
      tt-pcp SCROLLING.

DEFINE QUERY br-pcp-ref FOR 
      tt-pcp-ref SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-pcp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-pcp w-livre _FREEFORM
  QUERY br-pcp NO-LOCK DISPLAY
      tt-pcp.num-progr                COLUMN-LABEL "Prog"      WIDTH  6
      tt-pcp.it-codigo                COLUMN-LABEL "Item"      WIDTH  7
      fn-desc-item() @ c-desc-item    COLUMN-LABEL "Descri‡Æo" WIDTH  37
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 55 BY 6.75
         FONT 1
         TITLE "Programa‡äes de Produ‡Æo" ROW-HEIGHT-CHARS .63.

DEFINE BROWSE br-pcp-ref
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-pcp-ref w-livre _FREEFORM
  QUERY br-pcp-ref NO-LOCK DISPLAY
      tt-pcp-ref.cod-refer    COLUMN-LABEL "Referˆncia" FORMAT "99-9999-9" 
      fn-fundo()              COLUMN-LABEL "Fundo"      WIDTH  5
      tt-pcp-ref.qtd-progr    COLUMN-LABEL "Qtde Programada"     COLUMN-FONT 9   FORMAT ">,>>>,>>9"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 35 BY 6.75
         FONT 3
         TITLE "Desenhos" ROW-HEIGHT-CHARS .63.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     bt-param AT ROW 1.17 COL 67.43
     bt-consulta AT ROW 1.17 COL 71.72
     bt-elim-prog AT ROW 1.17 COL 76
     rs-ppp AT ROW 1.25 COL 2 NO-LABEL
     br-pcp AT ROW 2.75 COL 1
     br-pcp-ref AT ROW 2.75 COL 57
     bt-vapra AT ROW 2.96 COL 93.57
     bt-desenho AT ROW 4.25 COL 93.57
     bt-anl-gerencial AT ROW 6.83 COL 93.57
     bt-imprime AT ROW 8.08 COL 93.57
     fi-usr-ult-proc AT ROW 10.54 COL 44.43 COLON-ALIGNED
     fi-dt-ult-proc AT ROW 11.54 COL 44.43 COLON-ALIGNED
     fi-usr-ult-prog AT ROW 11.71 COL 8 COLON-ALIGNED
     fi-usr-ult-pron AT ROW 11.71 COL 78 COLON-ALIGNED
     fi-qtd-acum-proc AT ROW 12.5 COL 44.43 COLON-ALIGNED
     fi-dt-ult-prog AT ROW 12.71 COL 8 COLON-ALIGNED
     fi-dt-ult-pron AT ROW 12.71 COL 78 COLON-ALIGNED
     fi-qtd-sld-prog AT ROW 13.67 COL 8 COLON-ALIGNED
     fi-qtd-acum-pron AT ROW 13.71 COL 78 COLON-ALIGNED
     bt-proc AT ROW 14.17 COL 45
     bt-mod-prog AT ROW 15.25 COL 3.29
     bt-cancel-prog AT ROW 15.25 COL 15.57
     bt-cancel-pron AT ROW 15.25 COL 80
     fi-qtd-work AT ROW 15.5 COL 57.01 RIGHT-ALIGNED NO-LABEL
     bt-prog AT ROW 15.58 COL 31.86
     bt-pron AT ROW 15.63 COL 58.72
     edt_msg AT ROW 17.25 COL 1 NO-LABEL
     " Saldo Programar" VIEW-AS TEXT
          SIZE 17.29 BY .75 AT ROW 9.75 COL 3.72
          BGCOLOR 12 FGCOLOR 15 FONT 9
     " Pronto" VIEW-AS TEXT
          SIZE 8 BY .67 AT ROW 9.75 COL 69
          BGCOLOR 2 FGCOLOR 15 FONT 8
     " Processo" VIEW-AS TEXT
          SIZE 10 BY .71 AT ROW 9.75 COL 34.72
          BGCOLOR 16 FGCOLOR 15 FONT 9
     RECT-11 AT ROW 9.96 COL 32
     RECT-12 AT ROW 9.92 COL 66
     RECT-13 AT ROW 9.96 COL 1
     RECT-14 AT ROW 2.75 COL 93
     rt-button AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 119.14 BY 22.17
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
         TITLE              = "Consulta Programa‡Æo de Produ‡Æo"
         HEIGHT             = 21.88
         WIDTH              = 97.57
         MAX-HEIGHT         = 28.21
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 28.21
         VIRTUAL-WIDTH      = 146.29
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
/* BROWSE-TAB br-pcp rs-ppp f-cad */
/* BROWSE-TAB br-pcp-ref br-pcp f-cad */
/* SETTINGS FOR BUTTON bt-cancel-prog IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-cancel-pron IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-mod-prog IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-proc IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-prog IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-pron IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-dt-ult-proc IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-dt-ult-prog IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-dt-ult-pron IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qtd-acum-proc IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qtd-acum-pron IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qtd-sld-prog IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qtd-work IN FRAME f-cad
   ALIGN-R                                                              */
/* SETTINGS FOR FILL-IN fi-usr-ult-proc IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-usr-ult-prog IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-usr-ult-pron IN FRAME f-cad
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-pcp
/* Query rebuild information for BROWSE br-pcp
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-pcp WHERE
                                 (tt-pcp.l-prog AND rs-ppp = 1) OR
                                 (tt-pcp.l-proc AND rs-ppp = 2) OR
                                 (tt-pcp.l-pron AND rs-ppp = 3) OR
                                 rs-ppp = 4 NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Query            is NOT OPENED
*/  /* BROWSE br-pcp */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-pcp-ref
/* Query rebuild information for BROWSE br-pcp-ref
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-pcp-ref WHERE
                                 tt-pcp-ref.num-progr = tt-pcp.num-progr AND
                                 ((tt-pcp-ref.qtd-sld-prog > 0 AND rs-ppp = 1) OR
                                  (tt-pcp-ref.qtd-proc > 0     AND rs-ppp = 2) OR
                                  (tt-pcp-ref.qtd-pron > 0     AND rs-ppp = 3) OR
                                  rs-ppp = 4) NO-LOCK
                                 BY tt-pcp-ref.cod-refer.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-pcp-ref */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON END-ERROR OF w-livre /* Consulta Programa‡Æo de Produ‡Æo */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON WINDOW-CLOSE OF w-livre /* Consulta Programa‡Æo de Produ‡Æo */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-pcp
&Scoped-define SELF-NAME br-pcp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-pcp w-livre
ON VALUE-CHANGED OF br-pcp IN FRAME f-cad /* Programa‡äes de Produ‡Æo */
DO:
    ASSIGN bt-elim-prog:SENSITIVE = YES.
    IF CAN-FIND (FIRST tt-pcp-ref  WHERE
                       tt-pcp-ref.num-progr = tt-pcp.num-progr AND
                       tt-pcp-ref.qtd-proc > 0) OR
       CAN-FIND (FIRST tt-pcp-ref  WHERE
                       tt-pcp-ref.num-progr = tt-pcp.num-progr AND
                       tt-pcp-ref.qtd-pron > 0) THEN
       ASSIGN bt-elim-prog:SENSITIVE = NO.

   {&OPEN-QUERY-br-pcp-ref}
   APPLY 'value-changed' TO br-pcp-ref.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-pcp-ref
&Scoped-define SELF-NAME br-pcp-ref
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-pcp-ref w-livre
ON VALUE-CHANGED OF br-pcp-ref IN FRAME f-cad /* Desenhos */
DO:
   ASSIGN edt_msg = ""
          fi-usr-ult-prog = ""
          fi-dt-ult-prog = ?
          fi-qtd-sld-prog = 0
          fi-usr-ult-proc = ""
          fi-dt-ult-proc = ?
          fi-qtd-acum-proc = 0
          fi-usr-ult-pron = ""
          fi-dt-ult-pron = ?
          fi-qtd-acum-pron = 0
          fi-qtd-work = 0.

   IF AVAIL tt-pcp-ref THEN DO.
      ASSIGN edt_msg = tt-pcp-ref.observ 
             fi-usr-ult-prog = tt-pcp-ref.usr-ult-prog
             fi-dt-ult-prog = tt-pcp-ref.dt-ult-prog
             fi-qtd-sld-prog = tt-pcp-ref.qtd-sld-prog
             fi-usr-ult-proc = tt-pcp-ref.usr-ult-proc
             fi-dt-ult-proc = tt-pcp-ref.dt-ult-proc
             fi-qtd-acum-proc = tt-pcp-ref.qtd-proc
             fi-usr-ult-pron = tt-pcp-ref.usr-ult-pron
             fi-dt-ult-pron = tt-pcp-ref.dt-ult-pron
             fi-qtd-acum-pron = tt-pcp-ref.qtd-pron
             fi-qtd-work = 0.
    
      bt-prog:LOAD-IMAGE("image\im-nex.bmp").
      bt-proc:LOAD-IMAGE("image\im-down2.bmp").
      bt-pron:LOAD-IMAGE("image\im-pre.bmp").
    
      /* Busca Imagens da Referˆncia */
      ASSIGN c-arq-image = SESSION:TEMP-DIRECTORY + SUBSTR(tt-pcp-ref.cod-refer,3,4) + '.txt'.
             c-comando = 'DIR /b ' + param-dis.dir-img-item + '\*' + SUBSTR(tt-pcp-ref.cod-refer,3,4) + '* >' +
                         c-arq-image.
      OS-COMMAND SILENT VALUE(c-comando) NO-ERROR.
       
      EMPTY TEMP-TABLE tt-fotos.

      IF SEARCH(c-arq-image) <> ? THEN DO.
         INPUT FROM VALUE(c-arq-image) NO-ECHO.
         REPEAT.
             CREATE tt-fotos.
             IMPORT tt-fotos.
         END.
         INPUT CLOSE.
      END.
        
      FOR EACH tt-fotos.
          IF tt-fotos.arq-image MATCHES '*' + SUBSTR(tt-pcp-ref.cod-refer,3,4) + '*' THEN DO.
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
   END.

   ASSIGN bt-cancel-prog:SENSITIVE = NO
          bt-mod-prog:SENSITIVE = NO
          bt-cancel-pron:SENSITIVE = NO. 

   IF p-it-codigo-182 = '' THEN
      ASSIGN bt-prog:SENSITIVE = fi-qtd-sld-prog > 0 
             bt-proc:SENSITIVE = fi-qtd-acum-proc > 0
             bt-pron:SENSITIVE = fi-qtd-acum-pron > 0
             bt-cancel-prog:SENSITIVE = fi-qtd-sld-prog > 0 AND fi-qtd-acum-proc = 0 AND fi-qtd-acum-pron = 0 AND c-seg-usuario = 'janete' 
             bt-mod-prog:SENSITIVE = fi-qtd-sld-prog > 0 AND c-seg-usuario = 'janete'
             bt-cancel-pron:SENSITIVE = fi-qtd-acum-pron > 0. 
   ELSE
      IF p-manut-182 THEN
         ASSIGN bt-cancel-prog:SENSITIVE = fi-qtd-sld-prog > 0 AND fi-qtd-acum-proc = 0 AND fi-qtd-acum-pron = 0 AND c-seg-usuario = 'janete'
                bt-mod-prog:SENSITIVE = fi-qtd-sld-prog > 0 AND c-seg-usuario = 'janete'
                bt-cancel-pron:SENSITIVE = fi-qtd-acum-pron > 0. 

   DISP edt_msg                     fi-usr-ult-prog
        fi-dt-ult-prog              fi-qtd-sld-prog
        fi-usr-ult-proc             fi-dt-ult-proc
        fi-qtd-acum-proc            fi-usr-ult-pron
        fi-dt-ult-pron              fi-qtd-acum-pron 
        fi-qtd-work                
        WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-anl-gerencial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-anl-gerencial w-livre
ON CHOOSE OF bt-anl-gerencial IN FRAME f-cad
DO:
   IF AVAIL tt-pcp-ref THEN DO.
      ASSIGN w-livre:SENSITIVE = NO.
      ASSIGN p-it-codigo-150 = tt-pcp.it-codigo
             p-cod-refer-150 = tt-pcp-ref.cod-refer
             p-lote-rp-150 = YES.

      RUN esp/essp0150.w "SHARED".
      ASSIGN w-livre:SENSITIVE = YES.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cancel-prog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancel-prog w-livre
ON CHOOSE OF bt-cancel-prog IN FRAME f-cad /* Button 1 */
DO:
   MESSAGE "Deseja Realmente Cancelar a Quantidade Programda ?"
           VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
           TITLE "" UPDATE choice AS LOGICAL.

   IF NOT choice THEN RETURN NO-APPLY.

   FIND ob-pcp-ref WHERE 
        ob-pcp-ref.num-progr = tt-pcp-ref.num-progr AND
        ob-pcp-ref.cod-refer = tt-pcp-ref.cod-refer
        SHARE-LOCK NO-ERROR.

   IF ob-pcp-ref.qtd-proc > 0 OR
      ob-pcp-ref.qtd-pron > 0 THEN DO.
      MESSAGE 'Outro Usuario enviou Quantidade para Processo/Pronto....' SKIP
              'Imposs¡vel Cancela Programa‡Æo.'
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      RETURN NO-APPLY.
   END.

   DELETE ob-pcp-ref.
   DELETE tt-pcp-ref.

   ASSIGN tt-pcp.l-prog = CAN-FIND (FIRST tt-pcp-ref  WHERE
                                          tt-pcp-ref.num-progr = tt-pcp.num-progr AND
                                          tt-pcp-ref.qtd-sld-prog > 0)
          tt-pcp.l-proc = CAN-FIND (FIRST tt-pcp-ref  WHERE
                                          tt-pcp-ref.num-progr = tt-pcp.num-progr AND
                                          tt-pcp-ref.qtd-proc > 0)
          tt-pcp.l-pron = CAN-FIND (FIRST tt-pcp-ref  WHERE
                                          tt-pcp-ref.num-progr = tt-pcp.num-progr AND
                                          tt-pcp-ref.qtd-pron > 0).

   IF NOT tt-pcp.l-prog AND 
      NOT tt-pcp.l-proc AND 
      NOT tt-pcp.l-pron AND
      NOT CAN-FIND (FIRST ob-pcp-ref WHERE
                          ob-pcp-ref.num-progr = tt-pcp.num-progr) THEN DO.
      FIND ob-pcp WHERE
           ob-pcp.num-progr = tt-pcp.num-progr NO-ERROR.

      DELETE ob-pcp.
      DELETE tt-pcp.
   END.

   RUN pi-open-query.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cancel-pron
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancel-pron w-livre
ON CHOOSE OF bt-cancel-pron IN FRAME f-cad
DO:
   MESSAGE "Deseja Realmente Cancelar o Saldo do Pronto ?"
           VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
           TITLE "" UPDATE choice AS LOGICAL.

   IF NOT choice THEN RETURN NO-APPLY.

   FIND ob-pcp-ref WHERE 
        ob-pcp-ref.num-progr = tt-pcp-ref.num-progr AND
        ob-pcp-ref.cod-refer = tt-pcp-ref.cod-refer
        NO-ERROR.

   ASSIGN ob-pcp-ref.qtd-pron = 0
          ob-pcp-ref.usr-ult-pron = c-seg-usuario
          ob-pcp-ref.dt-ult-pron  = TODAY
          tt-pcp-ref.qtd-pron = 0
          tt-pcp.l-pron = NO.

   IF ob-pcp-ref.qtd-sld-prog = 0 AND
      ob-pcp-ref.qtd-proc = 0 AND
      ob-pcp-ref.qtd-pron = 0 THEN
      ASSIGN ob-pcp-ref.situacao = 2.

   APPLY 'VALUE-CHANGED' TO br-pcp-ref IN FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-consulta w-livre
ON CHOOSE OF bt-consulta IN FRAME f-cad
DO:
    ASSIGN p-progr-ini-166 = tt-pcp.num-prog
           p-progr-fin-166 = tt-pcp.num-prog.
    
    RUN esp/essp0166.w "SHARED".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-desenho
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-desenho w-livre
ON CHOOSE OF bt-desenho IN FRAME f-cad
DO:
   RUN esdlg/d01-desenho.w (INPUT tt-pcp-ref.cod-refer).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-elim-prog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-elim-prog w-livre
ON CHOOSE OF bt-elim-prog IN FRAME f-cad
DO:
    MESSAGE "Deseja Realmente Eliminar a Programa‡Æo e TODOS as Referˆncias ?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
            TITLE "" UPDATE choice AS LOGICAL.

    IF NOT choice THEN RETURN NO-APPLY.

    FIND ob-pcp WHERE
         ob-pcp.num-progr = tt-pcp.num-progr NO-ERROR.

    FOR EACH ob-pcp-ref OF ob-pcp.
        DELETE ob-pcp-ref.
    END.
    DELETE ob-pcp.

    FOR EACH tt-pcp-ref WHERE
             tt-pcp-ref.num-progr = tt-pcp.num-progr.
        DELETE tt-pcp-ref.
    END.
    DELETE tt-pcp.

    RUN pi-open-query.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-imprime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-imprime w-livre
ON CHOOSE OF bt-imprime IN FRAME f-cad
DO:
    RUN pi-imprime.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-mod-prog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-mod-prog w-livre
ON CHOOSE OF bt-mod-prog IN FRAME f-cad /* bt cancel prog 2 */
DO:
   ASSIGN fi-qtd-sld-prog:SENSITIVE = YES.
   APPLY 'ENTRY' TO fi-qtd-sld-prog.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-param
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-param w-livre
ON CHOOSE OF bt-param IN FRAME f-cad /* Sair */
DO:
   EMPTY TEMP-TABLE tt-digita.

   IF p-it-codigo-182 = "" THEN DO:
      ASSIGN w-livre:SENSITIVE = NO.

      RUN esapi/busca-estab-usuar.p (INPUT c-seg-usuario,
                                      OUTPUT c-cod-estabel).
      IF c-cod-estabel = '' OR c-cod-estabel = '0' THEN
         ASSIGN c-cod-estabel = '1'.

      RUN esp/essp0182a.w (INPUT-OUTPUT TABLE tt-digita,
                           INPUT-OUTPUT c-cod-estabel,
                           INPUT-OUTPUT c-periodo-ini,
                           INPUT-OUTPUT c-periodo-fin,
                           INPUT-OUTPUT i-num-progr-ini,
                           INPUT-OUTPUT i-num-progr-fin,
                           INPUT-OUTPUT c-it-codigo-ini,   
                           INPUT-OUTPUT c-it-codigo-fin,   
                           INPUT-OUTPUT c-cod-refer-ini,
                           INPUT-OUTPUT c-cod-refer-fin,
                           INPUT-OUTPUT c-cod-obsoleto-ini,
                           INPUT-OUTPUT c-cod-obsoleto-fin,
                           INPUT-OUTPUT i-tp-acab,
                           INPUT-OUTPUT i-tp-tecido,
                           INPUT-OUTPUT i-situacao,
                           INPUT-OUTPUT l-ok).
   END.
   ELSE
      ASSIGN c-it-codigo-ini = p-it-codigo-182
             c-it-codigo-fin = p-it-codigo-182
             c-cod-refer-ini = p-cod-refer-182
             c-cod-refer-fin = p-cod-refer-182
             c-cod-estabel = p-cod-estabel-182
             i-situacao = 1
             l-ok = YES.

   ASSIGN w-livre:SENSITIVE = YES.

   ASSIGN rs-ppp:SCREEN-VALUE = '4'.

   IF l-ok THEN   
      RUN pi-processa.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-proc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-proc w-livre
ON CHOOSE OF bt-proc IN FRAME f-cad
DO:
   IF SELF:IMAGE = "image\im-up2.bmp" THEN DO.
       FIND ob-pcp-ref WHERE 
            ob-pcp-ref.num-progr = tt-pcp-ref.num-progr AND
            ob-pcp-ref.cod-refer = tt-pcp-ref.cod-refer
            NO-ERROR.

      IF bt-prog:SENSITIVE THEN DO.
         IF fi-qtd-work:INPUT-VALUE > ob-pcp-ref.qtd-sld-prog THEN DO.
             MESSAGE 'Quantidade Informada Maior que Saldo Programado...'
                 VIEW-AS ALERT-BOX ERROR BUTTONS OK.
             APPLY 'entry' TO fi-qtd-work.
             RETURN NO-APPLY.
         END.
      END.

      IF bt-pron:SENSITIVE THEN DO.
         IF fi-qtd-work:INPUT-VALUE > ob-pcp-ref.qtd-pron THEN DO.
            MESSAGE 'Quantidade Informada Maior que Quantidade Pronta...'
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            APPLY 'entry' TO fi-qtd-work.
            RETURN NO-APPLY.
         END.
         ASSIGN tt-pcp-ref.qtd-pron = tt-pcp-ref.qtd-pron - fi-qtd-work:INPUT-VALUE.
      END.

      IF bt-prog:SENSITIVE THEN 
         ASSIGN tt-pcp-ref.qtd-sld-prog = ob-pcp-ref.qtd-sld-prog - fi-qtd-work:INPUT-VALUE.

      ASSIGN tt-pcp-ref.qtd-proc = ob-pcp-ref.qtd-proc + fi-qtd-work:INPUT-VALUE.

      ASSIGN ob-pcp-ref.qtd-sld-prog = tt-pcp-ref.qtd-sld-prog
             ob-pcp-ref.qtd-proc = tt-pcp-ref.qtd-proc
             ob-pcp-ref.qtd-pron = tt-pcp-ref.qtd-pron
             ob-pcp-ref.usr-ult-proc = c-seg-usuario
             ob-pcp-ref.dt-ult-proc = TODAY.

      ASSIGN bt-elim-prog:SENSITIVE = NO
             tt-pcp.l-proc = YES
             tt-pcp.l-pron = CAN-FIND (FIRST tt-pcp-ref  WHERE
                                             tt-pcp-ref.num-progr = tt-pcp.num-progr AND
                                             tt-pcp-ref.qtd-pron > 0)
             tt-pcp.l-prog = CAN-FIND (FIRST tt-pcp-ref  WHERE
                                             tt-pcp-ref.num-progr = tt-pcp.num-progr AND
                                             tt-pcp-ref.qtd-sld-prog > 0).

      ASSIGN fi-qtd-work:SCREEN-VALUE = "".

      APPLY 'VALUE-CHANGED' TO br-pcp-ref IN FRAME {&FRAME-NAME}.
   END.
   ELSE DO.
      ASSIGN fi-qtd-work:SCREEN-VALUE = fi-qtd-acum-proc:SCREEN-VALUE.

      bt-prog:LOAD-IMAGE("image\im-pre.bmp").
      bt-pron:LOAD-IMAGE("image\im-nex.bmp").

      ASSIGN bt-prog:SENSITIVE = YES 
             bt-pron:SENSITIVE = YES.

      APPLY 'entry' TO fi-qtd-work.
      RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-prog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-prog w-livre
ON CHOOSE OF bt-prog IN FRAME f-cad
DO:
   IF SELF:IMAGE = "image\im-pre.bmp" THEN DO.
      FIND ob-pcp-ref WHERE 
           ob-pcp-ref.num-progr = tt-pcp-ref.num-progr AND
           ob-pcp-ref.cod-refer = tt-pcp-ref.cod-refer
           NO-ERROR.

      IF fi-qtd-work:INPUT-VALUE > ob-pcp-ref.qtd-proc THEN DO.
         MESSAGE 'Quantidade Informada Maior que Quantidade em Processo...'
             VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY 'entry' TO fi-qtd-work.
         RETURN NO-APPLY.
      END.

      ASSIGN tt-pcp-ref.qtd-sld-prog = ob-pcp-ref.qtd-sld-prog + fi-qtd-work:INPUT-VALUE
             tt-pcp-ref.qtd-proc = ob-pcp-ref.qtd-proc - fi-qtd-work:INPUT-VALUE
             ob-pcp-ref.qtd-sld-prog = tt-pcp-ref.qtd-sld-prog 
             ob-pcp-ref.qtd-proc = tt-pcp-ref.qtd-proc.

      ASSIGN fi-qtd-work:SCREEN-VALUE = "".

      ASSIGN tt-pcp.l-prog = YES 
             tt-pcp.l-proc = CAN-FIND (FIRST tt-pcp-ref  WHERE
                                             tt-pcp-ref.num-progr = tt-pcp.num-progr AND
                                             tt-pcp-ref.qtd-proc > 0).
      
      APPLY 'VALUE-CHANGED' TO br-pcp-ref IN FRAME {&FRAME-NAME}.
   END.
   ELSE DO.
      FIND ob-pcp-ref WHERE 
           ob-pcp-ref.num-progr = tt-pcp-ref.num-progr AND
           ob-pcp-ref.cod-refer = tt-pcp-ref.cod-refer
           NO-ERROR.

      IF NOT AVAIL ob-pcp-ref OR
         ob-pcp-ref.qtd-sld-prog = 0 THEN DO.
         MESSAGE "Saldo a Programar Cancelado por Outro Usuario..."
             VIEW-AS ALERT-BOX ERROR BUTTONS OK.

         APPLY 'VALUE-CHANGED' TO br-pcp-ref IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.
      ASSIGN fi-qtd-sld-prog:SCREEN-VALUE = STRING(ob-pcp-ref.qtd-sld-prog).

      ASSIGN fi-qtd-work:SCREEN-VALUE = fi-qtd-sld-prog:SCREEN-VALUE.
      ASSIGN bt-proc:SENSITIVE = YES
             bt-pron:SENSITIVE = NO.
    
      bt-proc:LOAD-IMAGE("image\im-up2.bmp").

      APPLY 'entry' TO fi-qtd-work.
      RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-pron
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-pron w-livre
ON CHOOSE OF bt-pron IN FRAME f-cad
DO:
   IF SELF:IMAGE = "image\im-nex.bmp" THEN DO.
      FIND ob-pcp-ref WHERE 
           ob-pcp-ref.num-progr = tt-pcp-ref.num-progr AND
           ob-pcp-ref.cod-refer = tt-pcp-ref.cod-refer
           NO-ERROR.

      IF fi-qtd-work:INPUT-VALUE > tt-pcp-ref.qtd-proc THEN DO.
         MESSAGE 'Quantidade Informada Maior que Quantidade em Processo...'
             VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY 'entry' TO fi-qtd-work.
         RETURN NO-APPLY.
      END.
      ASSIGN tt-pcp-ref.qtd-proc = tt-pcp-ref.qtd-proc - fi-qtd-work:INPUT-VALUE
             tt-pcp-ref.qtd-pron = tt-pcp-ref.qtd-pron + fi-qtd-work:INPUT-VALUE.

      ASSIGN ob-pcp-ref.qtd-proc = tt-pcp-ref.qtd-proc 
             ob-pcp-ref.qtd-pron = tt-pcp-ref.qtd-pron 
             ob-pcp-ref.usr-ult-pron = c-seg-usuario
             ob-pcp-ref.dt-ult-pron = TODAY.

      ASSIGN fi-qtd-work:SCREEN-VALUE = "".

      ASSIGN tt-pcp.l-pron = YES
             tt-pcp.l-proc = CAN-FIND (FIRST tt-pcp-ref  WHERE
                                             tt-pcp-ref.num-progr = tt-pcp.num-progr AND
                                             tt-pcp-ref.qtd-proc > 0).

      APPLY 'VALUE-CHANGED' TO br-pcp-ref IN FRAME {&FRAME-NAME}.
   END.
   ELSE DO.
      ASSIGN fi-qtd-work:SCREEN-VALUE = fi-qtd-acum-pron:SCREEN-VALUE.

      bt-prog:LOAD-IMAGE("image\im-pre.bmp").
      bt-proc:LOAD-IMAGE("image\im-up2.bmp").

      ASSIGN bt-proc:SENSITIVE = YES
             bt-prog:SENSITIVE = NO.

      APPLY 'entry' TO fi-qtd-work.
      RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-vapra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-vapra w-livre
ON CHOOSE OF bt-vapra IN FRAME f-cad
DO:
   RUN esdlg/d01essp0160.w (OUTPUT c-cod-refer).
   IF c-cod-refer <> "" THEN DO:
      FIND FIRST tt-pcp-ref WHERE
                 tt-pcp-ref.cod-refer = c-cod-refer NO-LOCK NO-ERROR.
      IF AVAIL tt-pcp-ref THEN
         h-qry-ref:REPOSITION-TO-ROWID(ROWID(tt-pcp-ref)) NO-ERROR. 
      ELSE
         MESSAGE "Referˆncia nÆo encontrada na sele‡Æo..."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.

      APPLY 'value-changed' TO br-pcp-ref IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-qtd-acum-proc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-qtd-acum-proc w-livre
ON LEAVE OF fi-qtd-acum-proc IN FRAME f-cad /* Qtde Acumulada */
DO:
    /*
  IF INPUT FRAME {&FRAME-NAME} fi-qtd-work1 > ref-item-ext.qtd-prog THEN DO:
     MESSAGE "Quantidade excede a quantidade Programada." VIEW-AS ALERT-BOX.
     APPLY 'entry' TO fi-qtd-work1 IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END.
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-qtd-sld-prog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-qtd-sld-prog w-livre
ON LEAVE OF fi-qtd-sld-prog IN FRAME f-cad /* Saldo */
DO:
   FIND ob-pcp-ref WHERE 
        ob-pcp-ref.num-progr = tt-pcp-ref.num-progr AND
        ob-pcp-ref.cod-refer = tt-pcp-ref.cod-refer
        NO-ERROR.

   /*
   IF SELF:INPUT-VALUE > ob-pcp-ref.qtd-sld-prog THEN DO:
      MESSAGE "Quantidade Informada NÇO pode ser maior que Saldo Programado..." VIEW-AS ALERT-BOX.

      ASSIGN SELF:SCREEN-VALUE = STRING(ob-pcp-ref.qtd-sld-prog).

      APPLY 'ENTRY' TO SELF.
      RETURN NO-APPLY.
   END.
   */

   ASSIGN tt-pcp-ref.qtd-sld-prog = SELF:INPUT-VALUE
          ob-pcp-ref.qtd-sld-prog = tt-pcp-ref.qtd-sld-prog.
          
   ASSIGN fi-qtd-sld-prog:SENSITIVE = NO.

   br-pcp-ref:REFRESH().

   IF ob-pcp-ref.qtd-sld-prog = 0 AND
      ob-pcp-ref.qtd-proc = 0 AND
      ob-pcp-ref.qtd-pron = 0 THEN DO.
      DELETE ob-pcp-ref.
      DELETE tt-pcp-ref.

      ASSIGN tt-pcp.l-prog = CAN-FIND (FIRST ob-pcp-ref  WHERE
                                             ob-pcp-ref.num-progr = tt-pcp.num-progr AND
                                             ob-pcp-ref.qtd-sld-prog > 0)
             tt-pcp.l-proc = CAN-FIND (FIRST ob-pcp-ref  WHERE
                                             ob-pcp-ref.num-progr = tt-pcp.num-progr AND
                                             ob-pcp-ref.qtd-proc > 0)
             tt-pcp.l-pron = CAN-FIND (FIRST ob-pcp-ref  WHERE
                                             ob-pcp-ref.num-progr = tt-pcp.num-progr AND
                                             ob-pcp-ref.qtd-pron > 0).

      IF NOT tt-pcp.l-prog AND 
         NOT tt-pcp.l-proc AND 
         NOT tt-pcp.l-pron AND
         NOT CAN-FIND (FIRST ob-pcp-ref WHERE
                             ob-pcp-ref.num-progr = tt-pcp.num-progr) THEN DO.
         FIND ob-pcp WHERE
              ob-pcp.num-progr = tt-pcp.num-progr NO-ERROR.

         DELETE ob-pcp.
         DELETE tt-pcp.
      END.

      RUN pi-open-query.
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


&Scoped-define SELF-NAME rs-ppp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-ppp w-livre
ON VALUE-CHANGED OF rs-ppp IN FRAME f-cad
DO:
   ASSIGN INPUT FRAME {&FRAME-NAME} rs-ppp.

   {&OPEN-QUERY-br-pcp}

   APPLY 'value-changed' TO br-pcp.
   APPLY 'entry' TO br-pcp.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-pcp
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-livre 


/* ***************************  Main Block  *************************** */
ASSIGN h-qry-ref = br-pcp-ref:QUERY.

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
       RUN set-position IN h_p-exihel ( 1.17 , 81.29 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             bt-elim-prog:HANDLE IN FRAME f-cad , 'AFTER':U ).
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
  DISPLAY rs-ppp fi-usr-ult-proc fi-dt-ult-proc fi-usr-ult-prog fi-usr-ult-pron 
          fi-qtd-acum-proc fi-dt-ult-prog fi-dt-ult-pron fi-qtd-sld-prog 
          fi-qtd-acum-pron fi-qtd-work edt_msg 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE RECT-11 RECT-12 RECT-13 RECT-14 rt-button bt-param bt-consulta 
         bt-elim-prog rs-ppp br-pcp br-pcp-ref bt-vapra bt-desenho 
         bt-anl-gerencial bt-imprime fi-qtd-work edt_msg 
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
  RUN pi-before-initialize.

  {include/win-size.i}

  /*{utp/ut9000.i "XX9999" "9.99.99.999"}*/

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  RUN pi-after-initialize.

  /* Busca Nome da Empresa */
  FIND FIRST param-global NO-LOCK NO-ERROR.
  FIND FIRST empresa WHERE
             empresa.ep-codigo = param-global.empresa-prin NO-LOCK NO-ERROR. 
  ASSIGN c-empresa = (IF AVAIL empresa THEN empresa.razao-social ELSE "").

  FIND FIRST param-dis NO-LOCK NO-ERROR.

  ASSIGN c-periodo-ini = '01' + STRING(YEAR(TODAY),'9999')
         c-periodo-fin = STRING(MONTH(TODAY),'99') + STRING(YEAR(TODAY),'9999')
         i-situacao = 1
         i-tp-acab = 3.
  
  IF p-it-codigo-182 <> "" THEN
     ASSIGN c-periodo-ini = '012008'
            bt-anl-gerencial:SENSITIVE = NO
            fi-qtd-work:SENSITIVE = NO
            rs-ppp:SCREEN-VALUE = '4'.
   
  ASSIGN bt-cancel-prog:SENSITIVE = c-seg-usuario = 'janete' OR c-seg-usuario = 'albino' OR c-seg-usuario = 'super'.

  APPLY 'choose' TO bt-param.
              
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
        "DATA: "                                  AT  63
        STRING(TODAY,"99/99/9999") FORMAT "X(10)" AT  69
        "HORA: "                                  AT  97
        STRING(TIME,"hh:mm:ss")                   AT 103
        "PAG:"                                    AT 128
        i-pag FORMAT ">>>"                        AT 133
        SKIP(1).

    PUT "RELATORIO PROGRAMACAO DE PRODUCAO" AT 45 
        SKIP(1). 

    PUT "Item   Descri»’o                 Acb Refer     Fundo Progr. Dt.Progr.  Programado Data Prog    Processo Data Proc      Pronto Data Pron " AT 1.   
    PUT "------ ------------------------- ------------- ----- ------ ---------- ---------- ---------- ---------- ---------- ---------- ----------" AT 1.

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
  RUN utp/ut-utils.p PERSISTENT SET h-prog.

  ASSIGN de-tot-des-prog = 0   de-tot-it-prog = 0   de-tot-ger-prog = 0   
         de-tot-des-proc = 0   de-tot-it-proc = 0   de-tot-ger-proc = 0  
         de-tot-des-pron = 0   de-tot-it-pron = 0   de-tot-ger-pron = 0.

  RUN esapi/saida-imp.p (OUTPUT i-saida,
                         OUTPUT c-saida,
                         OUTPUT i-num-copias,
                         OUTPUT l-ok).

  CASE i-saida:
      WHEN 1 THEN DO.
          OUTPUT TO VALUE(c-saida) CONVERT SOURCE "ibm850" PAGED PAGE-SIZE 61.
          PUT CONTROL "~033E~033(s18H".   
      END.
      WHEN 2 THEN
          OUTPUT TO VALUE(c-saida).
      WHEN 3 THEN DO.
          ASSIGN c-saida = SESSION:TEMP-DIRECTORY + "essp0182.tmp".
          OUTPUT TO VALUE(c-saida).
      END.
  END CASE.

  ASSIGN i-lin = 99
         i-pag =  1.

  FOR EACH tt-pcp WHERE
          (tt-pcp.l-prog AND (rs-ppp = 1 OR rs-ppp = 4) OR
           tt-pcp.l-proc AND (rs-ppp = 2 OR rs-ppp = 4) OR
           tt-pcp.l-pron AND (rs-ppp = 3 OR rs-ppp = 4)) NO-LOCK,
      EACH tt-pcp-ref  WHERE 
           tt-pcp-ref.num-progr = tt-pcp.num-progr AND
           (tt-pcp-ref.qtd-sld-prog > 0 AND (rs-ppp = 1 OR rs-ppp = 4) OR
            tt-pcp-ref.qtd-proc > 0 AND (rs-ppp = 2 OR rs-ppp = 4) OR
            tt-pcp-ref.qtd-pron > 0 AND (rs-ppp = 3 OR rs-ppp = 4)) NO-LOCK
           BREAK BY tt-pcp.it-codigo
                 BY tt-pcp.it-codigo + SUBSTR(tt-pcp-ref.cod-refer,3,4).

      FIND referencia-ext WHERE
           referencia-ext.cod-refer = tt-pcp-ref.cod-refer
           NO-LOCK NO-ERROR.
      

          IF i-lin > 61 THEN DO:
             RUN pi-imp-cabec.
             ASSIGN i-lin = 7.
          END.
    
          IF FIRST-OF(tt-pcp.it-codigo) THEN
             PUT tt-pcp.it-codigo        FORMAT "x(6)"       AT 1
                 fn-desc-item()          FORMAT "X(25)"      AT 8.
    
          /*IF FIRST-OF(tt-pcp.it-codigo +  SUBSTR(tt-pcp-ref.cod-refer,3,4)) THEN */
          IF AVAIL referencia-ext THEN DO.   
             PUT tt-pcp-ref.cod-refer    FORMAT "99.9999-9"  AT 34
                 referencia-ext.cod-fundo                    AT 48.
          END.
          ELSE DO.
              PUT tt-pcp-ref.cod-refer    FORMAT "99.9999-9"  AT 34
                  ""                                          AT 48.
          END. 

    
          IF i-lin > 61 THEN DO:
             RUN pi-imp-cabec.
             ASSIGN i-lin = 7.
          END.
    
          ASSIGN de-tot-des-prog = de-tot-des-prog + tt-pcp-ref.qtd-sld-prog
                 de-tot-des-proc = de-tot-des-proc + tt-pcp-ref.qtd-proc
                 de-tot-des-pron = de-tot-des-pron + tt-pcp-ref.qtd-pron
                 de-tot-it-prog = de-tot-it-prog + tt-pcp-ref.qtd-sld-prog
                 de-tot-it-proc = de-tot-it-proc + tt-pcp-ref.qtd-proc
                 de-tot-it-pron = de-tot-it-pron + tt-pcp-ref.qtd-pron
                 de-tot-ger-prog = de-tot-ger-prog + tt-pcp-ref.qtd-sld-prog
                 de-tot-ger-proc = de-tot-ger-proc + tt-pcp-ref.qtd-proc
                 de-tot-ger-pron = de-tot-ger-pron + tt-pcp-ref.qtd-pron.
    
          PUT tt-pcp.num-progr        FORMAT ">>>>>9"      AT 54
              tt-pcp.dt-progr                              AT 61
              tt-pcp-ref.qtd-sld-prog FORMAT ">>>,>>9.99"  AT 72
              tt-pcp-ref.dt-ult-prog                       AT 83
              tt-pcp-ref.qtd-proc     FORMAT ">>>,>>9.99"  AT 94
              tt-pcp-ref.dt-ult-proc                       AT 105
              tt-pcp-ref.qtd-pron     FORMAT ">>>,>>9.99"  AT 116
              tt-pcp-ref.dt-ult-pron                       AT 127
              SKIP.
    
          IF LAST-OF(tt-pcp.it-codigo + SUBSTR(tt-pcp-ref.cod-refer,3,4)) THEN DO.
             PUT SKIP
                 "TOTAL DESENHO...................:" AT 38
                 de-tot-des-prog         FORMAT ">>>,>>9.99" AT  72
                 de-tot-des-proc         FORMAT ">>>,>>9.99" AT  94 
                 de-tot-des-pron         FORMAT ">>>,>>9.99" AT  116
                 SKIP(1).
    
             ASSIGN de-tot-des-prog = 0
                    de-tot-des-proc = 0
                    de-tot-des-pron = 0.
    
             ASSIGN i-lin = i-lin + 1.
          END.
    
          IF LAST-OF(tt-pcp.it-codigo) THEN DO.
             PUT SKIP(1).
             ASSIGN i-lin = i-lin + 2.
    
             IF i-lin > 61 THEN DO:
                RUN pi-imp-cabec.
                ASSIGN i-lin = 7.
             END.
        
             IF i-lin <> 7 THEN 
                ASSIGN i-lin = i-lin + 1.
    
             PUT "TOTAL ITEM.................................:" AT 27.
             PUT de-tot-it-prog             FORMAT ">>>,>>9.99" AT 72
                 de-tot-it-proc             FORMAT ">>>,>>9.99" AT 94
                 de-tot-it-pron             FORMAT ">>>,>>9.99" AT 116.
        
             ASSIGN de-tot-it-prog = 0
                    de-tot-it-proc = 0
                    de-tot-it-pron = 0.
    
             ASSIGN i-lin = i-lin + 1.
    
             PUT SKIP(2).
          END.
      
      PUT SKIP.
  END.
  PUT SKIP(2).
  PUT "TOTAL GERAL................................:" AT 27
      de-tot-ger-prog   FORMAT ">>>>,>>9.99"  AT 71
      de-tot-ger-proc   FORMAT ">>>>,>>9.99"  AT 93
      de-tot-ger-pron   FORMAT ">>>>,>>9.99"  AT 115.

  OUTPUT CLOSE.
  
  IF i-saida = 3 THEN DO.
     RUN EXECUTE IN h-prog(INPUT "notepad.exe",
                               INPUT c-saida).
     DELETE PROCEDURE h-prog.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-open-query w-livre 
PROCEDURE pi-open-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    br-pcp-ref:DELETE-CURRENT-ROW() IN FRAME {&FRAME-NAME}.

    IF h-qry-ref:NUM-RESULTS = 0 THEN DO.
       br-pcp:DELETE-CURRENT-ROW() IN FRAME {&FRAME-NAME}.

       APPLY 'VALUE-CHANGED' TO br-pcp-ref.
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
    {utp/ut-liter.i Selecionando_Programa‡Æo_de_Produ‡Æo *}
    RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

    EMPTY TEMP-TABLE tt-pcp.
    EMPTY TEMP-TABLE tt-pcp-ref.

    RUN esapi/ret-udm.p (INPUT c-periodo-fin, OUTPUT c-dia).
    ASSIGN da-dt-progr-ini = DATE('01'  + SUBSTR(c-periodo-ini,1,2) + SUBSTR(c-periodo-ini,3,4))
           da-dt-progr-fin = DATE(c-dia + SUBSTR(c-periodo-fin,1,2) + SUBSTR(c-periodo-fin,3,4)).

    FOR EACH ob-pcp WHERE 
             ob-pcp.cod-estabel = c-cod-estabel AND
             ob-pcp.dt-progr  >= da-dt-progr-ini AND
             ob-pcp.dt-progr  <= da-dt-progr-fin AND
             ob-pcp.num-progr >= i-num-progr-ini AND
             ob-pcp.num-progr <= i-num-progr-fin AND
             ob-pcp.it-codigo >= c-it-codigo-ini AND
             ob-pcp.it-codigo <= c-it-codigo-fin NO-LOCK,
        EACH ob-pcp-ref WHERE
             ob-pcp-ref.num-progr  = ob-pcp.num-prog AND
             ob-pcp-ref.cod-refer >= c-cod-refer-ini AND
             ob-pcp-ref.cod-refer <= c-cod-refer-fin NO-LOCK
             BREAK BY ob-pcp.it-codigo BY ob-pcp-ref.cod-refer.

        IF i-situacao <> 3 AND
           ob-pcp-ref.situacao <> i-situacao  THEN NEXT.
           
        FIND item-ext WHERE
             item-ext.it-codigo = ob-pcp.it-codigo NO-LOCK NO-ERROR.
       /* IF AVAIL item-ext THEN DO:
           IF i-tp-tecido = 1 AND item-ext.indigo <> YES THEN NEXT. /* Somente Indigo */
           IF i-tp-tecido = 2 AND item-ext.indigo <> NO  THEN NEXT. /* Somente NÆo Indigo */
        END. */
    
        RUN pi-acompanhar IN h-acomp (INPUT "Data Programa‡Æo: " + STRING(ob-pcp.dt-progr) + " " + 
                                            "Item: " + ob-pcp.it-codigo).
        FIND ref-item-ext WHERE
             ref-item-ext.it-codigo = ob-pcp.it-codigo AND
             ref-item-ext.cod-refer = ob-pcp-ref.cod-refer NO-LOCK NO-ERROR.
    
        IF AVAIL ref-item-ext AND
           (ref-item-ext.cod-obsoleto < c-cod-obsoleto-ini OR
            ref-item-ext.cod-obsoleto > c-cod-obsoleto-fin) THEN NEXT.

        IF i-tp-acab = 1 AND SUBSTR(ob-pcp-ref.cod-refer,7,1) <> '0' THEN NEXT.
        IF i-tp-acab = 2 AND SUBSTR(ob-pcp-ref.cod-refer,7,1) = '0' THEN NEXT.

        FIND tt-pcp WHERE
             tt-pcp.num-progr  = ob-pcp.num-progr AND
             tt-pcp.it-codigo  = ob-pcp.it-codigo NO-ERROR.
        IF NOT AVAIL tt-pcp THEN DO:
           CREATE tt-pcp.
           ASSIGN tt-pcp.num-progr = ob-pcp.num-progr
                  tt-pcp.dt-progr  = ob-pcp.dt-progr
                  tt-pcp.it-codigo = ob-pcp.it-codigo
                  tt-pcp.situacao  = ob-pcp-ref.situacao.
        END.

        CREATE tt-pcp-ref.
        BUFFER-COPY ob-pcp-ref TO tt-pcp-ref.
    END.

    FOR EACH tt-pcp.
        ASSIGN tt-pcp.l-prog = CAN-FIND (FIRST tt-pcp-ref  WHERE
                                               tt-pcp-ref.num-progr = tt-pcp.num-progr AND
                                               tt-pcp-ref.qtd-sld-prog > 0)
               tt-pcp.l-proc = CAN-FIND (FIRST tt-pcp-ref  WHERE
                                               tt-pcp-ref.num-progr = tt-pcp.num-progr AND
                                               tt-pcp-ref.qtd-proc > 0)
               tt-pcp.l-pron = CAN-FIND (FIRST tt-pcp-ref  WHERE
                                               tt-pcp-ref.num-progr = tt-pcp.num-progr AND
                                               tt-pcp-ref.qtd-pron > 0).
    END.

    RUN pi-finalizar IN h-acomp.
    APPLY 'VALUE-CHANGED' TO rs-ppp IN FRAME {&FRAME-NAME}.

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
  {src/adm/template/snd-list.i "tt-pcp-ref"}
  {src/adm/template/snd-list.i "tt-pcp"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-desc-item w-livre 
FUNCTION fn-desc-item RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND ITEM WHERE
       ITEM.it-codigo = tt-pcp.it-codigo NO-LOCK NO-ERROR.

  RETURN item.desc-item.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-desc-situacao w-livre 
FUNCTION fn-desc-situacao RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  IF tt-pcp.situacao = 1 THEN
     RETURN "Aberta".  
  ELSE
     RETURN "Fechada". 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-fundo w-livre 
FUNCTION fn-fundo RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND referencia-ext WHERE
       referencia-ext.cod-refer = tt-pcp-ref.cod-refer
       NO-LOCK NO-ERROR.
  
  IF AVAIL referencia-ext THEN
     RETURN referencia-ext.cod-fundo.   /* Function return value. */
  ELSE
     RETURN ''.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-situacao w-livre 
FUNCTION fn-situacao RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

