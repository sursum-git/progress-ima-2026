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
    FIELD marca         AS CHAR FORMAT "x(2)"
    INDEX indice1 IS PRIMARY num-progr.

DEF TEMP-TABLE tt-pcp-ref
    FIELD num-progr     LIKE ob-pcp.num-progr
    FIELD cod-refer     LIKE ob-pcp-ref.cod-refer
    FIELD qtd-progr     LIKE ob-pcp-ref.qtd-progr    
    FIELD obs           AS CHAR
    FIELD cod-msg       AS INT FORMAT ">>>9"
    FIELD texto-msg     AS CHAR FORMAT "x(2000)"
    FIELD marca         AS CHAR FORMAT "x(2)"
    INDEX indice1 cod-refer.

DEF TEMP-TABLE tt-digita
    FIELD opcao AS CHAR
    FIELD campo AS CHAR
    FIELD valor AS CHAR.


DEF BUFFER b-tt-pcp-ref FOR tt-pcp-ref.
DEF BUFFER b-tt-pcp     FOR tt-pcp.

DEF {1} VAR p-progr-ini-166 LIKE ob-pcp.num-progr.
DEF {1} VAR p-progr-fin-166 LIKE ob-pcp.num-progr.

/* --- Local Variable Definitions --- */
DEF VAR h-acomp           AS HANDLE NO-UNDO.
DEF VAR h-query           AS HANDLE.
DEF VAR r-rowid           AS ROWID NO-UNDO.

DEF VAR c-desc-item   LIKE ITEM.desc-item.
DEF VAR c-desc-sit    AS CHAR FORMAT "x(14)".
DEF VAR i-num-progr   LIKE ob-pcp.num-progr.
DEF VAR c-lotes       AS CHAR FORMAT "x(18)".
DEF VAR arq-saida     AS CHAR FORMAT "x(45)".
DEF VAR c-arq-email   AS CHAR FORMAT "x(45)".

DEF VAR c-texto-msg  AS CHAR.
DEF VAR c-dia        AS CHAR.
DEF VAR da-dt-progr-ini LIKE ob-pcp.dt-progr.
DEF VAR da-dt-progr-fin LIKE ob-pcp.dt-progr.
DEF VAR i-lin        AS INT.
DEF VAR i-pag        AS INT.
DEF VAR de-tot-prog  AS DEC.
DEF VAR de-tot-ger   AS DEC.
DEF VAR de-tot-des   AS DEC.
DEF VAR c-mensagem    AS   CHAR.


/* Variaveis da Rotina de Impress∆o */
DEFINE VAR i-saida             AS INT.
DEFINE VAR c-saida             AS CHAR.
DEFINE VAR i-num-copias        AS INT.

/* Variavies de ParÉmetros */
DEFINE VAR c-periodo-ini      AS CHAR.
DEFINE VAR c-periodo-fin      AS CHAR.
DEFINE VAR i-num-progr-ini    LIKE ob-pcp.num-progr.
DEFINE VAR i-num-progr-fin    LIKE ob-pcp.num-progr          INIT "9999999".
DEFINE VAR c-it-codigo-ini    LIKE ob-etiqueta.it-codigo     INIT "".
DEFINE VAR c-it-codigo-fin    LIKE ob-etiqueta.it-codigo     INIT "ZZZZZZ".
DEFINE VAR c-cod-refer-ini    LIKE ob-etiqueta.cod-refer     INIT "". 
DEFINE VAR c-cod-refer-fin    LIKE ob-etiqueta.cod-refer     INIT "ZZZZZZZ".
DEFINE VAR c-cod-obsoleto-ini LIKE ref-item-ext.cod-obsoleto INIT "".
DEFINE VAR c-cod-obsoleto-fin LIKE ref-item-ext.cod-obsoleto INIT "Z".
DEFINE VAR i-sit-ini          AS INT INITIAL 1.
DEFINE VAR i-sit-fin          AS INT INITIAL 3.
DEFINE VAR i-tp-tecido        AS INT INITIAL 3.
DEFINE VAR l-ok               AS LOG.

/* Variaveis para o Excel */
DEFINE VAR c-empresa LIKE empresa.razao-social.
DEFINE VAR i-canal     AS INTEGER.
DEFINE VAR sys         AS INTEGER.
DEFINE VAR c-lin       AS CHARACTER FORMAT "x(500)".
DEFINE VAR aux-command AS CHAR FORMAT "x(100)".

{include/tt-edit.i}
{include/pi-edit.i}

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
&Scoped-define FIELDS-IN-QUERY-br-pcp tt-pcp.num-progr tt-pcp.dt-progr tt-pcp.it-codigo fn-desc-item() @ c-desc-item fn-desc-situacao() @ c-desc-sit tt-pcp.marca   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-pcp   
&Scoped-define SELF-NAME br-pcp
&Scoped-define QUERY-STRING-br-pcp FOR EACH tt-pcp WHERE                                  tt-pcp.situacao >= i-sit-ini AND                                  tt-pcp.situacao <= i-sit-fin NO-LOCK
&Scoped-define OPEN-QUERY-br-pcp OPEN QUERY {&SELF-NAME} FOR EACH tt-pcp WHERE                                  tt-pcp.situacao >= i-sit-ini AND                                  tt-pcp.situacao <= i-sit-fin NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-pcp tt-pcp
&Scoped-define FIRST-TABLE-IN-QUERY-br-pcp tt-pcp


/* Definitions for BROWSE br-pcp-ref                                    */
&Scoped-define FIELDS-IN-QUERY-br-pcp-ref tt-pcp-ref.cod-refer tt-pcp-ref.qtd-progr tt-pcp-ref.marca   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-pcp-ref   
&Scoped-define SELF-NAME br-pcp-ref
&Scoped-define QUERY-STRING-br-pcp-ref FOR EACH tt-pcp-ref WHERE                                  tt-pcp-ref.num-progr = tt-pcp.num-progr NO-LOCK                                  BY tt-pcp-ref.cod-refer
&Scoped-define OPEN-QUERY-br-pcp-ref OPEN QUERY {&SELF-NAME} FOR EACH tt-pcp-ref WHERE                                  tt-pcp-ref.num-progr = tt-pcp.num-progr NO-LOCK                                  BY tt-pcp-ref.cod-refer.
&Scoped-define TABLES-IN-QUERY-br-pcp-ref tt-pcp-ref
&Scoped-define FIRST-TABLE-IN-QUERY-br-pcp-ref tt-pcp-ref


/* Definitions for FRAME f-cad                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-cad ~
    ~{&OPEN-QUERY-br-pcp-ref}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-2 rt-buttom rt-buttom-2 rt-button ~
bt-param rs-situacao br-pcp bt-vapara bt-marca bt-desmarca bt-todos ~
bt-nenhum bt-msg br-pcp-ref bt-marca-ref edt_msg bt-desmarca-ref ~
bt-todos-ref bt-nenhum-ref 
&Scoped-Define DISPLAYED-OBJECTS rs-situacao edt_msg 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-6 bt-excel bt-email 

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
DEFINE BUTTON bt-desmarca AUTO-GO 
     IMAGE-UP FILE "image/im-cllps.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Desmarca"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-desmarca-ref AUTO-GO 
     IMAGE-UP FILE "image/im-cllps.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Desmarca"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-email 
     IMAGE-UP FILE "image/im-email.bmp":U
     LABEL "" 
     SIZE 4 BY 1.21 TOOLTIP "Envia e-mail das Programaá‰es Selecionadas".

DEFINE BUTTON bt-excel 
     IMAGE-UP FILE "image/excel.bmp":U
     LABEL "Button 2" 
     SIZE 4 BY 1.21 TOOLTIP "Gerar Planilha Excel".

DEFINE BUTTON bt-imprime 
     IMAGE-UP FILE "image/im-pri.bmp":U
     LABEL "" 
     SIZE 4 BY 1.21 TOOLTIP "Imprimir a Programaá∆o da Produá∆o".

DEFINE BUTTON bt-marca AUTO-GO 
     IMAGE-UP FILE "image/im-expan.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Marca"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-marca-ref AUTO-GO 
     IMAGE-UP FILE "image/im-expan.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Marca"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-msg AUTO-GO 
     IMAGE-UP FILE "image/gr-zoo.bmp":U
     LABEL "" 
     SIZE 7.72 BY 1 TOOLTIP "Escolhe Mensagem"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-nenhum AUTO-GO 
     IMAGE-UP FILE "image/im-nenhum.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Desmarca TODOS"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-nenhum-ref AUTO-GO 
     IMAGE-UP FILE "image/im-nenhum.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Desmarca TODOS"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-param AUTO-GO 
     IMAGE-UP FILE "image/im-fil.bmp":U
     LABEL "&Sair" 
     SIZE 4 BY 1.21 TOOLTIP "ParÉmetros"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-todos AUTO-GO 
     IMAGE-UP FILE "image/im-ran_a.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Marca TODOS"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-todos-ref AUTO-GO 
     IMAGE-UP FILE "image/im-ran_a.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Marca TODOS"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-vapara AUTO-GO 
     IMAGE-UP FILE "image/im-vapra.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Posicionar no Item"
     BGCOLOR 8 FONT 10.

DEFINE VARIABLE edt_msg AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 55.43 BY 5.42
     BGCOLOR 15 FGCOLOR 12 FONT 9 NO-UNDO.

DEFINE VARIABLE rs-situacao AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Abertas", 1,
"Encerradas", 2,
"Todas", 3
     SIZE 51 BY 1
     FGCOLOR 9 FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 47.57 BY 1
     BGCOLOR 8 FGCOLOR 0 .

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 7.72 BY 7.21
     BGCOLOR 8 FGCOLOR 0 .

DEFINE RECTANGLE rt-buttom-2
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 7 BY 6.46
     BGCOLOR 8 FGCOLOR 0 .

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 89.72 BY 1.46
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
      tt-pcp.num-progr                COLUMN-LABEL "Programaá∆o"    WIDTH  10
      tt-pcp.dt-progr                 COLUMN-LABEL "Dt.Programaá∆o"  WIDTH 11
      tt-pcp.it-codigo                COLUMN-LABEL "Item"      WIDTH  7
      fn-desc-item() @ c-desc-item    COLUMN-LABEL "Descriá∆o" WIDTH  36.1
      fn-desc-situacao() @ c-desc-sit COLUMN-LABEL "Situacao"  WIDTH  8   
      tt-pcp.marca                    COLUMN-LABEL "M" COLUMN-FGCOLOR 12 COLUMN-FONT 0    WIDTH 2.5
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 81 BY 7.25
         FONT 1
         TITLE "Programaá‰es de Produá∆o" ROW-HEIGHT-CHARS .63.

DEFINE BROWSE br-pcp-ref
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-pcp-ref w-livre _FREEFORM
  QUERY br-pcp-ref NO-LOCK DISPLAY
      tt-pcp-ref.cod-refer WIDTH  9   COLUMN-LABEL "Referància" FORMAT "99-9999-9"
      tt-pcp-ref.qtd-progr WIDTH 10.4 COLUMN-LABEL "Programado" FORMAT "->,>>>,>>9.99"
      tt-pcp-ref.marca                COLUMN-LABEL "M" COLUMN-FGCOLOR 12 COLUMN-FONT 0    WIDTH 2.5
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 26 BY 6.46
         FONT 1
         TITLE "Referàncias Programadas" ROW-HEIGHT-CHARS .63.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     bt-imprime AT ROW 1.17 COL 63.72
     bt-param AT ROW 1.21 COL 55.29
     bt-excel AT ROW 1.21 COL 59.43
     bt-email AT ROW 1.21 COL 68.14
     rs-situacao AT ROW 1.25 COL 2 NO-LABEL
     br-pcp AT ROW 2.75 COL 1.29
     bt-vapara AT ROW 3.08 COL 84.14
     bt-marca AT ROW 4.42 COL 84.14
     bt-desmarca AT ROW 5.71 COL 84.14
     bt-todos AT ROW 7.04 COL 84.14
     bt-nenhum AT ROW 8.42 COL 84.14
     bt-msg AT ROW 10.13 COL 83
     br-pcp-ref AT ROW 10.17 COL 1.14
     bt-marca-ref AT ROW 10.38 COL 28.43
     edt_msg AT ROW 11.21 COL 35 NO-LABEL
     bt-desmarca-ref AT ROW 11.88 COL 28.43
     bt-todos-ref AT ROW 13.42 COL 28.43
     bt-nenhum-ref AT ROW 15 COL 28.43
     "Texto da Mensagem" VIEW-AS TEXT
          SIZE 18 BY .54 AT ROW 10.38 COL 36
          BGCOLOR 8 FGCOLOR 12 FONT 6
     RECT-2 AT ROW 10.13 COL 35
     rt-buttom AT ROW 2.75 COL 82.72
     rt-buttom-2 AT ROW 10.08 COL 27.43
     rt-button AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 16.67.


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
         TITLE              = "Consulta Programaá∆o de Produá∆o"
         HEIGHT             = 15.88
         WIDTH              = 90.43
         MAX-HEIGHT         = 17
         MAX-WIDTH          = 96.57
         VIRTUAL-HEIGHT     = 17
         VIRTUAL-WIDTH      = 96.57
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
/* BROWSE-TAB br-pcp rs-situacao f-cad */
/* BROWSE-TAB br-pcp-ref bt-msg f-cad */
/* SETTINGS FOR BUTTON bt-email IN FRAME f-cad
   NO-ENABLE 6                                                          */
/* SETTINGS FOR BUTTON bt-excel IN FRAME f-cad
   NO-ENABLE 6                                                          */
/* SETTINGS FOR BUTTON bt-imprime IN FRAME f-cad
   NO-ENABLE                                                            */
ASSIGN 
       edt_msg:READ-ONLY IN FRAME f-cad        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-pcp
/* Query rebuild information for BROWSE br-pcp
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-pcp WHERE
                                 tt-pcp.situacao >= i-sit-ini AND
                                 tt-pcp.situacao <= i-sit-fin NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Query            is NOT OPENED
*/  /* BROWSE br-pcp */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-pcp-ref
/* Query rebuild information for BROWSE br-pcp-ref
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-pcp-ref WHERE
                                 tt-pcp-ref.num-progr = tt-pcp.num-progr NO-LOCK
                                 BY tt-pcp-ref.cod-refer.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-pcp-ref */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON END-ERROR OF w-livre /* Consulta Programaá∆o de Produá∆o */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON WINDOW-CLOSE OF w-livre /* Consulta Programaá∆o de Produá∆o */
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
ON VALUE-CHANGED OF br-pcp IN FRAME f-cad /* Programaá‰es de Produá∆o */
DO:
  {&OPEN-QUERY-br-pcp-ref}
  APPLY 'value-changed' TO br-pcp-ref.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-pcp-ref
&Scoped-define SELF-NAME br-pcp-ref
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-pcp-ref w-livre
ON VALUE-CHANGED OF br-pcp-ref IN FRAME f-cad /* Referàncias Programadas */
DO:
   ASSIGN edt_msg:SCREEN-VALUE = "".
   IF AVAIL tt-pcp-ref THEN DO.
      ASSIGN edt_msg:READ-ONLY    = YES
             bt-msg:SENSITIVE     = NO
             bt-excel:SENSITIVE   = NO
             bt-imprime:SENSITIVE = NO
             bt-email:SENSITIVE   = NO.

      IF CAN-FIND(FIRST tt-pcp-ref WHERE
                        tt-pcp-ref.marca = "*") THEN
          ASSIGN bt-excel:SENSITIVE   = YES
                 bt-imprime:SENSITIVE = YES
                 bt-email:SENSITIVE   = YES
                 bt-msg:SENSITIVE     = YES
                 edt_msg:READ-ONLY    = NO.


     IF CAN-FIND(FIRST tt-pcp WHERE
                       tt-pcp.marca = "*") THEN
         ASSIGN bt-excel:SENSITIVE   = YES
                bt-imprime:SENSITIVE = YES
                bt-email:SENSITIVE   = YES.  

      ASSIGN edt_msg:SCREEN-VALUE = tt-pcp-ref.obs + CHR(13).
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-desmarca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-desmarca w-livre
ON CHOOSE OF bt-desmarca IN FRAME f-cad
DO:
   ASSIGN tt-pcp.marca = "".
   br-pcp:REFRESH().
   APPLY 'value-changed' TO br-pcp IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-desmarca-ref
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-desmarca-ref w-livre
ON CHOOSE OF bt-desmarca-ref IN FRAME f-cad
DO:
   ASSIGN tt-pcp-ref.marca = "".
   IF NOT CAN-FIND(FIRST tt-pcp-ref WHERE
                         tt-pcp-ref.marca = "*") THEN
      ASSIGN tt-pcp.marca = "".
   br-pcp-ref:REFRESH().
   br-pcp:REFRESH().
   APPLY 'value-changed' TO br-pcp-ref IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-email
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-email w-livre
ON CHOOSE OF bt-email IN FRAME f-cad
DO:
   FIND FIRST tt-pcp WHERE
              tt-pcp.marca = "*" NO-LOCK NO-ERROR.
   IF NOT AVAIL tt-pcp THEN DO:
      MESSAGE "Favor marcar quais programaá‰es deseja imprimir ! ! !"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
   END.

   ASSIGN c-mensagem = "Segue Anexo Arquivo com Programaá‰es de Produá∆o" + CHR(10) + CHR(10) + CHR(10) +
                       "Atenciosamente," + CHR(10) +
                       "PCP".

   ASSIGN de-tot-prog = 0
          de-tot-ger = 0
          de-tot-des = 0.

   OUTPUT TO VALUE(c-arq-email).
       PUT "Progr.  Dt.Progr.   Item    Descriá∆o                  Referància  Fundo   Quantidade  Prioridade    Executado     Numero de OB" AT 1.
       PUT "------  ----------  ------  -------------------------  ----------  -----  -----------  ------------  ------------  ------------" AT 1.

       FOR EACH tt-pcp WHERE 
                tt-pcp.marca = "*" NO-LOCK.
    
           PUT tt-pcp.num-progr   FORMAT ">>>>>9" AT 1
               tt-pcp.dt-progr                    AT 9
               tt-pcp.it-codigo   FORMAT "x(6)"   AT 21
               fn-desc-item()     FORMAT "X(25)"  AT 29.
    
           ASSIGN de-tot-prog = 0.
           FOR EACH tt-pcp-ref  WHERE 
                    tt-pcp-ref.num-progr  = tt-pcp.num-progr NO-LOCK
                    BREAK BY SUBSTR(tt-pcp-ref.cod-refer,3,4).
    
               FIND referencia-ext WHERE
                    referencia-ext.cod-refer = tt-pcp-ref.cod-refer
                    NO-LOCK NO-ERROR.
    
               ASSIGN de-tot-prog = de-tot-prog + tt-pcp-ref.qtd-progr 
                      de-tot-ger  = de-tot-ger  + tt-pcp-ref.qtd-progr
                      de-tot-des = de-tot-des + tt-pcp-ref.qtd-progr.

               PUT tt-pcp-ref.cod-refer FORMAT "99.9999-9"    AT 56
                   referencia-ext.cod-fundo                   AT 69
                   tt-pcp-ref.qtd-progr FORMAT ">>>>,>>9.99"  AT 75
                   FILL("_",12)         FORMAT "x(12)"        AT 88
                   FILL("_",12)         FORMAT "x(12)"        AT 102
                   FILL("_",12)         FORMAT "x(12)"        AT 116 
                   tt-pcp-ref.obs                             AT 56  SKIP.
    
               IF tt-pcp-ref.texto-msg <> '' THEN DO.
                  PUT "Obs.:" AT 56.
    
                  RUN pi-print-editor(INPUT REPLACE(REPLACE(TRIM(tt-pcp-ref.texto-msg),CHR(13)," "),CHR(10)," "), INPUT 65). 
                  FOR EACH tt-editor:
                      PUT tt-editor.conteudo AT 62 SKIP.
                      ASSIGN i-lin = i-lin + 1.
                  END.
               END.
               PUT SKIP(1).
    
               IF LAST-OF(SUBSTR(tt-pcp-ref.cod-refer,3,4)) THEN DO.
                  PUT "TOTAL DESENHO...:" AT 56
                       de-tot-des         FORMAT ">>>>,>>9.99" AT  75
                       SKIP.
                  ASSIGN de-tot-des = 0.
               END.
    
               PUT SKIP(2).
           END.
           PUT SKIP(1).
    
           PUT "TOTAL ITEM.........................................:" AT 21.
           PUT de-tot-prog  FORMAT ">>>>,>>9.99" AT  75.
           ASSIGN de-tot-prog = 0.
       END.
       PUT SKIP(2).
       PUT "TOTAL GERAL........................................:" AT 21
           de-tot-ger  FORMAT ">>>>,>>9.99" AT  75.
   OUTPUT CLOSE.

   RUN esapi/esapi002.p (INPUT "janete.oliveira@teartextil.com.br", /* e-mail remetente */
                         INPUT param-dis.grp-recipiente, /* e-mail destinat†rio */  
                         INPUT "Programaá∆o de Produá∆o" , /* Assunto */
                         INPUT c-mensagem, /* Mensagem */
                         INPUT c-arq-email, /*arquivo anexo*/
                         INPUT YES). /* Mostra Erros */

   MESSAGE 'Email Enviado com Sucesso....'
       VIEW-AS ALERT-BOX INFO BUTTONS OK.
       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-excel w-livre
ON CHOOSE OF bt-excel IN FRAME f-cad /* Button 2 */
DO:
   RUN esdlg/d02essp0166.w (INPUT c-periodo-ini, OUTPUT arq-saida).
   IF arq-saida <> "" THEN DO:
      RUN pi-gera-excel (INPUT arq-saida).
      MESSAGE "O arquivo foi gerado em " TRIM(arq-saida) + "." SKIP 
              "Para acess†-lo,  abra-o atravÇs do Excel."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
   END.
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


&Scoped-define SELF-NAME bt-marca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-marca w-livre
ON CHOOSE OF bt-marca IN FRAME f-cad
DO:
   ASSIGN tt-pcp.marca = "*".
   br-pcp:REFRESH().
   APPLY 'value-changed' TO br-pcp IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-marca-ref
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-marca-ref w-livre
ON CHOOSE OF bt-marca-ref IN FRAME f-cad
DO:
   ASSIGN tt-pcp-ref.marca = "*"
          tt-pcp.marca     = "*".
   br-pcp-ref:REFRESH().
   br-pcp:REFRESH().
   APPLY 'value-changed' TO br-pcp-ref IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-msg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-msg w-livre
ON CHOOSE OF bt-msg IN FRAME f-cad
DO:
   ASSIGN c-texto-msg = "".
   RUN esp/essp0166b.p (OUTPUT c-texto-msg).

   ASSIGN tt-pcp-ref.obs = tt-pcp-ref.obs + c-texto-msg.

   APPLY 'value-changed' TO br-pcp-ref.
   APPLY 'ENTRY' TO edt_msg.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-nenhum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-nenhum w-livre
ON CHOOSE OF bt-nenhum IN FRAME f-cad
DO:
    FOR EACH tt-pcp SHARE-LOCK.
        ASSIGN tt-pcp.marca = "".
    END.
    br-pcp:REFRESH().
    APPLY 'value-changed' TO br-pcp IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-nenhum-ref
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-nenhum-ref w-livre
ON CHOOSE OF bt-nenhum-ref IN FRAME f-cad
DO:
    FOR EACH tt-pcp-ref WHERE 
             tt-pcp-ref.num-progr = tt-pcp.num-progr SHARE-LOCK.
        ASSIGN tt-pcp-ref.marca = "".
    END.
    ASSIGN tt-pcp.marca = "".
    br-pcp:REFRESH().
    br-pcp-ref:REFRESH().
    APPLY 'value-changed' TO br-pcp-ref IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-param
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-param w-livre
ON CHOOSE OF bt-param IN FRAME f-cad /* Sair */
DO:
    
   ASSIGN w-livre:SENSITIVE = NO.
   
   RUN esp/essp0166a.w (INPUT-OUTPUT TABLE tt-digita,
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
                        INPUT-OUTPUT i-tp-tecido,
                        INPUT-OUTPUT l-ok).

   ASSIGN w-livre:SENSITIVE = YES.

   IF l-ok THEN                                     
      RUN pi-processa.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-todos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-todos w-livre
ON CHOOSE OF bt-todos IN FRAME f-cad
DO:
   FOR EACH tt-pcp SHARE-LOCK.
       ASSIGN tt-pcp.marca = "*".
   END.

   br-pcp:REFRESH().
   APPLY 'value-changed' TO br-pcp IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-todos-ref
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-todos-ref w-livre
ON CHOOSE OF bt-todos-ref IN FRAME f-cad
DO:
   FOR EACH tt-pcp-ref WHERE 
            tt-pcp-ref.num-progr = tt-pcp.num-progr SHARE-LOCK.
       ASSIGN tt-pcp-ref.marca = "*".
   END.
   ASSIGN tt-pcp.marca = "*".
   br-pcp-ref:REFRESH().
   br-pcp:REFRESH().
   APPLY 'value-changed' TO br-pcp-ref IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-vapara
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-vapara w-livre
ON CHOOSE OF bt-vapara IN FRAME f-cad
DO:
  
   RUN esdlg/d01essp0166.w (OUTPUT i-num-progr).

   IF i-num-progr <> ? THEN DO:
      FIND tt-pcp WHERE
           tt-pcp.num-progr = i-num-progr NO-LOCK NO-ERROR. 

      IF NOT AVAIL tt-pcp THEN DO.
         MESSAGE "Programaá∆o n∆o est† contido na seleá∆o!"
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
         RETURN NO-APPLY.
      END.
       /* br-pcp:QUERY:REPOSITION-TO-ROWID(ROWID(tt-pcp)) NO-ERROR. */
      h-query:REPOSITION-TO-ROWID(ROWID(tt-pcp)) NO-ERROR.
      APPLY 'VALUE-CHANGED' TO br-pcp.
   END.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME edt_msg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL edt_msg w-livre
ON LEAVE OF edt_msg IN FRAME f-cad
DO:
  ASSIGN r-rowid = ROWID(tt-pcp). /* Salva Registro Atual */
  FOR EACH tt-pcp-ref WHERE
           tt-pcp-ref.num-progr = tt-pcp.num-progr AND
           tt-pcp-ref.marca     = "*" SHARE-LOCK.

      FIND ob-pcp-ref WHERE
           ob-pcp-ref.num-progr = tt-pcp-ref.num-progr AND
           ob-pcp-ref.cod-refer = tt-pcp-ref.cod-refer NO-ERROR.

      ASSIGN ob-pcp-ref.observ = SELF:SCREEN-VALUE
             tt-pcp-ref.obs    = SELF:SCREEN-VALUE.

      ASSIGN tt-pcp-ref.marca = "".
  END.
  ASSIGN tt-pcp.marca = "".
  br-pcp:QUERY:REPOSITION-TO-ROWID(r-rowid). /* Retorno Registro Salvo */
  br-pcp:REFRESH().
  br-pcp-ref:REFRESH().
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


&Scoped-define SELF-NAME rs-situacao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-situacao w-livre
ON VALUE-CHANGED OF rs-situacao IN FRAME f-cad
DO:
    FOR EACH tt-pcp SHARE-LOCK.
        ASSIGN tt-pcp.marca = "".
    END.
    FOR EACH tt-pcp-ref SHARE-LOCK.
        ASSIGN tt-pcp-ref.marca = "".
    END.
    ASSIGN i-sit-fin = INPUT FRAME {&FRAME-NAME} rs-situacao.

    ASSIGN i-sit-ini = IF i-sit-fin = 3
                       THEN 1 ELSE i-sit-fin.

    {&OPEN-QUERY-br-pcp}
    APPLY 'value-changed' TO br-pcp IN FRAME {&FRAME-NAME}.
    APPLY 'entry' TO br-pcp IN FRAME {&FRAME-NAME}.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-pcp
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-livre 


/* ***************************  Main Block  *************************** */
ASSIGN h-query = br-pcp:QUERY.

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
       RUN set-position IN h_p-exihel ( 1.17 , 74.14 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             bt-imprime:HANDLE IN FRAME f-cad , 'AFTER':U ).
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
  DISPLAY rs-situacao edt_msg 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE RECT-2 rt-buttom rt-buttom-2 rt-button bt-param rs-situacao br-pcp 
         bt-vapara bt-marca bt-desmarca bt-todos bt-nenhum bt-msg br-pcp-ref 
         bt-marca-ref edt_msg bt-desmarca-ref bt-todos-ref bt-nenhum-ref 
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

  /*{utp/ut9000.i "XX9999" "9.99.99.999"}*/

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  RUN pi-after-initialize.

  FIND FIRST param-dis NO-LOCK NO-ERROR.

  ASSIGN c-periodo-ini = '01' + STRING(YEAR(TODAY),'9999')
         c-periodo-fin = STRING(MONTH(TODAY),'99') + STRING(YEAR(TODAY),'9999')
         c-arq-email = SESSION:TEMP-DIRECTORY + "pcp.txt".

  IF p-progr-ini-166 = 0 THEN
     APPLY 'choose' TO bt-param.
  ELSE DO.
     ASSIGN i-num-progr-ini = p-progr-ini-166
            i-num-progr-fin = p-progr-fin-166
            c-periodo-ini = STRING(MONTH(TODAY - 365),'99') + STRING(YEAR(TODAY - 365),'9999').

     RUN pi-processa.
  END.
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
    DEF INPUT PARAMETER p-arquivo AS CHAR.

    DEF VAR h-prog as HANDLE NO-UNDO.
    RUN utp/ut-utils.p persistent set h-prog.

    RUN EXECUTE IN h-prog(INPUT "EXCEL.EXE", INPUT p-arquivo).

    DELETE PROCEDURE h-prog.
    PAUSE 5 NO-MESSAGE.

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
    DEF INPUT PARAMETER p-arq-saida AS CHAR FORMAT "x(45)".
    
    /*DEFINE FRAME frm_excel WITH SIZE 85.25 BY 15.42.*/
    DEFINE FRAME frm_excel WITH SIZE 2 BY 2 ROW 1 COLUMN 1.
    ENABLE ALL WITH FRAME frm_excel.
    
    RUN pi-abre-excel (INPUT "").
    PAUSE 3 NO-MESSAGE.

    DDE INITIATE sys FRAME FRAME frm_excel:HANDLE APPLICATION "EXCEL" TOPIC "System".
    DDE INITIATE i-canal FRAME FRAME frm_excel:HANDLE APPLICATION "EXCEL" TOPIC "Pasta1".

    /* Gerar Movimento Total na TEMP-TABLE */
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
        "PAG:"                                    AT 126
        i-pag FORMAT ">>>"                        AT 131
        SKIP(1).

    PUT "RELAT‡RIO PROGRAMAÄ«O DE PRODUÄ«O DO PER÷ODO: " AT 38. 
    PUT SUBSTR(c-periodo-ini, 1, 2) + "/" + SUBSTR(c-periodo-ini, 3,4) SKIP(1).

    PUT "Progr. Dt.Progr.  Item   Descriá∆o                 Fundo Acb Refer  Quantidade Prioridade           Executado            Numero de OB" AT 1.
    PUT "------ ---------- ------ ------------------------- ----- ---------  ---------- -------------------- -------------------- ------------" AT 1.
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

  FIND FIRST tt-pcp WHERE
             tt-pcp.marca = "*" NO-LOCK NO-ERROR.
  IF NOT AVAIL tt-pcp THEN DO:
     MESSAGE "Favor marcar quais programaá‰es deseja imprimir ! ! !"
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     RETURN NO-APPLY.
  END.

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
          OUTPUT TO VALUE(c-saida) CONVERT SOURCE "IBM850".
      END.
  END CASE.
  DO i-ct = 1 TO i-num-copias.
     ASSIGN de-tot-prog = 0
            de-tot-ger  = 0
            de-tot-des  = 0
            i-lin       = 99
            i-pag       =  1.

     FOR EACH tt-pcp WHERE 
              tt-pcp.marca     = "*"       AND
              tt-pcp.situacao >= i-sit-ini AND
              tt-pcp.situacao <= i-sit-fin NO-LOCK.

         IF i-lin > 61 THEN DO:
            RUN pi-imp-cabec.
            ASSIGN i-lin = 7.
         END.

         PUT tt-pcp.num-progr   FORMAT ">>>>>9" AT 1
             tt-pcp.dt-progr                    AT 8
             tt-pcp.it-codigo   FORMAT "x(6)"   AT 19
             fn-desc-item()     FORMAT "X(25)"  AT 27.

         ASSIGN de-tot-prog = 0.
         FOR EACH tt-pcp-ref  WHERE 
                  tt-pcp-ref.num-progr  = tt-pcp.num-progr AND
                  tt-pcp-ref.qtd-progr  > 0 NO-LOCK
                  BREAK BY SUBSTR(tt-pcp-ref.cod-refer,3,4).
               
             IF i-lin > 61 THEN DO:
                RUN pi-imp-cabec.
                ASSIGN i-lin = 7.
             END.

             FIND referencia-ext WHERE
                  referencia-ext.cod-refer = tt-pcp-ref.cod-refer
                  NO-LOCK NO-ERROR.

             ASSIGN de-tot-prog = de-tot-prog + tt-pcp-ref.qtd-progr 
                    de-tot-ger  = de-tot-ger  + tt-pcp-ref.qtd-progr
                    de-tot-des = de-tot-des + tt-pcp-ref.qtd-progr.

             IF AVAIL referencia-ext THEN
                PUT referencia-ext.cod-fundo                   AT  52.

             PUT tt-pcp-ref.cod-refer FORMAT "99.9999-9"    AT  58
                 tt-pcp-ref.qtd-progr FORMAT ">>>>,>>9.99"  AT  68
                 FILL("_",20)         FORMAT "x(20)"        AT  80
                 FILL("_",20)         FORMAT "x(20)"        AT 101
                 FILL("_",12)         FORMAT "x(12)"        AT 122.
             ASSIGN i-lin = i-lin + 1.

             IF tt-pcp-ref.obs <> "" THEN DO.
                IF i-lin > 61 THEN DO:
                   RUN pi-imp-cabec.
                   ASSIGN i-lin = 7.
                END.
                PUT "Obs.:" AT 63.

                RUN pi-print-editor(INPUT REPLACE(REPLACE(TRIM(tt-pcp-ref.obs),CHR(13)," "),CHR(10)," "), INPUT 65). 
                FOR EACH tt-editor:
                    PUT tt-editor.conteudo AT 68 SKIP.
                    ASSIGN i-lin = i-lin + 1.
                END.
             END.
             PUT SKIP(1).
             ASSIGN i-lin = i-lin + 1.
             IF LAST-OF(SUBSTR(tt-pcp-ref.cod-refer,3,4)) THEN DO.
                IF i-lin > 61 THEN DO:
                   RUN pi-imp-cabec.
                   ASSIGN i-lin = 7.
                END.
                PUT "TOTAL DESENHO...:" AT 51
                     de-tot-des         FORMAT ">>>>,>>9.99" AT  68
                     SKIP.
                ASSIGN de-tot-des = 0.

                ASSIGN i-lin = i-lin + 1.
             END.

             PUT SKIP(2).
             ASSIGN i-lin = i-lin + 2.
         END.
         PUT SKIP(1).
         ASSIGN i-lin = i-lin + 2.

         IF i-lin > 61 THEN DO:
            RUN pi-imp-cabec.
            ASSIGN i-lin = 7.
         END.
         IF i-lin <> 7 THEN 
            ASSIGN i-lin = i-lin + 1.

         PUT "TOTAL ITEM.........................................:" AT 16.
         PUT de-tot-prog  FORMAT ">>>>,>>9.99" AT  68.
         ASSIGN de-tot-prog = 0.
         ASSIGN i-lin = i-lin + 1.
     END.
     PUT SKIP(2).
     PUT "TOTAL GERAL........................................:" AT 16
         de-tot-ger  FORMAT ">>>>,>>9.99" AT  68.

     IF i-saida = 3 THEN DO.
        RUN EXECUTE IN h-prog(INPUT "notepad.exe",
                              INPUT c-saida).
        DELETE PROCEDURE h-prog.
     END.
     OUTPUT CLOSE.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-monta-planilha w-livre 
PROCEDURE pi-monta-planilha :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    /* Cabeáalho  da Planilha */
    ASSIGN c-Lin = c-empresa + "             " + " PROGRAMAÄ«O DE PRODUÄ«O NO PERIODO: "  + SUBSTR(c-periodo-ini,1,2) + "/" + SUBSTR(c-periodo-ini,3,4). 
    DDE SEND i-canal SOURCE c-Lin ITEM "L1C1".
    DDE EXECUTE i-canal COMMAND '[select("L1C1:L1C8")]'.
    /* Fonte, Tamanho, Negrito, It†lico, Sublinhado, Atachado, Cor(0=Autom†tico, 1=Preto, 2=Branco 3=Vermelho...) */
    DDE EXECUTE sys COMMAND "[format.font(~"Verdana~",12,True,False,False,False,3)]".

    /* Cabeáalho dos Dados */
    DDE SEND i-canal SOURCE "Nß PROGRAMAÄ«O"  ITEM "L3C1".
    DDE SEND i-canal SOURCE "DT.PROGRAMAÄ«O"  ITEM "L3C2".
    DDE SEND i-canal SOURCE "ITEM"            ITEM "L3C3".
    DDE SEND i-canal SOURCE "DESCRIÄ«O"       ITEM "L3C4".
    DDE SEND i-canal SOURCE "SITUAÄ«O"        ITEM "L3C5".
    DDE SEND i-canal SOURCE "REFER“NCIA"      ITEM "L3C6".
    DDE SEND i-canal SOURCE "QUANTIDADE"      ITEM "L3C7".
    DDE SEND i-canal SOURCE "OBSERVAÄ«O"      ITEM "L3C8".

    /* Formataá∆o das Celulas do Cabeáalho de Dados */
    DDE EXECUTE i-canal COMMAND '[select("L3C1:L3C8")]'.
    /* Fonte, Tamanho, Negrito, It†lico, Sublinhado, Atachado, Cor(0=Autom†tico, 1=Preto, 2=Branco 3=Vermelho...) */
    DDE EXECUTE sys COMMAND "[format.font(~"Verdana~",8,True,False,False,False,5)]".
    
    DDE EXECUTE i-canal COMMAND "[select(~"C1~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(18.00)]".
   
    DDE EXECUTE i-canal COMMAND "[select(~"C2~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(18.00)]".
    
    DDE EXECUTE i-canal COMMAND "[select(~"C3~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(6.00)]". 

    DDE EXECUTE i-canal COMMAND "[select(~"C4~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(25.00)]".

    DDE EXECUTE i-canal COMMAND "[select(~"C5~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(10.00)]".

    DDE EXECUTE i-canal COMMAND "[select(~"C6~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(12.00)]".

    DDE EXECUTE i-canal COMMAND "[select(~"C7~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(17.00)]".
    DDE EXECUTE sys     COMMAND "[format.number(~"###.###.##0,00~")]".
    DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]".

    DDE EXECUTE i-canal COMMAND "[select(~"C8~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(25.00)]".
    
    /* Montagem das Celulas de Dados */
    ASSIGN i-Lin = 4
           de-tot-ger  = 0.
    FOR EACH tt-pcp WHERE 
             tt-pcp.marca = "*" NO-LOCK.

        DDE SEND i-canal SOURCE STRING(tt-pcp.num-progr) ITEM "L" + TRIM(STRING(i-Lin)) + "C1". 
        DDE SEND i-canal SOURCE STRING(tt-pcp.dt-progr)  ITEM "L" + TRIM(STRING(i-Lin)) + "C2".
        DDE SEND i-canal SOURCE STRING(tt-pcp.it-codigo) ITEM "L" + TRIM(STRING(i-Lin)) + "C3".
        DDE SEND i-canal SOURCE STRING(fn-desc-item())     ITEM "L" + TRIM(STRING(i-Lin)) + "C4".
        DDE SEND i-canal SOURCE STRING(fn-desc-situacao()) ITEM "L" + TRIM(STRING(i-Lin)) + "C5".
        
        ASSIGN de-tot-prog = 0.
        FOR EACH tt-pcp-ref  WHERE 
                 tt-pcp-ref.num-progr  = tt-pcp.num-progr NO-LOCK.  
            DDE SEND i-canal SOURCE STRING(tt-pcp-ref.cod-refer, "99-9999-99")       ITEM "L" + TRIM(STRING(i-Lin)) + "C6".
            DDE SEND i-canal SOURCE STRING(tt-pcp-ref.qtd-progr)                     ITEM "L" + TRIM(STRING(i-Lin)) + "C7".
            DDE SEND i-canal SOURCE STRING(tt-pcp-ref.obs + " " + tt-pcp-ref.texto-msg)  ITEM "L" + TRIM(STRING(i-Lin)) + "C8".
            ASSIGN aux-command = '[select("L' + TRIM(STRING(i-Lin)) + 'C1:L' + TRIM(STRING(i-Lin)) + 'C8")]'.
            DDE EXECUTE i-canal COMMAND aux-command.
            /* Fonte, Tamanho, Negrito, It†lico, Sublinhado, Atachado, Cor(0=Autom†tico, 1=Preto, 2=Branco 3=Vermelho...) */
            DDE EXECUTE sys COMMAND "[format.font(~"Verdana~",8,False,False,False,False,0)]".
            ASSIGN de-tot-prog = de-tot-prog + tt-pcp-ref.qtd-progr 
                   de-tot-ger  = de-tot-ger  + tt-pcp-ref.qtd-progr.
            ASSIGN i-lin = i-lin + 1.
        END.
        DDE SEND i-canal SOURCE "TOTAL ITEM"   ITEM "L" + TRIM(STRING(i-Lin)) + "C6".
        DDE SEND i-canal SOURCE STRING(de-tot-prog) ITEM "L" + TRIM(STRING(i-Lin)) + "C7".
        ASSIGN aux-command = '[select("L' + TRIM(STRING(i-Lin)) + 'C1:L' + TRIM(STRING(i-Lin)) + 'C8")]'.
        DDE EXECUTE i-canal COMMAND aux-command.
        /* Fonte, Tamanho, Negrito, It†lico, Sublinhado, Atachado, Cor(0=Autom†tico, 1=Preto, 2=Branco 3=Vermelho...) */
        DDE EXECUTE sys COMMAND "[format.font(~"Verdana~",8,True,False,False,False,0)]".
        ASSIGN i-Lin = i-Lin + 1.
    END.
    DDE SEND i-canal SOURCE "TOTAL GERAL"  ITEM "L" + TRIM(STRING(i-Lin)) + "C6".
    DDE SEND i-canal SOURCE STRING(de-tot-ger) ITEM "L" + TRIM(STRING(i-Lin)) + "C7".
    ASSIGN aux-command = '[select("L' + TRIM(STRING(i-Lin)) + 'C1:L' + TRIM(STRING(i-Lin)) + 'C11")]'.
    DDE EXECUTE i-canal COMMAND aux-command.
    /* Fonte, Tamanho, Negrito, It†lico, Sublinhado, Atachado, Cor(0=Autom†tico, 1=Preto, 2=Branco 3=Vermelho...) */
    DDE EXECUTE sys COMMAND "[format.font(~"Verdana~",8,True,False,False,False,0)]".
    ASSIGN i-Lin = i-Lin + 1.

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
    {utp/ut-liter.i Selecionando_Programaá∆o_de_Produá∆o *}
    RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

    EMPTY TEMP-TABLE tt-pcp.
    EMPTY TEMP-TABLE tt-pcp-ref.


    /* Busca Nome da Empresa */
    FIND FIRST param-global NO-LOCK NO-ERROR.
    FIND FIRST empresa WHERE
               empresa.ep-codigo = param-global.empresa-prin NO-LOCK NO-ERROR. 
    ASSIGN c-empresa = (IF AVAIL empresa THEN empresa.razao-social ELSE "").

    RUN esapi/ret-udm.p (INPUT c-periodo-fin, OUTPUT c-dia).

    ASSIGN da-dt-progr-ini = DATE('01'  + SUBSTR(c-periodo-ini,1,2) + SUBSTR(c-periodo-ini,3,4))
           da-dt-progr-fin = DATE(c-dia + SUBSTR(c-periodo-fin,1,2) + SUBSTR(c-periodo-fin,3,4)).

    FOR EACH ob-pcp WHERE 
             ob-pcp.dt-progr  >= da-dt-progr-ini AND 
             ob-pcp.dt-progr  <= da-dt-progr-fin AND 
             ob-pcp.num-progr >= i-num-progr-ini AND
             ob-pcp.num-progr <= i-num-progr-fin AND 
             ob-pcp.it-codigo >= c-it-codigo-ini AND
             ob-pcp.it-codigo <= c-it-codigo-fin NO-LOCK.

       /* FIND item-ext WHERE
             item-ext.it-codigo = ob-pcp.it-codigo NO-LOCK NO-ERROR.
        IF AVAIL item-ext THEN DO:
           IF i-tp-tecido = 1 AND item-ext.indigo <> YES THEN NEXT. /* Somente Indigo */
           IF i-tp-tecido = 2 AND item-ext.indigo <> NO  THEN NEXT. /* Somente N∆o Indigo */
        END.*/
    
        RUN pi-acompanhar IN h-acomp (INPUT "Data Programaá∆o: " + STRING(ob-pcp.dt-progr) + " " + 
                                            "Item: " + ob-pcp.it-codigo).

        FOR EACH ob-pcp-ref WHERE
                 ob-pcp-ref.num-progr  = ob-pcp.num-prog AND
                 ob-pcp-ref.cod-refer >= c-cod-refer-ini AND
                 ob-pcp-ref.cod-refer <= c-cod-refer-fin NO-LOCK.
    
            FIND ref-item-ext WHERE
                 ref-item-ext.it-codigo = ob-pcp.it-codigo AND
                 ref-item-ext.cod-refer = ob-pcp-ref.cod-refer NO-LOCK NO-ERROR.
    
            IF AVAIL ref-item-ext AND
               (ref-item-ext.cod-obsoleto < c-cod-obsoleto-ini OR
                ref-item-ext.cod-obsoleto > c-cod-obsoleto-fin) THEN NEXT.
    
            FIND tt-pcp WHERE
                 tt-pcp.num-progr  = ob-pcp.num-progr AND
                 tt-pcp.it-codigo  = ob-pcp.it-codigo NO-ERROR.
            IF NOT AVAIL tt-pcp THEN DO:
               CREATE tt-pcp.
               ASSIGN tt-pcp.num-progr = ob-pcp.num-progr
                      tt-pcp.dt-progr  = ob-pcp.dt-progr
                      tt-pcp.it-codigo = ob-pcp.it-codigo
                      tt-pcp.situacao  = ob-pcp-ref.situacao
                      tt-pcp.marca     = IF p-progr-ini-166 > 0
                                         THEN "*" ELSE "".
            END.
    
            CREATE tt-pcp-ref.
            ASSIGN tt-pcp-ref.num-progr = ob-pcp-ref.num-progr
                   tt-pcp-ref.cod-refer = ob-pcp-ref.cod-refer
                   tt-pcp-ref.obs       = ob-pcp-ref.observ
                   tt-pcp-ref.qtd-progr = ob-pcp-ref.qtd-sld-prog.
        END.
    END.

    RUN pi-finalizar in h-acomp.

    APPLY 'VALUE-CHANGED' TO rs-situacao IN FRAME {&FRAME-NAME}.
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

