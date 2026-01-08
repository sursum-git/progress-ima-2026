&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
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
{include/i-prgvrs.i XX9999 9.99.99.999}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Preprocessadores do Template de Relat¢rio                            */
/* Obs: Retirar o valor do preprocessador para as p†ginas que n∆o existirem  */

&GLOBAL-DEFINE PGLAY f-pg-lay
&GLOBAL-DEFINE PGSEL
&GLOBAL-DEFINE PGPAR f-pg-par
&GLOBAL-DEFINE PGLOG f-pg-log

/* Parameters Definitions ---                                           */

/* Temporary Table Definitions ---                                      */

define temp-table tt-param
    FIELD destino          as integer
    FIELD arq-destino      AS CHAR
    field arq-entrada      as char
    field todos            as integer
    field usuario          as char
    field data-exec        as date
    field hora-exec        as integer
    FIELD cod-estabel      AS CHAR
    FIELD tg-pedido        AS LOG
    FIELD tg-item          AS LOG
    FIELD tg-avulsa        AS LOG
    FIELD tg-inventario    AS LOG
    FIELD tg-cancel-res    AS LOG
    FIELD tg-reimp-etq     AS LOG
    FIELD tg-localiz       AS LOG
    FIELD tg-conf-localiz  AS LOG
    FIELD tg-imp-sit-etq   AS LOG
    FIELD tg-transf-etq    AS LOG
    FIELD nr-pedcli        LIKE ped-venda.nr-pedcli
    FIELD nome-abrev       LIKE ped-venda.nome-abrev
    FIELD nr-coletor       AS INT
    FIELD nro-docto        AS INT
    FIELD dt-invent        AS DATE.


/* Transfer Definitions */

def var raw-param        as raw no-undo.

def temp-table tt-raw-digita
   field raw-digita      as raw.
                    
/* Local Variable Definitions ---                                       */

DEF {1} VAR g-tp-dados AS CHAR.
DEF     VAR c-arq-bat  AS CHAR.
DEF     VAR i-handle   AS INTEGER NO-UNDO.

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

def var l-ok               as logical no-undo.
def var c-arq-digita       as char    no-undo.
def var c-terminal         as char    no-undo.
def var c-arq-term         as char    no-undo.

DEF VAR c-cod-estabel      AS CHAR.


{include/i-imdef.i}

PROCEDURE FindWindowA EXTERNAL "USER32.DLL":
    DEFINE INPUT  PARAMETER intClassName AS LONG.
    DEFINE INPUT  PARAMETER chrCaption   AS CHARACTER.
    DEFINE RETURN PARAMETER intHandle    AS LONG.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-impor
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-import

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS im-pg-lay im-pg-log im-pg-par bt-executar ~
bt-cancelar bt-ajuda 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ajuda 
     LABEL "Ajuda" 
     SIZE 10 BY 1
     FONT 1.

DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "Cancelar" 
     SIZE 10 BY 1
     FONT 1.

DEFINE BUTTON bt-executar 
     LABEL "Executar" 
     SIZE 10 BY 1
     FONT 1.

DEFINE IMAGE im-pg-lay
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.21.

DEFINE IMAGE im-pg-log
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.21.

DEFINE IMAGE im-pg-par
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.21.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 79 BY 1.42
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 0    
     SIZE 78.72 BY .13
     BGCOLOR 7 .

DEFINE RECTANGLE rt-folder
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 79 BY 11.38
     FGCOLOR 0 .

DEFINE RECTANGLE rt-folder-left
     EDGE-PIXELS 0    
     SIZE .43 BY 11.21
     BGCOLOR 15 .

DEFINE RECTANGLE rt-folder-right
     EDGE-PIXELS 0    
     SIZE .43 BY 11.17
     BGCOLOR 7 .

DEFINE RECTANGLE rt-folder-top
     EDGE-PIXELS 0    
     SIZE 78.72 BY .13
     BGCOLOR 15 .

DEFINE BUTTON bt-editar 
     LABEL "Editar Layout" 
     SIZE 20 BY 1.

DEFINE VARIABLE ed-layout AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL LARGE
     SIZE 76 BY 9.25
     FONT 2 NO-UNDO.

DEFINE BUTTON bt-arquivo-destino 
     IMAGE-UP FILE "image\im-sea":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE BUTTON bt-config-impr-destino 
     IMAGE-UP FILE "image\im-cfprt":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE VARIABLE c-arquivo-destino AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 40 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE text-destino AS CHARACTER FORMAT "X(256)":U INITIAL " Destino" 
      VIEW-AS TEXT 
     SIZE 6.14 BY .63 NO-UNDO.

DEFINE VARIABLE text-destino-2 AS CHARACTER FORMAT "X(256)":U INITIAL "Imprime" 
      VIEW-AS TEXT 
     SIZE 9 BY .63 NO-UNDO.

DEFINE VARIABLE text-modo AS CHARACTER FORMAT "X(256)":U INITIAL "Execuá∆o" 
      VIEW-AS TEXT 
     SIZE 10.86 BY .63 NO-UNDO.

DEFINE VARIABLE rs-destino AS INTEGER INITIAL 3 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Impressora", 1,
"Arquivo", 2,
"Terminal", 3
     SIZE 44 BY 1.08 NO-UNDO.

DEFINE VARIABLE rs-execucao AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "On-Line", 1,
"Batch", 2
     SIZE 27.72 BY .92 NO-UNDO.

DEFINE VARIABLE rs-todos AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Todos", 1,
"Rejeitados", 2
     SIZE 34 BY .79 NO-UNDO.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 47 BY 1.71.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 47 BY 2.92.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 47 BY 1.71.

DEFINE BUTTON bt-arquivo-entrada 
     IMAGE-UP FILE "image\im-sea":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE VARIABLE c-arquivo-entrada AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 40 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi-cod-estabel AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-invent AS DATE FORMAT "99/99/9999":U 
     LABEL "Data" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nome-estabel AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 39 BY .88
     BGCOLOR 8 FGCOLOR 12 FONT 6 NO-UNDO.

DEFINE VARIABLE fi-nr-coletor AS INTEGER FORMAT "9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 3.86 BY 1.38
     FONT 20 NO-UNDO.

DEFINE VARIABLE fi-nro-docto AS INTEGER FORMAT ">>>>,>,>9":U INITIAL 0 
     LABEL "Ficha" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE text-entrada AS CHARACTER FORMAT "X(256)":U INITIAL " Arquivo de Entrada" 
      VIEW-AS TEXT 
     SIZE 14.86 BY .63 NO-UNDO.

DEFINE VARIABLE rs-tp-import AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Autom†tico", 1,
"Manual", 2
     SIZE 10.29 BY 1.5 NO-UNDO.

DEFINE RECTANGLE RECT-35
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 24.86 BY 3.5.

DEFINE RECTANGLE RECT-37
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 47 BY 4.5.

DEFINE RECTANGLE RECT-39
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 25 BY 3.75.

DEFINE RECTANGLE RECT-40
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 47 BY 2.25.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 47 BY 2.

DEFINE VARIABLE tg-cancel-res AS LOGICAL INITIAL no 
     LABEL "Cancelamento de Reservas" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .75 NO-UNDO.

DEFINE VARIABLE tg-conf-localiz AS LOGICAL INITIAL no 
     LABEL "Conferància de Docas" 
     VIEW-AS TOGGLE-BOX
     SIZE 19.57 BY .75 NO-UNDO.

DEFINE VARIABLE tg-imp-sit AS LOGICAL INITIAL yes 
     LABEL "Imprimir Situaá∆o da Etiqueta" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY .83 NO-UNDO.

DEFINE VARIABLE tg-inventario AS LOGICAL INITIAL no 
     LABEL "Invent†rio" 
     VIEW-AS TOGGLE-BOX
     SIZE 10 BY 1.08 NO-UNDO.

DEFINE VARIABLE tg-item AS LOGICAL INITIAL no 
     LABEL "Separaá∆o por Item" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .75 NO-UNDO.

DEFINE VARIABLE tg-localiz AS LOGICAL INITIAL no 
     LABEL "Localizaá∆o de Etiquetas" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .75 NO-UNDO.

DEFINE VARIABLE tg-pedido AS LOGICAL INITIAL no 
     LABEL "Separaá∆o por Pedido" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .75 NO-UNDO.

DEFINE VARIABLE tg-reimp-etq AS LOGICAL INITIAL no 
     LABEL "Reimpress∆o de Etiquetas" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .75 NO-UNDO.

DEFINE VARIABLE tg-transf AS LOGICAL INITIAL no 
     LABEL "Transferància de Etiquetas" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .75 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-pg-lay
     ed-layout AT ROW 1 COL 1 NO-LABEL
     bt-editar AT ROW 10.38 COL 1 HELP
          "Dispara a Impress∆o do Layout"
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 76.86 BY 10.46.

DEFINE FRAME f-import
     bt-executar AT ROW 14.54 COL 3 HELP
          "Dispara a execuá∆o do relat¢rio"
     bt-cancelar AT ROW 14.54 COL 14 HELP
          "Cancelar"
     bt-ajuda AT ROW 14.54 COL 70 HELP
          "Ajuda"
     rt-folder-left AT ROW 2.54 COL 2.14
     rt-folder-right AT ROW 2.67 COL 80.43
     rt-folder-top AT ROW 2.54 COL 2.14
     RECT-6 AT ROW 13.75 COL 2.14
     RECT-1 AT ROW 14.29 COL 2
     rt-folder AT ROW 2.5 COL 2
     im-pg-lay AT ROW 1.5 COL 2
     im-pg-log AT ROW 1.5 COL 33.43
     im-pg-par AT ROW 1.5 COL 17.72
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81 BY 15
         DEFAULT-BUTTON bt-executar.

DEFINE FRAME f-pg-log
     rs-todos AT ROW 2.5 COL 3.29 NO-LABEL
     rs-destino AT ROW 5 COL 3.29 HELP
          "Destino de Impress∆o do Relat¢rio" NO-LABEL
     c-arquivo-destino AT ROW 6.25 COL 3.29 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL
     bt-arquivo-destino AT ROW 6.25 COL 43 HELP
          "Escolha do nome do arquivo"
     bt-config-impr-destino AT ROW 6.25 COL 43 HELP
          "Configuraá∆o da impressora"
     rs-execucao AT ROW 8.38 COL 3.14 HELP
          "Modo de Execuá∆o" NO-LABEL
     text-destino-2 AT ROW 1.71 COL 4 NO-LABEL
     text-destino AT ROW 4.25 COL 3.86 NO-LABEL
     text-modo AT ROW 7.63 COL 1.14 COLON-ALIGNED NO-LABEL
     RECT-11 AT ROW 2 COL 2
     RECT-7 AT ROW 4.54 COL 2
     RECT-9 AT ROW 7.92 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 76.29 BY 10.46
         FONT 1.

DEFINE FRAME f-pg-par
     fi-cod-estabel AT ROW 2.17 COL 3.57 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     fi-nome-estabel AT ROW 2.17 COL 7 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     tg-imp-sit AT ROW 10 COL 51.29
     fi-nr-coletor AT ROW 2.5 COL 51 COLON-ALIGNED NO-LABEL
     rs-tp-import AT ROW 2.5 COL 59 NO-LABEL
     fi-nro-docto AT ROW 8 COL 56 COLON-ALIGNED
     fi-dt-invent AT ROW 7 COL 56 COLON-ALIGNED
     tg-pedido AT ROW 5.5 COL 29.72
     tg-item AT ROW 4.5 COL 29.72
     tg-inventario AT ROW 5.75 COL 58.14
     tg-cancel-res AT ROW 5.5 COL 5
     tg-reimp-etq AT ROW 7.5 COL 5
     text-entrada AT ROW 8.67 COL 5 NO-LABEL
     c-arquivo-entrada AT ROW 9.67 COL 5 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL
     bt-arquivo-entrada AT ROW 9.67 COL 45 HELP
          "Escolha do nome do arquivo"
     tg-localiz AT ROW 6.5 COL 5
     tg-transf AT ROW 4.5 COL 5 WIDGET-ID 2
     tg-conf-localiz AT ROW 6.46 COL 29.72 WIDGET-ID 4
     " Baixa Coletor" VIEW-AS TEXT
          SIZE 10.86 BY .63 AT ROW 1.17 COL 52
     " Estabelecimento" VIEW-AS TEXT
          SIZE 12.43 BY .63 AT ROW 1.17 COL 3.57 WIDGET-ID 10
     RECT-35 AT ROW 1.5 COL 51
     RECT-37 AT ROW 4.08 COL 3
     RECT-39 AT ROW 5.5 COL 51
     RECT-8 AT ROW 9 COL 3
     RECT-40 AT ROW 1.5 COL 3 WIDGET-ID 8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3.08
         SIZE 76.29 BY 10.29
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: w-impor
   Allow: Basic,Browse,DB-Fields,Window,Query
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Importa Dados do Coletor"
         HEIGHT             = 15
         WIDTH              = 81.14
         MAX-HEIGHT         = 22.33
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 22.33
         VIRTUAL-WIDTH      = 114.29
         RESIZE             = yes
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-impor.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-import
   FRAME-NAME                                                           */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME f-import
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-6 IN FRAME f-import
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder IN FRAME f-import
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder-left IN FRAME f-import
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder-right IN FRAME f-import
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder-top IN FRAME f-import
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME f-pg-lay
                                                                        */
ASSIGN 
       ed-layout:RETURN-INSERTED IN FRAME f-pg-lay  = TRUE
       ed-layout:READ-ONLY IN FRAME f-pg-lay        = TRUE.

/* SETTINGS FOR FRAME f-pg-log
                                                                        */
/* SETTINGS FOR FILL-IN text-destino IN FRAME f-pg-log
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       text-destino:PRIVATE-DATA IN FRAME f-pg-log     = 
                "Destino".

/* SETTINGS FOR FILL-IN text-destino-2 IN FRAME f-pg-log
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       text-destino-2:PRIVATE-DATA IN FRAME f-pg-log     = 
                "Imprime".

/* SETTINGS FOR FILL-IN text-modo IN FRAME f-pg-log
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       text-modo:PRIVATE-DATA IN FRAME f-pg-log     = 
                "Execuá∆o".

/* SETTINGS FOR FRAME f-pg-par
   Custom                                                               */
ASSIGN 
       fi-cod-estabel:READ-ONLY IN FRAME f-pg-par        = TRUE.

/* SETTINGS FOR FILL-IN fi-dt-invent IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nome-estabel IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nro-docto IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN text-entrada IN FRAME f-pg-par
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       text-entrada:PRIVATE-DATA IN FRAME f-pg-par     = 
                " Arquivo de Entrada".

/* SETTINGS FOR TOGGLE-BOX tg-cancel-res IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-imp-sit IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-item IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-pedido IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-reimp-etq IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-transf IN FRAME f-pg-par
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-pg-log
/* Query rebuild information for FRAME f-pg-log
     _Query            is NOT OPENED
*/  /* FRAME f-pg-log */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Importa Dados do Coletor */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Importa Dados do Coletor */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda C-Win
ON CHOOSE OF bt-ajuda IN FRAME f-import /* Ajuda */
DO:
   {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-log
&Scoped-define SELF-NAME bt-arquivo-destino
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-arquivo-destino C-Win
ON CHOOSE OF bt-arquivo-destino IN FRAME f-pg-log
DO:
    {include/i-imarq.i c-arquivo-destino f-pg-log}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME bt-arquivo-entrada
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-arquivo-entrada C-Win
ON CHOOSE OF bt-arquivo-entrada IN FRAME f-pg-par
DO:
    {include/i-imarq.i c-arquivo-entrada f-pg-par}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-import
&Scoped-define SELF-NAME bt-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancelar C-Win
ON CHOOSE OF bt-cancelar IN FRAME f-import /* Cancelar */
DO:
   apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-log
&Scoped-define SELF-NAME bt-config-impr-destino
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-config-impr-destino C-Win
ON CHOOSE OF bt-config-impr-destino IN FRAME f-pg-log
DO:
   {include/i-imimp.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-lay
&Scoped-define SELF-NAME bt-editar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-editar C-Win
ON CHOOSE OF bt-editar IN FRAME f-pg-lay /* Editar Layout */
DO:
   {include/i-imedl.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-import
&Scoped-define SELF-NAME bt-executar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-executar C-Win
ON CHOOSE OF bt-executar IN FRAME f-import /* Executar */
DO:
   do  on error undo, return no-apply:
       run pi-executar.
   end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME fi-dt-invent
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-dt-invent C-Win
ON LEAVE OF fi-dt-invent IN FRAME f-pg-par /* Data */
DO:
    /*
  FIND LAST inv-acab WHERE
            inv-acab.data-invent = INPUT FRAME {&FRAME-NAME} fi-dt-invent AND
            SUBSTR(STRING(inv-acab.docto),1,5) = '1459' + fi-nr-coletor:SCREEN-VALUE
            USE-INDEX indice1 NO-LOCK NO-ERROR.

  ASSIGN fi-nro-docto:SCREEN-VALUE = IF AVAIL inv-acab
                                     THEN STRING(inv-acab.docto + 1)
                                     ELSE '1459' + fi-nr-coletor:SCREEN-VALUE + '01'.
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-nr-coletor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nr-coletor C-Win
ON LEAVE OF fi-nr-coletor IN FRAME f-pg-par
DO:
  IF SELF:INPUT-VALUE <> 0 THEN DO.
     IF tg-pedido:INPUT-VALUE THEN
        ASSIGN c-arquivo-entrada:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "N:\Coletor\Receber\PEDIDO" +
                                                  SELF:SCREEN-VALUE + ".TXT".
     IF tg-item:INPUT-VALUE THEN
        ASSIGN c-arquivo-entrada:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "N:\Coletor\Receber\ITEM" +
                                                   SELF:SCREEN-VALUE + ".TXT".

     IF tg-inventario:INPUT-VALUE THEN DO.
        /*
        FIND LAST inv-acab WHERE
                  inv-acab.data-invent = INPUT FRAME {&FRAME-NAME} fi-dt-invent AND
                  SUBSTR(STRING(inv-acab.docto),1,5) = '1459' + SELF:SCREEN-VALUE
                  USE-INDEX indice1 NO-LOCK NO-ERROR.

        ASSIGN fi-nro-docto:SCREEN-VALUE = IF AVAIL inv-acab
                                           THEN STRING(inv-acab.docto + 1)
                                           ELSE '1459' + SELF:SCREEN-VALUE + '01'.
        */                                   
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-import
&Scoped-define SELF-NAME im-pg-lay
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-lay C-Win
ON MOUSE-SELECT-CLICK OF im-pg-lay IN FRAME f-import
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME im-pg-log
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-log C-Win
ON MOUSE-SELECT-CLICK OF im-pg-log IN FRAME f-import
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME im-pg-par
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-par C-Win
ON MOUSE-SELECT-CLICK OF im-pg-par IN FRAME f-import
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-log
&Scoped-define SELF-NAME rs-destino
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-destino C-Win
ON VALUE-CHANGED OF rs-destino IN FRAME f-pg-log
DO:
do  with frame f-pg-log:
    case self:screen-value:
        when "1" then do:
            assign c-arquivo-destino:sensitive     = no
                   bt-arquivo-destino:visible      = no
                   bt-config-impr-destino:visible  = yes.
        end.
        when "2" then do:
            assign c-arquivo-destino:sensitive     = yes
                   bt-arquivo-destino:visible      = yes
                   bt-config-impr-destino:visible  = no.
        end.
        when "3" then do:
            assign c-arquivo-destino:sensitive     = no
                   bt-arquivo-destino:visible      = no
                   bt-config-impr-destino:visible  = no.
        end.
    end case.
end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-execucao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-execucao C-Win
ON VALUE-CHANGED OF rs-execucao IN FRAME f-pg-log
DO:
   {include/i-imrse.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME rs-tp-import
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-tp-import C-Win
ON VALUE-CHANGED OF rs-tp-import IN FRAME f-pg-par
DO:
   ASSIGN fi-nr-coletor:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
   IF SELF:INPUT-VALUE = 2 THEN
      ASSIGN fi-nr-coletor:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

   APPLY 'entry' TO fi-nr-coletor.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-cancel-res
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-cancel-res C-Win
ON VALUE-CHANGED OF tg-cancel-res IN FRAME f-pg-par /* Cancelamento de Reservas */
DO:
  IF SELF:SCREEN-VALUE = 'YES' THEN
     ASSIGN tg-pedido:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'
            tg-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'
            tg-inventario:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'
            tg-reimp-etq:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'
            tg-localiz:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'
            c-arquivo-entrada:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "N:\Coletor\Receber\CANCEL-RES.TXT".
  ELSE
     ASSIGN c-arquivo-entrada:SCREEN-VALUE IN FRAME {&FRAME-NAME} = " ".

  ASSIGN fi-dt-invent:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
         fi-nro-docto:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
         fi-dt-invent:SENSITIVE IN FRAME {&FRAME-NAME} = NO
         fi-nro-docto:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-conf-localiz
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-conf-localiz C-Win
ON VALUE-CHANGED OF tg-conf-localiz IN FRAME f-pg-par /* Conferància de Docas */
DO:
    IF SELF:SCREEN-VALUE = 'YES' THEN
       ASSIGN tg-pedido:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'
              tg-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'
              tg-inventario:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'
              tg-cancel-res:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'
              tg-reimp-etq:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'
              c-arquivo-entrada:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "c:\temp\CONF-LOC.TXT".
    ELSE
       ASSIGN c-arquivo-entrada:SCREEN-VALUE IN FRAME {&FRAME-NAME} = " ".

    ASSIGN tg-imp-sit:SENSITIVE IN FRAME {&FRAME-NAME} = SELF:INPUT-VALUE.

    ASSIGN fi-dt-invent:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
           fi-nro-docto:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
           fi-dt-invent:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           fi-nro-docto:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           fi-nr-coletor:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-inventario
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-inventario C-Win
ON VALUE-CHANGED OF tg-inventario IN FRAME f-pg-par /* Invent†rio */
DO:
   IF SELF:SCREEN-VALUE = 'YES' THEN DO.
      ASSIGN tg-pedido:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'
             tg-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'
             tg-cancel-res:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'
             tg-reimp-etq:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'
             tg-localiz:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'
             c-arquivo-entrada:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "N:\Coletor\Receber\INVENTARIO.TXT".

      ASSIGN fi-nro-docto:SENSITIVE IN FRAME {&FRAME-NAME} = YES
             fi-dt-invent:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

      ASSIGN fi-dt-invent:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY).
      APPLY 'entry' TO fi-dt-invent.
      RETURN NO-APPLY.
   END.
   ELSE
      ASSIGN c-arquivo-entrada:SCREEN-VALUE IN FRAME {&FRAME-NAME} = " "
             fi-dt-invent:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
             fi-nro-docto:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
             fi-nro-docto:SENSITIVE IN FRAME {&FRAME-NAME} = NO
             fi-dt-invent:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-item
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-item C-Win
ON VALUE-CHANGED OF tg-item IN FRAME f-pg-par /* Separaá∆o por Item */
DO:
    IF SELF:SCREEN-VALUE = 'YES' THEN
       ASSIGN tg-pedido:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'
              tg-inventario:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'
              tg-inventario:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'
              tg-cancel-res:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'
              tg-reimp-etq:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'
              tg-localiz:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'
              c-arquivo-entrada:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "N:\Coletor\Receber\ITEM" +
                                                                      fi-nr-coletor:SCREEN-VALUE + ".TXT".

    ELSE
       ASSIGN c-arquivo-entrada:SCREEN-VALUE IN FRAME {&FRAME-NAME} = " ".

    ASSIGN fi-dt-invent:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
           fi-nro-docto:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
           fi-nr-coletor:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-localiz
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-localiz C-Win
ON VALUE-CHANGED OF tg-localiz IN FRAME f-pg-par /* Localizaá∆o de Etiquetas */
DO:
    IF SELF:SCREEN-VALUE = 'YES' THEN
       ASSIGN tg-pedido:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'
              tg-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'
              tg-inventario:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'
              tg-cancel-res:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'
              tg-reimp-etq:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'
              c-arquivo-entrada:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "c:\temp\LOCALIZ.TXT".
    ELSE
       ASSIGN c-arquivo-entrada:SCREEN-VALUE IN FRAME {&FRAME-NAME} = " ".

    ASSIGN tg-imp-sit:SENSITIVE IN FRAME {&FRAME-NAME} = SELF:INPUT-VALUE.

    ASSIGN fi-dt-invent:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
           fi-nro-docto:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
           fi-dt-invent:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           fi-nro-docto:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           fi-nr-coletor:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-pedido
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-pedido C-Win
ON VALUE-CHANGED OF tg-pedido IN FRAME f-pg-par /* Separaá∆o por Pedido */
DO:
  IF SELF:SCREEN-VALUE = 'YES' THEN 
     ASSIGN tg-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'
            tg-inventario:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'
            tg-cancel-res:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'
            tg-reimp-etq:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'
            tg-localiz:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'
            c-arquivo-entrada:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "N:\Coletor\Receber\PEDIDO" +
                                                                    fi-nr-coletor:SCREEN-VALUE + ".TXT".
  ELSE
     ASSIGN c-arquivo-entrada:SCREEN-VALUE IN FRAME {&FRAME-NAME} = " ".

  ASSIGN fi-dt-invent:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
         fi-nro-docto:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
         fi-dt-invent:SENSITIVE IN FRAME {&FRAME-NAME} = NO
         fi-nro-docto:SENSITIVE IN FRAME {&FRAME-NAME} = NO
         fi-nr-coletor:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-reimp-etq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-reimp-etq C-Win
ON VALUE-CHANGED OF tg-reimp-etq IN FRAME f-pg-par /* Reimpress∆o de Etiquetas */
DO:
    IF SELF:SCREEN-VALUE = 'YES' THEN
       ASSIGN tg-pedido:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'
              tg-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'
              tg-inventario:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'
              tg-cancel-res:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'
              tg-localiz:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'
              c-arquivo-entrada:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "c:\temp\LOCALIZ.TXT".
    ELSE
       ASSIGN c-arquivo-entrada:SCREEN-VALUE IN FRAME {&FRAME-NAME} = " ".

    ASSIGN fi-dt-invent:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
           fi-nro-docto:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
           fi-nro-docto:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           fi-dt-invent:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-transf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-transf C-Win
ON VALUE-CHANGED OF tg-transf IN FRAME f-pg-par /* Transferància de Etiquetas */
DO:
    IF SELF:SCREEN-VALUE = 'YES' THEN
       ASSIGN tg-pedido:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'
              tg-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'
              tg-inventario:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'
              tg-cancel-res:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'
              tg-reimp-etq:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'
              c-arquivo-entrada:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "N:\Coletor\Receber\TRANSF.TXT".
    ELSE
       ASSIGN c-arquivo-entrada:SCREEN-VALUE IN FRAME {&FRAME-NAME} = " ".

    ASSIGN tg-imp-sit:SENSITIVE IN FRAME {&FRAME-NAME} = SELF:INPUT-VALUE.

    ASSIGN fi-dt-invent:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
           fi-nro-docto:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
           fi-dt-invent:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           fi-nro-docto:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           fi-nr-coletor:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-import
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{utp/ut9000.i "ESSP0105" "2.04.00.001"}

/*:T inicializaá‰es do template de importaá∆o */
{include/i-imini.i}

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

{include/i-imlbl.i}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO  ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    RUN enable_UI.
  
    FIND FIRST para-ped NO-LOCK NO-ERROR.
    FIND estabelec WHERE
         estabelec.cod-estabel = para-ped.estab-padrao
         NO-LOCK NO-ERROR.
    ASSIGN fi-cod-estabel:SCREEN-VALUE IN FRAME f-pg-par = estabelec.cod-estabel
           fi-nome-estabel:SCREEN-VALUE IN FRAME f-pg-par = estabelec.nome.

    ASSIGN fi-cod-estabel:FONT IN FRAME f-pg-par = 6
           fi-nome-estabel:FONT IN FRAME f-pg-par = 6.

    CASE g-tp-dados:
        WHEN "PED" THEN DO.
             ASSIGN tg-pedido:SCREEN-VALUE IN FRAME f-pg-par = 'YES'.
             APPLY 'VALUE-CHANGED' TO tg-pedido IN FRAME f-pg-par.
        END.
        WHEN "ITEM" THEN DO. 
            ASSIGN tg-item:SCREEN-VALUE IN FRAME f-pg-par = 'YES'.
            APPLY 'VALUE-CHANGED' TO tg-item IN FRAME f-pg-par.
        END.
        WHEN "INVENTARIO" THEN DO.
            ASSIGN tg-inventario:SCREEN-VALUE IN FRAME f-pg-par = 'YES'.
            APPLY 'VALUE-CHANGED' TO tg-inventario IN FRAME f-pg-par.
        END.
        WHEN "LOCALIZACAO" THEN DO.
            ASSIGN rs-tp-import:SCREEN-VALUE IN FRAME f-pg-par = "2"
                   tg-localiz:SCREEN-VALUE IN FRAME f-pg-par = "yes"
                   c-arquivo-entrada:SCREEN-VALUE IN FRAME f-pg-par = "N:\Coletor\Receber\LOCALIZ.TXT".


            /*
             ASSIGN tg-localiz:SCREEN-VALUE IN FRAME f-pg-par = 'YES'.
             APPLY 'VALUE-CHANGED' TO tg-localiz IN FRAME f-pg-par.
             ASSIGN fi-nr-coletor:SENSITIVE IN FRAME f-pg-par = NO
                    rs-tp-import:SENSITIVE IN FRAME f-pg-par = NO
                    tg-pedido:SENSITIVE IN FRAME f-pg-par = NO
                    tg-cancel-res:SENSITIVE IN FRAME f-pg-par = NO
                    tg-localiz:SENSITIVE IN FRAME f-pg-par = NO
                    tg-reimp-etq:SENSITIVE IN FRAME f-pg-par = NO
                    tg-item:SENSITIVE IN FRAME f-pg-par = NO
                    tg-avulsa:SENSITIVE IN FRAME f-pg-par = NO
                    fi-cod-emit:SENSITIVE IN FRAME f-pg-par = NO
                    fi-nr-pedcli:SENSITIVE IN FRAME f-pg-par = NO
                    tg-inventario:SENSITIVE IN FRAME f-pg-par = NO
                    fi-dt-invent:SENSITIVE IN FRAME f-pg-par = NO
                    fi-nro-docto:SENSITIVE IN FRAME f-pg-par = NO
                    c-arquivo-entrada:SENSITIVE IN FRAME f-pg-par = NO
                    bt-arquivo-entrada:SENSITIVE IN FRAME f-pg-par = NO.
                    */
        END.
    END CASE.

    {include/i-immbl.i "im-pg-par"}

    /*{include/i-imvrf.i &programa=ESSP0105 &versao-layout=001}*/
  
    APPLY 'ENTRY' TO fi-cod-estabel IN FRAME f-pg-par.

    IF c-seg-usuario = "esoares"  THEN 
       ASSIGN tg-reimp-etq:SENSITIVE IN FRAME f-pg-par = YES.
    IF c-seg-usuario = "fandrade" THEN
        ASSIGN tg-reimp-etq:SENSITIVE IN FRAME f-pg-par = YES.
    IF c-seg-usuario = "emagno" THEN
        ASSIGN tg-reimp-etq:SENSITIVE IN FRAME f-pg-par = YES.

    IF  NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects C-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available C-Win  _ADM-ROW-AVAILABLE
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
  ENABLE im-pg-lay im-pg-log im-pg-par bt-executar bt-cancelar bt-ajuda 
      WITH FRAME f-import IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-import}
  DISPLAY ed-layout 
      WITH FRAME f-pg-lay IN WINDOW C-Win.
  ENABLE ed-layout bt-editar 
      WITH FRAME f-pg-lay IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-lay}
  DISPLAY rs-todos rs-destino c-arquivo-destino rs-execucao 
      WITH FRAME f-pg-log IN WINDOW C-Win.
  ENABLE RECT-11 RECT-7 RECT-9 rs-todos rs-destino c-arquivo-destino 
         bt-arquivo-destino bt-config-impr-destino rs-execucao 
      WITH FRAME f-pg-log IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-log}
  DISPLAY fi-cod-estabel fi-nome-estabel tg-imp-sit fi-nr-coletor rs-tp-import 
          fi-nro-docto fi-dt-invent tg-pedido tg-item tg-inventario 
          tg-cancel-res tg-reimp-etq c-arquivo-entrada tg-localiz tg-transf 
          tg-conf-localiz 
      WITH FRAME f-pg-par IN WINDOW C-Win.
  ENABLE fi-cod-estabel fi-nr-coletor rs-tp-import tg-inventario 
         c-arquivo-entrada bt-arquivo-entrada tg-localiz tg-conf-localiz 
         RECT-35 RECT-37 RECT-39 RECT-8 RECT-40 
      WITH FRAME f-pg-par IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-par}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit C-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-executar C-Win 
PROCEDURE pi-executar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

do  on error undo, return error
    on stop  undo, return error:     

    {include/i-rpexa.i}

    if input frame f-pg-log rs-destino = 2 and
       input frame f-pg-log rs-execucao = 1 then do:
       run utp/ut-vlarq.p (input input frame f-pg-log c-arquivo-destino).
       if return-value = "nok":U then do:
          run utp/ut-msgs.p (input "show":U,
                             input 73,
                             input "").
          apply 'mouse-select-click':U to im-pg-log in frame f-import.
          apply 'entry':U to c-arquivo-destino in frame f-pg-log.                   
          return error.
       end.
    end.
    
    IF INPUT FRAME f-pg-par rs-tp-import = 1 AND 
       fi-nr-coletor:SENSITIVE IN FRAME f-pg-par AND
       INPUT FRAME f-pg-par fi-nr-coletor = 0 THEN DO.
       MESSAGE "N£mero do Coletor deve ser Informado..." VIEW-AS ALERT-BOX.
       APPLY "MOUSE-SELECT-CLICK":U TO im-pg-par IN FRAME f-import.
       APPLY 'entry' TO fi-nr-coletor IN FRAME f-pg-par.
       RETURN ERROR.
    END.

    IF INPUT FRAME f-pg-par tg-inventario = YES  AND
       INPUT FRAME f-pg-par fi-nro-docto = 0 THEN DO.
       MESSAGE "N£mero do Documnto deve ser Informado..." VIEW-AS ALERT-BOX.
       APPLY "MOUSE-SELECT-CLICK":U TO im-pg-par IN FRAME f-import.
       APPLY 'entry' TO fi-nro-docto IN FRAME f-pg-par.
       RETURN ERROR.
    END.

    IF INPUT FRAME f-pg-par tg-inventario = YES  AND
       INPUT FRAME f-pg-par fi-dt-invent = ? THEN DO.
       MESSAGE "Data do Inventario deve ser Informada..." VIEW-AS ALERT-BOX.
       APPLY "MOUSE-SELECT-CLICK":U TO im-pg-par IN FRAME f-import.
       APPLY 'entry' TO fi-dt-invent IN FRAME f-pg-par.
       RETURN ERROR.
    END.



    ASSIGN c-cod-estabel = para-ped.estab-padr.

    /*:T Coloque aqui as validaá‰es das outras p†ginas, lembrando que elas
       devem apresentar uma mensagem de erro cadastrada, posicionar na p†gina 
       com problemas e colocar o focus no campo com problemas             */    

    EMPTY TEMP-TABLE tt-param.
         
    CREATE tt-param.
    ASSIGN tt-param.usuario         = c-seg-usuario
           tt-param.destino         = input frame f-pg-log rs-destino
           tt-param.arq-entrada     = input frame f-pg-par c-arquivo-entrada
           tt-param.data-exec       = today
           tt-param.hora-exec       = time.

    IF tt-param.destino = 1 THEN
       ASSIGN tt-param.arq-destino = "".
    ELSE
    IF tt-param.destino = 2 THEN 
       ASSIGN tt-param.arq-destino = INPUT FRAME f-pg-log c-arquivo-destino.
    ELSE
       ASSIGN tt-param.arq-destino = SESSION:TEMP-DIRECTORY + c-programa-mg97 + ".tmp":U.

    /*:T Coloque aqui a l¢gica de gravaá∆o dos parÉmtros e seleá∆o na temp-table
       tt-param */ 

    ASSIGN tt-param.cod-estabel    = c-cod-estabel
           tt-param.tg-pedido      = INPUT FRAME f-pg-par tg-pedido
           tt-param.tg-item        = INPUT FRAME f-pg-par tg-item
           tt-param.tg-inventario  = INPUT FRAME f-pg-par tg-inventario
           tt-param.tg-cancel-res  = INPUT FRAME f-pg-par tg-cancel-res
           tt-param.tg-reimp-etq   = INPUT FRAME f-pg-par tg-reimp-etq
           tt-param.tg-imp-sit-etq = INPUT FRAME f-pg-par tg-imp-sit
           tt-param.tg-transf-etq  = INPUT FRAME f-pg-par tg-transf
           tt-param.tg-localiz     = INPUT FRAME f-pg-par tg-localiz
           tt-param.tg-conf-localiz = INPUT FRAME f-pg-par tg-conf-localiz
           tt-param.nro-docto      = INPUT FRAME f-pg-par fi-nro-docto
           tt-param.dt-invent      = INPUT FRAME f-pg-par fi-dt-invent
           tt-param.nr-coletor     = INPUT FRAME f-pg-par fi-nr-coletor.

    /* Baixa os dados do Coletor */

    IF rs-tp-import:INPUT-VALUE = 1 THEN DO.
       IF SESSION:SET-WAIT-STATE("general":U) THEN.
    
       OS-DELETE SILENT VALUE(tt-param.arq-entrada).
       IF SEARCH("C:\IMPROTEC\P220\P220.EXE") <> ? THEN DO.
          ASSIGN c-arq-bat = SESSION:TEMP-DIRECTORY + "p220.bat".
          IF c-cod-estabel = '1' THEN DO:
             OUTPUT TO value(c-arq-bat).
                 PUT "c:" SKIP
                     "cd " SESSION:TEMP-DIRECTORY SKIP
                     "C:\IMPROTEC\P220\P220.EXE R 2 4 "
                     tt-param.arq-entrada FORMAT "x(40)" SKIP.
             OUTPUT CLOSE.
          END.
          ELSE DO:
             OUTPUT TO value(c-arq-bat).
                 PUT "c:" SKIP
                     "cd " SESSION:TEMP-DIRECTORY SKIP
                     "C:\IMPROTEC\P220\P220.EXE R 2 1 "
                     tt-param.arq-entrada FORMAT "x(40)" SKIP.
             OUTPUT CLOSE.
          END.

          IF SEARCH(c-arq-bat) <> ? THEN DO.
             OS-COMMAND SILENT VALUE(c-arq-bat).
             PAUSE 5 NO-MESSAGE.
             REPEAT. 
                RUN FindWindowA (0, "IMODEM", OUTPUT i-handle).
                IF i-handle = 0 THEN LEAVE.
             END. 
             /*OS-DELETE SILENT VALUE(c-arq-bat). */
          END.
       END.
    END.
    /*
    ELSE DO.

        IF tg-transf = NO  THEN DO.

                OS-COMMAND SILENT "C:\IMPROTEC\P220\P220.EXE".
    
                PAUSE 5 NO-MESSAGE.
    
                REPEAT. 
    
                    RUN FindWindowA (0, "IMODEM", OUTPUT i-handle).
    
                    IF i-handle = 0 THEN LEAVE.
    
                END. 
        END.
    END.
    */


    IF SESSION:SET-WAIT-STATE("") THEN.
    
    IF SEARCH(tt-param.arq-entrada) = ? THEN DO.
       MESSAGE "Arquivo n∆o foi Baixado do Coletor..." VIEW-AS ALERT-BOX.
       APPLY "MOUSE-SELECT-CLICK":U TO im-pg-par IN FRAME f-import.
       APPLY 'entry' TO c-arquivo-entrada IN FRAME f-pg-par.
       RETURN ERROR.
    END.
    
    {include/i-imexb.i}

    IF SESSION:SET-WAIT-STATE("general":U) THEN.

    {include/i-imrun.i esrp/essp0105rp.p}

    {include/i-imexc.i} 

    IF SESSION:SET-WAIT-STATE("") THEN.
    
    {include/i-imtrm.i tt-param.arq-destino tt-param.destino} 
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-troca-pagina C-Win 
PROCEDURE pi-troca-pagina :
/*:T------------------------------------------------------------------------------
  Purpose: Gerencia a Troca de P†gina (folder)   
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

{include/i-imtrp.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records C-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this w-impor, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed C-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
  
  run pi-trata-state (p-issuer-hdl, p-state).
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

