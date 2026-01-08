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
{include/i-prgvrs.i ESSP0116 2.04.00.000}

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

DEFINE TEMP-TABLE tt-param
       FIELD destino          AS INTEGER 
       FIELD arq-destino      AS CHAR
       FIELD todos            AS INTEGER 
       FIELD usuario          AS CHAR
       FIELD data-exec        AS DATE
       FIELD hora-exec        AS INTEGER
       FIELD tg-fibro         AS LOG
       FIELD dir-fibro        AS CHAR
       FIELD nr-arq-dbf       AS INTEGER
       FIELD arq-entrada      AS CHAR 
       FIELD arq-entrada-f    AS CHAR
       FIELD tg-micronaire    AS LOG
       FIELD finura           AS DEC 
       FIELD maturidade       AS DEC
       FIELD tg-presley       AS LOG
       FIELD resistencia      AS DEC
       FIELD tg-shirley       AS LOG
       FIELD residuos         AS DEC
       FIELD nr-nota-fis      AS INT
       FIELD cod-fornec       AS INT.


/* Transfer Definitions */

def var raw-param        as raw no-undo.

def temp-table tt-raw-digita
   field raw-digita      as raw.
                    
/* Local Variable Definitions ---                                       */

def var l-ok               as logical no-undo.
def var c-arq-digita       as char    no-undo.
def var c-terminal         as char    no-undo.
def var c-arq-term         as char    no-undo.

{include/i-imdef.i}

DEF VAR c-dir-fibro LIKE mp-param.dir-fibro.

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
&Scoped-define List-1 fi-arq-dbf fi-dir-fibro bt-dir-fibro 
&Scoped-define List-2 fi-finura fi-maturidade 
&Scoped-define List-3 fi-resistencia 
&Scoped-define List-4 fi-residuos 
&Scoped-define List-6 fi-nota-fis fi-cod-fornec 

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
     SIZE 8.57 BY .63 NO-UNDO.

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

DEFINE BUTTON bt-arquivo-entrada-f 
     IMAGE-UP FILE "image\im-sea":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE BUTTON bt-dir-fibro 
     IMAGE-UP FILE "image/im-open.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-open.bmp":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE VARIABLE c-arquivo-entrada AS CHARACTER INITIAL "c:~\fibro~\C0000000.DBF" 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 25.14 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE c-arquivo-entrada-f AS CHARACTER INITIAL "c:~\fibro~\F0000000.DBF" 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 25 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi-arq-dbf AS INTEGER FORMAT ">>>>>>>":U INITIAL 0 
     LABEL "Arquivo" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-fornec AS INTEGER FORMAT ">>>>>>>>9" INITIAL 0 
     LABEL "Fornecedor":R12 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dir-fibro AS CHARACTER FORMAT "X(256)":U INITIAL "c:~\fibro" 
     LABEL "Diret¢rio Fibr¢rgrafo" 
     VIEW-AS FILL-IN 
     SIZE 25 BY .88 NO-UNDO.

DEFINE VARIABLE fi-finura AS DECIMAL FORMAT ">>9.99" INITIAL 0 
     LABEL "Finura" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE VARIABLE fi-maturidade AS DECIMAL FORMAT ">>9.99" INITIAL 0 
     LABEL "Maturidade" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nome-emit AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 30.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nota-fis AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "Nota Fiscal" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE fi-residuos AS DECIMAL FORMAT ">>9.99" INITIAL 0 
     LABEL "Res°duos" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE VARIABLE fi-resistencia AS DECIMAL FORMAT ">>9.99" INITIAL 0 
     LABEL "Resistància" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 50 BY 6.5.

DEFINE RECTANGLE RECT-15
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 50 BY 2.75.

DEFINE RECTANGLE RECT-40
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 22.14 BY 3.5.

DEFINE RECTANGLE RECT-41
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 22.14 BY 2.75.

DEFINE RECTANGLE RECT-42
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 22 BY 2.75.

DEFINE VARIABLE tg-fibro AS LOGICAL INITIAL no 
     LABEL " Fibr¢grafo (Comprimento)" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .83 NO-UNDO.

DEFINE VARIABLE tg-micronaire AS LOGICAL INITIAL no 
     LABEL "Micronaire" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .83 NO-UNDO.

DEFINE VARIABLE tg-presley AS LOGICAL INITIAL no 
     LABEL "Presley" 
     VIEW-AS TOGGLE-BOX
     SIZE 8.72 BY .58 NO-UNDO.

DEFINE VARIABLE tg-shirley AS LOGICAL INITIAL no 
     LABEL "Shirley" 
     VIEW-AS TOGGLE-BOX
     SIZE 7.86 BY .83 NO-UNDO.


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
     bt-executar AT ROW 14.29 COL 3 HELP
          "Dispara a execuá∆o do relat¢rio"
     bt-cancelar AT ROW 14.29 COL 14 HELP
          "Cancelar"
     bt-ajuda AT ROW 14.29 COL 70 HELP
          "Ajuda"
     RECT-6 AT ROW 13.75 COL 2.14
     RECT-1 AT ROW 14.04 COL 2
     rt-folder AT ROW 2.5 COL 2
     rt-folder-left AT ROW 2.54 COL 2.14
     rt-folder-top AT ROW 2.54 COL 2.14
     rt-folder-right AT ROW 2.67 COL 80.43
     im-pg-lay AT ROW 1.5 COL 2
     im-pg-log AT ROW 1.5 COL 33.43
     im-pg-par AT ROW 1.5 COL 17.72
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81 BY 15
         DEFAULT-BUTTON bt-executar.

DEFINE FRAME f-pg-log
     rs-todos AT ROW 2.25 COL 3.29 NO-LABEL
     rs-destino AT ROW 4.5 COL 3.29 HELP
          "Destino de Impress∆o do Relat¢rio" NO-LABEL
     bt-config-impr-destino AT ROW 5.71 COL 43.29 HELP
          "Configuraá∆o da impressora"
     bt-arquivo-destino AT ROW 5.71 COL 43.29 HELP
          "Escolha do nome do arquivo"
     c-arquivo-destino AT ROW 5.75 COL 3.29 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL
     rs-execucao AT ROW 7.88 COL 3.14 HELP
          "Modo de Execuá∆o" NO-LABEL
     text-destino-2 AT ROW 1.46 COL 4 NO-LABEL
     text-destino AT ROW 3.75 COL 3.86 NO-LABEL
     text-modo AT ROW 7.13 COL 1.14 COLON-ALIGNED NO-LABEL
     RECT-11 AT ROW 1.75 COL 2
     RECT-7 AT ROW 4.04 COL 2
     RECT-9 AT ROW 7.42 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 76.29 BY 10.46.

DEFINE FRAME f-pg-par
     tg-fibro AT ROW 1.5 COL 4.86
     fi-arq-dbf AT ROW 2.71 COL 19.72 COLON-ALIGNED
     fi-dir-fibro AT ROW 3.75 COL 19.72 COLON-ALIGNED
     bt-dir-fibro AT ROW 3.67 COL 47.57 HELP
          "Localiza Analises do Fibr¢grafo"
     c-arquivo-entrada AT ROW 4.79 COL 21.72 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL
     bt-arquivo-entrada AT ROW 4.75 COL 47.57 HELP
          "Escolha do nome do arquivo"
     c-arquivo-entrada-f AT ROW 5.83 COL 21.72 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL
     bt-arquivo-entrada-f AT ROW 5.83 COL 47.57 HELP
          "Escolha do nome do arquivo"
     tg-micronaire AT ROW 1.5 COL 55.86
     fi-finura AT ROW 2.5 COL 65.86 COLON-ALIGNED
     fi-maturidade AT ROW 3.5 COL 65.86 COLON-ALIGNED
     tg-presley AT ROW 5.38 COL 55.86
     fi-resistencia AT ROW 6.25 COL 65.86 COLON-ALIGNED
     tg-shirley AT ROW 8.25 COL 4.86
     fi-residuos AT ROW 9.5 COL 11.86 COLON-ALIGNED
     fi-nota-fis AT ROW 8.5 COL 33.86 COLON-ALIGNED
     fi-cod-fornec AT ROW 9.5 COL 33.86 COLON-ALIGNED
     fi-nome-emit AT ROW 9.5 COL 41.29 COLON-ALIGNED NO-LABEL
     "Arquivo de Entrada ( C ) :" VIEW-AS TEXT
          SIZE 17 BY .75 AT ROW 4.88 COL 4.43
     "Arquivo de Entrada ( F ) :" VIEW-AS TEXT
          SIZE 17 BY .75 AT ROW 5.88 COL 4.43
     RECT-12 AT ROW 1.25 COL 2.86
     RECT-15 AT ROW 8 COL 25.86
     RECT-40 AT ROW 1.25 COL 53.72
     RECT-41 AT ROW 5 COL 53.72
     RECT-42 AT ROW 8 COL 2.86
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 76.14 BY 10.38
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
         TITLE              = "Importa Resultado de Analises"
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
/* SETTINGS FOR BUTTON bt-arquivo-entrada IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-arquivo-entrada-f IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-dir-fibro IN FRAME f-pg-par
   NO-ENABLE 1                                                          */
/* SETTINGS FOR EDITOR c-arquivo-entrada IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR c-arquivo-entrada-f IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-arq-dbf IN FRAME f-pg-par
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN fi-cod-fornec IN FRAME f-pg-par
   NO-ENABLE 6                                                          */
/* SETTINGS FOR FILL-IN fi-dir-fibro IN FRAME f-pg-par
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN fi-finura IN FRAME f-pg-par
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi-maturidade IN FRAME f-pg-par
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi-nome-emit IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nota-fis IN FRAME f-pg-par
   NO-ENABLE 6                                                          */
/* SETTINGS FOR FILL-IN fi-residuos IN FRAME f-pg-par
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-resistencia IN FRAME f-pg-par
   NO-ENABLE 3                                                          */
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
ON END-ERROR OF C-Win /* Importa Resultado de Analises */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Importa Resultado de Analises */
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


&Scoped-define SELF-NAME bt-arquivo-entrada-f
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-arquivo-entrada-f C-Win
ON CHOOSE OF bt-arquivo-entrada-f IN FRAME f-pg-par
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


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME bt-dir-fibro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-dir-fibro C-Win
ON CHOOSE OF bt-dir-fibro IN FRAME f-pg-par
DO:
   RUN utp/ut-dir.p (INPUT "Escolha a Pasta de Analises do Fibr¢grafo",
                     OUTPUT c-dir-fibro,
                     OUTPUT l-ok).

   IF NOT l-ok THEN DO.
      
      ASSIGN fi-dir-fibro:SCREEN-VALUE = IF SUBSTR(c-dir-fibro,LENGTH(c-dir-fibro)) = "\"
                                          THEN SUBSTR(c-dir-fibro,1,(LENGTH(c-dir-fibro) - 1))
                                          ELSE c-dir-fibro.
      APPLY 'value-changed' TO fi-dir-fibro.
   END.
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
&Scoped-define SELF-NAME fi-arq-dbf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-arq-dbf C-Win
ON LEAVE OF fi-arq-dbf IN FRAME f-pg-par /* Arquivo */
DO:
  IF SELF:SCREEN-VALUE <> '' AND
     (SEARCH(c-arquivo-entrada:SCREEN-VALUE) = ? OR
      SEARCH(c-arquivo-entrada-f:SCREEN-VALUE) = ?) THEN DO.
     MESSAGE 'Arquivo de Analise n∆o Encontrado...'
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     APPLY 'entry' TO SELF.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-arq-dbf C-Win
ON VALUE-CHANGED OF fi-arq-dbf IN FRAME f-pg-par /* Arquivo */
DO:
  ASSIGN c-arquivo-entrada:SCREEN-VALUE = fi-dir-fibro:SCREEN-VALUE + "\C" + 
                                          STRING(SELF:INPUT-VALUE,"9999999") + ".DBF" 
         c-arquivo-entrada-f:SCREEN-VALUE = fi-dir-fibro:SCREEN-VALUE + "\F" + 
                                          STRING(SELF:INPUT-VALUE,"9999999") + ".DBF".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-fornec
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-fornec C-Win
ON LEAVE OF fi-cod-fornec IN FRAME f-pg-par /* Fornecedor */
DO:
  FIND emitente WHERE
       emitente.cod-emite = SELF:INPUT-VALUE NO-LOCK NO-ERROR.
  IF NOT AVAIL emitente THEN DO.
     MESSAGE "Fornecedor n∆o Cadastrado..."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     APPLY 'entry' TO SELF.
     RETURN NO-APPLY.
  END.
  ASSIGN fi-nome-emit:SCREEN-VALUE = emitente.nome-emit.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-dir-fibro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-dir-fibro C-Win
ON / OF fi-dir-fibro IN FRAME f-pg-par /* Diret¢rio Fibr¢rgrafo */
DO:
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-dir-fibro C-Win
ON VALUE-CHANGED OF fi-dir-fibro IN FRAME f-pg-par /* Diret¢rio Fibr¢rgrafo */
DO:
   ASSIGN c-arquivo-entrada:SCREEN-VALUE = SELF:SCREEN-VALUE + "\C" + 
                                           STRING(fi-arq-dbf:INPUT-VALUE,"9999999") + ".DBF" 
          c-arquivo-entrada-f:SCREEN-VALUE = SELF:SCREEN-VALUE + "\F" + 
                                             STRING(fi-arq-dbf:INPUT-VALUE,"9999999") + ".DBF".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-nota-fis
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nota-fis C-Win
ON LEAVE OF fi-nota-fis IN FRAME f-pg-par /* Nota Fiscal */
DO:
  FIND LAST mp-entr-mat WHERE
            mp-entr-mat.nro-docto = SELF:INPUT-VALUE NO-LOCK NO-ERROR.
  IF AVAIL mp-entr-mat THEN
     ASSIGN fi-cod-fornec:SCREEN-VALUE = STRING(mp-entr-mat.cod-emit).

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
&Scoped-define SELF-NAME tg-fibro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-fibro C-Win
ON VALUE-CHANGED OF tg-fibro IN FRAME f-pg-par /*  Fibr¢grafo (Comprimento) */
DO:
  IF SELF:INPUT-VALUE = YES THEN DO.
     ENABLE {&list-1} WITH FRAME {&FRAME-NAME}.
     APPLY 'entry' TO fi-arq-dbf.
     RETURN NO-APPLY.
  END.
  ELSE DO.
     ASSIGN fi-arq-dbf:SCREEN-VALUE = ''.
     APPLY 'value-changed' TO fi-arq-dbf.
     DISABLE {&list-1} WITH FRAME {&FRAME-NAME}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-micronaire
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-micronaire C-Win
ON VALUE-CHANGED OF tg-micronaire IN FRAME f-pg-par /* Micronaire */
DO:
  IF SELF:INPUT-VALUE = YES THEN DO.
     ENABLE {&list-2} WITH FRAME {&FRAME-NAME}.
     ENABLE {&list-6} WITH FRAME {&FRAME-NAME}.
     APPLY 'entry' TO fi-finura.
     RETURN NO-APPLY.
  END.
  ELSE DO.
     ASSIGN fi-finura:SCREEN-VALUE = '0'
            fi-maturidade:SCREEN-VALUE = '0'.
     DISABLE {&list-2} WITH FRAME {&FRAME-NAME}.

     IF NOT tg-presley:INPUT-VALUE AND
        NOT tg-shirley:INPUT-VALUE THEN
        DISABLE {&list-6} WITH FRAME {&FRAME-NAME}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-presley
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-presley C-Win
ON VALUE-CHANGED OF tg-presley IN FRAME f-pg-par /* Presley */
DO:
  IF SELF:INPUT-VALUE = YES THEN DO.
     ENABLE {&list-3} WITH FRAME {&FRAME-NAME}.
     ENABLE {&list-6} WITH FRAME {&FRAME-NAME}.
     APPLY 'entry' TO fi-resistencia.
     RETURN NO-APPLY.
  END.
  ELSE DO.
     ASSIGN fi-resistencia:SCREEN-VALUE = '0'.
     DISABLE {&list-3} WITH FRAME {&FRAME-NAME}.

     IF NOT tg-micronaire:INPUT-VALUE AND
        NOT tg-shirley:INPUT-VALUE THEN
        DISABLE {&list-6} WITH FRAME {&FRAME-NAME}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-shirley
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-shirley C-Win
ON VALUE-CHANGED OF tg-shirley IN FRAME f-pg-par /* Shirley */
DO:
  IF SELF:INPUT-VALUE = YES THEN DO.
     ENABLE {&list-4} WITH FRAME {&FRAME-NAME}.
     ENABLE {&list-6} WITH FRAME {&FRAME-NAME}.
     APPLY 'entry' TO fi-residuos.
     RETURN NO-APPLY.
  END.
  ELSE DO.
     ASSIGN fi-residuos:SCREEN-VALUE = '0'.
     DISABLE {&list-4} WITH FRAME {&FRAME-NAME}.

     IF NOT tg-micronaire:INPUT-VALUE AND
        NOT tg-presley:INPUT-VALUE THEN
        DISABLE {&list-6} WITH FRAME {&FRAME-NAME}.
  END.
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

{utp/ut9000.i "ESSP0116" "2.00.04.000"}

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
  
    {include/i-immbl.i "im-pg-par"}

    {include/i-imvrf.i &programa=XX9999 &versao-layout=001}
  
    FIND FIRST mp-param NO-LOCK NO-ERROR.
    IF AVAIL mp-param THEN DO.
       ASSIGN fi-dir-fibro:SCREEN-VALUE IN FRAME f-pg-par = mp-param.dir-fibro.
       APPLY 'value-changed' TO fi-dir-fibro IN FRAME f-pg-par.
    END.

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
  ENABLE RECT-11 RECT-7 RECT-9 rs-todos rs-destino bt-config-impr-destino 
         bt-arquivo-destino c-arquivo-destino rs-execucao 
      WITH FRAME f-pg-log IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-log}
  DISPLAY tg-fibro fi-arq-dbf fi-dir-fibro c-arquivo-entrada c-arquivo-entrada-f 
          tg-micronaire fi-finura fi-maturidade tg-presley fi-resistencia 
          tg-shirley fi-residuos fi-nota-fis fi-cod-fornec fi-nome-emit 
      WITH FRAME f-pg-par IN WINDOW C-Win.
  ENABLE tg-fibro tg-micronaire tg-presley tg-shirley RECT-12 RECT-15 RECT-40 
         RECT-41 RECT-42 
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

    IF INPUT FRAME f-pg-log rs-destino = 2 AND
       INPUT FRAME f-pg-log rs-execucao = 1 THEN DO:
       RUN utp/ut-vlarq.p (INPUT INPUT FRAME f-pg-log c-arquivo-destino).
       IF RETURN-VALUE = "nok":U THEN DO:
          RUN utp/ut-msgs.p (INPUT "show":U,
                             INPUT 73,
                             INPUT "").
          APPLY 'mouse-select-click':U TO im-pg-log IN FRAME f-import.
          APPLY 'entry':U TO c-arquivo-destino IN FRAME f-pg-log.                   
          RETURN ERROR.
       END.
    END.

    IF INPUT FRAME f-pg-par tg-fibro THEN DO.
       ASSIGN FILE-INFO:FILE-NAME = INPUT FRAME f-pg-par c-arquivo-entrada.
       IF FILE-INFO:PATHNAME = ? AND
          INPUT FRAME f-pg-log rs-execucao = 1 THEN DO:
          RUN utp/ut-msgs.p (INPUT "show":U,
                             INPUT 326,
                             INPUT c-arquivo-entrada).                               
          APPLY 'mouse-select-click':U TO im-pg-par IN FRAME f-import.
          APPLY 'entry':U TO fi-arq-dbf IN FRAME f-pg-par.                
          RETURN ERROR.
       END. 
    END.
            
    /*:T Coloque aqui as validaá‰es das outras p†ginas, lembrando que elas
       devem apresentar uma mensagem de erro cadastrada, posicionar na p†gina 
       com problemas e colocar o focus no campo com problemas             */    

    IF INPUT FRAME f-pg-par tg-micronaire AND
       (INPUT FRAME f-pg-par fi-finura = 0 OR
        INPUT FRAME f-pg-par fi-maturidade = 0) THEN DO.
       MESSAGE "Favor Informar o Percentual de Finura/Maturidade"
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
       APPLY 'mouse-select-click':U TO im-pg-par IN FRAME f-import.
       APPLY 'entry':U to fi-finura IN FRAME f-pg-par.                   
       RETURN ERROR.
    END.

    IF INPUT FRAME f-pg-par tg-presley AND
       INPUT FRAME f-pg-par fi-resistencia = 0 THEN DO.
       MESSAGE "Favor Informar o Percentual de Resistencia"
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
       APPLY 'mouse-select-click':U TO im-pg-par IN FRAME f-import.
       APPLY 'entry':U to fi-resistencia IN FRAME f-pg-par.                   
       RETURN ERROR.
    END.

    IF INPUT FRAME f-pg-par tg-shirley AND
       INPUT FRAME f-pg-par fi-residuos = 0 THEN DO.
       MESSAGE "Favor Informar o Percentual de Res°duos"
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
       APPLY 'mouse-select-click':U TO im-pg-par IN FRAME f-import.
       APPLY 'entry':U to fi-residuos IN FRAME f-pg-par.                   
       RETURN ERROR.
    END.

    IF (INPUT FRAME f-pg-par tg-micronaire OR 
        INPUT FRAME f-pg-par tg-presley OR 
        INPUT FRAME f-pg-par tg-shirley) AND
        (INPUT FRAME f-pg-par fi-nota-fis = 0 OR
        INPUT FRAME f-pg-par fi-cod-fornec = 0) THEN DO.

        MESSAGE "Favor informar Nota Fiscal e Fornecedor "
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY 'mouse-select-click':U TO im-pg-par IN FRAME f-import.
        APPLY 'entry':U to fi-nota-fis IN FRAME f-pg-par.                   
        RETURN ERROR.

        FIND mp-entr-mat WHERE
             mp-entr-mat.cod-emit = INPUT FRAME f-pg-par fi-cod-fornec AND
             mp-entr-mat.nro-docto = INPUT FRAME f-pg-par fi-nota-fis NO-LOCK NO-ERROR.
        IF NOT AVAIL mp-entr-mat THEN DO.
           MESSAGE "Nota Fiscal n∆o Encontrada na Descarga de Fardos..."
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
           APPLY 'mouse-select-click':U TO im-pg-par IN FRAME f-import.
           APPLY 'entry':U to fi-nota-fis IN FRAME f-pg-par.                   
           RETURN ERROR.
        END.
    END.

    IF fi-cod-fornec:SENSITIVE IN FRAME f-pg-par THEN DO.
       FIND emitente WHERE
            emitente.cod-emite = INPUT FRAME f-pg-par fi-cod-fornec NO-LOCK NO-ERROR.
       IF NOT AVAIL emitente THEN DO.
          MESSAGE "Fornecedor n∆o Cadastrado..."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          APPLY 'mouse-select-click':U TO im-pg-par IN FRAME f-import.
          APPLY 'entry':U to fi-cod-fornec IN FRAME f-pg-par.                   
          RETURN ERROR.
       END.
    END.

    CREATE tt-param.
    ASSIGN tt-param.usuario         = c-seg-usuario
           tt-param.destino         = INPUT FRAME f-pg-log rs-destino
           tt-param.todos           = INPUT FRAME f-pg-log rs-todos
           tt-param.data-exec       = TODAY
           tt-param.hora-exec       = TIME
           tt-param.tg-fibro        = INPUT FRAME f-pg-par tg-fibro
           tt-param.dir-fibro       = INPUT FRAME f-pg-par fi-dir-fibro
           tt-param.nr-arq-dbf      = INPUT FRAME f-pg-par fi-arq-dbf
           tt-param.arq-entrada     = INPUT FRAME f-pg-par c-arquivo-entrada
           tt-param.arq-entrada-f   = INPUT FRAME f-pg-par c-arquivo-entrada-f
           tt-param.tg-micronaire   = INPUT FRAME f-pg-par tg-micronaire
           tt-param.finura          = INPUT FRAME f-pg-par fi-finura
           tt-param.maturidade      = INPUT FRAME f-pg-par fi-maturidade
           tt-param.tg-presley      = INPUT FRAME f-pg-par tg-presley
           tt-param.resistencia     = INPUT FRAME f-pg-par fi-resistencia
           tt-param.tg-shirley      = INPUT FRAME f-pg-par tg-shirley
           tt-param.residuos        = INPUT FRAME f-pg-par fi-residuos
           tt-param.nr-nota-fis     = INPUT FRAME f-pg-par fi-nota-fis
           tt-param.cod-fornec      = INPUT FRAME f-pg-par fi-cod-fornec.

    if tt-param.destino = 1 then
       assign tt-param.arq-destino = "".
    else
    if tt-param.destino = 2 then 
       assign tt-param.arq-destino = input frame f-pg-log c-arquivo-destino.
    else
       assign tt-param.arq-destino = session:temp-directory + c-programa-mg97 + ".tmp":U.

    /*:T Coloque aqui a l¢gica de gravaá∆o dos parÉmtros e seleá∆o na temp-table
       tt-param */ 

    {include/i-imexb.i}

    if session:set-wait-state("general":U) then.

    {include/i-imrun.i esrp/essp0116rp.p}

    {include/i-imexc.i}

    if session:set-wait-state("") then.
    
    {include/i-imtrm.i tt-param.arq-destino tt-param.destino}
    
end.
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

