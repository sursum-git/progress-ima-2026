&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wReport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wReport 
/*:T*******************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i RE0520 2.00.00.008 } /*** 010008 ***/

/* Chamada a include do gerenciador de licen»as. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m½dulo>:  Informar qual o m½dulo a qual o programa pertence.                  */

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i RE0520 MRE}
&ENDIF

CREATE WIDGET-POOL.

{cdp/cdcfgmat.i} /* Pre-processadores Materiais */ 
{cdp/cdcfgdis.i}
/* Preprocessors Definitions ---                                      */
&GLOBAL-DEFINE Program        RE0520
&GLOBAL-DEFINE Version        2.00.00.008

&GLOBAL-DEFINE Folder         YES
&GLOBAL-DEFINE InitialPage    1
&GLOBAL-DEFINE FolderLabels   Sele»’o,Par³metros,Impress’o

&GLOBAL-DEFINE PGSEL          YES
&GLOBAL-DEFINE PGPAR          YES
&GLOBAL-DEFINE PGIMP          YES

&GLOBAL-DEFINE RTF            NO

&GLOBAL-DEFINE page0Widgets   btOk ~
                              btCancel ~
                              btHelp2

&GLOBAL-DEFINE page2Widgets   c-cod-estabel c-cod-estabel-fim

&GLOBAL-DEFINE page4Widgets   rs-nfe-sincroniz ~
                              rs-emit-sincroniz ~
                              tg-pesquisar
                              
&GLOBAL-DEFINE page6Widgets   rsDestiny ~
                              btConfigImpr ~
                              btFile ~
                              rsExecution 

/* Parameters Definitions ---                                           */

define temp-table tt-param no-undo
    FIELD cod-estabel      LIKE estabelec.cod-estabel   
    FIELD cod-estabel-fim  LIKE estabelec.cod-estabel   
    FIELD nfe-sincroniz    AS INTEGER                   
    FIELD emit-sincroniz   AS INTEGER                   
    FIELD l-pesquisar      AS LOGICAL
    FIELD acao             AS INTEGER
    field destino          as integer
    field arquivo          as char format "x(35)":U
    field usuario          as char format "x(12)":U
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)":U
    field modelo           AS char format "x(35)":U
    
    /*Alterado 15/02/2005 - tech1007 - Criado campo l½gico para verificar se o RTF foi habilitado*/
    field l-habilitaRtf    as LOG.
    /*Fim alteracao 15/02/2005*/

define temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9":U
    field exemplo          as character format "x(30)":U
    index id ordem.

define buffer b-tt-digita for tt-digita.

/* Transfer Definitions */

def var raw-param        as raw no-undo.

def temp-table tt-raw-digita
   field raw-digita      as raw.

def var l-ok                 as logical no-undo.
def var c-arq-digita         as char    no-undo.
def var c-terminal           as char    no-undo.
def var c-rtf                as char    no-undo.
def var c-arq-layout         as char    no-undo.      
def var c-arq-temp           as char    no-undo.
DEF VAR c-modelo-default     AS CHAR    NO-UNDO.
DEF VAR tp-integ             AS CHAR    NO-UNDO.

DEFINE VARIABLE cTpTrans AS CHARACTER NO-UNDO.

def stream s-imp.

/*15/02/2005 - tech1007 - Variavel definida para tratar se o programa estÿ rodando no WebEnabler*/
DEFINE SHARED VARIABLE hWenController AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fpage0

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rtToolBar btOK btCancel btHelp2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wReport AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btCancel 
     LABEL "&Fechar" 
     SIZE 10 BY 1.

DEFINE BUTTON btHelp2 
     LABEL "&Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON btOK 
     LABEL "&Executar" 
     SIZE 10 BY 1.

DEFINE RECTANGLE rtToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 90 BY 1.42
     BGCOLOR 7 .

DEFINE VARIABLE c-cod-estabel AS CHARACTER FORMAT "X(5)":U 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE c-cod-estabel-fim AS CHARACTER FORMAT "X(5)":U INITIAL "ZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE VARIABLE rs-acao-tc-20 AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Solicitar Sincroniza»’o NF-e Destinada", 1,
"Atualizar Sincroniza»’o NF-e Destinada", 2
     SIZE 42 BY 1.5 NO-UNDO.

DEFINE VARIABLE rs-emit-sincroniz AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Todos os Emitentes", 0,
"N’o considerar Transfer¼ncia entre Estabelecimentos", 1
     SIZE 64 BY 1.54 NO-UNDO.

DEFINE VARIABLE rs-nfe-sincroniz AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Todas as NF-e", 0,
"Somente NF-e sem Manifesta»’o do Destinatÿrio", 1,
"Sem Manifesta»’o do Destinatÿrio ou Ci¼ncia da Opera»’o", 2
     SIZE 67 BY 2.33 NO-UNDO.

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 82 BY 3.21.

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 82 BY 2.75.

DEFINE RECTANGLE RECT-14
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 82 BY 2.25.

DEFINE VARIABLE tg-pesquisar AS LOGICAL INITIAL no 
     LABEL "Somente pesquisar documentos sincronizados" 
     VIEW-AS TOGGLE-BOX
     SIZE 39 BY .83 NO-UNDO.

DEFINE BUTTON btConfigImpr 
     IMAGE-UP FILE "image\im-cfprt":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE BUTTON btFile 
     IMAGE-UP FILE "image\im-sea":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE BUTTON btModelRtf 
     IMAGE-UP FILE "image\im-sea":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE VARIABLE cFile AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 40 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE cModelRTF AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 40 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE text-ModelRtf AS CHARACTER FORMAT "X(256)":U INITIAL "Modelo:" 
      VIEW-AS TEXT 
     SIZE 10 BY .67 NO-UNDO.

DEFINE VARIABLE text-rtf AS CHARACTER FORMAT "X(256)":U INITIAL "Rich Text Format(RTF)" 
      VIEW-AS TEXT 
     SIZE 16 BY .67 NO-UNDO.

DEFINE VARIABLE rsDestiny AS INTEGER INITIAL 3 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Impressora", 1,
"Arquivo", 2,
"Terminal", 3
     SIZE 44 BY 1.08
     FONT 1 NO-UNDO.

DEFINE VARIABLE rsExecution AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "On-Line", 1,
"Batch", 2
     SIZE 27.86 BY .92
     FONT 1 NO-UNDO.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.14 BY 2.92.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.14 BY 1.71.

DEFINE RECTANGLE rect-rtf
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.14 BY 3.21.

DEFINE VARIABLE l-habilitaRtf AS LOGICAL INITIAL no 
     LABEL "RTF" 
     VIEW-AS TOGGLE-BOX
     SIZE 44 BY 1.08
     FONT 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fpage0
     btOK AT ROW 16.75 COL 2
     btCancel AT ROW 16.75 COL 13
     btHelp2 AT ROW 16.75 COL 80
     rtToolBar AT ROW 16.5 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 17
         FONT 1 WIDGET-ID 100.

DEFINE FRAME fPage2
     c-cod-estabel AT ROW 2.04 COL 15.57
     c-cod-estabel-fim AT ROW 2.04 COL 45.29 COLON-ALIGNED NO-LABEL
     IMAGE-1 AT ROW 2.08 COL 36.72
     IMAGE-2 AT ROW 2.08 COL 44.29
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.57 ROW 2.79
         SIZE 84.43 BY 11.21
         FONT 1 WIDGET-ID 100.

DEFINE FRAME fPage6
     rsDestiny AT ROW 2.38 COL 3.14 HELP
          "Destino de Impress’o do Relat½rio" NO-LABEL
     cFile AT ROW 3.63 COL 3.14 HELP
          "Nome do arquivo de destino do relat½rio" NO-LABEL
     btFile AT ROW 3.5 COL 43 HELP
          "Escolha do nome do arquivo"
     btConfigImpr AT ROW 3.5 COL 43 HELP
          "Configura»’o da impressora"
     l-habilitaRtf AT ROW 5.58 COL 3.14
     cModelRTF AT ROW 7.29 COL 3 HELP
          "Nome do arquivo de modelo" NO-LABEL
     btModelRtf AT ROW 7.29 COL 43 HELP
          "Escolha o arquivo de modelo"
     rsExecution AT ROW 9.5 COL 2.86 HELP
          "Modo de Execu»’o" NO-LABEL
     text-rtf AT ROW 5 COL 2 COLON-ALIGNED NO-LABEL
     text-ModelRtf AT ROW 6.54 COL 2 COLON-ALIGNED NO-LABEL
     " Destino" VIEW-AS TEXT
          SIZE 8.14 BY .63 AT ROW 1.63 COL 2.86
     " Execu»’o" VIEW-AS TEXT
          SIZE 10.86 BY .63 AT ROW 8.75 COL 2.86
     rect-rtf AT ROW 5.29 COL 2
     RECT-7 AT ROW 1.92 COL 2.14
     RECT-9 AT ROW 9 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.57 ROW 2.81
         SIZE 84.43 BY 10.15
         FONT 1 WIDGET-ID 100.

DEFINE FRAME fPage4
     tg-pesquisar AT ROW 1.21 COL 4 WIDGET-ID 16
     rs-acao-tc-20 AT ROW 3 COL 4.86 NO-LABEL WIDGET-ID 20
     rs-nfe-sincroniz AT ROW 5.75 COL 5 HELP
          "Informe quais NF-e deverÿ ser sincronizado" NO-LABEL WIDGET-ID 6
     rs-emit-sincroniz AT ROW 9.79 COL 5 HELP
          "Informe os Emitentes a serem considerados p/ sincroniza‡Æo" NO-LABEL WIDGET-ID 10
     " Informe quais NF-e Sincronizar" VIEW-AS TEXT
          SIZE 23 BY .63 AT ROW 5.04 COL 4
     " Informe quais Emitentes buscar as NF-e para sincronizar" VIEW-AS TEXT
          SIZE 41 BY .63 AT ROW 8.96 COL 4
     "Informe a a»’o a ser executada" VIEW-AS TEXT
          SIZE 24 BY .54 AT ROW 2.25 COL 4 WIDGET-ID 24
     RECT-12 AT ROW 5.29 COL 2
     RECT-13 AT ROW 9.21 COL 2 WIDGET-ID 2
     RECT-14 AT ROW 2.5 COL 2 WIDGET-ID 18
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.57 ROW 2.79
         SIZE 84.43 BY 11.21
         FONT 1 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Add Fields to: Neither
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wReport ASSIGN
         HIDDEN             = YES
         TITLE              = "Sincroniza»’o Documentos Manifest Dest"
         HEIGHT             = 17
         WIDTH              = 90
         MAX-HEIGHT         = 22
         MAX-WIDTH          = 114.14
         VIRTUAL-HEIGHT     = 22
         VIRTUAL-WIDTH      = 114.14
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wReport 
/* ************************* Included-Libraries *********************** */

{report/report.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wReport
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* REPARENT FRAME */
ASSIGN FRAME fPage2:FRAME = FRAME fpage0:HANDLE
       FRAME fPage4:FRAME = FRAME fpage0:HANDLE
       FRAME fPage6:FRAME = FRAME fpage0:HANDLE.

/* SETTINGS FOR FRAME fpage0
   NOT-VISIBLE FRAME-NAME                                               */
/* SETTINGS FOR FRAME fPage2
                                                                        */
/* SETTINGS FOR FILL-IN c-cod-estabel IN FRAME fPage2
   ALIGN-L                                                              */
/* SETTINGS FOR FRAME fPage4
                                                                        */
/* SETTINGS FOR RADIO-SET rs-acao-tc-20 IN FRAME fPage4
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-pesquisar IN FRAME fPage4
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME fPage6
   Custom                                                               */
ASSIGN 
       btModelRtf:HIDDEN IN FRAME fPage6           = TRUE.

ASSIGN 
       cModelRTF:HIDDEN IN FRAME fPage6           = TRUE.

ASSIGN 
       text-ModelRtf:HIDDEN IN FRAME fPage6           = TRUE
       text-ModelRtf:PRIVATE-DATA IN FRAME fPage6     = 
                "Modelo:".

ASSIGN 
       text-rtf:HIDDEN IN FRAME fPage6           = TRUE
       text-rtf:PRIVATE-DATA IN FRAME fPage6     = 
                "Rich Text Format(RTF)".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wReport)
THEN wReport:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fpage0
/* Query rebuild information for FRAME fpage0
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME fpage0 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fPage2
/* Query rebuild information for FRAME fPage2
     _Query            is NOT OPENED
*/  /* FRAME fPage2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fPage6
/* Query rebuild information for FRAME fPage6
     _Query            is NOT OPENED
*/  /* FRAME fPage6 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wReport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wReport wReport
ON END-ERROR OF wReport /* Sincroniza»’o Documentos Manifest Dest */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wReport wReport
ON WINDOW-CLOSE OF wReport /* Sincroniza»’o Documentos Manifest Dest */
DO:
  /* This event will close the window and terminate the procedure.  */
  {report/logfin.i}  
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCancel wReport
ON CHOOSE OF btCancel IN FRAME fpage0 /* Fechar */
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage6
&Scoped-define SELF-NAME btConfigImpr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btConfigImpr wReport
ON CHOOSE OF btConfigImpr IN FRAME fPage6
DO:
   {report/rpimp.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btFile
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btFile wReport
ON CHOOSE OF btFile IN FRAME fPage6
DO:
    {report/rparq.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fpage0
&Scoped-define SELF-NAME btHelp2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btHelp2 wReport
ON CHOOSE OF btHelp2 IN FRAME fpage0 /* Ajuda */
DO:
    {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage6
&Scoped-define SELF-NAME btModelRtf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btModelRtf wReport
ON CHOOSE OF btModelRtf IN FRAME fPage6
DO:
    def var cFile as char no-undo.
    def var l-ok  as logical no-undo.

    assign cModelRTF = replace(input frame {&frame-name} cModelRTF, "/", "~\").
    SYSTEM-DIALOG GET-FILE cFile
       FILTERS "*.rtf" "*.rtf",
               "*.*" "*.*"
       DEFAULT-EXTENSION "rtf"
       INITIAL-DIR "modelos" 
       MUST-EXIST
       USE-FILENAME
       UPDATE l-ok.
    if  l-ok = yes then
        assign cModelRTF:screen-value in frame {&frame-name}  = replace(cFile, "~\", "/"). 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fpage0
&Scoped-define SELF-NAME btOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btOK wReport
ON CHOOSE OF btOK IN FRAME fpage0 /* Executar */
DO:
   do  on error undo, return no-apply:
       run piExecute.
   end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage2
&Scoped-define SELF-NAME c-cod-estabel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-cod-estabel wReport
ON LEAVE OF c-cod-estabel IN FRAME fPage2 /* Estabelecimento */
DO:
    /*
    ASSIGN rs-acao-tc-20:SENSITIVE IN FRAME fPage4 = NO
           tg-pesquisar:SENSITIVE IN FRAME fPage4  = NO.

    FOR EACH estabelec NO-LOCK
        WHERE estabelec.cod-estabel >= c-cod-estabel:SCREEN-VALUE IN FRAME fPage2
        AND   estabelec.cod-estabel <= c-cod-estabel-fim:SCREEN-VALUE IN FRAME fPage2:

        RUN cdp/cd0360b.p (INPUT estabelec.cod-estabel,
                           INPUT "MD-e",
                           OUTPUT cTpTrans).

        IF cTpTrans = "TC2":U THEN LEAVE.
    END.

    IF  cTpTrans = "TC2":U THEN DO:
        ASSIGN rs-acao-tc-20:SENSITIVE IN FRAME fPage4 = YES
               tg-pesquisar:SENSITIVE IN FRAME fPage4  = YES.
    END.
    ELSE DO:
        ASSIGN rs-acao-tc-20:SENSITIVE IN FRAME fPage4 = NO
               tg-pesquisar:SENSITIVE IN FRAME fPage4  = YES.
    END.
    */
    ASSIGN rs-acao-tc-20:SENSITIVE IN FRAME fPage4 = YES
           tg-pesquisar:SENSITIVE IN FRAME fPage4  = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define FRAME-NAME fPage2
&Scoped-define SELF-NAME c-cod-estabel-fim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-cod-estabel-fim wReport
ON LEAVE OF c-cod-estabel-fim IN FRAME fPage2 /* Estabelecimento */
DO:
    /*
    ASSIGN rs-acao-tc-20:SENSITIVE IN FRAME fPage4 = NO
           tg-pesquisar:SENSITIVE IN FRAME fPage4  = NO.

    FOR EACH estabelec NO-LOCK
        WHERE estabelec.cod-estabel >= c-cod-estabel:SCREEN-VALUE IN FRAME fPage2
        AND   estabelec.cod-estabel <= c-cod-estabel-fim:SCREEN-VALUE IN FRAME fPage2:

        RUN cdp/cd0360b.p (INPUT estabelec.cod-estabel,
                           INPUT "MD-e",
                           OUTPUT cTpTrans).

        IF cTpTrans = "TC2":U THEN LEAVE.
    END.

    IF  cTpTrans = "TC2":U THEN DO:
        ASSIGN rs-acao-tc-20:SENSITIVE IN FRAME fPage4 = YES
               tg-pesquisar:SENSITIVE IN FRAME fPage4  = YES.
    END.
    ELSE DO:
        ASSIGN rs-acao-tc-20:SENSITIVE IN FRAME fPage4 = NO
               tg-pesquisar:SENSITIVE IN FRAME fPage4  = YES.
    END.
    */
    ASSIGN rs-acao-tc-20:SENSITIVE IN FRAME fPage4 = YES
           tg-pesquisar:SENSITIVE IN FRAME fPage4  = YES.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage6
&Scoped-define SELF-NAME l-habilitaRtf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL l-habilitaRtf wReport
ON VALUE-CHANGED OF l-habilitaRtf IN FRAME fPage6 /* RTF */
DO:
    &IF "{&RTF}":U = "YES":U &THEN
    RUN pi-habilitaRtf.  
    &endif
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rsDestiny
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsDestiny wReport
ON VALUE-CHANGED OF rsDestiny IN FRAME fPage6
DO:
do  with frame fPage6:
    case self:screen-value:
        when "1":U then do:
            assign cFile:sensitive       = no
                   cFile:visible         = yes
                   btFile:visible        = no
                   btConfigImpr:visible  = yes
                   /*Alterado 15/02/2005 - tech1007 - Alterado para suportar adequadamente com a 
                     funcionalidade de RTF*/
                   &IF "{&RTF}":U = "YES":U &THEN
                   l-habilitaRtf:sensitive  = NO
                   l-habilitaRtf:SCREEN-VALUE IN FRAME fPage6 = "No"
                   l-habilitaRtf = NO
                   &endif
                   .
                   /*Fim alteracao 15/02/2005*/
        end.
        when "2":U then do:
            assign cFile:sensitive       = yes
                   cFile:visible         = yes
                   btFile:visible        = yes
                   btConfigImpr:visible  = no
                   &IF "{&RTF}":U = "YES":U &THEN
                   l-habilitaRtf:sensitive  = YES
                   &endif
                   .
        end.
        when "3":U then do:
            assign cFile:visible         = no
                   cFile:sensitive       = no
                   btFile:visible        = no
                   btConfigImpr:visible  = no
                   &IF "{&RTF}":U = "YES":U &THEN
                   l-habilitaRtf:sensitive  = YES
                   &endif
                   .
            /*Alterado 15/02/2005 - tech1007 - Teste para funcionar corretamente no WebEnabler*/
            &IF "{&RTF}":U = "YES":U &THEN
            IF VALID-HANDLE(hWenController) THEN DO:
                ASSIGN l-habilitaRtf:sensitive  = NO
                       l-habilitaRtf:SCREEN-VALUE IN FRAME fPage6 = "No"
                       l-habilitaRtf = NO.
            END.
            &endif
            /*Fim alteracao 15/02/2005*/
        END.
        /*Alterado 15/02/2005 - tech1007 - Condi»’o removida pois RTF n’o ² mais um destino
        when "4":U then do:
            assign cFile:sensitive       = no
                   cFile:visible         = yes
                   btFile:visible        = no
                   btConfigImpr:visible  = yes
                   text-ModelRtf:VISIBLE   = YES
                   rect-rtf:VISIBLE       = YES
                   blModelRtf:VISIBLE       = yes.
        end.
        Fim alteracao 15/02/2005*/
    end case.
end.
&IF "{&RTF}":U = "YES":U &THEN
RUN pi-habilitaRtf.  
&endif
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rsExecution
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsExecution wReport
ON VALUE-CHANGED OF rsExecution IN FRAME fPage6
DO:
   {report/rprse.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage4
&Scoped-define SELF-NAME tg-pesquisar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-pesquisar wReport
ON VALUE-CHANGED OF tg-pesquisar IN FRAME fPage4 /* Somente pesquisar documentos sincronizados */
DO:
  ASSIGN rs-nfe-sincroniz:SENSITIVE IN FRAME fPage4 = NOT(tg-pesquisar:CHECKED IN FRAME fPage4)
         rs-emit-sincroniz:SENSITIVE IN FRAME fPage4 = NOT(tg-pesquisar:CHECKED IN FRAME fPage4).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fpage0
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wReport 


/*:T--- L½gica para inicializa»’o do programam ---*/
{report/mainblock.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE afterInitializeInterface wReport 
PROCEDURE afterInitializeInterface :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*Alterado 17/02/2005 - tech1007 - Foi criado essa procedure para que seja realizado a inicializa»’o
  correta dos componentes do RTF quando executado em ambiente local e no WebEnabler.*/
/*Fim alteracao 17/02/2005*/

RUN cdp/cd0360b.p (INPUT "",
                   INPUT "MD-e",
                   output tp-integ).

IF  LOOKUP("TSS":U, tp-integ) = 0 AND 
    LOOKUP("TC2":U, tp-integ) = 0 AND
    LOOKUP("TC":U, tp-integ)  = 0 THEN DO:
    RUN utp/ut-msgs.p (INPUT "SHOW",
                       INPUT 424,
                       INPUT "").
    RETURN "NOK":u.

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piExecute wReport 
PROCEDURE piExecute :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

define var r-tt-digita as rowid no-undo.

do on error undo, return error on stop  undo, return error:
    {report/rpexa.i}

    /*15/02/2005 - tech1007 - Teste alterado pois RTF n’o ² mais op»’o de Destino*/
    if input frame fPage6 rsDestiny = 2 and
       input frame fPage6 rsExecution = 1 then do:
        run utp/ut-vlarq.p (input input frame fPage6 cFile).
        
        if return-value = "NOK":U then do:
            run utp/ut-msgs.p (input "show":U, input 73, input "":U).
            apply "ENTRY":U to cFile in frame fPage6.
            return error.
        end.
    end.
    
    /*:T Aqui s’o gravados os campos da temp-table que serÿ passada como par³metro
       para o programa RP.P */
    
    create tt-param.
    assign tt-param.usuario         = c-seg-usuario
           tt-param.destino         = input frame fPage6 rsDestiny
           tt-param.data-exec       = today
           tt-param.hora-exec       = time
           tt-param.cod-estabel     = c-cod-estabel:SCREEN-VALUE IN FRAME fPage2
           tt-param.cod-estabel-fim = c-cod-estabel-fim:SCREEN-VALUE IN FRAME fPage2
           tt-param.nfe-sincroniz   = INPUT FRAME fPage4 rs-nfe-sincroniz
           tt-param.emit-sincroniz  = INPUT FRAME fPage4 rs-emit-sincroniz
           tt-param.l-pesquisar     = tg-pesquisar:CHECKED IN FRAME fPage4
           tt-param.acao            = IF NOT cTpTrans = "TC2":U THEN 0 ELSE INPUT FRAME fPage4 rs-acao-tc-20.
    
    if tt-param.destino = 1 then 
        assign tt-param.arquivo = "":U.
    else if  tt-param.destino = 2 
        then assign tt-param.arquivo = input frame fPage6 cFile.
         else assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp":U.
    
    /*:T Coloque aqui a l½gica de grava»’o dos demais campos que devem ser passados
       como par³metros para o programa RP.P, atrav²s da temp-table tt-param */
    
    
    
    /*:T Executar do programa RP.P que irÿ criar o relat½rio */
    {report/rpexb.i}
    
    SESSION:SET-WAIT-STATE("GENERAL":U).
    
    {report/rprun.i rep/re0520rp.p}
    
    {report/rpexc.i}
    
    SESSION:SET-WAIT-STATE("":U).
    
    {report/rptrm.i}
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

