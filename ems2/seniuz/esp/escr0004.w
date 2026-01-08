&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          mgcad            PROGRESS
*/
&Scoped-define WINDOW-NAME w-relat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-relat 
/*:T*******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESCR0004 2.04.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/*:T Preprocessadores do Template de Relat¢rio                            */
/*:T Obs: Retirar o valor do preprocessador para as p†ginas que n∆o existirem  */

&GLOBAL-DEFINE PGSEL f-pg-sel
&GLOBAL-DEFINE PGCLA 
&GLOBAL-DEFINE PGPAR f-pg-par
&GLOBAL-DEFINE PGDIG f-pg-dig
&GLOBAL-DEFINE PGIMP f-pg-imp
  
/* Parameters Definitions ---                                           */

/* Temporary Table Definitions ---                                      */

define temp-table tt-param no-undo
       field destino          as integer
       field arquivo          as char format "x(35)"
       field usuario          as char format "x(12)"
       field data-exec        as date
       field hora-exec        as integer
       FIELD ep-codigo        LIKE titulo.ep-codigo
       FIELD cod-estabel      LIKE titulo.cod-estabel
       FIELD cod-esp          LIKE titulo.cod-esp
       FIELD serie            LIKE titulo.serie 
       FIELD nr-docto-ini     LIKE titulo.nr-docto
       FIELD nr-docto-fin     LIKE titulo.nr-docto
       FIELD parcela-ini      LIKE titulo.parcela 
       FIELD parcela-fin      LIKE titulo.parcela
       FIELD e-mail           AS LOG
       FIELD subject          AS CHAR FORMAT "x(40)"
       FIELD texto            AS CHAR FORMAT "x(2000)".

define temp-table tt-digita no-undo
       field ep-codigo        LIKE titulo.ep-codigo
       field cod-estabel      LIKE titulo.cod-estabel
       FIELD cod-esp          LIKE titulo.cod-esp
       FIELD serie            LIKE titulo.serie
       FIELD nr-docto         LIKE titulo.nr-docto
       FIELD parcela          LIKE titulo.parcela
       index id ep-codigo
                cod-estabel
                cod-esp
                serie
                nr-docto
                parcela.

define buffer b-tt-digita for tt-digita.

/* Transfer Definitions */

def var raw-param        as raw no-undo.

def temp-table tt-raw-digita
   field raw-digita      as raw.
                    
/* Local Variable Definitions ---                                       */

def var l-ok               as logical no-undo.
def var c-arq-digita       as char    no-undo.
def var c-terminal         as char    no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-relat
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME f-pg-dig
&Scoped-define BROWSE-NAME br-digita

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-digita

/* Definitions for BROWSE br-digita                                     */
&Scoped-define FIELDS-IN-QUERY-br-digita tt-digita.ep-codigo tt-digita.cod-estabel tt-digita.cod-esp tt-digita.serie tt-digita.nr-docto tt-digita.parcela   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-digita tt-digita.ep-codigo  tt-digita.cod-estabel tt-digita.cod-esp   tt-digita.serie ~
tt-digita.nr-docto  tt-digita.parcela   
&Scoped-define ENABLED-TABLES-IN-QUERY-br-digita tt-digita
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br-digita tt-digita
&Scoped-define SELF-NAME br-digita
&Scoped-define QUERY-STRING-br-digita FOR EACH tt-digita
&Scoped-define OPEN-QUERY-br-digita OPEN QUERY br-digita FOR EACH tt-digita.
&Scoped-define TABLES-IN-QUERY-br-digita tt-digita
&Scoped-define FIRST-TABLE-IN-QUERY-br-digita tt-digita


/* Definitions for FRAME f-pg-dig                                       */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-pg-dig ~
    ~{&OPEN-QUERY-br-digita}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-digita bt-inserir bt-recuperar 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-relat AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-alterar 
     LABEL "Alterar" 
     SIZE 15 BY 1
     FONT 1.

DEFINE BUTTON bt-inserir 
     LABEL "Inserir" 
     SIZE 15 BY 1
     FONT 1.

DEFINE BUTTON bt-recuperar 
     LABEL "Recuperar" 
     SIZE 15 BY 1
     FONT 1.

DEFINE BUTTON bt-retirar 
     LABEL "Retirar" 
     SIZE 15 BY 1
     FONT 1.

DEFINE BUTTON bt-salvar 
     LABEL "Salvar" 
     SIZE 15 BY 1
     FONT 1.

DEFINE BUTTON bt-arquivo 
     IMAGE-UP FILE "image\im-sea":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE BUTTON bt-config-impr 
     IMAGE-UP FILE "image\im-cfprt":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE VARIABLE c-arquivo AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 40 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE text-destino AS CHARACTER FORMAT "X(256)":U INITIAL " Destino" 
      VIEW-AS TEXT 
     SIZE 8.57 BY .63 NO-UNDO.

DEFINE VARIABLE text-modo AS CHARACTER FORMAT "X(256)":U INITIAL "Execuá∆o" 
      VIEW-AS TEXT 
     SIZE 10.86 BY .63 NO-UNDO.

DEFINE VARIABLE rs-destino AS INTEGER INITIAL 2 
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

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 46.29 BY 2.92.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 46.29 BY 1.71.

DEFINE BUTTON btn-anterior 
     IMAGE-UP FILE "image/im-pre.bmp":U
     LABEL "Button 2" 
     SIZE 4 BY 1 TOOLTIP "Mensagem Anterior".

DEFINE BUTTON btn-proximo 
     IMAGE-UP FILE "image/im-nex.bmp":U
     LABEL "Button 1" 
     SIZE 4 BY 1 TOOLTIP "Pr¢xima Mensagem".

DEFINE VARIABLE ed-texto AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 70 BY 7.75 NO-UNDO.

DEFINE VARIABLE fi-assunto AS CHARACTER FORMAT "X(256)":U 
     LABEL "Assunto" 
     VIEW-AS FILL-IN 
     SIZE 40 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-mensagem AS INTEGER FORMAT ">>9":U INITIAL 26 
     LABEL "Mensagem" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE tg-email AS LOGICAL INITIAL no 
     LABEL "Envia e-mail" 
     VIEW-AS TOGGLE-BOX
     SIZE 12.72 BY .83 NO-UNDO.

DEFINE VARIABLE fi-cod-esp AS CHARACTER FORMAT "!!" INITIAL "DP" 
     LABEL "EspÇcie":R9 
     VIEW-AS FILL-IN 
     SIZE 3.57 BY .88 TOOLTIP "C¢digo da espÇcie do documento" NO-UNDO.

DEFINE VARIABLE fi-cod-estabel AS CHARACTER FORMAT "x(3)" INITIAL "2" 
     LABEL "Estabel":R7 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 TOOLTIP "C¢digo do estabelecimento" NO-UNDO.

DEFINE VARIABLE fi-ep-codigo AS INTEGER FORMAT ">>9" INITIAL 1 
     LABEL "Empresa":R9 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 TOOLTIP "C¢digo da empresa"
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-fin-nr-docto AS CHARACTER FORMAT "x(16)" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 TOOLTIP "N£mero do documento final" NO-UNDO.

DEFINE VARIABLE fi-fin-parcela AS CHARACTER FORMAT "x(2)" INITIAL "99" 
     VIEW-AS FILL-IN 
     SIZE 3.57 BY .88 TOOLTIP "N£mero da parcela final" NO-UNDO.

DEFINE VARIABLE fi-ini-nr-docto AS CHARACTER FORMAT "x(16)" 
     LABEL "Documento":R11 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 TOOLTIP "N£mero do documento" NO-UNDO.

DEFINE VARIABLE fi-ini-parcela AS CHARACTER FORMAT "x(2)" INITIAL "01" 
     LABEL "Parcela":R9 
     VIEW-AS FILL-IN 
     SIZE 3.57 BY .88 TOOLTIP "N£mero da parcela do documento" NO-UNDO.

DEFINE VARIABLE fi-nome-estab AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 47.14 BY .88
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE fi-razao-social AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 47.14 BY .88
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE fi-serie AS CHARACTER FORMAT "x(5)" 
     LABEL "SÇrie":R7 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 TOOLTIP "SÇrie do documento" NO-UNDO.

DEFINE IMAGE IMAGE-3
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-4
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-5
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-6
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE BUTTON bt-ajuda 
     LABEL "Ajuda" 
     SIZE 10 BY 1
     FONT 1.

DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "Fechar" 
     SIZE 10 BY 1
     FONT 1.

DEFINE BUTTON bt-executar 
     LABEL "Executar" 
     SIZE 10 BY 1
     FONT 1.

DEFINE IMAGE im-pg-dig
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.21.

DEFINE IMAGE im-pg-imp
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.21.

DEFINE IMAGE im-pg-par
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.21.

DEFINE IMAGE im-pg-sel
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

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-digita FOR 
      tt-digita SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-digita w-relat _FREEFORM
  QUERY br-digita DISPLAY
      tt-digita.ep-codigo
tt-digita.cod-estabel
tt-digita.cod-esp
tt-digita.serie
tt-digita.nr-docto
tt-digita.parcela
ENABLE
tt-digita.ep-codigo  
tt-digita.cod-estabel
tt-digita.cod-esp    
tt-digita.serie      
tt-digita.nr-docto   
tt-digita.parcela
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 76.57 BY 9
         BGCOLOR 15 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-relat
     bt-executar AT ROW 14.54 COL 3 HELP
          "Dispara a execuá∆o do relat¢rio"
     bt-cancelar AT ROW 14.54 COL 14 HELP
          "Fechar"
     bt-ajuda AT ROW 14.54 COL 70 HELP
          "Ajuda"
     rt-folder-right AT ROW 2.67 COL 80.43
     RECT-6 AT ROW 13.75 COL 2.14
     RECT-1 AT ROW 14.29 COL 2
     rt-folder-top AT ROW 2.54 COL 2.14
     rt-folder AT ROW 2.5 COL 2
     rt-folder-left AT ROW 2.54 COL 2.14
     im-pg-dig AT ROW 1.5 COL 33.43
     im-pg-imp AT ROW 1.5 COL 49.14
     im-pg-par AT ROW 1.5 COL 17.72
     im-pg-sel AT ROW 1.5 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81 BY 15
         DEFAULT-BUTTON bt-executar.

DEFINE FRAME f-pg-imp
     rs-destino AT ROW 2.38 COL 3.29 HELP
          "Destino de Impress∆o do Relat¢rio" NO-LABEL
     bt-arquivo AT ROW 3.58 COL 43.29 HELP
          "Escolha do nome do arquivo"
     bt-config-impr AT ROW 3.58 COL 43.29 HELP
          "Configuraá∆o da impressora"
     c-arquivo AT ROW 3.63 COL 3.29 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL
     rs-execucao AT ROW 5.75 COL 3 HELP
          "Modo de Execuá∆o" NO-LABEL
     text-destino AT ROW 1.63 COL 3.86 NO-LABEL
     text-modo AT ROW 5 COL 1.29 COLON-ALIGNED NO-LABEL
     RECT-7 AT ROW 1.92 COL 2.14
     RECT-9 AT ROW 5.29 COL 2.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 73.72 BY 10.

DEFINE FRAME f-pg-par
     fi-cod-mensagem AT ROW 1.25 COL 11.43 COLON-ALIGNED
     fi-assunto AT ROW 1.25 COL 30.29 COLON-ALIGNED
     btn-anterior AT ROW 2.25 COL 13.29
     btn-proximo AT ROW 2.25 COL 17.29
     tg-email AT ROW 2.25 COL 32.29
     ed-texto AT ROW 3.25 COL 4.57 NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 75 BY 10.42
         FONT 1.

DEFINE FRAME f-pg-dig
     br-digita AT ROW 1 COL 1
     bt-inserir AT ROW 10 COL 1
     bt-alterar AT ROW 10 COL 16
     bt-retirar AT ROW 10 COL 31
     bt-salvar AT ROW 10 COL 46
     bt-recuperar AT ROW 10 COL 61
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3.31
         SIZE 76.86 BY 10.15.

DEFINE FRAME f-pg-sel
     fi-ep-codigo AT ROW 1.79 COL 18.43 COLON-ALIGNED HELP
          "C¢digo da empresa"
     fi-razao-social AT ROW 1.79 COL 23.57 COLON-ALIGNED NO-LABEL
     fi-cod-estabel AT ROW 2.79 COL 18.43 COLON-ALIGNED
     fi-nome-estab AT ROW 2.79 COL 23.57 COLON-ALIGNED NO-LABEL
     fi-cod-esp AT ROW 3.79 COL 18.43 COLON-ALIGNED HELP
          "C¢digo da espÇcie do documento"
     fi-serie AT ROW 4.79 COL 18.43 COLON-ALIGNED HELP
          "SÇrie da nota fiscal"
     fi-ini-nr-docto AT ROW 5.79 COL 18.43 COLON-ALIGNED HELP
          "N£mero do documento"
     fi-fin-nr-docto AT ROW 5.79 COL 37 COLON-ALIGNED HELP
          "N£mero do documento" NO-LABEL
     fi-ini-parcela AT ROW 6.79 COL 18.43 COLON-ALIGNED
     fi-fin-parcela AT ROW 6.75 COL 37 COLON-ALIGNED NO-LABEL
     IMAGE-3 AT ROW 5.79 COL 29.72
     IMAGE-4 AT ROW 5.79 COL 35.72
     IMAGE-5 AT ROW 6.79 COL 29.72
     IMAGE-6 AT ROW 6.79 COL 35.72
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.85
         SIZE 76.86 BY 10.62
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: w-relat
   Allow: Basic,Browse,DB-Fields,Window,Query
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-relat ASSIGN
         HIDDEN             = YES
         TITLE              = "Carta de Declaraá∆o de Quitaá∆o de T°tulos"
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-relat 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-relat.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-relat
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-pg-dig
                                                                        */
/* BROWSE-TAB br-digita 1 f-pg-dig */
/* SETTINGS FOR BUTTON bt-alterar IN FRAME f-pg-dig
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-retirar IN FRAME f-pg-dig
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-salvar IN FRAME f-pg-dig
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME f-pg-imp
                                                                        */
/* SETTINGS FOR FILL-IN text-destino IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       text-destino:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Destino".

/* SETTINGS FOR FILL-IN text-modo IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       text-modo:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Execuá∆o".

/* SETTINGS FOR FRAME f-pg-par
                                                                        */
/* SETTINGS FOR FILL-IN fi-assunto IN FRAME f-pg-par
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FRAME f-pg-sel
   Custom                                                               */
/* SETTINGS FOR FILL-IN fi-nome-estab IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-razao-social IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME f-relat
                                                                        */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-6 IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder-left IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder-right IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder-top IN FRAME f-relat
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-relat)
THEN w-relat:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-digita
/* Query rebuild information for BROWSE br-digita
     _START_FREEFORM
OPEN QUERY br-digita FOR EACH tt-digita.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-digita */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-pg-imp
/* Query rebuild information for FRAME f-pg-imp
     _Query            is NOT OPENED
*/  /* FRAME f-pg-imp */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-pg-sel
/* Query rebuild information for FRAME f-pg-sel
     _Query            is NOT OPENED
*/  /* FRAME f-pg-sel */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-relat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON END-ERROR OF w-relat /* Carta de Declaraá∆o de Quitaá∆o de T°tulos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON WINDOW-CLOSE OF w-relat /* Carta de Declaraá∆o de Quitaá∆o de T°tulos */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-digita
&Scoped-define SELF-NAME br-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON DEL OF br-digita IN FRAME f-pg-dig
DO:
   apply 'choose':U to bt-retirar in frame f-pg-dig.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON END-ERROR OF br-digita IN FRAME f-pg-dig
ANYWHERE 
DO:
    if  br-digita:new-row in frame f-pg-dig then do:
        if  avail tt-digita then
            delete tt-digita.
        if  br-digita:delete-current-row() in frame f-pg-dig then. 
    end.                                                               
    else do:
        get current br-digita.
        display tt-digita.ep-codigo
                tt-digita.cod-estabel
                tt-digita.cod-esp
                tt-digita.serie
                tt-digita.nr-docto
                tt-digita.parcela
                with browse br-digita. 
    end.
    return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON ENTER OF br-digita IN FRAME f-pg-dig
ANYWHERE
DO:
  apply 'tab':U to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON INS OF br-digita IN FRAME f-pg-dig
DO:
   apply 'choose':U to bt-inserir in frame f-pg-dig.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON OFF-END OF br-digita IN FRAME f-pg-dig
DO:
   apply 'entry':U to bt-inserir in frame f-pg-dig.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON OFF-HOME OF br-digita IN FRAME f-pg-dig
DO:
  apply 'entry':U to bt-recuperar in frame f-pg-dig.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON ROW-ENTRY OF br-digita IN FRAME f-pg-dig
DO:
   /*:T trigger para inicializar campos da temp table de digitaá∆o */
   if  br-digita:new-row in frame f-pg-dig then do:
       assign tt-digita.ep-codigo:SCREEN-VALUE IN BROWSE br-digita   = "1"
              tt-digita.cod-estabel:screen-value in browse br-digita = "2"
              tt-digita.cod-esp:SCREEN-VALUE IN BROWSE br-digita     = "DP"
              tt-digita.serie:SCREEN-VALUE IN BROWSE br-digita       = "1".
   end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON ROW-LEAVE OF br-digita IN FRAME f-pg-dig
DO:
    /*:T ê aqui que a gravaá∆o da linha da temp-table Ç efetivada.
       PorÇm as validaá‰es dos registros devem ser feitas na procedure pi-executar,
       no local indicado pelo coment†rio */
    
    if br-digita:NEW-ROW in frame f-pg-dig then 
    do transaction on error undo, return no-apply:
        create tt-digita.
        assign input browse br-digita tt-digita.ep-codigo
               input browse br-digita tt-digita.cod-estabel
               INPUT BROWSE br-digita tt-digita.cod-esp
               INPUT BROWSE br-digita tt-digita.serie
               INPUT BROWSE br-digita tt-digita.nr-docto
               INPUT BROWSE br-digita tt-digita.parcela.
    
        br-digita:CREATE-RESULT-LIST-ENTRY() in frame f-pg-dig.
    end.
    else do transaction on error undo, return no-apply:
        assign input browse br-digita tt-digita.ep-codigo
               input browse br-digita tt-digita.cod-estabel
               INPUT BROWSE br-digita tt-digita.cod-esp 
               INPUT BROWSE br-digita tt-digita.serie   
               INPUT BROWSE br-digita tt-digita.nr-docto
               INPUT BROWSE br-digita tt-digita.parcela.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda w-relat
ON CHOOSE OF bt-ajuda IN FRAME f-relat /* Ajuda */
DO:
   {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-dig
&Scoped-define SELF-NAME bt-alterar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-alterar w-relat
ON CHOOSE OF bt-alterar IN FRAME f-pg-dig /* Alterar */
DO:
   apply 'entry':U to tt-digita.ep-codigo in browse br-digita. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-imp
&Scoped-define SELF-NAME bt-arquivo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-arquivo w-relat
ON CHOOSE OF bt-arquivo IN FRAME f-pg-imp
DO:
    {include/i-rparq.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME bt-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancelar w-relat
ON CHOOSE OF bt-cancelar IN FRAME f-relat /* Fechar */
DO:
   apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-imp
&Scoped-define SELF-NAME bt-config-impr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-config-impr w-relat
ON CHOOSE OF bt-config-impr IN FRAME f-pg-imp
DO:
   {include/i-rpimp.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME bt-executar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-executar w-relat
ON CHOOSE OF bt-executar IN FRAME f-relat /* Executar */
DO:
   do  on error undo, return no-apply:
       run pi-executar.
   end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-dig
&Scoped-define SELF-NAME bt-inserir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-inserir w-relat
ON CHOOSE OF bt-inserir IN FRAME f-pg-dig /* Inserir */
DO:
    assign bt-alterar:SENSITIVE in frame f-pg-dig = yes
           bt-retirar:SENSITIVE in frame f-pg-dig = yes
           bt-salvar:SENSITIVE in frame f-pg-dig  = yes.
    
    if num-results("br-digita":U) > 0 then
        br-digita:INSERT-ROW("after":U) in frame f-pg-dig.
    else do transaction:
        create tt-digita.
        
        assign tt-digita.ep-codigo   = 1
               tt-digita.cod-estabel = "2"
               tt-digita.cod-esp     = "DP"
               tt-digita.serie       = "1".

        open query br-digita for each tt-digita.
        
        apply "entry":U to tt-digita.ep-codigo in browse br-digita. 
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-recuperar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-recuperar w-relat
ON CHOOSE OF bt-recuperar IN FRAME f-pg-dig /* Recuperar */
DO:
    {include/i-rprcd.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-retirar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-retirar w-relat
ON CHOOSE OF bt-retirar IN FRAME f-pg-dig /* Retirar */
DO:
    if  br-digita:num-selected-rows > 0 then do on error undo, return no-apply:
        get current br-digita.
        delete tt-digita.
        if  br-digita:delete-current-row() in frame f-pg-dig then.
    end.
    
    if num-results("br-digita":U) = 0 then
        assign bt-alterar:SENSITIVE in frame f-pg-dig = no
               bt-retirar:SENSITIVE in frame f-pg-dig = no
               bt-salvar:SENSITIVE in frame f-pg-dig  = no.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-salvar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-salvar w-relat
ON CHOOSE OF bt-salvar IN FRAME f-pg-dig /* Salvar */
DO:
   {include/i-rpsvd.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME btn-anterior
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-anterior w-relat
ON CHOOSE OF btn-anterior IN FRAME f-pg-par /* Button 2 */
DO:
  FIND PREV mensagem NO-LOCK NO-ERROR.
  IF AVAIL mensagem THEN
     ASSIGN fi-cod-mensagem:SCREEN-VALUE IN FRAME f-pg-par = STRING(mensagem.cod-mensag)
            fi-assunto:SCREEN-VALUE IN FRAME f-pg-par = mensagem.descricao
            ed-texto:SCREEN-VALUE IN FRAME f-pg-par = mensagem.texto-mensag + CHR(13) + CHR(13) +
                                                      "Atenciosamente," + CHR(13) + CHR(13) + CHR(13) +
                                                      empresa.razao-social  + CHR(13) +
                                                      "Departamento Financeiro/Cobranca".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-proximo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-proximo w-relat
ON CHOOSE OF btn-proximo IN FRAME f-pg-par /* Button 1 */
DO:
  FIND NEXT mensagem NO-LOCK NO-ERROR.
  IF AVAIL mensagem THEN
     ASSIGN fi-cod-mensagem:SCREEN-VALUE IN FRAME f-pg-par = STRING(mensagem.cod-mensag)
            fi-assunto:SCREEN-VALUE IN FRAME f-pg-par = mensagem.descricao
            ed-texto:SCREEN-VALUE IN FRAME f-pg-par = mensagem.texto-mensag + CHR(13) + CHR(13) +
                                                      "Atenciosamente," + CHR(13) + CHR(13) + CHR(13) +
                                                      empresa.razao-social  + CHR(13) +
                                                      "Departamento Financeiro/Cobranca".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-sel
&Scoped-define SELF-NAME fi-cod-estabel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-estabel w-relat
ON ENTRY OF fi-cod-estabel IN FRAME f-pg-sel /* Estabel */
DO:
  FIND estabelec WHERE
       estabelec.cod-estabel = INPUT FRAME {&FRAME-NAME} fi-cod-estabel NO-LOCK NO-ERROR.

  IF NOT AVAIL estabelec THEN DO.
     MESSAGE "Estabeleciemnto n∆o Cadastrado." VIEW-AS ALERT-BOX.
     RETURN NO-APPLY.
  END.
  ASSIGN fi-nome-estab:SCREEN-VALUE IN FRAME {&FRAME-NAME} = estabelec.nome.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-estabel w-relat
ON LEAVE OF fi-cod-estabel IN FRAME f-pg-sel /* Estabel */
DO:
  FIND estabelec WHERE
       estabelec.cod-estabel = INPUT FRAME {&FRAME-NAME} fi-cod-estabel NO-LOCK NO-ERROR.

  IF NOT AVAIL estabelec THEN DO.
     MESSAGE "Estabeleciemnto n∆o Cadastrado." VIEW-AS ALERT-BOX.
     RETURN NO-APPLY.
  END.
  ASSIGN fi-nome-estab:SCREEN-VALUE IN FRAME {&FRAME-NAME} = estabelec.nome.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-estabel w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-cod-estabel IN FRAME f-pg-sel /* Estabel */
DO:
  {include/zoomvar.i &prog-zoom = adzoom/z01ad107.w
                     &campo     = fi-cod-estabel
                     &campozoom = cod-estabel
                     &FRAME     = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME fi-cod-mensagem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-mensagem w-relat
ON ENTRY OF fi-cod-mensagem IN FRAME f-pg-par /* Mensagem */
DO:
  FIND mensagem WHERE
       mensagem.cod-mensagem = INPUT FRAME {&FRAME-NAME} fi-cod-mensagem NO-LOCK NO-ERROR.

  ASSIGN fi-cod-mensagem:SCREEN-VALUE IN FRAME f-pg-par = STRING(mensagem.cod-mensag)
         fi-assunto:SCREEN-VALUE IN FRAME f-pg-par = mensagem.descricao
         ed-texto:SCREEN-VALUE IN FRAME f-pg-par = mensagem.texto-mensag + CHR(13) + CHR(13) +
                                                   "Atenciosamente," + CHR(13) + CHR(13) + CHR(13) +
                                                   empresa.razao-social  + CHR(13) +
                                                   "Departamento Financeiro/Cobranca".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-mensagem w-relat
ON GO OF fi-cod-mensagem IN FRAME f-pg-par /* Mensagem */
DO:
  FIND mensagem WHERE
       mensagem.cod-mensagem = INPUT FRAME {&FRAME-NAME} fi-cod-mensagem NO-LOCK NO-ERROR.

  ASSIGN fi-cod-mensagem:SCREEN-VALUE IN FRAME f-pg-par = STRING(mensagem.cod-mensag)
         fi-assunto:SCREEN-VALUE IN FRAME f-pg-par = mensagem.descricao
         ed-texto:SCREEN-VALUE IN FRAME f-pg-par = mensagem.texto-mensag + CHR(13) + CHR(13) +
                                                   "Atenciosamente," + CHR(13) + CHR(13) + CHR(13) +
                                                   empresa.razao-social  + CHR(13) +
                                                   "Departamento Financeiro/Cobranca".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-mensagem w-relat
ON LEAVE OF fi-cod-mensagem IN FRAME f-pg-par /* Mensagem */
DO:
  FIND mensagem WHERE
       mensagem.cod-mensagem = INPUT FRAME {&FRAME-NAME} fi-cod-mensagem NO-LOCK NO-ERROR.

  IF NOT AVAIL mensagem THEN DO:
     MESSAGE "Mensagem n∆o Cadastrada." VIEW-AS ALERT-BOX.
     APPLY 'entry' TO SELF.
     RETURN NO-APPLY.
  END.

  ASSIGN fi-cod-mensagem:SCREEN-VALUE IN FRAME f-pg-par = STRING(mensagem.cod-mensag)
         fi-assunto:SCREEN-VALUE IN FRAME f-pg-par = mensagem.descricao
         ed-texto:SCREEN-VALUE IN FRAME f-pg-par = mensagem.texto-mensag + CHR(13) + CHR(13) +
                                                   "Atenciosamente," + CHR(13) + CHR(13) + CHR(13) +
                                                   empresa.razao-social  + CHR(13) +
                                                   "Departamento Financeiro/Cobranca".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-mensagem w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-cod-mensagem IN FRAME f-pg-par /* Mensagem */
DO:
  {include/zoomvar.i &prog-zoom = adzoom/z01ad176.w
                     &campo     = fi-cod-mensagem
                     &campozoom = cod-mensagem
                     &FRAME     = f-pg-par}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-sel
&Scoped-define SELF-NAME fi-ep-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ep-codigo w-relat
ON ENTRY OF fi-ep-codigo IN FRAME f-pg-sel /* Empresa */
DO:
  IF SELF:SCREEN-VALUE <> "" THEN DO.
     FIND empresa WHERE
          empresa.ep-codigo = INPUT FRAME {&FRAME-NAME} fi-ep-codigo
          NO-LOCK NO-ERROR.
     IF AVAIL empresa THEN
        ASSIGN fi-razao-social:SCREEN-VALUE IN FRAME {&FRAME-NAME} = empresa.razao-social.
  END.
  ELSE ASSIGN fi-razao-social:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ep-codigo w-relat
ON LEAVE OF fi-ep-codigo IN FRAME f-pg-sel /* Empresa */
DO:
  FIND empresa WHERE
       empresa.ep-codigo = INPUT FRAME {&FRAME-NAME} fi-ep-codigo NO-LOCK NO-ERROR.

  IF NOT AVAIL empresa THEN DO.
     MESSAGE "Empresa n∆o Cadastrada." VIEW-AS ALERT-BOX.
     RETURN NO-APPLY.
  END.
  ASSIGN fi-razao-social:SCREEN-VALUE IN FRAME {&FRAME-NAME} = empresa.razao-social.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ep-codigo w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ep-codigo IN FRAME f-pg-sel /* Empresa */
DO:
  {include/zoomvar.i &prog-zoom = unzoom/z01un004.w
                     &campo     = fi-ep-codigo
                     &campozoom = ep-codigo
                     &FRAME     = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME im-pg-dig
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-dig w-relat
ON MOUSE-SELECT-CLICK OF im-pg-dig IN FRAME f-relat
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME im-pg-imp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-imp w-relat
ON MOUSE-SELECT-CLICK OF im-pg-imp IN FRAME f-relat
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME im-pg-par
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-par w-relat
ON MOUSE-SELECT-CLICK OF im-pg-par IN FRAME f-relat
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME im-pg-sel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-sel w-relat
ON MOUSE-SELECT-CLICK OF im-pg-sel IN FRAME f-relat
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-imp
&Scoped-define SELF-NAME rs-destino
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-destino w-relat
ON VALUE-CHANGED OF rs-destino IN FRAME f-pg-imp
DO:
do  with frame f-pg-imp:
    case self:screen-value:
        when "1" then do:
            assign c-arquivo:sensitive    = no
                   bt-arquivo:visible     = no
                   bt-config-impr:visible = yes.
        end.
        when "2" then do:
            assign c-arquivo:sensitive     = yes
                   bt-arquivo:visible      = yes
                   bt-config-impr:visible  = no.
        end.
        when "3" then do:
            assign c-arquivo:sensitive     = no
                   bt-arquivo:visible      = no
                   bt-config-impr:visible  = no.
        end.
    end case.
end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-execucao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-execucao w-relat
ON VALUE-CHANGED OF rs-execucao IN FRAME f-pg-imp
DO:
   {include/i-rprse.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME tg-email
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-email w-relat
ON VALUE-CHANGED OF tg-email IN FRAME f-pg-par /* Envia e-mail */
DO:
  IF INPUT FRAME {&frame-name} tg-email = YES THEN DO.
     FIND mensagem WHERE
          mensagem.cod-mensagem = INPUT FRAME {&FRAME-NAME} fi-cod-mensagem NO-LOCK NO-ERROR.

     ASSIGN fi-assunto:SENSITIVE IN FRAME f-pg-par = YES.

     rs-destino:DISABLE(ENTRY(5,rs-destino:RADIO-BUTTONS IN FRAME f-pg-imp)) IN FRAME f-pg-imp.
     rs-destino:DISABLE(ENTRY(1,rs-destino:RADIO-BUTTONS IN FRAME f-pg-imp)) IN FRAME f-pg-imp.
  END.
  ELSE DO.
     ASSIGN fi-assunto:SENSITIVE IN FRAME f-pg-par = NO.

     rs-destino:ENABLE(ENTRY(5,rs-destino:RADIO-BUTTONS IN FRAME f-pg-imp)) IN FRAME f-pg-imp.
     rs-destino:ENABLE(ENTRY(1,rs-destino:RADIO-BUTTONS IN FRAME f-pg-imp)) IN FRAME f-pg-imp.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-dig
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-relat 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{utp/ut9000.i "ESCR0004" "2.04.00.000"}

/*:T inicializaá‰es do template de relat¢rio */
{include/i-rpini.i}

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

{include/i-rplbl.i}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO  ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    RUN enable_UI.

    ASSIGN fi-ep-codigo:SCREEN-VALUE IN FRAME f-pg-sel = "1" 
           fi-cod-estabel:SCREEN-VALUE IN FRAME f-pg-sel = "2".

    FIND estabelec WHERE estabelec.cod-estabel = "2" NO-LOCK NO-ERROR.
    IF AVAIL estabelec THEN
       ASSIGN fi-nome-estab:SCREEN-VALUE IN FRAME f-pg-sel = estabelec.nome.
    
    {include/i-rpmbl.i}

    fi-ep-codigo:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-cod-estabel:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.

    {include/i-rpmbl.i im-pg-par}  /* Entra na frame f-pg-par para carregar o texto da mensagem */
    {include/i-rpmbl.i im-pg-sel}  /* Retorna para frame f-pg-sel */

    IF  NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-relat  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-relat  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-relat  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-relat)
  THEN DELETE WIDGET w-relat.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-relat  _DEFAULT-ENABLE
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
  ENABLE bt-executar bt-cancelar bt-ajuda im-pg-dig im-pg-imp im-pg-par 
         im-pg-sel 
      WITH FRAME f-relat IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}
  DISPLAY fi-ep-codigo fi-razao-social fi-cod-estabel fi-nome-estab fi-cod-esp 
          fi-serie fi-ini-nr-docto fi-fin-nr-docto fi-ini-parcela fi-fin-parcela 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  ENABLE fi-ep-codigo fi-cod-estabel fi-cod-esp fi-serie fi-ini-nr-docto 
         fi-fin-nr-docto fi-ini-parcela fi-fin-parcela IMAGE-3 IMAGE-4 IMAGE-5 
         IMAGE-6 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}
  DISPLAY rs-destino c-arquivo rs-execucao 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  ENABLE rs-destino bt-arquivo bt-config-impr c-arquivo rs-execucao RECT-7 
         RECT-9 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}
  DISPLAY fi-cod-mensagem tg-email ed-texto 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  ENABLE fi-cod-mensagem btn-anterior btn-proximo tg-email ed-texto 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-par}
  ENABLE br-digita bt-inserir bt-recuperar 
      WITH FRAME f-pg-dig IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-dig}
  VIEW w-relat.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-relat 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-executar w-relat 
PROCEDURE pi-executar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define var r-tt-digita as rowid no-undo.

do on error undo, return error on stop  undo, return error:
    {include/i-rpexa.i}
    
    if input frame f-pg-imp rs-destino = 2 and
       input frame f-pg-imp rs-execucao = 1 then do:
        run utp/ut-vlarq.p (input input frame f-pg-imp c-arquivo).
        
        if return-value = "NOK":U then do:
            run utp/ut-msgs.p (input "show":U, input 73, input "").
            
            apply "MOUSE-SELECT-CLICK":U to im-pg-imp in frame f-relat.
            apply "ENTRY":U to c-arquivo in frame f-pg-imp.
            return error.
        end.
    end.
    
    /*:T Coloque aqui as validaá‰es da p†gina de Digitaá∆o, lembrando que elas devem
       apresentar uma mensagem de erro cadastrada, posicionar nesta p†gina e colocar
       o focus no campo com problemas */
    /*browse br-digita:SET-REPOSITIONED-ROW (browse br-digita:DOWN, "ALWAYS":U).*/
    
    for each tt-digita no-lock:
        assign r-tt-digita = rowid(tt-digita).
        
        /*:T Validaá∆o de duplicidade de registro na temp-table tt-digita */
        find first b-tt-digita where b-tt-digita.ep-codigo   = tt-digita.ep-codigo
                                 and b-tt-digita.cod-estabel = tt-digita.cod-estabel
                                 AND b-tt-digita.serie       = tt-digita.serie
                                 AND b-tt-digita.nr-docto    = tt-digita.nr-docto
                                 AND b-tt-digita.parcela     = tt-digita.parcela
                                 AND rowid(b-tt-digita) <> rowid(tt-digita) no-lock no-error.
        if avail b-tt-digita then do:
            apply "MOUSE-SELECT-CLICK":U to im-pg-dig in frame f-relat.
            reposition br-digita to rowid rowid(b-tt-digita).
            
            run utp/ut-msgs.p (input "show":U, input 108, input "").
            apply "ENTRY":U to tt-digita.ep-codigo in browse br-digita.

            return error.
        end.
        
        /*:T Validaá∆o de registro da temp-table tt-digita x titulo */
        find titulo where titulo.ep-codigo   = tt-digita.ep-codigo   
                      and titulo.cod-estabel = tt-digita.cod-estabel 
                      AND titulo.serie       = tt-digita.serie       
                      and titulo.nr-docto    = tt-digita.nr-docto    
                      AND titulo.parcela     = tt-digita.parcela     
                    no-lock no-error.
        if NOT avail titulo then do:
            MESSAGE "T°tulo - Empresa:" tt-digita.ep-codigo
                             "Estab:" tt-digita.cod-estabel
                             "SÇrie:" tt-digita.serie
                             "Docto:" tt-digita.nr-docto
                             "Parcela:" tt-digita.parcela
                    "- N∆o cadastrado."
                    VIEW-AS ALERT-BOX.
            apply "ENTRY":U to tt-digita.ep-codigo in browse br-digita.
            return error.
        end.
    end.
    
    
    /*:T Coloque aqui as validaá‰es das outras p†ginas, lembrando que elas devem 
       apresentar uma mensagem de erro cadastrada, posicionar na p†gina com 
       problemas e colocar o focus no campo com problemas */
    
    
    
    /*:T Aqui s∆o gravados os campos da temp-table que ser† passada como parÉmetro
       para o programa RP.P */
    
    create tt-param.
    assign tt-param.usuario      = c-seg-usuario
           tt-param.destino      = input frame f-pg-imp rs-destino
           tt-param.data-exec    = today
           tt-param.hora-exec    = TIME
           tt-param.ep-codigo    = INPUT FRAME f-pg-sel fi-ep-codigo
           tt-param.cod-estabel  = INPUT FRAME f-pg-sel fi-cod-estabel
           tt-param.cod-esp      = INPUT FRAME f-pg-sel fi-cod-esp
           tt-param.serie        = INPUT FRAME f-pg-sel fi-serie
           tt-param.nr-docto-ini = INPUT FRAME f-pg-sel fi-ini-nr-docto
           tt-param.nr-docto-fin = INPUT FRAME f-pg-sel fi-fin-nr-docto
           tt-param.parcela-ini  = INPUT FRAME f-pg-sel fi-ini-parcela
           tt-param.parcela-fin  = INPUT FRAME f-pg-sel fi-fin-parcela
           tt-param.e-mail       = INPUT FRAME f-pg-par tg-email
           tt-param.subject      = INPUT FRAME f-pg-par fi-assunto 
           tt-param.texto        = INPUT FRAME f-pg-par ed-texto.
    
    if tt-param.destino = 1 
    then assign tt-param.arquivo = "".
    else if  tt-param.destino = 2 
         then assign tt-param.arquivo = input frame f-pg-imp c-arquivo.
         else assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp":U.
    
    /*:T Coloque aqui a l¢gica de gravaá∆o dos demais campos que devem ser passados
       como parÉmetros para o programa RP.P, atravÇs da temp-table tt-param */
    
    
    
    /*:T Executar do programa RP.P que ir† criar o relat¢rio */
    {include/i-rpexb.i}
    
    SESSION:SET-WAIT-STATE("general":U).
    
    {include/i-rprun.i esrp/escr0004rp.p}
    
    {include/i-rpexc.i}
    
    SESSION:SET-WAIT-STATE("":U).
    
    {include/i-rptrm.i}
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-troca-pagina w-relat 
PROCEDURE pi-troca-pagina :
/*:T------------------------------------------------------------------------------
  Purpose: Gerencia a Troca de P†gina (folder)   
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

{include/i-rptrp.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-relat  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-digita"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-relat 
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

