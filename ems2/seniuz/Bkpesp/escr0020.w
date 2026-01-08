&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
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
{include/i-prgvrs.i ESCR0011 9.99.99.999}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/*:T Preprocessadores do Template de Relat¢rio                            */
/*:T Obs: Retirar o valor do preprocessador para as p ginas que nÆo existirem  */

&GLOBAL-DEFINE PGSEL f-pg-sel
&GLOBAL-DEFINE PGCLA 
&GLOBAL-DEFINE PGPAR f-pg-par
&GLOBAL-DEFINE PGDIG 
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
    FIELD cod-estabel-ini  LIKE titulo.cod-estabel
    FIELD cod-estabel-fim  LIKE titulo.cod-estabel
    FIELD cod-esp-ini      LIKE titulo.cod-esp
    FIELD cod-esp-fim      LIKE titulo.cod-esp
    FIELD serie-ini        LIKE titulo.serie
    FIELD serie-fim        LIKE titulo.serie
    FIELD repres-ini       LIKE titulo.cod-rep
    FIELD repres-fim       LIKE titulo.cod-rep
    FIELD cod-port-ini     LIKE titulo.cod-port
    FIELD cod-port-fim     LIKE titulo.cod-port
    FIELD cliente-ini      LIKE titulo.cod-emitente
    FIELD cliente-fim      LIKE titulo.cod-emitente
    FIELD dt-emissao-ini   LIKE titulo.dt-emissao
    FIELD dt-emissao-fim   LIKE titulo.dt-emissao
    FIELD dt-vencimen-ini  LIKE titulo.dt-vencimen
    FIELD dt-vencimen-fim  LIKE titulo.dt-vencimen
    FIELD cod-mensagem     LIKE mensagem.cod-mensagem
    FIELD e-mail           AS LOG
    FIELD subject          AS CHAR FORMAT "x(40)"
    FIELD texto            AS CHAR FORMAT "x(2000)".

define temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id ordem.

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

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-pg-imp

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 RECT-7 RECT-9 rs-destino bt-arquivo ~
bt-config-impr c-arquivo rs-execucao 
&Scoped-Define DISPLAYED-OBJECTS rs-destino c-arquivo rs-execucao 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-relat AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
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

DEFINE VARIABLE text-modo AS CHARACTER FORMAT "X(256)":U INITIAL "Execu‡Æo" 
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

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 77 BY 10.67.

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
     SIZE 70 BY 6.5 NO-UNDO.

DEFINE VARIABLE fi-assunto AS CHARACTER FORMAT "X(256)":U 
     LABEL "Assunto" 
     VIEW-AS FILL-IN 
     SIZE 40 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-mensagem AS INTEGER FORMAT ">>9":U INITIAL 26 
     LABEL "Mensagem" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ep-codigo AS INTEGER FORMAT ">>9":U INITIAL 1 
     LABEL "Empresa" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nome-empresa AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 53.29 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-15
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 77 BY 10.67.

DEFINE VARIABLE tg-email AS LOGICAL INITIAL no 
     LABEL "Envia e-mail" 
     VIEW-AS TOGGLE-BOX
     SIZE 12.72 BY .83 NO-UNDO.

DEFINE VARIABLE fi-fim-cod-emitente AS INTEGER FORMAT ">>>>>>>>9" INITIAL 999999999 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fim-cod-esp AS CHARACTER FORMAT "!!":U INITIAL "ZZ" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fim-cod-estabel AS CHARACTER FORMAT "X(3)":U INITIAL "ZZZ" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fim-cod-port AS INTEGER FORMAT ">>>>9":U INITIAL 99999 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fim-cod-rep AS INTEGER FORMAT ">>>>9" INITIAL 99999 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fim-dt-emissao AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fim-dt-vencimen AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fim-serie AS CHARACTER FORMAT "x(5)" INITIAL "1" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 TOOLTIP "S‚rie final" NO-UNDO.

DEFINE VARIABLE fi-ini-cod-emitente AS INTEGER FORMAT ">>>>>>>>9" INITIAL 0 
     LABEL "Cliente":R17 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-cod-esp AS CHARACTER FORMAT "!!":U 
     LABEL "Esp‚cie" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-cod-estabel AS CHARACTER FORMAT "X(3)":U 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-cod-port AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Portador" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-cod-rep AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Representante" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-dt-emissao AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Data EmissÆo" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-dt-vencimen AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Data Vencimento" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-serie AS CHARACTER FORMAT "x(5)" INITIAL "1" 
     LABEL "S‚rie":R7 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 TOOLTIP "S‚rie inicial" NO-UNDO.

DEFINE VARIABLE fi-razao-social AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 47.14 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-10
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-27
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-28
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-29
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-30
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-31
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-32
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-33
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-34
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-35
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-36
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-7
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-8
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-9
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE RECTANGLE RECT-16
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 77 BY 10.71.

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


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-pg-imp
     rs-destino AT ROW 2.38 COL 3.29 HELP
          "Destino de ImpressÆo do Relat¢rio" NO-LABEL
     bt-arquivo AT ROW 3.58 COL 43.29 HELP
          "Escolha do nome do arquivo"
     bt-config-impr AT ROW 3.58 COL 43.29 HELP
          "Configura‡Æo da impressora"
     c-arquivo AT ROW 3.63 COL 3.29 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL
     rs-execucao AT ROW 5.75 COL 3 HELP
          "Modo de Execu‡Æo" NO-LABEL
     text-destino AT ROW 1.63 COL 3.86 NO-LABEL
     text-modo AT ROW 5 COL 1.29 COLON-ALIGNED NO-LABEL
     RECT-17 AT ROW 1.08 COL 1.29
     RECT-7 AT ROW 1.92 COL 2.14
     RECT-9 AT ROW 5.29 COL 2.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2.43 ROW 2.83
         SIZE 77.72 BY 10.83.

DEFINE FRAME f-relat
     bt-executar AT ROW 14.54 COL 3 HELP
          "Dispara a execu‡Æo do relat¢rio"
     bt-cancelar AT ROW 14.54 COL 14 HELP
          "Fechar"
     bt-ajuda AT ROW 14.54 COL 70 HELP
          "Ajuda"
     rt-folder-top AT ROW 2.54 COL 2.14
     RECT-1 AT ROW 14.29 COL 2
     RECT-6 AT ROW 13.75 COL 2.14
     rt-folder-right AT ROW 2.67 COL 80.43
     rt-folder-left AT ROW 2.54 COL 2.14
     rt-folder AT ROW 2.5 COL 2
     im-pg-imp AT ROW 1.5 COL 33.43
     im-pg-par AT ROW 1.5 COL 17.72
     im-pg-sel AT ROW 1.5 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81 BY 15
         DEFAULT-BUTTON bt-executar.

DEFINE FRAME f-pg-par
     fi-ep-codigo AT ROW 1.67 COL 11.14 COLON-ALIGNED
     fi-nome-empresa AT ROW 1.67 COL 16.72 COLON-ALIGNED NO-LABEL
     fi-cod-mensagem AT ROW 2.67 COL 11.14 COLON-ALIGNED
     fi-assunto AT ROW 2.67 COL 30 COLON-ALIGNED
     btn-anterior AT ROW 3.67 COL 13
     btn-proximo AT ROW 3.67 COL 17
     tg-email AT ROW 3.67 COL 32
     ed-texto AT ROW 4.92 COL 4.29 NO-LABEL
     RECT-15 AT ROW 1.08 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2.72 ROW 2.83
         SIZE 77.29 BY 10.79
         FONT 1.

DEFINE FRAME f-pg-sel
     fi-ep-codigo AT ROW 1.79 COL 18.43 COLON-ALIGNED HELP
          "C¢digo da empresa"
          LABEL "Empresa":R9 FORMAT ">>9"
          VIEW-AS FILL-IN 
          SIZE 5 BY .88 TOOLTIP "C¢digo da empresa"
     fi-razao-social AT ROW 1.79 COL 23.57 COLON-ALIGNED NO-LABEL
     fi-ini-cod-estabel AT ROW 2.79 COL 18.43 COLON-ALIGNED
     fi-fim-cod-estabel AT ROW 2.79 COL 42.43 COLON-ALIGNED NO-LABEL
     fi-ini-cod-esp AT ROW 3.79 COL 18.43 COLON-ALIGNED
     fi-fim-cod-esp AT ROW 3.79 COL 42.43 COLON-ALIGNED NO-LABEL
     fi-ini-serie AT ROW 4.79 COL 18.43 COLON-ALIGNED HELP
          "S‚rie da nota fiscal"
     fi-fim-serie AT ROW 4.79 COL 42.43 COLON-ALIGNED HELP
          "S‚rie da nota fiscal" NO-LABEL
     fi-ini-cod-rep AT ROW 5.79 COL 18.43 COLON-ALIGNED
     fi-fim-cod-rep AT ROW 5.79 COL 42.43 COLON-ALIGNED HELP
          "Nome abreviado do representante principal" NO-LABEL
     fi-ini-cod-port AT ROW 6.79 COL 18.43 COLON-ALIGNED
     fi-fim-cod-port AT ROW 6.79 COL 42.43 COLON-ALIGNED NO-LABEL
     fi-ini-cod-emitente AT ROW 7.79 COL 18.43 COLON-ALIGNED HELP
          "Nome abreviado do cliente/fornecedor"
     fi-fim-cod-emitente AT ROW 7.79 COL 42.43 COLON-ALIGNED HELP
          "Nome abreviado do cliente/fornecedor" NO-LABEL
     fi-ini-dt-emissao AT ROW 8.79 COL 18.43 COLON-ALIGNED
     fi-fim-dt-emissao AT ROW 8.79 COL 42.43 COLON-ALIGNED HELP
          "C¢digo do Item" NO-LABEL
     fi-ini-dt-vencimen AT ROW 9.79 COL 18.43 COLON-ALIGNED
     fi-fim-dt-vencimen AT ROW 9.79 COL 42.43 COLON-ALIGNED HELP
          "C¢digo do Item" NO-LABEL
     IMAGE-1 AT ROW 7.83 COL 31.86
     IMAGE-10 AT ROW 4.83 COL 40.72
     IMAGE-2 AT ROW 7.83 COL 40.72
     IMAGE-27 AT ROW 2.83 COL 31.86
     IMAGE-28 AT ROW 2.83 COL 40.72
     IMAGE-29 AT ROW 8.83 COL 31.86
     IMAGE-30 AT ROW 8.83 COL 40.72
     IMAGE-31 AT ROW 3.83 COL 31.86
     IMAGE-32 AT ROW 3.83 COL 40.72
     IMAGE-33 AT ROW 5.83 COL 31.86
     IMAGE-34 AT ROW 5.83 COL 40.72
     IMAGE-35 AT ROW 9.79 COL 31.86
     IMAGE-36 AT ROW 9.79 COL 40.72
     IMAGE-7 AT ROW 6.83 COL 31.86
     IMAGE-8 AT ROW 6.83 COL 40.72
     IMAGE-9 AT ROW 4.83 COL 31.86
     RECT-16 AT ROW 1.08 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2.72 ROW 2.83
         SIZE 77.43 BY 10.79
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
         TITLE              = "Cartas de Notifica‡Æo"
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
/* SETTINGS FOR FRAME f-pg-imp
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN text-destino IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       text-destino:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Destino".

/* SETTINGS FOR FILL-IN text-modo IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       text-modo:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Execu‡Æo".

/* SETTINGS FOR FRAME f-pg-par
                                                                        */
/* SETTINGS FOR FILL-IN fi-assunto IN FRAME f-pg-par
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FILL-IN fi-nome-empresa IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME f-pg-sel
                                                                        */
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
ON END-ERROR OF w-relat /* Cartas de Notifica‡Æo */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON WINDOW-CLOSE OF w-relat /* Cartas de Notifica‡Æo */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
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


&Scoped-define SELF-NAME fi-cod-mensagem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-mensagem w-relat
ON ENTRY OF fi-cod-mensagem IN FRAME f-pg-par /* Mensagem */
DO:
  FIND mensagem WHERE
       mensagem.cod-mensagem = INPUT FRAME {&FRAME-NAME} fi-cod-mensagem NO-LOCK NO-ERROR.

  IF AVAIL mensagem THEN 
     ASSIGN ed-texto:SCREEN-VALUE IN FRAME f-pg-par = mensagem.texto-mensag.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-mensagem w-relat
ON LEAVE OF fi-cod-mensagem IN FRAME f-pg-par /* Mensagem */
DO:
  FIND mensagem WHERE
       mensagem.cod-mensagem = INPUT FRAME {&FRAME-NAME} fi-cod-mensagem NO-LOCK NO-ERROR.

  IF NOT AVAIL mensagem THEN DO.
     MESSAGE "Mensagem nÆo Cadastrada..." VIEW-AS ALERT-BOX.
     APPLY 'entry' TO SELF.
     RETURN NO-APPLY.
  END.
  ASSIGN ed-texto:SCREEN-VALUE IN FRAME f-pg-par = mensagem.texto-mensag + CHR(13) + CHR(13) +
                                                   "Atenciosamente," + CHR(13) + CHR(13) + CHR(13) +
                                                   empresa.razao-social  + CHR(13) +
                                                   "Departamento Financeiro/Cobranca".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-mensagem w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-cod-mensagem IN FRAME f-pg-par /* Mensagem */
DO:
  {include/zoomvar.i &prog-zoom=adzoom/z01ad176.w
                     &campo=fi-cod-mensagem
                     &campozoom=cod-mensagem
                     &FRAME=f-pg-par}
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
     MESSAGE "Empresa nÆo Cadastrada." VIEW-AS ALERT-BOX.
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


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME fi-ep-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ep-codigo w-relat
ON ENTRY OF fi-ep-codigo IN FRAME f-pg-par /* Empresa */
DO:
  IF INT(SELF:SCREEN-VALUE) <> 0 THEN DO.
     FIND FIRST empresa WHERE
                empresa.ep-codigo = INPUT FRAME {&FRAME-NAME} fi-ep-codigo NO-LOCK NO-ERROR. 

     IF AVAIL empresa  THEN
        ASSIGN fi-nome-empresa:SCREEN-VALUE IN FRAME {&FRAME-NAME} = empresa.razao-social.
  END.
  ELSE ASSIGN fi-nome-empresa:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ep-codigo w-relat
ON LEAVE OF fi-ep-codigo IN FRAME f-pg-par /* Empresa */
DO:
  find FIRST empresa WHERE
             empresa.ep-codigo = INPUT FRAME {&FRAME-NAME} fi-ep-codigo NO-LOCK NO-ERROR. 

  IF NOT AVAIL empresa THEN DO.
     MESSAGE "Empresa nÆo Cadastrada..." VIEW-AS ALERT-BOX.
     RETURN NO-APPLY.
  END.
  ASSIGN fi-nome-empresa:SCREEN-VALUE IN FRAME {&FRAME-NAME} = empresa.razao-social.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ep-codigo w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ep-codigo IN FRAME f-pg-par /* Empresa */
DO:
  {include/zoomvar.i &prog-zoom=unzoom/z01un004.w
                     &campo=fi-ep-codigo
                     &campozoom=ep-codigo
                     &FRAME=f-pg-par}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-sel
&Scoped-define SELF-NAME fi-fim-cod-emitente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fim-cod-emitente w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-fim-cod-emitente IN FRAME f-pg-sel
DO:
    {include/zoomvar.i &prog-zoom=adzoom/z02ad098.w
                       &campo=fi-fim-cod-emitente
                       &campozoom=cod-emitente
                       &FRAME=f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-fim-cod-esp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fim-cod-esp w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-fim-cod-esp IN FRAME f-pg-sel
DO:
    {include/zoomvar.i &prog-zoom=adzoom/z01ad103.w
                       &campo=fi-fim-cod-esp
                       &campozoom=cod-esp
                       &FRAME=f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-fim-cod-estabel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fim-cod-estabel w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-fim-cod-estabel IN FRAME f-pg-sel
DO:
  {include/zoomvar.i &prog-zoom=adzoom/z01ad107.w
                     &campo=fi-fim-cod-estabel
                     &campozoom=cod-estabel
                     &FRAME=f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-fim-cod-rep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fim-cod-rep w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-fim-cod-rep IN FRAME f-pg-sel
DO:
    {include/zoomvar.i &prog-zoom=adzoom/z01ad229.w
                       &campo=fi-fim-cod-rep
                       &campozoom=cod-rep
                       &FRAME=f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ini-cod-emitente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-cod-emitente w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ini-cod-emitente IN FRAME f-pg-sel /* Cliente */
DO:
    {include/zoomvar.i &prog-zoom=adzoom/z02ad098.w
                       &campo=fi-ini-cod-emitente
                       &campozoom=cod-emitente
                       &FRAME=f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ini-cod-esp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-cod-esp w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ini-cod-esp IN FRAME f-pg-sel /* Esp‚cie */
DO:
  {include/zoomvar.i &prog-zoom=adzoom/z01ad103.w
                     &campo=fi-ini-cod-esp
                     &campozoom=cod-esp
                     &FRAME=f-pg-sel}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ini-cod-estabel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-cod-estabel w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ini-cod-estabel IN FRAME f-pg-sel /* Estabelecimento */
DO:
  {include/zoomvar.i &prog-zoom=adzoom/z01ad107.w
                     &campo=fi-ini-cod-estabel
                     &campozoom=cod-estabel
                     &FRAME=f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ini-cod-rep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-cod-rep w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ini-cod-rep IN FRAME f-pg-sel /* Representante */
DO:
    {include/zoomvar.i &prog-zoom=adzoom/z01ad229.w
                       &campo=fi-ini-cod-rep
                       &campozoom=cod-rep
                       &FRAME=f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
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


&Scoped-define FRAME-NAME f-pg-imp
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-relat 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{utp/ut9000.i "ESCR0020" "2.04.00.000"}

/*:T inicializa‡äes do template de relat¢rio */
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
           fi-ini-cod-estabel:SCREEN-VALUE IN FRAME f-pg-sel = "2"
           fi-fim-cod-estabel:SCREEN-VALUE IN FRAME f-pg-sel = "2"
           fi-ini-dt-vencimen:SCREEN-VALUE IN FRAME f-pg-sel = string(TODAY)
           fi-fim-dt-vencimen:SCREEN-VALUE IN FRAME f-pg-sel = string(TODAY).

    FIND empresa WHERE
         empresa.ep-codigo = 1 NO-LOCK NO-ERROR.

    FIND mensagem WHERE
         mensagem.cod-mensagem = 26 NO-LOCK NO-ERROR.

    IF AVAIL mensagem THEN
       ASSIGN fi-assunto:SCREEN-VALUE IN FRAME f-pg-par = mensagem.descricao
              ed-texto:SCREEN-VALUE IN FRAME f-pg-par = mensagem.texto-mensag + CHR(13) + CHR(13) +
                                                        "Atenciosamente," + CHR(13) + CHR(13) + CHR(13) +
                                                        empresa.razao-social  + CHR(13) +
                                                        "Departamento Financeiro/Cobranca".
  
    {include/i-rpmbl.i}

    fi-ep-codigo:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-par.
    fi-cod-mensagem:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-par.
    fi-ini-cod-estabel:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-fim-cod-estabel:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-ini-cod-esp:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-fim-cod-esp:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-ini-cod-rep:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-fim-cod-rep:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-ini-cod-emitente:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-fim-cod-emitente:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
  
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
  ENABLE im-pg-imp im-pg-par im-pg-sel bt-executar bt-cancelar bt-ajuda 
      WITH FRAME f-relat IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}
  DISPLAY rs-destino c-arquivo rs-execucao 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  ENABLE RECT-17 RECT-7 RECT-9 rs-destino bt-arquivo bt-config-impr c-arquivo 
         rs-execucao 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}
  DISPLAY fi-ep-codigo fi-nome-empresa fi-cod-mensagem tg-email ed-texto 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  ENABLE RECT-15 fi-ep-codigo fi-cod-mensagem btn-anterior btn-proximo tg-email 
         ed-texto 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-par}
  DISPLAY fi-ep-codigo fi-razao-social fi-ini-cod-estabel fi-fim-cod-estabel 
          fi-ini-cod-esp fi-fim-cod-esp fi-ini-serie fi-fim-serie fi-ini-cod-rep 
          fi-fim-cod-rep fi-ini-cod-port fi-fim-cod-port fi-ini-cod-emitente 
          fi-fim-cod-emitente fi-ini-dt-emissao fi-fim-dt-emissao 
          fi-ini-dt-vencimen fi-fim-dt-vencimen 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  ENABLE IMAGE-1 IMAGE-10 IMAGE-2 IMAGE-27 IMAGE-28 IMAGE-29 IMAGE-30 IMAGE-31 
         IMAGE-32 IMAGE-33 IMAGE-34 IMAGE-35 IMAGE-36 IMAGE-7 IMAGE-8 IMAGE-9 
         RECT-16 fi-ep-codigo fi-ini-cod-estabel fi-fim-cod-estabel 
         fi-ini-cod-esp fi-fim-cod-esp fi-ini-serie fi-fim-serie fi-ini-cod-rep 
         fi-fim-cod-rep fi-ini-cod-port fi-fim-cod-port fi-ini-cod-emitente 
         fi-fim-cod-emitente fi-ini-dt-emissao fi-fim-dt-emissao 
         fi-ini-dt-vencimen fi-fim-dt-vencimen 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}
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
    
    /*:T Coloque aqui as valida‡äes da p gina de Digita‡Æo, lembrando que elas devem
       apresentar uma mensagem de erro cadastrada, posicionar nesta p gina e colocar
       o focus no campo com problemas */
    /*browse br-digita:SET-REPOSITIONED-ROW (browse br-digita:DOWN, "ALWAYS":U).*/
    
    
    /*:T Coloque aqui as valida‡äes das outras p ginas, lembrando que elas devem 
       apresentar uma mensagem de erro cadastrada, posicionar na p gina com 
       problemas e colocar o focus no campo com problemas */
    
    
    
    /*:T Aqui sÆo gravados os campos da temp-table que ser  passada como parƒmetro
       para o programa RP.P */
    
    create tt-param.
    assign tt-param.usuario         = c-seg-usuario
           tt-param.destino         = input frame f-pg-imp rs-destino
           tt-param.data-exec       = today
           tt-param.hora-exec       = time.
    
    if tt-param.destino = 1 
    then assign tt-param.arquivo = "".
    else if  tt-param.destino = 2 
         then assign tt-param.arquivo = input frame f-pg-imp c-arquivo.
         else assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp":U.
    
    /*:T Coloque aqui a l¢gica de grava‡Æo dos demais campos que devem ser passados
       como parƒmetros para o programa RP.P, atrav‚s da temp-table tt-param */
    ASSIGN tt-param.ep-codigo       = INPUT FRAME f-pg-par fi-ep-codigo      
           tt-param.cod-estabel-ini = INPUT FRAME f-pg-sel fi-ini-cod-estabel
           tt-param.cod-estabel-fim = INPUT FRAME f-pg-sel fi-fim-cod-estabel
           tt-param.cod-esp-ini     = INPUT FRAME f-pg-sel fi-ini-cod-esp
           tt-param.cod-esp-fim     = INPUT FRAME f-pg-sel fi-fim-cod-esp
           tt-param.serie-ini       = INPUT FRAME f-pg-sel fi-ini-serie 
           tt-param.serie-fim       = INPUT FRAME f-pg-sel fi-fim-serie 
           tt-param.repres-ini      = input frame f-pg-sel fi-ini-cod-rep
           tt-param.repres-fim      = input frame f-pg-sel fi-fim-cod-rep
           tt-param.cod-port-ini    = INPUT FRAME f-pg-sel fi-ini-cod-port
           tt-param.cod-port-fim    = INPUT FRAME f-pg-sel fi-fim-cod-port
           tt-param.cliente-ini     = input frame f-pg-sel fi-ini-cod-emitente
           tt-param.cliente-fim     = input frame f-pg-sel fi-fim-cod-emitente
           tt-param.dt-emissao-ini  = INPUT FRAME f-pg-sel fi-ini-dt-emissao
           tt-param.dt-emissao-fim  = INPUT FRAME f-pg-sel fi-fim-dt-emissao
           tt-param.dt-vencimen-ini = INPUT FRAME f-pg-sel fi-ini-dt-vencimen
           tt-param.dt-vencimen-fim = INPUT FRAME f-pg-sel fi-fim-dt-vencimen
           tt-param.cod-mensagem    = INPUT FRAME f-pg-par fi-cod-mensagem
           tt-param.e-mail          = INPUT FRAME f-pg-par tg-email
           tt-param.subject         = INPUT FRAME f-pg-par fi-assunto 
           tt-param.texto           = INPUT FRAME f-pg-par ed-texto.
           
     /*:T Executar do programa RP.P que ir  criar o relat¢rio */
    {include/i-rpexb.i}
    
    SESSION:SET-WAIT-STATE("general":U).
    
    {include/i-rprun.i esrp/escr0020rp.p} 
    
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
  Purpose: Gerencia a Troca de P gina (folder)   
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

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this w-relat, and there are no
     tables specified in any contained Browse, Query, or Frame. */

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

