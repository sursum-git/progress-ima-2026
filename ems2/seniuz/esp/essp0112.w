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
{include/i-prgvrs.i ESSP0112 2.04.00.000}

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
&GLOBAL-DEFINE PGCLA f-pg-cla
&GLOBAL-DEFINE PGPAR f-pg-par
&GLOBAL-DEFINE PGDIG
&GLOBAL-DEFINE PGIMP f-pg-imp
  
/* Parameters Definitions ---                                           */

DEF TEMP-TABLE tt-param-ini
    FIELD dt-chamada-ini AS CHAR
    FIELD dt-chamada-fin AS CHAR
    FIELD arq-cham-loc   AS CHAR  
    FIELD arq-cham-nloc  AS CHAR
    FIELD arq-ramais     AS CHAR
    FIELD arq-tmp-email  AS CHAR
    FIELD arq-resumo     AS CHAR
    FIELD gerar-resumo   AS CHAR
    FIELD vlr-fat-loc    AS DEC
    FIELD vlr-fat-nloc   AS DEC
    FIELD email-telefon  AS CHAR
    FIELD email-info     AS CHAR.

/* Temporary Table Definitions ---                                      */

define temp-table tt-param no-undo
       field destino          as integer
       field arquivo          as char format "x(35)"
       field usuario          as char format "x(12)"
       field data-exec        as date
       field hora-exec        as integer
       field classifica       as integer
       field desc-classifica  as char format "x(40)"
       FIELD ini-responsavel  AS CHAR FORMAT "x(20)"
       FIELD fin-responsavel  AS CHAR FORMAT "x(20)"
       FIELD ini-dt-chamada   AS DATE FORMAT 99/99/9999
       FIELD fin-dt-chamada   AS DATE FORMAT 99/99/9999
       FIELD ini-ramal        AS INTEGER
       FIELD fin-ramal        AS INTEGER
       FIELD arq-cham-loc     AS CHAR FORMAT "x(55)"
       FIELD arq-cham-nloc    AS CHAR FORMAT "x(55)"
       FIELD arq-ramais       AS CHAR FORMAT "x(55)"
       FIELD arq-tmp-email    AS CHAR FORMAT "x(55)"
       FIELD vlr-fat-loc      AS DEC
       FIELD vlr-fat-nloc     AS DEC
       FIELD gerar-resumo     AS LOG FORMAT "Sim/N∆o"
       FIELD arq-resumo       AS CHAR FORMAT "x(55)"
       FIELD enviar-e-mail    AS LOG FORMAT "Sim/N∆o"
       FIELD enviar-resp      AS LOG FORMAT "Sim/N∆o"
       FIELD enviar-telefon   AS LOG FORMAT "Sim/N∆o"
       FIELD enviar-info      AS LOG FORMAT "Sim/N∆o"
       FIELD email-telefon    AS CHAR FORMAT "x(40)"
       FIELD email-info       AS CHAR FORMAT "x(40)"
       FIELD subject-e-mail   AS CHAR FORMAT "x(40)"
       FIELD texto-e-mail     AS CHAR FORMAT "x(2000)"
       FIELD imp-param        AS LOG.

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

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME f-pg-cla

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS to-imp-param to-enviar-e-mail 
&Scoped-Define DISPLAYED-OBJECTS to-imp-param to-enviar-e-mail ~
tg-enviar-resp tg-enviar-telefon fi-email-telefon tg-enviar-info ~
fi-email-info fi-texto-e-mail 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-relat AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE fi-texto-e-mail AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 58 BY 4 NO-UNDO.

DEFINE VARIABLE fi-assunto-e-mail AS CHARACTER FORMAT "X(256)":U 
     LABEL "Assunto" 
     VIEW-AS FILL-IN 
     SIZE 47 BY .88 NO-UNDO.

DEFINE VARIABLE fi-email-info AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 30 BY .88 NO-UNDO.

DEFINE VARIABLE fi-email-telefon AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 30 BY .88 NO-UNDO.

DEFINE VARIABLE tg-enviar-info AS LOGICAL INITIAL no 
     LABEL "Inform†tica" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE tg-enviar-resp AS LOGICAL INITIAL no 
     LABEL "Respons†vel" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE tg-enviar-telefon AS LOGICAL INITIAL no 
     LABEL "Telefonista" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .83 NO-UNDO.

DEFINE VARIABLE to-enviar-e-mail AS LOGICAL INITIAL no 
     LABEL "Envia e-mail" 
     VIEW-AS TOGGLE-BOX
     SIZE 12.72 BY 1.08 NO-UNDO.

DEFINE VARIABLE to-imp-param AS LOGICAL INITIAL yes 
     LABEL "Imprimir ParÉmetros/Seleá∆o" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .83 TOOLTIP "Imprimir ParÉmetros/Seleá∆o ao final do relat¢rio."
     FONT 1 NO-UNDO.

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

DEFINE VARIABLE fi-arq-cham-loc AS CHARACTER FORMAT "X(55)":U 
     LABEL "Chamadas Locais" 
     VIEW-AS FILL-IN 
     SIZE 36 BY .79 TOOLTIP "Caminho/Nome do arquivo de chamadas Locais" NO-UNDO.

DEFINE VARIABLE fi-arq-cham-nloc AS CHARACTER FORMAT "X(55)":U 
     LABEL "Chamadas N∆o Locais" 
     VIEW-AS FILL-IN 
     SIZE 36 BY .79 TOOLTIP "Caminho/Nome do arquivo de chamadas N∆o Locais" NO-UNDO.

DEFINE VARIABLE fi-arq-ramais AS CHARACTER FORMAT "X(55)":U 
     LABEL "Ramais" 
     VIEW-AS FILL-IN 
     SIZE 36 BY .79 TOOLTIP "Caminho/Nome do arquivo de Ramais" NO-UNDO.

DEFINE VARIABLE fi-arq-resumo AS CHARACTER FORMAT "X(55)":U 
     LABEL "Arquivo" 
     VIEW-AS FILL-IN 
     SIZE 36 BY .88 NO-UNDO.

DEFINE VARIABLE fi-arq-tmp-email AS CHARACTER FORMAT "X(55)":U 
     LABEL "Tempor†rio de E-mail" 
     VIEW-AS FILL-IN 
     SIZE 36 BY .79 TOOLTIP "Caminho/Nome do arquivo temporio de e-mail" NO-UNDO.

DEFINE VARIABLE fi-vlr-fat-loc AS DECIMAL FORMAT ">>>,>>9.99":U INITIAL 0 
     LABEL "Fatura - Locais" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 TOOLTIP "Valor da fatura de chamadas locais" NO-UNDO.

DEFINE VARIABLE fi-vlr-fat-nloc AS DECIMAL FORMAT ">>>,>>9.99":U INITIAL 0 
     LABEL "Fatura - N∆o Locais" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 TOOLTIP "Valor da fatura de chamadas n∆o locais" NO-UNDO.

DEFINE VARIABLE tg-gerar-resumo AS LOGICAL INITIAL no 
     LABEL "Gerar Resumo Mensal" 
     VIEW-AS TOGGLE-BOX
     SIZE 18.14 BY .88 TOOLTIP "Imprimir ParÉmetros/Seleá∆o ao final do relat¢rio."
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-fin-dt-chamada AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "Data da chamada final" NO-UNDO.

DEFINE VARIABLE fi-fin-ramal AS INTEGER FORMAT ">>9" INITIAL 999 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 TOOLTIP "Ramal final" NO-UNDO.

DEFINE VARIABLE fi-fin-responsavel AS CHARACTER FORMAT "X(20)":U INITIAL "ZZZZZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 21 BY .88 TOOLTIP "Nome do Respons†vel final" NO-UNDO.

DEFINE VARIABLE fi-ini-dt-chamada AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 
     LABEL "Data Chamada":R12 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "Data da chamada inicial" NO-UNDO.

DEFINE VARIABLE fi-ini-ramal AS INTEGER FORMAT ">>9" INITIAL 0 
     LABEL "Ramal":R16 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 TOOLTIP "Ramal inicial" NO-UNDO.

DEFINE VARIABLE fi-ini-responsavel AS CHARACTER FORMAT "X(20)":U 
     LABEL "Respons†vel" 
     VIEW-AS FILL-IN 
     SIZE 21 BY .88 TOOLTIP "Nome do Respons†vel inicial" NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-3
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-6
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-8
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-9
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

DEFINE IMAGE im-pg-cla
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


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-relat
     bt-executar AT ROW 14.54 COL 3 HELP
          "Dispara a execuá∆o do relat¢rio"
     bt-cancelar AT ROW 14.54 COL 14 HELP
          "Fechar"
     bt-ajuda AT ROW 14.54 COL 70 HELP
          "Ajuda"
     RECT-1 AT ROW 14.29 COL 2
     rt-folder-top AT ROW 2.54 COL 2.14
     rt-folder-right AT ROW 2.67 COL 80.43
     rt-folder-left AT ROW 2.54 COL 2.14
     rt-folder AT ROW 2.5 COL 2
     RECT-6 AT ROW 13.75 COL 2.14
     im-pg-cla AT ROW 1.5 COL 33.43
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
         SIZE 73.72 BY 10
         FONT 1.

DEFINE FRAME f-pg-sel
     fi-ini-responsavel AT ROW 1.79 COL 15.29 COLON-ALIGNED
     fi-fin-responsavel AT ROW 1.79 COL 46.72 COLON-ALIGNED NO-LABEL
     fi-ini-dt-chamada AT ROW 2.79 COL 15.29 COLON-ALIGNED HELP
          "Data prevista para entrega do Pedido"
     fi-fin-dt-chamada AT ROW 2.79 COL 46.72 COLON-ALIGNED HELP
          "Data prevista para entrega do Pedido" NO-LABEL
     fi-ini-ramal AT ROW 3.79 COL 15.29 COLON-ALIGNED HELP
          "C¢digo do representante"
     fi-fin-ramal AT ROW 3.79 COL 46.72 COLON-ALIGNED HELP
          "C¢digo do representante" NO-LABEL
     IMAGE-1 AT ROW 2.79 COL 38.72
     IMAGE-2 AT ROW 2.79 COL 45.14
     IMAGE-3 AT ROW 3.79 COL 38.72
     IMAGE-6 AT ROW 3.79 COL 45.14
     IMAGE-8 AT ROW 1.79 COL 38.72
     IMAGE-9 AT ROW 1.79 COL 45.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.85
         SIZE 76.86 BY 10.62
         FONT 1.

DEFINE FRAME f-pg-par
     fi-arq-cham-loc AT ROW 1.83 COL 18 COLON-ALIGNED
     fi-arq-cham-nloc AT ROW 2.75 COL 18 COLON-ALIGNED
     fi-arq-ramais AT ROW 3.75 COL 18 COLON-ALIGNED
     fi-arq-tmp-email AT ROW 4.75 COL 18 COLON-ALIGNED
     fi-vlr-fat-loc AT ROW 5.75 COL 18 COLON-ALIGNED
     fi-vlr-fat-nloc AT ROW 6.75 COL 18 COLON-ALIGNED
     tg-gerar-resumo AT ROW 7.75 COL 7.86
     fi-arq-resumo AT ROW 7.75 COL 32.72 COLON-ALIGNED
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 75 BY 10
         FONT 1.

DEFINE FRAME f-pg-cla
     to-imp-param AT ROW 1.46 COL 27
     to-enviar-e-mail AT ROW 2.5 COL 3.86
     tg-enviar-resp AT ROW 2.5 COL 17
     tg-enviar-telefon AT ROW 3.5 COL 17
     fi-email-telefon AT ROW 3.5 COL 27.72 COLON-ALIGNED NO-LABEL
     tg-enviar-info AT ROW 4.5 COL 17
     fi-email-info AT ROW 4.5 COL 27.72 COLON-ALIGNED NO-LABEL
     fi-assunto-e-mail AT ROW 5.5 COL 11 COLON-ALIGNED
     fi-texto-e-mail AT ROW 6.5 COL 13 NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 76.86 BY 10.31
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
         TITLE              = "Detalhamento de Ligaá‰es Telefìnicas"
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
/* SETTINGS FOR FRAME f-pg-cla
                                                                        */
/* SETTINGS FOR FILL-IN fi-assunto-e-mail IN FRAME f-pg-cla
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FILL-IN fi-email-info IN FRAME f-pg-cla
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-email-telefon IN FRAME f-pg-cla
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR fi-texto-e-mail IN FRAME f-pg-cla
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-enviar-info IN FRAME f-pg-cla
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-enviar-resp IN FRAME f-pg-cla
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-enviar-telefon IN FRAME f-pg-cla
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
/* SETTINGS FOR FILL-IN fi-arq-resumo IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME f-pg-sel
                                                                        */
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
ON END-ERROR OF w-relat /* Detalhamento de Ligaá‰es Telefìnicas */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON WINDOW-CLOSE OF w-relat /* Detalhamento de Ligaá‰es Telefìnicas */
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
&Scoped-define SELF-NAME fi-arq-resumo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-arq-resumo w-relat
ON LEAVE OF fi-arq-resumo IN FRAME f-pg-par /* Arquivo */
DO:
  IF NOT fi-arq-resumo:SCREEN-VALUE IN FRAME {&FRAME-NAME} MATCHES "*.csv*" THEN DO:
     MESSAGE "Nome do arquivo para Resumo est† inv†lido." SKIP
             "Deve ser do tipo Caminho/Arquivo.csv" VIEW-AS ALERT-BOX.
     ASSIGN fi-arq-resumo:SCREEN-VALUE IN FRAME f-pg-par = tt-param-ini.arq-resumo.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME im-pg-cla
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-cla w-relat
ON MOUSE-SELECT-CLICK OF im-pg-cla IN FRAME f-relat
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


&Scoped-define FRAME-NAME f-pg-cla
&Scoped-define SELF-NAME tg-enviar-info
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-enviar-info w-relat
ON VALUE-CHANGED OF tg-enviar-info IN FRAME f-pg-cla /* Inform†tica */
DO:
  IF INPUT FRAME {&frame-name} tg-enviar-info = YES THEN
     ASSIGN fi-email-info:SENSITIVE IN FRAME f-pg-cla = YES
            fi-email-info:SCREEN-VALUE IN FRAME f-pg-cla = tt-param-ini.email-info.
  ELSE 
     ASSIGN fi-email-info:SENSITIVE IN FRAME f-pg-cla = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-enviar-telefon
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-enviar-telefon w-relat
ON VALUE-CHANGED OF tg-enviar-telefon IN FRAME f-pg-cla /* Telefonista */
DO:
  IF INPUT FRAME {&frame-name} tg-enviar-telefon = YES THEN
     ASSIGN fi-email-telefon:SENSITIVE IN FRAME f-pg-cla = YES
            fi-email-telefon:SCREEN-VALUE IN FRAME f-pg-cla = tt-param-ini.email-telefon.
  ELSE 
     ASSIGN fi-email-telefon:SENSITIVE IN FRAME f-pg-cla = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME tg-gerar-resumo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-gerar-resumo w-relat
ON VALUE-CHANGED OF tg-gerar-resumo IN FRAME f-pg-par /* Gerar Resumo Mensal */
DO:
  IF INPUT FRAME {&FRAME-NAME} tg-gerar-resumo = NO THEN
     ASSIGN fi-arq-resumo:SENSITIVE IN FRAME {&FRAME-NAME} = NO
            fi-arq-resumo:SCREEN-VALUE IN FRAME f-pg-par = "".
  ELSE
     ASSIGN fi-arq-resumo:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            fi-arq-resumo:SCREEN-VALUE IN FRAME f-pg-par = tt-param-ini.arq-resumo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-cla
&Scoped-define SELF-NAME to-enviar-e-mail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL to-enviar-e-mail w-relat
ON VALUE-CHANGED OF to-enviar-e-mail IN FRAME f-pg-cla /* Envia e-mail */
DO:
  IF INPUT FRAME {&frame-name} to-enviar-e-mail = YES THEN DO:
     ASSIGN tg-enviar-resp:SENSITIVE IN FRAME f-pg-cla = YES   
            tg-enviar-telefon:SENSITIVE IN FRAME f-pg-cla = YES
            tg-enviar-info:SENSITIVE IN FRAME f-pg-cla = YES   
            fi-email-telefon:SENSITIVE IN FRAME f-pg-cla = YES
            fi-email-info:SENSITIVE IN FRAME f-pg-cla = YES   
            fi-assunto-e-mail:SENSITIVE IN FRAME f-pg-cla = YES 
            fi-texto-e-mail:SENSITIVE IN FRAME f-pg-cla = YES.
     
     ASSIGN tg-enviar-resp:SCREEN-VALUE IN FRAME f-pg-cla = "YES"    
            tg-enviar-telefon:SCREEN-VALUE IN FRAME f-pg-cla = "YES" 
            tg-enviar-info:SCREEN-VALUE IN FRAME f-pg-cla = "YES".

     ASSIGN fi-email-telefon:SCREEN-VALUE IN FRAME f-pg-cla = tt-param-ini.email-telefon
            fi-email-info:SCREEN-VALUE IN FRAME f-pg-cla = tt-param-ini.email-info
            fi-assunto-e-mail:SCREEN-VALUE IN FRAME f-pg-cla = "Detalhamento de Ligaá‰es Telefìnicas".

     ASSIGN fi-texto-e-mail:SCREEN-VALUE IN FRAME f-pg-cla = 
            "Segue anexo detalhamento das ligaá‰es telefìnicas efetuadas nos ramais" + CHR(13) +
            "sob sua responsabilidade, para seu conhecimento." + CHR(13) + CHR(13) + 
            "Atenciosamente," + CHR(13) + CHR(13) +
            "TEAR T“XTIL INDÈSTRIA E COMêRCIO LTDA." + CHR(13) + 
            "Gilvando Souza Araujo - Inform†tica." + CHR(13) +
            "(31) 2194-4250 - gilvando@teartextil.com.br". 
  END.
  ELSE DO.
     ASSIGN tg-enviar-resp:SENSITIVE IN FRAME f-pg-cla = NO
            tg-enviar-telefon:SENSITIVE IN FRAME f-pg-cla = NO 
            tg-enviar-info:SENSITIVE IN FRAME f-pg-cla = NO 
            fi-email-telefon:SENSITIVE IN FRAME f-pg-cla = NO 
            fi-email-info:SENSITIVE IN FRAME f-pg-cla = NO
            fi-assunto-e-mail:SENSITIVE IN FRAME f-pg-cla = NO 
            fi-texto-e-mail:SENSITIVE IN FRAME f-pg-cla = NO.

     ASSIGN fi-email-telefon:SCREEN-VALUE IN FRAME f-pg-cla = ""
            fi-email-info:SCREEN-VALUE IN FRAME f-pg-cla = ""
            fi-assunto-e-mail:SCREEN-VALUE IN FRAME f-pg-cla = ""
            fi-texto-e-mail:SCREEN-VALUE IN FRAME f-pg-cla = "".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-relat 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{utp/ut9000.i "ESSP0112" "2.04.00.000"}

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
  
    {include/i-rpmbl.i}
    {esinc/i-chlbl.i "im-pg-cla" "ParÉmetros 2"}
    {include/i-rpmbl.i}
  
    /* Carrega ParÉmetros */
    IF SEARCH("especificos/Embratel/Parametros.csv") = ? AND 
       SEARCH("c:/Embratel/Parametros.csv") = ? THEN DO:
       MESSAGE "Prezado Usu†rio:" SKIP(2)
               "N∆o foi possivel encontrar o arquivo de parÉmetros:" SKIP
               "Parametros.csv."
               VIEW-AS ALERT-BOX INFO.
       QUIT.
    END.
    IF SEARCH("especificos/Embratel/Parametros.csv") <> ? THEN
       INPUT FROM "especificos/Embratel/Parametros.csv" CONVERT SOURCE "ibm850".
    ELSE
       INPUT FROM "c:/Embratel/Parametros.csv" CONVERT SOURCE "ibm850".
    SET ^.
    REPEAT:
       CREATE tt-param-ini.
       IMPORT DELIMITER ";" tt-param-ini.
    END.
    INPUT CLOSE.

    FIND FIRST tt-param-ini WHERE tt-param-ini.dt-chamada-ini <> "" NO-LOCK.
    ASSIGN fi-ini-responsavel:SCREEN-VALUE IN FRAME f-pg-sel = ""
           fi-fin-responsavel:SCREEN-VALUE IN FRAME f-pg-sel = "ZZZZZZZZZZZZZZZZZZZZ"
           fi-ini-dt-chamada:SCREEN-VALUE IN FRAME f-pg-sel  = tt-param-ini.dt-chamada-ini
           fi-fin-dt-chamada:SCREEN-VALUE IN FRAME f-pg-sel  = tt-param-ini.dt-chamada-fin
           fi-ini-ramal:SCREEN-VALUE IN FRAME f-pg-sel       = "0"
           fi-fin-ramal:SCREEN-VALUE IN FRAME f-pg-sel       = "999"
           fi-arq-cham-loc:SCREEN-VALUE IN FRAME f-pg-par    = tt-param-ini.arq-cham-loc
           fi-arq-cham-nloc:SCREEN-VALUE IN FRAME f-pg-par   = tt-param-ini.arq-cham-nloc
           fi-arq-ramais:SCREEN-VALUE IN FRAME f-pg-par      = tt-param-ini.arq-ramais
           fi-arq-tmp-email:SCREEN-VALUE IN FRAME f-pg-par   = tt-param-ini.arq-tmp-email
           fi-vlr-fat-loc:SCREEN-VALUE IN FRAME f-pg-par     = STRING(tt-param-ini.vlr-fat-loc)
           fi-vlr-fat-nloc:SCREEN-VALUE IN FRAME f-pg-par    = STRING(tt-param-ini.vlr-fat-nloc)
           tg-gerar-resumo:SCREEN-VALUE IN FRAME f-pg-par    = 
                                        IF tt-param-ini.gerar-resumo = "S" THEN "Yes" ELSE "No"
           fi-arq-resumo:SCREEN-VALUE IN FRAME f-pg-par      = "".

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
  ENABLE bt-executar bt-cancelar bt-ajuda im-pg-cla im-pg-imp im-pg-par 
         im-pg-sel 
      WITH FRAME f-relat IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}
  DISPLAY fi-ini-responsavel fi-fin-responsavel fi-ini-dt-chamada 
          fi-fin-dt-chamada fi-ini-ramal fi-fin-ramal 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  ENABLE fi-ini-responsavel fi-fin-responsavel fi-ini-dt-chamada 
         fi-fin-dt-chamada fi-ini-ramal fi-fin-ramal IMAGE-1 IMAGE-2 IMAGE-3 
         IMAGE-6 IMAGE-8 IMAGE-9 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}
  DISPLAY to-imp-param to-enviar-e-mail tg-enviar-resp tg-enviar-telefon 
          fi-email-telefon tg-enviar-info fi-email-info fi-texto-e-mail 
      WITH FRAME f-pg-cla IN WINDOW w-relat.
  ENABLE to-imp-param to-enviar-e-mail 
      WITH FRAME f-pg-cla IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-cla}
  DISPLAY rs-destino c-arquivo rs-execucao 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  ENABLE rs-destino bt-arquivo bt-config-impr c-arquivo rs-execucao RECT-7 
         RECT-9 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}
  DISPLAY fi-arq-cham-loc fi-arq-cham-nloc fi-arq-ramais fi-arq-tmp-email 
          fi-vlr-fat-loc fi-vlr-fat-nloc tg-gerar-resumo fi-arq-resumo 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  ENABLE fi-arq-cham-loc fi-arq-cham-nloc fi-arq-ramais fi-arq-tmp-email 
         fi-vlr-fat-loc fi-vlr-fat-nloc tg-gerar-resumo 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-par}
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
    
    /*:T Aqui s∆o gravados os campos da temp-table que ser† passada como parÉmetro
       para o programa RP.P */
    
    create tt-param.
    assign tt-param.usuario          = c-seg-usuario
           tt-param.destino          = input frame f-pg-imp rs-destino
           tt-param.data-exec        = today
           tt-param.hora-exec        = TIME
           tt-param.ini-responsavel  = INPUT FRAME f-pg-sel fi-ini-responsavel
           tt-param.fin-responsavel  = INPUT FRAME f-pg-sel fi-fin-responsavel          
           tt-param.ini-dt-chamada   = INPUT FRAME f-pg-sel fi-ini-dt-chamada        
           tt-param.fin-dt-chamada   = INPUT FRAME f-pg-sel fi-fin-dt-chamada              
           tt-param.ini-ramal        = int(INPUT FRAME f-pg-sel fi-ini-ramal)      
           tt-param.fin-ramal        = int(INPUT FRAME f-pg-sel fi-fin-ramal)      
           tt-param.arq-cham-loc     = INPUT FRAME f-pg-par fi-arq-cham-loc
           tt-param.arq-cham-nloc    = INPUT FRAME f-pg-par fi-arq-cham-nloc  
           tt-param.arq-ramais       = INPUT FRAME f-pg-par fi-arq-ramais     
           tt-param.arq-tmp-email    = INPUT FRAME f-pg-par fi-arq-tmp-email  
           tt-param.vlr-fat-loc      = dec(INPUT FRAME f-pg-par fi-vlr-fat-loc)
           tt-param.vlr-fat-nloc     = dec(INPUT FRAME f-pg-par fi-vlr-fat-nloc)
           tt-param.gerar-resumo     = INPUT FRAME f-pg-par tg-gerar-resumo 
           tt-param.arq-resumo       = INPUT FRAME f-pg-par fi-arq-resumo
           tt-param.enviar-e-mail    = INPUT FRAME f-pg-cla to-enviar-e-mail
           tt-param.enviar-resp      = INPUT FRAME f-pg-cla tg-enviar-resp
           tt-param.enviar-telefon   = INPUT FRAME f-pg-cla tg-enviar-telefon
           tt-param.enviar-info      = INPUT FRAME f-pg-cla tg-enviar-info
           tt-param.email-telefon    = INPUT FRAME f-pg-cla fi-email-telefon
           tt-param.email-info       = INPUT FRAME f-pg-cla fi-email-info
           tt-param.subject-e-mail   = INPUT FRAME f-pg-cla fi-assunto-e-mail
           tt-param.texto-e-mail     = INPUT FRAME f-pg-cla fi-texto-e-mail
           tt-param.imp-param        = INPUT FRAME f-pg-cla to-imp-param.
    
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
    
    /*{include/i-rprun.i esrp/essp0112rp.p}*/
    {include/i-rprun.i esrp/essp0112rp.p}

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

