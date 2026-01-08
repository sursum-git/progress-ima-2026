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
{include/i-prgvrs.i XX9999 9.99.99.999}

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
       FIELD c-cod-estabel    LIKE estabelec.cod-estabel
       FIELD c-item-ini       LIKE ITEM.it-codigo
       FIELD c-item-fim       LIKE ITEM.it-codigo
       FIELD c-ref-ini        LIKE ref-item.cod-refer
       FIELD c-ref-fim        LIKE ref-item.cod-refer
       FIELD c-acond-ini      AS CHAR FORMAT "x(5)" 
       FIELD c-acond-fim      AS CHAR FORMAT "x(5)" 
       FIELD c-lote-ini       AS CHAR FORMAT "x(2)" 
       FIELD c-lote-fim       AS CHAR FORMAT "x(2)" 
       FIELD c-obso-ini       LIKE item-ext.cod-obsoleto
       FIELD c-obso-fim       LIKE item-ext.cod-obsoleto
       FIELD c-estoq-bloq     AS CHARACTER
       FIELD c-opcao-list     AS CHARACTER FORMAT "x(9)"
       FIELD mascara-item     AS CHAR FORMAT "x(6)"
       FIELD mascara-refer    AS CHAR FORMAT "x(7)"
       FIELD l-lista-teccru   AS LOGICAL
       FIELD impr-param       AS LOGICAL.

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
&Scoped-define FRAME-NAME f-pg-imp

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rs-destino bt-arquivo bt-config-impr ~
c-arquivo rs-execucao RECT-17 RECT-7 RECT-9 
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

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 77 BY 10.67.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 46.29 BY 2.92.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 46.29 BY 1.71.

DEFINE VARIABLE fi-masc-item AS CHARACTER FORMAT "X(6)":U INITIAL "******" 
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 TOOLTIP "Posiá‰es com * n∆o ser∆o filtradas." NO-UNDO.

DEFINE VARIABLE fi-masc-refer AS CHARACTER FORMAT "X(7)":U INITIAL "*******" 
     LABEL "Referància" 
     VIEW-AS FILL-IN 
     SIZE 10.57 BY .88 TOOLTIP "Posiá‰es com * n∆o ser∆o filtradas." NO-UNDO.

DEFINE VARIABLE rs-estoq-bloq AS CHARACTER INITIAL "A" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Bloqueado", "B",
"Liberado", "L",
"Ambos", "A"
     SIZE 34.86 BY .75 TOOLTIP "Incluir estoque bloqueado: Inclusive, Exclusive ou Ambos." NO-UNDO.

DEFINE VARIABLE rs-saldo AS INTEGER INITIAL 3 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Positivos", 1,
"Negativos", 2,
"Ambos", 3
     SIZE 10.14 BY 3.54 NO-UNDO.

DEFINE RECTANGLE RECT-15
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 77 BY 10.67.

DEFINE RECTANGLE RECT-26
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 35 BY 4.25.

DEFINE RECTANGLE RECT-27
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 73 BY 2.92.

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 73 BY 1.5.

DEFINE RECTANGLE RECT-29
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 35 BY 4.25.

DEFINE VARIABLE tg-impr-param AS LOGICAL INITIAL no 
     LABEL "Imprime ParÉmetros" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .83 NO-UNDO.

DEFINE VARIABLE tg-lista-teccru AS LOGICAL INITIAL no 
     LABEL "Itens Tecido CRU (Posiá∆o 6 = 0)" 
     VIEW-AS TOGGLE-BOX
     SIZE 34.14 BY .83 NO-UNDO.

DEFINE VARIABLE fi-cod-estabel AS CHARACTER FORMAT "X(3)":U 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fim-acond AS CHARACTER FORMAT "x(4)" INITIAL "ZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fim-cod-obsoleto AS CHARACTER FORMAT "x(1)" INITIAL "4" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 TOOLTIP "C¢dgo obsoleto final" NO-UNDO.

DEFINE VARIABLE fi-fim-cod-refer AS CHARACTER FORMAT "x(8)" INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fim-it-codigo AS CHARACTER FORMAT "x(16)" INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 17.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fim-lote AS CHARACTER FORMAT "x(2)" INITIAL "ZZ" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-acond AS CHARACTER FORMAT "x(4)" 
     LABEL "Acondicionamento":R5 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-cod-obsoleto AS CHARACTER FORMAT "x(1)" INITIAL "0" 
     LABEL "Cod.Obsoleto" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 TOOLTIP "C¢dgo obsoleto inicial" NO-UNDO.

DEFINE VARIABLE fi-ini-cod-refer AS CHARACTER FORMAT "x(8)" 
     LABEL "Referància":R5 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-it-codigo AS CHARACTER FORMAT "x(16)" 
     LABEL "Item":R5 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-lote AS CHARACTER FORMAT "x(2)" 
     LABEL "Lote":R5 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nome-estabel AS CHARACTER FORMAT "X(256)":U 
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
     RECT-17 AT ROW 1.08 COL 1.29
     RECT-7 AT ROW 1.92 COL 2.14
     RECT-9 AT ROW 5.29 COL 2.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2.43 ROW 2.83
         SIZE 77.72 BY 10.83.

DEFINE FRAME f-relat
     bt-executar AT ROW 14.54 COL 3 HELP
          "Dispara a execuá∆o do relat¢rio"
     bt-cancelar AT ROW 14.54 COL 14 HELP
          "Fechar"
     bt-ajuda AT ROW 14.54 COL 70 HELP
          "Ajuda"
     RECT-6 AT ROW 13.75 COL 2.14
     rt-folder-right AT ROW 2.67 COL 80.43
     rt-folder-left AT ROW 2.54 COL 2.14
     rt-folder AT ROW 2.5 COL 2
     rt-folder-top AT ROW 2.54 COL 2.14
     RECT-1 AT ROW 14.29 COL 2
     im-pg-imp AT ROW 1.5 COL 33.43
     im-pg-par AT ROW 1.5 COL 17.72
     im-pg-sel AT ROW 1.5 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81 BY 15
         DEFAULT-BUTTON bt-executar.

DEFINE FRAME f-pg-par
     rs-estoq-bloq AT ROW 1.88 COL 29.72 NO-LABEL
     rs-saldo AT ROW 3.79 COL 13.86 NO-LABEL
     fi-masc-item AT ROW 4.5 COL 54.43 COLON-ALIGNED
     fi-masc-refer AT ROW 5.58 COL 54.43 COLON-ALIGNED
     tg-lista-teccru AT ROW 8.63 COL 14.72
     tg-impr-param AT ROW 9.71 COL 14.86
     RECT-15 AT ROW 1.08 COL 1
     RECT-26 AT ROW 3.5 COL 3
     RECT-27 AT ROW 8.13 COL 3
     RECT-28 AT ROW 1.5 COL 3
     RECT-29 AT ROW 3.5 COL 41
     " Saldo dos Itens" VIEW-AS TEXT
          SIZE 12.14 BY .71 AT ROW 3.13 COL 5.57
     " Outros" VIEW-AS TEXT
          SIZE 5.43 BY .75 AT ROW 7.79 COL 6.29
     "Estoque para Venda:" VIEW-AS TEXT
          SIZE 14.72 BY .54 AT ROW 1.88 COL 15
     "M†scara para Filtro" VIEW-AS TEXT
          SIZE 13.43 BY .71 AT ROW 3.13 COL 44.57
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2.72 ROW 2.83
         SIZE 77.29 BY 10.79
         FONT 1.

DEFINE FRAME f-pg-sel
     fi-cod-estabel AT ROW 2 COL 18 COLON-ALIGNED
     fi-nome-estabel AT ROW 2 COL 23.72 COLON-ALIGNED NO-LABEL
     fi-ini-it-codigo AT ROW 3 COL 18 COLON-ALIGNED HELP
          "C¢digo do Item"
     fi-fim-it-codigo AT ROW 3 COL 49 COLON-ALIGNED HELP
          "C¢digo do Item" NO-LABEL
     fi-ini-cod-refer AT ROW 4 COL 18 COLON-ALIGNED HELP
          "C¢digo da Referencia"
     fi-fim-cod-refer AT ROW 4 COL 48.86 COLON-ALIGNED HELP
          "C¢digo da Referencia" NO-LABEL
     fi-ini-acond AT ROW 5 COL 18 COLON-ALIGNED HELP
          "C¢digo do Acondicionamento"
     fi-fim-acond AT ROW 5 COL 48.86 COLON-ALIGNED HELP
          "C¢digo do Acondicionamento" NO-LABEL
     fi-ini-lote AT ROW 6 COL 18 COLON-ALIGNED HELP
          "C¢digo do Lote"
     fi-fim-lote AT ROW 6 COL 48.86 COLON-ALIGNED HELP
          "C¢digo do Lote" NO-LABEL
     fi-ini-cod-obsoleto AT ROW 7.08 COL 18 COLON-ALIGNED HELP
          "Codigo obsoleto"
     fi-fim-cod-obsoleto AT ROW 7.08 COL 49 COLON-ALIGNED HELP
          "Codigo obsoleto" NO-LABEL
     IMAGE-1 AT ROW 3 COL 38.43
     IMAGE-10 AT ROW 7.08 COL 47.14
     IMAGE-2 AT ROW 3 COL 47.14
     IMAGE-3 AT ROW 4.08 COL 38.43
     IMAGE-4 AT ROW 4.08 COL 47.14
     IMAGE-5 AT ROW 5.08 COL 38.43
     IMAGE-6 AT ROW 5.08 COL 47.14
     IMAGE-7 AT ROW 6.08 COL 38.43
     IMAGE-8 AT ROW 6.08 COL 47.14
     IMAGE-9 AT ROW 7.08 COL 38.43
     RECT-16 AT ROW 1.04 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2.72 ROW 2.83
         SIZE 77.43 BY 10.83
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
         TITLE              = "Estoque Atual / Dispon°vel por Item"
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
/* SETTINGS FOR FRAME f-pg-sel
                                                                        */
/* SETTINGS FOR FILL-IN fi-nome-estabel IN FRAME f-pg-sel
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
ON END-ERROR OF w-relat /* Estoque Atual / Dispon°vel por Item */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON WINDOW-CLOSE OF w-relat /* Estoque Atual / Dispon°vel por Item */
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


&Scoped-define FRAME-NAME f-pg-sel
&Scoped-define SELF-NAME fi-cod-estabel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-estabel w-relat
ON ENTRY OF fi-cod-estabel IN FRAME f-pg-sel /* Estabelecimento */
DO:
  IF SELF:SCREEN-VALUE <> "" THEN DO.
     FIND estabelec WHERE
          estabelec.cod-estabel = INPUT FRAME {&FRAME-NAME} fi-cod-estabel
          NO-LOCK NO-ERROR.
     IF AVAIL estabelec THEN
        ASSIGN fi-nome-estabel:SCREEN-VALUE IN FRAME {&FRAME-NAME} = estabelec.nome.
  END.
  ELSE ASSIGN fi-nome-estabel:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-estabel w-relat
ON LEAVE OF fi-cod-estabel IN FRAME f-pg-sel /* Estabelecimento */
DO:
  FIND estabelec WHERE
       estabelec.cod-estabel = INPUT FRAME {&FRAME-NAME} fi-cod-estabel NO-LOCK NO-ERROR.

  IF NOT AVAIL estabelec THEN DO.
     MESSAGE "Estabelecimento n∆o Cadastrado..." VIEW-AS ALERT-BOX.
     RETURN NO-APPLY.
  END.
  ASSIGN fi-nome-estabel:SCREEN-VALUE IN FRAME {&FRAME-NAME} = estabelec.nome.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-estabel w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-cod-estabel IN FRAME f-pg-sel /* Estabelecimento */
DO:
  {include/zoomvar.i &prog-zoom=adzoom/z01ad107.w
                     &campo=fi-cod-estabel
                     &campozoom=cod-estabel
                     &FRAME=f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-fim-acond
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fim-acond w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-fim-acond IN FRAME f-pg-sel
DO:
    {include/zoomvar.i &prog-zoom=inzoom/z01in172.w
                   &campo=fi-fim-it-codigo
                   &campozoom=it-codigo
                   &FRAME=f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-fim-cod-refer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fim-cod-refer w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-fim-cod-refer IN FRAME f-pg-sel
DO:
    {include/zoomvar.i &prog-zoom=inzoom/z01in172.w
                   &campo=fi-fim-it-codigo
                   &campozoom=it-codigo
                   &FRAME=f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-fim-it-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fim-it-codigo w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-fim-it-codigo IN FRAME f-pg-sel
DO:
    {include/zoomvar.i &prog-zoom=inzoom/z01in172.w
                   &campo=fi-fim-it-codigo
                   &campozoom=it-codigo
                   &FRAME=f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-fim-lote
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fim-lote w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-fim-lote IN FRAME f-pg-sel
DO:
    {include/zoomvar.i &prog-zoom=inzoom/z01in172.w
                   &campo=fi-fim-it-codigo
                   &campozoom=it-codigo
                   &FRAME=f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ini-acond
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-acond w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ini-acond IN FRAME f-pg-sel /* Acondicionamento */
DO:
    {include/zoomvar.i &prog-zoom=inzoom/z01in172.w
                     &campo=fi-ini-it-codigo
                     &campozoom=it-codigo
                     &FRAME=f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ini-cod-refer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-cod-refer w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ini-cod-refer IN FRAME f-pg-sel /* Referància */
DO:
    {include/zoomvar.i &prog-zoom=inzoom/z01in172.w
                     &campo=fi-ini-it-codigo
                     &campozoom=it-codigo
                     &FRAME=f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ini-it-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-it-codigo w-relat
ON LEAVE OF fi-ini-it-codigo IN FRAME f-pg-sel /* Item */
DO:
  ASSIGN fi-fim-it-codigo:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-it-codigo w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ini-it-codigo IN FRAME f-pg-sel /* Item */
DO:
    {include/zoomvar.i &prog-zoom=inzoom/z01in172.w
                     &campo=fi-ini-it-codigo
                     &campozoom=it-codigo
                     &FRAME=f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ini-lote
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-lote w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ini-lote IN FRAME f-pg-sel /* Lote */
DO:
    {include/zoomvar.i &prog-zoom=inzoom/z01in172.w
                     &campo=fi-ini-it-codigo
                     &campozoom=it-codigo
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


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-relat 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{utp/ut9000.i "ESCE0005" "2.00.04.001"} 

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
  
    ASSIGN fi-cod-estabel:SCREEN-VALUE IN FRAME f-pg-sel = "2".
    
    {include/i-rpmbl.i}

    fi-ini-it-codigo:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-fim-it-codigo:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-cod-estabel:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
  
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
  ENABLE bt-executar bt-cancelar bt-ajuda im-pg-imp im-pg-par im-pg-sel 
      WITH FRAME f-relat IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}
  DISPLAY rs-destino c-arquivo rs-execucao 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  ENABLE rs-destino bt-arquivo bt-config-impr c-arquivo rs-execucao RECT-17 
         RECT-7 RECT-9 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}
  DISPLAY rs-estoq-bloq rs-saldo fi-masc-item fi-masc-refer tg-lista-teccru 
          tg-impr-param 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  ENABLE rs-estoq-bloq rs-saldo fi-masc-item fi-masc-refer tg-lista-teccru 
         tg-impr-param RECT-15 RECT-26 RECT-27 RECT-28 RECT-29 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-par}
  DISPLAY fi-cod-estabel fi-nome-estabel fi-ini-it-codigo fi-fim-it-codigo 
          fi-ini-cod-refer fi-fim-cod-refer fi-ini-acond fi-fim-acond 
          fi-ini-lote fi-fim-lote fi-ini-cod-obsoleto fi-fim-cod-obsoleto 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  ENABLE fi-cod-estabel fi-ini-it-codigo fi-fim-it-codigo fi-ini-cod-refer 
         fi-fim-cod-refer fi-ini-acond fi-fim-acond fi-ini-lote fi-fim-lote 
         fi-ini-cod-obsoleto fi-fim-cod-obsoleto IMAGE-1 IMAGE-10 IMAGE-2 
         IMAGE-3 IMAGE-4 IMAGE-5 IMAGE-6 IMAGE-7 IMAGE-8 IMAGE-9 RECT-16 
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
    
    /*:T Coloque aqui as validaá‰es da p†gina de Digitaá∆o, lembrando que elas devem
       apresentar uma mensagem de erro cadastrada, posicionar nesta p†gina e colocar
       o focus no campo com problemas */
    /*browse br-digita:SET-REPOSITIONED-ROW (browse br-digita:DOWN, "ALWAYS":U).*/
    
    
    /*:T Coloque aqui as validaá‰es das outras p†ginas, lembrando que elas devem 
       apresentar uma mensagem de erro cadastrada, posicionar na p†gina com 
       problemas e colocar o focus no campo com problemas */
    
    
    
    /*:T Aqui s∆o gravados os campos da temp-table que ser† passada como parÉmetro
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
    
    /*:T Coloque aqui a l¢gica de gravaá∆o dos demais campos que devem ser passados
       como parÉmetros para o programa RP.P, atravÇs da temp-table tt-param */
    ASSIGN tt-param.c-cod-estabel  = INPUT FRAME f-pg-sel fi-cod-estabel
           tt-param.c-item-ini     = INPUT FRAME f-pg-sel fi-ini-it-codigo
           tt-param.c-item-fim     = INPUT FRAME f-pg-sel fi-fim-it-codigo
           tt-param.c-ref-ini      = INPUT FRAME f-pg-sel fi-ini-cod-refer
           tt-param.c-ref-fim      = INPUT FRAME f-pg-sel fi-fim-cod-refer
           tt-param.c-acond-ini    = INPUT FRAME f-pg-sel fi-ini-acond
           tt-param.c-acond-fim    = INPUT FRAME f-pg-sel fi-fim-acond
           tt-param.c-lote-ini     = INPUT FRAME f-pg-sel fi-ini-lote
           tt-param.c-lote-fim     = INPUT FRAME f-pg-sel fi-fim-lote
           tt-param.c-obso-ini     = INPUT FRAME f-pg-sel fi-ini-cod-obsoleto
           tt-param.c-obso-fim     = INPUT FRAME f-pg-sel fi-fim-cod-obsoleto
           tt-param.c-estoq-bloq   = INPUT FRAME f-pg-par rs-estoq-bloq
           tt-param.mascara-item   = INPUT FRAME f-pg-par fi-masc-item
           tt-param.mascara-refer  = INPUT FRAME f-pg-par fi-masc-refer
           tt-param.l-lista-teccru = INPUT FRAME f-pg-par tg-lista-teccru
           tt-param.impr-param     = INPUT FRAME f-pg-par tg-impr-param.
           
           {esinc/i-dsrb.i rs-saldo "INPUT FRAME f-pg-par rs-saldo" tt-param.c-opcao-list}. 

    /*:T Executar do programa RP.P que ir† criar o relat¢rio */
    {include/i-rpexb.i}
    
    SESSION:SET-WAIT-STATE("general":U).
    
    {include/i-rprun.i esrp/esce0005rp.p} 
    
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

