&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME w-relat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-relat 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESPD0005 2.04.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Preprocessadores do Template de Relat¢rio                            */
/* Obs: Retirar o valor do preprocessador para as p†ginas que n∆o existirem  */

&GLOBAL-DEFINE PGSEL f-pg-sel
&GLOBAL-DEFINE PGCLA 
&GLOBAL-DEFINE PGPAR f-pg-par
&GLOBAL-DEFINE PGDIG 
&GLOBAL-DEFINE PGIMP f-pg-imp
  
/* Parameters Definitions ---                                           */

/* Temporary Table Definitions ---                                      */

DEFINE TEMP-TABLE tt-param  NO-UNDO
       FIELD destino           AS INTEGER
       FIELD arquivo           AS CHAR FORMAT "x(35)"
       FIELD usuario           AS CHAR FORMAT "x(12)"
       FIELD data-exec         AS DATE
       FIELD hora-exec         AS INTEGER
       FIELD classifica        AS INTEGER
       FIELD desc-classifica   AS CHAR FORMAT "x(40)"
       FIELD ini-dt-entrega    LIKE ped-venda.dt-entrega
       FIELD fin-dt-entrega    LIKE ped-venda.dt-entrega
       FIELD ini-dt-precoant   LIKE ped-venda.dt-entrega
       FIELD fin-dt-precoant   LIKE ped-venda.dt-entrega
       FIELD ini-it-codigo     LIKE ITEM.it-codigo
       FIELD fin-it-codigo     LIKE ITEM.it-codigo
       FIELD ini-ge-codigo     LIKE ITEM.ge-codigo
       FIELD fin-ge-codigo     LIKE ITEM.ge-codigo
       FIELD ini-nome-repres   LIKE ped-venda.no-ab-reppri
       FIELD fin-nome-repres   LIKE ped-venda.no-ab-reppri
       FIELD emite-dup         AS LOG FORMAT "Sim/N∆o"
       FIELD imp-param         AS LOG.

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
DEF VAR da-aux             AS DATE.

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
c-arquivo rs-execucao RECT-7 RECT-9 
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

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 46.29 BY 2.92.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 46.29 BY 1.71.

DEFINE RECTANGLE RECT-27
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 67 BY 1.5.

DEFINE VARIABLE tg-emite-dup AS LOGICAL INITIAL yes 
     LABEL "Emite Duplicata" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.57 BY .83 TOOLTIP "Considerar apenas os Pedidos cuja Natureza emita duplicata."
     FONT 1 NO-UNDO.

DEFINE VARIABLE tg-imp-param AS LOGICAL INITIAL no 
     LABEL "Imprimir ParÉmetros/Seleá∆o" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .83 TOOLTIP "Imprimir ParÉmetros/Seleá∆o ao final do relat¢rio."
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-fin-dt-entrega AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 TOOLTIP "Data de entrega final" NO-UNDO.

DEFINE VARIABLE fi-fin-dt-precoant AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 TOOLTIP "Data de preco anterior final" NO-UNDO.

DEFINE VARIABLE fi-fin-ge-codigo AS INTEGER FORMAT ">9" INITIAL 99 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 TOOLTIP "Grupo de estoque final" NO-UNDO.

DEFINE VARIABLE fi-fin-it-codigo AS CHARACTER FORMAT "x(16)" INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 TOOLTIP "Familia final" NO-UNDO.

DEFINE VARIABLE fi-fin-nome-repres AS CHARACTER FORMAT "x(12)" INITIAL "ZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 TOOLTIP "Representante final" NO-UNDO.

DEFINE VARIABLE fi-ini-dt-entrega AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 
     LABEL "Data Entrega":R12 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 TOOLTIP "Data de entrega inicial" NO-UNDO.

DEFINE VARIABLE fi-ini-dt-precoant AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Data Preáo anterior" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 TOOLTIP "Data de preco anterior inicial" NO-UNDO.

DEFINE VARIABLE fi-ini-ge-codigo AS INTEGER FORMAT ">9" INITIAL 0 
     LABEL "Grupo Estoque":R16 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 TOOLTIP "Grupo de estoque inicial" NO-UNDO.

DEFINE VARIABLE fi-ini-it-codigo AS CHARACTER FORMAT "x(16)" 
     LABEL "Item":R9 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 TOOLTIP "Familia inicial" NO-UNDO.

DEFINE VARIABLE fi-ini-nome-repres AS CHARACTER FORMAT "X(12)":U 
     LABEL "Representante" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 TOOLTIP "Representante inicial" NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-13
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-14
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-3
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-4
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-5
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-6
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-7
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-8
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE BUTTON bt-ajuda 
     LABEL "Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "Fechar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-executar 
     LABEL "Executar" 
     SIZE 10 BY 1.

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
     RECT-7 AT ROW 1.92 COL 2.14
     RECT-9 AT ROW 5.29 COL 2.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 73.72 BY 10.

DEFINE FRAME f-relat
     bt-executar AT ROW 14.54 COL 3 HELP
          "Dispara a execuá∆o do relat¢rio"
     bt-ajuda AT ROW 14.54 COL 70 HELP
          "Ajuda"
     bt-cancelar AT ROW 14.58 COL 14 HELP
          "Fechar"
     rt-folder-left AT ROW 2.54 COL 2.14
     RECT-6 AT ROW 13.75 COL 2.14
     rt-folder-top AT ROW 2.54 COL 2.14
     rt-folder-right AT ROW 2.67 COL 80.43
     RECT-1 AT ROW 14.29 COL 2
     rt-folder AT ROW 2.54 COL 2
     im-pg-imp AT ROW 1.5 COL 33.57
     im-pg-par AT ROW 1.5 COL 17.86
     im-pg-sel AT ROW 1.5 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81 BY 15
         DEFAULT-BUTTON bt-executar.

DEFINE FRAME f-pg-sel
     fi-ini-dt-entrega AT ROW 1.54 COL 15.29 COLON-ALIGNED HELP
          "Data prevista para entrega do Pedido"
     fi-fin-dt-entrega AT ROW 1.54 COL 48.29 COLON-ALIGNED HELP
          "Data prevista para entrega do Pedido" NO-LABEL
     fi-ini-dt-precoant AT ROW 2.54 COL 15.29 COLON-ALIGNED
     fi-fin-dt-precoant AT ROW 2.54 COL 48.29 COLON-ALIGNED NO-LABEL
     fi-ini-it-codigo AT ROW 3.54 COL 15.29 COLON-ALIGNED HELP
          "C¢digo da Fam°lia de Material"
     fi-fin-it-codigo AT ROW 3.54 COL 48.29 COLON-ALIGNED HELP
          "C¢digo da Fam°lia de Material" NO-LABEL
     fi-ini-ge-codigo AT ROW 4.54 COL 15.29 COLON-ALIGNED HELP
          "Grupo de estoque a que pertence o item"
     fi-fin-ge-codigo AT ROW 4.54 COL 48.29 COLON-ALIGNED HELP
          "Grupo de estoque a que pertence o item" NO-LABEL
     fi-ini-nome-repres AT ROW 5.54 COL 15.29 COLON-ALIGNED
     fi-fin-nome-repres AT ROW 5.54 COL 48.29 COLON-ALIGNED HELP
          "Nome abreviado do representante principal" NO-LABEL
     IMAGE-1 AT ROW 1.54 COL 31.86
     IMAGE-13 AT ROW 5.54 COL 31.86
     IMAGE-14 AT ROW 5.54 COL 46.72
     IMAGE-2 AT ROW 1.54 COL 46.72
     IMAGE-3 AT ROW 2.54 COL 31.86
     IMAGE-4 AT ROW 3.54 COL 31.86
     IMAGE-5 AT ROW 4.54 COL 31.86
     IMAGE-6 AT ROW 2.54 COL 46.72
     IMAGE-7 AT ROW 3.54 COL 46.72
     IMAGE-8 AT ROW 4.54 COL 46.72
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 4 ROW 3.21
         SIZE 75.86 BY 10.25
         FONT 1.

DEFINE FRAME f-pg-par
     tg-imp-param AT ROW 2 COL 44.29
     tg-emite-dup AT ROW 2 COL 13.43
     RECT-27 AT ROW 1.71 COL 6.29
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 77.14 BY 10.5
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
         TITLE              = "Resumo de Pedidos de Vendas em Carteira"
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
   Custom                                                               */
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
ON END-ERROR OF w-relat /* Resumo de Pedidos de Vendas em Carteira */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON WINDOW-CLOSE OF w-relat /* Resumo de Pedidos de Vendas em Carteira */
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
   apply "close" to this-procedure.
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
&Scoped-define SELF-NAME fi-fin-ge-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fin-ge-codigo w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-fin-ge-codigo IN FRAME f-pg-sel
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z01in142.w
                     &campo     = fi-fin-ge-codigo
                     &campozoom = ge-codigo
                     &FRAME     = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-fin-it-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fin-it-codigo w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-fin-it-codigo IN FRAME f-pg-sel
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z01in122.w
                     &campo     = fi-fin-it-codigo
                     &campozoom = it-codigo
                     &FRAME     = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-fin-nome-repres
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fin-nome-repres w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-fin-nome-repres IN FRAME f-pg-sel
DO:
  {include/zoomvar.i &prog-zoom = adzoom/z01ad229.w
                     &campo     = fi-fin-nome-repres
                     &campozoom = nome-abrev
                     &FRAME     = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ini-ge-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-ge-codigo w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ini-ge-codigo IN FRAME f-pg-sel /* Grupo Estoque */
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z01in142.w
                     &campo     = fi-ini-ge-codigo
                     &campozoom = ge-codigo
                     &FRAME     = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ini-it-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-it-codigo w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ini-it-codigo IN FRAME f-pg-sel /* Item */
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z01in172.w
                     &campo     = fi-ini-it-codigo
                     &campozoom = it-codigo
                     &FRAME     = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ini-nome-repres
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-nome-repres w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ini-nome-repres IN FRAME f-pg-sel /* Representante */
DO:
  {include/zoomvar.i &prog-zoom = adzoom/z01ad229.w
                     &campo     = fi-ini-nome-repres
                     &campozoom = nome-abrev
                     &FRAME     = f-pg-sel}
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

{utp/ut9000.i "ESPD0005" "2.04.00.000"}

/* inicializaá‰es do template de relat¢rio */
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

    ASSIGN fi-ini-dt-entrega:SCREEN-VALUE IN FRAME f-pg-sel = STRING(DATE(MONTH(TODAY),1,YEAR(TODAY)),"99/99/9999")
           fi-fin-dt-entrega:SCREEN-VALUE IN FRAME f-pg-sel = STRING(TODAY,"99/99/9999")
           fi-fin-dt-precoant:SCREEN-VALUE IN FRAME f-pg-sel = STRING(DATE(MONTH(TODAY),1,YEAR(TODAY)) - 1,"99/99/9999").
    ASSIGN da-aux = DATE(MONTH(TODAY),1,YEAR(TODAY)) - 1.
    ASSIGN fi-ini-dt-precoant:SCREEN-VALUE IN FRAME f-pg-sel =  STRING(DATE(MONTH(da-aux),1,YEAR(da-aux)),"99/99/9999").
    
    {include/i-rpmbl.i}

    fi-ini-it-codigo:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-fin-it-codigo:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-ini-ge-codigo:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-fin-ge-codigo:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-ini-nome-repres:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-fin-nome-repres:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.

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
  ENABLE bt-executar bt-ajuda bt-cancelar im-pg-imp im-pg-par im-pg-sel 
      WITH FRAME f-relat IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}
  DISPLAY rs-destino c-arquivo rs-execucao 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  ENABLE rs-destino bt-arquivo bt-config-impr c-arquivo rs-execucao RECT-7 
         RECT-9 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}
  DISPLAY tg-imp-param tg-emite-dup 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  ENABLE tg-imp-param tg-emite-dup RECT-27 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-par}
  DISPLAY fi-ini-dt-entrega fi-fin-dt-entrega fi-ini-dt-precoant 
          fi-fin-dt-precoant fi-ini-it-codigo fi-fin-it-codigo fi-ini-ge-codigo 
          fi-fin-ge-codigo fi-ini-nome-repres fi-fin-nome-repres 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  ENABLE fi-ini-dt-entrega fi-fin-dt-entrega fi-ini-dt-precoant 
         fi-fin-dt-precoant fi-ini-it-codigo fi-fin-it-codigo fi-ini-ge-codigo 
         fi-fin-ge-codigo fi-ini-nome-repres fi-fin-nome-repres IMAGE-1 
         IMAGE-13 IMAGE-14 IMAGE-2 IMAGE-3 IMAGE-4 IMAGE-5 IMAGE-6 IMAGE-7 
         IMAGE-8 
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
            run utp/ut-msgs.p (input "show", input 73, input "").
            
            apply "MOUSE-SELECT-CLICK":U to im-pg-imp in frame f-relat.
            apply "ENTRY":U to c-arquivo in frame f-pg-imp.
            return error.
        end.
    end.
    
    create tt-param.
    assign tt-param.usuario         = c-seg-usuario
           tt-param.destino         = input frame f-pg-imp rs-destino
           tt-param.data-exec       = today
           tt-param.hora-exec       = time
           tt-param.ini-dt-entrega  = INPUT FRAME f-pg-sel fi-ini-dt-entrega                
           tt-param.fin-dt-entrega  = INPUT FRAME f-pg-sel fi-fin-dt-entrega             
           tt-param.ini-dt-precoant = INPUT FRAME f-pg-sel fi-ini-dt-precoant                 
           tt-param.fin-dt-precoant = INPUT FRAME f-pg-sel fi-fin-dt-precoant
           tt-param.ini-it-codigo   = INPUT FRAME f-pg-sel fi-ini-it-codigo
           tt-param.fin-it-codigo   = INPUT FRAME f-pg-sel fi-fin-it-codigo                
           tt-param.ini-ge-codigo   = INPUT FRAME f-pg-sel fi-ini-ge-codigo                
           tt-param.fin-ge-codigo   = INPUT FRAME f-pg-sel fi-fin-ge-codigo                
           tt-param.ini-nome-repres = INPUT FRAME f-pg-sel fi-ini-nome-repres                   
           tt-param.fin-nome-repres = INPUT FRAME f-pg-sel fi-fin-nome-repres
           tt-param.emite-dup       = INPUT FRAME f-pg-par tg-emite-dup
           tt-param.imp-param       = INPUT FRAME f-pg-par tg-imp-param.                      
           
    if tt-param.destino = 1 THEN
       assign tt-param.arquivo = "".
    else 
    if tt-param.destino = 2 THEN
       assign tt-param.arquivo = input frame f-pg-imp c-arquivo.
    else 
       ASSIGN tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp".
    
    /* Executar do programa RP.P que ir† criar o relat¢rio */
    {include/i-rpexb.i}
    
    SESSION:SET-WAIT-STATE("general":U).
    
    {include/i-rprun.i esrp/espd0005rp.p}
    
    {include/i-rpexc.i}
    
    SESSION:SET-WAIT-STATE("":U).
    
    {include/i-rptrm.i}
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-troca-pagina w-relat 
PROCEDURE pi-troca-pagina :
/*------------------------------------------------------------------------------
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

