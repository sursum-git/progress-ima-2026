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
{include/i-prgvrs.i IMCD020 2.04.00.001}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Preprocessadores do Template de Relat¢rio                            */
/* Obs: Retirar o valor do preprocessador para as p ginas que nÆo existirem  */

&GLOBAL-DEFINE PGSEL f-pg-sel
&GLOBAL-DEFINE PGPAR f-pg-par
&GLOBAL-DEFINE PGIMP f-pg-imp
  
/* Parameters Definitions ---                                           */

/* Temporary Table Definitions ---                                      */

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    FIELD c-cod-estabel    LIKE ped-venda.cod-estabel
    FIELD c-ini-it-codigo  LIKE ITEM.it-codigo
    FIELD c-fim-it-codigo  LIKE ITEM.it-codigo
    FIELD c-ini-cod-refer  LIKE referencia.cod-refer
    FIELD c-fim-cod-refer  LIKE referencia.cod-refer
    FIELD c-nr-tabpre      LIKE tb-preco.nr-tabpre
    FIELD i-ind-finan      AS INTEGER
    FIELD rd-emite         AS logical
    FIELD tg-excel         AS LOG
    FIELD tg-saldo         AS LOG.

/*define temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id ordem.

define buffer b-tt-digita for tt-digita.*/

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
&Scoped-Define ENABLED-OBJECTS RECT-7 RECT-9 rs-destino bt-config-impr ~
bt-arquivo c-arquivo rs-execucao tg-excel 
&Scoped-Define DISPLAYED-OBJECTS rs-destino c-arquivo rs-execucao tg-excel 

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
     SIZE 43.72 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE text-destino AS CHARACTER FORMAT "X(256)":U INITIAL " Destino" 
      VIEW-AS TEXT 
     SIZE 8.57 BY .63 NO-UNDO.

DEFINE VARIABLE text-modo AS CHARACTER FORMAT "X(256)":U INITIAL "Execu‡Æo" 
      VIEW-AS TEXT 
     SIZE 10.86 BY .63 NO-UNDO.

DEFINE VARIABLE rs-destino AS INTEGER INITIAL 4 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Impressora", 1,
"Arquivo", 2,
"Terminal", 3
     SIZE 41.72 BY 1.08 NO-UNDO.

DEFINE VARIABLE rs-execucao AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "On-Line", 1,
"Batch", 2
     SIZE 27.72 BY .92 NO-UNDO.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 50.86 BY 2.92.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 50.86 BY 1.71.

DEFINE VARIABLE tg-excel AS LOGICAL INITIAL no 
     LABEL "Gerar planilha no Excel" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .83 NO-UNDO.

DEFINE VARIABLE c-cod-estabel AS CHARACTER FORMAT "x(1)" 
     LABEL "Cod.Estabel":R12 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88.

DEFINE VARIABLE c-descricao AS CHARACTER FORMAT "x(40)" 
     VIEW-AS FILL-IN 
     SIZE 41.14 BY .88.

DEFINE VARIABLE c-nr-tabpre AS CHARACTER FORMAT "x(08)" 
     LABEL "Tab Pre‡os":R12 
     VIEW-AS FILL-IN 
     SIZE 10.14 BY .88.

DEFINE VARIABLE i-ind-finan AS INTEGER FORMAT ">9":U INITIAL 4 
     LABEL "Financiamento" 
     VIEW-AS FILL-IN 
     SIZE 4.14 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 69 BY 4.

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 69 BY 4.5.

DEFINE VARIABLE rd-emite AS LOGICAL INITIAL no 
     LABEL "Imprimir Estoque Disponivel" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .83 TOOLTIP "Marque caso desse imprimir o estoque e desmarque caso nÆo deseje imprimir" NO-UNDO.

DEFINE VARIABLE tg-saldo AS LOGICAL INITIAL yes 
     LABEL "Somente Itens com Saldo" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .83 NO-UNDO.

DEFINE VARIABLE c-fim-cod-refer AS CHARACTER FORMAT "x(6)" INITIAL "ZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88.

DEFINE VARIABLE c-fim-it-codigo AS CHARACTER FORMAT "x(16)" INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 17.14 BY .88.

DEFINE VARIABLE c-ini-cod-refer AS CHARACTER FORMAT "x(6)" 
     LABEL "Referˆncia":R5 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88.

DEFINE VARIABLE c-ini-it-codigo AS CHARACTER FORMAT "x(16)" 
     LABEL "Item":R5 
     VIEW-AS FILL-IN 
     SIZE 17.14 BY .88.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
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
          "Dispara a execu‡Æo do relat¢rio"
     bt-cancelar AT ROW 14.54 COL 14 HELP
          "Fechar"
     bt-ajuda AT ROW 14.54 COL 70 HELP
          "Ajuda"
     RECT-1 AT ROW 14.29 COL 2
     RECT-6 AT ROW 13.75 COL 2.14
     rt-folder-right AT ROW 2.67 COL 80.43
     rt-folder-top AT ROW 2.54 COL 2.14
     rt-folder-left AT ROW 2.54 COL 2.14
     im-pg-imp AT ROW 1.5 COL 33.57
     im-pg-par AT ROW 1.5 COL 17.86
     im-pg-sel AT ROW 1.5 COL 2.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81 BY 15
         FONT 1
         DEFAULT-BUTTON bt-executar.

DEFINE FRAME f-pg-imp
     rs-destino AT ROW 2.42 COL 3 HELP
          "Destino de ImpressÆo do Relat¢rio" NO-LABEL
     bt-config-impr AT ROW 3.58 COL 46.86 HELP
          "Configura‡Æo da impressora"
     bt-arquivo AT ROW 3.58 COL 47.14 HELP
          "Escolha do nome do arquivo"
     c-arquivo AT ROW 3.63 COL 3.29 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL
     rs-execucao AT ROW 5.75 COL 3 HELP
          "Modo de Execu‡Æo" NO-LABEL
     tg-excel AT ROW 7.75 COL 3 WIDGET-ID 2
     text-destino AT ROW 1.63 COL 3.86 NO-LABEL
     text-modo AT ROW 5 COL 1.29 COLON-ALIGNED NO-LABEL
     "~"Obs.: NÆo ‚ poss¡vel gerar um arquivo Excel com saldo estoque.~"" VIEW-AS TEXT
          SIZE 46 BY .54 AT ROW 9 COL 5 WIDGET-ID 4
     RECT-7 AT ROW 1.92 COL 2.14
     RECT-9 AT ROW 5.29 COL 2.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 73.72 BY 10
         FONT 1.

DEFINE FRAME f-pg-par
     c-cod-estabel AT ROW 1.5 COL 16.86 COLON-ALIGNED HELP
          "C¢digo do Estabelecimento"
     c-nr-tabpre AT ROW 2.75 COL 16.86 COLON-ALIGNED HELP
          "Tabela de Pre‡os"
     c-descricao AT ROW 2.75 COL 27.43 COLON-ALIGNED NO-LABEL
     i-ind-finan AT ROW 3.75 COL 16.86 COLON-ALIGNED
     rd-emite AT ROW 6.25 COL 12.14
     tg-saldo AT ROW 7.5 COL 12 WIDGET-ID 24
     "parcelas" VIEW-AS TEXT
          SIZE 8 BY .67 AT ROW 4 COL 23.86
     RECT-11 AT ROW 1 COL 3
     RECT-12 AT ROW 5.25 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 76 BY 10
         FONT 1.

DEFINE FRAME f-pg-sel
     c-ini-it-codigo AT ROW 3.13 COL 12.86 COLON-ALIGNED HELP
          "C¢digo do Item"
     c-fim-it-codigo AT ROW 3.13 COL 48 COLON-ALIGNED HELP
          "C¢digo do Item" NO-LABEL
     c-ini-cod-refer AT ROW 4.13 COL 13 COLON-ALIGNED HELP
          "C¢digo do Item" WIDGET-ID 4
     c-fim-cod-refer AT ROW 4.13 COL 48.14 COLON-ALIGNED HELP
          "C¢digo do Item" NO-LABEL WIDGET-ID 2
     IMAGE-1 AT ROW 3.13 COL 31.86
     IMAGE-2 AT ROW 3.13 COL 47.29
     IMAGE-3 AT ROW 4.13 COL 32 WIDGET-ID 6
     IMAGE-4 AT ROW 4.13 COL 47.43 WIDGET-ID 8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.85
         SIZE 76.86 BY 10.62.


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
         TITLE              = "Relat¢rio Tabela de Pre‡os"
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
/* SETTINGS FOR FILL-IN c-descricao IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME f-pg-sel
                                                                        */
/* SETTINGS FOR FRAME f-relat
                                                                        */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-6 IN FRAME f-relat
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
ON END-ERROR OF w-relat /* Relat¢rio Tabela de Pre‡os */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON WINDOW-CLOSE OF w-relat /* Relat¢rio Tabela de Pre‡os */
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
       IF rs-destino:SCREEN-VALUE IN frame f-pg-imp <> "3" THEN 
       ASSIGN tg-excel:SCREEN-VALUE IN frame f-pg-imp = "NO".
       
       run pi-executar.
   end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME c-cod-estabel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-cod-estabel w-relat
ON F5 OF c-cod-estabel IN FRAME f-pg-par /* Cod.Estabel */
DO:
    {include/zoomvar.i &prog-zoom=dizoom/z01di189.w
                       &campo=c-nr-tabpre
                       &campo2=c-descricao
                       &campozoom=nr-tabpre
                       &campozoom2=descricao}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-cod-estabel w-relat
ON MOUSE-SELECT-DBLCLICK OF c-cod-estabel IN FRAME f-pg-par /* Cod.Estabel */
DO:
  APPLY "f5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME c-nr-tabpre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-nr-tabpre w-relat
ON F5 OF c-nr-tabpre IN FRAME f-pg-par /* Tab Pre‡os */
DO:
    {include/zoomvar.i &prog-zoom=dizoom/z01di189.w
                       &campo=c-nr-tabpre
                       &campo2=c-descricao
                       &campozoom=nr-tabpre
                       &campozoom2=descricao}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-nr-tabpre w-relat
ON LEAVE OF c-nr-tabpre IN FRAME f-pg-par /* Tab Pre‡os */
DO:
    {include/LEAVE.i &tabela       = tb-preco
                     &atributo-ref = descricao
                     &variavel-ref = c-descricao
                     &WHERE = "tb-preco.nr-tabpre = input frame {&frame-name} c-nr-tabpre"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-nr-tabpre w-relat
ON MOUSE-SELECT-DBLCLICK OF c-nr-tabpre IN FRAME f-pg-par /* Tab Pre‡os */
DO:
  APPLY "f5" TO SELF.
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


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME rd-emite
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-emite w-relat
ON LEAVE OF rd-emite IN FRAME f-pg-par /* Imprimir Estoque Disponivel */
DO:
  /*IF INPUT FRAME f-pg-par rd-emite = YES
  THEN ENABLE de-qt-min-kg de-qt-min-m WITH FRAME f-pg-par.
  ELSE DISABLE de-qt-min-kg de-qt-min-m WITH FRAME f-pg-par.
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-emite w-relat
ON VALUE-CHANGED OF rd-emite IN FRAME f-pg-par /* Imprimir Estoque Disponivel */
DO:
  IF INPUT FRAME f-pg-par rd-emite = YES THEN DO:
     /*ENABLE de-qt-min-kg de-qt-min-m WITH FRAME f-pg-par.*/
     ASSIGN tg-excel:SCREEN-VALUE IN frame f-pg-imp = "NO".
  END.

     /*DISABLE de-qt-min-kg de-qt-min-m WITH FRAME f-pg-par.*/
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-imp
&Scoped-define SELF-NAME rs-destino
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-destino w-relat
ON VALUE-CHANGED OF rs-destino IN FRAME f-pg-imp
DO:
    MESSAGE self:SCREEN-VALUE
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    CASE self:screen-value:
        when "1" then do:
            assign c-arquivo:sensitive    = no
                   bt-arquivo:visible     = no
                   bt-config-impr:visible = YES.
            
        end.
        when "2" then do:
            assign c-arquivo:sensitive     = yes
                   bt-arquivo:visible      = yes
                   bt-config-impr:visible  = no.
        end.
        when "3" then do:
            assign c-arquivo:sensitive     = no
                   bt-arquivo:visible      = no
                   bt-config-impr:visible  = NO.
        end.
    end case.
    
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


&Scoped-define SELF-NAME tg-excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-excel w-relat
ON VALUE-CHANGED OF tg-excel IN FRAME f-pg-imp /* Gerar planilha no Excel */
DO:
    IF input frame f-pg-imp tg-excel THEN DO:
       ASSIGN rd-emite:SCREEN-VALUE IN frame f-pg-par = "NO"
              rs-destino:SCREEN-VALUE IN frame f-pg-imp = "3".
       APPLY "value-changed" TO rd-emite IN FRAME f-pg-par.

    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-relat 


/* ***************************  Main Block  *************************** */
if c-nr-tabpre:load-mouse-pointer("image\lupa.cur") then.

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{utp/ut9000.i "IMCD020" "2.06.00.001"}

/* inicializa‡äes do template de relat¢rio */
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
  
    FIND FIRST para-ped NO-LOCK NO-ERROR.
    ASSIGN c-cod-estabel:SCREEN-VALUE IN FRAME f-pg-par = para-ped.estab-padrao.
                                                          

    IF c-cod-estabel:SCREEN-VALUE IN FRAME f-pg-par = '1' THEN
       ASSIGN c-nr-tabpre:SCREEN-VALUE IN FRAME f-pg-par = "TAB A12".
    ELSE
       ASSIGN c-nr-tabpre:SCREEN-VALUE IN FRAME f-pg-par = "TAB E12".

    apply "leave":U to c-nr-tabpre in frame f-pg-par.
    apply "ENTRY":U to c-nr-tabpre in frame f-pg-par.

    {include/i-rpmbl.i im-pg-par}

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
  DISPLAY c-ini-it-codigo c-fim-it-codigo c-ini-cod-refer c-fim-cod-refer 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  ENABLE IMAGE-1 IMAGE-2 IMAGE-3 IMAGE-4 c-ini-it-codigo c-fim-it-codigo 
         c-ini-cod-refer c-fim-cod-refer 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}
  DISPLAY rs-destino c-arquivo rs-execucao tg-excel 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  ENABLE RECT-7 RECT-9 rs-destino bt-config-impr bt-arquivo c-arquivo 
         rs-execucao tg-excel 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}
  DISPLAY c-cod-estabel c-nr-tabpre c-descricao i-ind-finan rd-emite tg-saldo 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  ENABLE RECT-11 RECT-12 c-cod-estabel c-nr-tabpre i-ind-finan rd-emite 
         tg-saldo 
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
            run utp/ut-msgs.p (input "show", input 73, input "").
            
            apply "MOUSE-SELECT-CLICK":U to im-pg-imp in frame f-relat.
            apply "ENTRY":U to c-arquivo in frame f-pg-imp.
            return error.
        end.
    end.
    
    /* Coloque aqui as valida‡äes das outras p ginas, lembrando que elas devem 
       apresentar uma mensagem de erro cadastrada, posicionar na p gina com 
       problemas e colocar o focus no campo com problemas */
    
    if input frame f-pg-par c-nr-tabpre = "" then do:
        MESSAGE "Tabela de Pre‡o deve ser informada !" VIEW-AS ALERT-BOX ERROR.
        apply "MOUSE-SELECT-CLICK":U to im-pg-par in frame f-relat.
        apply "ENTRY":U to c-nr-tabpre in frame f-pg-par.
        return error.
    end.
    
    if input frame f-pg-par i-ind-finan = 0 then do:
        MESSAGE "N£mero de parcelas deve ser informado !" VIEW-AS ALERT-BOX ERROR.
        apply "MOUSE-SELECT-CLICK":U to im-pg-par in frame f-relat.
        apply "ENTRY":U to i-ind-finan in frame f-pg-par.
        return error.
    end.

    /* Aqui sÆo gravados os campos da temp-table que ser  passada como parƒmetro
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
         else assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp".
    
    /* Coloque aqui a l¢gica de grava‡Æo dos demais campos que devem ser passados
       como parƒmetros para o programa RP.P, atrav‚s da temp-table tt-param */
     assign tt-param.c-ini-it-codigo = input frame f-pg-sel c-ini-it-codigo
            tt-param.c-fim-it-codigo = input frame f-pg-sel c-fim-it-codigo
            tt-param.c-ini-cod-refer = input frame f-pg-sel c-ini-cod-refer
            tt-param.c-fim-cod-refer = input frame f-pg-sel c-fim-cod-refer
            tt-param.c-cod-estabel   = input frame f-pg-par c-cod-estabel
            tt-param.c-nr-tabpre     = input frame f-pg-par c-nr-tabpre
            tt-param.i-ind-finan     = input frame f-pg-par i-ind-finan
            tt-param.rd-emite        = input frame f-pg-par rd-emite
            tt-param.tg-excel        = input frame f-pg-imp tg-excel
            tt-param.tg-saldo        = input frame f-pg-par tg-saldo.
            
           /* tt-param.rd-ativo        = INPUT FRAME f-pg-par rd-ativo
            tt-param.rd-todos         =INPUT FRAME f-pg-par  rd-todos.*/
    
    
    /* Executar do programa RP.P que ir  criar o relat¢rio */
    {include/i-rpexb.i}
    
    SESSION:SET-WAIT-STATE("general":U).
    
    {include/i-rprun.i imp/imcd020rp.p}
    
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

