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
{include/i-prgvrs.i ESSP0156 2.04.00.000}

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
&GLOBAL-DEFINE PGCLA f-pg-cla
&GLOBAL-DEFINE PGPAR f-pg-par
&GLOBAL-DEFINE PGDIG
&GLOBAL-DEFINE PGIMP f-pg-imp
  
/* Parameters Definitions ---                                           */

/* Temporary Table Definitions ---                                      */

DEFINE TEMP-TABLE tt-param NO-UNDO
       FIELD destino          AS INTEGER
       FIELD arquivo          AS CHAR FORMAT "x(35)"
       FIELD usuario          AS CHAR FORMAT "x(12)"
       FIELD data-exec        AS DATE
       FIELD hora-exec        AS INTEGER
       FIELD classifica       AS INTEGER
       FIELD desc-classifica  AS CHAR FORMAT "x(40)"
       FIELD cod-estabel      AS CHAR
       FIELD ini-dt-emissao   LIKE ob-etiqueta.dt-emissao
       FIELD fin-dt-emissao   LIKE ob-etiqueta.dt-emissao
       FIELD lote-todos       AS LOG
       FIELD lote-pp          AS LOG
       FIELD lote-pd          AS LOG
       FIELD lote-rp          AS LOG
       FIELD lote-rd          AS LOG
       FIELD lote-sc          AS LOG
       FIELD lote-ca          AS LOG
       FIELD etq-todas        AS LOG
       FIELD impressa         AS LOG
       FIELD em-producao      AS LOG
       FIELD em-estoque       AS LOG
       FIELD reservada        AS LOG
       FIELD faturada         AS LOG
       FIELD em-reproc        AS LOG
       FIELD corte            AS LOG
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
DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

def var l-ok               as logical no-undo.
def var c-arq-digita       as char    no-undo.
def var c-terminal         as char    no-undo.

DEF VAR c-cod-estabel AS CHAR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-relat
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-pg-cla

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-11 rs-classif 
&Scoped-Define DISPLAYED-OBJECTS rs-classif 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-relat AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE rs-classif AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Por Data", 1,
"Por Item", 2
     SIZE 11.57 BY 3.71
     FONT 1 NO-UNDO.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 22.72 BY 4.5.

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
     SIZE 67 BY 2.

DEFINE VARIABLE to-imp-param AS LOGICAL INITIAL no 
     LABEL "Imprimir ParÉmetros/Seleá∆o" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .83 TOOLTIP "Imprimir ParÉmetros/Seleá∆o ao final do relat¢rio."
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-cod-estabel AS CHARACTER FORMAT "x(3)" 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fin-dt-emissao AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "Data final de emiss∆o da Etiqueta." NO-UNDO.

DEFINE VARIABLE fi-ini-dt-emissao AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 
     LABEL "Dt Emiss∆o Etq" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "Data inicial de emiss∆o da Etiqueta." NO-UNDO.

DEFINE VARIABLE fi-nome-estabel AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 36 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-4
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-7
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE VARIABLE tg-corte AS LOGICAL INITIAL no 
     LABEL "Consumo Corte" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .88 TOOLTIP "Apenas Etiquetas para Consumo Corte." NO-UNDO.

DEFINE VARIABLE tg-em-estoque AS LOGICAL INITIAL no 
     LABEL "Em Estoque" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .88 TOOLTIP "Apenas Etiquetas em Estoque." NO-UNDO.

DEFINE VARIABLE tg-em-producao AS LOGICAL INITIAL no 
     LABEL "Em Produá∆o" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .88 TOOLTIP "Apenas Etiquetas em Produá∆o." NO-UNDO.

DEFINE VARIABLE tg-em-reproc AS LOGICAL INITIAL no 
     LABEL "Em Reprocesso" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .88 TOOLTIP "Apenas Etiquetas em Reprocesso." NO-UNDO.

DEFINE VARIABLE tg-etq-todas AS LOGICAL INITIAL yes 
     LABEL "TODAS" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .88 TOOLTIP "Todas as Situaá‰es da Etiquetas (1,2,3,4,5,6)." NO-UNDO.

DEFINE VARIABLE tg-faturada AS LOGICAL INITIAL no 
     LABEL "Faturada" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .88 TOOLTIP "Apenas Etiquetas Faturadas." NO-UNDO.

DEFINE VARIABLE tg-impressa AS LOGICAL INITIAL no 
     LABEL "Impressa" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .88 TOOLTIP "Apenas Etiquetas Impressas." NO-UNDO.

DEFINE VARIABLE tg-lote-ca AS LOGICAL INITIAL no 
     LABEL "CA" 
     VIEW-AS TOGGLE-BOX
     SIZE 5.57 BY .88 TOOLTIP "Apenas o lote (Corte de Amostra)." NO-UNDO.

DEFINE VARIABLE tg-lote-pd AS LOGICAL INITIAL no 
     LABEL "PD" 
     VIEW-AS TOGGLE-BOX
     SIZE 5.57 BY .88 TOOLTIP "Apenas o lote PD (Peáa Defeituosa)" NO-UNDO.

DEFINE VARIABLE tg-lote-pp AS LOGICAL INITIAL no 
     LABEL "PP" 
     VIEW-AS TOGGLE-BOX
     SIZE 5.57 BY .88 TOOLTIP "Apenas o lote PP (Peáa Perfeita)." NO-UNDO.

DEFINE VARIABLE tg-lote-rd AS LOGICAL INITIAL no 
     LABEL "RD" 
     VIEW-AS TOGGLE-BOX
     SIZE 5.57 BY .88 TOOLTIP "Apenas o lote RD (Rolo Defeituoso)." NO-UNDO.

DEFINE VARIABLE tg-lote-rp AS LOGICAL INITIAL no 
     LABEL "RP" 
     VIEW-AS TOGGLE-BOX
     SIZE 5.57 BY .88 TOOLTIP "Apenas o lote RP (Rolo Perfeito)." NO-UNDO.

DEFINE VARIABLE tg-lote-sc AS LOGICAL INITIAL no 
     LABEL "SC" 
     VIEW-AS TOGGLE-BOX
     SIZE 5.57 BY .88 TOOLTIP "Apenas o lote SC (Saco)." NO-UNDO.

DEFINE VARIABLE tg-lote-todos AS LOGICAL INITIAL yes 
     LABEL "TODOS" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .88 TOOLTIP "Todas as qualidades (PP, PD, RP e RD)." NO-UNDO.

DEFINE VARIABLE tg-reservada AS LOGICAL INITIAL no 
     LABEL "Reservada" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .88 TOOLTIP "Apenas Etiquetas Reservadas." NO-UNDO.

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
     to-imp-param AT ROW 2.13 COL 29.86
     RECT-27 AT ROW 1.54 COL 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 75 BY 10.

DEFINE FRAME f-pg-cla
     rs-classif AT ROW 2.92 COL 31.43 HELP
          "Classificaá∆o para emiss∆o do relat¢rio" NO-LABEL
     RECT-11 AT ROW 2.58 COL 25.29
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 76.86 BY 10.31.

DEFINE FRAME f-relat
     bt-executar AT ROW 14.54 COL 3 HELP
          "Dispara a execuá∆o do relat¢rio"
     bt-cancelar AT ROW 14.54 COL 14 HELP
          "Fechar"
     bt-ajuda AT ROW 14.54 COL 70 HELP
          "Ajuda"
     RECT-6 AT ROW 13.75 COL 2.14
     RECT-1 AT ROW 14.29 COL 2
     rt-folder-top AT ROW 2.54 COL 2.14
     rt-folder AT ROW 2.5 COL 2
     rt-folder-left AT ROW 2.54 COL 2.14
     rt-folder-right AT ROW 2.67 COL 80.43
     im-pg-cla AT ROW 1.5 COL 17.72
     im-pg-imp AT ROW 1.5 COL 49.14
     im-pg-par AT ROW 1.5 COL 33.43
     im-pg-sel AT ROW 1.5 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81 BY 15
         DEFAULT-BUTTON bt-executar.

DEFINE FRAME f-pg-sel
     fi-cod-estabel AT ROW 1.46 COL 17.57 COLON-ALIGNED WIDGET-ID 2
     fi-nome-estabel AT ROW 1.46 COL 21.14 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     fi-ini-dt-emissao AT ROW 2.46 COL 17.57 COLON-ALIGNED
     fi-fin-dt-emissao AT ROW 2.46 COL 43.43 COLON-ALIGNED NO-LABEL
     tg-lote-todos AT ROW 3.46 COL 20
     tg-lote-pp AT ROW 3.46 COL 31.86
     tg-lote-pd AT ROW 3.46 COL 37.86
     tg-lote-rp AT ROW 3.46 COL 43.86
     tg-lote-rd AT ROW 3.46 COL 49.86
     tg-lote-sc AT ROW 3.46 COL 55.86
     tg-lote-ca AT ROW 3.46 COL 61.86
     tg-etq-todas AT ROW 4.75 COL 20 WIDGET-ID 8
     tg-impressa AT ROW 4.75 COL 31.86 WIDGET-ID 10
     tg-em-producao AT ROW 5.75 COL 31.86 WIDGET-ID 12
     tg-em-estoque AT ROW 6.75 COL 31.86 WIDGET-ID 14
     tg-reservada AT ROW 7.75 COL 31.86 WIDGET-ID 16
     tg-faturada AT ROW 8.75 COL 31.86 WIDGET-ID 18
     tg-em-reproc AT ROW 9.75 COL 31.86 WIDGET-ID 20
     tg-corte AT ROW 10.75 COL 31.86 WIDGET-ID 22
     "Lote:" VIEW-AS TEXT
          SIZE 3.57 BY .54 AT ROW 3.58 COL 15.72
     "Situaá∆o da Etiqueta:" VIEW-AS TEXT
          SIZE 15 BY .54 AT ROW 4.88 COL 4.57 WIDGET-ID 6
     IMAGE-4 AT ROW 2.46 COL 30
     IMAGE-7 AT ROW 2.46 COL 42.14
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
         TITLE              = "Etiquetas n∆o Resportadas no EMS"
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
   FRAME-NAME                                                           */
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
/* SETTINGS FOR TOGGLE-BOX tg-corte IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-em-estoque IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-em-producao IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-em-reproc IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-faturada IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-impressa IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-lote-ca IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-lote-pd IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-lote-pp IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-lote-rd IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-lote-rp IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-lote-sc IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-reservada IN FRAME f-pg-sel
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
ON END-ERROR OF w-relat /* Etiquetas n∆o Resportadas no EMS */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON WINDOW-CLOSE OF w-relat /* Etiquetas n∆o Resportadas no EMS */
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
&Scoped-define SELF-NAME fi-cod-estabel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-estabel w-relat
ON LEAVE OF fi-cod-estabel IN FRAME f-pg-sel /* Estabelecimento */
DO:
    FIND estabelec WHERE
         estabelec.cod-estabel = INPUT FRAME {&FRAME-NAME} fi-cod-estabel 
         NO-LOCK NO-ERROR.
    IF NOT AVAIL estabelec THEN DO.
       MESSAGE 'Estabelecimento n∆o Cadastrado....'
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
       APPLY 'ENTRY' TO SELF.
       RETURN NO-APPLY.
    END.
    ASSIGN fi-nome-estabel:SCREEN-VALUE IN FRAME f-pg-sel = TRIM(estabelec.cidade) + " / " + TRIM(estabelec.bairro).
  
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


&Scoped-define FRAME-NAME f-pg-sel
&Scoped-define SELF-NAME tg-etq-todas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-etq-todas w-relat
ON VALUE-CHANGED OF tg-etq-todas IN FRAME f-pg-sel /* TODAS */
DO:
  ASSIGN  tg-impressa:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = "NO"
          tg-em-producao:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO"
          tg-em-estoque:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = "NO"
          tg-reservada:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = "NO"
          tg-faturada:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = "NO"
          tg-em-reproc:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = "NO"
          tg-corte:SCREEN-VALUE IN FRAME {&FRAME-NAME}       = "NO".
  IF INPUT FRAME {&FRAME-NAME} tg-etq-todas = NO THEN
     ASSIGN tg-impressa:SENSITIVE IN FRAME {&FRAME-NAME}    = YES
            tg-em-producao:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            tg-em-estoque:SENSITIVE IN FRAME {&FRAME-NAME}  = YES
            tg-reservada:SENSITIVE IN FRAME {&FRAME-NAME}   = YES
            tg-faturada:SENSITIVE IN FRAME {&FRAME-NAME}    = YES
            tg-em-reproc:SENSITIVE IN FRAME {&FRAME-NAME}   = YES
            tg-corte:SENSITIVE IN FRAME {&FRAME-NAME}       = YES
            tg-reservada:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = "YES"
            tg-faturada:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = "YES".
  ELSE
      ASSIGN tg-impressa:SENSITIVE IN FRAME {&FRAME-NAME}    = NO 
             tg-em-producao:SENSITIVE IN FRAME {&FRAME-NAME} = NO 
             tg-em-estoque:SENSITIVE IN FRAME {&FRAME-NAME}  = NO 
             tg-reservada:SENSITIVE IN FRAME {&FRAME-NAME}   = NO 
             tg-faturada:SENSITIVE IN FRAME {&FRAME-NAME}    = NO 
             tg-em-reproc:SENSITIVE IN FRAME {&FRAME-NAME}   = NO 
             tg-corte:SENSITIVE IN FRAME {&FRAME-NAME}       = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-lote-todos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-lote-todos w-relat
ON VALUE-CHANGED OF tg-lote-todos IN FRAME f-pg-sel /* TODOS */
DO:
  ASSIGN  tg-lote-pp:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO"
          tg-lote-pd:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO"
          tg-lote-rp:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO"
          tg-lote-rd:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO"
          tg-lote-sc:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO"
          tg-lote-ca:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO".
  IF INPUT FRAME {&FRAME-NAME} tg-lote-todos = NO THEN
     ASSIGN tg-lote-pp:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            tg-lote-pd:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            tg-lote-rp:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            tg-lote-rd:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            tg-lote-sc:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            tg-lote-ca:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            tg-lote-pp:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "YES"
            tg-lote-rp:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "YES".
  ELSE
      ASSIGN tg-lote-pp:SENSITIVE IN FRAME {&FRAME-NAME} = NO
             tg-lote-pd:SENSITIVE IN FRAME {&FRAME-NAME} = NO
             tg-lote-rp:SENSITIVE IN FRAME {&FRAME-NAME} = NO
             tg-lote-rd:SENSITIVE IN FRAME {&FRAME-NAME} = NO
             tg-lote-sc:SENSITIVE IN FRAME {&FRAME-NAME} = NO
             tg-lote-ca:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-cla
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-relat 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{utp/ut9000.i "ESSP0156" "2.04.00.000"}

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

    RUN esapi/busca-estab-usuar.p (INPUT c-seg-usuario,
                                   OUTPUT c-cod-estabel).
    IF c-cod-estabel = '' THEN DO.
       MESSAGE 'Usuario ' c-seg-usuario ' n∆o relacionado Ö um Estabelecimento....'
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
       APPLY "choose" TO bt-cancelar IN FRAME f-relat.
    END.
    ASSIGN fi-cod-estabel:SCREEN-VALUE IN FRAME f-pg-sel = c-cod-estabel.
    FIND estabelec WHERE
         estabelec.cod-estabel = c-cod-estabel 
         NO-LOCK NO-ERROR.
    IF AVAIL estabelec THEN
       ASSIGN fi-nome-estabel:SCREEN-VALUE IN FRAME f-pg-sel = TRIM(estabelec.cidade) + " / " + TRIM(estabelec.bairro).




    {include/i-rpmbl.i}

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
  ENABLE im-pg-cla im-pg-imp im-pg-par im-pg-sel bt-executar bt-cancelar 
         bt-ajuda 
      WITH FRAME f-relat IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}
  DISPLAY fi-cod-estabel fi-nome-estabel fi-ini-dt-emissao fi-fin-dt-emissao 
          tg-lote-todos tg-lote-pp tg-lote-pd tg-lote-rp tg-lote-rd tg-lote-sc 
          tg-lote-ca tg-etq-todas tg-impressa tg-em-producao tg-em-estoque 
          tg-reservada tg-faturada tg-em-reproc tg-corte 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  ENABLE IMAGE-4 IMAGE-7 fi-cod-estabel fi-ini-dt-emissao fi-fin-dt-emissao 
         tg-lote-todos tg-etq-todas 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}
  DISPLAY rs-classif 
      WITH FRAME f-pg-cla IN WINDOW w-relat.
  ENABLE RECT-11 rs-classif 
      WITH FRAME f-pg-cla IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-cla}
  DISPLAY rs-destino c-arquivo rs-execucao 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  ENABLE RECT-7 RECT-9 rs-destino bt-arquivo bt-config-impr c-arquivo 
         rs-execucao 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}
  DISPLAY to-imp-param 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  ENABLE RECT-27 to-imp-param 
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
   
    IF tg-etq-todas:SCREEN-VALUE IN FRAME f-pg-sel   = "NO" AND
       tg-impressa:SCREEN-VALUE IN FRAME f-pg-sel    = "NO" AND
       tg-em-producao:SCREEN-VALUE IN FRAME f-pg-sel = "NO" AND
       tg-em-estoque:SCREEN-VALUE IN FRAME f-pg-sel  = "NO" AND 
       tg-reservada:SCREEN-VALUE IN FRAME f-pg-sel   = "NO" AND
       tg-faturada:SCREEN-VALUE IN FRAME f-pg-sel    = "NO" AND    
       tg-em-reproc:SCREEN-VALUE IN FRAME f-pg-sel   = "NO" AND   
       tg-corte:SCREEN-VALUE IN FRAME f-pg-sel       = "NO"  THEN DO:
       MESSAGE "Favor marcar uma opá∆o da SITUAÄ«O DA ETIQUETA ! ! "
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
       APPLY 'ENTRY' TO tg-etq-todas.
       RETURN NO-APPLY.
    END.

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
           tt-param.classifica      = input frame f-pg-cla rs-classif
           tt-param.desc-classifica = entry((tt-param.classifica - 1) * 2 + 1, 
                                             rs-classif:radio-buttons in frame f-pg-cla)
           tt-param.cod-estabel     = INPUT FRAME f-pg-sel fi-cod-estabel
           tt-param.ini-dt-emissao  = INPUT FRAME f-pg-sel fi-ini-dt-emissao  
           tt-param.fin-dt-emissao  = INPUT FRAME f-pg-sel fi-fin-dt-emissao
           tt-param.lote-todos      = INPUT FRAME f-pg-sel tg-lote-todos
           tt-param.lote-pp         = INPUT FRAME f-pg-sel tg-lote-pp
           tt-param.lote-pd         = INPUT FRAME f-pg-sel tg-lote-pd
           tt-param.lote-rp         = INPUT FRAME f-pg-sel tg-lote-rp
           tt-param.lote-rd         = INPUT FRAME f-pg-sel tg-lote-rd
           tt-param.lote-sc         = INPUT FRAME f-pg-sel tg-lote-sc
           tt-param.lote-ca         = INPUT FRAME f-pg-sel tg-lote-ca
           tt-param.etq-todas       = INPUT FRAME f-pg-sel tg-etq-todas
           tt-param.impressa        = INPUT FRAME f-pg-sel tg-impressa   
           tt-param.em-producao     = INPUT FRAME f-pg-sel tg-em-producao
           tt-param.em-estoque      = INPUT FRAME f-pg-sel tg-em-estoque 
           tt-param.reservada       = INPUT FRAME f-pg-sel tg-reservada  
           tt-param.faturada        = INPUT FRAME f-pg-sel tg-faturada   
           tt-param.em-reproc       = INPUT FRAME f-pg-sel tg-em-reproc  
           tt-param.corte           = INPUT FRAME f-pg-sel tg-corte      
           tt-param.imp-param       = INPUT FRAME f-pg-par to-imp-param. 

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
    
    {include/i-rprun.i esrp/essp0156rp.p}
    
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

