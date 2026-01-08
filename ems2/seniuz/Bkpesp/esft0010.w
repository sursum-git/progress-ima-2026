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
{include/i-prgvrs.i ESFT0010 2.04.00.000}

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

define temp-table tt-param  no-undo
       field destino           as integer
       field arquivo           as char format "x(35)"
       field usuario           as char format "x(12)"
       field data-exec         as date
       field hora-exec         as integer
       field classifica        as integer
       field desc-classifica   as char format "x(40)"
       FIELD cod-estabel       like nota-fiscal.cod-estabel 
       FIELD ini-dt-emissao    LIKE nota-fiscal.dt-emis-nota
       FIELD fin-dt-emissao    LIKE nota-fiscal.dt-emis-nota
       FIELD ini-cod-emitente  LIKE nota-fiscal.cod-emitente
       FIELD fin-cod-emitente  LIKE nota-fiscal.cod-emitente
       FIELD ini-cod-rep       LIKE nota-fiscal.cod-rep
       FIELD fin-cod-rep       LIKE nota-fiscal.cod-rep
       FIELD ini-it-codigo     LIKE item.it-codigo     
       FIELD fin-it-codigo     LIKE item.it-codigo     
       FIELD ini-ge-codigo     LIKE ITEM.ge-codigo     
       FIELD fin-ge-codigo     LIKE ITEM.ge-codigo     
       FIELD ini-dt-acumulo    LIKE nota-fiscal.dt-emis-nota
       FIELD gerar-excel       AS LOG FORMAT "Sim/N∆o"
       FIELD arq-excel         AS CHAR FORMAT "x(45)"
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-relat
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME f-pg-imp

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rs-destino bt-config-impr bt-arquivo ~
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

DEFINE VARIABLE fi-arq-excel AS CHARACTER FORMAT "X(256)":U 
     LABEL "Arquivo" 
     VIEW-AS FILL-IN 
     SIZE 36 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-27
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 67 BY 2.

DEFINE RECTANGLE RECT-29
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 67 BY 2.

DEFINE VARIABLE tg-gerar-excel AS LOGICAL INITIAL no 
     LABEL "Gerar Planilha Excel" 
     VIEW-AS TOGGLE-BOX
     SIZE 18.14 BY .88 TOOLTIP "Imprimir ParÉmetros/Seleá∆o ao final do relat¢rio."
     FONT 1 NO-UNDO.

DEFINE VARIABLE to-imp-param AS LOGICAL INITIAL no 
     LABEL "Imprimir ParÉmetros/Seleá∆o" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .83 TOOLTIP "Imprimir ParÉmetros/Seleá∆o ao final do relat¢rio."
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-cod-estabel AS CHARACTER FORMAT "X(3)":U 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fin-cod-emitente AS INTEGER FORMAT ">>>>>>>>9" INITIAL 999999999 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 TOOLTIP "Cliente final" NO-UNDO.

DEFINE VARIABLE fi-fin-cod-rep AS INTEGER FORMAT ">>>>9" INITIAL 99999 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 TOOLTIP "Representante final" NO-UNDO.

DEFINE VARIABLE fi-fin-dt-emissao AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "Data de emiss∆o da NF final" NO-UNDO.

DEFINE VARIABLE fi-fin-ge-codigo AS INTEGER FORMAT ">9" INITIAL 99 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 TOOLTIP "Grupo de estoque final" NO-UNDO.

DEFINE VARIABLE fi-fin-it-codigo AS CHARACTER FORMAT "x(16)" INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88 TOOLTIP "Familia final" NO-UNDO.

DEFINE VARIABLE fi-ini-cod-emitente AS INTEGER FORMAT ">>>>>>>>9" INITIAL 0 
     LABEL "Cliente":R17 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 TOOLTIP "Cliente inicial" NO-UNDO.

DEFINE VARIABLE fi-ini-cod-rep AS INTEGER FORMAT ">>>>9" INITIAL 0 
     LABEL "Representante":R16 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 TOOLTIP "Representante inicial" NO-UNDO.

DEFINE VARIABLE fi-ini-dt-acumulo AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 
     LABEL "Acumulado desde":R12 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "Data de ac£mulo inicial" NO-UNDO.

DEFINE VARIABLE fi-ini-dt-emissao AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 
     LABEL "Data Emissao":R12 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "Data de emiss∆o da NF inicial" NO-UNDO.

DEFINE VARIABLE fi-ini-ge-codigo AS INTEGER FORMAT ">9" INITIAL 0 
     LABEL "Grupo Estoque":R16 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 TOOLTIP "Grupo de estoque inicial" NO-UNDO.

DEFINE VARIABLE fi-ini-it-codigo AS CHARACTER FORMAT "x(16)" 
     LABEL "Item":R9 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88 TOOLTIP "Familia inicial" NO-UNDO.

DEFINE VARIABLE fi-nome-estabel AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 43.72 BY .88
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

DEFINE IMAGE IMAGE-9
     FILENAME "image\im-fir":U
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
     bt-config-impr AT ROW 3.58 COL 43.29 HELP
          "Configuraá∆o da impressora"
     bt-arquivo AT ROW 3.58 COL 43.29 HELP
          "Escolha do nome do arquivo"
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
     fi-cod-estabel AT ROW 1.54 COL 15.29 COLON-ALIGNED
     fi-nome-estabel AT ROW 1.54 COL 20.43 COLON-ALIGNED NO-LABEL
     fi-ini-dt-emissao AT ROW 2.54 COL 15.29 COLON-ALIGNED HELP
          "Data prevista para entrega do Pedido"
     fi-fin-dt-emissao AT ROW 2.54 COL 48.29 COLON-ALIGNED HELP
          "Data prevista para entrega do Pedido" NO-LABEL
     fi-ini-cod-emitente AT ROW 3.54 COL 15.29 COLON-ALIGNED HELP
          "C¢digo do emitente"
     fi-fin-cod-emitente AT ROW 3.54 COL 48.29 COLON-ALIGNED HELP
          "C¢digo do emitente" NO-LABEL
     fi-ini-cod-rep AT ROW 4.54 COL 15.29 COLON-ALIGNED HELP
          "C¢digo do representante"
     fi-fin-cod-rep AT ROW 4.54 COL 48.29 COLON-ALIGNED HELP
          "C¢digo do representante" NO-LABEL
     fi-ini-it-codigo AT ROW 5.54 COL 15.29 COLON-ALIGNED HELP
          "C¢digo da Fam°lia de Material"
     fi-fin-it-codigo AT ROW 5.54 COL 48.29 COLON-ALIGNED HELP
          "C¢digo do Item de Material" NO-LABEL
     fi-ini-ge-codigo AT ROW 6.54 COL 15.29 COLON-ALIGNED HELP
          "Grupo de estoque a que pertence o item"
     fi-fin-ge-codigo AT ROW 6.54 COL 48.29 COLON-ALIGNED HELP
          "Grupo de estoque a que pertence o item" NO-LABEL
     fi-ini-dt-acumulo AT ROW 7.54 COL 15.29 COLON-ALIGNED HELP
          "Data de emiss∆o da nota fiscal"
     IMAGE-1 AT ROW 2.54 COL 34.29
     IMAGE-10 AT ROW 5.54 COL 46.72
     IMAGE-2 AT ROW 2.54 COL 46.72
     IMAGE-3 AT ROW 3.54 COL 34.29
     IMAGE-4 AT ROW 4.54 COL 34.29
     IMAGE-5 AT ROW 6.54 COL 34.29
     IMAGE-6 AT ROW 3.54 COL 46.72
     IMAGE-7 AT ROW 4.54 COL 46.72
     IMAGE-8 AT ROW 6.54 COL 46.72
     IMAGE-9 AT ROW 5.54 COL 34.29
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 4 ROW 3.21
         SIZE 75.86 BY 10.25
         FONT 1.

DEFINE FRAME f-pg-par
     to-imp-param AT ROW 2.13 COL 29.86
     fi-arq-excel AT ROW 4.83 COL 32.72 COLON-ALIGNED
     tg-gerar-excel AT ROW 4.83 COL 7.86
     RECT-27 AT ROW 1.54 COL 6
     RECT-29 AT ROW 4.29 COL 6
     "Planilha para Excel" VIEW-AS TEXT
          SIZE 13.57 BY .54 AT ROW 4 COL 30.86
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
         TITLE              = "Ranking do Faturamento por Representante"
         HEIGHT             = 15
         WIDTH              = 81.14
         MAX-HEIGHT         = 28.63
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 28.63
         VIRTUAL-WIDTH      = 146.29
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
/* SETTINGS FOR FILL-IN fi-arq-excel IN FRAME f-pg-par
   NO-ENABLE                                                            */
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
ON END-ERROR OF w-relat /* Ranking do Faturamento por Representante */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON WINDOW-CLOSE OF w-relat /* Ranking do Faturamento por Representante */
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


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME fi-arq-excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-arq-excel w-relat
ON LEAVE OF fi-arq-excel IN FRAME f-pg-par /* Arquivo */
DO:
  IF NOT fi-arq-excel:SCREEN-VALUE IN FRAME {&FRAME-NAME} MATCHES "*.xls*" THEN DO:
     MESSAGE "Nome do arquivo para Excel est† inv†lido." SKIP
             "Deve ser do tipo Caminho/Arquivo.xls" VIEW-AS ALERT-BOX.
     ASSIGN fi-arq-excel:SCREEN-VALUE IN FRAME f-pg-par = SESSION:TEMP-DIRECTORY + 
                         "ranking-faturamento.xls".
     RETURN NO-APPLY.
  END.
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
  {include/zoomvar.i &prog-zoom = adzoom/z01ad107.w
                     &campo     = fi-cod-estabel
                     &campozoom = cod-estabel
                     &FRAME     = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-fin-cod-emitente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fin-cod-emitente w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-fin-cod-emitente IN FRAME f-pg-sel
DO:
  {include/zoomvar.i &prog-zoom = adzoom/z02ad098.w
                     &campo     = fi-fin-cod-emitente
                     &campozoom = cod-emitente
                     &FRAME     = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-fin-cod-rep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fin-cod-rep w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-fin-cod-rep IN FRAME f-pg-sel
DO:
  {include/zoomvar.i &prog-zoom = adzoom/z01ad229.w
                     &campo     = fi-fin-cod-rep
                     &campozoom = cod-rep
                     &FRAME     = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
  {include/zoomvar.i &prog-zoom = inzoom/z01in172.w
                     &campo     = fi-fin-it-codigo
                     &campozoom = it-codigo
                     &FRAME     = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ini-cod-emitente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-cod-emitente w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ini-cod-emitente IN FRAME f-pg-sel /* Cliente */
DO:
  {include/zoomvar.i &prog-zoom = adzoom/z02ad098.w
                     &campo     = fi-ini-cod-emitente
                     &campozoom = cod-emitente
                     &FRAME     = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ini-cod-rep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-cod-rep w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ini-cod-rep IN FRAME f-pg-sel /* Representante */
DO:
  {include/zoomvar.i &prog-zoom = adzoom/z01ad229.w
                     &campo     = fi-ini-cod-rep
                     &campozoom = cod-rep
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
&Scoped-define SELF-NAME tg-gerar-excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-gerar-excel w-relat
ON VALUE-CHANGED OF tg-gerar-excel IN FRAME f-pg-par /* Gerar Planilha Excel */
DO:
  IF INPUT FRAME {&FRAME-NAME} tg-gerar-excel = NO THEN
     ASSIGN fi-arq-excel:SENSITIVE IN FRAME {&FRAME-NAME} = NO
            fi-arq-excel:SCREEN-VALUE IN FRAME f-pg-par = "".
  ELSE
     ASSIGN fi-arq-excel:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            fi-arq-excel:SCREEN-VALUE IN FRAME f-pg-par = SESSION:TEMP-DIRECTORY + 
                         "ranking-faturamento.xls".
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

{utp/ut9000.i "ESFT0010" "2.04.00.000"}

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

    ASSIGN fi-cod-estabel:SCREEN-VALUE IN FRAME f-pg-sel = "2"
           fi-ini-dt-emissao:SCREEN-VALUE IN FRAME f-pg-sel = STRING(DATE(MONTH(TODAY),1,YEAR(TODAY)),"99/99/9999")
           fi-fin-dt-emissao:SCREEN-VALUE IN FRAME f-pg-sel = STRING(TODAY,"99/99/9999")
           fi-ini-dt-acumulo:SCREEN-VALUE IN FRAME f-pg-sel = STRING(DATE(1,1,YEAR(TODAY)),"99/99/9999")
           fi-ini-it-codigo:SCREEN-VALUE IN FRAME f-pg-sel = "5"
           fi-fin-it-codigo:SCREEN-VALUE IN FRAME f-pg-sel = "5ZZZZZZZZZZZZZZZ"
           fi-ini-ge-codigo:SCREEN-VALUE IN FRAME f-pg-sel = "50"
           fi-fin-ge-codigo:SCREEN-VALUE IN FRAME f-pg-sel = "59".

    {include/i-rpmbl.i}

    fi-cod-estabel:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-ini-cod-emitente:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-fin-cod-emitente:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-ini-cod-rep:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-fin-cod-rep:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.

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
  ENABLE rs-destino bt-config-impr bt-arquivo c-arquivo rs-execucao RECT-7 
         RECT-9 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}
  DISPLAY to-imp-param fi-arq-excel tg-gerar-excel 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  ENABLE to-imp-param tg-gerar-excel RECT-27 RECT-29 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-par}
  DISPLAY fi-cod-estabel fi-nome-estabel fi-ini-dt-emissao fi-fin-dt-emissao 
          fi-ini-cod-emitente fi-fin-cod-emitente fi-ini-cod-rep fi-fin-cod-rep 
          fi-ini-it-codigo fi-fin-it-codigo fi-ini-ge-codigo fi-fin-ge-codigo 
          fi-ini-dt-acumulo 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  ENABLE fi-cod-estabel fi-ini-dt-emissao fi-fin-dt-emissao fi-ini-cod-emitente 
         fi-fin-cod-emitente fi-ini-cod-rep fi-fin-cod-rep fi-ini-it-codigo 
         fi-fin-it-codigo fi-ini-ge-codigo fi-fin-ge-codigo fi-ini-dt-acumulo 
         IMAGE-1 IMAGE-10 IMAGE-2 IMAGE-3 IMAGE-4 IMAGE-5 IMAGE-6 IMAGE-7 
         IMAGE-8 IMAGE-9 
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
    assign tt-param.usuario          = c-seg-usuario
           tt-param.destino          = input frame f-pg-imp rs-destino
           tt-param.data-exec        = today
           tt-param.hora-exec        = time
           tt-param.cod-estabel      = INPUT FRAME f-pg-sel fi-cod-estabel
           tt-param.ini-dt-emissao   = INPUT FRAME f-pg-sel fi-ini-dt-emissao                
           tt-param.fin-dt-emissao   = INPUT FRAME f-pg-sel fi-fin-dt-emissao             
           tt-param.ini-cod-emitente = INPUT FRAME f-pg-sel fi-ini-cod-emitente                 
           tt-param.fin-cod-emitente = INPUT FRAME f-pg-sel fi-fin-cod-emitente
           tt-param.ini-cod-rep      = INPUT FRAME f-pg-sel fi-ini-cod-rep
           tt-param.fin-cod-rep      = INPUT FRAME f-pg-sel fi-fin-cod-rep
           tt-param.ini-it-codigo    = INPUT FRAME f-pg-sel fi-ini-it-codigo
           tt-param.fin-it-codigo    = INPUT FRAME f-pg-sel fi-fin-it-codigo
           tt-param.ini-ge-codigo    = INPUT FRAME f-pg-sel fi-ini-ge-codigo
           tt-param.fin-ge-codigo    = INPUT FRAME f-pg-sel fi-fin-ge-codigo
           tt-param.ini-dt-acumulo   = INPUT FRAME f-pg-sel fi-ini-dt-acumulo
           tt-param.gerar-excel      = INPUT FRAME f-pg-par tg-gerar-excel
           tt-param.arq-excel        = INPUT FRAME f-pg-par fi-arq-excel
           tt-param.imp-param        = INPUT FRAME f-pg-par to-imp-param.                      
                                   
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
    
    {include/i-rprun.i esrp/esft0010rp.p}
    
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

