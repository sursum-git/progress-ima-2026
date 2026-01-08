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
{include/i-prgvrs.i ESFT0034 2.04.00.000}

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
&GLOBAL-DEFINE PGPAR 
&GLOBAL-DEFINE PGDIG 
&GLOBAL-DEFINE PGIMP f-pg-imp
  
/* Parameters Definitions ---                                           */

/* Temporary Table Definitions ---                                      */

DEFINE TEMP-TABLE tt-param NO-UNDO
       FIELD destino           AS INTEGER
       FIELD arquivo           AS CHAR FORMAT "x(35)"
       FIELD usuario           AS CHAR FORMAT "x(12)"
       FIELD data-exec         AS DATE
       FIELD hora-exec         AS INTEGER
       FIELD classifica        AS INTEGER
       FIELD desc-classifica   AS CHAR FORMAT "x(40)"
       FIELD ini-periodo       AS CHAR FORMAT "99/9999"
       FIELD fin-periodo       AS CHAR FORMAT "99/9999"
       FIELD ini-it-codigo     LIKE item.it-codigo
       FIELD fin-it-codigo     LIKE item.it-codigo
       FIELD ini-nome-abrev    LIKE nota-fiscal.nome-abrev
       FIELD fin-nome-abrev    LIKE nota-fiscal.nome-abrev
       FIELD ini-no-ab-reppri  LIKE nota-fiscal.no-ab-reppri
       FIELD fin-no-ab-reppri  LIKE nota-fiscal.no-ab-reppri
       FIELD tipo-inform       AS   INT.
       
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
DEF VAR i-cont-mes         AS INT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-relat
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME f-pg-cla

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rs-classif 
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
          "Por Valor", 1,
"Por Quantidade", 2,
"Por Cliente" ,3
     SIZE 48.86 BY 2.88
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

DEFINE VARIABLE fi-fin-it-codigo AS CHARACTER FORMAT "x(16)" INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88 TOOLTIP "C¢digo do Item final." NO-UNDO.

DEFINE VARIABLE fi-fin-no-ab-reppri AS CHARACTER FORMAT "X(12)" INITIAL "ZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 TOOLTIP "Nome abreviado do Representante final." NO-UNDO.

DEFINE VARIABLE fi-fin-nome-abrev AS CHARACTER FORMAT "X(12)" INITIAL "ZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 TOOLTIP "Nome abreviado do Cliente final." NO-UNDO.

DEFINE VARIABLE fi-fin-periodo AS CHARACTER FORMAT "99/9999":U INITIAL "010001" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 TOOLTIP "Màs/Ano final." NO-UNDO.

DEFINE VARIABLE fi-ini-it-codigo AS CHARACTER FORMAT "x(16)" 
     LABEL "Item":R9 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88 TOOLTIP "C¢digo do Item inicial." NO-UNDO.

DEFINE VARIABLE fi-ini-no-ab-reppri AS CHARACTER FORMAT "X(12)":U 
     LABEL "Representante" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 TOOLTIP "Nome abreviado do Representante inicial." NO-UNDO.

DEFINE VARIABLE fi-ini-nome-abrev AS CHARACTER FORMAT "X(12)" 
     LABEL "Cliente":R17 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 TOOLTIP "Nome abreviado do Cliente inicial." NO-UNDO.

DEFINE VARIABLE fi-ini-periodo AS CHARACTER FORMAT "99/9999":U INITIAL "010001" 
     LABEL "Per°odo" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 TOOLTIP "Màs/Ano inicial." NO-UNDO.

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

DEFINE IMAGE IMAGE-4
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-5
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-7
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-8
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

DEFINE FRAME f-pg-sel
     fi-ini-periodo AT ROW 1.54 COL 14.86 COLON-ALIGNED
     fi-fin-periodo AT ROW 1.54 COL 47.86 COLON-ALIGNED NO-LABEL
     fi-ini-it-codigo AT ROW 2.54 COL 14.86 COLON-ALIGNED HELP
          "C¢digo da Fam°lia de Material"
     fi-fin-it-codigo AT ROW 2.54 COL 47.86 COLON-ALIGNED HELP
          "C¢digo do Item de Material" NO-LABEL
     fi-ini-nome-abrev AT ROW 3.54 COL 14.86 COLON-ALIGNED HELP
          "Nome abreviado do cliente/fornecedor"
     fi-fin-nome-abrev AT ROW 3.54 COL 47.86 COLON-ALIGNED HELP
          "Nome abreviado do cliente/fornecedor" NO-LABEL
     fi-ini-no-ab-reppri AT ROW 4.54 COL 14.86 COLON-ALIGNED
     fi-fin-no-ab-reppri AT ROW 4.54 COL 47.86 COLON-ALIGNED HELP
          "Nome abreviado do representante principal" NO-LABEL
     IMAGE-31 AT ROW 4.54 COL 34.14
     IMAGE-32 AT ROW 4.54 COL 46.43
     IMAGE-33 AT ROW 1.54 COL 34.14
     IMAGE-34 AT ROW 1.54 COL 46.43
     IMAGE-4 AT ROW 2.54 COL 34.14
     IMAGE-5 AT ROW 3.54 COL 34.14
     IMAGE-7 AT ROW 2.54 COL 46.43
     IMAGE-8 AT ROW 3.54 COL 46.43
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 4 ROW 3.21
         SIZE 75.86 BY 10.25
         FONT 1.

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
         SIZE 75.72 BY 10
         FONT 1.

DEFINE FRAME f-relat
     bt-executar AT ROW 14.54 COL 3 HELP
          "Dispara a execuá∆o do relat¢rio"
     bt-ajuda AT ROW 14.54 COL 70 HELP
          "Ajuda"
     bt-cancelar AT ROW 14.58 COL 14 HELP
          "Fechar"
     RECT-6 AT ROW 13.75 COL 2.14
     rt-folder-top AT ROW 2.54 COL 2.14
     rt-folder-right AT ROW 2.67 COL 80.43
     RECT-1 AT ROW 14.29 COL 2
     rt-folder AT ROW 2.54 COL 2
     rt-folder-left AT ROW 2.54 COL 2.14
     im-pg-cla AT ROW 1.5 COL 17.86
     im-pg-imp AT ROW 1.5 COL 33.57
     im-pg-sel AT ROW 1.5 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81 BY 15
         DEFAULT-BUTTON bt-executar.

DEFINE FRAME f-pg-cla
     rs-classif AT ROW 2.38 COL 14 HELP
          "Classificaá∆o para emiss∆o do relat¢rio" NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 76.86 BY 10.31.


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
         TITLE              = "Rank Vendas por Cliente"
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
/* REPARENT FRAME */
ASSIGN FRAME f-pg-cla:FRAME = FRAME f-relat:HANDLE.

/* SETTINGS FOR FRAME f-pg-cla
                                                                        */
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
ON END-ERROR OF w-relat /* Rank Vendas por Cliente */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON WINDOW-CLOSE OF w-relat /* Rank Vendas por Cliente */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME f-pg-imp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL f-pg-imp w-relat
ON ENTRY OF FRAME f-pg-imp
DO:
    /*
    IF rs-destino:DISABLE(ENTRY(1,(rs-destino:RADIO-BUTTONS IN FRAME f-pg-imp))) THEN.
    IF rs-destino:DISABLE(ENTRY(5,(rs-destino:RADIO-BUTTONS IN FRAME f-pg-imp))) THEN.

    rs-execucao:SENSITIVE = NO.

    ASSIGN rs-destino:SCREEN-VALUE = '2'.
    APPLY 'value-changed' TO rs-destino.

    FIND usuar_mestre WHERE
         usuar_mestre.cod_usuario = c-seg-usuario NO-LOCK NO-ERROR.
    IF AVAIL usuar_mestre THEN
       ASSIGN c-arquivo:SCREEN-VALUE IN FRAME f-pg-imp = usuar_mestre.nom_dir_spool +
                                              usuar_mestre.nom_subdir_spool + "RANK-VENDAS-CLI.XLS".
    */                                              
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
&Scoped-define SELF-NAME fi-fin-it-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fin-it-codigo w-relat
ON LEAVE OF fi-fin-it-codigo IN FRAME f-pg-sel
DO:
  IF INPUT FRAME {&FRAME-NAME} fi-fin-it-codigo < "5" OR 
     INPUT FRAME {&FRAME-NAME} fi-fin-it-codigo > "5ZZZZZZZZZZZZZZZ" THEN
     ASSIGN fi-fin-it-codigo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "5ZZZZZZZZZZZZZZZ".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fin-it-codigo w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-fin-it-codigo IN FRAME f-pg-sel
DO:
  {include/zoomvar.i &prog-zoom = eszoom/z01in172.w
                     &campo     = fi-fin-it-codigo
                     &campozoom = it-codigo
                     &FRAME     = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-fin-no-ab-reppri
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fin-no-ab-reppri w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-fin-no-ab-reppri IN FRAME f-pg-sel
DO:
    {include/zoomvar.i &prog-zoom = adzoom/z01ad229.w
                       &campo     = fi-fin-no-ab-reppri
                       &campozoom = nome-abrev
                       &FRAME     = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-fin-nome-abrev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fin-nome-abrev w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-fin-nome-abrev IN FRAME f-pg-sel
DO:
   {include/zoomvar.i &prog-zoom = eszoom/z01ad098.w
                      &campo     = fi-fin-nome-abrev
                      &campozoom = nome-abrev
                      &FRAME     = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-fin-periodo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fin-periodo w-relat
ON LEAVE OF fi-fin-periodo IN FRAME f-pg-sel
DO:
  IF INT(SUBSTR(INPUT FRAME {&FRAME-NAME} fi-fin-periodo,1,2)) < 1 OR
     INT(SUBSTR(INPUT FRAME {&FRAME-NAME} fi-fin-periodo,1,2)) > 12 or
     INT(SUBSTR(INPUT FRAME {&FRAME-NAME} fi-fin-periodo,3,4)) = 0 OR 
     INT(SUBSTR(INPUT FRAME {&FRAME-NAME} fi-fin-periodo,3,4)) > INT(YEAR(TODAY)) THEN DO:
     MESSAGE "Màs deve estar entre 1 e 12. Ano deve estar entre 1 e " + 
             STRING(YEAR(TODAY),"9999") VIEW-AS ALERT-BOX.
     APPLY 'entry' TO fi-fin-periodo IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END.
  IF SUBSTR(INPUT FRAME {&FRAME-NAME} fi-fin-periodo,3,4) + 
     SUBSTR(INPUT FRAME {&FRAME-NAME} fi-fin-periodo,1,2) >
     STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") THEN DO:
     MESSAGE "Per°odo Final deve ser menor ou igual ao Màs/Ano da data atual." VIEW-AS ALERT-BOX.
     APPLY 'entry' TO fi-fin-periodo IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ini-it-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-it-codigo w-relat
ON LEAVE OF fi-ini-it-codigo IN FRAME f-pg-sel /* Item */
DO:
  IF INPUT FRAME {&FRAME-NAME} fi-ini-it-codigo < "5" OR 
     INPUT FRAME {&FRAME-NAME} fi-ini-it-codigo > "5z" THEN
     ASSIGN fi-ini-it-codigo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "5".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-it-codigo w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ini-it-codigo IN FRAME f-pg-sel /* Item */
DO:
  {include/zoomvar.i &prog-zoom = eszoom/z01in172.w
                     &campo     = fi-ini-it-codigo
                     &campozoom = it-codigo
                     &FRAME     = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ini-no-ab-reppri
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-no-ab-reppri w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ini-no-ab-reppri IN FRAME f-pg-sel /* Representante */
DO:
    {include/zoomvar.i &prog-zoom = adzoom/z01ad229.w
                       &campo     = fi-ini-no-ab-reppri
                       &campozoom = nome-abrev
                       &FRAME     = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ini-nome-abrev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-nome-abrev w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-ini-nome-abrev IN FRAME f-pg-sel /* Cliente */
DO:
   {include/zoomvar.i &prog-zoom = eszoom/z01ad098.w
                      &campo     = fi-ini-nome-abrev
                      &campozoom = nome-abrev
                      &FRAME     = f-pg-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ini-periodo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-periodo w-relat
ON LEAVE OF fi-ini-periodo IN FRAME f-pg-sel /* Per°odo */
DO:
  IF INT(SUBSTR(INPUT FRAME {&FRAME-NAME} fi-ini-periodo,1,2)) < 1 OR
     INT(SUBSTR(INPUT FRAME {&FRAME-NAME} fi-ini-periodo,1,2)) > 12 or
     INT(SUBSTR(INPUT FRAME {&FRAME-NAME} fi-ini-periodo,3,4)) = 0 OR 
     INT(SUBSTR(INPUT FRAME {&FRAME-NAME} fi-ini-periodo,3,4)) > INT(YEAR(TODAY)) THEN DO:
     MESSAGE "Màs deve estar entre 1 e 12. Ano deve estar entre 1 e " + 
             STRING(YEAR(TODAY),"9999") VIEW-AS ALERT-BOX.
     APPLY 'entry' TO fi-ini-periodo IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END.
  IF SUBSTR(INPUT FRAME {&FRAME-NAME} fi-ini-periodo,3,4) + 
     SUBSTR(INPUT FRAME {&FRAME-NAME} fi-ini-periodo,1,2) >
     STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") THEN DO:
     MESSAGE "Per°odo Inicial deve ser menor ou igual ao Màs/Ano da data atual." VIEW-AS ALERT-BOX.
     APPLY 'entry' TO fi-ini-periodo IN FRAME {&FRAME-NAME}.
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
ON MOUSE-SELECT-CLICK OF rs-destino IN FRAME f-pg-imp
DO:
  APPLY 'value-changed' TO SELF.
  IF SELF:SCREEN-VALUE = '2' THEN DO.
     FIND usuar_mestre WHERE
          usuar_mestre.cod_usuario = c-seg-usuario NO-LOCK NO-ERROR.
     IF AVAIL usuar_mestre THEN
        ASSIGN c-arquivo:SCREEN-VALUE IN FRAME f-pg-imp = usuar_mestre.nom_dir_spool + "/" +
                                               usuar_mestre.nom_subdir_spool + "RANK-VENDAS-CLI.XLS".
     ELSE
        ASSIGN c-arquivo:SCREEN-VALUE = "spool\RANK-VENDAS-CLI.XLS".

     ASSIGN c-arquivo:SENSITIVE     = NO
            bt-arquivo:SENSITIVE    = NO
            bt-config-impr:VISIBLE  = NO.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-relat 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{utp/ut9000.i "ESFT0034" "2.04.00.000"}

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

    ASSIGN fi-ini-periodo:SCREEN-VALUE IN FRAME f-pg-sel = STRING(MONTH(TODAY),"99") + 
                                                           STRING(YEAR(TODAY),"9999")
           fi-fin-periodo:SCREEN-VALUE IN FRAME f-pg-sel = STRING(MONTH(TODAY),"99") + 
                                                           STRING(YEAR(TODAY),"9999")
           fi-ini-it-codigo:SCREEN-VALUE IN FRAME f-pg-sel =  "5"
           fi-fin-it-codigo:SCREEN-VALUE IN FRAME f-pg-sel =  "5ZZZZZZZZZZZZZZZ"
           fi-ini-nome-abrev:SCREEN-VALUE IN FRAME f-pg-sel =  ""
           fi-fin-nome-abrev:SCREEN-VALUE IN FRAME f-pg-sel =  "ZZZZZZZZZZZZ".

    IF c-seg-usuario BEGINS "REP" THEN DO:
       FIND repres WHERE repres.cod-rep = INT(SUBSTR(c-seg-usuario,4,3)) NO-LOCK NO-ERROR.
       IF AVAIL repres THEN
          ASSIGN fi-ini-no-ab-reppri:SCREEN-VALUE IN FRAME f-pg-sel = repres.nome-abrev
                 fi-fin-no-ab-reppri:SCREEN-VALUE IN FRAME f-pg-sel = repres.nome-abrev.
       ELSE
          ASSIGN fi-ini-no-ab-reppri:SCREEN-VALUE IN FRAME f-pg-sel = ""
                 fi-fin-no-ab-reppri:SCREEN-VALUE IN FRAME f-pg-sel = "".

       ASSIGN fi-ini-no-ab-reppri:SENSITIVE IN FRAME f-pg-sel = NO
              fi-fin-no-ab-reppri:SENSITIVE IN FRAME f-pg-sel = NO.
    END.
    ELSE
       ASSIGN fi-ini-no-ab-reppri:SCREEN-VALUE IN FRAME f-pg-sel = ""
              fi-fin-no-ab-reppri:SCREEN-VALUE IN FRAME f-pg-sel = "ZZZZZZZZZZZZ".

    FIND usuar_mestre WHERE usuar_mestre.cod_usuario = c-seg-usuario NO-LOCK NO-ERROR.
    IF AVAIL usuar_mestre THEN
       ASSIGN c-arquivo:SCREEN-VALUE IN FRAME f-pg-imp = usuar_mestre.nom_dir_spool +
                                              usuar_mestre.nom_subdir_spool + "RANK-VENDAS-CLI.XLS".
    apply "MOUSE-SELECT-CLICK":U to im-pg-imp in frame f-relat.

    {include/i-rpmbl.i}

    fi-ini-it-codigo:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-fin-it-codigo:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-ini-nome-abrev:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-fin-nome-abrev:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-ini-no-ab-reppri:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.
    fi-fin-no-ab-reppri:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME f-pg-sel.

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
  ENABLE bt-executar bt-ajuda bt-cancelar im-pg-cla im-pg-imp im-pg-sel 
      WITH FRAME f-relat IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}
  DISPLAY rs-classif 
      WITH FRAME f-pg-cla IN WINDOW w-relat.
  ENABLE rs-classif 
      WITH FRAME f-pg-cla IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-cla}
  DISPLAY rs-destino c-arquivo rs-execucao 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  ENABLE rs-destino bt-arquivo bt-config-impr c-arquivo rs-execucao RECT-7 
         RECT-9 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}
  DISPLAY fi-ini-periodo fi-fin-periodo fi-ini-it-codigo fi-fin-it-codigo 
          fi-ini-nome-abrev fi-fin-nome-abrev fi-ini-no-ab-reppri 
          fi-fin-no-ab-reppri 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  ENABLE fi-ini-periodo fi-fin-periodo fi-ini-it-codigo fi-fin-it-codigo 
         fi-ini-nome-abrev fi-fin-nome-abrev fi-ini-no-ab-reppri 
         fi-fin-no-ab-reppri IMAGE-31 IMAGE-32 IMAGE-33 IMAGE-34 IMAGE-4 
         IMAGE-5 IMAGE-7 IMAGE-8 
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

    if input FRAME f-pg-imp rs-destino = 2 and
       input frame f-pg-imp rs-execucao = 1 then do:
        run utp/ut-vlarq.p (input input frame f-pg-imp c-arquivo).
        
        if return-value = "NOK":U then do:
            run utp/ut-msgs.p (input "show", input 73, input "").
            
            apply "MOUSE-SELECT-CLICK":U to im-pg-imp in frame f-relat.
            apply "ENTRY":U to c-arquivo in frame f-pg-imp.
            return error.
        end.
    end.

    IF INPUT FRAME f-pg-imp rs-destino = 2 AND
       NOT input frame f-pg-imp c-arquivo MATCHES "*.xls" THEN DO:
       MESSAGE "Nome do arquivo para Excel est† inv†lido." SKIP
               "Deve ser do tipo Caminho/Arquivo.xls" VIEW-AS ALERT-BOX.
       apply "MOUSE-SELECT-CLICK":U to im-pg-imp in frame f-relat.
       apply "ENTRY":U to c-arquivo in frame f-pg-imp.
       return error.
    END.
    
    IF SUBSTR(INPUT FRAME f-pg-sel fi-ini-periodo,3,4) <> 
       SUBSTR(INPUT FRAME f-pg-sel fi-fin-periodo,3,4) THEN
       ASSIGN i-cont-mes = (13 - INT(SUBSTR(INPUT FRAME f-pg-sel fi-ini-periodo,1,2))) + 
                                 INT(SUBSTR(INPUT FRAME f-pg-sel fi-fin-periodo,1,2)).
    ELSE
       ASSIGN i-cont-mes = INT(SUBSTR(INPUT FRAME f-pg-sel fi-fin-periodo,1,2)) - 
                           INT(SUBSTR(INPUT FRAME f-pg-sel fi-ini-periodo,1,2)) + 1.
    IF i-cont-mes < 1 OR i-cont-mes > 12 THEN DO:
       MESSAGE "N£mero de meses, na seleá∆o de Per°odos, deve estar entre 1 e 12 meses." 
               VIEW-AS ALERT-BOX.
       apply "MOUSE-SELECT-CLICK":U to im-pg-sel in frame f-relat.
       apply "ENTRY":U to fi-ini-periodo in frame f-pg-sel.
       return error.
    END.

    create tt-param.
    assign tt-param.usuario          = c-seg-usuario
           tt-param.destino          = input frame f-pg-imp rs-destino
           tt-param.data-exec        = today
           tt-param.hora-exec        = time
           tt-param.classifica      = input frame f-pg-cla rs-classif
           tt-param.desc-classifica = entry((tt-param.classifica - 1) * 2 + 1, 
                                             rs-classif:radio-buttons in frame f-pg-cla)
           tt-param.ini-periodo      = INPUT FRAME f-pg-sel fi-ini-periodo
           tt-param.fin-periodo      = INPUT FRAME f-pg-sel fi-fin-periodo
           tt-param.ini-it-codigo    = INPUT FRAME f-pg-sel fi-ini-it-codigo
           tt-param.fin-it-codigo    = INPUT FRAME f-pg-sel fi-fin-it-codigo                
           tt-param.ini-nome-abrev   = INPUT FRAME f-pg-sel fi-ini-nome-abrev                
           tt-param.fin-nome-abrev   = INPUT FRAME f-pg-sel fi-fin-nome-abrev
           tt-param.ini-no-ab-reppri = INPUT FRAME f-pg-sel fi-ini-no-ab-reppri                
           tt-param.fin-no-ab-reppri = INPUT FRAME f-pg-sel fi-fin-no-ab-reppri.
           
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
    
    {include/i-rprun.i esrp/esft0034rp.p}
    
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

