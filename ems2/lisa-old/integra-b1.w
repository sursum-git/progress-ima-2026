&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r11 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i B06di154 2.06.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
DEFINE BUFFER empresa FOR mgcad.empresa.

/* Parameters Definitions ---                                           */
    
DEF TEMP-TABLE tt-notas LIKE nota-fiscal
    FIELD nr-container      LIKE pp-container.nr-container
    FIELD nota-remessa      LIKE nota-fiscal.nr-nota-fis
    FIELD nome-emit         LIKE emitente.nome-emit
    FIELD ind-situacao      LIKE lisa-integra.ind-situacao
    FIELD chave             LIKE lisa-integra.chave
    FIELD acao              LIKE lisa-integra.acao
    FIELD erro-integra      AS LOGICAL
    FIELD visualiza         AS LOG INIT YES.

DEFINE TEMP-TABLE tt-etiqueta NO-UNDO LIKE ob-etiqueta
    FIELD acao              AS CHAR
    FIELD cod-trans         LIKE lisa-integra.cod-trans
    FIELD chave             LIKE lisa-integra.chave
    FIELD ind-situacao      LIKE lisa-integra.ind-situacao.

DEF BUFFER b-tt-notas FOR tt-notas.

DEF TEMP-TABLE tt-nota-fisc LIKE nota-fiscal.
DEF TEMP-TABLE tt-it-nota-fisc LIKE it-nota-fisc.

DEF TEMP-TABLE tt-notas-geradas NO-UNDO
    FIELD rw-nota-fiscal AS   ROWID
    FIELD nr-nota        LIKE nota-fiscal.nr-nota-fis
    FIELD seq-wt-docto   LIKE wt-docto.seq-wt-docto.

DEF BUFFER b-nota-fiscal FOR nota-fiscal.
DEF BUFFER b-lisa-integra FOR lisa-integra.

/* Global Variable Definitions ---                                      */
DEF NEW GLOBAL SHARED VAR c-tab-lisa AS CHAR NO-UNDO.
DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

/* Local Variable Definitions ---                                       */
DEF VAR c-status   AS CHAR.
DEF VAR de-tot-etq AS INT.
DEF VAR i-cor AS INT.
DEF VAR c-chave AS CHAR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-doc

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-notas tt-etiqueta

/* Definitions for BROWSE br-doc                                        */
&Scoped-define FIELDS-IN-QUERY-br-doc tt-notas.nr-nota-fis tt-notas.serie tt-notas.cod-emit tt-notas.nome-emit tt-notas.nr-container tt-notas.nota-remessa   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-doc   
&Scoped-define SELF-NAME br-doc
&Scoped-define QUERY-STRING-br-doc FOR EACH tt-notas WHERE                                  tt-notas.visualiza = YES  NO-LOCK                               BY tt-notas.nr-nota-fis DESCENDING
&Scoped-define OPEN-QUERY-br-doc OPEN QUERY {&SELF-NAME} FOR EACH tt-notas WHERE                                  tt-notas.visualiza = YES  NO-LOCK                               BY tt-notas.nr-nota-fis DESCENDING.
&Scoped-define TABLES-IN-QUERY-br-doc tt-notas
&Scoped-define FIRST-TABLE-IN-QUERY-br-doc tt-notas


/* Definitions for BROWSE br-etiquetas                                  */
&Scoped-define FIELDS-IN-QUERY-br-etiquetas tt-etiqueta.it-codigo tt-etiqueta.cod-refer tt-etiqueta.num-rolo-imp tt-etiqueta.quantidade tt-etiqueta.num-etiqueta tt-etiqueta.ind-situacao tt-etiqueta.chave tt-etiqueta.cod-trans   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-etiquetas   
&Scoped-define SELF-NAME br-etiquetas
&Scoped-define QUERY-STRING-br-etiquetas FOR EACH tt-etiqueta
&Scoped-define OPEN-QUERY-br-etiquetas OPEN QUERY {&SELF-NAME} FOR EACH tt-etiqueta.
&Scoped-define TABLES-IN-QUERY-br-etiquetas tt-etiqueta
&Scoped-define FIRST-TABLE-IN-QUERY-br-etiquetas tt-etiqueta


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-br-doc}~
    ~{&OPEN-QUERY-br-etiquetas}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS bt-retorna fi-nr-nota-fis-ini ~
fi-nr-nota-fis-fin fi-dt-emis-ini fi-dt-emis-fin tg-aguardando tg-integrado ~
tg-erro bt-sel br-doc bt-refresh bt-integra fi-nr-container-ini ~
fi-nr-container-fin bt-integra-pckl FILL-IN-12 FILL-IN-10 FILL-IN-8 ~
FILL-IN-16 FILL-IN-17 FILL-IN-18 bt-reenv-pckl bt-avanca bt-inc-nota ~
bt-del-nota RECT-2 RECT-4 RECT-96 IMAGE-5 IMAGE-6 IMAGE-108 IMAGE-109 ~
IMAGE-110 IMAGE-111 RECT-97 br-etiquetas RECT-100 
&Scoped-Define DISPLAYED-OBJECTS fi-nr-nota-fis-ini fi-nr-nota-fis-fin ~
fi-dt-emis-ini fi-dt-emis-fin tg-aguardando tg-integrado tg-erro ~
fi-nr-container-ini fi-nr-container-fin FILL-IN-12 FILL-IN-10 FILL-IN-8 ~
FILL-IN-16 FILL-IN-17 FILL-IN-18 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 bt-refresh bt-integra bt-integra-pckl 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" B-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<FOREIGN-KEYS>
nr-ord-produ||y|mgmov.ped-item.nr-ord-produ
nr-programa||y|mgmov.ped-item.nr-programa
it-codigo||y|mgmov.ped-item.it-codigo
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "nr-ord-produ,nr-programa,it-codigo"':U).

/* Tell the ADM to use the OPEN-QUERY-CASES. */
&Scoped-define OPEN-QUERY-CASES RUN dispatch ('open-query-cases':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Advanced Query Options" B-table-Win _INLINE
/* Actions: ? adm/support/advqedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<SORTBY-OPTIONS>
</SORTBY-OPTIONS>
<SORTBY-RUN-CODE>
************************
* Set attributes related to SORTBY-OPTIONS */
RUN set-attribute-list (
    'SortBy-Options = ""':U).
/************************
</SORTBY-RUN-CODE>
<FILTER-ATTRIBUTES>
</FILTER-ATTRIBUTES> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-avanca 
     IMAGE-UP FILE "image/toolbar/im-reini.bmp":U
     LABEL "ReEnviar" 
     SIZE 5 BY 1.42 TOOLTIP "Avan‡ar Proxima A‡Æo".

DEFINE BUTTON bt-del-nota 
     IMAGE-UP FILE "image/toolbar/im-era.bmp":U
     LABEL "DelNota" 
     SIZE 5 BY 1.42 TOOLTIP "Eliminar Remessa de Nota AVULSA".

DEFINE BUTTON bt-inc-nota 
     IMAGE-UP FILE "image/toolbar/im-inl.bmp":U
     LABEL "IncluirNota" 
     SIZE 5 BY 1.42 TOOLTIP "Adicionar Nota AVULSA e Etiquetas para Remessa".

DEFINE BUTTON bt-integra AUTO-GO 
     IMAGE-UP FILE "image/im-integra.jpg":U
     LABEL "" 
     SIZE 5 BY 1.42 TOOLTIP "Integra Notas de Remessa"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-integra-pckl AUTO-GO 
     IMAGE-UP FILE "image/im-integra.bmp":U
     IMAGE-INSENSITIVE FILE "image/im-integra.jpg":U
     LABEL "" 
     SIZE 5 BY 1.5 TOOLTIP "Integra Packin List do Documento"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-reenv-pckl 
     IMAGE-UP FILE "image/toolbar/im-undo.bmp":U
     LABEL "ReEnviar" 
     SIZE 5 BY 1.5 TOOLTIP "Re-enviar Packing List".

DEFINE BUTTON bt-refresh AUTO-GO 
     IMAGE-UP FILE "image/im-autom.bmp":U
     LABEL "" 
     SIZE 5 BY 1.42 TOOLTIP "Atualiza Dados"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-retorna 
     IMAGE-UP FILE "image/toolbar/im-undo.bmp":U
     LABEL "ReEnviar" 
     SIZE 5 BY 1.42 TOOLTIP "Retornar A‡Æo Anterior".

DEFINE BUTTON bt-sel AUTO-GO 
     IMAGE-UP FILE "image/im-cq.bmp":U
     LABEL "OK" 
     SIZE 9 BY 3.25 TOOLTIP "Processa Dados".

DEFINE VARIABLE fi-dt-emis-fin AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-emis-ini AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 
     LABEL "Per¡odo" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nr-container-fin AS INTEGER FORMAT ">>>>>>9" INITIAL 999999 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nr-container-ini AS INTEGER FORMAT ">>>>>>9" INITIAL 0 
     LABEL "Container" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nr-nota-fis-fin AS CHARACTER FORMAT "x(16)" INITIAL "ZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 TOOLTIP "Item final." NO-UNDO.

DEFINE VARIABLE fi-nr-nota-fis-ini AS CHARACTER FORMAT "x(16)" 
     LABEL "Documento" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE FILL-IN-10 AS CHARACTER FORMAT "X(256)":U INITIAL "Enviar Nota Remessa" 
     VIEW-AS FILL-IN 
     SIZE 19 BY .79
     BGCOLOR 15 FGCOLOR 2 FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-12 AS CHARACTER FORMAT "X(256)":U INITIAL "Gerar Nota Remessa" 
     VIEW-AS FILL-IN 
     SIZE 19 BY .79
     BGCOLOR 15 FGCOLOR 9 FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-16 AS CHARACTER FORMAT "X(256)":U INITIAL "Nota Enviada" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .79
     FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-17 AS CHARACTER FORMAT "X(256)":U INITIAL "Pendente de Envio" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .79
     BGCOLOR 15 FGCOLOR 2 FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-18 AS CHARACTER FORMAT "X(256)":U INITIAL "J  Enviado" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .79
     FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-8 AS CHARACTER FORMAT "X(256)":U INITIAL "Erro na Integra‡Æo" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .79
     BGCOLOR 15 FGCOLOR 12 FONT 17 NO-UNDO.

DEFINE IMAGE IMAGE-108
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-109
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-110
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-111
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-5
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-6
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE RECTANGLE RECT-100
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 76 BY 1.42.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 66 BY 1.25
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 142 BY 23
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-96
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 140 BY 4.

DEFINE RECTANGLE RECT-97
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 27 BY 3.25.

DEFINE VARIABLE tg-aguardando AS LOGICAL INITIAL yes 
     LABEL "Aguardando Integra‡Æo" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .67
     FGCOLOR 12  NO-UNDO.

DEFINE VARIABLE tg-erro AS LOGICAL INITIAL yes 
     LABEL "Erro na Integra‡Æo" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .67 NO-UNDO.

DEFINE VARIABLE tg-integrado AS LOGICAL INITIAL no 
     LABEL "Integrado" 
     VIEW-AS TOGGLE-BOX
     SIZE 10.86 BY .67 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-doc FOR 
      tt-notas SCROLLING.

DEFINE QUERY br-etiquetas FOR 
      tt-etiqueta SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-doc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-doc B-table-Win _FREEFORM
  QUERY br-doc NO-LOCK DISPLAY
      tt-notas.nr-nota-fis      COLUMN-LABEL "Documento"    WIDTH 10
      tt-notas.serie            COLUMN-LABEL "Serie"        WIDTH 5
      tt-notas.cod-emit         COLUMN-LABEL "Cliente"      WIDTH 8
      tt-notas.nome-emit        COLUMN-LABEL "Nome"         WIDTH 45
      tt-notas.nr-container     COLUMN-LABEL "Container"    WIDTH 10
      tt-notas.nota-remessa     COLUMN-LABEL "Nota Remessa" WIDTH 10
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 95 BY 16.5
         FONT 1
         TITLE "Documentos LISA" ROW-HEIGHT-CHARS .67.

DEFINE BROWSE br-etiquetas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-etiquetas B-table-Win _FREEFORM
  QUERY br-etiquetas DISPLAY
      tt-etiqueta.it-codigo       COLUMN-LABEL "Item"                 WIDTH 10
   tt-etiqueta.cod-refer          COLUMN-LABEL "Ref"                  WIDTH 5
   tt-etiqueta.num-rolo-imp       COLUMN-LABEL "NrRolo"               WIDTH 6
   tt-etiqueta.quantidade         COLUMN-LABEL "Qtde" FORMAT '>>>>9.99' WIDTH 5
   tt-etiqueta.num-etiqueta
tt-etiqueta.ind-situacao
tt-etiqueta.chave
tt-etiqueta.cod-trans
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 44 BY 16.5
         FONT 1
         TITLE "Packing List" ROW-HEIGHT-CHARS .71.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     bt-retorna AT ROW 22.25 COL 13 WIDGET-ID 496
     fi-nr-nota-fis-ini AT ROW 2.5 COL 20 COLON-ALIGNED HELP
          "Codigo do Ötem no EMS" WIDGET-ID 8
     fi-nr-nota-fis-fin AT ROW 2.5 COL 44 COLON-ALIGNED HELP
          "Codigo do Ötem no EMS" NO-LABEL WIDGET-ID 6
     fi-dt-emis-ini AT ROW 1.5 COL 20 COLON-ALIGNED WIDGET-ID 20
     fi-dt-emis-fin AT ROW 1.5 COL 44 COLON-ALIGNED NO-LABEL WIDGET-ID 462
     tg-aguardando AT ROW 2.5 COL 89 WIDGET-ID 174
     tg-integrado AT ROW 3.33 COL 89 WIDGET-ID 214
     tg-erro AT ROW 4.17 COL 89 WIDGET-ID 160
     bt-sel AT ROW 1.75 COL 131 WIDGET-ID 96
     br-doc AT ROW 5.5 COL 2 WIDGET-ID 100
     bt-refresh AT ROW 22.25 COL 2 WIDGET-ID 420
     bt-integra AT ROW 22.25 COL 7 WIDGET-ID 468
     fi-nr-container-ini AT ROW 3.5 COL 20 COLON-ALIGNED HELP
          "Codigo do Ötem no EMS" WIDGET-ID 474
     fi-nr-container-fin AT ROW 3.5 COL 44 COLON-ALIGNED HELP
          "Codigo do Ötem no EMS" NO-LABEL WIDGET-ID 472
     bt-integra-pckl AT ROW 22.25 COL 112 WIDGET-ID 476
     FILL-IN-12 AT ROW 22.58 COL 34 COLON-ALIGNED NO-LABEL WIDGET-ID 410
     FILL-IN-10 AT ROW 22.58 COL 54 COLON-ALIGNED NO-LABEL WIDGET-ID 408
     FILL-IN-8 AT ROW 22.58 COL 88 COLON-ALIGNED NO-LABEL WIDGET-ID 404
     FILL-IN-16 AT ROW 22.58 COL 74 COLON-ALIGNED NO-LABEL WIDGET-ID 490
     FILL-IN-17 AT ROW 22.21 COL 120.86 COLON-ALIGNED NO-LABEL WIDGET-ID 492
     FILL-IN-18 AT ROW 23.04 COL 120.86 COLON-ALIGNED NO-LABEL WIDGET-ID 494
     bt-reenv-pckl AT ROW 22.25 COL 117 WIDGET-ID 498
     bt-avanca AT ROW 22.25 COL 18 WIDGET-ID 504
     bt-inc-nota AT ROW 22.25 COL 23 WIDGET-ID 506
     bt-del-nota AT ROW 22.25 COL 28 WIDGET-ID 508
     br-etiquetas AT ROW 5.5 COL 98 WIDGET-ID 200
     " Sele‡Æo" VIEW-AS TEXT
          SIZE 8 BY .54 AT ROW 1.08 COL 4.43 WIDGET-ID 444
     " Situa‡Æo" VIEW-AS TEXT
          SIZE 7 BY .75 AT ROW 1.46 COL 84.86 WIDGET-ID 114
     RECT-2 AT ROW 13 COL 2 WIDGET-ID 4
     RECT-4 AT ROW 1 COL 1 WIDGET-ID 296
     RECT-96 AT ROW 1.25 COL 2 WIDGET-ID 442
     IMAGE-5 AT ROW 1.5 COL 38 WIDGET-ID 10
     IMAGE-6 AT ROW 1.5 COL 42.43 WIDGET-ID 12
     IMAGE-108 AT ROW 2.5 COL 38 WIDGET-ID 22
     IMAGE-109 AT ROW 2.5 COL 42.43 WIDGET-ID 464
     IMAGE-110 AT ROW 3.5 COL 38 WIDGET-ID 34
     IMAGE-111 AT ROW 3.5 COL 42.43 WIDGET-ID 36
     RECT-97 AT ROW 1.75 COL 83 WIDGET-ID 466
     RECT-100 AT ROW 22.25 COL 34 WIDGET-ID 510
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 23.08
         WIDTH              = 142.14.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{include/c-browse.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit Custom                            */
/* BROWSE-TAB br-doc bt-sel F-Main */
/* BROWSE-TAB br-etiquetas RECT-97 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON bt-integra IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON bt-integra-pckl IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON bt-refresh IN FRAME F-Main
   1                                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-doc
/* Query rebuild information for BROWSE br-doc
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-notas WHERE
                                 tt-notas.visualiza = YES  NO-LOCK
                              BY tt-notas.nr-nota-fis DESCENDING.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-doc */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-etiquetas
/* Query rebuild information for BROWSE br-etiquetas
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-etiqueta.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-etiquetas */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br-doc
&Scoped-define SELF-NAME br-doc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-doc B-table-Win
ON ROW-DISPLAY OF br-doc IN FRAME F-Main /* Documentos LISA */
DO:
   ASSIGN i-cor = ?.
   
   CASE tt-notas.acao.
       WHEN 'GERAR'  THEN ASSIGN i-cor = 9.
       WHEN 'ENVIAR' THEN ASSIGN i-cor = 2.
   END CASE.

   IF tt-notas.erro-integra THEN
      ASSIGN i-cor = 12.

   ASSIGN tt-notas.nr-nota-fis:FGCOLOR IN BROWSE br-doc = i-cor
          tt-notas.serie:FGCOLOR IN BROWSE br-doc = i-cor         
          tt-notas.cod-emit:FGCOLOR IN BROWSE br-doc = i-cor      
          tt-notas.nome-emit:FGCOLOR IN BROWSE br-doc = i-cor     
          tt-notas.nr-container:FGCOLOR IN BROWSE br-doc = i-cor
          tt-notas.nota-remessa:FGCOLOR IN BROWSE br-doc = i-cor.  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-doc B-table-Win
ON VALUE-CHANGED OF br-doc IN FRAME F-Main /* Documentos LISA */
DO:
   
   DO WITH FRAME {&FRAME-NAME}.
   END.

   ASSIGN bt-integra:SENSITIVE = YES
          bt-integra-pckl:SENSITIVE = YES
          bt-retorna:SENSITIVE = NO
          bt-avanca:SENSITIVE = NO
          bt-del-nota:SENSITIVE = NO
          bt-reenv-pckl:SENSITIVE = NO.

   EMPTY TEMP-TABLE tt-etiqueta.

   IF AVAIL tt-notas THEN DO.
      ASSIGN bt-retorna:SENSITIVE = YES
             bt-avanca:SENSITIVE = YES.
      
      IF tt-notas.nota-remessa = 'AVULSA' THEN DO.
         FOR EACH b-lisa-integra WHERE
                  b-lisa-integra.cod-trans = 'PackingAvulso' AND
                  b-lisa-integra.chave = tt-notas.chave NO-LOCK.
             FIND ob-etiqueta WHERE
                  ob-etiqueta.cod-estabel = tt-notas.cod-estabel AND
                  ob-etiqueta.num-etiqueta = INTEGER(b-lisa-integra.conteudo)
                  NO-LOCK NO-ERROR.

             IF AVAIL ob-etiqueta THEN DO.
                CREATE tt-etiqueta.
                BUFFER-COPY ob-etiqueta TO tt-etiqueta.
             END.
             ELSE DO.
                CREATE tt-etiqueta.
                ASSIGN tt-etiqueta.num-etiqueta = INTEGER(b-lisa-integra.conteudo).
                /*
                MESSAGE 'ERRO FATAL.....' SKIP
                        'NÆo foi Localizada a Etiqueta ' b-lisa-integra.conteudo SKIP
                        'Provavelmente foi eliminada...' 
                    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
                */    
                NEXT.
             END.

             ASSIGN tt-etiqueta.cod-trans = b-lisa-integra.cod-trans
                    tt-etiqueta.chave = b-lisa-integra.chave
                    tt-etiqueta.acao = b-lisa-integra.acao
                    tt-etiqueta.ind-situacao = b-lisa-integra.ind-situacao.
         END.

         IF tt-notas.acao = 'ENVIAR' AND
            tt-notas.ind-situacao = 1 THEN
            ASSIGN bt-del-nota:SENSITIVE = YES.
      END.
      ELSE DO.
          FIND lisa-integra WHERE
               lisa-integra.cod-trans = 'PackingList' AND
               lisa-integra.chave = STRING(tt-notas.nr-container)
               SHARE-LOCK NO-ERROR.
          IF NOT AVAIL lisa-integra THEN DO.
             CREATE lisa-integra.
             ASSIGN lisa-integra.cod-trans = lisa-integra.cod-trans
                    lisa-integra.chave = STRING(tt-notas.nr-container) 
                    lisa-integra.acao = 'ENVIAR'
                    lisa-integra.ind-situacao = 1.
          END.
          
          FOR EACH ob-etiqueta WHERE 
                   ob-etiqueta.cod-estabel = tt-notas.cod-estabel AND
                   ob-etiqueta.nr-container = tt-notas.nr-container NO-LOCK.
              CREATE tt-etiqueta.
              BUFFER-COPY ob-etiqueta TO tt-etiqueta.
    
              ASSIGN tt-etiqueta.cod-trans = lisa-integra.cod-trans
                     tt-etiqueta.chave = lisa-integra.chave
                     tt-etiqueta.acao = lisa-integra.acao
                     tt-etiqueta.ind-situacao = lisa-integra.ind-situacao.
          END.
      END.
      
      FIND FIRST tt-etiqueta NO-LOCK NO-ERROR.
      IF AVAIL tt-etiqueta THEN DO.
         ASSIGN bt-reenv-pckl:SENSITIVE = YES.
         IF tt-etiqueta.ind-situacao = 1 THEN DO.
            ASSIGN bt-reenv-pckl:TOOLTIP = "Marcar Packing List como Enviado". 
            bt-reenv-pckl:LOAD-IMAGE("image/toolbar/im-reini.bmp").
         END.
         ELSE DO.
            ASSIGN bt-reenv-pckl:TOOLTIP = "Re-enviar Packing List". 
            bt-reenv-pckl:LOAD-IMAGE("image/toolbar/im-undo.bmp").
         END.
      END.
   END.
   {&OPEN-QUERY-br-etiquetas}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-etiquetas
&Scoped-define SELF-NAME br-etiquetas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-etiquetas B-table-Win
ON ROW-DISPLAY OF br-etiquetas IN FRAME F-Main /* Packing List */
DO:
   ASSIGN i-cor = ?.
   IF tt-etiqueta.ind-situacao = 1 THEN
       ASSIGN i-cor = 2.

   ASSIGN tt-etiqueta.it-codigo:FGCOLOR IN BROWSE br-etiquetas = i-cor
          tt-etiqueta.cod-refer:FGCOLOR IN BROWSE br-etiquetas = i-cor
          tt-etiqueta.num-rolo-imp:FGCOLOR IN BROWSE br-etiquetas = i-cor
          tt-etiqueta.quantidade:FGCOLOR IN BROWSE br-etiquetas = i-cor.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-avanca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-avanca B-table-Win
ON CHOOSE OF bt-avanca IN FRAME F-Main /* ReEnviar */
DO:

    IF tt-notas.acao = '' THEN
       RETURN NO-APPLY.

    IF tt-notas.nota-remessa = 'AVULSA' THEN DO.
       FOR EACH lisa-integra WHERE
                lisa-integra.cod-trans = 'NotaAvulsa' AND
                lisa-integra.chave = tt-notas.chave SHARE-LOCK.

           CASE tt-notas.acao.
                WHEN 'ENVIAR' THEN ASSIGN lisa-integra.acao = ''
                                          lisa-integra.ind-situacao = 2.
                WHEN 'GERAR'  THEN ASSIGN lisa-integra.acao = 'ENVIAR'
                                          lisa-integra.ind-situacao = 1.
           END CASE.
       END.
    END.
    ELSE DO.
       FOR EACH lisa-integra WHERE
                lisa-integra.cod-trans = 'NotaRemessa' AND
                lisa-integra.chave = tt-notas.chave SHARE-LOCK.
    
           CASE tt-notas.acao.
                WHEN 'ENVIAR' THEN ASSIGN lisa-integra.acao = ''
                                          lisa-integra.ind-situacao = 2.
                WHEN 'GERAR'  THEN ASSIGN lisa-integra.acao = 'ENVIAR'
                                          lisa-integra.ind-situacao = 1.
           END CASE.
       END.
    END.

    APPLY 'CHOOSE' TO bt-refresh.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-del-nota
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-del-nota B-table-Win
ON CHOOSE OF bt-del-nota IN FRAME F-Main /* DelNota */
DO:
    FOR EACH lisa-integra WHERE
             lisa-integra.cod-trans = 'NotaAvulsa' AND
             lisa-integra.chave = tt-notas.chave SHARE-LOCK.

        // Packing List
        FOR EACH b-lisa-integra WHERE
                 b-lisa-integra.cod-trans = 'PackingAvulso' AND
                 b-lisa-integra.chave = lisa-integra.chave SHARE-LOCK.
            DELETE b-lisa-integra.
        END.

        DELETE lisa-integra.
    END.

    APPLY 'CHOOSE' TO bt-refresh.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-inc-nota
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-inc-nota B-table-Win
ON CHOOSE OF bt-inc-nota IN FRAME F-Main /* IncluirNota */
DO:
    RUN lisa/d1-integra-b1.w.
    APPLY 'CHOOSE' TO bt-refresh.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-integra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-integra B-table-Win
ON CHOOSE OF bt-integra IN FRAME F-Main
DO:
   ASSIGN c-tab-lisa = "NotaRemessa".
   RUN lisa/rel-giv.r.
   APPLY 'CHOOSE' TO bt-refresh.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-integra-pckl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-integra-pckl B-table-Win
ON CHOOSE OF bt-integra-pckl IN FRAME F-Main
DO:
   ASSIGN c-tab-lisa = 'PackingList'.
   RUN lisa/rel-giv.r.
   APPLY 'VALUE-CHANGED' TO br-doc.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-reenv-pckl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-reenv-pckl B-table-Win
ON CHOOSE OF bt-reenv-pckl IN FRAME F-Main /* ReEnviar */
DO:
   
   IF SELF:IMAGE = "image/toolbar/im-undo.bmp" THEN DO.   // Voltar
      MESSAGE 'Packlist ser  exclu¡do no sistema da LISA, deseja Continuar ?' 
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE l-confirma AS LOGICAL.
      IF NOT l-confirma THEN RETURN NO-APPLY.
   END.

   FIND FIRST tt-etiqueta NO-LOCK.
   FOR EACH lisa-integra WHERE
            lisa-integra.cod-trans  = tt-etiqueta.cod-trans AND
            lisa-integra.chave      = tt-etiqueta.chave SHARE-LOCK.
       IF SELF:IMAGE = "image/toolbar/im-undo.bmp" THEN   // voltar
          ASSIGN lisa-integra.acao = 'ENVIAR'
                 lisa-integra.ind-situacao = 1.
       ELSE 
          ASSIGN lisa-integra.acao = ''
                 lisa-integra.ind-situacao = 2.   
    END.
   
    APPLY 'VALUE-CHANGED' TO br-doc.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-refresh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-refresh B-table-Win
ON CHOOSE OF bt-refresh IN FRAME F-Main
DO:
   EMPTY TEMP-TABLE tt-notas.

   IF INPUT FRAME {&FRAME-NAME} tg-aguardando THEN
      RUN pi-processa (INPUT 1).

   IF INPUT FRAME {&FRAME-NAME} tg-integrado = YES THEN
      RUN pi-processa (INPUT 2).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-retorna
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-retorna B-table-Win
ON CHOOSE OF bt-retorna IN FRAME F-Main /* ReEnviar */
DO:
    IF tt-notas.acao = 'GERAR' THEN
       RETURN NO-APPLY.

    IF tt-notas.nota-remessa = 'AVULSA' THEN DO.
       FOR EACH lisa-integra WHERE
                lisa-integra.cod-trans = 'NotaAvulsa' AND
                lisa-integra.chave = tt-notas.chave SHARE-LOCK.

           CASE tt-notas.acao.
                WHEN ''       THEN ASSIGN lisa-integra.acao = 'ENVIAR'
                                          lisa-integra.ind-situacao = 1.
                WHEN 'ENVIAR' THEN ASSIGN lisa-integra.acao = 'ENVIAR'
                                          lisa-integra.ind-situacao = 1.
           END CASE.
       END.
    END.
    ELSE DO.
       FOR EACH lisa-integra WHERE
                lisa-integra.cod-trans = 'NotaRemessa' AND
                lisa-integra.chave = tt-notas.chave SHARE-LOCK.
    
           CASE tt-notas.acao.
                WHEN ''       THEN ASSIGN lisa-integra.acao = 'ENVIAR'
                                          lisa-integra.ind-situacao = 1.
                WHEN 'ENVIAR' THEN ASSIGN lisa-integra.acao = 'GERAR'
                                          lisa-integra.ind-situacao = 1.
           END CASE.
       END.
    END.

    APPLY 'CHOOSE' TO bt-refresh.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-sel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-sel B-table-Win
ON CHOOSE OF bt-sel IN FRAME F-Main /* OK */
DO:
   ASSIGN INPUT FRAME {&FRAME-NAME} fi-dt-emis-ini fi-dt-emis-fin
                                    fi-nr-nota-fis-ini fi-nr-nota-fis-fin
                                    fi-nr-container-ini fi-nr-container-fin
                                    tg-aguardando tg-integrado tg-erro.

   
   FOR EACH tt-notas NO-LOCK.
       ASSIGN tt-notas.visualiza = NO.
   END.

   IF tg-aguardando AND
      NOT CAN-FIND (FIRST tt-notas WHERE tt-notas.ind-situacao = 1) THEN
      RUN pi-processa (INPUT 1).

   IF tg-integrado = YES AND
      NOT CAN-FIND (FIRST tt-notas WHERE tt-notas.ind-situacao = 2) THEN
      RUN pi-processa (INPUT 2).

   FOR EACH tt-notas WHERE
            tt-notas.dt-emis >= fi-dt-emis-ini AND
            tt-notas.dt-emis <= fi-dt-emis-fin AND
            tt-notas.nr-nota-fis >= fi-nr-nota-fis-ini AND
            tt-notas.nr-nota-fis <= fi-nr-nota-fis-fin AND
            tt-notas.nr-container >= fi-nr-container-ini AND
            tt-notas.nr-container <= fi-nr-container-fin NO-LOCK.

       IF tg-erro       = NO AND tt-notas.erro-integra = YES THEN NEXT.
       IF tg-integrado  = NO AND tt-notas.acao = '' THEN NEXT.
       IF tg-aguardando = NO AND tt-notas.acao <> '' THEN NEXT.

       ASSIGN tt-notas.visualiza = YES.
   END.
   
   {&OPEN-QUERY-br-doc}
   APPLY 'VALUE-CHANGED' TO br-doc.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-nr-container-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nr-container-ini B-table-Win
ON LEAVE OF fi-nr-container-ini IN FRAME F-Main /* Container */
DO:
  ASSIGN fi-nr-container-fin:SCREEN-VALUE = '9999999'.
  IF SELF:SCREEN-VALUE <> '' THEN
     ASSIGN fi-nr-container-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-nr-nota-fis-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nr-nota-fis-ini B-table-Win
ON LEAVE OF fi-nr-nota-fis-ini IN FRAME F-Main /* Documento */
DO:
  ASSIGN fi-nr-nota-fis-fin:SCREEN-VALUE = 'ZZZZZZZZZZZZZZZZ'.
  IF SELF:SCREEN-VALUE <> '' THEN
     ASSIGN fi-nr-nota-fis-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-aguardando
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-aguardando B-table-Win
ON VALUE-CHANGED OF tg-aguardando IN FRAME F-Main /* Aguardando Integra‡Æo */
DO:
   APPLY 'choose' TO bt-sel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-erro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-erro B-table-Win
ON VALUE-CHANGED OF tg-erro IN FRAME F-Main /* Erro na Integra‡Æo */
DO:
  APPLY 'choose' TO bt-sel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-integrado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-integrado B-table-Win
ON VALUE-CHANGED OF tg-integrado IN FRAME F-Main /* Integrado */
DO:
  APPLY 'choose' TO bt-sel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-doc
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-open-query-cases B-table-Win  adm/support/_adm-opn.p
PROCEDURE adm-open-query-cases :
/*------------------------------------------------------------------------------
  Purpose:     Opens different cases of the query based on attributes
               such as the 'Key-Name', or 'SortBy-Case'
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* No Foreign keys are accepted by this SmartObject. */

  {&OPEN-QUERY-{&BROWSE-NAME}}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available B-table-Win  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI B-table-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize B-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-processa B-table-Win 
PROCEDURE pi-processa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER p-situacao AS INTEGER.

    FOR EACH lisa-integra WHERE
             lisa-integra.cod-trans = 'NotaRemessa' AND
             lisa-integra.ind-situacao = p-situacao  
             NO-LOCK USE-INDEX idx2.

        FIND pp-container WHERE
             pp-container.nr-container = INTEGER(lisa-integra.chave)
             NO-LOCK NO-ERROR.

        FIND processo-imp WHERE
             processo-imp.nr-proc-imp = STRING(pp-container.nr-container)
             NO-LOCK NO-ERROR.

        FIND FIRST item-doc-est WHERE
                   item-doc-est.num-pedido = processo-imp.num-pedido NO-LOCK NO-ERROR.

        FIND LAST nota-fiscal WHERE
                  nota-fiscal.cod-estabel = pp-container.cod-estabel AND
                  nota-fiscal.serie = item-doc-est.serie-docto AND
                  nota-fiscal.nr-nota-fis = item-doc-est.nro-docto AND
                  nota-fiscal.dt-cancela = ?
                  NO-LOCK NO-ERROR.

        IF NOT AVAIL nota-fiscal THEN NEXT.

        FIND emitente WHERE
             emitente.cod-emit = nota-fiscal.cod-emit NO-LOCK NO-ERROR.
        
        FIND tt-notas WHERE
             tt-notas.cod-estabel = nota-fiscal.cod-estabel AND
             tt-notas.serie = nota-fiscal.serie AND
             tt-notas.nr-nota-fis = nota-fiscal.nr-nota-fis NO-ERROR.
        IF AVAIL tt-notas THEN NEXT.

        CREATE tt-notas.
        BUFFER-COPY nota-fiscal TO tt-notas.
        
        ASSIGN tt-notas.nr-container = pp-container.nr-container
               tt-notas.nome-emit = emitente.nome-emit
               tt-notas.ind-situacao = lisa-integra.ind-situacao
               tt-notas.chave = lisa-integra.chave
               tt-notas.acao = lisa-integra.acao.

        FIND b-nota-fiscal WHERE
             b-nota-fiscal.cod-estabel = pp-container.cod-estabel AND
             b-nota-fiscal.nro-proc-entrada = pp-container.nr-container AND
             b-nota-fiscal.dt-cancela = ?  NO-LOCK NO-ERROR.
        IF AVAIL b-nota-fiscal THEN
           ASSIGN tt-notas.nota-remessa = b-nota-fiscal.nr-nota-fis.

    END.
    

    // Tratar Novas Avulsas (Foram Inseridas manualmente)
    FOR EACH lisa-integra WHERE
             lisa-integra.cod-trans = 'NotaAvulsa' AND
             lisa-integra.ind-situacao = p-situacao 
             NO-LOCK USE-INDEX idx2.

        FIND LAST nota-fiscal WHERE
                  nota-fiscal.cod-estabel = ENTRY(1,lisa-integra.chave,"|") AND
                  nota-fiscal.serie = ENTRY(2,lisa-integra.chave,"|") AND
                  nota-fiscal.nr-nota-fis = ENTRY(3,lisa-integra.chave,"|") AND
                  nota-fiscal.dt-cancela = ?
                  NO-LOCK NO-ERROR.

        FIND emitente WHERE
             emitente.cod-emit = nota-fiscal.cod-emit NO-LOCK NO-ERROR.
        
        IF NOT AVAIL nota-fiscal THEN NEXT.

        FIND tt-notas WHERE
             tt-notas.cod-estabel = nota-fiscal.cod-estabel AND
             tt-notas.serie = nota-fiscal.serie AND
             tt-notas.nr-nota-fis = nota-fiscal.nr-nota-fis NO-ERROR.
        IF AVAIL tt-notas THEN NEXT.

        CREATE tt-notas.
        BUFFER-COPY nota-fiscal TO tt-notas.
        
        ASSIGN tt-notas.nome-emit = emitente.nome-emit
               tt-notas.ind-situacao = lisa-integra.ind-situacao
               tt-notas.chave = lisa-integra.chave
               tt-notas.acao = lisa-integra.acao.

        ASSIGN tt-notas.nota-remessa = 'AVULSA'.
    END.
    
    
    APPLY 'CHOOSE' TO bt-sel IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key B-table-Win  adm/support/_key-snd.p
PROCEDURE send-key :
/*------------------------------------------------------------------------------
  Purpose:     Sends a requested KEY value back to the calling
               SmartObject.
  Parameters:  <see adm/template/sndkytop.i>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/sndkytop.i}

  /* Return the key value associated with each key case.             */
  {src/adm/template/sndkycas.i "nr-ord-produ" "ped-item" "nr-ord-produ"}
  {src/adm/template/sndkycas.i "nr-programa" "ped-item" "nr-programa"}
  {src/adm/template/sndkycas.i "it-codigo" "ped-item" "it-codigo"}

  /* Close the CASE statement and end the procedure.                 */
  {src/adm/template/sndkyend.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records B-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-etiqueta"}
  {src/adm/template/snd-list.i "tt-notas"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed B-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/bstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

