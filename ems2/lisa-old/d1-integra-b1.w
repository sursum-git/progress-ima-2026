&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*:T*******************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i D99XX999 9.99.99.999}

/* Chamada a include do gerenciador de licenáas. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */
/*                                                                                */
/* OBS: Para os smartobjects o parametro m¢dulo dever† ser MUT                    */

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i <programa> MUT}
&ENDIF

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
DEF TEMP-TABLE tt-etq-disp LIKE ob-etiqueta LABEL "Etiquetas DISP"
    FIELD marca AS LOG.

DEF TEMP-TABLE tt-etq-sel LIKE ob-etiqueta LABEL "Etiquetas SEL"
    FIELD integrado AS LOG
    FIELD marca AS LOG.

DEF BUFFER b-lisa-integra FOR lisa-integra.

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEF VAR i-row AS INT.
DEF VAR c-chave AS CHAR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog
&Scoped-define BROWSE-NAME br-etq-disp

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-etq-disp tt-etq-sel

/* Definitions for BROWSE br-etq-disp                                   */
&Scoped-define FIELDS-IN-QUERY-br-etq-disp tt-etq-disp.num-etiqueta tt-etq-disp.num-rolo-imp tt-etq-disp.it-codigo tt-etq-disp.cod-refer tt-etq-disp.quantidade   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-etq-disp   
&Scoped-define SELF-NAME br-etq-disp
&Scoped-define QUERY-STRING-br-etq-disp FOR EACH tt-etq-disp NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-etq-disp OPEN QUERY {&SELF-NAME} FOR EACH tt-etq-disp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-etq-disp tt-etq-disp
&Scoped-define FIRST-TABLE-IN-QUERY-br-etq-disp tt-etq-disp


/* Definitions for BROWSE br-etq-sel                                    */
&Scoped-define FIELDS-IN-QUERY-br-etq-sel tt-etq-sel.num-etiqueta tt-etq-sel.num-rolo-imp tt-etq-sel.quantidade   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-etq-sel   
&Scoped-define SELF-NAME br-etq-sel
&Scoped-define OPEN-QUERY-br-etq-sel RUN pi-totais. OPEN QUERY {&SELF-NAME} FOR EACH tt-etq-sel NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-etq-sel tt-etq-sel
&Scoped-define FIRST-TABLE-IN-QUERY-br-etq-sel tt-etq-sel


/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-D-Dialog ~
    ~{&OPEN-QUERY-br-etq-disp}~
    ~{&OPEN-QUERY-br-etq-sel}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi-cod-estab fi-serie fi-nr-nota-fis ~
fi-it-codigo-ini fi-it-codigo-fin fi-cod-refer-ini fi-cod-refer-fin ~
fi-nr-container fi-num-etiqueta bt-sel br-etq-disp br-etq-sel bt-add bt-del ~
bt-ok bt-cancela bt-ajuda rt-buttom RECT-98 IMAGE-1 IMAGE-2 IMAGE-28 ~
IMAGE-29 RECT-99 
&Scoped-Define DISPLAYED-OBJECTS fi-tot-sel fi-cod-estab fi-nome-estab ~
fi-serie fi-nr-nota-fis fi-it-codigo-ini fi-it-codigo-fin fi-cod-refer-ini ~
fi-cod-refer-fin fi-nr-container fi-num-etiqueta 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 fi-cod-estab fi-serie fi-nr-nota-fis fi-it-codigo-ini ~
fi-it-codigo-fin fi-cod-refer-ini fi-cod-refer-fin fi-nr-container ~
fi-num-etiqueta 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-add 
     IMAGE-UP FILE "adeicon\next-au":U
     IMAGE-INSENSITIVE FILE "adeicon\next-ai":U
     LABEL "" 
     SIZE 5 BY 1.17.

DEFINE BUTTON bt-ajuda 
     LABEL "&Ajuda" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-cancela AUTO-END-KEY 
     LABEL "&Cancelar" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-del 
     IMAGE-UP FILE "adeicon\prev-au":U
     IMAGE-INSENSITIVE FILE "adeicon\prev-ai":U
     LABEL "" 
     SIZE 5 BY 1.17.

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "&OK" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-sel 
     IMAGE-UP FILE "image/toolbar/im-zoo.bmp":U
     LABEL "" 
     SIZE 5 BY 2 TOOLTIP "Aplica Filtro"
     BGCOLOR 8 FONT 10.

DEFINE VARIABLE fi-cod-estab AS CHARACTER FORMAT "X(256)":U INITIAL "505" 
     LABEL "Estab" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-refer-fin AS CHARACTER FORMAT "x(8)" INITIAL "ZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 6.14 BY .88 TOOLTIP "Codigo de Referància Final" NO-UNDO.

DEFINE VARIABLE fi-cod-refer-ini AS CHARACTER FORMAT "x(8)" 
     LABEL "Referància":R12 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 TOOLTIP "Codigo de Referància Inicial" NO-UNDO.

DEFINE VARIABLE fi-it-codigo-fin AS CHARACTER FORMAT "X(16)" INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 10.14 BY .88 TOOLTIP "C¢digo do Item final." NO-UNDO.

DEFINE VARIABLE fi-it-codigo-ini AS CHARACTER FORMAT "x(16)" 
     LABEL "Item":R5 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "C¢digo do Item inicial." NO-UNDO.

DEFINE VARIABLE fi-nome-estab AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 52.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nr-container AS INTEGER FORMAT ">>>>>>>":U INITIAL 0 
     LABEL "Container" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nr-nota-fis AS CHARACTER FORMAT "X(256)":U 
     LABEL "Nota Fiscal" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-num-etiqueta AS INTEGER FORMAT ">>>>>>>>>":U INITIAL 0 
     LABEL "Etiqueta" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 NO-UNDO.

DEFINE VARIABLE fi-serie AS CHARACTER FORMAT "X(256)":U 
     LABEL "SÇrie" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 NO-UNDO.

DEFINE VARIABLE fi-tot-sel AS DECIMAL FORMAT ">>>,>>9.99":U INITIAL 0 
     LABEL "Total" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-28
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-29
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE RECTANGLE RECT-98
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 68 BY 3.5.

DEFINE RECTANGLE RECT-99
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 68 BY 2.5.

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 68 BY 1.42
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-etq-disp FOR 
      tt-etq-disp SCROLLING.

DEFINE QUERY br-etq-sel FOR 
      tt-etq-sel SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-etq-disp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-etq-disp D-Dialog _FREEFORM
  QUERY br-etq-disp NO-LOCK DISPLAY
      tt-etq-disp.num-etiqueta   COLUMN-LABEL "Etiqueta" 
      tt-etq-disp.num-rolo-imp   COLUMN-LABEL "Rolo"      WIDTH 5
      tt-etq-disp.it-codigo      COLUMN-LABEL "Item"      WIDTH 8
      tt-etq-disp.cod-refer      COLUMN-LABEL "Ref"       WIDTH 5
      tt-etq-disp.quantidade     COLUMN-LABEL "Qtde"      WIDTH 5
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 37 BY 10
         FONT 1
         TITLE "Etiquetas Dispon°veis".

DEFINE BROWSE br-etq-sel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-etq-sel D-Dialog _FREEFORM
  QUERY br-etq-sel NO-LOCK DISPLAY
      tt-etq-sel.num-etiqueta    COLUMN-LABEL "Etiqueta" 
      tt-etq-sel.num-rolo-imp    COLUMN-LABEL "Rolo"     WIDTH 5
      tt-etq-sel.quantidade      COLUMN-LABEL "Qtde"     WIDTH 6
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 24 BY 8.75
         FONT 1
         TITLE "Etiquetas Seleciondas".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     fi-tot-sel AT ROW 16.25 COL 56 COLON-ALIGNED WIDGET-ID 54
     fi-cod-estab AT ROW 1.5 COL 10 COLON-ALIGNED WIDGET-ID 6 NO-TAB-STOP 
     fi-nome-estab AT ROW 1.5 COL 14.43 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     fi-serie AT ROW 2.5 COL 10 COLON-ALIGNED WIDGET-ID 4 NO-TAB-STOP 
     fi-nr-nota-fis AT ROW 3.5 COL 10 COLON-ALIGNED WIDGET-ID 2
     fi-it-codigo-ini AT ROW 5.04 COL 10 COLON-ALIGNED HELP
          "Codigo do Item Inicial" WIDGET-ID 34
     fi-it-codigo-fin AT ROW 5 COL 28.43 COLON-ALIGNED NO-LABEL WIDGET-ID 32
     fi-cod-refer-ini AT ROW 6.04 COL 10 COLON-ALIGNED HELP
          "Codigo de Referància Inicial" WIDGET-ID 30
     fi-cod-refer-fin AT ROW 6 COL 28.43 COLON-ALIGNED HELP
          "Codigo de Referància Final" NO-LABEL WIDGET-ID 28
     fi-nr-container AT ROW 5 COL 49 COLON-ALIGNED WIDGET-ID 10
     fi-num-etiqueta AT ROW 6 COL 49 COLON-ALIGNED WIDGET-ID 52
     bt-sel AT ROW 5 COL 64 WIDGET-ID 26
     br-etq-disp AT ROW 7.25 COL 2 WIDGET-ID 200
     br-etq-sel AT ROW 7.25 COL 46 WIDGET-ID 300
     bt-add AT ROW 11.25 COL 40 WIDGET-ID 48
     bt-del AT ROW 12.42 COL 40 WIDGET-ID 50
     bt-ok AT ROW 17.75 COL 3
     bt-cancela AT ROW 17.75 COL 14
     bt-ajuda AT ROW 17.75 COL 59
     rt-buttom AT ROW 17.5 COL 2
     RECT-98 AT ROW 1.25 COL 2 WIDGET-ID 12
     IMAGE-1 AT ROW 5.04 COL 23 WIDGET-ID 36
     IMAGE-2 AT ROW 5 COL 26.86 WIDGET-ID 38
     IMAGE-28 AT ROW 6.04 COL 23 WIDGET-ID 40
     IMAGE-29 AT ROW 6 COL 26.86 WIDGET-ID 42
     RECT-99 AT ROW 4.75 COL 2 WIDGET-ID 44
     SPACE(0.71) SKIP(11.91)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 1
         TITLE "Adiciona Nota AVULSA para Remessa - d1-integra-b1.w"
         DEFAULT-BUTTON bt-ok WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/d-dialog.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB br-etq-disp bt-sel D-Dialog */
/* BROWSE-TAB br-etq-sel br-etq-disp D-Dialog */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fi-cod-estab IN FRAME D-Dialog
   1                                                                    */
/* SETTINGS FOR FILL-IN fi-cod-refer-fin IN FRAME D-Dialog
   1                                                                    */
/* SETTINGS FOR FILL-IN fi-cod-refer-ini IN FRAME D-Dialog
   1                                                                    */
/* SETTINGS FOR FILL-IN fi-it-codigo-fin IN FRAME D-Dialog
   1                                                                    */
/* SETTINGS FOR FILL-IN fi-it-codigo-ini IN FRAME D-Dialog
   1                                                                    */
/* SETTINGS FOR FILL-IN fi-nome-estab IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nr-container IN FRAME D-Dialog
   1                                                                    */
/* SETTINGS FOR FILL-IN fi-nr-nota-fis IN FRAME D-Dialog
   1                                                                    */
/* SETTINGS FOR FILL-IN fi-num-etiqueta IN FRAME D-Dialog
   1                                                                    */
/* SETTINGS FOR FILL-IN fi-serie IN FRAME D-Dialog
   1                                                                    */
/* SETTINGS FOR FILL-IN fi-tot-sel IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-etq-disp
/* Query rebuild information for BROWSE br-etq-disp
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-etq-disp NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-etq-disp */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-etq-sel
/* Query rebuild information for BROWSE br-etq-sel
     _START_FREEFORM
RUN pi-totais.
OPEN QUERY {&SELF-NAME} FOR EACH tt-etq-sel NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-etq-sel */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Adiciona Nota AVULSA para Remessa - d1-integra-b1.w */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-etq-sel
&Scoped-define SELF-NAME br-etq-sel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-etq-sel D-Dialog
ON VALUE-CHANGED OF br-etq-sel IN FRAME D-Dialog /* Etiquetas Seleciondas */
DO:
    IF AVAIL tt-etq-sel THEN DO.
       IF tt-etq-sel.integrado THEN
          ASSIGN bt-del:SENSITIVE = NO.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-add D-Dialog
ON CHOOSE OF bt-add IN FRAME D-Dialog
DO:
   DO i-row = 1 TO br-etq-disp:NUM-SELECTED-ROWS:
      IF br-etq-disp:FETCH-SELECTED-ROW(i-row) THEN DO.
         FIND tt-etq-sel WHERE
              tt-etq-sel.num-etiqueta = tt-etq-disp.num-etiqueta NO-ERROR.
         IF NOT AVAIL tt-etq-sel THEN DO.
            CREATE tt-etq-sel.
            ASSIGN tt-etq-sel.num-etiqueta = tt-etq-disp.num-etiqueta
                   tt-etq-sel.num-rolo-imp = tt-etq-disp.num-rolo-imp
                   tt-etq-sel.quantidade = tt-etq-disp.quantidade.
         END.
         DELETE tt-etq-disp.
      END.
   END.

   {&OPEN-QUERY-br-etq-sel}
   {&OPEN-QUERY-br-etq-disp}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda D-Dialog
ON CHOOSE OF bt-ajuda IN FRAME D-Dialog /* Ajuda */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-del
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-del D-Dialog
ON CHOOSE OF bt-del IN FRAME D-Dialog
DO:
   DO i-row = 1 TO br-etq-sel:NUM-SELECTED-ROWS.
      IF br-etq-sel:FETCH-SELECTED-ROW(i-row) THEN DO.

         IF tt-etq-sel.integrado THEN NEXT.

         FIND ob-etiqueta WHERE
              ob-etiqueta.cod-estabel = tt-etq-sel.cod-estabel AND
              ob-etiqueta.num-etiqueta = tt-etq-sel.num-etiqueta
              NO-LOCK NO-ERROR.

         CREATE tt-etq-disp.
         BUFFER-COPY ob-etiqueta TO tt-etq-disp.

         DELETE tt-etq-sel.
      END.
   END.

   {&OPEN-QUERY-br-etq-disp}
   {&OPEN-QUERY-br-etq-sel}
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok D-Dialog
ON CHOOSE OF bt-ok IN FRAME D-Dialog /* OK */
DO:
    ASSIGN INPUT FRAME {&FRAME-NAME} fi-cod-estab fi-serie fi-nr-nota-fis fi-nr-container.

    IF fi-nr-nota-fis = '' THEN DO.
       MESSAGE 'Favor Informar a Nota Fiscal...'
           VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
       RETURN NO-APPLY.
    END.

    FIND nota-fiscal WHERE
         nota-fiscal.cod-estabel = fi-cod-estab AND
         nota-fiscal.serie = fi-serie AND
         nota-fiscal.nr-nota-fis = fi-nr-nota-fis
         NO-LOCK NO-ERROR.
    IF NOT AVAIL nota-fiscal THEN DO.
       MESSAGE 'Nota Fiscal n∆o Encontrada...'
           VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
       RETURN NO-APPLY.
    END.

    /*
    FIND FIRST tt-etq-sel NO-LOCK NO-ERROR.
    IF NOT AVAIL tt-etq-sel THEN DO.
       MESSAGE 'Favor Selecionar Etiquetas...'
           VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
       RETURN NO-APPLY.
    END.
    */

    //NotaRemessa
    ASSIGN c-chave = nota-fiscal.cod-estabel + "|" + 
                     nota-fiscal.serie + "|" + 
                     nota-fiscal.nr-nota-fis. 

    FIND lisa-integra WHERE
         lisa-integra.cod-trans = 'NotaAvulsa' AND     
         lisa-integra.chave = c-chave SHARE-LOCK NO-ERROR.
    IF NOT AVAIL lisa-integra THEN DO.
       CREATE lisa-integra.
       ASSIGN lisa-integra.cod-trans = 'NotaAvulsa'
              lisa-integra.chave = c-chave.
    END.
    ASSIGN lisa-integra.acao = 'ENVIAR'
           lisa-integra.ind-situacao = 1.

    // Lima a Tabela para Ficar somente as selecionadas
    FOR EACH b-lisa-integra WHERE
             b-lisa-integra.cod-trans = 'PackingAvulso' AND
             b-lisa-integra.chave = c-chave SHARE-LOCK.
        DELETE b-lisa-integra.
    END.

    // Packing List
    FOR EACH tt-etq-sel NO-LOCK.
        FIND b-lisa-integra WHERE
             b-lisa-integra.cod-trans = 'PackingAvulso' AND
             b-lisa-integra.chave = c-chave AND 
             b-lisa-integra.conteudo = STRING(tt-etq-sel.num-etiqueta) 
             SHARE-LOCK NO-ERROR.
        IF NOT AVAIL b-lisa-integra THEN DO.
           CREATE b-lisa-integra.
           ASSIGN b-lisa-integra.cod-trans = 'PackingAvulso'
                  b-lisa-integra.chave = c-chave
                  b-lisa-integra.conteudo = STRING(tt-etq-sel.num-etiqueta) 
                  b-lisa-integra.acao = 'ENVIAR'
                  b-lisa-integra.ind-situacao = 1.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-sel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-sel D-Dialog
ON CHOOSE OF bt-sel IN FRAME D-Dialog
DO:
   ASSIGN INPUT {&list-1}.

   EMPTY TEMP-TABLE tt-etq-disp.

   // Busca Etiquetas selecionadas na NOTA
   FIND nota-fiscal WHERE
        nota-fiscal.cod-estabel = fi-cod-estab AND
        nota-fiscal.serie = fi-serie AND
        nota-fiscal.nr-nota-fis = fi-nr-nota-fis
        NO-LOCK NO-ERROR.
   IF AVAIL nota-fiscal THEN
      ASSIGN c-chave = nota-fiscal.cod-estabel + "|" + 
                       nota-fiscal.serie + "|" + 
                       nota-fiscal.nr-nota-fis. 

   FOR EACH b-lisa-integra WHERE
            b-lisa-integra.cod-trans = 'PackingAvulso' AND
            b-lisa-integra.chave = c-chave NO-LOCK.

       FIND ob-etiqueta WHERE
            ob-etiqueta.cod-estabel = nota-fiscal.cod-estabel AND
            ob-etiqueta.num-etiqueta = INTEGER(b-lisa-integra.conteudo)
            NO-LOCK NO-ERROR.
    
       CREATE tt-etq-sel.
       BUFFER-COPY ob-etiqueta TO tt-etq-sel.

       IF b-lisa-integra.ind-situacao <> 1 THEN
          ASSIGN tt-etq-sel.integrado = YES.
   END.

   // Busca Etiquetas disponiveis
   IF fi-num-etiqueta <> 0 THEN DO.
      FIND ob-etiqueta WHERE
           ob-etiqueta.cod-estab = fi-cod-estab AND
           ob-etiqueta.num-etiqueta = fi-num-etiqueta NO-LOCK NO-ERROR.
      IF NOT AVAIL ob-etiqueta THEN DO.
         MESSAGE 'Etiqueta n∆o Cadastrada...'
             VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
         APPLY 'ENTRY' TO SELF.
         RETURN NO-APPLY.
      END.

      IF ob-etiqueta.situacao <> 3 THEN DO.
         MESSAGE 'Situaá∆o da Etiqueta n∆o permite Remessa...'
             VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
         APPLY 'ENTRY' TO SELF.
         RETURN NO-APPLY.
      END.

      FIND tt-etq-sel WHERE
           tt-etq-sel.num-etiqueta = tt-etq-disp.num-etiqueta NO-ERROR.
      IF AVAIL tt-etq-sel THEN NEXT.

      CREATE tt-etq-disp.
      BUFFER-COPY ob-etiqueta TO tt-etq-disp.
   END.
   ELSE DO.
      FOR EACH ob-etiqueta WHERE
               ob-etiqueta.cod-estab = fi-cod-estab AND
               ob-etiqueta.situacao = 3 AND
               ob-etiqueta.it-codigo >= fi-it-codigo-ini AND
               ob-etiqueta.it-codigo <= fi-it-codigo-fin AND
               ob-etiqueta.cod-refer >= fi-cod-refer-ini AND
               ob-etiqueta.cod-refer <= fi-cod-refer-fin NO-LOCK.
    
          IF fi-nr-container <> 0 AND
             ob-etiqueta.nr-container <> fi-nr-container THEN NEXT.
    
          FIND tt-etq-sel WHERE
               tt-etq-sel.num-etiqueta = tt-etq-disp.num-etiqueta NO-ERROR.
          IF AVAIL tt-etq-sel THEN NEXT.

          CREATE tt-etq-disp.
          BUFFER-COPY ob-etiqueta TO tt-etq-disp.
      END.
   END.

   {&OPEN-QUERY-br-etq-disp}
   {&OPEN-QUERY-br-etq-sel}

   APPLY 'VALUE-CHANGED' TO br-etq-sel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-estab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-estab D-Dialog
ON LEAVE OF fi-cod-estab IN FRAME D-Dialog /* Estab */
DO:
   FIND estabelec WHERE
        estabelec.cod-estab = INPUT FRAME {&FRAME-NAME} fi-cod-estab
        NO-LOCK NO-ERROR.
   IF AVAIL estabelec THEN DO.
      ASSIGN fi-nome-estab:SCREEN-VALUE = estabelec.nome.

      ASSIGN fi-serie:SCREEN-VALUE = estabelec.serie.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-refer-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-refer-ini D-Dialog
ON LEAVE OF fi-cod-refer-ini IN FRAME D-Dialog /* Referància */
DO:
   IF SELF:SCREEN-VALUE <> '' THEN
      ASSIGN fi-cod-refer-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-it-codigo-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo-ini D-Dialog
ON LEAVE OF fi-it-codigo-ini IN FRAME D-Dialog /* Item */
DO:
   IF SELF:SCREEN-VALUE <> '' THEN
      ASSIGN fi-it-codigo-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-nr-nota-fis
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nr-nota-fis D-Dialog
ON LEAVE OF fi-nr-nota-fis IN FRAME D-Dialog /* Nota Fiscal */
DO:
    FIND nota-fiscal WHERE
         nota-fiscal.cod-estabel = INPUT FRAME {&FRAME-NAME} fi-cod-estab AND
         nota-fiscal.serie = INPUT FRAME {&FRAME-NAME} fi-serie AND
         nota-fiscal.nr-nota-fis = SELF:INPUT-VALUE 
         NO-LOCK NO-ERROR.
    IF NOT AVAIL nota-fiscal THEN DO.
       MESSAGE 'Nota Fiscal n∆o Encontrada...'
           VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
       RETURN NO-APPLY.
    END.
    
    FIND FIRST usuar-depos WHERE
               usuar-depos.cod-estab = nota-fiscal.cod-estabel NO-LOCK NO-ERROR.
    
    FIND deposito WHERE
         deposito.cod-depos = usuar-depos.cod-depos NO-LOCK NO-ERROR.
    
    IF deposito.nome-abrev <> nota-fiscal.nome-ab-cli THEN DO.
       MESSAGE 'Nota Fiscal n∆o Ç da LISA...' SKIP
               'Verifique...' 
           VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
       RETURN NO-APPLY.
    END.

    APPLY 'CHOOSE' TO bt-sel.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-etq-disp
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

{src/adm/template/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects D-Dialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available D-Dialog  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI D-Dialog  _DEFAULT-DISABLE
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
  HIDE FRAME D-Dialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI D-Dialog  _DEFAULT-ENABLE
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
  DISPLAY fi-tot-sel fi-cod-estab fi-nome-estab fi-serie fi-nr-nota-fis 
          fi-it-codigo-ini fi-it-codigo-fin fi-cod-refer-ini fi-cod-refer-fin 
          fi-nr-container fi-num-etiqueta 
      WITH FRAME D-Dialog.
  ENABLE fi-cod-estab fi-serie fi-nr-nota-fis fi-it-codigo-ini fi-it-codigo-fin 
         fi-cod-refer-ini fi-cod-refer-fin fi-nr-container fi-num-etiqueta 
         bt-sel br-etq-disp br-etq-sel bt-add bt-del bt-ok bt-cancela bt-ajuda 
         rt-buttom RECT-98 IMAGE-1 IMAGE-2 IMAGE-28 IMAGE-29 RECT-99 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy D-Dialog 
PROCEDURE local-destroy :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .
  {include/i-logfin.i}

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize D-Dialog 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}.
  END.

  APPLY 'LEAVE' TO fi-cod-estab.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-totais D-Dialog 
PROCEDURE pi-totais :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN fi-tot-sel = 0.
    FOR EACH tt-etq-sel.
        ASSIGN fi-tot-sel = fi-tot-sel + tt-etq-sel.quantidade.
    END.
    DISP fi-tot-sel WITH FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records D-Dialog  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-etq-sel"}
  {src/adm/template/snd-list.i "tt-etq-disp"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed D-Dialog 
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

