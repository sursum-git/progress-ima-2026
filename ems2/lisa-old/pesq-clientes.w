&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*:T*******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i D99XX999 9.99.99.999}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
DEF TEMP-TABLE tt-clientes LIKE emitente
    FIELD integrado       AS LOG
    FIELD log-erro-integr AS LOG
    FIELD acao AS CHAR.

DEF TEMP-TABLE tt-digita
    FIELD opcao  AS CHAR
    FIELD campo AS CHAR
    FIELD valor AS CHAR.

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEF VAR i-cor AS INT.
DEF VAR h-acomp AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog
&Scoped-define BROWSE-NAME br-clientes

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-clientes

/* Definitions for BROWSE br-clientes                                   */
&Scoped-define FIELDS-IN-QUERY-br-clientes tt-clientes.cod-emit tt-clientes.nome-emit   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-clientes   
&Scoped-define SELF-NAME br-clientes
&Scoped-define QUERY-STRING-br-clientes FOR EACH tt-clientes NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-clientes OPEN QUERY {&SELF-NAME} FOR EACH tt-clientes NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-clientes tt-clientes
&Scoped-define FIRST-TABLE-IN-QUERY-br-clientes tt-clientes


/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-D-Dialog ~
    ~{&OPEN-QUERY-br-clientes}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-buttom IMAGE-5 IMAGE-6 IMAGE-108 ~
IMAGE-109 RECT-56 bt-sel fi-cod-emit-ini fi-cod-emit-fin bt-dig-item ~
bt-ex-item fi-gr-cli-ini fi-gr-cli-fin bt-dig-ge bt-ex-ge fi-nome-emit ~
br-clientes bt-ok bt-cancela bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS fi-cod-emit-ini fi-cod-emit-fin ~
fi-gr-cli-ini fi-gr-cli-fin fi-nome-emit 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ajuda 
     LABEL "&Ajuda" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-cancela AUTO-END-KEY 
     LABEL "&Cancelar" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-dig-ge 
     IMAGE-UP FILE "image/im-ran.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-ran.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Digita Itens".

DEFINE BUTTON bt-dig-item 
     IMAGE-UP FILE "image/im-ran.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-ran.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Digita Itens".

DEFINE BUTTON bt-ex-ge 
     IMAGE-UP FILE "image/imt-retira.bmp":U
     IMAGE-INSENSITIVE FILE "image/iit-retira.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Exceto Itens".

DEFINE BUTTON bt-ex-item 
     IMAGE-UP FILE "image/imt-retira.bmp":U
     IMAGE-INSENSITIVE FILE "image/iit-retira.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Exceto Itens".

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "&Confirmar" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-sel 
     IMAGE-UP FILE "image/im-sav.bmp":U
     LABEL "OK" 
     SIZE 6.86 BY 3.33 TOOLTIP "Salva Altera‡äes".

DEFINE VARIABLE fi-cod-emit-fin AS INTEGER FORMAT ">,>>>,>>9" INITIAL 9999999 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "Item final." NO-UNDO.

DEFINE VARIABLE fi-cod-emit-ini AS INTEGER FORMAT ">,>>>,>>9" INITIAL 0 
     LABEL "Cliente" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-gr-cli-fin AS INTEGER FORMAT ">>9" INITIAL 999 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-gr-cli-ini AS INTEGER FORMAT ">>9" INITIAL 0 
     LABEL "Grupo de Clientes" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nome-emit AS CHARACTER FORMAT "X(256)":U 
     LABEL "Nome Cliente" 
     VIEW-AS FILL-IN 
     SIZE 43 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-108
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-109
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-5
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-6
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE RECTANGLE RECT-56
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 69 BY 3.75.

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 69 BY 1.42
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-clientes FOR 
      tt-clientes SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-clientes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-clientes D-Dialog _FREEFORM
  QUERY br-clientes NO-LOCK DISPLAY
      tt-clientes.cod-emit 
      tt-clientes.nome-emit FORMAT "x(60)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 69 BY 11.5
         FONT 1
         TITLE "Clientes Selecionados" FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     bt-sel AT ROW 1.42 COL 63 WIDGET-ID 42
     fi-cod-emit-ini AT ROW 1.5 COL 14 COLON-ALIGNED HELP
          "Codigo do Ötem no EMS" WIDGET-ID 470
     fi-cod-emit-fin AT ROW 1.5 COL 37 COLON-ALIGNED HELP
          "Codigo do Ötem no EMS" NO-LABEL WIDGET-ID 468
     bt-dig-item AT ROW 1.5 COL 50 WIDGET-ID 464
     bt-ex-item AT ROW 1.5 COL 55 WIDGET-ID 460
     fi-gr-cli-ini AT ROW 2.5 COL 14 COLON-ALIGNED HELP
          "Codigo do Ötem no EMS" WIDGET-ID 474
     fi-gr-cli-fin AT ROW 2.5 COL 37 COLON-ALIGNED HELP
          "Codigo do Ötem no EMS" NO-LABEL WIDGET-ID 472
     bt-dig-ge AT ROW 2.5 COL 50 WIDGET-ID 462
     bt-ex-ge AT ROW 2.5 COL 55 WIDGET-ID 466
     fi-nome-emit AT ROW 3.75 COL 14 COLON-ALIGNED WIDGET-ID 476
     br-clientes AT ROW 5.25 COL 2 WIDGET-ID 200
     bt-ok AT ROW 17.25 COL 3
     bt-cancela AT ROW 17.25 COL 14
     bt-ajuda AT ROW 17.25 COL 60.29
     rt-buttom AT ROW 17 COL 2
     IMAGE-5 AT ROW 1.5 COL 31 WIDGET-ID 10
     IMAGE-6 AT ROW 1.5 COL 35.43 WIDGET-ID 12
     IMAGE-108 AT ROW 2.58 COL 31 WIDGET-ID 22
     IMAGE-109 AT ROW 2.58 COL 35.43 WIDGET-ID 24
     RECT-56 AT ROW 1.25 COL 2 WIDGET-ID 40
     SPACE(0.99) SKIP(13.53)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 1
         TITLE "Seleciona Produtos"
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
   NOT-VISIBLE FRAME-NAME L-To-R                                        */
/* BROWSE-TAB br-clientes fi-nome-emit D-Dialog */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-clientes
/* Query rebuild information for BROWSE br-clientes
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-clientes NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-clientes */
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
ON WINDOW-CLOSE OF FRAME D-Dialog /* Seleciona Produtos */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-clientes
&Scoped-define SELF-NAME br-clientes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-clientes D-Dialog
ON ROW-DISPLAY OF br-clientes IN FRAME D-Dialog /* Clientes Selecionados */
DO:
   ASSIGN i-cor = ?.

   IF tt-clientes.acao <> '' THEN
       ASSIGN i-cor = 9.

   ELSE IF tt-clientes.log-erro-integr THEN
       ASSIGN i-cor = 12.
   ELSE IF tt-clientes.integrado THEN
      ASSIGN i-cor = 2.

   ASSIGN tt-clientes.cod-emit:FGCOLOR IN BROWSE br-clientes = i-cor. 
          tt-clientes.nome-emit:FGCOLOR IN BROWSE br-clientes = i-cor.  

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


&Scoped-define SELF-NAME bt-dig-ge
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-dig-ge D-Dialog
ON CHOOSE OF bt-dig-ge IN FRAME D-Dialog
DO:
    RUN esdlg/d01-digita.w (INPUT-OUTPUT TABLE tt-digita, 
                            INPUT "D",
                            INPUT "Grup Estoque").

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-dig-item
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-dig-item D-Dialog
ON CHOOSE OF bt-dig-item IN FRAME D-Dialog
DO:
    RUN esdlg/d01-digita.w (INPUT-OUTPUT TABLE tt-digita, 
                            INPUT "D",
                            INPUT "Item").

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ex-ge
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ex-ge D-Dialog
ON CHOOSE OF bt-ex-ge IN FRAME D-Dialog
DO:
    RUN esdlg/d01-digita.w (INPUT-OUTPUT TABLE tt-digita, 
                              INPUT "E",
                              INPUT "Grup Estoque").

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ex-item
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ex-item D-Dialog
ON CHOOSE OF bt-ex-item IN FRAME D-Dialog
DO:
    RUN esdlg/d01-digita.w (INPUT-OUTPUT TABLE tt-digita, 
                              INPUT "E",
                              INPUT "Item").

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok D-Dialog
ON CHOOSE OF bt-ok IN FRAME D-Dialog /* Confirmar */
DO:
   FOR EACH tt-clientes.
       FIND giv-integra WHERE
            giv-integra.cod-tab = 'Clientes' AND
            giv-integra.conteudo = STRING(tt-clientes.cod-emit) NO-LOCK NO-ERROR.
       IF NOT AVAIL giv-integra THEN DO.
          CREATE giv-integra.
          ASSIGN giv-integra.cod-tab = 'Clientes'
                 giv-integra.conteudo = STRING(tt-clientes.cod-emit)
                 giv-integra.acao = 'Incluir'.
       END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-sel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-sel D-Dialog
ON CHOOSE OF bt-sel IN FRAME D-Dialog /* OK */
DO:
    ASSIGN INPUT FRAME {&FRAME-NAME} fi-cod-emit-ini 
                                     fi-cod-emit-fin
                                     fi-gr-cli-ini    
                                     fi-gr-cli-fin
                                     fi-nome-emit.

    RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
    {utp/ut-liter.i Selecionando_Clientes *}
    RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

    EMPTY TEMP-TABLE tt-clientes.
    FOR EACH emitente WHERE
             emitente.identific <> 2 AND
             emitente.cod-emit >= fi-cod-emit-ini AND
             emitente.cod-emit <= fi-cod-emit-fin AND
             emitente.cod-gr-cli >= fi-gr-cli-ini AND
             emitente.cod-gr-cli <= fi-gr-cli-fin AND
             (emitente.nome-emit BEGINS fi-nome-emit OR fi-nome-emit = '')
             NO-LOCK.

        RUN pi-acompanhar IN h-acomp (INPUT "Cliente: " + emitente.nome-abrev).

        CREATE tt-clientes.
        BUFFER-COPY emitente TO tt-clientes.

        FIND giv-integra WHERE
             giv-integra.cod-tab = 'Clientes' AND
             giv-integra.conteudo = STRING(tt-clientes.cod-emit) NO-LOCK NO-ERROR.
        IF AVAIL giv-integra THEN
           ASSIGN tt-clientes.acao = giv-integra.acao
                  tt-clientes.integrado = IF giv-integra.acao = '' THEN YES ELSE NO.
    END.
    RUN pi-finalizar in h-acomp.

    {&OPEN-QUERY-br-clientes}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-emit-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-emit-ini D-Dialog
ON LEAVE OF fi-cod-emit-ini IN FRAME D-Dialog /* Cliente */
DO:
  ASSIGN fi-cod-emit-fin:SCREEN-VALUE = '9999999'.
  IF SELF:INPUT-VALUE <> 0 THEN
     ASSIGN fi-cod-emit-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-gr-cli-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-gr-cli-ini D-Dialog
ON LEAVE OF fi-gr-cli-ini IN FRAME D-Dialog /* Grupo de Clientes */
DO:
  ASSIGN fi-gr-cli-fin:SCREEN-VALUE = '999'.
  IF SELF:INPUT-VALUE <> 0 THEN
     ASSIGN fi-gr-cli-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
  DISPLAY fi-cod-emit-ini fi-cod-emit-fin fi-gr-cli-ini fi-gr-cli-fin 
          fi-nome-emit 
      WITH FRAME D-Dialog.
  ENABLE rt-buttom IMAGE-5 IMAGE-6 IMAGE-108 IMAGE-109 RECT-56 bt-sel 
         fi-cod-emit-ini fi-cod-emit-fin bt-dig-item bt-ex-item fi-gr-cli-ini 
         fi-gr-cli-fin bt-dig-ge bt-ex-ge fi-nome-emit br-clientes bt-ok 
         bt-cancela bt-ajuda 
      WITH FRAME D-Dialog.
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

  //{utp/ut9000.i "D99XX999" "9.99.99.999"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  APPLY 'ENTRY' TO fi-cod-emit-ini IN FRAME {&FRAME-NAME}.
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
  {src/adm/template/snd-list.i "tt-clientes"}

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

