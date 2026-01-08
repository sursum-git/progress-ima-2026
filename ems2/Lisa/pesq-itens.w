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
DEF TEMP-TABLE tt-itens LIKE item
    FIELD integrado AS LOGICAL
    FIELD log-erro-integr AS LOGICAL
    FIELD acao AS CHAR.

DEF TEMP-TABLE tt-digita
    FIELD opcao  AS CHAR
    FIELD campo AS CHAR
    FIELD valor AS CHAR.

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEF VAR i-cor AS INT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog
&Scoped-define BROWSE-NAME br-itens

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-itens

/* Definitions for BROWSE br-itens                                      */
&Scoped-define FIELDS-IN-QUERY-br-itens tt-itens.it-codigo tt-itens.desc-item   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-itens   
&Scoped-define SELF-NAME br-itens
&Scoped-define QUERY-STRING-br-itens FOR EACH tt-itens NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-itens OPEN QUERY {&SELF-NAME} FOR EACH tt-itens NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-itens tt-itens
&Scoped-define FIRST-TABLE-IN-QUERY-br-itens tt-itens


/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-D-Dialog ~
    ~{&OPEN-QUERY-br-itens}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-buttom IMAGE-5 IMAGE-6 IMAGE-108 ~
IMAGE-109 IMAGE-110 IMAGE-111 RECT-56 bt-dig-item bt-ex-item bt-sel ~
fi-it-codigo-ini fi-it-codigo-fin bt-dig-item-2 bt-ex-item-2 ~
fi-fm-codigo-ini fi-fm-codigo-fin bt-dig-item-3 bt-ex-item-3 ~
fi-ge-codigo-ini fi-ge-codigo-fin fi-desc-item br-itens bt-ok bt-cancela ~
bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS fi-it-codigo-ini fi-it-codigo-fin ~
fi-fm-codigo-ini fi-fm-codigo-fin fi-ge-codigo-ini fi-ge-codigo-fin ~
fi-desc-item 

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

DEFINE BUTTON bt-dig-item 
     IMAGE-UP FILE "image/im-ran.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-ran.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Digita Itens".

DEFINE BUTTON bt-dig-item-2 
     IMAGE-UP FILE "image/im-ran.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-ran.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Digita Itens".

DEFINE BUTTON bt-dig-item-3 
     IMAGE-UP FILE "image/im-ran.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-ran.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Digita Itens".

DEFINE BUTTON bt-ex-item 
     IMAGE-UP FILE "image/imt-retira.bmp":U
     IMAGE-INSENSITIVE FILE "image/iit-retira.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Exceto Itens".

DEFINE BUTTON bt-ex-item-2 
     IMAGE-UP FILE "image/imt-retira.bmp":U
     IMAGE-INSENSITIVE FILE "image/iit-retira.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Exceto Itens".

DEFINE BUTTON bt-ex-item-3 
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
     SIZE 4.86 BY 4 TOOLTIP "Salva Altera‡äes".

DEFINE VARIABLE fi-desc-item AS CHARACTER FORMAT "X(256)":U 
     LABEL "Descri‡ao" 
     VIEW-AS FILL-IN 
     SIZE 49 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fm-codigo-fin AS CHARACTER FORMAT "x(16)" INITIAL "ZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fm-codigo-ini AS CHARACTER FORMAT "x(16)" 
     LABEL "Fam¡lia" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ge-codigo-fin AS INTEGER FORMAT ">>9" INITIAL 999 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ge-codigo-ini AS INTEGER FORMAT ">>9" INITIAL 0 
     LABEL "Grupo Estoque" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-it-codigo-fin AS CHARACTER FORMAT "x(16)" INITIAL "ZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 TOOLTIP "Item final." NO-UNDO.

DEFINE VARIABLE fi-it-codigo-ini AS CHARACTER FORMAT "x(16)" 
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

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

DEFINE RECTANGLE RECT-56
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 69 BY 4.5.

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 69 BY 1.42
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-itens FOR 
      tt-itens SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-itens
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-itens D-Dialog _FREEFORM
  QUERY br-itens NO-LOCK DISPLAY
      tt-itens.it-codigo FORMAT "x(16)":U
      tt-itens.desc-item FORMAT "x(60)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 69 BY 10.75
         FONT 1
         TITLE "Produtos Selecionados" FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     bt-dig-item AT ROW 1.42 COL 55 WIDGET-ID 2
     bt-ex-item AT ROW 1.42 COL 60 WIDGET-ID 4
     bt-sel AT ROW 1.42 COL 65 WIDGET-ID 42
     fi-it-codigo-ini AT ROW 1.5 COL 13 COLON-ALIGNED HELP
          "Codigo do Ötem no EMS" WIDGET-ID 8
     fi-it-codigo-fin AT ROW 1.5 COL 37 COLON-ALIGNED HELP
          "Codigo do Ötem no EMS" NO-LABEL WIDGET-ID 6
     bt-dig-item-2 AT ROW 2.42 COL 55 WIDGET-ID 14
     bt-ex-item-2 AT ROW 2.42 COL 60 WIDGET-ID 16
     fi-fm-codigo-ini AT ROW 2.5 COL 13 COLON-ALIGNED HELP
          "Codigo do Ötem no EMS" WIDGET-ID 20
     fi-fm-codigo-fin AT ROW 2.5 COL 37 COLON-ALIGNED HELP
          "Codigo do Ötem no EMS" NO-LABEL WIDGET-ID 18
     bt-dig-item-3 AT ROW 3.42 COL 55 WIDGET-ID 26
     bt-ex-item-3 AT ROW 3.42 COL 60 WIDGET-ID 28
     fi-ge-codigo-ini AT ROW 3.5 COL 13 COLON-ALIGNED HELP
          "Codigo do Ötem no EMS" WIDGET-ID 32
     fi-ge-codigo-fin AT ROW 3.5 COL 37 COLON-ALIGNED HELP
          "Codigo do Ötem no EMS" NO-LABEL WIDGET-ID 30
     fi-desc-item AT ROW 4.5 COL 13 COLON-ALIGNED WIDGET-ID 38
     br-itens AT ROW 6 COL 2 WIDGET-ID 200
     bt-ok AT ROW 17.25 COL 3
     bt-cancela AT ROW 17.25 COL 14
     bt-ajuda AT ROW 17.25 COL 60.29
     rt-buttom AT ROW 17 COL 2
     IMAGE-5 AT ROW 1.5 COL 31 WIDGET-ID 10
     IMAGE-6 AT ROW 1.5 COL 35.43 WIDGET-ID 12
     IMAGE-108 AT ROW 2.58 COL 31 WIDGET-ID 22
     IMAGE-109 AT ROW 2.58 COL 35.43 WIDGET-ID 24
     IMAGE-110 AT ROW 3.58 COL 31 WIDGET-ID 34
     IMAGE-111 AT ROW 3.58 COL 35.43 WIDGET-ID 36
     RECT-56 AT ROW 1.25 COL 2 WIDGET-ID 40
     SPACE(0.99) SKIP(12.78)
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
   FRAME-NAME L-To-R                                                    */
/* BROWSE-TAB br-itens fi-desc-item D-Dialog */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-itens
/* Query rebuild information for BROWSE br-itens
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-itens NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-itens */
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


&Scoped-define BROWSE-NAME br-itens
&Scoped-define SELF-NAME br-itens
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-itens D-Dialog
ON ROW-DISPLAY OF br-itens IN FRAME D-Dialog /* Produtos Selecionados */
DO:
   ASSIGN i-cor = ?.

   IF tt-itens.acao <> '' THEN
       ASSIGN i-cor = 9.

   ELSE IF tt-itens.log-erro-integr THEN
       ASSIGN i-cor = 12.
   ELSE IF tt-itens.integrado THEN
      ASSIGN i-cor = 2.

   ASSIGN tt-itens.it-codigo:FGCOLOR IN BROWSE br-itens = i-cor. 
          tt-itens.desc-item:FGCOLOR IN BROWSE br-itens = i-cor.  

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


&Scoped-define SELF-NAME bt-dig-item-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-dig-item-2 D-Dialog
ON CHOOSE OF bt-dig-item-2 IN FRAME D-Dialog
DO:
    RUN esdlg/d01-digita.w (INPUT-OUTPUT TABLE tt-digita, 
                            INPUT "D",
                            INPUT "Item").

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-dig-item-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-dig-item-3 D-Dialog
ON CHOOSE OF bt-dig-item-3 IN FRAME D-Dialog
DO:
    RUN esdlg/d01-digita.w (INPUT-OUTPUT TABLE tt-digita, 
                            INPUT "D",
                            INPUT "Item").

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


&Scoped-define SELF-NAME bt-ex-item-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ex-item-2 D-Dialog
ON CHOOSE OF bt-ex-item-2 IN FRAME D-Dialog
DO:
    RUN esdlg/d01-digita.w (INPUT-OUTPUT TABLE tt-digita, 
                              INPUT "E",
                              INPUT "Item").

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ex-item-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ex-item-3 D-Dialog
ON CHOOSE OF bt-ex-item-3 IN FRAME D-Dialog
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
   FOR EACH tt-itens.
       FIND giv-integra WHERE
            giv-integra.cod-tab = 'Produtos' AND
            giv-integra.conteudo = tt-itens.it-codigo NO-LOCK NO-ERROR.
       IF NOT AVAIL giv-integra THEN DO.
          CREATE giv-integra.
          ASSIGN giv-integra.cod-tab = 'Produtos'
                 giv-integra.conteudo = tt-itens.it-codigo
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
    ASSIGN INPUT FRAME {&FRAME-NAME} fi-it-codigo-ini 
                                     fi-it-codigo-fin
                                     fi-fm-codigo-ini     
                                     fi-fm-codigo-fin
                                     fi-ge-codigo-ini    
                                     fi-ge-codigo-fin
                                     fi-desc-item.

    EMPTY TEMP-TABLE tt-itens.
    FOR EACH item WHERE
             item.it-codigo >= fi-it-codigo-ini AND
             item.it-codigo <= fi-it-codigo-fin AND
             item.fm-codigo >= fi-fm-codigo-ini AND
             item.fm-codigo <= fi-fm-codigo-fin AND
             item.ge-codigo >= fi-ge-codigo-ini AND
             item.ge-codigo <= fi-ge-codigo-fin AND 
             (item.desc-item BEGINS fi-desc-item OR fi-desc-item = '')
             NO-LOCK.

        CREATE tt-itens.
        BUFFER-COPY item TO tt-itens.

        FIND giv-integra WHERE
             giv-integra.cod-tab = 'PRODUTOS' AND
             giv-integra.conteudo = tt-itens.it-codigo NO-LOCK NO-ERROR.
        IF AVAIL giv-integra THEN DO.
           ASSIGN tt-itens.acao = giv-integra.acao.
           ASSIGN tt-itens.integrado = IF giv-integra.acao = '' THEN YES ELSE NO.
        END.

    END.

    {&OPEN-QUERY-br-itens}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-fm-codigo-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fm-codigo-ini D-Dialog
ON LEAVE OF fi-fm-codigo-ini IN FRAME D-Dialog /* Fam¡lia */
DO:
  IF SELF:SCREEN-VALUE <> '' THEN
     ASSIGN fi-it-codigo-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ge-codigo-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ge-codigo-ini D-Dialog
ON LEAVE OF fi-ge-codigo-ini IN FRAME D-Dialog /* Grupo Estoque */
DO:
  IF SELF:INPUT-VALUE <> 0 THEN
     ASSIGN fi-it-codigo-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
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
  DISPLAY fi-it-codigo-ini fi-it-codigo-fin fi-fm-codigo-ini fi-fm-codigo-fin 
          fi-ge-codigo-ini fi-ge-codigo-fin fi-desc-item 
      WITH FRAME D-Dialog.
  ENABLE rt-buttom IMAGE-5 IMAGE-6 IMAGE-108 IMAGE-109 IMAGE-110 IMAGE-111 
         RECT-56 bt-dig-item bt-ex-item bt-sel fi-it-codigo-ini 
         fi-it-codigo-fin bt-dig-item-2 bt-ex-item-2 fi-fm-codigo-ini 
         fi-fm-codigo-fin bt-dig-item-3 bt-ex-item-3 fi-ge-codigo-ini 
         fi-ge-codigo-fin fi-desc-item br-itens bt-ok bt-cancela bt-ajuda 
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

  //{utp/ut9000.i "D99XX999" "9.99.99.999"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  APPLY 'ENTRY' TO fi-it-codigo-ini IN FRAME {&FRAME-NAME}.
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
  {src/adm/template/snd-list.i "tt-itens"}

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

