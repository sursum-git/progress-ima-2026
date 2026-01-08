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

/* Parameters Definitions ---                                           */
DEF INPUT PARAMETER p-row-table AS ROWID.

/* Local Variable Definitions ---                                       */
DEF TEMP-TABLE tt-pecas-fat
    FIELD num-etiqueta LIKE ob-etiqueta.num-etiqueta
    FIELD quantidade LIKE ob-etiqueta.quantidade.

DEF TEMP-TABLE tt-pecas-dev
    FIELD num-etiqueta LIKE ob-etiqueta.num-etiqueta
    FIELD quantidade LIKE ob-etiqueta.quantidade.

DEF VAR i-row AS INT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog
&Scoped-define BROWSE-NAME br-pecas-dev

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-pecas-dev tt-pecas-fat

/* Definitions for BROWSE br-pecas-dev                                  */
&Scoped-define FIELDS-IN-QUERY-br-pecas-dev tt-pecas-dev.num-etiqueta tt-pecas-dev.quantidade   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-pecas-dev   
&Scoped-define SELF-NAME br-pecas-dev
&Scoped-define QUERY-STRING-br-pecas-dev FOR EACH tt-pecas-dev NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-pecas-dev OPEN QUERY {&SELF-NAME} FOR EACH tt-pecas-dev NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-pecas-dev tt-pecas-dev
&Scoped-define FIRST-TABLE-IN-QUERY-br-pecas-dev tt-pecas-dev


/* Definitions for BROWSE br-pecas-fat                                  */
&Scoped-define FIELDS-IN-QUERY-br-pecas-fat tt-pecas-fat.num-etiqueta tt-pecas-fat.quantidade   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-pecas-fat   
&Scoped-define SELF-NAME br-pecas-fat
&Scoped-define QUERY-STRING-br-pecas-fat FOR EACH tt-pecas-fat NO-LOCK                                  BY tt-pecas-fat.num-etiqueta INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-pecas-fat OPEN QUERY {&SELF-NAME} FOR EACH tt-pecas-fat NO-LOCK                                  BY tt-pecas-fat.num-etiqueta INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-pecas-fat tt-pecas-fat
&Scoped-define FIRST-TABLE-IN-QUERY-br-pecas-fat tt-pecas-fat


/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-D-Dialog ~
    ~{&OPEN-QUERY-br-pecas-dev}~
    ~{&OPEN-QUERY-br-pecas-fat}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-br-pecas-dev rt-br-pecas-fat rt-buttom ~
rt-key-parent br-pecas-fat br-pecas-dev bt-add bt-del bt-ok bt-cancela ~
bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS fi-seq fi-it-codigo fi-desc-item ~
fi-cod-refer fi-qt-dev fi-tot-fat fi-tot-dev 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-add 
     IMAGE-UP FILE "adeicon\next-au":U
     IMAGE-INSENSITIVE FILE "adeicon\next-ai":U
     LABEL "" 
     SIZE 7 BY 1.

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
     SIZE 7 BY 1.

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "&OK" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE fi-cod-refer AS CHARACTER FORMAT "X(256)":U 
     LABEL "Referˆncia" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-item AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-it-codigo AS CHARACTER FORMAT "X(8)":U 
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-qt-dev AS DECIMAL FORMAT "-ZZ,ZZZ,ZZ9.9999":U INITIAL 0 
     LABEL "Quantidade" 
     VIEW-AS FILL-IN 
     SIZE 10.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-seq AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Seq" 
     VIEW-AS FILL-IN 
     SIZE 5.43 BY .88 NO-UNDO.

DEFINE VARIABLE fi-tot-dev AS DECIMAL FORMAT "-ZZZ,ZZZ,ZZ9.99":U INITIAL 0 
     LABEL "Qtd Total Devolvida" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .79 NO-UNDO.

DEFINE VARIABLE fi-tot-fat AS DECIMAL FORMAT "-ZZZ,ZZZ,ZZ9.99":U INITIAL 0 
     LABEL "Qtd Total Faturada" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 NO-UNDO.

DEFINE RECTANGLE rt-br-pecas-dev
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 33 BY 9.58.

DEFINE RECTANGLE rt-br-pecas-fat
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 33.14 BY 9.5.

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 75 BY 1.42
     BGCOLOR 7 .

DEFINE RECTANGLE rt-key-parent
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 75 BY 3.5.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-pecas-dev FOR 
      tt-pecas-dev SCROLLING.

DEFINE QUERY br-pecas-fat FOR 
      tt-pecas-fat SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-pecas-dev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-pecas-dev D-Dialog _FREEFORM
  QUERY br-pecas-dev DISPLAY
      tt-pecas-dev.num-etiqueta  COLUMN-LABEL "Numero ID" WIDTH 10
       tt-pecas-dev.quantidade COLUMN-LABEL "Quantidade"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 30.57 BY 7.75
         FONT 1
         TITLE "Pe‡as Devolvidas" FIT-LAST-COLUMN.

DEFINE BROWSE br-pecas-fat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-pecas-fat D-Dialog _FREEFORM
  QUERY br-pecas-fat NO-LOCK DISPLAY
      tt-pecas-fat.num-etiqueta  COLUMN-LABEL "Numero ID" WIDTH 10
   tt-pecas-fat.quantidade COLUMN-LABEL "Quantidade"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 30.72 BY 7.75
         FONT 1
         TITLE "Pe‡as Faturadas" FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     fi-seq AT ROW 1.5 COL 10.57 COLON-ALIGNED
     fi-it-codigo AT ROW 2.5 COL 10.57 COLON-ALIGNED
     fi-desc-item AT ROW 2.5 COL 25 COLON-ALIGNED NO-LABEL
     fi-cod-refer AT ROW 3.5 COL 10.57 COLON-ALIGNED
     fi-qt-dev AT ROW 3.5 COL 60 COLON-ALIGNED
     br-pecas-fat AT ROW 5.25 COL 3.29
     br-pecas-dev AT ROW 5.25 COL 45
     bt-add AT ROW 7.5 COL 36
     bt-del AT ROW 9.13 COL 36
     fi-tot-fat AT ROW 13.25 COL 18 COLON-ALIGNED
     fi-tot-dev AT ROW 13.25 COL 62.14 COLON-ALIGNED
     bt-ok AT ROW 14.83 COL 3
     bt-cancela AT ROW 14.83 COL 14
     bt-ajuda AT ROW 14.83 COL 66.14
     rt-br-pecas-dev AT ROW 4.92 COL 44
     rt-br-pecas-fat AT ROW 4.92 COL 1.86
     rt-buttom AT ROW 14.58 COL 2
     rt-key-parent AT ROW 1.25 COL 2
     SPACE(1.13) SKIP(11.49)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 1
         TITLE "Pe‡as Devolvidas de um Item - UPC-RE1001C1"
         DEFAULT-BUTTON bt-ok.


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
/* BROWSE-TAB br-pecas-fat fi-qt-dev D-Dialog */
/* BROWSE-TAB br-pecas-dev br-pecas-fat D-Dialog */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fi-cod-refer IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-item IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-it-codigo IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qt-dev IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-seq IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-dev IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-fat IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-pecas-dev
/* Query rebuild information for BROWSE br-pecas-dev
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-pecas-dev NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-pecas-dev */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-pecas-fat
/* Query rebuild information for BROWSE br-pecas-fat
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-pecas-fat NO-LOCK
                                 BY tt-pecas-fat.num-etiqueta INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-pecas-fat */
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
ON WINDOW-CLOSE OF FRAME D-Dialog /* Pe‡as Devolvidas de um Item - UPC-RE1001C1 */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-add D-Dialog
ON CHOOSE OF bt-add IN FRAME D-Dialog
DO:
   DO i-row = 1 TO br-pecas-fat:NUM-SELECTED-ROWS:
      IF br-pecas-fat:FETCH-SELECTED-ROW(i-row) THEN DO.
         CREATE tt-pecas-dev.
         ASSIGN tt-pecas-dev.num-etiqueta = tt-pecas-fat.num-etiqueta
                tt-pecas-dev.quantidade = tt-pecas-fat.quantidade.

         ASSIGN fi-tot-fat = fi-tot-fat - tt-pecas-fat.quantidade
                fi-tot-dev = fi-tot-dev + tt-pecas-fat.quantidade.

         DELETE tt-pecas-fat.
      END.
   END.

   {&OPEN-QUERY-br-pecas-fat}
   {&OPEN-QUERY-br-pecas-dev}

   ASSIGN fi-tot-fat:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(fi-tot-fat)
          fi-tot-dev:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(fi-tot-dev).
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
   DO i-row = 1 TO br-pecas-dev:NUM-SELECTED-ROWS:
      IF br-pecas-dev:FETCH-SELECTED-ROW(i-row) THEN DO.
         CREATE tt-pecas-fat.
         ASSIGN tt-pecas-fat.num-etiqueta = tt-pecas-dev.num-etiqueta
                tt-pecas-fat.quantidade = tt-pecas-dev.quantidade.

         ASSIGN fi-tot-fat = fi-tot-fat + tt-pecas-dev.quantidade
                fi-tot-dev = fi-tot-dev - tt-pecas-dev.quantidade.

         DELETE tt-pecas-dev.
      END.
   END.

   {&OPEN-QUERY-br-pecas-fat}
   {&OPEN-QUERY-br-pecas-dev}

   ASSIGN fi-tot-fat:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(fi-tot-fat)
          fi-tot-dev:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(fi-tot-dev).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok D-Dialog
ON CHOOSE OF bt-ok IN FRAME D-Dialog /* OK */
DO:
   FOR EACH tt-pecas-fat.
       FOR EACH ped-item-rom WHERE
                ped-item-rom.nr-pedcli = nota-fiscal.nr-pedcli AND
                ped-item-rom.nome-abrev = nota-fiscal.nome-ab-cli AND
                ped-item-rom.num-etiqueta = tt-pecas-fat.num-etiqueta 
                EXCLUSIVE-LOCK.
           ASSIGN ped-item-rom.marca = ''.
       END.
   END.

   FOR EACH tt-pecas-dev.
       FOR EACH ped-item-rom WHERE
                ped-item-rom.nr-pedcli = nota-fiscal.nr-pedcli AND
                ped-item-rom.nome-abrev = nota-fiscal.nome-ab-cli AND
                ped-item-rom.num-etiqueta = tt-pecas-dev.num-etiqueta
                EXCLUSIVE-LOCK.

           ASSIGN ped-item-rom.marca = 'DEV'.
       END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-pecas-dev
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
  DISPLAY fi-seq fi-it-codigo fi-desc-item fi-cod-refer fi-qt-dev fi-tot-fat 
          fi-tot-dev 
      WITH FRAME D-Dialog.
  ENABLE rt-br-pecas-dev rt-br-pecas-fat rt-buttom rt-key-parent br-pecas-fat 
         br-pecas-dev bt-add bt-del bt-ok bt-cancela bt-ajuda 
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

  FIND item-doc-est WHERE
       ROWID(item-doc-est) = p-row-table NO-LOCK NO-ERROR.

  IF NOT AVAIL item-doc-est THEN RETURN 'NOK'.

  FIND docum-est OF item-doc-est NO-LOCK NO-ERROR.

  FIND item WHERE
       item.it-codigo = item-doc-est.it-codigo NO-LOCK NO-ERROR.

  ASSIGN fi-seq:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(item-doc-est.sequencia)
         fi-it-codigo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item-doc-est.it-codigo
         fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item
         fi-cod-refer:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item-doc-est.cod-refer
         fi-qt-dev:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(item-doc-est.quantidade).

  RUN pi-popula-browse.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-popula-browse D-Dialog 
PROCEDURE pi-popula-browse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    FIND nota-fiscal WHERE
         nota-fiscal.cod-estabel = docum-est.cod-estabel AND
         nota-fiscal.serie = item-doc-est.serie-comp AND
         nota-fiscal.nr-nota-fis = item-doc-est.nro-comp
         NO-LOCK NO-ERROR.
    /*
    FIND it-nota-fisc OF nota-fiscal WHERE
         it-nota-fisc.nr-seq-fat = item-doc-est.seq-comp AND
         it-nota-fisc.it-codigo = item-doc-est.it-codigo
         NO-LOCK NO-ERROR. 
    */
    ASSIGN fi-tot-fat = 0.
    FOR EACH it-nota-fisc OF nota-fiscal WHERE
             it-nota-fisc.it-codigo = item-doc-est.it-codigo AND
             it-nota-fisc.cod-refer = item-doc-est.cod-refer 
             NO-LOCK. 

        FOR EACH ped-item-res WHERE 
                 ped-item-res.nome-abrev   = nota-fiscal.nome-ab-cli AND 
                 ped-item-res.nr-pedcli    = it-nota-fisc.nr-pedcli AND
                 ped-item-res.it-codigo    = it-nota-fisc.it-codigo AND 
                 ped-item-res.nr-sequencia = it-nota-fisc.nr-seq-ped
                 NO-LOCK.
    
           FOR EACH ped-item-rom WHERE
                    ped-item-rom.nome-abrev = ped-item-res.nome-abrev AND
                    ped-item-rom.nr-pedcli = ped-item-res.nr-pedcli AND
                    ped-item-rom.nr-sequencia = ped-item-res.nr-sequencia NO-LOCK.
               FIND ob-etiqueta WHERE
                    ob-etiqueta.cod-estabel = ped-item-rom.cod-estabel AND
                    ob-etiqueta.num-etiqueta = ped-item-rom.num-etiqueta
                    EXCLUSIVE-LOCK NO-ERROR.
    
               IF AVAIL ob-etiqueta THEN DO.
                  IF ped-item-rom.marca = 'DEV' THEN DO.
                      CREATE tt-pecas-dev.
                      ASSIGN tt-pecas-dev.num-etiqueta = ped-item-rom.num-etiqueta
                             tt-pecas-dev.quantidade = ped-item-rom.quantidade.
    
                      ASSIGN fi-tot-dev = fi-tot-dev + ped-item-rom.quantidade.
                  END.
                  ELSE DO.
                      CREATE tt-pecas-fat.
                      ASSIGN tt-pecas-fat.num-etiqueta = ped-item-rom.num-etiqueta
                             tt-pecas-fat.quantidade = ped-item-rom.quantidade.
    
                      ASSIGN fi-tot-fat = fi-tot-fat + ped-item-rom.quantidade.
                  END.
               END.
           END.
        END.
    END.

    {&OPEN-QUERY-br-pecas-fat}
    {&OPEN-QUERY-br-pecas-dev}

    ASSIGN fi-tot-fat:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(fi-tot-fat)
           fi-tot-dev:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(fi-tot-dev).

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
  {src/adm/template/snd-list.i "tt-pecas-fat"}
  {src/adm/template/snd-list.i "tt-pecas-dev"}

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

