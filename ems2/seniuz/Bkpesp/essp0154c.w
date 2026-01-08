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

/* Local Variable Definitions ---                                       */

DEF TEMP-TABLE tt-etq-lidas
    FIELD i-lin        AS INT
    FIELD num-etiqueta LIKE ob-etiqueta.num-etiqueta.

DEF OUTPUT PARAMETER TABLE FOR tt-etq-lidas.

DEF VAR c-arq-etq AS CHAR.
DEF VAR i-digito  AS INT.
DEF VAR i-ct      AS INT.

{esinc/sz-pcl.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog
&Scoped-define BROWSE-NAME br-etq-lidas

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-etq-lidas

/* Definitions for BROWSE br-etq-lidas                                  */
&Scoped-define FIELDS-IN-QUERY-br-etq-lidas tt-etq-lidas.i-lin tt-etq-lidas.num-etiqueta   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-etq-lidas   
&Scoped-define SELF-NAME br-etq-lidas
&Scoped-define QUERY-STRING-br-etq-lidas FOR EACH tt-etq-lidas NO-LOCK                                  BY tt-etq-lidas.i-lin DESCENDING INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-etq-lidas OPEN QUERY {&SELF-NAME} FOR EACH tt-etq-lidas NO-LOCK                                  BY tt-etq-lidas.i-lin DESCENDING INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-etq-lidas tt-etq-lidas
&Scoped-define FIRST-TABLE-IN-QUERY-br-etq-lidas tt-etq-lidas


/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-D-Dialog ~
    ~{&OPEN-QUERY-br-etq-lidas}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-buttom RECT-51 bt-conf bt-importa ~
fi-num-etiqueta br-etq-lidas bt-exclui bt-ok bt-cancela 
&Scoped-Define DISPLAYED-OBJECTS fi-num-etiqueta 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-4 bt-conf bt-importa bt-exclui 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-cancela AUTO-END-KEY 
     LABEL "&Cancelar" 
     SIZE 11 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-conf AUTO-GO 
     IMAGE-UP FILE "image/im-chck1.bmp":U
     LABEL "Btn 2" 
     SIZE 4 BY 1 TOOLTIP "Confirma Etiquetas Lidas".

DEFINE BUTTON bt-exclui 
     IMAGE-UP FILE "image/im-era.bmp":U
     LABEL "Elimina" 
     SIZE 4 BY 1.21 TOOLTIP "Exclui Etiquetas Lidas".

DEFINE BUTTON bt-importa AUTO-GO 
     IMAGE-UP FILE "image/im-ascii.bmp":U
     LABEL "Btn 2" 
     SIZE 4 BY 1 TOOLTIP "Importa Etiquetas Lidas".

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "Confirmar" 
     SIZE 11 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE fi-num-etiqueta AS CHARACTER FORMAT "x(10)":U 
     LABEL "Etiqueta" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88
     FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-51
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 40 BY 11.

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 40 BY 1.58
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-etq-lidas FOR 
      tt-etq-lidas SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-etq-lidas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-etq-lidas D-Dialog _FREEFORM
  QUERY br-etq-lidas NO-LOCK DISPLAY
      tt-etq-lidas.i-lin        FORMAT ">>>9"            COLUMN-LABEL "Qt" 
      tt-etq-lidas.num-etiqueta FORMAT "999999999":U     COLUMN-LABEL "Etiqueta"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 36 BY 9.5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     bt-conf AT ROW 1.46 COL 31.57 WIDGET-ID 4
     bt-importa AT ROW 1.46 COL 36 WIDGET-ID 16
     fi-num-etiqueta AT ROW 1.5 COL 11 COLON-ALIGNED WIDGET-ID 12
     br-etq-lidas AT ROW 2.58 COL 4 WIDGET-ID 200
     bt-exclui AT ROW 12.63 COL 37 WIDGET-ID 2
     bt-ok AT ROW 12.67 COL 3
     bt-cancela AT ROW 12.67 COL 15
     rt-buttom AT ROW 12.42 COL 2
     RECT-51 AT ROW 1.25 COL 2 WIDGET-ID 14
     SPACE(0.71) SKIP(1.91)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Separa Etqiuetas pelo Leitor - ESSP0154C"
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
/* BROWSE-TAB br-etq-lidas fi-num-etiqueta D-Dialog */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON bt-conf IN FRAME D-Dialog
   4                                                                    */
/* SETTINGS FOR BUTTON bt-exclui IN FRAME D-Dialog
   4                                                                    */
/* SETTINGS FOR BUTTON bt-importa IN FRAME D-Dialog
   4                                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-etq-lidas
/* Query rebuild information for BROWSE br-etq-lidas
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-etq-lidas NO-LOCK
                                 BY tt-etq-lidas.i-lin DESCENDING INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-etq-lidas */
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
ON WINDOW-CLOSE OF FRAME D-Dialog /* Separa Etqiuetas pelo Leitor - ESSP0154C */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cancela
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancela D-Dialog
ON CHOOSE OF bt-cancela IN FRAME D-Dialog /* Cancelar */
DO:
   OS-DELETE SILENT VALUE(c-arq-etq).
    /*
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
      */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-conf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-conf D-Dialog
ON CHOOSE OF bt-conf IN FRAME D-Dialog /* Btn 2 */
DO:
    IF fi-num-etiqueta:SCREEN-VALUE <> '' THEN DO.
       IF i-digito <> INT(fn-calc-digito(INPUT fi-num-etiqueta:SCREEN-VALUE)) THEN DO.
          MESSAGE "Somente Leitura com o Coletor Ç Permitido ! ! !"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
          APPLY 'entry' TO fi-num-etiqueta.
          RETURN NO-APPLY.
       END.
       
       ASSIGN i-ct = i-ct + 1.

       CREATE tt-etq-lidas.
       ASSIGN tt-etq-lidas.i-lin = i-ct
              tt-etq-lidas.num-etiqueta = INT(SUBSTR(fi-num-etiqueta:SCREEN-VALUE,1,9)).
    
       OUTPUT TO VALUE(c-arq-etq) APPEND.
          EXPORT tt-etq-lidas.
       OUTPUT CLOSE.

       {&OPEN-QUERY-br-etq-lidas}

       ASSIGN fi-num-etiqueta:SCREEN-VALUE = ''.
       APPLY 'entry' TO fi-num-etiqueta.
       RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-exclui
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-exclui D-Dialog
ON CHOOSE OF bt-exclui IN FRAME D-Dialog /* Elimina */
DO:
    DELETE tt-etq-lidas.
    {&OPEN-QUERY-br-etq-lidas}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-importa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-importa D-Dialog
ON CHOOSE OF bt-importa IN FRAME D-Dialog /* Btn 2 */
DO:
    IF SEARCH(c-arq-etq) = ? THEN DO.
       MESSAGE "N∆o foi Encontrado Arquivo com peáas Lidas"
           VIEW-AS ALERT-BOX ERROR BUTTONS OK.
       RETURN NO-APPLY.
    END.

    INPUT FROM VALUE(c-arq-etq).
    REPEAT.
        CREATE tt-etq-lidas.
        IMPORT tt-etq-lidas.

        ASSIGN i-ct = i-ct + 1.
    END.
    INPUT CLOSE.
       
    {&OPEN-QUERY-br-etq-lidas}

    ASSIGN fi-num-etiqueta:SCREEN-VALUE = ''.
    APPLY 'entry' TO fi-num-etiqueta.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok D-Dialog
ON CHOOSE OF bt-ok IN FRAME D-Dialog /* Confirmar */
DO:
  FOR EACH tt-etq-lidas NO-LOCK.
      FIND ob-etiqueta WHERE
           ob-etiqueta.cod-estabel  = ped-venda.cod-estabel AND
           ob-etiqueta.num-etiqueta = tt-etq-lidas.num-etiqueta
           USE-INDEX indice4 SHARE-LOCK NO-ERROR.
    
      IF NOT AVAIL ob-etiqueta THEN DO.
         MESSAGE 'Etiqueta' tt-etq-lidas.num-etiqueta ' n∆o Cadastrada no Sistema...' SKIP
                 'Ser† Desconsiderada...'
              VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         DELETE tt-etq-lidas.
      END.
  END.
  OS-DELETE SILENT VALUE(c-arq-etq).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-num-etiqueta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-num-etiqueta D-Dialog
ON RETURN OF fi-num-etiqueta IN FRAME D-Dialog /* Etiqueta */
DO:
   APPLY 'CHOOSE' TO bt-conf.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-num-etiqueta D-Dialog
ON VALUE-CHANGED OF fi-num-etiqueta IN FRAME D-Dialog /* Etiqueta */
DO:
  IF SELF:SCREEN-VALUE <> '' AND
     (SUBSTR(SELF:SCREEN-VALUE,LENGTH(SELF:SCREEN-VALUE),1) < '0' OR
     SUBSTR(SELF:SCREEN-VALUE,LENGTH(SELF:SCREEN-VALUE),1) > '9') THEN DO.
     BELL.
     APPLY 'backspace' TO SELF.
     RETURN NO-APPLY.
  END.

  ASSIGN i-digito = 99.
  IF LENGTH(SELF:SCREEN-VALUE) = 10 THEN DO.
     ASSIGN i-digito = INT(SUBSTRING(SELF:SCREEN-VALUE,10,1)).
     ASSIGN SELF:SCREEN-VALUE = SUBSTR(SELF:SCREEN-VALUE,1,9).
     APPLY 'END' TO SELF.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-etq-lidas
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
  DISPLAY fi-num-etiqueta 
      WITH FRAME D-Dialog.
  ENABLE rt-buttom RECT-51 bt-conf bt-importa fi-num-etiqueta br-etq-lidas 
         bt-exclui bt-ok bt-cancela 
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
  FIND FIRST ped-venda NO-LOCK NO-ERROR.

  ASSIGN c-arq-etq = SESSION:TEMP-DIRECTORY + "etq-lidas.txt".

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
  {src/adm/template/snd-list.i "tt-etq-lidas"}

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

