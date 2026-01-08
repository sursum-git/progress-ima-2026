&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          espec            PROGRESS
          ems206           PROGRESS
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
{include/i-prgvrs.i ESSP0195B 2.04.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

DEF TEMP-TABLE tt-det-hist
    FIELD dt-manut     AS CHAR FORMAT "x(10)" 
    FIELD usuario      AS CHAR FORMAT "x(12)"
    FIELD cod-estabel  AS CHAR
    FIELD num-etiqueta LIKE ob-etiqueta.num-etiqueta
    FIELD localiz-ant  LIKE ob-etiqueta.localizacao
    FIELD it-codigo    LIKE ob-etiqueta.it-codigo
    FIELD cod-refer    LIKE ob-etiqueta.cod-refer
    FIELD lote         LIKE ob-etiqueta.nr-lote
    FIELD corte-comerc LIKE corte-comerc.codigo.

DEFINE TEMP-TABLE tt-work LIKE ob-etiqueta.

DEFINE INPUT PARAMETER TABLE FOR tt-det-hist.
DEFINE INPUT PARAMETER p-dt-manut AS CHAR FORMAT "x(10)".
DEFINE INPUT PARAMETER p-usuario  AS CHAR FORMAT "x(12)".



/* Local Variable Definitions ---                                       */

DEF VAR h-acomp      AS HANDLE NO-UNDO.
DEF VAR c-empresa    AS CHAR.
DEF VAR c-data       AS CHAR FORMAT "x(10)".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog
&Scoped-define BROWSE-NAME br-work

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-work ITEM corte-comerc

/* Definitions for BROWSE br-work                                       */
&Scoped-define FIELDS-IN-QUERY-br-work tt-work.it-codigo ITEM.desc-item tt-work.cod-refer tt-work.nr-lote corte-comerc.descricao tt-work.localizacao SUBSTR(tt-work.char-1,23, 6) tt-work.quantidade   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-work   
&Scoped-define SELF-NAME br-work
&Scoped-define OPEN-QUERY-br-work RUN pi-total. OPEN QUERY {&SELF-NAME} FOR EACH tt-work NO-LOCK, ~
                                   FIRST ITEM WHERE                                   ITEM.it-codigo = tt-work.it-codigo NO-LOCK, ~
                                   FIRST corte-comerc WHERE                                   corte-comerc.codigo = tt-work.corte-comerc NO-LOCK                               BY SUBSTR(tt-work.char-1, ~
      23, ~
        6)                               BY tt-work.it-codigo                               BY tt-work.num-etiqueta.   /*        FOR EACH tt-work NO-LOCK, ~
                                   FIRST ITEM WHERE                                   ITEM.it-codigo = tt-work.it-codigo NO-LOCK, ~
                                   FIRST corte-comerc WHERE                                   corte-comerc.codigo = tt-work.corte-comerc NO-LOCK                               BY SUBSTR(tt-work.char-1, ~
      23, ~
        6)                               BY tt-work.it-codigo                               BY tt-work.num-etiqueta.   */.
&Scoped-define TABLES-IN-QUERY-br-work tt-work ITEM corte-comerc
&Scoped-define FIRST-TABLE-IN-QUERY-br-work tt-work
&Scoped-define SECOND-TABLE-IN-QUERY-br-work ITEM
&Scoped-define THIRD-TABLE-IN-QUERY-br-work corte-comerc


/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-D-Dialog ~
    ~{&OPEN-QUERY-br-work}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-work rt-buttom fi-total bt-ajuda bt-ok ~
bt-cancela 
&Scoped-Define DISPLAYED-OBJECTS fi-total 

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

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "&OK" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE fi-total AS DECIMAL FORMAT ">>>>,>>9.99":R21 INITIAL 0 
     LABEL "Total Geral" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .79 NO-UNDO.

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 91.43 BY 1.42
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-work FOR 
      tt-work, 
      ITEM, 
      corte-comerc SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-work
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-work D-Dialog _FREEFORM
  QUERY br-work NO-LOCK DISPLAY
      tt-work.it-codigo      COLUMN-LABEL "Item"            WIDTH 6
      ITEM.desc-item         COLUMN-LABEL "Descri‡Æo"       WIDTH 28
      tt-work.cod-refer      COLUMN-LABEL "Referencia"      WIDTH 9
      tt-work.nr-lote        COLUMN-LABEL "Lote"            WIDTH 3
      corte-comerc.descricao COLUMN-LABEL "Corte Comercial" WIDTH 11
      tt-work.localizacao          FORMAT "999/999" COLUMN-LABEL "Local ATU" WIDTH 8.5
      SUBSTR(tt-work.char-1,23, 6) FORMAT "999/999" COLUMN-LABEL "Local ANT" WIDTH 8.5
      tt-work.quantidade     COLUMN-LABEL "Qtde (M)" WIDTH 9.8
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 91.43 BY 12.21
         FONT 1
         TITLE "Etiquetas das Localiza‡äes Manuais" ROW-HEIGHT-CHARS .6.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     br-work AT ROW 1.04 COL 1.57
     fi-total AT ROW 13.5 COL 77.43 COLON-ALIGNED
     bt-ajuda AT ROW 14.75 COL 82.29
     bt-ok AT ROW 14.79 COL 2.57
     bt-cancela AT ROW 14.79 COL 13.57
     rt-buttom AT ROW 14.54 COL 1.57
     SPACE(0.56) SKIP(0.24)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 1
         TITLE "Etiquetas Manuten‡Æo Localiza‡Æo - ESSP0195C"
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
   NOT-VISIBLE FRAME-NAME L-To-R                                        */
/* BROWSE-TAB br-work 1 D-Dialog */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-work
/* Query rebuild information for BROWSE br-work
     _START_FREEFORM
RUN pi-total.
OPEN QUERY {&SELF-NAME} FOR EACH tt-work NO-LOCK,
                            FIRST ITEM WHERE
                                  ITEM.it-codigo = tt-work.it-codigo NO-LOCK,
                            FIRST corte-comerc WHERE
                                  corte-comerc.codigo = tt-work.corte-comerc NO-LOCK
                              BY SUBSTR(tt-work.char-1,23,  6)
                              BY tt-work.it-codigo
                              BY tt-work.num-etiqueta.


/*



    FOR EACH tt-work NO-LOCK,
                            FIRST ITEM WHERE
                                  ITEM.it-codigo = tt-work.it-codigo NO-LOCK,
                            FIRST corte-comerc WHERE
                                  corte-comerc.codigo = tt-work.corte-comerc NO-LOCK
                              BY SUBSTR(tt-work.char-1,23,  6)
                              BY tt-work.it-codigo
                              BY tt-work.num-etiqueta.


*/
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-work */
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
ON WINDOW-CLOSE OF FRAME D-Dialog /* Etiquetas Manuten‡Æo Localiza‡Æo - ESSP0195C */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
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


&Scoped-define BROWSE-NAME br-work
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
  DISPLAY fi-total 
      WITH FRAME D-Dialog.
  ENABLE br-work rt-buttom fi-total bt-ajuda bt-ok bt-cancela 
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

  /* {utp/ut9000.i "ESSP0174A" "2.04.00.000"} */ 

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  /* Busca Nome da Empresa */
  FIND FIRST param-global NO-LOCK NO-ERROR.
  FIND FIRST empresa
       WHERE empresa.ep-codigo = param-global.empresa-prin NO-LOCK NO-ERROR. 
  ASSIGN c-empresa = (IF AVAIL empresa THEN empresa.razao-social ELSE "").

  RUN pi-processa.

  {&OPEN-QUERY-br-work}
  APPLY 'entry' TO br-work IN FRAME {&FRAME-NAME}.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-processa D-Dialog 
PROCEDURE pi-processa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

 RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
 {utp/ut-liter.i Gerando_Historico *}
 RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

 FOR EACH tt-det-hist WHERE
          tt-det-hist.dt-manut = p-dt-manut AND
          tt-det-hist.usuario  = p-usuario.
/*
     MESSAGE tt-det-hist.cod-estabel SKIP
             tt-det-hist.it-codigo SKIP
             tt-det-hist.cod-refer SKIP
             tt-det-hist.lote SKIP
             tt-det-hist.corte-comerc
         VIEW-AS ALERT-BOX INFO BUTTONS OK.  */

     FIND FIRST ob-etiqueta WHERE
          ob-etiqueta.cod-estabel  = tt-det-hist.cod-estabel AND
          ob-etiqueta.it-codigo = tt-det-hist.it-codigo AND
          ob-etiqueta.cod-refer = tt-det-hist.cod-refer AND
          SUBSTRING(ob-etiqueta.nr-lote,1,2) = tt-det-hist.lote AND
          ob-etiqueta.corte-comerc = tt-det-hist.corte-comerc  NO-LOCK NO-ERROR.
     IF AVAIL ob-etiqueta THEN DO.

         FIND tt-work WHERE
              tt-work.cod-estabel  = ob-etiqueta.cod-estabel AND
              tt-work.num-etiqueta = ob-etiqueta.num-etiqueta NO-LOCK NO-ERROR.
         IF NOT AVAIL tt-work THEN DO:
           
            CREATE tt-work.
            BUFFER-COPY ob-etiqueta TO tt-work.
         END.
     END.
     
 END.
 RUN pi-finalizar in h-acomp.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-total D-Dialog 
PROCEDURE pi-total :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 ASSIGN fi-total = 0.
 FOR EACH tt-work NO-LOCK.
     ASSIGN fi-total =  fi-total + tt-work.quantidade.
 END.
 DISP fi-total WITH FRAME {&FRAME-NAME}.

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
  {src/adm/template/snd-list.i "tt-work"}
  {src/adm/template/snd-list.i "ITEM"}
  {src/adm/template/snd-list.i "corte-comerc"}

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

