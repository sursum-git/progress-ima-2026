&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
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
DEFINE OUTPUT PARAMETER i-saida      AS INT.
DEFINE OUTPUT PARAMETER c-saida      AS CHAR.
DEFINE OUTPUT PARAMETER i-num-copias AS INT.
DEFINE OUTPUT PARAMETER l-ok         AS LOG.

/* Local Variable Definitions ---                                       */
DEF VAR c-imp-old AS CHAR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-buttom bt-ok bt-cancela bt-ajuda 

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

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 49 BY 1.42
     BGCOLOR 7 .

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
     SIZE 40 BY .88 NO-UNDO.

DEFINE VARIABLE fi-copias AS INTEGER FORMAT ">>>9":U INITIAL 1 
     LABEL "N£mero de C¢pias" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE rs-destino AS INTEGER INITIAL 3 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Impressora", 1,
"Arquivo", 2,
"Terminal", 3
     SIZE 44 BY 1.08 NO-UNDO.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 49 BY 4.42.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     bt-ok AT ROW 6.25 COL 2
     bt-cancela AT ROW 6.25 COL 12.43
     bt-ajuda AT ROW 6.25 COL 39.29
     rt-buttom AT ROW 6 COL 1
     SPACE(0.42) SKIP(0.45)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 1
         TITLE "Sa°da de Impress∆o"
         DEFAULT-BUTTON bt-ok.

DEFINE FRAME f-pg-imp
     rs-destino AT ROW 1.63 COL 3.29 HELP
          "Destino de Impress∆o do Relat¢rio" NO-LABEL
     bt-config-impr AT ROW 2.67 COL 43.43 HELP
          "Configuraá∆o da impressora"
     bt-arquivo AT ROW 2.67 COL 43.43 HELP
          "Escolha do nome do arquivo"
     c-arquivo AT ROW 2.75 COL 3.29 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL
     fi-copias AT ROW 4.38 COL 15 COLON-ALIGNED
     " Destino" VIEW-AS TEXT
          SIZE 6 BY .5 AT ROW 1.08 COL 3.29
     RECT-8 AT ROW 1.33 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 49 BY 4.75
         FONT 1.


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
/* REPARENT FRAME */
ASSIGN FRAME f-pg-imp:FRAME = FRAME D-Dialog:HANDLE.

/* SETTINGS FOR DIALOG-BOX D-Dialog
   FRAME-NAME L-To-R                                                    */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME f-pg-imp
                                                                        */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Sa°da de Impress∆o */
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


&Scoped-define FRAME-NAME f-pg-imp
&Scoped-define SELF-NAME bt-arquivo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-arquivo D-Dialog
ON CHOOSE OF bt-arquivo IN FRAME f-pg-imp
DO:
    {include/i-rparq.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME D-Dialog
&Scoped-define SELF-NAME bt-cancela
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancela D-Dialog
ON CHOOSE OF bt-cancela IN FRAME D-Dialog /* Cancelar */
DO:
   ASSIGN i-num-copias = 0
          l-ok         = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-imp
&Scoped-define SELF-NAME bt-config-impr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-config-impr D-Dialog
ON CHOOSE OF bt-config-impr IN FRAME f-pg-imp
DO:
   {include/i-rpimp.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME D-Dialog
&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok D-Dialog
ON CHOOSE OF bt-ok IN FRAME D-Dialog /* OK */
DO:
   ASSIGN i-saida      = INPUT FRAME f-pg-imp rs-destino
          i-num-copias = INPUT FRAME f-pg-imp fi-copias
          l-ok         = YES.

   IF i-saida = 1 THEN DO.
      FIND imprsor_usuar WHERE
           imprsor_usuar.nom_impressora = ENTRY(1,INPUT FRAME f-pg-imp c-arquivo,':') AND
           imprsor_usuar.cod_usuario = c-seg-usuario NO-LOCK NO-ERROR.
      IF AVAIL imprsor_usuar THEN
         ASSIGN c-saida = imprsor_usuar.nom_disposit_so.
   END.
   ELSE
      ASSIGN c-saida = INPUT FRAME f-pg-imp c-arquivo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-imp
&Scoped-define SELF-NAME rs-destino
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-destino D-Dialog
ON VALUE-CHANGED OF rs-destino IN FRAME f-pg-imp
DO:
    ASSIGN c-arquivo:VISIBLE = YES
           c-arquivo:SCREEN-VALUE = "".

    CASE SELF:SCREEN-VALUE:
        WHEN "1" THEN DO:
            ASSIGN c-arquivo:SENSITIVE    = NO
                   fi-copias:SENSITIVE    = YES
                   /*CtrlFrame:SENSITIVE    = YES */
                   bt-arquivo:VISIBLE     = NO
                   bt-config-impr:VISIBLE = YES.

            FIND imprsor_usuar WHERE
                 imprsor_usuar.cod_usuario = c-seg-usuario AND
                 imprsor_usuar.log_imprsor_princ = YES NO-LOCK NO-ERROR.

            IF AVAIL imprsor_usuar THEN DO.
               FIND layout_impres WHERE
                    layout_impres.nom_impressora = imprsor_usuar.nom_impressora AND
                    layout_impres.log_layout_impres_princ = YES NO-LOCK NO-ERROR.

               ASSIGN c-arquivo:SCREEN-VALUE = IF AVAIL layout_impres 
                                               THEN imprsor_usuar.nom_impressora + ":" + layout_impres.cod_layout_impres
                                               ELSE imprsor_usuar.nom_impressora.
            END.
        END.
        WHEN "2" THEN DO:
            ASSIGN c-arquivo:SENSITIVE     = YES
                   fi-copias:SENSITIVE     = NO
                   /*CtrlFrame:SENSITIVE     = NO */
                   bt-arquivo:VISIBLE      = YES
                   bt-config-impr:VISIBLE  = NO.
            ASSIGN fi-copias:SCREEN-VALUE = '1'.

            ASSIGN c-arquivo:SCREEN-VALUE = SESSION:TEMP-DIRECTORY + c-seg-usuario + ".txt".
        END.
        WHEN "3" THEN DO:
            ASSIGN c-arquivo:SENSITIVE     = NO
                   fi-copias:SENSITIVE     = NO
                   /*CtrlFrame:SENSITIVE     = NO */
                   bt-arquivo:VISIBLE      = NO
                   bt-config-impr:VISIBLE  = NO.
            ASSIGN fi-copias:SCREEN-VALUE = '1'.
        END.
    END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME D-Dialog
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
  HIDE FRAME f-pg-imp.
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
  ENABLE rt-buttom bt-ok bt-cancela bt-ajuda 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
  DISPLAY rs-destino c-arquivo fi-copias 
      WITH FRAME f-pg-imp.
  ENABLE RECT-8 rs-destino bt-config-impr bt-arquivo c-arquivo fi-copias 
      WITH FRAME f-pg-imp.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}
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
  APPLY 'value-changed' TO rs-destino IN FRAME f-pg-imp.

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

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartDialog, and there are no
     tables specified in any contained Browse, Query, or Frame. */

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

