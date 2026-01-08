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
{include/i-prgvrs.i D01ESSP0181 2.04.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT-OUTPUT PARAMETER c-item-ini    LIKE mov-est-acbm.it-codigo.
DEFINE INPUT-OUTPUT PARAMETER c-item-fin    LIKE mov-est-acbm.it-codigo.
DEFINE INPUT-OUTPUT PARAMETER c-defeito-ini LIKE mov-est-acbm.it-codigo.
DEFINE INPUT-OUTPUT PARAMETER c-defeito-fin LIKE mov-est-acbm.it-codigo.
DEFINE INPUT-OUTPUT PARAMETER i-tipo-rel    AS INT.
DEFINE INPUT-OUTPUT PARAMETER l-ok          AS LOG.
DEFINE OUTPUT PARAMETER p-arq-saida AS CHAR FORMAT "x(45)".

/* Local Variable Definitions ---                                       */
DEF VAR OKPressed AS LOG.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME D-Dialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rs-tipo-plan fi-cod-tipo-def-ini ~
fi-cod-tipo-def-fin fi-arq-saida bt-arq-sai bt-ok bt-cancela bt-ajuda ~
IMAGE-100 IMAGE-101 IMAGE-88 IMAGE-89 RECT-44 rt-buttom 
&Scoped-Define DISPLAYED-OBJECTS rs-tipo-plan fi-cod-tipo-def-ini ~
fi-cod-tipo-def-fin fi-it-codigo-ini fi-it-codigo-fin fi-arq-saida 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-4 fi-cod-tipo-def-ini fi-cod-tipo-def-fin 
&Scoped-define List-5 fi-it-codigo-ini fi-it-codigo-fin 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ajuda 
     LABEL "&Ajuda" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-arq-sai 
     IMAGE-UP FILE "image/im-open3.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Arquivo de pagamento de sa¡da (ajuntado).".

DEFINE BUTTON bt-cancela AUTO-END-KEY 
     LABEL "&Cancelar" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "&OK" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE fi-arq-saida AS CHARACTER FORMAT "X(45)":U 
     LABEL "Arquivo Sa¡da" 
     VIEW-AS FILL-IN 
     SIZE 45 BY .88 TOOLTIP "Caminho/nome do arquivo de sa¡da."
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-cod-tipo-def-fin AS CHARACTER FORMAT "X(1)" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 TOOLTIP "C¢digo do Tipo de Defeito Final" NO-UNDO.

DEFINE VARIABLE fi-cod-tipo-def-ini AS CHARACTER FORMAT "X(1)" 
     LABEL "Tipo do Defeito" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 TOOLTIP "C¢digo do Tipo de Defeito Inicial" NO-UNDO.

DEFINE VARIABLE fi-it-codigo-fin AS CHARACTER FORMAT "X(6)" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "C¢digo do Item Final" NO-UNDO.

DEFINE VARIABLE fi-it-codigo-ini AS CHARACTER FORMAT "X(6)" 
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "C¢digo do Item Inicial" NO-UNDO.

DEFINE IMAGE IMAGE-100
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-101
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-88
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-89
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE VARIABLE rs-tipo-plan AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Sint‚tica", 1,
"Anal¡tica", 2
     SIZE 32.14 BY .79 TOOLTIP "Planilha Sint‚tica ou Anal¡tica" NO-UNDO.

DEFINE RECTANGLE RECT-44
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 42 BY 1.5.

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 68 BY 1.42
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     rs-tipo-plan AT ROW 1.67 COL 23.86 NO-LABEL
     fi-cod-tipo-def-ini AT ROW 3.04 COL 18 COLON-ALIGNED
     fi-cod-tipo-def-fin AT ROW 3.04 COL 45.29 COLON-ALIGNED NO-LABEL
     fi-it-codigo-ini AT ROW 4.04 COL 18 COLON-ALIGNED
     fi-it-codigo-fin AT ROW 4.04 COL 45.29 COLON-ALIGNED NO-LABEL
     fi-arq-saida AT ROW 5.42 COL 14.14 COLON-ALIGNED
     bt-arq-sai AT ROW 5.42 COL 61.29 HELP
          "Arquivo de pagamento de sa¡da (ajuntado)."
     bt-ok AT ROW 6.79 COL 2.43
     bt-cancela AT ROW 6.79 COL 13.43
     bt-ajuda AT ROW 6.79 COL 58.43
     IMAGE-100 AT ROW 3.04 COL 30.14
     IMAGE-101 AT ROW 4.04 COL 44.29
     IMAGE-88 AT ROW 4.04 COL 30.14
     IMAGE-89 AT ROW 3.04 COL 44.29
     RECT-44 AT ROW 1.29 COL 15
     rt-buttom AT ROW 6.54 COL 1.57
     "Tipo da Planilha" VIEW-AS TEXT
          SIZE 12 BY .54 AT ROW 1 COL 30.29
     SPACE(28.56) SKIP(6.53)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 1
         TITLE "Parƒmetros do Relat¢rio"
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
   L-To-R                                                               */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fi-cod-tipo-def-fin IN FRAME D-Dialog
   4                                                                    */
/* SETTINGS FOR FILL-IN fi-cod-tipo-def-ini IN FRAME D-Dialog
   4                                                                    */
/* SETTINGS FOR FILL-IN fi-it-codigo-fin IN FRAME D-Dialog
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN fi-it-codigo-ini IN FRAME D-Dialog
   NO-ENABLE 5                                                          */
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
ON WINDOW-CLOSE OF FRAME D-Dialog /* Parƒmetros do Relat¢rio */
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


&Scoped-define SELF-NAME bt-arq-sai
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-arq-sai D-Dialog
ON CHOOSE OF bt-arq-sai IN FRAME D-Dialog
DO:
    SYSTEM-DIALOG GET-FILE fi-arq-saida
        TITLE   "Escolha o Arquivo"
        FILTERS "Planilhas Excel (*.xls)" "*.xls"
        INITIAL-DIR SESSION:TEMP-DIRECTORY
        USE-FILENAME
        UPDATE OKpressed.
      
    IF OKpressed = TRUE THEN DO:
       IF fi-arq-saida <> "" AND INDEX(fi-arq-saida,".") = 0 THEN
          ASSIGN fi-arq-saida:SCREEN-VALUE = fi-arq-saida + ".xls".
       ELSE
          ASSIGN fi-arq-saida:SCREEN-VALUE = fi-arq-saida.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok D-Dialog
ON CHOOSE OF bt-ok IN FRAME D-Dialog /* OK */
DO:
  ASSIGN p-arq-saida   = INPUT FRAME {&FRAME-NAME} fi-arq-saida
         c-item-ini    = INPUT FRAME {&FRAME-NAME} fi-it-codigo-ini   
         c-item-fin    = INPUT FRAME {&FRAME-NAME} fi-it-codigo-fin   
         c-defeito-ini = INPUT FRAME {&FRAME-NAME} fi-cod-tipo-def-ini
         c-defeito-fin = INPUT FRAME {&FRAME-NAME} fi-cod-tipo-def-fin
         i-tipo-rel    = INPUT FRAME {&FRAME-NAME} rs-tipo-plan. 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-arq-saida
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-arq-saida D-Dialog
ON LEAVE OF fi-arq-saida IN FRAME D-Dialog /* Arquivo Sa¡da */
DO:
  IF NOT fi-arq-saida:SCREEN-VALUE IN FRAME {&FRAME-NAME} MATCHES "*.xls*" THEN DO:
     MESSAGE "Nome do arquivo para Excel est  inv lido." SKIP
             "Deve ser do tipo Caminho/Arquivo.xls" VIEW-AS ALERT-BOX.
     ASSIGN fi-arq-saida:SCREEN-VALUE IN FRAME {&FRAME-NAME} = SESSION:TEMP-DIRECTORY + 
                         "Evolu‡Æo do Estoque.xls".
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-tipo-plan
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-tipo-plan D-Dialog
ON VALUE-CHANGED OF rs-tipo-plan IN FRAME D-Dialog
DO:
    DISABLE {&list-4} WITH FRAME {&FRAME-NAME}.
    DISABLE {&list-5} WITH FRAME {&FRAME-NAME}.
    ASSIGN fi-it-codigo-ini:SCREEN-VALUE    = ""    
           fi-it-codigo-fin:SCREEN-VALUE    = ""    
           fi-cod-tipo-def-ini:SCREEN-VALUE = ""
           fi-cod-tipo-def-fin:SCREEN-VALUE = "".
    IF rs-tipo-plan:INPUT-VALUE = 1 THEN DO:
       ASSIGN fi-cod-tipo-def-ini:SCREEN-VALUE = c-defeito-ini
              fi-cod-tipo-def-fin:SCREEN-VALUE = c-defeito-fin.
       ENABLE {&list-4} WITH FRAME {&FRAME-NAME} .
    END.
    ELSE DO:
       ASSIGN fi-it-codigo-ini:SCREEN-VALUE    = c-item-ini    
              fi-it-codigo-fin:SCREEN-VALUE    = c-item-fin    
              fi-cod-tipo-def-ini:SCREEN-VALUE = c-defeito-ini
              fi-cod-tipo-def-fin:SCREEN-VALUE = c-defeito-fin.
       ENABLE {&list-4} WITH FRAME {&FRAME-NAME}.
       ENABLE {&list-5} WITH FRAME {&FRAME-NAME}.
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

  ASSIGN fi-it-codigo-ini    = c-item-ini   
         fi-it-codigo-fin    = c-item-fin
         fi-cod-tipo-def-ini = c-defeito-ini
         fi-cod-tipo-def-fin = c-defeito-fin
         rs-tipo-plan        = i-tipo-rel. 


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
  DISPLAY rs-tipo-plan fi-cod-tipo-def-ini fi-cod-tipo-def-fin fi-it-codigo-ini 
          fi-it-codigo-fin fi-arq-saida 
      WITH FRAME D-Dialog.
  ENABLE rs-tipo-plan fi-cod-tipo-def-ini fi-cod-tipo-def-fin fi-arq-saida 
         bt-arq-sai bt-ok bt-cancela bt-ajuda IMAGE-100 IMAGE-101 IMAGE-88 
         IMAGE-89 RECT-44 rt-buttom 
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

  {utp/ut9000.i "D02ESSP0181" "2.04.00.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  DISABLE {&list-5} WITH FRAME {&FRAME-NAME}.
  ASSIGN fi-arq-saida:SCREEN-VALUE IN FRAME {&FRAME-NAME} = SESSION:TEMP-DIRECTORY + "Analise dos Defeitos na Produ‡Æo.xls".
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

