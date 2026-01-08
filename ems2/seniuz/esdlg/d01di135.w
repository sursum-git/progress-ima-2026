&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i D01DI135 2.04.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE NEW GLOBAL SHARED VAR fi-cod-rep          LIKE nota-fiscal.cod-rep.
DEFINE NEW GLOBAL SHARED VAR fi-ini-cod-estabel  LIKE nota-fiscal.cod-estabel.
DEFINE NEW GLOBAL SHARED VAR fi-fin-cod-estabel  LIKE nota-fiscal.cod-estabel.
DEFINE NEW GLOBAL SHARED VAR fi-ini-serie        LIKE nota-fiscal.serie.
DEFINE NEW GLOBAL SHARED VAR fi-fin-serie        LIKE nota-fiscal.serie.
DEFINE NEW GLOBAL SHARED VAR fi-ini-nr-nota-fis  LIKE nota-fiscal.nr-nota-fis.
DEFINE NEW GLOBAL SHARED VAR fi-fin-nr-nota-fis  LIKE nota-fiscal.nr-nota-fis.
DEFINE NEW GLOBAL SHARED VAR fi-ini-dt-emis-nota LIKE nota-fiscal.dt-emis-nota.
DEFINE NEW GLOBAL SHARED VAR fi-fin-dt-emis-nota LIKE nota-fiscal.dt-emis-nota.

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
&Scoped-Define ENABLED-OBJECTS fi-cod-rep fi-ini-cod-estabel1 ~
fi-fin-cod-estabel1 fi-ini-serie1 fi-fin-serie1 fi-ini-nr-nota-fis1 ~
fi-fin-nr-nota-fis1 fi-ini-dt-emis-nota1 fi-fin-dt-emis-nota1 bt-ok ~
bt-cancela bt-ajuda IMAGE-1 IMAGE-2 IMAGE-3 IMAGE-4 IMAGE-5 IMAGE-6 IMAGE-7 ~
IMAGE-8 RECT-1 rt-buttom 
&Scoped-Define DISPLAYED-OBJECTS fi-cod-rep fi-nome-rep fi-ini-cod-estabel1 ~
fi-fin-cod-estabel1 fi-ini-serie1 fi-fin-serie1 fi-ini-nr-nota-fis1 ~
fi-fin-nr-nota-fis1 fi-ini-dt-emis-nota1 fi-fin-dt-emis-nota1 

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

DEFINE VARIABLE fi-cod-rep AS CHARACTER FORMAT "X(3)" 
     LABEL "Representante":R18 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 TOOLTIP "Estabelecimento inicial" NO-UNDO.

DEFINE VARIABLE fi-fin-cod-estabel1 AS CHARACTER FORMAT "X(3)" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 TOOLTIP "Estabelecimento final" NO-UNDO.

DEFINE VARIABLE fi-fin-dt-emis-nota1 AS DATE FORMAT "99/99/9999" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "Data de emiss∆o final" NO-UNDO.

DEFINE VARIABLE fi-fin-nr-nota-fis1 AS CHARACTER FORMAT "x(16)" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 TOOLTIP "N£mero final" NO-UNDO.

DEFINE VARIABLE fi-fin-serie1 AS CHARACTER FORMAT "x(5)" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 TOOLTIP "SÇrie inicial" NO-UNDO.

DEFINE VARIABLE fi-ini-cod-estabel1 AS CHARACTER FORMAT "X(3)" 
     LABEL "Estabelecimento":R18 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 TOOLTIP "Estabelecimento inicial" NO-UNDO.

DEFINE VARIABLE fi-ini-dt-emis-nota1 AS DATE FORMAT "99/99/9999" 
     LABEL "Dt Emiss∆o":R12 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "Data de emiss∆o inicial" NO-UNDO.

DEFINE VARIABLE fi-ini-nr-nota-fis1 AS CHARACTER FORMAT "x(16)" 
     LABEL "Nr Nota Fiscal":R17 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 TOOLTIP "N£mero inicial" NO-UNDO.

DEFINE VARIABLE fi-ini-serie1 AS CHARACTER FORMAT "x(5)" 
     LABEL "SÇrie":R7 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 TOOLTIP "SÇrie inicial" NO-UNDO.

DEFINE VARIABLE fi-nome-rep AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 46 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-3
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-4
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-5
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-6
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-7
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-8
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 68 BY 5.5.

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 68 BY 1.42
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     fi-cod-rep AT ROW 1.42 COL 15.29 COLON-ALIGNED HELP
          "C¢digo do estabelecimento"
     fi-nome-rep AT ROW 1.42 COL 20 COLON-ALIGNED NO-LABEL
     fi-ini-cod-estabel1 AT ROW 2.42 COL 15.29 COLON-ALIGNED HELP
          "C¢digo do estabelecimento"
     fi-fin-cod-estabel1 AT ROW 2.42 COL 42.29 COLON-ALIGNED HELP
          "C¢digo do estabelecimento" NO-LABEL
     fi-ini-serie1 AT ROW 3.42 COL 15.29 COLON-ALIGNED HELP
          "SÇrie da nota fiscal"
     fi-fin-serie1 AT ROW 3.42 COL 42.29 COLON-ALIGNED HELP
          "SÇrie da nota fiscal" NO-LABEL
     fi-ini-nr-nota-fis1 AT ROW 4.42 COL 15.29 COLON-ALIGNED HELP
          "N£mero da nota fiscal"
     fi-fin-nr-nota-fis1 AT ROW 4.42 COL 42.29 COLON-ALIGNED HELP
          "N£mero da nota fiscal" NO-LABEL
     fi-ini-dt-emis-nota1 AT ROW 5.42 COL 15.29 COLON-ALIGNED HELP
          "Data de emiss∆o da nota fiscal"
     fi-fin-dt-emis-nota1 AT ROW 5.42 COL 42.29 COLON-ALIGNED HELP
          "Data de emiss∆o da nota fiscal" NO-LABEL
     bt-ok AT ROW 7.33 COL 3
     bt-cancela AT ROW 7.33 COL 14
     bt-ajuda AT ROW 7.33 COL 59
     IMAGE-1 AT ROW 2.46 COL 28.86
     IMAGE-2 AT ROW 2.46 COL 40.86
     IMAGE-3 AT ROW 3.46 COL 28.86
     IMAGE-4 AT ROW 3.46 COL 40.86
     IMAGE-5 AT ROW 4.46 COL 28.86
     IMAGE-6 AT ROW 4.46 COL 40.86
     IMAGE-7 AT ROW 5.46 COL 28.86
     IMAGE-8 AT ROW 5.46 COL 40.86
     RECT-1 AT ROW 1.25 COL 2
     rt-buttom AT ROW 7.08 COL 2
     SPACE(0.85) SKIP(0.24)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 1
         TITLE "Seleá∆o de Notas Fiscais"
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
   Custom                                                               */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fi-nome-rep IN FRAME D-Dialog
   NO-ENABLE                                                            */
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
ON WINDOW-CLOSE OF FRAME D-Dialog /* Seleá∆o de Notas Fiscais */
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


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok D-Dialog
ON CHOOSE OF bt-ok IN FRAME D-Dialog /* OK */
DO:
  ASSIGN fi-ini-cod-estabel  = INPUT FRAME {&FRAME-NAME} fi-ini-cod-estabel1
         fi-fin-cod-estabel  = INPUT FRAME {&FRAME-NAME} fi-fin-cod-estabel1
         fi-ini-serie        = INPUT FRAME {&FRAME-NAME} fi-ini-serie1
         fi-fin-serie        = INPUT FRAME {&FRAME-NAME} fi-fin-serie1
         fi-ini-nr-nota-fis  = INPUT FRAME {&FRAME-NAME} fi-ini-nr-nota-fis1
         fi-fin-nr-nota-fis  = INPUT FRAME {&FRAME-NAME} fi-fin-nr-nota-fis1
         fi-ini-dt-emis-nota = INPUT FRAME {&FRAME-NAME} fi-ini-dt-emis-nota1
         fi-fin-dt-emis-nota = INPUT FRAME {&FRAME-NAME} fi-fin-dt-emis-nota1.

  ASSIGN INPUT FRAME {&FRAME-NAME} fi-cod-rep.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-rep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-rep D-Dialog
ON LEAVE OF fi-cod-rep IN FRAME D-Dialog /* Representante */
DO:
   FIND repres WHERE
        repres.cod-rep = SELF:INPUT-VALUE NO-LOCK NO-ERROR.

   IF NOT AVAIL repres THEN DO.
      MESSAGE "Representante n∆o Cadastrado..." VIEW-AS ALERT-BOX.
      APPLY 'entry' TO SELF.
      RETURN NO-APPLY.
   END.
   ASSIGN fi-nome-rep:SCREEN-VALUE IN FRAME {&FRAME-NAME} = repres.nome.
                       
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
  DISPLAY fi-cod-rep fi-nome-rep fi-ini-cod-estabel1 fi-fin-cod-estabel1 
          fi-ini-serie1 fi-fin-serie1 fi-ini-nr-nota-fis1 fi-fin-nr-nota-fis1 
          fi-ini-dt-emis-nota1 fi-fin-dt-emis-nota1 
      WITH FRAME D-Dialog.
  ENABLE fi-cod-rep fi-ini-cod-estabel1 fi-fin-cod-estabel1 fi-ini-serie1 
         fi-fin-serie1 fi-ini-nr-nota-fis1 fi-fin-nr-nota-fis1 
         fi-ini-dt-emis-nota1 fi-fin-dt-emis-nota1 bt-ok bt-cancela bt-ajuda 
         IMAGE-1 IMAGE-2 IMAGE-3 IMAGE-4 IMAGE-5 IMAGE-6 IMAGE-7 IMAGE-8 RECT-1 
         rt-buttom 
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

  {utp/ut9000.i "D01DI135" "2.04.00.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  FIND repres WHERE
       repres.cod-rep = fi-cod-rep NO-LOCK NO-ERROR.

  ASSIGN fi-cod-rep:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = STRING(fi-cod-rep)
         fi-nome-rep:SCREEN-VALUE IN FRAME {&FRAME-NAME} = IF AVAIL repres 
                                                           THEN repres.nome ELSE ""
         fi-ini-cod-estabel1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = fi-ini-cod-estabel
         fi-fin-cod-estabel1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = fi-fin-cod-estabel
         fi-ini-serie1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = fi-ini-serie
         fi-fin-serie1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = fi-fin-serie
         fi-ini-nr-nota-fis1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = fi-ini-nr-nota-fis
         fi-fin-nr-nota-fis1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = fi-fin-nr-nota-fis
         fi-ini-dt-emis-nota1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(fi-ini-dt-emis-nota)
         fi-fin-dt-emis-nota1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(fi-fin-dt-emis-nota).


                                                                    
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

