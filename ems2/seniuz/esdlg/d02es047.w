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
{include/i-prgvrs.i D02ES047 2.04.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE NEW GLOBAL SHARED VAR fi-cod-estabel-ini AS CHAR.
DEFINE NEW GLOBAL SHARED VAR fi-cod-estabel-fin AS CHAR.
DEFINE NEW GLOBAL SHARED VAR fi-ini-nr-ob       LIKE ordem-benefic.nr-ob.
DEFINE NEW GLOBAL SHARED VAR fi-fin-nr-ob       LIKE ordem-benefic.nr-ob.
DEFINE NEW GLOBAL SHARED VAR fi-ini-dt-ob       LIKE ordem-benefic.dt-ob.
DEFINE NEW GLOBAL SHARED VAR fi-fin-dt-ob       LIKE ordem-benefic.dt-ob.
DEFINE NEW GLOBAL SHARED VAR fi-ini-it-codigo   LIKE ordem-benefic.it-codigo.
DEFINE NEW GLOBAL SHARED VAR fi-fin-it-codigo   LIKE ordem-benefic.it-codigo.
DEFINE NEW GLOBAL SHARED VAR fi-ini-cod-refer   LIKE ordem-benefic.cod-refer.
DEFINE NEW GLOBAL SHARED VAR fi-fin-cod-refer   LIKE ordem-benefic.cod-refer.
DEFINE NEW GLOBAL SHARED VAR to-dsp             AS LOG.
DEFINE NEW GLOBAL SHARED VAR to-rev             AS LOG.
DEFINE NEW GLOBAL SHARED VAR to-par             AS LOG.
DEFINE NEW GLOBAL SHARED VAR to-tot             AS LOG.
DEFINE NEW GLOBAL SHARED VAR to-rep             AS LOG.
DEFINE NEW GLOBAL SHARED VAR to-prd             AS LOG.
DEFINE NEW GLOBAL SHARED VAR to-ret             AS LOG.
DEFINE NEW GLOBAL SHARED VAR to-con             AS LOG.

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
&Scoped-Define ENABLED-OBJECTS IMAGE-1 IMAGE-2 IMAGE-3 IMAGE-4 IMAGE-5 ~
IMAGE-6 IMAGE-7 IMAGE-8 RECT-1 RECT-2 rt-buttom IMAGE-9 IMAGE-10 ~
fi-cod-estabel-ini1 fi-cod-estabel-fin1 fi-ini-nr-ob1 fi-fin-nr-ob1 ~
fi-ini-dt-ob1 fi-fin-dt-ob1 fi-ini-it-codigo1 fi-fin-it-codigo1 ~
fi-ini-cod-refer1 fi-fin-cod-refer1 to-dsp1 to-prd1 to-rev1 to-ret1 to-par1 ~
to-tot1 to-con1 to-rep1 bt-ok bt-cancela bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS fi-cod-estabel-ini1 fi-cod-estabel-fin1 ~
fi-ini-nr-ob1 fi-fin-nr-ob1 fi-ini-dt-ob1 fi-fin-dt-ob1 fi-ini-it-codigo1 ~
fi-fin-it-codigo1 fi-ini-cod-refer1 fi-fin-cod-refer1 to-dsp1 to-prd1 ~
to-rev1 to-ret1 to-par1 to-tot1 to-con1 to-rep1 

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

DEFINE VARIABLE fi-cod-estabel-fin1 AS CHARACTER FORMAT "X(3)":U 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-estabel-ini1 AS CHARACTER FORMAT "X(3)":U 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fin-cod-refer1 AS CHARACTER FORMAT "X(8)" INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "C¢digo de Referància final" NO-UNDO.

DEFINE VARIABLE fi-fin-dt-ob1 AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "Data da OB final" NO-UNDO.

DEFINE VARIABLE fi-fin-it-codigo1 AS CHARACTER FORMAT "x(16)" INITIAL "599999" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fin-nr-ob1 AS INTEGER FORMAT ">>>,>>9" INITIAL 999999 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 TOOLTIP "N£mero da OB final" NO-UNDO.

DEFINE VARIABLE fi-ini-cod-refer1 AS CHARACTER FORMAT "X(8)" 
     LABEL "Referància" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "C¢digo de Referància inicial" NO-UNDO.

DEFINE VARIABLE fi-ini-dt-ob1 AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 
     LABEL "Data da OB" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "Data da OB inicial" NO-UNDO.

DEFINE VARIABLE fi-ini-it-codigo1 AS CHARACTER FORMAT "x(16)" INITIAL "500000" 
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-nr-ob1 AS INTEGER FORMAT ">>>,>>9" INITIAL 0 
     LABEL "N£mero da OB" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 TOOLTIP "N£mero da OB inicial"
     FONT 1 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-10
     FILENAME "image\im-las":U
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

DEFINE IMAGE IMAGE-9
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 26.72 BY 5 TOOLTIP "Seleá∆o das Situaá‰es de ÷tens de Pedido a serem mostradas.".

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 26.72 BY 5 TOOLTIP "Seleá∆o das Situaá‰es de ÷tens de Pedido a serem mostradas.".

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 68 BY 1.42
     BGCOLOR 7 .

DEFINE VARIABLE to-con1 AS LOGICAL INITIAL no 
     LABEL "Conserto" 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE to-dsp1 AS LOGICAL INITIAL no 
     LABEL "Dispon°vel" 
     VIEW-AS TOGGLE-BOX
     SIZE 10.43 BY .88 NO-UNDO.

DEFINE VARIABLE to-par1 AS LOGICAL INITIAL no 
     LABEL "Revis∆o Parcial" 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE to-prd1 AS LOGICAL INITIAL no 
     LABEL "Produá∆o" 
     VIEW-AS TOGGLE-BOX
     SIZE 10.43 BY .88 NO-UNDO.

DEFINE VARIABLE to-rep1 AS LOGICAL INITIAL no 
     LABEL "Reportado" 
     VIEW-AS TOGGLE-BOX
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE to-ret1 AS LOGICAL INITIAL no 
     LABEL "Retrabalho" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.43 BY .88 NO-UNDO.

DEFINE VARIABLE to-rev1 AS LOGICAL INITIAL no 
     LABEL "Em Revis∆o" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.43 BY .88 NO-UNDO.

DEFINE VARIABLE to-tot1 AS LOGICAL INITIAL no 
     LABEL "Revis∆o Total" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.72 BY .88 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     fi-cod-estabel-ini1 AT ROW 1.17 COL 17 COLON-ALIGNED WIDGET-ID 2
     fi-cod-estabel-fin1 AT ROW 1.17 COL 46.14 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     fi-ini-nr-ob1 AT ROW 2.17 COL 17 COLON-ALIGNED
     fi-fin-nr-ob1 AT ROW 2.17 COL 46.14 COLON-ALIGNED NO-LABEL
     fi-ini-dt-ob1 AT ROW 3.17 COL 17 COLON-ALIGNED
     fi-fin-dt-ob1 AT ROW 3.17 COL 46.14 COLON-ALIGNED NO-LABEL
     fi-ini-it-codigo1 AT ROW 4.17 COL 17 COLON-ALIGNED HELP
          "Codigo do ÷tem no EMS"
     fi-fin-it-codigo1 AT ROW 4.17 COL 46.14 COLON-ALIGNED HELP
          "Codigo do ÷tem no EMS" NO-LABEL
     fi-ini-cod-refer1 AT ROW 5.17 COL 17 COLON-ALIGNED
     fi-fin-cod-refer1 AT ROW 5.17 COL 46.14 COLON-ALIGNED NO-LABEL
     to-dsp1 AT ROW 7.17 COL 12.57
     to-prd1 AT ROW 7.58 COL 41.72
     to-rev1 AT ROW 8 COL 12.57
     to-ret1 AT ROW 8.79 COL 41.72
     to-par1 AT ROW 8.83 COL 12.57
     to-tot1 AT ROW 9.67 COL 12.57
     to-con1 AT ROW 10 COL 41.72
     to-rep1 AT ROW 10.5 COL 12.57
     bt-ok AT ROW 12.17 COL 3
     bt-cancela AT ROW 12.17 COL 14
     bt-ajuda AT ROW 12.17 COL 59
     "Tipo de Ordem" VIEW-AS TEXT
          SIZE 10.43 BY .54 AT ROW 6.38 COL 44.86
          FONT 1
     "Situaá∆o das Revis‰es" VIEW-AS TEXT
          SIZE 16.14 BY .54 AT ROW 6.38 COL 12.14
     IMAGE-1 AT ROW 2.17 COL 30.43
     IMAGE-2 AT ROW 2.17 COL 44.72
     IMAGE-3 AT ROW 3.17 COL 30.43
     IMAGE-4 AT ROW 3.17 COL 44.72
     IMAGE-5 AT ROW 4.17 COL 30.43
     IMAGE-6 AT ROW 4.17 COL 44.72
     IMAGE-7 AT ROW 5.17 COL 30.43
     IMAGE-8 AT ROW 5.17 COL 44.72
     RECT-1 AT ROW 6.67 COL 7.57
     RECT-2 AT ROW 6.67 COL 37
     rt-buttom AT ROW 11.92 COL 2
     IMAGE-9 AT ROW 1.17 COL 30.43 WIDGET-ID 6
     IMAGE-10 AT ROW 1.17 COL 44.72 WIDGET-ID 8
     SPACE(23.13) SKIP(11.40)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 1
         TITLE "Seleá∆o de Revis‰es de OB's"
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
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

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
ON WINDOW-CLOSE OF FRAME D-Dialog /* Seleá∆o de Revis‰es de OB's */
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
  ASSIGN fi-cod-estabel-ini = INPUT FRAME {&FRAME-NAME} fi-cod-estabel-ini1
         fi-cod-estabel-fin = INPUT FRAME {&FRAME-NAME} fi-cod-estabel-fin1
         fi-ini-nr-ob       = INPUT FRAME {&FRAME-NAME} fi-ini-nr-ob1
         fi-fin-nr-ob       = INPUT FRAME {&FRAME-NAME} fi-fin-nr-ob1
         fi-ini-dt-ob       = INPUT FRAME {&FRAME-NAME} fi-ini-dt-ob1
         fi-fin-dt-ob       = INPUT FRAME {&FRAME-NAME} fi-fin-dt-ob1
         fi-ini-it-codigo   = INPUT FRAME {&FRAME-NAME} fi-ini-it-codigo1
         fi-fin-it-codigo   = INPUT FRAME {&FRAME-NAME} fi-fin-it-codigo1
         fi-ini-cod-refer   = INPUT FRAME {&FRAME-NAME} fi-ini-cod-refer1
         fi-fin-cod-refer   = INPUT FRAME {&FRAME-NAME} fi-fin-cod-refer1
         to-dsp             = INPUT FRAME {&FRAME-NAME} to-dsp1
         to-rev             = INPUT FRAME {&FRAME-NAME} to-rev1
         to-par             = INPUT FRAME {&FRAME-NAME} to-par1
         to-tot             = INPUT FRAME {&FRAME-NAME} to-tot1
         to-rep             = INPUT FRAME {&FRAME-NAME} to-rep1
         to-prd             = INPUT FRAME {&FRAME-NAME} to-prd1
         to-ret             = INPUT FRAME {&FRAME-NAME} to-ret1
         to-con             = INPUT FRAME {&FRAME-NAME} to-con1.
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
  DISPLAY fi-cod-estabel-ini1 fi-cod-estabel-fin1 fi-ini-nr-ob1 fi-fin-nr-ob1 
          fi-ini-dt-ob1 fi-fin-dt-ob1 fi-ini-it-codigo1 fi-fin-it-codigo1 
          fi-ini-cod-refer1 fi-fin-cod-refer1 to-dsp1 to-prd1 to-rev1 to-ret1 
          to-par1 to-tot1 to-con1 to-rep1 
      WITH FRAME D-Dialog.
  ENABLE IMAGE-1 IMAGE-2 IMAGE-3 IMAGE-4 IMAGE-5 IMAGE-6 IMAGE-7 IMAGE-8 RECT-1 
         RECT-2 rt-buttom IMAGE-9 IMAGE-10 fi-cod-estabel-ini1 
         fi-cod-estabel-fin1 fi-ini-nr-ob1 fi-fin-nr-ob1 fi-ini-dt-ob1 
         fi-fin-dt-ob1 fi-ini-it-codigo1 fi-fin-it-codigo1 fi-ini-cod-refer1 
         fi-fin-cod-refer1 to-dsp1 to-prd1 to-rev1 to-ret1 to-par1 to-tot1 
         to-con1 to-rep1 bt-ok bt-cancela bt-ajuda 
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

  {utp/ut9000.i "D02ES047" "2.04.00.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN fi-cod-estabel-ini1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = fi-cod-estabel-ini
         fi-cod-estabel-fin1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = fi-cod-estabel-fin
         fi-ini-nr-ob1:SCREEN-VALUE IN FRAME {&FRAME-NAME}       = STRING(fi-ini-nr-ob)
         fi-fin-nr-ob1:SCREEN-VALUE IN FRAME {&FRAME-NAME}       = STRING(fi-fin-nr-ob)
         fi-ini-dt-ob1:SCREEN-VALUE IN FRAME {&FRAME-NAME}       = STRING(fi-ini-dt-ob)
         fi-fin-dt-ob1:SCREEN-VALUE IN FRAME {&FRAME-NAME}       = STRING(fi-fin-dt-ob)
         fi-ini-it-codigo1:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = fi-ini-it-codigo
         fi-fin-it-codigo1:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = fi-fin-it-codigo
         fi-ini-cod-refer1:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = fi-ini-cod-refer
         fi-fin-cod-refer1:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = fi-fin-cod-refer
         to-dsp1:SCREEN-VALUE IN FRAME {&FRAME-NAME}             = STRING(to-dsp)
         to-rev1:SCREEN-VALUE IN FRAME {&FRAME-NAME}             = STRING(to-rev)
         to-par1:SCREEN-VALUE IN FRAME {&FRAME-NAME}             = STRING(to-par)
         to-tot1:SCREEN-VALUE IN FRAME {&FRAME-NAME}             = STRING(to-tot)
         to-rep1:SCREEN-VALUE IN FRAME {&FRAME-NAME}             = STRING(to-rep)
         to-prd1:SCREEN-VALUE IN FRAME {&FRAME-NAME}             = STRING(to-prd)
         to-ret1:SCREEN-VALUE IN FRAME {&FRAME-NAME}             = STRING(to-ret)
         to-con1:SCREEN-VALUE IN FRAME {&FRAME-NAME}             = STRING(to-con).
 

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

