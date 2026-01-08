&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME w-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-digita 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESSP0169B 2.04.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT-OUTPUT PARAMETER c-item-ini LIKE ob-sl-estoq-per.it-codigo.
DEFINE INPUT-OUTPUT PARAMETER c-item-fin LIKE ob-sl-estoq-per.it-codigo.
DEFINE INPUT-OUTPUT PARAMETER l-item     AS LOG.
DEFINE INPUT-OUTPUT PARAMETER p-tipo-rel AS INT.
DEFINE INPUT-OUTPUT PARAMETER l-ok       AS LOG.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Digitacao
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rs-tipo-rel fi-it-codigo-ini ~
fi-it-codigo-fin tg-item bt-ajuda bt-ok bt-cancelar IMAGE-88 IMAGE-89 ~
RECT-1 RECT-44 RECT-50 
&Scoped-Define DISPLAYED-OBJECTS rs-tipo-rel fi-it-codigo-ini ~
fi-it-codigo-fin tg-item 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-4 fi-it-codigo-ini fi-it-codigo-fin tg-item 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-digita AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ajuda 
     LABEL "Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "Cancelar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "OK" 
     SIZE 10 BY 1.

DEFINE VARIABLE fi-it-codigo-fin AS CHARACTER FORMAT "X(6)" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "C¢digo do Item Final" NO-UNDO.

DEFINE VARIABLE fi-it-codigo-ini AS CHARACTER FORMAT "X(6)" 
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "C¢digo do Item Inicial" NO-UNDO.

DEFINE IMAGE IMAGE-88
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-89
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE VARIABLE rs-tipo-rel AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Sint‚tico", 1,
"Anal¡tico", 2
     SIZE 32.14 BY .79 TOOLTIP "Relat¢rio Sint‚tico ou Anal¡tico" NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 69 BY 1.38
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-44
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 42 BY 1.5.

DEFINE RECTANGLE RECT-50
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 69 BY 6.

DEFINE VARIABLE tg-item AS LOGICAL INITIAL no 
     LABEL "Imprime somente o Item marcado" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY .88 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     rs-tipo-rel AT ROW 2.42 COL 23.86 NO-LABEL
     fi-it-codigo-ini AT ROW 4.13 COL 17.29 COLON-ALIGNED
     fi-it-codigo-fin AT ROW 4.13 COL 44.57 COLON-ALIGNED NO-LABEL
     tg-item AT ROW 5.88 COL 25
     bt-ajuda AT ROW 7.5 COL 59.86
     bt-ok AT ROW 7.54 COL 3.29
     bt-cancelar AT ROW 7.54 COL 14.29
     IMAGE-88 AT ROW 4.13 COL 29.43
     IMAGE-89 AT ROW 4.13 COL 43.57
     RECT-1 AT ROW 7.33 COL 2
     RECT-44 AT ROW 2.04 COL 15
     RECT-50 AT ROW 1.25 COL 2
     " Sele‡Æo" VIEW-AS TEXT
          SIZE 7 BY .54 AT ROW 1 COL 4.29
     "Tipo do Relat¢rio" VIEW-AS TEXT
          SIZE 12 BY .54 AT ROW 1.75 COL 30.29
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 21.29
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Digitacao
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
   Design Page: 1
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-digita ASSIGN
         HIDDEN             = YES
         TITLE              = "Sele‡Æo dos Itens do Relat¢rio"
         COLUMN             = 26.43
         ROW                = 9.38
         HEIGHT             = 7.71
         WIDTH              = 70.72
         MAX-HEIGHT         = 29
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 29
         VIRTUAL-WIDTH      = 146.29
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-digita 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-digit.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-digita
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
                                                                        */
/* SETTINGS FOR FILL-IN fi-it-codigo-fin IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR FILL-IN fi-it-codigo-ini IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR TOGGLE-BOX tg-item IN FRAME F-Main
   4                                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-digita)
THEN w-digita:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-digita w-digita
ON END-ERROR OF w-digita /* Sele‡Æo dos Itens do Relat¢rio */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-digita w-digita
ON WINDOW-CLOSE OF w-digita /* Sele‡Æo dos Itens do Relat¢rio */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda w-digita
ON CHOOSE OF bt-ajuda IN FRAME F-Main /* Ajuda */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancelar w-digita
ON CHOOSE OF bt-cancelar IN FRAME F-Main /* Cancelar */
DO:
  ASSIGN l-ok = NO.
  APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-digita
ON CHOOSE OF bt-ok IN FRAME F-Main /* OK */
DO:
  ASSIGN c-item-ini  = INPUT FRAME {&FRAME-NAME} fi-it-codigo-ini   
         c-item-fin  = INPUT FRAME {&FRAME-NAME} fi-it-codigo-fin 
         p-tipo-rel  = INPUT FRAME {&FRAME-NAME} rs-tipo-rel
         l-item      = INPUT FRAME {&FRAME-NAME} tg-item. 
  APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-tipo-rel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-tipo-rel w-digita
ON VALUE-CHANGED OF rs-tipo-rel IN FRAME F-Main
DO:
    ASSIGN fi-it-codigo-ini:SCREEN-VALUE    = ""    
           fi-it-codigo-fin:SCREEN-VALUE    = "".    
    IF rs-tipo-rel:INPUT-VALUE = 1 THEN DO: 
       ASSIGN fi-it-codigo-ini:SCREEN-VALUE    = ""   
              fi-it-codigo-fin:SCREEN-VALUE    = "ZZZZZZ"
              tg-item:SCREEN-VALUE             = 'NO'.
       tg-item:SENSITIVE = NO.
    END.
    ELSE DO:
        ASSIGN fi-it-codigo-ini:SCREEN-VALUE    = c-item-ini    
               fi-it-codigo-fin:SCREEN-VALUE    = c-item-fin.
               tg-item:SCREEN-VALUE             = STRING(l-item).
        tg-item:SENSITIVE = YES.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-item
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-item w-digita
ON VALUE-CHANGED OF tg-item IN FRAME F-Main /* Imprime somente o Item marcado */
DO:
  IF SELF:SCREEN-VALUE = "NO" THEN DO:
     ASSIGN fi-it-codigo-ini:SCREEN-VALUE = ""
            fi-it-codigo-fin:SCREEN-VALUE = "ZZZZZZ".
     ASSIGN fi-it-codigo-ini:SENSITIVE = NO
            fi-it-codigo-fin:SENSITIVE = NO.
  END.
  ELSE DO:
     ASSIGN fi-it-codigo-ini:SCREEN-VALUE = c-item-ini
            fi-it-codigo-fin:SCREEN-VALUE = c-item-fin.
     ASSIGN fi-it-codigo-ini:SENSITIVE = YES
            fi-it-codigo-fin:SENSITIVE = YES.
  END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-digita 


/* ***************************  Main Block  *************************** */
/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-digita  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  /* Select a Startup page. */
  IF adm-current-page eq 0 
  THEN RUN select-page IN THIS-PROCEDURE ( 1 ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-digita  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-digita  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-digita)
  THEN DELETE WIDGET w-digita.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-digita  _DEFAULT-ENABLE
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
  DISPLAY rs-tipo-rel fi-it-codigo-ini fi-it-codigo-fin tg-item 
      WITH FRAME F-Main IN WINDOW w-digita.
  ENABLE rs-tipo-rel fi-it-codigo-ini fi-it-codigo-fin tg-item bt-ajuda bt-ok 
         bt-cancelar IMAGE-88 IMAGE-89 RECT-1 RECT-44 RECT-50 
      WITH FRAME F-Main IN WINDOW w-digita.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW w-digita.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-digita 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-digita 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
   RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-digita 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  /* Code placed here will execute PRIOR to standard behavior. */
  ASSIGN fi-it-codigo-ini = c-item-ini   
         fi-it-codigo-fin = c-item-fin
         rs-tipo-rel      = p-tipo-rel
         tg-item          = l-item. 

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-digita  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this Digitacao, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-digita 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

