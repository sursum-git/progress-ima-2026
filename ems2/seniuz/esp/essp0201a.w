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
{include/i-prgvrs.i ESSP0201A 2.04.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT-OUTPUT PARAMETER l-todos-locais AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-normal       AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-amostra      AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-exporta      AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-exclusivo    AS LOG. 
DEFINE INPUT-OUTPUT PARAMETER l-ind          AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-retalho      AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-bloqueada    AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-refat        AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-pilotagem    AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-outros       AS LOG.
DEFINE INPUT-OUTPUT PARAMETER p-rua-ini      AS CHAR.
DEFINE INPUT-OUTPUT PARAMETER p-rua-fin      AS CHAR.
DEFINE INPUT-OUTPUT PARAMETER p-doca-ini     AS CHAR.
DEFINE INPUT-OUTPUT PARAMETER p-doca-fin     AS CHAR.
DEFINE INPUT-OUTPUT PARAMETER p-tipo         AS INT.
DEFINE INPUT-OUTPUT PARAMETER l-ok           AS LOG.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Digitacao
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi-rua-ini fi-rua-fin fi-doca-ini ~
fi-doca-fin RECT-1 rs-tipo RECT-50 tg-todos IMAGE-1 IMAGE-2 IMAGE-3 IMAGE-4 ~
RECT-51 bt-ok bt-cancelar bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS fi-rua-ini fi-rua-fin fi-doca-ini ~
fi-doca-fin rs-tipo tg-todos tg-normal tg-exclusivo tg-bloqueada tg-exporta ~
tg-amostra tg-ind tg-refat tg-retalho tg-pilotagem tg-outros 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

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

DEFINE VARIABLE fi-doca-fin AS CHARACTER FORMAT "X(256)":U INITIAL "ZZZ" 
     VIEW-AS FILL-IN 
     SIZE 7.14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-doca-ini AS CHARACTER FORMAT "X(256)":U 
     LABEL "Doca" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE VARIABLE fi-rua-fin AS CHARACTER FORMAT "X(256)":U INITIAL "ZZZ" 
     VIEW-AS FILL-IN 
     SIZE 7.29 BY .88 NO-UNDO.

DEFINE VARIABLE fi-rua-ini AS CHARACTER FORMAT "X(256)":U 
     LABEL "Rua" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

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

DEFINE VARIABLE rs-tipo AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Relat¢rio", 1,
"C¢digo de Barras", 2
     SIZE 18 BY 1.5 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 69 BY 1.38
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-50
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 68 BY 9.

DEFINE RECTANGLE RECT-51
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 24 BY 2.25.

DEFINE VARIABLE tg-amostra AS LOGICAL INITIAL no 
     LABEL "Amostra" 
     VIEW-AS TOGGLE-BOX
     SIZE 8.29 BY .63 NO-UNDO.

DEFINE VARIABLE tg-bloqueada AS LOGICAL INITIAL no 
     LABEL "Bloqueada" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .63 NO-UNDO.

DEFINE VARIABLE tg-exclusivo AS LOGICAL INITIAL no 
     LABEL "Desenho Exclusivo" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .63 NO-UNDO.

DEFINE VARIABLE tg-exporta AS LOGICAL INITIAL no 
     LABEL "Montagem de Volumes" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .63 NO-UNDO.

DEFINE VARIABLE tg-ind AS LOGICAL INITIAL no 
     LABEL "Industrializa‡Æo" 
     VIEW-AS TOGGLE-BOX
     SIZE 13 BY .63 NO-UNDO.

DEFINE VARIABLE tg-normal AS LOGICAL INITIAL no 
     LABEL "Normal" 
     VIEW-AS TOGGLE-BOX
     SIZE 8 BY .63 NO-UNDO.

DEFINE VARIABLE tg-outros AS LOGICAL INITIAL no 
     LABEL "Outros" 
     VIEW-AS TOGGLE-BOX
     SIZE 8.57 BY .63 NO-UNDO.

DEFINE VARIABLE tg-pilotagem AS LOGICAL INITIAL no 
     LABEL "Em corte" 
     VIEW-AS TOGGLE-BOX
     SIZE 9.86 BY .63 NO-UNDO.

DEFINE VARIABLE tg-refat AS LOGICAL INITIAL no 
     LABEL "Refaturamento" 
     VIEW-AS TOGGLE-BOX
     SIZE 12.72 BY .63 NO-UNDO.

DEFINE VARIABLE tg-retalho AS LOGICAL INITIAL no 
     LABEL "Retalho" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .63 NO-UNDO.

DEFINE VARIABLE tg-todos AS LOGICAL INITIAL no 
     LABEL "TODAS" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fi-rua-ini AT ROW 2.88 COL 10 COLON-ALIGNED WIDGET-ID 32
     fi-rua-fin AT ROW 2.88 COL 26.72 COLON-ALIGNED NO-LABEL WIDGET-ID 30
     fi-doca-ini AT ROW 3.88 COL 10 COLON-ALIGNED WIDGET-ID 28
     fi-doca-fin AT ROW 3.88 COL 26.86 COLON-ALIGNED NO-LABEL WIDGET-ID 26
     rs-tipo AT ROW 6.67 COL 15.29 NO-LABEL WIDGET-ID 44
     tg-todos AT ROW 1.42 COL 49 WIDGET-ID 4
     tg-normal AT ROW 2.5 COL 49 WIDGET-ID 6
     tg-exclusivo AT ROW 3.25 COL 49 WIDGET-ID 12
     tg-bloqueada AT ROW 4 COL 49 WIDGET-ID 18
     tg-exporta AT ROW 4.75 COL 49 WIDGET-ID 10
     tg-amostra AT ROW 5.5 COL 49 WIDGET-ID 8
     tg-ind AT ROW 6.25 COL 49 WIDGET-ID 14
     tg-refat AT ROW 7 COL 49 WIDGET-ID 20
     tg-retalho AT ROW 7.75 COL 49 WIDGET-ID 16
     tg-pilotagem AT ROW 8.5 COL 49 WIDGET-ID 22
     tg-outros AT ROW 9.21 COL 49 WIDGET-ID 24
     bt-ok AT ROW 10.67 COL 3.29
     bt-cancelar AT ROW 10.67 COL 14.29
     bt-ajuda AT ROW 10.67 COL 59.86
     " Tipo" VIEW-AS TEXT
          SIZE 4 BY .54 AT ROW 5.96 COL 13 WIDGET-ID 50
     " Sele‡Æo" VIEW-AS TEXT
          SIZE 7 BY .54 AT ROW 1 COL 4.29
     "Localiza‡Æo:" VIEW-AS TEXT
          SIZE 8.86 BY .79 AT ROW 1.38 COL 40 WIDGET-ID 2
     RECT-1 AT ROW 10.46 COL 2
     RECT-50 AT ROW 1.25 COL 2
     IMAGE-1 AT ROW 2.88 COL 18.57 WIDGET-ID 34
     IMAGE-2 AT ROW 2.88 COL 25.72 WIDGET-ID 36
     IMAGE-3 AT ROW 3.88 COL 18.57 WIDGET-ID 38
     IMAGE-4 AT ROW 3.88 COL 25.72 WIDGET-ID 40
     RECT-51 AT ROW 6.21 COL 12 WIDGET-ID 48
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
         TITLE              = "Sele‡Æo do Relat¢rio de  Localiza‡äes"
         COLUMN             = 24.43
         ROW                = 12.58
         HEIGHT             = 11.04
         WIDTH              = 70.29
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
   FRAME-NAME Custom                                                    */
/* SETTINGS FOR TOGGLE-BOX tg-amostra IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-bloqueada IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-exclusivo IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-exporta IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-ind IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-normal IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-outros IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-pilotagem IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-refat IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-retalho IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-digita)
THEN w-digita:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-digita w-digita
ON END-ERROR OF w-digita /* Sele‡Æo do Relat¢rio de  Localiza‡äes */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-digita w-digita
ON WINDOW-CLOSE OF w-digita /* Sele‡Æo do Relat¢rio de  Localiza‡äes */
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
  
  ASSIGN l-todos-locais = INPUT FRAME {&FRAME-NAME} tg-todos
         l-normal       = INPUT FRAME {&FRAME-NAME} tg-normal   
         l-amostra      = INPUT FRAME {&FRAME-NAME} tg-amostra   
         l-exporta      = INPUT FRAME {&FRAME-NAME} tg-exporta
         l-exclusivo    = INPUT FRAME {&FRAME-NAME} tg-exclusivo
         l-ind          = INPUT FRAME {&FRAME-NAME} tg-ind
         l-retalho      = INPUT FRAME {&FRAME-NAME} tg-retalho   
         l-bloqueada    = INPUT FRAME {&FRAME-NAME} tg-bloqueada
         l-refat        = INPUT FRAME {&FRAME-NAME} tg-refat
         l-pilotagem    = INPUT FRAME {&FRAME-NAME} tg-pilotagem
         l-outros       = INPUT FRAME {&FRAME-NAME} tg-outros
         p-rua-ini      = fi-rua-ini:SCREEN-VALUE
         p-rua-fin      = INPUT FRAME {&FRAME-NAME} fi-rua-fin
         p-doca-ini     = fi-doca-ini:SCREEN-VALUE
         p-doca-fin     = INPUT FRAME {&FRAME-NAME} fi-doca-fin
         p-tipo         = INPUT FRAME {&FRAME-NAME} rs-tipo
         l-ok           = YES. 
  APPLY "CLOSE":U TO THIS-PROCEDURE.

           
 
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-todos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-todos w-digita
ON VALUE-CHANGED OF tg-todos IN FRAME F-Main /* TODAS */
DO:
    ASSIGN tg-normal:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = 'NO'
           tg-amostra:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = 'NO'
           tg-exporta:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = 'NO'
           tg-exclusivo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'
           tg-ind:SCREEN-VALUE IN FRAME {&FRAME-NAME}       = 'NO'
           tg-retalho:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = 'NO'
           tg-bloqueada:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'
           tg-refat:SCREEN-VALUE IN FRAME {&FRAME-NAME}     = 'NO'
           tg-pilotagem:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'
           tg-outros:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = 'NO'.
    IF INPUT FRAME {&FRAME-NAME} tg-todos = NO THEN
       ASSIGN tg-normal:SENSITIVE IN FRAME {&FRAME-NAME}    = YES
              tg-amostra:SENSITIVE IN FRAME {&FRAME-NAME}   = YES
              tg-exporta:SENSITIVE IN FRAME {&FRAME-NAME}   = YES
              tg-exclusivo:SENSITIVE IN FRAME {&FRAME-NAME} = YES
              tg-ind:SENSITIVE IN FRAME {&FRAME-NAME}       = YES
              tg-retalho:SENSITIVE IN FRAME {&FRAME-NAME}   = YES
              tg-bloqueada:SENSITIVE IN FRAME {&FRAME-NAME} = YES
              tg-refat:SENSITIVE IN FRAME {&FRAME-NAME}     = YES
              tg-pilotagem:SENSITIVE IN FRAME {&FRAME-NAME} = YES
              tg-outros:SENSITIVE IN FRAME {&FRAME-NAME}    = YES
              tg-normal:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'YES'.
    ELSE
        ASSIGN tg-normal:SENSITIVE IN FRAME {&FRAME-NAME}    = NO 
               tg-amostra:SENSITIVE IN FRAME {&FRAME-NAME}   = NO 
               tg-exporta:SENSITIVE IN FRAME {&FRAME-NAME}   = NO 
               tg-exclusivo:SENSITIVE IN FRAME {&FRAME-NAME} = NO 
               tg-ind:SENSITIVE IN FRAME {&FRAME-NAME}       = NO 
               tg-retalho:SENSITIVE IN FRAME {&FRAME-NAME}   = NO 
               tg-bloqueada:SENSITIVE IN FRAME {&FRAME-NAME} = NO 
               tg-refat:SENSITIVE IN FRAME {&FRAME-NAME}     = NO 
               tg-pilotagem:SENSITIVE IN FRAME {&FRAME-NAME} = NO 
               tg-outros:SENSITIVE IN FRAME {&FRAME-NAME}    = NO.
  
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
  DISPLAY fi-rua-ini fi-rua-fin fi-doca-ini fi-doca-fin rs-tipo tg-todos 
          tg-normal tg-exclusivo tg-bloqueada tg-exporta tg-amostra tg-ind 
          tg-refat tg-retalho tg-pilotagem tg-outros 
      WITH FRAME F-Main IN WINDOW w-digita.
  ENABLE fi-rua-ini fi-rua-fin fi-doca-ini fi-doca-fin RECT-1 rs-tipo RECT-50 
         tg-todos IMAGE-1 IMAGE-2 IMAGE-3 IMAGE-4 RECT-51 bt-ok bt-cancelar 
         bt-ajuda 
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
  
  ASSIGN tg-todos     = l-todos-locais   
         tg-normal    = l-normal
         tg-amostra   = l-amostra
         tg-exporta   = l-exporta
         tg-exclusivo = l-exclusivo
         tg-ind       = l-ind
         tg-retalho   = l-retalho
         tg-bloqueada = l-bloqueada
         tg-refat     = l-refat
         tg-pilotagem = l-pilotagem
         tg-outros    = l-outros.

 
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

