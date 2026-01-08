&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          mgmov            PROGRESS
*/
&Scoped-define WINDOW-NAME w-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-digita 
/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ACT001 2.04.00.000}
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: 
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEF VAR i-tp-embal AS INT.
DEF VAR da-dt-liq  LIKE titulo.dt-liq.

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
&Scoped-Define ENABLED-FIELDS titulo.ep-codigo titulo.cod-estabel ~
titulo.cod-esp titulo.serie titulo.nr-docto titulo.parcela 
&Scoped-define ENABLED-TABLES titulo
&Scoped-define FIRST-ENABLED-TABLE titulo
&Scoped-Define ENABLED-OBJECTS RECT-1 BUTTON-1 fi-dt-liq bt-cancelar ~
bt-ajuda 
&Scoped-Define DISPLAYED-FIELDS titulo.ep-codigo titulo.cod-estabel ~
titulo.cod-esp titulo.serie titulo.nr-docto titulo.parcela ~
titulo.cod-emitente titulo.vl-original titulo.dt-liq 
&Scoped-define DISPLAYED-TABLES titulo
&Scoped-define FIRST-DISPLAYED-TABLE titulo
&Scoped-Define DISPLAYED-OBJECTS fi-nome-emit fi-dt-liq 

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

DEFINE BUTTON bt-ant 
     IMAGE-UP FILE "image/im-ante.bmp":U
     LABEL "Button 2" 
     SIZE 4 BY 1 TOOLTIP "Consulta Etiqueta Anterior".

DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "Sair" 
     SIZE 10 BY 1 TOOLTIP "Sair do Programa.".

DEFINE BUTTON bt-desfaz AUTO-END-KEY 
     LABEL "Desfazer" 
     SIZE 10 BY 1 TOOLTIP "Desfaz a alteraá∆o.".

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "OK" 
     SIZE 10 BY 1 TOOLTIP "Confirma a alteraá∆o.".

DEFINE BUTTON bt-prox 
     IMAGE-UP FILE "image/im-nex.bmp":U
     LABEL "Button 3" 
     SIZE 4 BY 1 TOOLTIP "Consulta Proxima Etiqueta".

DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "image/im-vapra.bmp":U
     LABEL "Button 1" 
     SIZE 4 BY .88.

DEFINE VARIABLE fi-dt-liq AS DATE FORMAT "99/99/9999":U 
     LABEL "Nova Data" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "Nova data de liquidaá∆o do t°tulo." NO-UNDO.

DEFINE VARIABLE fi-nome-emit AS CHARACTER FORMAT "X(40)" 
     VIEW-AS FILL-IN 
     SIZE 48 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 80 BY 1.38
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     titulo.ep-codigo AT ROW 1.29 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 3 BY .88
     titulo.cod-estabel AT ROW 2.29 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4 BY .88
     titulo.cod-esp AT ROW 3.29 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4 BY .88
     titulo.serie AT ROW 4.29 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4 BY .88
     titulo.nr-docto AT ROW 5.29 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
     bt-ant AT ROW 6.21 COL 23.29
     bt-prox AT ROW 6.21 COL 27.29
     titulo.parcela AT ROW 6.29 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4 BY .88
     BUTTON-1 AT ROW 6.29 COL 18.43
     titulo.cod-emitente AT ROW 7.29 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8.86 BY .88
     fi-nome-emit AT ROW 7.29 COL 21 COLON-ALIGNED HELP
          "Nome Completo do Emitente" NO-LABEL
     titulo.vl-original AT ROW 8.29 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8.86 BY .88
     titulo.dt-liq AT ROW 9.29 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
     fi-dt-liq AT ROW 9.29 COL 31.86 COLON-ALIGNED
     bt-ok AT ROW 11.25 COL 1.72
     bt-desfaz AT ROW 11.25 COL 12
     bt-cancelar AT ROW 11.25 COL 25.14
     bt-ajuda AT ROW 11.25 COL 70.57
     RECT-1 AT ROW 11.04 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 14.29
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
         TITLE              = "Acerta Data de Liquidaá∆o de T°tulos"
         HEIGHT             = 11.63
         WIDTH              = 80
         MAX-HEIGHT         = 22
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 22
         VIRTUAL-WIDTH      = 114.29
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
   FRAME-NAME                                                           */
/* SETTINGS FOR BUTTON bt-ant IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-desfaz IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-ok IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-prox IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN titulo.cod-emitente IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN titulo.dt-liq IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nome-emit IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN titulo.vl-original IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-digita)
THEN w-digita:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-digita w-digita
ON END-ERROR OF w-digita /* Acerta Data de Liquidaá∆o de T°tulos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-digita w-digita
ON WINDOW-CLOSE OF w-digita /* Acerta Data de Liquidaá∆o de T°tulos */
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


&Scoped-define SELF-NAME bt-ant
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ant w-digita
ON CHOOSE OF bt-ant IN FRAME F-Main /* Button 2 */
DO:
  FIND PREV titulo NO-LOCK NO-ERROR.
  IF AVAIL titulo THEN DO:
     ASSIGN titulo.ep-codigo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(titulo.ep-codigo)
            titulo.cod-estabel:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(titulo.cod-estabel)
            titulo.cod-esp:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(titulo.cod-esp)
            titulo.serie:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(titulo.serie)
            titulo.nr-docto:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(titulo.nr-docto)
            titulo.parcela:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(titulo.parcela).
     apply "choose":U to BUTTON-1.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancelar w-digita
ON CHOOSE OF bt-cancelar IN FRAME F-Main /* Sair */
DO:
  apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-desfaz
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-desfaz w-digita
ON CHOOSE OF bt-desfaz IN FRAME F-Main /* Desfazer */
DO:
  apply "choose":U to BUTTON-1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-digita
ON CHOOSE OF bt-ok IN FRAME F-Main /* OK */
DO:
  FIND CURRENT titulo EXCLUSIVE-LOCK NO-ERROR.
  ASSIGN da-dt-liq = titulo.dt-liq.
  ASSIGN titulo.dt-liq = INPUT FRAME {&FRAME-NAME} fi-dt-liq.
  FOR EACH mov-tit OF titulo WHERE mov-tit.dt-credito = da-dt-liq:
      ASSIGN mov-tit.dt-credito = INPUT FRAME {&FRAME-NAME} fi-dt-liq
             mov-tit.dt-movto = INPUT FRAME {&FRAME-NAME} fi-dt-liq
             mov-tit.dt-trans = INPUT FRAME {&FRAME-NAME} fi-dt-liq.
  END.
  MESSAGE "T°tulo atualizado com a nova data de liquidaá∆o."
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
  ASSIGN fi-dt-liq:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
  APPLY 'entry' TO titulo.parcela.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-prox
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-prox w-digita
ON CHOOSE OF bt-prox IN FRAME F-Main /* Button 3 */
DO:
  FIND NEXT titulo NO-LOCK NO-ERROR.
  IF AVAIL titulo THEN DO:
     ASSIGN titulo.ep-codigo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(titulo.ep-codigo)
            titulo.cod-estabel:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(titulo.cod-estabel)
            titulo.cod-esp:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(titulo.cod-esp)
            titulo.serie:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(titulo.serie)
            titulo.nr-docto:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(titulo.nr-docto)
            titulo.parcela:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(titulo.parcela).
     apply "choose":U to BUTTON-1.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 w-digita
ON CHOOSE OF BUTTON-1 IN FRAME F-Main /* Button 1 */
DO:
  FIND titulo WHERE titulo.ep-codigo   = INPUT FRAME {&FRAME-NAME} titulo.ep-codigo
                AND titulo.cod-estabel = INPUT FRAME {&FRAME-NAME} titulo.cod-estabel
                AND titulo.cod-esp     = INPUT FRAME {&FRAME-NAME} titulo.cod-esp
                AND titulo.serie       = INPUT FRAME {&FRAME-NAME} titulo.serie
                AND titulo.nr-docto    = INPUT FRAME {&FRAME-NAME} titulo.nr-docto
                AND titulo.parcela     = INPUT FRAME {&FRAME-NAME} titulo.parcela
              NO-LOCK NO-ERROR.
  
  IF NOT AVAIL titulo THEN DO:
     MESSAGE "T°tulo n∆o encontrado."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     APPLY 'entry' TO titulo.nr-docto IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END. 
  ELSE DO:
     IF titulo.vl-saldo <> 0 THEN DO:
        MESSAGE "T°tulo n∆o est† liquidado. N∆o pode ser alterado."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY 'entry' TO titulo.nr-docto IN FRAME {&FRAME-NAME}.
        RETURN NO-APPLY.
     END.
     FIND emitente WHERE emitente.cod-emitente = titulo.cod-emitente NO-LOCK.
     ASSIGN titulo.cod-emitente:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(titulo.cod-emitente)
            fi-nome-emit:SCREEN-VALUE IN FRAME {&FRAME-NAME} = emitente.nome-emit
            titulo.vl-original:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(titulo.vl-original)
            titulo.dt-liq:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(titulo.dt-liq)
            fi-dt-liq:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(titulo.dt-liq).
     ASSIGN bt-ok:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            bt-desfaz:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            bt-ant:SENSITIVE IN FRAME {&FRAME-NAME}  = YES
            bt-prox:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
     APPLY 'entry' TO fi-dt-liq IN FRAME {&FRAME-NAME}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME titulo.parcela
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL titulo.parcela w-digita
ON LEAVE OF titulo.parcela IN FRAME F-Main /* Parcela */
DO:
   APPLY 'choose' TO button-1.
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
  DISPLAY fi-nome-emit fi-dt-liq 
      WITH FRAME F-Main IN WINDOW w-digita.
  IF AVAILABLE titulo THEN 
    DISPLAY titulo.ep-codigo titulo.cod-estabel titulo.cod-esp titulo.serie 
          titulo.nr-docto titulo.parcela titulo.cod-emitente titulo.vl-original 
          titulo.dt-liq 
      WITH FRAME F-Main IN WINDOW w-digita.
  ENABLE RECT-1 titulo.ep-codigo titulo.cod-estabel titulo.cod-esp titulo.serie 
         titulo.nr-docto titulo.parcela BUTTON-1 fi-dt-liq bt-cancelar bt-ajuda 
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

  /* {utp/ut9000.i "ACT0001" "2.04.00.000"} */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  {include/i-inifld.i}

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

