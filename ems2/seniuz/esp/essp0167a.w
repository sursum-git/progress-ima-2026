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
{include/i-prgvrs.i ESSP0167A 2.04.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT-OUTPUT PARAMETER c-dt-limite-ini     AS CHAR.
DEFINE INPUT-OUTPUT PARAMETER c-dt-limite-fin     AS CHAR.
DEFINE INPUT-OUTPUT PARAMETER c-nr-pedcli-ini     LIKE ped-item-ext.nr-pedcli.
DEFINE INPUT-OUTPUT PARAMETER c-nr-pedcli-fin     LIKE ped-item-ext.nr-pedcli.
DEFINE INPUT-OUTPUT PARAMETER c-nome-abrev-ini    LIKE ped-venda.nome-abrev. 
DEFINE INPUT-OUTPUT PARAMETER c-nome-abrev-fin    LIKE ped-venda.nome-abrev.
DEFINE INPUT-OUTPUT PARAMETER c-no-ab-reppri-ini  LIKE ped-venda.no-ab-reppri. 
DEFINE INPUT-OUTPUT PARAMETER c-no-ab-reppri-fin  LIKE ped-venda.no-ab-reppri.
DEFINE INPUT-OUTPUT PARAMETER i-reserva           AS INT.
DEFINE INPUT-OUTPUT PARAMETER l-ok                AS LOG.

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
&Scoped-Define ENABLED-OBJECTS IMAGE-3 IMAGE-4 IMAGE-5 IMAGE-6 IMAGE-88 ~
IMAGE-89 IMAGE-90 IMAGE-91 RECT-1 RECT-50 RECT-52 fi-dt-limite-ini ~
fi-dt-limite-fin fi-nr-pedcli-ini fi-nr-pedcli-fin fi-nome-abrev-ini ~
fi-nome-abrev-fin fi-no-ab-reppri-ini fi-no-ab-reppri-fin rs-reserva ~
bt-ajuda bt-ok bt-cancelar 
&Scoped-Define DISPLAYED-OBJECTS fi-dt-limite-ini fi-dt-limite-fin ~
fi-nr-pedcli-ini fi-nr-pedcli-fin fi-nome-abrev-ini fi-nome-abrev-fin ~
fi-no-ab-reppri-ini fi-no-ab-reppri-fin rs-reserva 

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

DEFINE VARIABLE fi-dt-limite-fin AS CHARACTER FORMAT "99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 TOOLTIP "Data limite final (MM/AAAA)." NO-UNDO.

DEFINE VARIABLE fi-dt-limite-ini AS CHARACTER FORMAT "99/9999":U 
     LABEL "Data Limite" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 TOOLTIP "Data limite inicial (MM/AAAA)." NO-UNDO.

DEFINE VARIABLE fi-no-ab-reppri-fin AS CHARACTER FORMAT "x(16)" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 TOOLTIP "Representante final." NO-UNDO.

DEFINE VARIABLE fi-no-ab-reppri-ini AS CHARACTER FORMAT "x(16)" 
     LABEL "Representante" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 TOOLTIP "Representante inicial." NO-UNDO.

DEFINE VARIABLE fi-nome-abrev-fin AS CHARACTER FORMAT "x(12)" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 TOOLTIP "Cliente final." NO-UNDO.

DEFINE VARIABLE fi-nome-abrev-ini AS CHARACTER FORMAT "x(12)" 
     LABEL "Cliente":R9 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 TOOLTIP "Cliente inicial." NO-UNDO.

DEFINE VARIABLE fi-nr-pedcli-fin AS CHARACTER FORMAT "x(12)" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 TOOLTIP "Pedido final." NO-UNDO.

DEFINE VARIABLE fi-nr-pedcli-ini AS CHARACTER FORMAT "x(12)" 
     LABEL "Pedido":R17 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 TOOLTIP "Pedido inicial." NO-UNDO.

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

DEFINE IMAGE IMAGE-88
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-89
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-90
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-91
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE VARIABLE rs-reserva AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Pedidos Com Reserva", 1,
"Pedidos Sem Reserva", 2,
"Ambos", 3
     SIZE 20 BY 2.75 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 65 BY 1.38
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-50
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 65 BY 8.75.

DEFINE RECTANGLE RECT-52
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 34 BY 3.25.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fi-dt-limite-ini AT ROW 1.75 COL 14 COLON-ALIGNED
     fi-dt-limite-fin AT ROW 1.75 COL 42 COLON-ALIGNED NO-LABEL
     fi-nr-pedcli-ini AT ROW 2.75 COL 14 COLON-ALIGNED HELP
          "N£mero do pedido do cliente"
     fi-nr-pedcli-fin AT ROW 2.75 COL 42 COLON-ALIGNED HELP
          "N£mero do pedido do cliente" NO-LABEL
     fi-nome-abrev-ini AT ROW 3.75 COL 14 COLON-ALIGNED HELP
          "Nome abreviado do cliente"
     fi-nome-abrev-fin AT ROW 3.75 COL 42 COLON-ALIGNED HELP
          "Nome abreviado do cliente" NO-LABEL
     fi-no-ab-reppri-ini AT ROW 4.75 COL 14 COLON-ALIGNED HELP
          "Codigo do Ötem no EMS"
     fi-no-ab-reppri-fin AT ROW 4.75 COL 42 COLON-ALIGNED HELP
          "Codigo do Ötem no EMS" NO-LABEL
     rs-reserva AT ROW 6.58 COL 25 NO-LABEL
     bt-ajuda AT ROW 10.33 COL 55.86
     bt-ok AT ROW 10.38 COL 3.29
     bt-cancelar AT ROW 10.38 COL 14.29
     " Sele‡Æo" VIEW-AS TEXT
          SIZE 7 BY .54 AT ROW 1 COL 4.29
     IMAGE-3 AT ROW 2.75 COL 31.57
     IMAGE-4 AT ROW 2.75 COL 40.57
     IMAGE-5 AT ROW 3.75 COL 31.57
     IMAGE-6 AT ROW 3.75 COL 40.57
     IMAGE-88 AT ROW 1.75 COL 31.57
     IMAGE-89 AT ROW 1.75 COL 40.57
     IMAGE-90 AT ROW 4.75 COL 31.57
     IMAGE-91 AT ROW 4.75 COL 40.57
     RECT-1 AT ROW 10.17 COL 2
     RECT-50 AT ROW 1.25 COL 2
     RECT-52 AT ROW 6.25 COL 17.72
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
         TITLE              = "Sele‡Æo de Pedido"
         COLUMN             = 43.29
         ROW                = 8.83
         HEIGHT             = 10.71
         WIDTH              = 66.43
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
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-digita)
THEN w-digita:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-digita w-digita
ON END-ERROR OF w-digita /* Sele‡Æo de Pedido */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-digita w-digita
ON WINDOW-CLOSE OF w-digita /* Sele‡Æo de Pedido */
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
  apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-digita
ON CHOOSE OF bt-ok IN FRAME F-Main /* OK */
DO:
  ASSIGN INPUT FRAME {&FRAME-NAME} fi-dt-limite-ini.

  IF INT(SUBSTR(fi-dt-limite-ini,1,2)) <  1 OR
     INT(SUBSTR(fi-dt-limite-ini,1,2)) > 12  THEN DO:
      MESSAGE "MES invalido ! ! !"  VIEW-AS ALERT-BOX. 
      APPLY 'entry' TO fi-dt-limite-ini.
      RETURN NO-APPLY.
  END.
  IF INT(SUBSTR(fi-dt-limite-ini,4,4)) <  1 THEN DO:
      MESSAGE "ANO invalido ! ! !"  VIEW-AS ALERT-BOX. 
      APPLY 'entry' TO fi-dt-limite-ini.
      RETURN NO-APPLY.
  END.

  ASSIGN c-dt-limite-ini    = INPUT FRAME {&FRAME-NAME} fi-dt-limite-ini   
         c-dt-limite-fin    = INPUT FRAME {&FRAME-NAME} fi-dt-limite-fin   
         c-nr-pedcli-ini    = INPUT FRAME {&FRAME-NAME} fi-nr-pedcli-ini   
         c-nr-pedcli-fin    = INPUT FRAME {&FRAME-NAME} fi-nr-pedcli-fin   
         c-nome-abrev-ini   = INPUT FRAME {&FRAME-NAME} fi-nome-abrev-ini
         c-nome-abrev-fin   = INPUT FRAME {&FRAME-NAME} fi-nome-abrev-fin
         c-no-ab-reppri-ini = INPUT FRAME {&FRAME-NAME} fi-no-ab-reppri-ini
         c-no-ab-reppri-fin = INPUT FRAME {&FRAME-NAME} fi-no-ab-reppri-fin
         i-reserva          = INPUT FRAME {&FRAME-NAME} rs-reserva
         l-ok = YES.  

  apply "close":U to this-procedure.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-dt-limite-fin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-dt-limite-fin w-digita
ON LEAVE OF fi-dt-limite-fin IN FRAME F-Main
DO:
  IF INT(SUBSTR(SELF:SCREEN-VALUE,1,2)) <  1 OR
     INT(SUBSTR(SELF:SCREEN-VALUE,1,2)) > 12  THEN DO:
      MESSAGE "MES invalido ! ! !"  VIEW-AS ALERT-BOX. 
      APPLY 'entry' TO SELF.
      RETURN NO-APPLY.
  END.
  IF INT(SUBSTR(SELF:SCREEN-VALUE,4,4)) <  1 THEN DO:
      MESSAGE "ANO invalido ! ! !"  VIEW-AS ALERT-BOX. 
      APPLY 'entry' TO SELF.
      RETURN NO-APPLY.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-no-ab-reppri-fin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-no-ab-reppri-fin w-digita
ON MOUSE-SELECT-DBLCLICK OF fi-no-ab-reppri-fin IN FRAME F-Main
DO:
  {include/zoomvar.i &prog-zoom = adzoom/z01ad229.w
                     &campo     = fi-no-ab-reppri-fin
                     &campozoom = nome-abrev}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-no-ab-reppri-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-no-ab-reppri-ini w-digita
ON LEAVE OF fi-no-ab-reppri-ini IN FRAME F-Main /* Representante */
DO:
  IF SELF:SCREEN-VALUE <> '' THEN
     ASSIGN fi-no-ab-reppri-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-no-ab-reppri-ini w-digita
ON MOUSE-SELECT-DBLCLICK OF fi-no-ab-reppri-ini IN FRAME F-Main /* Representante */
DO:
  {include/zoomvar.i &prog-zoom = adzoom/z01ad229.w
                     &campo     = fi-no-ab-reppri-ini
                     &campozoom = nome-abrev}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-nome-abrev-fin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nome-abrev-fin w-digita
ON LEAVE OF fi-nome-abrev-fin IN FRAME F-Main
DO:
   IF SELF:SCREEN-VALUE <> '' THEN DO.
      FIND emitente USE-INDEX nome WHERE 
           emitente.nome-abrev = INPUT FRAME {&FRAME-NAME} fi-nome-abrev-fin NO-LOCK NO-ERROR.
      IF NOT AVAIL emitente THEN
         FIND emitente USE-INDEX codigo WHERE 
              STRING(emitente.cod-emit) = INPUT FRAME {&FRAME-NAME} fi-nome-abrev-fin NO-LOCK NO-ERROR.

      IF AVAIL emitente THEN
          ASSIGN fi-nome-abrev-fin:SCREEN-VALUE IN FRAME {&FRAME-NAME} = emitente.nome-abrev.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nome-abrev-fin w-digita
ON MOUSE-SELECT-DBLCLICK OF fi-nome-abrev-fin IN FRAME F-Main
DO:
    {include/zoomvar.i &prog-zoom = adzoom/z02ad098.w
                       &campo     = fi-nome-abrev-fin
                       &campozoom = nome-abrev}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-nome-abrev-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nome-abrev-ini w-digita
ON LEAVE OF fi-nome-abrev-ini IN FRAME F-Main /* Cliente */
DO:
  IF SELF:SCREEN-VALUE <> '' THEN
     FIND emitente WHERE 
          emitente.nome-abrev = INPUT FRAME {&FRAME-NAME} fi-nome-abrev-ini
          USE-INDEX nome NO-LOCK NO-ERROR.
     IF NOT AVAIL emitente THEN
        FIND emitente WHERE 
             STRING(emitente.cod-emit) = INPUT FRAME {&FRAME-NAME} fi-nome-abrev-ini 
             USE-INDEX codigo NO-LOCK NO-ERROR.

     IF AVAIL emitente THEN
         ASSIGN fi-nome-abrev-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = emitente.nome-abrev.

     ASSIGN fi-nome-abrev-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nome-abrev-ini w-digita
ON MOUSE-SELECT-DBLCLICK OF fi-nome-abrev-ini IN FRAME F-Main /* Cliente */
DO:
    {include/zoomvar.i &prog-zoom = adzoom/z02ad098.w
                       &campo     = fi-nome-abrev-ini
                       &campozoom = nome-abrev}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-nr-pedcli-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nr-pedcli-ini w-digita
ON LEAVE OF fi-nr-pedcli-ini IN FRAME F-Main /* Pedido */
DO:
  IF SELF:SCREEN-VALUE <> '' THEN
     ASSIGN fi-nr-pedcli-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-digita 


/* ***************************  Main Block  *************************** */
fi-nome-abrev-ini:LOAD-MOUSE-POINTER("image/lupa.cur")   IN FRAME {&FRAME-NAME}.
fi-nome-abrev-fin:LOAD-MOUSE-POINTER("image/lupa.cur")   IN FRAME {&FRAME-NAME}.
fi-no-ab-reppri-ini:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.
fi-no-ab-reppri-fin:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.

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
  DISPLAY fi-dt-limite-ini fi-dt-limite-fin fi-nr-pedcli-ini fi-nr-pedcli-fin 
          fi-nome-abrev-ini fi-nome-abrev-fin fi-no-ab-reppri-ini 
          fi-no-ab-reppri-fin rs-reserva 
      WITH FRAME F-Main IN WINDOW w-digita.
  ENABLE IMAGE-3 IMAGE-4 IMAGE-5 IMAGE-6 IMAGE-88 IMAGE-89 IMAGE-90 IMAGE-91 
         RECT-1 RECT-50 RECT-52 fi-dt-limite-ini fi-dt-limite-fin 
         fi-nr-pedcli-ini fi-nr-pedcli-fin fi-nome-abrev-ini fi-nome-abrev-fin 
         fi-no-ab-reppri-ini fi-no-ab-reppri-fin rs-reserva bt-ajuda bt-ok 
         bt-cancelar 
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
  ASSIGN fi-dt-limite-ini    = c-dt-limite-ini   
         fi-dt-limite-fin    = c-dt-limite-fin
         fi-nr-pedcli-ini    = c-nr-pedcli-ini   
         fi-nr-pedcli-fin    = c-nr-pedcli-fin   
         fi-nome-abrev-ini   = c-nome-abrev-ini
         fi-nome-abrev-fin   = c-nome-abrev-fin
         fi-no-ab-reppri-ini = c-no-ab-reppri-ini
         fi-no-ab-reppri-fin = c-no-ab-reppri-fin
         rs-reserva          = i-reserva.

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

