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
** AUTOR: FµBIO COELHO LANZA - JULHO 2010.
*******************************************************************************/
{include/i-prgvrs.i ESSP0197A 2.04.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT-OUTPUT PARAMETER da-dt-saida       LIKE nota-fiscal.dt-saida.
DEFINE INPUT-OUTPUT PARAMETER c-nome-transp-ini LIKE nota-fiscal.nome-transp.
DEFINE INPUT-OUTPUT PARAMETER c-nome-transp-fin LIKE nota-fiscal.nome-transp.
DEFINE INPUT-OUTPUT PARAMETER de-cap-diaria01   AS DEC.
DEFINE INPUT-OUTPUT PARAMETER de-cap-diaria02   AS DEC.
DEFINE INPUT-OUTPUT PARAMETER de-cap-diaria03   AS DEC.
DEFINE INPUT-OUTPUT PARAMETER de-cap-diaria04   AS DEC.
DEFINE INPUT-OUTPUT PARAMETER de-cap-diaria05   AS DEC.
DEFINE INPUT-OUTPUT PARAMETER c-cod-estabel     LIKE estabelec.cod-estabel.
DEFINE INPUT-OUTPUT PARAMETER l-ok              AS LOG.

DEF VAR c-semana  AS CHAR INIT "DOMINGO,2¶ FEIRA,3¶ FEIRA,4¶ FEIRA,5¶ FEIRA,6¶ FEIRA,SABADO".

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
&Scoped-Define ENABLED-OBJECTS fi-dt-saida fi-nome-transp-ini ~
fi-nome-transp-fin fi-cap-diaria01 fi-cap-diaria02 fi-cap-diaria03 ~
fi-cap-diaria04 fi-cap-diaria05 bt-ajuda bt-ok bt-cancelar fi-cod-estabel ~
IMAGE-90 IMAGE-91 RECT-1 RECT-50 
&Scoped-Define DISPLAYED-OBJECTS fi-cab-01 fi-dt-saida fi-nome-transp-ini ~
fi-nome-transp-fin fi-cap-diaria01 fi-cap-diaria02 fi-cap-diaria03 ~
fi-cap-diaria04 fi-cap-diaria05 fi-cod-estabel fi-cab-02 fi-cab-03 ~
fi-cab-04 fi-cab-05 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-dia-mes w-digita 
FUNCTION fn-dia-mes RETURNS CHARACTER
  (INPUT da-dt-saida AS DATE) FORWARD.

/* _UIB-CODE-BLOCK-END */
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

DEFINE VARIABLE fi-cab-01 AS CHARACTER FORMAT "X(17)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi-cab-02 AS CHARACTER FORMAT "X(17)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi-cab-03 AS CHARACTER FORMAT "X(17)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi-cab-04 AS CHARACTER FORMAT "X(17)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi-cab-05 AS CHARACTER FORMAT "X(17)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi-cap-diaria01 AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Capacidade Diaria Carregamentos" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 TOOLTIP "Capacidade Diaria de Carregamento" NO-UNDO.

DEFINE VARIABLE fi-cap-diaria02 AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 TOOLTIP "Capacidade Diaria de Carregamento" NO-UNDO.

DEFINE VARIABLE fi-cap-diaria03 AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 TOOLTIP "Capacidade Diaria de Carregamento" NO-UNDO.

DEFINE VARIABLE fi-cap-diaria04 AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 TOOLTIP "Capacidade Diaria de Carregamento" NO-UNDO.

DEFINE VARIABLE fi-cap-diaria05 AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 TOOLTIP "Capacidade Diaria de Carregamento" NO-UNDO.

DEFINE VARIABLE fi-cod-estabel AS CHARACTER FORMAT "X(3)":U 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-saida AS DATE FORMAT "99/99/9999" 
     LABEL "Data Saida":R14 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "Data de Embarque da Mercadoria" NO-UNDO.

DEFINE VARIABLE fi-nome-transp-fin AS CHARACTER FORMAT "X(12)" INITIAL "ZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 TOOLTIP "Transportadora Final" NO-UNDO.

DEFINE VARIABLE fi-nome-transp-ini AS CHARACTER FORMAT "X(12)" 
     LABEL "Transportadora":R17 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 TOOLTIP "Transportadora Inicial" NO-UNDO.

DEFINE IMAGE IMAGE-90
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-91
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 81.14 BY 1.38
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-50
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 81 BY 8.88.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fi-cab-01 AT ROW 4.75 COL 35.29 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     fi-dt-saida AT ROW 2.75 COL 26.14 COLON-ALIGNED HELP
          "Data de embarque da nota fiscal"
     fi-nome-transp-ini AT ROW 3.75 COL 26.14 COLON-ALIGNED
     fi-nome-transp-fin AT ROW 3.75 COL 55.43 COLON-ALIGNED NO-LABEL
     fi-cap-diaria01 AT ROW 4.75 COL 26.14 COLON-ALIGNED WIDGET-ID 2
     fi-cap-diaria02 AT ROW 5.75 COL 26.14 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     fi-cap-diaria03 AT ROW 6.75 COL 26.14 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     fi-cap-diaria04 AT ROW 7.75 COL 26.14 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     fi-cap-diaria05 AT ROW 8.75 COL 26.14 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     bt-ajuda AT ROW 10.58 COL 71.72
     bt-ok AT ROW 10.63 COL 3
     bt-cancelar AT ROW 10.63 COL 14
     fi-cod-estabel AT ROW 1.75 COL 26.14 COLON-ALIGNED WIDGET-ID 4
     fi-cab-02 AT ROW 5.75 COL 35.29 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     fi-cab-03 AT ROW 6.75 COL 35.29 COLON-ALIGNED NO-LABEL WIDGET-ID 18
     fi-cab-04 AT ROW 7.75 COL 35.29 COLON-ALIGNED NO-LABEL WIDGET-ID 20
     fi-cab-05 AT ROW 8.75 COL 35.29 COLON-ALIGNED NO-LABEL WIDGET-ID 22
     " Seleá∆o" VIEW-AS TEXT
          SIZE 7 BY .54 AT ROW 1 COL 4.29
     IMAGE-90 AT ROW 3.75 COL 42.14
     IMAGE-91 AT ROW 3.75 COL 54.43
     RECT-1 AT ROW 10.38 COL 1.72
     RECT-50 AT ROW 1.38 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.14 ROW 1
         SIZE 83.43 BY 11.08
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
         TITLE              = "Seleá∆o do Controle Diario de Carregamento"
         COLUMN             = 31.86
         ROW                = 13.71
         HEIGHT             = 10.83
         WIDTH              = 83.57
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
   FRAME-NAME Custom                                                    */
/* SETTINGS FOR FILL-IN fi-cab-01 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-cab-02 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-cab-03 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-cab-04 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-cab-05 IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-digita)
THEN w-digita:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-digita w-digita
ON END-ERROR OF w-digita /* Seleá∆o do Controle Diario de Carregamento */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-digita w-digita
ON WINDOW-CLOSE OF w-digita /* Seleá∆o do Controle Diario de Carregamento */
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
  ASSIGN da-dt-saida       = INPUT FRAME {&FRAME-NAME} fi-dt-saida   
         c-nome-transp-ini = INPUT FRAME {&FRAME-NAME} fi-nome-transp-ini   
         c-nome-transp-fin = INPUT FRAME {&FRAME-NAME} fi-nome-transp-fin
         de-cap-diaria01   = INPUT FRAME {&FRAME-NAME} fi-cap-diaria01
         de-cap-diaria02   = INPUT FRAME {&FRAME-NAME} fi-cap-diaria02
         de-cap-diaria03   = INPUT FRAME {&FRAME-NAME} fi-cap-diaria03
         de-cap-diaria04   = INPUT FRAME {&FRAME-NAME} fi-cap-diaria04
         de-cap-diaria05   = INPUT FRAME {&FRAME-NAME} fi-cap-diaria05
         c-cod-estabel     = INPUT FRAME {&FRAME-NAME} fi-cod-estabel
         l-ok              = YES.  

  APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-estabel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-estabel w-digita
ON LEAVE OF fi-cod-estabel IN FRAME F-Main /* Estabelecimento */
DO:
  FIND estabelec WHERE
       estabelec.cod-estabel = INPUT FRAME {&FRAME-NAME} fi-cod-estabel NO-LOCK NO-ERROR.

  IF NOT AVAIL estabelec THEN DO.
     MESSAGE "Estabelecimento n∆o Cadastrado..." VIEW-AS ALERT-BOX.
     RETURN NO-APPLY.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-estabel w-digita
ON MOUSE-SELECT-DBLCLICK OF fi-cod-estabel IN FRAME F-Main /* Estabelecimento */
DO:
  {include/zoomvar.i &prog-zoom=adzoom/z01ad107.w
                     &campo=fi-cod-estabel
                     &campozoom=cod-estabel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-dt-saida
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-dt-saida w-digita
ON ENTRY OF fi-dt-saida IN FRAME F-Main /* Data Saida */
DO:
  ASSIGN fi-cab-01:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" 
         fi-cab-02:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" 
         fi-cab-03:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" 
         fi-cab-04:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" 
         fi-cab-05:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-dt-saida w-digita
ON LEAVE OF fi-dt-saida IN FRAME F-Main /* Data Saida */
DO:
   ASSIGN da-dt-saida = INPUT FRAME {&FRAME-NAME} fi-dt-saida.
   ASSIGN fi-cab-01:SCREEN-VALUE IN FRAME {&FRAME-NAME} = fn-dia-mes(INPUT da-dt-saida)     + ENTRY(WEEKDAY(da-dt-saida),     c-semana,",") 
          fi-cab-02:SCREEN-VALUE IN FRAME {&FRAME-NAME} = fn-dia-mes(INPUT da-dt-saida + 1) + ENTRY(WEEKDAY(da-dt-saida + 1), c-semana,",") 
          fi-cab-03:SCREEN-VALUE IN FRAME {&FRAME-NAME} = fn-dia-mes(INPUT da-dt-saida + 2) + ENTRY(WEEKDAY(da-dt-saida + 2), c-semana,",") 
          fi-cab-04:SCREEN-VALUE IN FRAME {&FRAME-NAME} = fn-dia-mes(INPUT da-dt-saida + 3) + ENTRY(WEEKDAY(da-dt-saida + 3), c-semana,",") 
          fi-cab-05:SCREEN-VALUE IN FRAME {&FRAME-NAME} = fn-dia-mes(INPUT da-dt-saida + 4) + ENTRY(WEEKDAY(da-dt-saida + 4), c-semana,",").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-nome-transp-fin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nome-transp-fin w-digita
ON LEAVE OF fi-nome-transp-fin IN FRAME F-Main
DO:
    FIND transporte WHERE 
         transporte.nome-abrev = INPUT FRAME {&FRAME-NAME} fi-nome-transp-fin NO-LOCK NO-ERROR.
    IF NOT AVAIL transporte THEN
       FIND transporte WHERE 
            STRING(transporte.cod-transp) = INPUT FRAME {&FRAME-NAME} fi-nome-transp-fin NO-LOCK NO-ERROR.

    IF AVAIL transporte THEN
        ASSIGN fi-nome-transp-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = transporte.nome-abrev.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nome-transp-fin w-digita
ON MOUSE-SELECT-DBLCLICK OF fi-nome-transp-fin IN FRAME F-Main
DO:
  {include/zoomvar.i &prog-zoom = adzoom\z01ad268.w
                     &campo     = fi-nome-transp-fin
                     &campozoom = nome-abrev}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-nome-transp-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nome-transp-ini w-digita
ON LEAVE OF fi-nome-transp-ini IN FRAME F-Main /* Transportadora */
DO:
  IF SELF:SCREEN-VALUE <> "" THEN DO:
     FIND transporte WHERE 
          transporte.nome-abrev = INPUT FRAME {&FRAME-NAME} fi-nome-transp-ini NO-LOCK NO-ERROR.
     IF NOT AVAIL transporte THEN
        FIND transporte WHERE 
             STRING(transporte.cod-transp) = INPUT FRAME {&FRAME-NAME} fi-nome-transp-ini NO-LOCK NO-ERROR.

     IF AVAIL transporte THEN
         ASSIGN fi-nome-transp-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = transporte.nome-abrev
                fi-nome-transp-fin:SCREEN-VALUE IN FRAME {&FRAME-NAME} = transporte.nome-abrev.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nome-transp-ini w-digita
ON MOUSE-SELECT-DBLCLICK OF fi-nome-transp-ini IN FRAME F-Main /* Transportadora */
DO:
  {include/zoomvar.i &prog-zoom = adzoom\z01ad268.w
                     &campo     = fi-nome-transp-ini
                     &campozoom = nome-abrev}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-digita 


/* ***************************  Main Block  *************************** */

fi-nome-transp-ini:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.
fi-nome-transp-fin:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.
fi-cod-estabel:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.


/* Substitui TAB por ENTER */
ON 'RETURN':U ANYWHERE DO:
    APPLY 'TAB' TO SELF.
    RETURN NO-APPLY.
END.

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
  DISPLAY fi-cab-01 fi-dt-saida fi-nome-transp-ini fi-nome-transp-fin 
          fi-cap-diaria01 fi-cap-diaria02 fi-cap-diaria03 fi-cap-diaria04 
          fi-cap-diaria05 fi-cod-estabel fi-cab-02 fi-cab-03 fi-cab-04 fi-cab-05 
      WITH FRAME F-Main IN WINDOW w-digita.
  ENABLE fi-dt-saida fi-nome-transp-ini fi-nome-transp-fin fi-cap-diaria01 
         fi-cap-diaria02 fi-cap-diaria03 fi-cap-diaria04 fi-cap-diaria05 
         bt-ajuda bt-ok bt-cancelar fi-cod-estabel IMAGE-90 IMAGE-91 RECT-1 
         RECT-50 
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
  ASSIGN fi-dt-saida        = da-dt-saida   
         fi-nome-transp-ini = c-nome-transp-ini
         fi-nome-transp-fin = c-nome-transp-fin
         fi-cod-estabel     = c-cod-estabel
         fi-cap-diaria01    = de-cap-diaria01
         fi-cap-diaria02    = de-cap-diaria02
         fi-cap-diaria03    = de-cap-diaria03
         fi-cap-diaria04    = de-cap-diaria04
         fi-cap-diaria05    = de-cap-diaria05.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  APPLY 'entry' TO fi-dt-saida IN FRAME {&FRAME-NAME}.
  RETURN NO-APPLY.

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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-dia-mes w-digita 
FUNCTION fn-dia-mes RETURNS CHARACTER
  (INPUT da-dt-saida AS DATE):
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR c-dia-mes AS CHAR INIT "".

  ASSIGN c-dia-mes = "(" + STRING(DAY(da-dt-saida), "99") +
                     "/" + STRING(MONTH(da-dt-saida), "99") + ")  ".

  RETURN c-dia-mes.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

