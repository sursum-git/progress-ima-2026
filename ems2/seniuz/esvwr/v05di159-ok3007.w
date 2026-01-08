&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i V99XX999 9.99.99.999}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
&Scop adm-attribute-dlg support/viewerd.w

/* global variable definitions */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
def var v-row-parent as rowid no-undo.

DEF NEW GLOBAL SHARED VAR h-essp0160 AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-b05di154  AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME f-main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS bt-seleciona fi-dt-limite fi-nr-pedcli-ini ~
fi-nr-pedcli-fin fi-it-codigo-ini fi-it-codigo-fin fi-cod-refer-ini ~
fi-cod-refer-fin fi-nr-seq-ini fi-nr-seq-fin fi-cod-obsoleto-ini ~
fi-cod-obsoleto-fin fi-perc-min fi-perc-max rs-opc-artigo IMAGE-100 ~
IMAGE-101 IMAGE-102 IMAGE-103 IMAGE-106 IMAGE-107 IMAGE-2 IMAGE-51 IMAGE-52 ~
IMAGE-73 IMAGE-77 IMAGE-83 rt-key 
&Scoped-Define DISPLAYED-OBJECTS fi-dt-limite fi-nr-pedcli-ini ~
fi-nr-pedcli-fin fi-it-codigo-ini fi-it-codigo-fin fi-cod-refer-ini ~
fi-cod-refer-fin fi-nr-seq-ini fi-nr-seq-fin fi-cod-obsoleto-ini ~
fi-cod-obsoleto-fin fi-perc-min fi-perc-max rs-opc-artigo 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-CREATE-FIELDS fi-nr-pedcli-ini fi-nr-pedcli-fin ~
fi-it-codigo-ini fi-it-codigo-fin fi-cod-refer-ini fi-cod-refer-fin 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "",
     Keys-Supplied = ""':U).
/**************************
</EXECUTING-CODE> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-abre-2 
     IMAGE-UP FILE "image/im-open.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-open.bmp":U
     LABEL "bt abre 2" 
     SIZE 4 BY 1 TOOLTIP "Exceto".

DEFINE BUTTON bt-abre-3 
     IMAGE-UP FILE "image/im-open.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-open.bmp":U
     LABEL "bt abre 3" 
     SIZE 4 BY 1 TOOLTIP "Exceto".

DEFINE BUTTON bt-abre1 
     IMAGE-UP FILE "image/im-open.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-open.bmp":U
     LABEL "Button 2" 
     SIZE 4 BY 1 TOOLTIP "Exceto".

DEFINE BUTTON bt-seleciona 
     IMAGE-UP FILE "image/imt-res-u.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Processa Separa‡Æo Autom tica" 
     SIZE 46.14 BY 2.13 TOOLTIP "Processa Separa‡Æo Autom tica".

DEFINE VARIABLE fi-cod-obsoleto-fin AS CHARACTER FORMAT "x(1)" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-obsoleto-ini AS CHARACTER FORMAT "x(1)" 
     LABEL "Codigo Obsoleto" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-refer-fin AS CHARACTER FORMAT "X(8)":U 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-refer-ini AS CHARACTER FORMAT "X(8)":U 
     LABEL "Referˆncia" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-limite AS CHARACTER FORMAT "99/9999":U 
     LABEL "Data Limite" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE fi-it-codigo-fin AS CHARACTER FORMAT "X(16)":U 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 NO-UNDO.

DEFINE VARIABLE fi-it-codigo-ini AS CHARACTER FORMAT "X(16)":U 
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nr-pedcli-fin AS CHARACTER FORMAT "X(16)":U 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nr-pedcli-ini AS CHARACTER FORMAT "X(16)":U 
     LABEL "Pedido" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nr-seq-fin AS INTEGER FORMAT ">>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nr-seq-ini AS INTEGER FORMAT ">>>>9" INITIAL 0 
     LABEL "Sequˆncia" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE VARIABLE fi-perc-max AS DECIMAL FORMAT ">9.99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE VARIABLE fi-perc-min AS DECIMAL FORMAT "->9.99" INITIAL 0 
     LABEL "Tolerƒncia" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88
     FGCOLOR 12  NO-UNDO.

DEFINE IMAGE IMAGE-100
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-101
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-102
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-103
     FILENAME "image\im-las":U
     SIZE 3 BY .79.

DEFINE IMAGE IMAGE-106
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-107
     FILENAME "image\im-las":U
     SIZE 3 BY .79.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-51
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-52
     FILENAME "image\im-las":U
     SIZE 3 BY .79.

DEFINE IMAGE IMAGE-73
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-77
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-83
     FILENAME "image\im-las":U
     SIZE 3 BY .79.

DEFINE VARIABLE rs-opc-artigo AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Indigo", "I",
"Outros", "O",
"Ambos", "A"
     SIZE 36.29 BY .79 NO-UNDO.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 109 BY 17.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     bt-seleciona AT ROW 12.75 COL 33.86
     fi-dt-limite AT ROW 2.75 COL 32 COLON-ALIGNED
     bt-abre-3 AT ROW 3.71 COL 80.43
     fi-nr-pedcli-ini AT ROW 3.75 COL 32 COLON-ALIGNED
     fi-nr-pedcli-fin AT ROW 3.75 COL 62 COLON-ALIGNED NO-LABEL
     bt-abre1 AT ROW 4.75 COL 80.57
     fi-it-codigo-ini AT ROW 4.79 COL 32 COLON-ALIGNED
     fi-it-codigo-fin AT ROW 4.79 COL 62 COLON-ALIGNED NO-LABEL
     fi-cod-refer-ini AT ROW 5.79 COL 32 COLON-ALIGNED
     fi-cod-refer-fin AT ROW 5.79 COL 62 COLON-ALIGNED NO-LABEL
     bt-abre-2 AT ROW 5.79 COL 80.57
     fi-nr-seq-ini AT ROW 6.75 COL 32 COLON-ALIGNED HELP
          "Codigo obsoleto"
     fi-nr-seq-fin AT ROW 6.75 COL 62 COLON-ALIGNED HELP
          "Codigo obsoleto" NO-LABEL
     fi-cod-obsoleto-ini AT ROW 7.75 COL 32 COLON-ALIGNED HELP
          "Codigo obsoleto"
     fi-cod-obsoleto-fin AT ROW 7.75 COL 62 COLON-ALIGNED HELP
          "Codigo obsoleto" NO-LABEL
     fi-perc-min AT ROW 8.75 COL 32 COLON-ALIGNED HELP
          "Codigo obsoleto"
     fi-perc-max AT ROW 8.75 COL 62 COLON-ALIGNED NO-LABEL
     rs-opc-artigo AT ROW 10 COL 34 NO-LABEL
     IMAGE-100 AT ROW 3.75 COL 60
     IMAGE-101 AT ROW 3.75 COL 51
     IMAGE-102 AT ROW 6.75 COL 51.14
     IMAGE-103 AT ROW 6.75 COL 60.14
     IMAGE-106 AT ROW 8.75 COL 51
     IMAGE-107 AT ROW 8.75 COL 60
     IMAGE-2 AT ROW 4.79 COL 60
     IMAGE-51 AT ROW 5.79 COL 51
     IMAGE-52 AT ROW 5.79 COL 60
     IMAGE-73 AT ROW 4.79 COL 51
     IMAGE-77 AT ROW 7.75 COL 51.14
     IMAGE-83 AT ROW 7.75 COL 60.14
     rt-key AT ROW 1 COL 1
     "Tipo de Artigo:" VIEW-AS TEXT
          SIZE 10.57 BY 1 AT ROW 9.75 COL 23.57
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 17.08
         WIDTH              = 109.57.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
{include/c-viewer.i}
{utp/ut-glob.i}
{include/i_dbtype.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-main
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME f-main:SCROLLABLE       = FALSE
       FRAME f-main:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON bt-abre-2 IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-abre-3 IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-abre1 IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-cod-refer-fin IN FRAME f-main
   1                                                                    */
/* SETTINGS FOR FILL-IN fi-cod-refer-ini IN FRAME f-main
   1                                                                    */
/* SETTINGS FOR FILL-IN fi-it-codigo-fin IN FRAME f-main
   1                                                                    */
/* SETTINGS FOR FILL-IN fi-it-codigo-ini IN FRAME f-main
   1                                                                    */
/* SETTINGS FOR FILL-IN fi-nr-pedcli-fin IN FRAME f-main
   1                                                                    */
/* SETTINGS FOR FILL-IN fi-nr-pedcli-ini IN FRAME f-main
   1                                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-main
/* Query rebuild information for FRAME f-main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME f-main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME bt-seleciona
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-seleciona V-table-Win
ON CHOOSE OF bt-seleciona IN FRAME f-main /* Processa Separa‡Æo Autom tica */
DO:
   ASSIGN INPUT FRAME {&FRAME-NAME} fi-dt-limite
          INPUT FRAME {&FRAME-NAME} fi-it-codigo-ini    
          INPUT FRAME {&FRAME-NAME} fi-it-codigo-fin    
          INPUT FRAME {&FRAME-NAME} fi-cod-refer-ini    
          INPUT FRAME {&FRAME-NAME} fi-cod-refer-fin    
          INPUT FRAME {&FRAME-NAME} fi-nr-pedcli-ini 
          INPUT FRAME {&FRAME-NAME} fi-nr-pedcli-fin
          INPUT FRAME {&FRAME-NAME} fi-nr-seq-ini
          INPUT FRAME {&FRAME-NAME} fi-nr-seq-fin
          INPUT FRAME {&FRAME-NAME} fi-cod-obsoleto-ini 
          INPUT FRAME {&FRAME-NAME} fi-cod-obsoleto-fin
          INPUT FRAME {&FRAME-NAME} rs-opc-artigo
          INPUT FRAME {&FRAME-NAME} fi-perc-min
          INPUT FRAME {&FRAME-NAME} fi-perc-max.

   RUN pi-select-page IN h-essp0160 (INPUT 2).
   RUN pi-processa IN h-b05di154 (INPUT fi-dt-limite,
                                  INPUT fi-it-codigo-ini,
                                  INPUT fi-it-codigo-fin,
                                  INPUT fi-cod-refer-ini,
                                  INPUT fi-cod-refer-fin,
                                  INPUT fi-nr-pedcli-ini,
                                  INPUT fi-nr-pedcli-fin,
                                  INPUT fi-nr-seq-ini,
                                  INPUT fi-nr-seq-fin,
                                  INPUT fi-cod-obsoleto-ini,
                                  INPUT fi-cod-obsoleto-fin,
                                  INPUT ABS(fi-perc-min),
                                  INPUT fi-perc-max,
                                  INPUT rs-opc-artigo).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-obsoleto-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-obsoleto-ini V-table-Win
ON LEAVE OF fi-cod-obsoleto-ini IN FRAME f-main /* Codigo Obsoleto */
DO:
  IF SELF:SCREEN-VALUE <> '' THEN
     ASSIGN fi-cod-obsoleto-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-refer-fin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-refer-fin V-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-cod-refer-fin IN FRAME f-main
DO:
    {include/zoomvar.i &prog-zoom = inzoom/z02in377.w
                       &campo     = fi-cod-refer-fin
                       &campozoom = cod-refer}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-refer-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-refer-ini V-table-Win
ON LEAVE OF fi-cod-refer-ini IN FRAME f-main /* Referˆncia */
DO:
  IF SELF:SCREEN-VALUE <> '' THEN
     ASSIGN fi-cod-refer-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-refer-ini V-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-cod-refer-ini IN FRAME f-main /* Referˆncia */
DO:
    {include/zoomvar.i &prog-zoom = inzoom/z02in377.w
                       &campo     = fi-cod-refer-ini
                       &campozoom = cod-refer}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-dt-limite
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-dt-limite V-table-Win
ON LEAVE OF fi-dt-limite IN FRAME f-main /* Data Limite */
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


&Scoped-define SELF-NAME fi-it-codigo-fin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo-fin V-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-it-codigo-fin IN FRAME f-main
DO:
    {include/zoomvar.i &prog-zoom = inzoom/z02in172.w
                       &campo     = fi-it-codigo-fin
                       &campozoom = it-codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-it-codigo-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo-ini V-table-Win
ON LEAVE OF fi-it-codigo-ini IN FRAME f-main /* Item */
DO:
  IF SELF:SCREEN-VALUE <> '' AND
     SELF:SCREEN-VALUE <> '5' THEN
     ASSIGN fi-it-codigo-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo-ini V-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-it-codigo-ini IN FRAME f-main /* Item */
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z02in172.w
                     &campo     = fi-it-codigo-ini
                     &campozoom = it-codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-nr-pedcli-fin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nr-pedcli-fin V-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-nr-pedcli-fin IN FRAME f-main
DO:
    {include/zoomvar.i &prog-zoom = dizoom/z01di159.w
                       &campo     = fi-nr-pedcli-fin
                       &campozoom = nr-pedcli}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-nr-pedcli-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nr-pedcli-ini V-table-Win
ON LEAVE OF fi-nr-pedcli-ini IN FRAME f-main /* Pedido */
DO:
   IF SELF:SCREEN-VALUE <> '' THEN
      ASSIGN fi-nr-pedcli-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nr-pedcli-ini V-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-nr-pedcli-ini IN FRAME f-main /* Pedido */
DO:
  {include/zoomvar.i &prog-zoom = dizoom/z01di159.w
                     &campo     = fi-nr-pedcli-ini
                     &campozoom = nr-pedcli}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-nr-seq-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nr-seq-ini V-table-Win
ON LEAVE OF fi-nr-seq-ini IN FRAME f-main /* Sequˆncia */
DO:
  IF SELF:SCREEN-VALUE <> '' THEN
     ASSIGN fi-nr-seq-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-perc-min
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-perc-min V-table-Win
ON LEAVE OF fi-perc-min IN FRAME f-main /* Tolerƒncia */
DO:
  IF SELF:SCREEN-VALUE <> '' THEN
     ASSIGN fi-cod-obsoleto-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-perc-min V-table-Win
ON VALUE-CHANGED OF fi-perc-min IN FRAME f-main /* Tolerƒncia */
DO:
  ASSIGN SELF:FGCOLOR = 0.
  IF SELF:INPUT-VALUE < 0 THEN
     ASSIGN SELF:FGCOLOR = 12.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
  fi-it-codigo-ini:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.
  fi-it-codigo-fin:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.
  fi-cod-refer-ini:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.
  fi-cod-refer-fin:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.
  fi-nr-pedcli-ini:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.
  fi-nr-pedcli-fin:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win  _DEFAULT-DISABLE
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
  HIDE FRAME f-main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-apply-entry V-table-Win 
PROCEDURE local-apply-entry :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'apply-entry':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  APPLY 'entry' TO fi-dt-limite IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-disable-fields V-table-Win 
PROCEDURE local-disable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    
    /* Code placed here will execute PRIOR to standard behavior. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'disable-fields':U ) .
    
    /* Code placed here will execute AFTER standard behavior.    */
    &if  defined(ADM-MODIFY-FIELDS) &then
    disable {&ADM-MODIFY-FIELDS} with frame {&frame-name}.
    &endif
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields V-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    
    /* Code placed here will execute PRIOR to standard behavior. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .
    
    /* Code placed here will execute AFTER standard behavior.    */
    &if  defined(ADM-MODIFY-FIELDS) &then
    if adm-new-record = yes then
        enable {&ADM-MODIFY-FIELDS} with frame {&frame-name}.
    &endif

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize-fields V-table-Win 
PROCEDURE local-initialize-fields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN fi-dt-limite:SCREEN-VALUE IN FRAME {&FRAME-NAME}        = STRING(MONTH(TODAY),'99') + STRING(YEAR(TODAY),'9999')
         fi-it-codigo-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = '5'
         fi-it-codigo-fin:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = '5ZZZZZZZZZZZZZZZ'
         fi-cod-refer-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = ''
         fi-cod-refer-fin:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = 'ZZZZZZZZZZ'
         fi-nr-pedcli-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = ''
         fi-nr-pedcli-fin:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = 'ZZZZZZZZZZ'
         fi-nr-seq-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME}       = '0'
         fi-nr-seq-fin:SCREEN-VALUE IN FRAME {&FRAME-NAME}       = '9999'    
         fi-cod-obsoleto-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '0'
         fi-cod-obsoleto-fin:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '4'
         fi-perc-min:SCREEN-VALUE IN FRAME {&FRAME-NAME}         = '-5'
         fi-perc-max:SCREEN-VALUE IN FRAME {&FRAME-NAME}         = '10'
         rs-opc-artigo:SCREEN-VALUE IN FRAME {&FRAME-NAME}       = 'A'.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartViewer, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed V-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/vstates.i}
  END CASE.
  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

