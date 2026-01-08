&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          espec            PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/********************************************************************************
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME f-main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES cota-fam
&Scoped-define FIRST-EXTERNAL-TABLE cota-fam


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR cota-fam.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS cota-fam.fam-adic1 cota-fam.fam-adic2 ~
cota-fam.fam-adic3 cota-fam.fam-adic4 cota-fam.fam-adic5 cota-fam.fam-adic6 ~
cota-fam.estoque cota-fam.producao cota-fam.prd-dia cota-fam.dt-min-ped ~
cota-fam.cota1 cota-fam.cota2 cota-fam.susp-cota1 cota-fam.susp-cota2 
&Scoped-define ENABLED-TABLES cota-fam
&Scoped-define FIRST-ENABLED-TABLE cota-fam
&Scoped-Define ENABLED-OBJECTS rt-key rt-mold 
&Scoped-Define DISPLAYED-FIELDS cota-fam.fm-codigo cota-fam.fam-adic1 ~
cota-fam.fam-adic2 cota-fam.fam-adic3 cota-fam.fam-adic4 cota-fam.fam-adic5 ~
cota-fam.fam-adic6 cota-fam.estoque cota-fam.producao cota-fam.prd-dia ~
cota-fam.dt-min-ped cota-fam.cota1 cota-fam.cota2 cota-fam.susp-cota1 ~
cota-fam.susp-cota2 
&Scoped-define DISPLAYED-TABLES cota-fam
&Scoped-define FIRST-DISPLAYED-TABLE cota-fam
&Scoped-Define DISPLAYED-OBJECTS fi-desc-familia fi-desc-familia1 ~
fi-desc-familia2 fi-desc-familia3 fi-desc-familia4 fi-desc-familia5 ~
fi-desc-familia6 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-CREATE-FIELDS cota-fam.fm-codigo 

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
DEFINE VARIABLE fi-desc-familia AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 42.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-familia1 AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 42.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-familia2 AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 42.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-familia3 AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 42.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-familia4 AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 42.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-familia5 AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 42.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-familia6 AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 42.57 BY .88 NO-UNDO.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 88.57 BY 1.25.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 88.57 BY 10.33.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     cota-fam.fm-codigo AT ROW 1.17 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.14 BY .88
     fi-desc-familia AT ROW 1.17 COL 28.29 COLON-ALIGNED NO-LABEL
     cota-fam.fam-adic1 AT ROW 2.67 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.14 BY .88
     fi-desc-familia1 AT ROW 2.67 COL 28.29 COLON-ALIGNED NO-LABEL
     cota-fam.fam-adic2 AT ROW 3.67 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.14 BY .88
     fi-desc-familia2 AT ROW 3.67 COL 28.29 COLON-ALIGNED NO-LABEL
     cota-fam.fam-adic3 AT ROW 4.67 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.14 BY .88
     fi-desc-familia3 AT ROW 4.67 COL 28.29 COLON-ALIGNED NO-LABEL
     cota-fam.fam-adic4 AT ROW 5.67 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.14 BY .88
     fi-desc-familia4 AT ROW 5.67 COL 28.29 COLON-ALIGNED NO-LABEL
     cota-fam.fam-adic5 AT ROW 6.67 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.14 BY .88
     fi-desc-familia5 AT ROW 6.67 COL 28.29 COLON-ALIGNED NO-LABEL
     cota-fam.fam-adic6 AT ROW 7.67 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.14 BY .88
     fi-desc-familia6 AT ROW 7.67 COL 28.29 COLON-ALIGNED NO-LABEL
     cota-fam.estoque AT ROW 8.67 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.43 BY .88
     cota-fam.producao AT ROW 9.67 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.43 BY .88
     cota-fam.prd-dia AT ROW 10.67 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.43 BY .88
     cota-fam.dt-min-ped AT ROW 11.67 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.57 BY .88
     cota-fam.cota1 AT ROW 10.67 COL 45.43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.57 BY .88
     cota-fam.cota2 AT ROW 11.67 COL 45.43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.57 BY .88
     cota-fam.susp-cota1 AT ROW 10.67 COL 62.29
          VIEW-AS TOGGLE-BOX
          SIZE 14.14 BY .88
     cota-fam.susp-cota2 AT ROW 11.67 COL 62.29
          VIEW-AS TOGGLE-BOX
          SIZE 14.14 BY .88
     rt-key AT ROW 1 COL 1
     rt-mold AT ROW 2.42 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: espec.cota-fam
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
         HEIGHT             = 11.88
         WIDTH              = 88.57.
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
   NOT-VISIBLE Size-to-Fit Custom                                       */
ASSIGN 
       FRAME f-main:SCROLLABLE       = FALSE
       FRAME f-main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fi-desc-familia IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-familia1 IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-familia2 IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-familia3 IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-familia4 IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-familia5 IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-familia6 IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cota-fam.fm-codigo IN FRAME f-main
   NO-ENABLE 1                                                          */
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

&Scoped-define SELF-NAME cota-fam.fam-adic1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-fam.fam-adic1 V-table-Win
ON ENTRY OF cota-fam.fam-adic1 IN FRAME f-main /* Familia Adic.1 */
DO:
  IF INPUT FRAME {&FRAME-NAME} cota-fam.fam-adic1 <> "" THEN DO:
     FIND familia WHERE familia.fm-codigo = INPUT FRAME {&FRAME-NAME} cota-fam.fam-adic1
                  NO-LOCK NO-ERROR.
     IF AVAIL familia THEN
        ASSIGN fi-desc-familia1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = familia.descricao.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-fam.fam-adic1 V-table-Win
ON LEAVE OF cota-fam.fam-adic1 IN FRAME f-main /* Familia Adic.1 */
DO:
  IF INPUT FRAME {&FRAME-NAME} cota-fam.fam-adic1 <> "" THEN DO:
     FIND familia WHERE familia.fm-codigo = INPUT FRAME {&FRAME-NAME} cota-fam.fam-adic1
                  NO-LOCK NO-ERROR.
     IF AVAIL familia THEN
        ASSIGN fi-desc-familia1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = familia.descricao.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-fam.fam-adic1 V-table-Win
ON MOUSE-SELECT-DBLCLICK OF cota-fam.fam-adic1 IN FRAME f-main /* Familia Adic.1 */
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z01in122.w
                     &campo     = cota-fam.fam-adic1
                     &campozoom = fm-codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cota-fam.fam-adic2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-fam.fam-adic2 V-table-Win
ON ENTRY OF cota-fam.fam-adic2 IN FRAME f-main /* Familia Adic.2 */
DO:
  IF INPUT FRAME {&FRAME-NAME} cota-fam.fam-adic2 <> "" THEN DO:
     FIND familia WHERE familia.fm-codigo = INPUT FRAME {&FRAME-NAME} cota-fam.fam-adic2
                  NO-LOCK NO-ERROR.
     IF AVAIL familia THEN
        ASSIGN fi-desc-familia2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = familia.descricao.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-fam.fam-adic2 V-table-Win
ON LEAVE OF cota-fam.fam-adic2 IN FRAME f-main /* Familia Adic.2 */
DO:
  IF INPUT FRAME {&FRAME-NAME} cota-fam.fam-adic2 <> "" THEN DO:
     FIND familia WHERE familia.fm-codigo = INPUT FRAME {&FRAME-NAME} cota-fam.fam-adic2
                  NO-LOCK NO-ERROR.
     IF AVAIL familia THEN
        ASSIGN fi-desc-familia2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = familia.descricao.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-fam.fam-adic2 V-table-Win
ON MOUSE-SELECT-DBLCLICK OF cota-fam.fam-adic2 IN FRAME f-main /* Familia Adic.2 */
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z01in122.w
                     &campo     = cota-fam.fam-adic2
                     &campozoom = fm-codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cota-fam.fam-adic3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-fam.fam-adic3 V-table-Win
ON ENTRY OF cota-fam.fam-adic3 IN FRAME f-main /* Familia Adic.3 */
DO:
  IF INPUT FRAME {&FRAME-NAME} cota-fam.fam-adic3 <> "" THEN DO:
     FIND familia WHERE familia.fm-codigo = INPUT FRAME {&FRAME-NAME} cota-fam.fam-adic3
                  NO-LOCK NO-ERROR.
     IF AVAIL familia THEN
        ASSIGN fi-desc-familia3:SCREEN-VALUE IN FRAME {&FRAME-NAME} = familia.descricao.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-fam.fam-adic3 V-table-Win
ON LEAVE OF cota-fam.fam-adic3 IN FRAME f-main /* Familia Adic.3 */
DO:
  IF INPUT FRAME {&FRAME-NAME} cota-fam.fam-adic3 <> "" THEN DO:
     FIND familia WHERE familia.fm-codigo = INPUT FRAME {&FRAME-NAME} cota-fam.fam-adic3
                  NO-LOCK NO-ERROR.
     IF AVAIL familia THEN
        ASSIGN fi-desc-familia3:SCREEN-VALUE IN FRAME {&FRAME-NAME} = familia.descricao.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-fam.fam-adic3 V-table-Win
ON MOUSE-SELECT-DBLCLICK OF cota-fam.fam-adic3 IN FRAME f-main /* Familia Adic.3 */
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z01in122.w
                     &campo     = cota-fam.fam-adic3
                     &campozoom = fm-codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cota-fam.fam-adic4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-fam.fam-adic4 V-table-Win
ON ENTRY OF cota-fam.fam-adic4 IN FRAME f-main /* Familia Adic.4 */
DO:
  IF INPUT FRAME {&FRAME-NAME} cota-fam.fam-adic4 <> "" THEN DO:
     FIND familia WHERE familia.fm-codigo = INPUT FRAME {&FRAME-NAME} cota-fam.fam-adic4
                  NO-LOCK NO-ERROR.
     IF AVAIL familia THEN
        ASSIGN fi-desc-familia4:SCREEN-VALUE IN FRAME {&FRAME-NAME} = familia.descricao.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-fam.fam-adic4 V-table-Win
ON LEAVE OF cota-fam.fam-adic4 IN FRAME f-main /* Familia Adic.4 */
DO:
  IF INPUT FRAME {&FRAME-NAME} cota-fam.fam-adic4 <> "" THEN DO:
     FIND familia WHERE familia.fm-codigo = INPUT FRAME {&FRAME-NAME} cota-fam.fam-adic4
                  NO-LOCK NO-ERROR.
     IF AVAIL familia THEN
        ASSIGN fi-desc-familia4:SCREEN-VALUE IN FRAME {&FRAME-NAME} = familia.descricao.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-fam.fam-adic4 V-table-Win
ON MOUSE-SELECT-DBLCLICK OF cota-fam.fam-adic4 IN FRAME f-main /* Familia Adic.4 */
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z01in122.w
                     &campo     = cota-fam.fam-adic4
                     &campozoom = fm-codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cota-fam.fam-adic5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-fam.fam-adic5 V-table-Win
ON ENTRY OF cota-fam.fam-adic5 IN FRAME f-main /* Familia Adic.5 */
DO:
  IF INPUT FRAME {&FRAME-NAME} cota-fam.fam-adic5 <> "" THEN DO:
     FIND familia WHERE familia.fm-codigo = INPUT FRAME {&FRAME-NAME} cota-fam.fam-adic5
                  NO-LOCK NO-ERROR.
     IF AVAIL familia THEN
        ASSIGN fi-desc-familia5:SCREEN-VALUE IN FRAME {&FRAME-NAME} = familia.descricao.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-fam.fam-adic5 V-table-Win
ON LEAVE OF cota-fam.fam-adic5 IN FRAME f-main /* Familia Adic.5 */
DO:
  IF INPUT FRAME {&FRAME-NAME} cota-fam.fam-adic5 <> "" THEN DO:
     FIND familia WHERE familia.fm-codigo = INPUT FRAME {&FRAME-NAME} cota-fam.fam-adic5
                  NO-LOCK NO-ERROR.
     IF AVAIL familia THEN
        ASSIGN fi-desc-familia5:SCREEN-VALUE IN FRAME {&FRAME-NAME} = familia.descricao.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-fam.fam-adic5 V-table-Win
ON MOUSE-SELECT-DBLCLICK OF cota-fam.fam-adic5 IN FRAME f-main /* Familia Adic.5 */
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z01in122.w
                     &campo     = cota-fam.fam-adic5
                     &campozoom = fm-codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cota-fam.fam-adic6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-fam.fam-adic6 V-table-Win
ON ENTRY OF cota-fam.fam-adic6 IN FRAME f-main /* Familia Adic.6 */
DO:
  IF INPUT FRAME {&FRAME-NAME} cota-fam.fam-adic6 <> "" THEN DO:
     FIND familia WHERE familia.fm-codigo = INPUT FRAME {&FRAME-NAME} cota-fam.fam-adic6
                  NO-LOCK NO-ERROR.
     IF AVAIL familia THEN
        ASSIGN fi-desc-familia6:SCREEN-VALUE IN FRAME {&FRAME-NAME} = familia.descricao.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-fam.fam-adic6 V-table-Win
ON LEAVE OF cota-fam.fam-adic6 IN FRAME f-main /* Familia Adic.6 */
DO:
  IF INPUT FRAME {&FRAME-NAME} cota-fam.fam-adic6 <> "" THEN DO:
     FIND familia WHERE familia.fm-codigo = INPUT FRAME {&FRAME-NAME} cota-fam.fam-adic6
                  NO-LOCK NO-ERROR.
     IF AVAIL familia THEN
        ASSIGN fi-desc-familia6:SCREEN-VALUE IN FRAME {&FRAME-NAME} = familia.descricao.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-fam.fam-adic6 V-table-Win
ON MOUSE-SELECT-DBLCLICK OF cota-fam.fam-adic6 IN FRAME f-main /* Familia Adic.6 */
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z01in122.w
                     &campo     = cota-fam.fam-adic6
                     &campozoom = fm-codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cota-fam.fm-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-fam.fm-codigo V-table-Win
ON ENTRY OF cota-fam.fm-codigo IN FRAME f-main /* Familia */
DO:
  FIND familia WHERE familia.fm-codigo = INPUT FRAME {&FRAME-NAME} cota-fam.fm-codigo
               NO-LOCK NO-ERROR.
  IF AVAIL familia THEN
     ASSIGN fi-desc-familia:SCREEN-VALUE IN FRAME {&FRAME-NAME} = familia.descricao.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-fam.fm-codigo V-table-Win
ON LEAVE OF cota-fam.fm-codigo IN FRAME f-main /* Familia */
DO:
  IF INPUT FRAME {&FRAME-NAME} cota-fam.fm-codigo <> "" THEN DO:
     FIND familia WHERE familia.fm-codigo = INPUT FRAME {&FRAME-NAME} cota-fam.fm-codigo
                  NO-LOCK NO-ERROR.
     IF AVAIL familia THEN
        ASSIGN fi-desc-familia:SCREEN-VALUE IN FRAME {&FRAME-NAME} = familia.descricao.
     FIND cota-fam WHERE cota-fam.fm-codigo = INPUT FRAME {&FRAME-NAME} cota-fam.fm-codigo
                   NO-LOCK NO-ERROR.
     IF AVAIL cota-fam THEN DO:
        MESSAGE "J  existe um registro de SAC para essa Fam¡lia." VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
     END.
  END.
  ELSE DO:
     MESSAGE "Fam¡lia inv lida." VIEW-AS ALERT-BOX.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-fam.fm-codigo V-table-Win
ON MOUSE-SELECT-DBLCLICK OF cota-fam.fm-codigo IN FRAME f-main /* Familia */
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z01in122.w
                     &campo     = cota-fam.fm-codigo
                     &campozoom = fm-codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  
  cota-fam.fm-codigo:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.
  cota-fam.fam-adic1:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.
  cota-fam.fam-adic2:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.
  cota-fam.fam-adic3:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.
  cota-fam.fam-adic4:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.
  cota-fam.fam-adic5:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.
  cota-fam.fam-adic6:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.
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

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "cota-fam"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "cota-fam"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
    {include/i-valid.i}
    
    /* Ponha na pi-validate todas as valida‡äes */
    /* NÆo gravar nada no registro antes do dispatch do assign-record e 
       nem na PI-validate. */
    
    RUN pi-validate.
    IF RETURN-VALUE = 'ADM-ERROR':U THEN
       RETURN 'ADM-ERROR':U.

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.
    
    /* Todos os assignïs nÆo feitos pelo assign-record devem ser feitos aqui */  
    /* Code placed here will execute AFTER standard behavior.    */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    
    /* Code placed here will execute PRIOR to standard behavior. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ).
    
    /* Code placed here will execute AFTER standard behavior.    */

    FIND familia WHERE familia.fm-codigo = cota-fam.fm-codigo NO-LOCK NO-ERROR.
         IF AVAIL familia THEN
            ASSIGN fi-desc-familia:SCREEN-VALUE IN FRAME {&FRAME-NAME} = familia.descricao.
     
    IF AVAIL cota-fam THEN DO:
       IF cota-fam.fam-adic1 <> "" THEN DO:
          FIND familia WHERE familia.fm-codigo = cota-fam.fam-adic1 NO-LOCK NO-ERROR.
          IF AVAIL familia THEN
             ASSIGN fi-desc-familia1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = familia.descricao.
       END.
   
       IF cota-fam.fam-adic2 <> "" THEN DO:
          FIND familia WHERE familia.fm-codigo = cota-fam.fam-adic2 NO-LOCK NO-ERROR.
          IF AVAIL familia THEN
             ASSIGN fi-desc-familia2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = familia.descricao.
       END.
       
       IF cota-fam.fam-adic3 <> "" THEN DO:
          FIND familia WHERE familia.fm-codigo = cota-fam.fam-adic3 NO-LOCK NO-ERROR.
          IF AVAIL familia THEN
             ASSIGN fi-desc-familia3:SCREEN-VALUE IN FRAME {&FRAME-NAME} = familia.descricao.
       END.
   
       IF cota-fam.fam-adic4 <> "" THEN DO:
          FIND familia WHERE familia.fm-codigo = cota-fam.fam-adic4 NO-LOCK NO-ERROR.
          IF AVAIL familia THEN
             ASSIGN fi-desc-familia4:SCREEN-VALUE IN FRAME {&FRAME-NAME} = familia.descricao.
       END.
   
       IF cota-fam.fam-adic5 <> "" THEN DO:
          FIND familia WHERE familia.fm-codigo = cota-fam.fam-adic5 NO-LOCK NO-ERROR.
          IF AVAIL familia THEN
             ASSIGN fi-desc-familia5:SCREEN-VALUE IN FRAME {&FRAME-NAME} = familia.descricao.
       END.
   
       IF cota-fam.fam-adic6 <> "" THEN DO:
          FIND familia WHERE familia.fm-codigo = cota-fam.fam-adic6 NO-LOCK NO-ERROR.
          IF AVAIL familia THEN
             ASSIGN fi-desc-familia6:SCREEN-VALUE IN FRAME {&FRAME-NAME} = familia.descricao.
       END.
    END.   

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-atualiza-parent V-table-Win 
PROCEDURE pi-atualiza-parent :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    define input parameter v-row-parent-externo as rowid no-undo.
    
    assign v-row-parent = v-row-parent-externo.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pi-validate V-table-Win 
PROCEDURE Pi-validate :
/*------------------------------------------------------------------------------
  Purpose:Validar a viewer     
  Parameters:  <none>
  Notes: NÆo fazer assign aqui. Nesta procedure
  devem ser colocadas apenas valida‡äes, pois neste ponto do programa o registro 
  ainda nÆo foi criado.       
------------------------------------------------------------------------------*/
    {include/i-vldfrm.i} /* Valida‡Æo de dicion rio */
    
/*/*    Segue um exemplo de valida‡Æo de programa */
 *     find tabela where tabela.campo1 = c-variavel and
 *                       tabela.campo2 > i-variavel no-lock no-error.
 *     
 *     /* Este include deve ser colocado sempre antes do ut-msgs.p */
 *     {include/i-vldprg.i}
 *     run utp/ut-msgs.p (input "show":U, input 7, input return-value).
 *     return 'ADM-ERROR':U.*/

 IF INPUT FRAME {&FRAME-NAME} cota-fam.fam-adic1 <> "" THEN DO:
    FIND familia WHERE familia.fm-codigo = INPUT FRAME {&FRAME-NAME} cota-fam.fam-adic1                 
                 NO-LOCK NO-ERROR.                                                                             
    IF NOT AVAIL familia THEN DO:                                                                              
       MESSAGE "Fam¡lia inv lida." VIEW-AS ALERT-BOX.                              
       APPLY "entry" TO cota-fam.fam-adic1.                                                                
       return 'ADM-ERROR':U.                                                                                   
    END.                                                                                                       
 END.

 IF INPUT FRAME {&FRAME-NAME} cota-fam.fam-adic2 <> "" THEN DO:
    FIND familia WHERE familia.fm-codigo = INPUT FRAME {&FRAME-NAME} cota-fam.fam-adic2                 
                 NO-LOCK NO-ERROR.                                                                             
    IF NOT AVAIL familia THEN DO:                                                                              
       MESSAGE "Fam¡lia inv lida." VIEW-AS ALERT-BOX.                              
       APPLY "entry" TO cota-fam.fam-adic2.                                                                
       return 'ADM-ERROR':U.                                                                                   
    END.                                                                                                       
 END.

 IF INPUT FRAME {&FRAME-NAME} cota-fam.fam-adic3 <> "" THEN DO:
    FIND familia WHERE familia.fm-codigo = INPUT FRAME {&FRAME-NAME} cota-fam.fam-adic3                 
                 NO-LOCK NO-ERROR.                                                                             
    IF NOT AVAIL familia THEN DO:                                                                              
       MESSAGE "Fam¡lia inv lida." VIEW-AS ALERT-BOX.                              
       APPLY "entry" TO cota-fam.fam-adic3.                                                                
       return 'ADM-ERROR':U.                                                                                   
    END.                                                                                                       
 END.

 IF INPUT FRAME {&FRAME-NAME} cota-fam.fam-adic4 <> "" THEN DO:
    FIND familia WHERE familia.fm-codigo = INPUT FRAME {&FRAME-NAME} cota-fam.fam-adic4                 
                 NO-LOCK NO-ERROR.                                                                             
    IF NOT AVAIL familia THEN DO:                                                                              
       MESSAGE "Fam¡lia inv lida." VIEW-AS ALERT-BOX.                              
       APPLY "entry" TO cota-fam.fam-adic4.                                                                
       return 'ADM-ERROR':U.                                                                                   
    END.                                                                                                       
 END.

 IF INPUT FRAME {&FRAME-NAME} cota-fam.fam-adic5 <> "" THEN DO:
    FIND familia WHERE familia.fm-codigo = INPUT FRAME {&FRAME-NAME} cota-fam.fam-adic5                 
                 NO-LOCK NO-ERROR.                                                                             
    IF NOT AVAIL familia THEN DO:                                                                              
       MESSAGE "Fam¡lia inv lida." VIEW-AS ALERT-BOX.                              
       APPLY "entry" TO cota-fam.fam-adic5.                                                                
       return 'ADM-ERROR':U.                                                                                   
    END.                                                                                                       
 END.

 IF INPUT FRAME {&FRAME-NAME} cota-fam.fam-adic6 <> "" THEN DO:
    FIND familia WHERE familia.fm-codigo = INPUT FRAME {&FRAME-NAME} cota-fam.fam-adic6                 
                 NO-LOCK NO-ERROR.                                                                             
    IF NOT AVAIL familia THEN DO:                                                                              
       MESSAGE "Fam¡lia inv lida." VIEW-AS ALERT-BOX.                              
       APPLY "entry" TO cota-fam.fam-adic6.                                                                
       return 'ADM-ERROR':U.                                                                                   
    END.                                                                                                       
 END.

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

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "cota-fam"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

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

