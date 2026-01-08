&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          espec            PROGRESS
*/
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

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.
DEFINE SHARED VARIABLE base AS CHARACTER.
DEFINE VARIABLE var-error   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE var-message AS LOGICAL     NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES espec.malote-mov
&Scoped-define FIRST-EXTERNAL-TABLE espec.malote-mov


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR espec.malote-mov.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS espec.malote-mov.cod-malote-mov ~
espec.malote-mov.data-mov espec.malote-mov.hora-mov ~
espec.malote-mov.minuto-mov espec.malote-mov.usuario ~
espec.malote-mov.cod-estabel-aux espec.malote-mov.num-malote ~
espec.malote-mov.tipo-mov ~
espec.malote-mov.num-lacre[1] espec.malote-mov.num-lacre[2] ~
espec.malote-mov.num-lacre[3] espec.malote-mov.num-lacre[4] 

&Scoped-define ENABLED-TABLES espec.malote-mov
&Scoped-define FIRST-ENABLED-TABLE espec.malote-mov
&Scoped-Define ENABLED-OBJECTS rs-leitora rt-key rt-mold RECT-3 RECT-4 ~
RECT-7 
&Scoped-Define DISPLAYED-FIELDS espec.malote-mov.cod-malote-mov ~
espec.malote-mov.data-mov espec.malote-mov.hora-mov ~
espec.malote-mov.minuto-mov espec.malote-mov.usuario ~
espec.malote-mov.cod-estabel-aux espec.malote-mov.num-malote ~
espec.malote-mov.tipo-mov ~
espec.malote-mov.num-lacre[1] espec.malote-mov.num-lacre[2] ~
espec.malote-mov.num-lacre[3] espec.malote-mov.num-lacre[4]

&Scoped-define DISPLAYED-TABLES espec.malote-mov
&Scoped-define FIRST-DISPLAYED-TABLE espec.malote-mov
&Scoped-Define DISPLAYED-OBJECTS rs-leitora fi-desc-estabel-aux ~
fi-desc-malote fi-cod-barras 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-CREATE-FIELDS rs-leitora fi-cod-barras 
&Scoped-define ADM-ASSIGN-FIELDS rs-leitora fi-cod-barras 
&Scoped-define ADM-MODIFY-FIELDS rs-leitora fi-cod-barras 
&Scoped-define List-5 espec.malote-mov.cod-malote-mov ~
espec.malote-mov.usuario espec.malote-mov.data-mov ~
espec.malote-mov.hora-mov espec.malote-mov.minuto-mov 
&Scoped-define List-6 espec.malote-mov.num-lacre[1] ~
espec.malote-mov.num-lacre[2] espec.malote-mov.num-lacre[3] ~
espec.malote-mov.num-lacre[4] 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
num-malote|y|y|espec.malote-mov.num-malote
cod-malote-mov|y|y|espec.malote-mov.cod-malote-mov
cod-estabel-aux||y|espec.malote-mov.cod-estabel-aux
Usuario||y|espec.malote-mov.Usuario
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "num-malote,cod-malote-mov",
     Keys-Supplied = "num-malote,cod-malote-mov,cod-estabel-aux,Usuario"':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE fi-cod-barras AS CHARACTER FORMAT "9(36)":U INITIAL "0" 
     VIEW-AS FILL-IN 
     SIZE 47 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-estabel-aux AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 41 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-malote AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 41 BY .88 NO-UNDO.

DEFINE VARIABLE rs-leitora AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Num. Malote", 1,
"Cod. Barras", 2
     SIZE 11.43 BY 2.5 NO-UNDO.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 72 BY 1.75.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 72 BY 2.67.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 72 BY 3.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.57 BY 1.63.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.57 BY 10.71.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     rs-leitora AT ROW 5.04 COL 13.86 NO-LABEL WIDGET-ID 38
     espec.malote-mov.cod-malote-mov AT ROW 1.5 COL 16 COLON-ALIGNED WIDGET-ID 20
          VIEW-AS FILL-IN 
          SIZE 6.14 BY .88
     espec.malote-mov.usuario AT ROW 1.5 COL 72 COLON-ALIGNED WIDGET-ID 24
          VIEW-AS FILL-IN 
          SIZE 14.57 BY .88
     espec.malote-mov.cod-estabel-aux AT ROW 3.25 COL 16 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 5.72 BY .88
     fi-desc-estabel-aux AT ROW 3.25 COL 21.86 COLON-ALIGNED NO-LABEL WIDGET-ID 34
     espec.malote-mov.num-malote AT ROW 5.21 COL 23.29 COLON-ALIGNED NO-LABEL WIDGET-ID 12
          VIEW-AS FILL-IN 
          SIZE 5.72 BY .88
     fi-desc-malote AT ROW 5.21 COL 29.14 COLON-ALIGNED NO-LABEL WIDGET-ID 36 NO-TAB-STOP 
     espec.malote-mov.tipo-mov AT ROW 8.88 COL 25 NO-LABEL WIDGET-ID 14
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Entrada":U, yes,
"Saida":U, no
          SIZE 50 BY .54
     espec.malote-mov.num-lacre[1] AT ROW 10.71 COL 30 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 8 BY .88
     espec.malote-mov.num-lacre[2] AT ROW 11.88 COL 30 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 8 BY .88
     espec.malote-mov.num-lacre[3] AT ROW 10.71 COL 63 COLON-ALIGNED WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 8 BY .88
     espec.malote-mov.num-lacre[4] AT ROW 11.88 COL 63 COLON-ALIGNED WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 8 BY .88
     fi-cod-barras AT ROW 6.5 COL 23.29 COLON-ALIGNED NO-LABEL WIDGET-ID 42
     espec.malote-mov.data-mov AT ROW 1.5 COL 33 COLON-ALIGNED WIDGET-ID 50
          VIEW-AS FILL-IN 
          SIZE 8.29 BY .88
     espec.malote-mov.hora-mov AT ROW 1.5 COL 46.72 COLON-ALIGNED WIDGET-ID 52
          VIEW-AS FILL-IN 
          SIZE 2.57 BY .88
     espec.malote-mov.minuto-mov AT ROW 1.5 COL 50.29 COLON-ALIGNED NO-LABEL WIDGET-ID 54
          VIEW-AS FILL-IN 
          SIZE 2.57 BY .88
     " Lacres:" VIEW-AS TEXT
          SIZE 6 BY .54 AT ROW 10.08 COL 12 WIDGET-ID 32
     "Tipo:" VIEW-AS TEXT
          SIZE 4 BY .54 AT ROW 8 COL 12.29 WIDGET-ID 26
     ":" VIEW-AS TEXT
          SIZE .57 BY .54 AT ROW 1.58 COL 51.57 WIDGET-ID 56
     rt-key AT ROW 1.13 COL 1.29
     rt-mold AT ROW 2.79 COL 1.29
     RECT-3 AT ROW 8.25 COL 9 WIDGET-ID 28
     RECT-4 AT ROW 10.38 COL 9 WIDGET-ID 30
     RECT-7 AT ROW 4.75 COL 9 WIDGET-ID 48
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 1 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: espec.malote-mov
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
         HEIGHT             = 12.67
         WIDTH              = 89.
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit Custom                            */
ASSIGN 
       FRAME f-main:SCROLLABLE       = FALSE
       FRAME f-main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN espec.malote-mov.cod-malote-mov IN FRAME f-main
   5                                                                    */
/* SETTINGS FOR FILL-IN espec.malote-mov.data-mov IN FRAME f-main
   5                                                                    */
/* SETTINGS FOR FILL-IN fi-cod-barras IN FRAME f-main
   NO-ENABLE 1 2 3                                                      */
/* SETTINGS FOR FILL-IN fi-desc-estabel-aux IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-malote IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN espec.malote-mov.hora-mov IN FRAME f-main
   5                                                                    */
/* SETTINGS FOR FILL-IN espec.malote-mov.minuto-mov IN FRAME f-main
   5                                                                    */
/* SETTINGS FOR FILL-IN espec.malote-mov.num-lacre[1] IN FRAME f-main
   6                                                                    */
/* SETTINGS FOR FILL-IN espec.malote-mov.num-lacre[2] IN FRAME f-main
   6                                                                    */
/* SETTINGS FOR FILL-IN espec.malote-mov.num-lacre[3] IN FRAME f-main
   6                                                                    */
/* SETTINGS FOR FILL-IN espec.malote-mov.num-lacre[4] IN FRAME f-main
   6                                                                    */
/* SETTINGS FOR RADIO-SET rs-leitora IN FRAME f-main
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN espec.malote-mov.usuario IN FRAME f-main
   5                                                                    */
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

&Scoped-define SELF-NAME espec.malote-mov.cod-estabel-aux
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL espec.malote-mov.cod-estabel-aux V-table-Win
ON LEAVE OF espec.malote-mov.cod-estabel-aux IN FRAME f-main /* Cod. Estab */
DO:
  FIND FIRST estabel-aux WHERE estabel-aux.cod-estabel-aux = INPUT FRAME {&FRAME-NAME} malote-mov.cod-estabel-aux /*:SCREEN-VALUE IN FRAME {&FRAME-NAME}*/ NO-LOCK NO-ERROR.
  ASSIGN fi-desc-estabel-aux:SCREEN-VALUE IN FRAME {&FRAME-NAME} = IF AVAIL estabel-aux THEN estabel-aux.nome ELSE "Codigo inv†lido".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL espec.malote-mov.cod-estabel-aux V-table-Win
ON MOUSE-SELECT-DBLCLICK OF espec.malote-mov.cod-estabel-aux IN FRAME f-main /* Cod. Estab */
DO:
 {include/zoomvar.i &prog-zoom=eszoom\z01estabel-aux.w
                     &campo=malote-mov.cod-estabel-aux
                     &campozoom=cod-estabel-aux}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-barras
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-barras V-table-Win
ON LEAVE OF fi-cod-barras IN FRAME f-main
DO:
  IF INPUT FRAME {&frame-name} fi-cod-barras <> "" AND INPUT FRAME {&frame-name} fi-cod-barras <> "0" THEN DO:
      ASSIGN malote-mov.num-malote:SCREEN-VALUE IN FRAME {&FRAME-NAME} = SUBSTRING(SELF:SCREEN-VALUE,31,5).
             malote-mov.tipo-mov  :SCREEN-VALUE IN FRAME {&FRAME-NAME} = IF SUBSTRING(SELF:screen-value,30,1) = "8" THEN "YES" ELSE "NO".
      RUN pi-leave-malote.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-barras V-table-Win
ON VALUE-CHANGED OF fi-cod-barras IN FRAME f-main
DO:
  IF LENGTH(SELF:SCREEN-VALUE) = 35 THEN
     APPLY "leave" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME espec.malote-mov.num-malote
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL espec.malote-mov.num-malote V-table-Win
ON ENTRY OF espec.malote-mov.num-malote IN FRAME f-main /* Num Malote */
DO:
  /*ASSIGN fi-desc-malote:SCREEN-VALUE IN FRAME {&FRAME-NAME} = malote-mov.num-malote:SCREEN-VALUE IN FRAME {&FRAME-NAME}. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL espec.malote-mov.num-malote V-table-Win
ON LEAVE OF espec.malote-mov.num-malote IN FRAME f-main /* Num Malote */
DO:
  /*IF fi-desc-malote:SCREEN-VALUE IN FRAME {&FRAME-NAME} <> malote-mov.num-malote:SCREEN-VALUE IN FRAME {&FRAME-NAME} THEN
  ASSIGN malote-mov.num-lacre[1]:screen-value in frame {&frame-name} = "0"
         malote-mov.num-lacre[2]:screen-value in frame {&frame-name} = "0"
         malote-mov.num-lacre[3]:screen-value in frame {&frame-name} = "0"
         malote-mov.num-lacre[4]:screen-value in frame {&frame-name} = "0".

  ASSIGN fi-desc-malote:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
  */
  
  IF INPUT FRAME {&FRAME-NAME} malote-mov.num-malote <> 0 THEN DO:
     FIND LAST malote-mov WHERE malote-mov.num-malote = INPUT FRAME {&FRAME-NAME} malote-mov.num-malote NO-LOCK NO-ERROR.
     IF AVAIL malote-mov THEN
        ASSIGN malote-mov.tipo-mov:SCREEN-VALUE IN FRAME {&FRAME-NAME} = IF malote-mov.tipo-mov THEN "NO" ELSE "YES".
     RUN pi-leave-malote.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-leitora
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-leitora V-table-Win
ON VALUE-CHANGED OF rs-leitora IN FRAME f-main
DO:
  IF SELF:SCREEN-VALUE = "1" THEN DO:
     ASSIGN malote-mov.num-malote:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            malote-mov.tipo-mov:SENSITIVE IN FRAME {&FRAME-NAME}   = YES
            fi-cod-barras:SENSITIVE IN FRAME {&FRAME-NAME}         = NO.
            
     APPLY "entry" TO malote-mov.num-malote IN FRAME {&FRAME-NAME}.
  END.
  ELSE DO:
     ASSIGN malote-mov.num-malote:SENSITIVE IN FRAME {&FRAME-NAME} = NO
            malote-mov.tipo-mov:SENSITIVE IN FRAME {&FRAME-NAME}   = NO
            fi-cod-barras:SENSITIVE IN FRAME {&FRAME-NAME}         = YES.
     APPLY "entry" TO fi-cod-barras IN FRAME {&FRAME-NAME}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME espec.malote-mov.tipo-mov
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL espec.malote-mov.tipo-mov V-table-Win
ON VALUE-CHANGED OF espec.malote-mov.tipo-mov IN FRAME f-main /* Tp Mov */
DO:
  ENABLE {&list-6} WITH FRAME {&FRAME-NAME}. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  
  malote-mov.cod-estabel-aux:LOAD-MOUSE-POINTER("image/lupa.cur"). 

  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-find-using-key V-table-Win  adm/support/_key-fnd.p
PROCEDURE adm-find-using-key :
/*------------------------------------------------------------------------------
  Purpose:     Finds the current record using the contents of
               the 'Key-Name' and 'Key-Value' attributes.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEF VAR key-value AS CHAR NO-UNDO.
  DEF VAR row-avail-enabled AS LOGICAL NO-UNDO.

  /* LOCK status on the find depends on FIELDS-ENABLED. */
  RUN get-attribute ('FIELDS-ENABLED':U).
  row-avail-enabled = (RETURN-VALUE eq 'yes':U).
  /* Look up the current key-value. */
  RUN get-attribute ('Key-Value':U).
  key-value = RETURN-VALUE.

  /* Find the current record using the current Key-Name. */
  RUN get-attribute ('Key-Name':U).
  CASE RETURN-VALUE:
    WHEN 'num-malote':U THEN
       {src/adm/template/find-tbl.i
           &TABLE = espec.malote-mov
           &WHERE = "WHERE espec.malote-mov.num-malote eq INTEGER(key-value)"
       }
    WHEN 'cod-malote-mov':U THEN
       {src/adm/template/find-tbl.i
           &TABLE = espec.malote-mov
           &WHERE = "WHERE espec.malote-mov.cod-malote-mov eq INTEGER(key-value)"
       }
  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  {src/adm/template/row-list.i "espec.malote-mov"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "espec.malote-mov"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record V-table-Win 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  /*FIND LAST malote-mov USE-INDEX cod-malote-mov NO-LOCK NO-ERROR.*/
 
  ASSIGN /*malote-mov.cod-malote-mov:SCREEN-VALUE IN FRAME {&FRAME-NAME} = IF AVAIL malote-mov THEN string(malote-mov.cod-malote-mov + 1) ELSE "1"*/
         malote-mov.cod-estabel-aux:SCREEN-VALUE IN FRAME {&FRAME-NAME} = IF base MATCHES "*ima*" THEN "1" ELSE "5"
         /*malote-mov.data-mov:SCREEN-VALUE IN FRAME {&FRAME-NAME}        = string(TODAY)
         malote-mov.hora-mov:SCREEN-VALUE IN FRAME {&FRAME-NAME}        = entry(1,string(TIME,"hh:mm"),":")
         malote-mov.minuto-mov:SCREEN-VALUE IN FRAME {&FRAME-NAME}      = entry(2,string(TIME,"hh:mm"),":")*/
         malote-mov.usuario:SCREEN-VALUE IN FRAME {&FRAME-NAME}         = UPPER(c-seg-usuario)
         rs-leitora:SCREEN-VALUE IN FRAME {&FRAME-NAME}                 = "2".
  
  FIND LAST malote-mov USE-INDEX cod-malote-mov NO-LOCK NO-ERROR.
  ASSIGN malote-mov.cod-malote-mov:SCREEN-VALUE IN FRAME {&frame-name} = IF AVAIL malote-mov THEN string(malote-mov.cod-malote-mov + 1) ELSE "1".

    
  APPLY "leave" TO malote-mov.cod-estabel-aux IN FRAME {&FRAME-NAME}.
  APPLY "value-changed" TO rs-leitora IN FRAME {&FRAME-NAME}.
  

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
    
    /*:T Ponha na pi-validate todas as validaá‰es */
    RUN pi-validate.
    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.
    /*:T N∆o gravar nada no registro antes do dispatch do assign-record e 
       nem na PI-validate. */
    /*FIND LAST malote-mov USE-INDEX cod-malote-mov NO-LOCK NO-ERROR.
    IF AVAIL malote-mov THEN
       ASSIGN malote-mov.cod-malote-mov:SCREEN-VALUE IN FRAME {&frame-name} = string(malote-mov.cod-malote-mov + 1).
     
    ASSIGN INPUT FRAME {&frame-name} malote-mov.cod-malote-mov.
    */
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.
    
    /*:T Todos os assignÔs n∆o feitos pelo assign-record devem ser feitos aqui */  
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
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':u ).

APPLY "leave" TO malote-mov.cod-estabel-aux in FRAME f-main.
RUN pi-desc-malote.

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

    DISABLE {&list-5} {&list-6} WITH FRAME {&FRAME-NAME}.
    

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-desc-malote V-table-Win 
PROCEDURE pi-desc-malote :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FIND FIRST malote WHERE malote.num-malote = INPUT FRAME {&FRAME-NAME} malote-mov.num-malote NO-LOCK NO-ERROR.
IF AVAIL malote THEN
   ASSIGN fi-desc-malote:SCREEN-VALUE IN FRAME {&FRAME-NAME} = malote.descricao.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-leave-malote V-table-Win 
PROCEDURE pi-leave-malote :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN pi-valida-mov.
IF RETURN-VALUE = 'ADM-ERROR':U then DO:
   APPLY "entry" TO fi-cod-barras IN FRAME {&FRAME-NAME}.
   return NO-APPLY.
END.
      
ENABLE {&list-6} WITH FRAME {&FRAME-NAME}. 
APPLY "entry" TO malote-mov.num-lacre[1] IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-valida-mov V-table-Win 
PROCEDURE pi-valida-mov :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN pi-desc-malote. /* FIND malote */
  /* #Malote */
  IF AVAIL malote THEN DO:
     
     IF INPUT FRAME {&FRAME-NAME} malote-mov.cod-estabel-aux <> malote.cod-estabel-orig AND
        INPUT FRAME {&FRAME-NAME} malote-mov.cod-estabel-aux <> malote.cod-estabel-dest THEN DO:
        ASSIGN var-error = var-error + "- Estabelecimento n∆o cadastrado para este malote" + CHR(10).
     END.

     /* Procura ultima movimentaá∆o do malote*/
     FIND LAST malote-mov WHERE malote-mov.num-malote = INPUT FRAME {&FRAME-NAME} malote-mov.num-malote USE-INDEX cod-malote-mov NO-LOCK NO-ERROR.  
     
     /* #malote-mov */
     IF AVAIL malote-mov THEN DO:
        
        /* #mov-completo ----- Se o malote for de entra e saida nos dois estabelecimento */ 
        IF malote.mov-completo THEN DO:
           DISABLE malote-mov.tipo-mov WITH FRAME {&FRAME-NAME}.

           /* Troca o tipo de movimento ENTRADA p/ SAIDA ou SAIDA p/ ENTRADRA */
           ASSIGN malote-mov.tipo-mov:SCREEN-VALUE IN FRAME {&FRAME-NAME} = IF malote-mov.tipo-mov THEN "NO" ELSE "YES".
           
           /*# Troca o ESTABELECIMENTO */
           /* Se o ultimo registro for de SAIDA o proximo tem que ser uma ENTRADA em OUTRO ESTABELECIMENTO*/
           IF malote-mov.tipo-mov = NO THEN DO: 
              IF malote-mov.cod-estabel-aux = INPUT FRAME {&FRAME-NAME} malote-mov.cod-estabel-aux THEN DO:
                 MESSAGE "N∆o Ç possivel registrar uma ENTRADA no estabel. " INPUT FRAME {&FRAME-NAME} malote-mov.cod-estabel-aux SKIP
                         "O ultimo movimento foi de SAIDA no mesmo estabel. " /*malote-mov.cod-estabel-aux*/ SKIP
                         "A ENTRADA deve ser em estabelecimento diferente da SAIDA"
                     VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                 ASSIGN malote-mov.cod-estabel-aux:screen-value in frame {&frame-name} = IF malote-mov.cod-estabel-aux = malote.cod-estabel-orig THEN STRING(malote.cod-estabel-dest) ELSE STRING(malote.cod-estabel-orig).
                 RETURN 'ADM-ERROR'.
              END.
           END.
           ELSE DO:
              /* Se o ultimo registro for de ENTRADA o proximo tem que ser uma saida no MESMO ESTABELECIMENTO*/
              IF malote-mov.cod-estabel-aux <> INPUT FRAME {&FRAME-NAME} malote-mov.cod-estabel-aux THEN DO:
                 MESSAGE "N∆o Ç possivel registrar uma SAIDA no estabel. " INPUT FRAME {&FRAME-NAME} malote-mov.cod-estabel-aux SKIP
                         "O ultimo movimento foi de ENTRADA no estabel. " malote-mov.cod-estabel-aux SKIP 
                         "A SAIDA deve ser no mesmo estabelecimento de ENTRADA"
                     VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                 ASSIGN malote-mov.cod-estabel-aux:screen-value in frame {&frame-name} = IF malote-mov.cod-estabel-aux <> malote.cod-estabel-orig THEN STRING(malote.cod-estabel-dest) ELSE STRING(malote.cod-estabel-orig).
                 RETURN 'ADM-ERROR'.
              END.
           END.
           /* #FIM Troca o ESTABELECIMENTO */

           /* Busca os LACRE se for uma ENTRADA */
           IF INPUT FRAME {&FRAME-NAME} malote-mov.tipo-mov THEN
              ASSIGN malote-mov.num-lacre[1]:screen-value in frame {&frame-name} = string(malote-mov.num-lacre[1])
                     malote-mov.num-lacre[2]:screen-value in frame {&frame-name} = string(malote-mov.num-lacre[2])
                     malote-mov.num-lacre[3]:screen-value in frame {&frame-name} = string(malote-mov.num-lacre[3])
                     malote-mov.num-lacre[4]:screen-value in frame {&frame-name} = string(malote-mov.num-lacre[4]).

        END. /* #FIM mov-completo*/
        
        IF malote-mov.cod-estabel-aux = INPUT FRAME {&FRAME-NAME} malote-mov.cod-estabel-aux and
           /*malote-mov.dt-mov        = INPUT FRAME {&FRAME-NAME} malote-mov.dt-mov          and*/
           malote-mov.tipo-mov        = INPUT FRAME {&FRAME-NAME} malote-mov.tipo-mov        THEN DO: 

           MESSAGE "N∆o Ç possivel criar uma " IF malote-mov.tipo-mov THEN "entrada" ELSE "saida"
                   " para o malote " INPUT FRAME {&FRAME-NAME} malote-mov.num-malote SKIP
                   "Crie uma " IF NOT malote-mov.tipo-mov THEN "entrada" ELSE "saida"
                   " antes de registrar outra " IF malote-mov.tipo-mov THEN "entrada." ELSE "saida."
               VIEW-AS ALERT-BOX ERROR BUTTONS OK.                           
           /*APPLY "entry" TO malote-mov.num-malote IN FRAME {&FRAME-NAME}.*/
           return 'ADM-ERROR'.
        END.

     END. /* #FIM malote-mov */
  END. /* #FIM Malote */
  ELSE DO:
       MESSAGE "N∆o foi encontrado o malote " + INPUT FRAME {&FRAME-NAME} malote-mov.num-malote SKIP
               "Deseja cadastra o malote?"
               VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE var-message.
       
       IF var-message = YES THEN
          RUN esp/malote.w.

       return 'ADM-ERROR'.
  END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pi-validate V-table-Win 
PROCEDURE Pi-validate :
/*:T------------------------------------------------------------------------------
  Purpose:Validar a viewer     
  Parameters:  <none>
  Notes: N∆o fazer assign aqui. Nesta procedure
  devem ser colocadas apenas validaá‰es, pois neste ponto do programa o registro 
  ainda n∆o foi criado.       
------------------------------------------------------------------------------*/
    {include/i-vldfrm.i} /*:T Validaá∆o de dicion†rio */
    
/*:T    Segue um exemplo de validaá∆o de programa */
/*       find tabela where tabela.campo1 = c-variavel and               */
/*                         tabela.campo2 > i-variavel no-lock no-error. */
      
      /*:T Este include deve ser colocado sempre antes do ut-msgs.p */
/*       {include/i-vldprg.i}                                             */
/*       run utp/ut-msgs.p (input "show":U, input 7, input return-value). */
/*       return 'ADM-ERROR':U.                                            */
  
  
  IF INPUT FRAME {&FRAME-NAME} malote-mov.num-lacre[1] = 0 and
     INPUT FRAME {&FRAME-NAME} malote-mov.num-lacre[2] = 0 and
     INPUT FRAME {&FRAME-NAME} malote-mov.num-lacre[3] = 0 and
     INPUT FRAME {&FRAME-NAME} malote-mov.num-lacre[4] = 0 THEN DO:
     MESSAGE "Nenhum lacre foi informado."
         VIEW-AS ALERT-BOX ERROR BUTTONS OK.
     return 'ADM-ERROR'.
  END.

  RUN pi-valida-mov.

  IF RETURN-VALUE = 'ADM-ERROR':U then DO:
     APPLY "entry" TO fi-cod-barras IN FRAME {&FRAME-NAME}.
     return 'ADM-ERROR':U.
  END.
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key V-table-Win  adm/support/_key-snd.p
PROCEDURE send-key :
/*------------------------------------------------------------------------------
  Purpose:     Sends a requested KEY value back to the calling
               SmartObject.
  Parameters:  <see adm/template/sndkytop.i>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/sndkytop.i}

  /* Return the key value associated with each key case.             */
  {src/adm/template/sndkycas.i "num-malote" "espec.malote-mov" "num-malote"}
  {src/adm/template/sndkycas.i "cod-malote-mov" "espec.malote-mov" "cod-malote-mov"}
  {src/adm/template/sndkycas.i "cod-estabel-aux" "espec.malote-mov" "cod-estabel-aux"}
  {src/adm/template/sndkycas.i "Usuario" "espec.malote-mov" "Usuario"}

  /* Close the CASE statement and end the procedure.                 */
  {src/adm/template/sndkyend.i}

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
  {src/adm/template/snd-list.i "espec.malote-mov"}

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

