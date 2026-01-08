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
&Scoped-define EXTERNAL-TABLES mov-man
&Scoped-define FIRST-EXTERNAL-TABLE mov-man


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR mov-man.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS mov-man.ativ-exec[1] mov-man.func-exec[1] ~
mov-man.hora-iexec[1] mov-man.hora-texec[1] mov-man.min-parado[1] ~
mov-man.ativ-exec[2] mov-man.func-exec[2] mov-man.hora-iexec[2] ~
mov-man.hora-texec[2] mov-man.min-parado[2] mov-man.ativ-exec[3] ~
mov-man.func-exec[3] mov-man.hora-iexec[3] mov-man.hora-texec[3] ~
mov-man.min-parado[3] mov-man.ativ-exec[4] mov-man.func-exec[4] ~
mov-man.hora-iexec[4] mov-man.hora-texec[4] mov-man.min-parado[4] ~
mov-man.ativ-exec[5] mov-man.func-exec[5] mov-man.hora-iexec[5] ~
mov-man.hora-texec[5] mov-man.min-parado[5] mov-man.ativ-exec[6] ~
mov-man.func-exec[6] mov-man.hora-iexec[6] mov-man.hora-texec[6] ~
mov-man.min-parado[6] mov-man.ativ-exec[7] mov-man.func-exec[7] ~
mov-man.hora-iexec[7] mov-man.hora-texec[7] mov-man.min-parado[7] ~
mov-man.ativ-exec[8] mov-man.func-exec[8] mov-man.hora-iexec[8] ~
mov-man.hora-texec[8] mov-man.min-parado[8] mov-man.ativ-exec[9] ~
mov-man.func-exec[9] mov-man.hora-iexec[9] mov-man.hora-texec[9] ~
mov-man.min-parado[9] mov-man.ativ-exec[10] mov-man.func-exec[10] ~
mov-man.hora-iexec[10] mov-man.hora-texec[10] mov-man.min-parado[10] 
&Scoped-define ENABLED-TABLES mov-man
&Scoped-define FIRST-ENABLED-TABLE mov-man
&Scoped-Define ENABLED-OBJECTS rt-mold 
&Scoped-Define DISPLAYED-FIELDS mov-man.ativ-exec[1] mov-man.func-exec[1] ~
mov-man.hora-iexec[1] mov-man.hora-texec[1] mov-man.min-parado[1] ~
mov-man.ativ-exec[2] mov-man.func-exec[2] mov-man.hora-iexec[2] ~
mov-man.hora-texec[2] mov-man.min-parado[2] mov-man.ativ-exec[3] ~
mov-man.func-exec[3] mov-man.hora-iexec[3] mov-man.hora-texec[3] ~
mov-man.min-parado[3] mov-man.ativ-exec[4] mov-man.func-exec[4] ~
mov-man.hora-iexec[4] mov-man.hora-texec[4] mov-man.min-parado[4] ~
mov-man.ativ-exec[5] mov-man.func-exec[5] mov-man.hora-iexec[5] ~
mov-man.hora-texec[5] mov-man.min-parado[5] mov-man.ativ-exec[6] ~
mov-man.func-exec[6] mov-man.hora-iexec[6] mov-man.hora-texec[6] ~
mov-man.min-parado[6] mov-man.ativ-exec[7] mov-man.func-exec[7] ~
mov-man.hora-iexec[7] mov-man.hora-texec[7] mov-man.min-parado[7] ~
mov-man.ativ-exec[8] mov-man.func-exec[8] mov-man.hora-iexec[8] ~
mov-man.hora-texec[8] mov-man.min-parado[8] mov-man.ativ-exec[9] ~
mov-man.func-exec[9] mov-man.hora-iexec[9] mov-man.hora-texec[9] ~
mov-man.min-parado[9] mov-man.ativ-exec[10] mov-man.func-exec[10] ~
mov-man.hora-iexec[10] mov-man.hora-texec[10] mov-man.min-parado[10] 
&Scoped-define DISPLAYED-TABLES mov-man
&Scoped-define FIRST-DISPLAYED-TABLE mov-man


/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */

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
DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 87 BY 10.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     mov-man.ativ-exec[1] AT ROW 1.25 COL 5 COLON-ALIGNED
          LABEL "ATIV"
          VIEW-AS FILL-IN 
          SIZE 3.14 BY .88
     mov-man.func-exec[1] AT ROW 1.25 COL 23.43 COLON-ALIGNED
          LABEL "FUNCIONµRIO"
          VIEW-AS FILL-IN 
          SIZE 8 BY .88
     mov-man.hora-iexec[1] AT ROW 1.25 COL 40 COLON-ALIGNED
          LABEL "IN÷CIO"
          VIEW-AS FILL-IN 
          SIZE 6.86 BY .88
     mov-man.hora-texec[1] AT ROW 1.25 COL 58.29 COLON-ALIGNED
          LABEL "TêRMINO"
          VIEW-AS FILL-IN 
          SIZE 6.86 BY .88
     mov-man.min-parado[1] AT ROW 1.25 COL 79.14 COLON-ALIGNED
          LABEL "MIN.PARADO"
          VIEW-AS FILL-IN 
          SIZE 5.72 BY .88 TOOLTIP "Minutos parados"
     mov-man.ativ-exec[2] AT ROW 2.25 COL 5 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.14 BY .88
     mov-man.func-exec[2] AT ROW 2.25 COL 23.43 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 8 BY .88
     mov-man.hora-iexec[2] AT ROW 2.25 COL 40 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6.86 BY .88
     mov-man.hora-texec[2] AT ROW 2.25 COL 58.29 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6.86 BY .88
     mov-man.min-parado[2] AT ROW 2.25 COL 79.14 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5.72 BY .88
     mov-man.ativ-exec[3] AT ROW 3.21 COL 5 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.14 BY .88
     mov-man.func-exec[3] AT ROW 3.21 COL 23.43 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 8 BY .88
     mov-man.hora-iexec[3] AT ROW 3.21 COL 40 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6.86 BY .88
     mov-man.hora-texec[3] AT ROW 3.21 COL 58.29 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6.86 BY .88
     mov-man.min-parado[3] AT ROW 3.21 COL 79.14 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5.72 BY .88
     mov-man.ativ-exec[4] AT ROW 4.21 COL 5 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.14 BY .88
     mov-man.func-exec[4] AT ROW 4.21 COL 23.43 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 8 BY .88
     mov-man.hora-iexec[4] AT ROW 4.21 COL 40 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6.86 BY .88
     mov-man.hora-texec[4] AT ROW 4.21 COL 58.29 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6.86 BY .88
     mov-man.min-parado[4] AT ROW 4.21 COL 79.14 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5.72 BY .88
     mov-man.ativ-exec[5] AT ROW 5.17 COL 5 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.14 BY .88
     mov-man.func-exec[5] AT ROW 5.17 COL 23.43 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 8 BY .88
     mov-man.hora-iexec[5] AT ROW 5.17 COL 40 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6.86 BY .88
     mov-man.hora-texec[5] AT ROW 5.17 COL 58.29 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6.86 BY .88
     mov-man.min-parado[5] AT ROW 5.17 COL 79.14 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5.72 BY .88
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 1.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME f-main
     mov-man.ativ-exec[6] AT ROW 6.17 COL 5 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.14 BY .88
     mov-man.func-exec[6] AT ROW 6.17 COL 23.43 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 8 BY .88
     mov-man.hora-iexec[6] AT ROW 6.17 COL 40 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6.86 BY .88
     mov-man.hora-texec[6] AT ROW 6.17 COL 58.29 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6.86 BY .88
     mov-man.min-parado[6] AT ROW 6.17 COL 79.14 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5.72 BY .88
     mov-man.ativ-exec[7] AT ROW 7.13 COL 5 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.14 BY .88
     mov-man.func-exec[7] AT ROW 7.13 COL 23.43 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 8 BY .88
     mov-man.hora-iexec[7] AT ROW 7.13 COL 40 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6.86 BY .88
     mov-man.hora-texec[7] AT ROW 7.13 COL 58.29 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6.86 BY .88
     mov-man.min-parado[7] AT ROW 7.13 COL 79.14 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5.72 BY .88
     mov-man.ativ-exec[8] AT ROW 8.13 COL 5 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.14 BY .88
     mov-man.func-exec[8] AT ROW 8.13 COL 23.43 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 8 BY .88
     mov-man.hora-iexec[8] AT ROW 8.13 COL 40 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6.86 BY .88
     mov-man.hora-texec[8] AT ROW 8.13 COL 58.29 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6.86 BY .88
     mov-man.min-parado[8] AT ROW 8.13 COL 79.14 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5.72 BY .88
     mov-man.ativ-exec[9] AT ROW 9.08 COL 5 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.14 BY .88
     mov-man.func-exec[9] AT ROW 9.08 COL 23.43 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 8 BY .88
     mov-man.hora-iexec[9] AT ROW 9.08 COL 40 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6.86 BY .88
     mov-man.hora-texec[9] AT ROW 9.08 COL 58.29 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6.86 BY .88
     mov-man.min-parado[9] AT ROW 9.08 COL 79.14 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5.72 BY .88
     mov-man.ativ-exec[10] AT ROW 10.08 COL 5 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.14 BY .88
     mov-man.func-exec[10] AT ROW 10.08 COL 23.43 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 8 BY .88
     mov-man.hora-iexec[10] AT ROW 10.08 COL 40 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6.86 BY .88
     mov-man.hora-texec[10] AT ROW 10.08 COL 58.29 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6.86 BY .88
     mov-man.min-parado[10] AT ROW 10.08 COL 79.14 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5.72 BY .88
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 1.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME f-main
     rt-mold AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: espec.mov-man
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
         HEIGHT             = 10.04
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
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME f-main:SCROLLABLE       = FALSE
       FRAME f-main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN mov-man.ativ-exec[1] IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN mov-man.func-exec[1] IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN mov-man.hora-iexec[1] IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN mov-man.hora-texec[1] IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN mov-man.min-parado[1] IN FRAME f-main
   EXP-LABEL                                                            */
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

&Scoped-define SELF-NAME mov-man.ativ-exec[10]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-man.ativ-exec[10] V-table-Win
ON LEAVE OF mov-man.ativ-exec[10] IN FRAME f-main /* Ativ[10] */
DO:
  IF KEYFUNCTION(LAST-KEY) <> "BACK-TAB" THEN DO:
     ASSIGN mov-man.func-exec[10]:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
     APPLY "ENTRY" TO mov-man.func-exec[10] IN FRAME {&FRAME-NAME}.  
     RETURN NO-APPLY. 
     APPLY "TAB" TO mov-man.func-exec[10] IN FRAME {&FRAME-NAME}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mov-man.ativ-exec[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-man.ativ-exec[1] V-table-Win
ON LEAVE OF mov-man.ativ-exec[1] IN FRAME f-main /* ATIV */
DO:
  IF KEYFUNCTION(LAST-KEY) <> "BACK-TAB" THEN DO:
     ASSIGN mov-man.func-exec[1]:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
     APPLY "ENTRY" TO mov-man.func-exec[1] IN FRAME {&FRAME-NAME}.  
     RETURN NO-APPLY. 
     APPLY "TAB" TO mov-man.func-exec[1] IN FRAME {&FRAME-NAME}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mov-man.ativ-exec[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-man.ativ-exec[2] V-table-Win
ON LEAVE OF mov-man.ativ-exec[2] IN FRAME f-main /* Ativ[2] */
DO:
  IF KEYFUNCTION(LAST-KEY) <> "BACK-TAB" THEN DO:
     ASSIGN mov-man.func-exec[2]:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
     APPLY "ENTRY" TO mov-man.func-exec[2] IN FRAME {&FRAME-NAME}.  
     RETURN NO-APPLY. 
     APPLY "TAB" TO mov-man.func-exec[2] IN FRAME {&FRAME-NAME}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mov-man.ativ-exec[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-man.ativ-exec[3] V-table-Win
ON LEAVE OF mov-man.ativ-exec[3] IN FRAME f-main /* Ativ[3] */
DO:
  IF KEYFUNCTION(LAST-KEY) <> "BACK-TAB" THEN DO:
     ASSIGN mov-man.func-exec[3]:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
     APPLY "ENTRY" TO mov-man.func-exec[3] IN FRAME {&FRAME-NAME}.  
     RETURN NO-APPLY. 
     APPLY "TAB" TO mov-man.func-exec[3] IN FRAME {&FRAME-NAME}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mov-man.ativ-exec[4]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-man.ativ-exec[4] V-table-Win
ON LEAVE OF mov-man.ativ-exec[4] IN FRAME f-main /* Ativ[4] */
DO:
  IF KEYFUNCTION(LAST-KEY) <> "BACK-TAB" THEN DO:
     ASSIGN mov-man.func-exec[4]:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
     APPLY "ENTRY" TO mov-man.func-exec[4] IN FRAME {&FRAME-NAME}.  
     RETURN NO-APPLY. 
     APPLY "TAB" TO mov-man.func-exec[4] IN FRAME {&FRAME-NAME}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mov-man.ativ-exec[5]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-man.ativ-exec[5] V-table-Win
ON LEAVE OF mov-man.ativ-exec[5] IN FRAME f-main /* Ativ[5] */
DO:
  IF KEYFUNCTION(LAST-KEY) <> "BACK-TAB" THEN DO:
     ASSIGN mov-man.func-exec[5]:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
     APPLY "ENTRY" TO mov-man.func-exec[5] IN FRAME {&FRAME-NAME}.  
     RETURN NO-APPLY. 
     APPLY "TAB" TO mov-man.func-exec[5] IN FRAME {&FRAME-NAME}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mov-man.ativ-exec[6]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-man.ativ-exec[6] V-table-Win
ON LEAVE OF mov-man.ativ-exec[6] IN FRAME f-main /* Ativ[6] */
DO:
  IF KEYFUNCTION(LAST-KEY) <> "BACK-TAB" THEN DO:
     ASSIGN mov-man.func-exec[6]:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
     APPLY "ENTRY" TO mov-man.func-exec[6] IN FRAME {&FRAME-NAME}.  
     RETURN NO-APPLY. 
     APPLY "TAB" TO mov-man.func-exec[6] IN FRAME {&FRAME-NAME}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mov-man.ativ-exec[7]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-man.ativ-exec[7] V-table-Win
ON LEAVE OF mov-man.ativ-exec[7] IN FRAME f-main /* Ativ[7] */
DO:
  IF KEYFUNCTION(LAST-KEY) <> "BACK-TAB" THEN DO:
     ASSIGN mov-man.func-exec[7]:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
     APPLY "ENTRY" TO mov-man.func-exec[7] IN FRAME {&FRAME-NAME}.  
     RETURN NO-APPLY. 
     APPLY "TAB" TO mov-man.func-exec[7] IN FRAME {&FRAME-NAME}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mov-man.ativ-exec[8]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-man.ativ-exec[8] V-table-Win
ON LEAVE OF mov-man.ativ-exec[8] IN FRAME f-main /* Ativ[8] */
DO:
  IF KEYFUNCTION(LAST-KEY) <> "BACK-TAB" THEN DO:
     ASSIGN mov-man.func-exec[8]:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
     APPLY "ENTRY" TO mov-man.func-exec[8] IN FRAME {&FRAME-NAME}.  
     RETURN NO-APPLY. 
     APPLY "TAB" TO mov-man.func-exec[8] IN FRAME {&FRAME-NAME}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mov-man.ativ-exec[9]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-man.ativ-exec[9] V-table-Win
ON LEAVE OF mov-man.ativ-exec[9] IN FRAME f-main /* Ativ[9] */
DO:
  IF KEYFUNCTION(LAST-KEY) <> "BACK-TAB" THEN DO:
     ASSIGN mov-man.func-exec[9]:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
     APPLY "ENTRY" TO mov-man.func-exec[9] IN FRAME {&FRAME-NAME}.  
     RETURN NO-APPLY. 
     APPLY "TAB" TO mov-man.func-exec[9] IN FRAME {&FRAME-NAME}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mov-man.func-exec[10]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-man.func-exec[10] V-table-Win
ON LEAVE OF mov-man.func-exec[10] IN FRAME f-main /* Func[10] */
DO:
  IF KEYFUNCTION(LAST-KEY) = "TAB" THEN DO:
     ASSIGN mov-man.hora-iexec[10]:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
     APPLY "ENTRY" TO mov-man.hora-iexec[10] IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mov-man.func-exec[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-man.func-exec[1] V-table-Win
ON LEAVE OF mov-man.func-exec[1] IN FRAME f-main /* FUNCIONµRIO */
DO:
  IF KEYFUNCTION(LAST-KEY) = "TAB" THEN DO:
     ASSIGN mov-man.hora-iexec[1]:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
     APPLY "ENTRY" TO mov-man.hora-iexec[1] IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mov-man.func-exec[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-man.func-exec[2] V-table-Win
ON LEAVE OF mov-man.func-exec[2] IN FRAME f-main /* Func[2] */
DO:
  IF KEYFUNCTION(LAST-KEY) = "TAB" THEN DO:
     ASSIGN mov-man.hora-iexec[2]:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
     APPLY "ENTRY" TO mov-man.hora-iexec[2] IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mov-man.func-exec[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-man.func-exec[3] V-table-Win
ON LEAVE OF mov-man.func-exec[3] IN FRAME f-main /* Func[3] */
DO:
  IF KEYFUNCTION(LAST-KEY) = "TAB" THEN DO:
     ASSIGN mov-man.hora-iexec[3]:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
     APPLY "ENTRY" TO mov-man.hora-iexec[3] IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mov-man.func-exec[4]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-man.func-exec[4] V-table-Win
ON LEAVE OF mov-man.func-exec[4] IN FRAME f-main /* Func[4] */
DO:
  IF KEYFUNCTION(LAST-KEY) = "TAB" THEN DO:
     ASSIGN mov-man.hora-iexec[4]:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
     APPLY "ENTRY" TO mov-man.hora-iexec[4] IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mov-man.func-exec[5]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-man.func-exec[5] V-table-Win
ON LEAVE OF mov-man.func-exec[5] IN FRAME f-main /* Func[5] */
DO:
  IF KEYFUNCTION(LAST-KEY) = "TAB" THEN DO:
     ASSIGN mov-man.hora-iexec[5]:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
     APPLY "ENTRY" TO mov-man.hora-iexec[5] IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mov-man.func-exec[6]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-man.func-exec[6] V-table-Win
ON LEAVE OF mov-man.func-exec[6] IN FRAME f-main /* Func[6] */
DO:
  IF KEYFUNCTION(LAST-KEY) = "TAB" THEN DO:
     ASSIGN mov-man.hora-iexec[6]:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
     APPLY "ENTRY" TO mov-man.hora-iexec[6] IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mov-man.func-exec[7]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-man.func-exec[7] V-table-Win
ON LEAVE OF mov-man.func-exec[7] IN FRAME f-main /* Func[7] */
DO:
  IF KEYFUNCTION(LAST-KEY) = "TAB" THEN DO:
     ASSIGN mov-man.hora-iexec[7]:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
     APPLY "ENTRY" TO mov-man.hora-iexec[7] IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mov-man.func-exec[8]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-man.func-exec[8] V-table-Win
ON LEAVE OF mov-man.func-exec[8] IN FRAME f-main /* Func[8] */
DO:
  IF KEYFUNCTION(LAST-KEY) = "TAB" THEN DO:
     ASSIGN mov-man.hora-iexec[8]:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
     APPLY "ENTRY" TO mov-man.hora-iexec[8] IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mov-man.func-exec[9]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-man.func-exec[9] V-table-Win
ON LEAVE OF mov-man.func-exec[9] IN FRAME f-main /* Func[9] */
DO:
  IF KEYFUNCTION(LAST-KEY) = "TAB" THEN DO:
     ASSIGN mov-man.hora-iexec[9]:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
     APPLY "ENTRY" TO mov-man.hora-iexec[9] IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mov-man.hora-iexec[10]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-man.hora-iexec[10] V-table-Win
ON LEAVE OF mov-man.hora-iexec[10] IN FRAME f-main /* Inic[10] */
DO:
  IF KEYFUNCTION(LAST-KEY) = "TAB" THEN DO:
     ASSIGN mov-man.hora-texec[10]:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
     APPLY "ENTRY" TO mov-man.hora-texec[10] IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mov-man.hora-iexec[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-man.hora-iexec[1] V-table-Win
ON LEAVE OF mov-man.hora-iexec[1] IN FRAME f-main /* IN÷CIO */
DO:
  IF KEYFUNCTION(LAST-KEY) = "TAB" THEN DO:
     ASSIGN mov-man.hora-texec[1]:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
     APPLY "ENTRY" TO mov-man.hora-texec[1] IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mov-man.hora-iexec[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-man.hora-iexec[2] V-table-Win
ON LEAVE OF mov-man.hora-iexec[2] IN FRAME f-main /* Inic[2] */
DO:
  IF KEYFUNCTION(LAST-KEY) = "TAB" THEN DO:
     ASSIGN mov-man.hora-texec[2]:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
     APPLY "ENTRY" TO mov-man.hora-texec[2] IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mov-man.hora-iexec[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-man.hora-iexec[3] V-table-Win
ON LEAVE OF mov-man.hora-iexec[3] IN FRAME f-main /* Inic[3] */
DO:
  IF KEYFUNCTION(LAST-KEY) = "TAB" THEN DO:
     ASSIGN mov-man.hora-texec[3]:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
     APPLY "ENTRY" TO mov-man.hora-texec[3] IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mov-man.hora-iexec[4]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-man.hora-iexec[4] V-table-Win
ON LEAVE OF mov-man.hora-iexec[4] IN FRAME f-main /* Inic[4] */
DO:
  IF KEYFUNCTION(LAST-KEY) = "TAB" THEN DO:
     ASSIGN mov-man.hora-texec[4]:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
     APPLY "ENTRY" TO mov-man.hora-texec[4] IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mov-man.hora-iexec[5]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-man.hora-iexec[5] V-table-Win
ON LEAVE OF mov-man.hora-iexec[5] IN FRAME f-main /* Inic[5] */
DO:
  IF KEYFUNCTION(LAST-KEY) = "TAB" THEN DO:
     ASSIGN mov-man.hora-texec[5]:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
     APPLY "ENTRY" TO mov-man.hora-texec[5] IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mov-man.hora-iexec[6]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-man.hora-iexec[6] V-table-Win
ON LEAVE OF mov-man.hora-iexec[6] IN FRAME f-main /* Inic[6] */
DO:
  IF KEYFUNCTION(LAST-KEY) = "TAB" THEN DO:
     ASSIGN mov-man.hora-texec[6]:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
     APPLY "ENTRY" TO mov-man.hora-texec[6] IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mov-man.hora-iexec[7]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-man.hora-iexec[7] V-table-Win
ON LEAVE OF mov-man.hora-iexec[7] IN FRAME f-main /* Inic[7] */
DO:
  IF KEYFUNCTION(LAST-KEY) = "TAB" THEN DO:
     ASSIGN mov-man.hora-texec[7]:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
     APPLY "ENTRY" TO mov-man.hora-texec[7] IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mov-man.hora-iexec[8]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-man.hora-iexec[8] V-table-Win
ON LEAVE OF mov-man.hora-iexec[8] IN FRAME f-main /* Inic[8] */
DO:
  IF KEYFUNCTION(LAST-KEY) = "TAB" THEN DO:
     ASSIGN mov-man.hora-texec[8]:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
     APPLY "ENTRY" TO mov-man.hora-texec[8] IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mov-man.hora-iexec[9]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-man.hora-iexec[9] V-table-Win
ON LEAVE OF mov-man.hora-iexec[9] IN FRAME f-main /* Inic[9] */
DO:
  IF KEYFUNCTION(LAST-KEY) = "TAB" THEN DO:
     ASSIGN mov-man.hora-texec[9]:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
     APPLY "ENTRY" TO mov-man.hora-texec[9] IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mov-man.hora-texec[10]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-man.hora-texec[10] V-table-Win
ON LEAVE OF mov-man.hora-texec[10] IN FRAME f-main /* Term[10] */
DO:
  IF KEYFUNCTION(LAST-KEY) = "TAB" THEN DO:
     ASSIGN mov-man.min-parado[10]:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
     APPLY "ENTRY" TO mov-man.min-parado[10] IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mov-man.hora-texec[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-man.hora-texec[1] V-table-Win
ON LEAVE OF mov-man.hora-texec[1] IN FRAME f-main /* TêRMINO */
DO:
  IF KEYFUNCTION(LAST-KEY) = "TAB" THEN DO:
     ASSIGN mov-man.min-parado[1]:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
     APPLY "ENTRY" TO mov-man.min-parado[1] IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mov-man.hora-texec[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-man.hora-texec[2] V-table-Win
ON LEAVE OF mov-man.hora-texec[2] IN FRAME f-main /* Term[2] */
DO:
  IF KEYFUNCTION(LAST-KEY) = "TAB" THEN DO:
     ASSIGN mov-man.min-parado[2]:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
     APPLY "ENTRY" TO mov-man.min-parado[2] IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mov-man.hora-texec[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-man.hora-texec[3] V-table-Win
ON LEAVE OF mov-man.hora-texec[3] IN FRAME f-main /* Term[3] */
DO:
  IF KEYFUNCTION(LAST-KEY) = "TAB" THEN DO:
     ASSIGN mov-man.min-parado[3]:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
     APPLY "ENTRY" TO mov-man.min-parado[3] IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mov-man.hora-texec[4]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-man.hora-texec[4] V-table-Win
ON LEAVE OF mov-man.hora-texec[4] IN FRAME f-main /* Term[4] */
DO:
  IF KEYFUNCTION(LAST-KEY) = "TAB" THEN DO:
     ASSIGN mov-man.min-parado[4]:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
     APPLY "ENTRY" TO mov-man.min-parado[4] IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mov-man.hora-texec[5]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-man.hora-texec[5] V-table-Win
ON LEAVE OF mov-man.hora-texec[5] IN FRAME f-main /* Term[5] */
DO:
  IF KEYFUNCTION(LAST-KEY) = "TAB" THEN DO:
     ASSIGN mov-man.min-parado[5]:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
     APPLY "ENTRY" TO mov-man.min-parado[5] IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mov-man.hora-texec[6]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-man.hora-texec[6] V-table-Win
ON LEAVE OF mov-man.hora-texec[6] IN FRAME f-main /* Term[6] */
DO:
  IF KEYFUNCTION(LAST-KEY) = "TAB" THEN DO:
     ASSIGN mov-man.min-parado[6]:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
     APPLY "ENTRY" TO mov-man.min-parado[6] IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mov-man.hora-texec[7]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-man.hora-texec[7] V-table-Win
ON LEAVE OF mov-man.hora-texec[7] IN FRAME f-main /* Term[7] */
DO:
  IF KEYFUNCTION(LAST-KEY) = "TAB" THEN DO:
     ASSIGN mov-man.min-parado[7]:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
     APPLY "ENTRY" TO mov-man.min-parado[7] IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mov-man.hora-texec[8]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-man.hora-texec[8] V-table-Win
ON LEAVE OF mov-man.hora-texec[8] IN FRAME f-main /* Term[8] */
DO:
  IF KEYFUNCTION(LAST-KEY) = "TAB" THEN DO:
     ASSIGN mov-man.min-parado[8]:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
     APPLY "ENTRY" TO mov-man.min-parado[8] IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mov-man.hora-texec[9]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-man.hora-texec[9] V-table-Win
ON LEAVE OF mov-man.hora-texec[9] IN FRAME f-main /* Term[9] */
DO:
  IF KEYFUNCTION(LAST-KEY) = "TAB" THEN DO:
     ASSIGN mov-man.min-parado[9]:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
     APPLY "ENTRY" TO mov-man.min-parado[9] IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mov-man.min-parado[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-man.min-parado[1] V-table-Win
ON LEAVE OF mov-man.min-parado[1] IN FRAME f-main /* MIN.PARADO */
DO:
  IF KEYFUNCTION(LAST-KEY) = "TAB" THEN DO:
     ASSIGN mov-man.ativ-exec[2]:SENSITIVE IN FRAME {&FRAME-NAME} = YES. 
     APPLY 'entry' TO mov-man.ativ-exec[2] IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY. 
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mov-man.min-parado[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-man.min-parado[2] V-table-Win
ON LEAVE OF mov-man.min-parado[2] IN FRAME f-main /* MPar[2] */
DO:
  IF KEYFUNCTION(LAST-KEY) = "TAB" THEN DO:
     ASSIGN mov-man.ativ-exec[3]:SENSITIVE IN FRAME {&FRAME-NAME} = YES. 
     APPLY 'entry' TO mov-man.ativ-exec[3] IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY. 
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mov-man.min-parado[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-man.min-parado[3] V-table-Win
ON LEAVE OF mov-man.min-parado[3] IN FRAME f-main /* MPar[3] */
DO:
  IF KEYFUNCTION(LAST-KEY) = "TAB" THEN DO:
     ASSIGN mov-man.ativ-exec[4]:SENSITIVE IN FRAME {&FRAME-NAME} = YES. 
     APPLY 'entry' TO mov-man.ativ-exec[4] IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY. 
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mov-man.min-parado[4]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-man.min-parado[4] V-table-Win
ON LEAVE OF mov-man.min-parado[4] IN FRAME f-main /* MPar[4] */
DO:
  IF KEYFUNCTION(LAST-KEY) = "TAB" THEN DO:
     ASSIGN mov-man.ativ-exec[5]:SENSITIVE IN FRAME {&FRAME-NAME} = YES. 
     APPLY 'entry' TO mov-man.ativ-exec[5] IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY. 
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mov-man.min-parado[5]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-man.min-parado[5] V-table-Win
ON LEAVE OF mov-man.min-parado[5] IN FRAME f-main /* MPar[5] */
DO:
  IF KEYFUNCTION(LAST-KEY) = "TAB" THEN DO:
     ASSIGN mov-man.ativ-exec[6]:SENSITIVE IN FRAME {&FRAME-NAME} = YES. 
     APPLY 'entry' TO mov-man.ativ-exec[6] IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY. 
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mov-man.min-parado[6]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-man.min-parado[6] V-table-Win
ON LEAVE OF mov-man.min-parado[6] IN FRAME f-main /* MPar[6] */
DO:
  IF KEYFUNCTION(LAST-KEY) = "TAB" THEN DO:
     ASSIGN mov-man.ativ-exec[7]:SENSITIVE IN FRAME {&FRAME-NAME} = YES. 
     APPLY 'entry' TO mov-man.ativ-exec[7] IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY. 
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mov-man.min-parado[7]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-man.min-parado[7] V-table-Win
ON LEAVE OF mov-man.min-parado[7] IN FRAME f-main /* MPar[7] */
DO:
  IF KEYFUNCTION(LAST-KEY) = "TAB" THEN DO:
     ASSIGN mov-man.ativ-exec[8]:SENSITIVE IN FRAME {&FRAME-NAME} = YES. 
     APPLY 'entry' TO mov-man.ativ-exec[8] IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY. 
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mov-man.min-parado[8]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-man.min-parado[8] V-table-Win
ON LEAVE OF mov-man.min-parado[8] IN FRAME f-main /* MPar[8] */
DO:
  IF KEYFUNCTION(LAST-KEY) = "TAB" THEN DO:
     ASSIGN mov-man.ativ-exec[9]:SENSITIVE IN FRAME {&FRAME-NAME} = YES. 
     APPLY 'entry' TO mov-man.ativ-exec[9] IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY. 
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mov-man.min-parado[9]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-man.min-parado[9] V-table-Win
ON LEAVE OF mov-man.min-parado[9] IN FRAME f-main /* MPar[9] */
DO:
  IF KEYFUNCTION(LAST-KEY) = "TAB" THEN DO:
     ASSIGN mov-man.ativ-exec[10]:SENSITIVE IN FRAME {&FRAME-NAME} = YES. 
     APPLY 'entry' TO mov-man.ativ-exec[10] IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY. 
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

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

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "mov-man"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "mov-man"}

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
    
    /* Ponha na pi-validate todas as validaá‰es */
    /* N∆o gravar nada no registro antes do dispatch do assign-record e 
       nem na PI-validate. */
    
    RUN pi-validate.
    if RETURN-VALUE = 'ADM-ERROR':U then 
       return 'ADM-ERROR':U.

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.
    
    /* Todos os assignÔs n∆o feitos pelo assign-record devem ser feitos aqui */  
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

    ASSIGN mov-man.func-exec[1]:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           mov-man.hora-iexec[1]:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           mov-man.hora-texec[1]:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           mov-man.min-parado[1]:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           mov-man.ativ-exec[2]:SENSITIVE IN FRAME {&FRAME-NAME} = NO 
           mov-man.func-exec[2]:SENSITIVE IN FRAME {&FRAME-NAME} = NO 
           mov-man.hora-iexec[2]:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           mov-man.hora-texec[2]:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           mov-man.min-parado[2]:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           mov-man.ativ-exec[3]:SENSITIVE IN FRAME {&FRAME-NAME} = NO 
           mov-man.func-exec[3]:SENSITIVE IN FRAME {&FRAME-NAME} = NO 
           mov-man.hora-iexec[3]:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           mov-man.hora-texec[3]:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           mov-man.min-parado[3]:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           mov-man.ativ-exec[4]:SENSITIVE IN FRAME {&FRAME-NAME} = NO 
           mov-man.func-exec[4]:SENSITIVE IN FRAME {&FRAME-NAME} = NO 
           mov-man.hora-iexec[4]:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           mov-man.hora-texec[4]:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           mov-man.min-parado[4]:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           mov-man.ativ-exec[5]:SENSITIVE IN FRAME {&FRAME-NAME} = NO 
           mov-man.func-exec[5]:SENSITIVE IN FRAME {&FRAME-NAME} = NO 
           mov-man.hora-iexec[5]:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           mov-man.hora-texec[5]:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           mov-man.min-parado[5]:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           mov-man.ativ-exec[6]:SENSITIVE IN FRAME {&FRAME-NAME} = NO 
           mov-man.func-exec[6]:SENSITIVE IN FRAME {&FRAME-NAME} = NO 
           mov-man.hora-iexec[6]:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           mov-man.hora-texec[6]:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           mov-man.min-parado[6]:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           mov-man.ativ-exec[7]:SENSITIVE IN FRAME {&FRAME-NAME} = NO 
           mov-man.func-exec[7]:SENSITIVE IN FRAME {&FRAME-NAME} = NO 
           mov-man.hora-iexec[7]:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           mov-man.hora-texec[7]:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           mov-man.min-parado[7]:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           mov-man.ativ-exec[8]:SENSITIVE IN FRAME {&FRAME-NAME} = NO 
           mov-man.func-exec[8]:SENSITIVE IN FRAME {&FRAME-NAME} = NO 
           mov-man.hora-iexec[8]:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           mov-man.hora-texec[8]:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           mov-man.min-parado[8]:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           mov-man.ativ-exec[9]:SENSITIVE IN FRAME {&FRAME-NAME} = NO 
           mov-man.func-exec[9]:SENSITIVE IN FRAME {&FRAME-NAME} = NO 
           mov-man.hora-iexec[9]:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           mov-man.hora-texec[9]:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           mov-man.min-parado[9]:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           mov-man.ativ-exec[10]:SENSITIVE IN FRAME {&FRAME-NAME} = NO 
           mov-man.func-exec[10]:SENSITIVE IN FRAME {&FRAME-NAME} = NO 
           mov-man.hora-iexec[10]:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           mov-man.hora-texec[10]:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           mov-man.min-parado[10]:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
        
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
  Notes: N∆o fazer assign aqui. Nesta procedure
  devem ser colocadas apenas validaá‰es, pois neste ponto do programa o registro 
  ainda n∆o foi criado.       
------------------------------------------------------------------------------*/
    {include/i-vldfrm.i} /* Validaá∆o de dicion†rio */
 /*
    IF mov-man.ativ-exec[1]:SCREEN-VALUE IN FRAME {&FRAME-NAME} <> "6" THEN DO:
       MESSAGE "Valor incorreto - foda-se!" VIEW-AS ALERT-BOX.
       APPLY "entry" TO mov-man.ativ-exec[1].
       return 'ADM-ERROR':U.
    END.
 */
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
  {src/adm/template/snd-list.i "mov-man"}

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

