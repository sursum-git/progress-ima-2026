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
&Scoped-Define ENABLED-FIELDS mov-man.ativ-exec[1] 
&Scoped-define ENABLED-TABLES mov-man
&Scoped-define FIRST-ENABLED-TABLE mov-man
&Scoped-Define ENABLED-OBJECTS rt-mold 
&Scoped-Define DISPLAYED-FIELDS mov-man.ativ-exec[1] 
&Scoped-define DISPLAYED-TABLES mov-man
&Scoped-define FIRST-DISPLAYED-TABLE mov-man
&Scoped-Define DISPLAYED-OBJECTS fi-func-exec1 fi-hora-iexec1 ~
fi-hora-texec1 fi-min-parado1 fi-ativ-exec2 fi-func-exec2 fi-hora-iexec2 ~
fi-hora-texec2 fi-min-parado2 fi-ativ-exec3 fi-func-exec3 fi-hora-iexec3 ~
fi-hora-texec3 fi-min-parado3 fi-ativ-exec4 fi-func-exec4 fi-hora-iexec4 ~
fi-hora-texec4 fi-min-parado4 fi-ativ-exec5 fi-func-exec5 fi-hora-iexec5 ~
fi-hora-texec5 fi-min-parado5 fi-ativ-exec6 fi-func-exec6 fi-hora-iexec6 ~
fi-hora-texec6 fi-min-parado6 fi-ativ-exec7 fi-func-exec7 fi-hora-iexec7 ~
fi-hora-texec7 fi-min-parado7 fi-ativ-exec8 fi-func-exec8 fi-hora-iexec8 ~
fi-hora-texec8 fi-min-parado8 fi-ativ-exec9 fi-func-exec9 fi-hora-iexec9 ~
fi-hora-texec9 fi-min-parado9 fi-ativ-exec10 fi-func-exec10 fi-hora-iexec10 ~
fi-hora-texec10 fi-min-parado10 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define List-4 fi-func-exec1 fi-hora-iexec1 fi-hora-texec1 ~
fi-min-parado1 fi-ativ-exec2 fi-func-exec2 fi-hora-iexec2 fi-hora-texec2 ~
fi-min-parado2 fi-ativ-exec3 fi-func-exec3 fi-hora-iexec3 fi-hora-texec3 ~
fi-min-parado3 fi-ativ-exec4 fi-func-exec4 fi-hora-iexec4 fi-hora-texec4 ~
fi-min-parado4 fi-ativ-exec5 fi-func-exec5 fi-hora-iexec5 fi-hora-texec5 ~
fi-min-parado5 fi-ativ-exec6 fi-func-exec6 fi-hora-iexec6 fi-hora-texec6 ~
fi-min-parado6 fi-ativ-exec7 fi-func-exec7 fi-hora-iexec7 fi-hora-texec7 ~
fi-min-parado7 fi-ativ-exec8 fi-func-exec8 fi-hora-iexec8 fi-hora-texec8 ~
fi-min-parado8 fi-ativ-exec9 fi-func-exec9 fi-hora-iexec9 fi-hora-texec9 ~
fi-min-parado9 fi-ativ-exec10 fi-func-exec10 fi-hora-iexec10 ~
fi-hora-texec10 fi-min-parado10 

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
DEFINE VARIABLE fi-ativ-exec10 AS CHARACTER FORMAT "x(1)" 
     VIEW-AS FILL-IN 
     SIZE 3.14 BY .88.

DEFINE VARIABLE fi-ativ-exec2 AS CHARACTER FORMAT "x(1)" 
     VIEW-AS FILL-IN 
     SIZE 3.14 BY .88.

DEFINE VARIABLE fi-ativ-exec3 AS CHARACTER FORMAT "x(1)" 
     VIEW-AS FILL-IN 
     SIZE 3.14 BY .88.

DEFINE VARIABLE fi-ativ-exec4 AS CHARACTER FORMAT "x(1)" 
     VIEW-AS FILL-IN 
     SIZE 3.14 BY .88.

DEFINE VARIABLE fi-ativ-exec5 AS CHARACTER FORMAT "x(1)" 
     VIEW-AS FILL-IN 
     SIZE 3.14 BY .88.

DEFINE VARIABLE fi-ativ-exec6 AS CHARACTER FORMAT "x(1)" 
     VIEW-AS FILL-IN 
     SIZE 3.14 BY .88.

DEFINE VARIABLE fi-ativ-exec7 AS CHARACTER FORMAT "x(1)" 
     VIEW-AS FILL-IN 
     SIZE 3.14 BY .88.

DEFINE VARIABLE fi-ativ-exec8 AS CHARACTER FORMAT "x(1)" 
     VIEW-AS FILL-IN 
     SIZE 3.14 BY .88.

DEFINE VARIABLE fi-ativ-exec9 AS CHARACTER FORMAT "x(1)" 
     VIEW-AS FILL-IN 
     SIZE 3.14 BY .88.

DEFINE VARIABLE fi-func-exec1 AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "FUNCIONµRIO" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88.

DEFINE VARIABLE fi-func-exec10 AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88.

DEFINE VARIABLE fi-func-exec2 AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88.

DEFINE VARIABLE fi-func-exec3 AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88.

DEFINE VARIABLE fi-func-exec4 AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88.

DEFINE VARIABLE fi-func-exec5 AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88.

DEFINE VARIABLE fi-func-exec6 AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88.

DEFINE VARIABLE fi-func-exec7 AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88.

DEFINE VARIABLE fi-func-exec8 AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88.

DEFINE VARIABLE fi-func-exec9 AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88.

DEFINE VARIABLE fi-hora-iexec1 AS CHARACTER FORMAT "xx:xx" 
     LABEL "IN÷CIO" 
     VIEW-AS FILL-IN 
     SIZE 6.86 BY .88.

DEFINE VARIABLE fi-hora-iexec10 AS CHARACTER FORMAT "xx:xx" 
     VIEW-AS FILL-IN 
     SIZE 6.86 BY .88.

DEFINE VARIABLE fi-hora-iexec2 AS CHARACTER FORMAT "xx:xx" 
     VIEW-AS FILL-IN 
     SIZE 6.86 BY .88.

DEFINE VARIABLE fi-hora-iexec3 AS CHARACTER FORMAT "xx:xx" 
     VIEW-AS FILL-IN 
     SIZE 6.86 BY .88.

DEFINE VARIABLE fi-hora-iexec4 AS CHARACTER FORMAT "xx:xx" 
     VIEW-AS FILL-IN 
     SIZE 6.86 BY .88.

DEFINE VARIABLE fi-hora-iexec5 AS CHARACTER FORMAT "xx:xx" 
     VIEW-AS FILL-IN 
     SIZE 6.86 BY .88.

DEFINE VARIABLE fi-hora-iexec6 AS CHARACTER FORMAT "xx:xx" 
     VIEW-AS FILL-IN 
     SIZE 6.86 BY .88.

DEFINE VARIABLE fi-hora-iexec7 AS CHARACTER FORMAT "xx:xx" 
     VIEW-AS FILL-IN 
     SIZE 6.86 BY .88.

DEFINE VARIABLE fi-hora-iexec8 AS CHARACTER FORMAT "xx:xx" 
     VIEW-AS FILL-IN 
     SIZE 6.86 BY .88.

DEFINE VARIABLE fi-hora-iexec9 AS CHARACTER FORMAT "xx:xx" 
     VIEW-AS FILL-IN 
     SIZE 6.86 BY .88.

DEFINE VARIABLE fi-hora-texec1 AS CHARACTER FORMAT "xx:xx" 
     LABEL "TêRMINO" 
     VIEW-AS FILL-IN 
     SIZE 6.86 BY .88.

DEFINE VARIABLE fi-hora-texec10 AS CHARACTER FORMAT "xx:xx" 
     VIEW-AS FILL-IN 
     SIZE 6.86 BY .88.

DEFINE VARIABLE fi-hora-texec2 AS CHARACTER FORMAT "xx:xx" 
     VIEW-AS FILL-IN 
     SIZE 6.86 BY .88.

DEFINE VARIABLE fi-hora-texec3 AS CHARACTER FORMAT "xx:xx" 
     VIEW-AS FILL-IN 
     SIZE 6.86 BY .88.

DEFINE VARIABLE fi-hora-texec4 AS CHARACTER FORMAT "xx:xx" 
     VIEW-AS FILL-IN 
     SIZE 6.86 BY .88.

DEFINE VARIABLE fi-hora-texec5 AS CHARACTER FORMAT "xx:xx" 
     VIEW-AS FILL-IN 
     SIZE 6.86 BY .88.

DEFINE VARIABLE fi-hora-texec6 AS CHARACTER FORMAT "xx:xx" 
     VIEW-AS FILL-IN 
     SIZE 6.86 BY .88.

DEFINE VARIABLE fi-hora-texec7 AS CHARACTER FORMAT "xx:xx" 
     VIEW-AS FILL-IN 
     SIZE 6.86 BY .88.

DEFINE VARIABLE fi-hora-texec8 AS CHARACTER FORMAT "xx:xx" 
     VIEW-AS FILL-IN 
     SIZE 6.86 BY .88.

DEFINE VARIABLE fi-hora-texec9 AS CHARACTER FORMAT "xx:xx" 
     VIEW-AS FILL-IN 
     SIZE 6.86 BY .88.

DEFINE VARIABLE fi-min-parado1 AS INTEGER FORMAT ">>>9" INITIAL 0 
     LABEL "MIN.PARADO" 
     VIEW-AS FILL-IN 
     SIZE 5.72 BY .88 TOOLTIP "Minutos parados".

DEFINE VARIABLE fi-min-parado10 AS INTEGER FORMAT ">>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5.72 BY .88.

DEFINE VARIABLE fi-min-parado2 AS INTEGER FORMAT ">>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5.72 BY .88.

DEFINE VARIABLE fi-min-parado3 AS INTEGER FORMAT ">>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5.72 BY .88.

DEFINE VARIABLE fi-min-parado4 AS INTEGER FORMAT ">>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5.72 BY .88.

DEFINE VARIABLE fi-min-parado5 AS INTEGER FORMAT ">>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5.72 BY .88.

DEFINE VARIABLE fi-min-parado6 AS INTEGER FORMAT ">>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5.72 BY .88.

DEFINE VARIABLE fi-min-parado7 AS INTEGER FORMAT ">>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5.72 BY .88.

DEFINE VARIABLE fi-min-parado8 AS INTEGER FORMAT ">>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5.72 BY .88.

DEFINE VARIABLE fi-min-parado9 AS INTEGER FORMAT ">>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5.72 BY .88.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 87 BY 10.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     mov-man.ativ-exec[1] AT ROW 1.17 COL 5 COLON-ALIGNED
          LABEL "ATIV"
          VIEW-AS FILL-IN 
          SIZE 3.14 BY 1
     fi-func-exec1 AT ROW 1.25 COL 23.43 COLON-ALIGNED
     fi-hora-iexec1 AT ROW 1.25 COL 40 COLON-ALIGNED
     fi-hora-texec1 AT ROW 1.25 COL 58.29 COLON-ALIGNED
     fi-min-parado1 AT ROW 1.25 COL 79.14 COLON-ALIGNED
     fi-ativ-exec2 AT ROW 2.25 COL 5 COLON-ALIGNED NO-LABEL
     fi-func-exec2 AT ROW 2.25 COL 23.43 COLON-ALIGNED NO-LABEL
     fi-hora-iexec2 AT ROW 2.25 COL 40 COLON-ALIGNED NO-LABEL
     fi-hora-texec2 AT ROW 2.25 COL 58.29 COLON-ALIGNED NO-LABEL
     fi-min-parado2 AT ROW 2.25 COL 79.14 COLON-ALIGNED NO-LABEL
     fi-ativ-exec3 AT ROW 3.21 COL 5 COLON-ALIGNED NO-LABEL
     fi-func-exec3 AT ROW 3.21 COL 23.43 COLON-ALIGNED NO-LABEL
     fi-hora-iexec3 AT ROW 3.21 COL 40 COLON-ALIGNED NO-LABEL
     fi-hora-texec3 AT ROW 3.21 COL 58.29 COLON-ALIGNED NO-LABEL
     fi-min-parado3 AT ROW 3.21 COL 79.14 COLON-ALIGNED NO-LABEL
     fi-ativ-exec4 AT ROW 4.21 COL 5 COLON-ALIGNED NO-LABEL
     fi-func-exec4 AT ROW 4.21 COL 23.43 COLON-ALIGNED NO-LABEL
     fi-hora-iexec4 AT ROW 4.21 COL 40 COLON-ALIGNED NO-LABEL
     fi-hora-texec4 AT ROW 4.21 COL 58.29 COLON-ALIGNED NO-LABEL
     fi-min-parado4 AT ROW 4.21 COL 79.14 COLON-ALIGNED NO-LABEL
     fi-ativ-exec5 AT ROW 5.17 COL 5 COLON-ALIGNED NO-LABEL
     fi-func-exec5 AT ROW 5.17 COL 23.43 COLON-ALIGNED NO-LABEL
     fi-hora-iexec5 AT ROW 5.17 COL 40 COLON-ALIGNED NO-LABEL
     fi-hora-texec5 AT ROW 5.17 COL 58.29 COLON-ALIGNED NO-LABEL
     fi-min-parado5 AT ROW 5.17 COL 79.14 COLON-ALIGNED NO-LABEL
     fi-ativ-exec6 AT ROW 6.17 COL 5 COLON-ALIGNED NO-LABEL
     fi-func-exec6 AT ROW 6.17 COL 23.43 COLON-ALIGNED NO-LABEL
     fi-hora-iexec6 AT ROW 6.17 COL 40 COLON-ALIGNED NO-LABEL
     fi-hora-texec6 AT ROW 6.17 COL 58.29 COLON-ALIGNED NO-LABEL
     fi-min-parado6 AT ROW 6.17 COL 79.14 COLON-ALIGNED NO-LABEL
     fi-ativ-exec7 AT ROW 7.13 COL 5 COLON-ALIGNED NO-LABEL
     fi-func-exec7 AT ROW 7.13 COL 23.43 COLON-ALIGNED NO-LABEL
     fi-hora-iexec7 AT ROW 7.13 COL 40 COLON-ALIGNED NO-LABEL
     fi-hora-texec7 AT ROW 7.13 COL 58.29 COLON-ALIGNED NO-LABEL
     fi-min-parado7 AT ROW 7.13 COL 79.14 COLON-ALIGNED NO-LABEL
     fi-ativ-exec8 AT ROW 8.13 COL 5 COLON-ALIGNED NO-LABEL
     fi-func-exec8 AT ROW 8.13 COL 23.43 COLON-ALIGNED NO-LABEL
     fi-hora-iexec8 AT ROW 8.13 COL 40 COLON-ALIGNED NO-LABEL
     fi-hora-texec8 AT ROW 8.13 COL 58.29 COLON-ALIGNED NO-LABEL
     fi-min-parado8 AT ROW 8.13 COL 79.14 COLON-ALIGNED NO-LABEL
     fi-ativ-exec9 AT ROW 9.08 COL 5 COLON-ALIGNED NO-LABEL
     fi-func-exec9 AT ROW 9.08 COL 23.43 COLON-ALIGNED NO-LABEL
     fi-hora-iexec9 AT ROW 9.08 COL 40 COLON-ALIGNED NO-LABEL
     fi-hora-texec9 AT ROW 9.08 COL 58.29 COLON-ALIGNED NO-LABEL
     fi-min-parado9 AT ROW 9.08 COL 79.14 COLON-ALIGNED NO-LABEL
     fi-ativ-exec10 AT ROW 10.08 COL 5 COLON-ALIGNED NO-LABEL
     fi-func-exec10 AT ROW 10.08 COL 23.43 COLON-ALIGNED NO-LABEL
     fi-hora-iexec10 AT ROW 10.08 COL 40 COLON-ALIGNED NO-LABEL
     fi-hora-texec10 AT ROW 10.08 COL 58.29 COLON-ALIGNED NO-LABEL
     fi-min-parado10 AT ROW 10.08 COL 79.14 COLON-ALIGNED NO-LABEL
     rt-mold AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


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
/* SETTINGS FOR FILL-IN fi-ativ-exec10 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-ativ-exec2 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-ativ-exec3 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-ativ-exec4 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-ativ-exec5 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-ativ-exec6 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-ativ-exec7 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-ativ-exec8 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-ativ-exec9 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-func-exec1 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-func-exec10 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-func-exec2 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-func-exec3 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-func-exec4 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-func-exec5 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-func-exec6 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-func-exec7 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-func-exec8 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-func-exec9 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-hora-iexec1 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-hora-iexec10 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-hora-iexec2 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-hora-iexec3 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-hora-iexec4 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-hora-iexec5 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-hora-iexec6 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-hora-iexec7 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-hora-iexec8 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-hora-iexec9 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-hora-texec1 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-hora-texec10 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-hora-texec2 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-hora-texec3 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-hora-texec4 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-hora-texec5 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-hora-texec6 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-hora-texec7 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-hora-texec8 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-hora-texec9 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-min-parado1 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-min-parado10 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-min-parado2 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-min-parado3 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-min-parado4 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-min-parado5 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-min-parado6 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-min-parado7 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-min-parado8 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-min-parado9 IN FRAME f-main
   NO-ENABLE 4                                                          */
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

&Scoped-define SELF-NAME fi-ativ-exec10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ativ-exec10 V-table-Win
ON LEAVE OF fi-ativ-exec10 IN FRAME f-main
DO:
  IF lookup(INPUT FRAME {&frame-name} fi-ativ-exec10,"1,2,3,4,5,") = 0 THEN DO:
     MESSAGE "Atividade deve ser 1,2,3,4 ou 5." VIEW-AS ALERT-BOX.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ativ-exec2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ativ-exec2 V-table-Win
ON LEAVE OF fi-ativ-exec2 IN FRAME f-main
DO:
  IF lookup(INPUT FRAME {&frame-name} fi-ativ-exec2,"1,2,3,4,5,") = 0 THEN DO:
     MESSAGE "Atividade deve ser 1,2,3,4 ou 5." VIEW-AS ALERT-BOX.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ativ-exec3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ativ-exec3 V-table-Win
ON LEAVE OF fi-ativ-exec3 IN FRAME f-main
DO:
  IF lookup(INPUT FRAME {&frame-name} fi-ativ-exec3,"1,2,3,4,5,") = 0 THEN DO:
     MESSAGE "Atividade deve ser 1,2,3,4 ou 5." VIEW-AS ALERT-BOX.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ativ-exec4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ativ-exec4 V-table-Win
ON LEAVE OF fi-ativ-exec4 IN FRAME f-main
DO:
  IF lookup(INPUT FRAME {&frame-name} fi-ativ-exec4,"1,2,3,4,5,") = 0 THEN DO:
     MESSAGE "Atividade deve ser 1,2,3,4 ou 5." VIEW-AS ALERT-BOX.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ativ-exec5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ativ-exec5 V-table-Win
ON LEAVE OF fi-ativ-exec5 IN FRAME f-main
DO:
  IF lookup(INPUT FRAME {&frame-name} fi-ativ-exec5,"1,2,3,4,5,") = 0 THEN DO:
     MESSAGE "Atividade deve ser 1,2,3,4 ou 5." VIEW-AS ALERT-BOX.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ativ-exec6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ativ-exec6 V-table-Win
ON LEAVE OF fi-ativ-exec6 IN FRAME f-main
DO:
  IF lookup(INPUT FRAME {&frame-name} fi-ativ-exec6,"1,2,3,4,5,") = 0 THEN DO:
     MESSAGE "Atividade deve ser 1,2,3,4 ou 5." VIEW-AS ALERT-BOX.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ativ-exec7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ativ-exec7 V-table-Win
ON LEAVE OF fi-ativ-exec7 IN FRAME f-main
DO:
  IF lookup(INPUT FRAME {&frame-name} fi-ativ-exec7,"1,2,3,4,5,") = 0 THEN DO:
     MESSAGE "Atividade deve ser 1,2,3,4 ou 5." VIEW-AS ALERT-BOX.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ativ-exec8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ativ-exec8 V-table-Win
ON LEAVE OF fi-ativ-exec8 IN FRAME f-main
DO:
  IF lookup(INPUT FRAME {&frame-name} fi-ativ-exec8,"1,2,3,4,5,") = 0 THEN DO:
     MESSAGE "Atividade deve ser 1,2,3,4 ou 5." VIEW-AS ALERT-BOX.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ativ-exec9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ativ-exec9 V-table-Win
ON LEAVE OF fi-ativ-exec9 IN FRAME f-main
DO:
  IF lookup(INPUT FRAME {&frame-name} fi-ativ-exec9,"1,2,3,4,5,") = 0 THEN DO:
     MESSAGE "Atividade deve ser 1,2,3,4 ou 5." VIEW-AS ALERT-BOX.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-hora-iexec1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-hora-iexec1 V-table-Win
ON LEAVE OF fi-hora-iexec1 IN FRAME f-main /* IN÷CIO */
DO:
   IF INPUT FRAME {&frame-name} fi-hora-iexec1 <> "" THEN DO.
      IF NOT (SUBSTR(INPUT FRAME {&FRAME-NAME} fi-hora-iexec1,1,1) >= "0" and
              SUBSTR(INPUT FRAME {&FRAME-NAME} fi-hora-iexec1,1,1) <= "9" AND
              SUBSTR(INPUT FRAME {&FRAME-NAME} fi-hora-iexec1,2,1) >= "0" AND
              SUBSTR(INPUT FRAME {&FRAME-NAME} fi-hora-iexec1,2,1) <= "9" AND
              SUBSTR(INPUT FRAME {&FRAME-NAME} fi-hora-iexec1,3,1) >= "0" and
              SUBSTR(INPUT FRAME {&FRAME-NAME} fi-hora-iexec1,3,1) <= "9" AND
              SUBSTR(INPUT FRAME {&FRAME-NAME} fi-hora-iexec1,4,1) >= "0" AND
              SUBSTR(INPUT FRAME {&FRAME-NAME} fi-hora-iexec1,4,1) <= "9") THEN DO:
         MESSAGE "Hora contÇm caracteres inv†lidos." VIEW-AS ALERT-BOX. 
         RETURN NO-APPLY.
      END.
      ELSE
      IF NOT (SUBSTR(INPUT FRAME {&FRAME-NAME} fi-hora-iexec1,1,2) >= "00" and
              SUBSTR(INPUT FRAME {&FRAME-NAME} fi-hora-iexec1,1,2) <= "23" AND
              SUBSTR(INPUT FRAME {&FRAME-NAME} fi-hora-iexec1,3,2) >= "00" AND
              SUBSTR(INPUT FRAME {&FRAME-NAME} fi-hora-iexec1,3,2) <= "59")  THEN DO:
         MESSAGE "Hora deve estar entre 00:00 e 23:59." VIEW-AS ALERT-BOX. 
         RETURN NO-APPLY.
      END.
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

    /*--- O bloco abaixo foi comentado devido ao fato de n∆o existir campos 
     relacionados ao Banco de Dados nesta viewer.
     */
    
    /* Dispatch standard ADM method.                             */
     RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
     if RETURN-VALUE = 'ADM-ERROR':U then 
         return 'ADM-ERROR':U.

    /* Todos os assignÔs n∆o feitos pelo assign-record devem ser feitos aqui */  
    /* Code placed here will execute AFTER standard behavior.    */
    ASSIGN mov-man.ativ-exec[2]   = fi-ativ-exec2
           mov-man.ativ-exec[3]   = fi-ativ-exec3
           mov-man.ativ-exec[4]   = fi-ativ-exec4
           mov-man.ativ-exec[5]   = fi-ativ-exec5
           mov-man.ativ-exec[6]   = fi-ativ-exec6
           mov-man.ativ-exec[7]   = fi-ativ-exec7
           mov-man.ativ-exec[8]   = fi-ativ-exec8
           mov-man.ativ-exec[9]   = fi-ativ-exec9
           mov-man.ativ-exec[10]  = fi-ativ-exec10
           mov-man.func-exec[1]   = fi-func-exec1 
           mov-man.func-exec[2]   = fi-func-exec2 
           mov-man.func-exec[3]   = fi-func-exec3 
           mov-man.func-exec[4]   = fi-func-exec4 
           mov-man.func-exec[5]   = fi-func-exec5 
           mov-man.func-exec[6]   = fi-func-exec6 
           mov-man.func-exec[7]   = fi-func-exec7 
           mov-man.func-exec[8]   = fi-func-exec8 
           mov-man.func-exec[9]   = fi-func-exec9 
           mov-man.func-exec[10]  = fi-func-exec10
           mov-man.hora-iexec[1]  = fi-hora-iexec1 
           mov-man.hora-iexec[2]  = fi-hora-iexec2 
           mov-man.hora-iexec[3]  = fi-hora-iexec3 
           mov-man.hora-iexec[4]  = fi-hora-iexec4 
           mov-man.hora-iexec[5]  = fi-hora-iexec5 
           mov-man.hora-iexec[6]  = fi-hora-iexec6 
           mov-man.hora-iexec[7]  = fi-hora-iexec7 
           mov-man.hora-iexec[8]  = fi-hora-iexec8 
           mov-man.hora-iexec[9]  = fi-hora-iexec9 
           mov-man.hora-iexec[10] = fi-hora-iexec10
           mov-man.hora-texec[1]  = fi-hora-texec1 
           mov-man.hora-texec[2]  = fi-hora-texec2 
           mov-man.hora-texec[3]  = fi-hora-texec3 
           mov-man.hora-texec[4]  = fi-hora-texec4 
           mov-man.hora-texec[5]  = fi-hora-texec5 
           mov-man.hora-texec[6]  = fi-hora-texec6 
           mov-man.hora-texec[7]  = fi-hora-texec7 
           mov-man.hora-texec[8]  = fi-hora-texec8 
           mov-man.hora-texec[9]  = fi-hora-texec9 
           mov-man.hora-texec[10] = fi-hora-texec10
           mov-man.min-parado[1]  = fi-min-parado1 
           mov-man.min-parado[2]  = fi-min-parado2 
           mov-man.min-parado[3]  = fi-min-parado3 
           mov-man.min-parado[4]  = fi-min-parado4 
           mov-man.min-parado[5]  = fi-min-parado5 
           mov-man.min-parado[6]  = fi-min-parado6 
           mov-man.min-parado[7]  = fi-min-parado7 
           mov-man.min-parado[8]  = fi-min-parado8 
           mov-man.min-parado[9]  = fi-min-parado9 
           mov-man.min-parado[10] = fi-min-parado10.

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
    
    &if  defined(list-4) &then
       DISABLE {&list-4} WITH FRAME {&FRAME-NAME}. 
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
    IF AVAIL mov-man THEN
       ASSIGN fi-ativ-exec2:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = mov-man.ativ-exec[2]
              fi-ativ-exec3:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = mov-man.ativ-exec[3]
              fi-ativ-exec4:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = mov-man.ativ-exec[4]
              fi-ativ-exec5:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = mov-man.ativ-exec[5]
              fi-ativ-exec6:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = mov-man.ativ-exec[6]
              fi-ativ-exec7:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = mov-man.ativ-exec[7]
              fi-ativ-exec8:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = mov-man.ativ-exec[8]
              fi-ativ-exec9:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = mov-man.ativ-exec[9]
              fi-ativ-exec10:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = mov-man.ativ-exec[10]
              fi-func-exec1:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = string(mov-man.func-exec[1])  
              fi-func-exec2:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = string(mov-man.func-exec[2])  
              fi-func-exec3:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = string(mov-man.func-exec[3])  
              fi-func-exec4:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = string(mov-man.func-exec[4])  
              fi-func-exec5:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = string(mov-man.func-exec[5])  
              fi-func-exec6:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = string(mov-man.func-exec[6])  
              fi-func-exec7:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = string(mov-man.func-exec[7])  
              fi-func-exec8:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = string(mov-man.func-exec[8])  
              fi-func-exec9:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = string(mov-man.func-exec[9])  
              fi-func-exec10:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = string(mov-man.func-exec[10]) 
              fi-hora-iexec1:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = mov-man.hora-iexec[1]  
              fi-hora-iexec2:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = mov-man.hora-iexec[2]  
              fi-hora-iexec3:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = mov-man.hora-iexec[3]  
              fi-hora-iexec4:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = mov-man.hora-iexec[4]  
              fi-hora-iexec5:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = mov-man.hora-iexec[5]  
              fi-hora-iexec6:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = mov-man.hora-iexec[6]  
              fi-hora-iexec7:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = mov-man.hora-iexec[7]  
              fi-hora-iexec8:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = mov-man.hora-iexec[8]  
              fi-hora-iexec9:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = mov-man.hora-iexec[9]  
              fi-hora-iexec10:SCREEN-VALUE IN FRAME {&FRAME-NAME} = mov-man.hora-iexec[10] 
              fi-hora-texec1:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = mov-man.hora-texec[1] 
              fi-hora-texec2:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = mov-man.hora-texec[2] 
              fi-hora-texec3:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = mov-man.hora-texec[3] 
              fi-hora-texec4:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = mov-man.hora-texec[4] 
              fi-hora-texec5:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = mov-man.hora-texec[5] 
              fi-hora-texec6:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = mov-man.hora-texec[6] 
              fi-hora-texec7:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = mov-man.hora-texec[7] 
              fi-hora-texec8:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = mov-man.hora-texec[8] 
              fi-hora-texec9:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = mov-man.hora-texec[9] 
              fi-hora-texec10:SCREEN-VALUE IN FRAME {&FRAME-NAME} = mov-man.hora-texec[10]
              fi-min-parado1:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = string(mov-man.min-parado[1]) 
              fi-min-parado2:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = string(mov-man.min-parado[2]) 
              fi-min-parado3:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = string(mov-man.min-parado[3]) 
              fi-min-parado4:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = string(mov-man.min-parado[4]) 
              fi-min-parado5:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = string(mov-man.min-parado[5]) 
              fi-min-parado6:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = string(mov-man.min-parado[6]) 
              fi-min-parado7:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = string(mov-man.min-parado[7]) 
              fi-min-parado8:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = string(mov-man.min-parado[8]) 
              fi-min-parado9:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = string(mov-man.min-parado[9]) 
              fi-min-parado10:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(mov-man.min-parado[10]).                                                                          
       
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
    
    &if  defined(list-4) &then
       ENABLE {&list-4} WITH FRAME {&FRAME-NAME}.
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
  Purpose: Validar a viewer     
  Parameters: <none>
  Notes: N∆o fazer assign aqui. Nesta procedure
  devem ser colocadas apenas validaá‰es, pois neste ponto do programa o registro 
  ainda n∆o foi criado.       
------------------------------------------------------------------------------*/
    {include/i-vldfrm.i} /* Validaá∆o de dicion†rio */
    
  IF INPUT FRAME {&frame-name} fi-ativ-exec2 <> "" and
     INPUT FRAME {&frame-name} mov-man.ativ-exec[1] = "" THEN DO:
     /* {include/i-vldprg.i}
     run utp/ut-msgs.p (input "show":U, input 9, input return-value). */
     MESSAGE "A seqÅància de entrada das Atividades deve ser cont°nua." VIEW-AS ALERT-BOX. 
     APPLY 'entry' TO mov-man.ativ-exec[1].
     return 'ADM-ERROR':U.
  END.     
  IF INPUT FRAME {&frame-name} fi-ativ-exec3 <> "" and
     INPUT FRAME {&frame-name} fi-ativ-exec2 = "" THEN DO:
     MESSAGE "A seqÅància de entrada das Atividades deve ser cont°nua." VIEW-AS ALERT-BOX. 
     APPLY 'entry' TO fi-ativ-exec2.
     return 'ADM-ERROR':U.
  END.
  IF INPUT FRAME {&frame-name} fi-ativ-exec4 <> "" and
     INPUT FRAME {&frame-name} fi-ativ-exec3 = "" THEN DO:
     MESSAGE "A seqÅància de entrada das Atividades deve ser cont°nua." VIEW-AS ALERT-BOX. 
     APPLY 'entry' TO fi-ativ-exec3.
     return 'ADM-ERROR':U.
  END.
  IF INPUT FRAME {&frame-name} fi-ativ-exec5 <> "" and
     INPUT FRAME {&frame-name} fi-ativ-exec4 = "" THEN DO:
     MESSAGE "A seqÅància de entrada das Atividades deve ser cont°nua." VIEW-AS ALERT-BOX. 
     APPLY 'entry' TO fi-ativ-exec4.
     return 'ADM-ERROR':U.
  END.
  IF INPUT FRAME {&frame-name} fi-ativ-exec6 <> "" and
     INPUT FRAME {&frame-name} fi-ativ-exec5 = "" THEN DO:
     /* {include/i-vldprg.i}
     run utp/ut-msgs.p (input "show":U, input 9, input return-value). */
     MESSAGE "A seqÅància de entrada das Atividades deve ser cont°nua." VIEW-AS ALERT-BOX. 
     APPLY 'entry' TO fi-ativ-exec5.
     return 'ADM-ERROR':U.
  END.     
  IF INPUT FRAME {&frame-name} fi-ativ-exec7 <> "" and
     INPUT FRAME {&frame-name} fi-ativ-exec6 = "" THEN DO:
     MESSAGE "A seqÅància de entrada das Atividades deve ser cont°nua." VIEW-AS ALERT-BOX. 
     APPLY 'entry' TO fi-ativ-exec6.
     return 'ADM-ERROR':U.
  END.
  IF INPUT FRAME {&frame-name} fi-ativ-exec8 <> "" and
     INPUT FRAME {&frame-name} fi-ativ-exec7 = "" THEN DO:
     MESSAGE "A seqÅància de entrada das Atividades deve ser cont°nua." VIEW-AS ALERT-BOX. 
     APPLY 'entry' TO fi-ativ-exec7.
     return 'ADM-ERROR':U.
  END.
  IF INPUT FRAME {&frame-name} fi-ativ-exec9 <> "" and
     INPUT FRAME {&frame-name} fi-ativ-exec8 = "" THEN DO:
     MESSAGE "A seqÅància de entrada das Atividades deve ser cont°nua." VIEW-AS ALERT-BOX. 
     APPLY 'entry' TO fi-ativ-exec8.
     return 'ADM-ERROR':U.
  END.
  IF INPUT FRAME {&frame-name} fi-ativ-exec10 <> "" and
     INPUT FRAME {&frame-name} fi-ativ-exec9 = "" THEN DO:
     MESSAGE "A seqÅància de entrada das Atividades deve ser cont°nua." VIEW-AS ALERT-BOX. 
     APPLY 'entry' TO fi-ativ-exec9.
     return 'ADM-ERROR':U.
  END.

  IF (INPUT FRAME {&frame-name} mov-man.ativ-exec[1] = "" AND
      (INPUT FRAME {&frame-name} fi-func-exec1 <> 0 OR
       INPUT FRAME {&frame-name} fi-hora-iexec1 <> "" OR
       INPUT FRAME {&FRAME-NAME} fi-hora-texec1 <> "" OR
       INPUT FRAME {&FRAME-NAME} fi-min-parado1 <> 0)) OR
     (INPUT FRAME {&FRAME-NAME} mov-man.ativ-exec[1] <> "" AND
      (INPUT FRAME {&FRAME-NAME} fi-func-exec1 = 0 OR
       INPUT FRAME {&FRAME-NAME} fi-hora-iexec1 = "" OR
       INPUT FRAME {&FRAME-NAME} fi-hora-texec1 = "")) THEN DO:
     MESSAGE "N∆o pode haver Atividade sem Funcion†rio e/ou In°cio e TÇrmino" VIEW-AS ALERT-BOX. 
     APPLY 'entry' TO mov-man.ativ-exec[1].
     return 'ADM-ERROR':U.
  END.
  IF (INPUT FRAME {&frame-name} fi-ativ-exec2 = "" AND
      (INPUT FRAME {&frame-name} fi-func-exec2 <> 0 OR
       INPUT FRAME {&frame-name} fi-hora-iexec2 <> "" OR
       INPUT FRAME {&FRAME-NAME} fi-hora-texec2 <> "" OR
       INPUT FRAME {&FRAME-NAME} fi-min-parado2 <> 0)) OR
     (INPUT FRAME {&FRAME-NAME} fi-ativ-exec2 <> "" AND
      (INPUT FRAME {&FRAME-NAME} fi-func-exec2 = 0 OR
       INPUT FRAME {&FRAME-NAME} fi-hora-iexec2 = "" OR
       INPUT FRAME {&FRAME-NAME} fi-hora-texec2 = "")) THEN DO:
     MESSAGE "N∆o pode haver Atividade sem Funcion†rio e/ou In°cio e TÇrmino" VIEW-AS ALERT-BOX. 
     APPLY 'entry' TO fi-ativ-exec2.
     return 'ADM-ERROR':U.
  END.
  IF (INPUT FRAME {&frame-name} fi-ativ-exec3 = "" AND
      (INPUT FRAME {&frame-name} fi-func-exec3 <> 0 OR
       INPUT FRAME {&frame-name} fi-hora-iexec3 <> "" OR
       INPUT FRAME {&FRAME-NAME} fi-hora-texec3 <> "" OR
       INPUT FRAME {&FRAME-NAME} fi-min-parado3 <> 0)) OR
     (INPUT FRAME {&FRAME-NAME} fi-ativ-exec3 <> "" AND
      (INPUT FRAME {&FRAME-NAME} fi-func-exec3 = 0 OR
       INPUT FRAME {&FRAME-NAME} fi-hora-iexec3 = "" OR
       INPUT FRAME {&FRAME-NAME} fi-hora-texec3 = "")) THEN DO:
     MESSAGE "N∆o pode haver Atividade sem Funcion†rio e/ou In°cio e TÇrmino" VIEW-AS ALERT-BOX. 
     APPLY 'entry' TO fi-ativ-exec3.
     return 'ADM-ERROR':U.
  END.
  IF (INPUT FRAME {&frame-name} fi-ativ-exec4 = "" AND
      (INPUT FRAME {&frame-name} fi-func-exec4 <> 0 OR
       INPUT FRAME {&frame-name} fi-hora-iexec4 <> "" OR
       INPUT FRAME {&FRAME-NAME} fi-hora-texec4 <> "" OR
       INPUT FRAME {&FRAME-NAME} fi-min-parado4 <> 0)) OR
     (INPUT FRAME {&FRAME-NAME} fi-ativ-exec4 <> "" AND
      (INPUT FRAME {&FRAME-NAME} fi-func-exec4 = 0 OR
       INPUT FRAME {&FRAME-NAME} fi-hora-iexec4 = "" OR
       INPUT FRAME {&FRAME-NAME} fi-hora-texec4 = "")) THEN DO:
     MESSAGE "N∆o pode haver Atividade sem Funcion†rio e/ou In°cio e TÇrmino" VIEW-AS ALERT-BOX. 
     APPLY 'entry' TO fi-ativ-exec4.
     return 'ADM-ERROR':U.
  END.
  IF (INPUT FRAME {&frame-name} fi-ativ-exec5 = "" AND
      (INPUT FRAME {&frame-name} fi-func-exec5 <> 0 OR
       INPUT FRAME {&frame-name} fi-hora-iexec5 <> "" OR
       INPUT FRAME {&FRAME-NAME} fi-hora-texec5 <> "" OR
       INPUT FRAME {&FRAME-NAME} fi-min-parado5 <> 0)) OR
     (INPUT FRAME {&FRAME-NAME} fi-ativ-exec5 <> "" AND
      (INPUT FRAME {&FRAME-NAME} fi-func-exec5 = 0 OR
       INPUT FRAME {&FRAME-NAME} fi-hora-iexec5 = "" OR
       INPUT FRAME {&FRAME-NAME} fi-hora-texec5 = "")) THEN DO:
     MESSAGE "N∆o pode haver Atividade sem Funcion†rio e/ou In°cio e TÇrmino" VIEW-AS ALERT-BOX. 
     APPLY 'entry' TO fi-ativ-exec5.
     return 'ADM-ERROR':U.
  END.
  IF (INPUT FRAME {&frame-name} fi-ativ-exec6 = "" AND
      (INPUT FRAME {&frame-name} fi-func-exec6 <> 0 OR
       INPUT FRAME {&frame-name} fi-hora-iexec6 <> "" OR
       INPUT FRAME {&FRAME-NAME} fi-hora-texec6 <> "" OR
       INPUT FRAME {&FRAME-NAME} fi-min-parado6 <> 0)) OR
     (INPUT FRAME {&FRAME-NAME} fi-ativ-exec6 <> "" AND
      (INPUT FRAME {&FRAME-NAME} fi-func-exec6 = 0 OR
       INPUT FRAME {&FRAME-NAME} fi-hora-iexec6 = "" OR
       INPUT FRAME {&FRAME-NAME} fi-hora-texec6 = "")) THEN DO:
     MESSAGE "N∆o pode haver Atividade sem Funcion†rio e/ou In°cio e TÇrmino" VIEW-AS ALERT-BOX. 
     APPLY 'entry' TO fi-ativ-exec6.
     return 'ADM-ERROR':U.
  END.
  IF (INPUT FRAME {&frame-name} fi-ativ-exec7 = "" AND
      (INPUT FRAME {&frame-name} fi-func-exec7 <> 0 OR
       INPUT FRAME {&frame-name} fi-hora-iexec7 <> "" OR
       INPUT FRAME {&FRAME-NAME} fi-hora-texec7 <> "" OR
       INPUT FRAME {&FRAME-NAME} fi-min-parado7 <> 0)) OR
     (INPUT FRAME {&FRAME-NAME} fi-ativ-exec7 <> "" AND
      (INPUT FRAME {&FRAME-NAME} fi-func-exec7 = 0 OR
       INPUT FRAME {&FRAME-NAME} fi-hora-iexec7 = "" OR
       INPUT FRAME {&FRAME-NAME} fi-hora-texec7 = "")) THEN DO:
     MESSAGE "N∆o pode haver Atividade sem Funcion†rio e/ou In°cio e TÇrmino" VIEW-AS ALERT-BOX. 
     APPLY 'entry' TO fi-ativ-exec7.
     return 'ADM-ERROR':U.
  END.
  IF (INPUT FRAME {&frame-name} fi-ativ-exec8 = "" AND
      (INPUT FRAME {&frame-name} fi-func-exec8 <> 0 OR
       INPUT FRAME {&frame-name} fi-hora-iexec8 <> "" OR
       INPUT FRAME {&FRAME-NAME} fi-hora-texec8 <> "" OR
       INPUT FRAME {&FRAME-NAME} fi-min-parado8 <> 0)) OR
     (INPUT FRAME {&FRAME-NAME} fi-ativ-exec8 <> "" AND
      (INPUT FRAME {&FRAME-NAME} fi-func-exec8 = 0 OR
       INPUT FRAME {&FRAME-NAME} fi-hora-iexec8 = "" OR
       INPUT FRAME {&FRAME-NAME} fi-hora-texec8 = "")) THEN DO:
     MESSAGE "N∆o pode haver Atividade sem Funcion†rio e/ou In°cio e TÇrmino" VIEW-AS ALERT-BOX. 
     APPLY 'entry' TO fi-ativ-exec8.
     return 'ADM-ERROR':U.
  END.
  IF (INPUT FRAME {&frame-name} fi-ativ-exec9 = "" AND
      (INPUT FRAME {&frame-name} fi-func-exec9 <> 0 OR
       INPUT FRAME {&frame-name} fi-hora-iexec9 <> "" OR
       INPUT FRAME {&FRAME-NAME} fi-hora-texec9 <> "" OR
       INPUT FRAME {&FRAME-NAME} fi-min-parado9 <> 0)) OR
     (INPUT FRAME {&FRAME-NAME} fi-ativ-exec9 <> "" AND
      (INPUT FRAME {&FRAME-NAME} fi-func-exec9 = 0 OR
       INPUT FRAME {&FRAME-NAME} fi-hora-iexec9 = "" OR
       INPUT FRAME {&FRAME-NAME} fi-hora-texec9 = "")) THEN DO:
     MESSAGE "N∆o pode haver Atividade sem Funcion†rio e/ou In°cio e TÇrmino" VIEW-AS ALERT-BOX. 
     APPLY 'entry' TO fi-ativ-exec9.
     return 'ADM-ERROR':U.
  END.
  IF (INPUT FRAME {&frame-name} fi-ativ-exec10 = "" AND
      (INPUT FRAME {&frame-name} fi-func-exec10 <> 0 OR
       INPUT FRAME {&frame-name} fi-hora-iexec10 <> "" OR
       INPUT FRAME {&FRAME-NAME} fi-hora-texec10 <> "" OR
       INPUT FRAME {&FRAME-NAME} fi-min-parado10 <> 0)) OR
     (INPUT FRAME {&FRAME-NAME} fi-ativ-exec10 <> "" AND
      (INPUT FRAME {&FRAME-NAME} fi-func-exec10 = 0 OR
       INPUT FRAME {&FRAME-NAME} fi-hora-iexec10 = "" OR
       INPUT FRAME {&FRAME-NAME} fi-hora-texec10 = "")) THEN DO:
     MESSAGE "N∆o pode haver Atividade sem Funcion†rio e/ou In°cio e TÇrmino" VIEW-AS ALERT-BOX. 
     APPLY 'entry' TO fi-ativ-exec10.
     return 'ADM-ERROR':U.
  END.

  ASSIGN INPUT FRAME {&frame-name} fi-ativ-exec2
         INPUT FRAME {&frame-name} fi-ativ-exec3
         INPUT FRAME {&frame-name} fi-ativ-exec4
         INPUT FRAME {&frame-name} fi-ativ-exec5
         INPUT FRAME {&frame-name} fi-ativ-exec6
         INPUT FRAME {&frame-name} fi-ativ-exec7
         INPUT FRAME {&frame-name} fi-ativ-exec8
         INPUT FRAME {&frame-name} fi-ativ-exec9
         INPUT FRAME {&frame-name} fi-ativ-exec10
         INPUT FRAME {&frame-name} fi-func-exec1 
         INPUT FRAME {&frame-name} fi-func-exec2 
         INPUT FRAME {&frame-name} fi-func-exec3 
         INPUT FRAME {&frame-name} fi-func-exec4 
         INPUT FRAME {&frame-name} fi-func-exec5 
         INPUT FRAME {&frame-name} fi-func-exec6 
         INPUT FRAME {&frame-name} fi-func-exec7 
         INPUT FRAME {&frame-name} fi-func-exec8 
         INPUT FRAME {&frame-name} fi-func-exec9 
         INPUT FRAME {&frame-name} fi-func-exec10
         INPUT FRAME {&frame-name} fi-hora-iexec1 
         INPUT FRAME {&frame-name} fi-hora-iexec2 
         INPUT FRAME {&frame-name} fi-hora-iexec3 
         INPUT FRAME {&frame-name} fi-hora-iexec4 
         INPUT FRAME {&frame-name} fi-hora-iexec5 
         INPUT FRAME {&frame-name} fi-hora-iexec6 
         INPUT FRAME {&frame-name} fi-hora-iexec7 
         INPUT FRAME {&frame-name} fi-hora-iexec8 
         INPUT FRAME {&frame-name} fi-hora-iexec9 
         INPUT FRAME {&frame-name} fi-hora-iexec10
         INPUT FRAME {&frame-name} fi-hora-texec1 
         INPUT FRAME {&frame-name} fi-hora-texec2 
         INPUT FRAME {&frame-name} fi-hora-texec3 
         INPUT FRAME {&frame-name} fi-hora-texec4 
         INPUT FRAME {&frame-name} fi-hora-texec5 
         INPUT FRAME {&frame-name} fi-hora-texec6 
         INPUT FRAME {&frame-name} fi-hora-texec7 
         INPUT FRAME {&frame-name} fi-hora-texec8 
         INPUT FRAME {&frame-name} fi-hora-texec9 
         INPUT FRAME {&frame-name} fi-hora-texec10
         INPUT FRAME {&frame-name} fi-min-parado1 
         INPUT FRAME {&frame-name} fi-min-parado2 
         INPUT FRAME {&frame-name} fi-min-parado3 
         INPUT FRAME {&frame-name} fi-min-parado4 
         INPUT FRAME {&frame-name} fi-min-parado5 
         INPUT FRAME {&frame-name} fi-min-parado6 
         INPUT FRAME {&frame-name} fi-min-parado7 
         INPUT FRAME {&frame-name} fi-min-parado8 
         INPUT FRAME {&frame-name} fi-min-parado9 
         INPUT FRAME {&frame-name} fi-min-parado10.
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

