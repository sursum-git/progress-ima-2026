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
{include/i-prgvrs.i V01ES044 2.04.00.000}

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
DEF VAR i-cont       AS INT.
DEF VAR c-descricao  LIKE item-ext.des-gr-ref[1].

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
&Scoped-define EXTERNAL-TABLES cota-item
&Scoped-define FIRST-EXTERNAL-TABLE cota-item


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR cota-item.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS cota-item.item-adic1 cota-item.acab-adic1 ~
cota-item.item-adic2 cota-item.acab-adic2 cota-item.item-adic3 ~
cota-item.acab-adic3 cota-item.item-adic4 cota-item.acab-adic4 ~
cota-item.item-adic5 cota-item.acab-adic5 cota-item.item-adic6 ~
cota-item.acab-adic6 cota-item.item-adic7 cota-item.acab-adic7 ~
cota-item.item-adic8 cota-item.acab-adic8 cota-item.item-adic9 ~
cota-item.acab-adic9 cota-item.item-adic10 cota-item.acab-adic10 ~
cota-item.item-adic11 cota-item.acab-adic11 cota-item.item-adic12 ~
cota-item.acab-adic12 cota-item.item-adic13 cota-item.acab-adic13 ~
cota-item.item-adic14 cota-item.acab-adic14 cota-item.item-adic15 ~
cota-item.acab-adic15 cota-item.estoque cota-item.producao ~
cota-item.cart-mes-ant cota-item.dt-min-ped cota-item.cota1 cota-item.cota2 ~
cota-item.susp-cota1 cota-item.susp-cota2 
&Scoped-define ENABLED-TABLES cota-item
&Scoped-define FIRST-ENABLED-TABLE cota-item
&Scoped-Define ENABLED-OBJECTS rt-key rt-mold 
&Scoped-Define DISPLAYED-FIELDS cota-item.it-codigo cota-item.item-adic1 ~
cota-item.acab-adic1 cota-item.item-adic2 cota-item.acab-adic2 ~
cota-item.item-adic3 cota-item.acab-adic3 cota-item.item-adic4 ~
cota-item.acab-adic4 cota-item.item-adic5 cota-item.acab-adic5 ~
cota-item.item-adic6 cota-item.acab-adic6 cota-item.item-adic7 ~
cota-item.acab-adic7 cota-item.item-adic8 cota-item.acab-adic8 ~
cota-item.item-adic9 cota-item.acab-adic9 cota-item.item-adic10 ~
cota-item.acab-adic10 cota-item.item-adic11 cota-item.acab-adic11 ~
cota-item.item-adic12 cota-item.acab-adic12 cota-item.item-adic13 ~
cota-item.acab-adic13 cota-item.item-adic14 cota-item.acab-adic14 ~
cota-item.item-adic15 cota-item.acab-adic15 cota-item.estoque ~
cota-item.producao cota-item.cart-mes-ant cota-item.dt-min-ped ~
cota-item.cota1 cota-item.cota2 cota-item.susp-cota1 cota-item.susp-cota2 
&Scoped-define DISPLAYED-TABLES cota-item
&Scoped-define FIRST-DISPLAYED-TABLE cota-item
&Scoped-Define DISPLAYED-OBJECTS fi-desc-item fi-desc-item1 fi-desc-item2 ~
fi-desc-item3 fi-desc-item4 fi-desc-item5 fi-desc-item6 fi-desc-item7 ~
fi-desc-item8 fi-desc-item9 fi-desc-item10 fi-desc-item11 fi-desc-item12 ~
fi-desc-item13 fi-desc-item14 fi-desc-item15 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-CREATE-FIELDS cota-item.it-codigo 
&Scoped-define ADM-ASSIGN-FIELDS cota-item.it-codigo 

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
DEFINE VARIABLE fi-desc-item AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-item1 AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-item10 AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-item11 AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-item12 AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-item13 AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-item14 AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-item15 AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-item2 AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-item3 AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-item4 AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-item5 AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-item6 AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-item7 AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-item8 AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-item9 AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 57 BY .88 NO-UNDO.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 88.57 BY 1.25.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 88.57 BY 19.33.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     cota-item.it-codigo AT ROW 1.17 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY .88
     fi-desc-item AT ROW 1.17 COL 28.14 COLON-ALIGNED NO-LABEL
     cota-item.item-adic1 AT ROW 2.67 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
     cota-item.acab-adic1 AT ROW 2.67 COL 24.43 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4 BY .88
     fi-desc-item1 AT ROW 2.67 COL 29 COLON-ALIGNED NO-LABEL
     cota-item.item-adic2 AT ROW 3.67 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
     cota-item.acab-adic2 AT ROW 3.67 COL 24.43 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4 BY .88
     fi-desc-item2 AT ROW 3.67 COL 29 COLON-ALIGNED NO-LABEL
     cota-item.item-adic3 AT ROW 4.67 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
     cota-item.acab-adic3 AT ROW 4.67 COL 24.43 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4 BY .88
     fi-desc-item3 AT ROW 4.67 COL 29 COLON-ALIGNED NO-LABEL
     cota-item.item-adic4 AT ROW 5.67 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
     cota-item.acab-adic4 AT ROW 5.67 COL 24.43 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4 BY .88
     fi-desc-item4 AT ROW 5.67 COL 29 COLON-ALIGNED NO-LABEL
     cota-item.item-adic5 AT ROW 6.67 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
     cota-item.acab-adic5 AT ROW 6.67 COL 24.43 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4 BY .88
     fi-desc-item5 AT ROW 6.67 COL 29 COLON-ALIGNED NO-LABEL
     cota-item.item-adic6 AT ROW 7.67 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
     cota-item.acab-adic6 AT ROW 7.67 COL 24.43 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4 BY .88
     fi-desc-item6 AT ROW 7.67 COL 29 COLON-ALIGNED NO-LABEL
     cota-item.item-adic7 AT ROW 8.67 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
     cota-item.acab-adic7 AT ROW 8.67 COL 24.43 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4 BY .88
     fi-desc-item7 AT ROW 8.67 COL 29 COLON-ALIGNED NO-LABEL
     cota-item.item-adic8 AT ROW 9.67 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
     cota-item.acab-adic8 AT ROW 9.67 COL 24.43 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4 BY .88
     fi-desc-item8 AT ROW 9.67 COL 29 COLON-ALIGNED NO-LABEL
     cota-item.item-adic9 AT ROW 10.67 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
     cota-item.acab-adic9 AT ROW 10.67 COL 24.43 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4 BY .88
     fi-desc-item9 AT ROW 10.67 COL 29 COLON-ALIGNED NO-LABEL
     cota-item.item-adic10 AT ROW 11.67 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
     cota-item.acab-adic10 AT ROW 11.67 COL 24.43 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4 BY .88
     fi-desc-item10 AT ROW 11.67 COL 29 COLON-ALIGNED NO-LABEL
     cota-item.item-adic11 AT ROW 12.67 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 1.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME f-main
     cota-item.acab-adic11 AT ROW 12.67 COL 24.43 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4 BY .88
     fi-desc-item11 AT ROW 12.67 COL 29 COLON-ALIGNED NO-LABEL
     cota-item.item-adic12 AT ROW 13.67 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
     cota-item.acab-adic12 AT ROW 13.67 COL 24.43 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4 BY .88
     fi-desc-item12 AT ROW 13.67 COL 29 COLON-ALIGNED NO-LABEL
     cota-item.item-adic13 AT ROW 14.67 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
     cota-item.acab-adic13 AT ROW 14.67 COL 24.43 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4 BY .88
     fi-desc-item13 AT ROW 14.67 COL 29 COLON-ALIGNED NO-LABEL
     cota-item.item-adic14 AT ROW 15.67 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
     cota-item.acab-adic14 AT ROW 15.67 COL 24.43 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4 BY .88
     fi-desc-item14 AT ROW 15.67 COL 29 COLON-ALIGNED NO-LABEL
     cota-item.item-adic15 AT ROW 16.67 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
     cota-item.acab-adic15 AT ROW 16.67 COL 24.43 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4 BY .88
     fi-desc-item15 AT ROW 16.67 COL 29 COLON-ALIGNED NO-LABEL
     cota-item.estoque AT ROW 17.67 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.43 BY .88
     cota-item.producao AT ROW 18.67 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.43 BY .88
     cota-item.cart-mes-ant AT ROW 19.67 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.43 BY .88
     cota-item.dt-min-ped AT ROW 20.67 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.43 BY .88
     cota-item.cota1 AT ROW 19.67 COL 45.43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.57 BY .88
     cota-item.cota2 AT ROW 20.67 COL 45.43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.57 BY .88
     cota-item.susp-cota1 AT ROW 19.67 COL 62.29
          VIEW-AS TOGGLE-BOX
          SIZE 14.14 BY .88
     cota-item.susp-cota2 AT ROW 20.67 COL 62.29
          VIEW-AS TOGGLE-BOX
          SIZE 14.14 BY .88
     rt-key AT ROW 1 COL 1
     rt-mold AT ROW 2.42 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: espec.cota-item
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
         HEIGHT             = 20.83
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

/* SETTINGS FOR FILL-IN fi-desc-item IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-item1 IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-item10 IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-item11 IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-item12 IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-item13 IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-item14 IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-item15 IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-item2 IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-item3 IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-item4 IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-item5 IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-item6 IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-item7 IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-item8 IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-item9 IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cota-item.it-codigo IN FRAME f-main
   NO-ENABLE 1 2                                                        */
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

&Scoped-define SELF-NAME cota-item.acab-adic1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-item.acab-adic1 V-table-Win
ON LEAVE OF cota-item.acab-adic1 IN FRAME f-main /* AcAd01 */
DO:
   /*-- Verifica se o Item tem nome comercial diferenciado --*/
   IF INPUT FRAME {&FRAME-NAME} cota-item.acab-adic1 <> "" THEN DO:
      FIND item-ext WHERE item-ext.it-codigo = INPUT FRAME {&FRAME-NAME} cota-item.item-adic1 
                    NO-LOCK NO-ERROR.
      ASSIGN c-descricao = "".
      IF AVAIL item-ext THEN DO:
         DO i-cont = 1 TO 99:
            IF item-ext.cod-gr-ref[i-cont] = INPUT FRAME {&FRAME-NAME} cota-item.acab-adic1 THEN DO:
               ASSIGN c-descricao = item-ext.des-gr-ref[i-cont].
               LEAVE.
            END.
         END.
      END.
      IF c-descricao = "" THEN DO:
         MESSAGE "N∆o h† informaá∆o de Nome Comercial para esse Acabamento." VIEW-AS ALERT-BOX.
         APPLY 'entry' TO cota-item.acab-adic1.
         RETURN NO-APPLY.
      END.
      ELSE
         ASSIGN fi-desc-item1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = c-descricao.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cota-item.acab-adic10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-item.acab-adic10 V-table-Win
ON LEAVE OF cota-item.acab-adic10 IN FRAME f-main /* AcAd10 */
DO:
   /*-- Verifica se o Item tem nome comercial diferenciado --*/
   IF INPUT FRAME {&FRAME-NAME} cota-item.acab-adic10 <> "" THEN DO:
      FIND item-ext WHERE item-ext.it-codigo = INPUT FRAME {&FRAME-NAME} cota-item.item-adic10 
                    NO-LOCK NO-ERROR.
      ASSIGN c-descricao = "".
      IF AVAIL item-ext THEN DO:
         DO i-cont = 1 TO 99:
            IF item-ext.cod-gr-ref[i-cont] = INPUT FRAME {&FRAME-NAME} cota-item.acab-adic10 THEN DO:
               ASSIGN c-descricao = item-ext.des-gr-ref[i-cont].
               LEAVE.
            END.
         END.
      END.
      IF c-descricao = "" THEN DO:
         MESSAGE "N∆o h† informaá∆o de Nome Comercial para esse Acabamento." VIEW-AS ALERT-BOX.
         APPLY 'entry' TO cota-item.acab-adic10.
         RETURN NO-APPLY.
      END.
      ELSE
         ASSIGN fi-desc-item10:SCREEN-VALUE IN FRAME {&FRAME-NAME} = c-descricao.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cota-item.acab-adic11
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-item.acab-adic11 V-table-Win
ON LEAVE OF cota-item.acab-adic11 IN FRAME f-main /* AcAd11 */
DO:
   /*-- Verifica se o Item tem nome comercial diferenciado --*/
   IF INPUT FRAME {&FRAME-NAME} cota-item.acab-adic11 <> "" THEN DO:
      FIND item-ext WHERE item-ext.it-codigo = INPUT FRAME {&FRAME-NAME} cota-item.item-adic11 
                    NO-LOCK NO-ERROR.
      ASSIGN c-descricao = "".
      IF AVAIL item-ext THEN DO:
         DO i-cont = 1 TO 99:
            IF item-ext.cod-gr-ref[i-cont] = INPUT FRAME {&FRAME-NAME} cota-item.acab-adic11 THEN DO:
               ASSIGN c-descricao = item-ext.des-gr-ref[i-cont].
               LEAVE.
            END.
         END.
      END.
      IF c-descricao = "" THEN DO:
         MESSAGE "N∆o h† informaá∆o de Nome Comercial para esse Acabamento." VIEW-AS ALERT-BOX.
         APPLY 'entry' TO cota-item.acab-adic11.
         RETURN NO-APPLY.
      END.
      ELSE
         ASSIGN fi-desc-item11:SCREEN-VALUE IN FRAME {&FRAME-NAME} = c-descricao.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cota-item.acab-adic12
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-item.acab-adic12 V-table-Win
ON LEAVE OF cota-item.acab-adic12 IN FRAME f-main /* AcAd12 */
DO:
   /*-- Verifica se o Item tem nome comercial diferenciado --*/
   IF INPUT FRAME {&FRAME-NAME} cota-item.acab-adic12 <> "" THEN DO:
      FIND item-ext WHERE item-ext.it-codigo = INPUT FRAME {&FRAME-NAME} cota-item.item-adic12 
                    NO-LOCK NO-ERROR.
      ASSIGN c-descricao = "".
      IF AVAIL item-ext THEN DO:
         DO i-cont = 1 TO 99:
            IF item-ext.cod-gr-ref[i-cont] = INPUT FRAME {&FRAME-NAME} cota-item.acab-adic12 THEN DO:
               ASSIGN c-descricao = item-ext.des-gr-ref[i-cont].
               LEAVE.
            END.
         END.
      END.
      IF c-descricao = "" THEN DO:
         MESSAGE "N∆o h† informaá∆o de Nome Comercial para esse Acabamento." VIEW-AS ALERT-BOX.
         APPLY 'entry' TO cota-item.acab-adic10.
         RETURN NO-APPLY.
      END.
      ELSE
         ASSIGN fi-desc-item12:SCREEN-VALUE IN FRAME {&FRAME-NAME} = c-descricao.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cota-item.acab-adic13
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-item.acab-adic13 V-table-Win
ON LEAVE OF cota-item.acab-adic13 IN FRAME f-main /* AcAd13 */
DO:
   /*-- Verifica se o Item tem nome comercial diferenciado --*/
   IF INPUT FRAME {&FRAME-NAME} cota-item.acab-adic13 <> "" THEN DO:
      FIND item-ext WHERE item-ext.it-codigo = INPUT FRAME {&FRAME-NAME} cota-item.item-adic13 
                    NO-LOCK NO-ERROR.
      ASSIGN c-descricao = "".
      IF AVAIL item-ext THEN DO:
         DO i-cont = 1 TO 99:
            IF item-ext.cod-gr-ref[i-cont] = INPUT FRAME {&FRAME-NAME} cota-item.acab-adic13 THEN DO:
               ASSIGN c-descricao = item-ext.des-gr-ref[i-cont].
               LEAVE.
            END.
         END.
      END.
      IF c-descricao = "" THEN DO:
         MESSAGE "N∆o h† informaá∆o de Nome Comercial para esse Acabamento." VIEW-AS ALERT-BOX.
         APPLY 'entry' TO cota-item.acab-adic13.
         RETURN NO-APPLY.
      END.
      ELSE
         ASSIGN fi-desc-item13:SCREEN-VALUE IN FRAME {&FRAME-NAME} = c-descricao.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cota-item.acab-adic14
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-item.acab-adic14 V-table-Win
ON LEAVE OF cota-item.acab-adic14 IN FRAME f-main /* AcAd14 */
DO:
   /*-- Verifica se o Item tem nome comercial diferenciado --*/
   IF INPUT FRAME {&FRAME-NAME} cota-item.acab-adic14 <> "" THEN DO:
      FIND item-ext WHERE item-ext.it-codigo = INPUT FRAME {&FRAME-NAME} cota-item.item-adic14 
                    NO-LOCK NO-ERROR.
      ASSIGN c-descricao = "".
      IF AVAIL item-ext THEN DO:
         DO i-cont = 1 TO 99:
            IF item-ext.cod-gr-ref[i-cont] = INPUT FRAME {&FRAME-NAME} cota-item.acab-adic14 THEN DO:
               ASSIGN c-descricao = item-ext.des-gr-ref[i-cont].
               LEAVE.
            END.
         END.
      END.
      IF c-descricao = "" THEN DO:
         MESSAGE "N∆o h† informaá∆o de Nome Comercial para esse Acabamento." VIEW-AS ALERT-BOX.
         APPLY 'entry' TO cota-item.acab-adic14.
         RETURN NO-APPLY.
      END.
      ELSE
         ASSIGN fi-desc-item14:SCREEN-VALUE IN FRAME {&FRAME-NAME} = c-descricao.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cota-item.acab-adic15
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-item.acab-adic15 V-table-Win
ON LEAVE OF cota-item.acab-adic15 IN FRAME f-main /* AcAd15 */
DO:
   /*-- Verifica se o Item tem nome comercial diferenciado --*/
   IF INPUT FRAME {&FRAME-NAME} cota-item.acab-adic15 <> "" THEN DO:
      FIND item-ext WHERE item-ext.it-codigo = INPUT FRAME {&FRAME-NAME} cota-item.item-adic15 
                    NO-LOCK NO-ERROR.
      ASSIGN c-descricao = "".
      IF AVAIL item-ext THEN DO:
         DO i-cont = 1 TO 99:
            IF item-ext.cod-gr-ref[i-cont] = INPUT FRAME {&FRAME-NAME} cota-item.acab-adic15 THEN DO:
               ASSIGN c-descricao = item-ext.des-gr-ref[i-cont].
               LEAVE.
            END.
         END.
      END.
      IF c-descricao = "" THEN DO:
         MESSAGE "N∆o h† informaá∆o de Nome Comercial para esse Acabamento." VIEW-AS ALERT-BOX.
         APPLY 'entry' TO cota-item.acab-adic15.
         RETURN NO-APPLY.
      END.
      ELSE
         ASSIGN fi-desc-item15:SCREEN-VALUE IN FRAME {&FRAME-NAME} = c-descricao.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cota-item.acab-adic2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-item.acab-adic2 V-table-Win
ON LEAVE OF cota-item.acab-adic2 IN FRAME f-main /* AcAd02 */
DO:
   /*-- Verifica se o Item tem nome comercial diferenciado --*/
   IF INPUT FRAME {&FRAME-NAME} cota-item.acab-adic2 <> "" THEN DO:
      FIND item-ext WHERE item-ext.it-codigo = INPUT FRAME {&FRAME-NAME} cota-item.item-adic2 
                    NO-LOCK NO-ERROR.
      ASSIGN c-descricao = "".
      IF AVAIL item-ext THEN DO:
         DO i-cont = 1 TO 99:
            IF item-ext.cod-gr-ref[i-cont] = INPUT FRAME {&FRAME-NAME} cota-item.acab-adic2 THEN DO:
               ASSIGN c-descricao = item-ext.des-gr-ref[i-cont].
               LEAVE.
            END.
         END.
      END.
      IF c-descricao = "" THEN DO:
         MESSAGE "N∆o h† informaá∆o de Nome Comercial para esse Acabamento." VIEW-AS ALERT-BOX.
         APPLY 'entry' TO cota-item.acab-adic2.
         RETURN NO-APPLY.
      END.
      ELSE
         ASSIGN fi-desc-item2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = c-descricao.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cota-item.acab-adic3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-item.acab-adic3 V-table-Win
ON LEAVE OF cota-item.acab-adic3 IN FRAME f-main /* AcAd03 */
DO:
   /*-- Verifica se o Item tem nome comercial diferenciado --*/
   IF INPUT FRAME {&FRAME-NAME} cota-item.acab-adic3 <> "" THEN DO:
      FIND item-ext WHERE item-ext.it-codigo = INPUT FRAME {&FRAME-NAME} cota-item.item-adic3 
                    NO-LOCK NO-ERROR.
      ASSIGN c-descricao = "".
      IF AVAIL item-ext THEN DO:
         DO i-cont = 1 TO 99:
            IF item-ext.cod-gr-ref[i-cont] = INPUT FRAME {&FRAME-NAME} cota-item.acab-adic3 THEN DO:
               ASSIGN c-descricao = item-ext.des-gr-ref[i-cont].
               LEAVE.
            END.
         END.
      END.
      IF c-descricao = "" THEN DO:
         MESSAGE "N∆o h† informaá∆o de Nome Comercial para esse Acabamento." VIEW-AS ALERT-BOX.
         APPLY 'entry' TO cota-item.acab-adic1.
         RETURN NO-APPLY.
      END.
      ELSE
         ASSIGN fi-desc-item3:SCREEN-VALUE IN FRAME {&FRAME-NAME} = c-descricao.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cota-item.acab-adic4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-item.acab-adic4 V-table-Win
ON LEAVE OF cota-item.acab-adic4 IN FRAME f-main /* AcAd04 */
DO:
   /*-- Verifica se o Item tem nome comercial diferenciado --*/
   IF INPUT FRAME {&FRAME-NAME} cota-item.acab-adic4 <> "" THEN DO:
      FIND item-ext WHERE item-ext.it-codigo = INPUT FRAME {&FRAME-NAME} cota-item.item-adic4 
                    NO-LOCK NO-ERROR.
      ASSIGN c-descricao = "".
      IF AVAIL item-ext THEN DO:
         DO i-cont = 1 TO 99:
            IF item-ext.cod-gr-ref[i-cont] = INPUT FRAME {&FRAME-NAME} cota-item.acab-adic4 THEN DO:
               ASSIGN c-descricao = item-ext.des-gr-ref[i-cont].
               LEAVE.
            END.
         END.
      END.
      IF c-descricao = "" THEN DO:
         MESSAGE "N∆o h† informaá∆o de Nome Comercial para esse Acabamento." VIEW-AS ALERT-BOX.
         APPLY 'entry' TO cota-item.acab-adic4.
         RETURN NO-APPLY.
      END.
      ELSE
         ASSIGN fi-desc-item4:SCREEN-VALUE IN FRAME {&FRAME-NAME} = c-descricao.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cota-item.acab-adic5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-item.acab-adic5 V-table-Win
ON LEAVE OF cota-item.acab-adic5 IN FRAME f-main /* AcAd05 */
DO:
   /*-- Verifica se o Item tem nome comercial diferenciado --*/
   IF INPUT FRAME {&FRAME-NAME} cota-item.acab-adic5 <> "" THEN DO:
      FIND item-ext WHERE item-ext.it-codigo = INPUT FRAME {&FRAME-NAME} cota-item.item-adic5 
                    NO-LOCK NO-ERROR.
      ASSIGN c-descricao = "".
      IF AVAIL item-ext THEN DO:
         DO i-cont = 1 TO 99:
            IF item-ext.cod-gr-ref[i-cont] = INPUT FRAME {&FRAME-NAME} cota-item.acab-adic5 THEN DO:
               ASSIGN c-descricao = item-ext.des-gr-ref[i-cont].
               LEAVE.
            END.
         END.
      END.
      IF c-descricao = "" THEN DO:
         MESSAGE "N∆o h† informaá∆o de Nome Comercial para esse Acabamento." VIEW-AS ALERT-BOX.
         APPLY 'entry' TO cota-item.acab-adic5.
         RETURN NO-APPLY.
      END.
      ELSE
         ASSIGN fi-desc-item5:SCREEN-VALUE IN FRAME {&FRAME-NAME} = c-descricao.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cota-item.acab-adic6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-item.acab-adic6 V-table-Win
ON LEAVE OF cota-item.acab-adic6 IN FRAME f-main /* AcAd06 */
DO:
   /*-- Verifica se o Item tem nome comercial diferenciado --*/
   IF INPUT FRAME {&FRAME-NAME} cota-item.acab-adic6 <> "" THEN DO:
      FIND item-ext WHERE item-ext.it-codigo = INPUT FRAME {&FRAME-NAME} cota-item.item-adic6 
                    NO-LOCK NO-ERROR.
      ASSIGN c-descricao = "".
      IF AVAIL item-ext THEN DO:
         DO i-cont = 1 TO 99:
            IF item-ext.cod-gr-ref[i-cont] = INPUT FRAME {&FRAME-NAME} cota-item.acab-adic6 THEN DO:
               ASSIGN c-descricao = item-ext.des-gr-ref[i-cont].
               LEAVE.
            END.
         END.
      END.
      IF c-descricao = "" THEN DO:
         MESSAGE "N∆o h† informaá∆o de Nome Comercial para esse Acabamento." VIEW-AS ALERT-BOX.
         APPLY 'entry' TO cota-item.acab-adic6.
         RETURN NO-APPLY.
      END.
      ELSE
         ASSIGN fi-desc-item6:SCREEN-VALUE IN FRAME {&FRAME-NAME} = c-descricao.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cota-item.acab-adic7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-item.acab-adic7 V-table-Win
ON LEAVE OF cota-item.acab-adic7 IN FRAME f-main /* AcAd07 */
DO:
   /*-- Verifica se o Item tem nome comercial diferenciado --*/
   IF INPUT FRAME {&FRAME-NAME} cota-item.acab-adic7 <> "" THEN DO:
      FIND item-ext WHERE item-ext.it-codigo = INPUT FRAME {&FRAME-NAME} cota-item.item-adic7 
                    NO-LOCK NO-ERROR.
      ASSIGN c-descricao = "".
      IF AVAIL item-ext THEN DO:
         DO i-cont = 1 TO 99:
            IF item-ext.cod-gr-ref[i-cont] = INPUT FRAME {&FRAME-NAME} cota-item.acab-adic7 THEN DO:
               ASSIGN c-descricao = item-ext.des-gr-ref[i-cont].
               LEAVE.
            END.
         END.
      END.
      IF c-descricao = "" THEN DO:
         MESSAGE "N∆o h† informaá∆o de Nome Comercial para esse Acabamento." VIEW-AS ALERT-BOX.
         APPLY 'entry' TO cota-item.acab-adic7.
         RETURN NO-APPLY.
      END.
      ELSE
         ASSIGN fi-desc-item7:SCREEN-VALUE IN FRAME {&FRAME-NAME} = c-descricao.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cota-item.acab-adic8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-item.acab-adic8 V-table-Win
ON LEAVE OF cota-item.acab-adic8 IN FRAME f-main /* AcAd08 */
DO:
   /*-- Verifica se o Item tem nome comercial diferenciado --*/
   IF INPUT FRAME {&FRAME-NAME} cota-item.acab-adic8 <> "" THEN DO:
      FIND item-ext WHERE item-ext.it-codigo = INPUT FRAME {&FRAME-NAME} cota-item.item-adic8 
                    NO-LOCK NO-ERROR.
      ASSIGN c-descricao = "".
      IF AVAIL item-ext THEN DO:
         DO i-cont = 1 TO 99:
            IF item-ext.cod-gr-ref[i-cont] = INPUT FRAME {&FRAME-NAME} cota-item.acab-adic8 THEN DO:
               ASSIGN c-descricao = item-ext.des-gr-ref[i-cont].
               LEAVE.
            END.
         END.
      END.
      IF c-descricao = "" THEN DO:
         MESSAGE "N∆o h† informaá∆o de Nome Comercial para esse Acabamento." VIEW-AS ALERT-BOX.
         APPLY 'entry' TO cota-item.acab-adic8.
         RETURN NO-APPLY.
      END.
      ELSE
         ASSIGN fi-desc-item8:SCREEN-VALUE IN FRAME {&FRAME-NAME} = c-descricao.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cota-item.acab-adic9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-item.acab-adic9 V-table-Win
ON LEAVE OF cota-item.acab-adic9 IN FRAME f-main /* AcAd09 */
DO:
   /*-- Verifica se o Item tem nome comercial diferenciado --*/
   IF INPUT FRAME {&FRAME-NAME} cota-item.acab-adic9 <> "" THEN DO:
      FIND item-ext WHERE item-ext.it-codigo = INPUT FRAME {&FRAME-NAME} cota-item.item-adic9 
                    NO-LOCK NO-ERROR.
      ASSIGN c-descricao = "".
      IF AVAIL item-ext THEN DO:
         DO i-cont = 1 TO 99:
            IF item-ext.cod-gr-ref[i-cont] = INPUT FRAME {&FRAME-NAME} cota-item.acab-adic9 THEN DO:
               ASSIGN c-descricao = item-ext.des-gr-ref[i-cont].
               LEAVE.
            END.
         END.
      END.
      IF c-descricao = "" THEN DO:
         MESSAGE "N∆o h† informaá∆o de Nome Comercial para esse Acabamento." VIEW-AS ALERT-BOX.
         APPLY 'entry' TO cota-item.acab-adic9.
         RETURN NO-APPLY.
      END.
      ELSE
         ASSIGN fi-desc-item9:SCREEN-VALUE IN FRAME {&FRAME-NAME} = c-descricao.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cota-item.it-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-item.it-codigo V-table-Win
ON ENTRY OF cota-item.it-codigo IN FRAME f-main /* Item */
DO:
  FIND item WHERE item.it-codigo = INPUT FRAME {&FRAME-NAME} cota-item.it-codigo NO-LOCK NO-ERROR.
  IF AVAIL item THEN
     ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-item.it-codigo V-table-Win
ON LEAVE OF cota-item.it-codigo IN FRAME f-main /* Item */
DO:
  IF INPUT FRAME {&FRAME-NAME} cota-item.it-codigo <> "" THEN DO:
     FIND item WHERE item.it-codigo = INPUT FRAME {&FRAME-NAME} cota-item.it-codigo
                  NO-LOCK NO-ERROR.
     IF AVAIL item THEN
        ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
     FIND cota-item WHERE cota-item.it-codigo = INPUT FRAME {&FRAME-NAME} cota-item.it-codigo
                   NO-LOCK NO-ERROR.
     IF AVAIL cota-item THEN DO:
        MESSAGE "J† existe um registro de Cota para esse ÷tem. Use Opá∆o de alteraá∆o." 
                VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
     END.
  END.
  ELSE DO:
     MESSAGE "÷tem inv†lido." VIEW-AS ALERT-BOX.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-item.it-codigo V-table-Win
ON MOUSE-SELECT-DBLCLICK OF cota-item.it-codigo IN FRAME f-main /* Item */
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z01in172.w
                     &campo     = cota-item.it-codigo
                     &campozoom = it-codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cota-item.item-adic1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-item.item-adic1 V-table-Win
ON LEAVE OF cota-item.item-adic1 IN FRAME f-main /* Item Adic.1 */
DO:
  IF INPUT FRAME {&FRAME-NAME} cota-item.item-adic1 <> "" THEN DO:
     FIND item WHERE item.it-codigo = INPUT FRAME {&FRAME-NAME} cota-item.item-adic1
               NO-LOCK NO-ERROR.
     IF AVAIL item THEN
        ASSIGN fi-desc-item1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
     ELSE DO:
        MESSAGE "Item n∆o cadastrado." VIEW-AS ALERT-BOX.
        ASSIGN fi-desc-item1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "". 
        APPLY 'entry' TO cota-item.item-adic1.
        RETURN NO-APPLY.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-item.item-adic1 V-table-Win
ON MOUSE-SELECT-DBLCLICK OF cota-item.item-adic1 IN FRAME f-main /* Item Adic.1 */
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z01in172.w
                     &campo     = cota-item.item-adic1
                     &campozoom = it-codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cota-item.item-adic10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-item.item-adic10 V-table-Win
ON LEAVE OF cota-item.item-adic10 IN FRAME f-main /* Item Adic.10 */
DO:
  IF INPUT FRAME {&FRAME-NAME} cota-item.item-adic10 <> "" THEN DO:
     FIND item WHERE item.it-codigo = INPUT FRAME {&FRAME-NAME} cota-item.item-adic10
               NO-LOCK NO-ERROR.
     IF AVAIL item THEN
        ASSIGN fi-desc-item10:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
     ELSE DO:
        MESSAGE "Item n∆o cadastrado." VIEW-AS ALERT-BOX.
        ASSIGN fi-desc-item10:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "". 
        APPLY 'entry' TO cota-item.item-adic10.
        RETURN NO-APPLY.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-item.item-adic10 V-table-Win
ON MOUSE-SELECT-DBLCLICK OF cota-item.item-adic10 IN FRAME f-main /* Item Adic.10 */
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z01in172.w
                     &campo     = cota-item.item-adic10
                     &campozoom = it-codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cota-item.item-adic11
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-item.item-adic11 V-table-Win
ON LEAVE OF cota-item.item-adic11 IN FRAME f-main /* Item Adic.11 */
DO:
  IF INPUT FRAME {&FRAME-NAME} cota-item.item-adic11 <> "" THEN DO:
     FIND item WHERE item.it-codigo = INPUT FRAME {&FRAME-NAME} cota-item.item-adic11
               NO-LOCK NO-ERROR.
     IF AVAIL item THEN
        ASSIGN fi-desc-item11:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
     ELSE DO:
        MESSAGE "Item n∆o cadastrado." VIEW-AS ALERT-BOX.
        ASSIGN fi-desc-item11:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "". 
        APPLY 'entry' TO cota-item.item-adic11.
        RETURN NO-APPLY.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-item.item-adic11 V-table-Win
ON MOUSE-SELECT-DBLCLICK OF cota-item.item-adic11 IN FRAME f-main /* Item Adic.11 */
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z01in172.w
                     &campo     = cota-item.item-adic11
                     &campozoom = it-codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cota-item.item-adic12
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-item.item-adic12 V-table-Win
ON LEAVE OF cota-item.item-adic12 IN FRAME f-main /* Item Adic.12 */
DO:
  IF INPUT FRAME {&FRAME-NAME} cota-item.item-adic12 <> "" THEN DO:
     FIND item WHERE item.it-codigo = INPUT FRAME {&FRAME-NAME} cota-item.item-adic12
               NO-LOCK NO-ERROR.
     IF AVAIL item THEN
        ASSIGN fi-desc-item12:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
     ELSE DO:
        MESSAGE "Item n∆o cadastrado." VIEW-AS ALERT-BOX.
        ASSIGN fi-desc-item12:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "". 
        APPLY 'entry' TO cota-item.item-adic12.
        RETURN NO-APPLY.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-item.item-adic12 V-table-Win
ON MOUSE-SELECT-DBLCLICK OF cota-item.item-adic12 IN FRAME f-main /* Item Adic.12 */
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z01in172.w
                     &campo     = cota-item.item-adic12
                     &campozoom = it-codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cota-item.item-adic13
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-item.item-adic13 V-table-Win
ON LEAVE OF cota-item.item-adic13 IN FRAME f-main /* Item Adic.13 */
DO:
  IF INPUT FRAME {&FRAME-NAME} cota-item.item-adic13 <> "" THEN DO:
     FIND item WHERE item.it-codigo = INPUT FRAME {&FRAME-NAME} cota-item.item-adic13
               NO-LOCK NO-ERROR.
     IF AVAIL item THEN
        ASSIGN fi-desc-item13:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
     ELSE DO:
        MESSAGE "Item n∆o cadastrado." VIEW-AS ALERT-BOX.
        ASSIGN fi-desc-item13:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "". 
        APPLY 'entry' TO cota-item.item-adic13.
        RETURN NO-APPLY.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-item.item-adic13 V-table-Win
ON MOUSE-SELECT-DBLCLICK OF cota-item.item-adic13 IN FRAME f-main /* Item Adic.13 */
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z01in172.w
                     &campo     = cota-item.item-adic13
                     &campozoom = it-codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cota-item.item-adic14
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-item.item-adic14 V-table-Win
ON LEAVE OF cota-item.item-adic14 IN FRAME f-main /* Item Adic.14 */
DO:
  IF INPUT FRAME {&FRAME-NAME} cota-item.item-adic14 <> "" THEN DO:
     FIND item WHERE item.it-codigo = INPUT FRAME {&FRAME-NAME} cota-item.item-adic14
               NO-LOCK NO-ERROR.
     IF AVAIL item THEN
        ASSIGN fi-desc-item14:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
     ELSE DO:
        MESSAGE "Item n∆o cadastrado." VIEW-AS ALERT-BOX.
        ASSIGN fi-desc-item14:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "". 
        APPLY 'entry' TO cota-item.item-adic14.
        RETURN NO-APPLY.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-item.item-adic14 V-table-Win
ON MOUSE-SELECT-DBLCLICK OF cota-item.item-adic14 IN FRAME f-main /* Item Adic.14 */
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z01in172.w
                     &campo     = cota-item.item-adic14
                     &campozoom = it-codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cota-item.item-adic15
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-item.item-adic15 V-table-Win
ON LEAVE OF cota-item.item-adic15 IN FRAME f-main /* Item Adic.15 */
DO:
  IF INPUT FRAME {&FRAME-NAME} cota-item.item-adic15 <> "" THEN DO:
     FIND item WHERE item.it-codigo = INPUT FRAME {&FRAME-NAME} cota-item.item-adic15
               NO-LOCK NO-ERROR.
     IF AVAIL item THEN
        ASSIGN fi-desc-item15:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
     ELSE DO:
        MESSAGE "Item n∆o cadastrado." VIEW-AS ALERT-BOX.
        ASSIGN fi-desc-item15:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "". 
        APPLY 'entry' TO cota-item.item-adic15.
        RETURN NO-APPLY.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-item.item-adic15 V-table-Win
ON MOUSE-SELECT-DBLCLICK OF cota-item.item-adic15 IN FRAME f-main /* Item Adic.15 */
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z01in172.w
                     &campo     = cota-item.item-adic15
                     &campozoom = it-codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cota-item.item-adic2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-item.item-adic2 V-table-Win
ON LEAVE OF cota-item.item-adic2 IN FRAME f-main /* Item Adic.2 */
DO:
  IF INPUT FRAME {&FRAME-NAME} cota-item.item-adic2 <> "" THEN DO:
     FIND item WHERE item.it-codigo = INPUT FRAME {&FRAME-NAME} cota-item.item-adic2
               NO-LOCK NO-ERROR.
     IF AVAIL item THEN
        ASSIGN fi-desc-item2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
     ELSE DO:
        MESSAGE "Item n∆o cadastrado." VIEW-AS ALERT-BOX.
        ASSIGN fi-desc-item2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "". 
        APPLY 'entry' TO cota-item.item-adic2.
        RETURN NO-APPLY.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-item.item-adic2 V-table-Win
ON MOUSE-SELECT-DBLCLICK OF cota-item.item-adic2 IN FRAME f-main /* Item Adic.2 */
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z01in172.w
                     &campo     = cota-item.item-adic2
                     &campozoom = it-codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cota-item.item-adic3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-item.item-adic3 V-table-Win
ON LEAVE OF cota-item.item-adic3 IN FRAME f-main /* Item Adic.3 */
DO:
  IF INPUT FRAME {&FRAME-NAME} cota-item.item-adic3 <> "" THEN DO:
     FIND item WHERE item.it-codigo = INPUT FRAME {&FRAME-NAME} cota-item.item-adic3
               NO-LOCK NO-ERROR.
     IF AVAIL item THEN
        ASSIGN fi-desc-item3:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
     ELSE DO:
        MESSAGE "Item n∆o cadastrado." VIEW-AS ALERT-BOX.
        ASSIGN fi-desc-item3:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "". 
        APPLY 'entry' TO cota-item.item-adic3.
        RETURN NO-APPLY.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-item.item-adic3 V-table-Win
ON MOUSE-SELECT-DBLCLICK OF cota-item.item-adic3 IN FRAME f-main /* Item Adic.3 */
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z01in172.w
                     &campo     = cota-item.item-adic3
                     &campozoom = it-codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cota-item.item-adic4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-item.item-adic4 V-table-Win
ON LEAVE OF cota-item.item-adic4 IN FRAME f-main /* Item Adic.4 */
DO:
  IF INPUT FRAME {&FRAME-NAME} cota-item.item-adic4 <> "" THEN DO:
     FIND item WHERE item.it-codigo = INPUT FRAME {&FRAME-NAME} cota-item.item-adic4
               NO-LOCK NO-ERROR.
     IF AVAIL item THEN
        ASSIGN fi-desc-item4:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
     ELSE DO:
        MESSAGE "Item n∆o cadastrado." VIEW-AS ALERT-BOX.
        ASSIGN fi-desc-item4:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "". 
        APPLY 'entry' TO cota-item.item-adic4.
        RETURN NO-APPLY.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-item.item-adic4 V-table-Win
ON MOUSE-SELECT-DBLCLICK OF cota-item.item-adic4 IN FRAME f-main /* Item Adic.4 */
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z01in172.w
                     &campo     = cota-item.item-adic4
                     &campozoom = it-codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cota-item.item-adic5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-item.item-adic5 V-table-Win
ON LEAVE OF cota-item.item-adic5 IN FRAME f-main /* Item Adic.5 */
DO:
  IF INPUT FRAME {&FRAME-NAME} cota-item.item-adic5 <> "" THEN DO:
     FIND item WHERE item.it-codigo = INPUT FRAME {&FRAME-NAME} cota-item.item-adic5
               NO-LOCK NO-ERROR.
     IF AVAIL item THEN
        ASSIGN fi-desc-item5:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
     ELSE DO:
        MESSAGE "Item n∆o cadastrado." VIEW-AS ALERT-BOX.
        ASSIGN fi-desc-item5:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "". 
        APPLY 'entry' TO cota-item.item-adic5.
        RETURN NO-APPLY.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-item.item-adic5 V-table-Win
ON MOUSE-SELECT-DBLCLICK OF cota-item.item-adic5 IN FRAME f-main /* Item Adic.5 */
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z01in172.w
                     &campo     = cota-item.item-adic5
                     &campozoom = it-codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cota-item.item-adic6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-item.item-adic6 V-table-Win
ON LEAVE OF cota-item.item-adic6 IN FRAME f-main /* Item Adic.6 */
DO:
  IF INPUT FRAME {&FRAME-NAME} cota-item.item-adic6 <> "" THEN DO:
     FIND item WHERE item.it-codigo = INPUT FRAME {&FRAME-NAME} cota-item.item-adic6
               NO-LOCK NO-ERROR.
     IF AVAIL item THEN
        ASSIGN fi-desc-item6:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
     ELSE DO:
        MESSAGE "Item n∆o cadastrado." VIEW-AS ALERT-BOX.
        ASSIGN fi-desc-item6:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "". 
        APPLY 'entry' TO cota-item.item-adic6.
        RETURN NO-APPLY.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-item.item-adic6 V-table-Win
ON MOUSE-SELECT-DBLCLICK OF cota-item.item-adic6 IN FRAME f-main /* Item Adic.6 */
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z01in172.w
                     &campo     = cota-item.item-adic6
                     &campozoom = it-codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cota-item.item-adic7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-item.item-adic7 V-table-Win
ON LEAVE OF cota-item.item-adic7 IN FRAME f-main /* Item Adic.7 */
DO:
  IF INPUT FRAME {&FRAME-NAME} cota-item.item-adic7 <> "" THEN DO:
     FIND item WHERE item.it-codigo = INPUT FRAME {&FRAME-NAME} cota-item.item-adic7
               NO-LOCK NO-ERROR.
     IF AVAIL item THEN
        ASSIGN fi-desc-item7:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
     ELSE DO:
        MESSAGE "Item n∆o cadastrado." VIEW-AS ALERT-BOX.
        ASSIGN fi-desc-item7:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "". 
        APPLY 'entry' TO cota-item.item-adic7.
        RETURN NO-APPLY.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-item.item-adic7 V-table-Win
ON MOUSE-SELECT-DBLCLICK OF cota-item.item-adic7 IN FRAME f-main /* Item Adic.7 */
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z01in172.w
                     &campo     = cota-item.item-adic7
                     &campozoom = it-codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cota-item.item-adic8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-item.item-adic8 V-table-Win
ON LEAVE OF cota-item.item-adic8 IN FRAME f-main /* Item Adic.8 */
DO:
  IF INPUT FRAME {&FRAME-NAME} cota-item.item-adic8 <> "" THEN DO:
     FIND item WHERE item.it-codigo = INPUT FRAME {&FRAME-NAME} cota-item.item-adic8
               NO-LOCK NO-ERROR.
     IF AVAIL item THEN
        ASSIGN fi-desc-item8:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
     ELSE DO:
        MESSAGE "Item n∆o cadastrado." VIEW-AS ALERT-BOX.
        ASSIGN fi-desc-item8:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "". 
        APPLY 'entry' TO cota-item.item-adic8.
        RETURN NO-APPLY.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-item.item-adic8 V-table-Win
ON MOUSE-SELECT-DBLCLICK OF cota-item.item-adic8 IN FRAME f-main /* Item Adic.8 */
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z01in172.w
                     &campo     = cota-item.item-adic8
                     &campozoom = it-codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cota-item.item-adic9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-item.item-adic9 V-table-Win
ON LEAVE OF cota-item.item-adic9 IN FRAME f-main /* Item Adic.9 */
DO:
  IF INPUT FRAME {&FRAME-NAME} cota-item.item-adic9 <> "" THEN DO:
     FIND item WHERE item.it-codigo = INPUT FRAME {&FRAME-NAME} cota-item.item-adic9
               NO-LOCK NO-ERROR.
     IF AVAIL item THEN
        ASSIGN fi-desc-item9:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
     ELSE DO:
        MESSAGE "Item n∆o cadastrado." VIEW-AS ALERT-BOX.
        ASSIGN fi-desc-item9:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "". 
        APPLY 'entry' TO cota-item.item-adic9.
        RETURN NO-APPLY.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cota-item.item-adic9 V-table-Win
ON MOUSE-SELECT-DBLCLICK OF cota-item.item-adic9 IN FRAME f-main /* Item Adic.9 */
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z01in172.w
                     &campo     = cota-item.item-adic9
                     &campozoom = it-codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  
  cota-item.it-codigo:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.
  cota-item.item-adic1:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.
  cota-item.item-adic2:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.
  cota-item.item-adic3:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.
  cota-item.item-adic4:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.
  cota-item.item-adic5:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.
  cota-item.item-adic6:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.
  cota-item.item-adic7:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.
  cota-item.item-adic8:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.
  cota-item.item-adic9:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.
  cota-item.item-adic10:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.
  cota-item.item-adic11:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.
  cota-item.item-adic12:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.
  cota-item.item-adic13:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.
  cota-item.item-adic14:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.
  cota-item.item-adic15:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.
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
  {src/adm/template/row-list.i "cota-item"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "cota-item"}

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
    IF RETURN-VALUE = 'ADM-ERROR':U THEN
       RETURN 'ADM-ERROR':U.

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

    FIND ITEM WHERE item.it-codigo = cota-item.it-codigo NO-LOCK NO-ERROR.
         IF AVAIL ITEM THEN
            ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ITEM.desc-item.
     
    IF AVAIL cota-item THEN DO:
       IF cota-item.item-adic1 <> "" THEN DO:
          FIND item WHERE item.it-codigo = cota-item.item-adic1 NO-LOCK NO-ERROR.
          IF AVAIL item THEN
             ASSIGN fi-desc-item1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
       END.
   
       IF cota-item.item-adic2 <> "" THEN DO:
          FIND item WHERE item.it-codigo = cota-item.item-adic2 NO-LOCK NO-ERROR.
          IF AVAIL item THEN
             ASSIGN fi-desc-item2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
       END.
       
       IF cota-item.item-adic3 <> "" THEN DO:
          FIND item WHERE item.it-codigo = cota-item.item-adic3 NO-LOCK NO-ERROR.
          IF AVAIL item THEN
             ASSIGN fi-desc-item3:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
       END.
   
       IF cota-item.item-adic4 <> "" THEN DO:
          FIND item WHERE item.it-codigo = cota-item.item-adic4 NO-LOCK NO-ERROR.
          IF AVAIL item THEN
             ASSIGN fi-desc-item4:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
       END.
   
       IF cota-item.item-adic5 <> "" THEN DO:
          FIND item WHERE item.it-codigo = cota-item.item-adic5 NO-LOCK NO-ERROR.
          IF AVAIL item THEN
             ASSIGN fi-desc-item5:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
       END.
   
       IF cota-item.item-adic6 <> "" THEN DO:
          FIND item WHERE item.it-codigo = cota-item.item-adic6 NO-LOCK NO-ERROR.
          IF AVAIL item THEN
             ASSIGN fi-desc-item6:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
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
  Notes: N∆o fazer assign aqui. Nesta procedure
  devem ser colocadas apenas validaá‰es, pois neste ponto do programa o registro 
  ainda n∆o foi criado.       
------------------------------------------------------------------------------*/
    {include/i-vldfrm.i} /* Validaá∆o de dicion†rio */
    
/*/*    Segue um exemplo de validaá∆o de programa */
 *     find tabela where tabela.campo1 = c-variavel and
 *                       tabela.campo2 > i-variavel no-lock no-error.
 *     
 *     /* Este include deve ser colocado sempre antes do ut-msgs.p */
 *     {include/i-vldprg.i}
 *     run utp/ut-msgs.p (input "show":U, input 7, input return-value).
 *     return 'ADM-ERROR':U.*/

 IF INPUT FRAME {&FRAME-NAME} cota-item.item-adic1 <> "" THEN DO:
    find item WHERE item.it-codigo = INPUT FRAME {&FRAME-NAME} cota-item.item-adic1                 
                 NO-LOCK NO-ERROR.                                                                             
    IF NOT AVAIL item THEN DO:                                                                              
       MESSAGE "÷tem inv†lido." VIEW-AS ALERT-BOX.                              
       APPLY "entry" TO cota-item.item-adic1.                                                                
       return 'ADM-ERROR':U.                                                                                   
    END.                                                                                                       
 END.

 IF INPUT FRAME {&FRAME-NAME} cota-item.item-adic2 <> "" THEN DO:
    find item WHERE item.it-codigo = INPUT FRAME {&FRAME-NAME} cota-item.item-adic2                 
                 NO-LOCK NO-ERROR.                                                                             
    IF NOT AVAIL item THEN DO:                                                                              
       MESSAGE "÷tem inv†lido." VIEW-AS ALERT-BOX.                              
       APPLY "entry" TO cota-item.item-adic2.                                                                
       return 'ADM-ERROR':U.                                                                                   
    END.                                                                                                       
 END.

 IF INPUT FRAME {&FRAME-NAME} cota-item.item-adic3 <> "" THEN DO:
    find item WHERE item.it-codigo = INPUT FRAME {&FRAME-NAME} cota-item.item-adic3                 
                 NO-LOCK NO-ERROR.                                                                             
    IF NOT AVAIL item THEN DO:                                                                              
       MESSAGE "÷tem inv†lido." VIEW-AS ALERT-BOX.                              
       APPLY "entry" TO cota-item.item-adic3.                                                                
       return 'ADM-ERROR':U.                                                                                   
    END.                                                                                                       
 END.

 IF INPUT FRAME {&FRAME-NAME} cota-item.item-adic4 <> "" THEN DO:
    find item WHERE item.it-codigo = INPUT FRAME {&FRAME-NAME} cota-item.item-adic4                 
                 NO-LOCK NO-ERROR.                                                                             
    IF NOT AVAIL item THEN DO:                                                                              
       MESSAGE "÷tem inv†lido." VIEW-AS ALERT-BOX.                              
       APPLY "entry" TO cota-item.item-adic4.                                                                
       return 'ADM-ERROR':U.                                                                                   
    END.                                                                                                       
 END.

 IF INPUT FRAME {&FRAME-NAME} cota-item.item-adic5 <> "" THEN DO:
    find item WHERE item.it-codigo = INPUT FRAME {&FRAME-NAME} cota-item.item-adic5                 
                 NO-LOCK NO-ERROR.                                                                             
    IF NOT AVAIL item THEN DO:                                                                              
       MESSAGE "÷tem inv†lido." VIEW-AS ALERT-BOX.                              
       APPLY "entry" TO cota-item.item-adic5.                                                                
       return 'ADM-ERROR':U.                                                                                   
    END.                                                                                                       
 END.

 IF INPUT FRAME {&FRAME-NAME} cota-item.item-adic6 <> "" THEN DO:
    find item WHERE item.it-codigo = INPUT FRAME {&FRAME-NAME} cota-item.item-adic6                 
                 NO-LOCK NO-ERROR.                                                                             
    IF NOT AVAIL item THEN DO:                                                                              
       MESSAGE "÷tem inv†lido." VIEW-AS ALERT-BOX.                              
       APPLY "entry" TO cota-item.item-adic6.                                                                
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
  {src/adm/template/snd-list.i "cota-item"}

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

