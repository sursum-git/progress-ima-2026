&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          mgcad            PROGRESS
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
{include/i-prgvrs.i V01IN377 2.04.00.000}

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
DEF VAR c-cod-refer LIKE referencia.cod-refer.

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
&Scoped-define EXTERNAL-TABLES referencia
&Scoped-define FIRST-EXTERNAL-TABLE referencia


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR referencia.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS referencia.descricao 
&Scoped-define ENABLED-TABLES referencia
&Scoped-define FIRST-ENABLED-TABLE referencia
&Scoped-Define ENABLED-OBJECTS rt-key rt-mold 
&Scoped-Define DISPLAYED-FIELDS referencia.cod-refer referencia.descricao 
&Scoped-define DISPLAYED-TABLES referencia
&Scoped-define FIRST-DISPLAYED-TABLE referencia
&Scoped-Define DISPLAYED-OBJECTS tg-var1 tg-var2 tg-var3 tg-var4 tg-var5 ~
tg-var6 tg-var7 tg-var8 tg-var9 fi-cod-obsoleto fi-cod-fundo fi-cor ~
fi-colecao 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-CREATE-FIELDS referencia.cod-refer tg-var1 tg-var2 ~
tg-var3 tg-var4 tg-var5 tg-var6 tg-var7 tg-var8 tg-var9 fi-cod-obsoleto ~
fi-cod-fundo fi-cor fi-colecao 

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
DEFINE VARIABLE fi-cod-fundo AS CHARACTER FORMAT "x(4)" 
     LABEL "Codigo Fundo" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-obsoleto AS CHARACTER FORMAT "x(1)" 
     LABEL "Cod.Obsoleto" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-colecao AS CHARACTER FORMAT "x(20)" 
     LABEL "Colecao" 
     VIEW-AS FILL-IN 
     SIZE 30 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cor AS CHARACTER FORMAT "X(20)" 
     LABEL "Cor" 
     VIEW-AS FILL-IN 
     SIZE 30 BY .88 NO-UNDO.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 88 BY 1.25.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 88 BY 6.5.

DEFINE VARIABLE tg-var1 AS LOGICAL INITIAL no 
     LABEL "Var-1" 
     VIEW-AS TOGGLE-BOX
     SIZE 6.57 BY .83 NO-UNDO.

DEFINE VARIABLE tg-var2 AS LOGICAL INITIAL no 
     LABEL "Var-2" 
     VIEW-AS TOGGLE-BOX
     SIZE 6.57 BY .83 NO-UNDO.

DEFINE VARIABLE tg-var3 AS LOGICAL INITIAL no 
     LABEL "Var-3" 
     VIEW-AS TOGGLE-BOX
     SIZE 6.57 BY .83 NO-UNDO.

DEFINE VARIABLE tg-var4 AS LOGICAL INITIAL no 
     LABEL "Var-4" 
     VIEW-AS TOGGLE-BOX
     SIZE 6.57 BY .83 NO-UNDO.

DEFINE VARIABLE tg-var5 AS LOGICAL INITIAL no 
     LABEL "Var-5" 
     VIEW-AS TOGGLE-BOX
     SIZE 6.57 BY .83 NO-UNDO.

DEFINE VARIABLE tg-var6 AS LOGICAL INITIAL no 
     LABEL "Var-6" 
     VIEW-AS TOGGLE-BOX
     SIZE 6.57 BY .83 NO-UNDO.

DEFINE VARIABLE tg-var7 AS LOGICAL INITIAL no 
     LABEL "Var-7" 
     VIEW-AS TOGGLE-BOX
     SIZE 6.57 BY .83 NO-UNDO.

DEFINE VARIABLE tg-var8 AS LOGICAL INITIAL no 
     LABEL "Var-8" 
     VIEW-AS TOGGLE-BOX
     SIZE 6.57 BY .83 NO-UNDO.

DEFINE VARIABLE tg-var9 AS LOGICAL INITIAL no 
     LABEL "Var-9" 
     VIEW-AS TOGGLE-BOX
     SIZE 6.57 BY .83 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     referencia.cod-refer AT ROW 1.17 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.14 BY .88
     referencia.descricao AT ROW 2.58 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 33.14 BY .88
     tg-var1 AT ROW 3.58 COL 15
     tg-var2 AT ROW 3.58 COL 22
     tg-var3 AT ROW 3.58 COL 29
     tg-var4 AT ROW 3.58 COL 36
     tg-var5 AT ROW 3.58 COL 43
     tg-var6 AT ROW 3.58 COL 51
     tg-var7 AT ROW 3.58 COL 58
     tg-var8 AT ROW 3.58 COL 65
     tg-var9 AT ROW 3.58 COL 72
     fi-cod-obsoleto AT ROW 4.58 COL 13 COLON-ALIGNED HELP
          "Codigo obsoleto"
     fi-cod-fundo AT ROW 5.58 COL 13 COLON-ALIGNED HELP
          "Codigo de fundo para programacao de producao"
     fi-cor AT ROW 6.58 COL 13 COLON-ALIGNED
     fi-colecao AT ROW 7.58 COL 13 COLON-ALIGNED HELP
          "Colecao do item"
     rt-key AT ROW 1 COL 1
     rt-mold AT ROW 2.25 COL 1
     "Variantes:" VIEW-AS TEXT
          SIZE 7.14 BY .54 AT ROW 3.71 COL 7.86
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: mgcad.referencia
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
         HEIGHT             = 7.83
         WIDTH              = 88.14.
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

/* SETTINGS FOR FILL-IN referencia.cod-refer IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN fi-cod-fundo IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN fi-cod-obsoleto IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN fi-colecao IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN fi-cor IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR TOGGLE-BOX tg-var1 IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR TOGGLE-BOX tg-var2 IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR TOGGLE-BOX tg-var3 IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR TOGGLE-BOX tg-var4 IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR TOGGLE-BOX tg-var5 IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR TOGGLE-BOX tg-var6 IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR TOGGLE-BOX tg-var7 IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR TOGGLE-BOX tg-var8 IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR TOGGLE-BOX tg-var9 IN FRAME f-main
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

&Scoped-define SELF-NAME referencia.cod-refer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL referencia.cod-refer V-table-Win
ON LEAVE OF referencia.cod-refer IN FRAME f-main /* Referància */
DO:
  FIND referencia WHERE referencia.cod-refer = INPUT FRAME {&FRAME-NAME} referencia.cod-refer
                  NO-LOCK NO-ERROR.
  IF AVAIL referencia THEN DO:
     MESSAGE "Referància j† existe com esse C¢digo!"
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     APPLY 'entry' TO referencia.cod-refer IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END.
  FIND acab-tecido WHERE 
       acab-tecido.codigo = int(SUBSTR(INPUT FRAME {&FRAME-NAME} referencia.cod-refer,1,2))
       NO-LOCK NO-ERROR.
  IF NOT AVAIL acab-tecido THEN DO:
      MESSAGE "Acabamento " SUBSTR(INPUT FRAME {&FRAME-NAME} referencia.cod-refer,1,2) 
              " n∆o encontrado!"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY 'entry' TO referencia.cod-refer IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
  END.
  ELSE
      ASSIGN referencia.descricao:SCREEN-VALUE IN FRAME {&FRAME-NAME} =
             acab-tecido.sigla + " " + SUBSTR(INPUT FRAME {&FRAME-NAME} referencia.cod-refer,3,4) +
             "-" + SUBSTR(INPUT FRAME {&FRAME-NAME} referencia.cod-refer,7,1).
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
  {src/adm/template/row-list.i "referencia"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "referencia"}

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
    
    /*:T Ponha na pi-validate todas as validaá‰es */
    /*:T N∆o gravar nada no registro antes do dispatch do assign-record e 
       nem na PI-validate. */
    
    /* Dispatch standard ADM method.                             */
   
    RUN pi-validate.
    if RETURN-VALUE = 'ADM-ERROR':U then 
       return 'ADM-ERROR':U.
        
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.
    
    FIND referencia-ext WHERE referencia-ext.cod-refer = referencia.cod-refer SHARE-LOCK NO-ERROR.
    IF NOT AVAIL referencia-ext THEN DO:
       CREATE referencia-ext.
       ASSIGN referencia-ext.cod-refer    = referencia.cod-refer
              referencia-ext.cod-obsoleto = INPUT FRAME {&FRAME-NAME} fi-cod-obsoleto
              referencia-ext.colecao      = INPUT FRAME {&FRAME-NAME} fi-colecao
              referencia-ext.cod-fundo    = INPUT FRAME {&FRAME-NAME} fi-cod-fundo
              referencia-ext.cor          = INPUT FRAME {&FRAME-NAME} fi-cor.
    END.
    ELSE DO:
       ASSIGN referencia-ext.cod-obsoleto = INPUT FRAME {&FRAME-NAME} fi-cod-obsoleto
              referencia-ext.colecao      = INPUT FRAME {&FRAME-NAME} fi-colecao
              referencia-ext.cod-fundo    = INPUT FRAME {&FRAME-NAME} fi-cod-fundo
              referencia-ext.cor          = INPUT FRAME {&FRAME-NAME} fi-cor.
    END.

    IF INPUT FRAME {&FRAME-NAME} tg-var1 = YES THEN DO:
       ASSIGN c-cod-refer = SUBSTR(INPUT FRAME {&FRAME-NAME} referencia.cod-refer,1,6) + "1".
       FIND referencia WHERE referencia.cod-refer = c-cod-refer NO-LOCK NO-ERROR.
       IF NOT AVAIL referencia THEN DO:
          CREATE referencia.
          ASSIGN referencia.cod-refer = c-cod-refer
                 referencia.descricao = SUBSTR(INPUT FRAME {&FRAME-NAME} referencia.descricao,1,9) + "1".

          FIND referencia-ext WHERE referencia-ext.cod-refer = referencia.cod-refer NO-LOCK NO-ERROR.
          IF NOT AVAIL referencia-ext THEN DO:
             CREATE referencia-ext.
             ASSIGN referencia-ext.cod-refer    = referencia.cod-refer
                    referencia-ext.cod-obsoleto = INPUT FRAME {&FRAME-NAME} fi-cod-obsoleto
                    referencia-ext.colecao      = INPUT FRAME {&FRAME-NAME} fi-colecao
                    referencia-ext.cod-fundo    = INPUT FRAME {&FRAME-NAME} fi-cod-fundo
                    referencia-ext.cor          = INPUT FRAME {&FRAME-NAME} fi-cor.
          END.
       END.
    END.
    IF INPUT FRAME {&FRAME-NAME} tg-var2 = YES THEN DO:
       ASSIGN c-cod-refer = SUBSTR(INPUT FRAME {&FRAME-NAME} referencia.cod-refer,1,6) + "2".
       FIND referencia WHERE referencia.cod-refer = c-cod-refer NO-LOCK NO-ERROR.
       IF NOT AVAIL referencia THEN DO:
          CREATE referencia.
          ASSIGN referencia.cod-refer = c-cod-refer
                 referencia.descricao = SUBSTR(INPUT FRAME {&FRAME-NAME} referencia.descricao,1,9) + "2".

          FIND referencia-ext WHERE referencia-ext.cod-refer = referencia.cod-refer NO-LOCK NO-ERROR.
          IF NOT AVAIL referencia-ext THEN DO:
             CREATE referencia-ext.
             ASSIGN referencia-ext.cod-refer    = referencia.cod-refer
                    referencia-ext.cod-obsoleto = INPUT FRAME {&FRAME-NAME} fi-cod-obsoleto
                    referencia-ext.colecao      = INPUT FRAME {&FRAME-NAME} fi-colecao
                    referencia-ext.cod-fundo    = INPUT FRAME {&FRAME-NAME} fi-cod-fundo
                    referencia-ext.cor          = INPUT FRAME {&FRAME-NAME} fi-cor.
          END.
       END.
    END.
    IF INPUT FRAME {&FRAME-NAME} tg-var3 = YES THEN DO:
       ASSIGN c-cod-refer = SUBSTR(INPUT FRAME {&FRAME-NAME} referencia.cod-refer,1,6) + "3".
       FIND referencia WHERE referencia.cod-refer = c-cod-refer NO-LOCK NO-ERROR.
       IF NOT AVAIL referencia THEN DO:
          CREATE referencia.
          ASSIGN referencia.cod-refer = c-cod-refer
                 referencia.descricao = SUBSTR(INPUT FRAME {&FRAME-NAME} referencia.descricao,1,9) + "3".

          FIND referencia-ext WHERE referencia-ext.cod-refer = referencia.cod-refer NO-LOCK NO-ERROR.
          IF NOT AVAIL referencia-ext THEN DO:
             CREATE referencia-ext.
             ASSIGN referencia-ext.cod-refer    = referencia.cod-refer
                    referencia-ext.cod-obsoleto = INPUT FRAME {&FRAME-NAME} fi-cod-obsoleto
                    referencia-ext.colecao      = INPUT FRAME {&FRAME-NAME} fi-colecao
                    referencia-ext.cod-fundo    = INPUT FRAME {&FRAME-NAME} fi-cod-fundo
                    referencia-ext.cor          = INPUT FRAME {&FRAME-NAME} fi-cor.
          END.
       END.
    END.
    IF INPUT FRAME {&FRAME-NAME} tg-var4 = YES THEN DO:
       ASSIGN c-cod-refer = SUBSTR(INPUT FRAME {&FRAME-NAME} referencia.cod-refer,1,6) + "4".
       FIND referencia WHERE referencia.cod-refer = c-cod-refer NO-LOCK NO-ERROR.
       IF NOT AVAIL referencia THEN DO:
          CREATE referencia.
          ASSIGN referencia.cod-refer = c-cod-refer
                 referencia.descricao = SUBSTR(INPUT FRAME {&FRAME-NAME} referencia.descricao,1,9) + "4".

          FIND referencia-ext WHERE referencia-ext.cod-refer = referencia.cod-refer NO-LOCK NO-ERROR.
          IF NOT AVAIL referencia-ext THEN DO:
             CREATE referencia-ext.
             ASSIGN referencia-ext.cod-refer    = referencia.cod-refer
                    referencia-ext.cod-obsoleto = INPUT FRAME {&FRAME-NAME} fi-cod-obsoleto
                    referencia-ext.colecao      = INPUT FRAME {&FRAME-NAME} fi-colecao
                    referencia-ext.cod-fundo    = INPUT FRAME {&FRAME-NAME} fi-cod-fundo
                    referencia-ext.cor          = INPUT FRAME {&FRAME-NAME} fi-cor.
          END.
       END.
    END.
    IF INPUT FRAME {&FRAME-NAME} tg-var5 = YES THEN DO:
       ASSIGN c-cod-refer = SUBSTR(INPUT FRAME {&FRAME-NAME} referencia.cod-refer,1,6) + "5".
       FIND referencia WHERE referencia.cod-refer = c-cod-refer NO-LOCK NO-ERROR.
       IF NOT AVAIL referencia THEN DO:
          CREATE referencia.
          ASSIGN referencia.cod-refer = c-cod-refer
                 referencia.descricao = SUBSTR(INPUT FRAME {&FRAME-NAME} referencia.descricao,1,9) + "5".

          FIND referencia-ext WHERE referencia-ext.cod-refer = referencia.cod-refer NO-LOCK NO-ERROR.
          IF NOT AVAIL referencia-ext THEN DO:
             CREATE referencia-ext.
             ASSIGN referencia-ext.cod-refer    = referencia.cod-refer
                    referencia-ext.cod-obsoleto = INPUT FRAME {&FRAME-NAME} fi-cod-obsoleto
                    referencia-ext.colecao      = INPUT FRAME {&FRAME-NAME} fi-colecao
                    referencia-ext.cod-fundo    = INPUT FRAME {&FRAME-NAME} fi-cod-fundo
                    referencia-ext.cor          = INPUT FRAME {&FRAME-NAME} fi-cor.
          END.
       END.
    END.
    IF INPUT FRAME {&FRAME-NAME} tg-var6 = YES THEN DO:
       ASSIGN c-cod-refer = SUBSTR(INPUT FRAME {&FRAME-NAME} referencia.cod-refer,1,6) + "6".
       FIND referencia WHERE referencia.cod-refer = c-cod-refer NO-LOCK NO-ERROR.
       IF NOT AVAIL referencia THEN DO:
          CREATE referencia.
          ASSIGN referencia.cod-refer = c-cod-refer
                 referencia.descricao = SUBSTR(INPUT FRAME {&FRAME-NAME} referencia.descricao,1,9) + "6".

          FIND referencia-ext WHERE referencia-ext.cod-refer = referencia.cod-refer NO-LOCK NO-ERROR.
          IF NOT AVAIL referencia-ext THEN DO:
             CREATE referencia-ext.
             ASSIGN referencia-ext.cod-refer    = referencia.cod-refer
                    referencia-ext.cod-obsoleto = INPUT FRAME {&FRAME-NAME} fi-cod-obsoleto
                    referencia-ext.colecao      = INPUT FRAME {&FRAME-NAME} fi-colecao
                    referencia-ext.cod-fundo    = INPUT FRAME {&FRAME-NAME} fi-cod-fundo
                    referencia-ext.cor          = INPUT FRAME {&FRAME-NAME} fi-cor.
          END.
       END.
    END.
    IF INPUT FRAME {&FRAME-NAME} tg-var7 = YES THEN DO:
       ASSIGN c-cod-refer = SUBSTR(INPUT FRAME {&FRAME-NAME} referencia.cod-refer,1,6) + "7".
       FIND referencia WHERE referencia.cod-refer = c-cod-refer NO-LOCK NO-ERROR.
       IF NOT AVAIL referencia THEN DO:
          CREATE referencia.
          ASSIGN referencia.cod-refer = c-cod-refer
                 referencia.descricao = SUBSTR(INPUT FRAME {&FRAME-NAME} referencia.descricao,1,9) + "7".

          FIND referencia-ext WHERE referencia-ext.cod-refer = referencia.cod-refer NO-LOCK NO-ERROR.
          IF NOT AVAIL referencia-ext THEN DO:
             CREATE referencia-ext.
             ASSIGN referencia-ext.cod-refer    = referencia.cod-refer
                    referencia-ext.cod-obsoleto = INPUT FRAME {&FRAME-NAME} fi-cod-obsoleto
                    referencia-ext.colecao      = INPUT FRAME {&FRAME-NAME} fi-colecao
                    referencia-ext.cod-fundo    = INPUT FRAME {&FRAME-NAME} fi-cod-fundo
                    referencia-ext.cor          = INPUT FRAME {&FRAME-NAME} fi-cor.
          END.
       END.
    END.
    IF INPUT FRAME {&FRAME-NAME} tg-var8 = YES THEN DO:
       ASSIGN c-cod-refer = SUBSTR(INPUT FRAME {&FRAME-NAME} referencia.cod-refer,1,6) + "8".
       FIND referencia WHERE referencia.cod-refer = c-cod-refer NO-LOCK NO-ERROR.
       IF NOT AVAIL referencia THEN DO:
          CREATE referencia.
          ASSIGN referencia.cod-refer = c-cod-refer
                 referencia.descricao = SUBSTR(INPUT FRAME {&FRAME-NAME} referencia.descricao,1,9) + "8".

          FIND referencia-ext WHERE referencia-ext.cod-refer = referencia.cod-refer NO-LOCK NO-ERROR.
          IF NOT AVAIL referencia-ext THEN DO:
             CREATE referencia-ext.
             ASSIGN referencia-ext.cod-refer    = referencia.cod-refer
                    referencia-ext.cod-obsoleto = INPUT FRAME {&FRAME-NAME} fi-cod-obsoleto
                    referencia-ext.colecao      = INPUT FRAME {&FRAME-NAME} fi-colecao
                    referencia-ext.cod-fundo    = INPUT FRAME {&FRAME-NAME} fi-cod-fundo
                    referencia-ext.cor          = INPUT FRAME {&FRAME-NAME} fi-cor.
          END.
       END.
    END.
    IF INPUT FRAME {&FRAME-NAME} tg-var9 = YES THEN DO:
       ASSIGN c-cod-refer = SUBSTR(INPUT FRAME {&FRAME-NAME} referencia.cod-refer,1,6) + "9".
       FIND referencia WHERE referencia.cod-refer = c-cod-refer NO-LOCK NO-ERROR.
       IF NOT AVAIL referencia THEN DO:
          CREATE referencia.
          ASSIGN referencia.cod-refer = c-cod-refer
                 referencia.descricao = SUBSTR(INPUT FRAME {&FRAME-NAME} referencia.descricao,1,9) + "9".

          FIND referencia-ext WHERE referencia-ext.cod-refer = referencia.cod-refer NO-LOCK NO-ERROR.
          IF NOT AVAIL referencia-ext THEN DO:
             CREATE referencia-ext.
             ASSIGN referencia-ext.cod-refer    = referencia.cod-refer
                    referencia-ext.cod-obsoleto = INPUT FRAME {&FRAME-NAME} fi-cod-obsoleto
                    referencia-ext.colecao      = INPUT FRAME {&FRAME-NAME} fi-colecao
                    referencia-ext.cod-fundo    = INPUT FRAME {&FRAME-NAME} fi-cod-fundo
                    referencia-ext.cor          = INPUT FRAME {&FRAME-NAME} fi-cor.
          END.
       END.
    END.

    ASSIGN tg-var1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO"
           tg-var2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO"
           tg-var3:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO"
           tg-var4:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO"
           tg-var5:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO"
           tg-var6:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO"
           tg-var7:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO"
           tg-var8:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO"
           tg-var9:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO".

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
    /* Code placed here will execute PRIOR to standard behavior. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ).
    
    /* Code placed here will execute AFTER standard behavior.    */

    IF AVAIL referencia THEN DO:
       FIND referencia-ext WHERE
            referencia-ext.cod-refer = referencia.cod-refer NO-LOCK NO-ERROR.
       IF AVAIL referencia-ext THEN 
          ASSIGN fi-cod-obsoleto:SCREEN-VALUE IN FRAME {&FRAME-NAME} = referencia-ext.cod-obsoleto
                 fi-colecao:SCREEN-VALUE IN FRAME {&FRAME-NAME}      = referencia-ext.colecao
                 fi-cod-fundo:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = referencia-ext.cod-fundo
                 fi-cor:SCREEN-VALUE IN FRAME {&FRAME-NAME}          = referencia-ext.cor.
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

    IF adm-new-record = NO THEN
       ASSIGN referencia.descricao:SENSITIVE IN FRAME {&FRAME-NAME} = NO
              fi-cod-obsoleto:SENSITIVE IN FRAME {&FRAME-NAME}      = YES 
              fi-cod-fundo:SENSITIVE IN FRAME {&FRAME-NAME}         = YES 
              fi-cor:SENSITIVE IN FRAME {&FRAME-NAME}               = YES
              fi-colecao:SENSITIVE IN FRAME {&FRAME-NAME}           = YES.





END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-altera V-table-Win 
PROCEDURE pi-altera :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 ASSIGN fi-cod-obsoleto:SENSITIVE IN FRAME {&FRAME-NAME} = YES
        fi-cod-fundo:SENSITIVE IN FRAME {&FRAME-NAME}    = YES
        fi-cor:SENSITIVE IN FRAME {&FRAME-NAME}          = YES
        fi-colecao:SENSITIVE IN FRAME {&FRAME-NAME}      = YES.
 APPLY 'entry' TO fi-cod-obsoleto.
MESSAGE "Pi-altera"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-validate V-table-Win 
PROCEDURE pi-validate :
/*------------------------------------------------------------------------------
  Purpose:Validar a viewer     
  Parameters:  <none>
  Notes: N∆o fazer assign aqui. Nesta procedure
  devem ser colocadas apenas validaá‰es, pois neste ponto do programa o registro 
  ainda n∆o foi criado.       
------------------------------------------------------------------------------*/
    {include/i-vldfrm.i} /* Validaá∆o de dicion†rio */
    
    IF INPUT FRAME {&frame-name} fi-cod-obsoleto < "0" or
       INPUT FRAME {&frame-name} fi-cod-obsoleto > "5" THEN DO:
       MESSAGE "C¢digo Obsoleto deve estar entre 0 e 5." VIEW-AS ALERT-BOX.
       APPLY 'entry' TO fi-cod-obsoleto.                                                          
       return 'ADM-ERROR':U.                                                                    
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
  {src/adm/template/snd-list.i "referencia"}

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

