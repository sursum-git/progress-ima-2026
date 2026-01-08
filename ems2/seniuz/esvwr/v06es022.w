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
&Scoped-Define ENABLED-FIELDS mov-man.sist-exec[1] 
&Scoped-define ENABLED-TABLES mov-man
&Scoped-define FIRST-ENABLED-TABLE mov-man
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 RECT-3 rt-mold 
&Scoped-Define DISPLAYED-FIELDS mov-man.sist-exec[1] 
&Scoped-define DISPLAYED-TABLES mov-man
&Scoped-define FIRST-DISPLAYED-TABLE mov-man
&Scoped-Define DISPLAYED-OBJECTS fi-ssist-exec1 fi-desc-exec1 fi-sist-exec2 ~
fi-ssist-exec2 fi-desc-exec2 fi-sist-exec3 fi-ssist-exec3 fi-desc-exec3 ~
fi-sist-exec4 fi-ssist-exec4 fi-desc-exec4 fi-sist-exec5 fi-ssist-exec5 ~
fi-desc-exec5 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define List-4 fi-ssist-exec1 fi-desc-exec1 fi-sist-exec2 ~
fi-ssist-exec2 fi-desc-exec2 fi-sist-exec3 fi-ssist-exec3 fi-desc-exec3 ~
fi-sist-exec4 fi-ssist-exec4 fi-desc-exec4 fi-sist-exec5 fi-ssist-exec5 ~
fi-desc-exec5 

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
DEFINE VARIABLE fi-desc-exec1 AS CHARACTER FORMAT "x(45)" 
     VIEW-AS FILL-IN 
     SIZE 43 BY .88.

DEFINE VARIABLE fi-desc-exec2 AS CHARACTER FORMAT "x(45)" 
     VIEW-AS FILL-IN 
     SIZE 43 BY .88.

DEFINE VARIABLE fi-desc-exec3 AS CHARACTER FORMAT "x(45)" 
     VIEW-AS FILL-IN 
     SIZE 43 BY .88.

DEFINE VARIABLE fi-desc-exec4 AS CHARACTER FORMAT "x(45)" 
     VIEW-AS FILL-IN 
     SIZE 43 BY .88.

DEFINE VARIABLE fi-desc-exec5 AS CHARACTER FORMAT "x(45)" 
     VIEW-AS FILL-IN 
     SIZE 43 BY .88
     FONT 1.

DEFINE VARIABLE fi-sist-exec2 AS CHARACTER FORMAT "x(1)" 
     VIEW-AS FILL-IN 
     SIZE 3.14 BY .88.

DEFINE VARIABLE fi-sist-exec3 AS CHARACTER FORMAT "x(1)" 
     VIEW-AS FILL-IN 
     SIZE 3.14 BY .88.

DEFINE VARIABLE fi-sist-exec4 AS CHARACTER FORMAT "x(1)" 
     VIEW-AS FILL-IN 
     SIZE 3.14 BY .88.

DEFINE VARIABLE fi-sist-exec5 AS CHARACTER FORMAT "x(1)" 
     VIEW-AS FILL-IN 
     SIZE 3.14 BY .88.

DEFINE VARIABLE fi-ssist-exec1 AS CHARACTER FORMAT "X(5)" 
     VIEW-AS FILL-IN 
     SIZE 9.14 BY .88.

DEFINE VARIABLE fi-ssist-exec2 AS CHARACTER FORMAT "x(5)" 
     VIEW-AS FILL-IN 
     SIZE 9.14 BY .88.

DEFINE VARIABLE fi-ssist-exec3 AS CHARACTER FORMAT "x(5)" 
     VIEW-AS FILL-IN 
     SIZE 9.14 BY .88.

DEFINE VARIABLE fi-ssist-exec4 AS CHARACTER FORMAT "x(5)" 
     VIEW-AS FILL-IN 
     SIZE 9.14 BY .88.

DEFINE VARIABLE fi-ssist-exec5 AS CHARACTER FORMAT "x(5)" 
     VIEW-AS FILL-IN 
     SIZE 9.14 BY .88.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 11 BY 5.75.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 16 BY 5.75.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 48 BY 5.75.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 87 BY 10.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     mov-man.sist-exec[1] AT ROW 2.33 COL 6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.14 BY 1
     fi-ssist-exec1 AT ROW 2.33 COL 18.14 COLON-ALIGNED NO-LABEL
     fi-desc-exec1 AT ROW 2.33 COL 37.57 COLON-ALIGNED NO-LABEL
     fi-sist-exec2 AT ROW 3.33 COL 6 COLON-ALIGNED NO-LABEL
     fi-ssist-exec2 AT ROW 3.33 COL 18.14 COLON-ALIGNED NO-LABEL
     fi-desc-exec2 AT ROW 3.33 COL 37.57 COLON-ALIGNED NO-LABEL
     fi-sist-exec3 AT ROW 4.33 COL 6 COLON-ALIGNED NO-LABEL
     fi-ssist-exec3 AT ROW 4.33 COL 18.14 COLON-ALIGNED NO-LABEL
     fi-desc-exec3 AT ROW 4.33 COL 37.57 COLON-ALIGNED NO-LABEL
     fi-sist-exec4 AT ROW 5.33 COL 6 COLON-ALIGNED NO-LABEL
     fi-ssist-exec4 AT ROW 5.33 COL 18.14 COLON-ALIGNED NO-LABEL
     fi-desc-exec4 AT ROW 5.33 COL 37.57 COLON-ALIGNED NO-LABEL
     fi-sist-exec5 AT ROW 6.33 COL 6 COLON-ALIGNED NO-LABEL
     fi-ssist-exec5 AT ROW 6.33 COL 18.14 COLON-ALIGNED NO-LABEL
     fi-desc-exec5 AT ROW 6.33 COL 37.57 COLON-ALIGNED NO-LABEL
     RECT-1 AT ROW 2 COL 4
     RECT-2 AT ROW 2 COL 17
     RECT-3 AT ROW 2 COL 37
     rt-mold AT ROW 1 COL 1
     "SISTEMA" VIEW-AS TEXT
          SIZE 7.86 BY .67 AT ROW 1.17 COL 5.72
     "SUBSISTEMA" VIEW-AS TEXT
          SIZE 11.29 BY .67 AT ROW 1.17 COL 19.72
     "D E S C R I Ä « O" VIEW-AS TEXT
          SIZE 16 BY .67 AT ROW 1.17 COL 53.43
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
         HEIGHT             = 10.13
         WIDTH              = 87.14.
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

/* SETTINGS FOR FILL-IN fi-desc-exec1 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-desc-exec2 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-desc-exec3 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-desc-exec4 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-desc-exec5 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-sist-exec2 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-sist-exec3 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-sist-exec4 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-sist-exec5 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-ssist-exec1 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-ssist-exec2 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-ssist-exec3 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-ssist-exec4 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-ssist-exec5 IN FRAME f-main
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

&Scoped-define SELF-NAME fi-sist-exec2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-sist-exec2 V-table-Win
ON LEAVE OF fi-sist-exec2 IN FRAME f-main
DO:
  IF lookup(INPUT FRAME {&frame-name} fi-sist-exec2,"1,2,3,") = 0 THEN DO:
     MESSAGE "Sistema deve ser 1,2 ou 3" VIEW-AS ALERT-BOX.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-sist-exec3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-sist-exec3 V-table-Win
ON LEAVE OF fi-sist-exec3 IN FRAME f-main
DO:
  IF lookup(INPUT FRAME {&frame-name} fi-sist-exec3,"1,2,3,") = 0 THEN DO:
     MESSAGE "Sistema deve ser 1,2 ou 3" VIEW-AS ALERT-BOX.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-sist-exec4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-sist-exec4 V-table-Win
ON LEAVE OF fi-sist-exec4 IN FRAME f-main
DO:
  IF lookup(INPUT FRAME {&frame-name} fi-sist-exec4,"1,2,3,") = 0 THEN DO:
     MESSAGE "Sistema deve ser 1,2 ou 3" VIEW-AS ALERT-BOX.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-sist-exec5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-sist-exec5 V-table-Win
ON LEAVE OF fi-sist-exec5 IN FRAME f-main
DO:
  IF lookup(INPUT FRAME {&frame-name} fi-sist-exec5,"1,2,3,") = 0 THEN DO:
     MESSAGE "Sistema deve ser 1,2 ou 3" VIEW-AS ALERT-BOX.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ssist-exec1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ssist-exec1 V-table-Win
ON LEAVE OF fi-ssist-exec1 IN FRAME f-main
DO:
  IF INPUT FRAME {&frame-name} fi-ssist-exec1 <> "" THEN DO:
     FIND ssist-mec WHERE ssist-mec.codigo = INPUT FRAME {&FRAME-NAME} fi-ssist-exec1
                    NO-LOCK NO-ERROR.
     IF NOT AVAIL ssist-mec THEN DO:
        MESSAGE "SubSistema inv†lido!" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ssist-exec1 V-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-ssist-exec1 IN FRAME f-main
DO:
  {include/zoomvar.i &prog-zoom = eszoom/z01es027.w
                     &campo     = fi-ssist-exec1
                     &campozoom = codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ssist-exec2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ssist-exec2 V-table-Win
ON LEAVE OF fi-ssist-exec2 IN FRAME f-main
DO:
  IF INPUT FRAME {&frame-name} fi-ssist-exec2 <> "" THEN DO:
     FIND ssist-mec WHERE ssist-mec.codigo = INPUT FRAME {&FRAME-NAME} fi-ssist-exec2
                    NO-LOCK NO-ERROR.
     IF NOT AVAIL ssist-mec THEN DO:
        MESSAGE "SubSistema inv†lido!" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ssist-exec2 V-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-ssist-exec2 IN FRAME f-main
DO:
  {include/zoomvar.i &prog-zoom = eszoom/z01es027.w
                     &campo     = fi-ssist-exec2
                     &campozoom = codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ssist-exec3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ssist-exec3 V-table-Win
ON LEAVE OF fi-ssist-exec3 IN FRAME f-main
DO:
  IF INPUT FRAME {&frame-name} fi-ssist-exec3 <> "" THEN DO:
     FIND ssist-mec WHERE ssist-mec.codigo = INPUT FRAME {&FRAME-NAME} fi-ssist-exec3
                    NO-LOCK NO-ERROR.
     IF NOT AVAIL ssist-mec THEN DO:
        MESSAGE "SubSistema inv†lido!" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ssist-exec3 V-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-ssist-exec3 IN FRAME f-main
DO:
  {include/zoomvar.i &prog-zoom = eszoom/z01es027.w
                     &campo     = fi-ssist-exec3
                     &campozoom = codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ssist-exec4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ssist-exec4 V-table-Win
ON LEAVE OF fi-ssist-exec4 IN FRAME f-main
DO:
  IF INPUT FRAME {&frame-name} fi-ssist-exec4 <> "" THEN DO:
     FIND ssist-mec WHERE ssist-mec.codigo = INPUT FRAME {&FRAME-NAME} fi-ssist-exec4
                    NO-LOCK NO-ERROR.
     IF NOT AVAIL ssist-mec THEN DO:
        MESSAGE "SubSistema inv†lido!" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ssist-exec4 V-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-ssist-exec4 IN FRAME f-main
DO:
  {include/zoomvar.i &prog-zoom = eszoom/z01es027.w
                     &campo     = fi-ssist-exec4
                     &campozoom = codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ssist-exec5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ssist-exec5 V-table-Win
ON LEAVE OF fi-ssist-exec5 IN FRAME f-main
DO:
  IF INPUT FRAME {&frame-name} fi-ssist-exec5 <> "" THEN DO:
     FIND ssist-mec WHERE ssist-mec.codigo = INPUT FRAME {&FRAME-NAME} fi-ssist-exec5
                    NO-LOCK NO-ERROR.
     IF NOT AVAIL ssist-mec THEN DO:
        MESSAGE "SubSistema inv†lido!" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ssist-exec5 V-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-ssist-exec5 IN FRAME f-main
DO:
  {include/zoomvar.i &prog-zoom = eszoom/z01es027.w
                     &campo     = fi-ssist-exec5
                     &campozoom = codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  fi-ssist-exec1:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.
  fi-ssist-exec2:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.
  fi-ssist-exec3:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.
  fi-ssist-exec4:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.
  fi-ssist-exec5:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.

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
    ASSIGN mov-man.sist-exec[2]  = fi-sist-exec2   
           mov-man.sist-exec[3]  = fi-sist-exec3   
           mov-man.sist-exec[4]  = fi-sist-exec4   
           mov-man.sist-exec[5]  = fi-sist-exec5
           mov-man.ssist-exec[1] = fi-ssist-exec1 
           mov-man.ssist-exec[2] = fi-ssist-exec2 
           mov-man.ssist-exec[3] = fi-ssist-exec3 
           mov-man.ssist-exec[4] = fi-ssist-exec4 
           mov-man.ssist-exec[5] = fi-ssist-exec5
           mov-man.desc-exec[1]  = fi-desc-exec1 
           mov-man.desc-exec[2]  = fi-desc-exec2 
           mov-man.desc-exec[3]  = fi-desc-exec3 
           mov-man.desc-exec[4]  = fi-desc-exec4 
           mov-man.desc-exec[5]  = fi-desc-exec5.

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
       ASSIGN fi-sist-exec2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = mov-man.sist-exec[2]
              fi-sist-exec3:SCREEN-VALUE IN FRAME {&FRAME-NAME} = mov-man.sist-exec[3]
              fi-sist-exec4:SCREEN-VALUE IN FRAME {&FRAME-NAME} = mov-man.sist-exec[4]
              fi-sist-exec5:SCREEN-VALUE IN FRAME {&FRAME-NAME} = mov-man.sist-exec[5]
              fi-ssist-exec1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = mov-man.ssist-exec[1]
              fi-ssist-exec2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = mov-man.ssist-exec[2]
              fi-ssist-exec3:SCREEN-VALUE IN FRAME {&FRAME-NAME} = mov-man.ssist-exec[3]
              fi-ssist-exec4:SCREEN-VALUE IN FRAME {&FRAME-NAME} = mov-man.ssist-exec[4]
              fi-ssist-exec5:SCREEN-VALUE IN FRAME {&FRAME-NAME} = mov-man.ssist-exec[5]
              fi-desc-exec1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = mov-man.desc-exec[1]
              fi-desc-exec2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = mov-man.desc-exec[2]
              fi-desc-exec3:SCREEN-VALUE IN FRAME {&FRAME-NAME} = mov-man.desc-exec[3]
              fi-desc-exec4:SCREEN-VALUE IN FRAME {&FRAME-NAME} = mov-man.desc-exec[4]
              fi-desc-exec5:SCREEN-VALUE IN FRAME {&FRAME-NAME} = mov-man.desc-exec[5].

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
  Purpose:Validar a viewer     
  Parameters:  <none>
  Notes: N∆o fazer assign aqui. Nesta procedure
  devem ser colocadas apenas validaá‰es, pois neste ponto do programa o registro 
  ainda n∆o foi criado.       
------------------------------------------------------------------------------*/
    {include/i-vldfrm.i} /* Validaá∆o de dicion†rio */
    
  IF INPUT FRAME {&frame-name} fi-sist-exec2 <> "" and
     INPUT FRAME {&frame-name} mov-man.sist-exec[1] = "" THEN DO:
     /* {include/i-vldprg.i}
     run utp/ut-msgs.p (input "show":U, input 9, input return-value). */
     MESSAGE "A seqÅància de entrada dos Sistemas deve ser cont°nua." VIEW-AS ALERT-BOX. 
     APPLY 'entry' TO mov-man.sist-exec[1].
     return 'ADM-ERROR':U.
  END.     
  IF INPUT FRAME {&frame-name} fi-sist-exec3 <> "" and
     INPUT FRAME {&frame-name} fi-sist-exec2 = "" THEN DO:
     MESSAGE "A seqÅància de entrada dos Sistemas deve ser cont°nua." VIEW-AS ALERT-BOX. 
     APPLY 'entry' TO fi-sist-exec2.
     return 'ADM-ERROR':U.
  END.
  IF INPUT FRAME {&frame-name} fi-sist-exec4 <> "" and
     INPUT FRAME {&frame-name} fi-sist-exec3 = "" THEN DO:
     MESSAGE "A seqÅància de entrada dos Sistemas deve ser cont°nua." VIEW-AS ALERT-BOX. 
     APPLY 'entry' TO fi-sist-exec3.
     return 'ADM-ERROR':U.
  END.
  IF INPUT FRAME {&frame-name} fi-sist-exec5 <> "" and
     INPUT FRAME {&frame-name} fi-sist-exec4 = "" THEN DO:
     MESSAGE "A seqÅància de entrada dos Sistemas deve ser cont°nua." VIEW-AS ALERT-BOX. 
     APPLY 'entry' TO fi-sist-exec4.
     return 'ADM-ERROR':U.
  END.

  IF (INPUT FRAME {&frame-name} mov-man.sist-exec[1] = "" and
      (INPUT FRAME {&frame-name} fi-ssist-exec1 <> "" or
       INPUT FRAME {&frame-name} fi-desc-exec1 <> "")) OR
     (INPUT FRAME {&FRAME-NAME} mov-man.sist-exec[1] <> "" AND
      (INPUT FRAME {&FRAME-NAME} fi-ssist-exec1 = "" OR
       INPUT FRAME {&FRAME-NAME} fi-desc-exec1 = "")) THEN DO:
     MESSAGE "N∆o pode haver Sistema sem SubSistema e/ou Descriá∆o." VIEW-AS ALERT-BOX. 
     APPLY 'entry' TO mov-man.sist-exec[1].
     return 'ADM-ERROR':U.
  END.
  IF (INPUT FRAME {&frame-name} fi-sist-exec2 = "" and
      (INPUT FRAME {&frame-name} fi-ssist-exec2 <> "" or
       INPUT FRAME {&frame-name} fi-desc-exec2 <> "")) OR
     (INPUT FRAME {&FRAME-NAME} fi-sist-exec2 <> "" AND
      (INPUT FRAME {&FRAME-NAME} fi-ssist-exec2 = "" OR
       INPUT FRAME {&FRAME-NAME} fi-desc-exec2 = "")) THEN DO:
     MESSAGE "N∆o pode haver Sistema sem SubSistema e/ou Descriá∆o." VIEW-AS ALERT-BOX. 
     APPLY 'entry' TO fi-sist-exec2.
     return 'ADM-ERROR':U.
  END.
  IF (INPUT FRAME {&frame-name} fi-sist-exec3 = "" and
      (INPUT FRAME {&frame-name} fi-ssist-exec3 <> "" or
       INPUT FRAME {&frame-name} fi-desc-exec3 <> "")) OR
     (INPUT FRAME {&FRAME-NAME} fi-sist-exec3 <> "" AND
      (INPUT FRAME {&FRAME-NAME} fi-ssist-exec3 = "" OR
       INPUT FRAME {&FRAME-NAME} fi-desc-exec3 = "")) THEN DO:
     MESSAGE "N∆o pode haver Sistema sem SubSistema e/ou Descriá∆o." VIEW-AS ALERT-BOX. 
     APPLY 'entry' TO fi-sist-exec3.
     return 'ADM-ERROR':U.
  END.
  IF (INPUT FRAME {&frame-name} fi-sist-exec4 = "" and
      (INPUT FRAME {&frame-name} fi-ssist-exec4 <> "" or
       INPUT FRAME {&frame-name} fi-desc-exec4 <> "")) OR
     (INPUT FRAME {&FRAME-NAME} fi-sist-exec4 <> "" AND
      (INPUT FRAME {&FRAME-NAME} fi-ssist-exec4 = "" OR
       INPUT FRAME {&FRAME-NAME} fi-desc-exec4 = "")) THEN DO:
     MESSAGE "N∆o pode haver Sistema sem SubSistema e/ou Descriá∆o." VIEW-AS ALERT-BOX. 
     APPLY 'entry' TO fi-sist-exec4.
     return 'ADM-ERROR':U.
  END.
  IF (INPUT FRAME {&frame-name} fi-sist-exec5 = "" and
      (INPUT FRAME {&frame-name} fi-ssist-exec5 <> "" or
       INPUT FRAME {&frame-name} fi-desc-exec5 <> "")) OR
     (INPUT FRAME {&FRAME-NAME} fi-sist-exec5 <> "" AND
      (INPUT FRAME {&FRAME-NAME} fi-ssist-exec5 = "" OR
       INPUT FRAME {&FRAME-NAME} fi-desc-exec5 = "")) THEN DO:
     MESSAGE "N∆o pode haver Sistema sem SubSistema e/ou Descriá∆o." VIEW-AS ALERT-BOX. 
     APPLY 'entry' TO fi-sist-exec5.
     return 'ADM-ERROR':U.
  END.


  ASSIGN INPUT FRAME {&frame-name} fi-sist-exec2   
         INPUT FRAME {&frame-name} fi-sist-exec3   
         INPUT FRAME {&frame-name} fi-sist-exec4   
         INPUT FRAME {&frame-name} fi-sist-exec5
         INPUT FRAME {&frame-name} fi-ssist-exec1 
         INPUT FRAME {&frame-name} fi-ssist-exec2 
         INPUT FRAME {&frame-name} fi-ssist-exec3 
         INPUT FRAME {&frame-name} fi-ssist-exec4 
         INPUT FRAME {&frame-name} fi-ssist-exec5
         INPUT FRAME {&frame-name} fi-desc-exec1 
         INPUT FRAME {&frame-name} fi-desc-exec2 
         INPUT FRAME {&frame-name} fi-desc-exec3 
         INPUT FRAME {&frame-name} fi-desc-exec4 
         INPUT FRAME {&FRAME-NAME} fi-desc-exec5.


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

