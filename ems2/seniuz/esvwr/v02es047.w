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

DEF VAR c-cores AS CHAR INIT "Vermelha,12,Amarela,14,Azul,9,Verde,2,Preta,0,Laranja,16".
DEF VAR i-cor   LIKE ordem-benefic.cor-etiqueta.
DEF VAR i-ct    AS   INT.

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
&Scoped-define EXTERNAL-TABLES ordem-benefic
&Scoped-define FIRST-EXTERNAL-TABLE ordem-benefic


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR ordem-benefic.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS ordem-benefic.nr-carro ~
ordem-benefic.it-codigo ordem-benefic.cod-refer ordem-benefic.quantidade ~
ordem-benefic.observacao 
&Scoped-define ENABLED-TABLES ordem-benefic
&Scoped-define FIRST-ENABLED-TABLE ordem-benefic
&Scoped-Define ENABLED-OBJECTS RECT-1 rt-key rt-mold 
&Scoped-Define DISPLAYED-FIELDS ordem-benefic.responsavel ~
ordem-benefic.nr-ob ordem-benefic.dt-ob ordem-benefic.nr-carro ~
ordem-benefic.it-codigo ordem-benefic.cod-refer ordem-benefic.quantidade ~
ordem-benefic.un ordem-benefic.observacao 
&Scoped-define DISPLAYED-TABLES ordem-benefic
&Scoped-define FIRST-DISPLAYED-TABLE ordem-benefic
&Scoped-Define DISPLAYED-OBJECTS fi-desc-refer fi-desc-item fi-cor ~
fi-desc-cor fi-acondic1 fi-qt-acondic1 fi-acondic2 fi-qt-acondic2 ~
fi-acondic3 fi-qt-acondic3 fi-acondic4 fi-qt-acondic4 fi-acondic5 ~
fi-qt-acondic5 fi-acondic6 fi-qt-acondic6 fi-acondic7 fi-qt-acondic7 ~
fi-acondic8 fi-qt-acondic8 fi-acondic9 fi-qt-acondic9 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-CREATE-FIELDS ordem-benefic.nr-ob ordem-benefic.dt-ob ~
fi-desc-cor 
&Scoped-define ADM-ASSIGN-FIELDS ordem-benefic.responsavel ~
ordem-benefic.nr-ob ordem-benefic.dt-ob ordem-benefic.un 
&Scoped-define ADM-MODIFY-FIELDS fi-desc-cor 
&Scoped-define List-4 fi-desc-cor fi-acondic1 fi-qt-acondic1 fi-acondic2 ~
fi-qt-acondic2 fi-acondic3 fi-qt-acondic3 fi-acondic4 fi-qt-acondic4 ~
fi-acondic5 fi-qt-acondic5 fi-acondic6 fi-qt-acondic6 fi-acondic7 ~
fi-qt-acondic7 fi-acondic8 fi-qt-acondic8 fi-acondic9 fi-qt-acondic9 

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
DEFINE VARIABLE fi-acondic1 AS CHARACTER FORMAT "X(8)" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88.

DEFINE VARIABLE fi-acondic2 AS CHARACTER FORMAT "X(8)" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88.

DEFINE VARIABLE fi-acondic3 AS CHARACTER FORMAT "X(8)" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88.

DEFINE VARIABLE fi-acondic4 AS CHARACTER FORMAT "X(8)" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88.

DEFINE VARIABLE fi-acondic5 AS CHARACTER FORMAT "X(8)" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88.

DEFINE VARIABLE fi-acondic6 AS CHARACTER FORMAT "X(8)" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88.

DEFINE VARIABLE fi-acondic7 AS CHARACTER FORMAT "X(8)" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88.

DEFINE VARIABLE fi-acondic8 AS CHARACTER FORMAT "X(8)" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88.

DEFINE VARIABLE fi-acondic9 AS CHARACTER FORMAT "X(8)" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88.

DEFINE VARIABLE fi-cor AS CHARACTER FORMAT "X(256)":U 
     LABEL "Cor da Etiqueta" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-cor AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 17.29 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-item AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 49.43 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-refer AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 31.29 BY .88 NO-UNDO.

DEFINE VARIABLE fi-qt-acondic1 AS INTEGER FORMAT ">>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88.

DEFINE VARIABLE fi-qt-acondic2 AS INTEGER FORMAT ">>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88.

DEFINE VARIABLE fi-qt-acondic3 AS INTEGER FORMAT ">>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88.

DEFINE VARIABLE fi-qt-acondic4 AS INTEGER FORMAT ">>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88.

DEFINE VARIABLE fi-qt-acondic5 AS INTEGER FORMAT ">>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88.

DEFINE VARIABLE fi-qt-acondic6 AS INTEGER FORMAT ">>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88.

DEFINE VARIABLE fi-qt-acondic7 AS INTEGER FORMAT ">>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88.

DEFINE VARIABLE fi-qt-acondic8 AS INTEGER FORMAT ">>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88.

DEFINE VARIABLE fi-qt-acondic9 AS INTEGER FORMAT ">>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 22.57 BY 9.75.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 88.57 BY 2.33.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 88.57 BY 11.75.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     ordem-benefic.responsavel AT ROW 2.25 COL 48 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 21 BY .88
     fi-desc-refer AT ROW 4.75 COL 31.72 COLON-ALIGNED NO-LABEL
     ordem-benefic.nr-ob AT ROW 1.17 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY .88
     ordem-benefic.dt-ob AT ROW 1.17 COL 48 COLON-ALIGNED
          LABEL "Data de Emiss∆o"
          VIEW-AS FILL-IN 
          SIZE 11 BY .88
     ordem-benefic.nr-carro AT ROW 2.25 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5 BY .88
     ordem-benefic.it-codigo AT ROW 3.75 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY .88
     fi-desc-item AT ROW 3.75 COL 35.57 COLON-ALIGNED NO-LABEL
     ordem-benefic.cod-refer AT ROW 4.75 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.14 BY .88
     ordem-benefic.quantidade AT ROW 5.75 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY .88
     ordem-benefic.un AT ROW 5.75 COL 31.57 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3 BY .88
     fi-cor AT ROW 6.75 COL 19 COLON-ALIGNED NO-TAB-STOP 
     fi-desc-cor AT ROW 6.75 COL 23.43 COLON-ALIGNED NO-LABEL
     ordem-benefic.observacao AT ROW 8.75 COL 2.14 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 62.86 BY 6.25
          FGCOLOR 12 FONT 10
     fi-acondic1 AT ROW 5.75 COL 65.29 COLON-ALIGNED NO-LABEL
     fi-qt-acondic1 AT ROW 5.75 COL 80 COLON-ALIGNED NO-LABEL
     fi-acondic2 AT ROW 6.75 COL 65.29 COLON-ALIGNED NO-LABEL
     fi-qt-acondic2 AT ROW 6.75 COL 80 COLON-ALIGNED NO-LABEL
     fi-acondic3 AT ROW 7.75 COL 65.29 COLON-ALIGNED NO-LABEL
     fi-qt-acondic3 AT ROW 7.75 COL 80 COLON-ALIGNED NO-LABEL
     fi-acondic4 AT ROW 8.75 COL 65.29 COLON-ALIGNED NO-LABEL
     fi-qt-acondic4 AT ROW 8.75 COL 80 COLON-ALIGNED NO-LABEL
     fi-acondic5 AT ROW 9.75 COL 65.29 COLON-ALIGNED NO-LABEL
     fi-qt-acondic5 AT ROW 9.75 COL 80 COLON-ALIGNED NO-LABEL
     fi-acondic6 AT ROW 10.75 COL 65.29 COLON-ALIGNED NO-LABEL
     fi-qt-acondic6 AT ROW 10.75 COL 80 COLON-ALIGNED NO-LABEL
     fi-acondic7 AT ROW 11.75 COL 65.29 COLON-ALIGNED NO-LABEL
     fi-qt-acondic7 AT ROW 11.75 COL 80 COLON-ALIGNED NO-LABEL
     fi-acondic8 AT ROW 12.75 COL 65.29 COLON-ALIGNED NO-LABEL
     fi-qt-acondic8 AT ROW 12.75 COL 80 COLON-ALIGNED NO-LABEL
     fi-acondic9 AT ROW 13.75 COL 65.29 COLON-ALIGNED NO-LABEL
     fi-qt-acondic9 AT ROW 13.75 COL 80 COLON-ALIGNED NO-LABEL
     RECT-1 AT ROW 5.25 COL 66
     rt-key AT ROW 1 COL 1
     rt-mold AT ROW 3.5 COL 1
     "Observaá‰es" VIEW-AS TEXT
          SIZE 9.57 BY .75 AT ROW 7.88 COL 2.43
     " Acondicionamentos" VIEW-AS TEXT
          SIZE 14.86 BY .75 AT ROW 4.88 COL 67
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: espec.ordem-benefic
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
         HEIGHT             = 14.38
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

/* SETTINGS FOR FILL-IN ordem-benefic.dt-ob IN FRAME f-main
   NO-ENABLE 1 2 EXP-LABEL                                              */
/* SETTINGS FOR FILL-IN fi-acondic1 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-acondic2 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-acondic3 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-acondic4 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-acondic5 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-acondic6 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-acondic7 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-acondic8 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-acondic9 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-cor IN FRAME f-main
   NO-ENABLE                                                            */
ASSIGN 
       fi-cor:READ-ONLY IN FRAME f-main        = TRUE.

/* SETTINGS FOR FILL-IN fi-desc-cor IN FRAME f-main
   NO-ENABLE 1 3 4                                                      */
/* SETTINGS FOR FILL-IN fi-desc-item IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-refer IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qt-acondic1 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-qt-acondic2 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-qt-acondic3 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-qt-acondic4 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-qt-acondic5 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-qt-acondic6 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-qt-acondic7 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-qt-acondic8 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-qt-acondic9 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN ordem-benefic.nr-ob IN FRAME f-main
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN ordem-benefic.responsavel IN FRAME f-main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN ordem-benefic.un IN FRAME f-main
   NO-ENABLE 2                                                          */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-main
/* Query rebuild information for FRAME f-main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME f-main */
&ANALYZE-RESUME

 


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
  {src/adm/template/row-list.i "ordem-benefic"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "ordem-benefic"}

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
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    IF AVAIL ordem-benefic THEN DO.
       FIND item WHERE
            item.it-codigo = ordem-benefic.it-codigo NO-LOCK NO-ERROR.
       IF AVAIL item THEN
          ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.

       FIND referencia WHERE
            referencia.cod-refer = ordem-benefic.cod-refer NO-LOCK NO-ERROR.
       IF AVAIL referencia THEN
          ASSIGN fi-desc-refer:SCREEN-VALUE IN FRAME {&FRAME-NAME} = referencia.descricao.

       RUN pi-cor (INPUT ordem-benefic.cor-etiqueta).

       ASSIGN i-ct = 1.
       FOR EACH ob-acondic OF ordem-benefic NO-LOCK.
           CASE i-ct:
               WHEN 1 THEN ASSIGN fi-acondic1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ob-acondic.acondic
                                  fi-qt-acondic1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ob-acondic.qtd-prevista).
               WHEN 2 THEN ASSIGN fi-acondic2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ob-acondic.acondic
                                  fi-qt-acondic2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ob-acondic.qtd-prevista).
               WHEN 3 THEN ASSIGN fi-acondic3:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ob-acondic.acondic
                                  fi-qt-acondic3:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ob-acondic.qtd-prevista).
               WHEN 4 THEN ASSIGN fi-acondic4:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ob-acondic.acondic
                                  fi-qt-acondic4:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ob-acondic.qtd-prevista).
               WHEN 5 THEN ASSIGN fi-acondic5:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ob-acondic.acondic
                                  fi-qt-acondic5:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ob-acondic.qtd-prevista).
               WHEN 6 THEN ASSIGN fi-acondic6:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ob-acondic.acondic
                                  fi-qt-acondic6:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ob-acondic.qtd-prevista).
               WHEN 7 THEN ASSIGN fi-acondic7:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ob-acondic.acondic
                                  fi-qt-acondic7:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ob-acondic.qtd-prevista).
               WHEN 8 THEN ASSIGN fi-acondic8:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ob-acondic.acondic
                                  fi-qt-acondic8:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ob-acondic.qtd-prevista).
               WHEN 9 THEN ASSIGN fi-acondic9:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ob-acondic.acondic
                                  fi-qt-acondic9:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ob-acondic.qtd-prevista).
           END CASE.
           ASSIGN i-ct = i-ct + 1.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cor V-table-Win 
PROCEDURE pi-cor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER p-cor AS INT.

  ASSIGN fi-desc-cor:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(LOOKUP(STRING(p-cor),c-cores) - 1,c-cores).
  ASSIGN fi-desc-cor:FGCOLOR = p-cor.

  ASSIGN fi-cor:BGCOLOR = p-cor.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-get-record V-table-Win 
PROCEDURE pi-get-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER p-row-table AS ROWID.
    ASSIGN p-row-table = ROWID(ordem-benefic).
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
  {src/adm/template/snd-list.i "ordem-benefic"}

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

