&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          ems2cad          PROGRESS
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
&Scoped-define EXTERNAL-TABLES repres
&Scoped-define FIRST-EXTERNAL-TABLE repres


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR repres.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS repres.cod-rep 
&Scoped-define ENABLED-TABLES repres
&Scoped-define FIRST-ENABLED-TABLE repres
&Scoped-Define ENABLED-OBJECTS rt-key RECT-1 RECT-2 RECT-3 RECT-4 RECT-5 
&Scoped-Define DISPLAYED-FIELDS repres.cod-rep repres.nome 
&Scoped-define DISPLAYED-TABLES repres
&Scoped-define FIRST-DISPLAYED-TABLE repres
&Scoped-Define DISPLAYED-OBJECTS fi-perc-comis rs-classe fi-dias-tit-venc ~
fi-dias-cheque-dev rs-tp-aplic fi-perc-ir tg-bloqueado 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define List-4 fi-perc-comis rs-classe fi-dias-tit-venc ~
fi-dias-cheque-dev rs-tp-aplic fi-perc-ir tg-bloqueado 

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
DEFINE VARIABLE fi-dias-cheque-dev AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Dias para Cheques Vencidos" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dias-tit-venc AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Dias para T°tulos Vencidos" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-perc-comis AS DECIMAL FORMAT ">>9.99":U INITIAL 0 
     LABEL "% de Comiss∆o" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE VARIABLE fi-perc-ir AS DECIMAL FORMAT ">>9.99":U INITIAL 0 
     LABEL "% IR" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE VARIABLE rs-classe AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Gerente Geral", 1,
"Gerente Loja", 2,
"Pracista", 3,
"Interno", 4,
"Externo", 5
     SIZE 12.72 BY 4.04 NO-UNDO.

DEFINE VARIABLE rs-tp-aplic AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Faturamento Total", 1,
"Faturamento Pr¢prio", 2,
"Faturamento da Hierarquia", 3,
"Faturmaneto OUTLET + Hierarquia", 4
     SIZE 27 BY 2.88 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 95 BY 5.25.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 20 BY 4.5.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 28.86 BY 2.75.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 41 BY 4.5.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 29 BY 1.75.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 95 BY 1.5.

DEFINE VARIABLE tg-bloqueado AS LOGICAL INITIAL no 
     LABEL "Bloqueado" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     repres.cod-rep AT ROW 1.25 COL 16 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 6.86 BY .88
     repres.nome AT ROW 1.25 COL 23.14 COLON-ALIGNED NO-LABEL WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 55 BY .88
     fi-perc-comis AT ROW 3.5 COL 65.29 COLON-ALIGNED WIDGET-ID 34
     rs-classe AT ROW 3.63 COL 6 NO-LABEL WIDGET-ID 8
     fi-dias-tit-venc AT ROW 3.75 COL 43 COLON-ALIGNED WIDGET-ID 22
     fi-dias-cheque-dev AT ROW 4.75 COL 43 COLON-ALIGNED WIDGET-ID 24
     rs-tp-aplic AT ROW 4.75 COL 67.29 NO-LABEL WIDGET-ID 52
     fi-perc-ir AT ROW 6.5 COL 27 COLON-ALIGNED WIDGET-ID 36
     tg-bloqueado AT ROW 6.5 COL 39 WIDGET-ID 30
     " Comiss∆o" VIEW-AS TEXT
          SIZE 7.57 BY .63 AT ROW 2.92 COL 54.43 WIDGET-ID 28
          FGCOLOR 1 FONT 6
     " Outros" VIEW-AS TEXT
          SIZE 8.29 BY .63 AT ROW 2.96 COL 24.72 WIDGET-ID 20
          FGCOLOR 1 FONT 6
     "Aplica % Sobre:" VIEW-AS TEXT
          SIZE 11 BY .54 AT ROW 4.83 COL 55.86 WIDGET-ID 50
     " Classe" VIEW-AS TEXT
          SIZE 7.14 BY .63 AT ROW 2.96 COL 3.57 WIDGET-ID 16
          FGCOLOR 1 FONT 6
     rt-key AT ROW 1 COL 1
     RECT-1 AT ROW 2.75 COL 1 WIDGET-ID 6
     RECT-2 AT ROW 3.25 COL 2 WIDGET-ID 14
     RECT-3 AT ROW 3.25 COL 23.14 WIDGET-ID 18
     RECT-4 AT ROW 3.25 COL 54 WIDGET-ID 26
     RECT-5 AT ROW 6 COL 23 WIDGET-ID 42
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 1 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: mgcom.repres
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
         HEIGHT             = 7
         WIDTH              = 96.43.
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME f-main:SCROLLABLE       = FALSE
       FRAME f-main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fi-dias-cheque-dev IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-dias-tit-venc IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-perc-comis IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-perc-ir IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN repres.nome IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET rs-classe IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR RADIO-SET rs-tp-aplic IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR TOGGLE-BOX tg-bloqueado IN FRAME f-main
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
  {src/adm/template/row-list.i "repres"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "repres"}

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
    
    ASSIGN INPUT FRAME {&FRAME-NAME} rs-classe
           INPUT FRAME {&FRAME-NAME} fi-dias-tit-venc
           INPUT FRAME {&FRAME-NAME} fi-dias-cheque-dev
           INPUT FRAME {&FRAME-NAME} tg-bloqueado
           INPUT FRAME {&FRAME-NAME} fi-perc-comis
           INPUT FRAME {&FRAME-NAME} rs-tp-aplic
           INPUT FRAME {&FRAME-NAME} fi-perc-ir.

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.
    
    /*:T Todos os assignÔs n∆o feitos pelo assign-record devem ser feitos aqui */  
    /* Code placed here will execute AFTER standard behavior.    */

    ASSIGN repres.comis-direta = fi-perc-comis.

    FIND cm-ext-repres WHERE
         cm-ext-repres.cod-rep = repres.cod-rep NO-ERROR.
    IF NOT AVAIL cm-ext-repres THEN DO.
       CREATE cm-ext-repres.
       ASSIGN cm-ext-repres.cod-rep = repres.cod-rep.
    END.
    ASSIGN cm-ext-repres.classe = rs-classe
           cm-ext-repres.tp-aplic = rs-tp-aplic
           cm-ext-repres.dias-tit-venc = fi-dias-tit-venc
           cm-ext-repres.dias-cheque-dev = fi-dias-cheque-dev
           cm-ext-repres.bloqueado = tg-bloqueado
           cm-ext-repres.perc-ir = fi-perc-ir.
    
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
    DISABLE {&list-4} with frame {&frame-name}.

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

    ASSIGN fi-perc-comis = 0
           rs-classe = 1
           rs-tp-aplic = 1
           fi-dias-tit-venc = 0
           fi-dias-cheque-dev = 0
           tg-bloqueado = NO
           fi-perc-ir = 0.

    IF AVAIL repres THEN DO.
       ASSIGN fi-perc-comis = repres.comis-direta.

       FIND cm-ext-repres WHERE
            cm-ext-repres.cod-rep = repres.cod-rep NO-ERROR.
       IF AVAIL cm-ext-repres THEN
          ASSIGN rs-classe = cm-ext-repres.classe  
                 rs-tp-aplic = cm-ext-repres.tp-aplic-comis
                 fi-dias-tit-venc = cm-ext-repres.dias-tit-venc  
                 fi-dias-cheque-dev = cm-ext-repres.dias-cheque-dev 
                 tg-bloqueado = cm-ext-repres.bloqueado 
                 fi-perc-ir = cm-ext-repres.perc-ir.
    END.
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

    IF AVAIL cm-ext-repres THEN DO.
       IF cm-ext-repres.tp-aplic = 3 OR 
          cm-ext-repres.tp-aplic = 4 THEN
          RUN new-state IN THIS-PROCEDURE (INPUT "enable_folder_5").
       ELSE
          RUN new-state IN THIS-PROCEDURE (INPUT "disable_folder_5").
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
    ASSIGN repres.cod-rep:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

    ENABLE {&list-4} with frame {&frame-name}.

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
  {src/adm/template/snd-list.i "repres"}

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

