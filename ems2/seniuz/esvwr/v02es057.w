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
{include/i-prgvrs.i V02ES057 2.04.00.000}

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
DEF BUFFER b-mp-entr-cam FOR mp-entr-cam.
DEF VAR i-nr-cdr       LIKE mp-entr-cam.nr-cdr.
DEF VAR i-hr-entrada   LIKE mp-entr-cam.hr-entrada.
def var v-row-parent as rowid no-undo.
DEF VAR c-container    AS CHARACTER.
DEF VAR c-tipo         AS CHAR FORMAT "X(20)".
DEF NEW GLOBAL SHARED VAR g-tipo-mov AS INT.

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
&Scoped-define EXTERNAL-TABLES mp-entr-cam
&Scoped-define FIRST-EXTERNAL-TABLE mp-entr-cam


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR mp-entr-cam.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS mp-entr-cam.tipo-mov 
&Scoped-define ENABLED-TABLES mp-entr-cam
&Scoped-define FIRST-ENABLED-TABLE mp-entr-cam
&Scoped-Define ENABLED-OBJECTS bt-confirma RECT-4 rt-key 
&Scoped-Define DISPLAYED-FIELDS mp-entr-cam.placa mp-entr-cam.tipo-mov ~
mp-entr-cam.dt-entrada 
&Scoped-define DISPLAYED-TABLES mp-entr-cam
&Scoped-define FIRST-DISPLAYED-TABLE mp-entr-cam
&Scoped-Define DISPLAYED-OBJECTS fi-hr-entrada fi-mod-op-balanca 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-CREATE-FIELDS mp-entr-cam.placa mp-entr-cam.dt-entrada ~
fi-hr-entrada 
&Scoped-define ADM-ASSIGN-FIELDS mp-entr-cam.placa mp-entr-cam.dt-entrada ~
fi-hr-entrada 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
nr-cdr|y|y|espec.mp-entr-cam.nr-cdr
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "nr-cdr",
     Keys-Supplied = "nr-cdr"':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-confirma 
     IMAGE-UP FILE "image/ii-tick2.bmp":U
     LABEL "" 
     SIZE 4 BY 1.13.

DEFINE VARIABLE fi-hr-entrada AS CHARACTER FORMAT "XX:XX":U 
     VIEW-AS FILL-IN 
     SIZE 5.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-mod-op-balanca AS LOGICAL FORMAT "Manual/Automatico":U INITIAL NO 
     LABEL "Modo de Operaá∆o da Balanca" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .79 TOOLTIP "Modo de Operaá∆o da Balanáa"
     FGCOLOR 12  NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 35 BY 1.5.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 88.57 BY 3.42.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     mp-entr-cam.placa AT ROW 1.75 COL 7 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.29 BY .88
     mp-entr-cam.tipo-mov AT ROW 2.88 COL 15.86 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Carga Tecido", 1,
"Descarga Algodao", 2,
"Descarga Outros", 3,
"Carga Outros", 4
          SIZE 62 BY .88 TOOLTIP "Tipo de Operaá∆o do Veiculos na Balanáa"
     mp-entr-cam.dt-entrada AT ROW 1.75 COL 26.72 COLON-ALIGNED
          LABEL "Entrada"
          VIEW-AS FILL-IN 
          SIZE 12 BY .88
     fi-hr-entrada AT ROW 1.75 COL 39 COLON-ALIGNED NO-LABEL
     bt-confirma AT ROW 3.08 COL 83.86
     fi-mod-op-balanca AT ROW 1.58 COL 73.57 COLON-ALIGNED
     RECT-4 AT ROW 1.25 COL 52
     rt-key AT ROW 1.08 COL 1
     "Tipo Movimento:" VIEW-AS TEXT
          SIZE 12 BY .88 AT ROW 2.79 COL 4.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: espec.mp-entr-cam
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
         HEIGHT             = 3.58
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

/* SETTINGS FOR FILL-IN mp-entr-cam.dt-entrada IN FRAME f-main
   NO-ENABLE 1 2 EXP-LABEL                                              */
/* SETTINGS FOR FILL-IN fi-hr-entrada IN FRAME f-main
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN fi-mod-op-balanca IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN mp-entr-cam.placa IN FRAME f-main
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

&Scoped-define SELF-NAME bt-confirma
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-confirma V-table-Win
ON CHOOSE OF bt-confirma IN FRAME f-main
DO:
    IF length(mp-entr-cam.placa:SCREEN-VALUE) <> 8 THEN DO:
       MESSAGE "A Placa n∆o Ç v†lida." VIEW-AS ALERT-BOX. 
       APPLY 'entry' TO mp-entr-cam.placa.
       return 'ADM-ERROR':U.
    END.
    IF INPUT FRAME {&frame-name} mp-entr-cam.dt-entrada = ? THEN DO:
       MESSAGE "A data de entrada n∆o Ç v†lida." VIEW-AS ALERT-BOX. 
       APPLY 'entry' TO mp-entr-cam.dt-entrada.
       return 'ADM-ERROR':U.
    END.
    IF INPUT FRAME {&frame-name} mp-entr-cam.dt-entrada > TODAY THEN DO:
       MESSAGE "A data de entrada n∆o pode ser maior que a data de hoje." VIEW-AS ALERT-BOX. 
       APPLY 'entry' TO mp-entr-cam.dt-entrada.
       return 'ADM-ERROR':U.
    END.
    IF length(fi-hr-entrada:SCREEN-VALUE) <> 5 THEN DO:
       MESSAGE "A hora de entrada n∆o Ç valida." VIEW-AS ALERT-BOX. 
       APPLY 'entry' TO fi-hr-entrada.
       return 'ADM-ERROR':U.
    END.
    FIND FIRST b-mp-entr-cam WHERE b-mp-entr-cam.placa = INPUT FRAME {&FRAME-NAME} mp-entr-cam.placa
                               AND b-mp-entr-cam.dt-saida = ?
                             NO-LOCK NO-ERROR.
    IF AVAIL b-mp-entr-cam THEN DO:
       MESSAGE "H† uma entrada pendente para essa placa:" SKIP
               "Placa: " b-mp-entr-cam.placa 
               " Data: " b-mp-entr-cam.dt-entrada 
               " Hora: " string(b-mp-entr-cam.hr-entrada,"hh:mm") SKIP
               "Essa entrada n∆o ser† aceita!"
               VIEW-AS ALERT-BOX. 
       APPLY 'entry' TO mp-entr-cam.placa.
       return 'ADM-ERROR':U.
    END.

    RUN pi-desabilita-campos IN WIDGET-HANDLE(c-container) (INPUT INT(INPUT FRAME {&FRAME-NAME} mp-entr-cam.tipo-mov)).
    RUN pi-habilita-campos   IN WIDGET-HANDLE(c-container) (INPUT INT(INPUT FRAME {&FRAME-NAME} mp-entr-cam.tipo-mov),
                                                            INPUT YES).
    RUN pi-habilita-folders  IN WIDGET-HANDLE(c-container) (INPUT INT(INPUT FRAME {&FRAME-NAME} mp-entr-cam.tipo-mov)).
    ASSIGN g-tipo-mov = INT(INPUT FRAME {&FRAME-NAME} mp-entr-cam.tipo-mov).
    ASSIGN bt-confirma:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           mp-entr-cam.tipo-mov:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mp-entr-cam.tipo-mov
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mp-entr-cam.tipo-mov V-table-Win
ON ENTRY OF mp-entr-cam.tipo-mov IN FRAME f-main /* tipo-mov */
DO:
  ASSIGN bt-confirma:SENSITIVE IN FRAME {&FRAME-NAME} = yes.
  APPLY 'entry' TO bt-confirma.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mp-entr-cam.tipo-mov V-table-Win
ON VALUE-CHANGED OF mp-entr-cam.tipo-mov IN FRAME f-main /* tipo-mov */
DO:
   ASSIGN bt-confirma:SENSITIVE IN FRAME {&FRAME-NAME} = yes.
   APPLY 'entry' TO bt-confirma.

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
  FIND FIRST param-estoq NO-LOCK NO-ERROR.

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
    WHEN 'nr-cdr':U THEN
       {src/adm/template/find-tbl.i
           &TABLE = mp-entr-cam
           &WHERE = "WHERE mp-entr-cam.nr-cdr eq INTEGER(key-value)"
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
  {src/adm/template/row-list.i "mp-entr-cam"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "mp-entr-cam"}

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
  IF c-seg-usuario = "balanca" THEN
     ASSIGN fi-mod-op-balanca:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(mp-param.mod-op-balanca).
  ELSE
     ASSIGN fi-mod-op-balanca:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'Manual'.

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
  IF adm-new-record = YES THEN DO: /* INCLUIR */
      ASSIGN mp-entr-cam.dt-entrada:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY).
      RUN esapi/cv-hora.p (INPUT STRING(TIME,"hh:mm:ss"), OUTPUT i-hr-entrada).
      ASSIGN fi-hr-entrada:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(i-hr-entrada,"hh:mm").
  END.


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
  
  ASSIGN INPUT FRAME {&FRAME-NAME} fi-hr-entrada.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
  if RETURN-VALUE = 'ADM-ERROR':U then 
      return 'ADM-ERROR':U.
    
  /*:T Todos os assignÔs n∆o feitos pelo assign-record devem ser feitos aqui */  
  /* Code placed here will execute AFTER standard behavior.    */

  RUN esapi/cv-hora.p (INPUT STRING( fi-hr-entrada,"xx:xx:xx"), OUTPUT i-hr-entrada).

  IF adm-new-record = YES THEN DO:
     FIND LAST b-mp-entr-cam USE-INDEX indice4 NO-LOCK NO-ERROR.
     IF AVAIL b-mp-entr-cam THEN
        ASSIGN i-nr-cdr = b-mp-entr-cam.nr-cdr + 1.
     ELSE
       ASSIGN i-nr-cdr = 1.
     ASSIGN mp-entr-cam.nr-cdr      = i-nr-cdr
            mp-entr-cam.hr-entrada  = i-hr-entrada.
  END.
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
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ).
    
    /* Code placed here will execute AFTER standard behavior.    */
    ASSIGN bt-confirma:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

    FIND FIRST mp-param NO-LOCK NO-ERROR.
    IF c-seg-usuario = "balanca" THEN
       ASSIGN fi-mod-op-balanca:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(mp-param.mod-op-balanca). 
    ELSE
       ASSIGN fi-mod-op-balanca:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Manual". 

    IF AVAIL mp-entr-cam THEN DO.
       ASSIGN fi-hr-entrada:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(mp-entr-cam.hr-entrada,"HH:MM").
     /*         fi-mod-op-balanca:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(mp-param.mod-op-balanca).  */
       IF mp-entr-cam.dt-saida = ? THEN
          RUN pi-habilita-p-cadsim IN WIDGET-HANDLE(c-container) (INPUT YES).
       ELSE
           RUN pi-habilita-p-cadsim IN WIDGET-HANDLE(c-container) (INPUT NO).

       RUN pi-habilita-folders IN WIDGET-HANDLE(c-container) (INPUT mp-entr-cam.tipo-mov).
       ASSIGN g-tipo-mov = mp-entr-cam.tipo-mov.
    END.
    IF c-container <> "" THEN DO:
        RUN pi-desabilita-folders  IN WIDGET-HANDLE(c-container).
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
       ASSIGN mp-entr-cam.tipo-mov:SENSITIVE IN FRAME {&FRAME-NAME} = NO
              bt-confirma:SENSITIVE IN FRAME {&FRAME-NAME}          = NO.   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-end-update V-table-Win 
PROCEDURE local-end-update :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'end-update':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  RUN pi-refresh IN WIDGET-HANDLE(c-container).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize V-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN get-link-handle IN adm-broker-hdl ( INPUT THIS-PROCEDURE,
                                          INPUT "CONTAINER",
                                          OUTPUT c-container). 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-habilita-campos V-table-Win 
PROCEDURE pi-habilita-campos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN mp-entr-cam.tipo-mov:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-tipo-mov V-table-Win 
PROCEDURE pi-tipo-mov :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER i-tipo-mov AS INT.

  IF AVAIL mp-entr-cam THEN
     ASSIGN i-tipo-mov = mp-entr-cam.tipo-mov.

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
  {src/adm/template/sndkycas.i "nr-cdr" "mp-entr-cam" "nr-cdr"}

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
  {src/adm/template/snd-list.i "mp-entr-cam"}

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

