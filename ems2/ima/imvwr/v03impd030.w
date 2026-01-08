&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          ems2cad          PROGRESS
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
{include/i-prgvrs.i V03IMPD030 1.00.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
&Scop adm-attribute-dlg support/viewerd.w

/* global variable definitions */
DEFINE NEW GLOBAL SHARED VAR i-cod-emitente LIKE emitente.cod-emitente NO-UNDO. 
DEFINE NEW GLOBAL SHARED VAR h-impd030 AS HANDLE NO-UNDO.

/* Parameters Definitions ---                                           */
DEF BUFFER b-emitente FOR emitente.
DEF BUFFER b2-emitente FOR emitente.

/* Local Variable Definitions ---                                       */
def var v-row-parent as rowid no-undo.
{include/i-vrtab.i emitente}

DEF VAR i-ct AS INTEGER.
DEF VAR c-lst-coligadas AS CHAR.

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
&Scoped-define EXTERNAL-TABLES emitente
&Scoped-define FIRST-EXTERNAL-TABLE emitente


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR emitente.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS emitente.cod-emitente emitente.nome-emit ~
emitente.data-implant 
&Scoped-define ENABLED-TABLES emitente
&Scoped-define FIRST-ENABLED-TABLE emitente
&Scoped-Define ENABLED-OBJECTS rt-key cb-coligada 
&Scoped-Define DISPLAYED-FIELDS emitente.cod-emitente emitente.nome-emit ~
emitente.data-implant 
&Scoped-define DISPLAYED-TABLES emitente
&Scoped-define FIRST-DISPLAYED-TABLE emitente
&Scoped-Define DISPLAYED-OBJECTS cb-coligada 

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
DEFINE VARIABLE cb-coligada AS CHARACTER FORMAT "X(256)":U 
     LABEL "Coligadas" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 21 BY 1 NO-UNDO.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 110.57 BY 1.25.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     emitente.cod-emitente AT ROW 1.25 COL 6.86 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.14 BY .88
     emitente.nome-emit AT ROW 1.25 COL 17.57 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 40.43 BY .88
     cb-coligada AT ROW 1.25 COL 68 COLON-ALIGNED WIDGET-ID 2
     emitente.data-implant AT ROW 1.25 COL 98.72 COLON-ALIGNED
          LABEL "Dt Implant"
          VIEW-AS FILL-IN 
          SIZE 10.29 BY .88
     rt-key AT ROW 1.04 COL 1.57
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: mgadm.emitente
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
         HEIGHT             = 1.33
         WIDTH              = 111.14.
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

/* SETTINGS FOR FILL-IN emitente.data-implant IN FRAME f-main
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

&Scoped-define SELF-NAME cb-coligada
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-coligada V-table-Win
ON VALUE-CHANGED OF cb-coligada IN FRAME f-main /* Coligadas */
DO:
   FIND b-emitente WHERE
        b-emitente.nome-abrev = ENTRY(2,SELF:SCREEN-VALUE,"-") NO-LOCK NO-ERROR.
   IF AVAIL b-emitente THEN DO.
      ASSIGN emitente.cod-emitente:SCREEN-VALUE = STRING(b-emitente.cod-emitente)
             emitente.nome-emit:SCREEN-VALUE = b-emitente.nome-emit.

      RUN pi-monta-coligada (INPUT b-emitente.cod-emit).

      ASSIGN i-cod-emitente = b-emitente.cod-emitente.
      RUN pi-carrega-emitente IN h-impd030.
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
  {src/adm/template/row-list.i "emitente"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "emitente"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-row-available V-table-Win 
PROCEDURE local-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-available':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF AVAIL emitente THEN DO.
     ASSIGN gr-emitente = ROWID(emitente).

     RUN pi-monta-coligada (INPUT emitente.cod-emitente).
  END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-monta-coligada V-table-Win 
PROCEDURE pi-monta-coligada :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     DEF INPUT PARAMETER p-cod-emit LIKE emitente.cod-emit.

     FIND b2-emitente WHERE
          b2-emitente.cod-emit = p-cod-emit NO-LOCK NO-ERROR.

     ASSIGN cb-coligada:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
     ASSIGN c-lst-coligadas = ' '.

     // Emitente tem coligada...
     FIND ext-emitente WHERE
          ext-emitente.cod-emit = b2-emitente.cod-emitente NO-LOCK NO-ERROR.
     IF AVAIL ext-emitente AND
        ext-emitente.coligada <> '' THEN DO.

        DO i-ct = 1 TO NUM-ENTRIES(ext-emitente.coligada).
           FIND b-emitente WHERE
                b-emitente.nome-abrev = ENTRY(i-ct,ext-emitente.coligada)
                NO-LOCK NO-ERROR.
           IF NOT AVAIL b-emitente THEN
              FIND b-emitente WHERE
                   b-emitente.cod-emitente = INTEGER(ENTRY(i-ct,ext-emitente.coligada))
                   NO-LOCK NO-ERROR.

           IF AVAIL b-emitente AND NOT c-lst-coligadas MATCHES "*" + b-emitente.nome-abrev + "*" THEN
              ASSIGN c-lst-coligadas = IF c-lst-coligadas = ' '
                                       THEN STRING(b-emitente.cod-emit) + "-" + b-emitente.nome-abrev
                                       ELSE c-lst-coligadas + "," + STRING(b-emitente.cod-emit) + "-" + b-emitente.nome-abrev.
        END.
     END.
     
     // Verifica se Emitente esta como oligada de Outro Emitente
     FOR EACH ext-emitente WHERE
              ext-emitente.coligada <> '' NO-LOCK.

         IF LOOKUP(b2-emitente.nome-abrev,ext-emitente.coligada) > 0 THEN DO.
            FIND b-emitente WHERE
                 b-emitente.cod-emitente = ext-emitente.cod-emitente NO-LOCK NO-ERROR.
            IF AVAIL b-emitente AND NOT c-lst-coligadas MATCHES "*" + b-emitente.nome-abrev + "*" THEN
               ASSIGN c-lst-coligadas = IF c-lst-coligadas = ' '
                                        THEN STRING(b-emitente.cod-emit) + "-" + b-emitente.nome-abrev
                                        ELSE c-lst-coligadas + "," + STRING(b-emitente.cod-emit) + "-" + b-emitente.nome-abrev.
         END.
         ELSE IF LOOKUP(STRING(b2-emitente.cod-emit),ext-emitente.coligada) > 0 THEN DO.
            FIND b-emitente WHERE
                 b-emitente.cod-emitente = ext-emitente.cod-emitente NO-LOCK NO-ERROR.
            IF AVAIL b-emitente AND NOT c-lst-coligadas MATCHES "*" + b-emitente.nome-abrev + "*" THEN
               ASSIGN c-lst-coligadas = IF c-lst-coligadas = ' '
                                        THEN STRING(b-emitente.cod-emit) + "-" + b-emitente.nome-abrev
                                        ELSE c-lst-coligadas + "," + STRING(b-emitente.cod-emit) + "-" + b-emitente.nome-abrev.
         END.
     END.
     
     IF c-lst-coligadas = " " THEN
        ASSIGN cb-coligada:SENSITIVE = NO.

     &IF "{&FNC_MULTI_IDIOMA}" = "Yes" &THEN
        RUN utp/ut-lstit.p (INPUT-OUTPUT c-lst-coligadas).
        ASSIGN cb-coligada:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = c-lst-coligadas.
     &ELSE
        ASSIGN cb-coligada:LIST-ITEMS IN FRAME {&FRAME-NAME} = c-lst-coligadas.
     &ENDIF

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
  {src/adm/template/snd-list.i "emitente"}

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

