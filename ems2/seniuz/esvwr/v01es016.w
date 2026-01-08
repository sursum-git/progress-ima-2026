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
{include/i-prgvrs.i V01ES016 2.04.00.000}

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
DEF TEMP-TABLE tt-etq-reservadas LIKE ob-etiqueta
    FIELD tp-acao                AS CHAR.

DEF TEMP-TABLE tt-item-disp LIKE ped-reserva-it.

/* Local Variable Definitions ---                                       */
def var v-row-parent  AS rowid no-undo.
DEF VAR c-container   AS CHAR.
DEF VAR i-hr-reserva  LIKE ped-reserva.hr-reserva.
DEF VAR i-num-reserva LIKE ped-reserva.num-reserva.
DEF VAR i-nr-sequencia AS INT.

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
&Scoped-define EXTERNAL-TABLES espec.ped-reserva
&Scoped-define FIRST-EXTERNAL-TABLE espec.ped-reserva


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR espec.ped-reserva.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS espec.ped-reserva.usuario 
&Scoped-define ENABLED-TABLES espec.ped-reserva
&Scoped-define FIRST-ENABLED-TABLE espec.ped-reserva
&Scoped-Define ENABLED-OBJECTS rt-key 
&Scoped-Define DISPLAYED-FIELDS espec.ped-reserva.num-reserva ~
espec.ped-reserva.situacao espec.ped-reserva.dt-reserva ~
espec.ped-reserva.usuario 
&Scoped-define DISPLAYED-TABLES espec.ped-reserva
&Scoped-define FIRST-DISPLAYED-TABLE espec.ped-reserva
&Scoped-Define DISPLAYED-OBJECTS fi-hr-reserva 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-ASSIGN-FIELDS espec.ped-reserva.num-reserva ~
espec.ped-reserva.dt-reserva espec.ped-reserva.usuario 

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
DEFINE VARIABLE fi-hr-reserva AS CHARACTER FORMAT "X(8)" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 97 BY 2.5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     espec.ped-reserva.num-reserva AT ROW 1.25 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.43 BY .88
          FONT 6
     espec.ped-reserva.situacao AT ROW 1.25 COL 66.57 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Aberta":U, 1,
"Atendida":U, 2,
"Encerrada":U, 3
          SIZE 29.43 BY .88
     espec.ped-reserva.dt-reserva AT ROW 2.25 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.57 BY .88
     fi-hr-reserva AT ROW 2.25 COL 34 COLON-ALIGNED NO-LABEL
     espec.ped-reserva.usuario AT ROW 2.25 COL 64 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16.14 BY .88
     "Situaá∆o:" VIEW-AS TEXT
          SIZE 6.29 BY .88 AT ROW 1.25 COL 59.29
     rt-key AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: espec.ped-reserva
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
         HEIGHT             = 2.58
         WIDTH              = 97.
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

/* SETTINGS FOR FILL-IN espec.ped-reserva.dt-reserva IN FRAME f-main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi-hr-reserva IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN espec.ped-reserva.num-reserva IN FRAME f-main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR RADIO-SET espec.ped-reserva.situacao IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN espec.ped-reserva.usuario IN FRAME f-main
   2                                                                    */
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
  {src/adm/template/row-list.i "espec.ped-reserva"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "espec.ped-reserva"}

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

   ASSIGN i-num-reserva = NEXT-VALUE(seq-ped-reserva).

   ASSIGN ped-reserva.usuario:SENSITIVE IN FRAME {&FRAME-NAME}  = NO.

   ASSIGN ped-reserva.num-reserva:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(i-num-reserva)
          ped-reserva.dt-reserva:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = STRING(TODAY)
          fi-hr-reserva:SCREEN-VALUE IN FRAME {&FRAME-NAME}           = STRING(TIME,"HH:MM:SS")
          ped-reserva.usuario:SCREEN-VALUE IN FRAME {&FRAME-NAME}     = c-seg-usuario.

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
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    IF RETURN-VALUE = 'ADM-ERROR':U THEN 
       RETURN 'ADM-ERROR':U.
    
    /*:T Todos os assignÔs n∆o feitos pelo assign-record devem ser feitos aqui */  
    /* Code placed here will execute AFTER standard behavior.    */
    
    ASSIGN ped-reserva.hr-reserva   = TIME.

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

  IF AVAIL ped-reserva THEN DO:
     RUN pi-mostra-cli IN WIDGET-HANDLE(c-container) (INPUT ped-reserva.num-reserva).
     ASSIGN fi-hr-reserva:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ped-reserva.hr-reserva, "HH:MM:SS").
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit V-table-Win 
PROCEDURE local-exit :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'exit':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

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

  RUN get-link-handle IN adm-broker-hdl (INPUT  THIS-PROCEDURE,
                                         INPUT  "CONTAINER",
                                         OUTPUT c-container).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-add-record V-table-Win 
PROCEDURE pi-add-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER p-acao AS CHAR.

    IF p-acao = 'Inc' THEN 
       RUN local-add-record.
    ELSE
       ASSIGN i-num-reserva = ped-reserva.num-reserva.

    RUN pi-monta-itens IN WIDGET-HANDLE(c-container) (INPUT i-num-reserva).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-grava-dados V-table-Win 
PROCEDURE pi-grava-dados :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER TABLE FOR tt-etq-reservadas.  
    DEF INPUT PARAMETER TABLE FOR tt-item-disp.  
    DEF INPUT PARAMETER p-tp-acao       AS CHAR.
    DEF INPUT PARAMETER p-cod-emitente  AS INT.
    DEF INPUT PARAMETER p-cod-rep       AS INT.
    DEF INPUT PARAMETER p-dt-validade   AS DATE.
    DEF INPUT PARAMETER p-nr-sequencia  AS INT.
    DEF INPUT PARAMETER p-estab         AS CHAR.
   
    IF p-tp-acao = "Inc" THEN
       RUN local-assign-record.
    ELSE 
       FIND CURRENT ped-reserva NO-ERROR.
    
    ASSIGN ped-reserva.cod-emitente = p-cod-emitente
           ped-reserva.cod-rep      = p-cod-rep
           ped-reserva.cod-estabel  = p-estab
           ped-reserva.dt-validade  = p-dt-validade.

    /* Grava Item Reservado */
    IF p-tp-acao BEGINS 'Inc'  THEN DO.
       FIND tt-item-disp WHERE
            tt-item-disp.nr-sequencia = p-nr-sequencia NO-LOCK NO-ERROR.

       FIND ped-reserva-it WHERE
            ped-reserva-it.num-reserva = ped-reserva.num-reserva AND
            ped-reserva-it.nr-sequencia = p-nr-sequencia EXCLUSIVE-LOCK NO-ERROR.

       IF p-tp-acao = 'IncItem' AND
          AVAIL ped-reserva-it THEN DO.
          REPEAT WHILE AVAIL ped-reserva-it.
              ASSIGN p-nr-sequencia = p-nr-sequencia + 10.

              FIND ped-reserva-it WHERE
                   ped-reserva-it.num-reserva = ped-reserva.num-reserva AND
                   ped-reserva-it.nr-sequencia = p-nr-sequencia EXCLUSIVE-LOCK NO-ERROR.
          END.
       END.

       IF NOT AVAIL ped-reserva-it THEN DO.
          CREATE ped-reserva-it.
          ASSIGN ped-reserva-it.num-reserva  = INPUT FRAME {&FRAME-NAME} ped-reserva.num-reserva
                 ped-reserva-it.nr-sequencia = p-nr-sequencia
                 ped-reserva-it.it-codigo    = tt-item-disp.it-codigo
                 ped-reserva-it.cod-refer    = tt-item-disp.cod-refer
                 ped-reserva-it.nr-lote      = tt-item-disp.nr-lote
                 ped-reserva-it.corte-comerc = tt-item-disp.corte-comerc.
       END.
    END.

    /* Grava Etiquetas Reservadas */
    FOR EACH tt-etq-reservadas.
        CASE tt-etq-reservadas.tp-acao.
            WHEN 'Inc' THEN DO.
                CREATE ped-reserva-etq.
                ASSIGN ped-reserva-etq.num-reserva  = ped-reserva.num-reserva
                       ped-reserva-etq.nr-sequencia = p-nr-sequencia
                       ped-reserva-etq.num-etiqueta = tt-etq-reservadas.num-etiqueta.

                FIND ob-etiqueta WHERE
                     ob-etiqueta.cod-estabel  = ped-reserva.cod-estabel AND
                     ob-etiqueta.num-etiqueta = ped-reserva-etq.num-etiqueta NO-ERROR.
                ASSIGN ob-etiqueta.situacao = 4.
            END.
            WHEN 'Del' THEN DO.
                FIND ped-reserva-etq WHERE
                     ped-reserva-etq.num-reserva = ped-reserva.num-reserva AND
                     ped-reserva-etq.nr-sequencia = p-nr-sequencia AND
                     ped-reserva-etq.num-etiqueta = tt-etq-reservadas.num-etiqueta
                     EXCLUSIVE-LOCK NO-ERROR.
                IF AVAIL ped-reserva-etq THEN DO.
                   FIND ob-etiqueta WHERE
                        ob-etiqueta.cod-estabel  = ped-reserva.cod-estabel AND
                        ob-etiqueta.num-etiqueta = ped-reserva-etq.num-etiqueta NO-ERROR.
                   ASSIGN ob-etiqueta.situacao = 3.

                   DELETE ped-reserva-etq.
                END.
            END.
        END CASE.
    END.

    IF p-tp-acao = 'Mod' THEN DO: /* Verifique se deleta o Item / Reserva */
       FIND ped-reserva-it WHERE
            ped-reserva-it.num-reserva = ped-reserva.num-reserva AND
            ped-reserva-it.nr-sequencia = p-nr-sequencia EXCLUSIVE-LOCK NO-ERROR.

       FIND FIRST ped-reserva-etq OF ped-reserva-it NO-LOCK NO-ERROR.
       IF NOT AVAIL ped-reserva-etq THEN DO.
          DELETE ped-reserva-it.

          FIND FIRST ped-reserva-it OF ped-reserva NO-LOCK NO-ERROR.
          IF NOT AVAIL ped-reserva-it THEN  DO.
             FIND CURRENT ped-reserva NO-ERROR.
             DELETE ped-reserva.
          END.
       END.
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
  {src/adm/template/snd-list.i "espec.ped-reserva"}

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

