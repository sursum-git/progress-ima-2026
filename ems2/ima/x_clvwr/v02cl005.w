&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          ems2med          PROGRESS
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

DEF BUFFER bf-etiqueta FOR bc-etiqueta.
DEF BUFFER bf-etiq2    FOR bc-etiqueta.

DEF VAR i-seq   AS INTEGER NO-UNDO.
DEF VAR i-cont  AS INTEGER NO-UNDO.

DEF VAR l-primeiro  AS LOGICAL INITIAL YES  NO-UNDO.
DEF VAR i-qtde-orig AS INTEGER              NO-UNDO.

DEF NEW GLOBAL SHARED VAR h-viewer AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-browse AS HANDLE NO-UNDO.

DEF NEW GLOBAL SHARED VAR i-cod-emit-aux  LIKE emitente.cod-emitente NO-UNDO.
DEF NEW GLOBAL SHARED VAR c-nr-pedcli-aux LIKE ped-venda.nr-pedcli   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS bc-etiqueta.qt-item 
&Scoped-define ENABLED-TABLES bc-etiqueta
&Scoped-define FIRST-ENABLED-TABLE bc-etiqueta
&Scoped-Define ENABLED-OBJECTS rt-mold i-qt-etiquetas 
&Scoped-Define DISPLAYED-FIELDS bc-etiqueta.qt-item 
&Scoped-define DISPLAYED-TABLES bc-etiqueta
&Scoped-define FIRST-DISPLAYED-TABLE bc-etiqueta
&Scoped-Define DISPLAYED-OBJECTS i-qt-etiquetas 

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
DEFINE VARIABLE i-qt-etiquetas AS INTEGER FORMAT ">>>>>>>>>9":U INITIAL 0 
     LABEL "Qtde Etiquetas" 
     VIEW-AS FILL-IN 
     SIZE 11.14 BY .88 NO-UNDO.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 79.29 BY 2.5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     i-qt-etiquetas AT ROW 1.33 COL 26 COLON-ALIGNED
     bc-etiqueta.qt-item AT ROW 2.33 COL 26 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 28.57 BY .88
     rt-mold AT ROW 1.08 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
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
         WIDTH              = 79.57.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
{include/c-viewer.i}
{utp/ut-glob.i}

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
        IF  adm-new-record    = NO AND
            adm-adding-record = NO THEN 
            ASSIGN i-qtde-orig = bc-etiqueta.qt-item.
        
        FIND bf-etiqueta
            WHERE ROWID(bf-etiqueta) = v-row-parent
            SHARE-LOCK NO-ERROR.

        IF  bf-etiqueta.qt-item < (INPUT FRAME {&FRAME-NAME} i-qt-etiquetas * INPUT FRAME {&FRAME-NAME} bc-etiqueta.qt-item) THEN DO:
            MESSAGE "A etiqueta nÆo tem capacidade para ser cortada na quantidade de etiquetas informadas!" VIEW-AS ALERT-BOX ERROR TITLE "Erro".
            RETURN 'ADM-ERROR':U.
        END.
        /* Dispatch standard ADM method.                             */
        RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
        if RETURN-VALUE = 'ADM-ERROR':U then 
            return 'ADM-ERROR':U.
        FIND emitente
            WHERE emitente.cod-emitente = i-cod-emit-aux NO-LOCK NO-ERROR.

       IF  adm-new-record    = YES AND
            adm-adding-record = YES THEN DO:
            IF  AVAIL bf-etiqueta THEN DO:
                FIND bc-param-ext SHARE-LOCK
                    WHERE bc-param-ext.cod-chave-param-ext    = "IMA0001Q"
                      AND bc-param-ext.cod-entidade-param-ext = 'bc-tipo-trans'
                      AND bc-param-ext.cod-param-ext          = ("NR-SEQ-ETIQ" + "-" + TRIM(bf-etiqueta.cod-estabel))
                    NO-ERROR.
                IF  AVAIL bc-param-ext
                THEN DO:
                     ASSIGN i-seq                      = bc-param-ext.param-inteiro + 1
                            bc-param-ext.param-inteiro = i-seq.
                    FIND CURRENT bc-param-ext NO-LOCK NO-ERROR.
                END.
/*                 REPEAT:                                                                                                   */
/*                     FIND bc-param-ext share-LOCK                                                                      */
/*                         WHERE bc-param-ext.cod-chave-param-ext    = "IMA0001Q"                                            */
/*                           AND bc-param-ext.cod-entidade-param-ext = 'bc-tipo-trans'                                       */
/*                           AND bc-param-ext.cod-param-ext          = ("NR-SEQ-ETIQ" + "-" + TRIM(bf-etiqueta.cod-estabel)) */
/*                         NO-ERROR NO-WAIT.                                                                                 */
/*                     IF  LOCKED(bc-param-ext) THEN                                                                         */
/*                         NEXT.                                                                                             */
/*                     ELSE DO:                                                                                              */
/*                         ASSIGN i-seq                      = bc-param-ext.param-inteiro + 1                                */
/*                                bc-param-ext.param-inteiro = i-seq.                                                        */
/*                         LEAVE.                                                                                            */
/*                     END.                                                                                                  */
/*                 END.                                                                                                      */
                ASSIGN bc-etiqueta.progressivo = STRING(i-ep-codigo-usuario,"9") + trim(string(bf-etiqueta.cod-estabel,"x(03)")) + STRING(i-seq,"999999999")
                       bf-etiqueta.qt-item     = bf-etiqueta.qt-item - bc-etiqueta.qt-item
                       bf-etiqueta.nr-pedcl    = c-nr-pedcli-aux
                       bf-etiqueta.nome-abrev  = IF AVAIL emitente THEN emitente.nome-abrev ELSE "":U.
            END.
        END.
        ELSE
            ASSIGN bf-etiqueta.qt-item = bf-etiqueta.qt-item + i-qtde-orig - bc-etiqueta.qt-item
                   bf-etiqueta.nr-pedcl    = c-nr-pedcli-aux
                   bf-etiqueta.nome-abrev  = IF AVAIL emitente THEN emitente.nome-abrev ELSE "":U.

    
        IF  AVAIL bf-etiqueta THEN
            ASSIGN bc-etiqueta.it-codigo   = bf-etiqueta.it-codigo
                   bc-etiqueta.un          = bf-etiqueta.un
                   bc-etiqueta.num-pedido  = bf-etiqueta.num-pedido
                   bc-etiqueta.nr-nota-fis = bf-etiqueta.nr-nota-fis
                   bc-etiqueta.cod-estabel = bf-etiqueta.cod-estabel
                   bc-etiqueta.nome-abrev  = bf-etiqueta.nome-abrev
                   bc-etiqueta.serie       = bf-etiqueta.serie
                   bc-etiqueta.referencia  = bf-etiqueta.referencia
                   bc-etiqueta.lote        = bf-etiqueta.referencia
                   bc-etiqueta.nr-romaneio = bf-etiqueta.nr-seq-fat.
    
        ASSIGN bc-etiqueta.dt-criacao       = TODAY
               bc-etiqueta.hr-criacao       = STRING(TIME,"HH:MM:SS")
               bc-etiqueta.usuar-criacao    = c-seg-usuario
               bc-etiqueta.cod-estado       = 1
               bc-etiqueta.cd-trans         = "IMA0002Q"
               bc-etiqueta.cod-layout       = 1
               bc-etiqueta.num-versao       = 1
               bc-etiqueta.log-datasul      = NO.

         IF  AVAIL bf-etiqueta THEN             
             ASSIGN bf-etiqueta.cod-estado = 2.

         

        RUN dispatch IN h-viewer ( INPUT 'display-fields':U ) .
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */


    IF  adm-new-record    = YES AND
        adm-adding-record = YES and
        INPUT FRAME {&frame-name} i-qt-etiquetas = 1 THEN DO:
      /* Dispatch standard ADM method.                             */
        RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

        FIND bf-etiqueta 
            WHERE ROWID(bf-etiqueta) = v-row-parent no-lock no-error.
        IF  AVAILABLE bf-etiqueta THEN
            ASSIGN bc-etiqueta.nr-etiq-pai = bf-etiqueta.nr-etiq.
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
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  
  /* Code placed here will execute AFTER standard behavior.    */

    


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
    
    IF  adm-new-record    = NO and
        adm-adding-record = NO then 
        ASSIGN i-qt-etiquetas:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
        

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

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  
    IF  adm-new-record    = NO and
        adm-adding-record = NO THEN 
        RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .
    ELSE DO:
        IF  INPUT FRAME {&FRAME-NAME} i-qt-etiquetas = 1 THEN
            RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .
        ELSE
            RUN pi-gera-etiquetas.
    END.

  /* Code placed here will execute AFTER standard behavior.    */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-gera-etiquetas V-table-Win 
PROCEDURE pi-gera-etiquetas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEF VAR i-qt-total LIKE bc-etiqueta.qt-item NO-UNDO.


    FIND bf-etiqueta
        WHERE ROWID(bf-etiqueta) = v-row-parent
        share-lock NO-ERROR.
    ASSIGN i-qt-total = INPUT FRAME {&frame-name} i-qt-etiquetas * 
                        INPUT FRAME {&frame-name} bc-etiqueta.qt-item.
    IF  bf-etiqueta.qt-item < i-qt-total THEN DO:
        MESSAGE "Esse item nÆo tem capacidade para a quantidade que ser  emitida na etiqueta!" 
            VIEW-AS ALERT-BOX ERROR TITLE 'Erro':U.
        RETURN "ADM-ERROR":U.
    END.

    IF  AVAIL bf-etiqueta THEN DO:
        DO i-cont = 1 TO INPUT FRAME {&frame-name} i-qt-etiquetas:
        
            CREATE bf-etiq2.
            ASSIGN bf-etiq2.nr-etiq-pai = bf-etiqueta.nr-etiq
/*                    bf-etiq2.nr-etiq     = NEXT-VALUE(seq-nr-etiq) */
                   bf-etiq2.qt-item     = INPUT FRAME {&frame-name} bc-etiqueta.qt-item.


            IF  adm-new-record    = NO AND
                adm-adding-record = NO THEN 
                ASSIGN i-qtde-orig = INPUT FRAME {&frame-name} bc-etiqueta.qt-item.
            ELSE
                ASSIGN i-qtde-orig = 0.
            
            ASSIGN bf-etiqueta.qt-item = bf-etiqueta.qt-item + i-qtde-orig - bf-etiq2.qt-item.

            FIND bc-param-ext SHARE-LOCK
                WHERE bc-param-ext.cod-chave-param-ext    = "IMA0001Q"
                  AND bc-param-ext.cod-entidade-param-ext = 'bc-tipo-trans'
                  AND bc-param-ext.cod-param-ext          = ("NR-SEQ-ETIQ" + "-" + TRIM(bf-etiqueta.cod-estabel))
                NO-ERROR.
            IF  AVAIL bc-param-ext
            THEN DO:
                 ASSIGN i-seq                      = bc-param-ext.param-inteiro + 1
                        bc-param-ext.param-inteiro = i-seq.
                FIND CURRENT bc-param-ext NO-LOCK NO-ERROR.
            END.

/*             REPEAT:                                                                                                   */
/*                 FIND bc-param-ext share-lock-LOCK                                                                      */
/*                     WHERE bc-param-ext.cod-chave-param-ext    = "IMA0001Q"                                            */
/*                       AND bc-param-ext.cod-entidade-param-ext = 'bc-tipo-trans'                                       */
/*                       AND bc-param-ext.cod-param-ext          = ("NR-SEQ-ETIQ" + "-" + TRIM(bf-etiqueta.cod-estabel)) */
/*                     NO-ERROR NO-WAIT.                                                                                 */
/*                 IF  LOCKED(bc-param-ext) THEN                                                                         */
/*                     NEXT.                                                                                             */
/*                 ELSE DO:                                                                                              */
/*                     ASSIGN i-seq                      = bc-param-ext.param-inteiro + 1                                */
/*                            bc-param-ext.param-inteiro = i-seq.                                                        */
/*                     LEAVE.                                                                                            */
/*                 END.                                                                                                  */
/*             END.                                                                                                      */
            ASSIGN bf-etiq2.progressivo   = STRING(i-ep-codigo-usuario,"9") + trim(string(bf-etiqueta.cod-estabel,"x(03)")) + STRING(i-seq,"999999999")
                   bf-etiq2.it-codigo     = bf-etiqueta.it-codigo
                   bf-etiq2.un            = bf-etiqueta.un
                   bf-etiq2.nr-nota-fis   = bf-etiqueta.nr-nota-fis
                   bf-etiq2.cod-estabel   = bf-etiqueta.cod-estabel
                   bf-etiq2.dt-criacao    = TODAY
                   bf-etiq2.hr-criacao    = STRING(TIME,"HH:MM:SS")
                   bf-etiq2.usuar-criacao = c-seg-usuario
                   bf-etiq2.cod-estado    = 1
                   bf-etiq2.cd-trans      = "IMA0002Q"
                   bf-etiq2.nome-abrev    = bf-etiqueta.nome-abrev
                   bf-etiq2.serie         = bf-etiqueta.serie
                   bf-etiq2.referencia    = bf-etiqueta.referencia
                   bf-etiq2.lote          = bf-etiqueta.referencia
                   bf-etiq2.nr-romaneio   = bf-etiqueta.nr-seq-fat
                   bf-etiq2.cod-layout    = 1
                   bf-etiq2.num-versao    = 1
                   bf-etiq2.log-datasul   = NO
                   bf-etiqueta.cod-estado = 2.
                       
        END.
        RUN dispatch IN h-viewer ( INPUT 'display-fields':U ) .
        RUN dispatch IN h-browse (INPUT 'OPEN-QUERY':U).
    END.


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

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartViewer, and there are no
     tables specified in any contained Browse, Query, or Frame. */

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

