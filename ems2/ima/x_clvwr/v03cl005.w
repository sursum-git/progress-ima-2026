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
def var v-row-parent as rowid   no-undo.
DEF VAR i-seq        AS INTEGER NO-UNDO.
DEF VAR i-tp-embal   AS INTEGER.
DEF VAR c-cod-chave-param-ext  LIKE bc-param-ext.cod-chave-param-ext.

def temp-table tt-erro no-undo
    field i-sequen as int             
    field cd-erro  as int
    field mensagem as char format "x(255)".

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
&Scoped-define EXTERNAL-TABLES bc-etiqueta
&Scoped-define FIRST-EXTERNAL-TABLE bc-etiqueta


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR bc-etiqueta.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS bc-etiqueta.progressivo ~
bc-etiqueta.cod-estabel bc-etiqueta.it-codigo bc-etiqueta.referencia ~
bc-etiqueta.num-pedido bc-etiqueta.qt-item 
&Scoped-define ENABLED-TABLES bc-etiqueta
&Scoped-define FIRST-ENABLED-TABLE bc-etiqueta
&Scoped-Define ENABLED-OBJECTS rt-key rt-mold 
&Scoped-Define DISPLAYED-FIELDS bc-etiqueta.progressivo ~
bc-etiqueta.cod-estabel bc-etiqueta.it-codigo bc-etiqueta.referencia ~
bc-etiqueta.num-pedido bc-etiqueta.qt-item 
&Scoped-define DISPLAYED-TABLES bc-etiqueta
&Scoped-define FIRST-DISPLAYED-TABLE bc-etiqueta
&Scoped-Define DISPLAYED-OBJECTS c-desc-estabelec fi-num-etiqueta 

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
DEFINE VARIABLE c-desc-estabelec AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 41.14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-num-etiqueta AS CHARACTER FORMAT "X(256)":U 
     LABEL "Etiqueta Logistica:" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 NO-UNDO.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.57 BY 1.25.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.57 BY 4.75.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     bc-etiqueta.progressivo AT ROW 1.17 COL 26 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15 BY .88
     bc-etiqueta.cod-estabel AT ROW 2.75 COL 26 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7.14 BY .88
     bc-etiqueta.it-codigo AT ROW 3.75 COL 26 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.14 BY .88
     c-desc-estabelec AT ROW 3.75 COL 43.57 COLON-ALIGNED NO-LABEL
     bc-etiqueta.referencia AT ROW 4.75 COL 26 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.14 BY .88
     bc-etiqueta.num-pedido AT ROW 4.75 COL 68 COLON-ALIGNED WIDGET-ID 2
          LABEL "Container"
          VIEW-AS FILL-IN 
          SIZE 8 BY .88
     bc-etiqueta.qt-item AT ROW 5.75 COL 26 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.14 BY 1
     fi-num-etiqueta AT ROW 5.75 COL 68 COLON-ALIGNED
     rt-key AT ROW 1 COL 1
     rt-mold AT ROW 2.5 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: mgcld.bc-etiqueta
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
         HEIGHT             = 6.29
         WIDTH              = 88.57.
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

/* SETTINGS FOR FILL-IN c-desc-estabelec IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-num-etiqueta IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN bc-etiqueta.num-pedido IN FRAME f-main
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

&Scoped-define SELF-NAME bc-etiqueta.it-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bc-etiqueta.it-codigo V-table-Win
ON LEAVE OF bc-etiqueta.it-codigo IN FRAME f-main /* Cod Item */
DO:
   FIND ITEM WHERE
        ITEM.it-codigo = bc-etiqueta.it-codigo:SCREEN-VALUE NO-LOCK NO-ERROR.
   IF AVAIL ITEM THEN
      ASSIGN c-desc-estabelec:SCREEN-VALUE = ITEM.desc-item.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

IF  bc-etiqueta.it-codigo:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME} THEN
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
  {src/adm/template/row-list.i "bc-etiqueta"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "bc-etiqueta"}

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

    IF INPUT FRAME {&FRAME-NAME} bc-etiqueta.num-pedido = 0 THEN DO.
        MESSAGE "Container n∆o foi Informado.... " SKIP
                "Confirma etiqueta sem Container ?"
            VIEW-AS ALERT-BOX QUESTION TITLE "Container n∆o informado" UPDATE l-confirma AS LOGICAL.
        IF NOT l-confirma THEN DO.
           APPLY "entry" TO bc-etiqueta.num-pedido.
           RETURN "ADM-ERROR":U.
        END.
    END.
    ELSE DO.
        FIND pp-container WHERE
             pp-container.nr-container = INPUT FRAME {&FRAME-NAME} bc-etiqueta.num-pedido NO-LOCK NO-ERROR.
        IF NOT AVAIL pp-container THEN DO.
            MESSAGE "Container n∆o Cadastrado !!!" 
                VIEW-AS ALERT-BOX ERROR TITLE "Erro".
            APPLY "entry" TO bc-etiqueta.num-pedido.
            RETURN "ADM-ERROR":U.
        END.
    END.

    IF  bc-etiqueta.cod-estabel:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" THEN DO:
        MESSAGE "O c¢digo do estabelecimento deve ser informado!" VIEW-AS ALERT-BOX ERROR TITLE "Erro".
        APPLY "entry" TO bc-etiqueta.cod-estabel IN FRAME {&FRAME-NAME}.
        RETURN "ADM-ERROR":U.
    END.

    FIND ITEM
        WHERE item.it-codigo = bc-etiqueta.it-codigo:SCREEN-VALUE IN FRAME {&FRAME-NAME}
        NO-LOCK NO-ERROR.
    IF  NOT AVAIL ITEM THEN DO:
        MESSAGE "Est† sendo utilizado um item que n∆o est† cadastrado!" VIEW-AS ALERT-BOX ERROR TITLE "Erro".
        APPLY "Entry":U TO bc-etiqueta.it-codigo IN FRAME {&FRAME-NAME}.
        RETURN "ADM-ERROR":U.
    END.

    FIND estabelec
        WHERE estabelec.cod-estabel = bc-etiqueta.cod-estabel:SCREEN-VALUE IN FRAME {&FRAME-NAME}
        NO-LOCK NO-ERROR.
    IF  NOT AVAIL estabelec THEN DO:
        MESSAGE "Est† sendo utilizado um Estabelecimento que n∆o est† cadastrado!" VIEW-AS ALERT-BOX ERROR TITLE "Erro".
        APPLY "Entry":U TO bc-etiqueta.cod-estabel IN FRAME {&FRAME-NAME}.
        RETURN "ADM-ERROR":U.
    END.

    FIND ref-item
        WHERE ref-item.it-codigo = bc-etiqueta.it-codigo:SCREEN-VALUE IN FRAME {&FRAME-NAME}
          AND ref-item.cod-refer = bc-etiqueta.referencia:SCREEN-VALUE IN FRAME {&FRAME-NAME}
        NO-LOCK NO-ERROR.
    IF  NOT AVAIL ref-item THEN DO:
        MESSAGE "A referància informada n∆o Ç uma referància v†lida para este item!" VIEW-AS ALERT-BOX ERROR TITLE "Erro".
        APPLY "Entry":U TO bc-etiqueta.referencia IN FRAME {&FRAME-NAME}.
        RETURN "ADM-ERROR":U.
    END.

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.

    IF i-ep-codigo-usuario = '5' THEN
       ASSIGN c-cod-chave-param-ext = "MED0001Q".
    ELSE
       ASSIGN c-cod-chave-param-ext = "IMA0001Q".

       FIND bc-param-ext WHERE
            bc-param-ext.cod-chave-param-ext    = c-cod-chave-param-ext AND
            bc-param-ext.cod-entidade-param-ext = 'bc-tipo-trans' AND
            bc-param-ext.cod-param-ext          = ("NR-SEQ-ETIQ" + "-" + TRIM(bc-etiqueta.cod-estabel))
            SHARE-LOCK NO-ERROR.
    
    IF AVAIL bc-param-ext
    THEN DO:
        ASSIGN i-seq                      = bc-param-ext.param-inteiro + 1
               bc-param-ext.param-inteiro = i-seq.
        FIND CURRENT bc-param-ext NO-LOCK NO-ERROR.
    END.
    ASSIGN bc-etiqueta.progressivo   = STRING(i-ep-codigo-usuario,"9") + trim(string(bc-etiqueta.cod-estabel,"x(03)")) + STRING(i-seq,"999999999")
           bc-etiqueta.cod-estado    = 2
           bc-etiqueta.cd-trans      = c-cod-chave-param-ext
           bc-etiqueta.qt-un-1       = bc-etiqueta.qt-item
           bc-etiqueta.un            = ""
           bc-etiqueta.nr-nota-fis   = ""
           bc-etiqueta.dt-criacao    = TODAY
           bc-etiqueta.hr-criacao    = STRING(TIME,"HH:MM:SS")
           bc-etiqueta.usuar-criacao = c-seg-usuario
           bc-etiqueta.lote          = bc-etiqueta.referencia
           bc-etiqueta.cod-layout    = 1
           bc-etiqueta.num-versao    = 1
           bc-etiqueta.log-datasul   = NO.

    /* Cria Etiqueta Nova Logistica */
    FIND ITEM WHERE
         ITEM.it-codigo = bc-etiqueta.it-codigo NO-LOCK NO-ERROR.

    ASSIGN i-tp-embal = 1.
    IF ITEM.un = 'kg' THEN 
       ASSIGN i-tp-embal = 5.

    FIND corte-comerc WHERE
         corte-comerc.compr-min <= bc-etiqueta.qt-item AND
         corte-comerc.compr-max >= bc-etiqueta.qt-item AND
         corte-comerc.tp-embalag = i-tp-embal NO-LOCK NO-ERROR.

    CREATE ob-etiqueta.
    ASSIGN ob-etiqueta.cod-estabel     = bc-etiqueta.cod-estabel
           ob-etiqueta.dt-emissao      = TODAY
           ob-etiqueta.hr-emissao      = STRING(TIME,"HH:MM")
           ob-etiqueta.acondic         = ""
           ob-etiqueta.it-codigo       = bc-etiqueta.it-codigo
           ob-etiqueta.cod-refer       = bc-etiqueta.referencia
           ob-etiqueta.nr-lote         = IF bc-etiqueta.lote = '888'
                                         THEN 'RD' ELSE 'RP'
           ob-etiqueta.cod-qualid      = IF bc-etiqueta.lote = '888'
                                         THEN 'D' ELSE 'B'
           ob-etiqueta.corte-comerc    = IF AVAIL corte-comerc
                                         THEN corte-comerc.codigo
                                         ELSE ''
           ob-etiqueta.quantidade      = bc-etiqueta.qt-item
           ob-etiqueta.localizacao     = ''
           ob-etiqueta.situacao        = 3
           ob-etiqueta.ob-origem       = STRING(bc-etiqueta.num-pedido)
           ob-etiqueta.num-etiqueta    = IF bc-etiqueta.cod-estabel = '1' 
                                         THEN NEXT-VALUE(seq-etq-estoq-ima)
                                         ELSE NEXT-VALUE(seq-etq-estoq-med)
           ob-etiqueta.progressivo     = bc-etiqueta.progressivo.

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
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  APPLY "leave":u TO bc-etiqueta.it-codigo IN FRAME {&FRAME-NAME}.
  /* Code placed here will execute AFTER standard behavior.    */
  
  FIND ob-etiqueta WHERE
       ob-etiqueta.progressivo = bc-etiqueta.progressivo
       NO-LOCK NO-ERROR.
  ASSIGN fi-num-etiqueta:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
  IF AVAIL ob-etiqueta THEN
     ASSIGN fi-num-etiqueta:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ob-etiqueta.num-etiqueta,"999999999").
  
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imprime V-table-Win 
PROCEDURE pi-imprime :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR l-resp AS LOGICAL NO-UNDO.

    IF  AVAIL bc-etiqueta THEN DO:
    
        MESSAGE "A Etiqueta ser† impressa. Confirma?" VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO TITLE "Confirmaá∆o" UPDATE l-resp.
    
        IF  l-resp THEN DO:
            IF  SESSION:SET-WAIT-STATE("GENERAL") THEN.
            FOR EACH tt-erro:
                DELETE tt-erro.
            END.

            RUN bcp/bcx2000.p (INPUT ROWID(bc-etiqueta),
                                INPUT "IMA0004Q",
                                INPUT-OUTPUT TABLE tt-erro).
            IF  SESSION:SET-WAIT-STATE("") THEN.
            FIND FIRST tt-erro
                NO-LOCK NO-ERROR.
            IF  AVAIL tt-erro THEN DO:
                MESSAGE 'Ocorreram erros na impress∆o da etiqueta.' SKIP
                        'Verifique os erros antes de imprimir.' SKIP
                        tt-erro.cd-erro ' - ' tt-erro.mensagem '.' VIEW-AS ALERT-BOX ERROR TITLE "Erro".
                RETURN "ADM-ERROR":U.
            END.


        END.
    END.
    ELSE
        MESSAGE "N∆o h† etiqueta dispon°vel para impress∆o." VIEW-AS ALERT-BOX ERROR TITLE "Erro".

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
  {src/adm/template/snd-list.i "bc-etiqueta"}

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

