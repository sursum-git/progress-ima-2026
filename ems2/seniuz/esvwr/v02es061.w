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
/* DEF NEW GLOBAL SHARED VAR gr-mp-fardo AS ROWID NO-UNDO. */

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
&Scoped-define EXTERNAL-TABLES espec.mp-fardo
&Scoped-define FIRST-EXTERNAL-TABLE espec.mp-fardo


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR espec.mp-fardo.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS espec.mp-fardo.sl2 espec.mp-fardo.dt-baixa ~
espec.mp-fardo.nr-mistura espec.mp-fardo.situacao espec.mp-fardo.peso 
&Scoped-define ENABLED-TABLES espec.mp-fardo
&Scoped-define FIRST-ENABLED-TABLE espec.mp-fardo
&Scoped-Define ENABLED-OBJECTS RECT-4 RECT-5 rt-key rt-mold 
&Scoped-Define DISPLAYED-FIELDS espec.mp-fardo.nr-fardo espec.mp-fardo.sl1 ~
espec.mp-fardo.sl2 espec.mp-fardo.padrao espec.mp-fardo.ur ~
espec.mp-fardo.letra espec.mp-fardo.cd-tipo espec.mp-fardo.dt-baixa ~
espec.mp-fardo.nr-mistura espec.mp-fardo.cd-coloracao ~
espec.mp-fardo.cd-compr espec.mp-fardo.situacao espec.mp-fardo.peso ~
espec.mp-fardo.finura espec.mp-fardo.cod-depos espec.mp-fardo.maturidade ~
espec.mp-fardo.cod-localiz espec.mp-fardo.resistencia 
&Scoped-define DISPLAYED-TABLES espec.mp-fardo
&Scoped-define FIRST-DISPLAYED-TABLE espec.mp-fardo
&Scoped-Define DISPLAYED-OBJECTS fi-cod-emit fi-nome-emit fi-nro-docto ~
fi-dt-recebto fi-procedencia fi-desc-tipo fi-desc-cor fi-desc-tam 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define List-4 fi-dt-recebto 

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
DEFINE VARIABLE fi-cod-emit AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fornecedor" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-cor AS CHARACTER FORMAT "X(20)":U 
     VIEW-AS FILL-IN 
     SIZE 21 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-tam AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 21 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-tipo AS CHARACTER FORMAT "X(20)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 21 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-recebto AS DATE FORMAT "99/99/9999":U 
     LABEL "Dt Recebimento" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nome-emit AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nro-docto AS CHARACTER FORMAT "X(256)":U 
     LABEL "Nota Fiscal" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-procedencia AS CHARACTER FORMAT "X(256)":U 
     LABEL "Procedˆncia" 
     VIEW-AS FILL-IN 
     SIZE 24 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 18.29 BY 4.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 18.57 BY 4.25.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.57 BY 1.25.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.57 BY 13.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     espec.mp-fardo.nr-fardo AT ROW 1.17 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.43 BY .88
          FONT 1
     fi-cod-emit AT ROW 3 COL 13 COLON-ALIGNED
     fi-nome-emit AT ROW 3 COL 19.43 COLON-ALIGNED NO-LABEL
     espec.mp-fardo.sl1 AT ROW 3.79 COL 72.72 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
     fi-nro-docto AT ROW 4 COL 13 COLON-ALIGNED
     fi-dt-recebto AT ROW 4 COL 51.57 COLON-ALIGNED
     espec.mp-fardo.sl2 AT ROW 4.79 COL 72.72 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
     fi-procedencia AT ROW 5 COL 13 COLON-ALIGNED
     espec.mp-fardo.padrao AT ROW 5 COL 43.72 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 21.14 BY .88
          FONT 1
     espec.mp-fardo.ur AT ROW 5.79 COL 72.86 COLON-ALIGNED
          LABEL "UR"
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
          FONT 1
     espec.mp-fardo.letra AT ROW 6 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 3.14 BY .88
     espec.mp-fardo.cd-tipo AT ROW 7 COL 13 COLON-ALIGNED
          LABEL "Tipo"
          VIEW-AS FILL-IN 
          SIZE 3.43 BY .88
     fi-desc-tipo AT ROW 7 COL 16.57 COLON-ALIGNED
     espec.mp-fardo.dt-baixa AT ROW 7.46 COL 54.29 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
     espec.mp-fardo.nr-mistura AT ROW 7.46 COL 73.43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY .79
     espec.mp-fardo.cd-coloracao AT ROW 8 COL 13 COLON-ALIGNED
          LABEL "Tonalidade"
          VIEW-AS FILL-IN 
          SIZE 3.43 BY .88
     fi-desc-cor AT ROW 8 COL 16.86 COLON-ALIGNED NO-LABEL
     espec.mp-fardo.cd-compr AT ROW 9 COL 13 COLON-ALIGNED
          LABEL "Comprimento (mm)"
          VIEW-AS FILL-IN 
          SIZE 3.43 BY .88
     fi-desc-tam AT ROW 9 COL 17 COLON-ALIGNED NO-LABEL
     espec.mp-fardo.situacao AT ROW 9.33 COL 70 NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Em Descarga", 1,
"Aguardando Analise", 2,
"Estoque", 3,
"Requisitado", 4
          SIZE 17.14 BY 3.25
     espec.mp-fardo.peso AT ROW 10 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.29 BY .88
     espec.mp-fardo.finura AT ROW 10 COL 42 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY .88
     espec.mp-fardo.cod-depos AT ROW 11 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.14 BY .88
     espec.mp-fardo.maturidade AT ROW 11 COL 42 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY .88
     espec.mp-fardo.cod-localiz AT ROW 12 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.14 BY .88
     espec.mp-fardo.resistencia AT ROW 12 COL 42 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY .88
     " Situa‡Æo Fardo" VIEW-AS TEXT
          SIZE 11 BY .54 AT ROW 8.5 COL 71.14
     "  Fibrografo" VIEW-AS TEXT
          SIZE 8 BY .54 AT ROW 3 COL 70.57
     RECT-4 AT ROW 3.25 COL 69
     RECT-5 AT ROW 8.75 COL 69
     rt-key AT ROW 1 COL 1
     rt-mold AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: espec.mp-fardo
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
         HEIGHT             = 13
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME f-main:SCROLLABLE       = FALSE
       FRAME f-main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN espec.mp-fardo.cd-coloracao IN FRAME f-main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN espec.mp-fardo.cd-compr IN FRAME f-main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN espec.mp-fardo.cd-tipo IN FRAME f-main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN espec.mp-fardo.cod-depos IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN espec.mp-fardo.cod-localiz IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-cod-emit IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-cor IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-tam IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-tipo IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-dt-recebto IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-nome-emit IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nro-docto IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-procedencia IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN espec.mp-fardo.finura IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN espec.mp-fardo.letra IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN espec.mp-fardo.maturidade IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN espec.mp-fardo.nr-fardo IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN espec.mp-fardo.padrao IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN espec.mp-fardo.resistencia IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN espec.mp-fardo.sl1 IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN espec.mp-fardo.ur IN FRAME f-main
   NO-ENABLE EXP-LABEL                                                  */
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
  {src/adm/template/row-list.i "espec.mp-fardo"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "espec.mp-fardo"}

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
    
    /*:T Ponha na pi-validate todas as valida‡äes */
    /*:T NÆo gravar nada no registro antes do dispatch do assign-record e 
       nem na PI-validate. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.
    
    /*:T Todos os assignïs nÆo feitos pelo assign-record devem ser feitos aqui */  
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
    
    /* Code placed here will execute AFTER standard behavior.    */
    IF AVAIL mp-fardo THEN DO.
       FIND mp-entr-mat WHERE
            mp-entr-mat.nr-cdr = mp-fardo.nr-cdr NO-LOCK NO-ERROR.

       FIND emitente WHERE
            emitente.cod-emit = mp-entr-mat.cod-emit NO-LOCK NO-ERROR.
       IF AVAIL emitente THEN
          ASSIGN fi-nome-emit:SCREEN-VALUE IN FRAME {&FRAME-NAME} = emitente.nome-emit.

       ASSIGN fi-cod-emit:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(mp-entr-mat.cod-emit)
              fi-nro-docto:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(mp-entr-mat.nro-docto)
              fi-dt-recebto:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(mp-entr-mat.dt-recebimento,"99/99/9999")
              fi-procedencia:SCREEN-VALUE IN FRAME {&FRAME-NAME} = mp-entr-mat.procedencia.

       FIND mp-tipo WHERE mp-tipo.codigo = INPUT FRAME {&FRAME-NAME} mp-fardo.cd-tipo NO-LOCK NO-ERROR.
       IF AVAIL mp-tipo THEN
          ASSIGN fi-desc-tipo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = mp-tipo.tipo.

       FIND mp-coloracao WHERE mp-coloracao.codigo = INPUT FRAME {&FRAME-NAME} mp-fardo.cd-coloracao NO-LOCK NO-ERROR.
       IF AVAIL mp-coloracao THEN DO.
          ASSIGN fi-desc-cor:SCREEN-VALUE IN FRAME {&FRAME-NAME} = mp-coloracao.tonalidade.
          ASSIGN mp-fardo.letra:FGCOLOR IN FRAME {&FRAME-NAME} = mp-coloracao.cor.
       END.

       FIND mp-classificacao WHERE mp-classificacao.codigo = INPUT FRAME {&FRAME-NAME} mp-fardo.cd-compr NO-LOCK NO-ERROR.
       IF AVAIL mp-classificacao THEN
          ASSIGN fi-desc-tam:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(mp-classificacao.compr-min,">>9.99") + "  A  " + STRING(mp-classificacao.compr-max,">>9.99").
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
/*:T------------------------------------------------------------------------------
  Purpose:Validar a viewer     
  Parameters:  <none>
  Notes: NÆo fazer assign aqui. Nesta procedure
  devem ser colocadas apenas valida‡äes, pois neste ponto do programa o registro 
  ainda nÆo foi criado.       
------------------------------------------------------------------------------*/
    {include/i-vldfrm.i} /*:T Valida‡Æo de dicion rio */
    
/*:T    Segue um exemplo de valida‡Æo de programa */
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
  {src/adm/template/snd-list.i "espec.mp-fardo"}

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

