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

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES contr-sac
&Scoped-define FIRST-EXTERNAL-TABLE contr-sac


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR contr-sac.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS contr-sac.qtd-fat contr-sac.qtd-def ~
contr-sac.cod-tipo-def contr-sac.cod-defeito 
&Scoped-define ENABLED-TABLES contr-sac
&Scoped-define FIRST-ENABLED-TABLE contr-sac
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 rt-key 
&Scoped-Define DISPLAYED-FIELDS contr-sac.qtd-fat contr-sac.qtd-def ~
contr-sac.cod-tipo-def contr-sac.cod-defeito 
&Scoped-define DISPLAYED-TABLES contr-sac
&Scoped-define FIRST-DISPLAYED-TABLE contr-sac
&Scoped-Define DISPLAYED-OBJECTS cb-ocorrencias fi-desc-tipo-def ~
fi-desc-defeito 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-CREATE-FIELDS cb-ocorrencias 
&Scoped-define ADM-MODIFY-FIELDS cb-ocorrencias 

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
DEFINE VARIABLE cb-ocorrencias AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ocorrància" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEMS "","01-Qualidade do produto","02-Falta de Artigos","03-Excesso de Artigos","04-Troca artigo,cor,variante","05-Troca Rolo x Peca","06-Atraso entrega-Tear","07-Atraso entrega-Transp","08-Faturamento antecipado","09-Metragem em desacordo","10-Preench.incorreto pedido","11-Implantacao incorreta","12-Outros" 
     DROP-DOWN-LIST
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE fi-desc-defeito AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 33.43 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-tipo-def AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 35.57 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 84 BY 2.5.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 84 BY 3.5.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 86 BY 8.75.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     contr-sac.qtd-fat AT ROW 1.63 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.43 BY .88
     contr-sac.qtd-def AT ROW 2.63 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.43 BY .88
     cb-ocorrencias AT ROW 4.25 COL 21 COLON-ALIGNED
     contr-sac.cod-tipo-def AT ROW 5.29 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 3.14 BY .88
     fi-desc-tipo-def AT ROW 5.29 COL 24.43 COLON-ALIGNED NO-LABEL
     contr-sac.cod-defeito AT ROW 6.29 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5.14 BY .88
     fi-desc-defeito AT ROW 6.29 COL 26.57 COLON-ALIGNED NO-LABEL
     RECT-1 AT ROW 1.33 COL 2
     RECT-2 AT ROW 4.04 COL 2
     rt-key AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: espec.contr-sac
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
         HEIGHT             = 8.92
         WIDTH              = 87.43.
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

/* SETTINGS FOR COMBO-BOX cb-ocorrencias IN FRAME f-main
   NO-ENABLE 1 3                                                        */
/* SETTINGS FOR FILL-IN fi-desc-defeito IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-tipo-def IN FRAME f-main
   NO-ENABLE                                                            */
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

&Scoped-define SELF-NAME contr-sac.cod-defeito
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contr-sac.cod-defeito V-table-Win
ON ENTRY OF contr-sac.cod-defeito IN FRAME f-main /* Codigo Defeito */
DO:
   FIND defeito WHERE defeito.cod-tipo-def = INPUT FRAME {&FRAME-NAME} contr-sac.cod-tipo-def
                  AND defeito.cod-defeito  = INPUT FRAME {&FRAME-NAME} contr-sac.cod-defeito
                 NO-LOCK NO-ERROR.
   IF AVAIL defeito THEN
      ASSIGN fi-desc-defeito:SCREEN-VALUE IN FRAME {&FRAME-NAME} = defeito.descricao.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contr-sac.cod-defeito V-table-Win
ON LEAVE OF contr-sac.cod-defeito IN FRAME f-main /* Codigo Defeito */
DO:
   FIND defeito WHERE defeito.cod-tipo-def = INPUT FRAME {&FRAME-NAME} contr-sac.cod-tipo-def
                  AND defeito.cod-defeito  = INPUT FRAME {&FRAME-NAME} contr-sac.cod-defeito
                 NO-LOCK NO-ERROR.
   IF AVAIL defeito THEN
      ASSIGN fi-desc-defeito:SCREEN-VALUE IN FRAME {&FRAME-NAME} = defeito.descricao.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contr-sac.cod-defeito V-table-Win
ON MOUSE-SELECT-DBLCLICK OF contr-sac.cod-defeito IN FRAME f-main /* Codigo Defeito */
DO:
  {include/zoomvar.i &prog-zoom  = eszoom/z01es009.w
                     &campo      = contr-sac.cod-defeito
                     &campozoom  = cod-defeito
                     &parametros = "run pi-seta-inicial in 
                      wh-pesquisa(input input frame {&frame-name} contr-sac.cod-tipo-def)."}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME contr-sac.cod-tipo-def
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contr-sac.cod-tipo-def V-table-Win
ON ENTRY OF contr-sac.cod-tipo-def IN FRAME f-main /* Codigo Tipo Defeito */
DO:
   FIND tipo-def WHERE tipo-def.cod-tipo-def = INPUT FRAME {&FRAME-NAME} contr-sac.cod-tipo-def
                 NO-LOCK NO-ERROR.
   IF AVAIL tipo-def THEN
      ASSIGN fi-desc-tipo-def:SCREEN-VALUE IN FRAME {&FRAME-NAME} = tipo-def.descricao.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contr-sac.cod-tipo-def V-table-Win
ON LEAVE OF contr-sac.cod-tipo-def IN FRAME f-main /* Codigo Tipo Defeito */
DO:
   FIND tipo-def WHERE tipo-def.cod-tipo-def = INPUT FRAME {&FRAME-NAME} contr-sac.cod-tipo-def
                 NO-LOCK NO-ERROR.
   IF AVAIL tipo-def THEN
      ASSIGN fi-desc-tipo-def:SCREEN-VALUE IN FRAME {&FRAME-NAME} = tipo-def.descricao.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contr-sac.cod-tipo-def V-table-Win
ON MOUSE-SELECT-DBLCLICK OF contr-sac.cod-tipo-def IN FRAME f-main /* Codigo Tipo Defeito */
DO:
  {include/zoomvar.i &prog-zoom = eszoom/z01es037.w
                     &campo     = contr-sac.cod-tipo-def
                     &campozoom = cod-tipo-def}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  contr-sac.cod-tipo-def:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.
  contr-sac.cod-defeito:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.
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
  {src/adm/template/row-list.i "contr-sac"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "contr-sac"}

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
  ASSIGN cb-ocorrencias:SCREEN-VALUE IN FRAME {&FRAME-NAME} = " ".
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
    IF RETURN-VALUE = 'ADM-ERROR':U THEN
       RETURN 'ADM-ERROR':U.

     /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.
    
    /* Todos os assignÔs n∆o feitos pelo assign-record devem ser feitos aqui */  
    /* Code placed here will execute AFTER standard behavior.    */
     ASSIGN contr-sac.cod-ocorr = SUBSTR(INPUT FRAME {&FRAME-NAME} cb-ocorrencias,1,2).

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
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ).
    
    /* Code placed here will execute AFTER standard behavior.    */
    FIND tipo-def WHERE tipo-def.cod-tipo-def = contr-sac.cod-tipo-def NO-LOCK NO-ERROR.
    IF AVAIL tipo-def THEN
       ASSIGN fi-desc-tipo-def:SCREEN-VALUE IN FRAME {&FRAME-NAME} = tipo-def.descricao.

    FIND defeito WHERE defeito.cod-tipo-def = contr-sac.cod-tipo-def
                   AND defeito.cod-defeito  = contr-sac.cod-defeito 
                 NO-LOCK NO-ERROR.
    IF AVAIL defeito THEN
       ASSIGN fi-desc-defeito:SCREEN-VALUE IN FRAME {&FRAME-NAME} = defeito.descricao.

    IF contr-sac.cod-ocorr <> "" THEN
       ASSIGN cb-ocorrencias:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(INT(contr-sac.cod-ocorr) + 1,
                             cb-ocorrencias:LIST-ITEMS IN FRAME {&FRAME-NAME}).

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
    if adm-new-record = NO THEN
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
    
IF INPUT FRAME {&FRAME-NAME} contr-sac.qtd-fat <= 0 OR
   INPUT FRAME {&FRAME-NAME} contr-sac.qtd-def <= 0 THEN DO:
   MESSAGE "Quantidades Faturada e Defeituosa devem ser maiores que zero." VIEW-AS ALERT-BOX.           
   APPLY "entry" TO contr-sac.qtd-fat.                                                       
   return 'ADM-ERROR':U.                                                                          
END.    

IF INPUT FRAME {&FRAME-NAME} contr-sac.qtd-def > 
   INPUT FRAME {&FRAME-NAME} contr-sac.qtd-fat THEN DO:
   MESSAGE "Quantidade Defeituosa n∆o pode ser maior que a Faturada." VIEW-AS ALERT-BOX.           
   APPLY "entry" TO contr-sac.qtd-fat.                                                       
   return 'ADM-ERROR':U.                                                                          
END.

IF cb-ocorrencias:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" THEN DO.
   MESSAGE "Ocorrància deve ser Informada." VIEW-AS ALERT-BOX.
   APPLY "entry" TO cb-ocorrencias.                                                    
   return 'ADM-ERROR':U.                                                                       
END.

IF substr(cb-ocorrencias:SCREEN-VALUE IN FRAME {&FRAME-NAME},1,2) = "01" THEN DO:             
   FIND defeito WHERE defeito.cod-tipo-def = INPUT FRAME {&FRAME-NAME} contr-sac.cod-tipo-def     
                  AND defeito.cod-defeito  = INPUT FRAME {&FRAME-NAME} contr-sac.cod-defeito      
                NO-LOCK NO-ERROR.                                                                 
   IF NOT AVAIL defeito THEN DO:                                                                  
      MESSAGE "Defeito deve ser informado para esse tipo de ocorrància." VIEW-AS ALERT-BOX.        
      APPLY "entry" TO contr-sac.cod-tipo-def.                                                    
      return 'ADM-ERROR':U.                                                                       
   END.                                                                                           
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
  {src/adm/template/snd-list.i "contr-sac"}

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

