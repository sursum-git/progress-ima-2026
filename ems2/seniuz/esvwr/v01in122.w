&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          ems206cad        PROGRESS
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
&Scoped-define EXTERNAL-TABLES familia
&Scoped-define FIRST-EXTERNAL-TABLE familia


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR familia.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS familia.descricao 
&Scoped-define ENABLED-TABLES familia
&Scoped-define FIRST-ENABLED-TABLE familia
&Scoped-Define ENABLED-OBJECTS rt-key rt-mold 
&Scoped-Define DISPLAYED-FIELDS familia.fm-codigo familia.descricao 
&Scoped-define DISPLAYED-TABLES familia
&Scoped-define FIRST-DISPLAYED-TABLE familia
&Scoped-Define DISPLAYED-OBJECTS fi-cod-composi fi-desc-composi fi-cod-rlgp ~
to-indigo fi-largura fi-fator-conv fi-peso-liquido 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define List-4 fi-cod-composi fi-cod-rlgp to-indigo fi-largura ~
fi-peso-liquido 
&Scoped-define List-5 familia.descricao 

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
DEFINE VARIABLE fi-cod-composi AS CHARACTER FORMAT "x(2)" 
     LABEL "Composiá∆o" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-rlgp AS INTEGER FORMAT ">9" INITIAL 0 
     LABEL "Rec.Lavagem" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-composi AS CHARACTER FORMAT "x(45)" 
     VIEW-AS FILL-IN 
     SIZE 52.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fator-conv AS DECIMAL FORMAT ">>9.9999" INITIAL 0 
     LABEL "Fator M" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-largura AS DECIMAL FORMAT "9.99" INITIAL 0 
     LABEL "Largura" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE fi-peso-liquido AS DECIMAL FORMAT ">>>,>>9.99999" INITIAL 0 
     LABEL "Peso Liq":R10 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 88 BY 1.25.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 88 BY 4.58.

DEFINE VARIABLE to-indigo AS LOGICAL INITIAL no 
     LABEL "÷ndigo" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .88 TOOLTIP "A fam°lia Ç um ÷ndigo?" NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     familia.fm-codigo AT ROW 1.17 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.14 BY .88
     familia.descricao AT ROW 1.17 COL 26.29 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 31.14 BY .88
     fi-cod-composi AT ROW 2.71 COL 14 COLON-ALIGNED
     fi-desc-composi AT ROW 2.71 COL 19.14 COLON-ALIGNED NO-LABEL
     fi-cod-rlgp AT ROW 3.71 COL 14 COLON-ALIGNED HELP
          "Codigo de Recomendacao de Lavagem por Grupo de Produto"
     to-indigo AT ROW 3.71 COL 46
     fi-largura AT ROW 4.71 COL 14 COLON-ALIGNED HELP
          "Largura do tecido"
     fi-fator-conv AT ROW 5.71 COL 14 COLON-ALIGNED HELP
          "Fator de conversao para Metros"
     fi-peso-liquido AT ROW 5.71 COL 44 COLON-ALIGNED HELP
          "Peso l°quido do item"
     rt-key AT ROW 1 COL 1
     rt-mold AT ROW 2.42 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: mgind.familia
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
         HEIGHT             = 6.13
         WIDTH              = 88.
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

/* SETTINGS FOR FILL-IN familia.descricao IN FRAME f-main
   5                                                                    */
/* SETTINGS FOR FILL-IN fi-cod-composi IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-cod-rlgp IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-desc-composi IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-fator-conv IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-largura IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-peso-liquido IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN familia.fm-codigo IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX to-indigo IN FRAME f-main
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

&Scoped-define SELF-NAME fi-cod-composi
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-composi V-table-Win
ON ENTRY OF fi-cod-composi IN FRAME f-main /* Composiá∆o */
DO:
  FIND composi WHERE composi.cod-composi = INPUT FRAME {&FRAME-NAME} fi-cod-composi 
               NO-LOCK NO-ERROR.
  IF AVAIL composi THEN
     ASSIGN fi-desc-composi:SCREEN-VALUE IN FRAME {&FRAME-NAME} = composi.descricao.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-composi V-table-Win
ON LEAVE OF fi-cod-composi IN FRAME f-main /* Composiá∆o */
DO:
  FIND composi WHERE composi.cod-composi = INPUT FRAME {&FRAME-NAME} fi-cod-composi NO-LOCK NO-ERROR.
  IF AVAIL composi THEN
     ASSIGN fi-desc-composi:SCREEN-VALUE IN FRAME {&FRAME-NAME} = composi.descricao.
  ELSE DO:
     MESSAGE "Composiá∆o inv†lida." VIEW-AS ALERT-BOX.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-composi V-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-cod-composi IN FRAME f-main /* Composiá∆o */
DO:
  {include/zoomvar.i &prog-zoom = eszoom/z01es003.w
                     &campo     = fi-cod-composi
                     &campozoom = cod-composi}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-rlgp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-rlgp V-table-Win
ON LEAVE OF fi-cod-rlgp IN FRAME f-main /* Rec.Lavagem */
DO:
  IF INPUT FRAME {&frame-name} fi-cod-rlgp < "1" or
     INPUT FRAME {&frame-name} fi-cod-rlgp > "4" THEN DO:
     MESSAGE "C¢digo de Recomendaá∆o de Lavagem deve estar entre 1 e 4." VIEW-AS ALERT-BOX.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-peso-liquido
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-peso-liquido V-table-Win
ON LEAVE OF fi-peso-liquido IN FRAME f-main /* Peso Liq */
DO:
  ASSIGN fi-fator-conv:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(1 / INPUT FRAME {&FRAME-NAME} fi-peso-liquido).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF 
          
  fi-cod-composi:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.
  
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
  {src/adm/template/row-list.i "familia"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "familia"}

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
  /*  {include/i-valid.i} */
    
  /* Ponha na pi-validate todas as validaá‰es */
  /* N∆o gravar nada no registro antes do dispatch do assign-record e 
     nem na PI-validate. */
    
  /* Dispatch standard ADM method.                             */
  /* --- Comentado porque viewer s¢ tem campos fill-in habilitados ---
  * RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
  * if RETURN-VALUE = 'ADM-ERROR':U then 
  *    return 'ADM-ERROR':U. 
  -----------------------------------Gilvando Nov/2003---------------*/
  RUN pi-validate.
  if RETURN-VALUE = 'ADM-ERROR':U then 
     return 'ADM-ERROR':U.

  FIND familia-ext WHERE familia-ext.fm-codigo = familia.fm-codigo NO-ERROR.
  IF NOT AVAIL familia-ext THEN DO:
     CREATE familia-ext.
     ASSIGN familia-ext.fm-codigo = familia.fm-codigo.
  END.
  ASSIGN familia-ext.cod-composi = INPUT FRAME {&FRAME-NAME} fi-cod-composi 
         familia-ext.cod-rlgp    = dec(INPUT FRAME {&frame-name} fi-cod-rlgp)
         familia-ext.largura     = DEC(INPUT FRAME {&FRAME-NAME} fi-largura)
         familia-ext.fator-conv  = DEC(INPUT FRAME {&FRAME-NAME} fi-fator-conv)
         familia-ext.indigo      = INPUT FRAME {&FRAME-NAME} to-indigo.
    
  FOR EACH ITEM WHERE ITEM.fm-codigo = familia-ext.fm-codigo:
      IF ITEM.un = "kg" THEN
         ASSIGN ITEM.peso-liquido = 1
                ITEM.peso-bruto   = 1.
      ELSE
         ASSIGN ITEM.peso-liquido = INPUT FRAME {&FRAME-NAME} fi-peso-liquido
                ITEM.peso-bruto   = INPUT FRAME {&FRAME-NAME} fi-peso-liquido.
  END.
  MESSAGE "Peso L°quido/Bruto Atualizado para os ÷tens da Fam°lia." VIEW-AS ALERT-BOX.

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
    &if defined(ADM-MODIFY-FIELDS) &then
        disable {&ADM-MODIFY-FIELDS} with frame {&frame-name}.
    &endif
    
    &if defined(list-4) &then
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
    
    FIND familia-ext WHERE familia-ext.fm-codigo = familia.fm-codigo NO-LOCK NO-ERROR.
    IF AVAIL familia-ext THEN DO:
       ASSIGN fi-cod-composi:SCREEN-VALUE IN FRAME {&FRAME-NAME} = familia-ext.cod-composi 
              fi-cod-rlgp:SCREEN-VALUE IN FRAME {&FRAME-NAME}= string(familia-ext.cod-rlgp)
              fi-largura:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(familia-ext.largura)
              fi-fator-conv:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(familia-ext.fator-conv)
              to-indigo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(familia-ext.indigo).
       FIND composi WHERE composi.cod-composi = familia-ext.cod-composi NO-LOCK NO-ERROR.
       IF AVAIL composi THEN
          ASSIGN fi-desc-composi:SCREEN-VALUE IN FRAME {&FRAME-NAME} = composi.descricao.
       ELSE
          ASSIGN fi-desc-composi:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
       FIND FIRST item WHERE ITEM.fm-codigo    =  familia-ext.fm-codigo
                         AND ITEM.peso-liquido <> 0
                       NO-LOCK NO-ERROR.
       IF AVAIL item THEN
          ASSIGN fi-peso-liquido:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ITEM.peso-liquido).
       ELSE
          ASSIGN fi-peso-liquido:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
    END.
    ELSE DO:
        ASSIGN fi-cod-composi:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" 
               fi-cod-rlgp:SCREEN-VALUE IN FRAME {&FRAME-NAME}= ""
               fi-largura:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
               fi-fator-conv:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
               fi-desc-composi:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
               to-indigo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "no".
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
    if adm-new-record = NO then
        enable {&ADM-MODIFY-FIELDS} with frame {&frame-name}.
    &endif

    &if defined(list-4) &then
        ENABLE {&list-4} WITH FRAME {&FRAME-NAME}.
    &endif

    &if defined(list-5) &then
        DISABLE {&list-5} WITH FRAME {&FRAME-NAME}.
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
    
    FIND composi WHERE composi.cod-composi = INPUT FRAME {&FRAME-NAME} fi-cod-composi 
                 NO-LOCK NO-ERROR.
    IF NOT AVAIL composi THEN DO:
       MESSAGE "Composiá∆o inv†lida." VIEW-AS ALERT-BOX.     
       APPLY 'entry' TO fi-cod-composi.                                                          
       return 'ADM-ERROR':U.                                                                    
    END.                                                                                        

    IF INPUT FRAME {&frame-name} fi-cod-rlgp < "1" or
       INPUT FRAME {&frame-name} fi-cod-rlgp > "4" THEN DO:
       MESSAGE "C¢digo de Recomendaá∆o de Lavagem deve estar entre 1 e 4." VIEW-AS ALERT-BOX.
       APPLY 'entry' TO fi-cod-rlgp.                                                          
       return 'ADM-ERROR':U.                                                                    
    END.                                                                                        

    IF INPUT FRAME {&frame-name} fi-largura <= 0 THEN DO:
       MESSAGE "Largura deve ser maior que zero." VIEW-AS ALERT-BOX.
       APPLY 'entry' TO fi-largura.                                                          
       return 'ADM-ERROR':U.                                                                    
    END.                                                                                        

    IF INPUT FRAME {&frame-name} fi-peso-liquido <= 0 THEN DO:
       MESSAGE "Peso l°quido deve ser maior que zero." VIEW-AS ALERT-BOX.
       APPLY 'entry' TO fi-peso-liquido.                                                          
       return 'ADM-ERROR':U.                                                                    
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
  {src/adm/template/snd-list.i "familia"}

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

