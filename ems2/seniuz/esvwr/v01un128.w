&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          mgcad            PROGRESS
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
{include/i-prgvrs.i V01UN128 2.04.00.000}

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

DEF VAR c-container AS CHAR.
DEF VAR h-query AS HANDLE.

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
&Scoped-define EXTERNAL-TABLES prog_dtsul
&Scoped-define FIRST-EXTERNAL-TABLE prog_dtsul


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR prog_dtsul.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS prog_dtsul.cod_proced ~
prog_dtsul.nom_prog_dtsul_menu 
&Scoped-define ENABLED-TABLES prog_dtsul
&Scoped-define FIRST-ENABLED-TABLE prog_dtsul
&Scoped-Define ENABLED-OBJECTS fi-cod_prog_dtsul BUTTON-1 rt-key rt-mold 
&Scoped-Define DISPLAYED-FIELDS prog_dtsul.cod_proced ~
prog_dtsul.nom_prog_dtsul_menu 
&Scoped-define DISPLAYED-TABLES prog_dtsul
&Scoped-define FIRST-DISPLAYED-TABLE prog_dtsul
&Scoped-Define DISPLAYED-OBJECTS fi-cod_prog_dtsul fi-des_aplicat_dtsul ~
fi-des_sist_dtsul fi-des_modul_dtsul fi-des_rot_dtsul fi-des_sub_rot_dtsul 

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
DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "image/im-vapra.bmp":U
     LABEL "Button 1" 
     SIZE 4 BY .88.

DEFINE VARIABLE fi-cod_prog_dtsul AS CHARACTER FORMAT "x(50)" 
     LABEL "Programa":R10 
     VIEW-AS FILL-IN 
     SIZE 51.14 BY .88.

DEFINE VARIABLE fi-des_aplicat_dtsul AS CHARACTER FORMAT "x(32)" 
     LABEL "Aplicativo":R11 
     VIEW-AS FILL-IN 
     SIZE 32 BY .88 NO-UNDO.

DEFINE VARIABLE fi-des_modul_dtsul AS CHARACTER FORMAT "x(32)" 
     LABEL "M¢dulo":R11 
     VIEW-AS FILL-IN 
     SIZE 32 BY .88 NO-UNDO.

DEFINE VARIABLE fi-des_rot_dtsul AS CHARACTER FORMAT "x(32)" 
     LABEL "Rotina":R11 
     VIEW-AS FILL-IN 
     SIZE 32 BY .88 NO-UNDO.

DEFINE VARIABLE fi-des_sist_dtsul AS CHARACTER FORMAT "x(32)" 
     LABEL "Sistema":R11 
     VIEW-AS FILL-IN 
     SIZE 32 BY .88 NO-UNDO.

DEFINE VARIABLE fi-des_sub_rot_dtsul AS CHARACTER FORMAT "x(32)" 
     LABEL "Sub-Rotina":R24 
     VIEW-AS FILL-IN 
     SIZE 32 BY .88 NO-UNDO.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 88 BY 1.25.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 88 BY 7.5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     fi-cod_prog_dtsul AT ROW 1.17 COL 15 COLON-ALIGNED
     BUTTON-1 AT ROW 1.21 COL 68.72
     prog_dtsul.cod_proced AT ROW 2.75 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 33.14 BY .88
     prog_dtsul.nom_prog_dtsul_menu AT ROW 3.75 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 41.14 BY .88
     fi-des_aplicat_dtsul AT ROW 4.75 COL 15 COLON-ALIGNED HELP
          "Descriá∆o do Aplicativo"
     fi-des_sist_dtsul AT ROW 5.75 COL 15 COLON-ALIGNED HELP
          "Descriá∆o do Sistema"
     fi-des_modul_dtsul AT ROW 6.75 COL 15 COLON-ALIGNED HELP
          "Descriá∆o M¢dulo"
     fi-des_rot_dtsul AT ROW 7.75 COL 15 COLON-ALIGNED HELP
          "Descriá∆o da Rotina"
     fi-des_sub_rot_dtsul AT ROW 8.75 COL 15 COLON-ALIGNED HELP
          "Descriá∆o da Sub-Rotina"
     rt-key AT ROW 1 COL 1
     rt-mold AT ROW 2.25 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: mgcad.prog_dtsul
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
         HEIGHT             = 9.13
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

/* SETTINGS FOR FILL-IN fi-des_aplicat_dtsul IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-des_modul_dtsul IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-des_rot_dtsul IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-des_sist_dtsul IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-des_sub_rot_dtsul IN FRAME f-main
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

&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 V-table-Win
ON CHOOSE OF BUTTON-1 IN FRAME f-main /* Button 1 */
DO:
  FIND prog_dtsul WHERE prog_dtsul.cod_prog_dtsul = INPUT FRAME {&FRAME-NAME} fi-cod_prog_dtsul
                  NO-LOCK NO-ERROR.
  IF NOT AVAIL prog_dtsul THEN DO:
     MESSAGE "Programa n∆o cadastrado."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     APPLY 'entry' TO fi-cod_prog_dtsul IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END. 
  ELSE DO:
      RUN get-link-handle IN adm-broker-hdl ( INPUT THIS-PROCEDURE,
                                              INPUT "CONTAINER",
                                              OUTPUT c-container). 

      RUN pi-busca-qry IN WIDGET-HANDLE(c-container) (OUTPUT h-query).
          RUN pi-reposiciona-query IN h-query (INPUT ROWID(prog_dtsul)). 
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod_prog_dtsul
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod_prog_dtsul V-table-Win
ON RETURN OF fi-cod_prog_dtsul IN FRAME f-main /* Programa */
DO:
  APPLY 'choose' TO button-1.
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
  {src/adm/template/row-list.i "prog_dtsul"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "prog_dtsul"}

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
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.
    
    /*:T Todos os assignÔs n∆o feitos pelo assign-record devem ser feitos aqui */  
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
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ).
    
    /* Code placed here will execute AFTER standard behavior.    */
    
    ASSIGN fi-des_aplicat_dtsul:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
           fi-des_sist_dtsul:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
           fi-des_modul_dtsul:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
           fi-des_rot_dtsul:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
           fi-des_sub_rot_dtsul:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".  

    IF AVAIL prog_dtsul THEN DO:
       ASSIGN fi-cod_prog_dtsul:SCREEN-VALUE IN FRAME {&FRAME-NAME} = prog_dtsul.cod_prog_dtsul.

       FIND sub_rot_dtsul_proced WHERE
            sub_rot_dtsul_proced.cod_prog_dtsul = prog_dtsul.cod_prog_dtsul NO-LOCK NO-ERROR.
       IF NOT AVAIL sub_rot_dtsul_proced THEN
          FIND sub_rot_dtsul_proced WHERE
               sub_rot_dtsul_proced.cod_proced = prog_dtsul.cod_proced NO-LOCK NO-ERROR.
       
       IF AVAIL sub_rot_dtsul_proced THEN DO:
          FIND sub_rot_dtsul OF sub_rot_dtsul_proced NO-LOCK NO-ERROR.
          IF AVAIL sub_rot_dtsul THEN
             ASSIGN fi-des_sub_rot_dtsul:SCREEN-VALUE IN FRAME {&FRAME-NAME} = sub_rot_dtsul.des_sub_rot_dtsul.
       
          FIND modul_rot_proced WHERE
               modul_rot_proced.num_sub_rot_dtsul = sub_rot_dtsul_proced.num_sub_rot_dtsul NO-LOCK NO-ERROR.
       END.
       ELSE DO:
          FIND modul_rot_proced WHERE
               modul_rot_proced.cod_prog_dtsul = prog_dtsul.cod_prog_dtsul NO-LOCK NO-ERROR.
       
          IF NOT AVAIL modul_rot_proced THEN
             FIND modul_rot_proced WHERE
                  modul_rot_proced.cod_proced = prog_dtsul.cod_proced NO-LOCK NO-ERROR.
          FIND modul_dtsul OF modul_rot_proced NO-LOCK NO-ERROR.
          IF AVAIL modul_dtsul THEN
             ASSIGN fi-des_modul_dtsul:SCREEN-VALUE IN FRAME {&FRAME-NAME} = modul_dtsul.des_modul_dtsul.

          FIND modul_rot OF modul_rot_proced NO-LOCK NO-ERROR.
          FIND rot_dtsul OF modul_rot NO-LOCK NO-ERROR.
          IF AVAIL rot_dtsul THEN
             ASSIGN fi-des_rot_dtsul:SCREEN-VALUE IN FRAME {&FRAME-NAME} = rot_dtsul.des_rot_dtsul.
          
          FIND sist_dtsul OF modul_dtsul NO-LOCK NO-ERROR.
          IF AVAIL sist_dtsul THEN
             ASSIGN fi-des_sist_dtsul:SCREEN-VALUE IN FRAME {&FRAME-NAME} = sist_dtsul.des_sist_dtsul.

          FIND aplicat_dtsul OF sist_dtsul NO-LOCK NO-ERROR.
          IF AVAIL aplicat_dtsul THEN
             ASSIGN fi-des_aplicat_dtsul:SCREEN-VALUE IN FRAME {&FRAME-NAME} = aplicat_dtsul.des_aplicat_dtsul.
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
  {src/adm/template/snd-list.i "prog_dtsul"}

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

