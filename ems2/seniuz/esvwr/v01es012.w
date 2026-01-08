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
{include/i-prgvrs.i V01ES012 2.04.00.000}

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

DEF VAR c-item-ref    AS CHAR FORMAT "x(24)".
def var i-dv-fatores  as int extent 13 init[6,5,4,3,2,9,8,7,6,5,4,3,2].
def var i-dv-dig-calc as int.
def var i-dv-cont     as int.
def var i-dv-soma     as int.
DEF VAR l-dv-ok       AS LOG INIT NO.

DEF VAR da-data-invent LIKE inv-acab.data-invent.
DEF VAR i-docto LIKE inv-acab.docto.

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
&Scoped-define EXTERNAL-TABLES inv-acab
&Scoped-define FIRST-EXTERNAL-TABLE inv-acab


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR inv-acab.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS inv-acab.situacao inv-acab.qtd-inv ~
inv-acab.localizacao inv-acab.un inv-acab.usuario inv-acab.data-trans ~
inv-acab.hora-trans 
&Scoped-define ENABLED-TABLES inv-acab
&Scoped-define FIRST-ENABLED-TABLE inv-acab
&Scoped-Define ENABLED-OBJECTS rt-key rt-mold 
&Scoped-Define DISPLAYED-FIELDS inv-acab.cod-estabel inv-acab.data-invent ~
inv-acab.it-codigo inv-acab.cod-refer inv-acab.situacao inv-acab.qtd-inv ~
inv-acab.num-etiqueta inv-acab.localizacao inv-acab.un inv-acab.usuario ~
inv-acab.data-trans inv-acab.hora-trans 
&Scoped-define DISPLAYED-TABLES inv-acab
&Scoped-define FIRST-DISPLAYED-TABLE inv-acab
&Scoped-Define DISPLAYED-OBJECTS fi-nome-estab fi-desc-item fi-desc-ref 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-CREATE-FIELDS inv-acab.cod-estabel inv-acab.data-invent ~
inv-acab.it-codigo inv-acab.cod-refer 
&Scoped-define ADM-ASSIGN-FIELDS inv-acab.cod-estabel 

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
DEFINE VARIABLE fi-desc-item AS CHARACTER FORMAT "x(60)" 
     VIEW-AS FILL-IN 
     SIZE 55.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-ref AS CHARACTER FORMAT "X(8)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nome-estab AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 31.72 BY .88 NO-UNDO.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88 BY 7.25.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88 BY 1.38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     fi-nome-estab AT ROW 1.25 COL 17.29 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     inv-acab.cod-estabel AT ROW 1.25 COL 13 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 4 BY .88
     inv-acab.data-invent AT ROW 3.25 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
     inv-acab.it-codigo AT ROW 4.25 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.14 BY .88
     fi-desc-item AT ROW 4.25 COL 30.14 COLON-ALIGNED NO-LABEL
     inv-acab.cod-refer AT ROW 5.25 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7 BY .88
     fi-desc-ref AT ROW 5.25 COL 20 COLON-ALIGNED NO-LABEL
     inv-acab.situacao AT ROW 7.25 COL 62 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Pendente":U, 1,
"Atualizado EMS", 2
          SIZE 24.43 BY .71
     inv-acab.qtd-inv AT ROW 8.58 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.57 BY .88
     inv-acab.num-etiqueta AT ROW 2.25 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .88
     inv-acab.localizacao AT ROW 2.25 COL 41 COLON-ALIGNED WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 8 BY .88
     inv-acab.un AT ROW 6.33 COL 13 COLON-ALIGNED WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 4 BY .88
     inv-acab.usuario AT ROW 1.25 COL 66 COLON-ALIGNED WIDGET-ID 12
          VIEW-AS FILL-IN 
          SIZE 13 BY .88
     inv-acab.data-trans AT ROW 2.25 COL 66 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
     inv-acab.hora-trans AT ROW 3.25 COL 66 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 6 BY .88
     "Situaá∆o:" VIEW-AS TEXT
          SIZE 7 BY .54 AT ROW 7.25 COL 54.43
     rt-key AT ROW 1 COL 1
     rt-mold AT ROW 8.38 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: espec.inv-acab
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
         HEIGHT             = 8.79
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit Custom                            */
ASSIGN 
       FRAME f-main:SCROLLABLE       = FALSE
       FRAME f-main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN inv-acab.cod-estabel IN FRAME f-main
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN inv-acab.cod-refer IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN inv-acab.data-invent IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN fi-desc-item IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-ref IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nome-estab IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN inv-acab.it-codigo IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN inv-acab.num-etiqueta IN FRAME f-main
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

&Scoped-define SELF-NAME inv-acab.cod-estabel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-acab.cod-estabel V-table-Win
ON LEAVE OF inv-acab.cod-estabel IN FRAME f-main /* Estabelecimento */
DO:
   FIND estabelec WHERE 
        estabelec.cod-estabel = INPUT FRAME {&FRAME-NAME} inv-acab.cod-estabel NO-LOCK NO-ERROR.
  IF NOT AVAIL estabelec THEN DO.
     MESSAGE "Estabelecimento inv†lido." VIEW-AS ALERT-BOX.
     APPLY 'entry' TO SELF.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-acab.cod-estabel V-table-Win
ON MOUSE-SELECT-DBLCLICK OF inv-acab.cod-estabel IN FRAME f-main /* Estabelecimento */
DO:
 {include/zoomvar.i &prog-zoom = adzoom/z01ad107.w
                     &campo     = inv-acab.cod-estabel
                     &campozoom = cod-estabel}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME inv-acab.cod-refer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-acab.cod-refer V-table-Win
ON ENTRY OF inv-acab.cod-refer IN FRAME f-main /* Referencia */
DO:
  FIND referencia WHERE referencia.cod-refer = INPUT FRAME {&FRAME-NAME} inv-acab.cod-refer
                  NO-LOCK NO-ERROR.
  IF AVAIL referencia THEN
     ASSIGN fi-desc-ref:SCREEN-VALUE IN FRAME {&FRAME-NAME} = referencia.descricao.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-acab.cod-refer V-table-Win
ON LEAVE OF inv-acab.cod-refer IN FRAME f-main /* Referencia */
DO:
  IF item.tipo-con-est <> 4 AND INPUT FRAME {&FRAME-NAME} inv-acab.cod-refer <> "" THEN DO:
     MESSAGE "Item n∆o Ç controlado por Referància. Referància deve ser BRANCO." 
             VIEW-AS ALERT-BOX.
     RETURN NO-APPLY.
  END.
  
  /* Esta rotina foi colocada aqui provisoriamente para atender a uma emergància do Walter.
     Normalmente ela fica somente do leave do campo DV
     Gilvando - 23.12.2004*/
  
  /* Calculo do Digito verificador do item */
  assign i-dv-soma  = 0
         c-item-ref = INPUT FRAME {&FRAME-NAME} inv-acab.it-codigo + 
                      INPUT FRAME {&frame-name} inv-acab.cod-refer.

  DO i-dv-cont = 1 to 13:
     if  substr(c-item-ref,i-dv-cont,1) >= "0" 
     and substr(c-item-ref,i-dv-cont,1) <= "9" then
         assign i-dv-soma = i-dv-soma + 
                int(substr(c-item-ref,i-dv-cont,1)) * i-dv-fatores[i-dv-cont].
  end.
  
  IF item.tipo-con-est = 4 THEN DO:
     FIND referencia WHERE referencia.cod-refer = INPUT FRAME {&FRAME-NAME} inv-acab.cod-refer
                     NO-LOCK NO-ERROR.
     IF AVAIL referencia THEN DO:
        FIND ref-item WHERE ref-item.cod-refer = INPUT FRAME {&FRAME-NAME} inv-acab.cod-refer
                        AND ref-item.it-codigo = INPUT FRAME {&FRAME-NAME} inv-acab.it-codigo
                      NO-LOCK NO-ERROR.
        IF AVAIL ref-item THEN
           ASSIGN fi-desc-ref:SCREEN-VALUE IN FRAME {&FRAME-NAME} = referencia.descricao.
        ELSE DO:
           MESSAGE "Referància n∆o vinculada a esse °tem." VIEW-AS ALERT-BOX.
           RETURN NO-APPLY.
        END.
     END.
     ELSE DO:
        MESSAGE "Referància inv†lida." VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-acab.cod-refer V-table-Win
ON MOUSE-SELECT-DBLCLICK OF inv-acab.cod-refer IN FRAME f-main /* Referencia */
DO:
  {include/zoomvar.i &prog-zoom  = inzoom/z01in375.w
                     &campo      = inv-acab.cod-refer
                     &campozoom  = cod-refer
                     &parametros = "run pi-seta-inicial in 
                      wh-pesquisa(input input frame {&frame-name} inv-acab.it-codigo)."}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME inv-acab.it-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-acab.it-codigo V-table-Win
ON ENTRY OF inv-acab.it-codigo IN FRAME f-main /* Item */
DO:
  FIND ITEM WHERE ITEM.it-codigo = INPUT FRAME {&FRAME-NAME} inv-acab.it-codigo NO-LOCK NO-ERROR.
  IF AVAIL ITEM THEN
     ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ITEM.desc-item.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-acab.it-codigo V-table-Win
ON LEAVE OF inv-acab.it-codigo IN FRAME f-main /* Item */
DO:
  FIND ITEM WHERE ITEM.it-codigo = INPUT FRAME {&FRAME-NAME} inv-acab.it-codigo NO-LOCK NO-ERROR.
  IF AVAIL ITEM THEN
     ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ITEM.desc-item.
  ELSE DO:
     MESSAGE "÷tem inv†lido." VIEW-AS ALERT-BOX.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-acab.it-codigo V-table-Win
ON MOUSE-SELECT-DBLCLICK OF inv-acab.it-codigo IN FRAME f-main /* Item */
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z01in172.w
                     &campo     = inv-acab.it-codigo
                     &campozoom = it-codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME inv-acab.qtd-inv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-acab.qtd-inv V-table-Win
ON LEAVE OF inv-acab.qtd-inv IN FRAME f-main /* Quantidade */
DO:
  IF INPUT FRAME {&FRAME-NAME} inv-acab.qtd-inv = 0 THEN DO:
     MESSAGE "Quantidade deve ser maior que zero." VIEW-AS ALERT-BOX.
     APPLY 'entry' TO inv-acab.qtd-inv IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
  inv-acab.it-codigo:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.
  inv-acab.cod-refer:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.
  inv-acab.cod-estabel:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.

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
  {src/adm/template/row-list.i "inv-acab"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "inv-acab"}

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
    
    /* Dispatch standard ADM method. */
    RUN pi-validate.
    if RETURN-VALUE = 'ADM-ERROR':U then 
       return 'ADM-ERROR':U.

    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.


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
    
 /*   ASSIGN fi-lote:SENSITIVE IN FRAME {&FRAME-NAME} = NO.*/

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
    IF AVAIL inv-acab THEN DO.
       FIND estabelec WHERE
            estabelec.cod-estabel = inv-acab.cod-estabel NO-LOCK NO-ERROR.
       ASSIGN fi-nome-estab:SCREEN-VALUE IN FRAME {&FRAME-NAME} = estabelec.nome.

       FIND item WHERE 
            item.it-codigo = inv-acab.it-codigo NO-LOCK NO-ERROR.
       IF AVAIL item THEN
          ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
       ELSE
          ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".

       FIND referencia WHERE 
            referencia.cod-refer = inv-acab.cod-refer
            NO-LOCK NO-ERROR.
       IF AVAIL referencia THEN
          ASSIGN fi-desc-ref:SCREEN-VALUE IN FRAME {&FRAME-NAME} = referencia.descricao.
       ELSE 
          ASSIGN fi-desc-ref:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
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
    
/*    ASSIGN fi-lote:SENSITIVE IN FRAME {&FRAME-NAME} = YES. */

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
    
        
    /*--- Validaá∆o de DV do ÷tem ---*/

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
  {src/adm/template/snd-list.i "inv-acab"}

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

