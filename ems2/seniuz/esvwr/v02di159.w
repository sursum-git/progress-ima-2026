&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          movdis           PROGRESS
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

DEF VAR c-situacao  AS CHAR.
DEF VAR de-qtd-ped  AS DEC.
DEF VAR de-qtd-fat  AS DEC.
DEF VAR de-qtd-nfat AS DEC.
DEF VAR de-qtd-res  AS DEC.
DEF VAR de-vlr-tot  AS DEC.
DEF VAR de-vlr-res  AS DEC.

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
&Scoped-define EXTERNAL-TABLES ped-venda
&Scoped-define FIRST-EXTERNAL-TABLE ped-venda


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR ped-venda.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS ped-venda.observacoes 
&Scoped-define ENABLED-TABLES ped-venda
&Scoped-define FIRST-ENABLED-TABLE ped-venda
&Scoped-Define ENABLED-OBJECTS rt-mold 
&Scoped-Define DISPLAYED-FIELDS ped-venda.observacoes 
&Scoped-define DISPLAYED-TABLES ped-venda
&Scoped-define FIRST-DISPLAYED-TABLE ped-venda
&Scoped-Define DISPLAYED-OBJECTS fi-nat-operacao fi-ultima-nf fi-dt-ult-nf ~
fi-sit-credito fi-desc-bloq-cr fi-qtd-total fi-qtd-fatur fi-qtd-aberta ~
fi-qtd-reserv fi-vlr-total fi-vlr-reserv 

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
DEFINE VARIABLE fi-desc-bloq-cr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 65.29 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-ult-nf AS DATE FORMAT "99/99/9999":U 
     LABEL "Data Ult.NF" 
     VIEW-AS FILL-IN 
     SIZE 10.29 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nat-operacao AS CHARACTER FORMAT "9.99-xxx":U 
     LABEL "Nat.Opera‡Æo" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-qtd-aberta AS DECIMAL FORMAT ">,>>>,>>9.99":U INITIAL 0 
     LABEL "Qtd.Aberta" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-qtd-fatur AS DECIMAL FORMAT ">,>>>,>>9.99":U INITIAL 0 
     LABEL "Qtd.Faturada" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-qtd-reserv AS DECIMAL FORMAT ">,>>>,>>9.99":U INITIAL 0 
     LABEL "Qtd.Reservada" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-qtd-total AS DECIMAL FORMAT ">,>>>,>>9.99":U INITIAL 0 
     LABEL "Qtd.Total" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-sit-credito AS CHARACTER FORMAT "X(15)":U 
     LABEL "Situa‡Æo Cr‚dito" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ultima-nf AS CHARACTER FORMAT "X(8)":U 
     LABEL "éltima NF" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-vlr-reserv AS DECIMAL FORMAT ">,>>>,>>9.99":U INITIAL 0 
     LABEL "Valor Reserv" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-vlr-total AS DECIMAL FORMAT ">,>>>,>>9.99":U INITIAL 0 
     LABEL "Valor Total" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 93 BY 7.5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     fi-nat-operacao AT ROW 1.46 COL 11.57 COLON-ALIGNED
     fi-ultima-nf AT ROW 1.46 COL 30 COLON-ALIGNED
     fi-dt-ult-nf AT ROW 1.46 COL 48.72 COLON-ALIGNED
     fi-sit-credito AT ROW 2.46 COL 11.57 COLON-ALIGNED
     fi-desc-bloq-cr AT ROW 2.46 COL 25.72 COLON-ALIGNED NO-LABEL
     fi-qtd-total AT ROW 3.46 COL 11.57 COLON-ALIGNED
     fi-qtd-fatur AT ROW 3.46 COL 35.43 COLON-ALIGNED
     fi-qtd-aberta AT ROW 3.46 COL 56.14 COLON-ALIGNED
     fi-qtd-reserv AT ROW 3.46 COL 82 COLON-ALIGNED
     fi-vlr-total AT ROW 4.46 COL 11.57 COLON-ALIGNED
     fi-vlr-reserv AT ROW 4.46 COL 35.43 COLON-ALIGNED
     ped-venda.observacoes AT ROW 5.46 COL 2 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 91 BY 3
     rt-mold AT ROW 1.25 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: movdis.ped-venda
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
         HEIGHT             = 7.75
         WIDTH              = 93.43.
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

/* SETTINGS FOR FILL-IN fi-desc-bloq-cr IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-dt-ult-nf IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nat-operacao IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qtd-aberta IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qtd-fatur IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qtd-reserv IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qtd-total IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-sit-credito IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-ultima-nf IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-vlr-reserv IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-vlr-total IN FRAME f-main
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
  {src/adm/template/row-list.i "ped-venda"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "ped-venda"}

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
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ).
    
    /* Code placed here will execute AFTER standard behavior.    */
    IF AVAIL ped-venda THEN DO:
       find last nota-fiscal WHERE nota-fiscal.nome-ab-cli = ped-venda.nome-abrev
                               and nota-fiscal.nr-pedcli   = ped-venda.nr-pedcli
                             no-lock no-error.
       if avail nota-fiscal then
          assign fi-ultima-nf:SCREEN-VALUE IN FRAME {&FRAME-NAME} = nota-fiscal.nr-nota-fis
                 fi-dt-ult-nf:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(nota-fiscal.dt-emis-nota).
       else
          assign fi-ultima-nf:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
                 fi-dt-ult-nf:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
       
       assign de-qtd-ped  = 0
              de-qtd-nfat = 0.
       for each ped-item of ped-venda no-lock:
           assign de-qtd-ped  = de-qtd-ped  + ped-item.qt-pedida
                  de-qtd-nfat = de-qtd-nfat + (ped-item.qt-pedida -
                                               ped-item.qt-atendida +
                                               ped-item.qt-devolvida).
       end.
       assign de-qtd-fat = de-qtd-ped - de-qtd-nfat.
       assign de-qtd-res = 0
              de-vlr-res = 0.
       for each ped-item-res where ped-item-res.nome-abrev = ped-venda.nome-abrev
                              and ped-item-res.nr-pedcli  = ped-venda.nr-pedcli
                              and ped-item-res.faturado   = no
                             no-lock:
           find ped-item where ped-item.nome-abrev   = ped-item-res.nome-abrev
                           and ped-item.nr-pedcli    = ped-item-res.nr-pedcli
                           and ped-item.nr-sequencia = ped-item-res.nr-sequencia
                           and ped-item.it-codigo    = ped-item-res.it-codigo
                           and ped-item.cod-refer    = ped-item-res.cod-refer
                               no-lock no-error.
           assign de-qtd-res = de-qtd-res + ped-item-res.qt-pedida.
           if avail ped-item then
              assign de-vlr-res = de-vlr-res + ped-item-res.qt-pedida *
                                               ped-item.vl-preori.
       end.
       ASSIGN fi-qtd-total:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(de-qtd-ped)
              fi-qtd-fatur:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(de-qtd-fat)
              fi-qtd-aberta:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(de-qtd-nfat)
              fi-qtd-reserv:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(de-qtd-res)
              fi-vlr-total:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ped-venda.vl-tot-ped)
              fi-vlr-reserv:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(de-vlr-res)
              fi-nat-operacao:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ped-venda.nat-operacao.
       {esinc/i-dsrb.i ped-venda.cod-sit-aval ped-venda.cod-sit-aval c-situacao}
       ASSIGN fi-sit-credito:SCREEN-VALUE IN FRAME {&FRAME-NAME} = c-situacao
              fi-desc-bloq-cr:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ped-venda.desc-bloq-cr.
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
  {src/adm/template/snd-list.i "ped-venda"}

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

