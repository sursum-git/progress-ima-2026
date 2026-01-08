&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i B99XX999 9.99.99.999}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
&Scop adm-attribute-dlg support/browserd.w

/* Parameters Definitions ---                                           */
 
/* Local Variable Definitions ---                                       */

/* Variaveis usadas internamente pelo estilo, favor nao elimina-las     */

/* v†ri†veis de uso globla */
def  var v-row-parent    as rowid no-undo.

/* vari†veis de uso local */
def var v-row-table  as rowid no-undo.

/* fim das variaveis utilizadas no estilo */

DEF BUFFER bf-etiqueta FOR bc-etiqueta.

DEF VAR i-qt-soma LIKE bc-etiqueta.qt-item NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-viewer AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-browse AS HANDLE NO-UNDO.

def temp-table tt-erro no-undo
    field i-sequen as int             
    field cd-erro  as int
    field mensagem as char format "x(255)".

DEF VAR h-viewer        AS HANDLE                   NO-UNDO.
DEF VAR i-cod-emitente  LIKE emitente.cod-emitente  NO-UNDO.
DEF VAR c-nr-pedcli     LIKE ped-venda.nr-pedcli    NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE BrowserCadastro2
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-table

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES bc-etiqueta
&Scoped-define FIRST-EXTERNAL-TABLE bc-etiqueta


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR bc-etiqueta.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES bf-etiqueta

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br-table                                      */
&Scoped-define FIELDS-IN-QUERY-br-table bf-etiqueta.progressivo bf-etiqueta.qt-item bf-etiqueta.dt-criacao bf-etiqueta.hr-criacao bf-etiqueta.usuar-criacao   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-table   
&Scoped-define SELF-NAME br-table
&Scoped-define QUERY-STRING-br-table FOR EACH bf-etiqueta USE-INDEX ch-nr-etiq-pai     WHERE bf-etiqueta.nr-etiq-pai = bc-etiqueta.nr-etiq
&Scoped-define OPEN-QUERY-br-table OPEN QUERY {&SELF-NAME} FOR EACH bf-etiqueta USE-INDEX ch-nr-etiq-pai     WHERE bf-etiqueta.nr-etiq-pai = bc-etiqueta.nr-etiq.
&Scoped-define TABLES-IN-QUERY-br-table bf-etiqueta
&Scoped-define FIRST-TABLE-IN-QUERY-br-table bf-etiqueta


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-table bt-incluir bt-modificar bt-eliminar 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" B-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS
><EXECUTING-CODE>
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Advanced Query Options" B-table-Win _INLINE
/* Actions: ? adm/support/advqedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<SORTBY-OPTIONS>
</SORTBY-OPTIONS>
<SORTBY-RUN-CODE>
************************
* Set attributes related to SORTBY-OPTIONS */
RUN set-attribute-list (
    'SortBy-Options = ""':U).
/************************
</SORTBY-RUN-CODE>
<FILTER-ATTRIBUTES>
************************
* Initialize Filter Attributes */
RUN set-attribute-list IN THIS-PROCEDURE ('
  Filter-Value=':U).
/************************
</FILTER-ATTRIBUTES> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-eliminar 
     LABEL "&Eliminar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-imprime 
     LABEL "&Imprimir" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-incluir 
     LABEL "&Incluir" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-modificar 
     LABEL "&Modificar" 
     SIZE 10 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-table FOR 
      bf-etiqueta SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-table B-table-Win _FREEFORM
  QUERY br-table NO-LOCK DISPLAY
      bf-etiqueta.progressivo COLUMN-LABEL "Nr Etiqueta" FORMAT "X(12)"
        bf-etiqueta.qt-item   
        bf-etiqueta.dt-criacao
        bf-etiqueta.hr-criacao
        bf-etiqueta.usuar-criacao
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 66 BY 4.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br-table AT ROW 1 COL 1
     bt-incluir AT ROW 5.04 COL 1
     bt-modificar AT ROW 5.04 COL 11
     bt-eliminar AT ROW 5.04 COL 21
     bt-imprime AT ROW 5.04 COL 57
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: BrowserCadastro2
   External Tables: mgcld.bc-etiqueta
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: External-Tables
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 5.17
         WIDTH              = 66.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{utp/ut-glob.i}
{src/adm/method/browser.i}
{include/c-brows3.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit L-To-R                            */
/* BROWSE-TAB br-table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON bt-imprime IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-table
/* Query rebuild information for BROWSE br-table
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH bf-etiqueta USE-INDEX ch-nr-etiq-pai
    WHERE bf-etiqueta.nr-etiq-pai = bc-etiqueta.nr-etiq.
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Query            is NOT OPENED
*/  /* BROWSE br-table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br-table
&Scoped-define SELF-NAME br-table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table B-table-Win
ON MOUSE-SELECT-DBLCLICK OF br-table IN FRAME F-Main
DO:
    RUN New-State("DblClick, SELF":U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table B-table-Win
ON ROW-ENTRY OF br-table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table B-table-Win
ON ROW-LEAVE OF br-table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table B-table-Win
ON VALUE-CHANGED OF br-table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}

      IF  AVAIL bf-etiqueta THEN DO:
          IF  bf-etiqueta.cod-estado = 1 THEN
              ASSIGN bt-imprime:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
          ELSE
              ASSIGN bt-imprime:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

      END.
      IF  AVAIL bc-etiqueta THEN DO:
          IF  bc-etiqueta.qt-item = 0 THEN
              ASSIGN bt-incluir:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
          ELSE
              ASSIGN bt-incluir:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
      END.

      
    /* run new-state('New-Line|':U + string(rowid({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}))). */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-eliminar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-eliminar B-table-Win
ON CHOOSE OF bt-eliminar IN FRAME F-Main /* Eliminar */
DO:
    ASSIGN i-qt-soma = bf-etiqueta.qt-item.
    RUN pi-eliminar.
    IF  RETURN-VALUE = "":U THEN DO:
        FIND CURRENT bc-etiqueta SHARE-LOCK NO-ERROR.
        ASSIGN bc-etiqueta.qt-item = bc-etiqueta.qt-item + i-qt-soma.
        RUN dispatch IN h-viewer ( INPUT 'display-fields':U ) .
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-imprime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-imprime B-table-Win
ON CHOOSE OF bt-imprime IN FRAME F-Main /* Imprimir */
DO:
    FOR EACH tt-erro:
        DELETE tt-erro.
    END.
    
    RUN bcp/bcx2000.p (INPUT ROWID(bf-etiqueta),
                        INPUT "IMA0002Q",
                        INPUT-OUTPUT TABLE tt-erro).

    FOR EACH tt-erro NO-LOCK :
        MESSAGE 'Ocorreram erros na impress∆o da etiqueta FILHO.' SKIP
                'Verifique os erros antes de imprimir.' SKIP
                tt-erro.cd-erro ' - ' tt-erro.mensagem VIEW-AS ALERT-BOX ERROR TITLE "Erro Etiqueta FILHO".
        RETURN NO-APPLY.
    END.

    RUN bcp/bcx2000.p (INPUT ROWID(bc-etiqueta),
                        INPUT "IMA0002Q",
                        INPUT-OUTPUT TABLE tt-erro).
    FOR EACH tt-erro NO-LOCK :
        MESSAGE 'Ocorreram erros na impress∆o da etiqueta PAI.' SKIP
                'Verifique os erros antes de imprimir.' SKIP
                tt-erro.cd-erro ' - ' tt-erro.mensagem VIEW-AS ALERT-BOX ERROR TITLE "Erro Etiqueta PAI".
        RETURN NO-APPLY.
    END.
    ASSIGN bt-imprime:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-incluir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-incluir B-table-Win
ON CHOOSE OF bt-incluir IN FRAME F-Main /* Incluir */
DO:
    RUN pi-busca-valores IN h-viewer (OUTPUT i-cod-emitente,
                                      OUTPUT c-nr-pedcli).

    IF  i-cod-emitente <> 0 OR
        c-nr-pedcli    <> "" THEN DO:
        RUN pi-verifica-pedido.
        IF  RETURN-VALUE = "NOK" THEN
            RETURN NO-APPLY.
    END.
            
    
    RUN pi-Incmod ('incluir').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-modificar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-modificar B-table-Win
ON CHOOSE OF bt-modificar IN FRAME F-Main /* Modificar */
DO:
  RUN pi-Incmod ('modificar').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available B-table-Win  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI B-table-Win  _DEFAULT-DISABLE
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
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize B-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .
  ASSIGN h-browse = THIS-PROCEDURE.
  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-open-query B-table-Win 
PROCEDURE local-open-query :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  APPLY "value-changed" TO br-table IN FRAME {&FRAME-NAME}.



  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-view B-table-Win 
PROCEDURE local-view :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'view':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  apply 'value-changed':U to {&browse-name} in frame {&frame-name}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-recebe-handle B-table-Win 
PROCEDURE pi-recebe-handle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM p-viewer AS HANDLE.

ASSIGN h-viewer = p-viewer.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-verifica-pedido B-table-Win 
PROCEDURE pi-verifica-pedido :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


    FIND emitente
        WHERE emitente.cod-emitente = i-cod-emitente
        NO-LOCK NO-ERROR.
    IF  NOT AVAIL emitente THEN DO:
        MESSAGE "Emitente informado n∆o Ç um Emitente v†lido!" VIEW-AS ALERT-BOX ERROR TITLE "Erro".
        RETURN "NOK":U.
    END.

    FIND ped-item
        WHERE ped-item.nome-abrev = emitente.nome-abrev
          AND ped-item.nr-pedcli  = c-nr-pedcli
          AND ped-item.it-codigo  = bc-etiqueta.it-codigo
          AND ped-item.cod-refer  = bc-etiqueta.referencia
       USE-INDEX ch-cli-ped NO-LOCK NO-ERROR.
    IF  NOT AVAIL ped-item THEN DO:
        MESSAGE "N∆o ser† poss°vel fazer o corte da etiqueta pois o item desta " skip
                "Etiqueta n∆o pertence ao Pedido. Verifique!" VIEW-AS ALERT-BOX ERROR TITLE "Erro".
        RETURN "NOK":U.
    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records B-table-Win  _ADM-SEND-RECORDS
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
  {src/adm/template/snd-list.i "bf-etiqueta"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed B-table-Win 
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
      {src/adm/template/bstates.i}
  END CASE.
  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

