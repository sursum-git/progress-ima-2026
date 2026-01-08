&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          espec            PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*:T *******************************************************************************
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
DEF BUFFER bf-etiqueta FOR ob-etiqueta.
DEF TEMP-TABLE tt-etiqueta LIKE ob-etiqueta.

/*:T Variaveis usadas internamente pelo estilo, favor nao elimina-las     */

/*:T v†ri†veis de uso globla */
def  var v-row-parent    as rowid no-undo.

/*:T vari†veis de uso local */
def var v-row-table  as rowid no-undo.
DEF VAR l-perfeito AS LOGICAL.




/* Definiá∆o das Includes para o Reporte */
{cdp/cdcfgman.i}
{cdp/cd0666.i}
{cpp/cpapi009.i}

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
&Scoped-define EXTERNAL-TABLES espec.ordem-benefic
&Scoped-define FIRST-EXTERNAL-TABLE espec.ordem-benefic


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR espec.ordem-benefic.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES espec.ob-etiqueta

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br-table                                      */
&Scoped-define FIELDS-IN-QUERY-br-table espec.ob-etiqueta.dt-emissao ~
espec.ob-etiqueta.num-etiqueta espec.ob-etiqueta.quantidade ~
espec.ob-etiqueta.nr-lote espec.ob-etiqueta.nr-reporte ~
espec.ob-etiqueta.nr-ord-prod espec.ob-etiqueta.nr-revisadeira ~
espec.ob-etiqueta.resp-revisao espec.ob-etiqueta.hr-emissao 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-table 
&Scoped-define QUERY-STRING-br-table FOR EACH espec.ob-etiqueta OF espec.ordem-benefic WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-table OPEN QUERY br-table FOR EACH espec.ob-etiqueta OF espec.ordem-benefic WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-table espec.ob-etiqueta
&Scoped-define FIRST-TABLE-IN-QUERY-br-table espec.ob-etiqueta


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-table bt-eliminar 

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
it-codigo||y|espec.ob-etiqueta.it-codigo
cod-refer||y|espec.ob-etiqueta.cod-refer
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "it-codigo,cod-refer"':U).

/* Tell the ADM to use the OPEN-QUERY-CASES. */
&Scoped-define OPEN-QUERY-CASES RUN dispatch ('open-query-cases':U).
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

DEFINE BUTTON bt-incluir 
     LABEL "&Incluir" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-modificar 
     LABEL "&Modificar" 
     SIZE 10 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-table FOR 
      espec.ob-etiqueta SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-table B-table-Win _STRUCTURED
  QUERY br-table NO-LOCK DISPLAY
      espec.ob-etiqueta.dt-emissao COLUMN-LABEL "Dt Emiss∆o" FORMAT "99/99/9999":U
            WIDTH 10
      espec.ob-etiqueta.num-etiqueta FORMAT ">>>>>>>>>9":U
      espec.ob-etiqueta.quantidade FORMAT ">>>,>>9.99":U
      espec.ob-etiqueta.nr-lote FORMAT "X(4)":U
      espec.ob-etiqueta.nr-reporte FORMAT ">>>>>>>>9":U
      espec.ob-etiqueta.nr-ord-prod COLUMN-LABEL "Ordem Prod" FORMAT ">>>,>>>,>>9":U
            WIDTH 12
      espec.ob-etiqueta.nr-revisadeira FORMAT ">9":U
      espec.ob-etiqueta.resp-revisao COLUMN-LABEL "Revisador" FORMAT "X(12)":U
      espec.ob-etiqueta.hr-emissao COLUMN-LABEL "Hora" FORMAT "x(5)":U
            WIDTH 6
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 87 BY 7
         FONT 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br-table AT ROW 1 COL 1
     bt-incluir AT ROW 8.17 COL 1.57
     bt-modificar AT ROW 8.17 COL 11.57
     bt-eliminar AT ROW 8.17 COL 21.57
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: BrowserCadastro2
   External Tables: espec.ordem-benefic
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
         HEIGHT             = 8.25
         WIDTH              = 87.29.
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

/* SETTINGS FOR BUTTON bt-incluir IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-modificar IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-table
/* Query rebuild information for BROWSE br-table
     _TblList          = "espec.ob-etiqueta OF espec.ordem-benefic"
     _Options          = "NO-LOCK INDEXED-REPOSITION KEY-PHRASE SORTBY-PHRASE"
     _FldNameList[1]   > espec.ob-etiqueta.dt-emissao
"ob-etiqueta.dt-emissao" "Dt Emiss∆o" ? "date" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > espec.ob-etiqueta.num-etiqueta
"ob-etiqueta.num-etiqueta" ? ">>>>>>>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   = espec.ob-etiqueta.quantidade
     _FldNameList[4]   > espec.ob-etiqueta.nr-lote
"ob-etiqueta.nr-lote" ? "X(4)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   = espec.ob-etiqueta.nr-reporte
     _FldNameList[6]   > espec.ob-etiqueta.nr-ord-prod
"ob-etiqueta.nr-ord-prod" "Ordem Prod" ? "integer" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   = espec.ob-etiqueta.nr-revisadeira
     _FldNameList[8]   > espec.ob-etiqueta.resp-revisao
"ob-etiqueta.resp-revisao" "Revisador" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > espec.ob-etiqueta.hr-emissao
"ob-etiqueta.hr-emissao" "Hora" "x(5)" "character" ? ? ? ? ? ? no ? no no "6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
  /* run new-state('New-Line|':U + string(rowid({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}))). */
  
  IF AVAIL ob-etiqueta THEN DO.
     IF ob-etiqueta.dt-emissao <= param-estoq.mensal-ate THEN
        ASSIGN bt-eliminar:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
     ELSE
        ASSIGN bt-eliminar:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-eliminar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-eliminar B-table-Win
ON CHOOSE OF bt-eliminar IN FRAME F-Main /* Eliminar */
DO:
   FIND CURRENT ob-etiqueta NO-LOCK.
   IF ob-etiqueta.situacao = 3 THEN DO:
      MESSAGE "Etiqueta est† EM ESTOQUE. N∆o pode ser exclu°da nesta situaá∆o."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
   END.
   IF ob-etiqueta.situacao = 4 THEN DO:
      MESSAGE "Etiqueta est† RESERVADA. N∆o pode ser exclu°da nesta situaá∆o." SKIP
              "Para que ela possa ser exclu°da Ç necess†rio cancelar antes a sua reserva."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
   END.
   IF ob-etiqueta.situacao = 5 THEN DO:
      MESSAGE "Etiqueta est† FATURADA. N∆o pode ser exclu°da nesta situaá∆o." SKIP
              "Para que ela possa ser exclu°da Ç necess†rio cancelar antes a NF e a sua reserva."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
   END.
   IF ob-etiqueta.situacao > 5 THEN DO:
      MESSAGE "Etiqueta n∆o pode ser exclu°da nesta situaá∆o."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
   END.
    
   RUN utp/ut-msgs.p (INPUT "show":U,
                      INPUT 550,
                      INPUT RETURN-VALUE).
   IF RETURN-VALUE = 'NO' THEN 
      RETURN NO-APPLY.

   SESSION:SET-WAIT-STATE("general":U).

   FIND CURRENT ob-etiqueta SHARE-LOCK.

   FIND mov-est-acbm WHERE
        mov-est-acbm.data-mov = ob-etiqueta.dt-emissao AND
        mov-est-acbm.num-lote = ob-etiqueta.nr-ob AND
        mov-est-acbm.it-codigo = ob-etiqueta.it-codigo AND
        mov-est-acbm.cod-refer = ob-etiqueta.cod-refer
        SHARE-LOCK NO-ERROR.

   ASSIGN l-perfeito = YES.
   FOR EACH mov-est-acbd WHERE
            mov-est-acbd.data-mov = ob-etiqueta.dt-emissao AND
            mov-est-acbd.num-lote = ob-etiqueta.nr-ob AND
            mov-est-acbd.nr-carro = ob-etiqueta.nr-carro AND
            mov-est-acbd.acondic  = ob-etiqueta.acondic AND
            mov-est-acbd.nr-sequencia = ob-etiqueta.nr-sequencia AND
            mov-est-acbd.classif <> "RT" SHARE-LOCK.

       IF AVAIL mov-est-acbm THEN
          ASSIGN mov-est-acbm.qtd-tot-def = mov-est-acbm.qtd-tot-def - mov-est-acbd.qtd-defeit.
       DELETE mov-est-acbd.

       ASSIGN l-perfeito = NO.
   END.
   IF AVAIL mov-est-acbm AND l-perfeito THEN 
      ASSIGN mov-est-acbm.qtd-tot-perf = mov-est-acbm.qtd-tot-perf - ob-etiqueta.quantidade.

   IF AVAIL mov-est-acbm AND ob-etiqueta.nuance <> "" THEN
      RUN pi-del-nuance.


   CREATE movto-etq.
   ASSIGN movto-etq.dt-trans = TODAY
          movto-etq.esp-docto = 'DEL'
          movto-etq.nro-docto = STRING(ob-etiqueta.nr-ob)
          movto-etq.num-etiqueta = ob-etiqueta.num-etiqueta 
          movto-etq.quantidade = ob-etiqueta.quantidade
          movto-etq.tipo-trans = NO
          movto-etq.char-1 = "Usuario: " + c-seg-usuario + FILL(" ",10) +
                             "Hora: " + STRING(TIME,"HH:MM:SS") + FILL(" ",10) +
                             "Situacao: " + STRING(ob-etiqueta.situacao).

   DELETE ob-etiqueta.
   RUN adm-open-query-cases.

   SESSION:SET-WAIT-STATE("":U).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-incluir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-incluir B-table-Win
ON CHOOSE OF bt-incluir IN FRAME F-Main /* Incluir */
DO:
  RUN pi-Incmod ('incluir':U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-modificar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-modificar B-table-Win
ON CHOOSE OF bt-modificar IN FRAME F-Main /* Modificar */
DO:
  RUN pi-Incmod ('modificar':U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

FIND FIRST param-estoq NO-LOCK NO-ERROR.
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-open-query-cases B-table-Win  adm/support/_adm-opn.p
PROCEDURE adm-open-query-cases :
/*------------------------------------------------------------------------------
  Purpose:     Opens different cases of the query based on attributes
               such as the 'Key-Name', or 'SortBy-Case'
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEF VAR Filter-Value AS CHAR NO-UNDO.

  /* Copy 'Filter-Attributes' into local variables. */
  RUN get-attribute ('Filter-Value':U).
  Filter-Value = RETURN-VALUE.

  /* No Foreign keys are accepted by this SmartObject. */

  {&OPEN-QUERY-{&BROWSE-NAME}}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  {src/adm/template/row-list.i "espec.ordem-benefic"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "espec.ordem-benefic"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-del-nuance B-table-Win 
PROCEDURE pi-del-nuance :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR i-ind AS INT.

    DO i-ind = 1 TO EXTENT(mov-est-acbm.nuance-cla).
       IF mov-est-acbm.nuance-cla[i-ind] = ob-etiqueta.nuance THEN LEAVE.
    END.
    ASSIGN mov-est-acbm.nuance-qtd[i-ind] = mov-est-acbm.nuance-qtd[i-ind] - ob-etiqueta.quantidade.

    IF mov-est-acbm.nuance-qtd[i-ind] = 0 THEN
       ASSIGN mov-est-acbm.nuance-cla[i-ind] = "".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-estornar B-table-Win 
PROCEDURE pi-estornar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH tt-rep-prod.
    DELETE tt-rep-prod.
END.

CREATE tt-rep-prod.
ASSIGN tt-rep-prod.cod-versao-integracao = 001
       tt-rep-prod.nr-reporte = ob-etiqueta.nr-reporte
       tt-rep-prod.data = ob-etiqueta.dt-emissao.

RUN cpp/cpapi011.p (INPUT        TABLE tt-rep-prod,
                    INPUT-OUTPUT TABLE tt-erro,
                    INPUT        TABLE tt-relatorio,
                    INPUT        YES).

IF RETURN-VALUE = 'NOK' THEN DO.
   FOR EACH tt-erro.
       MESSAGE tt-erro.mensagem VIEW-AS ALERT-BOX INFO BUTTONS OK.
       RETURN 'NOK'.
   END.
END.
ELSE DO.
   FOR EACH bf-etiqueta WHERE
            bf-etiqueta.nr-reporte = ob-etiqueta.nr-reporte.
       ASSIGN bf-etiqueta.qtd-estornar = 0
              bf-etiqueta.nr-reporte = 0
              bf-etiqueta.nr-ord-prod = 0.
   END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key B-table-Win  adm/support/_key-snd.p
PROCEDURE send-key :
/*------------------------------------------------------------------------------
  Purpose:     Sends a requested KEY value back to the calling
               SmartObject.
  Parameters:  <see adm/template/sndkytop.i>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/sndkytop.i}

  /* Return the key value associated with each key case.             */
  {src/adm/template/sndkycas.i "it-codigo" "espec.ob-etiqueta" "it-codigo"}
  {src/adm/template/sndkycas.i "cod-refer" "espec.ob-etiqueta" "cod-refer"}

  /* Close the CASE statement and end the procedure.                 */
  {src/adm/template/sndkyend.i}

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
  {src/adm/template/snd-list.i "espec.ordem-benefic"}
  {src/adm/template/snd-list.i "espec.ob-etiqueta"}

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

  ASSIGN bt-incluir:SENSITIVE IN FRAME {&FRAME-NAME} = NO
         bt-modificar:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

  IF AVAIL ob-etiqueta AND ob-etiqueta.dt-emissao <= param-estoq.mensal-ate THEN 
     ASSIGN bt-eliminar:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

