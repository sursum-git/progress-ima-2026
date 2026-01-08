&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          ems2cad          PROGRESS
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

/*:T Variaveis usadas internamente pelo estilo, favor nao elimina-las     */

/*:T v ri veis de uso globla */
def new global shared var v-row-parent as rowid no-undo.

/*:T fim das variaveis utilizadas no estilo */
DEF NEW GLOBAL SHARED VAR wh-campo-conteudo AS WIDGET-HANDLE.
DEF NEW GLOBAL SHARED VAR wh-window         AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-pesquisa       AS WIDGET-HANDLE.
DEF NEW GLOBAL SHARED VAR l-implanta        AS LOGICAL INIT NO.
DEF NEW GLOBAL SHARED VAR adm-broker-hdl    AS HANDLE NO-UNDO.

DEFINE NEW GLOBAL SHARED VAR gr-emitente     AS ROWID NO-UNDO.

DEF TEMP-TABLE tt-cnae LIKE cnaes
    FIELD cod-tipo-cnae LIKE emitente_cnae.cod_tipo_cnae
    FIELD eliminado     LIKE emitente_cnae.eliminado 
    FIELD usr_elim      LIKE emitente_cnae.usr_elim  
    FIELD dt_elim       LIKE emitente_cnae.dt_elim   
    FIELD hr_elim       LIKE emitente_cnae.hr_elim.

DEF BUFFER b-tt-cnae FOR tt-cnae.

/*:T vari veis de uso local */
def var v-row-table  as rowid.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-cnae

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES emitente
&Scoped-define FIRST-EXTERNAL-TABLE emitente


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR emitente.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-cnae

/* Definitions for BROWSE br-cnae                                       */
&Scoped-define FIELDS-IN-QUERY-br-cnae tt-cnae.cod_cnae tt-cnae.descricao   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-cnae   
&Scoped-define SELF-NAME br-cnae
&Scoped-define QUERY-STRING-br-cnae FOR EACH tt-cnae WHERE                                  tt-cnae.eliminado = NO NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-cnae OPEN QUERY {&SELF-NAME} FOR EACH tt-cnae WHERE                                  tt-cnae.eliminado = NO NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-cnae tt-cnae
&Scoped-define FIRST-TABLE-IN-QUERY-br-cnae tt-cnae


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS emitente.int-1 
&Scoped-define ENABLED-TABLES emitente
&Scoped-define FIRST-ENABLED-TABLE emitente
&Scoped-Define ENABLED-OBJECTS FILL-IN-1 RECT-6 RECT-5 br-cnae 
&Scoped-Define DISPLAYED-FIELDS emitente.int-1 
&Scoped-define DISPLAYED-TABLES emitente
&Scoped-define FIRST-DISPLAYED-TABLE emitente
&Scoped-Define DISPLAYED-OBJECTS fi-cod-ramo-ativ fi-desc-ramo fi_cod_cnae ~
tg-primario fi-finalidade tg-varejo tg-atacado tg-industria tg-servico ~
FILL-IN-1 txt-primario 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 fi-cod-ramo-ativ fi_cod_cnae bt-cnae tg-primario ~
bt-del 
&Scoped-define List-3 fi-cod-ramo-ativ tg-varejo tg-atacado tg-industria ~
tg-servico 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" B-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<FOREIGN-KEYS>
cod_cnae||y|espec.emitente_cnae.cod_cnae
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "cod_cnae"':U).

/* Tell the ADM to use the OPEN-QUERY-CASES. */
&Scoped-define OPEN-QUERY-CASES RUN dispatch ('open-query-cases':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Advanced Query Optionsososos" B-table-Win _INLINE
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
DEFINE BUTTON bt-cnae 
     IMAGE-UP FILE "image/im-vapra.bmp":U
     LABEL "Button 1" 
     SIZE 4.43 BY 1.

DEFINE BUTTON bt-del 
     IMAGE-UP FILE "image/im-era.bmp":U
     LABEL "bt cnae 2" 
     SIZE 4.43 BY 1.25.

DEFINE VARIABLE fi-cod-ramo-ativ AS INTEGER FORMAT "999" INITIAL 0 
     LABEL "C¢digo Ramo" 
     VIEW-AS FILL-IN 
     SIZE 5.29 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-ramo AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 44.14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-finalidade AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 18 BY .79 NO-UNDO.

DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 2 BY .5
     BGCOLOR 12  NO-UNDO.

DEFINE VARIABLE fi_cod_cnae AS CHARACTER FORMAT "x(20)" 
     LABEL "CNAE" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE txt-primario AS CHARACTER FORMAT "X(256)":U INITIAL "CNAE Prim rio" 
      VIEW-AS TEXT 
     SIZE 14 BY .67
     BGCOLOR 8 FGCOLOR 12 FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 21 BY 8.75.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 85 BY 12.

DEFINE VARIABLE tg-atacado AS LOGICAL INITIAL no 
     LABEL "Atacado" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE tg-industria AS LOGICAL INITIAL no 
     LABEL "Industria" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE tg-primario AS LOGICAL INITIAL no 
     LABEL "Prim rio" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE tg-servico AS LOGICAL INITIAL no 
     LABEL "Servi‡o" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE tg-varejo AS LOGICAL INITIAL no 
     LABEL "Varejo" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-cnae FOR 
      tt-cnae SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-cnae
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-cnae B-table-Win _FREEFORM
  QUERY br-cnae NO-LOCK DISPLAY
      tt-cnae.cod_cnae   FORMAT "x(10)":U  COLUMN-LABEL "CNAE"
      tt-cnae.descricao                    COLUMN-LABEL "Descri‡Æo"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 61 BY 8
         FONT 1
         TITLE "CNAEs do Cliente".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fi-cod-ramo-ativ AT ROW 1.5 COL 11 COLON-ALIGNED WIDGET-ID 16
     fi-desc-ramo AT ROW 1.5 COL 16.86 COLON-ALIGNED NO-LABEL WIDGET-ID 18
     fi_cod_cnae AT ROW 2.75 COL 11 COLON-ALIGNED WIDGET-ID 32
     bt-cnae AT ROW 2.71 COL 25.43 WIDGET-ID 38
     tg-primario AT ROW 4.54 COL 66 WIDGET-ID 36
     fi-finalidade AT ROW 6.38 COL 64 COLON-ALIGNED NO-LABEL WIDGET-ID 30
     tg-varejo AT ROW 8.46 COL 70.29 WIDGET-ID 6
     tg-atacado AT ROW 9.21 COL 70.29 WIDGET-ID 8
     tg-industria AT ROW 9.96 COL 70.29 WIDGET-ID 10
     tg-servico AT ROW 10.71 COL 70.29 WIDGET-ID 12
     emitente.int-1 AT ROW 1.25 COL 80 COLON-ALIGNED WIDGET-ID 40
          LABEL "NÆo Remover"
          VIEW-AS FILL-IN 
          SIZE 3 BY .88
     bt-del AT ROW 11.38 COL 64.43 WIDGET-ID 44
     FILL-IN-1 AT ROW 12.25 COL 2.29 NO-LABEL WIDGET-ID 48
     txt-primario AT ROW 12.21 COL 3 COLON-ALIGNED NO-LABEL WIDGET-ID 46
     br-cnae AT ROW 4 COL 2
     "Finalid. Venda" VIEW-AS TEXT
          SIZE 13 BY .54 AT ROW 5.79 COL 66 WIDGET-ID 42
     " Atividade:" VIEW-AS TEXT
          SIZE 7.57 BY .75 AT ROW 7.71 COL 66 WIDGET-ID 28
     RECT-6 AT ROW 1 COL 1 WIDGET-ID 20
     RECT-5 AT ROW 4 COL 64 WIDGET-ID 26
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 FONT 1 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   External Tables: ems2cad.emitente
   Allow: Basic,Browse
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
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 12
         WIDTH              = 85.43.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{include/c-browse.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit Custom                            */
/* BROWSE-TAB br-cnae RECT-5 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON bt-cnae IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR BUTTON bt-del IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN fi-cod-ramo-ativ IN FRAME F-Main
   NO-ENABLE 1 3                                                        */
/* SETTINGS FOR FILL-IN fi-desc-ramo IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-finalidade IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-1 IN FRAME F-Main
   ALIGN-L                                                              */
ASSIGN 
       FILL-IN-1:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN fi_cod_cnae IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN emitente.int-1 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-atacado IN FRAME F-Main
   NO-ENABLE 3                                                          */
/* SETTINGS FOR TOGGLE-BOX tg-industria IN FRAME F-Main
   NO-ENABLE 3                                                          */
/* SETTINGS FOR TOGGLE-BOX tg-primario IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR TOGGLE-BOX tg-servico IN FRAME F-Main
   NO-ENABLE 3                                                          */
/* SETTINGS FOR TOGGLE-BOX tg-varejo IN FRAME F-Main
   NO-ENABLE 3                                                          */
/* SETTINGS FOR FILL-IN txt-primario IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-cnae
/* Query rebuild information for BROWSE br-cnae
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-cnae WHERE
                                 tt-cnae.eliminado = NO NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is NOT OPENED
*/  /* BROWSE br-cnae */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br-cnae
&Scoped-define SELF-NAME br-cnae
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-cnae B-table-Win
ON ROW-DISPLAY OF br-cnae IN FRAME F-Main /* CNAEs do Cliente */
DO:

  IF tt-cnae.cod-tipo-cnae = 1 THEN DO.
     ASSIGN tt-cnae.cod_cnae:FGCOLOR IN BROWSE br-cnae = 12
            tt-cnae.descricao:FGCOLOR IN BROWSE br-cnae = 12. 
     ASSIGN tt-cnae.cod_cnae:FONT IN BROWSE br-cnae = 6
            tt-cnae.descricao:FONT IN BROWSE br-cnae = 6. 
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-cnae B-table-Win
ON VALUE-CHANGED OF br-cnae IN FRAME F-Main /* CNAEs do Cliente */
DO:
    ASSIGN tg-primario:SCREEN-VALUE   = 'NO'
           tg-varejo:SCREEN-VALUE     = 'NO'
           tg-atacado:SCREEN-VALUE    = 'NO'
           tg-industria:SCREEN-VALUE  = 'NO'
           tg-servico:SCREEN-VALUE    = 'NO'
           fi-finalidade:SCREEN-VALUE = ''.

   ASSIGN bt-del:SENSITIVE = NO.
   IF AVAIL tt-cnae THEN DO.
      IF tt-cnae.cod-tipo-cnae = 1 THEN
         ASSIGN tg-primario:SCREEN-VALUE = 'YES'.

      CASE tt-cnae.cod_finalidade:
          WHEN '1' THEN ASSIGN fi-finalidade:SCREEN-VALUE = 'Envio para Dep¢sito'.
          WHEN '2' THEN ASSIGN fi-finalidade:SCREEN-VALUE = 'Consumo Pr¢prio'.
          WHEN '3' THEN ASSIGN fi-finalidade:SCREEN-VALUE = 'Envio para Industrializa‡Æo'.
          WHEN '4' THEN ASSIGN fi-finalidade:SCREEN-VALUE = 'Revenda de Mercadoria'.
      END.

      CASE tt-cnae.ind_tipo_atividade.
          WHEN '1' THEN ASSIGN tg-varejo:SCREEN-VALUE = 'YES'.
          WHEN '2' THEN ASSIGN tg-atacado:SCREEN-VALUE = 'YES'.
          WHEN '3' THEN ASSIGN tg-industria:SCREEN-VALUE = 'YES'.
          WHEN '4' THEN ASSIGN tg-servico:SCREEN-VALUE = 'YES'.
      END CASE.

      ASSIGN bt-del:SENSITIVE = fi_cod_cnae:SENSITIVE.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cnae
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cnae B-table-Win
ON CHOOSE OF bt-cnae IN FRAME F-Main /* Button 1 */
DO:
   FIND FIRST cnaes WHERE 
              cnaes.cod_cnae = fi_cod_cnae:SCREEN-VALUE NO-LOCK NO-ERROR.

   IF NOT AVAIL cnaes THEN DO.
      MESSAGE 'Cnae NÇO Cadastrado...'
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY 'ENTRY' TO SELF.
      RETURN NO-APPLY.
   END.

   FIND tt-cnae WHERE
        tt-cnae.cod_cnae = cnaes.cod_cnae NO-LOCK NO-ERROR.
   IF NOT AVAIL tt-cnae THEN DO.
      CREATE tt-cnae.
      BUFFER-COPY cnaes TO tt-cnae.
   END.
   
   ASSIGN tt-cnae.eliminado = NO
          tt-cnae.usr_elim  = ""
          tt-cnae.dt_elim   = ?
          tt-cnae.hr_elim   = "".
   
   /*
   FIND b-tt-cnae WHERE
        b-tt-cnae.cod-tipo-cnae = 1 NO-LOCK NO-ERROR.
   IF NOT AVAIL b-tt-cnae THEN
      ASSIGN tt-cnae.cod-tipo-cnae = 1.
   */

   {&OPEN-QUERY-br-cnae}
   APPLY 'VALUE-CHANGED' TO br-cnae.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-del
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-del B-table-Win
ON CHOOSE OF bt-del IN FRAME F-Main /* bt cnae 2 */
DO:
   ASSIGN tt-cnae.eliminado = YES
          tt-cnae.usr_elim = c-seg-usuario
          tt-cnae.dt_elim = TODAY
          tt-cnae.hr_elim = STRING(TIME,"HH:MM:SS").
    
   {&OPEN-QUERY-br-cnae}
   APPLY 'VALUE-CHANGED' TO br-cnae.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-ramo-ativ
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-ramo-ativ B-table-Win
ON LEAVE OF fi-cod-ramo-ativ IN FRAME F-Main /* C¢digo Ramo */
DO:
   FIND FIRST ramo-ativ WHERE 
              ramo-ativ.cod-ramo-ativ = INPUT FRAME {&FRAME-NAME} fi-cod-ramo-ativ NO-LOCK NO-ERROR.
   
   ASSIGN fi-desc-ramo:SCREEN-VALUE = "".
   IF AVAIL ramo-ativ THEN
      ASSIGN fi-desc-ramo:SCREEN-VALUE = ramo-ativ.descricao.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-ramo-ativ B-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-cod-ramo-ativ IN FRAME F-Main /* C¢digo Ramo */
DO:
  {include/zoomvar.i &prog-zoom=eszoom/z01ra001.r
                     &campo     = fi-cod-ramo-ativ
                     &campozoom = cod-ramo-ativ}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-atacado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-atacado B-table-Win
ON VALUE-CHANGED OF tg-atacado IN FRAME F-Main /* Atacado */
DO:
  RUN pi-trata-atividade.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-industria
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-industria B-table-Win
ON VALUE-CHANGED OF tg-industria IN FRAME F-Main /* Industria */
DO:
  RUN pi-trata-atividade.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-primario
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-primario B-table-Win
ON VALUE-CHANGED OF tg-primario IN FRAME F-Main /* Prim rio */
DO:
   ASSIGN v-row-table = ROWID(tt-cnae).

   FOR EACH b-tt-cnae.
       ASSIGN b-tt-cnae.cod-tipo-cnae = 2.
   END.

   IF SELF:SCREEN-VALUE = 'YES' THEN
      ASSIGN tt-cnae.cod-tipo-cnae = 1.

   {&OPEN-QUERY-br-cnae}

   br-cnae:QUERY:REPOSITION-TO-ROWID(v-row-table).
   APPLY 'VALUE-CHANGED' TO br-cnae.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-servico
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-servico B-table-Win
ON VALUE-CHANGED OF tg-servico IN FRAME F-Main /* Servi‡o */
DO:
  RUN pi-trata-atividade.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-varejo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-varejo B-table-Win
ON VALUE-CHANGED OF tg-varejo IN FRAME F-Main /* Varejo */
DO:
   RUN pi-trata-atividade.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-open-query-cases B-table-Win  adm/support/_adm-opn.p
PROCEDURE adm-open-query-cases :
/*------------------------------------------------------------------------------
  Purpose:     Opens different cases of the query based on attributes
               such as the 'Key-Name', or 'SortBy-Case'
  Parameters:  <none>
------------------------------------------------------------------------------*/

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
  {src/adm/template/row-list.i "emitente"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "emitente"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record B-table-Win 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN fi-cod-ramo-ativ:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ''
           fi-desc-ramo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ''.

    EMPTY TEMP-TABLE tt-cnae.   
    {&OPEN-QUERY-br-cnae}
    APPLY 'VALUE-CHANGED' TO br-cnae.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record B-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FIND emitente WHERE
         ROWID(emitente) = gr-emitente NO-LOCK NO-ERROR.

    ASSIGN INPUT FRAME {&FRAME-NAME} fi-cod-ramo-ativ     fi-finalidade
                                     tg-varejo            tg-primario 
                                     tg-atacado           tg-industria
                                     tg-servico.

    RUN pi-validate.
    IF RETURN-VALUE = 'ADM-ERROR' THEN
       RETURN 'ADM-ERROR'.

    FIND ext-emitente WHERE
         ext-emitente.cod-emitente = emitente.cod-emitente SHARE-LOCK NO-ERROR.
    IF NOT AVAIL ext-emitente THEN DO.
       CREATE ext-emitente.
       ASSIGN ext-emitente.cod-emitente = emitente.cod-emitente.
    END.
    ASSIGN ext-emitente.cod-ramo-ativ = fi-cod-ramo-ativ.

    FOR EACH tt-cnae NO-LOCK.
        FIND emitente_cnae WHERE
             emitente_cnae.cod_emitente = emitente.cod-emit AND
             emitente_cnae.cod_cnae = tt-cnae.cod_cnae
             SHARE-LOCK NO-ERROR.
        IF NOT AVAIL emitente_cnae THEN DO.
           CREATE emitente_cnae.
           ASSIGN emitente_cnae.cod_emitente = emitente.cod-emit 
                  emitente_cnae.cod_cnae = tt-cnae.cod_cnae.
        END.
        ASSIGN emitente_cnae.cod_tipo_cnae = tt-cnae.cod-tipo-cnae
               emitente_cnae.eliminado     = tt-cnae.eliminado
               emitente_cnae.usr_elim      = tt-cnae.usr_elim 
               emitente_cnae.dt_elim       = tt-cnae.dt_elim  
               emitente_cnae.hr_elim       = tt-cnae.hr_elim. 
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-copy-record B-table-Win 
PROCEDURE local-copy-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-disable-fields B-table-Win 
PROCEDURE local-disable-fields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /* Code placed here will execute PRIOR to standard behavior. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'disable-fields':U ) .
    
    /* Code placed here will execute AFTER standard behavior.    */
    &if  defined(ADM-MODIFY-FIELDS) &then
         disable {&ADM-MODIFY-FIELDS} with frame {&frame-name}.
    &endif

    DISABLE {&list-1} WITH FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields B-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE tt-cnae.
    ASSIGN fi-cod-ramo-ativ:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ''
           fi-desc-ramo:SCREEN-VALUE  = ""
           fi_cod_cnae:SCREEN-VALUE   = "".

    /* Code placed here will execute PRIOR to standard behavior. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .
    
    /* Code placed here will execute AFTER standard behavior.    */

    IF AVAIL emitente THEN DO.
       FIND ext-emitente WHERE
            ext-emitente.cod-emitente = emitente.cod-emitente NO-LOCK NO-ERROR.
       IF AVAIL ext-emitente THEN DO.
          ASSIGN fi-cod-ramo-ativ:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ext-emitente.cod-ramo-ativ).

          FIND FIRST ramo-ativ WHERE 
                     ramo-ativ.cod-ramo-ativ = ext-emitente.cod-ramo-ativ NO-LOCK NO-ERROR.

          IF AVAIL ramo-ativ THEN
             ASSIGN fi-desc-ramo:SCREEN-VALUE = ramo-ativ.descricao.  
       END.

       FOR EACH emitente_cnae WHERE 
                emitente_cnae.cod_emitente = emitente.cod-emit NO-LOCK.

           FIND cnaes where
                cnaes.cod_cnae = emitente_cnae.cod_cnae NO-LOCK NO-ERROR.
           
           CREATE tt-cnae.
           BUFFER-COPY cnaes TO tt-cnae
               ASSIGN tt-cnae.cod-tipo-cnae = emitente_cnae.cod_tipo_cnae.
           
           ASSIGN tt-cnae.eliminado = emitente_cnae.eliminado  
                  tt-cnae.usr_elim  = emitente_cnae.usr_elim    
                  tt-cnae.dt_elim   = emitente_cnae.dt_elim     
                  tt-cnae.hr_elim   = emitente_cnae.hr_elim.     
       END.                         
       {&OPEN-QUERY-br-cnae}
       APPLY 'VALUE-CHANGED' TO br-cnae.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields B-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
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

    ENABLE {&list-1} WITH FRAME {&FRAME-NAME}.
    
    ASSIGN emitente.int-1:SENSITIVE = NO.
    ASSIGN emitente.int-1:VISIBLE = NO.
    
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-validate B-table-Win 
PROCEDURE pi-validate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FIND FIRST ramo-ativ WHERE 
              ramo-ativ.cod-ramo-ativ = INPUT FRAME {&FRAME-NAME} fi-cod-ramo-ativ 
              NO-LOCK NO-ERROR.

   IF NOT AVAIL ramo-ativ THEN DO.
      MESSAGE 'Ramo de Atividade NÆo Cadastrado...'
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      RETURN 'ADM-ERROR'.
   END.

   FIND FIRST tt-cnae WHERE
              tt-cnae.cod-tipo-cnae = 1 NO-LOCK NO-ERROR.
   IF NOT AVAIL tt-cnae THEN DO.
      MESSAGE 'Favor informar CNAE Prim rio...'
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      RETURN 'ADM-ERROR'.
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
  {src/adm/template/sndkycas.i "cod_cnae" "emitente_cnae" "cod_cnae"}

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
  {src/adm/template/snd-list.i "emitente"}
  {src/adm/template/snd-list.i "tt-cnae"}

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

