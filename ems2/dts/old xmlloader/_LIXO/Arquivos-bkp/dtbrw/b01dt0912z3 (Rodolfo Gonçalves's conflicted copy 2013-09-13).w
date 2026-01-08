&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          mgmov            PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-saldo-terc NO-UNDO LIKE saldo-terc
       field vrowid as rowid.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*:T *******************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-buffer.i}
{include/i-prgvrs.i B01DT0912Z3 2.06.00.002}

/* Chamada a include do gerenciador de licen‡as. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */
/*                                                                                */
/* OBS: Para os smartobjects o parametro m¢dulo dever  ser MUT                    */

/* &IF "{&EMSFND_VERSION}" >= "1.00" &THEN          */
/*     {include/i-license-manager.i <programa> MUT} */
/* &ENDIF                                           */

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
define variable c-lista-valor as character init '':U no-undo.
DEFINE VARIABLE cDescricao  AS CHAR FORMAT "X(60)" NO-UNDO.
DEFINE VARIABLE cNatAux     LIKE natur-oper.nat-operacao.
DEFINE VARIABLE deQuantItem AS DECIMAL FORMAT ">>,>>>,>>>,>>9.9999" NO-UNDO.
DEFINE VARIABLE deQtAloc    AS DECIMAL FORMAT ">>,>>>,>>>,>>9.9999" NO-UNDO.
DEFINE VARIABLE deQuant     AS DECIMAL FORMAT ">>,>>>,>>,>>9.9999" NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-saldo-terc

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br-table                                      */
&Scoped-define FIELDS-IN-QUERY-br-table tt-saldo-terc.serie-docto tt-saldo-terc.nat-operacao tt-saldo-terc.nro-docto tt-saldo-terc.sequencia tt-saldo-terc.nr-ord-produ tt-saldo-terc.it-codigo tt-saldo-terc.quantidade tt-saldo-terc.dec-1 tt-saldo-terc.valor[1] tt-saldo-terc.cod-depos tt-saldo-terc.lote tt-saldo-terc.cod-refer fnDescItem(tt-saldo-terc.it-codigo) @ cDescricao   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-table   
&Scoped-define SELF-NAME br-table
&Scoped-define QUERY-STRING-br-table FOR EACH tt-saldo-terc  NO-LOCK
&Scoped-define OPEN-QUERY-br-table OPEN QUERY {&SELF-NAME} FOR EACH tt-saldo-terc  NO-LOCK .
&Scoped-define TABLES-IN-QUERY-br-table tt-saldo-terc
&Scoped-define FIRST-TABLE-IN-QUERY-br-table tt-saldo-terc


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS bt-confirma IMAGE-25 IMAGE-26 IMAGE-27 ~
IMAGE-28 c-inicial c-final br-table 
&Scoped-Define DISPLAYED-OBJECTS c-inicial c-final c-inicial-2 c-final-2 

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
cod-emitente|y|y|mgmov.saldo-terc.cod-emitente
num-pedido||y|mgmov.saldo-terc.num-pedido
sequencia||y|mgmov.saldo-terc.sequencia
it-codigo||y|mgmov.saldo-terc.it-codigo
nr-ord-produ||y|mgmov.saldo-terc.nr-ord-produ
numero-ordem||y|mgmov.saldo-terc.numero-ordem
cod-refer||y|mgmov.saldo-terc.cod-refer
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "cod-emitente",
     Keys-Supplied = "cod-emitente,num-pedido,sequencia,it-codigo,nr-ord-produ,numero-ordem,cod-refer"':U).

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

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnDescItem B-table-Win 
FUNCTION fnDescItem RETURNS CHARACTER
  ( INPUT cItem AS CHAR /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnQtdAlocada B-table-Win 
FUNCTION fnQtdAlocada RETURNS DECIMAL
  ( INPUT rwSaldoTerc AS ROWID /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnQtdeSaldo B-table-Win 
FUNCTION fnQtdeSaldo RETURNS DECIMAL
  ( INPUT rwSaldoTerc AS ROWID /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-confirma 
     IMAGE-UP FILE "image\im-sav":U
     LABEL "Button 1" 
     SIZE 5.14 BY 1.

DEFINE VARIABLE c-final AS CHARACTER FORMAT "x(16)" INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 16.72 BY .88 NO-UNDO.

DEFINE VARIABLE c-final-2 AS INTEGER FORMAT ">>>>>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16.72 BY .88 NO-UNDO.

DEFINE VARIABLE c-inicial AS CHARACTER FORMAT "x(16)" 
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 15.29 BY .88 NO-UNDO.

DEFINE VARIABLE c-inicial-2 AS INTEGER FORMAT ">>>>>>>>9" INITIAL 0 
     LABEL "Emitente" 
     VIEW-AS FILL-IN 
     SIZE 15.29 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-25
     FILENAME "image\ii-fir":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-26
     FILENAME "image\ii-las":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-27
     FILENAME "image\ii-fir":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-28
     FILENAME "image\ii-las":U
     SIZE 2.86 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-table FOR 
      tt-saldo-terc SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-table B-table-Win _FREEFORM
  QUERY br-table NO-LOCK DISPLAY
      tt-saldo-terc.serie-docto                                 FORMAT "x(5)":U
      tt-saldo-terc.nat-operacao                                FORMAT "x(06)":U
      tt-saldo-terc.nro-docto                                   FORMAT "x(16)":U
      tt-saldo-terc.sequencia                                   FORMAT ">>>>9":U WIDTH 6.86
      tt-saldo-terc.nr-ord-produ                                FORMAT ">>>,>>>,>>9":U
      tt-saldo-terc.it-codigo                                   FORMAT "x(16)":U WIDTH 13.57
      tt-saldo-terc.quantidade                                  FORMAT ">>,>>>,>>>,>>9.9999":U
      tt-saldo-terc.dec-1           COLUMN-LABEL "Qt Alocada"   FORMAT ">>,>>>,>>>,>>9.9999":U
      tt-saldo-terc.valor[1]        COLUMN-LABEL "Pre‡o Total"  FORMAT ">>,>>>,>>>,>>9.99":U
      tt-saldo-terc.cod-depos                                   FORMAT "x(3)":U WIDTH 5.57
      tt-saldo-terc.lote                                        FORMAT "x(10)":U WIDTH 12.86
      tt-saldo-terc.cod-refer                                   FORMAT "x(8)":U
      
      fnDescItem(tt-saldo-terc.it-codigo) @ cDescricao COLUMN-LABEL "Desri‡ao" FORMAT "X(60)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 92 BY 10.5
         FONT 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     bt-confirma AT ROW 1.04 COL 86
     c-inicial AT ROW 1.13 COL 10 COLON-ALIGNED WIDGET-ID 20
     c-final AT ROW 1.13 COL 48.29 COLON-ALIGNED NO-LABEL WIDGET-ID 18
     c-inicial-2 AT ROW 2.08 COL 10 COLON-ALIGNED WIDGET-ID 28
     c-final-2 AT ROW 2.08 COL 48.29 COLON-ALIGNED NO-LABEL WIDGET-ID 26
     br-table AT ROW 3.17 COL 1
     IMAGE-25 AT ROW 1.13 COL 27.72 WIDGET-ID 22
     IMAGE-26 AT ROW 1.13 COL 46.86 WIDGET-ID 24
     IMAGE-27 AT ROW 2.08 COL 27.72 WIDGET-ID 30
     IMAGE-28 AT ROW 2.08 COL 46.86 WIDGET-ID 32
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 FONT 1 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: tt-saldo-terc T "?" NO-UNDO mgmov saldo-terc
      ADDITIONAL-FIELDS:
          field vrowid as rowid
      END-FIELDS.
   END-TABLES.
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
         HEIGHT             = 12.92
         WIDTH              = 92.72.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{include/c-brwzoo.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit L-To-R                            */
/* BROWSE-TAB br-table c-final-2 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN c-final-2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-inicial-2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-table
/* Query rebuild information for BROWSE br-table
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-saldo-terc  NO-LOCK .
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION KEY-PHRASE"
     _Where[1]         = "mgmov.saldo-terc.it-codigo >= c-inicial
 AND mgmov.saldo-terc.it-codigo <= c-final
 AND mgmov.saldo-terc.cod-emitente >= c-inicial-2
 AND mgmov.saldo-terc.cod-emitente <= c-final-2"
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
    RUN New-State('DblClick':U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table B-table-Win
ON ROW-ENTRY OF br-table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}
  
  run new-state('New-Line|':U + string(rowid({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}))).
  run seta-valor.
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
  run new-state('New-Line|':U + string(rowid({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}))).
  run new-state('Value-Changed|':U + string(this-procedure)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-confirma
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-confirma B-table-Win
ON CHOOSE OF bt-confirma IN FRAME F-Main /* Button 1 */
DO:
  assign input frame {&frame-name} c-inicial c-final c-inicial-2 c-final-2.

  RUN pi-carrega-tt.
  OPEN QUERY br-table 
        FOR EACH tt-saldo-terc 
             NO-LOCK.  
  APPLY 'value-changed':U TO {&browse-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF
 ASSIGN
       c-inicial  :sensitive in frame {&frame-name}     = YES
       c-inicial-2:sensitive in frame {&frame-name}     = no
       c-final    :sensitive in frame {&frame-name}     = YES
       c-final-2  :sensitive in frame {&frame-name}     = no   .

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
  DEF VAR key-value AS CHAR NO-UNDO.

  DEF VAR Filter-Value AS CHAR NO-UNDO.

  /* Copy 'Filter-Attributes' into local variables. */
  RUN get-attribute ('Filter-Value':U).
  Filter-Value = RETURN-VALUE.
  /* Look up the current key-value. */
  RUN get-attribute ('Key-Value':U).
  key-value = RETURN-VALUE.

  /* Find the current record using the current Key-Name. */
  RUN get-attribute ('Key-Name':U).
  CASE RETURN-VALUE:
    WHEN 'cod-emitente':U THEN DO:
       &Scope KEY-PHRASE saldo-terc.cod-emitente eq key-value
       {&OPEN-QUERY-{&BROWSE-NAME}}
    END. /* cod-emitente */
    OTHERWISE DO:
       &Scope KEY-PHRASE TRUE
       {&OPEN-QUERY-{&BROWSE-NAME}}
    END. /* OTHERWISE...*/
  END CASE.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-carrega-tt B-table-Win 
PROCEDURE pi-carrega-tt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
EMPTY TEMP-TABLE tt-saldo-terc.
FOR EACH saldo-terc 
           WHERE saldo-terc.cod-emitente >= c-inicial-2
             AND saldo-terc.cod-emitente <= c-final-2
             AND saldo-terc.it-codigo    >= c-inicial
             AND saldo-terc.it-codigo    <= c-final   
              NO-LOCK.
    CREATE tt-saldo-terc.
    BUFFER-COPY saldo-terc TO tt-saldo-terc.
    ASSIGN tt-saldo-terc.vrowid = ROWID(saldo-terc).
           
END.
FOR EACH tt-saldo-terc EXCLUSIVE-LOCK:
    ASSIGN tt-saldo-terc.dec-1 = fnQtdAlocada(tt-saldo-terc.vrowid)
           tt-saldo-terc.quantidade = fnQtdeSaldo(tt-saldo-terc.vrowid).
    IF tt-saldo-terc.quantidade <= 0 OR 
       tt-saldo-terc.quantidade < deQuantItem THEN
        DELETE tt-saldo-terc.

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-faixa B-table-Win 
PROCEDURE pi-faixa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAM iEmitente    AS INTEGER FORMAT ">>>>>>>>9" NO-UNDO.
DEFINE INPUT PARAM cItem        AS CHAR    FORMAT "X(16)" NO-UNDO.
DEFINE INPUT PARAM cNatureza    LIKE natur-oper.nat-operacao.
DEFINE INPUT PARAM pQtItem      AS DECIMAL FORMAT ">>,>>>,>>>,>>9.9999" NO-UNDO.
ASSIGN c-inicial  :screen-value in frame {&frame-name}  = ""  
       c-inicial-2:screen-value in frame {&frame-name}  = STRING(iEmitente)  
       c-final    :screen-value in frame {&frame-name}  = "ZZZZZZZZZZZZZZZZ"      
       c-final-2  :screen-value in frame {&frame-name}  = STRING(iEmitente)  
       c-inicial  :sensitive in frame {&frame-name}     = YES
       c-inicial-2:sensitive in frame {&frame-name}     = no
       c-final    :sensitive in frame {&frame-name}     = YES
       c-final-2  :sensitive in frame {&frame-name}     = no   
       cNatAux     = cNatureza
       deQuantItem = pQtItem.

    ASSIGN c-inicial    = ""     
           c-inicial-2  = iEmitente  
           c-final      = "ZZZZZZZZZZZZZZZZ"    
           c-final-2    = iEmitente .
    RUN pi-carrega-tt.
    OPEN QUERY br-table 
        FOR EACH tt-saldo-terc 
/*           WHERE saldo-terc.cod-emitente >= c-inicial-2
             AND saldo-terc.cod-emitente <= c-final-2
             AND saldo-terc.it-codigo    >= c-inicial
             AND saldo-terc.it-codigo    <= c-final    */ NO-LOCK.
 ASSIGN
       c-inicial  :sensitive in frame {&frame-name}     = YES
       c-inicial-2:sensitive in frame {&frame-name}     = no
       c-final    :sensitive in frame {&frame-name}     = YES
       c-final-2  :sensitive in frame {&frame-name}     = no   .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-retorna-valor B-table-Win 
PROCEDURE pi-retorna-valor :
DEFINE INPUT PARAMETER P-CAMPO AS CHARACTER NO-UNDO.

    DEFINE VARIABLE P-VALOR AS CHAR INIT "" NO-UNDO.

    if  avail tt-saldo-terc then do:
        case p-campo:
            when "sequencia" then
                assign p-valor = string(tt-saldo-terc.sequencia).
            when "it-codigo" then
                assign p-valor = string(tt-saldo-terc.it-codigo).
            when "quantidade" then
                assign p-valor = string(tt-saldo-terc.quantidade).
            when "dec-1" then
                assign p-valor = string(tt-saldo-terc.dec-1).
            when "valor[1]" then
                assign p-valor = string(tt-saldo-terc.valor[1]).
            when "cod-depos" then
                assign p-valor = string(tt-saldo-terc.cod-depos).
            when "lote" then
                assign p-valor = string(tt-saldo-terc.lote).
            when "cod-refer" then
                assign p-valor = string(tt-saldo-terc.cod-refer).
            WHEN "serie" THEN
                assign p-valor = string(tt-saldo-terc.serie).
            WHEN "nat-operacao" THEN
                assign p-valor = string(tt-saldo-terc.nat-operacao).
            WHEN "nro-docto" THEN
                assign p-valor = string(tt-saldo-terc.nro-docto).
            WHEN "nr-ord-produ" THEN
                ASSIGN p-valor = STRING(tt-saldo-terc.nr-ord-produ).
        end.
    end.
    return p-valor.
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
  {src/adm/template/sndkycas.i "cod-emitente" "saldo-terc" "cod-emitente"}
  {src/adm/template/sndkycas.i "num-pedido" "saldo-terc" "num-pedido"}
  {src/adm/template/sndkycas.i "sequencia" "saldo-terc" "sequencia"}
  {src/adm/template/sndkycas.i "it-codigo" "saldo-terc" "it-codigo"}
  {src/adm/template/sndkycas.i "nr-ord-produ" "saldo-terc" "nr-ord-produ"}
  {src/adm/template/sndkycas.i "numero-ordem" "saldo-terc" "numero-ordem"}
  {src/adm/template/sndkycas.i "cod-refer" "saldo-terc" "cod-refer"}

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
  {src/adm/template/snd-list.i "tt-saldo-terc"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "RetornaValorCampo" B-table-Win _INLINE
/* Actions: ? ? ? ? support/brwrtval.p */
/* Procedure desativada */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnDescItem B-table-Win 
FUNCTION fnDescItem RETURNS CHARACTER
  ( INPUT cItem AS CHAR /* parameter-definitions */ ) :
  def var c-desc-item as char no-undo.

  FIND FIRST ITEM WHERE ITEM.it-codigo = cItem NO-LOCK NO-ERROR.
  IF AVAIL ITEM THEN
      RETURN ITEM.desc-item.
  ELSE
      RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnQtdAlocada B-table-Win 
FUNCTION fnQtdAlocada RETURNS DECIMAL
  ( INPUT rwSaldoTerc AS ROWID /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE BUFFER bSaldoTerc FOR saldo-terc.
  DEFINE BUFFER bNaturOper FOR natur-oper.
  DEFINE VARIABLE deQtdeEmFatur         AS DEC NO-UNDO.
  DEFINE VARIABLE lMovtoDevSimbConsig   AS LOGICAL NO-UNDO.

  FIND FIRST bSaldoTerc WHERE ROWID(bSaldoTerc) = rwSaldoTerc NO-LOCK NO-ERROR.
  IF AVAIL bSaldoTerc THEN DO:
      FIND FIRST natur-oper WHERE natur-oper.nat-operacao = cNatAux NO-LOCK NO-ERROR.
      IF  {cdp/cd0066.i2 bSaldoTerc.cod-estabel}
            AND AVAIL natur-oper 
            AND natur-oper.tp-oper-terc = 4 THEN DO:
            ASSIGN deQtdeEmFatur = 0.
            
            FOR EACH componente NO-LOCK
                WHERE componente.cod-emitente = bSaldoTerc.cod-emitente
                  AND componente.nro-comp     = bSaldoTerc.nro-docto
                  AND componente.serie-comp   = bSaldoTerc.serie-docto
                  AND componente.nat-comp     = bSaldoTerc.nat-operacao
                  AND componente.seq-comp     = bSaldoTerc.sequencia
                  AND componente.it-codigo    = bSaldoTerc.it-codigo:
                FIND FIRST bNaturOper NO-LOCK
                     WHERE bNaturOper.nat-operacao = componente.nat-operacao NO-ERROR.
                IF  AVAIL natur-oper THEN DO:
                    ASSIGN lMovtoDevSimbConsig = {cdp/cd0066.i1 bNaturOper componente}.
/*                     IF  natur-oper.terceiros AND                                          */
/*                         natur-oper.tp-oper-terc = 5 AND    /* Devolu»’o de Consigna»’o */ */
/*                       &IF "{&mguni_version}" >= "2.08" &THEN  /* Simb½lica */             */
/*                        (natur-oper.idi-tip-devol-consig = 1 OR                            */
/*                         componente.idi-tip-devol-consig = 1)                              */
/*                       &ELSE                                                               */
/*                        (INT(SUBSTR(natur-oper.char-1,140,1)) = 1 OR                       */
/*                         INT(SUBSTR(componente.char-1, 82,1)) = 1)                         */
/*                       &ENDIF THEN                                                         */
/*                         ASSIGN lMovtoDevSimbConsig = YES.                                 */
/*                     ELSE                                                                  */
/*                         ASSIGN lMovtoDevSimbConsig = NO.                                  */

                    /* ASSIGN lMovtoDevSimbConsig = {cdp/cd0066.i1 natur-oper componente} */
                END.
                ELSE
                    ASSIGN lMovtoDevSimbConsig = NO.
                
                IF  lMovtoDevSimbConsig THEN DO:
                    ASSIGN deQtdeEmFatur = deQtdeEmFatur +
                           &IF "{&mguni_version}" >= "2.08" &THEN
                                componente.qtd-afatur-consig
                           &ELSE
                              DEC(SUBSTR(componente.char-1,54,14))
                           &ENDIF .
                END.
            END.
            
/*             IF  deQtdeAFatur > 0 THEN                                       */
/*                 ASSIGN de-quantidade = (deQtdeAFatur * piProporcao ) / 100. */
/*             IF  deQtdeEmFatur > 0 THEN                                      */
/*                 ASSIGN RowObjectAux.dec-1 = deQtdeEmFatur.                  */
      END.
      ELSE DO:
        ASSIGN deQtdeEmFatur = (( bSaldoTerc.quantidade - bSaldoTerc.dec-1 ) * 100 ) / 100.
      END.

  END.
  RETURN deQtdeEmFatur.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnQtdeSaldo B-table-Win 
FUNCTION fnQtdeSaldo RETURNS DECIMAL
  ( INPUT rwSaldoTerc AS ROWID /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE BUFFER bSaldoTerc FOR saldo-terc.
  DEFINE BUFFER bNaturOper FOR natur-oper.
  DEFINE VARIABLE deQtdeAFatur          AS DEC NO-UNDO.
  DEFINE VARIABLE lMovtoDevSimbConsig   AS LOGICAL NO-UNDO.
  FIND FIRST bSaldoTerc WHERE ROWID(bSaldoTerc) = rwSaldoTerc NO-LOCK NO-ERROR.
  IF AVAIL bSaldoTerc THEN DO:
      FIND FIRST natur-oper WHERE natur-oper.nat-operacao = cNatAux NO-LOCK NO-ERROR.
      IF  {cdp/cd0066.i2 bSaldoTerc.cod-estabel}
            AND AVAIL natur-oper  
            AND natur-oper.tp-oper-terc = 4 THEN DO:
            ASSIGN deQtdeAFatur = 0.
            
            FOR EACH componente NO-LOCK
                WHERE componente.cod-emitente = bSaldoTerc.cod-emitente
                  AND componente.nro-comp     = bSaldoTerc.nro-docto
                  AND componente.serie-comp   = bSaldoTerc.serie-docto
                  AND componente.nat-comp     = bSaldoTerc.nat-operacao
                  AND componente.seq-comp     = bSaldoTerc.sequencia
                  AND componente.it-codigo    = bSaldoTerc.it-codigo:
                FIND FIRST bNaturOper NO-LOCK
                     WHERE bNaturOper.nat-operacao = componente.nat-operacao NO-ERROR.
                IF  AVAIL bNaturOper THEN DO:
                    ASSIGN lMovtoDevSimbConsig = {cdp/cd0066.i1 bNaturOper componente}.
                END.
                ELSE
                    ASSIGN lMovtoDevSimbConsig = NO.
                
                IF  lMovtoDevSimbConsig THEN DO:
                    ASSIGN deQtdeAFatur = deQtdeAFatur + 
                           &IF "{&mguni_version}" >= "2.08" &THEN
                            (componente.qtd-devol-simbol - componente.qtd-fatur-consig - componente.qtd-afatur-consig)
                           &ELSE
                            (DEC(SUBSTR(componente.char-1,40,14)) - DEC(SUBSTR(componente.char-1,68,14)) - DEC(SUBSTR(componente.char-1,54,14)))
                           &ENDIF .
                END.
            END.
            
/*             IF  deQtdeAFatur > 0 THEN                                       */
/*                 ASSIGN de-quantidade = (deQtdeAFatur * piProporcao ) / 100. */
/*             IF  deQtdeAFatur > 0 THEN                                      */
/*                 ASSIGN RowObjectAux.dec-1 = deQtdeAFatur.                  */
      END.
      ELSE DO:
        ASSIGN deQtdeAFatur = (( bSaldoTerc.quantidade - bSaldoTerc.dec-1 ) * 100 ) / 100.
      END.

  END.
  FIND FIRST ITEM WHERE ITEM.it-codigo = bSaldoTerc.it-codigo NO-LOCK NO-ERROR.
   IF  avail item 
   and not(item.fraciona)
   and natur-oper.tp-oper-terc <> 6 then
       assign deQtdeAFatur = truncate(deQtdeAFatur, 0 ).

  RETURN deQtdeAFatur.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

