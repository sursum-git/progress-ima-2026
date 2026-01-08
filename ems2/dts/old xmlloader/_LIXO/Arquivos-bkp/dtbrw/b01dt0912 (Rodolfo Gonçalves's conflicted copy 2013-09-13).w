&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r11 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          mgcad            PROGRESS
          xmlloader        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
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
{include/i-prgvrs.i B01DT0912 2.06.00.002}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE NEW GLOBAL SHARED VAR vRowIDDTDocumEst AS ROWID NO-UNDO.
/* Local Variable Definitions ---                                       */
DEF TEMP-TABLE tt-sel
    FIELD c-estab-ini       AS CHAR
    FIELD c-estab-fim       AS CHAR
    FIELD d-data-ini        AS DATE
    FIELD d-data-fim        AS DATE
    FIELD c-serie-ini       AS CHAR
    FIELD c-serie-fim       AS CHAR
    FIELD c-nr-docto-ini    AS CHAR
    FIELD c-nr-docto-fim    AS CHAR
    FIELD c-fornec-ini      AS INT
    FIELD c-fornec-fim      AS INT
    FIELD c-chave-ini       AS CHAR
    FIELD c-chave-fim       AS CHAR.

DEF VAR c-estab-ini       AS CHAR.
DEF VAR c-estab-fim       AS CHAR.
DEF VAR d-data-ini        AS DATE.
DEF VAR d-data-fim        AS DATE.
DEF VAR c-serie-ini       AS CHAR.
DEF VAR c-serie-fim       AS CHAR.
DEF VAR c-nr-docto-ini    AS CHAR.
DEF VAR c-nr-docto-fim    AS CHAR.
DEF VAR c-fornec-ini      AS INT .
DEF VAR c-fornec-fim      AS INT .
DEF VAR c-chave-ini       AS CHAR.
DEF VAR c-chave-fim       AS CHAR.
def var h-acomp           as handle no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-docto

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES dt-empresa
&Scoped-define FIRST-EXTERNAL-TABLE dt-empresa


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR dt-empresa.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES dt-docum-est emitente

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br-docto                                      */
&Scoped-define FIELDS-IN-QUERY-br-docto dt-docum-est.serie-docto dt-docum-est.nro-docto dt-docum-est.cod-emitente emitente.nome-emit dt-docum-est.dt-emissao dt-docum-est.tot-peso dt-docum-est.valor-mercad dt-docum-est.dec-1   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-docto dt-docum-est.nro-docto ~
dt-docum-est.cod-emitente ~
   dt-docum-est.dt-emissao   
&Scoped-define ENABLED-TABLES-IN-QUERY-br-docto dt-docum-est
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br-docto dt-docum-est
&Scoped-define SELF-NAME br-docto
&Scoped-define QUERY-STRING-br-docto FOR EACH dt-docum-est NO-LOCK     WHERE dt-docum-est.ep-codigo = dt-empresa.ep-codigo       AND dt-docum-est.log-situacao = FALSE       AND dt-docum-est.cod-estabel  >= c-estab-ini       AND dt-docum-est.cod-estabel  <= c-estab-fim       AND dt-docum-est.serie        >= c-serie-ini       AND dt-docum-est.serie        <= c-serie-fim       AND dt-docum-est.nro-docto    >= c-nr-docto-ini       AND dt-docum-est.nro-docto    <= c-nr-docto-fim       AND dt-docum-est.cod-emitente >= c-fornec-ini       AND dt-docum-est.cod-emitente <= c-fornec-fim       AND dt-docum-est.log-situacao = no       AND dt-docum-est.chave-xml    >= c-chave-ini       AND dt-docum-est.chave-xml       <= c-chave-fim       AND dt-docum-est.dt-emissao   >= d-data-ini       AND dt-docum-est.dt-emissao   <= d-data-fim, ~
           FIRST emitente         WHERE emitente.cod-emitente = dt-docum-est.cod-emitente NO-LOCK INDEXED-REPOSITION.     ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br-docto OPEN QUERY {&SELF-NAME} FOR EACH dt-docum-est NO-LOCK     WHERE dt-docum-est.ep-codigo = dt-empresa.ep-codigo       AND dt-docum-est.log-situacao = FALSE       AND dt-docum-est.cod-estabel  >= c-estab-ini       AND dt-docum-est.cod-estabel  <= c-estab-fim       AND dt-docum-est.serie        >= c-serie-ini       AND dt-docum-est.serie        <= c-serie-fim       AND dt-docum-est.nro-docto    >= c-nr-docto-ini       AND dt-docum-est.nro-docto    <= c-nr-docto-fim       AND dt-docum-est.cod-emitente >= c-fornec-ini       AND dt-docum-est.cod-emitente <= c-fornec-fim       AND dt-docum-est.log-situacao = no       AND dt-docum-est.chave-xml    >= c-chave-ini       AND dt-docum-est.chave-xml       <= c-chave-fim       AND dt-docum-est.dt-emissao   >= d-data-ini       AND dt-docum-est.dt-emissao   <= d-data-fim, ~
           FIRST emitente         WHERE emitente.cod-emitente = dt-docum-est.cod-emitente NO-LOCK INDEXED-REPOSITION.     ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br-docto dt-docum-est emitente
&Scoped-define FIRST-TABLE-IN-QUERY-br-docto dt-docum-est
&Scoped-define SECOND-TABLE-IN-QUERY-br-docto emitente


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-br-docto}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-docto bt-detalhe bt-revalida 

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
ep-codigo||y|xmlloader.dt-docum-est.ep-codigo
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "ep-codigo"':U).

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
</FILTER-ATTRIBUTES> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-detalhe 
     LABEL "Detalhar" 
     SIZE 10.57 BY 1.

DEFINE BUTTON bt-revalida 
     LABEL "Revalidar SEFAZ" 
     SIZE 15 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-docto FOR 
      dt-docum-est, 
      emitente SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-docto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-docto B-table-Win _FREEFORM
  QUERY br-docto NO-LOCK DISPLAY
      dt-docum-est.serie-docto FORMAT "x(5)":U
      dt-docum-est.nro-docto FORMAT "x(16)":U
      dt-docum-est.cod-emitente FORMAT ">>>>>>>>9":U COLUMN-LABEL 'Fornec'
      emitente.nome-emit FORMAT "x(40)":U COLUMN-LABEL 'RazÆo Social'
      dt-docum-est.dt-emissao
      dt-docum-est.tot-peso FORMAT "->,>>>,>>9.99" COLUMN-LABEL "Peso"
      dt-docum-est.valor-mercad FORMAT "->,>>>,>>9.99" COLUMN-LABEL "Vl Mercad"
      dt-docum-est.dec-1 FORMAT "->,>>>,>>9.99" COLUMN-LABEL "Vl Total"

enable dt-docum-est.nro-docto
       dt-docum-est.cod-emitente     
       dt-docum-est.dt-emissao
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 112 BY 8
         FONT 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br-docto AT ROW 1 COL 1
     bt-detalhe AT ROW 9.04 COL 1 WIDGET-ID 2
     bt-revalida AT ROW 9.04 COL 11.72 WIDGET-ID 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 FONT 1 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   External Tables: xmlloader.dt-empresa
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
         HEIGHT             = 9.04
         WIDTH              = 112.86.
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB br-docto 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       br-docto:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE
       br-docto:COLUMN-RESIZABLE IN FRAME F-Main       = TRUE
       br-docto:COLUMN-MOVABLE IN FRAME F-Main         = TRUE
       br-docto:ROW-RESIZABLE IN FRAME F-Main          = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-docto
/* Query rebuild information for BROWSE br-docto
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH dt-docum-est NO-LOCK
    WHERE dt-docum-est.ep-codigo = dt-empresa.ep-codigo
      AND dt-docum-est.log-situacao = FALSE
      AND dt-docum-est.cod-estabel  >= c-estab-ini
      AND dt-docum-est.cod-estabel  <= c-estab-fim
      AND dt-docum-est.serie        >= c-serie-ini
      AND dt-docum-est.serie        <= c-serie-fim
      AND dt-docum-est.nro-docto    >= c-nr-docto-ini
      AND dt-docum-est.nro-docto    <= c-nr-docto-fim
      AND dt-docum-est.cod-emitente >= c-fornec-ini
      AND dt-docum-est.cod-emitente <= c-fornec-fim
      AND dt-docum-est.log-situacao = no
      AND dt-docum-est.chave-xml    >= c-chave-ini
      AND dt-docum-est.chave-xml       <= c-chave-fim
      AND dt-docum-est.dt-emissao   >= d-data-ini
      AND dt-docum-est.dt-emissao   <= d-data-fim,
    FIRST emitente
        WHERE emitente.cod-emitente = dt-docum-est.cod-emitente NO-LOCK INDEXED-REPOSITION.
    ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _JoinCode[1]      = "xmlloader.dt-docum-est.ep-codigo = xmlloader.dt-empresa.ep-codigo"
     _Where[1]         = "xmlloader.dt-docum-est.log-situacao = FALSE"
     _Query            is OPENED
*/  /* BROWSE br-docto */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br-docto
&Scoped-define SELF-NAME br-docto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-docto B-table-Win
ON MOUSE-SELECT-DBLCLICK OF br-docto IN FRAME F-Main
DO:
    FIND CURRENT dt-docum-est NO-LOCK NO-ERROR.
    IF AVAIL dt-docum-est THEN DO:
        ASSIGN vRowIDDTDocumEst = ROWID(dt-docum-est).
        RUN dtp/dts0911.w.
    END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-docto B-table-Win
ON ROW-ENTRY OF br-docto IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-docto B-table-Win
ON ROW-LEAVE OF br-docto IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-docto B-table-Win
ON START-SEARCH OF br-docto IN FRAME F-Main
DO:
    DEFINE VARIABLE columnHandle AS HANDLE NO-UNDO.
    
    ASSIGN columnHandle = br-docto:CURRENT-COLUMN.
    APPLY 'END-SEARCH' TO br-docto.
    MESSAGE columnHandle:NAME
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    CASE columnHandle:NAME:
        WHEN 'nro-docto' THEN
            OPEN QUERY br-docto FOR EACH dt-docum-est NO-LOCK
                WHERE dt-docum-est.ep-codigo = dt-empresa.ep-codigo
                  AND dt-docum-est.log-situacao = FALSE
                  AND dt-docum-est.cod-estabel  >= c-estab-ini
                  AND dt-docum-est.cod-estabel  <= c-estab-fim
                  AND dt-docum-est.serie        >= c-serie-ini
                  AND dt-docum-est.serie        <= c-serie-fim
                  AND dt-docum-est.nro-docto    >= c-nr-docto-ini
                  AND dt-docum-est.nro-docto    <= c-nr-docto-fim
                  AND dt-docum-est.cod-emitente >= c-fornec-ini
                  AND dt-docum-est.cod-emitente <= c-fornec-fim
                  AND dt-docum-est.log-situacao = no
                  AND dt-docum-est.chave-xml    >= c-chave-ini
                  AND dt-docum-est.chave-xml       <= c-chave-fim
                  AND dt-docum-est.dt-emissao   >= d-data-ini
                  AND dt-docum-est.dt-emissao   <= d-data-fim,
                FIRST emitente
                    WHERE emitente.cod-emitente = dt-docum-est.cod-emitente NO-LOCK by dt-docum-est.nro-docto  INDEXED-REPOSITION.        
        WHEN 'cod-emitente' THEN
            OPEN QUERY br-docto FOR EACH dt-docum-est NO-LOCK
                WHERE dt-docum-est.ep-codigo = dt-empresa.ep-codigo
                  AND dt-docum-est.log-situacao = FALSE
                  AND dt-docum-est.cod-estabel  >= c-estab-ini
                  AND dt-docum-est.cod-estabel  <= c-estab-fim
                  AND dt-docum-est.serie        >= c-serie-ini
                  AND dt-docum-est.serie        <= c-serie-fim
                  AND dt-docum-est.nro-docto    >= c-nr-docto-ini
                  AND dt-docum-est.nro-docto    <= c-nr-docto-fim
                  AND dt-docum-est.cod-emitente >= c-fornec-ini
                  AND dt-docum-est.cod-emitente <= c-fornec-fim
                  AND dt-docum-est.log-situacao = no
                  AND dt-docum-est.chave-xml    >= c-chave-ini
                  AND dt-docum-est.chave-xml       <= c-chave-fim
                  AND dt-docum-est.dt-emissao   >= d-data-ini
                  AND dt-docum-est.dt-emissao   <= d-data-fim,
                FIRST emitente
                    WHERE emitente.cod-emitente = dt-docum-est.cod-emitente NO-LOCK by dt-docum-est.cod-emitente  INDEXED-REPOSITION.        
        WHEN 'dt-emissao' THEN
            OPEN QUERY br-docto FOR EACH dt-docum-est NO-LOCK
                WHERE dt-docum-est.ep-codigo = dt-empresa.ep-codigo
                  AND dt-docum-est.log-situacao = FALSE
                  AND dt-docum-est.cod-estabel  >= c-estab-ini
                  AND dt-docum-est.cod-estabel  <= c-estab-fim
                  AND dt-docum-est.serie        >= c-serie-ini
                  AND dt-docum-est.serie        <= c-serie-fim
                  AND dt-docum-est.nro-docto    >= c-nr-docto-ini
                  AND dt-docum-est.nro-docto    <= c-nr-docto-fim
                  AND dt-docum-est.cod-emitente >= c-fornec-ini
                  AND dt-docum-est.cod-emitente <= c-fornec-fim
                  AND dt-docum-est.log-situacao = no
                  AND dt-docum-est.chave-xml    >= c-chave-ini
                  AND dt-docum-est.chave-xml       <= c-chave-fim
                  AND dt-docum-est.dt-emissao   >= d-data-ini
                  AND dt-docum-est.dt-emissao   <= d-data-fim,
                FIRST emitente
                    WHERE emitente.cod-emitente = dt-docum-est.cod-emitente NO-LOCK by dt-docum-est.dt-emissao  INDEXED-REPOSITION.        
    END CASE. 
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-docto B-table-Win
ON VALUE-CHANGED OF br-docto IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-detalhe
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-detalhe B-table-Win
ON CHOOSE OF bt-detalhe IN FRAME F-Main /* Detalhar */
DO:
  FIND CURRENT dt-docum-est NO-LOCK NO-ERROR.
  IF AVAIL dt-docum-est THEN DO:
      ASSIGN vRowIDDTDocumEst = ROWID(dt-docum-est).
      RUN dtp/dts0911.w.
  END.
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-revalida
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-revalida B-table-Win
ON CHOOSE OF bt-revalida IN FRAME F-Main /* Revalidar SEFAZ */
DO:
    define variable cComandoValida   as char no-undo.
    define variable cArqValida       as char no-undo.
    define variable cArqRetorno      as char no-undo.
    define variable cMensagemRetorno as char no-undo extent 7.
    define variable c-linha          as char no-undo.
    define variable iContador        as integer no-undo.
    run utp/ut-acomp.p persistent set h-acomp.
    run pi-inicializar in h-acomp (input "Valida‡Æo").  
    run pi-acompanhar in h-acomp ( input "Validando SEFAZ" ).
    assign cArqValida  = dt-empresa.pasta-arq-config + "\ValidaXML.jar"
           cArqRetorno = dt-empresa.pasta-arq-config + "\".
    if search(cArqValida) <> ? then do:
        assign cComandoValida = "java -jar " + cArqValida.
    end.
    find current dt-docum-est no-lock no-error.
    if avail dt-docum-est then do:
        assign cArqRetorno = cArqRetorno + dt-docum-est.chave-xml + ".log".
        if index(dt-docum-est.nome-arq, "CTE") > 0 then do:
            assign cComandoValida = cComandoValida + " Cte " + dt-docum-est.serie-docto + " " + dt-docum-est.chave-xml + " " + dt-empresa.nome-arq-config.
            
        end.
        else do:
            assign cComandoValida = cComandoValida + " Nfe " + dt-docum-est.serie-docto  + " " + dt-docum-est.chave-xml + " " + dt-empresa.nome-arq-config.
        end.
    end.
    do iContador = 1 to 7:
        assign cMensagemRetorno[iContador] = "".
    end.
    if cComandoValida <> "" then do:
        ASSIGN cComandoValida = replace(cComandoValida,"\","/"). 
        DOS silent VALUE(cComandoValida).
        if search(cArqRetorno) <> ? then do:
            INPUT FROM VALUE(cArqRetorno) CONVERT SOURCE "iso8859-1".
            blk-import:
            REPEAT:
                IMPORT UNFORMATTED c-linha.

                IF c-linha = "" then leave blk-import. 
                IF c-linha = "" then leave.

                IF num-entries(c-linha,"##") >= 1 THEN DO:
                    if entry(1,c-linha,"##") = "Erro" then do:
                        do iContador = 1 to 7:
                            assign cMensagemRetorno[iContador] = entry(iContador,c-linha,"##").
                        end.
                        leave blk-import.
                    end.
                    else do: 
                        do iContador = 1 to 7:
                            assign cMensagemRetorno[iContador] = entry(iContador,c-linha,"##").
                        end.
                        leave blk-import.                
                    end.
                    /* ASSIGN i-tot-campos = NUM-ENTRIES(c-linha,";").   */
                END.
                NEXT.

            END.
            INPUT CLOSE.
            run pi-finalizar in h-acomp.
            if cMensagemRetorno[1] = "Erro" then
                run utp/ut-msgs.p (input "show",                                                                 
                                   input 17006,                                                                  
                                   input "Erro Valida‡Æo Documento - " + dt-docum-est.nro-docto + "~~" + cMensagemRetorno[3]).
            else 
                run utp/ut-msgs.p (input "show",
                                   input 15825,
                                   input "Valida‡Æo Documento - " + dt-docum-est.nro-docto + "~~" + cMensagemRetorno[5] + "-" + cMensagemRetorno[7]).

            ASSIGN cComandoValida =  "DEL " + cArqRetorno .
            ASSIGN cComandoValida = replace(cComandoValida,"/","\").
            DOS SILENT VALUE(cComandoValida).
        end.
        else 
            run pi-finalizar in h-acomp.


    end.
  
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
  {src/adm/template/row-list.i "dt-empresa"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "dt-empresa"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-selecao B-table-Win 
PROCEDURE pi-selecao :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER TABLE FOR tt-sel.
FIND FIRST tt-sel NO-LOCK NO-ERROR.
IF AVAIL tt-sel
THEN ASSIGN c-estab-ini   = tt-sel.c-estab-ini
            c-estab-fim   = tt-sel.c-estab-fim
            d-data-ini    = tt-sel.d-data-ini
            d-data-fim    = tt-sel.d-data-fim
            c-serie-ini   = tt-sel.c-serie-ini
            c-serie-fim   = tt-sel.c-serie-fim
            c-nr-docto-ini= tt-sel.c-nr-docto-ini
            c-nr-docto-fim= tt-sel.c-nr-docto-fim
            c-fornec-ini  = tt-sel.c-fornec-ini
            c-fornec-fim  = tt-sel.c-fornec-fim
            c-chave-ini   = tt-sel.c-chave-ini
            c-chave-fim   = tt-sel.c-chave-fim.

{&OPEN-QUERY-br-docto}
    ASSIGN dt-docum-est.nro-docto       :READ-ONLY IN BROWSE {&BROWSE-NAME} = TRUE
           dt-docum-est.cod-emitente    :READ-ONLY IN BROWSE {&BROWSE-NAME} = true
           dt-docum-est.dt-emissao      :READ-ONLY IN BROWSE {&BROWSE-NAME} = TRUE.
APPLY "value-changed" TO br-docto IN FRAME {&FRAME-NAME}.
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
  {src/adm/template/sndkycas.i "ep-codigo" "dt-docum-est" "ep-codigo"}

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
  {src/adm/template/snd-list.i "dt-empresa"}
  {src/adm/template/snd-list.i "dt-docum-est"}
  {src/adm/template/snd-list.i "emitente"}

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
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

