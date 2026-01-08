&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r11 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
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
**  ACRA 002 - 05/08/2013 - HARRY PFAFF JUNIOR
**           - INCLUS«O DE FILTRO DE DATA PARA PESQUISA DO LOG
**           - INCLUS«O DE CAMPO DE DATA NO BROSWE DO LOG
*******************************************************************************/
{utp/ut-glob.i}
{include/i-prgvrs.i B03DTS0918 2.06.00.002}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE wh-pesquisa     AS HANDLE NO-UNDO.
define variable h-acomp         as handle no-undo.

define temp-table tt-log-xmlloader no-undo
    field p-dados       as char
    field p-num-linha   as integer
    FIELD p-data        AS DATE
    index idx-linha p-num-linha.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-log-xmlloader

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-log-xmlloader

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br-log-xmlloader                              */
&Scoped-define FIELDS-IN-QUERY-br-log-xmlloader tt-log-xmlloader.p-num-linha tt-log-xmlloader.p-data tt-log-xmlloader.p-dados   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-log-xmlloader   
&Scoped-define SELF-NAME br-log-xmlloader
&Scoped-define QUERY-STRING-br-log-xmlloader For each tt-log-xmlloader no-lock
&Scoped-define OPEN-QUERY-br-log-xmlloader OPEN QUERY {&SELF-NAME} For each tt-log-xmlloader no-lock.
&Scoped-define TABLES-IN-QUERY-br-log-xmlloader tt-log-xmlloader
&Scoped-define FIRST-TABLE-IN-QUERY-br-log-xmlloader tt-log-xmlloader


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS p-estab-ini fi-num-linhas dt-pesq-ini ~
dt-pesq-fim bt-pesquisa-log br-log-xmlloader IMAGE-1 IMAGE-2 
&Scoped-Define DISPLAYED-OBJECTS p-estab-ini fi-num-linhas dt-pesq-ini ~
dt-pesq-fim 

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
</FILTER-ATTRIBUTES> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-pesquisa-log 
     LABEL "Pesquisa" 
     SIZE 8 BY 1.

DEFINE VARIABLE dt-pesq-fim AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE dt-pesq-ini AS DATE FORMAT "99/99/9999":U 
     LABEL "Data" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE fi-num-linhas AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "N£mero de Linhas" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 TOOLTIP "N£mero de Linhas a importar" NO-UNDO.

DEFINE VARIABLE p-estab-ini AS CHARACTER FORMAT "X(3)":U 
     LABEL "Estabel" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88
     FONT 8 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-log-xmlloader FOR 
      tt-log-xmlloader SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-log-xmlloader
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-log-xmlloader B-table-Win _FREEFORM
  QUERY br-log-xmlloader NO-LOCK DISPLAY
      tt-log-xmlloader.p-num-linha  format ">>>>>9" column-label "Linha"
      tt-log-xmlloader.p-data       format "99/99/9999" column-label "Data" 
      tt-log-xmlloader.p-dados      format "X(256)" column-label "LOG"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 111 BY 13
         FONT 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     p-estab-ini AT ROW 1.13 COL 17.72 COLON-ALIGNED WIDGET-ID 12
     fi-num-linhas AT ROW 2.08 COL 17.72 COLON-ALIGNED WIDGET-ID 2
     dt-pesq-ini AT ROW 1.25 COL 47 COLON-ALIGNED WIDGET-ID 24
     dt-pesq-fim AT ROW 1.25 COL 66.14 COLON-ALIGNED NO-LABEL WIDGET-ID 26
     bt-pesquisa-log AT ROW 2.13 COL 104 WIDGET-ID 22
     br-log-xmlloader AT ROW 3.25 COL 1
     IMAGE-1 AT ROW 1.25 COL 60.29 WIDGET-ID 8
     IMAGE-2 AT ROW 1.25 COL 64.86 WIDGET-ID 10
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
         HEIGHT             = 15.54
         WIDTH              = 111.43.
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
/* BROWSE-TAB br-log-xmlloader bt-pesquisa-log F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-log-xmlloader
/* Query rebuild information for BROWSE br-log-xmlloader
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} For each tt-log-xmlloader no-lock.
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Query            is NOT OPENED
*/  /* BROWSE br-log-xmlloader */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br-log-xmlloader
&Scoped-define SELF-NAME br-log-xmlloader
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-log-xmlloader B-table-Win
ON ROW-ENTRY OF br-log-xmlloader IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-log-xmlloader B-table-Win
ON ROW-LEAVE OF br-log-xmlloader IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-log-xmlloader B-table-Win
ON VALUE-CHANGED OF br-log-xmlloader IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-pesquisa-log
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-pesquisa-log B-table-Win
ON CHOOSE OF bt-pesquisa-log IN FRAME F-Main /* Pesquisa */
DO:
    if input frame {&frame-name} p-estab-ini = "" then do:
        RUN utp/ut-msgs.p (INPUT 'SHOW':U,
                           INPUT 17006,
                           INPUT 'Estabelcimento' + "~~" + 'ê necess†rio informar o estabelecimento').
        apply "entry" to p-estab-ini in frame {&frame-name}.
        RETURN 'NOK':U.

    end.
    if input frame {&frame-name} fi-num-linhas <= 0 then do:
        RUN utp/ut-msgs.p (INPUT 'SHOW':U,
                           INPUT 17006,
                           INPUT 'N£mero de Linhas' + "~~" + 'N£mero de Linhas para display deve ser maior que ZERO').
        apply "entry" to fi-num-linhas in frame {&frame-name}.
        RETURN 'NOK':U.

    end.

    RUN piOpenQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME p-estab-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL p-estab-ini B-table-Win
ON F5 OF p-estab-ini IN FRAME F-Main /* Estabel */
DO:
{include/zoomvar.i &prog-zoom="dtzoom/z01dt0912a.w"
                   &campo=p-estab-ini
                   &campozoom=cod-estabel}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL p-estab-ini B-table-Win
ON MOUSE-SELECT-DBLCLICK OF p-estab-ini IN FRAME F-Main /* Estabel */
DO:
  apply "F5" to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF
p-estab-ini:LOAD-MOUSE-POINTER("image/lupa.cur").

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piInitialize B-table-Win 
PROCEDURE piInitialize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
ASSIGN dt-pesq-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY,"99/99/9999")
       dt-pesq-fim:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY,"99/99/9999").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piOpenQuery B-table-Win 
PROCEDURE piOpenQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define variable pArqLOGXMLLoader as char no-undo.
define variable c-linha          as char format "X(1000)" no-undo.
define variable iContador        as integer no-undo.
define variable deTotLinhas      as integer no-undo.
define variable iLinhaInicial    as integer no-undo.
define variable pDia             as integer no-undo.
define variable pMes             as integer no-undo.
define variable pAno             as integer no-undo.
empty temp-table tt-log-xmlloader.
assign pArqLOGXMLLoader = "".
find first dt-empresa
    where dt-empresa.cod-estabel = input frame {&frame-name} p-estab-ini use-index ch-estab no-lock no-error.
if avail dt-empresa then do:
    assign pArqLOGXMLLoader = dt-empresa.pasta-arq-config + "\xmlloader.log".
end.
if pArqLOGXMLLoader <> "" and search(pArqLOGXMLLoader) <> ? then do:
    run utp/ut-acomp.p persistent set h-acomp.
    run pi-inicializar in h-acomp (input "Importaá∆o").  
    assign iContador = 1.
    INPUT FROM VALUE(pArqLOGXMLLoader) CONVERT SOURCE "iso8859-1".
    blk-import:
    REPEAT:

        run pi-acompanhar in h-acomp ( input "Importando Log " + string(iContador)  ).
        IMPORT UNFORMATTED c-linha.

        IF c-linha = "" then next blk-import. 
        if index(c-linha,"*************** Iniciando Processamento ***************") > 0 then next blk-import. 
        if index(c-linha,"*************** Fim Processamento ***************") > 0 then next blk-import. 
        if substring(c-linha,21,length(c-linha)) = "" then next blk-import. 
        if substring(c-linha,3,1) <> "-" then next blk-import. 
        /*IF c-linha = "" then leave. */
        create tt-log-xmlloader.
        assign tt-log-xmlloader.p-dados     = substring(c-linha,12,length(c-linha))
               tt-log-xmlloader.p-num-linha = iContador.
        assign pDia = int(substring(c-linha,1,2))
               pMes = int(substring(c-linha,4,2))
               pAno = int(substring(c-linha,7,4)).


        assign tt-log-xmlloader.p-data      = date(pMes,pDia,pAno).
        assign iContador = iContador + 1.
        NEXT.
    END.
    INPUT CLOSE.

end.
run pi-finalizar in h-acomp.
find last tt-log-xmlloader use-index idx-linha no-lock no-error.
if avail tt-log-xmlloader then
    assign deTotLinhas = tt-log-xmlloader.p-num-linha.

assign iLinhaInicial = deTotLinhas - input frame {&frame-name} fi-num-linhas.

if iLinhaInicial <= 0 then
    assign iLinhaInicial = 1.

open query br-log-xmlloader for each tt-log-xmlloader no-lock
                         where tt-log-xmlloader.p-num-linha >= iLinhaInicial  
                           and tt-log-xmlloader.p-data      >= input frame {&frame-name} dt-pesq-ini
                           and tt-log-xmlloader.p-data      <= input frame {&frame-name} dt-pesq-fim
                           BY tt-log-xmlloader.p-num-linha.


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
  {src/adm/template/snd-list.i "tt-log-xmlloader"}

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

