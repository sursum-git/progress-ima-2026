&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r11 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
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
{utp/ut-glob.i}
{include/i-prgvrs.i B02DTS0918 2.06.00.001}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
define buffer bDtDocumEstLoader for dt-docum-est.
define variable h-acomp as handle no-undo.
define variable i-cont  as integer no-undo.
DEFINE VARIABLE pRazaoSocial AS CHARACTER   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-arquivo

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES dt-docum-est

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br-arquivo                                    */
&Scoped-define FIELDS-IN-QUERY-br-arquivo dt-docum-est.log-situacao ~
dt-docum-est.cod-estabel dt-docum-est.serie-docto dt-docum-est.nro-docto ~
dt-docum-est.cod-emitente ~
pi-razao-emit(dt-docum-est.cod-emitente) @ pRazaoSocial ~
dt-docum-est.dt-emissao dt-docum-est.tot-peso dt-docum-est.valor-mercad ~
dt-docum-est.dec-1 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-arquivo 
&Scoped-define QUERY-STRING-br-arquivo FOR EACH dt-docum-est WHERE ~{&KEY-PHRASE} ~
      AND dt-docum-est.log-situacao = FALSE NO-LOCK ~
    BY dt-docum-est.cod-estabel ~
       BY dt-docum-est.dt-emissao ~
        BY dt-docum-est.nro-docto
&Scoped-define OPEN-QUERY-br-arquivo OPEN QUERY br-arquivo FOR EACH dt-docum-est WHERE ~{&KEY-PHRASE} ~
      AND dt-docum-est.log-situacao = FALSE NO-LOCK ~
    BY dt-docum-est.cod-estabel ~
       BY dt-docum-est.dt-emissao ~
        BY dt-docum-est.nro-docto.
&Scoped-define TABLES-IN-QUERY-br-arquivo dt-docum-est
&Scoped-define FIRST-TABLE-IN-QUERY-br-arquivo dt-docum-est


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS p-estab-ini p-estab-fim fi-forn-ini ~
fi-forn-fim fi-num-docto-ini fi-num-docto-fim dt-emis-ini dt-emis-fim ~
tg-importado bt-pesquisa-xml br-arquivo bt-importado bt-revalida IMAGE-1 ~
IMAGE-2 IMAGE-5 IMAGE-6 IMAGE-7 IMAGE-8 IMAGE-9 IMAGE-10 
&Scoped-Define DISPLAYED-OBJECTS p-estab-ini p-estab-fim fi-forn-ini ~
fi-forn-fim fi-num-docto-ini fi-num-docto-fim dt-emis-ini dt-emis-fim ~
tg-importado 

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

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD pi-razao-emit B-table-Win 
FUNCTION pi-razao-emit RETURNS CHARACTER
  ( input pCodEmitente as integer /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-importado 
     LABEL "Marca Importado" 
     SIZE 15 BY 1 TOOLTIP "Marca Arquivo como Importado".

DEFINE BUTTON bt-pesquisa-xml 
     LABEL "Pesquisa" 
     SIZE 8 BY 1.

DEFINE BUTTON bt-revalida 
     LABEL "Revalidar SEFAZ" 
     SIZE 15 BY 1.

DEFINE VARIABLE dt-emis-fim AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE dt-emis-ini AS DATE FORMAT "99/99/9999":U INITIAL 01/01/1900 
     LABEL "EmissÆo" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE fi-forn-fim AS INTEGER FORMAT ">>>>>>>>9":U INITIAL 999999999 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE fi-forn-ini AS INTEGER FORMAT ">>>>>>>>9":U INITIAL 0 
     LABEL "Fornecedor" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE fi-num-docto-fim AS CHARACTER FORMAT "X(16)":U INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88 NO-UNDO.

DEFINE VARIABLE fi-num-docto-ini AS CHARACTER FORMAT "X(16)":U 
     LABEL "Documento" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88 NO-UNDO.

DEFINE VARIABLE p-estab-fim AS CHARACTER FORMAT "X(3)":U INITIAL "ZZZ" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE p-estab-ini AS CHARACTER FORMAT "X(3)":U 
     LABEL "Estabel" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88
     FONT 8 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-10
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-5
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-6
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-7
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-8
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-9
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE VARIABLE tg-importado AS LOGICAL INITIAL no 
     LABEL "Mostra Documentos J  Importados" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .83 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-arquivo FOR 
      dt-docum-est SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-arquivo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-arquivo B-table-Win _STRUCTURED
  QUERY br-arquivo NO-LOCK DISPLAY
      dt-docum-est.log-situacao COLUMN-LABEL "Atual" FORMAT "Sim/NÆo":U
      dt-docum-est.cod-estabel FORMAT "x(3)":U
      dt-docum-est.serie-docto FORMAT "x(5)":U WIDTH 4.43
      dt-docum-est.nro-docto FORMAT "x(16)":U WIDTH 12.86
      dt-docum-est.cod-emitente COLUMN-LABEL "Fornec" FORMAT ">>>>>>>>9":U
      pi-razao-emit(dt-docum-est.cod-emitente) @ pRazaoSocial COLUMN-LABEL "Razao Social" FORMAT "X(80)":U
            WIDTH 34.29
      dt-docum-est.dt-emissao FORMAT "99/99/9999":U WIDTH 10.57
      dt-docum-est.tot-peso COLUMN-LABEL "Peso" FORMAT ">>>>,>>>,>>9.9999":U
            WIDTH 12.14
      dt-docum-est.valor-mercad COLUMN-LABEL "Vl Mercad" FORMAT ">>>>,>>>,>>9.99":U
      dt-docum-est.dec-1 COLUMN-LABEL "Vl Total" FORMAT "->>,>>9.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS MULTIPLE SIZE 112 BY 10.38
         FONT 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     p-estab-ini AT ROW 1.04 COL 22.57 COLON-ALIGNED WIDGET-ID 12
     p-estab-fim AT ROW 1.04 COL 42.43 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     fi-forn-ini AT ROW 2.04 COL 22.43 COLON-ALIGNED WIDGET-ID 28
     fi-forn-fim AT ROW 2.04 COL 42.43 COLON-ALIGNED NO-LABEL WIDGET-ID 30
     fi-num-docto-ini AT ROW 3.04 COL 15.43 COLON-ALIGNED WIDGET-ID 36
     fi-num-docto-fim AT ROW 3.04 COL 42.43 COLON-ALIGNED NO-LABEL WIDGET-ID 38
     dt-emis-ini AT ROW 4.04 COL 22.43 COLON-ALIGNED WIDGET-ID 44
     dt-emis-fim AT ROW 4.04 COL 42.43 COLON-ALIGNED NO-LABEL WIDGET-ID 46
     tg-importado AT ROW 1.25 COL 83 WIDGET-ID 52
     bt-pesquisa-xml AT ROW 4 COL 105.14 WIDGET-ID 22
     br-arquivo AT ROW 5.13 COL 1
     bt-importado AT ROW 15.54 COL 1.43 WIDGET-ID 24
     bt-revalida AT ROW 15.54 COL 16.29 WIDGET-ID 26
     IMAGE-1 AT ROW 1.04 COL 35.72 WIDGET-ID 8
     IMAGE-2 AT ROW 1.04 COL 41.43 WIDGET-ID 10
     IMAGE-5 AT ROW 2.04 COL 35.72 WIDGET-ID 32
     IMAGE-6 AT ROW 2.04 COL 41.43 WIDGET-ID 34
     IMAGE-7 AT ROW 3.04 COL 35.72 WIDGET-ID 40
     IMAGE-8 AT ROW 3.04 COL 41.43 WIDGET-ID 42
     IMAGE-9 AT ROW 4.04 COL 35.72 WIDGET-ID 48
     IMAGE-10 AT ROW 4.04 COL 41.43 WIDGET-ID 50
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
         HEIGHT             = 15.63
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit Custom                            */
/* BROWSE-TAB br-arquivo bt-pesquisa-xml F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-arquivo
/* Query rebuild information for BROWSE br-arquivo
     _TblList          = "xmlloader.dt-docum-est"
     _Options          = "NO-LOCK KEY-PHRASE"
     _TblOptList       = ","
     _OrdList          = "xmlloader.dt-docum-est.cod-estabel|yes,xmlloader.dt-docum-est.dt-emissao|yes,xmlloader.dt-docum-est.nro-docto|yes"
     _Where[1]         = "xmlloader.dt-docum-est.log-situacao = FALSE"
     _FldNameList[1]   > xmlloader.dt-docum-est.log-situacao
"dt-docum-est.log-situacao" "Atual" "Sim/NÆo" "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   = xmlloader.dt-docum-est.cod-estabel
     _FldNameList[3]   > xmlloader.dt-docum-est.serie-docto
"dt-docum-est.serie-docto" ? ? "character" ? ? ? ? ? ? no ? no no "4.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > xmlloader.dt-docum-est.nro-docto
"dt-docum-est.nro-docto" ? ? "character" ? ? ? ? ? ? no ? no no "12.86" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > xmlloader.dt-docum-est.cod-emitente
"dt-docum-est.cod-emitente" "Fornec" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"pi-razao-emit(dt-docum-est.cod-emitente) @ pRazaoSocial" "Razao Social" "X(80)" ? ? ? ? ? ? ? no ? no no "34.29" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > xmlloader.dt-docum-est.dt-emissao
"dt-docum-est.dt-emissao" ? ? "date" ? ? ? ? ? ? no ? no no "10.57" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > xmlloader.dt-docum-est.tot-peso
"dt-docum-est.tot-peso" "Peso" ? "decimal" ? ? ? ? ? ? no ? no no "12.14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > xmlloader.dt-docum-est.valor-mercad
"dt-docum-est.valor-mercad" "Vl Mercad" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > xmlloader.dt-docum-est.dec-1
"dt-docum-est.dec-1" "Vl Total" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE br-arquivo */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br-arquivo
&Scoped-define SELF-NAME br-arquivo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-arquivo B-table-Win
ON ROW-DISPLAY OF br-arquivo IN FRAME F-Main
DO:
  if dt-docum-est.log-situacao = yes  then do:
      assign dt-docum-est.cod-emitente:fgcolor in browse br-arquivo = 9 
             dt-docum-est.cod-estabel :fgcolor in browse br-arquivo = 9
             dt-docum-est.dec-1       :fgcolor in browse br-arquivo = 9
             dt-docum-est.dt-emissao  :fgcolor in browse br-arquivo = 9
             dt-docum-est.log-situacao:fgcolor in browse br-arquivo = 9 
             dt-docum-est.nro-docto   :fgcolor in browse br-arquivo = 9
             dt-docum-est.serie-docto :fgcolor in browse br-arquivo = 9
             dt-docum-est.tot-peso    :fgcolor in browse br-arquivo = 9
             dt-docum-est.valor-mercad:fgcolor in browse br-arquivo = 9
             pRazaoSocial             :fgcolor in browse br-arquivo = 9.
  end.
  else
      assign dt-docum-est.cod-emitente:fgcolor in browse br-arquivo = 0 
             dt-docum-est.cod-estabel :fgcolor in browse br-arquivo = 0
             dt-docum-est.dec-1       :fgcolor in browse br-arquivo = 0
             dt-docum-est.dt-emissao  :fgcolor in browse br-arquivo = 0
             dt-docum-est.log-situacao:fgcolor in browse br-arquivo = 0 
             dt-docum-est.nro-docto   :fgcolor in browse br-arquivo = 0
             dt-docum-est.serie-docto :fgcolor in browse br-arquivo = 0
             dt-docum-est.tot-peso    :fgcolor in browse br-arquivo = 0
             dt-docum-est.valor-mercad:fgcolor in browse br-arquivo = 0
             pRazaoSocial             :fgcolor in browse br-arquivo = 0.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-arquivo B-table-Win
ON ROW-ENTRY OF br-arquivo IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-arquivo B-table-Win
ON ROW-LEAVE OF br-arquivo IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-arquivo B-table-Win
ON VALUE-CHANGED OF br-arquivo IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
    
   if dt-docum-est.log-situacao then
       assign bt-importado:sensitive in frame {&frame-name} = no.
   else
       assign bt-importado:sensitive in frame {&frame-name} = yes.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-importado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-importado B-table-Win
ON CHOOSE OF bt-importado IN FRAME F-Main /* Marca Importado */
DO:
    
    do i-cont = 1 to br-arquivo:num-selected-rows in frame {&frame-name} :
        br-arquivo:fetch-selected-row ( i-cont ) in frame {&frame-name} .
        find first bDtDocumEstLoader
            where rowid(bDtDocumEstLoader) = rowid(dt-docum-est)  
              and bDtDocumEstLoader.log-situacao = no exclusive-lock no-error.
        if avail bDtDocumEstLoader then do:
            assign bDtDocumEstLoader.log-situacao = yes. 
        end.
    end.  
    apply "choose" to bt-pesquisa-xml in frame {&frame-name}.
    
/*   find current dt-docum-est no-lock no-error.                                           */
/*   if avail dt-docum-est then do:                                                        */
/*       find bDtDocumEstLoader                                                            */
/*           where rowid(bDtDocumEstLoader) = rowid(dt-docum-est) exclusive-lock no-error. */
/*       if avail bDtDocumEstLoader then do:                                               */
/*           assign bDtDocumEstLoader.log-situacao = yes.                                  */
/*           apply "choose" to bt-pesquisa-xml in frame {&frame-name}.                     */
/*       end.                                                                              */
/*                                                                                         */
/*                                                                                         */
/*   end.                                                                                  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-pesquisa-xml
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-pesquisa-xml B-table-Win
ON CHOOSE OF bt-pesquisa-xml IN FRAME F-Main /* Pesquisa */
DO:
  RUN piOpenQuery /* IN h_b01dts0918 */
    ( INPUT input frame {&frame-name} p-estab-ini /* CHARACTER */,
      INPUT input frame {&frame-name} p-estab-fim /* CHARACTER */,
      INPUT input frame {&frame-name} fi-forn-ini,   
      INPUT input frame {&frame-name} fi-forn-fim,   
      INPUT input frame {&frame-name} fi-num-docto-ini,  
      INPUT input frame {&frame-name} fi-num-docto-fim,  
      INPUT input frame {&frame-name} dt-emis-ini, 
      INPUT input frame {&frame-name} dt-emis-fim, 
      tg-importado:checked in frame {&frame-name}).

  apply "value-changed" to br-arquivo.


  
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
    find current dt-docum-est no-lock no-error.
    if avail dt-docum-est then do:
        find first dt-empresa
            where dt-empresa.ep-codigo = dt-docum-est.ep-codigo no-lock no-error.
        assign cArqValida  = dt-empresa.pasta-arq-config + "\ValidaXML.jar"
               cArqRetorno = dt-empresa.pasta-arq-config + "\".
        if search(cArqValida) <> ? then do:
            assign cComandoValida = "java -jar " + cArqValida.
        end.
        assign cArqRetorno = cArqRetorno + dt-docum-est.chave-xml + ".log".
        if index(dt-docum-est.nome-arq, "CTE") > 0 then do:
            assign cComandoValida = cComandoValida + " Cte " + dt-docum-est.serie-docto + " " + dt-docum-est.chave-xml + " " + dt-empresa.nome-arq-config.
            
        end.
        else do:
            assign cComandoValida = cComandoValida + " Nfe " + dt-docum-est.serie-docto  + " " + dt-docum-est.chave-xml + " " + dt-empresa.nome-arq-config.
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
                                if num-entries(c-linha,"##") >= iContador then
                                    assign cMensagemRetorno[iContador] = entry(iContador,c-linha,"##").
                            end.
                            leave blk-import.
                        end.
                        else do: 
                            do iContador = 1 to 7:
                                if num-entries(c-linha,"##") >= iContador then
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
    apply "choose" to bt-pesquisa-xml in frame {&frame-name}.
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
define input parameter pEstabIni  as char    format "X(3)"       no-undo.
define input parameter pEstabFim  as char    format "X(3)"       no-undo.
define input parameter pFornIni   as integer format ">>>>>>>>9"  no-undo.
define input parameter pFornFim   as integer format ">>>>>>>>9"  no-undo.
define input parameter pDocumIni  as char    format "X(16)"      no-undo.
define input parameter pDocumFim  as char    format "X(16)"      no-undo.
define input parameter pDtEmisIni as date                        no-undo.
define input parameter pDtEmisFim as date                        no-undo.
define input parameter pTodos     as logical                     no-undo.

open query br-arquivo for each dt-docum-est 
                         where (if pTodos = no then dt-docum-est.log-situacao = FALSE else true)
                           and dt-docum-est.cod-estabel  >= pEstabIni
                           and dt-docum-est.cod-estabel  <= pEstabFim 
                           and dt-docum-est.cod-emitente >= pFornIni   
                           and dt-docum-est.cod-emitente <= pFornFim   
                           and dt-docum-est.nro-docto    >= pDocumIni  
                           and dt-docum-est.nro-docto    <= pDocumFim  
                           and dt-docum-est.dt-emissao   >= pDtEmisIni 
                           and dt-docum-est.dt-emissao   <= pDtEmisFim 
                            no-lock
                          
                        BY dt-docum-est.cod-estabel
                           BY dt-docum-est.dt-emissao
                            BY dt-docum-est.nro-docto.
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
  {src/adm/template/snd-list.i "dt-docum-est"}

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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION pi-razao-emit B-table-Win 
FUNCTION pi-razao-emit RETURNS CHARACTER
  ( input pCodEmitente as integer /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND first emitente no-lock where emitente.cod-emitente = pCodEmitente no-error.
  if avail emitente then
      RETURN emitente.nome-emit.
  else 
      RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

