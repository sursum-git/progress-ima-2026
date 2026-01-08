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
**  ACRA 002 - 05/09/2013 
**           - COLOCADA FUN€ÇO DE PESQUISA COM "*" NO NOME DO ARQUIVO
*******************************************************************************/
{include/i-buffer.i}
{utp/ut-glob.i}
{include/i-prgvrs.i B01DTS0918 2.06.00.002}
/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

define variable fiSitArquivo as character format "X(4)" no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-log

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES dt-log-importa-xml

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br-log                                        */
&Scoped-define FIELDS-IN-QUERY-br-log ~
fc-sit-arquivo(dt-log-importa-xml.sit-arquivo) @ fiSitArquivo ~
dt-log-importa-xml.nome-arq dt-log-importa-xml.desc-erro ~
dt-log-importa-xml.dt-processo dt-log-importa-xml.hs-processo ~
dt-log-importa-xml.usuar-processo 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-log 
&Scoped-define QUERY-STRING-br-log FOR EACH dt-log-importa-xml WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br-log OPEN QUERY br-log FOR EACH dt-log-importa-xml WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br-log dt-log-importa-xml
&Scoped-define FIRST-TABLE-IN-QUERY-br-log dt-log-importa-xml


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS IMAGE-1 IMAGE-2 IMAGE-3 RECT-17 IMAGE-4 ~
dt-pesq-ini dt-pesq-fim rs-sit-import tp-pesq-nome fi-arquivo-ini ~
fi-arquivo-fim bt-pesquisa br-log btDetalhe 
&Scoped-Define DISPLAYED-OBJECTS dt-pesq-ini dt-pesq-fim rs-sit-import ~
tp-pesq-nome fi-arquivo-ini fi-arquivo-fim 

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

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fc-sit-arquivo B-table-Win 
FUNCTION fc-sit-arquivo RETURNS CHARACTER
  ( pSitArquivo as integer /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-pesquisa 
     LABEL "Pesquisa" 
     SIZE 8 BY 1.

DEFINE BUTTON btDetalhe 
     LABEL "Detalhe" 
     SIZE 15 BY 1.

DEFINE VARIABLE tp-pesq-nome AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 4 
     LABEL "Tipo Pesquisa Nome" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Qualquer Parte",1,
                     "Come‡a Com",2,
                     "Termina Com",3,
                     "Exata",4
     DROP-DOWN-LIST
     SIZE 17.43 BY 1 TOOLTIP "Informe como ser  feita a pesquisa pelo nome do arquivo" NO-UNDO.

DEFINE VARIABLE dt-pesq-fim AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE dt-pesq-ini AS DATE FORMAT "99/99/9999":U 
     LABEL "Data" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88
     FONT 8 NO-UNDO.

DEFINE VARIABLE fi-arquivo-fim AS CHARACTER FORMAT "X(256)":U INITIAL "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 26.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi-arquivo-ini AS CHARACTER FORMAT "X(256)":U 
     LABEL "Arquivo" 
     VIEW-AS FILL-IN 
     SIZE 26.72 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-3
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-4
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE VARIABLE rs-sit-import AS INTEGER INITIAL 3 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Importado com Sucesso", 1,
"Erro na Importa‡Æo", 2,
"Todos", 3
     SIZE 19 BY 2.25 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 22 BY 3.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-log FOR 
      dt-log-importa-xml SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-log
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-log B-table-Win _STRUCTURED
  QUERY br-log NO-LOCK DISPLAY
      fc-sit-arquivo(dt-log-importa-xml.sit-arquivo) @ fiSitArquivo COLUMN-LABEL "Situa‡Æo" FORMAT "X(6)":U
      dt-log-importa-xml.nome-arq FORMAT "X(80)":U WIDTH 34
      dt-log-importa-xml.desc-erro FORMAT "x(1000)":U WIDTH 35.72
      dt-log-importa-xml.dt-processo COLUMN-LABEL "Data Processo" FORMAT "99/99/9999":U
      dt-log-importa-xml.hs-processo FORMAT "x(8)":U
      dt-log-importa-xml.usuar-processo FORMAT "x(8)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 108 BY 11
         FONT 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     dt-pesq-ini AT ROW 1.29 COL 22.57 COLON-ALIGNED WIDGET-ID 12
     dt-pesq-fim AT ROW 1.29 COL 42.43 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     rs-sit-import AT ROW 1.88 COL 76.29 NO-LABEL WIDGET-ID 14
     tp-pesq-nome AT ROW 2.33 COL 16 COLON-ALIGNED WIDGET-ID 36
     fi-arquivo-ini AT ROW 3.38 COL 7 COLON-ALIGNED WIDGET-ID 26
     fi-arquivo-fim AT ROW 3.38 COL 42.43 COLON-ALIGNED NO-LABEL WIDGET-ID 32
     bt-pesquisa AT ROW 3.42 COL 101.14 WIDGET-ID 22
     br-log AT ROW 4.75 COL 1
     btDetalhe AT ROW 15.88 COL 1 WIDGET-ID 24
     "Situa‡Æo" VIEW-AS TEXT
          SIZE 6 BY .54 AT ROW 1.17 COL 75.57 WIDGET-ID 20
     IMAGE-1 AT ROW 1.29 COL 35.72 WIDGET-ID 8
     IMAGE-2 AT ROW 1.29 COL 41.43 WIDGET-ID 10
     IMAGE-3 AT ROW 3.38 COL 35.72 WIDGET-ID 28
     RECT-17 AT ROW 1.42 COL 74.57 WIDGET-ID 18
     IMAGE-4 AT ROW 3.38 COL 41.43 WIDGET-ID 30
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
         HEIGHT             = 15.96
         WIDTH              = 108.29.
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
/* BROWSE-TAB br-log bt-pesquisa F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-log
/* Query rebuild information for BROWSE br-log
     _TblList          = "xmlloader.dt-log-importa-xml"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = ", FIRST"
     _FldNameList[1]   > "_<CALC>"
"fc-sit-arquivo(dt-log-importa-xml.sit-arquivo) @ fiSitArquivo" "Situa‡Æo" "X(6)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > xmlloader.dt-log-importa-xml.nome-arq
"dt-log-importa-xml.nome-arq" ? ? "character" ? ? ? ? ? ? no ? no no "34" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > xmlloader.dt-log-importa-xml.desc-erro
"dt-log-importa-xml.desc-erro" ? ? "character" ? ? ? ? ? ? no ? no no "35.72" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > xmlloader.dt-log-importa-xml.dt-processo
"dt-log-importa-xml.dt-processo" "Data Processo" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   = xmlloader.dt-log-importa-xml.hs-processo
     _FldNameList[6]   = xmlloader.dt-log-importa-xml.usuar-processo
     _Query            is NOT OPENED
*/  /* BROWSE br-log */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br-log
&Scoped-define SELF-NAME br-log
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-log B-table-Win
ON ROW-DISPLAY OF br-log IN FRAME F-Main
DO:
    if dt-log-importa-xml.sit-arquivo = 1 then do: /* IMPORTADO COM SUCESSO */
        assign fiSitArquivo                     :FGCOLOR IN BROWSE br-log = 9  
               dt-log-importa-xml.desc-erro     :FGCOLOR IN BROWSE br-log = 9
               dt-log-importa-xml.dt-processo   :FGCOLOR IN BROWSE br-log = 9
               dt-log-importa-xml.hs-processo   :FGCOLOR IN BROWSE br-log = 9
               dt-log-importa-xml.nome-arq      :FGCOLOR IN BROWSE br-log = 9
               dt-log-importa-xml.usuar-processo:FGCOLOR IN BROWSE br-log = 9
               fiSitArquivo                     :FONT    IN BROWSE br-log = 6
               dt-log-importa-xml.desc-erro     :FONT    IN BROWSE br-log = 6
               dt-log-importa-xml.dt-processo   :FONT    IN BROWSE br-log = 6
               dt-log-importa-xml.hs-processo   :FONT    IN BROWSE br-log = 6
               dt-log-importa-xml.nome-arq      :FONT    IN BROWSE br-log = 6
               dt-log-importa-xml.usuar-processo:FONT    IN BROWSE br-log = 6.
    end.
    else do:
        assign fiSitArquivo                     :FGCOLOR IN BROWSE br-log = 12  
               dt-log-importa-xml.desc-erro     :FGCOLOR IN BROWSE br-log = 12
               dt-log-importa-xml.dt-processo   :FGCOLOR IN BROWSE br-log = 12
               dt-log-importa-xml.hs-processo   :FGCOLOR IN BROWSE br-log = 12
               dt-log-importa-xml.nome-arq      :FGCOLOR IN BROWSE br-log = 12
               dt-log-importa-xml.usuar-processo:FGCOLOR IN BROWSE br-log = 12
               fiSitArquivo                     :FONT    IN BROWSE br-log = 6
               dt-log-importa-xml.desc-erro     :FONT    IN BROWSE br-log = 6
               dt-log-importa-xml.dt-processo   :FONT    IN BROWSE br-log = 6
               dt-log-importa-xml.hs-processo   :FONT    IN BROWSE br-log = 6
               dt-log-importa-xml.nome-arq      :FONT    IN BROWSE br-log = 6
               dt-log-importa-xml.usuar-processo:FONT    IN BROWSE br-log = 6.

    end.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-log B-table-Win
ON ROW-ENTRY OF br-log IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-log B-table-Win
ON ROW-LEAVE OF br-log IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-log B-table-Win
ON VALUE-CHANGED OF br-log IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-pesquisa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-pesquisa B-table-Win
ON CHOOSE OF bt-pesquisa IN FRAME F-Main /* Pesquisa */
DO:
  RUN piOpenQuery /* IN h_b01dts0918 */
    ( INPUT input frame {&frame-name} rs-sit-import /* INTEGER */,
      INPUT input frame {&frame-name} dt-pesq-ini /* DATE */,
      INPUT input frame {&frame-name} dt-pesq-fim /* DATE */,
      INPUT input frame {&frame-name} fi-arquivo-ini /* CHARACTER */,
      INPUT input frame {&frame-name} fi-arquivo-fim /* CHARACTER */).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btDetalhe
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btDetalhe B-table-Win
ON CHOOSE OF btDetalhe IN FRAME F-Main /* Detalhe */
DO:
  find current dt-log-importa-xml no-lock no-error.
  if avail dt-log-importa-xml then do:
      run dtp/dts0918a.w(input rowid(dt-log-importa-xml)).
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-arquivo-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-arquivo-ini B-table-Win
ON LEAVE OF fi-arquivo-ini IN FRAME F-Main /* Arquivo */
DO:
  if index(input frame {&frame-name} fi-arquivo-ini,"*") > 0 then
      assign fi-arquivo-fim:sensitive in frame {&frame-name} = no.
  else
      assign fi-arquivo-fim:sensitive in frame {&frame-name} = yes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tp-pesq-nome
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tp-pesq-nome B-table-Win
ON VALUE-CHANGED OF tp-pesq-nome IN FRAME F-Main /* Tipo Pesquisa Nome */
DO:
  case input frame {&frame-name} tp-pesq-nome:
      when 4 then 
          assign fi-arquivo-ini:sensitive in frame {&frame-name} = yes
                 fi-arquivo-fim:sensitive in frame {&frame-name} = yes.
      otherwise
          assign fi-arquivo-ini:sensitive in frame {&frame-name} = yes
                 fi-arquivo-fim:sensitive in frame {&frame-name} = no.

                 
      
  end case.
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
  assign rs-sit-import    :screen-value in frame {&frame-name} = string(3). 
  assign dt-pesq-ini:screen-value in frame {&frame-name} = string(today, "99/99/9999")
         dt-pesq-fim:screen-value in frame {&frame-name} = string(today, "99/99/9999").
  apply "choose" to bt-pesquisa in frame {&frame-name}.

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
define input parameter p-sit-import     as integer no-undo.
define input parameter p-dt-pesq-ini    as date no-undo.
define input parameter p-dt-pesq-fim    as date no-undo.
define input parameter p-nom-arq-ini    as char no-undo.
define input parameter p-nom-arq-fim    as char no-undo.
if input frame {&frame-name} tp-pesq-nome = 4 then do:
    case p-sit-import:
        when 3 then do:
            open query br-log for each dt-log-importa-xml
                                 where dt-log-importa-xml.dt-processo >= p-dt-pesq-ini
                                   and dt-log-importa-xml.dt-processo <= p-dt-pesq-fim 
                                   and dt-log-importa-xml.nome-arq    >= p-nom-arq-ini 
                                   and dt-log-importa-xml.nome-arq    <= p-nom-arq-fim no-lock
            by dt-log-importa-xml.dt-processo
            by dt-log-importa-xml.hs-processo .

        end.
        otherwise do:
            open query br-log for each dt-log-importa-xml
                                 where dt-log-importa-xml.dt-processo >= p-dt-pesq-ini
                                   and dt-log-importa-xml.dt-processo <= p-dt-pesq-fim 
                                   and dt-log-importa-xml.sit-arquivo  = p-sit-import 
                                   and dt-log-importa-xml.nome-arq    >= p-nom-arq-ini 
                                   and dt-log-importa-xml.nome-arq    <= p-nom-arq-fim no-lock
            by dt-log-importa-xml.dt-processo
            by dt-log-importa-xml.hs-processo .
        end.
    end case.
end.
else do:
    if input frame {&frame-name} tp-pesq-nome = 1 then 
        assign p-nom-arq-ini = "*" + p-nom-arq-ini + "*".
    else 
        if input frame {&frame-name} tp-pesq-nome = 2 then 
            assign p-nom-arq-ini = p-nom-arq-ini + "*".
        else
            if input frame {&frame-name} tp-pesq-nome = 2 then 
                assign p-nom-arq-ini = "*" + p-nom-arq-ini.
            case p-sit-import:
                when 3 then do:
                    open query br-log for each dt-log-importa-xml
                                         where dt-log-importa-xml.dt-processo >= p-dt-pesq-ini
                                           and dt-log-importa-xml.dt-processo <= p-dt-pesq-fim 
                                           and dt-log-importa-xml.nome-arq    matches (p-nom-arq-ini)
                                           /* and dt-log-importa-xml.nome-arq    <= p-nom-arq-fim */ no-lock
                    by dt-log-importa-xml.dt-processo
                    by dt-log-importa-xml.hs-processo .

                end.
                otherwise do:
                    open query br-log for each dt-log-importa-xml
                                         where dt-log-importa-xml.dt-processo >= p-dt-pesq-ini
                                           and dt-log-importa-xml.dt-processo <= p-dt-pesq-fim 
                                           and dt-log-importa-xml.sit-arquivo  = p-sit-import 
                                           and dt-log-importa-xml.nome-arq    matches (p-nom-arq-ini) 
                                           /* and dt-log-importa-xml.nome-arq    <= p-nom-arq-fim */ no-lock
                    by dt-log-importa-xml.dt-processo
                    by dt-log-importa-xml.hs-processo .
                end.
            end case.

end.

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
  {src/adm/template/snd-list.i "dt-log-importa-xml"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fc-sit-arquivo B-table-Win 
FUNCTION fc-sit-arquivo RETURNS CHARACTER
  ( pSitArquivo as integer /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  if pSitArquivo = 1 then 
      return "OK" .
  else return "Erro".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

