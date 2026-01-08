&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
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

/*:T v†ri†veis de uso globla */
def new global shared var v-row-parent as rowid no-undo.

/*:T vari†veis de uso local */
def var v-row-table  as rowid.

/*:T fim das variaveis utilizadas no estilo */

DEF TEMP-TABLE tt-cm-exc-condpagto LIKE cm-exc-condpagto
    FIELD descricao LIKE cond-pagto.descricao.

def new global shared var wh-window        as handle no-undo.
def new global shared var wh-pesquisa      as widget-handle.
def new global shared var l-implanta       as logical init no.
def new global shared var adm-broker-hdl   as handle no-undo.

DEF VAR c-tab-preco AS CHAR.

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

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES repres
&Scoped-define FIRST-EXTERNAL-TABLE repres


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR repres.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-cm-exc-condpagto

/* Definitions for BROWSE br-table                                      */
&Scoped-define FIELDS-IN-QUERY-br-table tt-cm-exc-condpagto.cod-estab tt-cm-exc-condpagto.mes tt-cm-exc-condpagto.ano tt-cm-exc-condpagto.cod-cond-pag tt-cm-exc-condpagto.descricao tt-cm-exc-condpagto.tab-preco tt-cm-exc-condpagto.perc-menor-tab tt-cm-exc-condpagto.perc-maior-tab   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-table   
&Scoped-define SELF-NAME br-table
&Scoped-define QUERY-STRING-br-table FOR EACH tt-cm-exc-condpagto OF repres NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-table OPEN QUERY {&SELF-NAME} FOR EACH tt-cm-exc-condpagto OF repres NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-table tt-cm-exc-condpagto
&Scoped-define FIRST-TABLE-IN-QUERY-br-table tt-cm-exc-condpagto


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-table RECT-4 RECT-5 
&Scoped-Define DISPLAYED-OBJECTS fi-cod-estab fi-cod-cond-pag cb-tab-preco ~
fi-perc-menor-tab fi-perc-maior-tab 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-4 fi-cod-estab fi-cod-cond-pag cb-tab-preco ~
fi-perc-menor-tab fi-perc-maior-tab bt-conf bt-can 
&Scoped-define List-5 bt-mod bt-del 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" B-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<FOREIGN-KEYS>
cod-rep||y|espec.cm-exc-item.cod-rep
it-codigo||y|espec.cm-exc-item.it-codigo
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "cod-rep,it-codigo"':U).

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
DEFINE BUTTON bt-can AUTO-END-KEY 
     IMAGE-UP FILE "image/im-can.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-can.bmp":U
     LABEL "bt inclui 2" 
     SIZE 4 BY 1.13 TOOLTIP "Cancela modo Manutená∆o".

DEFINE BUTTON bt-conf 
     IMAGE-UP FILE "image/im-chck3.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-cq.bmp":U
     LABEL "Button 1" 
     SIZE 4 BY 1.13 TOOLTIP "Confirma Alteraá‰es".

DEFINE BUTTON bt-del 
     IMAGE-UP FILE "image/im-era.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-era.bmp":U
     LABEL "bt inclui 2" 
     SIZE 4 BY 1.13 TOOLTIP "Elimina Item do Container".

DEFINE BUTTON bt-inc 
     IMAGE-UP FILE "image/im-add.bmp":U
     LABEL "Button 3" 
     SIZE 4 BY 1.13 TOOLTIP "Cria Itens no Container".

DEFINE BUTTON bt-mod 
     IMAGE-UP FILE "image/im-mod.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-mod.bmp":U
     LABEL "bt inclui 2" 
     SIZE 4 BY 1.13 TOOLTIP "Modifica Itens do Container".

DEFINE VARIABLE cb-tab-preco AS CHARACTER 
     VIEW-AS COMBO-BOX INNER-LINES 7
     DROP-DOWN AUTO-COMPLETION UNIQUE-MATCH
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cod-cond-pag AS INTEGER FORMAT ">>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-estab AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 4.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi-perc-maior-tab AS DECIMAL FORMAT ">>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE VARIABLE fi-perc-menor-tab AS DECIMAL FORMAT ">>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 24 BY 1.5
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 24 BY 6.5.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-table FOR 
      tt-cm-exc-condpagto SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-table B-table-Win _FREEFORM
  QUERY br-table NO-LOCK DISPLAY
      tt-cm-exc-condpagto.cod-estab       WIDTH 3
      tt-cm-exc-condpagto.mes
      tt-cm-exc-condpagto.ano
      tt-cm-exc-condpagto.cod-cond-pag    WIDTH 4             COLUMN-LABEL "Cond"  
      tt-cm-exc-condpagto.descricao       WIDTH 30            COLUMN-LABEL "Descricao" 
      tt-cm-exc-condpagto.tab-preco 
      tt-cm-exc-condpagto.perc-menor-tab      FORMAT ">>9.99":U   COLUMN-LABEL "%Comis <"
      tt-cm-exc-condpagto.perc-maior-tab      FORMAT ">>9.99":U   COLUMN-LABEL "%Comis >"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 68 BY 8.08
         FONT 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br-table AT ROW 1 COL 1
     fi-cod-estab AT ROW 1.79 COL 70 COLON-ALIGNED NO-LABEL WIDGET-ID 34
     fi-cod-cond-pag AT ROW 3.33 COL 70 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     cb-tab-preco AT ROW 4.88 COL 70 COLON-ALIGNED NO-LABEL WIDGET-ID 42
     fi-perc-menor-tab AT ROW 6.46 COL 70 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     fi-perc-maior-tab AT ROW 6.42 COL 81 COLON-ALIGNED NO-LABEL WIDGET-ID 44
     bt-conf AT ROW 7.67 COL 70.86 WIDGET-ID 22
     bt-can AT ROW 7.67 COL 75 WIDGET-ID 20
     bt-inc AT ROW 7.71 COL 81 WIDGET-ID 18
     bt-mod AT ROW 7.71 COL 85.14 WIDGET-ID 26
     bt-del AT ROW 7.71 COL 89.29 WIDGET-ID 24
     "Cond Pagto" VIEW-AS TEXT
          SIZE 11 BY .5 AT ROW 2.83 COL 72 WIDGET-ID 16
          FGCOLOR 1 FONT 6
     "%Comis <" VIEW-AS TEXT
          SIZE 9 BY .5 AT ROW 5.88 COL 72 WIDGET-ID 12
          FGCOLOR 1 FONT 6
     "Estab" VIEW-AS TEXT
          SIZE 5 BY .5 AT ROW 1.25 COL 72 WIDGET-ID 36
          FGCOLOR 1 FONT 6
     "Tab. Preáo" VIEW-AS TEXT
          SIZE 9 BY .5 AT ROW 4.33 COL 72.14 WIDGET-ID 40
          FGCOLOR 1 FONT 6
     "%Comis >" VIEW-AS TEXT
          SIZE 9 BY .5 AT ROW 5.83 COL 83 WIDGET-ID 46
          FGCOLOR 1 FONT 6
     RECT-4 AT ROW 7.5 COL 70 WIDGET-ID 28
     RECT-5 AT ROW 1 COL 70 WIDGET-ID 30
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 FONT 1 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   External Tables: mgcom.repres
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
         HEIGHT             = 8.21
         WIDTH              = 94.
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
/* BROWSE-TAB br-table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON bt-can IN FRAME F-Main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR BUTTON bt-conf IN FRAME F-Main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR BUTTON bt-del IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR BUTTON bt-inc IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-mod IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR COMBO-BOX cb-tab-preco IN FRAME F-Main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-cod-cond-pag IN FRAME F-Main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-cod-estab IN FRAME F-Main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-perc-maior-tab IN FRAME F-Main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-perc-menor-tab IN FRAME F-Main
   NO-ENABLE 4                                                          */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-table
/* Query rebuild information for BROWSE br-table
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-cm-exc-condpagto OF repres NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
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
ON VALUE-CHANGED OF br-table IN FRAME F-Main
DO:
   ASSIGN fi-cod-estab = ""
          fi-cod-cond-pag = 0
          cb-tab-preco = ""
          fi-perc-menor-tab = 0
          fi-perc-maior-tab = 0.

   DISABLE {&list-5} WITH FRAME {&FRAME-NAME}.

   IF NUM-RESULTS("br-table":U) > 0 THEN DO.
      ASSIGN fi-cod-estab = tt-cm-exc-condpagto.cod-estab
             fi-cod-cond-pag = tt-cm-exc-condpagto.cod-cond-pag 
             cb-tab-preco = tt-cm-exc-condpagto.tab-preco
             fi-perc-menor-tab = tt-cm-exc-condpagto.perc-menor-tab
             fi-perc-maior-tab = tt-cm-exc-condpagto.perc-maior-tab.

      IF bt-inc:SENSITIVE THEN
         ENABLE {&list-5} WITH FRAME {&FRAME-NAME}.
   END.

   DISP fi-cod-estab
        fi-cod-cond-pag 
        cb-tab-preco
        fi-perc-menor-tab 
        fi-perc-maior-tab
        WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-can
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-can B-table-Win
ON CHOOSE OF bt-can IN FRAME F-Main /* bt inclui 2 */
DO:
   DISABLE {&list-4} WITH FRAME {&FRAME-NAME}.

   ASSIGN bt-inc:SENSITIVE IN FRAME {&FRAME-NAME} = YES
          bt-mod:SENSITIVE IN FRAME {&FRAME-NAME} = YES
          bt-del:SENSITIVE IN FRAME {&FRAME-NAME} = YES
          bt-can:SENSITIVE IN FRAME {&FRAME-NAME} = NO
          bt-conf:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

   APPLY 'value-changed' TO br-table.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-conf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-conf B-table-Win
ON CHOOSE OF bt-conf IN FRAME F-Main /* Button 1 */
DO:
    IF INPUT FRAME {&FRAME-NAME} fi-perc-menor-tab = 0 THEN DO.
       MESSAGE "Favor Informar % de Comiss∆o"
           VIEW-AS ALERT-BOX ERROR BUTTONS OK.
       APPLY "ENTRY" TO fi-perc-menor-tab.
       RETURN NO-APPLY.
    END.

    FIND tt-cm-exc-condpagto WHERE
         tt-cm-exc-condpagto.cod-estab = INPUT FRAME {&FRAME-NAME} fi-cod-estab AND
         tt-cm-exc-condpagto.cod-rep = repres.cod-rep AND
         tt-cm-exc-condpagto.cod-cond-pag = INPUT FRAME {&FRAME-NAME} fi-cod-cond-pag 
         NO-ERROR.

    IF NOT AVAIL tt-cm-exc-condpagto THEN DO.
       CREATE tt-cm-exc-condpagto.
       ASSIGN tt-cm-exc-condpagto.cod-estab = INPUT FRAME {&FRAME-NAME} fi-cod-estab 
              tt-cm-exc-condpagto.cod-rep = repres.cod-rep 
              tt-cm-exc-condpagto.cod-cond-pag = INPUT FRAME {&FRAME-NAME} fi-cod-cond-pag
              tt-cm-exc-condpagto.descricao = cond-pagto.descricao.
    END.
    ASSIGN tt-cm-exc-condpagto.tab-preco = INPUT FRAME {&FRAME-NAME} cb-tab-preco
           tt-cm-exc-condpagto.perc-menor-tab = INPUT FRAME {&FRAME-NAME} fi-perc-menor-tab
           tt-cm-exc-condpagto.perc-maior-tab = INPUT FRAME {&FRAME-NAME} fi-perc-maior-tab.

    {&OPEN-QUERY-br-table}

    DISABLE {&list-4} WITH FRAME {&FRAME-NAME}.

    ASSIGN bt-inc:SENSITIVE IN FRAME  {&FRAME-NAME} = YES
           bt-mod:SENSITIVE IN FRAME  {&FRAME-NAME} = YES
           bt-del:SENSITIVE IN FRAME  {&FRAME-NAME} = YES
           bt-can:SENSITIVE IN FRAME  {&FRAME-NAME} = NO
           bt-conf:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-del
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-del B-table-Win
ON CHOOSE OF bt-del IN FRAME F-Main /* bt inclui 2 */
DO:
  IF br-table:NUM-SELECTED-ROWS > 0 THEN DO ON ERROR UNDO, RETURN NO-APPLY:
     GET CURRENT br-table.

     DELETE tt-cm-exc-condpagto.
     IF br-table:DELETE-CURRENT-ROW() IN FRAME {&FRAME-NAME} THEN.
  END.

  ASSIGN fi-cod-cond-pag = 0
         cb-tab-preco = ""
         fi-perc-menor-tab = 0
         fi-perc-maior-tab = 0.

  IF NUM-RESULTS("br-table":U) = 0 THEN
     ASSIGN bt-mod:SENSITIVE IN FRAME {&FRAME-NAME} = NO 
            bt-del:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

   DISP fi-cod-cond-pag 
        cb-tab-preco
        fi-perc-menor-tab 
        fi-perc-maior-tab
        WITH FRAME {&FRAME-NAME}.

  {&OPEN-QUERY-br-table}
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-inc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-inc B-table-Win
ON CHOOSE OF bt-inc IN FRAME F-Main /* Button 3 */
DO:
   ENABLE {&list-4} WITH FRAME {&FRAME-NAME}.

   ASSIGN bt-inc:SENSITIVE IN FRAME {&FRAME-NAME} = NO
          bt-mod:SENSITIVE IN FRAME {&FRAME-NAME} = NO
          bt-del:SENSITIVE IN FRAME {&FRAME-NAME} = NO
          bt-can:SENSITIVE IN FRAME {&FRAME-NAME} = YES
          bt-conf:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

   ASSIGN fi-cod-estab = ""
          fi-cod-cond-pag = 0
          fi-perc-menor-tab = 0
          fi-perc-maior-tab = 0.

   DISP fi-cod-estab
        fi-cod-cond-pag 
        fi-perc-menor-tab 
        fi-perc-maior-tab
        WITH FRAME {&FRAME-NAME}.

   APPLY 'entry' TO fi-cod-estab.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-inc B-table-Win
ON RETURN OF bt-inc IN FRAME F-Main /* Button 3 */
DO:
  APPLY 'choose' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-mod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-mod B-table-Win
ON CHOOSE OF bt-mod IN FRAME F-Main /* bt inclui 2 */
DO:
    
   ENABLE {&list-4} WITH FRAME {&FRAME-NAME}.
   ASSIGN fi-cod-cond-pag:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

   ASSIGN bt-inc:SENSITIVE IN FRAME {&FRAME-NAME} = NO
          bt-mod:SENSITIVE IN FRAME {&FRAME-NAME} = NO
          bt-del:SENSITIVE IN FRAME {&FRAME-NAME} = NO
          bt-can:SENSITIVE IN FRAME {&FRAME-NAME} = YES
          bt-conf:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

   APPLY 'entry' TO cb-tab-preco.
   RETURN NO-APPLY.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-tab-preco
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-tab-preco B-table-Win
ON LEAVE OF cb-tab-preco IN FRAME F-Main
DO:
   /*
   IF cb-tab-preco:SCREEN-VALUE <> cb-tab-preco AND
      cb-tp-preco:SCREEN-VALUE <> "1" THEN DO.
      ASSIGN de-ind-finan = IF de-ind-finan = 0 THEN 1 ELSE de-ind-finan.
      FOR EACH tt-ped-item.
          FIND preco-item WHERE
               preco-item.nr-tabpre = cb-tab-preco:SCREEN-VALUE AND
               preco-item.it-codigo = tt-ped-item.it-codigo AND
               preco-item.cod-refer = tt-ped-item.cod-refer NO-LOCK NO-ERROR.
          IF AVAIL preco-item THEN
             ASSIGN tt-ped-item.vl-preuni = preco-item.preco-venda * de-ind-finan.
      END.
      RUN adm-open-query-cases.
   END.
   ASSIGN c-tab-preco = cb-tab-preco:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-tab-preco B-table-Win
ON VALUE-CHANGED OF cb-tab-preco IN FRAME F-Main
DO:
    /*
   FIND tb-preco WHERE
        tb-preco.nr-tabpre = SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
   IF AVAIL tb-preco THEN DO.
      FIND moeda WHERE
           moeda.mo-codigo = tb-preco.mo-codigo NO-LOCK NO-ERROR.
      IF AVAIL moeda THEN
         ASSIGN fi-moeda:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(tb-preco.mo-codigo)
                fi-desc-moeda:SCREEN-VALUE = moeda.descricao.
   END.
   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-cond-pag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-cond-pag B-table-Win
ON LEAVE OF fi-cod-cond-pag IN FRAME F-Main
DO:
   FIND cond-pagto WHERE
        cond-pagto.cod-cond-pag = INPUT FRAME {&FRAME-NAME} fi-cod-cond-pag NO-LOCK NO-ERROR.

   IF NOT AVAIL cond-pagto THEN DO.
      MESSAGE "Condiá∆o de Pagamento n∆o Cadastrada..."
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      APPLY "entry" TO SELF.
      RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-cond-pag B-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-cod-cond-pag IN FRAME F-Main
DO:
  {include/zoomvar.i &prog-zoom=adzoom\z01ad039.w
                     &campo=fi-cod-cond-pag
                     &campozoom=cod-cond-pag}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-estab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-estab B-table-Win
ON LEAVE OF fi-cod-estab IN FRAME F-Main
DO:
   FIND estabelec WHERE
        estabelec.cod-estab = INPUT FRAME {&FRAME-NAME} fi-cod-estab NO-LOCK NO-ERROR.

   IF NOT AVAIL estabelec THEN DO.
      MESSAGE "Estabelecimento n∆o Cadastrado..."
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      APPLY "entry" TO SELF.
      RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-estab B-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-cod-estab IN FRAME F-Main
DO:
  {include/zoomvar.i &prog-zoom=adzoom\z01ad107.w
                     &campo=fi-cod-estab
                     &campozoom=cod-estabel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
fi-cod-cond-pag:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.
fi-cod-estab:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.

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
  {src/adm/template/row-list.i "repres"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "repres"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record B-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH cm-exc-condpagto WHERE
             cm-exc-condpagto.cod-rep = repres.cod-rep EXCLUSIVE-LOCK.
        DELETE cm-exc-condpagto.
    END.

    FOR EACH tt-cm-exc-condpagto NO-LOCK.
        CREATE cm-exc-condpagto.
        BUFFER-COPY tt-cm-exc-condpagto TO cm-exc-condpagto.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-disable-fields B-table-Win 
PROCEDURE local-disable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */


  /* Code placed here will execute AFTER standard behavior.    */
  DISABLE bt-inc {&list-4} {&list-5} WITH FRAME {&FRAME-NAME}.

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
    
  ASSIGN fi-cod-estab = ""
         fi-cod-cond-pag = 0
         cb-tab-preco = ""
         fi-perc-menor-tab = 0.

   IF AVAIL tt-cm-exc-condpagto AND 
      NUM-RESULTS("br-table":U) > 0 THEN 
      ASSIGN fi-cod-estab = tt-cm-exc-condpagto.cod-estab
             fi-cod-cond-pag = tt-cm-exc-condpagto.cod-cond-pag
             cb-tab-preco = tt-cm-exc-condpagto.tab-preco
             fi-perc-menor-tab = tt-cm-exc-condpagto.perc-comis.

   DISP fi-cod-estab
        fi-cod-cond-pag 
        cb-tab-preco
        fi-perc-menor-tab 
        WITH FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields B-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute AFTER standard behavior.    */
  ENABLE bt-inc WITH FRAME {&FRAME-NAME}.

  DISABLE {&list-5} WITH FRAME {&FRAME-NAME}.

  IF NUM-RESULTS("br-table":U) > 0 THEN 
     ENABLE {&list-5} WITH FRAME {&FRAME-NAME}.
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

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN c-tab-preco = ''.
  FOR EACH tb-preco NO-LOCK.
      ASSIGN c-tab-preco = IF c-tab-preco = ''
                           THEN tb-preco.nr-tabpre
                           ELSE c-tab-preco + "," + tb-preco.nr-tabpre.
  END.
  //ASSIGN cb-tab-preco:LIST-ITEMS IN FRAME {&FRAME-NAME} = c-tab-preco.

  &if "{&FNC_MULTI_IDIOMA}" = "Yes" &then
      run utp/ut-lstit.p (input-output c-tab-preco).
      assign cb-tab-preco:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = c-tab-preco.
  &else
     ASSIGN cb-tab-preco:LIST-ITEMS IN FRAME {&FRAME-NAME} = c-tab-preco.
  &endif

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-open-query-cases B-table-Win 
PROCEDURE local-open-query-cases :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  EMPTY TEMP-TABLE tt-cm-exc-condpagto.
  FOR EACH cm-exc-condpagto OF repres NO-LOCK.
      CREATE tt-cm-exc-condpagto.
      BUFFER-COPY cm-exc-condpagto TO tt-cm-exc-condpagto.

      FIND cond-pagto WHERE
           cond-pagto.cod-cond-pag = cm-exc-condpagto.cod-cond-pag NO-LOCK NO-ERROR.
      IF AVAIL cond-pagto THEN
         ASSIGN tt-cm-exc-condpagto.descricao = cond-pagto.descricao.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query-cases':U ) .

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-validate B-table-Win 
PROCEDURE pi-validate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*:T------------------------------------------------------------------------------
  Purpose:Validar a viewer     
  Parameters:  <none>
  Notes: N∆o fazer assign aqui. Nesta procedure
  devem ser colocadas apenas validaá‰es, pois neste ponto do programa o registro 
  ainda n∆o foi criado.       
------------------------------------------------------------------------------*/
/*    {include/i-vldfrm.i} /*:T Validaá∆o de dicion†rio */*/
    
/*:T    Segue um exemplo de validaá∆o de programa */
/*       find tabela where tabela.campo1 = c-variavel and               */
/*                         tabela.campo2 > i-variavel no-lock no-error. */
      
      /*:T Este include deve ser colocado sempre antes do ut-msgs.p */
/*       {include/i-vldprg.i}                                             */
/*       run utp/ut-msgs.p (input "show":U, input 7, input return-value). */
/*       return 'ADM-ERROR':U.                                            */

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
  {src/adm/template/sndkycas.i "cod-rep" "cm-exc-item" "cod-rep"}
  {src/adm/template/sndkycas.i "it-codigo" "cm-exc-item" "it-codigo"}

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
  {src/adm/template/snd-list.i "repres"}
  {src/adm/template/snd-list.i "tt-cm-exc-condpagto"}

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

