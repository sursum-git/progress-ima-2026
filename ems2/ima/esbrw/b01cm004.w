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

DEF TEMP-TABLE tt-cm-emprestimo LIKE cm-emprestimo.

def new global shared var wh-window        as handle no-undo.
def new global shared var wh-pesquisa      as widget-handle.
def new global shared var l-implanta       as logical init no.
def new global shared var adm-broker-hdl   as handle no-undo.

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
&Scoped-define INTERNAL-TABLES tt-cm-emprestimo

/* Definitions for BROWSE br-table                                      */
&Scoped-define FIELDS-IN-QUERY-br-table tt-cm-emprestimo.cod-estab tt-cm-emprestimo.num-emprestimo tt-cm-emprestimo.mes-base tt-cm-emprestimo.vlr-emprestimo tt-cm-emprestimo.qtd-parcelas tt-cm-emprestimo.vlr-parcelas tt-cm-emprestimo.perc-juros tt-cm-emprestimo.qt-parc-pagas tt-cm-emprestimo.vlr-pago   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-table   
&Scoped-define SELF-NAME br-table
&Scoped-define QUERY-STRING-br-table FOR EACH tt-cm-emprestimo OF repres NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-table OPEN QUERY {&SELF-NAME} FOR EACH tt-cm-emprestimo OF repres NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-table tt-cm-emprestimo
&Scoped-define FIRST-TABLE-IN-QUERY-br-table tt-cm-emprestimo


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-table RECT-4 RECT-5 
&Scoped-Define DISPLAYED-OBJECTS fi-cod-estab cb-mes-base fi-ano-base ~
fi-vlr-emprestimo fi-qtd-parcelas fi-vlr-parcelas fi-perc-juros ~
fi-num-emprestimo 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-4 fi-cod-estab cb-mes-base fi-ano-base ~
fi-vlr-emprestimo fi-qtd-parcelas fi-perc-juros bt-conf bt-can 
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

DEFINE VARIABLE cb-mes-base AS CHARACTER FORMAT "x(15)" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Janeiro ","Fevereiro","Maráo","Abril","Maio","Junho","Julho","Agosto","Setembro","Outubro","Novembro","Dezembro" 
     DROP-DOWN-LIST
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE fi-ano-base AS INTEGER FORMAT ">>>>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-estab AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 4.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi-num-emprestimo AS INTEGER FORMAT ">>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-perc-juros AS DECIMAL FORMAT ">>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE VARIABLE fi-qtd-parcelas AS INTEGER FORMAT ">>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-vlr-emprestimo AS DECIMAL FORMAT ">>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-vlr-parcelas AS DECIMAL FORMAT ">>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

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
      tt-cm-emprestimo SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-table B-table-Win _FREEFORM
  QUERY br-table NO-LOCK DISPLAY
      tt-cm-emprestimo.cod-estab                                                        WIDTH 3
      tt-cm-emprestimo.num-emprestimo FORMAT ">9"             COLUMN-LABEL "Nro"        WIDTH 3
      tt-cm-emprestimo.mes-base                               COLUMN-LABEL "Mes Base"   WIDTH 13
      tt-cm-emprestimo.vlr-emprestimo FORMAT ">>,>>9.99":U    COLUMN-LABEL "Valor"      WIDTH 7
      tt-cm-emprestimo.qtd-parcelas   FORMAT ">>9":U          COLUMN-LABEL "Qt Parc"
      tt-cm-emprestimo.vlr-parcelas   FORMAT ">>,>>9.99":U    COLUMN-LABEL "Vl Parc"    WIDTH 7
      tt-cm-emprestimo.perc-juros     FORMAT ">>9.99":U       COLUMN-LABEL "% Juros"
      tt-cm-emprestimo.qt-parc-pagas  FORMAT ">>9":U          COLUMN-LABEL "Qt Pag"
      tt-cm-emprestimo.vlr-pago       FORMAT ">>,>>9.99":U    COLUMN-LABEL "Vl Pago"    WIDTH 7
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 68 BY 8.08
         FONT 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br-table AT ROW 1 COL 1
     fi-cod-estab AT ROW 1.75 COL 69 COLON-ALIGNED NO-LABEL WIDGET-ID 42
     cb-mes-base AT ROW 3.29 COL 69 COLON-ALIGNED NO-LABEL WIDGET-ID 32
     fi-ano-base AT ROW 3.25 COL 84.57 COLON-ALIGNED NO-LABEL WIDGET-ID 46
     fi-vlr-emprestimo AT ROW 4.83 COL 69 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     fi-qtd-parcelas AT ROW 4.79 COL 84.72 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     fi-vlr-parcelas AT ROW 6.42 COL 69 COLON-ALIGNED NO-LABEL WIDGET-ID 34
     fi-perc-juros AT ROW 6.42 COL 84.72 COLON-ALIGNED NO-LABEL WIDGET-ID 38
     bt-conf AT ROW 7.67 COL 70.86 WIDGET-ID 22
     bt-can AT ROW 7.67 COL 75.14 WIDGET-ID 20
     bt-inc AT ROW 7.67 COL 81.29 WIDGET-ID 18
     bt-mod AT ROW 7.67 COL 85.29 WIDGET-ID 26
     bt-del AT ROW 7.67 COL 89.29 WIDGET-ID 24
     fi-num-emprestimo AT ROW 1.75 COL 84.72 COLON-ALIGNED NO-LABEL WIDGET-ID 50
     "Estab" VIEW-AS TEXT
          SIZE 6 BY .5 AT ROW 1.21 COL 71 WIDGET-ID 44
          FGCOLOR 1 FONT 6
     "Mes Base" VIEW-AS TEXT
          SIZE 11 BY .5 AT ROW 2.75 COL 71 WIDGET-ID 16
          FGCOLOR 1 FONT 6
     "% Juros" VIEW-AS TEXT
          SIZE 6.57 BY .5 AT ROW 5.88 COL 86.72 WIDGET-ID 40
          FGCOLOR 1 FONT 6
     "Valor Parcelas" VIEW-AS TEXT
          SIZE 12 BY .5 AT ROW 5.88 COL 71 WIDGET-ID 36
          FGCOLOR 1 FONT 6
     "Qt Parc" VIEW-AS TEXT
          SIZE 6.57 BY .5 AT ROW 4.25 COL 86.72 WIDGET-ID 14
          FGCOLOR 1 FONT 6
     "Vlr Emprestimo" VIEW-AS TEXT
          SIZE 12 BY .5 AT ROW 4.29 COL 71 WIDGET-ID 12
          FGCOLOR 1 FONT 6
     "Ano" VIEW-AS TEXT
          SIZE 4.29 BY .5 AT ROW 2.71 COL 86.72 WIDGET-ID 48
          FGCOLOR 1 FONT 6
     "Num" VIEW-AS TEXT
          SIZE 6.29 BY .5 AT ROW 1.17 COL 86.72 WIDGET-ID 52
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
         HEIGHT             = 8.17
         WIDTH              = 93.86.
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
/* SETTINGS FOR COMBO-BOX cb-mes-base IN FRAME F-Main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-ano-base IN FRAME F-Main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-cod-estab IN FRAME F-Main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-num-emprestimo IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-perc-juros IN FRAME F-Main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-qtd-parcelas IN FRAME F-Main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-vlr-emprestimo IN FRAME F-Main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-vlr-parcelas IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-table
/* Query rebuild information for BROWSE br-table
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-cm-emprestimo OF repres NO-LOCK INDEXED-REPOSITION.
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
          fi-vlr-emprestimo = 0
          fi-num-emprestimo = 0
          cb-mes-base = ""
          fi-qtd-parcelas = 0
          fi-vlr-parcelas = 0
          fi-perc-juros = 0.

   DISABLE {&list-5} WITH FRAME {&FRAME-NAME}.

   IF NUM-RESULTS("br-table":U) > 0 THEN DO.
      ASSIGN fi-cod-estab = tt-cm-emprestimo.cod-estab
             fi-num-emprestimo = tt-cm-emprestimo.num-emprestimo
             cb-mes-base = tt-cm-emprestimo.mes-base
             fi-vlr-emprestimo = tt-cm-emprestimo.vlr-emprestimo
             fi-qtd-parcelas = tt-cm-emprestimo.qtd-parcelas
             fi-vlr-parcelas = tt-cm-emprestimo.vlr-parcelas
             fi-perc-juros = tt-cm-emprestimo.perc-juros.

      IF bt-inc:SENSITIVE THEN
         ENABLE {&list-5} WITH FRAME {&FRAME-NAME}.
   END.

   DISP fi-cod-estab
        fi-num-emprestimo
        cb-mes-base
        fi-vlr-emprestimo 
        fi-qtd-parcelas 
        fi-vlr-parcelas
        fi-perc-juros  
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
    IF INPUT FRAME {&FRAME-NAME} cb-mes-base = "" THEN DO.
       MESSAGE "Favor Informar o Mes Base"
           VIEW-AS ALERT-BOX ERROR BUTTONS OK.
       APPLY "ENTRY" TO cb-mes-base.
       RETURN NO-APPLY.
    END.

    IF INPUT FRAME {&FRAME-NAME} fi-ano-base = 0 THEN DO.
       MESSAGE "Favor Informar o Ano Base"
           VIEW-AS ALERT-BOX ERROR BUTTONS OK.
       APPLY "ENTRY" TO fi-ano-base.
       RETURN NO-APPLY.
    END.

    IF INPUT FRAME {&FRAME-NAME} fi-vlr-emprestimo = 0 THEN DO.
       MESSAGE "Favor Informar o Valor do Emprestimo"
           VIEW-AS ALERT-BOX ERROR BUTTONS OK.
       APPLY "ENTRY" TO fi-vlr-emprestimo.
       RETURN NO-APPLY.
    END.

    IF INPUT FRAME {&FRAME-NAME} fi-vlr-parcelas = 0 THEN DO.
       MESSAGE "Favor Informar o Valor das Parcelas"
           VIEW-AS ALERT-BOX ERROR BUTTONS OK.
       APPLY "ENTRY" TO fi-vlr-parcelas.
       RETURN NO-APPLY.
    END.

    IF INPUT FRAME {&FRAME-NAME} fi-perc-juros = 0 THEN DO.
       MESSAGE "Emprestimo sem Juros ? "
           VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE l-ok AS LOGICAL.
       IF NOT l-ok THEN DO.
          APPLY "ENTRY" TO fi-perc-juros.
          RETURN NO-APPLY.
       END.
    END.

    FIND tt-cm-emprestimo WHERE
         tt-cm-emprestimo.cod-estab = INPUT FRAME {&FRAME-NAME} fi-cod-estab AND
         tt-cm-emprestimo.cod-rep = repres.cod-rep AND
         tt-cm-emprestimo.num-emprestimo = INPUT FRAME {&FRAME-NAME} fi-num-emprestimo
         NO-ERROR.

    IF NOT AVAIL tt-cm-emprestimo THEN DO.
       CREATE tt-cm-emprestimo.
       ASSIGN tt-cm-emprestimo.cod-estab = INPUT FRAME {&FRAME-NAME} fi-cod-estab
              tt-cm-emprestimo.cod-rep = repres.cod-rep
              tt-cm-emprestimo.num-emprestimo = NEXT-VALUE(seq-cm-emprestimo). 
    END.
    ASSIGN tt-cm-emprestimo.mes-base = INPUT FRAME {&FRAME-NAME} cb-mes-base
           tt-cm-emprestimo.ano-base = INPUT FRAME {&FRAME-NAME} fi-ano-base
           tt-cm-emprestimo.vlr-emprestimo = INPUT FRAME {&FRAME-NAME} fi-vlr-emprestimo
           tt-cm-emprestimo.qtd-parcelas = INPUT FRAME {&FRAME-NAME} fi-qtd-parcelas
           tt-cm-emprestimo.vlr-parcelas = INPUT FRAME {&FRAME-NAME} fi-vlr-parcelas
           tt-cm-emprestimo.perc-juros = INPUT FRAME {&FRAME-NAME} fi-perc-juros.
        
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

     DELETE tt-cm-emprestimo.
     IF br-table:DELETE-CURRENT-ROW() IN FRAME {&FRAME-NAME} THEN.
  END.

  ASSIGN fi-vlr-emprestimo = 0
         fi-qtd-parcelas = 0.

  IF NUM-RESULTS("br-table":U) = 0 THEN
     ASSIGN bt-mod:SENSITIVE IN FRAME {&FRAME-NAME} = NO 
            bt-del:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

   DISP fi-vlr-emprestimo 
        fi-qtd-parcelas 
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
          fi-num-emprestimo = CURRENT-VALUE(seq-cm-emprestimo) + 1
          fi-vlr-emprestimo = 0
          fi-qtd-parcelas = 0
          fi-ano-base = 0
          fi-perc-juros = 0.

   DISP fi-cod-estab
        fi-num-emprestimo
        fi-vlr-emprestimo 
        fi-qtd-parcelas 
        fi-perc-juros
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

   ASSIGN bt-inc:SENSITIVE IN FRAME {&FRAME-NAME} = NO
          bt-mod:SENSITIVE IN FRAME {&FRAME-NAME} = NO
          bt-del:SENSITIVE IN FRAME {&FRAME-NAME} = NO
          bt-can:SENSITIVE IN FRAME {&FRAME-NAME} = YES
          bt-conf:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

   APPLY 'entry' TO cb-mes-base.
   RETURN NO-APPLY.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ano-base
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ano-base B-table-Win
ON LEAVE OF fi-ano-base IN FRAME F-Main
DO:
   RUN pi-calc-vlr-parc.
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


&Scoped-define SELF-NAME fi-num-emprestimo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-num-emprestimo B-table-Win
ON LEAVE OF fi-num-emprestimo IN FRAME F-Main
DO:
   RUN pi-calc-vlr-parc.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-qtd-parcelas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-qtd-parcelas B-table-Win
ON LEAVE OF fi-qtd-parcelas IN FRAME F-Main
DO:
   RUN pi-calc-vlr-parc.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-vlr-emprestimo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-vlr-emprestimo B-table-Win
ON LEAVE OF fi-vlr-emprestimo IN FRAME F-Main
DO:
   RUN pi-calc-vlr-parc.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
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
    FOR EACH cm-emprestimo WHERE
             cm-emprestimo.cod-rep = repres.cod-rep EXCLUSIVE-LOCK.
        DELETE cm-emprestimo.
    END.

    FOR EACH tt-cm-emprestimo NO-LOCK.
        CREATE cm-emprestimo.
        BUFFER-COPY tt-cm-emprestimo TO cm-emprestimo.
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
    ASSIGN fi-cod-estab      = ""
           fi-num-emprestimo = 0
           cb-mes-base       = ""
           fi-ano-base       = 0
           fi-vlr-emprestimo = 0
           fi-qtd-parcelas   = 0
           fi-vlr-parcelas   = 0
           fi-perc-juros     = 0.

    IF AVAIL tt-cm-emprestimo AND 
       NUM-RESULTS("br-table":U) > 0 THEN 
       ASSIGN fi-cod-estab = tt-cm-emprestimo.cod-estab
              fi-num-emprestimo = tt-cm-emprestimo.num-emprestimo
              cb-mes-base = tt-cm-emprestimo.mes-base
              fi-ano-base = tt-cm-emprestimo.ano-base
              fi-vlr-emprestimo = tt-cm-emprestimo.vlr-emprestimo
              fi-qtd-parcelas = tt-cm-emprestimo.qtd-parcelas
              fi-vlr-parcelas = tt-cm-emprestimo.vlr-parcelas
              fi-perc-juros = tt-cm-emprestimo.perc-juros.

    DISP fi-cod-estab
         fi-num-emprestimo
         cb-mes-base
         fi-ano-base
         fi-vlr-emprestimo 
         fi-qtd-parcelas 
         fi-vlr-parcelas
         fi-perc-juros  
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-open-query-cases B-table-Win 
PROCEDURE local-open-query-cases :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  EMPTY TEMP-TABLE tt-cm-emprestimo.
  FOR EACH cm-emprestimo OF repres NO-LOCK.
      CREATE tt-cm-emprestimo.
      BUFFER-COPY cm-emprestimo TO tt-cm-emprestimo.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-calc-vlr-parc B-table-Win 
PROCEDURE pi-calc-vlr-parc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     IF INPUT FRAME {&FRAME-NAME} fi-vlr-emprestimo > 0 AND
        INPUT FRAME {&FRAME-NAME} fi-qtd-parcelas > 0  THEN
        ASSIGN fi-vlr-parcelas = INPUT FRAME {&FRAME-NAME} fi-vlr-emprestimo / 
                                 INPUT FRAME {&FRAME-NAME} fi-qtd-parcelas.    

     DISP fi-vlr-parcelas 
          WITH FRAME {&FRAME-NAME}.

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
  {src/adm/template/snd-list.i "tt-cm-emprestimo"}

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

