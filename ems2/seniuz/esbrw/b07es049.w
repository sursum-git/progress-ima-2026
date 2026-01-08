&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
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
{include/i-prgvrs.i B07ES049 2.04.00.000}

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
define var v-row as rowid no-undo.
    
/*:T Variaveis usadas internamente pelo estilo, favor nao elimina-las     */

DEF TEMP-TABLE tt-etq-reservadas LIKE ob-etiqueta
    FIELD tp-acao                AS CHAR.

DEF TEMP-TABLE tt-etq-estoque LIKE ob-etiqueta
    FIELD tipo-tear           LIKE ordem-benefic.tipo-tear
    INDEX indice1 localizacao ASCENDING num-etiqueta DESCENDING.

DEF NEW GLOBAL SHARED VAR gr-ob-etiqueta AS ROWID NO-UNDO.
DEF VAR i-row AS INT.
DEF VAR h-acomp AS HANDLE.
DEF VAR c-dia AS CHAR.
DEF VAR da-dt-entrega AS DATE.
DEF VAR c-lotes AS CHAR.
DEFINE VARIABLE c-desc-dentro AS CHARACTER  NO-UNDO.
DEF VAR h-query AS HANDLE.  
DEFINE VAR c-nr-pedcli         LIKE ped-venda.nr-pedcli.

/* Variavies de Parƒmetros */
DEFINE VAR c-it-codigo-ini     LIKE ped-item-ext.it-codigo INIT "5".                 
DEFINE VAR c-it-codigo-fin     LIKE ped-item-ext.it-codigo INIT "5ZZZZZZZZZZZZZZZ".  
DEFINE VAR c-cod-refer-ini     LIKE ped-item-ext.cod-refer.
DEFINE VAR c-cod-refer-fin     LIKE ped-item-ext.cod-refer INIT "ZZZZZZZZZZ".   
DEFINE VAR c-corte-comerc-ini  LIKE corte-comerc.codigo    INIT "A".
DEFINE VAR c-corte-comerc-fin  LIKE corte-comerc.codigo    INIT "Z".
DEFINE VAR c-cod-qualid-ini    LIKE ob-etiqueta.cod-qualid INIT "A".
DEFINE VAR c-cod-qualid-fin    LIKE ob-etiqueta.cod-qualid INIT "Z".
DEFINE VAR c-cod-obsoleto-ini  LIKE ref-item-ext.cod-obsoleto INIT '0'.
DEFINE VAR c-cod-obsoleto-fin  LIKE ref-item-ext.cod-obsoleto INIT 'Z'.
DEFINE VAR c-nuance-ini        LIKE ob-etiqueta.nuance.
DEFINE VAR c-nuance-fin        LIKE ob-etiqueta.nuance INIT "ZZ".
DEFINE VAR c-tp-tecelagem      AS CHAR INIT "1".
DEFINE VAR c-opc-artigo        AS CHAR INIT 'A'.
DEFINE VAR l-lote-todos        AS LOG INIT YES.
DEFINE VAR l-lote-pp           AS LOG INIT NO.
DEFINE VAR l-lote-pd           AS LOG INIT NO.
DEFINE VAR l-lote-rp           AS LOG INIT NO.
DEFINE VAR l-lote-rd           AS LOG INIT NO.
DEFINE VAR l-lote-sc           AS LOG INIT NO.
DEFINE VAR l-lote-ca           AS LOG INIT NO.
DEFINE VAR l-ok                AS LOG.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE BrowserCadastro2
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-etq-estoque

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-etq-estoque tt-etq-reservadas

/* Definitions for BROWSE br-etq-estoque                                */
&Scoped-define FIELDS-IN-QUERY-br-etq-estoque tt-etq-estoque.localizacao tt-etq-estoque.num-etiqueta tt-etq-estoque.nuance tt-etq-estoque.cod-qualid tt-etq-estoque.tipo-tear tt-etq-estoque.quantidade   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-etq-estoque   
&Scoped-define SELF-NAME br-etq-estoque
&Scoped-define OPEN-QUERY-br-etq-estoque RUN pi-soma-est. OPEN QUERY {&SELF-NAME} FOR EACH tt-etq-estoque NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-etq-estoque tt-etq-estoque
&Scoped-define FIRST-TABLE-IN-QUERY-br-etq-estoque tt-etq-estoque


/* Definitions for BROWSE br-etq-reservadas                             */
&Scoped-define FIELDS-IN-QUERY-br-etq-reservadas tt-etq-reservadas.num-etiqueta tt-etq-reservadas.quantidade   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-etq-reservadas   
&Scoped-define SELF-NAME br-etq-reservadas
&Scoped-define OPEN-QUERY-br-etq-reservadas RUN pi-soma-res. OPEN QUERY {&SELF-NAME} FOR EACH tt-etq-reservadas WHERE                                  tt-etq-reservadas.tp-acao <> 'Del'                                  NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-etq-reservadas tt-etq-reservadas
&Scoped-define FIRST-TABLE-IN-QUERY-br-etq-reservadas tt-etq-reservadas


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-br-etq-estoque}~
    ~{&OPEN-QUERY-br-etq-reservadas}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-etq-estoque br-etq-reservadas bt-add ~
bt-del 
&Scoped-Define DISPLAYED-OBJECTS fi-tot-reservado fi-tot-estoque 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-4 bt-sel bt-det bt-add bt-del 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" B-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<FOREIGN-KEYS>
it-codigo|y|y|espec.ob-etiqueta.it-codigo
cod-refer||y|espec.ob-etiqueta.cod-refer
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "it-codigo",
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
DEFINE BUTTON bt-add 
     IMAGE-UP FILE "adeicon\next-au":U
     IMAGE-INSENSITIVE FILE "adeicon\next-ai":U
     LABEL "" 
     SIZE 5 BY 1.5.

DEFINE BUTTON bt-del 
     IMAGE-UP FILE "adeicon\prev-au":U
     IMAGE-INSENSITIVE FILE "adeicon\prev-ai":U
     LABEL "" 
     SIZE 5 BY 1.5.

DEFINE BUTTON bt-det 
     IMAGE-UP FILE "image/im-det.bmp":U
     LABEL "" 
     SIZE 4.86 BY 1.38 TOOLTIP "Detalha Etiquetas Reservadas".

DEFINE BUTTON bt-sel 
     IMAGE-UP FILE "image/im-ran.bmp":U
     LABEL "" 
     SIZE 4.86 BY 1.38 TOOLTIP "Detalha Etiquetas Reservadas".

DEFINE VARIABLE fi-tot-estoque AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Total Dispon¡vel" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .79
     FGCOLOR 9 FONT 6 NO-UNDO.

DEFINE VARIABLE fi-tot-reservado AS DECIMAL FORMAT ">>>,>>9.99":U INITIAL 0 
     LABEL "Reservado" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88
     FGCOLOR 2 FONT 6 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-etq-estoque FOR 
      tt-etq-estoque SCROLLING.

DEFINE QUERY br-etq-reservadas FOR 
      tt-etq-reservadas SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-etq-estoque
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-etq-estoque B-table-Win _FREEFORM
  QUERY br-etq-estoque NO-LOCK DISPLAY
      tt-etq-estoque.localizacao  FORMAT "999/999":U   COLUMN-LABEL "Localiza‡Æo"  WIDTH 9
      tt-etq-estoque.num-etiqueta FORMAT "999999999":U COLUMN-LABEL "Etiqueta"     WIDTH 8
      tt-etq-estoque.nuance       FORMAT " X(2)":U      COLUMN-LABEL "Nce"      WIDTH 3
      tt-etq-estoque.cod-qualid   FORMAT "X(2)":U      COLUMN-LABEL "Qualidade"        
      tt-etq-estoque.tipo-tear                         COLUMN-LABEL "Tecelagem"
      tt-etq-estoque.quantidade   FORMAT ">>9.99":U    COLUMN-LABEL "Qtde (m)"     WIDTH 8
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 52 BY 8.25
         FONT 1
         TITLE "Pe‡as Dispon¡veis".

DEFINE BROWSE br-etq-reservadas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-etq-reservadas B-table-Win _FREEFORM
  QUERY br-etq-reservadas NO-LOCK DISPLAY
      tt-etq-reservadas.num-etiqueta FORMAT "999999999":U COLUMN-LABEL "Etiqueta"
      tt-etq-reservadas.quantidade FORMAT ">>9.99":U COLUMN-LABEL "Qtde (m)" WIDTH 10
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 21.72 BY 8.25
         FONT 1
         TITLE "Pe‡as Reservadas" ROW-HEIGHT-CHARS .71.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br-etq-estoque AT ROW 1.38 COL 2.72
     br-etq-reservadas AT ROW 1.38 COL 65.14
     bt-sel AT ROW 1.75 COL 56.86
     bt-det AT ROW 3.17 COL 56.86
     bt-add AT ROW 5.25 COL 57
     bt-del AT ROW 6.75 COL 57
     fi-tot-reservado AT ROW 9.79 COL 71.43 COLON-ALIGNED
     fi-tot-estoque AT ROW 9.83 COL 41 COLON-ALIGNED
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: BrowserCadastro2
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
         HEIGHT             = 10
         WIDTH              = 87.86.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{utp/ut-glob.i}
{src/adm/method/browser.i}
{include/c-brows4.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit L-To-R                            */
/* BROWSE-TAB br-etq-estoque 1 F-Main */
/* BROWSE-TAB br-etq-reservadas br-etq-estoque F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON bt-add IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR BUTTON bt-del IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR BUTTON bt-det IN FRAME F-Main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR BUTTON bt-sel IN FRAME F-Main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-tot-estoque IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-reservado IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-etq-estoque
/* Query rebuild information for BROWSE br-etq-estoque
     _START_FREEFORM
RUN pi-soma-est.
OPEN QUERY {&SELF-NAME} FOR EACH tt-etq-estoque NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "espec.ob-etiqueta.situacao = 3 AND
espec.ob-etiqueta.it-codigo = tt-itens.it-codigo AND
espec.ob-etiqueta.cod-refer = tt-itens.cod-refer AND
espec.ob-etiqueta.nr-lote = tt-positivo.lote AND
espec.ob-etiqueta.corte-comerc = tt-positivo.corte-comerc"
     _Query            is OPENED
*/  /* BROWSE br-etq-estoque */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-etq-reservadas
/* Query rebuild information for BROWSE br-etq-reservadas
     _START_FREEFORM
RUN pi-soma-res.
OPEN QUERY {&SELF-NAME} FOR EACH tt-etq-reservadas WHERE
                                 tt-etq-reservadas.tp-acao <> 'Del'
                                 NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-etq-reservadas */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br-etq-estoque
&Scoped-define SELF-NAME br-etq-estoque
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-etq-estoque B-table-Win
ON VALUE-CHANGED OF br-etq-estoque IN FRAME F-Main /* Pe‡as Dispon¡veis */
DO:
   ASSIGN bt-add:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
   IF AVAIL tt-etq-estoque THEN
      ASSIGN bt-add:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-etq-reservadas
&Scoped-define SELF-NAME br-etq-reservadas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-etq-reservadas B-table-Win
ON ENTRY OF br-etq-reservadas IN FRAME F-Main /* Pe‡as Reservadas */
DO:
   APPLY 'value-changed' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-etq-reservadas B-table-Win
ON LEAVE OF br-etq-reservadas IN FRAME F-Main /* Pe‡as Reservadas */
DO:
   ASSIGN bt-det:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-etq-reservadas B-table-Win
ON MOUSE-SELECT-DBLCLICK OF br-etq-reservadas IN FRAME F-Main /* Pe‡as Reservadas */
DO:
   ASSIGN gr-ob-etiqueta = ROWID(tt-etq-reservadas).
   APPLY 'choose' TO bt-det.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-etq-reservadas B-table-Win
ON VALUE-CHANGED OF br-etq-reservadas IN FRAME F-Main /* Pe‡as Reservadas */
DO:
  ASSIGN bt-det:SENSITIVE IN FRAME {&FRAME-NAME} = NO
         bt-del:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
  IF AVAIL tt-etq-reservadas THEN DO.
     IF br-etq-reservadas:NUM-SELECTED-ROWS = 0 THEN
        br-etq-reservadas:SELECT-ROW(1).

     ASSIGN bt-det:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            bt-del:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-add B-table-Win
ON CHOOSE OF bt-add IN FRAME F-Main
DO:
   DO i-row = 1 TO br-etq-estoque:NUM-SELECTED-ROWS:
      IF br-etq-estoque:FETCH-SELECTED-ROW(i-row) THEN DO.
         FIND tt-etq-reservadas WHERE
              tt-etq-reservadas.cod-estabel  = tt-etq-estoque.cod-estabel AND
              tt-etq-reservadas.num-etiqueta = tt-etq-estoque.num-etiqueta NO-ERROR.
         IF NOT AVAIL tt-etq-reservadas THEN DO.
            CREATE tt-etq-reservadas.
            ASSIGN tt-etq-reservadas.cod-estabel = tt-etq-estoque.cod-estabel
                   tt-etq-reservadas.num-etiqueta = tt-etq-estoque.num-etiqueta
                   tt-etq-reservadas.quantidade = tt-etq-estoque.quantidade.
         END.
         ASSIGN tt-etq-reservadas.tp-acao = 'Inc'.

         DELETE tt-etq-estoque.
      END.
   END.

   {&OPEN-QUERY-br-etq-estoque}
   {&OPEN-QUERY-br-etq-reservadas}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-del
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-del B-table-Win
ON CHOOSE OF bt-del IN FRAME F-Main
DO:
   DO i-row = 1 TO br-etq-reservadas:NUM-SELECTED-ROWS.
      IF br-etq-reservadas:FETCH-SELECTED-ROW(i-row) THEN DO.
         ASSIGN tt-etq-reservadas.tp-acao = 'Del'.

         FIND ob-etiqueta WHERE
              ob-etiqueta.cod-estabel  = tt-etq-reservadas.cod-estabel AND
              ob-etiqueta.num-etiqueta = tt-etq-reservadas.num-etiqueta
              NO-LOCK NO-ERROR.

         IF ob-etiqueta.localizacao = '' OR 
            ob-etiqueta.localizacao BEGINS '6' OR
            ob-etiqueta.localizacao BEGINS '7'
            THEN NEXT.

         CREATE tt-etq-estoque.
         BUFFER-COPY ob-etiqueta TO tt-etq-estoque.

         FIND item-ext WHERE
              item-ext.it-codigo = ob-etiqueta.it-codigo NO-LOCK NO-ERROR.

         IF item-ext.indigo = YES THEN DO.
            FIND FIRST ordem-benefic WHERE 
                       ordem-benefic.nr-ob = ob-etiqueta.nr-ob NO-LOCK NO-ERROR.

            ASSIGN tt-etq-estoque.tipo-tear = UPPER(ordem-benefic.tipo-tear).
         END.
      END.
   END.

   {&OPEN-QUERY-br-etq-estoque}
   {&OPEN-QUERY-br-etq-reservadas}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-det
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-det B-table-Win
ON CHOOSE OF bt-det IN FRAME F-Main
DO:
   FIND ob-etiqueta WHERE
        ob-etiqueta.cod-estabel  = tt-etq-reservadas.cod-estabel AND
        ob-etiqueta.num-etiqueta = tt-etq-reservadas.num-etiqueta NO-LOCK NO-ERROR.
   ASSIGN gr-ob-etiqueta = ROWID(ob-etiqueta).

   RUN esp/essp0146.p. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-sel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-sel B-table-Win
ON CHOOSE OF bt-sel IN FRAME F-Main
DO:
    RUN esp/essp0162a.w (INPUT-OUTPUT c-it-codigo-ini,
                         INPUT-OUTPUT c-it-codigo-fin,
                         INPUT-OUTPUT c-cod-refer-ini,
                         INPUT-OUTPUT c-cod-refer-fin,
                         INPUT-OUTPUT c-corte-comerc-ini,
                         INPUT-OUTPUT c-corte-comerc-fin,
                         INPUT-OUTPUT c-cod-qualid-ini,
                         INPUT-OUTPUT c-cod-qualid-fin,
                         INPUT-OUTPUT c-cod-obsoleto-ini,
                         INPUT-OUTPUT c-cod-obsoleto-fin,
                         INPUT-OUTPUT c-nuance-ini,
                         INPUT-OUTPUT c-nuance-fin,
                         INPUT-OUTPUT c-tp-tecelagem,
                         INPUT-OUTPUT c-opc-artigo,
                         INPUT-OUTPUT l-lote-todos,
                         INPUT-OUTPUT l-lote-pp,
                         INPUT-OUTPUT l-lote-pd,
                         INPUT-OUTPUT l-lote-rp,
                         INPUT-OUTPUT l-lote-rd,
                         INPUT-OUTPUT l-lote-sc,
                         INPUT-OUTPUT l-lote-ca,
                         OUTPUT l-ok). 
    IF l-ok THEN DO.
       FOR EACH tt-etq-estoque.
           DELETE tt-etq-estoque.
       END.

      /* RUN pi-processa. */
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-etq-estoque
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
    WHEN 'it-codigo':U THEN DO:
       &Scope KEY-PHRASE ob-etiqueta.it-codigo eq key-value
       {&OPEN-QUERY-{&BROWSE-NAME}}
    END. /* it-codigo */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-habilita-bt B-table-Win 
PROCEDURE pi-habilita-bt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER p-acao AS LOG.
    IF p-acao THEN
       ENABLE {&list-4} WITH FRAME {&FRAME-NAME}.
    ELSE
       DISABLE {&list-4} WITH FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-soma-est B-table-Win 
PROCEDURE pi-soma-est :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN fi-tot-estoque = 0.

    ASSIGN bt-add:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

    FOR EACH tt-etq-estoque NO-LOCK.
        ASSIGN fi-tot-estoque =  fi-tot-estoque + tt-etq-estoque.quantidade.
    END.
    DISP fi-tot-estoque WITH FRAME {&FRAME-NAME}.

    IF fi-tot-estoque = 0 THEN
       ASSIGN bt-add:SENSITIVE IN FRAME {&FRAME-NAME} = NO.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-soma-res B-table-Win 
PROCEDURE pi-soma-res :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN fi-tot-reservado = 0.

    ASSIGN bt-del:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

    FOR EACH tt-etq-reservadas WHERE
             tt-etq-reservadas.tp-acao <> 'Del' NO-LOCK.
        ASSIGN fi-tot-reservado =  fi-tot-reservado + tt-etq-reservadas.quantidade.
    END.
    DISP fi-tot-reservado WITH FRAME {&FRAME-NAME}.

    IF fi-tot-reservado = 0 THEN
       ASSIGN bt-del:SENSITIVE IN FRAME {&FRAME-NAME} = NO.


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
  {src/adm/template/sndkycas.i "it-codigo" "ob-etiqueta" "it-codigo"}
  {src/adm/template/sndkycas.i "cod-refer" "ob-etiqueta" "cod-refer"}

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
  {src/adm/template/snd-list.i "tt-etq-reservadas"}
  {src/adm/template/snd-list.i "tt-etq-estoque"}

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

