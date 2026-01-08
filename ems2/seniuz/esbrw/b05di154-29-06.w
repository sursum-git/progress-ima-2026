&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r11 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          mgcad            PROGRESS
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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEF TEMP-TABLE tt-itens 
    FIELD it-codigo    LIKE ped-item.it-codigo
    FIELD qt-lida      LIKE ped-item.qt-pedida COLUMN-LABEL "Qt Lida" 
    FIELD qt-reservada LIKE ped-item.qt-pedida COLUMN-LABEL "Qt Reservada"
    FIELD qt-aberta    LIKE ped-item.qt-pedida COLUMN-LABEL "Qt Aberta"
    FIELD qt-regra     LIKE ped-item.qt-pedida COLUMN-LABEL "Qt Regra"
    FIELD qt-fila      LIKE ped-item.qt-pedida COLUMN-LABEL "Qt Fila"
    FIELD qt-crivada   LIKE ped-item.qt-pedida COLUMN-LABEL "Qt Crivada"
    INDEX indice1 IS PRIMARY it-codigo.

DEF TEMP-TABLE tt-itens-ped
    FIELD nr-pedcli    LIKE ped-item.nr-pedcli 
    FIELD nome-abrev   LIKE ped-item.nome-abrev
    FIELD nr-sequencia LIKE ped-item.nr-sequencia
    FIELD it-codigo    LIKE ped-item.it-codigo 
    FIELD cod-refer    LIKE ped-item.cod-refer 
    FIELD lote         LIKE ped-item-ext.lote 
    FIELD corte-comerc LIKE ped-item-ext.corte-comerc
    FIELD qt-pedida    LIKE ped-item.qt-pedida
    FIELD qt-reservada LIKE ped-item.qt-pedida
    FIELD qt-crivada   LIKE ped-item.qt-pedida
    FIELD qt-regra     LIKE ped-item.qt-pedida 
    FIELD qt-fila      LIKE ped-item.qt-pedida 
    FIELD dt-entrega   LIKE ped-venda.dt-entrega
    FIELD nome-transp  LIKE ped-venda.nome-transp
    FIELD exportacao   AS LOG INIT NO.

DEF TEMP-TABLE tt-etiquetas
    FIELD nr-pedcli    LIKE ped-item.nr-pedcli 
    FIELD nr-sequencia LIKE ped-item.nr-sequencia
    FIELD num-etiqueta LIKE ob-etiqueta.num-etiqueta
    INDEX indice1 IS PRIMARY nr-pedcli nr-sequencia num-etiqueta
    INDEX indice2 num-etiqueta.

DEF TEMP-TABLE wt-etiquetas
    FIELD reservada    AS   LOG INIT NO
    FIELD num-etiqueta LIKE ob-etiqueta.num-etiqueta
    FIELD localizacao  LIKE ob-etiqueta.localizacao
    FIELD quantidade   LIKE ob-etiqueta.quantidade
    FIELD tipo-tear    LIKE ordem-benefic.tipo-tear
    INDEX indice1 IS PRIMARY localizacao ASCENDING num-etiqueta DESCENDING
    INDEX indice2 quantidade.

DEF BUFFER b-wt-etiquetas FOR wt-etiquetas.

DEF VAR c-desc-item   LIKE ITEM.desc-item.

DEF VAR h-query       AS HANDLE.
DEF VAR h-acomp       AS HANDLE NO-UNDO.
DEF VAR da-dt-entrega AS DATE.
DEF VAR c-dia         AS CHAR.
DEF VAR c-lotes       AS CHAR.
DEF VAR c-desc-dentro AS CHAR. 

DEF VAR de-sulzer-tptear LIKE ob-etiqueta.quantidade.
DEF VAR de-outros-tptear LIKE ob-etiqueta.quantidade.
DEF VAR c-lst-tptear      AS   CHAR.

DEF NEW GLOBAL SHARED VAR h-essp0160 AS HANDLE NO-UNDO.

DEF VAR c-dt-limite        AS CHAR.      
DEF VAR c-it-codigo-ini    AS CHAR.      
DEF VAR c-it-codigo-fin    AS CHAR.      
DEF VAR c-cod-refer-ini    AS CHAR.      
DEF VAR c-cod-refer-fin    AS CHAR.      
DEF VAR c-nr-pedcli-ini    AS CHAR.      
DEF VAR c-nr-pedcli-fin    AS CHAR.      
DEF VAR i-nr-seq-ini       AS INT.       
DEF VAR i-nr-seq-fin       AS INT.       
DEF VAR c-cod-obsoleto-ini AS CHAR.      
DEF VAR c-cod-obsoleto-fin AS CHAR.      
DEF VAR de-perc-min        AS DEC.       
DEF VAR de-perc-max        AS DEC.       
DEF VAR c-tp-artigo        AS CHAR.
DEF VAR c-it-codigo     LIKE ped-item.it-codigo.

DEF VAR l-ok         AS LOG.
DEF VAR de-lida      AS DEC.
DEF VAR de-reservada AS DEC.
DEF VAR de-aberta    AS DEC.
DEF VAR de-regra     AS DEC.
DEF VAR de-fila      AS DEC.
DEF VAR de-crivada   AS DEC.
DEF VAR i-lin        AS INT.
DEF VAR i-pag        AS INT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-itens

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-itens

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br-itens                                      */
&Scoped-define FIELDS-IN-QUERY-br-itens tt-itens.it-codigo fn-desc-item() @ c-desc-item tt-itens.qt-lida tt-itens.qt-reservada tt-itens.qt-aberta tt-itens.qt-regra tt-itens.qt-fila tt-itens.qt-crivada   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-itens   
&Scoped-define SELF-NAME br-itens
&Scoped-define OPEN-QUERY-br-itens RUN pi-totais. OPEN QUERY {&SELF-NAME} FOR EACH tt-itens WHERE NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-itens tt-itens
&Scoped-define FIRST-TABLE-IN-QUERY-br-itens tt-itens


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-itens bt-reserva bt-estoque bt-regra ~
bt-fila bt-crivado bt-vapra bt-etiquetas bt-imprime 
&Scoped-Define DISPLAYED-OBJECTS fi-tot-lida fi-tot-reservada fi-tot-aberta ~
fi-tot-regra fi-tot-fila fi-tot-crivada 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-4 bt-reserva bt-estoque bt-regra bt-fila bt-crivado 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" B-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<FOREIGN-KEYS>
nr-ord-produ||y|mgmov.ped-item.nr-ord-produ
nr-programa||y|mgmov.ped-item.nr-programa
it-codigo||y|mgmov.ped-item.it-codigo
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "nr-ord-produ,nr-programa,it-codigo"':U).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-desc-item B-table-Win 
FUNCTION fn-desc-item RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-crivado 
     IMAGE-UP FILE "image/im-aval.bmp":U
     LABEL "" 
     SIZE 4 BY 1.29 TOOLTIP "Pedidos N∆o Reservados por CrÇdito"
     BGCOLOR 8 .

DEFINE BUTTON bt-estoque 
     IMAGE-UP FILE "image/im-chest.bmp":U
     LABEL "" 
     SIZE 4 BY 1.29 TOOLTIP "Etiquetas do Item N«O Reservadas"
     BGCOLOR 8 .

DEFINE BUTTON bt-etiquetas 
     IMAGE-UP FILE "image/im-local.bmp":U
     LABEL "" 
     SIZE 4 BY 1.29 TOOLTIP "Detalha Item Selecionado"
     BGCOLOR 8 .

DEFINE BUTTON bt-fila 
     IMAGE-UP FILE "image/im-aloca.bmp":U
     IMAGE-INSENSITIVE FILE "image/im-aloci.bmp":U
     LABEL "" 
     SIZE 4 BY 1.29 TOOLTIP "Pedidos N∆o Reservados por Fila"
     BGCOLOR 8 .

DEFINE BUTTON bt-imprime 
     IMAGE-UP FILE "image/im-pri.bmp":U
     LABEL "&Imprimir" 
     SIZE 4 BY 1.29 TOOLTIP "Relat¢rio de Itens Separados"
     BGCOLOR 8 .

DEFINE BUTTON bt-ok AUTO-GO 
     IMAGE-UP FILE "image/im-cq.bmp":U
     LABEL "OK" 
     SIZE 4 BY 1.25 TOOLTIP "Salva Reserva Autom†tica para os Itens".

DEFINE BUTTON bt-regra 
     IMAGE-UP FILE "image/im-bloq.bmp":U
     IMAGE-INSENSITIVE FILE "image/im-bloqi.bmp":U
     LABEL "" 
     SIZE 4 BY 1.29 TOOLTIP "Pedidos N∆o Reservados por Regra"
     BGCOLOR 8 .

DEFINE BUTTON bt-reserva 
     IMAGE-UP FILE "image/imt-res.bmp":U
     LABEL "" 
     SIZE 4 BY 1.29 TOOLTIP "Pedidos Reservados do Item"
     BGCOLOR 8 .

DEFINE BUTTON bt-vapra 
     IMAGE-UP FILE "image/im-vapra.bmp":U
     LABEL "" 
     SIZE 4 BY 1.29 TOOLTIP "V† para o Item"
     BGCOLOR 8 .

DEFINE VARIABLE fi-tot-aberta AS DECIMAL FORMAT "z,zzz,zz9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi-tot-crivada AS DECIMAL FORMAT "z,zzz,zz9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-tot-fila AS DECIMAL FORMAT "z,zzz,zz9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-tot-lida AS DECIMAL FORMAT "z,zzz,zz9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi-tot-regra AS DECIMAL FORMAT "-z,zzz,zz9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-tot-reservada AS DECIMAL FORMAT "z,zzz,zz9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10.57 BY .88 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-itens FOR 
      tt-itens SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-itens
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-itens B-table-Win _FREEFORM
  QUERY br-itens NO-LOCK DISPLAY
      tt-itens.it-codigo FORMAT "x(8)":U                    WIDTH 10
      fn-desc-item() @ c-desc-item COLUMN-LABEL "Descriá∆o" WIDTH 32
      tt-itens.qt-lida             FORMAT ">,>>>,>>9.99"    WIDTH 09
      tt-itens.qt-reservada        FORMAT ">,>>>,>>9.99"    WIDTH 10
      tt-itens.qt-aberta           FORMAT ">,>>>,>>9.99"    WIDTH 09
      tt-itens.qt-regra            FORMAT ">,>>>,>>9.99"    WIDTH 09
      tt-itens.qt-fila             FORMAT ">,>>>,>>9.99"    WIDTH 09
      tt-itens.qt-crivada          FORMAT ">,>>>,>>9.99"    WIDTH 09
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 104.43 BY 16
         FONT 1 ROW-HEIGHT-CHARS .63.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br-itens AT ROW 1 COL 1
     bt-reserva AT ROW 3.71 COL 105.86
     bt-estoque AT ROW 5.08 COL 105.86
     bt-regra AT ROW 6.42 COL 105.86
     bt-fila AT ROW 7.75 COL 105.86
     bt-crivado AT ROW 9.13 COL 105.86
     bt-vapra AT ROW 11.25 COL 105.86
     bt-etiquetas AT ROW 12.54 COL 105.86
     bt-imprime AT ROW 13.83 COL 105.86
     bt-ok AT ROW 15.75 COL 105.86
     fi-tot-lida AT ROW 17.13 COL 42.29 COLON-ALIGNED NO-LABEL
     fi-tot-reservada AT ROW 17.13 COL 52 COLON-ALIGNED NO-LABEL
     fi-tot-aberta AT ROW 17.13 COL 62.57 COLON-ALIGNED NO-LABEL
     fi-tot-regra AT ROW 17.13 COL 72.29 COLON-ALIGNED NO-LABEL
     fi-tot-fila AT ROW 17.13 COL 81.86 COLON-ALIGNED NO-LABEL
     fi-tot-crivada AT ROW 17.13 COL 91.43 COLON-ALIGNED NO-LABEL
     "Totais:" VIEW-AS TEXT
          SIZE 8.86 BY .75 AT ROW 17.21 COL 33.86
          FONT 0
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 FONT 1.


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
         HEIGHT             = 17.17
         WIDTH              = 109.
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
   NOT-VISIBLE Size-to-Fit                                              */
/* BROWSE-TAB br-itens 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON bt-crivado IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR BUTTON bt-estoque IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR BUTTON bt-fila IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR BUTTON bt-ok IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-regra IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR BUTTON bt-reserva IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR FILL-IN fi-tot-aberta IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-crivada IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-fila IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-lida IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-regra IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-reservada IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-itens
/* Query rebuild information for BROWSE br-itens
     _START_FREEFORM
RUN pi-totais.
OPEN QUERY {&SELF-NAME} FOR EACH tt-itens WHERE NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Query            is NOT OPENED
*/  /* BROWSE br-itens */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br-itens
&Scoped-define SELF-NAME br-itens
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-itens B-table-Win
ON VALUE-CHANGED OF br-itens IN FRAME F-Main
DO:
   DISABLE {&list-4} WITH FRAME {&FRAME-NAME}.
   IF AVAIL tt-itens THEN DO.
      IF tt-itens.qt-reservada > 0 THEN
         ASSIGN bt-reserva:SENSITIVE = YES.
    
      IF tt-itens.qt-crivada > 0 THEN
         ASSIGN bt-crivado:SENSITIVE = YES.

      IF tt-itens.qt-regra > 0 THEN
         ASSIGN bt-regra:SENSITIVE = YES.

      IF tt-itens.qt-fila > 0 THEN
         ASSIGN bt-fila:SENSITIVE = YES.

   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-crivado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-crivado B-table-Win
ON CHOOSE OF bt-crivado IN FRAME F-Main
DO:
  RUN esp/essp0160b.p (INPUT TABLE tt-itens-ped,
                       INPUT tt-itens.it-codigo).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-etiquetas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-etiquetas B-table-Win
ON CHOOSE OF bt-etiquetas IN FRAME F-Main
DO:
  RUN pi-imprime (INPUT NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-fila
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-fila B-table-Win
ON CHOOSE OF bt-fila IN FRAME F-Main
DO:
  RUN esp/essp0160e.p (INPUT TABLE tt-itens-ped,
                       INPUT tt-itens.it-codigo).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-imprime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-imprime B-table-Win
ON CHOOSE OF bt-imprime IN FRAME F-Main /* Imprimir */
DO:
  RUN pi-imprime (INPUT YES).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok B-table-Win
ON CHOOSE OF bt-ok IN FRAME F-Main /* OK */
DO:
    FOR EACH tt-itens-ped WHERE
             tt-itens-ped.qt-reservada > 0 NO-LOCK.

        FIND corte-comerc WHERE
             corte-comerc.codigo = tt-itens-ped.corte-comerc 
             NO-LOCK NO-ERROR.

        {esinc/i-dsrb.i corte-comerc.tp-embalag corte-comerc.tp-embalag c-desc-dentro}.
        ASSIGN c-desc-dentro = UPPER(c-desc-dentro) + 'S'.
        
        CREATE ped-item-res.
        ASSIGN ped-item-res.nome-abrev   = tt-itens-ped.nome-abrev
               ped-item-res.nr-pedcli    = tt-itens-ped.nr-pedcli
               ped-item-res.nr-sequencia = tt-itens-ped.nr-sequencia
               ped-item-res.it-codigo    = tt-itens-ped.it-codigo
               ped-item-res.cod-refer    = tt-itens-ped.cod-refer
               ped-item-res.nome-transp  = tt-itens-ped.nome-transp
               ped-item-res.sigla-emb    = tt-itens-ped.lote
               ped-item-res.desc-dentro  = c-desc-dentro
               ped-item-res.qt-pedida    = tt-itens-ped.qt-reservada
               ped-item-res.dt-trans     = TODAY
               ped-item-res.hr-trans     = STRING(TIME,"HH:MM:SS")
               ped-item-res.lote         = tt-itens-ped.lote + tt-itens-ped.cod-refer.

        FOR EACH tt-etiquetas WHERE
                 tt-etiquetas.nr-pedcli = tt-itens-ped.nr-pedcli AND
                 tt-etiquetas.nr-sequencia = tt-itens-ped.nr-sequencia NO-LOCK.

            FIND ob-etiqueta WHERE
                 ob-etiqueta.num-etiqueta = tt-etiquetas.num-etiqueta 
                 SHARE-LOCK NO-ERROR.

            FIND ped-item-rom WHERE
                 ped-item-rom.num-etiqueta = ob-etiqueta.num-etiqueta NO-LOCK NO-ERROR.

            IF NOT AVAIL ped-item-rom THEN DO.
               CREATE ped-item-rom.
               ASSIGN ped-item-rom.nome-abrev = ped-item-res.nome-abrev
                      ped-item-rom.nr-pedcli = ped-item-res.nr-pedcli
                      ped-item-rom.nr-sequencia = ped-item-res.nr-sequencia
                      ped-item-rom.num-etiqueta = ob-etiqueta.num-etiqueta
                      ped-item-rom.nr-ob = ob-etiqueta.nr-ob
                      ped-item-rom.nr-seq-etq = ob-etiqueta.nr-sequencia
                      ped-item-rom.quantidade = ob-etiqueta.quantidade.
            END.
            ASSIGN ob-etiqueta.situacao = 4.
        END.

        IF tt-itens-ped.qt-pedida <> tt-itens-ped.qt-reservada THEN
           RUN esapi/altera-peditem.p (INPUT tt-itens-ped.nr-pedcli,
                                       INPUT tt-itens-ped.nr-sequencia,
                                       INPUT tt-itens-ped.qt-reservada).
    END.

    ASSIGN bt-ok:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-reserva
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-reserva B-table-Win
ON CHOOSE OF bt-reserva IN FRAME F-Main
DO:
     RUN esp/essp0160a.p (INPUT TABLE tt-itens-ped,
                          INPUT tt-itens.it-codigo,
                          INPUT TABLE tt-etiquetas).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-vapra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-vapra B-table-Win
ON CHOOSE OF bt-vapra IN FRAME F-Main
DO:
  RUN esdlg/d01essp0160.w (OUTPUT c-it-codigo).
  IF c-it-codigo <> "" THEN DO:
     FIND FIRST tt-itens WHERE
                tt-itens.it-codigo = c-it-codigo NO-LOCK NO-ERROR.
     IF AVAIL tt-itens THEN
        h-query:REPOSITION-TO-ROWID(ROWID(tt-itens)) NO-ERROR. 
     ELSE
        MESSAGE "Item n∆o est† contido na seleá∆o!"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
br-itens:NUM-LOCKED-COLUMNS = 2.

ASSIGN h-query = br-itens:QUERY.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imp-cabec B-table-Win 
PROCEDURE pi-imp-cabec :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    PUT "TEAR TEXTIL INDUSTRIA E COMERCIO LTDA."  AT   1
        "DATA: "                                  AT  66
        STRING(TODAY,"99/99/9999") FORMAT "X(10)" AT  72
        "HORA: "                                  AT  88
        STRING(TIME,"hh:mm:ss")                   AT  94
        "PAGINA:"                                 AT 122
        i-pag FORMAT "999"                        AT 130
        SKIP(1).
        
    PUT "RELATORIO DA SEPARACAO AUTOMATICA" AT 51 SKIP(1).

    PUT "ITEM   DESCRICAO                             LIDA       RESERVADA           ABERTA           REGRA            FILA         CRIVADA" AT 1.
    PUT "------ ----------------------------- ------------    ------------     ------------    ------------    ------------    ------------" AT 1.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imprime B-table-Win 
PROCEDURE pi-imprime :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

 DEF INPUT PARAMETER p-imprime AS LOG.

 IF p-imprime = YES  THEN DO:
    SYSTEM-DIALOG PRINTER-SETUP UPDATE l-ok.
    OUTPUT TO PRINTER CONVERT TARGET "ISO8859-1" PAGED PAGE-SIZE 63. 
    /* OUTPUT TO PRINTER PAGED PAGE-SIZE 62. */
    PUT "~033(s17H".
 END.
 ELSE DO:
    OUTPUT TO "C:\TEMP\ESSP0160.LST".
    ASSIGN l-ok = YES.
 END.
    
 IF l-ok THEN DO:

    RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
    {utp/ut-liter.i Impress∆o *}
    RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).
       
    ASSIGN i-lin        = 99
           i-pag        =  1
           de-lida      =  0
           de-reservada =  0
           de-aberta    =  0
           de-regra     =  0
           de-fila      =  0
           de-crivada   =  0.
    
    FOR EACH tt-itens NO-LOCK BY tt-itens.it-codigo.
   
        RUN pi-acompanhar IN h-acomp (INPUT "Item: " + tt-itens.it-codigo).



        IF i-lin > 63 THEN DO:
           RUN pi-imp-cabec.
           ASSIGN i-lin = 7
                  i-pag = i-pag + 1.
        END.

        FIND ITEM WHERE
             ITEM.it-codigo = tt-itens.it-codigo NO-LOCK NO-ERROR.

        PUT tt-itens.it-codigo     FORMAT "x(6)"            AT   1
            ITEM.desc-item         FORMAT "x(29)"           AT   8
            tt-itens.qt-lida       FORMAT ">,>>>,>>9.99"    AT  38
            tt-itens.qt-reservada  FORMAT ">,>>>,>>9.99"    AT  54
            tt-itens.qt-aberta     FORMAT ">,>>>,>>9.99"    AT  71
            tt-itens.qt-regra      FORMAT ">,>>>,>>9.99"    AT  87
            tt-itens.qt-fila       FORMAT ">,>>>,>>9.99"    AT 103
            tt-itens.qt-crivada    FORMAT ">,>>>,>>9.99"    AT 119.
    
        ASSIGN i-lin        = i-lin        + 1
               de-lida      = de-lida      + tt-itens.qt-lida
               de-reservada = de-reservada + tt-itens.qt-reservada
               de-aberta    = de-aberta    + tt-itens.qt-aberta
               de-regra     = de-regra     + tt-itens.qt-regra
               de-fila      = de-fila      + tt-itens.qt-fila
               de-crivada   = de-crivada   + tt-itens.qt-crivada.
        FOR EACH tt-itens-ped WHERE tt-itens-ped.it-codigo = tt-itens.it-codigo NO-LOCK.
            IF i-lin > 63 THEN DO:
               RUN pi-imp-cabec.
               ASSIGN i-lin = 7
                      i-pag = i-pag + 1.
            END.
            PUT tt-itens-ped.nr-pedcli  FORMAT "x(6)"               AT  10
                tt-itens-ped.nome-abrev FORMAT "x(11)"              AT  17
                tt-itens-ped.cod-refer                              AT  31
                tt-itens-ped.qt-pedida     FORMAT ">,>>>,>>9.99"    AT  40
                tt-itens-ped.qt-reservada  FORMAT ">,>>>,>>9.99"    AT  56
                tt-itens-ped.qt-pedida - tt-itens-ped.qt-reservada  FORMAT "->,>>>,>>9.99"    AT  72
                tt-itens-ped.qt-regra      FORMAT ">,>>>,>>9.99"    AT  89
                tt-itens-ped.qt-crivada    FORMAT ">,>>>,>>9.99"    AT 121.
            ASSIGN i-lin = i-lin + 1.
            FOR EACH tt-etiquetas WHERE tt-etiquetas.nr-pedcli    = tt-itens-ped.nr-pedcli 
                                    AND tt-etiquetas.nr-sequencia = tt-itens-ped.nr-sequencia NO-LOCK. 
                FIND ob-etiqueta WHERE
                     ob-etiqueta.num-etiqueta = tt-etiquetas.num-etiqueta NO-LOCK NO-ERROR.
                IF NOT AVAIL ob-etiqueta THEN NEXT.
                FIND ordem-benefic WHERE
                     ordem-benefic.nr-ob = ob-etiqueta.nr-ob  NO-LOCK NO-ERROR.
                IF NOT AVAIL ordem-benefic THEN NEXT.
                IF i-lin > 63 THEN DO:
                   RUN pi-imp-cabec.
                   ASSIGN i-lin = 7
                          i-pag = i-pag + 1.
                END.
                PUT ob-etiqueta.num-etiqueta  FORMAT "999999999"        AT  12
                    ordem-benefic.tipo-tear                             AT  22
                    ob-etiqueta.localizacao FORMAT "XXX/XXX"            AT  30
                    ob-etiqueta.quantidade  FORMAT ">>9.99"             AT  64.
                ASSIGN i-lin = i-lin + 1.
            END.
        END.
        PUT "" AT 1.
        ASSIGN i-lin = i-lin + 1.
    END.
    IF de-lida      <> 0 OR  de-reservada <> 0 OR
       de-aberta    <> 0 OR  de-regra     <> 0 OR
       de-fila      <> 0 OR  de-crivada   <> 0 THEN DO:
       PUT "------------    ------------     ------------    ------------    ------------    ------------"   AT 38.
       PUT "T O T A I S  . . . ."  AT 08
           de-lida       FORMAT ">,>>>,>>9.99" AT  38
           de-reservada  FORMAT ">,>>>,>>9.99" AT  54 
           de-aberta     FORMAT ">,>>>,>>9.99" AT  71 
           de-regra      FORMAT ">,>>>,>>9.99" AT  87 
           de-fila       FORMAT ">,>>>,>>9.99" AT 103
           de-crivada    FORMAT ">,>>>,>>9.99" AT 119.
    END.
    OUTPUT CLOSE.
    RUN pi-finalizar in h-acomp.
 END.
    
 IF p-imprime = NO THEN DO. /* CONSULTA */
    RUN utp/ut-utils.p PERSISTENT SET h-prog.
    RUN EXECUTE IN h-prog(INPUT "notepad.exe", 
                          INPUT "C:\TEMP\ESSP0160.LST").
    DELETE PROCEDURE h-prog.
 END.





END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-limpa B-table-Win 
PROCEDURE pi-limpa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     FOR EACH tt-itens.
         DELETE tt-itens.
     END.
     RUN adm-open-query-cases.
     ASSIGN bt-ok:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-processa B-table-Win 
PROCEDURE pi-processa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER p-dt-limite        AS CHAR.
    DEF INPUT PARAMETER p-it-codigo-ini    AS CHAR.                              
    DEF INPUT PARAMETER p-it-codigo-fin    AS CHAR.                              
    DEF INPUT PARAMETER p-cod-refer-ini    AS CHAR.                              
    DEF INPUT PARAMETER p-cod-refer-fin    AS CHAR.
    DEF INPUT PARAMETER p-nr-pedcli-ini    AS CHAR.                              
    DEF INPUT PARAMETER p-nr-pedcli-fin    AS CHAR.
    DEF INPUT PARAMETER p-nr-seq-ini       AS INT.                              
    DEF INPUT PARAMETER p-nr-seq-fin       AS INT.
    DEF INPUT PARAMETER p-cod-obsoleto-ini AS CHAR.                              
    DEF INPUT PARAMETER p-cod-obsoleto-fin AS CHAR.
    DEF INPUT PARAMETER p-perc-min         AS DEC.
    DEF INPUT PARAMETER p-perc-max         AS DEC.
    DEF INPUT PARAMETER p-tp-artigo        AS CHAR.

    ASSIGN c-dt-limite        = p-dt-limite       
           c-it-codigo-ini    = p-it-codigo-ini   
           c-it-codigo-fin    = p-it-codigo-fin   
           c-cod-refer-ini    = p-cod-refer-ini   
           c-cod-refer-fin    = p-cod-refer-fin   
           c-nr-pedcli-ini    = p-nr-pedcli-ini   
           c-nr-pedcli-fin    = p-nr-pedcli-fin   
           i-nr-seq-ini       = p-nr-seq-ini      
           i-nr-seq-fin       = p-nr-seq-fin      
           c-cod-obsoleto-ini = p-cod-obsoleto-ini 
           c-cod-obsoleto-fin = p-cod-obsoleto-fin 
           de-perc-min        = p-perc-min        
           de-perc-max        = p-perc-max        
           c-tp-artigo        = p-tp-artigo.      

    EMPTY TEMP-TABLE tt-itens.
    EMPTY TEMP-TABLE tt-itens-ped.
    EMPTY TEMP-TABLE tt-etiquetas.

    RUN utp/ut-acomp.p PERSISTENT SET h-acomp.

    RUN esapi/ret-udm.p (INPUT c-dt-limite, OUTPUT c-dia).
    ASSIGN da-dt-entrega = DATE(c-dia + SUBSTR(c-dt-limite,1,2) + SUBSTR(c-dt-limite,3,4)).

    ASSIGN c-lotes = 'RP,RD'.

    {utp/ut-liter.i Calculando_Carteira *}
    RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

    IF c-nr-pedcli-ini <> '' THEN
       RUN pi-separa-pedidos.
    ELSE
       RUN pi-separa-itens.

    {utp/ut-liter.i Separando_Etiquetas *}
    RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

    /* Separa Etiquetas para cada Item dos Pedidos */
    FOR EACH tt-itens-ped WHERE 
             tt-itens-ped.qt-crivada = 0 EXCLUSIVE-LOCK
             BY tt-itens-ped.dt-entrega
             BY tt-itens-ped.nr-pedcli
             BY tt-itens-ped.nr-sequencia.

        ASSIGN de-sulzer-tptear = 0
               de-outros-tptear = 0.

        EMPTY TEMP-TABLE wt-etiquetas.
        FOR EACH ob-etiqueta WHERE
                 ob-etiqueta.it-codigo = tt-itens-ped.it-codigo AND
                 ob-etiqueta.cod-refer = tt-itens-ped.cod-refer AND
                 ob-etiqueta.nr-lote   = tt-itens-ped.lote AND
                 ob-etiqueta.situacao  = 3 AND
                 ob-etiqueta.corte-comerc = tt-itens-ped.corte-comerc NO-LOCK 
                 BY ob-etiqueta.corte-comerc.
    
            RUN pi-acompanhar IN h-acomp (INPUT "Etiqueta: " + STRING(ob-etiqueta.num-etiqueta,"999999999")).
                 
            IF ob-etiqueta.localizacao = '' OR 
               ob-etiqueta.localizacao BEGINS '6' OR
               ob-etiqueta.localizacao BEGINS '7'
               THEN NEXT.

            FIND item-ext WHERE
                 item-ext.it-codigo = ob-etiqueta.it-codigo NO-LOCK NO-ERROR.
            /*
            IF (item-ext.indigo = YES OR
                (ob-etiqueta.it-codigo = '501871' AND ob-etiqueta.cod-refer = '0101010') OR
                (ob-etiqueta.it-codigo = '501823' AND ob-etiqueta.cod-refer = '2101010')) AND
                ob-etiqueta.nuance = '' THEN NEXT.
             */

            IF item-ext.indigo = YES AND  ob-etiqueta.nuance = '' THEN NEXT.

            IF tt-itens-ped.exportacao = YES AND
               ob-etiqueta.cod-qualid <> 'A' THEN NEXT.
                   
            FIND FIRST tt-etiquetas WHERE
                       tt-etiquetas.num-etiqueta = ob-etiqueta.num-etiqueta 
                       USE-INDEX indice2 NO-ERROR.
            IF AVAIL tt-etiquetas THEN NEXT.

            CREATE wt-etiquetas.
            ASSIGN wt-etiquetas.num-etiqueta = ob-etiqueta.num-etiqueta
                   wt-etiquetas.quantidade = ob-etiqueta.quantidade
                   wt-etiquetas.localiz = ob-etiqueta.localiz.

            IF item-ext.indigo = YES THEN DO.
               FIND FIRST ordem-benefic WHERE 
                          ordem-benefic.nr-ob = ob-etiqueta.nr-ob NO-LOCK NO-ERROR.
    
               ASSIGN wt-etiquetas.tipo-tear = ordem-benefic.tipo-tear.

               IF ordem-benefic.tipo-tear = 'SULZER' THEN
                  ASSIGN de-sulzer-tptear = de-sulzer-tptear + wt-etiquetas.quantidade.
               ELSE
                  ASSIGN de-outros-tptear = de-outros-tptear + wt-etiquetas.quantidade.
            END.
        END.

        ASSIGN c-lst-tptear = 'NISSAN,PICANOL'.
        IF de-sulzer-tptear > de-outros-tptear THEN
           ASSIGN c-lst-tptear = 'SULZER'.

        FOR EACH wt-etiquetas NO-LOCK BY wt-etiquetas.localiz
                                      BY wt-etiquetas.num-etiqueta DESCENDING.
                 
            IF wt-etiquetas.reservada = NO THEN NEXT.

            IF wt-etiquetas.tipo-tear <> '' AND
               LOOKUP(wt-etiquetas.tipo-tear,c-lst-tptear) = 0 THEN NEXT.
                   
            IF (tt-itens-ped.qt-reservada + wt-etiquetas.quantidade) > tt-itens-ped.qt-pedida + (tt-itens-ped.qt-pedida * (de-perc-max / 100)) THEN DO.
               IF tt-itens-ped.qt-reservada > tt-itens-ped.qt-pedida - (tt-itens-ped.qt-pedida * (de-perc-min / 100)) THEN
                  LEAVE.

               FIND FIRST b-wt-etiquetas WHERE
                          b-wt-etiquetas.reservada = YES USE-INDEX indice2 NO-ERROR.

               IF NOT AVAIL b-wt-etiquetas THEN NEXT.

               ASSIGN b-wt-etiquetas.reservada = NO
                      tt-itens-ped.qt-reservada = tt-itens-ped.qt-reservada - b-wt-etiquetas.quantidade.

               FIND tt-etiquetas WHERE
                    tt-etiquetas.num-etiqueta = b-wt-etiquetas.num-etiqueta NO-ERROR.
               DELETE tt-etiquetas.
            END.

            ASSIGN wt-etiquetas.reservada = YES
                   tt-itens-ped.qt-reservada = tt-itens-ped.qt-reservada + wt-etiquetas.quantidade.
    
            FIND tt-etiquetas WHERE
                 tt-etiquetas.nr-pedcli = tt-itens-ped.nr-pedcli AND
                 tt-etiquetas.nr-sequencia = tt-itens-ped.nr-sequencia AND
                 tt-etiquetas.num-etiqueta = wt-etiquetas.num-etiqueta
                 NO-ERROR.
            IF NOT AVAIL tt-etiquetas THEN DO.
               CREATE tt-etiquetas.
               ASSIGN tt-etiquetas.nr-pedcli = tt-itens-ped.nr-pedcli 
                      tt-etiquetas.nr-sequencia = tt-itens-ped.nr-sequencia 
                      tt-etiquetas.num-etiqueta = wt-etiquetas.num-etiqueta.
            END.
            IF tt-itens-ped.qt-reservada >= tt-itens-ped.qt-pedida THEN LEAVE.
        END.
    END.

    FOR EACH tt-itens-ped.
        IF tt-itens-ped.qt-reservada < tt-itens-ped.qt-pedida - (tt-itens-ped.qt-pedida * (de-perc-min / 100)) THEN
           ASSIGN tt-itens-ped.qt-regra = tt-itens-ped.qt-pedida
                  tt-itens-ped.qt-reservada = 0.
    END.
      
    FOR EACH tt-itens-ped.
        FIND tt-itens WHERE
             tt-itens.it-codigo = tt-itens-ped.it-codigo
             NO-LOCK NO-ERROR.

        IF NOT AVAIL tt-itens THEN DO.
           CREATE tt-itens.
           ASSIGN tt-itens.it-codigo = tt-itens-ped.it-codigo.
        END.
        ASSIGN tt-itens.qt-lida = tt-itens.qt-lida + tt-itens-ped.qt-pedida
               tt-itens.qt-reservada = tt-itens.qt-reservada + tt-itens-ped.qt-reservada
               tt-itens.qt-regra = tt-itens.qt-regra + tt-itens-ped.qt-regra
               tt-itens.qt-crivada = tt-itens.qt-crivada + tt-itens-ped.qt-crivada
               tt-itens.qt-aberta = tt-itens.qt-lida - tt-itens.qt-reservada.

        IF tt-itens.qt-aberta < 0 THEN
           ASSIGN tt-itens.qt-aberta = 0.

    END.
    ASSIGN bt-ok:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
    IF CAN-FIND(FIRST tt-itens WHERE
                      tt-itens.qt-reservada > 0) THEN
       ASSIGN bt-ok:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

    RUN pi-finalizar in h-acomp.
    RUN adm-open-query-cases.

    APPLY 'VALUE-CHANGED' TO br-itens.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-separa-itens B-table-Win 
PROCEDURE pi-separa-itens :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH ped-item WHERE        
             ped-item.it-codigo >= c-it-codigo-ini AND
             ped-item.it-codigo <= c-it-codigo-fin AND
             ped-item.cod-refer >= c-cod-refer-ini AND 
             ped-item.cod-refer <= c-cod-refer-fin AND
             ped-item.nr-sequencia >= i-nr-seq-ini AND
             ped-item.nr-sequencia <= i-nr-seq-fin  AND
             (ped-item.cod-sit-item = 1 OR
              ped-item.cod-sit-item = 2 OR
              ped-item.cod-sit-item = 5)  NO-LOCK, 
        FIRST ped-venda OF ped-item WHERE
              ped-venda.nr-pedcli >= c-nr-pedcli-ini AND
              ped-venda.nr-pedcli <= c-nr-pedcli-fin AND
              ped-venda.dt-entrega <= da-dt-entrega NO-LOCK, 
        FIRST ped-item-ext OF ped-item 
              WHERE INDEX(c-lotes,SUBSTR(ped-item-ext.lote,1,2)) <> 0 NO-LOCK.

        RUN pi-acompanhar IN h-acomp (INPUT "Pedido: " + ped-item.nr-pedcli +
                                            "  Item: " + ped-item.it-codigo +
                                            "   Ref: " + ped-item.cod-refer).
        
        FIND ped-item-res WHERE
             ped-item-res.nome-abrev   = ped-item.nome-abrev   AND
             ped-item-res.nr-pedcli    = ped-item.nr-pedcli    AND
             ped-item-res.nr-sequencia = ped-item.nr-sequencia NO-LOCK NO-ERROR.
        IF AVAIL ped-item-res THEN NEXT.

        FIND item-ext WHERE
             item-ext.it-codigo = ped-item.it-codigo NO-LOCK NO-ERROR.
        IF c-tp-artigo <> 'A' THEN 
           IF (item-ext.indigo = YES AND c-tp-artigo <> "I") OR
              (item-ext.indigo = NO  AND c-tp-artigo <> "O") THEN NEXT.

        FIND ref-item-ext WHERE
             ref-item-ext.it-codigo = ped-item.it-codigo AND
             ref-item-ext.cod-refer = ped-item.cod-refer NO-LOCK NO-ERROR.
        IF ref-item-ext.cod-obsoleto < c-cod-obsoleto-ini OR
           ref-item-ext.cod-obsoleto > c-cod-obsoleto-fin THEN NEXT.
                          
        FIND emitente WHERE
             emitente.cod-emitente = ped-venda.cod-emitente NO-LOCK NO-ERROR.

        FIND tt-itens-ped WHERE
             tt-itens-ped.nr-pedcli = ped-item.nr-pedcli AND
             tt-itens-ped.nr-sequencia = ped-item.nr-sequencia 
             NO-LOCK NO-ERROR.

        IF NOT AVAIL tt-itens-ped THEN DO.
           CREATE tt-itens-ped.
           ASSIGN tt-itens-ped.nr-pedcli = ped-item.nr-pedcli 
                  tt-itens-ped.nome-abrev = ped-item.nome-abrev
                  tt-itens-ped.nr-sequencia = ped-item.nr-sequencia 
                  tt-itens-ped.it-codigo = ped-item.it-codigo 
                  tt-itens-ped.cod-refer = ped-item.cod-refer 
                  tt-itens-ped.lote = SUBSTR(ped-item-ext.lote,1,2)
                  tt-itens-ped.corte-comerc = ped-item-ext.corte-comerc
                  tt-itens-ped.dt-entrega = ped-venda.dt-entrega
                  tt-itens-ped.nome-transp = ped-venda.nome-transp
                  tt-itens-ped.exportacao = IF AVAIL emitente AND emitente.pais <> "brasil"
                                            THEN YES ELSE NO.
        END.
        ASSIGN tt-itens-ped.qt-pedida = tt-itens-ped.qt-pedida + ped-item.qt-pedida.

        FIND cond-pagto OF ped-venda NO-LOCK NO-ERROR.
        IF AVAIL cond-pagto THEN DO.
           IF (cond-pagto.cod-vencto < 2 OR cond-pagto.cod-vencto > 3) AND  
              (ped-venda.cod-sit-aval < 2 OR ped-venda.cod-sit-aval > 3) THEN 
              ASSIGN tt-itens-ped.qt-crivada = tt-itens-ped.qt-crivada + ped-item.qt-pedida.
        END.
        ELSE
            IF ped-venda.cod-sit-aval < 2 OR ped-venda.cod-sit-aval > 3 THEN
               ASSIGN tt-itens-ped.qt-crivada = tt-itens-ped.qt-crivada + ped-item.qt-pedida.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-separa-pedidos B-table-Win 
PROCEDURE pi-separa-pedidos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FOR EACH ped-venda WHERE
            ped-venda.nr-pedcli >= c-nr-pedcli-ini AND
            ped-venda.nr-pedcli <= c-nr-pedcli-fin AND
            ped-venda.dt-entrega <= da-dt-entrega NO-LOCK,
       EACH ped-item OF ped-venda WHERE        
            ped-item.it-codigo >= c-it-codigo-ini AND
            ped-item.it-codigo <= c-it-codigo-fin AND
            ped-item.cod-refer >= c-cod-refer-ini AND 
            ped-item.cod-refer <= c-cod-refer-fin AND
            ped-item.nr-sequencia >= i-nr-seq-ini AND
            ped-item.nr-sequencia <= i-nr-seq-fin AND
            (ped-item.cod-sit-item = 1 OR
             ped-item.cod-sit-item = 2 OR
             ped-item.cod-sit-item = 5) NO-LOCK,
       FIRST ped-item-ext OF ped-item 
             WHERE INDEX(c-lotes,SUBSTR(ped-item-ext.lote,1,2)) <> 0 NO-LOCK.

        RUN pi-acompanhar IN h-acomp (INPUT "Pedido: " + ped-item.nr-pedcli +
                                            "  Item: " + ped-item.it-codigo +
                                            "   Ref: " + ped-item.cod-refer).

        FIND ped-item-res WHERE
             ped-item-res.nome-abrev   = ped-item.nome-abrev   AND
             ped-item-res.nr-pedcli    = ped-item.nr-pedcli    AND
             ped-item-res.nr-sequencia = ped-item.nr-sequencia NO-LOCK NO-ERROR.
        IF AVAIL ped-item-res THEN NEXT.

        FIND item-ext WHERE
             item-ext.it-codigo = ped-item.it-codigo NO-LOCK NO-ERROR.
        IF c-tp-artigo <> 'A' THEN 
           IF (item-ext.indigo = YES AND c-tp-artigo <> "I") OR
              (item-ext.indigo = NO  AND c-tp-artigo <> "O") THEN NEXT.

        FIND ref-item-ext WHERE
             ref-item-ext.it-codigo = ped-item.it-codigo AND
             ref-item-ext.cod-refer = ped-item.cod-refer NO-LOCK NO-ERROR.
        IF ref-item-ext.cod-obsoleto < c-cod-obsoleto-ini OR
           ref-item-ext.cod-obsoleto > c-cod-obsoleto-fin THEN NEXT.
                          
        FIND emitente WHERE
             emitente.cod-emitente = ped-venda.cod-emitente NO-LOCK NO-ERROR.

        FIND tt-itens-ped WHERE
             tt-itens-ped.nr-pedcli = ped-item.nr-pedcli AND
             tt-itens-ped.nr-sequencia = ped-item.nr-sequencia 
             NO-LOCK NO-ERROR.

        IF NOT AVAIL tt-itens-ped THEN DO.
           CREATE tt-itens-ped.
           ASSIGN tt-itens-ped.nr-pedcli = ped-item.nr-pedcli 
                  tt-itens-ped.nome-abrev = ped-item.nome-abrev
                  tt-itens-ped.nr-sequencia = ped-item.nr-sequencia 
                  tt-itens-ped.it-codigo = ped-item.it-codigo 
                  tt-itens-ped.cod-refer = ped-item.cod-refer 
                  tt-itens-ped.lote = SUBSTR(ped-item-ext.lote,1,2)
                  tt-itens-ped.corte-comerc = ped-item-ext.corte-comerc
                  tt-itens-ped.dt-entrega = ped-venda.dt-entrega
                  tt-itens-ped.nome-transp = ped-venda.nome-transp
                  tt-itens-ped.exportacao = IF AVAIL emitente AND emitente.pais <> "brasil"
                                            THEN YES ELSE NO.
        END.
        ASSIGN tt-itens-ped.qt-pedida = tt-itens-ped.qt-pedida + ped-item.qt-pedida.

        FIND cond-pagto OF ped-venda NO-LOCK NO-ERROR.
        IF AVAIL cond-pagto THEN DO.
           IF (cond-pagto.cod-vencto < 2 OR cond-pagto.cod-vencto > 3) AND  
              (ped-venda.cod-sit-aval < 2 OR ped-venda.cod-sit-aval > 3) THEN 
              ASSIGN tt-itens-ped.qt-crivada = tt-itens-ped.qt-crivada + ped-item.qt-pedida.
        END.
        ELSE
            IF ped-venda.cod-sit-aval < 2 OR ped-venda.cod-sit-aval > 3 THEN
               ASSIGN tt-itens-ped.qt-crivada = tt-itens-ped.qt-crivada + ped-item.qt-pedida.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-totais B-table-Win 
PROCEDURE pi-totais :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN fi-tot-lida      = 0
           fi-tot-reservada = 0
           fi-tot-aberta    = 0
           fi-tot-regra     = 0
           fi-tot-fila      = 0
           fi-tot-crivada   = 0.

    FOR EACH tt-itens NO-LOCK.
        ASSIGN fi-tot-lida      = fi-tot-lida      + tt-itens.qt-lida
               fi-tot-reservada = fi-tot-reservada + tt-itens.qt-reservada
               fi-tot-aberta    = fi-tot-aberta    + tt-itens.qt-aberta
               fi-tot-regra     = fi-tot-regra     + tt-itens.qt-regra
               fi-tot-crivada   = fi-tot-crivada   + tt-itens.qt-crivada
               fi-tot-fila      = fi-tot-fila      + tt-itens.qt-fila.
    END.
    DISPLAY fi-tot-lida     
            fi-tot-reservada
            fi-tot-aberta   
            fi-tot-regra    
            fi-tot-fila     
            fi-tot-crivada
            WITH FRAME {&FRAME-NAME}.

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
  {src/adm/template/sndkycas.i "nr-ord-produ" "ped-item" "nr-ord-produ"}
  {src/adm/template/sndkycas.i "nr-programa" "ped-item" "nr-programa"}
  {src/adm/template/sndkycas.i "it-codigo" "ped-item" "it-codigo"}

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
  {src/adm/template/snd-list.i "tt-itens"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-desc-item B-table-Win 
FUNCTION fn-desc-item RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND ITEM WHERE
       ITEM.it-codigo = tt-itens.it-codigo NO-LOCK NO-ERROR.

  RETURN item.desc-item.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

