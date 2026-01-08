&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r11 GUI ADM1
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
DEF BUFFER empresa FOR mgcad.empresa.

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF TEMP-TABLE tt-aux 
    FIELD c-linha AS CHAR.

DEF TEMP-TABLE tt-itens 
    FIELD nr-sequencia AS INT
    FIELD data         AS DATE FORMAT "99/99/9999" 
    FIELD item-ori     LIKE ITEM.it-codigo
    FIELD nr-lote      AS CHAR FORMAT "x(2)"
    FIELD un-ori       LIKE ITEM.un
    FIELD qtde-ori     AS DEC
    FIELD it-codigo    LIKE ITEM.it-codigo
    FIELD cod-refer    LIKE it-nota-fisc.cod-refer
    FIELD un           LIKE ITEM.un
    FIELD quantidade   AS DEC
    FIELD valor        AS DEC
    FIELD vlr-tot-item AS DEC
    FIELD cancelado    AS LOG
    FIELD processado   AS LOG
    INDEX indice1 IS PRIMARY nr-sequencia.

DEF TEMP-TABLE tt-acum-itens
    FIELD nr-sequencia AS INT
    FIELD it-codigo    LIKE ITEM.it-codigo
    FIELD cod-refer    LIKE it-nota-fisc.cod-refer
    FIELD un           LIKE ITEM.un
    FIELD quantidade   AS DEC
    FIELD valor        AS DEC
    FIELD vlr-tot-item AS DEC.


DEF TEMP-TABLE tt-nota-fisc LIKE nota-fiscal.
DEF TEMP-TABLE tt-it-nota-fisc LIKE it-nota-fisc.

DEFINE TEMP-TABLE wt-notas-geradas
       FIELD rw-nota-fiscal AS ROWID.

/* Definicao da tabela temporaria tt-notas-geradas, include {dibo/bodi317ef.i1} */
DEF TEMP-TABLE tt-notas-geradas NO-UNDO
    FIELD rw-nota-fiscal AS   ROWID
    FIELD nr-nota        LIKE nota-fiscal.nr-nota-fis
    FIELD seq-wt-docto   LIKE wt-docto.seq-wt-docto.

/* Definiá∆o de um buffer para tt-notas-geradas */
DEF BUFFER b-tt-notas-geradas for tt-notas-geradas.


/* Variaveis do Recebimento Fiscal ---                                  */
DEF TEMP-TABLE tt-docum-est NO-UNDO LIKE docum-est 
    FIELD r-rowid AS ROWID.

DEF TEMP-TABLE tt-item-doc-est NO-UNDO LIKE item-doc-est
    FIELD r-rowid AS ROWID.

DEF TEMP-TABLE wt-item-doc-est NO-UNDO LIKE item-doc-est
    FIELD r-rowid AS ROWID.

DEF TEMP-TABLE tt-dupli-apagar NO-UNDO LIKE dupli-apagar
    FIELD r-rowid AS ROWID.

/* Temp-Table tt-erro Definitions ---*/
{method/dbotterr.i}

DEF VAR h-boin090      AS HANDLE.
DEF VAR h-boin176      AS HANDLE.

DEF VAR i-nr-seq       AS INT.
DEF VAR de-tot-valor   AS DEC.
DEF VAR de-tot-peso    AS DEC.
DEF VAR de-tot-despes  AS DEC.

DEF NEW GLOBAL SHARED VAR h-essp0189  AS HANDLE NO-UNDO.

/* Vari†veis Recebidas nos ParaÉmetros */
DEF VAR c-cod-estabel     LIKE estabelec.cod-estabel.
DEF VAR l-entrada         AS LOG.
DEF VAR l-saida           AS LOG.
DEF VAR c-cod-estabel-ori LIKE estabelec.cod-estabel.
DEF VAR c-serie-ori       LIKE nota-fiscal.serie.
DEF VAR c-nr-nota-fis-ori LIKE nota-fiscal.nr-nota-fis.
DEF VAR c-natur-oper-ori  LIKE natur-oper.nat-operacao.
DEF VAR c-cliente         LIKE emitente.nome-abrev.
DEF VAR c-nat-sai-kg      LIKE natur-oper.nat-operacao. 
DEF VAR c-nat-sai-un      LIKE natur-oper.nat-operacao. 
DEF VAR c-nat-sai-terc    LIKE natur-oper.nat-operacao. 
DEF VAR c-serie-sai-kg    LIKE nota-fiscal.serie.
DEF VAR c-serie-sai-un    LIKE nota-fiscal.serie.
DEF VAR c-serie-sai-terc  LIKE nota-fiscal.serie. 
DEF VAR c-grup-terc       LIKE ITEM.ge-codigo.
DEF VAR c-arquivo-entrada AS CHAR.

/* Ouras Vari†eis */
DEF VAR i-item AS INT.

DEF TEMP-TABLE tt-refer
    FIELD it-codigo    LIKE ped-item.it-codigo
    FIELD cod-refer    LIKE ped-item.cod-refer 
    FIELD lote         LIKE ped-item-ext.lote 
    FIELD corte-comerc LIKE ped-item-ext.corte-comerc
    FIELD qt-pedida    LIKE ped-item.qt-pedida
    FIELD qt-reservada LIKE ped-item.qt-pedida
    FIELD qt-crivada   LIKE ped-item.qt-pedida
    FIELD qt-regra     LIKE ped-item.qt-pedida 
    FIELD qt-fila      LIKE ped-item.qt-pedida
    INDEX indice1 it-codigo cod-refer lote corte-comerc.

DEF VAR c-desc-item   LIKE ITEM.desc-item.

DEF VAR h-query       AS HANDLE.
DEF VAR h-acomp       AS HANDLE NO-UNDO.
DEF VAR da-dt-entrega AS DATE.
DEF VAR c-dia         AS CHAR.
DEF VAR c-lotes       AS CHAR.
DEF VAR c-desc-dentro AS CHAR. 
DEF VAR c-empresa     AS CHAR.

DEF VAR de-sulzer         LIKE ob-etiqueta.quantidade.
DEF VAR de-outras-tecelag LIKE ob-etiqueta.quantidade.
DEF VAR c-lst-tecelag     AS CHAR.
DEF VAR l-vrf-tecelagem   AS LOG.

DEF VAR c-dt-limite        AS CHAR.      
DEF VAR c-it-codigo-ini    AS CHAR.      
DEF VAR c-it-codigo-fin    AS CHAR.      
DEF VAR c-cod-refer-ini    AS CHAR.      
DEF VAR c-cod-refer-fin    AS CHAR.      
DEF VAR c-nr-pedcli-ini    AS CHAR.      
DEF VAR c-nr-pedcli-fin    AS CHAR.      
DEF VAR i-nr-seq-ini       AS INT.       
DEF VAR i-nr-seq-fin       AS INT.       
DEF VAR c-localiz-ini      LIKE ob-etiqueta.localiz.
DEF VAR c-localiz-fin      LIKE ob-etiqueta.localiz.
DEF VAR c-corte-comerc-ini LIKE ob-etiqueta.corte-comerc.
DEF VAR c-corte-comerc-fin LIKE ob-etiqueta.corte-comerc.
DEF VAR c-cod-obsoleto-ini AS CHAR.      
DEF VAR c-cod-obsoleto-fin AS CHAR.      
DEF VAR de-perc-min        AS DEC.       
DEF VAR de-perc-max        AS DEC.       
DEF VAR l-lote-todos       AS LOG.
DEF VAR l-lote-pp          AS LOG.
DEF VAR l-lote-pd          AS LOG.
DEF VAR l-lote-rp          AS LOG.
DEF VAR l-lote-rd          AS LOG.
DEF VAR l-lote-sc          AS LOG.
DEF VAR l-lote-ca          AS LOG.
DEF VAR c-tp-artigo        AS CHAR.
DEF VAR l-res-crivo        AS LOG.
DEF VAR l-acomp            AS LOG.

DEF VAR c-cod-refer  LIKE ped-item.it-codigo.

/* Variaveis da Rotina de Impress∆o */
DEFINE VAR i-saida         AS INT.
DEFINE VAR c-saida         AS CHAR.
DEFINE VAR i-num-copias    AS INT.
DEFINE VAR l-ok            AS LOG.

DEF VAR i-lin              AS INT.
DEF VAR i-pag              AS INT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-itens

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-itens

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br-itens                                      */
&Scoped-define FIELDS-IN-QUERY-br-itens tt-itens.item-ori tt-itens.qtde-ori tt-itens.nr-lote tt-itens.un-ori tt-itens.it-codigo tt-itens.un tt-itens.quantidade tt-itens.valor tt-itens.vlr-tot-item fn-desc-item() @ c-desc-item   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-itens   
&Scoped-define SELF-NAME br-itens
&Scoped-define QUERY-STRING-br-itens FOR EACH tt-itens WHERE NO-LOCK
&Scoped-define OPEN-QUERY-br-itens OPEN QUERY {&SELF-NAME} FOR EACH tt-itens WHERE NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-itens tt-itens
&Scoped-define FIRST-TABLE-IN-QUERY-br-itens tt-itens


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-3 br-itens bt-cancel bt-imprime bt-ok 
&Scoped-Define DISPLAYED-OBJECTS fi-tot-qtde fi-tot-valor 

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
DEFINE BUTTON bt-cancel 
     IMAGE-UP FILE "image/im-cance.bmp":U
     LABEL "" 
     SIZE 4.86 BY 1.29 TOOLTIP "Marca Venda como Cancelada".

DEFINE BUTTON bt-imprime 
     IMAGE-UP FILE "image/im-pri.bmp":U
     LABEL "" 
     SIZE 4.86 BY 1.29 TOOLTIP "Imprimir Dados para Conferància".

DEFINE BUTTON bt-ok AUTO-GO 
     IMAGE-UP FILE "image/im-cq.bmp":U
     LABEL "OK" 
     SIZE 4.86 BY 1.25 TOOLTIP "Gera Notas de Entrada/Saida".

DEFINE VARIABLE fi-tot-qtde AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12.29 BY .88
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi-tot-valor AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12.29 BY .88
     FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 92.72 BY 1.58
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-itens FOR 
      tt-itens SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-itens
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-itens B-table-Win _FREEFORM
  QUERY br-itens NO-LOCK DISPLAY
      tt-itens.item-ori  FORMAT "x(12)":U  COLUMN-LABEL "Item ORI"   WIDTH 16
      tt-itens.qtde-ori                    COLUMN-LABEL "Qtde ORI"   
      tt-itens.nr-lote                     COLUMN-LABEL "Lote"   
      tt-itens.un-ori                      COLUMN-LABEL "Un ORI"   
      tt-itens.it-codigo FORMAT "x(12)":U                            WIDTH 16      
      tt-itens.un
      tt-itens.quantidade                  COLUMN-LABEL "Quantidade" 
      tt-itens.valor                       COLUMN-LABEL "Valor"    
      tt-itens.vlr-tot-item                COLUMN-LABEL "Tot Item" 
      fn-desc-item() @ c-desc-item         COLUMN-LABEL "Descriá∆o"  WIDTH 45
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 93 BY 11.25
         FONT 1
         TITLE "Itens Processados" ROW-HEIGHT-CHARS .63.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br-itens AT ROW 1 COL 1.43
     bt-cancel AT ROW 12.67 COL 75.86 WIDGET-ID 2
     bt-imprime AT ROW 12.67 COL 83.29
     bt-ok AT ROW 12.67 COL 88.57
     fi-tot-qtde AT ROW 12.88 COL 19.14 COLON-ALIGNED NO-LABEL
     fi-tot-valor AT ROW 12.88 COL 54.86 COLON-ALIGNED NO-LABEL
     "Qtde Total:" VIEW-AS TEXT
          SIZE 9 BY .75 AT ROW 12.96 COL 11.57
          BGCOLOR 8 FONT 6
     "Valor Total:" VIEW-AS TEXT
          SIZE 10 BY .75 AT ROW 12.96 COL 46.72
          BGCOLOR 8 FONT 6
     RECT-3 AT ROW 12.5 COL 1.29
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
         HEIGHT             = 13.29
         WIDTH              = 94.57.
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
/* BROWSE-TAB br-itens RECT-3 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fi-tot-qtde IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-valor IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-itens
/* Query rebuild information for BROWSE br-itens
     _START_FREEFORM
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
ON ROW-DISPLAY OF br-itens IN FRAME F-Main /* Itens Processados */
DO:
    tt-itens.item-ori:FONT IN BROWSE br-itens = 6.
    tt-itens.qtde-ori:FONT IN BROWSE br-itens = 6.            
    tt-itens.nr-lote:FONT IN BROWSE br-itens = 6.             
    tt-itens.un-ori:FONT IN BROWSE br-itens = 6.              
    tt-itens.it-codigo:FONT IN BROWSE br-itens = 6.
    tt-itens.un:FONT IN BROWSE br-itens = 6.                  
    tt-itens.quantidade:FONT IN BROWSE br-itens = 6.          
    tt-itens.valor:FONT IN BROWSE br-itens = 6.               
    tt-itens.vlr-tot-item:FONT IN BROWSE br-itens = 6.        
    c-desc-item:FONT IN BROWSE br-itens = 6. 

    tt-itens.item-ori:FGCOLOR IN BROWSE br-itens = ?.
    tt-itens.qtde-ori:FGCOLOR IN BROWSE br-itens = ?.            
    tt-itens.nr-lote:FGCOLOR IN BROWSE br-itens = ?.             
    tt-itens.un-ori:FGCOLOR IN BROWSE br-itens = ?.              
    tt-itens.it-codigo:FGCOLOR IN BROWSE br-itens = ?.
    tt-itens.un:FGCOLOR IN BROWSE br-itens = ?.                  
    tt-itens.quantidade:FGCOLOR IN BROWSE br-itens = ?.          
    tt-itens.valor:FGCOLOR IN BROWSE br-itens = ?.               
    tt-itens.vlr-tot-item:FGCOLOR IN BROWSE br-itens = ?.        
    c-desc-item:FGCOLOR IN BROWSE br-itens = ?. 

   IF tt-itens.cancelado THEN DO.
      tt-itens.item-ori:FONT IN BROWSE br-itens = 1.
      tt-itens.qtde-ori:FONT IN BROWSE br-itens = 1.            
      tt-itens.nr-lote:FONT IN BROWSE br-itens = 1.             
      tt-itens.un-ori:FONT IN BROWSE br-itens = 1.              
      tt-itens.it-codigo:FONT IN BROWSE br-itens = 1.
      tt-itens.un:FONT IN BROWSE br-itens = 1.                  
      tt-itens.quantidade:FONT IN BROWSE br-itens = 1.          
      tt-itens.valor:FONT IN BROWSE br-itens = 1.               
      tt-itens.vlr-tot-item:FONT IN BROWSE br-itens = 1.        
      c-desc-item:FONT IN BROWSE br-itens = 1. 

      tt-itens.item-ori:FGCOLOR IN BROWSE br-itens = 12.
      tt-itens.qtde-ori:FGCOLOR IN BROWSE br-itens = 12.            
      tt-itens.nr-lote:FGCOLOR IN BROWSE br-itens = 12.             
      tt-itens.un-ori:FGCOLOR IN BROWSE br-itens = 12.              
      tt-itens.it-codigo:FGCOLOR IN BROWSE br-itens = 12.
      tt-itens.un:FGCOLOR IN BROWSE br-itens = 12.                  
      tt-itens.quantidade:FGCOLOR IN BROWSE br-itens = 12.          
      tt-itens.valor:FGCOLOR IN BROWSE br-itens = 12.               
      tt-itens.vlr-tot-item:FGCOLOR IN BROWSE br-itens = 12.        
      c-desc-item:FGCOLOR IN BROWSE br-itens = 12. 
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-itens B-table-Win
ON VALUE-CHANGED OF br-itens IN FRAME F-Main /* Itens Processados */
DO:
  bt-cancel:LOAD-IMAGE("image/im-cance.bmp").
  bt-cancel:TOOLTIP = "Marca Venda como Cancelada".
  IF tt-itens.cancelado = YES THEN DO.
     bt-cancel:LOAD-IMAGE("image/im-reini.bmp").
     bt-cancel:TOOLTIP = "Desmarca Cancelamento da Venda ".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancel B-table-Win
ON CHOOSE OF bt-cancel IN FRAME F-Main
DO:
   IF tt-itens.cancelado = NO THEN DO.
      ASSIGN tt-itens.cancelado = YES.
      bt-cancel:LOAD-IMAGE("image/im-reini.bmp").
      bt-cancel:TOOLTIP = "Desmarca Cancelamento da Venda ".
   END.
   ELSE DO.
      ASSIGN tt-itens.cancelado = NO.
      bt-cancel:LOAD-IMAGE("image/im-cance.bmp").
      bt-cancel:TOOLTIP = "Marca Venda como Cancelada".
   END.

   br-itens:REFRESH().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-imprime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-imprime B-table-Win
ON CHOOSE OF bt-imprime IN FRAME F-Main
DO:
  RUN pi-imprime.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok B-table-Win
ON CHOOSE OF bt-ok IN FRAME F-Main /* OK */
DO:
    IF l-entrada THEN DO.
       FIND param-re WHERE
            param-re.usuario = c-seg-usuario NO-LOCK NO-ERROR.
       IF NOT AVAIL param-re THEN DO:
          MESSAGE "Usu†rio n∆o Cadastrado no Recebimento..."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN NO-APPLY.
       END.
       IF AVAIL param-re AND 
          param-re.baixa-estoq = NO THEN DO:
          MESSAGE "Usu†rio n∆o atualiza estoque autom†ticamente..."
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN NO-APPLY.
       END.

       DO TRANSACTION:
          RUN pi-recebimento.
          IF RETURN-VALUE = "ADM-ERROR" THEN 
             UNDO,LEAVE.

          FOR EACH tt-itens.
              DELETE tt-itens.
          END.
          RUN adm-open-query-cases.

          RUN pi-atualiza-recto.

          RUN pi-select-page IN h-essp0189 (INPUT 1).
       END.
    END.
    ELSE DO.
       RUN pi-faturamento.

       RUN pi-select-page IN h-essp0189 (INPUT 1).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-atualiza-recto B-table-Win 
PROCEDURE pi-atualiza-recto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF NOT VALID-HANDLE(h-boin090) OR
       h-boin090:TYPE      <> "PROCEDURE":U OR
       h-boin090:FILE-NAME <> "inbo/boin090.p":U THEN
       RUN inbo/boin090.p PERSISTENT SET h-boin090.

    EMPTY TEMP-TABLE tt-docum-est.

    FIND docum-est WHERE
         docum-est.declaracao-import = c-nr-nota-fis-ori
         NO-LOCK NO-ERROR.

    /* Cria Temp-Table para o Recebimento*/
    CREATE tt-docum-est.
    BUFFER-COPY docum-est TO tt-docum-est
           ASSIGN tt-docum-est.r-rowid = ROWID(docum-est).

    /* Atualiza Documento */
    RUN openQueryStatic IN h-boin090 (INPUT "Main":U).
    RUN emptyRowErrors IN h-boin090.
    RUN repositionRecord IN h-boin090 (input tt-docum-est.r-rowid).
    RUN AtualizaDocumento IN h-boin090.
    RUN getRowErrors IN h-boin090 (OUTPUT TABLE RowErrors).
    IF CAN-FIND(FIRST RowErrors 
               WHERE RowErrors.ErrorSubType = "ERROR":U  ) THEN DO:

       FOR EACH rowerrors WHERE
                RowErrors.ErrorSubType = "ERROR":U:
           MESSAGE "Erro ao Atualizar o Documento" SKIP
                    rowerrors.errordescription 
                   VIEW-AS ALERT-BOX.
       END.
       IF VALID-HANDLE(h-boin090) THEN
          DELETE PROCEDURE h-boin090.

       RETURN 'ADM-ERROR'.
    END.

    IF VALID-HANDLE(h-boin090) THEN
       DELETE PROCEDURE h-boin090.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-faturamento B-table-Win 
PROCEDURE pi-faturamento :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /* Definiá∆o da vari†veis */
    DEF VAR h-bodi317pr          AS HANDLE NO-UNDO.
    DEF VAR h-bodi317sd          AS HANDLE NO-UNDO.
    DEF VAR h-bodi317im1bra      AS HANDLE NO-UNDO.
    DEF VAR h-bodi317va          AS HANDLE NO-UNDO.
    DEF VAR h-bodi317in          AS HANDLE NO-UNDO.
    DEF VAR h-bodi317ef          AS HANDLE NO-UNDO.
    DEF VAR l-proc-ok-aux        AS LOG    NO-UNDO.
    DEF VAR c-ultimo-metodo-exec AS CHAR   NO-UNDO.
    DEF VAR da-dt-emis-nota      AS DATE   NO-UNDO.
    DEF VAR da-dt-base-dup       AS DATE   NO-UNDO.
    DEF VAR da-dt-prvenc         AS DATE   NO-UNDO.
    DEF VAR c-seg-usuario        AS CHAR   NO-UNDO.
    DEF VAR c-nome-abrev         AS CHAR   NO-UNDO.   
    DEF VAR c-nat-operacao       AS CHAR   NO-UNDO.
    DEF VAR c-cod-canal-venda    AS CHAR   NO-UNDO.
    DEF VAR i-seq-wt-docto       AS INT    NO-UNDO.
    DEF VAR i-seq-wt-it-docto    AS INT    NO-UNDO.

    DEF VAR i-nr-seq             AS INT    NO-UNDO.

    DEF VAR c-serie              AS CHAR   NO-UNDO.
    DEF VAR c-natur-oper         LIKE nota-fiscal.nat-oper.

    FOR EACH tt-itens WHERE 
             tt-itens.cancelado = NO NO-LOCK.
        IF tt-itens.un = "un" THEN DO.
           FIND ITEM WHERE
                ITEM.it-codigo = tt-itens.it-codigo NO-LOCK NO-ERROR.
           IF ITEM.ge-codigo = c-grup-terc THEN     /* Ç terceiro */
              ASSIGN c-serie = c-serie-sai-terc
                     c-natur-oper = c-nat-sai-terc.
           ELSE
              ASSIGN c-serie = c-serie-sai-un
                     c-natur-oper = c-nat-sai-un.
        END.
        ELSE
           ASSIGN c-serie = c-serie-sai-kg
                  c-natur-oper = c-nat-sai-kg.

        FIND tt-nota-fisc WHERE
             tt-nota-fisc.cod-estabel = c-cod-estabel AND 
             tt-nota-fisc.serie = c-serie AND
             tt-nota-fisc.nat-oper = c-natur-oper AND
             tt-nota-fisc.dt-emis-nota = tt-itens.data NO-LOCK NO-ERROR.

        IF NOT AVAIL tt-nota-fisc THEN DO.
           CREATE tt-nota-fisc.
           ASSIGN tt-nota-fisc.cod-estabel = c-cod-estabel  
                  tt-nota-fisc.serie = c-serie
                  tt-nota-fisc.nat-oper = c-natur-oper
                  tt-nota-fisc.dt-emis-nota = tt-itens.data. 

           ASSIGN i-nr-seq = 0.
        END.

        ASSIGN i-nr-seq = i-nr-seq + 10.
        
        CREATE tt-it-nota-fisc.
        ASSIGN tt-it-nota-fisc.nr-seq-fat = i-nr-seq
               tt-it-nota-fisc.cod-estabel = tt-nota-fisc.cod-estabel  
               tt-it-nota-fisc.serie = tt-nota-fisc.serie
               tt-it-nota-fisc.nat-oper = tt-nota-fisc.nat-oper
               tt-it-nota-fisc.dt-emis-nota = tt-nota-fisc.dt-emis-nota
               tt-it-nota-fisc.it-codigo = tt-itens.it-codigo
               tt-it-nota-fisc.qt-faturada[1] = tt-itens.quantidade
               tt-it-nota-fisc.vl-preuni = tt-itens.valor / tt-itens.quantidade.
    END.
    
    FOR EACH tt-nota-fisc NO-LOCK. 
        FIND FIRST nota-fiscal WHERE
                   nota-fiscal.cod-estabel = tt-nota-fisc.cod-estabel  AND
                   nota-fiscal.dt-emis-nota = tt-nota-fisc.dt-emis-nota
                   NO-LOCK NO-ERROR.
        IF AVAIL nota-fiscal THEN DO:
           MESSAGE "J† Existe Nota Fiscal para a Data " tt-nota-fisc.dt-emis-nota SKIP
                   "Deseja Continuar ? " 
                   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                   UPDATE l-continua AS LOGICAL.

           IF NOT l-continua THEN
              RETURN "ADM-ERROR".
        END.
    END.


    /* Inicializaá∆o das BOS para C†lculo */
    RUN dibo/bodi317in.p PERSISTENT SET h-bodi317in.
    RUN inicializaBOS IN h-bodi317in (OUTPUT h-bodi317pr,
                                      OUTPUT h-bodi317sd,     
                                      OUTPUT h-bodi317im1bra,
                                      OUTPUT h-bodi317va).

    FOR EACH tt-nota-fisc NO-LOCK.
        /* Limpar a tabela de erros em todas as BOS */
        RUN emptyRowErrors        IN h-bodi317in.
    
        /* Cria o registro WT-DOCTO para o pedido */
        RUN criaWtDocto IN h-bodi317sd (INPUT  c-seg-usuario,
                                        INPUT  tt-nota-fisc.cod-estabel,
                                        INPUT  tt-nota-fisc.serie,
                                        INPUT  "1", 
                                        INPUT  c-cliente,
                                        INPUT  "",
                                        INPUT  2,    
                                        INPUT  9999, 
                                        INPUT  tt-nota-fisc.dt-emis-nota,
                                        INPUT  0,  
                                        INPUT  tt-nota-fisc.nat-operacao,
                                        INPUT  "",
                                        OUTPUT i-seq-wt-docto,
                                        OUTPUT l-proc-ok-aux).
    
        /* Busca poss°veis erros que ocorreram nas validaá‰es */
        RUN devolveErrosbodi317sd IN h-bodi317sd (OUTPUT c-ultimo-metodo-exec,
                                                  OUTPUT TABLE RowErrors).
    
        /* Pesquisa algum erro ou advertància que tenha ocorrido */
        FIND FIRST RowErrors NO-LOCK NO-ERROR.
        
        /* Caso tenha achado algum erro ou advertància, mostra em tela */
        IF AVAIL RowErrors THEN DO.
           FOR EACH RowErrors:
               MESSAGE rowerrors.errordescription
                       VIEW-AS ALERT-BOX ERROR BUTTONS OK
                       TITLE "Erro - NF".
           END.
        END.
        
        /* Caso ocorreu problema nas validaá‰es, n∆o continua o processo */
        IF NOT l-proc-ok-aux THEN
           UNDO, LEAVE.
    
        FIND FIRST wt-docto WHERE
                   wt-docto.seq-wt-docto = i-seq-wt-docto NO-ERROR.
    
        FOR EACH tt-it-nota-fisc WHERE
                 tt-it-nota-fisc.cod-estabel = tt-nota-fisc.cod-estabel AND
                 tt-it-nota-fisc.serie = tt-nota-fisc.serie AND
                 tt-it-nota-fisc.nat-oper = tt-nota-fisc.nat-oper AND
                 tt-it-nota-fisc.dt-emis-nota = tt-nota-fisc.dt-emis-nota NO-LOCK.

            /* Limpar a tabela de erros em todas as BOS */
            RUN emptyRowErrors        IN h-bodi317in.

            RUN inicializaAcompanhamento      IN h-bodi317sd.
            RUN criaWtItDocto IN h-bodi317sd (INPUT  "",
                                              INPUT  "",
                                              INPUT  tt-it-nota-fisc.nr-seq-fat,
                                              INPUT  tt-it-nota-fisc.it-codigo, 
                                              INPUT  "",
                                              INPUT  tt-it-nota-fisc.nat-oper,    
                                              OUTPUT i-seq-wt-it-docto,
                                              OUTPUT l-proc-ok-aux).

            RUN finalizaAcompanhamento        IN h-bodi317sd.

            /* Busca poss°veis erros que ocorreram nas validaá‰es */
            RUN devolveErrosbodi317sd         IN h-bodi317sd (OUTPUT c-ultimo-metodo-exec,
                                                              OUTPUT TABLE RowErrors).

            /* Pesquisa algum erro ou advertància que tenha ocorrido */
            FIND FIRST RowErrors NO-LOCK NO-ERROR.

            /* Caso tenha achado algum erro ou advertància, mostra em tela */
            IF AVAIL RowErrors THEN DO.
               FOR EACH RowErrors:
                   MESSAGE rowerrors.errordescription 
                       VIEW-AS ALERT-BOX ERROR BUTTONS OK
                       TITLE 'Erro - Itens NF'.
               END.
            END.

            /* Caso ocorreu problema nas validaá‰es, n∆o continua o processo */
            IF NOT l-proc-ok-aux THEN
               UNDO, LEAVE.

            RUN GravaInfGeraisWtItDocto IN h-bodi317sd (INPUT  i-seq-wt-docto,
                                                        INPUT  i-seq-wt-it-docto,
                                                        INPUT  tt-it-nota-fisc.qt-faturada[1],
                                                        INPUT  tt-it-nota-fisc.vl-preuni, 
                                                        INPUT  0,
                                                        INPUT  0).

            /* Busca poss°veis erros que ocorreram nas validaá‰es */
            RUN devolveErrosbodi317sd         IN h-bodi317sd (OUTPUT c-ultimo-metodo-exec,
                                                              OUTPUT TABLE RowErrors).

            /* Pesquisa algum erro ou advertància que tenha ocorrido */
            FIND FIRST RowErrors NO-LOCK NO-ERROR.

            /* Caso tenha achado algum erro ou advertància, mostra em tela */
            IF AVAIL RowErrors THEN DO.
               FOR EACH RowErrors:
                   MESSAGE rowerrors.errordescription 
                       VIEW-AS ALERT-BOX ERROR BUTTONS OK
                       TITLE 'Erro - Itens NF'.
               END.
            END.

            /* Caso ocorreu problema nas validaá‰es, n∆o continua o processo */
            IF NOT l-proc-ok-aux THEN
               UNDO, LEAVE.
        END.

        FOR EACH wt-it-docto WHERE
                 wt-it-docto.seq-wt-docto = i-seq-wt-docto NO-LOCK.
    
            /* Limpar a tabela de erros em todas as BOS */
            RUN emptyRowErrors IN h-bodi317in.
    
            /* Atende todos os itens do pedido, com tela de acompanhamento */
            RUN inicializaAcompanhamento IN h-bodi317pr.
            RUN atendeTotalSeq IN h-bodi317pr (INPUT  i-seq-wt-docto,
                                               INPUT  wt-it-docto.seq-wt-it-docto,
                                               OUTPUT l-proc-ok-aux).
            RUN finalizaAcompanhamento IN h-bodi317pr.
             
            /* Busca poss°veis erros que ocorreram nas validaá‰es */
            RUN devolveErrosbodi317pr IN h-bodi317pr (OUTPUT c-ultimo-metodo-exec,
                                                      OUTPUT TABLE RowErrors).
    
            /* Pesquisa algum erro ou advertància que tenha ocorrido */
            FIND FIRST RowErrors NO-LOCK NO-ERROR.
    
            /* Caso tenha achado algum erro ou advertància, mostra em tela */
            IF AVAIL RowErrors THEN DO.
               FOR EACH RowErrors:
                    MESSAGE rowerrors.errordescription 
                        VIEW-AS ALERT-BOX ERROR BUTTONS OK
                        TITLE "ERRO - Atendendo Reservas".
               END.
            END.
    
            /* Caso ocorreu problema nas validaá‰es, n∆o continua o processo */
            IF NOT l-proc-ok-aux THEN
               UNDO, LEAVE.
        END.
    
        /* Limpar a tabela de erros em todas as BOS */
        RUN emptyRowErrors           IN h-bodi317in.
    
        /* Calcula o pedido, com acompanhamento */
        RUN inicializaAcompanhamento IN h-bodi317pr.
        RUN confirmaCalculo          IN h-bodi317pr (INPUT  i-seq-wt-docto,
                                                     OUTPUT l-proc-ok-aux).
        RUN finalizaAcompanhamento   IN h-bodi317pr.
    
        /* Busca poss°veis erros que ocorreram nas validaá‰es */
        RUN devolveErrosbodi317pr    IN h-bodi317pr (OUTPUT c-ultimo-metodo-exec,
                                                     OUTPUT TABLE RowErrors).
    
        /* Pesquisa algum erro ou advertància que tenha ocorrido */
        FIND FIRST RowErrors NO-LOCK NO-ERROR.
        
        /* Caso tenha achado algum erro ou advertància, mostra em tela */
        IF AVAIL RowErrors THEN
           FOR EACH RowErrors:
               MESSAGE rowerrors.errordescription
                        VIEW-AS ALERT-BOX ERROR BUTTONS OK
                        TITLE "Erro - Calculo".
           END.
        
        /* Caso ocorreu problema nas validaá‰es, n∆o continua o processo */
        IF NOT l-proc-ok-aux THEN
           UNDO, LEAVE.
    
        FIND wt-docto WHERE
             wt-docto.seq-wt-docto = i-seq-wt-docto.
    
        /* Efetiva os pedidos e cria a nota */
        RUN dibo/bodi317ef.p PERSISTENT SET h-bodi317ef.
        RUN emptyRowErrors           IN h-bodi317in.
        RUN inicializaAcompanhamento IN h-bodi317ef.
        RUN setaHandlesBOS           IN h-bodi317ef(h-bodi317pr,     
                                                    h-bodi317sd, 
                                                    h-bodi317im1bra, 
                                                    h-bodi317va).
        RUN efetivaNota              IN h-bodi317ef (INPUT i-seq-wt-docto,
                                                     INPUT YES,
                                                     OUTPUT l-proc-ok-aux).
        RUN finalizaAcompanhamento   IN h-bodi317ef.
    
    
        /* Busca poss°veis erros que ocorreram nas validaá‰es */
        RUN devolveErrosbodi317ef    IN h-bodi317ef (OUTPUT c-ultimo-metodo-exec,
                                                     OUTPUT TABLE RowErrors).
    
        /* Pesquisa algum erro ou advertància que tenha ocorrido */
        FIND FIRST RowErrors WHERE
                   RowErrors.ErrorSubType = "ERROR":U NO-ERROR.
    
        /* Caso tenha achado algum erro ou advertància, mostra em tela */
        IF AVAIL RowErrors THEN
           FOR EACH RowErrors:
               MESSAGE rowerrors.errordescription
                       VIEW-AS ALERT-BOX ERROR BUTTONS OK
                       TITLE "Erro - Efetivaá∆o".
           END.
        
        /* Caso ocorreu problema nas validaá‰es, n∆o continua o processo */
        IF NOT l-proc-ok-aux THEN DO:
           DELETE PROCEDURE h-bodi317ef.
           UNDO, LEAVE.
        END.
    
        /* Busca as notas fiscais geradas */
        RUN buscaTTNotasGeradas IN h-bodi317ef (OUTPUT l-proc-ok-aux,
                                                OUTPUT TABLE tt-notas-geradas).
    
        FOR EACH tt-notas-geradas.
            FIND FIRST nota-fiscal WHERE
                 ROWID(nota-fiscal) = tt-notas-geradas.rw-nota-fiscal
                 NO-ERROR.

            MESSAGE nota-fiscal.nr-nota-fis
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        END.


        /* Elimina o handle do programa bodi317ef */
        DELETE PROCEDURE h-bodi317ef.
    
        LEAVE.
    END.
            
    /* Finalizaá∆o das BOS utilizada no c†lculo */
    RUN finalizaBOS in h-bodi317in.
    
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
    PUT c-empresa  FORMAT "X(40)"                 AT   1
        "DATA: "                                  AT  80
        STRING(TODAY,"99/99/9999") FORMAT "X(10)" AT  86
        "HORA: "                                  AT 105
        STRING(TIME,"hh:mm:ss")                   AT 111
        "PAG:"                                    AT 132
        i-pag FORMAT ">>>"                        AT 137
        SKIP(1).

    PUT "RELATORIO DE MOVIMENTAÄ«O DAS LOJAS " AT 45
        SKIP(1).
    PUT "Loja: " AT 1
         estabelec.nome
         SKIP(1).

    PUT "ItemORI       QtdeORI Lote         UnORI  Item          Descriá∆o                     Un          Qtde      Valor Un   Valor Total" AT 1.  
    PUT "-------- ------------ ------------ ------ ------------- ----------------------------- -- ------------- ------------- -------------" AT 1.

    ASSIGN i-pag = i-pag + 1.                                                                              
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
  DEF VAR h-prog AS HANDLE NO-UNDO.
  DEF VAR i-ct   AS INT.

  RUN utp/ut-utils.p PERSISTENT SET h-prog.


  RUN esapi/saida-imp.p (OUTPUT i-saida,
                         OUTPUT c-saida,
                         OUTPUT i-num-copias,
                         OUTPUT l-ok).

  CASE i-saida:
      WHEN 1 THEN DO.
          OUTPUT TO VALUE(c-saida) CONVERT TARGET "ibm850" PAGED PAGE-SIZE 62.
          PUT CONTROL "~033E~033(s19H".    
      END.
      WHEN 2 THEN
          OUTPUT TO VALUE(c-saida).
      WHEN 3 THEN DO.
          ASSIGN c-saida = SESSION:TEMP-DIRECTORY + "essp0183.tmp".
          OUTPUT TO VALUE(c-saida) CONVERT SOURCE "ibm850".
      END.
  END CASE.

  /* Busca Nome da Empresa */
  FIND FIRST param-global NO-LOCK NO-ERROR.
  FIND FIRST empresa
       WHERE empresa.ep-codigo = param-global.empresa-prin NO-LOCK NO-ERROR. 
  ASSIGN c-empresa = (IF AVAIL empresa THEN empresa.razao-social ELSE "").


  FIND estabelec WHERE
       estabelec.cod-estabel = c-cod-estabel NO-LOCK NO-ERROR.

  DO i-ct = 1 TO i-num-copias.
     ASSIGN i-lin = 99
            i-pag =  1.
     FOR EACH tt-itens WHERE
              tt-itens.cancelado = NO NO-LOCK.
    
         IF i-lin > 62 THEN DO:
            RUN pi-imp-cabec.
            ASSIGN i-lin = 7.
         END.

         PUT tt-itens.item-ori              AT   1   FORMAT "x(8)"  
             tt-itens.qtde-ori              AT  10   FORMAT ">,>>>,>>9.99"   
             tt-itens.nr-lote               AT  23     
             tt-itens.un-ori                AT  36     
             tt-itens.it-codigo             AT  43   FORMAT "x(13)"    
             fn-desc-item()                 AT  57   FORMAT "x(29)"     
             tt-itens.un                    AT  87     
             tt-itens.quantidade            AT  90   FORMAT ">>,>>>,>>9.99"       
             tt-itens.valor                 AT 104   FORMAT ">>,>>>,>>9.99"       
             tt-itens.vlr-tot-item          AT 118   FORMAT ">>,>>>,>>9.99".    

         ASSIGN i-lin            = i-lin + 1.
         ACCUMULATE tt-itens.qtde-ori     (TOTAL).
         ACCUMULATE tt-itens.quantidade   (TOTAL).
         ACCUMULATE tt-itens.vlr-tot-item (TOTAL).
     END.

     PUT SKIP(1).
     PUT ACCUM TOTAL tt-itens.qtde-ori     FORMAT ">,>>>,>>9.99"  AT  10.
     PUT ACCUM TOTAL tt-itens.quantidade   FORMAT ">>,>>>,>>9.99" AT  90.
     PUT ACCUM TOTAL tt-itens.vlr-tot-item FORMAT ">>,>>>,>>9.99" AT  118.

     IF i-saida = 1 THEN DO:
        PAGE.
        PUT "" AT 1.
     END.
  END.

  IF i-saida = 3 THEN DO.
     RUN EXECUTE IN h-prog(INPUT "notepad.exe",
                           INPUT c-saida).
     DELETE PROCEDURE h-prog.
  END.
  OUTPUT CLOSE.

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
DEF INPUT PARAMETER p-cod-estabel      AS CHAR.
DEF INPUT PARAMETER p-entrada          AS LOG.
DEF INPUT PARAMETER p-saida            AS LOG.
DEF INPUT PARAMETER p-cod-estabel-ori  AS CHAR.
DEF INPUT PARAMETER p-serie-ori        AS CHAR.
DEF INPUT PARAMETER p-nr-nota-fis-ori  AS CHAR.
DEF INPUT PARAMETER p-natur-oper-ori   LIKE natur-oper.nat-operacao.
DEF INPUT PARAMETER p-cliente          AS CHAR.  
DEF INPUT PARAMETER p-nat-sai-kg       LIKE natur-oper.nat-operacao.
DEF INPUT PARAMETER p-nat-sai-un       LIKE natur-oper.nat-operacao.    
DEF INPUT PARAMETER p-nat-sai-terc     LIKE natur-oper.nat-operacao.
DEF INPUT PARAMETER p-serie-sai-kg     AS CHAR.  
DEF INPUT PARAMETER p-serie-sai-un     AS CHAR.  
DEF INPUT PARAMETER p-serie-sai-terc   AS CHAR.  
DEF INPUT PARAMETER p-grup-terc        LIKE ITEM.ge-codigo.
DEF INPUT PARAMETER p-arquivo-entrada  AS CHAR.

ASSIGN c-cod-estabel     = p-cod-estabel
       l-entrada         = p-entrada
       l-saida           = p-saida           
       c-cod-estabel-ori = p-cod-estabel-ori 
       c-serie-ori       = p-serie-ori       
       c-nr-nota-fis-ori = p-nr-nota-fis-ori 
       c-natur-oper-ori  = p-natur-oper-ori
       c-cliente         = p-cliente          
       c-nat-sai-kg      = p-nat-sai-kg       
       c-nat-sai-un      = p-nat-sai-un       
       c-nat-sai-terc    = p-nat-sai-terc     
       c-serie-sai-kg    = p-serie-sai-kg     
       c-serie-sai-un    = p-serie-sai-un     
       c-serie-sai-terc  = p-serie-sai-terc   
       c-grup-terc       = p-grup-terc
       c-arquivo-entrada = p-arquivo-entrada.

ASSIGN fi-tot-qtde = 0
       fi-tot-valor = 0.

EMPTY TEMP-TABLE tt-itens.
EMPTY TEMP-TABLE tt-aux.

IF l-saida THEN DO.  /* Notas Fiscais de Venda, importadas do Cupom Fiscal */
   INPUT FROM VALUE(c-arquivo-entrada).
   REPEAT.
      CREATE tt-aux.
      IMPORT DELIMITER ":&*@" tt-aux.
   END.
    
   FOR EACH tt-aux.
       IF NOT tt-aux.c-linha BEGINS "60D" THEN
          DELETE tt-aux.
   END.

   FOR EACH tt-aux.
       ASSIGN i-item = INT(SUBSTR(tt-aux.c-linha,32,14)).
    
       CREATE tt-itens.
       ASSIGN tt-itens.data = DATE(SUBSTR(tt-aux.c-linha,10,2) + "/" +
                                   SUBSTR(tt-aux.c-linha,8,2) + "/" +
                                   SUBSTR(tt-aux.c-linha,4,4))
              tt-itens.it-codigo = TRIM(STRING(i-item))
              tt-itens.quantidade = DEC(SUBSTR(tt-aux.c-linha,46,13)) / 1000
              tt-itens.vlr-tot-item = DEC(SUBSTR(tt-aux.c-linha,59,16)) / 100
              tt-itens.valor = tt-itens.vlr-tot-item / tt-itens.quantidade.
    
       FIND item WHERE
            item.it-codigo = tt-itens.it-codigo NO-LOCK NO-ERROR.

       IF AVAIL item THEN
          ASSIGN tt-itens.un = ITEM.un.

       ASSIGN fi-tot-qtde = fi-tot-qtde + tt-itens.quantidade
              fi-tot-valor = fi-tot-valor + tt-itens.vlr-tot-item.
   END.
   ASSIGN tt-itens.item-ori:VISIBLE IN BROWSE br-itens = NO
          tt-itens.qtde-ori:VISIBLE IN BROWSE br-itens = NO    
          tt-itens.nr-lote:VISIBLE IN BROWSE br-itens = NO     
          tt-itens.un-ori:VISIBLE IN BROWSE br-itens = NO.      
END.

IF l-entrada THEN DO. /* Notas Fiscais de Entrada, transferidas da F†brica */
   FIND nota-fiscal WHERE
        nota-fiscal.cod-estabel = c-cod-estabel-ori AND
        nota-fiscal.serie = c-serie-ori AND
        nota-fiscal.nr-nota-fis = c-nr-nota-fis-ori
        NO-LOCK NO-ERROR.

   FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK.
       FIND item WHERE
            item.it-codigo = it-nota-fisc.it-codigo NO-LOCK NO-ERROR.

       FIND estabelec WHERE
            estabelec.cod-estabel = c-cod-estabel NO-LOCK NO-ERROR.

       FIND emitente WHERE
            emitente.cgc = estabelec.cgc NO-LOCK NO-ERROR.
       
       FIND item-fornec WHERE
            item-fornec.cod-emit = emitente.cod-emit AND
            item-fornec.it-codigo = it-nota-fisc.it-codigo NO-LOCK NO-ERROR.

       IF NOT AVAIL item-fornec THEN DO.
          MESSAGE 'Item ' it-nota-fisc.it-codigo ' n∆o Cadastrado para o Estabelecimento...'
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          ASSIGN bt-ok:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
       END.

       FIND fat-ser-lote OF it-nota-fisc NO-LOCK NO-ERROR.

       CREATE tt-itens.
       ASSIGN tt-itens.data = TODAY
              tt-itens.nr-sequencia = it-nota-fisc.nr-seq-fat
              tt-itens.item-ori = it-nota-fisc.it-codigo
              tt-itens.un-ori = it-nota-fisc.un[1]
              tt-itens.qtde-ori = it-nota-fisc.qt-faturada[1]
              tt-itens.it-codigo = IF AVAIL item-fornec
                                   THEN item-fornec.item-do-forn
                                   ELSE ""
              tt-itens.quantidade = IF AVAIL item-fornec AND
                                       item-fornec.un <> it-nota-fisc.un[1]
                                    THEN it-nota-fisc.qt-faturada[1] * 
                                         (item-fornec.fator-conver / EXP(10,item-fornec.num-casa-dec))
                                    ELSE it-nota-fisc.qt-faturada[1] 
              tt-itens.un = IF AVAIL item-fornec 
                            THEN item-fornec.un
                            ELSE "" 
              tt-itens.cod-refer = it-nota-fisc.cod-refer
              tt-itens.nr-lote = IF AVAIL fat-ser-lote 
                                 THEN fat-ser-lote.nr-serlote
                                 ELSE ""
              tt-itens.quantidade = it-nota-fisc.qt-faturada[1]
              tt-itens.valor = it-nota-fisc.vl-preuni
              tt-itens.vlr-tot-item = it-nota-fisc.vl-tot-item
              tt-itens.un = it-nota-fisc.un[1].

       ASSIGN fi-tot-qtde = fi-tot-qtde + tt-itens.quantidade
              fi-tot-valor = fi-tot-valor + tt-itens.vlr-tot-item.
   END.
END.

RUN adm-open-query-cases.

DISP fi-tot-qtde
     fi-tot-valor
     WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-recebimento B-table-Win 
PROCEDURE pi-recebimento :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-----------------------------------------------------------------------------*/
    RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
    {utp/ut-liter.i Criando_Recebimento *}
    RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

    RUN pi-acompanhar IN h-acomp (INPUT "Aguarde").
 
    IF NOT VALID-HANDLE(h-boin090) OR
       h-boin090:TYPE      <> "PROCEDURE":U OR
       h-boin090:FILE-NAME <> "inbo/boin090.p":U THEN
       RUN inbo/boin090.p PERSISTENT SET h-boin090.
    
    IF NOT VALID-HANDLE(h-boin176) OR 
       h-boin176:TYPE      <> "PROCEDURE":U OR
       h-boin176:FILE-NAME <> "inbo/boin176.p":U THEN
       RUN inbo/boin176.p PERSISTENT SET h-boin176.

    EMPTY TEMP-TABLE tt-docum-est.
    EMPTY TEMP-TABLE tt-item-doc-est. 
    EMPTY TEMP-TABLE tt-dupli-apagar. 

    EMPTY TEMP-TABLE tt-acum-itens.

    FIND estabelec WHERE
         estabelec.cod-estabel = c-cod-estabel-ori NO-LOCK NO-ERROR.

    FIND emitente WHERE
         emitente.cgc = estabelec.cgc NO-LOCK NO-ERROR.

    /* Cria Temp-Table para o Recebimento*/
    CREATE tt-docum-est.
    ASSIGN tt-docum-est.cod-emit = emitente.cod-emit
           tt-docum-est.serie-docto = c-serie-ori
           tt-docum-est.nro-docto = c-nr-nota-fis-ori
           tt-docum-est.nat-operacao = c-natur-oper-ori
           tt-docum-est.cod-estabel = c-cod-estabel
           tt-docum-est.declaracao-import = c-nr-nota-fis-ori
           tt-docum-est.dt-emissao = TODAY
           tt-docum-est.dt-trans = TODAY
           tt-docum-est.char-1 = STRING(TODAY,"99/99/9999") + STRING(TIME,"HH:MM") + 'DIGITAÄ«O RµPIDA'.

    ASSIGN i-nr-seq = 0.
    FOR EACH tt-itens WHERE
             tt-itens.cancelado = NO NO-LOCK
             BREAK BY tt-itens.it-codigo.
        FIND tt-acum-itens WHERE
             tt-acum-itens.it-codigo = tt-itens.it-codigo AND
             tt-acum-itens.cod-refer = tt-itens.cod-refer NO-ERROR.

        IF NOT AVAIL tt-acum-itens THEN DO.
           ASSIGN i-nr-seq = i-nr-seq + 10.

           CREATE tt-acum-itens.
           ASSIGN tt-acum-itens.nr-sequencia = i-nr-seq
                  tt-acum-itens.it-codigo = tt-itens.it-codigo   
                  tt-acum-itens.cod-refer = tt-itens.cod-refer    
                  tt-acum-itens.un = tt-itens.un.
        END.
        ASSIGN tt-acum-itens.quantidade = tt-acum-itens.quantidade + tt-itens.quantidade  
               tt-acum-itens.valor = tt-acum-itens.valor + tt-itens.valor       
               tt-acum-itens.vlr-tot-item = tt-acum-itens.vlr-tot-item + tt-itens.vlr-tot-item.
    END.

    FOR EACH tt-acum-itens NO-LOCK
             BREAK BY tt-acum-itens.it-codigo.

        RUN pi-acompanhar IN h-acomp (INPUT "Criando Itens: " + tt-acum-itens.it-codigo).
        FIND item WHERE
             item.it-codigo = tt-acum-itens.it-codigo NO-LOCK NO-ERROR.

        /* Cria os Itens do Recebimento */
        CREATE tt-item-doc-est.
        ASSIGN tt-item-doc-est.nro-docto      = tt-docum-est.nro-docto
               tt-item-doc-est.serie-docto    = tt-docum-est.serie-docto
               tt-item-doc-est.cod-emitente   = tt-docum-est.cod-emit
               tt-item-doc-est.nat-operacao   = tt-docum-est.nat-operacao
               tt-item-doc-est.sequencia      = tt-acum-itens.nr-sequencia
               tt-item-doc-est.it-codigo      = tt-acum-itens.it-codigo
               tt-item-doc-est.cod-refer      = tt-acum-itens.cod-refer
               tt-item-doc-est.qt-do-forn     = tt-acum-itens.quantidade
               tt-item-doc-est.preco-total[1] = tt-acum-itens.vlr-tot-item
               tt-item-doc-est.preco-unit[1]  = tt-acum-itens.valor
               tt-item-doc-est.peso-bruto     = tt-acum-itens.quantidade
               tt-item-doc-est.base-icm       = tt-acum-itens.vlr-tot-item
               tt-item-doc-est.base-ipi       = tt-acum-itens.vlr-tot-item
               tt-item-doc-est.cod-depos      = '01'
               tt-item-doc-est.dt-vali-lote   = 12.31.9999
               tt-item-doc-est.narrativa      = "Recebimento Autom†tico".
    END.
    ASSIGN tt-docum-est.tot-valor = fi-tot-valor 
           tt-docum-est.tot-peso = fi-tot-qtde
           tt-docum-est.valor-mercad = fi-tot-valor
           tt-docum-est.base-icm = fi-tot-valor  
           tt-docum-est.base-ipi = fi-tot-valor 
           tt-docum-est.despesa-nota = de-tot-despes.

    RUN pi-acompanhar IN h-acomp (INPUT "Criando Recebimento").

    /* Cria Recebimento Fiscal */
    RUN openQueryStatic IN h-boin090 (INPUT "Main":U).
    RUN emptyRowErrors IN h-boin090.
    RUN setRecord IN h-boin090 (INPUT TABLE tt-docum-est).
    RUN createRecord IN h-boin090.
    RUN getRowErrors IN h-boin090 (OUTPUT TABLE RowErrors).
    IF CAN-FIND(FIRST RowErrors 
               WHERE RowErrors.ErrorSubType = "ERROR":U  ) THEN DO:

       RUN pi-finalizar IN h-acomp.

       FOR EACH rowerrors WHERE
                RowErrors.ErrorSubType = "ERROR":U:
           MESSAGE "Erro ao Gerar o Recebimento" SKIP
                    rowerrors.errordescription 
                   VIEW-AS ALERT-BOX.
       END.
       IF VALID-HANDLE(h-boin090) THEN
          DELETE PROCEDURE h-boin090.

       IF VALID-HANDLE(h-boin176) THEN
          DELETE PROCEDURE h-boin176.

       RETURN 'ADM-ERROR'.
    END.

    RUN pi-acompanhar IN h-acomp (INPUT "Criando Itens Recebimento").

    /* Cria Itens do Recebimento*/
    RUN openQueryStatic IN h-boin176 (INPUT "Main":U).
    RUN emptyRowErrors IN h-boin176.

    FOR EACH tt-item-doc-est:
        EMPTY TEMP-TABLE wt-item-doc-est.
        CREATE wt-item-doc-est.
        BUFFER-COPY tt-item-doc-est TO wt-item-doc-est.

        RUN setRecord IN h-boin176 (INPUT TABLE wt-item-doc-est).
        RUN createRecord IN h-boin176.
    END.
    RUN getRowErrors IN h-boin176 (OUTPUT TABLE RowErrors).
    IF CAN-FIND(FIRST RowErrors 
               WHERE RowErrors.ErrorSubType = "ERROR":U  ) THEN DO:

       RUN pi-finalizar IN h-acomp.

       FOR EACH rowerrors WHERE
                RowErrors.ErrorSubType = "ERROR":U:
           MESSAGE "Erro ao Gerar os Itens do Recebimento" SKIP
                    rowerrors.errordescription 
                   VIEW-AS ALERT-BOX.
       END.
       IF VALID-HANDLE(h-boin090) THEN
          DELETE PROCEDURE h-boin090.

       IF VALID-HANDLE(h-boin176) THEN
          DELETE PROCEDURE h-boin176.

       RETURN 'ADM-ERROR'.
    END.

    RUN pi-finalizar IN h-acomp.

    IF VALID-HANDLE(h-boin090) THEN
       DELETE PROCEDURE h-boin090.
    
    IF VALID-HANDLE(h-boin176) THEN
       DELETE PROCEDURE h-boin176.
              
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

  IF AVAIL ITEM THEN
     RETURN item.desc-item.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

