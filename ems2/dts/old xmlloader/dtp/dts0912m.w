&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          mgind            PROGRESS
          movdis           PROGRESS
          xmlloader        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-dt-it-docum-est NO-UNDO LIKE dt-it-docum-est
       FIELD pRowid AS ROWID.
DEFINE TEMP-TABLE tt-it-nota-fisc NO-UNDO LIKE it-nota-fisc
       field r-rowid as rowid.
DEFINE TEMP-TABLE tt-nota-fiscal NO-UNDO LIKE nota-fiscal
       field r-rowid as rowid.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*:T*******************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
**
**  ACRA....: HARRY PFAFF JUNIOR - @HPJ - DTS Consultoria 06/02/2015
**          - AJUSTE NA CHAMADA PARA A DEFINI€ÇO DA TABELA TEMPORµRIA tt-item-devol-cli
**            EM VEZ DE DEFINIR A TABELA, USAR A INCLUDE
*******************************************************************************/
{include/i-prgvrs.i DTS0912M 11.05.12.002}
{include/i_dbvers.i}
/* Chamada a include do gerenciador de licen‡as. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */
/*                                                                                */
/* OBS: Para os smartobjects o parametro m¢dulo dever  ser MUT                    */


/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
{inbo/boin176.i4 tt-item-devol-cli}
/* def temp-table tt-item-devol-cli no-undo                           */
/*     field rw-it-nota-fisc   as rowid                               */
/*     field quant-devol       like item-doc-est.quantidade           */
/*     field preco-devol       like item-doc-est.preco-total extent 0 */
/*     field cod-depos         like item-doc-est.cod-depos            */
/*     field reabre-pd         like item-doc-est.reabre-pd            */
/*     field vl-desconto       like item-doc-est.pr-total-cmi         */
/*     FIELD .                                                        */
/* {inbo/boin176.i4 tt-item-devol-cli} */
{dibo/bodi515.i tt-nota-fisc-adc}
/* Parameters Definitions ---                                           */
DEFINE INPUT        PARAMETER pNatureza  LIKE natur-oper.nat-operacao.
DEFINE INPUT        PARAMETER pRowidNF   AS ROWID NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tt-dt-it-docum-est.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tt-item-devol-cli.
DEFINE INPUT        PARAMETER pReabrePed AS LOGICAL NO-UNDO.
DEFINE OUTPUT       PARAMETER pOK        AS LOGICAL NO-UNDO.

/* Local Variable Definitions ---                                       */
define variable de-qtd-devol          as dec  no-undo.
define variable de-quant-dev          as dec  no-undo.
define variable pcAction              as char no-undo.
define variable h-bodi088na-devdet    as handle no-undo.
define variable h-boin176-devdet      as handle no-undo.
define variable h-boin245na-devdet    as handle no-undo.
define variable h-bodi135na-devdet    as handle no-undo.
define variable h-bodi515-devdet      as handle no-undo.
define variable de-indice             as dec    no-undo.
define variable l-desc                as log    no-undo.
define variable c-base-icm            as char   no-undo.
define variable i-base-st             as int    no-undo.
define variable c-char-2              as char   no-undo.
define variable iCodEmitente          as int    no-undo.
define variable cCodEstabel           as char   no-undo.
define variable cserie-docto          as char   no-undo.
define variable cnro-docto            as char   no-undo.
define variable cnat-operacao         as char   no-undo.
DEFINE VARIABLE h-boin090-devdet      AS HANDLE NO-UNDO.
define variable l-initialized  as log     no-undo.
&if "{&mguni_version}" >= "2.07" &then
    define variable c-narrativa           as char   no-undo.
    define variable h-boin172desc-devdet  as handle no-undo.
    define variable h-bodi128a-devdet     as handle no-undo.
&endif

define variable i-cod-emitente    like docum-est.cod-emitente no-undo.
define variable c-serie-docto     like docum-est.serie-docto  no-undo.
define variable c-nro-docto       like docum-est.nro-docto    no-undo.
define variable c-nat-operacao    like docum-est.nat-operacao no-undo.

DEFINE TEMP-TABLE tt2-item-devol-cli NO-UNDO LIKE tt-item-devol-cli 
    FIELD pItemXML  AS CHAR 
    FIELD pItemNF   AS CHARACTER
    FIELD pRowidItXML AS ROWID.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog
&Scoped-define BROWSE-NAME br-composicao

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt2-item-devol-cli tt-it-nota-fisc ITEM ~
tt-dt-it-docum-est tt-nota-fiscal

/* Definitions for BROWSE br-composicao                                 */
&Scoped-define FIELDS-IN-QUERY-br-composicao tt-it-nota-fisc.nr-seq-fat tt-it-nota-fisc.it-codigo item.desc-item tt2-item-devol-cli.quant-devol tt2-item-devol-cli.preco-devol tt2-item-devol-cli.cod-depos   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-composicao   
&Scoped-define SELF-NAME br-composicao
&Scoped-define QUERY-STRING-br-composicao FOR EACH tt2-item-devol-cli NO-LOCK, ~
                                   FIRST tt-it-nota-fisc WHERE ROWID(tt-it-nota-fisc) = tt2-item-devol-cli.rw-it-nota-fisc NO-LOCK, ~
                                   FIRST ITEM WHERE item.it-codigo = tt-it-nota-fisc.it-codigo INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-composicao OPEN QUERY {&SELF-NAME} FOR EACH tt2-item-devol-cli NO-LOCK, ~
                                   FIRST tt-it-nota-fisc WHERE ROWID(tt-it-nota-fisc) = tt2-item-devol-cli.rw-it-nota-fisc NO-LOCK, ~
                                   FIRST ITEM WHERE item.it-codigo = tt-it-nota-fisc.it-codigo INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-composicao tt2-item-devol-cli ~
tt-it-nota-fisc ITEM
&Scoped-define FIRST-TABLE-IN-QUERY-br-composicao tt2-item-devol-cli
&Scoped-define SECOND-TABLE-IN-QUERY-br-composicao tt-it-nota-fisc
&Scoped-define THIRD-TABLE-IN-QUERY-br-composicao ITEM


/* Definitions for BROWSE br-item-devolucao                             */
&Scoped-define FIELDS-IN-QUERY-br-item-devolucao tt-it-nota-fisc.nr-seq-fat ~
tt-it-nota-fisc.it-codigo item.desc-item tt-it-nota-fisc.qt-faturada[1] ~
tt-it-nota-fisc.vl-tot-item fn-qtd-devol() @ de-quant-dev 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-item-devolucao 
&Scoped-define QUERY-STRING-br-item-devolucao FOR EACH tt-it-nota-fisc NO-LOCK, ~
      EACH item OF tt-it-nota-fisc NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-item-devolucao OPEN QUERY br-item-devolucao FOR EACH tt-it-nota-fisc NO-LOCK, ~
      EACH item OF tt-it-nota-fisc NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-item-devolucao tt-it-nota-fisc item
&Scoped-define FIRST-TABLE-IN-QUERY-br-item-devolucao tt-it-nota-fisc
&Scoped-define SECOND-TABLE-IN-QUERY-br-item-devolucao item


/* Definitions for BROWSE br-item-rece                                  */
&Scoped-define FIELDS-IN-QUERY-br-item-rece tt-dt-it-docum-est.sequencia ~
tt-dt-it-docum-est.item-ems item.desc-item tt-dt-it-docum-est.quantidade 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-item-rece 
&Scoped-define QUERY-STRING-br-item-rece FOR EACH tt-dt-it-docum-est NO-LOCK, ~
      EACH item WHERE item.it-codigo = tt-dt-it-docum-est.item-ems ~
      AND item.tipo-con-est <> 1 OUTER-JOIN NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-item-rece OPEN QUERY br-item-rece FOR EACH tt-dt-it-docum-est NO-LOCK, ~
      EACH item WHERE item.it-codigo = tt-dt-it-docum-est.item-ems ~
      AND item.tipo-con-est <> 1 OUTER-JOIN NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-item-rece tt-dt-it-docum-est item
&Scoped-define FIRST-TABLE-IN-QUERY-br-item-rece tt-dt-it-docum-est
&Scoped-define SECOND-TABLE-IN-QUERY-br-item-rece item


/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define QUERY-STRING-D-Dialog FOR EACH tt-nota-fiscal SHARE-LOCK
&Scoped-define OPEN-QUERY-D-Dialog OPEN QUERY D-Dialog FOR EACH tt-nota-fiscal SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-D-Dialog tt-nota-fiscal
&Scoped-define FIRST-TABLE-IN-QUERY-D-Dialog tt-nota-fiscal


/* Definitions for FRAME f-devolucao                                    */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-devolucao ~
    ~{&OPEN-QUERY-br-composicao}~
    ~{&OPEN-QUERY-br-item-devolucao}~
    ~{&OPEN-QUERY-br-item-rece}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-buttom bt-ok bt-cancela bt-ajuda 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-qtd-devol D-Dialog 
FUNCTION fn-qtd-devol RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ajuda 
     LABEL "&Ajuda" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-cancela AUTO-END-KEY 
     LABEL "&Cancelar" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "&OK" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 89.86 BY 1.42
     BGCOLOR 7 .

DEFINE BUTTON bt-add 
     IMAGE-UP FILE "image/im-adduni.bmp":U
     LABEL "Adiciona" 
     SIZE 11 BY 1.13 TOOLTIP "Adiciona Sele‡Æo".

DEFINE BUTTON bt-erase 
     IMAGE-UP FILE "image/im-remuni.bmp":U
     LABEL "Remove" 
     SIZE 11 BY 1.13 TOOLTIP "Remove Sele‡Æo".

DEFINE RECTANGLE rtKeys
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89.72 BY 2.33.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-composicao FOR 
      tt2-item-devol-cli, 
      tt-it-nota-fisc, 
      ITEM SCROLLING.

DEFINE QUERY br-item-devolucao FOR 
      tt-it-nota-fisc, 
      item SCROLLING.

DEFINE QUERY br-item-rece FOR 
      tt-dt-it-docum-est, 
      item SCROLLING.

DEFINE QUERY D-Dialog FOR 
      tt-nota-fiscal SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-composicao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-composicao D-Dialog _FREEFORM
  QUERY br-composicao NO-LOCK DISPLAY
      tt-it-nota-fisc.nr-seq-fat
tt-it-nota-fisc.it-codigo
item.desc-item
tt2-item-devol-cli.quant-devol
tt2-item-devol-cli.preco-devol
tt2-item-devol-cli.cod-depos
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 87 BY 4.5
         FONT 1
         TITLE "Itens Devolvidos" FIT-LAST-COLUMN.

DEFINE BROWSE br-item-devolucao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-item-devolucao D-Dialog _STRUCTURED
  QUERY br-item-devolucao NO-LOCK DISPLAY
      tt-it-nota-fisc.nr-seq-fat FORMAT ">>,>>9":U WIDTH 5.43
      tt-it-nota-fisc.it-codigo FORMAT "x(16)":U
      item.desc-item FORMAT "x(60)":U WIDTH 31.43
      tt-it-nota-fisc.qt-faturada[1] FORMAT ">>>>,>>9.9999":U
      tt-it-nota-fisc.vl-tot-item FORMAT ">,>>>,>>>,>>9.99":U WIDTH 11.29
      fn-qtd-devol() @ de-quant-dev COLUMN-LABEL "Qt J  Devolvida"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 87 BY 4.5
         FONT 1
         TITLE "Item Faturado".

DEFINE BROWSE br-item-rece
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-item-rece D-Dialog _STRUCTURED
  QUERY br-item-rece NO-LOCK DISPLAY
      tt-dt-it-docum-est.sequencia COLUMN-LABEL "Seq" FORMAT ">>>>>>>>9":U
            WIDTH 4.43
      tt-dt-it-docum-est.item-ems COLUMN-LABEL "Item" FORMAT "x(16)":U
            WIDTH 14.43
      item.desc-item FORMAT "x(60)":U WIDTH 44.86
      tt-dt-it-docum-est.quantidade FORMAT "->>,>>9.99":U WIDTH 18.29
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 87 BY 4.5
         FONT 1
         TITLE "Item XML" FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     bt-ok AT ROW 21.58 COL 2.14
     bt-cancela AT ROW 21.58 COL 13.14
     bt-ajuda AT ROW 21.58 COL 80.57
     rt-buttom AT ROW 21.33 COL 1.14
     SPACE(0.28) SKIP(0.41)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 1
         TITLE "Itens Devolu‡Æo"
         DEFAULT-BUTTON bt-ok WIDGET-ID 100.

DEFINE FRAME f-devolucao
     tt-nota-fiscal.cod-estabel AT ROW 1.17 COL 20.72 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 5 BY .88
     tt-nota-fiscal.nr-nota-fis AT ROW 1.17 COL 52.14 COLON-ALIGNED WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 12.57 BY .88
     tt-nota-fiscal.serie AT ROW 2.17 COL 20.72 COLON-ALIGNED WIDGET-ID 14
          VIEW-AS FILL-IN 
          SIZE 7 BY .88
     tt-nota-fiscal.dt-emis-nota AT ROW 2.17 COL 52.14 COLON-ALIGNED WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 12.57 BY .88
     br-item-rece AT ROW 4.21 COL 2 WIDGET-ID 200
     br-item-devolucao AT ROW 8.92 COL 2 WIDGET-ID 400
     bt-add AT ROW 13.71 COL 20 WIDGET-ID 2
     bt-erase AT ROW 13.71 COL 57 WIDGET-ID 4
     br-composicao AT ROW 15.04 COL 2 WIDGET-ID 500
     rtKeys AT ROW 1 COL 1 WIDGET-ID 12
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 19.75
         FONT 1 WIDGET-ID 300.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: tt-dt-it-docum-est T "?" NO-UNDO xmlloader dt-it-docum-est
      ADDITIONAL-FIELDS:
          FIELD pRowid AS ROWID
      END-FIELDS.
      TABLE: tt-it-nota-fisc T "?" NO-UNDO movdis it-nota-fisc
      ADDITIONAL-FIELDS:
          field r-rowid as rowid
      END-FIELDS.
      TABLE: tt-nota-fiscal T "?" NO-UNDO movdis nota-fiscal
      ADDITIONAL-FIELDS:
          field r-rowid as rowid
      END-FIELDS.
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/d-dialog.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* REPARENT FRAME */
ASSIGN FRAME f-devolucao:FRAME = FRAME D-Dialog:HANDLE.

/* SETTINGS FOR DIALOG-BOX D-Dialog
   NOT-VISIBLE FRAME-NAME L-To-R                                        */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME f-devolucao
                                                                        */
/* BROWSE-TAB br-item-rece dt-emis-nota f-devolucao */
/* BROWSE-TAB br-item-devolucao br-item-rece f-devolucao */
/* BROWSE-TAB br-composicao bt-erase f-devolucao */
/* SETTINGS FOR FILL-IN tt-nota-fiscal.cod-estabel IN FRAME f-devolucao
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tt-nota-fiscal.dt-emis-nota IN FRAME f-devolucao
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tt-nota-fiscal.nr-nota-fis IN FRAME f-devolucao
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tt-nota-fiscal.serie IN FRAME f-devolucao
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-composicao
/* Query rebuild information for BROWSE br-composicao
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt2-item-devol-cli NO-LOCK,
                            FIRST tt-it-nota-fisc WHERE ROWID(tt-it-nota-fisc) = tt2-item-devol-cli.rw-it-nota-fisc NO-LOCK,
                            FIRST ITEM WHERE item.it-codigo = tt-it-nota-fisc.it-codigo INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-composicao */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-item-devolucao
/* Query rebuild information for BROWSE br-item-devolucao
     _TblList          = "Temp-Tables.tt-it-nota-fisc,mgind.item OF Temp-Tables.tt-it-nota-fisc"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.tt-it-nota-fisc.nr-seq-fat
"tt-it-nota-fisc.nr-seq-fat" ? ? "integer" ? ? ? ? ? ? no ? no no "5.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   = Temp-Tables.tt-it-nota-fisc.it-codigo
     _FldNameList[3]   > mgind.item.desc-item
"item.desc-item" ? ? "character" ? ? ? ? ? ? no ? no no "31.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   = Temp-Tables.tt-it-nota-fisc.qt-faturada[1]
     _FldNameList[5]   > Temp-Tables.tt-it-nota-fisc.vl-tot-item
"tt-it-nota-fisc.vl-tot-item" ? ? "decimal" ? ? ? ? ? ? no ? no no "11.29" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"fn-qtd-devol() @ de-quant-dev" "Qt J  Devolvida" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE br-item-devolucao */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-item-rece
/* Query rebuild information for BROWSE br-item-rece
     _TblList          = "Temp-Tables.tt-dt-it-docum-est,mgind.item WHERE Temp-Tables.tt-dt-it-docum-est ..."
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _TblOptList       = ", OUTER"
     _JoinCode[2]      = "mgind.item.it-codigo = Temp-Tables.tt-dt-it-docum-est.item-ems"
     _Where[2]         = "mgind.item.tipo-con-est <> 1"
     _FldNameList[1]   > Temp-Tables.tt-dt-it-docum-est.sequencia
"tt-dt-it-docum-est.sequencia" "Seq" ? "integer" ? ? ? ? ? ? no ? no no "4.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.tt-dt-it-docum-est.item-ems
"tt-dt-it-docum-est.item-ems" "Item" ? "character" ? ? ? ? ? ? no ? no no "14.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > mgind.item.desc-item
"item.desc-item" ? ? "character" ? ? ? ? ? ? no ? no no "44.86" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.tt-dt-it-docum-est.quantidade
"tt-dt-it-docum-est.quantidade" ? ? "decimal" ? ? ? ? ? ? no ? no no "18.29" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE br-item-rece */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _TblList          = "Temp-Tables.tt-nota-fiscal"
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Itens Devolu‡Æo */
DO:  
  
    /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-item-devolucao
&Scoped-define FRAME-NAME f-devolucao
&Scoped-define SELF-NAME br-item-devolucao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-item-devolucao D-Dialog
ON ROW-DISPLAY OF br-item-devolucao IN FRAME f-devolucao /* Item Faturado */
DO:
/*     FIND FIRST tt2-item-devol-cli                                                           */
/*         WHERE tt2-item-devol-cli.rw-it-nota-fisc = ROWID(tt-it-nota-fisc) NO-LOCK NO-ERROR. */
/*     IF AVAIL tt2-item-devol-cli THEN DO:                                                    */
/*     END.                                                                                    */
/*         MESSAGE "OK" SKIP                                                                   */
/*             tt-it-nota-fisc.it-codigo                                                       */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK.                                              */
  IF CAN-FIND(FIRST tt2-item-devol-cli
              WHERE tt2-item-devol-cli.rw-it-nota-fisc = ROWID(tt-it-nota-fisc) NO-LOCK ) THEN DO:
        ASSIGN tt-it-nota-fisc.nr-seq-fat     :fgcolor in browse br-item-devolucao = 9
               tt-it-nota-fisc.it-codigo      :fgcolor in browse br-item-devolucao = 9
               item.desc-item                 :fgcolor in browse br-item-devolucao = 9
               tt-it-nota-fisc.qt-faturada[1] :fgcolor in browse br-item-devolucao = 9
               tt-it-nota-fisc.vl-tot-item    :fgcolor in browse br-item-devolucao = 9
               de-quant-dev                   :fgcolor in browse br-item-devolucao = 9.







  END.
  ELSE DO:
      ASSIGN tt-it-nota-fisc.nr-seq-fat     :fgcolor in browse br-item-devolucao = 0
             tt-it-nota-fisc.it-codigo      :fgcolor in browse br-item-devolucao = 0
             item.desc-item                 :fgcolor in browse br-item-devolucao = 0
             tt-it-nota-fisc.qt-faturada[1] :fgcolor in browse br-item-devolucao = 0
             tt-it-nota-fisc.vl-tot-item    :fgcolor in browse br-item-devolucao = 0
             de-quant-dev                   :fgcolor in browse br-item-devolucao = 0.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-item-rece
&Scoped-define SELF-NAME br-item-rece
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-item-rece D-Dialog
ON ROW-DISPLAY OF br-item-rece IN FRAME f-devolucao /* Item XML */
DO:
    IF CAN-FIND(FIRST tt2-item-devol-cli
                WHERE tt2-item-devol-cli.pRowidItXML     = ROWID(tt-dt-it-docum-est) NO-LOCK ) THEN DO:

          ASSIGN tt-dt-it-docum-est.sequencia   :fgcolor in browse br-item-rece = 9
                 tt-dt-it-docum-est.item-ems    :fgcolor in browse br-item-rece = 9
                 item.desc-item                 :fgcolor in browse br-item-rece = 9
                 tt-dt-it-docum-est.quantidade  :fgcolor in browse br-item-rece = 9.







    END.
    ELSE
        ASSIGN tt-dt-it-docum-est.sequencia   :fgcolor in browse br-item-rece = 0
               tt-dt-it-docum-est.item-ems    :fgcolor in browse br-item-rece = 0
               item.desc-item                 :fgcolor in browse br-item-rece = 0
               tt-dt-it-docum-est.quantidade  :fgcolor in browse br-item-rece = 0.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-add D-Dialog
ON CHOOSE OF bt-add IN FRAME f-devolucao /* Adiciona */
DO:
    br-item-rece:FETCH-SELECTED-ROW(1) IN FRAME f-devolucao.
    br-item-devolucao:FETCH-SELECTED-ROW(1) IN FRAME f-devolucao.
    FIND FIRST tt2-item-devol-cli
        WHERE tt2-item-devol-cli.pItemXML = tt-dt-it-docum-est.it-codigo
           OR tt2-item-devol-cli.pItemNF  = tt-it-nota-fisc.it-codigo       NO-ERROR.
    IF AVAIL tt2-item-devol-cli THEN DO:
        RUN utp/ut-msgs.p (INPUT 'SHOW':U,
                           INPUT 17006,
                           INPUT 'Devolu‡Æo Item' + "~~" + 'Os itens selecionados j  estÆo associados.').
        RETURN "NOK".
        

    END.
    ELSE DO:
        IF tt-dt-it-docum-est.quantidade > tt-it-nota-fisc.qt-faturada[1] THEN DO:
            RUN utp/ut-msgs.p (INPUT 'SHOW':U,
                               INPUT 17006,
                               INPUT 'Devolu‡Æo Item' + "~~" + 'Quantidade devolvida informada no XML ‚ superior … quantidade faturada! ').
            RETURN "NOK".
            
        END.
        CREATE tt2-item-devol-cli.
        ASSIGN tt2-item-devol-cli.pItemXML        = tt-dt-it-docum-est.it-codigo 
               tt2-item-devol-cli.pItemNF         = tt-it-nota-fisc.it-codigo    
               tt2-item-devol-cli.rw-it-nota-fisc = ROWID(tt-it-nota-fisc)         
               tt2-item-devol-cli.quant-devol     = tt-dt-it-docum-est.quantidade  
               tt2-item-devol-cli.preco-devol     = tt-dt-it-docum-est.preco-total 
               tt2-item-devol-cli.cod-depos       = tt-it-nota-fisc.cod-depos      
               tt2-item-devol-cli.reabre-pd       = pReabrePed
               tt2-item-devol-cli.pRowidItXML     = ROWID(tt-dt-it-docum-est).                    

    END.
    {&open-query-br-composicao}
/*    FIND CURRENT tt-dt-it-docum-est NO-ERROR.
    FIND CURRENT tt-it-nota-fisc NO-LOCK NO-ERROR. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME D-Dialog
&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda D-Dialog
ON CHOOSE OF bt-ajuda IN FRAME D-Dialog /* Ajuda */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cancela
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancela D-Dialog
ON CHOOSE OF bt-cancela IN FRAME D-Dialog /* Cancelar */
DO:
  ASSIGN pOK = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-devolucao
&Scoped-define SELF-NAME bt-erase
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-erase D-Dialog
ON CHOOSE OF bt-erase IN FRAME f-devolucao /* Remove */
DO:
    br-composicao:FETCH-SELECTED-ROW(1) IN FRAME f-devolucao.  
    IF AVAILABLE tt2-item-devol-cli THEN 
        DELETE tt2-item-devol-cli.
    {&open-query-br-composicao}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME D-Dialog
&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok D-Dialog
ON CHOOSE OF bt-ok IN FRAME D-Dialog /* OK */
DO:
  FOR EACH tt2-item-devol-cli NO-LOCK:
      FIND FIRST tt-it-nota-fisc
          WHERE rowid(tt-it-nota-fisc) = tt2-item-devol-cli.rw-it-nota-fisc NO-LOCK NO-ERROR.
      FIND FIRST tt-dt-it-docum-est
          WHERE ROWID(tt-dt-it-docum-est) = tt2-item-devol-cli.pRowidItXML EXCLUSIVE-LOCK NO-ERROR.
      IF AVAIL tt-dt-it-docum-est THEN DO:
          ASSIGN tt-dt-it-docum-est.item-ems       = tt-it-nota-fisc.it-codigo
                 tt-dt-it-docum-est.serie-comp     = tt-it-nota-fisc.serie
                 tt-dt-it-docum-est.nro-comp       = tt-it-nota-fisc.nr-nota-fis
                 tt-dt-it-docum-est.nat-comp       = tt-it-nota-fisc.nat-operacao 
                 tt-dt-it-docum-est.seq-comp       = tt-it-nota-fisc.nr-seq-fat
/*                  tt-dt-it-docum-est.nr-pedcli      = tt-it-nota-fisc.nr-pedcli     */
/*                  tt-dt-it-docum-est.data-comp      = tt-it-nota-fisc.dt-emis-nota  */
/*                  tt-dt-it-docum-est.baixa-ce       = tt-it-nota-fisc.baixa-estoq   */
                 tt-dt-it-docum-est.class-fiscal   = tt-it-nota-fisc.class-fiscal.
      END.
  END.
  RUN destroyDBOs.
  ASSIGN pOK = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-composicao
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

{src/adm/template/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects D-Dialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available D-Dialog  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DestroyDBOs D-Dialog 
PROCEDURE DestroyDBOs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    if  valid-handle( h-boin090-devdet ) then do:
        delete procedure h-boin090-devdet.
        assign h-boin090-devdet = ?.
    end.
    /*--- Elimina o handle da BO de notas-fiscais ---*/
    if  valid-handle( h-bodi135na-devdet ) then do:
        delete procedure h-bodi135na-devdet.
        assign h-bodi135na-devdet = ?.
    end.


    /*--- Elimina o handle da BO de itens da nota-fiscal ---*/
    if  valid-handle( h-bodi088na-devdet ) then do:
        delete procedure h-bodi088na-devdet.
        assign h-bodi088na-devdet = ?.
    end.    

    /*--- Elimina o handle da BO de itens da nota-fiscal ---*/
    if  valid-handle( h-bodi515-devdet ) then do:
        delete procedure h-bodi515-devdet.
        assign h-bodi515-devdet = ?.
    end.

    &if "{&mguni_version}" >= "2.07" &then
        if  valid-handle( h-boin172desc-devdet ) then do:
            delete procedure h-boin172desc-devdet.
            assign h-boin172desc-devdet = ?.
        end.

        if  valid-handle( h-bodi128a-devdet ) then do:
            delete procedure h-bodi128a-devdet.
            assign h-bodi128a-devdet = ?.
        end.
    &endif

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI D-Dialog  _DEFAULT-DISABLE
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
  HIDE FRAME D-Dialog.
  HIDE FRAME f-devolucao.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI D-Dialog  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  ENABLE rt-buttom bt-ok bt-cancela bt-ajuda 
      WITH FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
  IF AVAILABLE tt-nota-fiscal THEN 
    DISPLAY tt-nota-fiscal.cod-estabel tt-nota-fiscal.nr-nota-fis 
          tt-nota-fiscal.serie tt-nota-fiscal.dt-emis-nota 
      WITH FRAME f-devolucao.
  ENABLE rtKeys br-item-rece br-item-devolucao bt-add bt-erase br-composicao 
      WITH FRAME f-devolucao.
  {&OPEN-BROWSERS-IN-QUERY-f-devolucao}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeDBOs D-Dialog 
PROCEDURE InitializeDBOs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF NOT VALID-HANDLE(h-boin090-devdet) THEN
        RUN inbo/boin090.p PERSISTENT SET h-boin090-devdet.
    if  not valid-handle( h-bodi135na-devdet ) then 
        run dibo/bodi135na.p persistent set h-bodi135na-devdet.
    
    RUN setConstraintNotaFiscal IN h-bodi135na-devdet (INPUT pRowidNF).
    run openQueryStatic         in h-bodi135na-devdet ( input "NotaFiscal":U ).
    run repositionRecord        in h-bodi135na-devdet ( input pRowidNF ).
    run getRecord               in h-bodi135na-devdet ( output table tt-nota-fiscal ).

    find first tt-nota-fiscal no-lock no-error.

    if  not valid-handle( h-bodi088na-devdet ) then 
        run dibo/bodi088na.p persistent set h-bodi088na-devdet.

    if  not valid-handle( h-boin245na-devdet ) then 
        run inbo/boin245na.p persistent set h-boin245na-devdet.


    &if "{&mguni_version}" >= "2.07" &then
        if  not valid-handle ( h-bodi128a-devdet ) then
            run dibo/bodi128a.p persistent set h-bodi128a-devdet.

        if  not valid-handle ( h-boin172desc-devdet ) then
            run inbo/boin172desc.p persistent set h-boin172desc-devdet.

        run openQueryStatic in h-boin172desc-devdet (input "Main":u) no-error.
        run openQueryStatic in h-bodi128a-devdet (input "Main":u) no-error.
    &endif

    RUN setConstraintDefault IN h-boin245na-devdet NO-ERROR.
    RUN openQueryStatic      IN h-boin245na-devdet (INPUT "Main":U) NO-ERROR.    

    return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy D-Dialog 
PROCEDURE local-destroy :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .
  {include/i-logfin.i}

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize D-Dialog 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  {utp/ut9000.i "DTS0912M" "11.05.12.002"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .
  EMPTY TEMP-TABLE tt2-item-devol-cli.
  /* Code placed here will execute AFTER standard behavior.    */
  RUN InitializeDBOs.
  {&open-query-br-item-rece}
  if  avail tt-nota-fiscal then do:
      assign tt-nota-fiscal.cod-estabel:screen-value  in frame f-devolucao = tt-nota-fiscal.cod-estabel
             tt-nota-fiscal.serie:screen-value        in frame f-devolucao = tt-nota-fiscal.serie
             tt-nota-fiscal.nr-nota-fis:screen-value  in frame f-devolucao = tt-nota-fiscal.nr-nota-fis
             tt-nota-fiscal.dt-emis-nota:screen-value in frame f-devolucao = string(tt-nota-fiscal.dt-emis-nota).
  
      run openQueriesSon in this-procedure.
      FOR EACH tt-dt-it-docum-est 
          WHERE tt-dt-it-docum-est.serie-comp = tt-nota-fiscal.serie
            AND tt-dt-it-docum-est.nro-comp   = tt-nota-fiscal.nr-nota-fis
            AND tt-dt-it-docum-est.nat-comp   = tt-nota-fiscal.nat-operacao NO-LOCK:
            FIND FIRST tt-it-nota-fisc 
                 WHERE tt-it-nota-fisc.serie        = tt-dt-it-docum-est.serie-comp
                   and tt-it-nota-fisc.nr-nota-fis  = tt-dt-it-docum-est.nro-comp  
                   and tt-it-nota-fisc.nat-operacao = tt-dt-it-docum-est.nat-comp  
                   and tt-it-nota-fisc.nr-seq-fat   = tt-dt-it-docum-est.seq-comp NO-LOCK NO-ERROR.
            IF AVAIL tt-it-nota-fisc THEN DO:
                CREATE tt2-item-devol-cli.
                ASSIGN tt2-item-devol-cli.pItemXML        = tt-dt-it-docum-est.it-codigo 
                       tt2-item-devol-cli.pItemNF         = tt-it-nota-fisc.it-codigo    
                       tt2-item-devol-cli.rw-it-nota-fisc = ROWID(tt-it-nota-fisc)         
                       tt2-item-devol-cli.quant-devol     = tt-dt-it-docum-est.quantidade  
                       tt2-item-devol-cli.preco-devol     = tt-dt-it-docum-est.preco-total 
                       tt2-item-devol-cli.cod-depos       = tt-it-nota-fisc.cod-depos      
                       tt2-item-devol-cli.reabre-pd       = pReabrePed
                       tt2-item-devol-cli.pRowidItXML     = ROWID(tt-dt-it-docum-est).                    
            END.
      END.
  end.     
  APPLY "entry" TO br-item-rece IN FRAME f-devolucao.
  {&open-query-br-composicao}
/*   FOR EACH tt2-item-devol-cli NO-LOCK:                                                        */
/*       FIND FIRST tt-it-nota-fisc                                                              */
/*           WHERE rowid(tt-it-nota-fisc) = tt2-item-devol-cli.rw-it-nota-fisc NO-LOCK NO-ERROR. */
/*       IF AVAIL tt-it-nota-fisc THEN                                                           */
/*           MESSAGE tt-it-nota-fisc.it-codigo                                                   */
/*               VIEW-AS ALERT-BOX INFO BUTTONS OK.                                              */
/*   END.                                                                                        */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE openQueriesSon D-Dialog 
PROCEDURE openQueriesSon :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    def var iRowsReturned as int no-undo.
    def var h-buffer      as handle no-undo.

    run setConstraintOfNotaFiscal in h-bodi088na-devdet ( tt-nota-fiscal.cod-estabel,
                                                   tt-nota-fiscal.serie,
                                                   tt-nota-fiscal.nr-nota-fis ).

    run openQueryStatic in h-bodi088na-devdet ( input "OfNotaFiscal":U ).
    run getBatchRecords in h-bodi088na-devdet ( input ?,
                                         input no,
                                         input 999,
                                         output iRowsReturned,
                                         output table tt-it-nota-fisc ).


    open query br-item-devolucao for each tt-it-nota-fisc NO-LOCK,
                                     EACH item OF tt-it-nota-fisc NO-LOCK .

    return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-VerificaDesconto D-Dialog 
PROCEDURE pi-VerificaDesconto :
/*------------------------------------------------------------------------------
  Purpose:     Verifica se existe desconto para a nota de venda e se os impostos
               foram calculados pelo Bruto.
  Parameters:  nenhum.
  Notes:       Procedure utilizado pela Fun‡Æo FN-QTD-DEVOL e pelo Leave do
               campo TT-IT-NOTA-FISC.DEC-1.
------------------------------------------------------------------------------*/

   /* Verifica se existe desconto para a nota de venda e se os impostos foram calculado pelo Bruto */
   run goToKey in h-boin245na-devdet ( input tt-it-nota-fisc.nat-operacao ).

   if  return-value = "OK":U then do:

       run getIntField in h-boin245na-devdet ( input "merc-base-icms":U, output c-base-icm ).

       run getCharField in h-boin245na-devdet ( input "char-2":U, output c-char-2 ).
   end.

   run findUnidFederEstab in h-boin090-devdet ( input  tt-it-nota-fisc.cod-estabel,
                                         output i-base-st ).

   if  (   substr(c-char-2,11,1) = "1"            /* IPI pelo Bruto */
        or c-base-icm    = "1"                    /* ICMS pelo Bruto */
        or (    i-base-st = 1                     /*Subs. Trib. Bruto*/
            and tt-it-nota-fisc.vl-icmsub-it > 0  /* Valor do ICMS Subs. */
            and tt-it-nota-fisc.ind-icm-ret) )    /*Item da saida teve subs. trib.*/
   and (   tt-it-nota-fisc.val-pct-desconto-total > 0
        or tt-it-nota-fisc.val-desconto-total     > 0 ) then
       assign l-desc = yes.
   else
       assign l-desc = no.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records D-Dialog  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-nota-fiscal"}
  {src/adm/template/snd-list.i "tt-dt-it-docum-est"}
  {src/adm/template/snd-list.i "item"}
  {src/adm/template/snd-list.i "tt-it-nota-fisc"}
  {src/adm/template/snd-list.i "tt2-item-devol-cli"}
  {src/adm/template/snd-list.i "tt-it-nota-fisc"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed D-Dialog 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
  
  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-qtd-devol D-Dialog 
FUNCTION fn-qtd-devol RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

   run pi-VerificaDesconto.

    IF VALID-HANDLE(h-bodi088na-devdet) THEN
     RUN returnQtdADev IN  h-bodi088na-devdet( input h-boin090-devdet, 
                                        input tt-it-nota-fisc.cod-estabel,
                                        input tt-it-nota-fisc.nr-nota-fis,
                                        input tt-it-nota-fisc.serie,
                                        input tt-it-nota-fisc.nr-seq-fat,
                                        input tt-it-nota-fisc.it-codigo,
                                        OUTPUT de-qtd-devol).


    assign de-indice =     IF   tt-it-nota-fisc.ind-fat-qtfam = NO THEN 1
                           ELSE tt-it-nota-fisc.qt-faturada[2] / tt-it-nota-fisc.qt-faturada[1]
                       
           tt-it-nota-fisc.dec-1 = if   l-initialized then tt-it-nota-fisc.dec-1
                                   else tt-it-nota-fisc.qt-faturada[1] - de-qtd-devol.

/*     assign de-indice = &IF "{&bf_dis_versao_ems}" >= "2.06" &THEN                                   */
/*                            IF   tt-it-nota-fisc.ind-fat-qtfam = NO THEN 1                           */
/*                            ELSE tt-it-nota-fisc.qt-faturada[2] / tt-it-nota-fisc.qt-faturada[1]     */
/*                        &ELSE                                                                        */
/*                            if tt-it-nota-fisc.ind-fat-qtfam                                         */
/*                                THEN tt-it-nota-fisc.qt-faturada[2] / tt-it-nota-fisc.qt-faturada[1] */
/*                            ELSE 1                                                                   */
/*                        &ENDIF                                                                       */
/*            tt-it-nota-fisc.dec-1 = if   l-initialized then tt-it-nota-fisc.dec-1                    */
/*                                    else tt-it-nota-fisc.qt-faturada[1] - de-qtd-devol.              */

   if  l-desc then do:
       assign tt-it-nota-fisc.dec-2       = tt-it-nota-fisc.vl-preori * (tt-it-nota-fisc.dec-1 * de-indice).

       if tt-it-nota-fisc.qt-faturada[1] = tt-it-nota-fisc.dec-1 then /*devolu‡Æo total*/
           assign tt-it-nota-fisc.vl-desconto = tt-it-nota-fisc.vl-merc-ori - tt-it-nota-fisc.vl-merc-liq.
       else
           assign tt-it-nota-fisc.vl-desconto = ((tt-it-nota-fisc.vl-preori - tt-it-nota-fisc.vl-preuni) 
                                           * (tt-it-nota-fisc.dec-1 * de-indice)).
   end.
   else         
       assign tt-it-nota-fisc.dec-2       = tt-it-nota-fisc.vl-preuni * (tt-it-nota-fisc.dec-1 * de-indice)
              tt-it-nota-fisc.vl-desconto = 0.

   RETURN de-qtd-devol.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

