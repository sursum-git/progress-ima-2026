&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          espec            PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i V01ES047 2.04.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
&Scop adm-attribute-dlg support/viewerd.w

/* global variable definitions */

/* Parameters Definitions ---                                           */
DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.
DEF NEW GLOBAL SHARED VAR gc-cod-estabel AS CHAR NO-UNDO.

DEF BUFFER b-ordem-benefic FOR ordem-benefic.

/* Define Temp Tables */
DEF TEMP-TABLE tt-work
    FIELD periodo      AS CHAR  FORMAT "x(6)"
    FIELD corte-comerc AS CHAR 
    FIELD nr-lote      LIKE ob-etiqueta.nr-lote
    FIELD qtd          LIKE ob-etiqueta.quantidade
    INDEX indice1 periodo corte-comerc.

DEF TEMP-TABLE tt-corte
    FIELD corte-comerc AS CHAR 
    FIELD qtd          LIKE ob-etiqueta.quantidade.

DEF TEMP-TABLE tt-estoque
    FIELD corte-comerc LIKE corte-comerc.codigo
    FIELD nr-lote      LIKE ob-etiqueta.nr-lote
    FIELD it-codigo    LIKE ob-etiqueta.it-codigo
    FIELD cod-refer    LIKE ob-etiqueta.cod-refer
    FIELD qt-estoque   LIKE ob-etiqueta.quantidade  COLUMN-LABEL "Estoque"
    FIELD qt-carteira  LIKE ped-item.qt-pedida      COLUMN-LABEL "Carteira"
    INDEX indice1 it-codigo
                  cod-refer
                  corte-comerc
                  nr-lote.

DEF TEMP-TABLE tt-carteira
    FIELD corte-comerc LIKE corte-comerc.codigo
    FIELD nr-lote      LIKE ob-etiqueta.nr-lote
    FIELD it-codigo    LIKE ob-etiqueta.it-codigo
    FIELD cod-refer    LIKE ob-etiqueta.cod-refer
    FIELD periodo      AS CHAR  FORMAT "x(6)"
    FIELD qt-estoque   LIKE ob-etiqueta.quantidade  COLUMN-LABEL "Estoque"
    FIELD qt-carteira  LIKE ped-item.qt-pedida      COLUMN-LABEL "Carteira"
    FIELD row-pedidos  AS CHAR
    INDEX indice1 periodo
                  corte-comerc
                  nr-lote
                  it-codigo
                  cod-refer.

/* Variaveis globais para abrir o essp0150 posicionado no item da OB*/
DEF NEW SHARED VAR p-it-codigo-150 AS CHAR.
DEF NEW SHARED VAR p-cod-refer-150 AS CHAR.
DEF NEW SHARED VAR p-lote-rp-150   AS LOG.

/* Definiá‰es para Imagens */
DEFINE TEMP-TABLE tt-fotos NO-UNDO
       FIELD arq-image AS CHAR.

DEF VAR c-arq-image AS CHAR.
DEF VAR c-comando AS CHAR.

/* Local Variable Definitions ---                                       */
DEF VAR v-row-parent      AS ROWID  NO-UNDO.
DEF VAR h-acomp           AS HANDLE NO-UNDO.
DEF VAR i-ct              AS INT.
DEF VAR c-corte           AS CHAR FORMAT "x(5)".
DEF VAR c-mensagem        AS CHAR.
DEF VAR c-arq-email       AS CHAR.
DEF VAR c-empresa         AS CHAR.
DEF VAR c-destinatario    AS CHAR.
DEF VAR i-ct-lin          AS INT.
DEF VAR i-ct-col          AS INT.
DEF VAR i-ct-acond        AS INT.
DEF VAR i-ind-ob          AS INT.
DEF VAR c-composicao      LIKE composi.descricao EXTENT 3.
DEF VAR c-item-cru        LIKE saldo-estoq.it-codigo.
DEF VAR de-sld-estoque    LIKE saldo-estoq.qtidade-atu.
DEF VAR i-num-bar         AS INT.
DEF VAR i-nr-seq          LIKE ob-etiqueta.nr-sequencia.
DEF VAR de-tot-carteira   AS DEC.
DEF VAR de-tot-sobra      AS DEC.
DEF VAR de-tot-acond      AS DEC.
DEF VAR de-tot-trans      AS DEC.
DEF VAR de-tot-export     AS DEC.
DEF VAR de-tot-emb-neutra AS DEC.
DEF VAR de-tot-revisao    AS DEC.
DEF VAR c-tear            AS CHAR.
DEF VAR c-cod-estabel     AS CHAR INITIAL "2".
DEF VAR c-lotes           AS CHAR INITIAL "PP RP".
DEF VAR c-periodo         AS CHAR.
DEF VAR c-num-pcp         AS CHAR.
DEF VAR l-ok              AS LOG.
DEF VAR l-gravou-macro    AS LOG.

{esinc/sz-pcl.i}
{include/tt-edit.i}
{include/pi-edit.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES ordem-benefic
&Scoped-define FIRST-EXTERNAL-TABLE ordem-benefic


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR ordem-benefic.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS ordem-benefic.it-codigo ~
ordem-benefic.cod-refer ordem-benefic.quantidade ordem-benefic.un ~
ordem-benefic.observacao ordem-benefic.situacao ordem-benefic.num-progr 
&Scoped-define ENABLED-TABLES ordem-benefic
&Scoped-define FIRST-ENABLED-TABLE ordem-benefic
&Scoped-Define ENABLED-OBJECTS bt-anl-gerencial RECT-1 RECT-2 rt-key ~
rt-mold 
&Scoped-Define DISPLAYED-FIELDS ordem-benefic.cod-estabel ~
ordem-benefic.nr-ob ordem-benefic.nr-carro ordem-benefic.tipo-ordem ~
ordem-benefic.responsavel ordem-benefic.it-codigo ordem-benefic.cod-refer ~
ordem-benefic.dt-ob ordem-benefic.quantidade ordem-benefic.un ~
ordem-benefic.observacao ordem-benefic.situacao ordem-benefic.num-progr 
&Scoped-define DISPLAYED-TABLES ordem-benefic
&Scoped-define FIRST-DISPLAYED-TABLE ordem-benefic
&Scoped-Define DISPLAYED-OBJECTS fi-desc-item fi-desc-refer cb-tipo-tear ~
fi-nr-trf fi-acondic1 fi-qt-acondic1 fi-acondic2 fi-qt-acondic2 fi-acondic3 ~
fi-qt-acondic3 fi-acondic4 fi-qt-acondic4 fi-acondic5 fi-qt-acondic5 ~
fi-acondic6 fi-qt-acondic6 fi-acondic7 fi-qt-acondic7 fi-acondic8 ~
fi-qt-acondic8 fi-acondic9 fi-qt-acondic9 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-CREATE-FIELDS ordem-benefic.cod-estabel ~
ordem-benefic.nr-ob ordem-benefic.nr-carro ordem-benefic.tipo-ordem ~
ordem-benefic.dt-ob 
&Scoped-define ADM-ASSIGN-FIELDS ordem-benefic.cod-estabel ~
ordem-benefic.nr-ob ordem-benefic.nr-carro ordem-benefic.responsavel ~
ordem-benefic.dt-ob ordem-benefic.num-progr 
&Scoped-define List-4 ordem-benefic.it-codigo ordem-benefic.cod-refer ~
ordem-benefic.quantidade ordem-benefic.un 
&Scoped-define List-5 cb-tipo-tear 
&Scoped-define List-6 fi-acondic1 fi-qt-acondic1 fi-acondic2 fi-qt-acondic2 ~
fi-acondic3 fi-qt-acondic3 fi-acondic4 fi-qt-acondic4 fi-acondic5 ~
fi-qt-acondic5 fi-acondic6 fi-qt-acondic6 fi-acondic7 fi-qt-acondic7 ~
fi-acondic8 fi-qt-acondic8 fi-acondic9 fi-qt-acondic9 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS> 
<EXECUTING-CODE>
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


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-anl-gerencial 
     IMAGE-UP FILE "image/im-150.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Consulta Gerencial do Estoque"
     BGCOLOR 8 .

DEFINE BUTTON bt-carteira 
     IMAGE-UP FILE "N:/image/im-consulta.bmp":U
     LABEL "Button 2" 
     SIZE 4 BY 1 TOOLTIP "Consulta Carteira de Pedidos por Corte".

DEFINE BUTTON bt-desenho 
     IMAGE-UP FILE "image/im-show.bmp":U
     LABEL "" 
     SIZE 5.43 BY 1.04 TOOLTIP "Visualiza Imagem do Desenho"
     BGCOLOR 8 .

DEFINE VARIABLE cb-tipo-tear AS CHARACTER FORMAT "X(9)":U 
     LABEL "Tipo do Tear" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 8
     LIST-ITEMS "NISSAN"," ","SULZER","PICANOL","TSUDAKOMA","TOYOTA" 
     DROP-DOWN-LIST
     SIZE 14 BY 1.

DEFINE VARIABLE fi-acondic1 AS CHARACTER FORMAT "X(17)" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88.

DEFINE VARIABLE fi-acondic2 AS CHARACTER FORMAT "X(17)" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88.

DEFINE VARIABLE fi-acondic3 AS CHARACTER FORMAT "X(17)" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88.

DEFINE VARIABLE fi-acondic4 AS CHARACTER FORMAT "X(17)" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88.

DEFINE VARIABLE fi-acondic5 AS CHARACTER FORMAT "X(17)" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88.

DEFINE VARIABLE fi-acondic6 AS CHARACTER FORMAT "X(17)" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88.

DEFINE VARIABLE fi-acondic7 AS CHARACTER FORMAT "X(17)" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88.

DEFINE VARIABLE fi-acondic8 AS CHARACTER FORMAT "X(17)" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88.

DEFINE VARIABLE fi-acondic9 AS CHARACTER FORMAT "X(17)" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88.

DEFINE VARIABLE fi-desc-item AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 60 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-refer AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 26.14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nr-trf AS INTEGER FORMAT ">>>>>>>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 NO-UNDO.

DEFINE VARIABLE fi-qt-acondic1 AS INTEGER FORMAT ">>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88.

DEFINE VARIABLE fi-qt-acondic2 AS INTEGER FORMAT ">>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88.

DEFINE VARIABLE fi-qt-acondic3 AS INTEGER FORMAT ">>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88.

DEFINE VARIABLE fi-qt-acondic4 AS INTEGER FORMAT ">>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88.

DEFINE VARIABLE fi-qt-acondic5 AS INTEGER FORMAT ">>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88.

DEFINE VARIABLE fi-qt-acondic6 AS INTEGER FORMAT ">>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88.

DEFINE VARIABLE fi-qt-acondic7 AS INTEGER FORMAT ">>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88.

DEFINE VARIABLE fi-qt-acondic8 AS INTEGER FORMAT ">>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88.

DEFINE VARIABLE fi-qt-acondic9 AS INTEGER FORMAT ">>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 26.57 BY 9.75.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 86.57 BY 1.25.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89 BY 3.25.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89 BY 13.25.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     ordem-benefic.cod-estabel AT ROW 1.17 COL 14.29 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 4 BY .88
     ordem-benefic.nr-ob AT ROW 1.17 COL 39.29 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY .88
     ordem-benefic.nr-carro AT ROW 2.17 COL 39.29 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5 BY .88
     ordem-benefic.tipo-ordem AT ROW 1.17 COL 60.86 NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Produá∆o", 1,
"Retrabalho", 2,
"Transformaá∆o", 3,
"Industrializaá∆o", 4
          SIZE 13 BY 3
     ordem-benefic.responsavel AT ROW 3.17 COL 14.29 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 30 BY .88
     ordem-benefic.it-codigo AT ROW 4.75 COL 14.29 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY .88
     fi-desc-item AT ROW 4.75 COL 27 COLON-ALIGNED NO-LABEL
     ordem-benefic.cod-refer AT ROW 5.75 COL 14.29 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY .88
     fi-desc-refer AT ROW 5.75 COL 26.86 COLON-ALIGNED NO-LABEL
     ordem-benefic.dt-ob AT ROW 2.17 COL 14.29 COLON-ALIGNED
          LABEL "Data de Emiss∆o"
          VIEW-AS FILL-IN 
          SIZE 12 BY .88 NO-TAB-STOP 
     ordem-benefic.quantidade AT ROW 6.75 COL 14.29 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY .88
     ordem-benefic.un AT ROW 6.75 COL 23.86 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4 BY .88 NO-TAB-STOP 
     bt-carteira AT ROW 6.71 COL 30.43 WIDGET-ID 6
     cb-tipo-tear AT ROW 7.83 COL 44.57 COLON-ALIGNED
     fi-nr-trf AT ROW 2.5 COL 75 COLON-ALIGNED NO-LABEL
     ordem-benefic.observacao AT ROW 9.5 COL 2 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 59 BY 6.25
          BGCOLOR 8 FGCOLOR 12 FONT 6
     fi-acondic1 AT ROW 6.54 COL 61.29 COLON-ALIGNED NO-LABEL
     fi-qt-acondic1 AT ROW 6.54 COL 80 COLON-ALIGNED NO-LABEL
     fi-acondic2 AT ROW 7.54 COL 61.29 COLON-ALIGNED NO-LABEL
     fi-qt-acondic2 AT ROW 7.54 COL 80 COLON-ALIGNED NO-LABEL
     fi-acondic3 AT ROW 8.54 COL 61.29 COLON-ALIGNED NO-LABEL
     fi-qt-acondic3 AT ROW 8.54 COL 80 COLON-ALIGNED NO-LABEL
     fi-acondic4 AT ROW 9.54 COL 61.29 COLON-ALIGNED NO-LABEL
     fi-qt-acondic4 AT ROW 9.54 COL 80 COLON-ALIGNED NO-LABEL
     fi-acondic5 AT ROW 10.54 COL 61.29 COLON-ALIGNED NO-LABEL
     fi-qt-acondic5 AT ROW 10.54 COL 80 COLON-ALIGNED NO-LABEL
     fi-acondic6 AT ROW 11.54 COL 61.29 COLON-ALIGNED NO-LABEL
     fi-qt-acondic6 AT ROW 11.54 COL 80 COLON-ALIGNED NO-LABEL
     fi-acondic7 AT ROW 12.54 COL 61.29 COLON-ALIGNED NO-LABEL
     fi-qt-acondic7 AT ROW 12.54 COL 80 COLON-ALIGNED NO-LABEL
     fi-acondic8 AT ROW 13.54 COL 61.29 COLON-ALIGNED NO-LABEL
     fi-qt-acondic8 AT ROW 13.54 COL 80 COLON-ALIGNED NO-LABEL
     fi-acondic9 AT ROW 14.54 COL 61.29 COLON-ALIGNED NO-LABEL
     fi-qt-acondic9 AT ROW 14.54 COL 80 COLON-ALIGNED NO-LABEL
     ordem-benefic.situacao AT ROW 16.25 COL 2.86 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Dispon°vel", 1,
"Em Revis∆o", 2,
"Revis∆o Parcial", 3,
"Revis∆o Total", 4,
"Reportado", 5
          SIZE 85.14 BY .75
          FGCOLOR 9 FONT 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 1.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME f-main
     ordem-benefic.num-progr AT ROW 7.75 COL 14.14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8.29 BY .88
     bt-desenho AT ROW 5.71 COL 55 WIDGET-ID 4
     bt-anl-gerencial AT ROW 6.71 COL 35 WIDGET-ID 8
     " Acondicionamentos" VIEW-AS TEXT
          SIZE 17.86 BY .75 AT ROW 5.67 COL 63.14
          FONT 6
     "Observaá‰es" VIEW-AS TEXT
          SIZE 11.72 BY .71 AT ROW 8.75 COL 2.29
          FONT 6
     "Tipo Ordem:" VIEW-AS TEXT
          SIZE 9 BY .54 AT ROW 1.25 COL 52
     RECT-1 AT ROW 6.04 COL 62
     RECT-2 AT ROW 16 COL 2
     rt-key AT ROW 1 COL 1
     rt-mold AT ROW 4.25 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: espec.ordem-benefic
   Allow: Basic,DB-Fields
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
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 16.71
         WIDTH              = 89.57.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
{include/c-viewer.i}
{utp/ut-glob.i}
{include/i_dbtype.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-main
   NOT-VISIBLE FRAME-NAME Size-to-Fit Custom                            */
ASSIGN 
       FRAME f-main:SCROLLABLE       = FALSE
       FRAME f-main:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON bt-carteira IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-desenho IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX cb-tipo-tear IN FRAME f-main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN ordem-benefic.cod-estabel IN FRAME f-main
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN ordem-benefic.cod-refer IN FRAME f-main
   4                                                                    */
/* SETTINGS FOR FILL-IN ordem-benefic.dt-ob IN FRAME f-main
   NO-ENABLE 1 2 EXP-LABEL                                              */
/* SETTINGS FOR FILL-IN fi-acondic1 IN FRAME f-main
   NO-ENABLE 6                                                          */
/* SETTINGS FOR FILL-IN fi-acondic2 IN FRAME f-main
   NO-ENABLE 6                                                          */
/* SETTINGS FOR FILL-IN fi-acondic3 IN FRAME f-main
   NO-ENABLE 6                                                          */
/* SETTINGS FOR FILL-IN fi-acondic4 IN FRAME f-main
   NO-ENABLE 6                                                          */
/* SETTINGS FOR FILL-IN fi-acondic5 IN FRAME f-main
   NO-ENABLE 6                                                          */
/* SETTINGS FOR FILL-IN fi-acondic6 IN FRAME f-main
   NO-ENABLE 6                                                          */
/* SETTINGS FOR FILL-IN fi-acondic7 IN FRAME f-main
   NO-ENABLE 6                                                          */
/* SETTINGS FOR FILL-IN fi-acondic8 IN FRAME f-main
   NO-ENABLE 6                                                          */
/* SETTINGS FOR FILL-IN fi-acondic9 IN FRAME f-main
   NO-ENABLE 6                                                          */
/* SETTINGS FOR FILL-IN fi-desc-item IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-refer IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nr-trf IN FRAME f-main
   NO-ENABLE                                                            */
ASSIGN 
       fi-nr-trf:HIDDEN IN FRAME f-main           = TRUE.

/* SETTINGS FOR FILL-IN fi-qt-acondic1 IN FRAME f-main
   NO-ENABLE 6                                                          */
/* SETTINGS FOR FILL-IN fi-qt-acondic2 IN FRAME f-main
   NO-ENABLE 6                                                          */
/* SETTINGS FOR FILL-IN fi-qt-acondic3 IN FRAME f-main
   NO-ENABLE 6                                                          */
/* SETTINGS FOR FILL-IN fi-qt-acondic4 IN FRAME f-main
   NO-ENABLE 6                                                          */
/* SETTINGS FOR FILL-IN fi-qt-acondic5 IN FRAME f-main
   NO-ENABLE 6                                                          */
/* SETTINGS FOR FILL-IN fi-qt-acondic6 IN FRAME f-main
   NO-ENABLE 6                                                          */
/* SETTINGS FOR FILL-IN fi-qt-acondic7 IN FRAME f-main
   NO-ENABLE 6                                                          */
/* SETTINGS FOR FILL-IN fi-qt-acondic8 IN FRAME f-main
   NO-ENABLE 6                                                          */
/* SETTINGS FOR FILL-IN fi-qt-acondic9 IN FRAME f-main
   NO-ENABLE 6                                                          */
/* SETTINGS FOR FILL-IN ordem-benefic.it-codigo IN FRAME f-main
   4                                                                    */
/* SETTINGS FOR FILL-IN ordem-benefic.nr-carro IN FRAME f-main
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN ordem-benefic.nr-ob IN FRAME f-main
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN ordem-benefic.num-progr IN FRAME f-main
   2                                                                    */
/* SETTINGS FOR FILL-IN ordem-benefic.quantidade IN FRAME f-main
   4                                                                    */
/* SETTINGS FOR FILL-IN ordem-benefic.responsavel IN FRAME f-main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR RADIO-SET ordem-benefic.tipo-ordem IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN ordem-benefic.un IN FRAME f-main
   4                                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-main
/* Query rebuild information for FRAME f-main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME f-main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME bt-anl-gerencial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-anl-gerencial V-table-Win
ON CHOOSE OF bt-anl-gerencial IN FRAME f-main
DO:
   ASSIGN FRAME F-Main:WINDOW:SENSITIVE = NO.
   ASSIGN p-it-codigo-150 = ITEM.it-codigo
          p-cod-refer-150 = referencia.cod-refer
          p-lote-rp-150 = YES.

   RUN esp/essp0150.w "SHARED".
   ASSIGN FRAME F-Main:WINDOW:SENSITIVE = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-carteira
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-carteira V-table-Win
ON CHOOSE OF bt-carteira IN FRAME f-main /* Button 2 */
DO:
  IF adm-new-record AND 
     INPUT FRAME {&FRAME-NAME} ordem-benefic.quantidade > 0 THEN DO.

     RUN esp/essp0100a.w (INPUT-OUTPUT TABLE tt-carteira,
                          INPUT-OUTPUT TABLE tt-work,
                          INPUT ordem-benefic.nr-ob:SCREEN-VALUE,
                          INPUT ordem-benefic.it-codigo:SCREEN-VALUE,
                          INPUT ITEM.desc-item,
                          INPUT ordem-benefic.cod-refer:SCREEN-VALUE,
                          INPUT ordem-benefic.quantidade:SCREEN-VALUE,
                          OUTPUT l-ok).
     IF l-ok = NO THEN
        RETURN NO-APPLY.

     RUN pi-limpa-acond.

     EMPTY TEMP-TABLE tt-corte.
     FOR EACH tt-work NO-LOCK.
         CREATE tt-corte.
         ASSIGN tt-corte.corte-comerc = tt-work.corte-comerc
                tt-corte.qtd = tt-work.qtd.
     END.

     ASSIGN i-ct-acond = 0.
     FOR EACH tt-corte.
         FIND corte-comerc WHERE 
              corte-comerc.codigo = tt-corte.corte-comerc NO-LOCK NO-ERROR.
         IF NOT AVAIL corte-comerc THEN NEXT.

         ASSIGN i-ct-acond = i-ct-acond + 1.

         RUN pi-mostra-acond(INPUT i-ct-acond,
                             INPUT corte-comerc.descricao,
                             INPUT STRING(INT(tt-corte.qtd / corte-comerc.compr-med))).
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-desenho
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-desenho V-table-Win
ON CHOOSE OF bt-desenho IN FRAME f-main
DO:
   RUN esdlg/d01-desenho.w (INPUT referencia.cod-refer).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ordem-benefic.cod-estabel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ordem-benefic.cod-estabel V-table-Win
ON LEAVE OF ordem-benefic.cod-estabel IN FRAME f-main /* Estabelecimento */
DO:
  FIND estabelec WHERE
       estabelec.cod-estabel = INPUT FRAME {&FRAME-NAME} ordem-benefic.cod-estabel NO-LOCK NO-ERROR.

  IF NOT AVAIL estabelec THEN DO.
     MESSAGE "Estabelecimento n∆o Cadastrado..." VIEW-AS ALERT-BOX.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ordem-benefic.cod-estabel V-table-Win
ON MOUSE-SELECT-DBLCLICK OF ordem-benefic.cod-estabel IN FRAME f-main /* Estabelecimento */
DO:
  {include/zoomvar.i &prog-zoom=adzoom/z01ad107.w
                     &campo=ordem-benefic.cod-estabel
                     &campozoom=cod-estabel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ordem-benefic.cod-refer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ordem-benefic.cod-refer V-table-Win
ON ENTRY OF ordem-benefic.cod-refer IN FRAME f-main /* Referància */
DO:
  IF SELF:SCREEN-VALUE <> "" THEN DO.
     FIND referencia WHERE
          referencia.cod-refer = INPUT FRAME {&FRAME-NAME} ordem-benefic.cod-refer
          NO-LOCK NO-ERROR.
     IF AVAIL referencia THEN
        ASSIGN fi-desc-refer:SCREEN-VALUE IN FRAME {&FRAME-NAME} = referencia.descricao.
  END.
  ELSE ASSIGN fi-desc-refer:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ordem-benefic.cod-refer V-table-Win
ON LEAVE OF ordem-benefic.cod-refer IN FRAME f-main /* Referància */
DO:
  IF KEY-FUNCTION(LASTKEY) = 'BACK-TAB' THEN DO.
     APPLY 'BACK-TAB' TO SELF.
     RETURN NO-APPLY.
  END.

  FIND referencia WHERE
       referencia.cod-refer = INPUT FRAME {&FRAME-NAME} ordem-benefic.cod-refer 
       NO-LOCK NO-ERROR.

  IF NOT AVAIL referencia THEN DO.
     MESSAGE 'Referància N∆o encontrada...'
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     APPLY 'ENTRY' TO SELF.
     RETURN NO-APPLY.
  END.
  ASSIGN fi-desc-refer:SCREEN-VALUE IN FRAME {&FRAME-NAME} = referencia.descricao.

  IF INPUT FRAME {&FRAME-NAME} ordem-benefic.tipo-ordem = 1 THEN DO.
     RUN pi-ver-pcp.
     IF RETURN-VALUE = 'ADM-ERROR' THEN DO.
        APPLY 'ENTRY' TO SELF.
        RETURN NO-APPLY.
     END.
  END.
  
  ASSIGN bt-anl-gerencial:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

  RUN pi-ver-desenho.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ordem-benefic.cod-refer V-table-Win
ON MOUSE-SELECT-DBLCLICK OF ordem-benefic.cod-refer IN FRAME f-main /* Referància */
DO:
  {include/zoomvar.i &prog-zoom  = inzoom/z01in375.w
                     &campo      = ordem-benefic.cod-refer
                     &campozoom  = cod-refer
                     &parametros = "run pi-seta-inicial in wh-pesquisa(INPUT INPUT FRAME {&FRAME-NAME} ordem-benefic.it-codigo)."}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ordem-benefic.cod-refer V-table-Win
ON VALUE-CHANGED OF ordem-benefic.cod-refer IN FRAME f-main /* Referància */
DO:
  IF SELF:SCREEN-VALUE <> "" THEN DO.
     FIND referencia WHERE
          referencia.cod-refer = INPUT FRAME {&FRAME-NAME} ordem-benefic.cod-refer NO-LOCK NO-ERROR.
     IF AVAIL referencia THEN 
        ASSIGN fi-desc-refer:SCREEN-VALUE IN FRAME {&FRAME-NAME} = referencia.descricao.
     ELSE
        ASSIGN fi-desc-refer:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-acondic1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-acondic1 V-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-acondic1 IN FRAME f-main
DO:
    {include/zoomvar.i &prog-zoom = eszoom/z01es065.w
                       &campo     = fi-acondic1
                       &campozoom = descricao}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-acondic2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-acondic2 V-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-acondic2 IN FRAME f-main
DO:
  {include/zoomvar.i &prog-zoom = eszoom/z01es065.w
                     &campo     = fi-acondic2
                     &campozoom = descricao}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-acondic3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-acondic3 V-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-acondic3 IN FRAME f-main
DO:
  {include/zoomvar.i &prog-zoom = eszoom/z01es065.w
                     &campo     = fi-acondic3
                     &campozoom = descricao}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-acondic4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-acondic4 V-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-acondic4 IN FRAME f-main
DO:
  {include/zoomvar.i &prog-zoom = eszoom/z01es065.w
                     &campo     = fi-acondic4
                     &campozoom = descricao}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-acondic5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-acondic5 V-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-acondic5 IN FRAME f-main
DO:
  {include/zoomvar.i &prog-zoom = eszoom/z01es065.w
                     &campo     = fi-acondic5
                     &campozoom = descricao}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-acondic6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-acondic6 V-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-acondic6 IN FRAME f-main
DO:
  {include/zoomvar.i &prog-zoom = eszoom/z01es065.w
                     &campo     = fi-acondic6
                     &campozoom = descricao}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-acondic7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-acondic7 V-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-acondic7 IN FRAME f-main
DO:
  {include/zoomvar.i &prog-zoom = eszoom/z01es065.w
                     &campo     = fi-acondic7
                     &campozoom = descricao}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-acondic8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-acondic8 V-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-acondic8 IN FRAME f-main
DO:
  {include/zoomvar.i &prog-zoom = eszoom/z01es065.w
                     &campo     = fi-acondic8
                     &campozoom = descricao}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-acondic9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-acondic9 V-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-acondic9 IN FRAME f-main
DO:
  {include/zoomvar.i &prog-zoom = eszoom/z01es065.w
                     &campo     = fi-acondic9
                     &campozoom = descricao}
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-nr-trf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nr-trf V-table-Win
ON LEAVE OF fi-nr-trf IN FRAME f-main
DO:
   IF LOOKUP(KEYFUNCTION(LAST-KEY),"TAB,RETURN") > 0 AND 
      INPUT FRAME {&FRAME-NAME} fi-nr-trf <> 0 THEN DO. 

      FIND ob-trf WHERE 
           ob-trf.num-trf = INPUT FRAME {&FRAME-NAME} fi-nr-trf NO-LOCK NO-ERROR.
      IF NOT AVAIL ob-trf THEN DO:
         MESSAGE "Codigo Transformaá∆o n∆o cadastrado ! ! !"
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
         APPLY 'entry' TO fi-nr-trf.
         RETURN NO-APPLY.
      END.

      IF AVAIL b-ordem-benefic THEN DO.
         IF b-ordem-benefic.it-codigo <> ob-trf.it-codigo OR
            b-ordem-benefic.cod-refer <> ob-trf.cod-refer THEN DO.
            MESSAGE "Item/Referencia da OB " b-ordem-benefic.it-codigo " " b-ordem-benefic.cod-refer SKIP
                    " Ç diferente do Item/Referencia da Transformaá∆o" ob-trf.it-codigo " " ob-trf.cod-refer
                     VIEW-AS ALERT-BOX INFO BUTTONS OK.
            APPLY 'entry' TO fi-nr-trf.
            RETURN NO-APPLY.
         END.
      END.

      FIND FIRST b-ordem-benefic WHERE
                 b-ordem-benefic.num-trf = INPUT FRAME {&FRAME-NAME} fi-nr-trf NO-LOCK NO-ERROR.
      IF AVAIL b-ordem-benefic THEN DO.
         MESSAGE "Codigo Transformaá∆o j† relacionado em outra OB..."
                   VIEW-AS ALERT-BOX INFO BUTTONS OK.
         APPLY 'entry' TO fi-nr-trf.
         RETURN NO-APPLY.
      END.

      IF INPUT FRAME {&FRAME-NAME} ordem-benefic.tipo-ordem = 2 AND
         SUBSTR(ob-trf.char-1,1,1) = '' THEN DO.
         MESSAGE "Transformaá∆o n∆o Ç de Retrabalho"
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
         APPLY 'entry' TO fi-nr-trf.
         RETURN NO-APPLY.
      END.

      IF INPUT FRAME {&FRAME-NAME} ordem-benefic.tipo-ordem = 3 AND
         SUBSTR(ob-trf.char-1,1,1) = 'S' THEN DO.
         MESSAGE "Transformaá∆o Ç de Retrabalho"
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
         APPLY 'entry' TO fi-nr-trf.
         RETURN NO-APPLY.
      END.
    
      IF SUBSTR(ob-trf.char-1,1,1) = "" THEN DO.  /* N∆o Ç Retrabalho */
         ASSIGN de-tot-trans = 0.
         FOR EACH ob-etq-trf WHERE 
                  ob-etq-trf.num-trf = ob-trf.num-trf NO-LOCK.
             FIND ob-etiqueta WHERE
                  ob-etiqueta.cod-estabel  = ob-etq-trf.cod-estabel AND
                  ob-etiqueta.num-etiqueta = ob-etq-trf.num-etiqueta NO-LOCK NO-ERROR.
             IF AVAIL ob-etiqueta THEN
                ASSIGN de-tot-trans = de-tot-trans + ob-etiqueta.quantidade.
         END.

         ASSIGN ordem-benefic.quantidade:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(de-tot-trans).
      END.
      ELSE   /* ê Retrabalho */
         ASSIGN ordem-benefic.quantidade:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ob-trf.dec-1).
    
      FIND item WHERE 
           item.it-codigo = ob-trf.it-codigo  NO-LOCK NO-ERROR.
      IF AVAIL item THEN 
         ASSIGN ordem-benefic.it-codigo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ITEM.it-codigo
                fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ITEM.desc-item
                ordem-benefic.un:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ITEM.un.
    
      FIND referencia WHERE
           referencia.cod-refer = ob-trf.cod-refer NO-LOCK NO-ERROR.
      IF AVAIL referencia THEN
         ASSIGN ordem-benefic.cod-refer:SCREEN-VALUE IN FRAME {&FRAME-NAME} = referencia.cod-refer
                fi-desc-refer:SCREEN-VALUE IN FRAME {&FRAME-NAME}           = referencia.descricao.
    

      FIND corte-comerc WHERE
           corte-comerc.codigo = ob-trf.corte-comerc NO-LOCK NO-ERROR.
      IF AVAIL corte-comerc THEN
         ASSIGN fi-acondic1:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = corte-comerc.descricao
                fi-qt-acondic1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ROUND(de-tot-trans / corte-comerc.compr-med,0)).

      APPLY "entry" TO fi-acondic1.
      RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nr-trf V-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-nr-trf IN FRAME f-main
DO:
    {include/zoomvar.i &prog-zoom=eszoom\z01es072.w
                       &campo=fi-nr-trf
                       &campozoom=num-trf}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ordem-benefic.it-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ordem-benefic.it-codigo V-table-Win
ON ENTRY OF ordem-benefic.it-codigo IN FRAME f-main /* Item */
DO:
   IF SELF:SCREEN-VALUE <> "" THEN DO.
      FIND item WHERE
           item.it-codigo = INPUT FRAME {&FRAME-NAME} ordem-benefic.it-codigo
           NO-LOCK NO-ERROR.
      IF AVAIL item THEN
         ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
   END.
   ELSE ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".

   ASSIGN ordem-benefic.cod-refer:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ordem-benefic.it-codigo V-table-Win
ON LEAVE OF ordem-benefic.it-codigo IN FRAME f-main /* Item */
DO:
  FIND item WHERE
       item.it-codigo = INPUT FRAME {&FRAME-NAME} ordem-benefic.it-codigo 
       NO-LOCK NO-ERROR.

  IF AVAIL item THEN 
     ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item
            ordem-benefic.un:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.un.

  IF item.tipo-con-est <> 4 THEN
     ASSIGN ordem-benefic.cod-refer:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ordem-benefic.it-codigo V-table-Win
ON MOUSE-SELECT-DBLCLICK OF ordem-benefic.it-codigo IN FRAME f-main /* Item */
DO:
  {include/zoomvar.i &prog-zoom=inzoom/z01in172.w
                     &campo=ordem-benefic.it-codigo
                     &campozoom=it-codigo}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ordem-benefic.it-codigo V-table-Win
ON VALUE-CHANGED OF ordem-benefic.it-codigo IN FRAME f-main /* Item */
DO:
  IF SELF:SCREEN-VALUE <> "" THEN DO.
     FIND item WHERE
          item.it-codigo = INPUT FRAME {&FRAME-NAME} ordem-benefic.it-codigo NO-LOCK NO-ERROR.
     IF AVAIL item THEN 
        ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
     ELSE
        ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ordem-benefic.nr-carro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ordem-benefic.nr-carro V-table-Win
ON ANY-KEY OF ordem-benefic.nr-carro IN FRAME f-main /* Carro */
DO:
  IF KEYFUNCTION(LASTKEY) = "" THEN 
     RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ordem-benefic.nr-carro V-table-Win
ON LEAVE OF ordem-benefic.nr-carro IN FRAME f-main /* Carro */
DO:
   ASSIGN SELF:SCREEN-VALUE = UPPER(SELF:SCREEN-VALUE). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ordem-benefic.nr-carro V-table-Win
ON VALUE-CHANGED OF ordem-benefic.nr-carro IN FRAME f-main /* Carro */
DO:
  IF LENGTH(SELF:SCREEN-VALUE) = 2 AND
     KEYFUNCTION(LASTKEY) >= "a" AND
     KEYFUNCTION(LASTKEY) <= "Z" THEN DO.
     ASSIGN SELF:SCREEN-VALUE = '0' + UPPER(SELF:SCREEN-VALUE). 
     APPLY 'entry' TO ordem-benefic.tipo-ordem.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ordem-benefic.nr-ob
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ordem-benefic.nr-ob V-table-Win
ON LEAVE OF ordem-benefic.nr-ob IN FRAME f-main /* N£mero da OB */
DO:
   ASSIGN ordem-benefic.tipo-ordem:SCREEN-VALUE = '1'.
   RUN pi-busca-ob.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ordem-benefic.quantidade
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ordem-benefic.quantidade V-table-Win
ON LEAVE OF ordem-benefic.quantidade IN FRAME f-main /* Quantidade */
DO:
    IF KEY-FUNCTION(LASTKEY) = 'BACK-TAB' THEN DO.
       APPLY 'BACK-TAB' TO SELF.
       RETURN NO-APPLY.
    END.
    
    IF adm-new-record THEN DO.
       IF ordem-benefic.quantidade:INPUT-VALUE <= 0 THEN RETURN NO-APPLY.

       ASSIGN de-tot-export = 0
              de-tot-emb-neutra = 0.

       RUN pi-calc-carteira.

       IF de-tot-export <> 0 THEN
          ASSIGN ordem-benefic.observacao:SCREEN-VALUE = "Exportaá∆o: " + STRING(de-tot-export, ">>>,>>>,>>9.99").
       IF de-tot-emb-neutra <> 0 THEN
          ASSIGN ordem-benefic.observacao:SCREEN-VALUE = ordem-benefic.observacao:SCREEN-VALUE + CHR(13) + "Embalagem Neutra: " + STRING(de-tot-emb-neutra, ">>>,>>>,>>9.99"). 
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ordem-benefic.quantidade V-table-Win
ON VALUE-CHANGED OF ordem-benefic.quantidade IN FRAME f-main /* Quantidade */
DO:
   ASSIGN bt-carteira:SENSITIVE = SELF:INPUT-VALUE > 0.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ordem-benefic.tipo-ordem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ordem-benefic.tipo-ordem V-table-Win
ON LEAVE OF ordem-benefic.tipo-ordem IN FRAME f-main /* Tipo */
DO:
  IF SELF:INPUT-VALUE = 1 AND 
     ordem-benefic.it-codigo:SCREEN-VALUE IN FRAME {&FRAME-NAME} <> "" THEN DO.
     RUN pi-ver-pcp.
     IF RETURN-VALUE = 'ADM-ERROR' THEN DO.
        APPLY 'ENTRY' TO SELF.
        RETURN NO-APPLY.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ordem-benefic.tipo-ordem V-table-Win
ON VALUE-CHANGED OF ordem-benefic.tipo-ordem IN FRAME f-main /* Tipo */
DO:
  RUN pi-busca-ob.
  RUN pi-limpa-acond.

  ASSIGN fi-nr-trf:VISIBLE IN FRAME {&FRAME-NAME} = NO
         ordem-benefic.quantidade:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
         fi-nr-trf:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".

  ENABLE {&List-4} WITH FRAME {&FRAME-NAME}. 
  ENABLE {&List-6} WITH FRAME {&FRAME-NAME}. 

  ASSIGN cb-tipo-tear:SENSITIVE = YES.

  IF SELF:INPUT-VALUE = 2 THEN DO.
     DISABLE {&List-4} WITH FRAME {&FRAME-NAME}.

     ASSIGN cb-tipo-tear:SENSITIVE = NO.

     ASSIGN fi-nr-trf:ROW = 2.50
            fi-nr-trf:VISIBLE IN FRAME {&FRAME-NAME} = YES
            fi-nr-trf:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

     APPLY 'entry' TO fi-nr-trf.
     RETURN NO-APPLY.
  END.

  IF SELF:INPUT-VALUE = 3 THEN DO.
     DISABLE {&List-4} WITH FRAME {&FRAME-NAME}.
     DISABLE {&List-6} WITH FRAME {&FRAME-NAME}.

     ASSIGN cb-tipo-tear:SENSITIVE = NO.

     ASSIGN fi-nr-trf:ROW = 3.17
            fi-nr-trf:VISIBLE IN FRAME {&FRAME-NAME} = YES
            fi-nr-trf:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

     APPLY 'entry' TO fi-nr-trf.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
  FIND FIRST ob-param NO-LOCK NO-ERROR.

  ordem-benefic.it-codigo:LOAD-MOUSE-POINTER("image/lupa.cur"). 
  ordem-benefic.cod-refer:LOAD-MOUSE-POINTER("image/lupa.cur"). 
  ordem-benefic.cod-estabel:LOAD-MOUSE-POINTER("image/lupa.cur"). 
  fi-nr-trf:LOAD-MOUSE-POINTER("image/lupa.cur"). 
  fi-acondic1:LOAD-MOUSE-POINTER("image/lupa.cur"). 
  fi-acondic2:LOAD-MOUSE-POINTER("image/lupa.cur"). 
  fi-acondic3:LOAD-MOUSE-POINTER("image/lupa.cur"). 
  fi-acondic4:LOAD-MOUSE-POINTER("image/lupa.cur"). 
  fi-acondic5:LOAD-MOUSE-POINTER("image/lupa.cur"). 
  fi-acondic6:LOAD-MOUSE-POINTER("image/lupa.cur"). 
  fi-acondic7:LOAD-MOUSE-POINTER("image/lupa.cur"). 
  fi-acondic8:LOAD-MOUSE-POINTER("image/lupa.cur"). 
  fi-acondic9:LOAD-MOUSE-POINTER("image/lupa.cur"). 

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win  _ADM-ROW-AVAILABLE
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
  {src/adm/template/row-list.i "ordem-benefic"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "ordem-benefic"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win  _DEFAULT-DISABLE
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
  HIDE FRAME f-main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record V-table-Win 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN ordem-benefic.dt-ob:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY,"99/99/9999")
         ordem-benefic.responsavel:SCREEN-VALUE IN FRAME {&FRAME-NAME} = c-seg-usuario
         ordem-benefic.tipo-ordem:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "1".

  ASSIGN fi-nr-trf:VISIBLE IN FRAME {&FRAME-NAME} = NO
         bt-desenho:SENSITIVE = NO
         bt-carteira:SENSITIVE = NO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-apply-entry V-table-Win 
PROCEDURE local-apply-entry :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

   /* Dispatch standard ADM method.                             */
   RUN dispatch IN THIS-PROCEDURE ( INPUT 'apply-entry':U ) .

   /* Code placed here will execute AFTER standard behavior.    */
   IF adm-new-record = YES THEN 
      RUN pi-limpa-acond.
   ELSE
       ASSIGN ordem-benefic.it-codigo:SENSITIVE IN FRAME {&FRAME-NAME} = NO
              ordem-benefic.cod-refer:SENSITIVE IN FRAME {&FRAME-NAME} = NO.


   RUN pi-habilita-acond (INPUT fi-acondic1:HANDLE IN FRAME {&FRAME-NAME}, 
                          INPUT fi-qt-acondic1:HANDLE IN FRAME {&FRAME-NAME}).
   RUN pi-habilita-acond (INPUT fi-acondic2:HANDLE IN FRAME {&FRAME-NAME}, 
                          INPUT fi-qt-acondic2:HANDLE IN FRAME {&FRAME-NAME}).
   RUN pi-habilita-acond (INPUT fi-acondic3:HANDLE IN FRAME {&FRAME-NAME}, 
                          INPUT fi-qt-acondic3:HANDLE IN FRAME {&FRAME-NAME}).
   RUN pi-habilita-acond (INPUT fi-acondic4:HANDLE IN FRAME {&FRAME-NAME}, 
                          INPUT fi-qt-acondic4:HANDLE IN FRAME {&FRAME-NAME}).
   RUN pi-habilita-acond (INPUT fi-acondic5:HANDLE IN FRAME {&FRAME-NAME}, 
                          INPUT fi-qt-acondic5:HANDLE IN FRAME {&FRAME-NAME}).
   RUN pi-habilita-acond (INPUT fi-acondic6:HANDLE IN FRAME {&FRAME-NAME}, 
                          INPUT fi-qt-acondic6:HANDLE IN FRAME {&FRAME-NAME}).
   RUN pi-habilita-acond (INPUT fi-acondic7:HANDLE IN FRAME {&FRAME-NAME}, 
                          INPUT fi-qt-acondic7:HANDLE IN FRAME {&FRAME-NAME}).
   RUN pi-habilita-acond (INPUT fi-acondic8:HANDLE IN FRAME {&FRAME-NAME}, 
                          INPUT fi-qt-acondic8:HANDLE IN FRAME {&FRAME-NAME}).
   RUN pi-habilita-acond (INPUT fi-acondic9:HANDLE IN FRAME {&FRAME-NAME}, 
                          INPUT fi-qt-acondic9:HANDLE IN FRAME {&FRAME-NAME}).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    /* Code placed here will execute PRIOR to standard behavior. */
    {include/i-valid.i}
    
    /*:T Ponha na pi-validate todas as validaá‰es */
    /*:T N∆o gravar nada no registro antes do dispatch do assign-record e 
       nem na PI-validate. */
    RUN pi-validate.
    IF RETURN-VALUE = 'ADM-ERROR':U THEN 
       RETURN 'ADM-ERROR':U.
     
    ASSIGN INPUT FRAME {&FRAME-NAME} cb-tipo-tear.

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    IF RETURN-VALUE = 'ADM-ERROR':U THEN 
       RETURN 'ADM-ERROR':U.

    /*:T Todos os assignÔs n∆o feitos pelo assign-record devem ser feitos aqui */  
    /* Code placed here will execute AFTER standard behavior.    */

    IF adm-new-record THEN
       ASSIGN ordem-benefic.ind-ob = i-ind-ob.

    ASSIGN ordem-benefic.num-trf      = fi-nr-trf
           ordem-benefic.cor-etiqueta = 0
           ordem-benefic.tipo-tear    = cb-tipo-tear.
    /*
    IF tg-tp-produto = YES THEN
       ASSIGN ordem-benefic.cor-etiqueta = 99.
    */

    RUN pi-grava-acond (INPUT fi-acondic1,
                        INPUT fi-qt-acondic1,
                        INPUT fi-acondic1:HANDLE IN FRAME {&FRAME-NAME}).
    RUN pi-grava-acond (INPUT fi-acondic2,
                        INPUT fi-qt-acondic2,
                        INPUT fi-acondic2:HANDLE IN FRAME {&FRAME-NAME}).
    RUN pi-grava-acond (INPUT fi-acondic3,
                        INPUT fi-qt-acondic3,
                        INPUT fi-acondic3:HANDLE IN FRAME {&FRAME-NAME}).
    RUN pi-grava-acond (INPUT fi-acondic4,
                        INPUT fi-qt-acondic4,
                        INPUT fi-acondic4:HANDLE IN FRAME {&FRAME-NAME}).
    RUN pi-grava-acond (INPUT fi-acondic5,
                        INPUT fi-qt-acondic5,
                        INPUT fi-acondic5:HANDLE IN FRAME {&FRAME-NAME}).
    RUN pi-grava-acond (INPUT fi-acondic6,
                        INPUT fi-qt-acondic6,
                        INPUT fi-acondic6:HANDLE IN FRAME {&FRAME-NAME}).
    RUN pi-grava-acond (INPUT fi-acondic7,
                        INPUT fi-qt-acondic7,
                        INPUT fi-acondic7:HANDLE IN FRAME {&FRAME-NAME}).
    RUN pi-grava-acond (INPUT fi-acondic8,
                        INPUT fi-qt-acondic8,
                        INPUT fi-acondic8:HANDLE IN FRAME {&FRAME-NAME}).
    RUN pi-grava-acond (INPUT fi-acondic9,
                        INPUT fi-qt-acondic9,
                        INPUT fi-acondic9:HANDLE IN FRAME {&FRAME-NAME}).

    IF INPUT FRAME {&FRAME-NAME} ordem-benefic.situacao <> 1 THEN 
       MESSAGE "............A T E N Ä « O............." SKIP(1)
               "OB N∆o estar† dispon°vel para Revis∆o..."
               VIEW-AS ALERT-BOX ERROR.

    MESSAGE "Deseja Imprimir Etiqueta ?" 
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "" UPDATE l-conf AS LOG.

    IF l-conf THEN DO.
       IF /*c-seg-usuario <> 'super' AND */
          c-seg-usuario <> 'albino' AND 
          l-gravou-macro = NO THEN DO.
          fn-grava-macro("n:\especificos\Etiqueta\image\logo-etq10.prn"). 
          fn-grava-macro("n:\especificos\Etiqueta\image\rlgp11.prn").
          fn-grava-macro("n:\especificos\Etiqueta\image\rlgp12.prn"). 
          fn-grava-macro("n:\especificos\Etiqueta\image\rlgp13.prn").
          fn-grava-macro("n:\especificos\Etiqueta\image\rlgp14.prn"). 
          fn-grava-macro("n:\especificos\Etiqueta\image\rlgp15.prn"). 
          fn-grava-macro("n:\especificos\Etiqueta\image\rlgp16.prn"). 
          fn-grava-macro("n:\especificos\Etiqueta\image\rlgp17.prn"). 
          fn-grava-macro("n:\especificos\Etiqueta\image\rlgp18.prn"). 
          fn-grava-macro("n:\especificos\Etiqueta\image\rlgp19.prn"). 
          fn-grava-macro("n:\especificos\Etiqueta\image\rlgp20.prn"). 
          fn-grava-macro("n:\especificos\Etiqueta\image\rlgp21.prn"). 
          fn-grava-macro("n:\especificos\Etiqueta\image\rlgp22.prn"). 
          fn-grava-macro("n:\especificos\Etiqueta\image\rlgp23.prn"). 
          fn-grava-macro("n:\especificos\Etiqueta\image\rlgp24.prn"). 

          ASSIGN l-gravou-macro = YES.
       END.
       RUN pi-etiqueta.
    END.
    ELSE DO.
       FOR EACH ob-etiqueta WHERE
                ob-etiqueta.nr-ob         = ordem-benefic.nr-ob       AND
                ob-etiqueta.dt-ob         = ordem-benefic.dt-ob       AND
                ob-etiqueta.nr-carro      = ordem-benefic.nr-carro    AND
                ob-etiqueta.num-etiqueta <> 0 AND 
                ob-etiqueta.situacao      = 0 SHARE-LOCK.
           ASSIGN ob-etiqueta.situacao = 1.
       END.
    END.

    RUN pi-envia-email.

    RELEASE ob-etiqueta.
    
    ASSIGN fi-acondic1 = ''    fi-qt-acondic1 = 0
           fi-acondic2 = ''    fi-qt-acondic2 = 0
           fi-acondic3 = ''    fi-qt-acondic3 = 0
           fi-acondic4 = ''    fi-qt-acondic4 = 0
           fi-acondic5 = ''    fi-qt-acondic5 = 0
           fi-acondic6 = ''    fi-qt-acondic6 = 0
           fi-acondic7 = ''    fi-qt-acondic7 = 0
           fi-acondic8 = ''    fi-qt-acondic8 = 0
           fi-acondic9 = ''    fi-qt-acondic9 = 0.
    
    RUN local-display-fields.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-disable-fields V-table-Win 
PROCEDURE local-disable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    
    /* Code placed here will execute PRIOR to standard behavior. */
          
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'disable-fields':U ) .
    
    /* Code placed here will execute AFTER standard behavior.    */
    &if  defined(ADM-MODIFY-FIELDS) &then
    disable {&ADM-MODIFY-FIELDS} with frame {&frame-name}.
    &endif
    
    DISABLE {&list-4} WITH FRAME {&FRAME-NAME}.
    DISABLE {&list-5} WITH FRAME {&FRAME-NAME}.
    DISABLE {&list-6} WITH FRAME {&FRAME-NAME}.
    DISABLE fi-nr-trf WITH FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /* Code placed here will execute PRIOR to standard behavior. */
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    ASSIGN fi-nr-trf:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ''
           cb-tipo-tear:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ' '. 

    IF AVAIL ordem-benefic THEN DO.
       ASSIGN fi-nr-trf:VISIBLE IN FRAME {&FRAME-NAME} = NO.

       IF ordem-benefic.tipo-ordem <> 1 THEN DO.
          IF ordem-benefic.tipo-ordem = 2 THEN
             ASSIGN fi-nr-trf:ROW = 2.17.
          ELSE
             ASSIGN fi-nr-trf:ROW = 3.17.

          ASSIGN fi-nr-trf:VISIBLE IN FRAME {&FRAME-NAME} = YES
                 fi-nr-trf:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ordem-benefic.num-trf).
       END.
       ASSIGN cb-tipo-tear:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ordem-benefic.tipo-tear.

       FIND item WHERE
            item.it-codigo = ordem-benefic.it-codigo NO-LOCK NO-ERROR.
       IF AVAIL item THEN
          ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.

       FIND referencia WHERE
            referencia.cod-refer = ordem-benefic.cod-refer NO-LOCK NO-ERROR.
       IF AVAIL referencia THEN DO.
          ASSIGN fi-desc-refer:SCREEN-VALUE IN FRAME {&FRAME-NAME} = referencia.descricao.
          RUN pi-ver-desenho.
       END.

       RUN pi-limpa-acond.

       ASSIGN i-ct = 1.
       FOR EACH ob-acondic WHERE 
                ob-acondic.nr-ob    = ordem-benefic.nr-ob AND
                ob-acondic.dt-ob    = ordem-benefic.dt-ob AND
                ob-acondic.nr-carro = ordem-benefic.nr-carro NO-LOCK.
           CASE i-ct:
               WHEN 1 THEN ASSIGN fi-acondic1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ob-acondic.acondic
                                  fi-qt-acondic1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ob-acondic.qtd-prevista).
               WHEN 2 THEN ASSIGN fi-acondic2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ob-acondic.acondic
                                  fi-qt-acondic2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ob-acondic.qtd-prevista).
               WHEN 3 THEN ASSIGN fi-acondic3:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ob-acondic.acondic
                                  fi-qt-acondic3:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ob-acondic.qtd-prevista).
               WHEN 4 THEN ASSIGN fi-acondic4:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ob-acondic.acondic
                                  fi-qt-acondic4:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ob-acondic.qtd-prevista).
               WHEN 5 THEN ASSIGN fi-acondic5:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ob-acondic.acondic
                                  fi-qt-acondic5:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ob-acondic.qtd-prevista).
               WHEN 6 THEN ASSIGN fi-acondic6:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ob-acondic.acondic
                                  fi-qt-acondic6:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ob-acondic.qtd-prevista).
               WHEN 7 THEN ASSIGN fi-acondic7:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ob-acondic.acondic
                                  fi-qt-acondic7:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ob-acondic.qtd-prevista).
               WHEN 8 THEN ASSIGN fi-acondic8:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ob-acondic.acondic
                                  fi-qt-acondic8:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ob-acondic.qtd-prevista).
               WHEN 9 THEN ASSIGN fi-acondic9:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ob-acondic.acondic
                                  fi-qt-acondic9:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ob-acondic.qtd-prevista).
           END CASE.
           ASSIGN i-ct = i-ct + 1.
       END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields V-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    /* Code placed here will execute PRIOR to standard behavior. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .
    
    /* Code placed here will execute AFTER standard behavior.    */
    &if  defined(ADM-MODIFY-FIELDS) &then
    if adm-new-record = yes then
        enable {&ADM-MODIFY-FIELDS} with frame {&frame-name}.
    &endif
    ENABLE {&list-5} WITH FRAME {&FRAME-NAME}.

    IF adm-new-record = YES THEN
       ASSIGN cb-tipo-tear:SCREEN-VALUE IN FRAME {&FRAME-NAME} = " ".

    IF ordem-benefic.tipo-ordem = 2 OR
       ordem-benefic.tipo-ordem = 3 THEN
       ASSIGN ordem-benefic.quantidade:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-end-update V-table-Win 
PROCEDURE local-end-update :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

   /* Code placed here will execute PRIOR to standard behavior. */

   /* Dispatch standard ADM method.                             */
   RUN dispatch IN THIS-PROCEDURE ( INPUT 'end-update':U ) .

   /* Code placed here will execute AFTER standard behavior.    */
   DISABLE {&list-4} WITH FRAME {&FRAME-NAME}.
   DISABLE {&list-5} WITH FRAME {&FRAME-NAME}.
   DISABLE {&list-6} WITH FRAME {&FRAME-NAME}.
   DISABLE fi-nr-trf WITH FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize V-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  FIND FIRST param-dis NO-LOCK NO-ERROR.
 
  RUN esapi/busca-estab-usuar.p (INPUT c-seg-usuario,
                                 OUTPUT gc-cod-estabel).

  ASSIGN l-gravou-macro = NO.

  /* Busca Nome da Empresa */
  FIND FIRST param-global NO-LOCK NO-ERROR.
  FIND FIRST empresa
       WHERE empresa.ep-codigo = param-global.empresa-prin NO-LOCK NO-ERROR. 
  ASSIGN c-empresa = (IF AVAIL empresa THEN empresa.razao-social ELSE "").





END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-atualiza-parent V-table-Win 
PROCEDURE pi-atualiza-parent :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    define input parameter v-row-parent-externo as rowid no-undo.
    
    assign v-row-parent = v-row-parent-externo.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-busca-ob V-table-Win 
PROCEDURE pi-busca-ob :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FIND FIRST b-ordem-benefic WHERE
              b-ordem-benefic.nr-ob = INPUT FRAME {&FRAME-NAME} ordem-benefic.nr-ob NO-LOCK NO-ERROR.
   IF AVAIL b-ordem-benefic THEN DO.
     /* ASSIGN cb-tipo-tear:SCREEN-VALUE IN FRAME {&FRAME-NAME} = b-ordem-benefic.tipo-tear. */
      FIND ITEM WHERE
           ITEM.it-codigo = b-ordem-benefic.it-codigo NO-LOCK NO-ERROR.
      IF AVAIL ITEM THEN
         ASSIGN ordem-benefic.it-codigo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = b-ordem-benefic.it-codigo
                fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME}            = item.desc-item
                ordem-benefic.un:SCREEN-VALUE IN FRAME {&FRAME-NAME}        = item.un.

      FIND referencia WHERE
           referencia.cod-refer = b-ordem-benefic.cod-refer NO-LOCK NO-ERROR.
      IF AVAIL referencia THEN
         ASSIGN ordem-benefic.cod-refer:SCREEN-VALUE IN FRAME {&FRAME-NAME} = b-ordem-benefic.cod-refer
                fi-desc-refer:SCREEN-VALUE IN FRAME {&FRAME-NAME}           = referencia.descricao.

      ASSIGN ordem-benefic.it-codigo:SENSITIVE IN FRAME {&FRAME-NAME} = NO
             ordem-benefic.cod-refer:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-calc-carteira V-table-Win 
PROCEDURE pi-calc-carteira :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF VAR c-lote AS CHAR.
  
  EMPTY TEMP-TABLE tt-estoque.
  EMPTY TEMP-TABLE tt-carteira.
  EMPTY TEMP-TABLE tt-work.

  /* CALCULANDO O ITEM/REFERENCIA EM REVISAO */
  ASSIGN de-tot-revisao = 0.
  FOR EACH b-ordem-benefic WHERE
           b-ordem-benefic.it-codigo = ordem-benefic.it-codigo AND
           b-ordem-benefic.cod-refer = ordem-benefic.cod-refer AND
           b-ordem-benefic.situacao  < 4 NO-LOCK.

      FOR EACH ob-acondic WHERE 
               ob-acondic.nr-ob    = b-ordem-benefic.nr-ob AND
               ob-acondic.dt-ob    = b-ordem-benefic.dt-ob AND
               ob-acondic.nr-carro = b-ordem-benefic.nr-carro NO-LOCK.

           FIND corte-comerc WHERE 
                corte-comerc.codigo = ob-acondic.corte-comerc NO-LOCK NO-ERROR.
           IF NOT AVAIL corte-comerc THEN NEXT.

           ASSIGN de-tot-revisao = 0.
           FOR EACH ob-etiqueta WHERE
                    ob-etiqueta.cod-estabel = INPUT FRAME {&FRAME-NAME} ordem-benefic.cod-estabel AND
                    ob-etiqueta.nr-ob = ob-acondic.nr-ob    AND
                    ob-etiqueta.dt-ob = ob-acondic.dt-ob    AND
                    ob-etiqueta.nr-carro = ob-acondic.nr-carro AND
                    ob-etiqueta.acondic = ob-acondic.acondic  NO-LOCK.
               ASSIGN de-tot-revisao = de-tot-revisao + ob-etiqueta.quantidade.
           END.

           CASE corte-comerc.tp-embalag.
               WHEN 1 THEN ASSIGN c-lote = "RP".
               WHEN 2 THEN ASSIGN c-lote = "PP".
           END CASE.

           FIND tt-estoque WHERE
                tt-estoque.it-codigo = b-ordem-benefic.it-codigo AND 
                tt-estoque.cod-refer = b-ordem-benefic.cod-refer AND
                tt-estoque.corte-comerc = corte-comerc.codigo       AND 
                tt-estoque.nr-lote = c-lote NO-LOCK NO-ERROR.
           IF NOT AVAIL tt-estoque THEN DO.
              CREATE tt-estoque.
              ASSIGN tt-estoque.it-codigo = b-ordem-benefic.it-codigo
                     tt-estoque.cod-refer = b-ordem-benefic.cod-refer
                     tt-estoque.corte-comerc = corte-comerc.codigo 
                     tt-estoque.nr-lote = c-lote.
           END.
           ASSIGN tt-estoque.qt-estoque = tt-estoque.qt-estoque +
                                          (corte-comerc.compr-med * ob-acondic.qtd-prevista) -
                                          de-tot-revisao.
      END.
  END.

  RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
  {utp/ut-liter.i Calculando_Estoque *}
  RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).
  /* GERANDO ESTOQUE */
  FOR EACH ob-etiqueta WHERE 
           ob-etiqueta.cod-estabel = INPUT FRAME {&FRAME-NAME} ordem-benefic.cod-estabel AND 
           ob-etiqueta.it-codigo = INPUT FRAME {&FRAME-NAME} ordem-benefic.it-codigo AND 
           ob-etiqueta.cod-refer = INPUT FRAME {&FRAME-NAME} ordem-benefic.cod-refer AND
           LOOKUP(STRING(ob-etiqueta.situacao,"9"),"3,4") > 0                        AND
           INDEX(c-lotes,ob-etiqueta.nr-lote) <> 0 NO-LOCK.
  
      FIND corte-comerc WHERE 
           corte-comerc.codigo = ob-etiqueta.corte-comerc NO-LOCK NO-ERROR.
      IF NOT AVAIL corte-comerc THEN NEXT.
  
      RUN pi-acompanhar IN h-acomp (INPUT ob-etiqueta.num-etiqueta).

      FIND tt-estoque WHERE
           tt-estoque.it-codigo    = ob-etiqueta.it-codigo    AND 
           tt-estoque.cod-refer    = ob-etiqueta.cod-refer    AND
           tt-estoque.corte-comerc = ob-etiqueta.corte-comerc AND 
           tt-estoque.nr-lote      = ob-etiqueta.nr-lote  NO-LOCK NO-ERROR.
      IF NOT AVAIL tt-estoque THEN DO.
         CREATE tt-estoque.
         ASSIGN tt-estoque.it-codigo    = ob-etiqueta.it-codigo
                tt-estoque.cod-refer    = ob-etiqueta.cod-refer
                tt-estoque.corte-comerc = ob-etiqueta.corte-comerc 
                tt-estoque.nr-lote      = ob-etiqueta.nr-lote.
      END.
      ASSIGN tt-estoque.qt-estoque = tt-estoque.qt-estoque + ob-etiqueta.quantidade.
  
  END.

  {utp/ut-liter.i Calculando_Carteira *}
  RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).
  /* GERANDO CARTEIRA TOTAL */
  FOR EACH ped-item WHERE
           ped-item.it-codigo = INPUT FRAME {&FRAME-NAME} ordem-benefic.it-codigo AND 
           ped-item.cod-refer = INPUT FRAME {&FRAME-NAME} ordem-benefic.cod-refer AND
           LOOKUP(STRING(ped-item.cod-sit-item,"9"),"1,2,5") > 0 NO-LOCK,
      FIRST ped-item-ext OF ped-item 
            WHERE INDEX(c-lotes,SUBSTR(ped-item-ext.lote,1,2)) <> 0 
            NO-LOCK.

      
      RUN pi-acompanhar IN h-acomp (INPUT "Pedido: " + ped-item.nr-pedcli +
                                          "  Item: " + ped-item.it-codigo +
                                          "   Ref: " + ped-item.cod-refer).

      FIND ped-venda OF ped-item WHERE 
           ped-venda.cod-estabel = INPUT FRAME {&FRAME-NAME} ordem-benefic.cod-estabel 
           NO-LOCK NO-ERROR.

      FIND ped-venda-ext WHERE
           ped-venda-ext.nr-pedido = ped-venda.nr-pedido NO-LOCK NO-ERROR.
      IF AVAIL ped-venda-ext THEN DO:
         IF ped-venda-ext.tp-pedido = "EXPORTAÄ«O" THEN
            ASSIGN de-tot-export = de-tot-export + ped-item.qt-pedida.
         IF ped-venda-ext.l-emb-neutra THEN
            ASSIGN de-tot-emb-neutra = de-tot-emb-neutra + ped-item.qt-pedida.
      END.
      ASSIGN c-periodo = STRING(YEAR(ped-venda.dt-entrega)) + 
                         STRING(MONTH(ped-venda.dt-entrega),"99").

      FIND tt-carteira WHERE
           tt-carteira.periodo      = c-periodo                     AND
           tt-carteira.corte-comerc = ped-item-ext.corte-comerc     AND
           tt-carteira.nr-lote      = SUBSTR(ped-item-ext.lote,1,2) AND
           tt-carteira.it-codigo    = ped-item.it-codigo            AND 
           tt-carteira.cod-refer    = ped-item.cod-refer NO-LOCK NO-ERROR.
      IF NOT AVAIL tt-carteira THEN DO.
         CREATE tt-carteira.
         ASSIGN tt-carteira.periodo      = c-periodo
                tt-carteira.corte-comerc = ped-item-ext.corte-comerc
                tt-carteira.nr-lote      = UPPER(SUBSTR(ped-item-ext.lote,1,2))
                tt-carteira.it-codigo    = ped-item.it-codigo 
                tt-carteira.cod-refer    = ped-item.cod-refer.
      END.
      ASSIGN tt-carteira.qt-carteira = tt-carteira.qt-carteira + ped-item.qt-pedida.

      ASSIGN tt-carteira.row-pedidos = IF tt-carteira.row-pedidos = ""
                                       THEN STRING(ROWID(ped-venda))
                                       ELSE tt-carteira.row-pedidos + ";" + STRING(ROWID(ped-venda)).
  END.

/* GERANDO CARTEIRA  N E G A T I V A  */
  FOR EACH tt-carteira NO-LOCK
        BY tt-carteira.periodo
        BY tt-carteira.corte-comerc
        BY tt-carteira.nr-lote:

      FIND tt-estoque WHERE
           tt-estoque.corte-comerc = tt-carteira.corte-comerc AND
           tt-estoque.nr-lote      = tt-carteira.nr-lote      AND
           tt-estoque.qt-estoque > 0 NO-ERROR.
      IF AVAIL tt-estoque THEN DO:

         IF tt-carteira.qt-carteira <= tt-estoque.qt-estoque THEN DO:
            ASSIGN tt-estoque.qt-estoque = tt-estoque.qt-estoque -
                                           tt-carteira.qt-carteira.
            ASSIGN tt-carteira.qt-carteira = 0.
         END.
         ELSE DO:
            ASSIGN tt-carteira.qt-carteira = tt-carteira.qt-carteira - tt-estoque.qt-estoque.
            FIND corte-comerc WHERE /* Verifica se Negativo e Menor que Qtd Minima do Corte */
                 corte-comerc.codigo = tt-carteira.corte-comerc NO-LOCK NO-ERROR.
            IF AVAIL corte-comerc AND tt-carteira.qt-carteira < corte-comerc.compr-min THEN
               ASSIGN tt-carteira.qt-carteira = 0.

            ASSIGN tt-estoque.qt-estoque = 0.
         END.
      END.

  END.

  RUN pi-finalizar in h-acomp.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cria-etiqueta V-table-Win 
PROCEDURE pi-cria-etiqueta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER p-qtde-etiq AS INT.

    FIND LAST ob-etiqueta WHERE
              ob-etiqueta.cod-estab = ordem-benefic.cod-estab AND
              ob-etiqueta.nr-ob = ordem-benefic.nr-ob AND
              ob-etiqueta.nr-carro = ordem-benefic.nr-carro USE-INDEX indice2
              NO-LOCK NO-ERROR.
    
    IF AVAIL ob-etiqueta THEN
       ASSIGN i-nr-seq = ob-etiqueta.nr-sequencia.
    ELSE
       ASSIGN i-nr-seq = (ordem-benefic.ind-ob * ob-param.max-etq-carro) - ob-param.max-etq-carro.

    DO i-ct = 1 TO p-qtde-etiq.
       ASSIGN i-nr-seq = i-nr-seq + 1.

       CREATE ob-etiqueta.
       ASSIGN ordem-benefic.qtd-planejada = ordem-benefic.qtd-planejada + 1
              ob-etiqueta.nr-ob           = ob-acondic.nr-ob
              ob-etiqueta.dt-ob           = ob-acondic.dt-ob
              ob-etiqueta.cod-estabel     = ordem-benefic.cod-estabel
              ob-etiqueta.dt-emissao      = TODAY
              ob-etiqueta.hr-emissao      = STRING(TIME,"HH:MM")
              ob-etiqueta.nr-carro        = ob-acondic.nr-carro
              ob-etiqueta.acondic         = ob-acondic.acondic
              ob-etiqueta.tipo-ordem      = ordem-benefic.tipo-ordem
              ob-etiqueta.it-codigo       = ordem-benefic.it-codigo
              ob-etiqueta.cod-refer       = ordem-benefic.cod-refer
              ob-etiqueta.situacao        = 0
              ob-etiqueta.num-etiqueta    = IF ordem-benefic.cod-estabel = '1'
                                            THEN NEXT-VALUE(seq-etq-estab1)
                                            ELSE NEXT-VALUE(seq-etq-estoq)
              ob-etiqueta.nr-sequencia    = i-nr-seq.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-envia-email V-table-Win 
PROCEDURE pi-envia-email :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   ASSIGN c-mensagem = "O usuario, " + UPPER(c-seg-usuario) + CHR(13) +
                       "Confirmou ou alterou a OB: " + STRING(ordem-benefic.nr-ob, ">>>,>>9") + " , conforme consta no  relatorio anexo." + CHR(13) +  CHR(13) + CHR(13) + 
                       "PROGRAMAÄ«O DE OB's" + CHR(13) +
                       "   Tear Textil".
       
   ASSIGN c-arq-email = SESSION:TEMP-DIRECTORY + STRING(ordem-benefic.nr-ob, ">>>>>9") + UPPER(c-seg-usuario) + ".TXT".
   OUTPUT TO VALUE(c-arq-email) CONVERT SOURCE "ibm850".

   RUN pi-imprime.

   OUTPUT CLOSE.

   /* ASSIGN c-destinatario = "controle.acabado@teartextil.com.br,marcia.rocha@teartextil.com.br,helder.ferreira@teartextil.com.br". */
   ASSIGN c-destinatario = "controle.acabado@teartextil.com.br".

   RUN esapi/esapi002.p (INPUT "teartextil@teartextil.com.br", /* e-mail remetente */
                         INPUT c-destinatario,  /* e-mail destinat†rio */ 
                         INPUT "Confirmaá∆o da OB " + STRING(ordem-benefic.nr-ob, ">>>>>9"), /* Assunto */
                         INPUT c-mensagem, /* Mensagem */
                         INPUT c-arq-email, /*arquivo anexo*/
                         INPUT YES). /* Mostra Erros */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-etiqueta V-table-Win 
PROCEDURE pi-etiqueta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR i-lin AS INT EXTENT 6 INIT [00,590,1190,1795,2395,2990].
    DEF VAR i-col AS INT EXTENT 3 INIT [0,810,1620].

    OUTPUT TO PRINTER PAGED.

    PUT UNFORMATTED 
        "~033&l3A"    /* Pagina Of°cio     */
        "~033&l170F"  /* tamanho do texto  */
        "~033&l170P"  /* tamanho da p†gina */ 
        "~033&l0L"    /* margem esquerda   */
        "~033&l0M"    /* margem direita    */
        "~033&l1E".   /* margem superior   */ 
        
    ASSIGN i-ct-lin = 1
           i-ct-col = 0.

    FOR EACH ob-etiqueta OF ordem-benefic WHERE
             ob-etiqueta.num-etiqueta <> 0 AND 
             ob-etiqueta.situacao      = 0 SHARE-LOCK.

        ASSIGN i-ct-col = i-ct-col + 1.
        IF i-ct-col > 3 THEN DO.
           ASSIGN i-ct-col = 1
                  i-ct-lin = i-ct-lin + 1.

           IF i-ct-lin > 6 THEN DO.
              ASSIGN i-ct-lin = 1.
              PAGE.
           END.
        END.

        RUN pi-imp-etiqueta (INPUT i-lin[i-ct-lin], INPUT i-col[i-ct-col]). 

        ASSIGN ob-etiqueta.situacao = 1.
    END.
    OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-grava-acond V-table-Win 
PROCEDURE pi-grava-acond :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER p-acond     LIKE ob-acondic.acondic.
    DEF INPUT PARAMETER p-qtde-prev LIKE ob-acondic.qtd-prevista.
    DEF INPUT PARAMETER h-acond     AS HANDLE.
    DEF VAR c-corte AS CHAR.

    IF adm-new-record = NO AND h-acond:SENSITIVE = NO THEN RETURN.

    IF p-acond <> "" AND p-qtde-prev <> 0 THEN DO.
       ASSIGN c-corte = "".
       FIND corte-comerc WHERE
            corte-comerc.descricao = p-acond NO-LOCK NO-ERROR. 
       IF AVAIL corte-comerc THEN
          ASSIGN c-corte = corte-comerc.codigo.
       FIND ob-acondic WHERE 
            ob-acondic.cod-estabel = ordem-benefic.cod-estabel AND
            ob-acondic.nr-ob       = ordem-benefic.nr-ob AND
            ob-acondic.dt-ob       = ordem-benefic.dt-ob AND
            ob-acondic.nr-carro    = ordem-benefic.nr-carro AND
            ob-acondic.acondic     = p-acond EXCLUSIVE-LOCK NO-ERROR.

       IF NOT AVAIL ob-acondic THEN DO.
          CREATE ob-acondic.
          ASSIGN ob-acondic.cod-estabel  = ordem-benefic.cod-estabel
                 ob-acondic.nr-ob        = ordem-benefic.nr-ob
                 ob-acondic.dt-ob        = ordem-benefic.dt-ob
                 ob-acondic.nr-carro     = ordem-benefic.nr-carro
                 ob-acondic.acondic      = p-acond
                 ob-acondic.corte-comerc = c-corte.
       END.
       ASSIGN ob-acondic.qtd-prevista = ob-acondic.qtd-prevista + p-qtde-prev.

       RUN pi-cria-etiqueta (INPUT p-qtde-prev).
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-habilita-acond V-table-Win 
PROCEDURE pi-habilita-acond :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER p-acond AS HANDLE.
    DEF INPUT PARAMETER p-qt-acond AS HANDLE.

    ASSIGN p-acond:SENSITIVE    = NO
           p-qt-acond:SENSITIVE = NO.
    IF p-acond:SCREEN-VALUE = '' THEN
       ASSIGN p-acond:SENSITIVE    = YES
              p-qt-acond:SENSITIVE = YES.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imp-etiqueta V-table-Win 
PROCEDURE pi-imp-etiqueta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF INPUT PARAMETER i-lin AS INT.
   DEF INPUT PARAMETER i-col AS INT.

   ASSIGN i-num-bar = INT(STRING(ob-etiqueta.num-etiqueta) + fn-calc-digito(INPUT STRING(ob-etiqueta.num-etiqueta,"999999999"))).

   FIND ITEM WHERE
        ITEM.it-codigo = ob-etiqueta.it-codigo NO-LOCK NO-ERROR.


   FIND FIRST item-ext WHERE
              item-ext.it-codigo = ITEM.it-codigo NO-LOCK NO-ERROR.

   IF AVAIL item-ext THEN DO.
      FIND FIRST composi WHERE
                 composi.cod-composi = item-ext.cod-composi NO-LOCK NO-ERROR.

      ASSIGN c-composicao = "".
      IF AVAIL composi THEN DO.
         DO i-ct = 1 TO NUM-ENTRIES(composi.descricao).
            ASSIGN c-composicao[INT(i-ct / 2)] = c-composicao[INT(i-ct / 2)] + ENTRY(i-ct,composi.descricao).
         END.
      END.
   END.

   PUT UNFORMATTED 
       fn-retangulo(input i-col, input i-lin, input i-col + 760, INPUT i-lin + 535, INPUT 3).

   ASSIGN c-corte = IF LOOKUP(SUBSTR(ob-etiqueta.acondic,1,1),"P,R") > 0 
                    THEN SUBSTR(ob-etiqueta.acondic,1,1) + " " + ENTRY(2,ob-etiqueta.acondic," ")
                    ELSE SUBSTR(ob-etiqueta.acondic,1,1).

   IF c-cod-estabel = '2' THEN
      PUT UNFORMATTED 
          fn-texto(INPUT i-col +  15, INPUT i-lin +  30, INPUT "TEAR TEXTIL INDUSTRIA E COMERCIO LTDA.",       INPUT 16602, INPUT  7, INPUT 4, INPUT 0, INPUT 1, INPUT "H")
          fn-texto(INPUT i-col + 230, INPUT i-lin +  60, INPUT "Av. General David Sarnoff, 5005D",             INPUT 16602, INPUT  6, INPUT 0, INPUT 0, INPUT 1, INPUT "H")
          fn-texto(INPUT i-col + 230, INPUT i-lin +  85, INPUT "32210-110 / CONTAGEM - MG   BRASIL",           INPUT 16602, INPUT  6, INPUT 0, INPUT 0, INPUT 1, INPUT "H")
          fn-texto(INPUT i-col + 230, INPUT i-lin + 110, INPUT "CNPJ:03.123.987/0002-00 IE:186020807.0187",    INPUT 16602, INPUT  6, INPUT 0, INPUT 0, INPUT 1, INPUT "H").
   ELSE
       PUT UNFORMATTED 
           fn-texto(INPUT i-col +  15, INPUT i-lin +  30, INPUT "TEAR TEXTIL INDUSTRIA E COMERCIO LTDA.",       INPUT 16602, INPUT  7, INPUT 4, INPUT 0, INPUT 1, INPUT "H")
           fn-texto(INPUT i-col + 230, INPUT i-lin +  60, INPUT "Av. Dom Cirilo, 196-A",                        INPUT 16602, INPUT  6, INPUT 0, INPUT 0, INPUT 1, INPUT "H")
           fn-texto(INPUT i-col + 230, INPUT i-lin +  85, INPUT "35774-000 / PARAOPEBA - MG   BRASIL",          INPUT 16602, INPUT  6, INPUT 0, INPUT 0, INPUT 1, INPUT "H")
           fn-texto(INPUT i-col + 230, INPUT i-lin + 110, INPUT "CNPJ:03.123.987/0001-20 IE:186020807.0004",    INPUT 16602, INPUT  6, INPUT 0, INPUT 0, INPUT 1, INPUT "H").

   PUT UNFORMATTED 
       fn-imp-macro(INPUT i-col + 5, INPUT i-lin + 28, INPUT 10)  
       fn-linha(INPUT i-col + 00, INPUT i-lin + 120, INPUT  760, INPUT 3, INPUT "H")  

       fn-texto(INPUT i-col + 135, INPUT i-lin + 160, INPUT ob-etiqueta.it-codigo,                        INPUT 16602, INPUT  9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
       fn-texto(INPUT i-col + 290, INPUT i-lin + 160, INPUT ob-etiqueta.cod-refer,                        INPUT 16602, INPUT  9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
       fn-texto(INPUT i-col + 470, INPUT i-lin + 160, INPUT STRING(ob-etiqueta.nr-ob),                    INPUT 16602, INPUT  9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
       fn-texto(INPUT i-col + 650, INPUT i-lin + 160, INPUT TRIM(c-corte),                                INPUT 16602, INPUT  9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
       fn-texto(INPUT i-col + 135, INPUT i-lin + 190, INPUT ITEM.desc-item,                               INPUT 16602, INPUT  6, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
       fn-texto(INPUT i-col + 135, INPUT i-lin + 215, INPUT c-composicao[1],                              INPUT 16602, INPUT  6, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
       fn-texto(INPUT i-col + 135, INPUT i-lin + 240, INPUT c-composicao[2],                              INPUT 16602, INPUT  6, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
       fn-texto(INPUT i-col + 135, INPUT i-lin + 273, INPUT "g/m:",                                       INPUT 16602, INPUT  9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
       fn-texto(INPUT i-col + 230, INPUT i-lin + 273, INPUT STRING((item.peso-liquido * 1000),"999.99"),  INPUT 16602, INPUT  9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
       fn-linha(INPUT i-col + 120, INPUT i-lin + 120, INPUT  415, INPUT 3, INPUT "V")  
       fn-texto(INPUT i-col + 230, INPUT i-lin + 525, INPUT STRING(ob-etiqueta.num-etiqueta,"999999999"), INPUT 16602, INPUT 20, INPUT 3, INPUT 0, INPUT 1, INPUT "H").

   PUT UNFORMATTED
       fn-code25 (INPUT i-col + 190, input i-lin + 283,
                  INPUT STRING(i-num-bar,"9999999999"),
                  INPUT "H",
                  INPUT 3.5,
                  INPUT 5.0).

   IF AVAIL item-ext THEN DO. /*Escolha da imagem para Imprimir*/
      CASE item-ext.cod-rlgp: 
        WHEN 1 THEN
            PUT UNFORMATTED
                fn-imp-macro(INPUT i-col +  5, INPUT i-lin + 120, INPUT 11)  
                fn-imp-macro(INPUT i-col +  5, INPUT i-lin + 200, INPUT 21)
                fn-imp-macro(INPUT i-col +  5, INPUT i-lin + 290, INPUT 13)
                fn-imp-macro(INPUT i-col +  8, INPUT i-lin + 370, INPUT 14)
                fn-imp-macro(INPUT i-col + 16, INPUT i-lin + 445, INPUT 23). 
        WHEN 2 THEN                         
            PUT UNFORMATTED
                fn-imp-macro(INPUT i-col +  5, INPUT i-lin + 120, INPUT 24)  
                fn-imp-macro(INPUT i-col +  5, INPUT i-lin + 200, INPUT 21)
                fn-imp-macro(INPUT i-col +  5, INPUT i-lin + 290, INPUT 16)
                fn-imp-macro(INPUT i-col +  8, INPUT i-lin + 370, INPUT 14)
                fn-imp-macro(INPUT i-col + 16, INPUT i-lin + 445, INPUT 23).  
        WHEN 3 THEN 
            PUT UNFORMATTED
                fn-imp-macro(INPUT i-col +  1, INPUT i-lin + 126, INPUT 22)  
                fn-imp-macro(INPUT i-col +  9, INPUT i-lin + 212, INPUT 21)
                fn-imp-macro(INPUT i-col + 18, INPUT i-lin + 307, INPUT 13)
                fn-imp-macro(INPUT i-col +  8, INPUT i-lin + 390, INPUT 20)
                fn-imp-macro(INPUT i-col + 16, INPUT i-lin + 450, INPUT 23).
        WHEN 4 THEN 
            PUT UNFORMATTED
                fn-imp-macro(INPUT i-col +  1, INPUT i-lin + 126, INPUT 11)  
                fn-imp-macro(INPUT i-col +  9, INPUT i-lin + 212, INPUT 21)
                fn-imp-macro(INPUT i-col +  5, INPUT i-lin + 307, INPUT 13)
                fn-imp-macro(INPUT i-col +  8, INPUT i-lin + 390, INPUT 14)
                fn-imp-macro(INPUT i-col +  1, INPUT i-lin + 450, INPUT 25).
     END CASE.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imprime V-table-Win 
PROCEDURE pi-imprime :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF VAR de-tot-negativo AS DEC.


 PUT c-empresa  FORMAT "X(40)"                 AT   1
     "DATA: "                                  AT  44
     STRING(TODAY,"99/99/9999") FORMAT "X(10)" AT  50
     "HORA: "                                  AT  67
     STRING(TIME,"hh:mm:ss")                   AT  73 SKIP(1).
 PUT "RELATORIO DE ACOMPANHAMENTO DE OB'S CONFIRMADAS" AT 19 SKIP(1). 
 PUT "  Nß DA OB  DT.EMISSAO ITEM    REFERENCIA      METROS  ACONDICIONAMENTO     QTDE" AT 1.   
 PUT "----------  ---------- ------  ----------  ----------  ----------------  -------" AT 1.

 PUT ordem-benefic.nr-ob       FORMAT ">>>,>>9"    AT  1
     ordem-benefic.dt-ob       FORMAT "99/99/9999" AT 13
     ordem-benefic.it-codigo   FORMAT "x(6)"       AT 24
     ordem-benefic.cod-refer   FORMAT "x(7)"       AT 35
     ordem-benefic.quantidade  FORMAT ">>>,>>9.99" AT 44.

 IF fi-acondic1:SENSITIVE IN FRAME {&FRAME-NAME} = YES AND fi-acondic1 <> "" THEN
    PUT fi-acondic1    FORMAT "x(16)"  AT 56
        fi-qt-acondic1 FORMAT ">>>>>9" AT 74 SKIP.

 IF fi-acondic2:SENSITIVE IN FRAME {&FRAME-NAME} = YES AND fi-acondic2 <> "" THEN
    PUT fi-acondic2    FORMAT "x(16)"  AT 56
        fi-qt-acondic2 FORMAT ">>>>>9" AT 74 SKIP.

 IF fi-acondic3:SENSITIVE IN FRAME {&FRAME-NAME} = YES AND fi-acondic3 <> "" THEN
    PUT fi-acondic3    FORMAT "x(16)"  AT 56
        fi-qt-acondic3 FORMAT ">>>>>9" AT 74 SKIP.

 IF fi-acondic4:SENSITIVE IN FRAME {&FRAME-NAME} = YES AND fi-acondic4 <> "" THEN
    PUT fi-acondic4    FORMAT "x(16)"  AT 56
        fi-qt-acondic4 FORMAT ">>>>>9" AT 74 SKIP.

 IF fi-acondic5:SENSITIVE IN FRAME {&FRAME-NAME} = YES AND fi-acondic5 <> "" THEN
    PUT fi-acondic5    FORMAT "x(16)"  AT 56
        fi-qt-acondic5 FORMAT ">>>>>9" AT 74 SKIP.

 IF fi-acondic6:SENSITIVE IN FRAME {&FRAME-NAME} = YES AND fi-acondic6 <> "" THEN
    PUT fi-acondic6    FORMAT "x(16)"  AT 56
        fi-qt-acondic6 FORMAT ">>>>>9" AT 74 SKIP.

 IF fi-acondic7:SENSITIVE IN FRAME {&FRAME-NAME} = YES AND fi-acondic7 <> "" THEN
    PUT fi-acondic7    FORMAT "x(16)"  AT 56
        fi-qt-acondic7 FORMAT ">>>>>9" AT 74 SKIP.

 IF fi-acondic8:SENSITIVE IN FRAME {&FRAME-NAME} = YES AND fi-acondic8 <> "" THEN
    PUT fi-acondic8    FORMAT "x(16)"  AT 56
        fi-qt-acondic8 FORMAT ">>>>>9" AT 74 SKIP.

 IF fi-acondic9:SENSITIVE IN FRAME {&FRAME-NAME} = YES AND fi-acondic9 <> "" THEN
    PUT fi-acondic9    FORMAT "x(16)"  AT 56
        fi-qt-acondic9 FORMAT ">>>>>9" AT 74 SKIP.


 PUT SKIP(1).

 PUT "********  CARTEIRA  NEGATIVA  *******" AT 1.
 PUT "PERIODO  CORTE       LOTE  QUANTIDADE" AT 1.
 PUT "-------  ----------  ----  ----------" AT 1.

 FOR EACH tt-carteira WHERE
          tt-carteira.qt-carteira > 0 NO-LOCK,
    FIRST corte-comerc WHERE
          corte-comerc.codigo = tt-carteira.corte-comerc NO-LOCK
       BY tt-carteira.periodo
       BY tt-carteira.corte-comerc DESCENDING
       BY tt-carteira.qt-carteira.

     PUT SUBSTR(tt-carteira.periodo,5,2) + "/" + SUBSTR(tt-carteira.periodo,1,4) AT  1
         corte-comerc.descricao   FORMAT "x(10)"                                 AT 10
         tt-carteira.nr-lote      FORMAT "X(2)"                                  AT 23
         tt-carteira.qt-carteira  FORMAT ">>>,>>9.99"                            AT 28.

     ASSIGN de-tot-negativo = de-tot-negativo + tt-carteira.qt-carteira.
 END.
 IF de-tot-negativo <> 0 THEN DO:
    PUT SKIP.
    PUT "TOTAL..................."           AT  1.
    PUT de-tot-negativo FORMAT ">>>>,>>9.99" AT 27.
 END.
 PUT SKIP(1).

 PUT "OBS.:" AT 1.
 RUN pi-print-editor(INPUT REPLACE(REPLACE(TRIM(ordem-benefic.observacao),CHR(13)," "),CHR(10)," "), INPUT 65). 
 FOR EACH tt-editor:
     PUT tt-editor.conteudo AT 7 SKIP.
 END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-limpa-acond V-table-Win 
PROCEDURE pi-limpa-acond :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 ASSIGN fi-acondic1:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = ""
        fi-qt-acondic1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
        fi-acondic2:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = ""
        fi-qt-acondic2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
        fi-acondic3:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = ""
        fi-qt-acondic3:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
        fi-acondic4:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = ""
        fi-qt-acondic4:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
        fi-acondic5:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = ""
        fi-qt-acondic5:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
        fi-acondic6:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = ""
        fi-qt-acondic6:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
        fi-acondic7:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = ""
        fi-qt-acondic7:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
        fi-acondic8:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = ""
        fi-qt-acondic8:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
        fi-acondic9:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = ""
        fi-qt-acondic9:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-mostra-acond V-table-Win 
PROCEDURE pi-mostra-acond :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER p-ind-acond AS INT.
    DEF INPUT PARAMETER p-acond AS CHAR.
    DEF INPUT PARAMETER p-qt-acond AS CHAR.

    CASE p-ind-acond.
        WHEN 1 THEN ASSIGN fi-acondic1:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = p-acond
                           fi-qt-acondic1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = p-qt-acond.
        WHEN 2 THEN ASSIGN fi-acondic2:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = p-acond
                           fi-qt-acondic2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = p-qt-acond.
        WHEN 3 THEN ASSIGN fi-acondic3:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = p-acond
                           fi-qt-acondic3:SCREEN-VALUE IN FRAME {&FRAME-NAME} = p-qt-acond.
        WHEN 4 THEN ASSIGN fi-acondic4:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = p-acond
                           fi-qt-acondic4:SCREEN-VALUE IN FRAME {&FRAME-NAME} = p-qt-acond.
        WHEN 5 THEN ASSIGN fi-acondic5:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = p-acond
                           fi-qt-acondic5:SCREEN-VALUE IN FRAME {&FRAME-NAME} = p-qt-acond.
        WHEN 6 THEN ASSIGN fi-acondic6:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = p-acond
                           fi-qt-acondic6:SCREEN-VALUE IN FRAME {&FRAME-NAME} = p-qt-acond.
        WHEN 7 THEN ASSIGN fi-acondic7:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = p-acond
                           fi-qt-acondic7:SCREEN-VALUE IN FRAME {&FRAME-NAME} = p-qt-acond.
        WHEN 8 THEN ASSIGN fi-acondic8:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = p-acond
                           fi-qt-acondic8:SCREEN-VALUE IN FRAME {&FRAME-NAME} = p-qt-acond.
        WHEN 9 THEN ASSIGN fi-acondic9:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = p-acond
                           fi-qt-acondic9:SCREEN-VALUE IN FRAME {&FRAME-NAME} = p-qt-acond.
    END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pi-validate V-table-Win 
PROCEDURE Pi-validate :
/*:T------------------------------------------------------------------------------
  Purpose:Validar a viewer     
  Parameters:  <none>
  Notes: N∆o fazer assign aqui. Nesta procedure
  devem ser colocadas apenas validaá‰es, pois neste ponto do programa o registro 
  ainda n∆o foi criado.       
------------------------------------------------------------------------------*/
    {include/i-vldfrm.i} /*:T Validaá∆o de dicion†rio */
    
     IF (INPUT FRAME {&FRAME-NAME} ordem-benefic.tipo-ordem = 1 OR
         INPUT FRAME {&FRAME-NAME} ordem-benefic.tipo-ordem = 4) AND
        cb-tipo-tear:SCREEN-VALUE = "" THEN DO:
        MESSAGE "O Tipo do Tear Ç Inv†lido...." VIEW-AS ALERT-BOX.
        APPLY 'entry' TO cb-tipo-tear.
        RETURN 'ADM-ERROR':U.  
     END.

    IF adm-new-record THEN DO.
       IF LOOKUP(SUBSTR(INPUT FRAME {&FRAME-NAME} ordem-benefic.nr-carro,
                        LENGTH(INPUT FRAME {&FRAME-NAME} ordem-benefic.nr-carro)),"A,B,C,D,E,F,G,H,I,J,K,L") = 0 THEN DO.
           MESSAGE "N£mero de Carro Inv†lido...." VIEW-AS ALERT-BOX.
           APPLY 'entry' TO ordem-benefic.nr-carro.
           RETURN 'ADM-ERROR':U.  
       END.

       IF INPUT FRAME {&FRAME-NAME} ordem-benefic.tipo-ordem = 2 OR
          INPUT FRAME {&FRAME-NAME} ordem-benefic.tipo-ordem = 3 THEN DO.
          FIND b-ordem-benefic WHERE
               b-ordem-benefic.num-trf = INPUT FRAME {&FRAME-NAME} fi-nr-trf NO-LOCK NO-ERROR.
          IF AVAIL b-ordem-benefic THEN DO.
             MESSAGE "Codigo Transformaá∆o j† relacionado em outra OB..."
                       VIEW-AS ALERT-BOX INFO BUTTONS OK.
             APPLY 'entry' TO fi-nr-trf.
             RETURN NO-APPLY.
          END.

          FIND ob-trf WHERE 
               ob-trf.num-trf = INPUT FRAME {&FRAME-NAME} fi-nr-trf NO-LOCK NO-ERROR.
          IF NOT AVAIL ob-trf THEN DO:
             MESSAGE "Codigo Transformaá∆o n∆o cadastrado ! ! !"
                       VIEW-AS ALERT-BOX INFO BUTTONS OK.
             APPLY 'entry' TO fi-nr-trf.
             RETURN 'ADM-ERROR':U.  
          END.
    
          IF INPUT FRAME {&FRAME-NAME} ordem-benefic.tipo-ordem = 2 AND
             SUBSTR(ob-trf.char-1,1,1) = '' THEN DO.
             MESSAGE "Transformaá∆o n∆o n∆o Ç de Retrabalho"
                       VIEW-AS ALERT-BOX INFO BUTTONS OK.
             APPLY 'entry' TO fi-nr-trf.
             RETURN 'ADM-ERROR':U.  
          END.

          IF INPUT FRAME {&FRAME-NAME} ordem-benefic.tipo-ordem = 3 AND
             SUBSTR(ob-trf.char-1,1,1) = 'S' THEN DO.
             MESSAGE "Transformaá∆o Ç de Retrabalho"
                         VIEW-AS ALERT-BOX INFO BUTTONS OK.
             APPLY 'entry' TO fi-nr-trf.
             RETURN NO-APPLY.
          END.
       END.

       IF fi-acondic1:SCREEN-VALUE <> "" THEN DO:
          FIND corte-comerc WHERE
               corte-comerc.descricao = fi-acondic1:SCREEN-VALUE  NO-LOCK NO-ERROR.
          IF NOT AVAIL corte-comerc THEN DO:
             MESSAGE "O Corte Comercial Informado n∆o existe ! ! !"
                      VIEW-AS ALERT-BOX INFO BUTTONS OK.
             APPLY 'entry' TO fi-acondic1 IN FRAME {&FRAME-NAME}.
             RETURN 'ADM-ERROR':U.  
          END.
       END.
       
       IF fi-acondic2:SCREEN-VALUE <> "" THEN DO:
          FIND corte-comerc WHERE
               corte-comerc.descricao = fi-acondic2:SCREEN-VALUE  NO-LOCK NO-ERROR.
          IF NOT AVAIL corte-comerc THEN DO:
             MESSAGE "O Corte Comercial Informado n∆o existe ! ! !"
                      VIEW-AS ALERT-BOX INFO BUTTONS OK.
             APPLY 'entry' TO fi-acondic2 IN FRAME {&FRAME-NAME}.
             RETURN 'ADM-ERROR':U.  
          END.
       END.

       IF fi-acondic3:SCREEN-VALUE <> "" THEN DO:
          FIND corte-comerc WHERE
               corte-comerc.descricao = fi-acondic3:SCREEN-VALUE  NO-LOCK NO-ERROR.
          IF NOT AVAIL corte-comerc THEN DO:
             MESSAGE "O Corte Comercial Informado n∆o existe ! ! !"
                      VIEW-AS ALERT-BOX INFO BUTTONS OK.
             APPLY 'entry' TO fi-acondic3 IN FRAME {&FRAME-NAME}.
             RETURN 'ADM-ERROR':U.  
          END.
       END.

       IF fi-acondic4:SCREEN-VALUE <> "" THEN DO:
          FIND corte-comerc WHERE
               corte-comerc.descricao = fi-acondic4:SCREEN-VALUE  NO-LOCK NO-ERROR.
          IF NOT AVAIL corte-comerc THEN DO:
             MESSAGE "O Corte Comercial Informado n∆o existe ! ! !"
                      VIEW-AS ALERT-BOX INFO BUTTONS OK.
             APPLY 'entry' TO fi-acondic4 IN FRAME {&FRAME-NAME}.
             RETURN 'ADM-ERROR':U.  
          END.
       END.

       IF fi-acondic5:SCREEN-VALUE <> "" THEN DO:
          FIND corte-comerc WHERE
               corte-comerc.descricao = fi-acondic5:SCREEN-VALUE  NO-LOCK NO-ERROR.
          IF NOT AVAIL corte-comerc THEN DO:
             MESSAGE "O Corte Comercial Informado n∆o existe ! ! !"
                      VIEW-AS ALERT-BOX INFO BUTTONS OK.
             APPLY 'entry' TO fi-acondic5 IN FRAME {&FRAME-NAME}.
             RETURN 'ADM-ERROR':U.  
          END.
       END.

       IF fi-acondic6:SCREEN-VALUE <> "" THEN DO:
          FIND corte-comerc WHERE
               corte-comerc.descricao = fi-acondic6:SCREEN-VALUE  NO-LOCK NO-ERROR.
          IF NOT AVAIL corte-comerc THEN DO:
             MESSAGE "O Corte Comercial Informado n∆o existe ! ! !"
                      VIEW-AS ALERT-BOX INFO BUTTONS OK.
             APPLY 'entry' TO fi-acondic6 IN FRAME {&FRAME-NAME}.
             RETURN 'ADM-ERROR':U.  
          END.
       END.

       IF fi-acondic7:SCREEN-VALUE <> "" THEN DO:
          FIND corte-comerc WHERE
               corte-comerc.descricao = fi-acondic7:SCREEN-VALUE  NO-LOCK NO-ERROR.
          IF NOT AVAIL corte-comerc THEN DO:
             MESSAGE "O Corte Comercial Informado n∆o existe ! ! !"
                      VIEW-AS ALERT-BOX INFO BUTTONS OK.
             APPLY 'entry' TO fi-acondic7 IN FRAME {&FRAME-NAME}.
             RETURN 'ADM-ERROR':U.  
          END.
       END.

       IF fi-acondic8:SCREEN-VALUE <> "" THEN DO:
          FIND corte-comerc WHERE
               corte-comerc.descricao = fi-acondic8:SCREEN-VALUE  NO-LOCK NO-ERROR.
          IF NOT AVAIL corte-comerc THEN DO:
             MESSAGE "O Corte Comercial Informado n∆o existe ! ! !"
                      VIEW-AS ALERT-BOX INFO BUTTONS OK.
             APPLY 'entry' TO fi-acondic8 IN FRAME {&FRAME-NAME}.
             RETURN 'ADM-ERROR':U.  
          END.
       END.

       IF fi-acondic9:SCREEN-VALUE <> "" THEN DO:
          FIND corte-comerc WHERE
               corte-comerc.descricao = fi-acondic9:SCREEN-VALUE  NO-LOCK NO-ERROR.
          IF NOT AVAIL corte-comerc THEN DO:
             MESSAGE "O Corte Comercial Informado n∆o existe ! ! !"
                      VIEW-AS ALERT-BOX INFO BUTTONS OK.
             APPLY 'entry' TO fi-acondic9 IN FRAME {&FRAME-NAME}.
             RETURN 'ADM-ERROR':U.  
          END.
       END.

       /* Calcula o ÷ndice da OB */
       ASSIGN i-ind-ob = 0.
       FOR EACH b-ordem-benefic WHERE
                b-ordem-benefic.nr-ob = INPUT FRAME {&FRAME-NAME} ordem-benefic.nr-ob
                BY b-ordem-benefic.ind-ob.
           ASSIGN i-ind-ob = b-ordem-benefic.ind-ob.
       END.
       ASSIGN i-ind-ob = i-ind-ob + 1.

       IF i-ind-ob > ob-param.max-carro-ob THEN DO.
          MESSAGE "Atingido o n£mero de m†ximo de Carros permitidos para esta OB..." VIEW-AS ALERT-BOX.
          APPLY 'entry' TO ordem-benefic.nr-ob.
          RETURN 'ADM-ERROR':U.  
       END.

       FIND b-ordem-benefic WHERE
            b-ordem-benefic.nr-ob = INPUT FRAME {&FRAME-NAME} ordem-benefic.nr-ob AND
            b-ordem-benefic.ind-ob = i-ind-ob NO-LOCK NO-ERROR.

       IF AMBIGUOUS b-ordem-benefic OR
          AVAIL b-ordem-benefic THEN DO.
          MESSAGE "Imposs°vel Gravar essa OB, indice Duplicado.." VIEW-AS ALERT-BOX.
          APPLY 'entry' TO ordem-benefic.nr-ob.
          RETURN 'ADM-ERROR':U.  
       END.
    END.

    FIND item WHERE
         item.it-codigo = INPUT FRAME {&FRAME-NAME} ordem-benefic.it-codigo 
         NO-LOCK NO-ERROR.
    IF NOT AVAIL item THEN DO.
       MESSAGE "Item n∆o Cadastrado..." VIEW-AS ALERT-BOX.
       APPLY 'entry' TO ordem-benefic.it-codigo.
       RETURN 'ADM-ERROR':U.  
    END.
    
    IF item.tipo-con-est = 4 THEN DO.
       FIND referencia WHERE
            referencia.cod-refer = INPUT FRAME {&FRAME-NAME} ordem-benefic.cod-refer 
            NO-LOCK NO-ERROR.
       IF NOT AVAIL referencia THEN DO.
          MESSAGE "Referància n∆o cadastrada..." VIEW-AS ALERT-BOX.
          APPLY 'entry' TO ordem-benefic.cod-refer.
          RETURN 'ADM-ERROR':U.  
       END.

       FIND ref-item-ext WHERE
            ref-item-ext.it-codigo = INPUT FRAME {&FRAME-NAME} ordem-benefic.it-codigo AND
            ref-item-ext.cod-refer = INPUT FRAME {&FRAME-NAME} ordem-benefic.cod-refer
            NO-LOCK NO-ERROR.
       IF NOT AVAIL ref-item-ext THEN DO.
          MESSAGE "Referància n∆o est† relacionada ao Item..." VIEW-AS ALERT-BOX.
          APPLY 'entry' TO ordem-benefic.cod-refer.
          RETURN 'ADM-ERROR':U.  
       END.

       IF INPUT FRAME {&FRAME-NAME} ordem-benefic.tipo-ordem = 1 AND
          INPUT FRAME {&FRAME-NAME} ordem-benefic.num-progr = 0 THEN DO.
          MESSAGE 'N∆o Selecionada a Programaá∆o de Produá∆o para o Item/Referància...'
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
          APPLY 'ENTRY' TO ordem-benefic.cod-refer.
          RETURN 'ADM-ERROR':U.
       END.
    END.
      
    IF INPUT FRAME {&FRAME-NAME} fi-acondic1 = "" THEN DO.
       MESSAGE "N∆o foram informados os Acondicionamentos..." VIEW-AS ALERT-BOX.
       APPLY 'entry' TO fi-acondic1.
       RETURN 'ADM-ERROR':U.  
    END.

    RUN pi-ver-acond (INPUT fi-acondic1:HANDLE,
                      INPUT fi-qt-acondic1:HANDLE).
    IF RETURN-VALUE = 'ADM-ERROR':U THEN RETURN 'ADM-ERROR':U.
    ASSIGN INPUT FRAME {&FRAME-NAME} fi-acondic1
           INPUT FRAME {&FRAME-NAME} fi-qt-acondic1.

    RUN pi-ver-acond (INPUT fi-acondic2:HANDLE,
                      INPUT fi-qt-acondic2:HANDLE).
    IF RETURN-VALUE = 'ADM-ERROR':U THEN RETURN 'ADM-ERROR':U.
    ASSIGN INPUT FRAME {&FRAME-NAME} fi-acondic2
           INPUT FRAME {&FRAME-NAME} fi-qt-acondic2.

    RUN pi-ver-acond (INPUT fi-acondic3:HANDLE,
                      INPUT fi-qt-acondic3:HANDLE).
    IF RETURN-VALUE = 'ADM-ERROR':U THEN RETURN 'ADM-ERROR':U.
    ASSIGN INPUT FRAME {&FRAME-NAME} fi-acondic3
           INPUT FRAME {&FRAME-NAME} fi-qt-acondic3.

    RUN pi-ver-acond (INPUT fi-acondic4:HANDLE,
                      INPUT fi-qt-acondic4:HANDLE).
    IF RETURN-VALUE = 'ADM-ERROR':U THEN RETURN 'ADM-ERROR':U.
    ASSIGN INPUT FRAME {&FRAME-NAME} fi-acondic4
           INPUT FRAME {&FRAME-NAME} fi-qt-acondic4.

    RUN pi-ver-acond (INPUT fi-acondic5:HANDLE,
                      INPUT fi-qt-acondic5:HANDLE).
    IF RETURN-VALUE = 'ADM-ERROR':U THEN RETURN 'ADM-ERROR':U.
    ASSIGN INPUT FRAME {&FRAME-NAME} fi-acondic5
           INPUT FRAME {&FRAME-NAME} fi-qt-acondic5.

    RUN pi-ver-acond (INPUT fi-acondic6:HANDLE,
                      INPUT fi-qt-acondic6:HANDLE).
    IF RETURN-VALUE = 'ADM-ERROR':U THEN RETURN 'ADM-ERROR':U.
    ASSIGN INPUT FRAME {&FRAME-NAME} fi-acondic6
           INPUT FRAME {&FRAME-NAME} fi-qt-acondic6.

    RUN pi-ver-acond (INPUT fi-acondic7:HANDLE,
                      INPUT fi-qt-acondic7:HANDLE).
    IF RETURN-VALUE = 'ADM-ERROR':U THEN RETURN 'ADM-ERROR':U.
    ASSIGN INPUT FRAME {&FRAME-NAME} fi-acondic7
           INPUT FRAME {&FRAME-NAME} fi-qt-acondic7.

    RUN pi-ver-acond (INPUT fi-acondic8:HANDLE,
                      INPUT fi-qt-acondic8:HANDLE).
    IF RETURN-VALUE = 'ADM-ERROR':U THEN RETURN 'ADM-ERROR':U.
    ASSIGN INPUT FRAME {&FRAME-NAME} fi-acondic8
           INPUT FRAME {&FRAME-NAME} fi-qt-acondic8.

    RUN pi-ver-acond (INPUT fi-acondic9:HANDLE,
                      INPUT fi-qt-acondic9:HANDLE).
    IF RETURN-VALUE = 'ADM-ERROR':U THEN RETURN 'ADM-ERROR':U.
    ASSIGN INPUT FRAME {&FRAME-NAME} fi-acondic9
           INPUT FRAME {&FRAME-NAME} fi-qt-acondic9.
    
    ASSIGN INPUT FRAME {&FRAME-NAME} fi-nr-trf.

    IF adm-new-record AND
       INPUT FRAME {&FRAME-NAME} ordem-benefic.tipo-ordem = 1 THEN DO.
       RUN pi-ver-estrutura.
       IF RETURN-VALUE = 'ADM-ERROR':U THEN RETURN 'ADM-ERROR':U.
    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-ver-acond V-table-Win 
PROCEDURE pi-ver-acond :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER p-acond AS HANDLE.
    DEF INPUT PARAMETER p-qt-acond AS HANDLE.

    IF p-acond:SCREEN-VALUE <> "" AND
       NOT p-acond:SCREEN-VALUE BEGINS "Peca" AND 
       NOT p-acond:SCREEN-VALUE BEGINS "Rolo" AND
       NOT p-acond:SCREEN-VALUE BEGINS "Corte" THEN DO.
       MESSAGE "Acondicionamento " p-acond " diferente de Rolo e Peca..." SKIP(1)
               VIEW-AS ALERT-BOX ERROR.  
       RETURN 'ADM-ERROR':U.  
    END.

    IF p-acond:SCREEN-VALUE    <> "" and
       p-qt-acond:SCREEN-VALUE = '' THEN DO.
       MESSAGE "Acondicionamento sem quantidade Planejada.."
                VIEW-AS ALERT-BOX ERROR.
       RETURN 'ADM-ERROR':U.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-ver-desenho V-table-Win 
PROCEDURE pi-ver-desenho :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* Busca Imagens da Referància */
  ASSIGN c-arq-image = SESSION:TEMP-DIRECTORY + SUBSTR(referencia.cod-refer,3,4) + '.txt'.
         c-comando = 'DIR /b ' + param-dis.dir-img-item + '\*' + SUBSTR(referencia.cod-refer,3,4) + '* >' +
                     c-arq-image.
  OS-COMMAND SILENT VALUE(c-comando) NO-ERROR.

  EMPTY TEMP-TABLE tt-fotos.

  IF SEARCH(c-arq-image) <> ? THEN DO.
     INPUT FROM VALUE(c-arq-image) NO-ECHO.
     REPEAT.
         CREATE tt-fotos.
         IMPORT tt-fotos.
     END.
     INPUT CLOSE.
  END.

  FOR EACH tt-fotos.
      IF tt-fotos.arq-image MATCHES '*' + SUBSTR(referencia.cod-refer,3,4) + '*' THEN DO.
         ASSIGN tt-fotos.arq-image = param-dis.dir-img-item + "\" + tt-fotos.arq-image.
         NEXT.
      END.
      ELSE
         DELETE tt-fotos.
  END.

  FIND FIRST tt-fotos NO-ERROR.
  ASSIGN bt-desenho:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
  IF AVAIL tt-fotos THEN
     ASSIGN bt-desenho:SENSITIVE = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-ver-estrutura V-table-Win 
PROCEDURE pi-ver-estrutura :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN c-item-cru = "".
    FOR EACH ref-estrut WHERE
             ref-estrut.it-codigo  = INPUT FRAME {&FRAME-NAME} ordem-benefic.it-codigo AND
             ref-estrut.cod-ref-it = INPUT FRAME {&FRAME-NAME} ordem-benefic.cod-refer
             NO-LOCK.
    
        FIND estrutura WHERE
             estrutura.it-codigo = ref-estrut.it-codigo AND
             estrutura.es-codigo = ref-estrut.es-codigo NO-LOCK NO-ERROR.
    
        IF estrutura.fantasma THEN 
           RUN pi-ver-fantasma (estrutura.es-codigo). 
        ELSE DO.
            FIND item WHERE
                 item.it-codigo = estrutura.es-codigo NO-LOCK NO-ERROR.
            IF item.ge-codigo = 50 or
               item.ge-codigo = 57 THEN
               ASSIGN c-item-cru = estrutura.es-codigo.
        END.

        IF c-item-cru <> "" THEN LEAVE.
    END.

    IF c-item-cru = "" THEN DO.
       MESSAGE "Item sem Estrutura, OB n∆o pode ser Revisada...." VIEW-AS ALERT-BOX.
       RETURN 'ADM-ERROR':U.
    END.

    ASSIGN de-sld-estoque = 0.
    FOR EACH saldo-estoq WHERE
             saldo-estoq.it-codigo = c-item-cru AND 
             saldo-estoq.cod-depos = 'BEN' NO-LOCK.
        ASSIGN de-sld-estoque = de-sld-estoque + saldo-estoq.qtidade-atu.
    END.    

    IF de-sld-estoque < INPUT FRAME {&FRAME-NAME} ordem-benefic.quantidade THEN DO.
       MESSAGE "Tecido CRU sem Saldo em Estoque, OB n∆o pode ser Revisada...." VIEW-AS ALERT-BOX.
       RETURN 'ADM-ERROR':U.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-ver-fantasma V-table-Win 
PROCEDURE pi-ver-fantasma :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER p-it-codigo AS CHAR.
    FOR EACH estrutura WHERE
             estrutura.it-codigo = p-it-codigo NO-LOCK.

        IF estrutura.fantasma THEN
           RUN pi-ver-fantasma (estrutura.es-codigo).
        ELSE DO.
            FIND item WHERE
                 item.it-codigo = estrutura.es-codigo NO-LOCK NO-ERROR.
            IF item.ge-codigo = 50 OR 
               item.ge-codigo = 57 THEN
               ASSIGN c-item-cru = estrutura.es-codigo.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-ver-pcp V-table-Win 
PROCEDURE pi-ver-pcp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR de-sld-pron AS DEC.
  DEF VAR de-tot-pron AS DEC.

  ASSIGN c-num-pcp = ""
         de-sld-pron = 0.
  FOR EACH ob-pcp WHERE
           ob-pcp.situacao = 1 AND
           ob-pcp.it-codigo = INPUT FRAME {&FRAME-NAME} ordem-benefic.it-codigo NO-LOCK,
      EACH ob-pcp-ref OF ob-pcp WHERE
           ob-pcp-ref.situacao = 1 AND
           ob-pcp-ref.cod-refer = INPUT FRAME {&FRAME-NAME} ordem-benefic.cod-refer AND
           ob-pcp-ref.qtd-pron > 0 NO-LOCK.

      ASSIGN de-sld-pron = de-sld-pron + ob-pcp-ref.qtd-pron.
      FOR EACH ordem-benefic WHERE
               ordem-benefic.it-codigo = ob-pcp.it-codigo AND
               ordem-benefic.cod-refer = ob-pcp-ref.cod-refer AND
               ordem-benefic.num-prog = ob-pcp.num-prog.

          FOR EACH ob-etiqueta OF ordem-benefic WHERE
                   ob-etiqueta.nr-reporte = 0 NO-LOCK.
              ASSIGN de-sld-pron = de-sld-pron - ob-etiqueta.quantidade.
          END.
      END.

      IF de-sld-pron > 0 THEN 
         ASSIGN c-num-pcp = IF c-num-pcp = ""
                            THEN STRING(ob-pcp.num-progr)
                            ELSE c-num-pcp + ';' + STRING(ob-pcp.num-progr)
                de-tot-pron = de-tot-pron + de-sld-pron
                de-sld-pron = 0.
  END.

  IF NUM-ENTRIES(c-num-pcp,";") = 0 THEN DO.
     MESSAGE 'N∆o Encontrado Programaá∆o de Produá∆o com Saldo para o Item/Referància...' SKIP
             'Saldo Pronto: ' + STRING(de-tot-pron,">>>,>>9.99") 
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
     RETURN 'ADM-ERROR'.
  END.

  IF NUM-ENTRIES(c-num-pcp,";") >= 2 THEN DO.
     RUN esp/essp0100b.p (INPUT-OUTPUT c-num-pcp,
                          INPUT INPUT FRAME {&FRAME-NAME} ordem-benefic.cod-refer,
                          OUTPUT l-ok).

     IF l-ok = NO THEN DO.
        MESSAGE 'N∆o Selecionada a Programaá∆o de Produá∆o para o Item/Referància...'
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN 'ADM-ERROR'.
     END.
  END.

  IF de-tot-pron = 0 THEN DO.
     MESSAGE 'N∆o existe Quantidade Pronta para o Item/Referància...'
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
     RETURN 'ADM-ERROR'.  
  END.

  FIND ob-pcp-ref WHERE
       ob-pcp-ref.num-prog = INT(c-num-pcp) AND
       ob-pcp-ref.cod-refer = INPUT FRAME {&FRAME-NAME} ordem-benefic.cod-refer
       NO-LOCK NO-ERROR.

  ASSIGN ordem-benefic.num-progr:SCREEN-VALUE IN FRAME {&FRAME-NAME} = c-num-pcp
         ordem-benefic.quantidade:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ob-pcp-ref.qtd-pron)
         ordem-benefic.observacao:SCREEN-VALUE = ordem-benefic.observacao:SCREEN-VALUE + CHR(13) + "OB PCP: " + ob-pcp-ref.observ. 

  ASSIGN bt-carteira:SENSITIVE = YES.

  RUN pi-calc-carteira.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "ordem-benefic"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed V-table-Win 
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
      {src/adm/template/vstates.i}
  END CASE.
  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

