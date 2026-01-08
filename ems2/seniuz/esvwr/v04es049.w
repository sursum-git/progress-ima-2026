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
{include/i-prgvrs.i V01ES049 2.04.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
&Scop adm-attribute-dlg support/viewerd.w

/* global variable definitions */

/* Buffer  Definitions ---                                           */
DEF BUFFER b-etiqueta FOR ob-etiqueta.

/* Local Variable Definitions ---                                       */
def var v-row-parent AS ROWID NO-UNDO.
DEF VAR i-nr-seq     LIKE ob-etiqueta.nr-sequencia.
DEF VAR i-tp-embal   LIKE corte-comerc.tp-embalag.
DEF VAR c-etiquetas  AS   CHAR FORMAT "x(30)".
DEF VAR de-tot-amostra AS DEC.
DEF VAR de-media-peso AS DEC.
DEF VAR de-peso-calc AS DEC.
DEF VAR de-qtd-max-corte AS DEC INIT 1.50.


/* Variaveis para impress∆o de etiquetas em EPL */
DEF VAR i-ct            AS INT.
DEF VAR i-qt-etq        AS INT.
DEF VAR c-desc-item     AS CHAR FORMAT "x(36)".
DEF VAR v-defeito       AS CHAR EXTENT 3.
DEF VAR i-lote          AS INT.
DEF VAR c-comando       AS CHAR.
DEF VAR c-code-ant      AS CHAR.
DEF VAR i-sit-ant       AS INT.
DEF VAR c-desc-situacao AS CHAR FORMAT "x(20)".
DEF VAR i-num-bar       AS INT.
DEF VAR l-erro-estoq    AS LOG.
DEF VAR l-erro-transf   AS LOG.

/* Includes para codigo de Barras */
{esinc/sz-pcl.i}

/* Includes para gerar Movto Estoq */

DEF VAR i-etq   LIKE bc-param-ext.param-inteiro.
DEF VAR c-cod-chave-param-ext LIKE bc-param-ext.cod-chave-param-ext.

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
&Scoped-define EXTERNAL-TABLES ob-etiqueta
&Scoped-define FIRST-EXTERNAL-TABLE ob-etiqueta


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR ob-etiqueta.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS ob-etiqueta.quantidade 
&Scoped-define ENABLED-TABLES ob-etiqueta
&Scoped-define FIRST-ENABLED-TABLE ob-etiqueta
&Scoped-Define ENABLED-OBJECTS rt-key 
&Scoped-Define DISPLAYED-FIELDS ob-etiqueta.num-etiqueta ob-etiqueta.nr-ob ~
ob-etiqueta.it-codigo ob-etiqueta.cod-refer ob-etiqueta.nr-lote ~
ob-etiqueta.quantidade ob-etiqueta.cod-estabel ob-etiqueta.corte-comerc 
&Scoped-define DISPLAYED-TABLES ob-etiqueta
&Scoped-define FIRST-DISPLAYED-TABLE ob-etiqueta
&Scoped-Define DISPLAYED-OBJECTS rs-destino1 fi-localiz fi-desc-item ~
fi-desc-refer fi-qtd-cortes1 fi-qtd-amostra1 fi-qtd-cortes2 fi-qtd-amostra2 ~
fi-qtd-cortes3 fi-qtd-amostra3 fi-qtd-cortes4 fi-qtd-amostra4 ~
fi-qtd-cortes5 fi-qtd-amostra5 fi-qtd-trapo fi-tot-amostra fi-qtd-saldo ~
tg-imp-etq tg-reimp fi-desc-corte rs-destino2 rs-destino3 rs-destino4 ~
rs-destino5 fi-tot-venda 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define List-4 rs-destino1 fi-qtd-cortes1 fi-qtd-amostra1 ~
fi-qtd-cortes2 fi-qtd-amostra2 fi-qtd-cortes3 fi-qtd-amostra3 ~
fi-qtd-cortes4 fi-qtd-amostra4 fi-qtd-cortes5 fi-qtd-amostra5 tg-imp-etq ~
tg-reimp rs-destino2 rs-destino3 rs-destino4 rs-destino5 

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
DEFINE VARIABLE fi-desc-corte AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-item AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 50.43 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-refer AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 31.43 BY .88 NO-UNDO.

DEFINE VARIABLE fi-localiz AS CHARACTER FORMAT "999/999":U 
     LABEL "Localizaá∆o" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88
     FONT 0 NO-UNDO.

DEFINE VARIABLE fi-qtd-amostra1 AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-qtd-amostra2 AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-qtd-amostra3 AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-qtd-amostra4 AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-qtd-amostra5 AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-qtd-cortes1 AS INTEGER FORMAT ">>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-qtd-cortes2 AS INTEGER FORMAT ">>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-qtd-cortes3 AS INTEGER FORMAT ">>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-qtd-cortes4 AS INTEGER FORMAT ">>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-qtd-cortes5 AS INTEGER FORMAT ">>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-qtd-saldo AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Saldo Peáa" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88
     FGCOLOR 12 FONT 0 NO-UNDO.

DEFINE VARIABLE fi-qtd-trapo AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Consumo" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-tot-amostra AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Total Amostra" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88
     FGCOLOR 9 FONT 0 NO-UNDO.

DEFINE VARIABLE fi-tot-venda AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Total Venda" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88
     FGCOLOR 9 FONT 0 NO-UNDO.

DEFINE VARIABLE rs-destino1 AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Venda", 2,
"Amostra", 4
     SIZE 19 BY .75 NO-UNDO.

DEFINE VARIABLE rs-destino2 AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Venda", 2,
"Amostra", 4
     SIZE 19 BY .75 NO-UNDO.

DEFINE VARIABLE rs-destino3 AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Venda", 2,
"Amostra", 4
     SIZE 19 BY .75 NO-UNDO.

DEFINE VARIABLE rs-destino4 AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Venda", 2,
"Amostra", 4
     SIZE 19 BY .75 NO-UNDO.

DEFINE VARIABLE rs-destino5 AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Venda", 2,
"Amostra", 4
     SIZE 19 BY .75 NO-UNDO.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88 BY 6.5.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88 BY 7.5.

DEFINE VARIABLE tg-imp-etq AS LOGICAL INITIAL yes 
     LABEL "Imprime Etiquetas de Corte" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .83 NO-UNDO.

DEFINE VARIABLE tg-reimp AS LOGICAL INITIAL yes 
     LABEL "Re-imprimir Etiqueta Original" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .83 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     rs-destino1 AT ROW 9.13 COL 38.72 NO-LABEL WIDGET-ID 20
     ob-etiqueta.num-etiqueta AT ROW 1.25 COL 43.43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .88
          FONT 6
     fi-localiz AT ROW 2.25 COL 16 COLON-ALIGNED
     ob-etiqueta.nr-ob AT ROW 3.25 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.14 BY .88
     ob-etiqueta.it-codigo AT ROW 4.25 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY .88
     fi-desc-item AT ROW 4.25 COL 28.57 COLON-ALIGNED NO-LABEL
     ob-etiqueta.cod-refer AT ROW 5.25 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY .88
     fi-desc-refer AT ROW 5.25 COL 28.57 COLON-ALIGNED NO-LABEL
     ob-etiqueta.nr-lote AT ROW 5.25 COL 65 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5 BY .88
     ob-etiqueta.quantidade AT ROW 6.25 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
     fi-qtd-cortes1 AT ROW 9 COL 16 COLON-ALIGNED NO-LABEL
     fi-qtd-amostra1 AT ROW 9 COL 24.14 COLON-ALIGNED NO-LABEL
     fi-qtd-cortes2 AT ROW 10 COL 16 COLON-ALIGNED NO-LABEL
     fi-qtd-amostra2 AT ROW 10 COL 24.14 COLON-ALIGNED NO-LABEL
     fi-qtd-cortes3 AT ROW 11 COL 16 COLON-ALIGNED NO-LABEL
     fi-qtd-amostra3 AT ROW 11 COL 24.14 COLON-ALIGNED NO-LABEL
     fi-qtd-cortes4 AT ROW 12 COL 16 COLON-ALIGNED NO-LABEL
     fi-qtd-amostra4 AT ROW 12 COL 24.14 COLON-ALIGNED NO-LABEL
     fi-qtd-cortes5 AT ROW 13 COL 16 COLON-ALIGNED NO-LABEL
     fi-qtd-amostra5 AT ROW 13 COL 24.14 COLON-ALIGNED NO-LABEL
     fi-qtd-trapo AT ROW 9 COL 69 COLON-ALIGNED
     fi-tot-amostra AT ROW 11.5 COL 69 COLON-ALIGNED
     fi-qtd-saldo AT ROW 13 COL 69 COLON-ALIGNED
     tg-imp-etq AT ROW 14.25 COL 18
     tg-reimp AT ROW 14.25 COL 48.14
     ob-etiqueta.cod-estabel AT ROW 1.25 COL 16 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 4 BY .88
          FGCOLOR 12 FONT 6
     ob-etiqueta.corte-comerc AT ROW 3.25 COL 63 COLON-ALIGNED WIDGET-ID 18
          LABEL "Corte Comercial"
          VIEW-AS FILL-IN 
          SIZE 2.29 BY .88
     fi-desc-corte AT ROW 3.25 COL 65.86 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     rs-destino2 AT ROW 10.17 COL 38.86 NO-LABEL WIDGET-ID 24
     rs-destino3 AT ROW 11.17 COL 38.86 NO-LABEL WIDGET-ID 28
     rs-destino4 AT ROW 12.17 COL 38.86 NO-LABEL WIDGET-ID 32
     rs-destino5 AT ROW 13.13 COL 38.86 NO-LABEL WIDGET-ID 36
     fi-tot-venda AT ROW 10.5 COL 69 COLON-ALIGNED WIDGET-ID 40
     "Qt Cortes" VIEW-AS TEXT
          SIZE 8 BY 1 AT ROW 8 COL 15.43
          FGCOLOR 12 FONT 6
     "Metragem" VIEW-AS TEXT
          SIZE 9 BY 1 AT ROW 8 COL 26
          FGCOLOR 12 FONT 6
     "Destino" VIEW-AS TEXT
          SIZE 9 BY 1 AT ROW 8 COL 39 WIDGET-ID 4
          FGCOLOR 12 FONT 6
     rt-key AT ROW 1 COL 1.14
     rt-mold AT ROW 7.75 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: espec.ob-etiqueta
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
         HEIGHT             = 14.54
         WIDTH              = 88.29.
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

/* SETTINGS FOR FILL-IN ob-etiqueta.cod-estabel IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ob-etiqueta.cod-refer IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ob-etiqueta.corte-comerc IN FRAME f-main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN fi-desc-corte IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-item IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-refer IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-localiz IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qtd-amostra1 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-qtd-amostra2 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-qtd-amostra3 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-qtd-amostra4 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-qtd-amostra5 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-qtd-cortes1 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-qtd-cortes2 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-qtd-cortes3 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-qtd-cortes4 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-qtd-cortes5 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-qtd-saldo IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qtd-trapo IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-amostra IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-venda IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ob-etiqueta.it-codigo IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ob-etiqueta.nr-lote IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ob-etiqueta.nr-ob IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ob-etiqueta.num-etiqueta IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET rs-destino1 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR RADIO-SET rs-destino2 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR RADIO-SET rs-destino3 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR RADIO-SET rs-destino4 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR RADIO-SET rs-destino5 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR RECTANGLE rt-mold IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-imp-etq IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR TOGGLE-BOX tg-reimp IN FRAME f-main
   NO-ENABLE 4                                                          */
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

&Scoped-define SELF-NAME fi-qtd-amostra1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-qtd-amostra1 V-table-Win
ON LEAVE OF fi-qtd-amostra1 IN FRAME f-main
DO:
   IF SELF:INPUT-VALUE = 0 THEN DO.
      ASSIGN fi-qtd-cortes1:SCREEN-VALUE = '0'.

      APPLY 'ENTRY' TO fi-qtd-trapo.
      RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-qtd-amostra1 V-table-Win
ON VALUE-CHANGED OF fi-qtd-amostra1 IN FRAME f-main
DO:
   RUN pi-calc-saldo.
   RUN pi-calc-amostra.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-qtd-amostra2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-qtd-amostra2 V-table-Win
ON LEAVE OF fi-qtd-amostra2 IN FRAME f-main
DO:
   IF SELF:INPUT-VALUE = 0 THEN DO.
      ASSIGN fi-qtd-cortes2:SCREEN-VALUE = '0'.

      APPLY 'ENTRY' TO fi-qtd-trapo.
      RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-qtd-amostra2 V-table-Win
ON VALUE-CHANGED OF fi-qtd-amostra2 IN FRAME f-main
DO:
   RUN pi-calc-saldo.
   RUN pi-calc-amostra.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-qtd-amostra3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-qtd-amostra3 V-table-Win
ON LEAVE OF fi-qtd-amostra3 IN FRAME f-main
DO:
   IF SELF:INPUT-VALUE = 0 THEN DO.
      ASSIGN fi-qtd-cortes3:SCREEN-VALUE = '0'.

      APPLY 'ENTRY' TO fi-qtd-trapo.
      RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-qtd-amostra3 V-table-Win
ON VALUE-CHANGED OF fi-qtd-amostra3 IN FRAME f-main
DO:
    RUN pi-calc-saldo.
    RUN pi-calc-amostra.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-qtd-amostra4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-qtd-amostra4 V-table-Win
ON LEAVE OF fi-qtd-amostra4 IN FRAME f-main
DO:
   IF SELF:INPUT-VALUE = 0 THEN DO.
      ASSIGN fi-qtd-cortes4:SCREEN-VALUE = '0'.

      APPLY 'ENTRY' TO fi-qtd-trapo.
      RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-qtd-amostra4 V-table-Win
ON VALUE-CHANGED OF fi-qtd-amostra4 IN FRAME f-main
DO:
   RUN pi-calc-saldo.
   RUN pi-calc-amostra.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-qtd-amostra5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-qtd-amostra5 V-table-Win
ON LEAVE OF fi-qtd-amostra5 IN FRAME f-main
DO:
   IF SELF:INPUT-VALUE = 0 THEN DO.
      ASSIGN fi-qtd-cortes5:SCREEN-VALUE = '0'.

      APPLY 'ENTRY' TO fi-qtd-trapo.
      RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-qtd-amostra5 V-table-Win
ON VALUE-CHANGED OF fi-qtd-amostra5 IN FRAME f-main
DO:
   RUN pi-calc-saldo.
   RUN pi-calc-amostra.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-qtd-cortes1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-qtd-cortes1 V-table-Win
ON ENTRY OF fi-qtd-cortes1 IN FRAME f-main
DO:
  IF SELF:INPUT-VALUE = 0 THEN
     ASSIGN SELF:SCREEN-VALUE = '1'.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-qtd-cortes1 V-table-Win
ON LEAVE OF fi-qtd-cortes1 IN FRAME f-main
DO:
   IF SELF:INPUT-VALUE = 0 THEN DO.
      APPLY 'ENTRY' TO fi-qtd-trapo.
      RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-qtd-cortes1 V-table-Win
ON VALUE-CHANGED OF fi-qtd-cortes1 IN FRAME f-main
DO:
  RUN pi-calc-saldo.
  RUN pi-calc-amostra.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-qtd-cortes2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-qtd-cortes2 V-table-Win
ON ENTRY OF fi-qtd-cortes2 IN FRAME f-main
DO:
  IF SELF:INPUT-VALUE = 0 THEN
     ASSIGN SELF:SCREEN-VALUE = '1'.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-qtd-cortes2 V-table-Win
ON LEAVE OF fi-qtd-cortes2 IN FRAME f-main
DO:
   IF SELF:INPUT-VALUE = 0 THEN DO.
      APPLY 'ENTRY' TO fi-qtd-trapo.
      RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-qtd-cortes2 V-table-Win
ON VALUE-CHANGED OF fi-qtd-cortes2 IN FRAME f-main
DO:
   RUN pi-calc-saldo.
   RUN pi-calc-amostra.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-qtd-cortes3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-qtd-cortes3 V-table-Win
ON ENTRY OF fi-qtd-cortes3 IN FRAME f-main
DO:
  IF SELF:INPUT-VALUE = 0 THEN
     ASSIGN SELF:SCREEN-VALUE = '1'.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-qtd-cortes3 V-table-Win
ON LEAVE OF fi-qtd-cortes3 IN FRAME f-main
DO:
  IF SELF:INPUT-VALUE = 0 THEN DO.
     APPLY 'ENTRY' TO fi-qtd-trapo.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-qtd-cortes3 V-table-Win
ON VALUE-CHANGED OF fi-qtd-cortes3 IN FRAME f-main
DO:
   RUN pi-calc-saldo.
   RUN pi-calc-amostra.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-qtd-cortes4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-qtd-cortes4 V-table-Win
ON ENTRY OF fi-qtd-cortes4 IN FRAME f-main
DO:
  IF SELF:INPUT-VALUE = 0 THEN
     ASSIGN SELF:SCREEN-VALUE = '1'.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-qtd-cortes4 V-table-Win
ON LEAVE OF fi-qtd-cortes4 IN FRAME f-main
DO:
   IF SELF:INPUT-VALUE = 0 THEN DO.
      APPLY 'ENTRY' TO fi-qtd-trapo.
      RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-qtd-cortes4 V-table-Win
ON VALUE-CHANGED OF fi-qtd-cortes4 IN FRAME f-main
DO:
  RUN pi-calc-saldo.
  RUN pi-calc-amostra.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-qtd-cortes5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-qtd-cortes5 V-table-Win
ON ENTRY OF fi-qtd-cortes5 IN FRAME f-main
DO:
  IF SELF:INPUT-VALUE = 0 THEN
     ASSIGN SELF:SCREEN-VALUE = '1'.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-qtd-cortes5 V-table-Win
ON LEAVE OF fi-qtd-cortes5 IN FRAME f-main
DO:
   IF SELF:INPUT-VALUE = 0 THEN DO.
      APPLY 'ENTRY' TO fi-qtd-trapo.
      RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-qtd-cortes5 V-table-Win
ON VALUE-CHANGED OF fi-qtd-cortes5 IN FRAME f-main
DO:
   RUN pi-calc-saldo.
   RUN pi-calc-amostra.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-qtd-trapo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-qtd-trapo V-table-Win
ON VALUE-CHANGED OF fi-qtd-trapo IN FRAME f-main /* Consumo */
DO:
   RUN pi-calc-saldo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-tot-amostra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-tot-amostra V-table-Win
ON VALUE-CHANGED OF fi-tot-amostra IN FRAME f-main /* Total Amostra */
DO:
   ASSIGN fi-qtd-saldo:SCREEN-VALUE = STRING(ob-etiqueta.quantidade - 
                                             (SELF:INPUT-VALUE * fi-qtd-cortes1:INPUT-VALUE))
          fi-qtd-trapo:SCREEN-VALUE = STRING(ob-etiqueta.quantidade - 
                                             (SELF:INPUT-VALUE * fi-qtd-cortes1:INPUT-VALUE) -
                                             fi-qtd-saldo:INPUT-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-tot-venda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-tot-venda V-table-Win
ON VALUE-CHANGED OF fi-tot-venda IN FRAME f-main /* Total Venda */
DO:
   ASSIGN fi-qtd-saldo:SCREEN-VALUE = STRING(ob-etiqueta.quantidade - 
                                             (SELF:INPUT-VALUE * fi-qtd-cortes1:INPUT-VALUE))
          fi-qtd-trapo:SCREEN-VALUE = STRING(ob-etiqueta.quantidade - 
                                             (SELF:INPUT-VALUE * fi-qtd-cortes1:INPUT-VALUE) -
                                             fi-qtd-saldo:INPUT-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-destino1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-destino1 V-table-Win
ON VALUE-CHANGED OF rs-destino1 IN FRAME f-main
DO:
  RUN pi-calc-saldo.
  RUN pi-calc-amostra.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-destino2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-destino2 V-table-Win
ON VALUE-CHANGED OF rs-destino2 IN FRAME f-main
DO:
  RUN pi-calc-saldo.
  RUN pi-calc-amostra.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-destino3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-destino3 V-table-Win
ON VALUE-CHANGED OF rs-destino3 IN FRAME f-main
DO:
  RUN pi-calc-saldo.
  RUN pi-calc-amostra.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-destino4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-destino4 V-table-Win
ON VALUE-CHANGED OF rs-destino4 IN FRAME f-main
DO:
  RUN pi-calc-saldo.
  RUN pi-calc-amostra.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-destino5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-destino5 V-table-Win
ON VALUE-CHANGED OF rs-destino5 IN FRAME f-main
DO:
  RUN pi-calc-saldo.
  RUN pi-calc-amostra.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

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
  {src/adm/template/row-list.i "ob-etiqueta"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "ob-etiqueta"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
    {include/i-valid.i}
    
    ASSIGN INPUT FRAME {&FRAME-NAME} fi-qtd-cortes1     
           INPUT FRAME {&FRAME-NAME} fi-qtd-amostra1   
           INPUT FRAME {&FRAME-NAME} rs-destino1
           INPUT FRAME {&FRAME-NAME} fi-qtd-cortes2     
           INPUT FRAME {&FRAME-NAME} fi-qtd-amostra2   
           INPUT FRAME {&FRAME-NAME} rs-destino2
           INPUT FRAME {&FRAME-NAME} fi-qtd-cortes3     
           INPUT FRAME {&FRAME-NAME} fi-qtd-amostra3   
           INPUT FRAME {&FRAME-NAME} rs-destino3
           INPUT FRAME {&FRAME-NAME} fi-qtd-cortes4     
           INPUT FRAME {&FRAME-NAME} fi-qtd-amostra4   
           INPUT FRAME {&FRAME-NAME} rs-destino4
           INPUT FRAME {&FRAME-NAME} fi-qtd-cortes5     
           INPUT FRAME {&FRAME-NAME} fi-qtd-amostra5
           INPUT FRAME {&FRAME-NAME} rs-destino5
           INPUT FRAME {&FRAME-NAME} fi-qtd-trapo
           INPUT FRAME {&FRAME-NAME} fi-qtd-saldo
           INPUT FRAME {&FRAME-NAME} fi-tot-venda
           INPUT FRAME {&FRAME-NAME} fi-tot-amostra
           INPUT FRAME {&FRAME-NAME} tg-reimp
           INPUT FRAME {&FRAME-NAME} tg-imp-etq.

    /*:T Ponha na pi-validate todas as validaá‰es */
    /*:T N∆o gravar nada no registro antes do dispatch do assign-record e 
       nem na PI-validate. */

    RUN pi-validate.
    IF RETURN-VALUE = 'ADM-ERROR':U THEN 
       RETURN 'ADM-ERROR':U.

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    IF RETURN-VALUE = 'ADM-ERROR':U THEN 
       RETURN 'ADM-ERROR':U.
    
    /*:T Todos os assignÔs n∆o feitos pelo assign-record devem ser feitos aqui */  
    /* Code placed here will execute AFTER standard behavior.    */

    FIND CURRENT ob-etiqueta SHARE-LOCK NO-ERROR.
    ASSIGN c-etiquetas = ''.
    IF fi-tot-amostra > 0 OR
       fi-tot-venda > 0 OR
       fi-qtd-trapo > 0 THEN DO.

       RUN pi-altera-etq.
       FIND CURRENT ob-etiqueta NO-LOCK NO-ERROR.

       IF fi-qtd-amostra1 > 0 THEN
          RUN pi-cria-corte (INPUT fi-qtd-cortes1,
                             INPUT fi-qtd-amostra1,
                             INPUT rs-destino1).
       IF fi-qtd-amostra2 > 0 THEN
          RUN pi-cria-corte (INPUT fi-qtd-cortes2,
                             INPUT fi-qtd-amostra2,
                             INPUT rs-destino2).
       IF fi-qtd-amostra3 > 0 THEN
          RUN pi-cria-corte (INPUT fi-qtd-cortes3,
                             INPUT fi-qtd-amostra3,
                             INPUT rs-destino3).
       IF fi-qtd-amostra4 > 0 THEN
          RUN pi-cria-corte (INPUT fi-qtd-cortes4,
                             INPUT fi-qtd-amostra4,
                             INPUT rs-destino4).
       IF fi-qtd-amostra5 > 0 THEN
          RUN pi-cria-corte (INPUT fi-qtd-cortes5,
                             INPUT fi-qtd-amostra5,
                             INPUT rs-destino5).
    END.

    FIND CURRENT ob-etiqueta NO-LOCK NO-ERROR.

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
    ASSIGN fi-qtd-cortes1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '0'
           fi-qtd-cortes2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '0'
           fi-qtd-cortes3:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '0'
           fi-qtd-cortes4:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '0'
           fi-qtd-cortes5:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '0'
           fi-qtd-amostra1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '0'
           fi-qtd-amostra2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '0'
           fi-qtd-amostra3:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '0'
           fi-qtd-amostra4:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '0'
           fi-qtd-amostra5:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '0'
           rs-destino1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '2'
           rs-destino2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '2'
           rs-destino3:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '2'
           rs-destino4:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '2'
           rs-destino5:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '2'.

    IF AVAIL ob-etiqueta THEN DO.
       FIND item WHERE
            item.it-codigo = ob-etiqueta.it-codigo NO-LOCK NO-ERROR.
       IF AVAIL item THEN
          ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
    
       FIND corte-comerc WHERE
            corte-comerc.codigo = ob-etiqueta.corte-comerc NO-LOCK NO-ERROR.
       IF AVAIL corte-comerc THEN
          ASSIGN fi-desc-corte:SCREEN-VALUE IN FRAME {&FRAME-NAME} = corte-comerc.descricao.

       FIND referencia WHERE
            referencia.cod-refer = ob-etiqueta.cod-refer NO-LOCK NO-ERROR.
       IF AVAIL referencia THEN
          ASSIGN fi-desc-refer:SCREEN-VALUE IN FRAME {&FRAME-NAME} = referencia.descricao.

       ASSIGN fi-localiz:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ob-etiqueta.localizacao.

       ASSIGN fi-qtd-amostra1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '0'
              fi-qtd-saldo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '0'
              fi-qtd-trapo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '0'
              fi-qtd-saldo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ob-etiqueta.quantidade).

       FIND ITEM WHERE
            ITEM.it-codigo = ob-etiqueta.it-codigo NO-LOCK NO-ERROR.

       ASSIGN i-tp-embal = 1.
       IF ITEM.un = 'kg' THEN 
          ASSIGN i-tp-embal = 5.

       FIND ob-localiz WHERE 
            ob-localiz.cod-localiz = ob-etiqueta.localizacao NO-LOCK NO-ERROR.
       RUN new-state ("disable-button").
       IF AVAIL ob-localiz THEN
          IF ob-localiz.tipo = 2 OR ob-localiz.tipo = 9 THEN
             RUN new-state ("enable-button").
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
    
    DISABLE ob-etiqueta.quantidade WITH FRAME {&FRAME-NAME}.
    ENABLE {&list-4} WITH FRAME {&FRAME-NAME}.
    APPLY 'ENTRY' TO fi-qtd-cortes1 IN FRAME {&FRAME-NAME}.
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

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-acerta-estoq V-table-Win 
PROCEDURE pi-acerta-estoq :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER p-it-codigo  AS CHAR.
    DEF INPUT PARAMETER p-cod-refer  AS CHAR.
    DEF INPUT PARAMETER p-lote       AS CHAR.
    DEF INPUT PARAMETER p-qtde       AS DEC.
    DEF INPUT PARAMETER p-esp-docto  AS INT.
    DEF INPUT PARAMETER p-tipo-trans AS INT.


    DEF VAR c-mensagem AS CHARACTER FORMAT "X(15000)" NO-UNDO.
    DEF VAR c-erro     AS CHAR FORMAT "x(100)".

    RUN esapi/cria-movto-estoq.p (INPUT ob-etiqueta.cod-estabel,
                                  INPUT p-it-codigo,
                                  INPUT p-cod-refer,
                                  INPUT p-lote, 
                                  INPUT p-qtde,
                                  INPUT p-esp-docto,   
                                  INPUT p-tipo-trans,  
                                  INPUT "Retirada de Corte",
                                  OUTPUT c-erro). 

    IF RETURN-VALUE = 'ADM-ERROR' THEN DO.
       MESSAGE "Erro ao Baixar a Etiqueta " SKIP
               c-erro
               VIEW-AS ALERT-BOX ERROR.

       ASSIGN c-mensagem = c-erro.

       RUN esapi/esapi002.p (INPUT "imatextil@imatextil.com.br", /* e-mail remetente */
                             INPUT "angelo.panzera@imatextil.com.br", /* e-mail destinat†rio */
                             INPUT "Erro ao Baixar Etiqueta: " + STRING(ob-etiqueta.num-etiqueta), /* Assunto */
                             INPUT c-mensagem, /* Mensagem */
                             INPUT "", /*arquivo anexo*/
                             INPUT NO). /* Mostra Erros */

       ASSIGN l-erro-estoq = YES.
    END.

    RETURN STRING(l-erro-estoq).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-altera-etq V-table-Win 
PROCEDURE pi-altera-etq :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN de-peso-calc = fi-qtd-saldo * ob-etiqueta.peso / ob-etiqueta.quantidade.

    ASSIGN ob-etiqueta.quantidade = fi-qtd-saldo
           ob-etiqueta.peso = de-peso-calc.

    FIND ITEM WHERE
         ITEM.it-codigo = ob-etiqueta.it-codigo NO-LOCK NO-ERROR.

    IF fi-qtd-saldo > 0 THEN DO.
       ASSIGN i-tp-embal = 1.

       /*
       FIND corte-comerc WHERE
            corte-comerc.compr-min <= fi-qtd-saldo AND
            corte-comerc.compr-max >= fi-qtd-saldo AND
            corte-comerc.tp-embalag = i-tp-embal AND 
            corte-comerc.un = item.un NO-LOCK NO-ERROR.

       ASSIGN ob-etiqueta.acondic = corte-comerc.descricao
              ob-etiqueta.corte-comerc = corte-comerc.codigo.
       */
    END.
    ELSE 
       ASSIGN ob-etiqueta.situacao = 7.

    IF fi-tot-amostra > 0 THEN DO.
       ASSIGN l-erro-transf = NO.
       RUN pi-acerta-estoq (INPUT ob-etiqueta.it-codigo,
                            INPUT ob-etiqueta.cod-refer,
                            INPUT ob-etiqueta.cod-refer, 
                            INPUT fi-tot-amostra,
                            INPUT 33,
                            INPUT 2).

       ASSIGN l-erro-transf = LOGICAL(RETURN-VALUE).
    END.

    IF fi-qtd-trapo > 0 THEN 
       RUN pi-acerta-estoq (INPUT ob-etiqueta.it-codigo,
                            INPUT ob-etiqueta.cod-refer,
                            INPUT ob-etiqueta.cod-refer, 
                            INPUT fi-qtd-trapo,
                            INPUT 6,
                            INPUT 2).

    IF tg-reimp AND ob-etiqueta.quantidade > 0 THEN
       RUN esapi/imp-etq-estoque.p (INPUT ob-etiqueta.cod-estabel,
                                    INPUT ob-etiqueta.num-etiqueta,
                                    INPUT NO).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-calc-amostra V-table-Win 
PROCEDURE pi-calc-amostra :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR de-tot-venda AS DEC.
   DEF VAR de-tot-amostra AS DEC.

   IF rs-destino1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '2' THEN
      ASSIGN de-tot-venda = de-tot-venda + (fi-qtd-cortes1:INPUT-VALUE * fi-qtd-amostra1:INPUT-VALUE).
   ELSE
      ASSIGN de-tot-amostra = de-tot-amostra + (fi-qtd-cortes1:INPUT-VALUE * fi-qtd-amostra1:INPUT-VALUE).

   IF rs-destino2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '2' THEN
      ASSIGN de-tot-venda = de-tot-venda + (fi-qtd-cortes2:INPUT-VALUE * fi-qtd-amostra2:INPUT-VALUE).
   ELSE
      ASSIGN de-tot-amostra = de-tot-amostra + (fi-qtd-cortes2:INPUT-VALUE * fi-qtd-amostra2:INPUT-VALUE).

   IF rs-destino3:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '2' THEN
      ASSIGN de-tot-venda = de-tot-venda + (fi-qtd-cortes3:INPUT-VALUE * fi-qtd-amostra3:INPUT-VALUE).
   ELSE
      ASSIGN de-tot-amostra = de-tot-amostra + (fi-qtd-cortes3:INPUT-VALUE * fi-qtd-amostra3:INPUT-VALUE).

   IF rs-destino4:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '2' THEN
      ASSIGN de-tot-venda = de-tot-venda + (fi-qtd-cortes4:INPUT-VALUE * fi-qtd-amostra4:INPUT-VALUE).
   ELSE
      ASSIGN de-tot-amostra = de-tot-amostra + (fi-qtd-cortes4:INPUT-VALUE * fi-qtd-amostra4:INPUT-VALUE).

   IF rs-destino5:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '2' THEN
      ASSIGN de-tot-venda = de-tot-venda + (fi-qtd-cortes5:INPUT-VALUE * fi-qtd-amostra5:INPUT-VALUE).
   ELSE
      ASSIGN de-tot-amostra = de-tot-amostra + (fi-qtd-cortes5:INPUT-VALUE * fi-qtd-amostra5:INPUT-VALUE).

   ASSIGN fi-tot-venda:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(de-tot-venda)
          fi-tot-amostra:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(de-tot-amostra).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-calc-saldo V-table-Win 
PROCEDURE pi-calc-saldo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ASSIGN fi-qtd-saldo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ob-etiqueta.quantidade - 
                                             (fi-qtd-cortes1:INPUT-VALUE * fi-qtd-amostra1:INPUT-VALUE) -
                                             (fi-qtd-cortes2:INPUT-VALUE * fi-qtd-amostra2:INPUT-VALUE) -
                                             (fi-qtd-cortes3:INPUT-VALUE * fi-qtd-amostra3:INPUT-VALUE) -
                                             (fi-qtd-cortes4:INPUT-VALUE * fi-qtd-amostra4:INPUT-VALUE) -
                                             (fi-qtd-cortes5:INPUT-VALUE * fi-qtd-amostra5:INPUT-VALUE) -
                                             fi-qtd-trapo:INPUT-VALUE).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cria-corte V-table-Win 
PROCEDURE pi-cria-corte :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER p-qtd-corte AS INT.
    DEF INPUT PARAMETER p-qtd-amostra AS DEC.
    DEF INPUT PARAMETER p-destino AS INT.

    FIND ITEM WHERE
         ITEM.it-codigo = ob-etiqueta.it-codigo NO-LOCK NO-ERROR.

    ASSIGN i-nr-seq = 0
           de-tot-amostra = 0.

    IF p-destino = 4 THEN
       FIND FIRST ob-localiz WHERE
                  ob-localiz.tipo = 2 NO-LOCK NO-ERROR.
    ELSE
       FIND FIRST ob-localiz WHERE
                  ob-localiz.tipo = 3 NO-LOCK NO-ERROR.

    DO i-qt-etq = 1 TO p-qtd-corte.
       ASSIGN i-nr-seq = i-nr-seq + 1.

       CREATE b-etiqueta.
       BUFFER-COPY ob-etiqueta TO b-etiqueta
           ASSIGN b-etiqueta.num-etiqueta = IF ob-etiqueta.cod-estabel = '1' 
                                            THEN NEXT-VALUE(seq-etq-estoq-ima)
                                            ELSE NEXT-VALUE(seq-etq-estoq-med)
                  b-etiqueta.nr-sequencia = i-nr-seq
                  b-etiqueta.situacao = 3
                  b-etiqueta.nr-lote = IF p-destino = 4
                                       THEN 'CA' ELSE 'PP' 
                  b-etiqueta.quantidade = p-qtd-amostra
                  b-etiqueta.embalagem = 'PCT'
                  b-etiqueta.localiz = IF AVAIL ob-localiz
                                       THEN ob-localiz.cod-localiz
                                       ELSE ""
                  b-etiqueta.dt-emissao = TODAY
                  b-etiqueta.hr-emissao = STRING(TIME,"HH:MM").

       FIND item-ext WHERE
            item-ext.it-codigo = b-etiqueta.it-codigo NO-LOCK NO-ERROR.

       ASSIGN de-media-peso = 0.
       FIND corte-comerc WHERE
            corte-comerc.codigo = b-etiqueta.corte-comerc NO-LOCK NO-ERROR.

       ASSIGN de-peso-calc = (ITEM.peso-liquido * b-etiqueta.quantidade) + de-media-peso.

       ASSIGN b-etiqueta.peso-bruto = de-peso-calc
              de-tot-amostra = de-tot-amostra + p-qtd-amostra.

       ASSIGN c-etiquetas = IF c-etiquetas = ''
                            THEN STRING(b-etiqueta.num-etiqueta)
                            ELSE c-etiquetas + ',' + STRING(b-etiqueta.num-etiqueta).

       IF i-ep-codigo-usuario = '5' THEN
          ASSIGN c-cod-chave-param-ext = "MED0001Q".
       ELSE
          ASSIGN c-cod-chave-param-ext = "IMA0001Q".

       FIND bc-param-ext WHERE
            bc-param-ext.cod-chave-param-ext    = c-cod-chave-param-ext AND
            bc-param-ext.cod-entidade-param-ext = 'bc-tipo-trans' AND
            bc-param-ext.cod-param-ext          = ("NR-SEQ-ETIQ" + "-" + TRIM(b-etiqueta.cod-estabel))
            SHARE-LOCK NO-ERROR.
       IF AVAIL bc-param-ext THEN DO:
          ASSIGN i-etq = bc-param-ext.param-inteiro + 1
                 bc-param-ext.param-inteiro = i-etq.
          FIND CURRENT bc-param-ext NO-LOCK NO-ERROR.
       END.

       IF tg-imp-etq:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'YES' THEN
          RUN esapi/imp-etq-estoque.p (INPUT b-etiqueta.cod-estabel,
                                       INPUT b-etiqueta.num-etiqueta,
                                       INPUT NO).

       /* Grava Log */
       CREATE movto-etq.
       ASSIGN movto-etq.cod-estabel  = ob-etiqueta.cod-estabel
              movto-etq.dt-trans     = TODAY
              movto-etq.esp-docto    = b-etiqueta.nr-lote
              movto-etq.nro-docto    = STRING(ob-etiqueta.nr-ob)
              movto-etq.num-etiqueta = ob-etiqueta.num-etiqueta 
              movto-etq.quantidade   = ob-etiqueta.quantidade
              movto-etq.tipo-trans   = NO 
              movto-etq.char-1 = "Usuario: " + c-seg-usuario + FILL(" ",3) +
                                 "Hora: " + STRING(TIME,"HH:MM:SS") + FILL(" ",3) +
                                 "Retirado " + b-etiqueta.nr-lote + ": " + STRING(de-tot-amostra) + " p/ Gerar Etiqueta: " + 
                                  STRING(b-etiqueta.num-etiqueta) + FILL(" ", 3) +
                                 "Programa: " + "ESSP0145.W".
    END.

    IF p-destino = 4 AND NOT l-erro-transf THEN DO.
       RUN pi-acerta-estoq (INPUT b-etiqueta.it-codigo,
                            INPUT b-etiqueta.cod-refer,
                            INPUT b-etiqueta.nr-lote + b-etiqueta.cod-refer,
                            INPUT de-tot-amostra,
                            INPUT 33,
                            INPUT 1).
    END.

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
    
    IF AVAIL ob-localiz THEN DO.
       IF ob-localiz.tipo <> 2 AND espec.ob-localiz.tipo <> 9 THEN DO.
          MESSAGE "Localizaá∆o da Etiqueta n∆o permite Retirar Corte..."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          APPLY 'entry' TO fi-qtd-amostra1 IN FRAME {&FRAME-NAME}.
          RETURN 'ADM-ERROR':U.    
       END.
    END. 
    
    IF fi-qtd-trapo > de-qtd-max-corte THEN DO.
       MESSAGE "Qtde de Consumo, n∆o pode ser Maior que " + STRING(de-qtd-max-corte,">>9.99") + " m ..."
                VIEW-AS ALERT-BOX.
       APPLY 'entry' TO fi-qtd-trapo IN FRAME {&FRAME-NAME}.
       RETURN 'ADM-ERROR':U.    
    END.

    /* 
    IF rs-destino1 = 4 AND
       fi-qtd-amostra1 > de-qtd-max-corte THEN DO.
       MESSAGE "Metragem de Amostra, n∆o pode ser Maior que " + STRING(de-qtd-max-corte,">>9.99") + " m ..."
               VIEW-AS ALERT-BOX.
       APPLY 'entry' TO fi-qtd-amostra1 IN FRAME {&FRAME-NAME}.
       RETURN 'ADM-ERROR':U.    
    END.

    IF rs-destino2 = 4 AND
       fi-qtd-amostra2 > de-qtd-max-corte THEN DO.
       MESSAGE "Metragem de Amostra, n∆o pode ser Maior que " + STRING(de-qtd-max-corte,">>9.99") + " m ..."
               VIEW-AS ALERT-BOX.
       APPLY 'entry' TO fi-qtd-amostra2 IN FRAME {&FRAME-NAME}.
       RETURN 'ADM-ERROR':U.    
    END.

    IF rs-destino3 = 4 AND
       fi-qtd-amostra3 > de-qtd-max-corte THEN DO.
       MESSAGE "Metragem de Amostra, n∆o pode ser Maior que " + STRING(de-qtd-max-corte,">>9.99") + " m ..."
                VIEW-AS ALERT-BOX.
      APPLY 'entry' TO fi-qtd-amostra3 IN FRAME {&FRAME-NAME}.
      RETURN 'ADM-ERROR':U.    
    END.

    IF rs-destino4 = 4 AND
       fi-qtd-amostra4 > de-qtd-max-corte THEN DO.
       MESSAGE "Metragem de Amostra, n∆o pode ser Maior que " + STRING(de-qtd-max-corte,">>9.99") + " m ..."
                VIEW-AS ALERT-BOX.
       APPLY 'entry' TO fi-qtd-amostra4 IN FRAME {&FRAME-NAME}.
       RETURN 'ADM-ERROR':U.    
    END.

    IF rs-destino5 = 4 AND
       fi-qtd-amostra5 > de-qtd-max-corte THEN DO.
       MESSAGE "Metragem de Amostra, n∆o pode ser Maior que " + STRING(de-qtd-max-corte,">>9.99") + " m ..."
                VIEW-AS ALERT-BOX.
       APPLY 'entry' TO fi-qtd-amostra5 IN FRAME {&FRAME-NAME}.
       RETURN 'ADM-ERROR':U.    
    END.
    */

    IF fi-tot-amostra + fi-tot-venda > ob-etiqueta.quantidade THEN DO.
       MESSAGE 'Total de Amostras n∆o pode ser maior que a quantidade da Peáa...'
           VIEW-AS ALERT-BOX ERROR BUTTONS OK.
       APPLY 'entry' TO fi-qtd-amostra1 IN FRAME {&FRAME-NAME}.
       RETURN 'ADM-ERROR':U.    
    END.

    IF (fi-tot-amostra + fi-tot-venda + fi-qtd-trapo) > ob-etiqueta.quantidade THEN DO.
       MESSAGE 'Saldo da Peáa n∆o pode ser Negativo...'
           VIEW-AS ALERT-BOX ERROR BUTTONS OK.
       APPLY 'entry' TO fi-qtd-amostra1 IN FRAME {&FRAME-NAME}.
       RETURN 'ADM-ERROR':U.    
    END.
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
  {src/adm/template/snd-list.i "ob-etiqueta"}

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

