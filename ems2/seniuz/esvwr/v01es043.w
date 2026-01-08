&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
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
{include/i-prgvrs.i V99XX999 9.99.99.999}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
&Scop adm-attribute-dlg support/viewerd.w

/* global variable definitions */

/* Temp-Table Definitions ---                                           */
DEF TEMP-TABLE tt-digita
    FIELD opcao  AS CHAR
    FIELD campo AS CHAR
    FIELD valor AS CHAR.

/* Local Variable Definitions ---                                       */
def var v-row-parent as rowid no-undo.

DEF VAR c-cod-estabel AS CHAR.

DEF NEW GLOBAL SHARED VAR h-essp0170 AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-b01es043  AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi-cod-estab fi-dt-limite fi-it-codigo-ini ~
fi-it-codigo-fin bt-seleciona bt-dig-item bt-ex-item fi-cod-refer-ini ~
fi-cod-refer-fin bt-dig-ref bt-ex-ref fi-corte-comerc-ini ~
fi-corte-comerc-fin bt-dig-ccom bt-ex-ccom fi-cod-obsoleto-ini ~
fi-cod-obsoleto-fin bt-dig-cob bt-ex-cob tg-lote-rp tg-lote-ca ~
rs-opc-artigo rs-credito rs-opc-acab tg-negativo tg-ppp tg-estoque ~
fi-min-prod IMAGE-2 IMAGE-51 IMAGE-52 IMAGE-73 IMAGE-77 IMAGE-78 IMAGE-83 ~
IMAGE-87 rt-key 
&Scoped-Define DISPLAYED-OBJECTS fi-cod-estab fi-dt-limite fi-it-codigo-ini ~
fi-it-codigo-fin fi-cod-refer-ini fi-cod-refer-fin fi-corte-comerc-ini ~
fi-corte-comerc-fin fi-cod-obsoleto-ini fi-cod-obsoleto-fin tg-lote-rp ~
tg-lote-ca rs-opc-artigo rs-credito rs-opc-acab tg-negativo tg-ppp ~
tg-estoque fi-min-prod 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-CREATE-FIELDS fi-it-codigo-ini fi-it-codigo-fin ~
fi-cod-refer-ini fi-cod-refer-fin 

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
DEFINE BUTTON bt-dig-ccom 
     IMAGE-UP FILE "image/im-ran.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-ran.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Digita Corte Comercial".

DEFINE BUTTON bt-dig-cob 
     IMAGE-UP FILE "image/im-ran.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-ran.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Digita C¢digo Obsoleto".

DEFINE BUTTON bt-dig-item 
     IMAGE-UP FILE "image/im-ran.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-ran.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Digita Itens".

DEFINE BUTTON bt-dig-ref 
     IMAGE-UP FILE "image/im-ran.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-ran.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Digita Referˆncias".

DEFINE BUTTON bt-ex-ccom 
     IMAGE-UP FILE "image/imt-retira.bmp":U
     IMAGE-INSENSITIVE FILE "image/iit-retira.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Exceto Corte Comercial".

DEFINE BUTTON bt-ex-cob 
     IMAGE-UP FILE "image/imt-retira.bmp":U
     IMAGE-INSENSITIVE FILE "image/iit-retira.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Exceto C¢digo Obsoleto".

DEFINE BUTTON bt-ex-item 
     IMAGE-UP FILE "image/imt-retira.bmp":U
     IMAGE-INSENSITIVE FILE "image/iit-retira.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Exceto Itens".

DEFINE BUTTON bt-ex-ref 
     IMAGE-UP FILE "image/imt-retira.bmp":U
     IMAGE-INSENSITIVE FILE "image/iit-retira.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Exceto Referˆncias".

DEFINE BUTTON bt-seleciona 
     IMAGE-UP FILE "image/imt-pcp.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 60.29 BY 2.13 TOOLTIP "Processa PCP".

DEFINE VARIABLE fi-cod-estab AS CHARACTER FORMAT "X(256)":U 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-obsoleto-fin AS CHARACTER FORMAT "x(1)" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-obsoleto-ini AS CHARACTER FORMAT "x(1)" 
     LABEL "Codigo Obsoleto" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-refer-fin AS CHARACTER FORMAT "X(8)":U 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-refer-ini AS CHARACTER FORMAT "X(8)":U 
     LABEL "Referˆncia" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-corte-comerc-fin AS CHARACTER FORMAT "!" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 NO-UNDO.

DEFINE VARIABLE fi-corte-comerc-ini AS CHARACTER FORMAT "!" INITIAL "A" 
     LABEL "Corte Comercial" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-limite AS CHARACTER FORMAT "99/9999":U 
     LABEL "Data Limite" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE fi-it-codigo-fin AS CHARACTER FORMAT "X(16)":U 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 NO-UNDO.

DEFINE VARIABLE fi-it-codigo-ini AS CHARACTER FORMAT "X(16)":U 
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 NO-UNDO.

DEFINE VARIABLE fi-min-prod AS DECIMAL FORMAT ">>>,>>9.99":U INITIAL 0 
     LABEL "Qtde MIN … Produzir" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-51
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-52
     FILENAME "image\im-las":U
     SIZE 3 BY .79.

DEFINE IMAGE IMAGE-73
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-77
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-78
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-83
     FILENAME "image\im-las":U
     SIZE 3 BY .79.

DEFINE IMAGE IMAGE-87
     FILENAME "image\im-las":U
     SIZE 3 BY .79.

DEFINE VARIABLE rs-credito AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL EXPAND 
     RADIO-BUTTONS 
          "Aprovado", 1,
"NÆo Aprovado", 2,
"Ambos", 3
     SIZE 44 BY .75 NO-UNDO.

DEFINE VARIABLE rs-opc-acab AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL EXPAND 
     RADIO-BUTTONS 
          "Liso", 1,
"Estampado", 2,
"Ambos", 3
     SIZE 44 BY .79 TOOLTIP "Tipo de artigo." NO-UNDO.

DEFINE VARIABLE rs-opc-artigo AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL EXPAND 
     RADIO-BUTTONS 
          "Indigo", "I",
"Outros", "O",
"Ambos", "A"
     SIZE 44 BY .79 NO-UNDO.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 109 BY 22.75.

DEFINE VARIABLE tg-estoque AS LOGICAL INITIAL no 
     LABEL "Itens com Saldo em Estoque" 
     VIEW-AS TOGGLE-BOX
     SIZE 28.72 BY .83 NO-UNDO.

DEFINE VARIABLE tg-lote-ca AS LOGICAL INITIAL no 
     LABEL "CA" 
     VIEW-AS TOGGLE-BOX
     SIZE 5.57 BY .88 TOOLTIP "Apenas o lote (Corte de Amostra)." NO-UNDO.

DEFINE VARIABLE tg-lote-rp AS LOGICAL INITIAL no 
     LABEL "RP" 
     VIEW-AS TOGGLE-BOX
     SIZE 5.57 BY .88 TOOLTIP "Apenas o lote RP (Rolo Perfeito)." NO-UNDO.

DEFINE VARIABLE tg-negativo AS LOGICAL INITIAL yes 
     LABEL "Itens com Negativo … Programar" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .83 NO-UNDO.

DEFINE VARIABLE tg-ppp AS LOGICAL INITIAL no 
     LABEL "Itens com Quantidades j  Programadas" 
     VIEW-AS TOGGLE-BOX
     SIZE 39.72 BY .83 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     fi-cod-estab AT ROW 1.92 COL 32.14 COLON-ALIGNED WIDGET-ID 8
     fi-dt-limite AT ROW 2.92 COL 32.14 COLON-ALIGNED
     fi-it-codigo-ini AT ROW 3.96 COL 32.14 COLON-ALIGNED
     fi-it-codigo-fin AT ROW 3.96 COL 62.14 COLON-ALIGNED NO-LABEL
     bt-seleciona AT ROW 19.83 COL 32.72
     bt-dig-item AT ROW 3.92 COL 81.14
     bt-ex-item AT ROW 3.92 COL 86.14
     fi-cod-refer-ini AT ROW 4.96 COL 32.14 COLON-ALIGNED
     fi-cod-refer-fin AT ROW 4.96 COL 62.14 COLON-ALIGNED NO-LABEL
     bt-dig-ref AT ROW 4.92 COL 81.14
     bt-ex-ref AT ROW 4.92 COL 86.14
     fi-corte-comerc-ini AT ROW 5.96 COL 32.14 COLON-ALIGNED
     fi-corte-comerc-fin AT ROW 5.96 COL 62.14 COLON-ALIGNED HELP
          "Corte Comercial" NO-LABEL
     bt-dig-ccom AT ROW 5.92 COL 81.14
     bt-ex-ccom AT ROW 5.92 COL 86.14
     fi-cod-obsoleto-ini AT ROW 6.92 COL 32.14 COLON-ALIGNED HELP
          "Codigo obsoleto"
     fi-cod-obsoleto-fin AT ROW 6.92 COL 62.14 COLON-ALIGNED HELP
          "Codigo obsoleto" NO-LABEL
     bt-dig-cob AT ROW 6.92 COL 81.14
     bt-ex-cob AT ROW 6.92 COL 86.14
     tg-lote-rp AT ROW 8.96 COL 34.14
     tg-lote-ca AT ROW 8.96 COL 41.14
     rs-opc-artigo AT ROW 10 COL 34 NO-LABEL
     rs-credito AT ROW 11.04 COL 34 NO-LABEL
     rs-opc-acab AT ROW 11.96 COL 34.14 NO-LABEL WIDGET-ID 2
     tg-negativo AT ROW 13.54 COL 34.29
     tg-ppp AT ROW 14.54 COL 34.29
     tg-estoque AT ROW 15.54 COL 34.29
     fi-min-prod AT ROW 17.25 COL 32 COLON-ALIGNED
     "Tipo de Artigo:" VIEW-AS TEXT
          SIZE 10 BY .75 AT ROW 9.96 COL 24
     "Acabamento:" VIEW-AS TEXT
          SIZE 9.43 BY .54 AT ROW 12.04 COL 24.86 WIDGET-ID 6
     "Lote:" VIEW-AS TEXT
          SIZE 3.57 BY .54 AT ROW 9.13 COL 30.14
     "Cr‚dito:" VIEW-AS TEXT
          SIZE 5.72 BY .58 AT ROW 11 COL 28.43
     IMAGE-2 AT ROW 3.92 COL 60.14
     IMAGE-51 AT ROW 4.96 COL 51.14
     IMAGE-52 AT ROW 4.96 COL 60.14
     IMAGE-73 AT ROW 3.96 COL 51.14
     IMAGE-77 AT ROW 6.92 COL 51.29
     IMAGE-78 AT ROW 5.92 COL 51.14
     IMAGE-83 AT ROW 6.92 COL 60.29
     IMAGE-87 AT ROW 5.92 COL 60.14
     rt-key AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
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
         HEIGHT             = 22.79
         WIDTH              = 109.57.
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

/* SETTINGS FOR FILL-IN fi-cod-refer-fin IN FRAME f-main
   1                                                                    */
/* SETTINGS FOR FILL-IN fi-cod-refer-ini IN FRAME f-main
   1                                                                    */
/* SETTINGS FOR FILL-IN fi-it-codigo-fin IN FRAME f-main
   1                                                                    */
/* SETTINGS FOR FILL-IN fi-it-codigo-ini IN FRAME f-main
   1                                                                    */
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

&Scoped-define SELF-NAME bt-dig-ccom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-dig-ccom V-table-Win
ON CHOOSE OF bt-dig-ccom IN FRAME f-main
DO:
    RUN esdlg/d01-digita.w (INPUT-OUTPUT TABLE tt-digita, 
                            INPUT "D",
                            INPUT "Corte_Comercial").

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-dig-cob
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-dig-cob V-table-Win
ON CHOOSE OF bt-dig-cob IN FRAME f-main
DO:
    RUN esdlg/d01-digita.w (INPUT-OUTPUT TABLE tt-digita, 
                            INPUT "D",
                            INPUT "Codigo_Obsoleto").

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-dig-item
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-dig-item V-table-Win
ON CHOOSE OF bt-dig-item IN FRAME f-main
DO:
    RUN esdlg/d01-digita.w (INPUT-OUTPUT TABLE tt-digita, 
                            INPUT "D",
                            INPUT "Item").

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-dig-ref
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-dig-ref V-table-Win
ON CHOOSE OF bt-dig-ref IN FRAME f-main
DO:
    RUN esdlg/d01-digita.w (INPUT-OUTPUT TABLE tt-digita, 
                            INPUT "D",
                            INPUT "Referˆncia").

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ex-ccom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ex-ccom V-table-Win
ON CHOOSE OF bt-ex-ccom IN FRAME f-main
DO:
    RUN esdlg/d01-digita.w (INPUT-OUTPUT TABLE tt-digita, 
                              INPUT "E",
                              INPUT "Corte_Comercial").

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ex-cob
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ex-cob V-table-Win
ON CHOOSE OF bt-ex-cob IN FRAME f-main
DO:
    RUN esdlg/d01-digita.w (INPUT-OUTPUT TABLE tt-digita,
                              INPUT "E",
                              INPUT "Codigo_Obsoleto").

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ex-item
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ex-item V-table-Win
ON CHOOSE OF bt-ex-item IN FRAME f-main
DO:
    RUN esdlg/d01-digita.w (INPUT-OUTPUT TABLE tt-digita, 
                              INPUT "E",
                              INPUT "Item").

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ex-ref
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ex-ref V-table-Win
ON CHOOSE OF bt-ex-ref IN FRAME f-main
DO:
   RUN esdlg/d01-digita.w (INPUT-OUTPUT TABLE tt-digita, 
                             INPUT "E",
                             INPUT "Referˆncia").

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-seleciona
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-seleciona V-table-Win
ON CHOOSE OF bt-seleciona IN FRAME f-main
DO:
   ASSIGN INPUT FRAME {&FRAME-NAME} fi-cod-estab
          INPUT FRAME {&FRAME-NAME} fi-dt-limite
          INPUT FRAME {&FRAME-NAME} fi-it-codigo-ini    
          INPUT FRAME {&FRAME-NAME} fi-it-codigo-fin    
          INPUT FRAME {&FRAME-NAME} fi-cod-refer-ini    
          INPUT FRAME {&FRAME-NAME} fi-cod-refer-fin    
          INPUT FRAME {&FRAME-NAME} fi-corte-comerc-ini
          INPUT FRAME {&FRAME-NAME} fi-corte-comerc-fin
          INPUT FRAME {&FRAME-NAME} fi-cod-obsoleto-ini 
          INPUT FRAME {&FRAME-NAME} fi-cod-obsoleto-fin
          INPUT FRAME {&FRAME-NAME} tg-lote-rp
          INPUT FRAME {&FRAME-NAME} tg-lote-ca
          INPUT FRAME {&FRAME-NAME} rs-opc-artigo
          INPUT FRAME {&FRAME-NAME} rs-credito
          INPUT FRAME {&FRAME-NAME} rs-opc-acab
          INPUT FRAME {&FRAME-NAME} tg-negativo
          INPUT FRAME {&FRAME-NAME} tg-ppp
          INPUT FRAME {&FRAME-NAME} tg-estoque
          INPUT FRAME {&FRAME-NAME} fi-min-prod.

   RUN pi-select-page IN h-essp0170 (INPUT 2).

   RUN pi-processa IN h-b01es043 (INPUT fi-cod-estab,
                                  INPUT fi-dt-limite,
                                  INPUT fi-it-codigo-ini,
                                  INPUT fi-it-codigo-fin,
                                  INPUT fi-cod-refer-ini,
                                  INPUT fi-cod-refer-fin,
                                  INPUT fi-corte-comerc-ini,      
                                  INPUT fi-corte-comerc-fin,      
                                  INPUT fi-cod-obsoleto-ini,
                                  INPUT fi-cod-obsoleto-fin,
                                  INPUT tg-lote-rp,                    
                                  INPUT tg-lote-ca,
                                  INPUT rs-opc-artigo,
                                  INPUT rs-credito,  
                                  INPUT rs-opc-acab,
                                  INPUT tg-negativo, 
                                  INPUT tg-ppp,
                                  INPUT tg-estoque,
                                  INPUT fi-min-prod,
                                  INPUT TABLE tt-digita).

    RUN local-initialize-fields.

    EMPTY TEMP-TABLE tt-digita.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-obsoleto-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-obsoleto-ini V-table-Win
ON LEAVE OF fi-cod-obsoleto-ini IN FRAME f-main /* Codigo Obsoleto */
DO:
  IF SELF:SCREEN-VALUE <> '0' THEN
     ASSIGN fi-cod-obsoleto-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-refer-fin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-refer-fin V-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-cod-refer-fin IN FRAME f-main
DO:
    {include/zoomvar.i &prog-zoom = inzoom/z02in377.w
                       &campo     = fi-cod-refer-fin
                       &campozoom = cod-refer}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-refer-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-refer-ini V-table-Win
ON LEAVE OF fi-cod-refer-ini IN FRAME f-main /* Referˆncia */
DO:
  IF SELF:SCREEN-VALUE <> '' THEN
     ASSIGN fi-cod-refer-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-refer-ini V-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-cod-refer-ini IN FRAME f-main /* Referˆncia */
DO:
    {include/zoomvar.i &prog-zoom = inzoom/z02in377.w
                       &campo     = fi-cod-refer-ini
                       &campozoom = cod-refer}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-corte-comerc-fin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-corte-comerc-fin V-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-corte-comerc-fin IN FRAME f-main
DO:
    {include/zoomvar.i &prog-zoom = eszoom/z01es065.w
                       &campo     = fi-corte-comerc-fin
                       &campozoom = codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-corte-comerc-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-corte-comerc-ini V-table-Win
ON LEAVE OF fi-corte-comerc-ini IN FRAME f-main /* Corte Comercial */
DO:
  IF SELF:SCREEN-VALUE = '' THEN DO.
     APPLY 'entry' TO SELF.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-corte-comerc-ini V-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-corte-comerc-ini IN FRAME f-main /* Corte Comercial */
DO:
    {include/zoomvar.i &prog-zoom = eszoom/z01es065.w
                       &campo     = fi-corte-comerc-ini
                       &campozoom = codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-dt-limite
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-dt-limite V-table-Win
ON LEAVE OF fi-dt-limite IN FRAME f-main /* Data Limite */
DO:
   IF INT(SUBSTR(SELF:SCREEN-VALUE,1,2)) <  1 OR
      INT(SUBSTR(SELF:SCREEN-VALUE,1,2)) > 12  THEN DO:
       MESSAGE "MES invalido ! ! !"  VIEW-AS ALERT-BOX. 
       APPLY 'entry' TO SELF.
       RETURN NO-APPLY.
   END.
   IF INT(SUBSTR(SELF:SCREEN-VALUE,4,4)) <  1 THEN DO:
       MESSAGE "ANO invalido ! ! !"  VIEW-AS ALERT-BOX. 
       APPLY 'entry' TO SELF.
       RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-it-codigo-fin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo-fin V-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-it-codigo-fin IN FRAME f-main
DO:
    {include/zoomvar.i &prog-zoom = inzoom/z02in172.w
                       &campo     = fi-it-codigo-fin
                       &campozoom = it-codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-it-codigo-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo-ini V-table-Win
ON LEAVE OF fi-it-codigo-ini IN FRAME f-main /* Item */
DO:
  IF SELF:SCREEN-VALUE <> '' AND
     SELF:SCREEN-VALUE <> '5' THEN
     ASSIGN fi-it-codigo-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo-ini V-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-it-codigo-ini IN FRAME f-main /* Item */
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z02in172.w
                     &campo     = fi-it-codigo-ini
                     &campozoom = it-codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
  fi-it-codigo-ini:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.
  fi-it-codigo-fin:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.
  fi-cod-refer-ini:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.
  fi-cod-refer-fin:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.

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

  APPLY 'entry' TO fi-dt-limite IN FRAME {&FRAME-NAME}.

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
  IF c-cod-estabel = '' THEN DO.
     RUN esapi/busca-estab-usuar.p (INPUT c-seg-usuario,
                                    OUTPUT c-cod-estabel).
     IF c-cod-estabel = '' OR c-cod-estabel = '0' THEN
        ASSIGN c-cod-estabel = '1'.
  END.
         
  ASSIGN fi-it-codigo-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = '5'
         fi-it-codigo-fin:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = '5ZZZZZZZZZZZZZZZ'
         fi-cod-refer-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = ''
         fi-cod-refer-fin:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = 'ZZZZZZZZZZ'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize-fields V-table-Win 
PROCEDURE local-initialize-fields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN fi-cod-estab:SCREEN-VALUE IN FRAME {&FRAME-NAME}        = c-cod-estabel
         fi-dt-limite:SCREEN-VALUE IN FRAME {&FRAME-NAME}        = STRING(MONTH(TODAY),'99') + STRING(YEAR(TODAY),'9999')
         fi-corte-comerc-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "A"
         fi-corte-comerc-fin:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Z"
         fi-cod-obsoleto-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '0'
         fi-cod-obsoleto-fin:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'Z'
         tg-lote-rp:SCREEN-VALUE IN FRAME {&FRAME-NAME}          = 'YES'
         tg-lote-ca:SCREEN-VALUE IN FRAME {&FRAME-NAME}          = 'NO'
         rs-opc-artigo:SCREEN-VALUE IN FRAME {&FRAME-NAME}       = 'A'
         rs-opc-acab:SCREEN-VALUE IN FRAME {&FRAME-NAME}         = '3'  
         rs-credito:SCREEN-VALUE IN FRAME {&FRAME-NAME}          = '3'
         tg-negativo:SCREEN-VALUE IN FRAME {&FRAME-NAME}         = 'YES'
         tg-ppp:SCREEN-VALUE IN FRAME {&FRAME-NAME}              = 'NO'
         tg-estoque:SCREEN-VALUE IN FRAME {&FRAME-NAME}          = 'NO'
         fi-min-prod:SCREEN-VALUE IN FRAME {&FRAME-NAME}         = '0'.

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

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartViewer, and there are no
     tables specified in any contained Browse, Query, or Frame. */

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

