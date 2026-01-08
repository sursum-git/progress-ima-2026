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
{include/i-prgvrs.i V05DI159 2.04.00.000}

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
DEF NEW GLOBAL SHARED VAR h-essp0160 AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-b05di154  AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

DEF VAR v-row-parent  AS ROWID NO-UNDO.
DEF VAR c-cod-estabel AS CHAR.

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
&Scoped-Define ENABLED-OBJECTS fi-dt-limite bt-seleciona fi-nr-pedcli-ini ~
fi-nr-pedcli-fin fi-it-codigo-ini fi-it-codigo-fin fi-cod-refer-ini ~
fi-cod-refer-fin fi-nr-seq-ini fi-nr-seq-fin fi-localiz-ini fi-localiz-fin ~
bt-ex-loc fi-corte-comerc-ini fi-corte-comerc-fin fi-cod-obsoleto-ini ~
fi-cod-obsoleto-fin tg-lote-todos tg-lote-pp tg-lote-pd tg-lote-rp ~
tg-lote-rd tg-lote-sc tg-lote-ca rs-opc-artigo tg-acomp tg-res-crivo ~
bt-ex-ped bt-ex-ref bt-ex-item bt-ex-seq bt-ex-ccom bt-ex-cob bt-dig-ped ~
bt-dig-item bt-dig-ref bt-dig-seq bt-dig-loc bt-dig-ccom bt-dig-cob ~
IMAGE-100 IMAGE-101 IMAGE-102 IMAGE-103 IMAGE-106 IMAGE-107 IMAGE-108 ~
IMAGE-109 IMAGE-2 IMAGE-51 IMAGE-52 IMAGE-73 IMAGE-77 IMAGE-78 IMAGE-83 ~
IMAGE-87 rt-key 
&Scoped-Define DISPLAYED-OBJECTS fi-dt-limite fi-nr-pedcli-ini ~
fi-nr-pedcli-fin fi-it-codigo-ini fi-it-codigo-fin fi-cod-refer-ini ~
fi-cod-refer-fin fi-nr-seq-ini fi-nr-seq-fin fi-localiz-ini fi-localiz-fin ~
fi-corte-comerc-ini fi-corte-comerc-fin fi-cod-obsoleto-ini ~
fi-cod-obsoleto-fin fi-perc-min fi-perc-max tg-lote-todos tg-lote-pp ~
tg-lote-pd tg-lote-rp tg-lote-rd tg-lote-sc tg-lote-ca rs-opc-artigo ~
tg-acomp tg-res-crivo fi-cod-estabel fi-nome-estabel 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-CREATE-FIELDS fi-nr-pedcli-ini fi-nr-pedcli-fin ~
fi-it-codigo-ini fi-it-codigo-fin fi-cod-refer-ini fi-cod-refer-fin ~
fi-localiz-ini fi-localiz-fin 
&Scoped-define List-5 fi-cod-estabel 

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

DEFINE BUTTON bt-dig-loc 
     IMAGE-UP FILE "image/im-ran.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-ran.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Digita Localiza‡Æo".

DEFINE BUTTON bt-dig-ped 
     IMAGE-UP FILE "image/im-ran.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-ran.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Digita Pedidos".

DEFINE BUTTON bt-dig-ref 
     IMAGE-UP FILE "image/im-ran.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-ran.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Digita Referˆncias".

DEFINE BUTTON bt-dig-seq 
     IMAGE-UP FILE "image/im-ran.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-ran.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Digita Sequˆncia".

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

DEFINE BUTTON bt-ex-loc 
     IMAGE-UP FILE "image/imt-retira.bmp":U
     IMAGE-INSENSITIVE FILE "image/iit-retira.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Exceto Localiza‡äes".

DEFINE BUTTON bt-ex-ped 
     IMAGE-UP FILE "image/imt-retira.bmp":U
     IMAGE-INSENSITIVE FILE "image/iit-retira.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Exceto Pedidos".

DEFINE BUTTON bt-ex-ref 
     IMAGE-UP FILE "image/imt-retira.bmp":U
     IMAGE-INSENSITIVE FILE "image/iit-retira.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Exceto Referˆncias".

DEFINE BUTTON bt-ex-seq 
     IMAGE-UP FILE "image/imt-retira.bmp":U
     IMAGE-INSENSITIVE FILE "image/iit-retira.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Exceto Sequˆncias".

DEFINE BUTTON bt-seleciona 
     IMAGE-UP FILE "image/imt-res-u.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Processa Separa‡Æo Autom tica" 
     SIZE 46.14 BY 2.13 TOOLTIP "Processa Separa‡Æo Autom tica".

DEFINE VARIABLE fi-cod-estabel AS CHARACTER FORMAT "X(256)":U 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

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

DEFINE VARIABLE fi-localiz-fin AS CHARACTER FORMAT "999/999":U 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE fi-localiz-ini AS CHARACTER FORMAT "999/999":U 
     LABEL "Localiza‡Æo" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nome-estabel AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nr-pedcli-fin AS CHARACTER FORMAT "X(16)":U 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nr-pedcli-ini AS CHARACTER FORMAT "X(16)":U 
     LABEL "Pedido" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nr-seq-fin AS INTEGER FORMAT ">>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nr-seq-ini AS INTEGER FORMAT ">>>>9" INITIAL 0 
     LABEL "Sequˆncia" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE VARIABLE fi-perc-max AS DECIMAL FORMAT ">9.99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE VARIABLE fi-perc-min AS DECIMAL FORMAT "->9.99" INITIAL 0 
     LABEL "Tolerƒncia" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88
     FGCOLOR 12  NO-UNDO.

DEFINE IMAGE IMAGE-100
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-101
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-102
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-103
     FILENAME "image\im-las":U
     SIZE 3 BY .79.

DEFINE IMAGE IMAGE-106
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-107
     FILENAME "image\im-las":U
     SIZE 3 BY .79.

DEFINE IMAGE IMAGE-108
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-109
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

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

DEFINE VARIABLE rs-opc-artigo AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Indigo", "I",
"Outros", "O",
"Ambos", "A"
     SIZE 36.29 BY .79 NO-UNDO.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 109 BY 17.

DEFINE VARIABLE tg-acomp AS LOGICAL INITIAL no 
     LABEL "Acompanhamento Passo a Passo" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .75 NO-UNDO.

DEFINE VARIABLE tg-lote-ca AS LOGICAL INITIAL no 
     LABEL "CA" 
     VIEW-AS TOGGLE-BOX
     SIZE 5.57 BY .88 TOOLTIP "Apenas o lote (Corte de Amostra)." NO-UNDO.

DEFINE VARIABLE tg-lote-pd AS LOGICAL INITIAL no 
     LABEL "PD" 
     VIEW-AS TOGGLE-BOX
     SIZE 5.57 BY .88 TOOLTIP "Apenas o lote PD (Pe‡a Defeituosa)" NO-UNDO.

DEFINE VARIABLE tg-lote-pp AS LOGICAL INITIAL no 
     LABEL "PP" 
     VIEW-AS TOGGLE-BOX
     SIZE 5.57 BY .88 TOOLTIP "Apenas o lote PP (Pe‡a Perfeita)." NO-UNDO.

DEFINE VARIABLE tg-lote-rd AS LOGICAL INITIAL no 
     LABEL "RD" 
     VIEW-AS TOGGLE-BOX
     SIZE 5.57 BY .88 TOOLTIP "Apenas o lote RD (Rolo Defeituoso)." NO-UNDO.

DEFINE VARIABLE tg-lote-rp AS LOGICAL INITIAL no 
     LABEL "RP" 
     VIEW-AS TOGGLE-BOX
     SIZE 5.57 BY .88 TOOLTIP "Apenas o lote RP (Rolo Perfeito)." NO-UNDO.

DEFINE VARIABLE tg-lote-sc AS LOGICAL INITIAL no 
     LABEL "SC" 
     VIEW-AS TOGGLE-BOX
     SIZE 5.57 BY .88 TOOLTIP "Apenas o lote SC (Saco)." NO-UNDO.

DEFINE VARIABLE tg-lote-todos AS LOGICAL INITIAL no 
     LABEL "TODOS" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .88 TOOLTIP "Todas as qualidades (PP, PD, RP e RD)." NO-UNDO.

DEFINE VARIABLE tg-res-crivo AS LOGICAL INITIAL no 
     LABEL "Reservar Pedidos Crivados" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .83 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     fi-dt-limite AT ROW 2.5 COL 32 COLON-ALIGNED
     bt-seleciona AT ROW 14.79 COL 32.57
     fi-nr-pedcli-ini AT ROW 3.5 COL 32 COLON-ALIGNED
     fi-nr-pedcli-fin AT ROW 3.5 COL 62 COLON-ALIGNED NO-LABEL
     fi-it-codigo-ini AT ROW 4.54 COL 32 COLON-ALIGNED
     fi-it-codigo-fin AT ROW 4.54 COL 62 COLON-ALIGNED NO-LABEL
     fi-cod-refer-ini AT ROW 5.54 COL 32 COLON-ALIGNED
     fi-cod-refer-fin AT ROW 5.54 COL 62 COLON-ALIGNED NO-LABEL
     fi-nr-seq-ini AT ROW 6.54 COL 32 COLON-ALIGNED HELP
          "Codigo obsoleto"
     fi-nr-seq-fin AT ROW 6.54 COL 62 COLON-ALIGNED HELP
          "Codigo obsoleto" NO-LABEL
     fi-localiz-ini AT ROW 7.54 COL 32 COLON-ALIGNED
     fi-localiz-fin AT ROW 7.54 COL 62 COLON-ALIGNED NO-LABEL
     bt-ex-loc AT ROW 7.5 COL 86
     fi-corte-comerc-ini AT ROW 8.54 COL 32 COLON-ALIGNED
     fi-corte-comerc-fin AT ROW 8.54 COL 62 COLON-ALIGNED HELP
          "Corte Comercial" NO-LABEL
     fi-cod-obsoleto-ini AT ROW 9.5 COL 32 COLON-ALIGNED HELP
          "Codigo obsoleto"
     fi-cod-obsoleto-fin AT ROW 9.5 COL 62 COLON-ALIGNED HELP
          "Codigo obsoleto" NO-LABEL
     fi-perc-min AT ROW 10.5 COL 32 COLON-ALIGNED HELP
          "Codigo obsoleto"
     fi-perc-max AT ROW 10.5 COL 62 COLON-ALIGNED NO-LABEL
     tg-lote-todos AT ROW 11.71 COL 34
     tg-lote-pp AT ROW 11.71 COL 45.72
     tg-lote-pd AT ROW 11.71 COL 52.14
     tg-lote-rp AT ROW 11.71 COL 58.72
     tg-lote-rd AT ROW 11.71 COL 65.57
     tg-lote-sc AT ROW 11.71 COL 72.29
     tg-lote-ca AT ROW 11.71 COL 78.57
     rs-opc-artigo AT ROW 12.75 COL 34 NO-LABEL
     tg-acomp AT ROW 17 COL 34
     tg-res-crivo AT ROW 13.75 COL 34.14
     bt-ex-ped AT ROW 3.5 COL 86
     bt-ex-ref AT ROW 5.5 COL 86
     bt-ex-item AT ROW 4.5 COL 86
     bt-ex-seq AT ROW 6.5 COL 86
     bt-ex-ccom AT ROW 8.5 COL 86
     bt-ex-cob AT ROW 9.5 COL 86
     bt-dig-ped AT ROW 3.5 COL 81
     bt-dig-item AT ROW 4.5 COL 81
     bt-dig-ref AT ROW 5.5 COL 81
     bt-dig-seq AT ROW 6.5 COL 81
     bt-dig-loc AT ROW 7.5 COL 81
     bt-dig-ccom AT ROW 8.5 COL 81
     bt-dig-cob AT ROW 9.5 COL 81
     fi-cod-estabel AT ROW 1.5 COL 32 COLON-ALIGNED WIDGET-ID 6
     fi-nome-estabel AT ROW 1.5 COL 37.43 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     "Tipo de Artigo:" VIEW-AS TEXT
          SIZE 10 BY .75 AT ROW 12.71 COL 24
     "Lote:" VIEW-AS TEXT
          SIZE 3.57 BY .54 AT ROW 11.83 COL 30
     IMAGE-100 AT ROW 3.5 COL 60
     IMAGE-101 AT ROW 3.5 COL 51
     IMAGE-102 AT ROW 6.5 COL 51.14
     IMAGE-103 AT ROW 6.5 COL 60.14
     IMAGE-106 AT ROW 10.5 COL 51
     IMAGE-107 AT ROW 10.5 COL 60
     IMAGE-108 AT ROW 7.54 COL 60
     IMAGE-109 AT ROW 7.5 COL 51
     IMAGE-2 AT ROW 4.54 COL 60
     IMAGE-51 AT ROW 5.54 COL 51
     IMAGE-52 AT ROW 5.54 COL 60
     IMAGE-73 AT ROW 4.54 COL 51
     IMAGE-77 AT ROW 9.5 COL 51.14
     IMAGE-78 AT ROW 8.5 COL 51
     IMAGE-83 AT ROW 9.5 COL 60.14
     IMAGE-87 AT ROW 8.5 COL 60
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
         HEIGHT             = 17.08
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

/* SETTINGS FOR FILL-IN fi-cod-estabel IN FRAME f-main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN fi-cod-refer-fin IN FRAME f-main
   1                                                                    */
/* SETTINGS FOR FILL-IN fi-cod-refer-ini IN FRAME f-main
   1                                                                    */
/* SETTINGS FOR FILL-IN fi-it-codigo-fin IN FRAME f-main
   1                                                                    */
/* SETTINGS FOR FILL-IN fi-it-codigo-ini IN FRAME f-main
   1                                                                    */
/* SETTINGS FOR FILL-IN fi-localiz-fin IN FRAME f-main
   1                                                                    */
/* SETTINGS FOR FILL-IN fi-localiz-ini IN FRAME f-main
   1                                                                    */
/* SETTINGS FOR FILL-IN fi-nome-estabel IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nr-pedcli-fin IN FRAME f-main
   1                                                                    */
/* SETTINGS FOR FILL-IN fi-nr-pedcli-ini IN FRAME f-main
   1                                                                    */
/* SETTINGS FOR FILL-IN fi-perc-max IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-perc-min IN FRAME f-main
   NO-ENABLE                                                            */
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


&Scoped-define SELF-NAME bt-dig-loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-dig-loc V-table-Win
ON CHOOSE OF bt-dig-loc IN FRAME f-main
DO:
    RUN esdlg/d01-digita.w (INPUT-OUTPUT TABLE tt-digita, 
                            INPUT "D",
                            INPUT "Localiza‡Æo").

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-dig-ped
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-dig-ped V-table-Win
ON CHOOSE OF bt-dig-ped IN FRAME f-main
DO:
    RUN esdlg/d01-digita.w (INPUT-OUTPUT TABLE tt-digita, 
                            INPUT "D",
                            INPUT "Pedido_de_Venda").

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


&Scoped-define SELF-NAME bt-dig-seq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-dig-seq V-table-Win
ON CHOOSE OF bt-dig-seq IN FRAME f-main
DO:
    RUN esdlg/d01-digita.w (INPUT-OUTPUT TABLE tt-digita,
                            INPUT "D",
                            INPUT "Sequˆncia").

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


&Scoped-define SELF-NAME bt-ex-loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ex-loc V-table-Win
ON CHOOSE OF bt-ex-loc IN FRAME f-main
DO:
   RUN esdlg/d01-digita.w (INPUT-OUTPUT TABLE tt-digita, 
                             INPUT "E",
                             INPUT "Localiza‡Æo").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ex-ped
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ex-ped V-table-Win
ON CHOOSE OF bt-ex-ped IN FRAME f-main
DO:
    RUN esdlg/d01-digita.w (INPUT-OUTPUT TABLE tt-digita,
                            INPUT "E",
                            INPUT "Pedido_de_Venda").

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


&Scoped-define SELF-NAME bt-ex-seq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ex-seq V-table-Win
ON CHOOSE OF bt-ex-seq IN FRAME f-main
DO:
    RUN esdlg/d01-digita.w (INPUT-OUTPUT TABLE tt-digita, 
                              INPUT "E",
                              INPUT "Sequˆncia").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-seleciona
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-seleciona V-table-Win
ON CHOOSE OF bt-seleciona IN FRAME f-main /* Processa Separa‡Æo Autom tica */
DO:
   ASSIGN INPUT FRAME {&FRAME-NAME} fi-cod-estabel
          INPUT FRAME {&FRAME-NAME} fi-dt-limite
          INPUT FRAME {&FRAME-NAME} fi-it-codigo-ini    
          INPUT FRAME {&FRAME-NAME} fi-it-codigo-fin    
          INPUT FRAME {&FRAME-NAME} fi-cod-refer-ini    
          INPUT FRAME {&FRAME-NAME} fi-cod-refer-fin    
          INPUT FRAME {&FRAME-NAME} fi-nr-pedcli-ini 
          INPUT FRAME {&FRAME-NAME} fi-nr-pedcli-fin
          INPUT FRAME {&FRAME-NAME} fi-nr-seq-ini
          INPUT FRAME {&FRAME-NAME} fi-nr-seq-fin
          INPUT FRAME {&FRAME-NAME} fi-localiz-ini
          INPUT FRAME {&FRAME-NAME} fi-localiz-fin
          INPUT FRAME {&FRAME-NAME} fi-corte-comerc-ini
          INPUT FRAME {&FRAME-NAME} fi-corte-comerc-fin
          INPUT FRAME {&FRAME-NAME} fi-cod-obsoleto-ini 
          INPUT FRAME {&FRAME-NAME} fi-cod-obsoleto-fin
          INPUT FRAME {&FRAME-NAME} fi-perc-min
          INPUT FRAME {&FRAME-NAME} fi-perc-max
          INPUT FRAME {&FRAME-NAME} tg-lote-todos
          INPUT FRAME {&FRAME-NAME} tg-lote-pp
          INPUT FRAME {&FRAME-NAME} tg-lote-pd
          INPUT FRAME {&FRAME-NAME} tg-lote-rp
          INPUT FRAME {&FRAME-NAME} tg-lote-rd
          INPUT FRAME {&FRAME-NAME} tg-lote-sc
          INPUT FRAME {&FRAME-NAME} tg-lote-ca
          INPUT FRAME {&FRAME-NAME} rs-opc-artigo
          INPUT FRAME {&FRAME-NAME} tg-res-crivo
          INPUT FRAME {&FRAME-NAME} tg-acomp.

   RUN pi-select-page IN h-essp0160 (INPUT 2).
   RUN pi-processa IN h-b05di154 (INPUT fi-cod-estabel,
                                  INPUT fi-dt-limite,
                                  INPUT fi-it-codigo-ini,
                                  INPUT fi-it-codigo-fin,
                                  INPUT fi-cod-refer-ini,
                                  INPUT fi-cod-refer-fin,
                                  INPUT fi-nr-pedcli-ini,
                                  INPUT fi-nr-pedcli-fin,
                                  INPUT fi-nr-seq-ini,
                                  INPUT fi-nr-seq-fin,
                                  INPUT fi-localiz-ini,
                                  INPUT fi-localiz-fin,
                                  INPUT fi-corte-comerc-ini,      
                                  INPUT fi-corte-comerc-fin,      
                                  INPUT fi-cod-obsoleto-ini,
                                  INPUT fi-cod-obsoleto-fin,
                                  INPUT ABS(fi-perc-min),
                                  INPUT fi-perc-max,
                                  INPUT tg-lote-todos,                 
                                  INPUT tg-lote-pp,                    
                                  INPUT tg-lote-pd,                    
                                  INPUT tg-lote-rp,                    
                                  INPUT tg-lote-rd,
                                  INPUT tg-lote-sc,
                                  INPUT tg-lote-ca,
                                  INPUT rs-opc-artigo,
                                  INPUT tg-res-crivo,
                                  INPUT tg-acomp,
                                  INPUT TABLE tt-digita).

    RUN local-initialize-fields.

    EMPTY TEMP-TABLE tt-digita.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-estabel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-estabel V-table-Win
ON LEAVE OF fi-cod-estabel IN FRAME f-main /* Estabelecimento */
DO:
  IF INPUT FRAME {&FRAME-NAME} fi-cod-estabel <> "" THEN DO.
     FIND estabelec WHERE
          estabelec.cod-estabel = INPUT FRAME {&FRAME-NAME} fi-cod-estabel 
          NO-LOCK NO-ERROR.
     IF NOT AVAIL estabelec THEN DO.
        MESSAGE 'Estabelecimento nÆo Cadastrado....'
             VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        APPLY 'ENTRY' TO SELF.
        RETURN NO-APPLY.
     END.
     ASSIGN fi-nome-estabel:SCREEN-VALUE = estabelec.nome.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-estabel V-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-cod-estabel IN FRAME f-main /* Estabelecimento */
DO:
  {include/zoomvar.i &prog-zoom=adzoom\z02ad098.w
                     &campo=fi-cod-estabel
                     &campozoom=cod-estabel}
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


&Scoped-define SELF-NAME fi-localiz-fin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-localiz-fin V-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-localiz-fin IN FRAME f-main
DO:
    {include/zoomvar.i &prog-zoom = inzoom/z02in172.w
                       &campo     = fi-it-codigo-fin
                       &campozoom = it-codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-localiz-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-localiz-ini V-table-Win
ON LEAVE OF fi-localiz-ini IN FRAME f-main /* Localiza‡Æo */
DO:
  IF SELF:SCREEN-VALUE <> '000/000' THEN
     ASSIGN fi-localiz-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-nr-pedcli-fin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nr-pedcli-fin V-table-Win
ON LEAVE OF fi-nr-pedcli-fin IN FRAME f-main
DO:
   ASSIGN fi-perc-min:SENSITIVE = NO
          fi-perc-max:SENSITIVE = NO.

   IF INPUT FRAME {&FRAME-NAME} fi-nr-pedcli-fin = INPUT FRAME {&FRAME-NAME} fi-nr-pedcli-ini AND
      INPUT FRAME {&FRAME-NAME} fi-nr-seq-fin = INPUT FRAME {&FRAME-NAME} fi-nr-seq-ini THEN
      ASSIGN fi-perc-min:SENSITIVE = YES
             fi-perc-max:SENSITIVE = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nr-pedcli-fin V-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-nr-pedcli-fin IN FRAME f-main
DO:
    {include/zoomvar.i &prog-zoom = dizoom/z01di159.w
                       &campo     = fi-nr-pedcli-fin
                       &campozoom = nr-pedcli}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-nr-pedcli-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nr-pedcli-ini V-table-Win
ON LEAVE OF fi-nr-pedcli-ini IN FRAME f-main /* Pedido */
DO:
   IF SELF:SCREEN-VALUE <> '' THEN
      ASSIGN fi-nr-pedcli-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nr-pedcli-ini V-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-nr-pedcli-ini IN FRAME f-main /* Pedido */
DO:
  {include/zoomvar.i &prog-zoom = dizoom/z01di159.w
                     &campo     = fi-nr-pedcli-ini
                     &campozoom = nr-pedcli}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-nr-seq-fin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nr-seq-fin V-table-Win
ON LEAVE OF fi-nr-seq-fin IN FRAME f-main
DO:
   ASSIGN fi-perc-min:SENSITIVE = NO
          fi-perc-max:SENSITIVE = NO.

   IF INPUT FRAME {&FRAME-NAME} fi-nr-pedcli-fin = INPUT FRAME {&FRAME-NAME} fi-nr-pedcli-ini AND
      INPUT FRAME {&FRAME-NAME} fi-nr-seq-fin = INPUT FRAME {&FRAME-NAME} fi-nr-seq-ini THEN
      ASSIGN fi-perc-min:SENSITIVE = YES
             fi-perc-max:SENSITIVE = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-nr-seq-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nr-seq-ini V-table-Win
ON LEAVE OF fi-nr-seq-ini IN FRAME f-main /* Sequˆncia */
DO:
  IF SELF:INPUT-VALUE <> 0 THEN
     ASSIGN fi-nr-seq-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-perc-min
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-perc-min V-table-Win
ON VALUE-CHANGED OF fi-perc-min IN FRAME f-main /* Tolerƒncia */
DO:
  ASSIGN SELF:FGCOLOR = 0.
  IF SELF:INPUT-VALUE < 0 THEN
     ASSIGN SELF:FGCOLOR = 12.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-acomp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-acomp V-table-Win
ON VALUE-CHANGED OF tg-acomp IN FRAME f-main /* Acompanhamento Passo a Passo */
DO:
   IF SELF:SCREEN-VALUE = 'YES' THEN DO.
      MESSAGE "ATEN€ÇO !!!   OP€ÇO NÇO RECOMENDADA !!!!" SKIP
              "Vocˆ est  solicitando um acompanhamento passo a passo," SKIP
              "essa op‡Æo obrigar  vocˆ a dar um clique ou enter em cada" SKIP
              "mensagem de Acompanhamento at‚ o t‚rmino do processamento," SKIP
              "nÆo sendo poss¡vel cancelar..." SKIP(2)
              "Realmente deseja isso ?"
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
          TITLE "NÇO RECOMENDADO" UPDATE choice AS LOGICAL.
      IF NOT choice THEN
         ASSIGN SELF:SCREEN-VALUE = 'NO'.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-lote-todos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-lote-todos V-table-Win
ON VALUE-CHANGED OF tg-lote-todos IN FRAME f-main /* TODOS */
DO:
  IF INPUT FRAME {&FRAME-NAME} tg-lote-todos = NO THEN
     ASSIGN tg-lote-pp:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            tg-lote-pd:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            tg-lote-rp:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            tg-lote-rd:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            tg-lote-sc:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            tg-lote-ca:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            tg-lote-rd:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'YES'
            tg-lote-rp:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'YES'.
  ELSE
     ASSIGN tg-lote-pp:SENSITIVE IN FRAME {&FRAME-NAME} = NO
            tg-lote-pd:SENSITIVE IN FRAME {&FRAME-NAME} = NO
            tg-lote-rp:SENSITIVE IN FRAME {&FRAME-NAME} = NO
            tg-lote-rd:SENSITIVE IN FRAME {&FRAME-NAME} = NO
            tg-lote-sc:SENSITIVE IN FRAME {&FRAME-NAME} = NO
            tg-lote-ca:SENSITIVE IN FRAME {&FRAME-NAME} = NO
            tg-lote-rd:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "no"
            tg-lote-rp:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "no".
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
  fi-nr-pedcli-ini:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.
  fi-nr-pedcli-fin:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize-fields V-table-Win 
PROCEDURE local-initialize-fields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN esapi/busca-estab-usuar.p (INPUT c-seg-usuario,
                                 OUTPUT c-cod-estabel).

  IF c-cod-estabel = "" THEN DO.
     MESSAGE 'Usuario ' c-seg-usuario ' nÆo relacionado … um Estabelecimento....'
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
     RETURN "ADM-ERROR".
  END.
  IF c-cod-estabel = '' OR c-cod-estabel = '0' THEN DO:
     ASSIGN c-cod-estabel = '1'.
     ASSIGN fi-cod-estabel:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
  END.

  ASSIGN fi-cod-estabel:SCREEN-VALUE IN FRAME {&FRAME-NAME}      = c-cod-estabel
         fi-dt-limite:SCREEN-VALUE IN FRAME {&FRAME-NAME}        = STRING(MONTH(TODAY),'99') + STRING(YEAR(TODAY),'9999')
         fi-it-codigo-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = ''
         fi-it-codigo-fin:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = 'ZZZZZZZZZZZZZZZ'
         fi-cod-refer-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = ''
         fi-cod-refer-fin:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = 'ZZZZZZZZZZ'
         fi-nr-pedcli-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = ''
         fi-nr-pedcli-fin:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = 'ZZZZZZZZZZ'
         fi-nr-seq-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME}       = '0'
         fi-nr-seq-fin:SCREEN-VALUE IN FRAME {&FRAME-NAME}       = '9999'    
         fi-localiz-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME}      = '000000'
         fi-localiz-fin:SCREEN-VALUE IN FRAME {&FRAME-NAME}      = '999999'
         fi-corte-comerc-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "A"
         fi-corte-comerc-fin:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Z"
         fi-cod-obsoleto-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '0'
         fi-cod-obsoleto-fin:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '4'
         fi-perc-min:SCREEN-VALUE IN FRAME {&FRAME-NAME}         = '-5'
         fi-perc-max:SCREEN-VALUE IN FRAME {&FRAME-NAME}         = '10'
         tg-lote-todos:SCREEN-VALUE IN FRAME {&FRAME-NAME}       = 'NO'
         tg-lote-rp:SCREEN-VALUE IN FRAME {&FRAME-NAME}          = 'YES'
         tg-lote-rd:SCREEN-VALUE IN FRAME {&FRAME-NAME}          = 'YES'
         rs-opc-artigo:SCREEN-VALUE IN FRAME {&FRAME-NAME}       = 'A'
         tg-res-crivo:SCREEN-VALUE IN FRAME {&FRAME-NAME}        = 'YES'
         tg-acomp:SCREEN-VALUE IN FRAME {&FRAME-NAME}            = 'NO'.    

  APPLY 'LEAVE' TO fi-cod-estabel IN FRAME {&FRAME-NAME}.

  ASSIGN fi-perc-min:SENSITIVE = NO
         fi-perc-max:SENSITIVE = NO.
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

